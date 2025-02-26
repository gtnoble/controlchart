import assert from 'node:assert/strict';

import Database from 'better-sqlite3';

import statistics from '@stdlib/stats';

import * as stats from './stats.js';
import type {
  ChartData,
  ChartType,
  ControlLimitsType,
  Observation,
} from './types/chart.d.ts';

export type TransformationName = "log" | undefined;
export type TransformationType = (value: number) => number;
export type TransformationPair = { forward: TransformationType, reverse: TransformationType };

interface ChartParametersSchema {
  chartType: ChartType;
  dataName: string;
  aggregationInterval: number;
  setupStartTime: number;
  setupEndTime: number;
  transformation: string;
}

type ChartQueryResultSchema = {
      id: number,
      value: number,
      cusumUpperStatistic: number,
      cusumLowerStatistic: number,
      time: number,
      annotations: string
    };

type ControlLimitsQueryResultSchema = {
  upperControlLimit: number,
  lowerControlLimit: number,
  cusumControlLimit: number,
  mean: number
}

interface ChartSetupSchema {
  setupStartTime: number,
  setupEndTime: number,
  creationTime: number
}

  const createSQL = {
    // Base tables
    createChartDataTable: `
      CREATE TABLE IF NOT EXISTS chart_data (
        id INTEGER PRIMARY KEY ASC AUTOINCREMENT,
        time INTEGER,
        value REAL,
        dataName STRING
      )`,
    createChartAnnotationsTable: `
      CREATE TABLE IF NOT EXISTS chart_annotations (
        id INTEGER PRIMARY KEY ASC AUTOINCREMENT,
        chart_data_id INTEGER REFERENCES chart_data(id),
        annotation TEXT,
        created_time INTEGER
      )`,
    createChartTable: `
      CREATE TABLE IF NOT EXISTS chart (
        chartName STRING PRIMARY KEY,
        chartType STRING,
        dataName STRING,
        aggregationInterval INTEGER
      )`,
    createSetupTable: `
      CREATE TABLE IF NOT EXISTS setup (
        id INTEGER PRIMARY KEY ASC AUTOINCREMENT,
        chartName STRING REFERENCES chart (chartName),
        setupStartTime INTEGER,
        setupEndTime INTEGER,
        creationTime INTEGER
      )`,
    createTransformationsTable: `
      CREATE TABLE IF NOT EXISTS transformations (
        chartName STRING REFERENCES chart (chartName),
        transformation STRING
      )`,
    createIndex: `
      CREATE INDEX IF NOT EXISTS chart_data_lookup ON chart_data (id, time, value, dataName)`,
    createChartIndex: `
      CREATE INDEX IF NOT EXISTS chart_lookup ON chart (chartName, chartType, dataName)
    `,
    createAnnotationsIndex: `
      CREATE INDEX IF NOT EXISTS annotations_lookup ON chart_annotations (id, chart_data_id)
    `,
    createSetupIndex: `
      CREATE INDEX IF NOT EXISTS setup_lookup ON setup (id, chartName, setupStartTime, setupEndTime)
    `,
    createTransformationsIndex: `
      CREATE INDEX IF NOT EXISTS transformations_lookup ON transformations (chartName, transformation)
    `,
    // Base views
    createLatestSetupView: `
      CREATE TEMP VIEW IF NOT EXISTS latest_setup AS
      WITH numbered_setups AS (
        SELECT 
          s.chartName,
          s.setupStartTime,
          s.setupEndTime,
          ROW_NUMBER() OVER (PARTITION BY s.chartName ORDER BY s.id DESC) AS rn
        FROM setup s
      )
      SELECT 
        chartName,
        setupStartTime,
        setupEndTime,
        rn
      FROM numbered_setups
      WHERE rn = 1
    `,

    // Core data views
    createBaseDataView: `
      CREATE TEMP VIEW IF NOT EXISTS base_data AS
      SELECT
        cd.id,
        cd.time,
        cd.value,
        cd.dataName,
        GROUP_CONCAT(ca.annotation) AS annotations,
        cp.chartName,
        cp.transformation,
        transform(cp.transformation, cd.value) AS transformed_value
      FROM chart_data cd
      LEFT JOIN chart_annotations ca ON cd.id = ca.chart_data_id
      JOIN chart_parameters cp ON cd.dataName = cp.dataName
      GROUP BY cd.id, cd.time, cd.value, cd.dataName
    `,

    createSetupStatsView: `
      CREATE TEMP VIEW IF NOT EXISTS chart_summary AS
      SELECT
        bd.chartName,
        AVG(bd.transformed_value) AS transformedMean,
        standardDeviation(bd.transformed_value) AS transformedStandardDeviation
      FROM base_data bd
      JOIN latest_setup ls ON bd.chartName = ls.chartName
      WHERE bd.time BETWEEN ls.setupStartTime AND ls.setupEndTime
      GROUP BY bd.chartName
    `,

    createChartParametersView: `
      CREATE TEMP VIEW IF NOT EXISTS chart_parameters AS
      SELECT 
        c.chartName,
        c.dataName,
        c.chartType,
        c.aggregationInterval,
        t.transformation,
        ls.setupStartTime,
        ls.setupEndTime
      FROM chart c
      LEFT JOIN transformations t ON c.chartName = t.chartName
      LEFT JOIN latest_setup ls ON c.chartName = ls.chartName
    `,

    // CUSUM views
    createCusumParametersView: `
      CREATE TEMP VIEW IF NOT EXISTS cusum_parameters AS
      WITH const AS (
        SELECT 1 AS delta, 0.0027 AS alpha, 0.01 AS beta
      ),
      intermediate AS (
        SELECT 
          cs.chartName,
          cs.transformedStandardDeviation * c.delta / 2 AS k,
          2 / (c.delta * c.delta) * ln((1 - c.beta) / c.alpha) AS d
        FROM
          chart_summary cs, const c
      )
      SELECT
        chartName,
        i.k,
        i.d * i.k AS h
      FROM intermediate i
    `,

    createCusumView: `
      CREATE TEMP VIEW IF NOT EXISTS cusum AS
      SELECT 
        bd.chartName,
        bd.id,
        cusumPositiveS(bd.transformed_value, cs.transformedMean, COALESCE(cp.k, 0)) 
          OVER (PARTITION BY bd.chartName ORDER BY bd.time, bd.id ROWS UNBOUNDED PRECEDING) 
          AS transformedUpperStatistic,
        cusumNegativeS(bd.transformed_value, cs.transformedMean, COALESCE(cp.k, 0)) 
          OVER (PARTITION BY bd.chartName ORDER BY bd.time, bd.id ROWS UNBOUNDED PRECEDING) 
          AS transformedLowerStatistic
      FROM base_data bd
      JOIN chart_summary cs ON bd.chartName = cs.chartName
      LEFT JOIN cusum_parameters cp ON bd.chartName = cp.chartName
      WHERE bd.time > COALESCE(
        (SELECT setupEndTime FROM latest_setup WHERE chartName = bd.chartName), 
        bd.time
      )
    `,

    // Control limits view
    createControlLimitsView: `
      CREATE TEMP VIEW IF NOT EXISTS control_limits AS
      SELECT
        cs.chartName,
        cs.transformedMean AS mean,
        cs.transformedMean + cs.transformedStandardDeviation * 3 AS upperControlLimit,
        cs.transformedMean - cs.transformedStandardDeviation * 3 AS lowerControlLimit,
        cp.h AS cusumControlLimit,
        t.transformation
      FROM chart_summary cs
      LEFT JOIN cusum_parameters cp ON cs.chartName = cp.chartName
      LEFT JOIN transformations t ON cs.chartName = t.chartName
    `,

    // Chart points view
    createChartPointsView: `
      CREATE TEMP VIEW IF NOT EXISTS chart_points AS
      SELECT 
        bd.id,
        bd.chartName,
        bd.time,
        bd.transformed_value AS transformed_value,
        bd.value AS original_value,
        cu.transformedUpperStatistic,
        cu.transformedLowerStatistic,
        bd.annotations,
        bd.transformation
      FROM base_data bd
      LEFT JOIN cusum cu ON bd.chartName = cu.chartName AND bd.id = cu.id
      ORDER BY bd.time, bd.id
    `
  } as const;

  const SQL = {
    getRecentChartPoints: `
      WITH recent_points AS (
        SELECT 
          cp.id,
          cp.chartName,
          cp.time,
          cp.transformed_value AS value,
          cp.transformedUpperStatistic AS cusumUpperStatistic,
          cp.transformedLowerStatistic AS cusumLowerStatistic,
          cp.transformedLowerStatistic,
          cp.annotations
        FROM chart_points cp
        WHERE cp.chartName = :chartName
        ORDER BY cp.time DESC, cp.id DESC
        LIMIT :count
      )
      SELECT 
        id,
        time,
        cusumUpperStatistic,
        cusumLowerStatistic,
        value,
        annotations
      FROM recent_points
      ORDER BY time ASC, id ASC
    `,
    insertObservation: `
      INSERT INTO chart_data (time, value, dataName) VALUES (?, ?, ?);`,
    getEarliestDataTime: `
      SELECT time FROM chart_data WHERE dataName = :data_name ORDER BY time LIMIT 1;`,
    getLatestDataTime: `
      SELECT time FROM chart_data WHERE dataName = :data_name ORDER BY time DESC LIMIT 1;`,
    setTransformation: `
      INSERT OR REPLACE INTO transformations (chartName, transformation) VALUES (:chartName, :transformation);`,
    getTransformation: `
      SELECT transformation FROM transformations WHERE chartName = :chartName LIMIT 1;`,
    insertSetup: `
      INSERT INTO setup (chartName, setupStartTime, setupEndTime, creationTime) VALUES (:chartName, :setupStartTime, :setupEndTime, :creationTime);`,
    getSetup: `
      SELECT * FROM latest_setup WHERE chartName = :chartName LIMIT 1;`,
    initializeChart: `
      INSERT OR REPLACE INTO chart (chartName, chartType, dataName, aggregationInterval) VALUES (:chartName, :chartType, :dataName, :aggregationInterval);`,
    getAvailableCharts: `
      SELECT DISTINCT chartName FROM chart ORDER BY chartName;`,
    getAvailableDataNames: `
      SELECT DISTINCT dataName FROM chart_data ORDER BY dataName;
    `,
    getChartParameters: `
      SELECT * FROM chart_parameters WHERE chartName = :chartName`,
    getChartPoints: `
      SELECT 
        cp.id,
        cp.chartName,
        cp.time,
        cp.transformed_value AS value,
        cp.transformedUpperStatistic AS cusumUpperStatistic,
        cp.transformedLowerStatistic AS cusumLowerStatistic,
        cp.annotations
      FROM chart_points cp
      WHERE 
        cp.chartName = :chartName 
        AND cp.time BETWEEN :startTime AND :endTime
      ORDER BY cp.time, cp.id;`,
    getControlLimits: `
      SELECT
        cl.chartName,
        cl.mean,
        cl.upperControlLimit,
        cl.lowerControlLimit,
        cl.cusumControlLimit
      FROM
        control_limits cl
      WHERE
        cl.chartName = :chartName
    `,
    addAnnotationQuery: `
      INSERT INTO chart_annotations 
        (chart_data_id, annotation, created_time) 
        VALUES (?, ?, ?)
    `,
    getAnnotationQuery: `
      SELECT annotation FROM chart_annotations 
        WHERE chart_data_id = ? 
        ORDER BY created_time DESC
        LIMIT 1
    `,
getExtremeChartPoints: `
  WITH cl_data AS (
    SELECT
      cl.chartName,
      cl.mean,
      cl.upperControlLimit,
      cl.lowerControlLimit,
      cl.cusumControlLimit
    FROM
      control_limits cl
    WHERE
      cl.chartName = :chartName
  ),
  cp_data AS (
    SELECT 
      cp.id,
      cp.chartName,
      cp.time,
      cp.transformed_value AS value,
      cp.transformedUpperStatistic AS cusumUpperStatistic,
      cp.transformedLowerStatistic AS cusumLowerStatistic,
      cp.annotations
    FROM chart_points cp
    WHERE 
      cp.chartName = :chartName 
      AND cp.time BETWEEN :startTime AND :endTime
    ORDER BY cp.time, cp.id
  ),
  blocks AS (
    SELECT
      cp.*,
      NTILE(:numBlocks) OVER (ORDER BY cp.time) AS block_number
    FROM cp_data cp
  ),
  block_means AS (
    SELECT
      block_number,
      AVG(value) as block_mean
    FROM blocks
    GROUP BY block_number
  ),
  block_extremes AS (
    SELECT
      b.block_number,
      b.id,
      b.time,
      b.value,
      b.cusumUpperStatistic,
      b.cusumLowerStatistic,
      b.annotations,
      CASE 
        WHEN cl.mean IS NOT NULL THEN ABS(b.value - cl.mean)
        ELSE ABS(b.value - bm.block_mean)
      END AS abs_diff
    FROM blocks b
    LEFT JOIN cl_data cl ON b.chartName = cl.chartName
    JOIN block_means bm ON b.block_number = bm.block_number
  ),
  max_extremes AS (
    SELECT
      be.block_number,
      be.id,
      be.time,
      be.value,
      be.cusumUpperStatistic,
      be.cusumLowerStatistic,
      be.annotations
    FROM block_extremes be
    WHERE (be.block_number, be.abs_diff) IN (
      SELECT
        block_number,
        MAX(abs_diff)
      FROM block_extremes
      GROUP BY block_number
    )
  )
  SELECT
    me.id,
    me.time,
    me.value,
    me.cusumUpperStatistic,
    me.cusumLowerStatistic,
    me.annotations
  FROM max_extremes me
  ORDER BY me.time, me.id;
`
} as const;

  const reverseTransform = (transformationName: TransformationName | false | undefined, value: number) => {
    if (transformationName === "log") {
      return Math.pow(10, Number(value));
    }
    else {
      return value
    }};

  export class ChartDb {
  

  database: Database.Database;
  preparedSql: { -readonly [key in keyof (typeof SQL)]?: Database.Statement} = {};

  constructor(databaseName: string) {
    this.database = new Database(databaseName);
    this.database.pragma('journal_mode = WAL');
    this.database.pragma('foreign_keys = ON');
    
    this.database.aggregate(
      'standardDeviation',
      {
        deterministic: true,
        start: () => [statistics.incr.incrstdev()],
        step: (array: any[], nextValue) => {array[0](nextValue)},
        result: (array) => array[0](),
        //@ts-ignore
        inverse: (array, next) => {return}
      }
    );
    
    this.database.aggregate.length
    
    this.database.aggregate(
      'cusumPositiveS',
      {
        deterministic: true,
        start: () => [0],
        //@ts-ignore
        step: (array: any[], nextValue: number, mean: number, k: number) => {
          const candidateNext = array[0] + nextValue - mean - k
          array[0] = candidateNext > 0 ? candidateNext : 0;
        },
        result: (array) => array[0],
        //@ts-ignore
        inverse: () => {return}
      }
    )

    this.database.aggregate(
      'cusumNegativeS',
      {
        deterministic: true,
        start: () => [0],
        //@ts-ignore
        step: (array: any[], nextValue: number, mean: number, k: number) => {
          const candidateNext = array[0] - nextValue + mean - k
          array[0] = candidateNext > 0 ? candidateNext : 0;
        },
        result: (array) => array[0],
        //@ts-ignore
        inverse: () => {return}
      }
    )
    
    
    this.database.function(
      'transform', 
      {deterministic: true}, 
      (transformationName, value) => {
      if (transformationName === "log") {
        return Math.log10(Number(value));
      }
      else {
        return value
      }
    })

    
    this.database.function(
      'reverseTransform', 
      {deterministic: true}, 
      //@ts-ignore
      reverseTransform
    )

    // Create tables first
    this.database.prepare(createSQL.createChartDataTable).run();
    this.database.prepare(createSQL.createChartAnnotationsTable).run();
    this.database.prepare(createSQL.createChartTable).run();
    this.database.prepare(createSQL.createSetupTable).run();
    this.database.prepare(createSQL.createTransformationsTable).run();

    // Create indexes
    this.database.prepare(createSQL.createIndex).run();
    this.database.prepare(createSQL.createChartIndex).run();
    this.database.prepare(createSQL.createSetupIndex).run();
    this.database.prepare(createSQL.createAnnotationsIndex).run();
    this.database.prepare(createSQL.createTransformationsIndex).run();

    // Create views in dependency order
    this.database.prepare(createSQL.createLatestSetupView).run();
    this.database.prepare(createSQL.createBaseDataView).run();
    this.database.prepare(createSQL.createSetupStatsView).run();
    this.database.prepare(createSQL.createChartParametersView).run();
    this.database.prepare(createSQL.createCusumParametersView).run();
    this.database.prepare(createSQL.createCusumView).run();
    this.database.prepare(createSQL.createControlLimitsView).run();
    this.database.prepare(createSQL.createChartPointsView).run();
    
    for (const key of Object.keys(SQL) as (keyof (typeof SQL))[]) {
      console.error(`preparing: ${key}`)
      this.preparedSql[key] = this.database.prepare(SQL[key]);
    }

  }

  saveObservation(observation: { value: number; dataName: string; time?: number; annotations?: string | string[] }) {
    assert(this.preparedSql.insertObservation)
    const insertTime = observation.time ? observation.time : Date.now();
    const result = this.preparedSql.insertObservation.run(insertTime, observation.value, observation.dataName);
      const id = Number(result.lastInsertRowid);
      
      if (typeof observation.annotations === "string") {
        this.addAnnotation(id, observation.annotations);
      }
      else if (observation.annotations instanceof Array) {
        for (const annotation of observation.annotations) {
          this.addAnnotation(id, annotation)
        }
      }
      
      return id;
    };

  addObservation(value: number, dataName: string, annotations?: string | string[]): number {
    const unixTime = Date.now();
    return this.saveObservation(
      {value: value, dataName: dataName, time: unixTime, annotations: annotations}
    );
  }

  addAnnotation(chartDataId: number, annotation: string) {
    const unixTime = Date.now();
    assert(this.preparedSql.addAnnotationQuery)
    this.preparedSql.addAnnotationQuery.run(chartDataId, annotation, unixTime);
  }

  getAnnotation(chartDataId: number): string | null {
    assert(this.preparedSql.getAnnotationQuery)
    const result = this.preparedSql.getAnnotationQuery.get({ chart_data_id: chartDataId }) as { annotation: string } | undefined;
    return result?.annotation || null;
  }

  getLatestDataTime(dataName: string) {
    assert(this.preparedSql.getLatestDataTime)
    return (this.preparedSql.getLatestDataTime.get({ data_name: dataName }) as { time: number }).time;
  }

  getEarliestDataTime(dataName: string) {
    assert(this.preparedSql.getEarliestDataTime)
    return (this.preparedSql.getEarliestDataTime.get({ data_name: dataName }) as { time: number }).time;
  }

  getChartDataLimits(chart: ChartParametersSchema) {
    return [this.getEarliestDataTime(chart.dataName), this.getLatestDataTime(chart.dataName)];
  }

  initializeChart(
    chartName: string,
    dataName: string,
    chartType: ChartType,
    aggregationInterval: number
  ) {

    assert(this.preparedSql.initializeChart)
    this.preparedSql.initializeChart.run(
      {
        chartName: chartName,
        chartType: chartType,
        dataName: dataName,
        aggregationInterval: aggregationInterval
      });
  }

  addChartSetup(
    chartName: string,
    setupStartTime: Date,
    setupEndTime: Date
  ) {
    assert(this.preparedSql.insertSetup)
    this.preparedSql.insertSetup.run(
      {
        chartName: chartName,
        setupStartTime: setupStartTime.valueOf(),
        setupEndTime: setupEndTime.valueOf(),
        creationTime: (new Date()).valueOf()
      }
    )
  }

  setTransformation(
    chartName: string,
    transformation: TransformationName
  ) {
    assert(this.preparedSql.setTransformation)
    this.preparedSql.setTransformation.run(
      { chartName: chartName, transformation: transformation }
    );
  }

  getTransformation(
    chartName: string
  ): TransformationName | undefined {
    assert(this.preparedSql.getTransformation)
    const transformationRow: any = this.preparedSql.getTransformation.get(
      { chartName: chartName }
    )

    const transformation = transformationRow?.transformation;
    return transformation;
  }


  getChartSetup(
    chartName: string
  ) {
    assert(this.preparedSql.getSetup);
    return this.preparedSql.getSetup.get({ chartName: chartName }) as ChartSetupSchema | undefined;
  }

  getChartParameters(chartName: string): ChartParametersSchema {
    assert(this.preparedSql.getChartParameters);
    return this.preparedSql.getChartParameters.get({chartName: chartName}) as ChartParametersSchema;
  }

  getChartPoints(chartName: string, startTime: number, endTime: number, isTransformed: boolean = false): ChartQueryResultSchema[] {
    assert(this.preparedSql.getChartPoints)
    assert(this.preparedSql.getExtremeChartPoints)
    const transformationName = ! isTransformed && this.getTransformation(chartName);
    return ((this.preparedSql.getExtremeChartPoints)
      .all(
        {chartName: chartName,
          startTime: startTime,
          endTime: endTime,
          numBlocks: 1000
        }
      ) as ChartQueryResultSchema[]).map(
        (row: ChartQueryResultSchema) => {
          return {
            ...row, 
            value: reverseTransform(transformationName, row.value),
            cusumLowerStatistic: reverseTransform(transformationName, row.cusumLowerStatistic),
            cusumUpperStatistic: reverseTransform(transformationName, row.cusumUpperStatistic)
          };
      })
  }

  getRecentChartPoints(chartName: string, count: number, isTransformed: boolean = false): ChartQueryResultSchema[] {
    assert(this.preparedSql.getRecentChartPoints)
    const transformationName = isTransformed && this.getTransformation(chartName);
    return ((this.preparedSql.getRecentChartPoints)
      .all({
        chartName: chartName,
        count: count
      }) as ChartQueryResultSchema[])
      .map(
        (row: ChartQueryResultSchema) => {
          return {
            ...row, 
            value: reverseTransform(transformationName, row.value),
            cusumLowerStatistic: reverseTransform(transformationName, row.cusumLowerStatistic),
            cusumUpperStatistic: reverseTransform(transformationName, row.cusumUpperStatistic)
          };
        }
      )
  }

  getRecentChart(chartName: string, count: number, isTransformed: boolean = false): ChartData {
    const parameters = this.getChartParameters(chartName);
    const setup = this.getChartSetup(chartName);
    const limits = setup && this.getControlLimits(chartName, isTransformed);
    
    const points = this.getRecentChartPoints(chartName, count, isTransformed);
    
    const values = points.map((point) => point.value);
    const observations: Observation[] = points.map((point) => ({
      id: point.id,
      individualsValue: point.value,
      cusum: {upperStatistic: point.cusumUpperStatistic, lowerStatistic: point.cusumLowerStatistic},
      time: point.time,
      isSetup: setup !== undefined && point.time >= setup.setupStartTime && point.time <= setup.setupEndTime,
      annotations: point.annotations ? point.annotations.split(',') : []
    }));

    return {
      type: parameters.chartType,
      tests: {
        runsRandom: stats.runsRandomnessTest(values),
        ksNormal: stats.normalityTest(values)
      },
      controlLimits: limits,
      observations: observations
    };
  }

  getControlLimits(
    chartName: string,
    isTransformed: boolean = false
  ): ControlLimitsType {
    assert(this.preparedSql.getControlLimits);
    const transformation =  ! isTransformed && this.getTransformation(chartName);
    const result = (this.preparedSql.getControlLimits)
      .get({
        chartName: chartName
      }) as ControlLimitsQueryResultSchema;

    return {
      individualsMean: reverseTransform(transformation, result.mean),
      upperIndividualsLimit: reverseTransform(transformation, result.upperControlLimit),
      lowerIndividualsLimit: reverseTransform(transformation, result.lowerControlLimit),
      cusumLimit: reverseTransform(transformation, result.cusumControlLimit)
    }
  }
  
  getChart(chartName: string, startTime?: number, endTime?: number, isTransformed: boolean = false): ChartData {
    const parameters = this.getChartParameters(chartName);
    const setup = this.getChartSetup(chartName);
    const limits = setup && this.getControlLimits(
      chartName,
      isTransformed
    );
    const dataStartTime = startTime ? startTime : this.getEarliestDataTime(parameters.dataName);
    const dataEndTime = endTime ? endTime : this.getLatestDataTime(parameters.dataName);
    const points = this.getChartPoints(chartName, dataStartTime, dataEndTime, isTransformed);
    
    const values = points.map((point) => point.value);
    const observations: Observation[] = points.map((point) => ({
      id: point.id,
      individualsValue: point.value,
      cusum: {upperStatistic: point.cusumUpperStatistic, lowerStatistic: point.cusumLowerStatistic},
      time: point.time,
      isSetup: setup !== undefined && point.time >= setup.setupStartTime && point.time <= setup.setupEndTime,
      annotations: point.annotations ? point.annotations.split(',') : []
    }));

    return {
      type: parameters.chartType,
      tests: {
        runsRandom: stats.runsRandomnessTest(values),
        ksNormal: stats.normalityTest(values)
      },
      controlLimits: limits,
      observations: observations
    };
  }

  getAvailableCharts() {
    assert(this.preparedSql.getAvailableCharts)
    return this.preparedSql.getAvailableCharts.all().map((row: any) => row.chartName);
  }
  
  getAvailableDataNames() {
    assert(this.preparedSql.getAvailableDataNames)
    return this.preparedSql.getAvailableDataNames.all().map((row: any) => row.dataName);
  }

}
