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

const LOG_TRANSFORMATION_PAIR: TransformationPair = {
  forward: (value) => Math.log10(value),
  reverse: (value) => Math.pow(10, value)
};

const NULL_TRANSFORMATION: TransformationPair = {
  forward: (value) => value,
  reverse: (value) => value
};

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
      CREATE INDEX IF NOT EXISTS chart_data_lookup ON chart_data (time, value, dataName)`,
    createLatestSetupView: `
      CREATE TEMP VIEW IF NOT EXISTS latest_setup AS
      SELECT 
        s.chartName,
        s.setupStartTime,
        s.setupEndTime,
        ROW_NUMBER() OVER (PARTITION BY s.chartName ORDER BY s.id DESC) AS rn
      FROM setup s
    `,
    setupPointsView: `
      CREATE TEMP VIEW IF NOT EXISTS setup_points AS
      WITH chart_data_base AS (
        SELECT
          cd.id,
          cd.time,
          cd.value,
          cd.dataName
        FROM chart_data cd
      )
      SELECT
        cdb.id,
        cdb.time,
        cdb.value,
        ls.chartName
      FROM chart_data_base cdb
      INNER JOIN chart c ON
        c.dataName = cdb.dataName
      INNER JOIN latest_setup ls ON
        c.chartName = ls.chartName AND ls.rn = 1
      WHERE
        cdb.time BETWEEN ls.setupStartTime AND ls.setupEndTime
    `,
    createSetupStatsView: `
      CREATE TEMP VIEW IF NOT EXISTS 
      chart_summary 
      AS
      WITH setup_data AS (
        SELECT
          sp.chartName,
          sp.value,
          t.transformation
        FROM setup_points sp
        INNER JOIN chart c ON sp.chartName = c.chartName
        LEFT JOIN transformations t ON c.chartName = t.chartName
      )
      SELECT
          sd.chartName,
          AVG(transform(sd.transformation, sd.value)) AS transformedMean,
          standardDeviation(transform(sd.transformation, sd.value)) AS transformedStandardDeviation
      FROM setup_data sd
      GROUP BY sd.chartName;
    `,
    createChartParametersView: `
      CREATE TEMP VIEW IF NOT EXISTS
        chart_parameters
      AS
      WITH chart_base AS (
        SELECT 
          c.chartName,
          c.dataName,
          c.chartType,
          c.aggregationInterval,
          t.transformation
        FROM chart c
        LEFT JOIN transformations t ON c.chartName = t.chartName
      )
      SELECT 
        cb.chartName AS chartName,
        cb.dataName AS dataName,
        cb.chartType AS chartType,
        cb.aggregationInterval AS aggregationInterval,
        cb.transformation AS transformation,
        s.setupStartTime AS setupStartTime,
        s.setupEndTime AS setupEndTime
      FROM chart_base cb
      LEFT JOIN latest_setup s ON cb.chartName = s.chartName
      WHERE s.rn IS NULL OR s.rn = 1`,
    createCusumParametersView: `
      CREATE TEMP VIEW IF NOT EXISTS
        cusum_parameters
      AS
      WITH 
      const AS (
        SELECT
          1 AS delta,
          0.0027 AS alpha,
          0.01 AS beta
      ),
      chart_stats AS (
        SELECT
          cs.chartName,
          cs.transformedMean,
          cs.transformedStandardDeviation,
          const.delta,
          const.alpha,
          const.beta
        FROM chart_summary cs
        CROSS JOIN const
      ),
      chart_vars AS (
        SELECT
          cs.chartName,
          cs.delta * cs.transformedStandardDeviation / 2 AS k,
          2 / (cs.delta * cs.delta) * ln((1 - cs.beta) / cs.alpha) AS d
        FROM chart_stats cs
      )
      SELECT
        cv.chartName,
        cv.k AS k,
        cv.d AS d,
        cv.d * cv.k AS h
      FROM chart_vars cv
    `,
    createCusumView: `
      CREATE TEMP VIEW IF NOT EXISTS
        cusum
      AS
      WITH chart_data_transformed AS (
        SELECT
          cd.id,
          cd.time,
          cd.value,
          cp.chartName,
          transform(cp.transformation, cd.value) AS transformed_value,
          cs.transformedMean,
          COALESCE(cup.k, 0) AS k
        FROM chart_data cd
        INNER JOIN chart_parameters cp ON cp.dataName = cd.dataName
        INNER JOIN chart_summary cs ON cp.chartName = cs.chartName
        LEFT JOIN cusum_parameters cup ON cp.chartName = cup.chartName
        WHERE cd.time > COALESCE(cp.setupStartTime, cd.time)
      )
      SELECT 
        cdt.chartName,
        cdt.id,
        cusumPositiveS(cdt.transformed_value, cdt.transformedMean, cdt.k) OVER (
          PARTITION BY cdt.chartName 
          ORDER BY cdt.time, cdt.id
          ROWS UNBOUNDED PRECEDING
        ) AS transformedUpperStatistic,
        cusumNegativeS(cdt.transformed_value, cdt.transformedMean, cdt.k) OVER (
          PARTITION BY cdt.chartName 
          ORDER BY cdt.time, cdt.id
          ROWS UNBOUNDED PRECEDING
        ) AS transformedLowerStatistic
      FROM chart_data_transformed cdt
      ORDER BY cdt.time, cdt.id;
    `,
    transformedControlLimitsView: `
      CREATE TEMP VIEW IF NOT EXISTS
        transformed_control_limits
      AS
      SELECT
        cs.chartName,
        cs.transformedMean AS mean,
        cs.transformedMean + cs.transformedStandardDeviation * 3 AS upperControlLimit,
        cs.transformedMean - cs.transformedStandardDeviation * 3 AS lowerControlLimit,
        cp.h AS cusumControlLimit
      FROM
        chart_summary cs
      LEFT JOIN cusum_parameters cp ON cs.chartName = cp.chartName
    `,
    transformedChartPointsView: `
      CREATE TEMP VIEW IF NOT EXISTS
        transformed_chart_points
      AS
      WITH chart_data_base AS (
        SELECT
          cd.id,
          cd.time,
          cd.value,
          cd.dataName,
          GROUP_CONCAT(ca.annotation) AS annotations
        FROM chart_data cd
        LEFT JOIN chart_annotations ca ON cd.id = ca.chart_data_id
        GROUP BY cd.id, cd.time, cd.value, cd.dataName
      ),
      chart_data_transformed AS (
        SELECT
          cdb.id,
          cdb.time,
          cdb.value,
          cdb.annotations,
          cp.chartName,
          transform(cp.transformation, cdb.value) AS transformed_value
        FROM chart_data_base cdb
        JOIN chart_parameters cp ON cdb.dataName = cp.dataName
      )
      SELECT 
        cdt.id AS id,
        cdt.chartName AS chartName,
        cdt.time AS time,
        cdt.transformed_value AS value,
        cu.transformedUpperStatistic AS cusumUpperStatistic,
        cu.transformedLowerStatistic AS cusumLowerStatistic,
        cdt.annotations AS annotations
      FROM chart_data_transformed cdt
      LEFT JOIN cusum cu ON cdt.chartName = cu.chartName AND cdt.id = cu.id
      ORDER BY cdt.time, cdt.id;`
  } as const;

  const SQL = {
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
    getTransformedChartPoints: `
      WITH filtered_points AS (
        SELECT 
          tcp.id,
          tcp.chartName,
          tcp.time,
          tcp.value,
          tcp.cusumUpperStatistic,
          tcp.cusumLowerStatistic,
          tcp.annotations
        FROM transformed_chart_points tcp
        WHERE 
          tcp.chartName = :chartName 
          AND tcp.time BETWEEN :startTime AND :endTime
      )
      SELECT 
        fp.id,
        fp.chartName,
        fp.time,
        fp.value,
        fp.cusumUpperStatistic,
        fp.cusumLowerStatistic,
        fp.annotations
      FROM filtered_points fp
      ORDER BY fp.time, fp.id;`,
    getChartPoints: `
      WITH chart_points_base AS (
        SELECT 
          tcp.id, 
          tcp.time, 
          tcp.value,
          tcp.cusumUpperStatistic,
          tcp.cusumLowerStatistic,
          tcp.annotations,
          tcp.chartName,
          t.transformation
        FROM transformed_chart_points tcp
        JOIN chart_parameters cp ON tcp.chartName = cp.chartName
        LEFT JOIN transformations t ON cp.chartName = t.chartName
        WHERE tcp.chartName = :chartName AND tcp.time BETWEEN :startTime AND :endTime
      )
      SELECT 
        id, 
        time, 
        reverseTransform(transformation, value) AS value, 
        reverseTransform(transformation, cusumUpperStatistic) as cusumUpperStatistic,
        reverseTransform(transformation, cusumLowerStatistic) as cusumLowerStatistic,
        annotations
      FROM chart_points_base
      ORDER BY time, id;`,
    getTransformedControlLimits: `
      SELECT
        tcl.chartName,
        tcl.mean,
        tcl.upperControlLimit,
        tcl.lowerControlLimit,
        tcl.cusumControlLimit
      FROM
        transformed_control_limits tcl
      WHERE
        tcl.chartName = :chartName
    `,
    getControlLimits: `
      SELECT
        reverseTransform(t.transformation, tcl.mean) AS mean,
        reverseTransform(t.transformation, tcl.upperControlLimit) AS upperControlLimit,
        reverseTransform(t.transformation, tcl.lowerControlLimit) AS lowerControlLimit,
        reverseTransform(t.transformation, tcl.cusumControlLimit) AS cusumControlLimit
      FROM
        transformed_control_limits tcl
      LEFT JOIN transformations t ON tcl.chartName = t.chartName
      WHERE
        tcl.chartName = :chartName
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
    `
  } as const;

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
      (transformationName, value) => {
      if (transformationName === "log") {
        return Math.pow(10, Number(value));
      }
      else {
        return value
      }
    })

    // Create tables first
    this.database.prepare(createSQL.createChartDataTable).run();
    this.database.prepare(createSQL.createChartAnnotationsTable).run();
    this.database.prepare(createSQL.createChartTable).run();
    this.database.prepare(createSQL.createSetupTable).run();
    this.database.prepare(createSQL.createTransformationsTable).run();
    this.database.prepare(createSQL.createIndex).run();

    // Create views in dependency order
    this.database.prepare(createSQL.createLatestSetupView).run();
    this.database.prepare(createSQL.setupPointsView).run();
    this.database.prepare(createSQL.createSetupStatsView).run();
    this.database.prepare(createSQL.createChartParametersView).run();
    this.database.prepare(createSQL.createCusumParametersView).run();
    this.database.prepare(createSQL.createCusumView).run();
    this.database.prepare(createSQL.transformedControlLimitsView).run();
    this.database.prepare(createSQL.transformedChartPointsView).run();
    
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
  ): TransformationPair {
    assert(this.preparedSql.getTransformation)
    const transformationRow: any = this.preparedSql.getTransformation.get(
      { chartName: chartName }
    )

    const transformation = transformationRow?.transformation;
    if (!transformation) {
      return NULL_TRANSFORMATION;
    }
    else {
      assert(transformation === "log");
      return LOG_TRANSFORMATION_PAIR
    }
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
    assert(this.preparedSql.getTransformedChartPoints)
    return (isTransformed ? this.preparedSql.getTransformedChartPoints : this.preparedSql.getChartPoints)
      .all(
        {chartName: chartName,
          startTime: startTime,
          endTime: endTime
        }
      ) as ChartQueryResultSchema[]
  }

  getControlLimits(
    chartName: string,
    isTransformed: boolean = false
  ): ControlLimitsType {
    assert(this.preparedSql.getControlLimits);
    assert(this.preparedSql.getTransformedControlLimits);
    const result = (isTransformed ? this.preparedSql.getTransformedControlLimits : this.preparedSql.getControlLimits)
      .get({
        chartName: chartName
      }) as ControlLimitsQueryResultSchema;

    return {
      individualsMean: result.mean,
      upperIndividualsLimit: result.upperControlLimit,
      lowerIndividualsLimit: result.lowerControlLimit,
      cusumLimit: result.cusumControlLimit
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
