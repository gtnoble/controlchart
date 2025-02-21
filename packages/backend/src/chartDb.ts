import assert from 'node:assert/strict';

import Database from 'better-sqlite3';

import statistics from '@stdlib/stats';

import * as stats from './stats.js';
import type {
  ChartData,
  ChartType,
  ChartValue,
  ControlLimitsType,
  Observation,
} from './types/chart.d.ts';

export type TransformationName = "log" | undefined;
export type TransformationType = (value: number) => number;
export type TransformationPair = { forward: TransformationType, reverse: TransformationType };

const LOG_TRANSFORMATION_PAIR: TransformationPair = {
  forward: (value) => Math.log(value),
  reverse: (value) => Math.exp(value)
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

interface ChartPointSchema {
  id?: number;
  time: number;
  individualsValue: ChartValue;
  cusumValue: ChartValue;
  annotations?: string;
}

type ChartQueryResultSchema = {
      id: number,
      value: number,
      transformedValue: number,
      cusum: number,
      transformedCusum: number,
      time: number,
      annotations: string
    }

interface ChartCountPointSchema {
  time: number;
  value: number;
}

interface ChartSetupSchema {
  setupStartTime: number,
  setupEndTime: number,
  creationTime: number
}

export class ChartDb {
  
  private readonly createSQL: Record<string, string> = {
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
      CREATE TEMP VIEW IF NOT EXISTS latest_setup (
        chartName, setupStartTime, setupEndTime
      )
      AS
      SELECT 
        setup.chartName, 
        max(setup.setupStartTime) OVER(PARTITION BY setup.chartName), 
        setup.endTime
      FROM setup;
    `,
    setupPointsView: `
      CREATE VIEW setup_points
      SELECT
        id, time, value, chartName
      FROM
        chart_data
      INNER JOIN
        latest_setup
      ON
        chart_data.dataName = latest_setup.dataName
      WHERE
        time <= setupEndTime AND
        time >= setupStartTime
    `,
    createSetupStatsView: `
      CREATE VIEW chart_summary AS
      SELECT
          sp.chartName,
          avg(transform(p.transformation, sp.value)) AS transformedMean,
          standardDeviation(transform(p.transformation, sp.value)) AS transformedStandardDeviation
      FROM setup_points
      INNER JOIN chart ON setup_points.chartName = chart.chartName
      GROUP BY setup_points.chartName, chart.transformation;
    `,
    createChartParametersView: `
      CREATE TEMP VIEW IF NOT EXISTS
        chart_parameters
      SELECT 
        c.dataName, 
        c.chartType, 
        c.aggregationInterval,
        (
          SELECT 
            s.setupStartTime, 
            s.setupEndTime 
          FROM setup s
          WHERE s.chartName = c.chartName
          ORDER BY s.id
          LIMIT 1
        )
      FROM chart c
      JOIN transformations t ON c.chartName = t.chartName`,
    createCusumView: `
      CREATE TEMP VIEW IF NOT EXISTS
        cusum
      SELECT 
        id, 
        sum(transform(transformation, value) - transformedMean) OVER (
          PARTITION BY chartName 
          ROWS UNBOUNDED PRECEDING
        ) AS transformedCusum,
        reverseTransform(transformation, transformedCusum) AS cusum
      FROM chart_data
      INNER JOIN chart_parameters ON chart_parameters.dataName = chart_data.dataName
      INNER JOIN chart_summary ON chartName
      WHERE 
        time > setupStartTime
      ORDER BY time, id;
    `
  }

  private readonly SQL = {
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
    getChartParameters: `
      SELECT * FROM chart_parameters WHERE chartName = :chartName`,
    getChartPoints: `
      SELECT 
        chart_data.id, 
        time, 
        value, 
        transformedValue as transform(transformation, value),
        GROUP_CONCAT(chart_annotations.annotation) AS annotations
      FROM chart_data
      LEFT JOIN chart_annotations ON chart_data.id = chart_annotations.chart_data_id
      LEFT JOIN chart_parameters ON chart_data.dataName = chart_parameters.dataName
      LEFT JOIN cusum ON cusum.id = chart_data.id
      WHERE chartName = :chartName AND time BETWEEN ? AND ?
      GROUP BY chart_data.id
      ORDER BY time, chart_data.id;`,
    getCusum: `
      SELECT 
        *
      FROM cusum
      WHERE 
        chartName = :chartName
      ORDER BY time, id;
    `,
    getIndividualsControlLimits: `
      SELECT
        reverseTransform(transformation, transformedMean) AS mean,
        transformedMean + transformedStandardDeviation * :sigma AS transformedUpperControlLimit,
        reverseTransform(transformation, transformedUpperControlLimit) AS upperControlLimit,
        transformedMean - transformedStandardDeviation * :sigma AS transformedLowerControlLimit,
        reverseTransform(transformation, transformedLowerControlLimit) AS lowerControlLimit
      FROM
        chart_summary
      WHERE
        chartName = :chartName
    `
  };

  database: Database.Database;
  insertObservationQuery: Database.Statement;
  initializeChartQuery: Database.Statement;
  insertSetupQuery: Database.Statement;
  getSetupQuery: Database.Statement;
  getChartPointsQuery: Database.Statement;
  getChartParametersQuery: Database.Statement;
  getAvailableChartsQuery: Database.Statement;
  getEarliestDataTimeQuery: Database.Statement;
  getLatestDataTimeQuery: Database.Statement;
  setTransformationQuery: Database.Statement;
  getTransformationQuery: Database.Statement;
  addAnnotationQuery: Database.Statement;
  getAnnotationQuery: Database.Statement;


  constructor(databaseName: string) {
    this.database = new Database(databaseName);
    this.database.pragma('journal_mode = WAL');
    this.database.pragma('foreign_keys = ON');
    
    this.database.aggregate(
      'standardDeviation',
      {
        start: () => [],
        step: (array: any[], nextValue) => {array.push(nextValue)},
        result: (array) => statistics.base.stdev(array.length, 1, array, 1)
      }
    );
    
    this.database.function('transform', (transformationName, value) => {
      if (transformationName === "log") {
        return Math.log10(Number(value));
      }
      else {
        return value
      }
    })

    this.database.function('reverseTransform', (transformationName, value) => {
      if (transformationName === "log") {
        return Math.pow(10, Number(value));
      }
      else {
        return value
      }
    })

    for (const key of Object.keys(this.createSQL)) {
      this.database.prepare(this.createSQL[key]).run();
    }

    this.getEarliestDataTimeQuery = this.database.prepare(this.SQL.getEarliestDataTime);
    this.getLatestDataTimeQuery = this.database.prepare(this.SQL.getLatestDataTime);
    this.insertObservationQuery = this.database.prepare(this.SQL.insertObservation);
    this.setTransformationQuery = this.database.prepare(this.SQL.setTransformation);
    this.getTransformationQuery = this.database.prepare(this.SQL.getTransformation);
    this.insertSetupQuery = this.database.prepare(this.SQL.insertSetup);
    this.getSetupQuery = this.database.prepare(this.SQL.getSetup);
    this.initializeChartQuery = this.database.prepare(this.SQL.initializeChart);
    this.getChartParametersQuery = this.database.prepare(this.SQL.getChartParameters);
    this.getAvailableChartsQuery = this.database.prepare(this.SQL.getAvailableCharts);
    this.getChartPointsQuery = this.database.prepare(this.SQL.getChartPoints);

    this.addAnnotationQuery = this.database.prepare(
      `INSERT INTO chart_annotations 
                (chart_data_id, annotation, created_time) 
                VALUES (?, ?, ?)`
    );

    this.getAnnotationQuery = this.database.prepare<{ chart_data_id: number }>(
      `SELECT annotation FROM chart_annotations 
            WHERE chart_data_id = ? 
            ORDER BY created_time DESC
            LIMIT 1`
    );
  }

  saveObservation(observation: { value: number; dataName: string; time: string }) {
      const sql = `INSERT INTO chart_data (time, value, dataName) VALUES (?, ?, ?)`;
      const result = this.database.prepare(sql).run(observation.time, observation.value, observation.dataName);
      return Number(result.lastInsertRowid);
    };

  addObservation(value: number, dataName: string): number {
    const unixTime = Date.now();
    const result = this.insertObservationQuery.run(unixTime, value, dataName);
    return Number(result.lastInsertRowid);
  }



  addAnnotation(chartDataId: number, annotation: string) {
    const unixTime = Date.now();
    this.addAnnotationQuery.run(chartDataId, annotation, unixTime);
  }

  getAnnotation(chartDataId: number): string | null {
    const result = this.getAnnotationQuery.get({ chart_data_id: chartDataId }) as { annotation: string } | undefined;
    return result?.annotation || null;
  }

  getLatestDataTime(dataName: string) {
    return (this.getLatestDataTimeQuery.get({ data_name: dataName }) as { time: number }).time;
  }

  getEarliestDataTime(dataName: string) {
    return (this.getEarliestDataTimeQuery.get({ data_name: dataName }) as { time: number }).time;
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

    this.initializeChartQuery.run(
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
    this.insertSetupQuery.run(
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
    this.setTransformationQuery.run(
      { chartName: chartName, transformation: transformation }
    );
  }

  getTransformation(
    chartName: string
  ): TransformationPair {
    const transformationRow: any = this.getTransformationQuery.get(
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
    return this.getSetupQuery.get({ chartName: chartName }) as ChartSetupSchema | undefined;
  }

  getChartParameters(chartName: string): ChartParametersSchema {
    return this.getChartParametersQuery.get(chartName) as ChartParametersSchema;
  }

  getChartPoints(dataName: string, startTime: number, endTime: number): ChartQueryResultSchema[] {
    return this.getChartPointsQuery.all(
      dataName,
      startTime,
      endTime
    ) as ChartQueryResultSchema[]
    
  }

  getControlLimits(
    chartName: string,
  ): ControlLimitsType {
    const result = this.database.prepare(this.SQL.getIndividualsControlLimits).get({
      chartName: chartName,
      sigma: 3
    }) as {
      mean: number,
      transformedMean: number,
      upperControlLimit: number,
      transformedUpperControlLimit: number,
      lowerControlLimit: number,
      transformedLowerControlLimit: number
    }
    return {
      mean: {value: result.mean, transformedValue: result.transformedMean},
      upperControlLimit: {value: result.upperControlLimit, transformedValue: result.transformedUpperControlLimit},
      lowerControlLimit: {value: result.lowerControlLimit, transformedValue: result.transformedLowerControlLimit}
    }
  }
  
  getChart(chartName: string, startTime?: number, endTime?: number): ChartData {
    const parameters = this.getChartParameters(chartName);
    const setup = this.getChartSetup(chartName);
    const limits = setup && this.getControlLimits(
      chartName,
    );
    const dataStartTime = startTime ? startTime : this.getEarliestDataTime(parameters.dataName);
    const dataEndTime = endTime ? endTime : this.getLatestDataTime(parameters.dataName);
    const points = this.getChartPoints(parameters.dataName, dataStartTime, dataEndTime);
    
    const values = points.map((point) => point.value);
    const observations: Observation[] = points.map((point): Observation => ({
      id: point.id,
      individualsValue: {value: point.value, transformedValue: point.transformedValue},
      cusumValue: {value: point.cusum, transformedValue: point.transformedCusum},
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
    return this.getAvailableChartsQuery.all().map((row: any) => row.chartName);
  }

}
