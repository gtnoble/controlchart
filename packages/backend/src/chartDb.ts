import assert from 'node:assert/strict';

import Database from 'better-sqlite3';

import * as stats from './stats.js';
import type { ChartData } from './types/chart.d.ts';

export type ChartType = "individuals" | "counts";

export type TransformationName = "log" | undefined;
export type TransformationType = (value: number) => number;
export type TransformationPair = {forward: TransformationType, reverse: TransformationType};

const LOG_TRANSFORMATION_PAIR: TransformationPair = {
  forward: (value) => Math.log(value),
  reverse: (value) => Math.exp(value)
};

const NULL_TRANSFORMATION: TransformationPair = {
  forward: (value) => value,
  reverse: (value) => value
};

interface ChartParametersSchema {
  chartType: string;
  dataName: string;
  aggregationInterval: number;
}

interface ChartPointSchema {
  time: number;
  value: number;
}

interface ChartSetupSchema {
  setupStartTime: number, 
  setupEndTime: number, 
  creationTime: number
}
  
export class ChartDb {

  database: Database.Database;
  insertObservationQuery: Database.Statement;
  initializeChartQuery: Database.Statement;
  insertSetupQuery: Database.Statement;
  getSetupQuery: Database.Statement;
  getChartPointsQuery: Database.Statement;
  getCountsQuery: Database.Statement;
  getChartParametersQuery: Database.Statement;
  getAvailableChartsQuery: Database.Statement;
  getEarliestDataTimeQuery: Database.Statement;
  getLatestDataTimeQuery: Database.Statement;
  setTransformationQuery: Database.Statement;
  getTransformationQuery: Database.Statement;

  constructor(databaseName: string) {
    this.database = new Database(databaseName);
    this.database.pragma('journal_mode = WAL');
    this.database.pragma('foreign_keys = ON');

    this.database.prepare(
      `
    CREATE TABLE IF NOT EXISTS chart_data (
      id INTEGER PRIMARY KEY ASC AUTOINCREMENT,
      time INTEGER,
      value REAL,
      dataName STRING
    )`).run();

    this.database.prepare(
      `
      CREATE TABLE IF NOT EXISTS chart_annotations (
        id INTEGER PRIMARY KEY ASC AUTOINCREMENT,
        chart_data_id INTEGER REFERENCES chart_data(id),
        annotation TEXT,
        created_time INTEGER
      )`).run();
    
    this.getEarliestDataTimeQuery = this.database.prepare(
      `
      SELECT time 
      FROM chart_data
      WHERE dataName = :data_name
      ORDER BY time
      LIMIT 1;
      `
    );

    this.getLatestDataTimeQuery = this.database.prepare(
      `
      SELECT time 
      FROM chart_data
      WHERE dataName = :data_name
      ORDER BY time DESC
      LIMIT 1;
      `
    );
    
    this.database.prepare(
      `
      CREATE INDEX IF NOT EXISTS chart_data_lookup ON chart_data (time, value, dataName);
      `
    ).run();

    this.insertObservationQuery = this.database.prepare(
      `INSERT INTO chart_data 
        (time, value, dataName) 
        VALUES (?, ?, ?);`);
        
    this.database.prepare(
      `
      CREATE TABLE IF NOT EXISTS chart (
        chartName STRING PRIMARY KEY,
        chartType STRING,
        dataName STRING,
        aggregationInterval INTEGER
      );`).run()
      
    this.database.prepare(
      `
      CREATE TABLE IF NOT EXISTS setup (
        id INTEGER PRIMARY KEY ASC AUTOINCREMENT,
        chartName STRING REFERENCES chart (chartName),
        setupStartTime INTEGER,
        setupEndTime INTEGER,
        creationTime INTEGER
      )
      `
    ).run()
    
    this.database.prepare(
      `
      CREATE TABLE IF NOT EXISTS transformations (
        chartName STRING REFERENCES chart (chartName),
        transformation STRING
      );
      `
    ).run();
    
    this.setTransformationQuery = this.database.prepare(
      `
      INSERT OR REPLACE INTO transformations (
        chartName,
        transformation
      )
      VALUES (
        :chartName,
        :transformation
      );
      `
    )
    
    this.getTransformationQuery = this.database.prepare(
      `
      SELECT transformation 
      FROM transformations
      WHERE chartName = :chartName
      LIMIT 1;
      `
    )
    
    this.insertSetupQuery = this.database.prepare(
      `
      INSERT INTO setup (
        chartName,
        setupStartTime,
        setupEndTime,
        creationTime
      )
      VALUES (
        :chartName, 
        :setupStartTime, 
        :setupEndTime, 
        :creationTime
      );
      `
    )
    
    this.getSetupQuery = this.database.prepare(
      `
      SELECT setupStartTime, setupEndTime, creationTime FROM setup
      WHERE chartName = :chartName
      ORDER BY id DESC
      LIMIT 1;
      `
    )

    this.initializeChartQuery = this.database.prepare(
      `
    INSERT OR REPLACE INTO chart (
      chartName,
      chartType,
      dataName,
      aggregationInterval
    )
    VALUES (
      :chartName, 
      :chartType, 
      :dataName, 
      :aggregationInterval
    );`);
    
    this.getChartParametersQuery = this.database.prepare(
      `SELECT 
        dataName, chartType, aggregationInterval 
        FROM chart 
        WHERE chartName = ? 
        LIMIT 1;`
    )
    
    this.getAvailableChartsQuery = this.database.prepare(
      `SELECT DISTINCT chartName FROM chart ORDER BY chartName`
    );
    
    this.getChartPointsQuery = this.database.prepare(
      `
      SELECT time, value 
      FROM chart_data 
      WHERE dataName = ? AND time BETWEEN ? AND ?
      ORDER BY time`
    )
    
    this.getCountsQuery = this.database.prepare(
      `
      WITH time_blocks AS (
    SELECT 
        FLOOR((time - :start_time) / :block_size) AS block_id,
        MIN(time) AS block_start,
        COUNT(*) AS record_count
    FROM chart_data
    WHERE time BETWEEN :start_time AND :end_time
    AND dataName = :data_name
    GROUP BY block_id
    HAVING block_start + :block_size <= :end_time
)
SELECT block_id, block_start, record_count FROM time_blocks;
      `
    )

  }

  addObservation(value: number, dataName: string): number {
    const unixTime = Date.now();
    this.insertObservationQuery.run(unixTime, value, dataName);
    return (this.database.prepare("SELECT last_insert_rowid() AS id").get() as { id: number }).id;
  }

  addAnnotation(chartDataId: number, annotation: string) {
    const unixTime = Date.now();
    this.database.prepare(
      `INSERT INTO chart_annotations 
        (chart_data_id, annotation, created_time) 
        VALUES (?, ?, ?)`
    ).run(chartDataId, annotation, unixTime);
  }

  getAnnotations(chartDataId: number): string[] {
    const results = this.database.prepare<{ chart_data_id: number }>(
      `SELECT annotation FROM chart_annotations 
       WHERE chart_data_id = ? 
       ORDER BY created_time`
    ).all({ chart_data_id: chartDataId }) as Array<{ annotation: string }>;
    return results.map(r => r.annotation);
  }
  
  getLatestDataTime(dataName: string) {
    return  (this.getLatestDataTimeQuery.get({data_name: dataName}) as {time: number}).time;
  }

  getEarliestDataTime(dataName: string) {
    return (this.getEarliestDataTimeQuery.get({data_name: dataName}) as {time: number}).time;
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
      {chartName: chartName, transformation: transformation}
    );
  }
  
  getTransformation(
    chartName: string
  ): TransformationPair {
    const transformationRow: any = this.getTransformationQuery.get(
      {chartName: chartName}
    )
    
    const transformation = transformationRow?.transformation;
    if (! transformation ) {
      return NULL_TRANSFORMATION;
    }
    else {
      assert( transformation === "log");
      return LOG_TRANSFORMATION_PAIR
    }
  }
  
  getChartSetup(
    chartName: string
  ) {
    return this.getSetupQuery.get({chartName: chartName}) as ChartSetupSchema | undefined;
  }
  
  getChartParameters(chartName: string): ChartParametersSchema {
    return this.getChartParametersQuery.get(chartName) as ChartParametersSchema;
  }
  
  getChartPoints(dataName: string, startTime: number, endTime: number): ChartPointSchema[] {
    return this.getChartPointsQuery.all(
      dataName, 
      startTime, 
      endTime
    ) as ChartPointSchema[];
  }
  
  getChartCounts(dataName: string, startTime: number, endTime: number, aggregationInterval: number) {
    const blockCounts: any[] = this.getCountsQuery.all(
      {
        data_name: dataName, 
        start_time: startTime, 
        end_time: endTime, 
        block_size: aggregationInterval
      });
    
    const counts: ChartPointSchema[] = [];
    
    for (const blockCount of blockCounts) {
      const index = blockCount.block_id;
      counts[index] = {time: blockCount.block_start, value: blockCount.record_count}
    }
    for (let ii = 0; ii < counts.length; ii++) {
      if (! counts[ii]) {
        counts[ii] = {time: startTime + ii * aggregationInterval, value: 0};
      }
    }
    return counts;
  }
  
  getControlLimits (
    chartParameters: ChartParametersSchema, 
    chartSetup: ChartSetupSchema,
    chartTransformation: TransformationPair = NULL_TRANSFORMATION
  ): stats.ControlLimitsType {
    
    if (chartParameters.chartType === "individuals") {
      const points = this.getChartPoints(
        chartParameters.dataName, 
        chartSetup.setupStartTime, 
        chartSetup.setupEndTime
      );
      const transformedValues = points.map((point) => chartTransformation.forward(point.value))
      const transformedStats = stats.individualsChartSetupParams(transformedValues);
      return {
        mean: chartTransformation.reverse(transformedStats.mean),
        upperControlLimit: chartTransformation.reverse(transformedStats.upperControlLimit),
        lowerControlLimit: chartTransformation.reverse(transformedStats.lowerControlLimit)
      };
    }
    else if (chartParameters.chartType === "counts") {
      const counts = this.getChartCounts(
        chartParameters.dataName,
        chartSetup.setupStartTime,
        chartSetup.setupEndTime,
        chartParameters.aggregationInterval
      );
      const values = counts.map((count) => count.value);
      return stats.countsChartSetupParams(values);
    }
    else {
      throw new Error(`Invalid chart type: ${chartParameters.chartType}`);
    }
    
  }
  
  getChart (chartName: string, startTime?: number, endTime?: number): ChartData {
    const parameters = this.getChartParameters(chartName);
    const setup = this.getChartSetup(chartName);
    const transformations = this.getTransformation(chartName);
    const limits = setup && this.getControlLimits(parameters, setup, transformations);
    let points: ChartPointSchema[] = [];
    const dataStartTime = startTime ? startTime : this.getEarliestDataTime(parameters.dataName);
    const dataEndTime = endTime ? endTime : this.getLatestDataTime(parameters.dataName);
    if (parameters.chartType === "individuals") {
      points = this.getChartPoints(parameters.dataName, dataStartTime, dataEndTime);
    }
    else if (parameters.chartType === "counts") {
      points = this.getChartCounts(
        parameters.dataName, 
        dataStartTime, 
        dataEndTime,
        parameters.aggregationInterval
      );
    }
    else {
      throw new Error(`Invalid chart type: ${parameters.chartType}`);
    }
    
    const observations = points.map((point) => {
      return {
        ...point, 
        isSetup: setup !== undefined && point.time >= setup.setupStartTime && point.time <= setup.setupEndTime
      }
    })
    
    return {
      type: parameters.chartType,
      controlLimits: limits,
      observations: observations
    };
  }
  
  getAvailableCharts () {
    return this.getAvailableChartsQuery.all().map((row: any) => row.chartName);
  }

}
