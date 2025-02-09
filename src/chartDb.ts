import Database from 'better-sqlite3';

import * as stats from './stats.js';

export type ChartType = "individuals" | "counts";

export interface Chart {
  type: ChartType;
  upperControlLimit: number;
  lowerControlLimit: number;
  mean: number;
  observations: {value: number, time: number, isSetup: boolean}[];
}

interface ChartParametersSchema {
  chartType: string;
  dataName: string;
  setupStartTime: number;
  setupEndTime: number;
  aggregationInterval: number;
}

interface ChartPointSchema {
  time: number;
  value: number;
}
  
export class ChartDb {

  database: Database.Database;
  insertObservationQuery: Database.Statement;
  initializeChartQuery: Database.Statement;
  getChartPointsQuery: Database.Statement;
  getCountsQuery: Database.Statement;
  getChartParametersQuery: Database.Statement;

  constructor(databaseName: string) {
    this.database = new Database(databaseName);
    this.database.pragma('journal_mode = WAL');
    this.database.pragma('foreign_keys = ON');

    this.database.prepare(
      `
    CREATE TABLE IF NOT EXISTS chart_data (
      time INTEGER,
      value REAL,
      dataName STRING
    )`).run();

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
        setupStartTime INTEGER,
        setupEndTime INTEGER,
        aggregationInterval INTEGER
      );`).run()

    this.initializeChartQuery = this.database.prepare(
      `
    INSERT OR REPLACE INTO chart (
      chartName,
      chartType,
      dataName,
      setupStartTime,
      setupEndTime,
      aggregationInterval
    )
    VALUES (
      :chartName, 
      :chartType, 
      :dataName, 
      :setupStartTime, 
      :setupEndTime, 
      :aggregationInterval
    );`);
    
    this.getChartParametersQuery = this.database.prepare(
      `SELECT 
        dataName, chartType, setupStartTime, setupEndTime, aggregationInterval 
        FROM chart 
        WHERE chartName = ? 
        LIMIT 1;`
    )
    
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
            ((time - :start_time) / :block_size) AS block_id,
            MIN(time) AS time,
            COUNT(*) AS value
        FROM chart_data
        WHERE time BETWEEN :start_time AND :end_time
        AND dataName = ':dataName'
        GROUP BY block_id
        HAVING time + :block_size <= :end_time
      )
      SELECT time, value FROM time_blocks;
      `
    )

  }

  addObservation(value: number, dataName: string) {
    const unixTime = Date.now();
    this.insertObservationQuery.run(unixTime, value, dataName);
  }

  initializeChart(
    chartName: string,
    dataName: string,
    chartType: ChartType,
    setupStartTime: Date,
    setupEndTime: Date,
    aggregationInterval: number
  ) {

    this.initializeChartQuery.run(
      {
        chartName: chartName,
        chartType: chartType,
        dataName: dataName,
        setupStartTime: setupStartTime.valueOf(),
        setupEndTime: setupEndTime.valueOf(),
        aggregationInterval: aggregationInterval
      });
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
    return this.getCountsQuery.all(
      {
        dataName: dataName, 
        start_time: startTime, 
        end_time: endTime, 
        block_size: aggregationInterval
      }) as ChartPointSchema[];
  }
  
  getControlLimits (chartParameters: ChartParametersSchema) {
    
    if (chartParameters.chartType === "individuals") {
      const points = this.getChartPoints(
        chartParameters.dataName, 
        chartParameters.setupStartTime, 
        chartParameters.setupEndTime
      );
      const values = points.map((point) => point.value)
      return stats.individualsChartSetupParams(values);
    }
    else if (chartParameters.chartType === "counts") {
      const counts = this.getChartCounts(
        chartParameters.dataName,
        chartParameters.setupStartTime,
        chartParameters.setupEndTime,
        chartParameters.aggregationInterval
      );
      const values = counts.map((count) => count.value);
      return stats.countsChartSetupParams(values);
    }
    else {
      throw new Error(`Invalid chart type: ${chartParameters.chartType}`);
    }
    
  }
  
  getChart (chartName: string, startTime: number, endTime: number): Chart {
    const parameters = this.getChartParameters(chartName);
    const limits = this.getControlLimits(parameters);
    let points: ChartPointSchema[] = [];
    if (parameters.chartType === "individuals") {
      points = this.getChartPoints(parameters.dataName, startTime, endTime);
    }
    else if (parameters.chartType === "counts") {
      points = this.getChartCounts(
        parameters.dataName, 
        parameters.setupStartTime, 
        parameters.setupEndTime,
        parameters.aggregationInterval
      );
    }
    else {
      throw new Error(`Invalid chart type: ${parameters.chartType}`);
    }
    
    const observations = points.map((point) => {
      return {
        ...point, 
        isSetup: point.time >= parameters.setupStartTime && point.time <= parameters.setupEndTime
      }
    })
    
    return {
      type: parameters.chartType,
      ...limits,
      observations: observations
    };
  }
  
  

}