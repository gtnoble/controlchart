import assert from 'node:assert/strict';
import Database from 'better-sqlite3';
import * as stats from './stats.js';
const LOG_TRANSFORMATION_PAIR = {
    forward: (value) => Math.log(value),
    reverse: (value) => Math.exp(value)
};
const NULL_TRANSFORMATION = {
    forward: (value) => value,
    reverse: (value) => value
};
export class ChartDb {
    database;
    insertObservationQuery;
    initializeChartQuery;
    insertSetupQuery;
    getSetupQuery;
    getChartPointsQuery;
    getCountsQuery;
    getChartParametersQuery;
    getAvailableChartsQuery;
    getEarliestDataTimeQuery;
    getLatestDataTimeQuery;
    setTransformationQuery;
    getTransformationQuery;
    constructor(databaseName) {
        this.database = new Database(databaseName);
        this.database.pragma('journal_mode = WAL');
        this.database.pragma('foreign_keys = ON');
        this.database.prepare(`
    CREATE TABLE IF NOT EXISTS chart_data (
      id INTEGER PRIMARY KEY ASC AUTOINCREMENT,
      time INTEGER,
      value REAL,
      dataName STRING
    )`).run();
        this.database.prepare(`
      CREATE TABLE IF NOT EXISTS chart_annotations (
        id INTEGER PRIMARY KEY ASC AUTOINCREMENT,
        chart_data_id INTEGER REFERENCES chart_data(id),
        annotation TEXT,
        created_time INTEGER
      )`).run();
        this.getEarliestDataTimeQuery = this.database.prepare(`
      SELECT time 
      FROM chart_data
      WHERE dataName = :data_name
      ORDER BY time
      LIMIT 1;
      `);
        this.getLatestDataTimeQuery = this.database.prepare(`
      SELECT time 
      FROM chart_data
      WHERE dataName = :data_name
      ORDER BY time DESC
      LIMIT 1;
      `);
        this.database.prepare(`
      CREATE INDEX IF NOT EXISTS chart_data_lookup ON chart_data (time, value, dataName);
      `).run();
        this.insertObservationQuery = this.database.prepare(`INSERT INTO chart_data 
        (time, value, dataName) 
        VALUES (?, ?, ?);`);
        this.database.prepare(`
      CREATE TABLE IF NOT EXISTS chart (
        chartName STRING PRIMARY KEY,
        chartType STRING,
        dataName STRING,
        aggregationInterval INTEGER
      );`).run();
        this.database.prepare(`
      CREATE TABLE IF NOT EXISTS setup (
        id INTEGER PRIMARY KEY ASC AUTOINCREMENT,
        chartName STRING REFERENCES chart (chartName),
        setupStartTime INTEGER,
        setupEndTime INTEGER,
        creationTime INTEGER
      )
      `).run();
        this.database.prepare(`
      CREATE TABLE IF NOT EXISTS transformations (
        chartName STRING REFERENCES chart (chartName),
        transformation STRING
      );
      `).run();
        this.setTransformationQuery = this.database.prepare(`
      INSERT OR REPLACE INTO transformations (
        chartName,
        transformation
      )
      VALUES (
        :chartName,
        :transformation
      );
      `);
        this.getTransformationQuery = this.database.prepare(`
      SELECT transformation 
      FROM transformations
      WHERE chartName = :chartName
      LIMIT 1;
      `);
        this.insertSetupQuery = this.database.prepare(`
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
      `);
        this.getSetupQuery = this.database.prepare(`
      SELECT setupStartTime, setupEndTime, creationTime FROM setup
      WHERE chartName = :chartName
      ORDER BY id DESC
      LIMIT 1;
      `);
        this.initializeChartQuery = this.database.prepare(`
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
        this.getChartParametersQuery = this.database.prepare(`SELECT 
        dataName, chartType, aggregationInterval 
        FROM chart 
        WHERE chartName = ? 
        LIMIT 1;`);
        this.getAvailableChartsQuery = this.database.prepare(`SELECT DISTINCT chartName FROM chart ORDER BY chartName`);
        this.getChartPointsQuery = this.database.prepare(`
      SELECT time, value 
      FROM chart_data 
      WHERE dataName = ? AND time BETWEEN ? AND ?
      ORDER BY time`);
        this.getCountsQuery = this.database.prepare(`
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
      `);
    }
    addObservation(value, dataName) {
        const unixTime = Date.now();
        this.insertObservationQuery.run(unixTime, value, dataName);
        return this.database.prepare("SELECT last_insert_rowid() AS id").get().id;
    }
    addAnnotation(chartDataId, annotation) {
        const unixTime = Date.now();
        this.database.prepare(`INSERT INTO chart_annotations 
        (chart_data_id, annotation, created_time) 
        VALUES (?, ?, ?)`).run(chartDataId, annotation, unixTime);
    }
    getAnnotations(chartDataId) {
        const results = this.database.prepare(`SELECT annotation FROM chart_annotations 
       WHERE chart_data_id = ? 
       ORDER BY created_time`).all({ chart_data_id: chartDataId });
        return results.map(r => r.annotation);
    }
    getLatestDataTime(dataName) {
        return this.getLatestDataTimeQuery.get({ data_name: dataName }).time;
    }
    getEarliestDataTime(dataName) {
        return this.getEarliestDataTimeQuery.get({ data_name: dataName }).time;
    }
    getChartDataLimits(chart) {
        return [this.getEarliestDataTime(chart.dataName), this.getLatestDataTime(chart.dataName)];
    }
    initializeChart(chartName, dataName, chartType, aggregationInterval) {
        this.initializeChartQuery.run({
            chartName: chartName,
            chartType: chartType,
            dataName: dataName,
            aggregationInterval: aggregationInterval
        });
    }
    addChartSetup(chartName, setupStartTime, setupEndTime) {
        this.insertSetupQuery.run({
            chartName: chartName,
            setupStartTime: setupStartTime.valueOf(),
            setupEndTime: setupEndTime.valueOf(),
            creationTime: (new Date()).valueOf()
        });
    }
    setTransformation(chartName, transformation) {
        this.setTransformationQuery.run({ chartName: chartName, transformation: transformation });
    }
    getTransformation(chartName) {
        const transformationRow = this.getTransformationQuery.get({ chartName: chartName });
        const transformation = transformationRow?.transformation;
        if (!transformation) {
            return NULL_TRANSFORMATION;
        }
        else {
            assert(transformation === "log");
            return LOG_TRANSFORMATION_PAIR;
        }
    }
    getChartSetup(chartName) {
        return this.getSetupQuery.get({ chartName: chartName });
    }
    getChartParameters(chartName) {
        return this.getChartParametersQuery.get(chartName);
    }
    getChartPoints(dataName, startTime, endTime) {
        return this.getChartPointsQuery.all(dataName, startTime, endTime);
    }
    getChartCounts(dataName, startTime, endTime, aggregationInterval) {
        const blockCounts = this.getCountsQuery.all({
            data_name: dataName,
            start_time: startTime,
            end_time: endTime,
            block_size: aggregationInterval
        });
        const counts = [];
        for (const blockCount of blockCounts) {
            const index = blockCount.block_id;
            counts[index] = { time: blockCount.block_start, value: blockCount.record_count };
        }
        for (let ii = 0; ii < counts.length; ii++) {
            if (!counts[ii]) {
                counts[ii] = { time: startTime + ii * aggregationInterval, value: 0 };
            }
        }
        return counts;
    }
    getControlLimits(chartParameters, chartSetup, chartTransformation = NULL_TRANSFORMATION) {
        if (chartParameters.chartType === "individuals") {
            const points = this.getChartPoints(chartParameters.dataName, chartSetup.setupStartTime, chartSetup.setupEndTime);
            const transformedValues = points.map((point) => chartTransformation.forward(point.value));
            const transformedStats = stats.individualsChartSetupParams(transformedValues);
            return {
                mean: chartTransformation.reverse(transformedStats.mean),
                upperControlLimit: chartTransformation.reverse(transformedStats.upperControlLimit),
                lowerControlLimit: chartTransformation.reverse(transformedStats.lowerControlLimit)
            };
        }
        else if (chartParameters.chartType === "counts") {
            const counts = this.getChartCounts(chartParameters.dataName, chartSetup.setupStartTime, chartSetup.setupEndTime, chartParameters.aggregationInterval);
            const values = counts.map((count) => count.value);
            return stats.countsChartSetupParams(values);
        }
        else {
            throw new Error(`Invalid chart type: ${chartParameters.chartType}`);
        }
    }
    getChart(chartName, startTime, endTime) {
        const parameters = this.getChartParameters(chartName);
        const setup = this.getChartSetup(chartName);
        const transformations = this.getTransformation(chartName);
        const limits = setup && this.getControlLimits(parameters, setup, transformations);
        let points = [];
        const dataStartTime = startTime ? startTime : this.getEarliestDataTime(parameters.dataName);
        const dataEndTime = endTime ? endTime : this.getLatestDataTime(parameters.dataName);
        if (parameters.chartType === "individuals") {
            points = this.getChartPoints(parameters.dataName, dataStartTime, dataEndTime);
        }
        else if (parameters.chartType === "counts") {
            points = this.getChartCounts(parameters.dataName, dataStartTime, dataEndTime, parameters.aggregationInterval);
        }
        else {
            throw new Error(`Invalid chart type: ${parameters.chartType}`);
        }
        const observations = points.map((point) => {
            return {
                ...point,
                isSetup: setup !== undefined && point.time >= setup.setupStartTime && point.time <= setup.setupEndTime
            };
        });
        return {
            type: parameters.chartType,
            controlLimits: limits,
            observations: observations
        };
    }
    getAvailableCharts() {
        return this.getAvailableChartsQuery.all().map((row) => row.chartName);
    }
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY2hhcnREYi5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbImNoYXJ0RGIudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUEsT0FBTyxNQUFNLE1BQU0sb0JBQW9CLENBQUM7QUFFeEMsT0FBTyxRQUFRLE1BQU0sZ0JBQWdCLENBQUM7QUFFdEMsT0FBTyxLQUFLLEtBQUssTUFBTSxZQUFZLENBQUM7QUFRcEMsTUFBTSx1QkFBdUIsR0FBdUI7SUFDbEQsT0FBTyxFQUFFLENBQUMsS0FBSyxFQUFFLEVBQUUsQ0FBQyxJQUFJLENBQUMsR0FBRyxDQUFDLEtBQUssQ0FBQztJQUNuQyxPQUFPLEVBQUUsQ0FBQyxLQUFLLEVBQUUsRUFBRSxDQUFDLElBQUksQ0FBQyxHQUFHLENBQUMsS0FBSyxDQUFDO0NBQ3BDLENBQUM7QUFFRixNQUFNLG1CQUFtQixHQUF1QjtJQUM5QyxPQUFPLEVBQUUsQ0FBQyxLQUFLLEVBQUUsRUFBRSxDQUFDLEtBQUs7SUFDekIsT0FBTyxFQUFFLENBQUMsS0FBSyxFQUFFLEVBQUUsQ0FBQyxLQUFLO0NBQzFCLENBQUM7QUF3QkYsTUFBTSxPQUFPLE9BQU87SUFFbEIsUUFBUSxDQUFvQjtJQUM1QixzQkFBc0IsQ0FBcUI7SUFDM0Msb0JBQW9CLENBQXFCO0lBQ3pDLGdCQUFnQixDQUFxQjtJQUNyQyxhQUFhLENBQXFCO0lBQ2xDLG1CQUFtQixDQUFxQjtJQUN4QyxjQUFjLENBQXFCO0lBQ25DLHVCQUF1QixDQUFxQjtJQUM1Qyx1QkFBdUIsQ0FBcUI7SUFDNUMsd0JBQXdCLENBQXFCO0lBQzdDLHNCQUFzQixDQUFxQjtJQUMzQyxzQkFBc0IsQ0FBcUI7SUFDM0Msc0JBQXNCLENBQXFCO0lBRTNDLFlBQVksWUFBb0I7UUFDOUIsSUFBSSxDQUFDLFFBQVEsR0FBRyxJQUFJLFFBQVEsQ0FBQyxZQUFZLENBQUMsQ0FBQztRQUMzQyxJQUFJLENBQUMsUUFBUSxDQUFDLE1BQU0sQ0FBQyxvQkFBb0IsQ0FBQyxDQUFDO1FBQzNDLElBQUksQ0FBQyxRQUFRLENBQUMsTUFBTSxDQUFDLG1CQUFtQixDQUFDLENBQUM7UUFFMUMsSUFBSSxDQUFDLFFBQVEsQ0FBQyxPQUFPLENBQ25COzs7Ozs7TUFNQSxDQUFDLENBQUMsR0FBRyxFQUFFLENBQUM7UUFFVixJQUFJLENBQUMsUUFBUSxDQUFDLE9BQU8sQ0FDbkI7Ozs7OztRQU1FLENBQUMsQ0FBQyxHQUFHLEVBQUUsQ0FBQztRQUVaLElBQUksQ0FBQyx3QkFBd0IsR0FBRyxJQUFJLENBQUMsUUFBUSxDQUFDLE9BQU8sQ0FDbkQ7Ozs7OztPQU1DLENBQ0YsQ0FBQztRQUVGLElBQUksQ0FBQyxzQkFBc0IsR0FBRyxJQUFJLENBQUMsUUFBUSxDQUFDLE9BQU8sQ0FDakQ7Ozs7OztPQU1DLENBQ0YsQ0FBQztRQUVGLElBQUksQ0FBQyxRQUFRLENBQUMsT0FBTyxDQUNuQjs7T0FFQyxDQUNGLENBQUMsR0FBRyxFQUFFLENBQUM7UUFFUixJQUFJLENBQUMsc0JBQXNCLEdBQUcsSUFBSSxDQUFDLFFBQVEsQ0FBQyxPQUFPLENBQ2pEOzswQkFFb0IsQ0FBQyxDQUFDO1FBRXhCLElBQUksQ0FBQyxRQUFRLENBQUMsT0FBTyxDQUNuQjs7Ozs7O1NBTUcsQ0FBQyxDQUFDLEdBQUcsRUFBRSxDQUFBO1FBRVosSUFBSSxDQUFDLFFBQVEsQ0FBQyxPQUFPLENBQ25COzs7Ozs7OztPQVFDLENBQ0YsQ0FBQyxHQUFHLEVBQUUsQ0FBQTtRQUVQLElBQUksQ0FBQyxRQUFRLENBQUMsT0FBTyxDQUNuQjs7Ozs7T0FLQyxDQUNGLENBQUMsR0FBRyxFQUFFLENBQUM7UUFFUixJQUFJLENBQUMsc0JBQXNCLEdBQUcsSUFBSSxDQUFDLFFBQVEsQ0FBQyxPQUFPLENBQ2pEOzs7Ozs7Ozs7T0FTQyxDQUNGLENBQUE7UUFFRCxJQUFJLENBQUMsc0JBQXNCLEdBQUcsSUFBSSxDQUFDLFFBQVEsQ0FBQyxPQUFPLENBQ2pEOzs7OztPQUtDLENBQ0YsQ0FBQTtRQUVELElBQUksQ0FBQyxnQkFBZ0IsR0FBRyxJQUFJLENBQUMsUUFBUSxDQUFDLE9BQU8sQ0FDM0M7Ozs7Ozs7Ozs7Ozs7T0FhQyxDQUNGLENBQUE7UUFFRCxJQUFJLENBQUMsYUFBYSxHQUFHLElBQUksQ0FBQyxRQUFRLENBQUMsT0FBTyxDQUN4Qzs7Ozs7T0FLQyxDQUNGLENBQUE7UUFFRCxJQUFJLENBQUMsb0JBQW9CLEdBQUcsSUFBSSxDQUFDLFFBQVEsQ0FBQyxPQUFPLENBQy9DOzs7Ozs7Ozs7Ozs7T0FZQyxDQUFDLENBQUM7UUFFTCxJQUFJLENBQUMsdUJBQXVCLEdBQUcsSUFBSSxDQUFDLFFBQVEsQ0FBQyxPQUFPLENBQ2xEOzs7O2lCQUlXLENBQ1osQ0FBQTtRQUVELElBQUksQ0FBQyx1QkFBdUIsR0FBRyxJQUFJLENBQUMsUUFBUSxDQUFDLE9BQU8sQ0FDbEQseURBQXlELENBQzFELENBQUM7UUFFRixJQUFJLENBQUMsbUJBQW1CLEdBQUcsSUFBSSxDQUFDLFFBQVEsQ0FBQyxPQUFPLENBQzlDOzs7O29CQUljLENBQ2YsQ0FBQTtRQUVELElBQUksQ0FBQyxjQUFjLEdBQUcsSUFBSSxDQUFDLFFBQVEsQ0FBQyxPQUFPLENBQ3pDOzs7Ozs7Ozs7Ozs7O09BYUMsQ0FDRixDQUFBO0lBRUgsQ0FBQztJQUVELGNBQWMsQ0FBQyxLQUFhLEVBQUUsUUFBZ0I7UUFDNUMsTUFBTSxRQUFRLEdBQUcsSUFBSSxDQUFDLEdBQUcsRUFBRSxDQUFDO1FBQzVCLElBQUksQ0FBQyxzQkFBc0IsQ0FBQyxHQUFHLENBQUMsUUFBUSxFQUFFLEtBQUssRUFBRSxRQUFRLENBQUMsQ0FBQztRQUMzRCxPQUFRLElBQUksQ0FBQyxRQUFRLENBQUMsT0FBTyxDQUFDLGtDQUFrQyxDQUFDLENBQUMsR0FBRyxFQUFxQixDQUFDLEVBQUUsQ0FBQztJQUNoRyxDQUFDO0lBRUQsYUFBYSxDQUFDLFdBQW1CLEVBQUUsVUFBa0I7UUFDbkQsTUFBTSxRQUFRLEdBQUcsSUFBSSxDQUFDLEdBQUcsRUFBRSxDQUFDO1FBQzVCLElBQUksQ0FBQyxRQUFRLENBQUMsT0FBTyxDQUNuQjs7eUJBRW1CLENBQ3BCLENBQUMsR0FBRyxDQUFDLFdBQVcsRUFBRSxVQUFVLEVBQUUsUUFBUSxDQUFDLENBQUM7SUFDM0MsQ0FBQztJQUVELGNBQWMsQ0FBQyxXQUFtQjtRQUNoQyxNQUFNLE9BQU8sR0FBRyxJQUFJLENBQUMsUUFBUSxDQUFDLE9BQU8sQ0FDbkM7OzZCQUV1QixDQUN4QixDQUFDLEdBQUcsQ0FBQyxFQUFFLGFBQWEsRUFBRSxXQUFXLEVBQUUsQ0FBa0MsQ0FBQztRQUN2RSxPQUFPLE9BQU8sQ0FBQyxHQUFHLENBQUMsQ0FBQyxDQUFDLEVBQUUsQ0FBQyxDQUFDLENBQUMsVUFBVSxDQUFDLENBQUM7SUFDeEMsQ0FBQztJQUVELGlCQUFpQixDQUFDLFFBQWdCO1FBQ2hDLE9BQVMsSUFBSSxDQUFDLHNCQUFzQixDQUFDLEdBQUcsQ0FBQyxFQUFDLFNBQVMsRUFBRSxRQUFRLEVBQUMsQ0FBb0IsQ0FBQyxJQUFJLENBQUM7SUFDMUYsQ0FBQztJQUVELG1CQUFtQixDQUFDLFFBQWdCO1FBQ2xDLE9BQVEsSUFBSSxDQUFDLHdCQUF3QixDQUFDLEdBQUcsQ0FBQyxFQUFDLFNBQVMsRUFBRSxRQUFRLEVBQUMsQ0FBb0IsQ0FBQyxJQUFJLENBQUM7SUFDM0YsQ0FBQztJQUVELGtCQUFrQixDQUFDLEtBQTRCO1FBQzdDLE9BQU8sQ0FBQyxJQUFJLENBQUMsbUJBQW1CLENBQUMsS0FBSyxDQUFDLFFBQVEsQ0FBQyxFQUFFLElBQUksQ0FBQyxpQkFBaUIsQ0FBQyxLQUFLLENBQUMsUUFBUSxDQUFDLENBQUMsQ0FBQztJQUM1RixDQUFDO0lBRUQsZUFBZSxDQUNiLFNBQWlCLEVBQ2pCLFFBQWdCLEVBQ2hCLFNBQW9CLEVBQ3BCLG1CQUEyQjtRQUczQixJQUFJLENBQUMsb0JBQW9CLENBQUMsR0FBRyxDQUMzQjtZQUNFLFNBQVMsRUFBRSxTQUFTO1lBQ3BCLFNBQVMsRUFBRSxTQUFTO1lBQ3BCLFFBQVEsRUFBRSxRQUFRO1lBQ2xCLG1CQUFtQixFQUFFLG1CQUFtQjtTQUN6QyxDQUFDLENBQUM7SUFDUCxDQUFDO0lBRUQsYUFBYSxDQUNYLFNBQWlCLEVBQ2pCLGNBQW9CLEVBQ3BCLFlBQWtCO1FBRWxCLElBQUksQ0FBQyxnQkFBZ0IsQ0FBQyxHQUFHLENBQ3ZCO1lBQ0UsU0FBUyxFQUFFLFNBQVM7WUFDcEIsY0FBYyxFQUFFLGNBQWMsQ0FBQyxPQUFPLEVBQUU7WUFDeEMsWUFBWSxFQUFFLFlBQVksQ0FBQyxPQUFPLEVBQUU7WUFDcEMsWUFBWSxFQUFFLENBQUMsSUFBSSxJQUFJLEVBQUUsQ0FBQyxDQUFDLE9BQU8sRUFBRTtTQUNyQyxDQUNGLENBQUE7SUFDSCxDQUFDO0lBRUQsaUJBQWlCLENBQ2YsU0FBaUIsRUFDakIsY0FBa0M7UUFFbEMsSUFBSSxDQUFDLHNCQUFzQixDQUFDLEdBQUcsQ0FDN0IsRUFBQyxTQUFTLEVBQUUsU0FBUyxFQUFFLGNBQWMsRUFBRSxjQUFjLEVBQUMsQ0FDdkQsQ0FBQztJQUNKLENBQUM7SUFFRCxpQkFBaUIsQ0FDZixTQUFpQjtRQUVqQixNQUFNLGlCQUFpQixHQUFRLElBQUksQ0FBQyxzQkFBc0IsQ0FBQyxHQUFHLENBQzVELEVBQUMsU0FBUyxFQUFFLFNBQVMsRUFBQyxDQUN2QixDQUFBO1FBRUQsTUFBTSxjQUFjLEdBQUcsaUJBQWlCLEVBQUUsY0FBYyxDQUFDO1FBQ3pELElBQUksQ0FBRSxjQUFjLEVBQUcsQ0FBQztZQUN0QixPQUFPLG1CQUFtQixDQUFDO1FBQzdCLENBQUM7YUFDSSxDQUFDO1lBQ0osTUFBTSxDQUFFLGNBQWMsS0FBSyxLQUFLLENBQUMsQ0FBQztZQUNsQyxPQUFPLHVCQUF1QixDQUFBO1FBQ2hDLENBQUM7SUFDSCxDQUFDO0lBRUQsYUFBYSxDQUNYLFNBQWlCO1FBRWpCLE9BQU8sSUFBSSxDQUFDLGFBQWEsQ0FBQyxHQUFHLENBQUMsRUFBQyxTQUFTLEVBQUUsU0FBUyxFQUFDLENBQWlDLENBQUM7SUFDeEYsQ0FBQztJQUVELGtCQUFrQixDQUFDLFNBQWlCO1FBQ2xDLE9BQU8sSUFBSSxDQUFDLHVCQUF1QixDQUFDLEdBQUcsQ0FBQyxTQUFTLENBQTBCLENBQUM7SUFDOUUsQ0FBQztJQUVELGNBQWMsQ0FBQyxRQUFnQixFQUFFLFNBQWlCLEVBQUUsT0FBZTtRQUNqRSxPQUFPLElBQUksQ0FBQyxtQkFBbUIsQ0FBQyxHQUFHLENBQ2pDLFFBQVEsRUFDUixTQUFTLEVBQ1QsT0FBTyxDQUNjLENBQUM7SUFDMUIsQ0FBQztJQUVELGNBQWMsQ0FBQyxRQUFnQixFQUFFLFNBQWlCLEVBQUUsT0FBZSxFQUFFLG1CQUEyQjtRQUM5RixNQUFNLFdBQVcsR0FBVSxJQUFJLENBQUMsY0FBYyxDQUFDLEdBQUcsQ0FDaEQ7WUFDRSxTQUFTLEVBQUUsUUFBUTtZQUNuQixVQUFVLEVBQUUsU0FBUztZQUNyQixRQUFRLEVBQUUsT0FBTztZQUNqQixVQUFVLEVBQUUsbUJBQW1CO1NBQ2hDLENBQUMsQ0FBQztRQUVMLE1BQU0sTUFBTSxHQUF1QixFQUFFLENBQUM7UUFFdEMsS0FBSyxNQUFNLFVBQVUsSUFBSSxXQUFXLEVBQUUsQ0FBQztZQUNyQyxNQUFNLEtBQUssR0FBRyxVQUFVLENBQUMsUUFBUSxDQUFDO1lBQ2xDLE1BQU0sQ0FBQyxLQUFLLENBQUMsR0FBRyxFQUFDLElBQUksRUFBRSxVQUFVLENBQUMsV0FBVyxFQUFFLEtBQUssRUFBRSxVQUFVLENBQUMsWUFBWSxFQUFDLENBQUE7UUFDaEYsQ0FBQztRQUNELEtBQUssSUFBSSxFQUFFLEdBQUcsQ0FBQyxFQUFFLEVBQUUsR0FBRyxNQUFNLENBQUMsTUFBTSxFQUFFLEVBQUUsRUFBRSxFQUFFLENBQUM7WUFDMUMsSUFBSSxDQUFFLE1BQU0sQ0FBQyxFQUFFLENBQUMsRUFBRSxDQUFDO2dCQUNqQixNQUFNLENBQUMsRUFBRSxDQUFDLEdBQUcsRUFBQyxJQUFJLEVBQUUsU0FBUyxHQUFHLEVBQUUsR0FBRyxtQkFBbUIsRUFBRSxLQUFLLEVBQUUsQ0FBQyxFQUFDLENBQUM7WUFDdEUsQ0FBQztRQUNILENBQUM7UUFDRCxPQUFPLE1BQU0sQ0FBQztJQUNoQixDQUFDO0lBRUQsZ0JBQWdCLENBQ2QsZUFBc0MsRUFDdEMsVUFBNEIsRUFDNUIsc0JBQTBDLG1CQUFtQjtRQUc3RCxJQUFJLGVBQWUsQ0FBQyxTQUFTLEtBQUssYUFBYSxFQUFFLENBQUM7WUFDaEQsTUFBTSxNQUFNLEdBQUcsSUFBSSxDQUFDLGNBQWMsQ0FDaEMsZUFBZSxDQUFDLFFBQVEsRUFDeEIsVUFBVSxDQUFDLGNBQWMsRUFDekIsVUFBVSxDQUFDLFlBQVksQ0FDeEIsQ0FBQztZQUNGLE1BQU0saUJBQWlCLEdBQUcsTUFBTSxDQUFDLEdBQUcsQ0FBQyxDQUFDLEtBQUssRUFBRSxFQUFFLENBQUMsbUJBQW1CLENBQUMsT0FBTyxDQUFDLEtBQUssQ0FBQyxLQUFLLENBQUMsQ0FBQyxDQUFBO1lBQ3pGLE1BQU0sZ0JBQWdCLEdBQUcsS0FBSyxDQUFDLDJCQUEyQixDQUFDLGlCQUFpQixDQUFDLENBQUM7WUFDOUUsT0FBTztnQkFDTCxJQUFJLEVBQUUsbUJBQW1CLENBQUMsT0FBTyxDQUFDLGdCQUFnQixDQUFDLElBQUksQ0FBQztnQkFDeEQsaUJBQWlCLEVBQUUsbUJBQW1CLENBQUMsT0FBTyxDQUFDLGdCQUFnQixDQUFDLGlCQUFpQixDQUFDO2dCQUNsRixpQkFBaUIsRUFBRSxtQkFBbUIsQ0FBQyxPQUFPLENBQUMsZ0JBQWdCLENBQUMsaUJBQWlCLENBQUM7YUFDbkYsQ0FBQztRQUNKLENBQUM7YUFDSSxJQUFJLGVBQWUsQ0FBQyxTQUFTLEtBQUssUUFBUSxFQUFFLENBQUM7WUFDaEQsTUFBTSxNQUFNLEdBQUcsSUFBSSxDQUFDLGNBQWMsQ0FDaEMsZUFBZSxDQUFDLFFBQVEsRUFDeEIsVUFBVSxDQUFDLGNBQWMsRUFDekIsVUFBVSxDQUFDLFlBQVksRUFDdkIsZUFBZSxDQUFDLG1CQUFtQixDQUNwQyxDQUFDO1lBQ0YsTUFBTSxNQUFNLEdBQUcsTUFBTSxDQUFDLEdBQUcsQ0FBQyxDQUFDLEtBQUssRUFBRSxFQUFFLENBQUMsS0FBSyxDQUFDLEtBQUssQ0FBQyxDQUFDO1lBQ2xELE9BQU8sS0FBSyxDQUFDLHNCQUFzQixDQUFDLE1BQU0sQ0FBQyxDQUFDO1FBQzlDLENBQUM7YUFDSSxDQUFDO1lBQ0osTUFBTSxJQUFJLEtBQUssQ0FBQyx1QkFBdUIsZUFBZSxDQUFDLFNBQVMsRUFBRSxDQUFDLENBQUM7UUFDdEUsQ0FBQztJQUVILENBQUM7SUFFRCxRQUFRLENBQUUsU0FBaUIsRUFBRSxTQUFrQixFQUFFLE9BQWdCO1FBQy9ELE1BQU0sVUFBVSxHQUFHLElBQUksQ0FBQyxrQkFBa0IsQ0FBQyxTQUFTLENBQUMsQ0FBQztRQUN0RCxNQUFNLEtBQUssR0FBRyxJQUFJLENBQUMsYUFBYSxDQUFDLFNBQVMsQ0FBQyxDQUFDO1FBQzVDLE1BQU0sZUFBZSxHQUFHLElBQUksQ0FBQyxpQkFBaUIsQ0FBQyxTQUFTLENBQUMsQ0FBQztRQUMxRCxNQUFNLE1BQU0sR0FBRyxLQUFLLElBQUksSUFBSSxDQUFDLGdCQUFnQixDQUFDLFVBQVUsRUFBRSxLQUFLLEVBQUUsZUFBZSxDQUFDLENBQUM7UUFDbEYsSUFBSSxNQUFNLEdBQXVCLEVBQUUsQ0FBQztRQUNwQyxNQUFNLGFBQWEsR0FBRyxTQUFTLENBQUMsQ0FBQyxDQUFDLFNBQVMsQ0FBQyxDQUFDLENBQUMsSUFBSSxDQUFDLG1CQUFtQixDQUFDLFVBQVUsQ0FBQyxRQUFRLENBQUMsQ0FBQztRQUM1RixNQUFNLFdBQVcsR0FBRyxPQUFPLENBQUMsQ0FBQyxDQUFDLE9BQU8sQ0FBQyxDQUFDLENBQUMsSUFBSSxDQUFDLGlCQUFpQixDQUFDLFVBQVUsQ0FBQyxRQUFRLENBQUMsQ0FBQztRQUNwRixJQUFJLFVBQVUsQ0FBQyxTQUFTLEtBQUssYUFBYSxFQUFFLENBQUM7WUFDM0MsTUFBTSxHQUFHLElBQUksQ0FBQyxjQUFjLENBQUMsVUFBVSxDQUFDLFFBQVEsRUFBRSxhQUFhLEVBQUUsV0FBVyxDQUFDLENBQUM7UUFDaEYsQ0FBQzthQUNJLElBQUksVUFBVSxDQUFDLFNBQVMsS0FBSyxRQUFRLEVBQUUsQ0FBQztZQUMzQyxNQUFNLEdBQUcsSUFBSSxDQUFDLGNBQWMsQ0FDMUIsVUFBVSxDQUFDLFFBQVEsRUFDbkIsYUFBYSxFQUNiLFdBQVcsRUFDWCxVQUFVLENBQUMsbUJBQW1CLENBQy9CLENBQUM7UUFDSixDQUFDO2FBQ0ksQ0FBQztZQUNKLE1BQU0sSUFBSSxLQUFLLENBQUMsdUJBQXVCLFVBQVUsQ0FBQyxTQUFTLEVBQUUsQ0FBQyxDQUFDO1FBQ2pFLENBQUM7UUFFRCxNQUFNLFlBQVksR0FBRyxNQUFNLENBQUMsR0FBRyxDQUFDLENBQUMsS0FBSyxFQUFFLEVBQUU7WUFDeEMsT0FBTztnQkFDTCxHQUFHLEtBQUs7Z0JBQ1IsT0FBTyxFQUFFLEtBQUssS0FBSyxTQUFTLElBQUksS0FBSyxDQUFDLElBQUksSUFBSSxLQUFLLENBQUMsY0FBYyxJQUFJLEtBQUssQ0FBQyxJQUFJLElBQUksS0FBSyxDQUFDLFlBQVk7YUFDdkcsQ0FBQTtRQUNILENBQUMsQ0FBQyxDQUFBO1FBRUYsT0FBTztZQUNMLElBQUksRUFBRSxVQUFVLENBQUMsU0FBUztZQUMxQixhQUFhLEVBQUUsTUFBTTtZQUNyQixZQUFZLEVBQUUsWUFBWTtTQUMzQixDQUFDO0lBQ0osQ0FBQztJQUVELGtCQUFrQjtRQUNoQixPQUFPLElBQUksQ0FBQyx1QkFBdUIsQ0FBQyxHQUFHLEVBQUUsQ0FBQyxHQUFHLENBQUMsQ0FBQyxHQUFRLEVBQUUsRUFBRSxDQUFDLEdBQUcsQ0FBQyxTQUFTLENBQUMsQ0FBQztJQUM3RSxDQUFDO0NBRUYifQ==