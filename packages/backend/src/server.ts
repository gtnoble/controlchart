import assert from 'assert';
import Fastify from 'fastify';
import path from 'path';
import { fileURLToPath } from 'url';

import fastifyForms from '@fastify/formbody';
import fastifyStatic from '@fastify/static';

import { ChartDb } from './chartDb.js';

interface ChartParams {
  chartName: string;
  startTime?: string;
  endTime?: string;
  filename?: string;
  dataPointId?: string;
}

interface ObservationRequestBody {
  value: number;
  dataName: string;
  time?: string;
  annotations?: string;
}

interface CreateChartRequestBody {
  dataName: string;
  chartType: "individuals" | "counts";
  aggregationInterval: number;
  transformation?: "log";
}

export class Server {
  private database: ChartDb;
  private fastify: Fastify.FastifyInstance;

  constructor(databaseFilename: string) {
    this.database = new ChartDb(databaseFilename);
    this.fastify = Fastify({ logger: true });

    this.fastify.register(fastifyForms);
    this.fastify.register(fastifyStatic, {
      root: path.join(path.dirname(fileURLToPath(import.meta.url)), "..", "..", "frontend/dist")
    });

    this.setupRoutes();
  }

  private setupRoutes() {
    // Redirect root to landing page
    this.fastify.get('/', (request, reply) => {
      return reply.redirect('/index.html');
    });

    this.fastify.get('/availableCharts', this.getAvailableChartsHandler.bind(this));
    this.fastify.get('/availableDataNames', this.getAvailableDataNamesHandler.bind(this));
    this.fastify.get('/chart/:chartName', this.getChartHandler.bind(this));
    this.fastify.get('/chart/:chartName/startTime/:startTime/endTime/:endTime/chart', this.serveChartHtmlHandler.bind(this));
    this.fastify.get('/chart/:chartName/startTime/:startTime/endTime/:endTime/:filename', this.serveFileHandler.bind(this));
    this.fastify.get('/chart/:chartName/startTime/:startTime/endTime/:endTime/data', this.getChartDataHandler.bind(this));
    this.fastify.get('/chart/:chartName/startTime/:startTime/endTime/:endTime/transformedData', (request, reply) => this.getChartDataHandler(request, reply, true));
    this.fastify.post('/chart/:chartName/startTime/:startTime/endTime/:endTime/setSetup', this.setChartSetupHandler.bind(this));
    this.fastify.get('/chart/:chartName/setup/data', (request, reply) => this.getChartSetupPointsHandler(request, reply, false));
    this.fastify.get('/chart/:chartName/setup/transformedData', (request, reply) => this.getChartSetupPointsHandler(request, reply, true));
    this.fastify.get('/chart/:chartName/setup/chart', this.serveChartHtmlHandler.bind(this));
    this.fastify.post('/dataPoint/:dataPointId/annotate', this.addAnnotationHandler.bind(this));
    this.fastify.get('/dataPoint/:dataPointId/annotation', this.getAnnotationHandler.bind(this));
    this.fastify.post<{ Body: ObservationRequestBody }>('/addObservation', this.addObservationHandler.bind(this));

    // New endpoint for adding observations
    this.fastify.post<{ Body: CreateChartRequestBody; Params: { chartName: string } }>('/createChart/:chartName', async (request, reply) => {
      try {
        const { chartName } = request.params;
        const { dataName, chartType, aggregationInterval } = request.body;

        if (!chartName || !dataName || !chartType || aggregationInterval === undefined) {
          return reply.status(400).send({ error: 'Missing required fields' });
        }

        if (!["individuals", "counts"].includes(chartType)) {
          return reply.status(400).send({ error: 'Invalid chart type. Must be "individuals" or "counts"' });
        }

        this.database.initializeChart(chartName, dataName, chartType, aggregationInterval);
        
        if (request.body.transformation) {
          this.database.setTransformation(chartName, request.body.transformation);
        }
        reply.status(201).send({ message: 'Chart created successfully' });
      } catch (error) {
        console.error('Error creating chart:', error);
        reply.status(500).send({ error: 'Failed to create chart' });
      }
    });


  }

  public async start(port: number) {
    await this.fastify.listen({ port });
  }

  // Handler functions
  private async getAvailableChartsHandler(request: any, reply: any) {
    try {
      return this.database.getAvailableCharts();
    } catch (error) {
      reply.code(500).send({ error: 'Failed to get available charts' });
    }
  
  }
  private async getAvailableDataNamesHandler(request: any, reply: any) {
    try {
      return this.database.getAvailableDataNames();
    } catch (error) {
      reply.code(500).send({ error: 'Failed to get available data names' });
    }
  }

  private async getChartHandler(request: any, reply: any) {
    try {
      const params = request.params as ChartParams;
      const chartParams = this.database.getChartParameters(params.chartName);
      const dataLimits = this.database.getChartDataLimits(chartParams);

      return reply.redirect(`/chart/${params.chartName}/startTime/${dataLimits[0]}/endTime/${dataLimits[1]}/chart`);
    } catch (error) {
      reply.code(500).send({ error: 'Failed to get chart' });
    }
  }

  private async serveChartHtmlHandler(request: any, reply: any) {
    try {
      return reply.sendFile("chart.html");
    } catch (error) {
      reply.code(500).send({ error: 'Failed to serve chart HTML' });
    }
  }

  private async serveFileHandler(request: any, reply: any) {
    try {
      const params = request.params as ChartParams;
      assert("filename" in params);
      const filename = params.filename as string;
      assert(typeof filename === "string");

      return reply.sendFile(filename);
    } catch (error) {
      reply.code(500).send({ error: 'Failed to serve file' });
    }
  }

  private async getChartDataHandler(request: any, reply: any, transformed: boolean = false) {
    try {
      const params = request.params as ChartParams;
      const startTime = Number(params.startTime);
      const endTime = Number(params.endTime);
      const chartName = params.chartName;
      assert(typeof chartName === "string");

      return this.database.getChart(chartName, startTime, endTime, transformed);
    } catch (error) {
      reply.code(500).send({ error: 'Failed to get chart data' });
    }
  }

  private async setChartSetupHandler(request: any, reply: any) {
    try {
      const params = request.params as ChartParams;
      const startTime = Number(params.startTime);
      const endTime = Number(params.endTime);
      const chartName = params.chartName;
      assert(typeof chartName === "string");

      this.database.addChartSetup(chartName, new Date(startTime), new Date(endTime));
      return reply.redirect("./chart");
    } catch (error) {
      reply.code(500).send({ error: 'Failed to set chart setup' });
    }
  }

  private async addAnnotationHandler(request: any, reply: any) {
    try {
      const params = request.params as ChartParams;
      const annotation = request.body as { annotation: string };
      const chartDataId = Number(params.dataPointId);

      this.database.addAnnotation(chartDataId, annotation.annotation);
      return { success: true };
    } catch (error) {
      reply.code(500).send({ error: 'Failed to add annotation' });
    }
  }

  private async getAnnotationHandler(request: any, reply: any) {
    try {
      const params = request.params as ChartParams;
      const annotation = await this.database.getAnnotation(Number(params.dataPointId));
      reply.send({ annotation });
    } catch (error) {
      reply.status(500).send(error instanceof Error ? error.message : 'Unknown error');
    }
  }

  private async getChartSetupPointsHandler(request: any, reply: any, isTransformed: boolean) {
    try {
      const params = request.params as ChartParams;
      const setup = this.database.getChartSetup(params.chartName);
      
      if (!setup) {
        return reply.code(404).send({ error: 'No setup found for this chart' });
      }

      return this.database.getChart(
        params.chartName,
        setup.setupStartTime,
        setup.setupEndTime,
        isTransformed
      );
    } catch (error) {
      reply.code(500).send({ error: 'Failed to get setup points' });
    }
  }    
  
  private async addObservationHandler (request: any, reply: any) {
    try {
    const { value, dataName, time, annotations } = request.body;

    if (!value || !dataName) {
        return reply.status(400).send({ error: 'Missing required fields: value and dataName' });
      }

      const observation = {
        value,
        dataName,
        time: time ? Number(time) : undefined,
        annotations: annotations 
      };

      this.database.saveObservation(observation)
      reply.status(201).send({ message: 'Observation saved successfully' });
    } catch (error) {
      console.error('Error saving observation:', error);
      reply.status(500).send({ error: 'Failed to save observation' });
    }
  };
}
