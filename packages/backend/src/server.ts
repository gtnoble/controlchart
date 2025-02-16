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
    this.fastify.get('/availableCharts', this.getAvailableChartsHandler.bind(this));
    this.fastify.get('/chart/:chartName', this.getChartHandler.bind(this));
    this.fastify.get('/chart/:chartName/startTime/:startTime/endTime/:endTime/chart', this.serveChartHtmlHandler.bind(this));
    this.fastify.get('/chart/:chartName/startTime/:startTime/endTime/:endTime/:filename', this.serveFileHandler.bind(this));
    this.fastify.get('/chart/:chartName/startTime/:startTime/endTime/:endTime/data', this.getChartDataHandler.bind(this));
    this.fastify.post('/chart/:chartName/startTime/:startTime/endTime/:endTime/setSetup', this.setChartSetupHandler.bind(this));
    this.fastify.post('/dataPoint/:dataPointId/annotate', this.addAnnotationHandler.bind(this));
    this.fastify.get('/dataPoint/:dataPointId/annotation', this.getAnnotationHandler.bind(this));

    // New endpoint for adding observations
    this.fastify.post<{ Body: ObservationRequestBody }>('/api/observations', async (request, reply) => {
      try {
        const { value, dataName } = request.body;

        if (!value || !dataName) {
          return reply.status(400).send({ error: 'Missing required fields: value and dataName' });
        }

        const observation = {
          value,
          dataName,
          time: request.body.time || new Date().toISOString()
        };

        this.database.saveObservation(observation);
        reply.status(201).send({ message: 'Observation saved successfully' });
      } catch (error) {
        console.error('Error saving observation:', error);
        reply.status(500).send({ error: 'Failed to save observation' });
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

  private async getChartDataHandler(request: any, reply: any) {
    try {
      const params = request.params as ChartParams;
      const startTime = Number(params.startTime);
      const endTime = Number(params.endTime);
      const chartName = params.chartName;
      assert(typeof chartName === "string");

      return this.database.getChart(chartName, startTime, endTime);
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
}