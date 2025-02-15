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

export async function startServer(databaseFilename: string) {
  const database = new ChartDb(databaseFilename);
  const fastify = Fastify({ logger: true });

  fastify.register(fastifyForms);

  fastify.register(fastifyStatic, {
    root: path.join(
      path.dirname(fileURLToPath(import.meta.url)),
      "..",
      "..",
      "frontend/dist"
    )
  });

  async function getAvailableChartsHandler(request: any, reply: any) {
    try {
      return database.getAvailableCharts();
    } catch (error) {
      reply.code(500).send({ error: 'Failed to get available charts' });
    }
  }

  async function getChartHandler(request: any, reply: any) {
    try {
      const params = request.params as ChartParams;
      const chartParams = database.getChartParameters(params.chartName);
      const dataLimits = database.getChartDataLimits(chartParams);
      
      return reply.redirect(
        `/chart/${params.chartName}/startTime/${dataLimits[0]}/endTime/${dataLimits[1]}/chart`
      );
    } catch (error) {
      reply.code(500).send({ error: 'Failed to get chart' });
    }
  }

  async function serveChartHtmlHandler(request: any, reply: any) {
    try {
      return reply.sendFile("chart.html");
    } catch (error) {
      reply.code(500).send({ error: 'Failed to serve chart HTML' });
    }
  }

  async function serveFileHandler(request: any, reply: any) {
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

  async function getChartDataHandler(request: any, reply: any) {
    try {
      const params = request.params as ChartParams;
      const startTime = Number(params.startTime);
      const endTime = Number(params.endTime);
      const chartName = params.chartName;
      assert(typeof chartName === "string");
      
      return database.getChart(chartName, startTime, endTime);
    } catch (error) {
      reply.code(500).send({ error: 'Failed to get chart data' });
    }
  }

  async function setChartSetupHandler(request: any, reply: any) {
    try {
      const params = request.params as ChartParams;
      const startTime = Number(params.startTime);
      const endTime = Number(params.endTime);
      const chartName = params.chartName;
      assert(typeof chartName === "string");

      database.addChartSetup(chartName, new Date(startTime), new Date(endTime));
      return reply.redirect("./chart");
    } catch (error) {
      reply.code(500).send({ error: 'Failed to set chart setup' });
    }
  }

  fastify.get('/availableCharts', getAvailableChartsHandler);
  fastify.get('/chart/:chartName', getChartHandler);
  fastify.get('/chart/:chartName/startTime/:startTime/endTime/:endTime/chart', serveChartHtmlHandler);
  fastify.get('/chart/:chartName/startTime/:startTime/endTime/:endTime/:filename', serveFileHandler);
  fastify.get('/chart/:chartName/startTime/:startTime/endTime/:endTime/data', getChartDataHandler);
  fastify.post('/chart/:chartName/startTime/:startTime/endTime/:endTime/setSetup', setChartSetupHandler);

  async function addAnnotationHandler(request: any, reply: any) {
    try {
      const params = request.params as ChartParams;
      const annotation = request.body as { annotation: string };
      
      // Get the chart data ID from URL parameters
      const chartDataId = Number(params.dataPointId);
      
      // Add the annotation
      database.addAnnotation(chartDataId, annotation.annotation);

      return { success: true };
    } catch (error) {
      reply.code(500).send({ error: 'Failed to add annotation' });
    }
  }

  fastify.post('/dataPoint/:dataPointId/annotate', addAnnotationHandler);

  fastify.get('/dataPoint/:dataPointId/annotation', async (request, reply) => {
    try {
      const params = request.params as ChartParams;
      const annotation = await database.getAnnotation(Number(params.dataPointId));
      reply.send({ annotation });
    } catch (error) {
      reply.status(500).send(error instanceof Error ? error.message : 'Unknown error');
    }
  });

  await fastify.listen({ port: 3000 });
}
