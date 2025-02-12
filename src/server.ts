import assert from 'assert';
import Fastify from 'fastify';
import path from 'path';
import { fileURLToPath } from 'url';

import fastifyForms from '@fastify/formbody';
import fastifyStatic from '@fastify/static';

import { ChartDb } from './chartDb.js';

export async function startServer (databaseFilename: string) {
  const database = new ChartDb(databaseFilename);
  const fastify = Fastify({logger: true});
  
  fastify.register(fastifyForms);

  fastify.register(fastifyStatic, {
    root: path.join(
      path.dirname(fileURLToPath(import.meta.url)), "..",
      "frontend/dist"
    )
  })
  
  fastify.get('/availableCharts', function (req, reply) {
    return database.getAvailableCharts();
  })

  fastify.get('/chart/:chartName', function (req, reply) {
    const params: any = req.params;
    const chartParams = database.getChartParameters(params.chartName);
    const dataLimits = database.getChartDataLimits(chartParams);
    reply.redirect(`/chart/${params.chartName}/startTime/${dataLimits[0]}/endTime/${dataLimits[1]}/chart`)
  })
  
  fastify.get('/chart/:chartName/startTime/:startTime/endTime/:endTime/chart', function (req, reply) {
    reply.sendFile("chart.html")
  })

  fastify.get('/chart/:chartName/startTime/:startTime/endTime/:endTime/:filename', function (req, reply) {
    const params: any = req.params;
    assert("filename" in params);
    const filename: any = params.filename;
    assert(typeof filename === "string");

    reply.sendFile(filename);
  })

  fastify.get('/chart/:chartName/startTime/:startTime/endTime/:endTime/data', (request, reply) => {
    const params: any = request.params;

    const startTime = Number(params.startTime);
    const endTime = Number(params.endTime);
    const chartName: any = params.chartName;
    assert(typeof chartName === "string")
    
    return database.getChart(chartName, startTime, endTime);

  });

  fastify.post('/chart/:chartName/startTime/:startTime/endTime/:endTime/setSetup', (request, reply) => {
    const params: any = request.params;

    const startTime = Number(params.startTime);
    const endTime = Number(params.endTime);
    const chartName: any = params.chartName;
    assert(typeof chartName === "string")

    database.addChartSetup(chartName, new Date(startTime), new Date(endTime));
    reply.redirect("./chart");
  })
  
  await fastify.listen({port: 3000});
}

