import assert from 'assert';
import Fastify from 'fastify';
import path from 'path';
import { fileURLToPath } from 'url';

import fastifyStatic from '@fastify/static';

import { ChartDb } from './chartDb.js';

export async function startServer (databaseFilename: string) {
  const database = new ChartDb(databaseFilename);
  const fastify = Fastify({logger: true});

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
    reply.sendFile("chart.html");
  })

  fastify.get('/chart/:chartName/:filename', function (req, reply) {
    const params: any = req.params;
    assert("filename" in params);
    const filename: any = params.filename;
    assert(typeof filename === "string");

    reply.sendFile(filename);
  })

  fastify.get('/chart/:chartName/data', (request, reply) => {
    const query: any = request.query;

    const startTime = query.startTime ? Number(query.startTime) : undefined;
    const endTime = query.endTime ? Number(query.endTime) : undefined;

    const params: any = request.params;
    assert("chartName" in params)
    const chartName: any = params.chartName;
    assert(typeof chartName === "string")
    
    return database.getChart(chartName, startTime, endTime);

  });
  
  await fastify.listen({port: 3000});
}

