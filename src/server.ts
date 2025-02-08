import Fastify from 'fastify';

import { ChartDb } from './chartDb.js';

function startServer (databaseFilename: string) {
  const database = new ChartDb(databaseFilename);
  const fastify = Fastify({logger: true});

  fastify.register(require('@fastify/static'), {
    root: "./frontend/dist"
  })

  fastify.get('/chart/*', function (req, reply) {
    reply.sendFile('chart.html');
  })
  fastify.get('/chart/:chartName/data', chartHandler);

  async function chartHandler(request, reply) {
    const chartName: string = request.params.chartName;
    const startTime: string = request.query.startTime;
    const endTime: string = request.query.endTime;
    
    return database.getChart(chartName, )

    

  }
    
}
