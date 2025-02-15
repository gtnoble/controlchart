import assert from 'assert';
import Fastify from 'fastify';
import path from 'path';
import { fileURLToPath } from 'url';
import fastifyForms from '@fastify/formbody';
import fastifyStatic from '@fastify/static';
import { ChartDb } from './chartDb.js';
export async function startServer(databaseFilename) {
    const database = new ChartDb(databaseFilename);
    const fastify = Fastify({ logger: true });
    fastify.register(fastifyForms);
    fastify.register(fastifyStatic, {
        root: path.join(path.dirname(fileURLToPath(import.meta.url)), "..", "frontend/dist")
    });
    async function getAvailableChartsHandler(request, reply) {
        try {
            return database.getAvailableCharts();
        }
        catch (error) {
            reply.code(500).send({ error: 'Failed to get available charts' });
        }
    }
    async function getChartHandler(request, reply) {
        try {
            const params = request.params;
            const chartParams = database.getChartParameters(params.chartName);
            const dataLimits = database.getChartDataLimits(chartParams);
            return reply.redirect(`/chart/${params.chartName}/startTime/${dataLimits[0]}/endTime/${dataLimits[1]}/chart`);
        }
        catch (error) {
            reply.code(500).send({ error: 'Failed to get chart' });
        }
    }
    async function serveChartHtmlHandler(request, reply) {
        try {
            return reply.sendFile("chart.html");
        }
        catch (error) {
            reply.code(500).send({ error: 'Failed to serve chart HTML' });
        }
    }
    async function serveFileHandler(request, reply) {
        try {
            const params = request.params;
            assert("filename" in params);
            const filename = params.filename;
            assert(typeof filename === "string");
            return reply.sendFile(filename);
        }
        catch (error) {
            reply.code(500).send({ error: 'Failed to serve file' });
        }
    }
    async function getChartDataHandler(request, reply) {
        try {
            const params = request.params;
            const startTime = Number(params.startTime);
            const endTime = Number(params.endTime);
            const chartName = params.chartName;
            assert(typeof chartName === "string");
            return database.getChart(chartName, startTime, endTime);
        }
        catch (error) {
            reply.code(500).send({ error: 'Failed to get chart data' });
        }
    }
    async function setChartSetupHandler(request, reply) {
        try {
            const params = request.params;
            const startTime = Number(params.startTime);
            const endTime = Number(params.endTime);
            const chartName = params.chartName;
            assert(typeof chartName === "string");
            database.addChartSetup(chartName, new Date(startTime), new Date(endTime));
            return reply.redirect("./chart");
        }
        catch (error) {
            reply.code(500).send({ error: 'Failed to set chart setup' });
        }
    }
    fastify.get('/availableCharts', getAvailableChartsHandler);
    fastify.get('/chart/:chartName', getChartHandler);
    fastify.get('/chart/:chartName/startTime/:startTime/endTime/:endTime/chart', serveChartHtmlHandler);
    fastify.get('/chart/:chartName/startTime/:startTime/endTime/:endTime/:filename', serveFileHandler);
    fastify.get('/chart/:chartName/startTime/:startTime/endTime/:endTime/data', getChartDataHandler);
    fastify.post('/chart/:chartName/startTime/:startTime/endTime/:endTime/setSetup', setChartSetupHandler);
    async function addAnnotationHandler(request, reply) {
        try {
            const params = request.params;
            const annotation = request.body;
            // Get the chart data ID from URL parameters
            const chartDataId = Number(params.dataPointId);
            // Add the annotation
            database.addAnnotation(chartDataId, annotation.annotation);
            return { success: true };
        }
        catch (error) {
            reply.code(500).send({ error: 'Failed to add annotation' });
        }
    }
    fastify.post('/dataPoint/:dataPointId/annotate', addAnnotationHandler);
    await fastify.listen({ port: 3000 });
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoic2VydmVyLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsic2VydmVyLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBLE9BQU8sTUFBTSxNQUFNLFFBQVEsQ0FBQztBQUM1QixPQUFPLE9BQU8sTUFBTSxTQUFTLENBQUM7QUFDOUIsT0FBTyxJQUFJLE1BQU0sTUFBTSxDQUFDO0FBQ3hCLE9BQU8sRUFBRSxhQUFhLEVBQUUsTUFBTSxLQUFLLENBQUM7QUFFcEMsT0FBTyxZQUFZLE1BQU0sbUJBQW1CLENBQUM7QUFDN0MsT0FBTyxhQUFhLE1BQU0saUJBQWlCLENBQUM7QUFFNUMsT0FBTyxFQUFFLE9BQU8sRUFBRSxNQUFNLGNBQWMsQ0FBQztBQVV2QyxNQUFNLENBQUMsS0FBSyxVQUFVLFdBQVcsQ0FBQyxnQkFBd0I7SUFDeEQsTUFBTSxRQUFRLEdBQUcsSUFBSSxPQUFPLENBQUMsZ0JBQWdCLENBQUMsQ0FBQztJQUMvQyxNQUFNLE9BQU8sR0FBRyxPQUFPLENBQUMsRUFBRSxNQUFNLEVBQUUsSUFBSSxFQUFFLENBQUMsQ0FBQztJQUUxQyxPQUFPLENBQUMsUUFBUSxDQUFDLFlBQVksQ0FBQyxDQUFDO0lBRS9CLE9BQU8sQ0FBQyxRQUFRLENBQUMsYUFBYSxFQUFFO1FBQzlCLElBQUksRUFBRSxJQUFJLENBQUMsSUFBSSxDQUNiLElBQUksQ0FBQyxPQUFPLENBQUMsYUFBYSxDQUFDLE1BQU0sQ0FBQyxJQUFJLENBQUMsR0FBRyxDQUFDLENBQUMsRUFDNUMsSUFBSSxFQUNKLGVBQWUsQ0FDaEI7S0FDRixDQUFDLENBQUM7SUFFSCxLQUFLLFVBQVUseUJBQXlCLENBQUMsT0FBWSxFQUFFLEtBQVU7UUFDL0QsSUFBSSxDQUFDO1lBQ0gsT0FBTyxRQUFRLENBQUMsa0JBQWtCLEVBQUUsQ0FBQztRQUN2QyxDQUFDO1FBQUMsT0FBTyxLQUFLLEVBQUUsQ0FBQztZQUNmLEtBQUssQ0FBQyxJQUFJLENBQUMsR0FBRyxDQUFDLENBQUMsSUFBSSxDQUFDLEVB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