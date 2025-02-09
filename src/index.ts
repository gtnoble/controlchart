import { execSync } from 'node:child_process';

import yargs from 'yargs';

import { ChartDb } from './chartDb.js';
import { startServer } from './server.js';

const ONE_DAY_MS = 1000 * 60 * 60 * 24;

async function main () {

  await yargs(process.argv.slice(2))
    .command(
      "collect <command>", 
      "collect data points", 
      (yargs) => {
        yargs
        .options({
          database: {type: "string", alias: "d", required: true},
          interval: {type: "number", alias: "i", default: 1000},
          name: {type: "string", alias: "n", required: true}
        })},
      collectHandler
    )
    .command(
      "init",
      "initialize a control chart",
      (yargs) => {
        yargs
        .options(
          {
            database: {type: "string", alias: "d", required: true},
            type: {type: "string", alias: "t", default: "individuals"},
            chart_name: {type: "string", alias: "c", required: true},
            data_name: {type: "string", alias: "n", required: true},
            setup_start_time: {type: "string", alias: "s", required: true},
            setup_end_time: {type: "string", alias: "e", required: true},
            aggregation_interval: {type: "number", alias: "g", default: ONE_DAY_MS}
          }
        )
      },
      initHandler
    )
      .command(
        "server",
        "start the web UI",
        (yargs) => {
          yargs.options({
            database: {type: "string", alias: "d", required: true}
          })
        },
        server
      )
      .command(
        "chart",
        "get the raw control chart data",
        (yargs) => {
          yargs.options({
            database: {type: "string", alias: "d", required: true},
            chart_name: {type: "string", alias: "c", required: true},
            start_time: {type: "string", alias: "s", default: (new Date(0)).toISOString()},
            end_time: {type: "string", alias: "e", default: (new Date()).toISOString()},
          })
        },
        chartHandler
      )
      .parse();


}

async function collectHandler (argv: any) {
  const command = argv.command;
  const database = new ChartDb(argv.d);
  
  while (true) {
    const commandOutput = execSync(command, {encoding: "utf-8"});
    const value = Number(commandOutput);
    database.addObservation(value, argv.name);
    await new Promise(resolve => setTimeout(resolve, argv.interval));
  }
  
}

async function initHandler (argv: any) {
  const database = new ChartDb(argv.d);
  
  database.initializeChart(
    argv.chart_name,
    argv.data_name,
    argv.type,
    new Date(argv.setup_start_time),
    new Date(argv.setup_end_time),
    argv.aggregation_interval
  );
}

async function server (argv: any) {
  await startServer(argv.database);
}

async function chartHandler (argv: any) {
  const database = new ChartDb(argv.d);
  
  console.log(
    JSON.stringify(
      database.getChart(
        argv.chart_name,
        (new Date(argv.start_time)).valueOf(),
        (new Date(argv.end_time)).valueOf()
      )
    )
  )
}

main();

  
  
