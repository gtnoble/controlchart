#!/usr/bin/env node
import { execSync } from 'node:child_process';
import * as readline from 'node:readline';

import yargs from 'yargs';

import { ChartDb } from './chartDb.js';
import { Server } from './server.js';

const ONE_DAY_MS = 1000 * 60 * 60 * 24;

async function main () {

  await yargs(process.argv.slice(2))
    .command(
      "collect [command]", 
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
            setup_start_time: {type: "string", alias: "s"},
            setup_end_time: {type: "string", alias: "e"},
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
            database: {type: "string", alias: "d", required: true},
            port: {type: "number", alias: "p", default: 42069}
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
      .command(
        "transform",
        "set the data transformation for the control chart",
        (yargs) => {
          yargs.options({
            database: {type: "string", alias: "d", required: true},
            chart_name: {type: "string", alias: "c", required: true},
            transformation: {type: "string", alias: "t", default: "none"}
          })
        },
        transformHandler
      )
      .strict()
      .demandCommand(1, "You must provide a valid command")
      .parse();


}

function transformHandler (argv: any) {
  const database = new ChartDb(argv.d);
  
  const transformationName = argv.transformation;
  
  validateChartName(argv.chart_name, database);
  
  if (transformationName === "none") {
    database.setTransformation(argv.chart_name, undefined);
  }
  else if (transformationName === "log") {
    database.setTransformation(argv.chart_name, "log");
  }
  else {
    throw new Error(
      `Transformation name must be either "none" or "log": given: ${transformationName}`
    )
  }
}

async function collectHandler (argv: any) {
  const command = argv.command;
  const database = new ChartDb(argv.d);
  
  if (command) {
    while (true) {
      let commandOutput: string;
      try {
        commandOutput = execSync(command, {encoding: "utf-8"});
      }
      catch (e) {
        console.error(`Command execution failed, ignoring: details: ${e}`)
        await new Promise(resolve => setTimeout(resolve, argv.interval));
        continue;
      }
      
      const value = Number(commandOutput);
      database.addObservation(value, argv.name);
      await new Promise(resolve => setTimeout(resolve, argv.interval));
    }
  }
  else {
    const rl = readline.createInterface(process.stdin);
    rl.on('line', (lineString) => database.addObservation(
      Number(lineString.trim()), argv.name)
    );
  }
  
}

async function initHandler (argv: any) {
  const database = new ChartDb(argv.d);
  
  database.initializeChart(
    argv.chart_name,
    argv.data_name,
    argv.type,
    argv.aggregation_interval
  );
  
  if (argv.setup_start_time && argv.setup_end_time) {
    database.addChartSetup(
      argv.chart_name, 
      new Date(argv.start_time),
      new Date(argv.end_time)
    )
  }
  else if (argv.setup_start_time || argv.setup_end_time) {
    throw new Error ("If you are specifying a setup interval, you must specify both the start and end times");
  }
}

async function server (argv: any) {
  const server = new Server(argv.database);
  server.start(argv.port)
}

async function chartHandler (argv: any) {
  const database = new ChartDb(argv.d);
  
  validateChartName(argv.chart_name, database);

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

function validateChartName (chartName: string, database: ChartDb) {
  const availableChartNames = database.getAvailableCharts();
  if (! availableChartNames.includes(chartName)) {
    throw new Error(`Chart name ${chartName} is not a valid chart name!`);
  }
}

main();

  
  
