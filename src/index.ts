import { execSync } from 'node:child_process';

import yargs from 'yargs';

import { ChartDb } from './chartDb.js';

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
    argv.setup_start_time,
    argv.setup_end_time,
    argv.aggregation_interval
  );
}

main();

  
  
