import yargs from 'yargs';
import { hideBin } from 'yargs/helpers';

import {
  TestSource,
  TestSourceConfig,
} from './testSource.js';

interface Args {
  distribution: 'normal' | 'uniform' | 'exponential' | 'poisson' | 'lognormal';
  frequency: number;
  duration?: number;
  dataName: string;
  mean?: number;
  stdDev?: number;
  min?: number;
  max?: number;
  lambda?: number;
  mu?: number;
  sigma?: number;
}

function getDistributionConfig(argv: Args): TestSourceConfig['distribution'] {
  switch (argv.distribution) {
    case 'normal':
      return {
        type: 'normal',
        params: {
          mean: argv.mean ?? 100,
          stdDev: argv.stdDev ?? 10
        }
      };
    case 'uniform':
      return {
        type: 'uniform',
        params: {
          min: argv.min ?? 80,
          max: argv.max ?? 120
        }
      };
    case 'exponential':
      return {
        type: 'exponential',
        params: {
          lambda: argv.lambda ?? 0.1
        }
      };
    case 'poisson':
      return {
        type: 'poisson',
        params: {
          lambda: argv.lambda ?? 5
        }
      };
    case 'lognormal':
      return {
        type: 'lognormal',
        params: {
          mu: argv.mu ?? 4.0,
          sigma: argv.sigma ?? 0.5
        }
      };
    default:
      throw new Error(`Unsupported distribution: ${argv.distribution}`);
  }
}

async function runExample(argv: Args) {
  const dataName = argv.dataName;

  const distribution = getDistributionConfig(argv);
  console.log(`Using ${argv.distribution} distribution with params:`, distribution.params);

  const testSource = new TestSource({
    dataName,
    distribution,
    updateFrequencyMs: argv.frequency
  });

  testSource.start();

  // Stop after specified duration if provided
  if (argv.duration) {
    setTimeout(() => {
      console.log('Stopping test source...');
      testSource.stop();
      process.exit(0);
    }, argv.duration * 1000);
  }

  // Handle SIGINT (Ctrl+C) for manual stopping
  process.on('SIGINT', () => {
    console.log('\nStopping test source...');
    testSource.stop();
    process.exit(0);
  });
}

// Run the example if this file is executed directly
  yargs(hideBin(process.argv))
    .strict()
    .option('distribution', {
      alias: 'd',
      describe: 'Distribution type',
      choices: ['normal', 'uniform', 'exponential', 'poisson', 'lognormal'],
      demandOption: true
    })
    .option('frequency', {
      alias: 'f',
      describe: 'Update frequency in milliseconds',
      type: 'number',
      default: 1000
    })
    .option('duration', {
      describe: 'Test duration in seconds',
      type: 'number',
      description: 'If not specified, runs indefinitely until Ctrl+C'
    })
    .option('dataName', {
      alias: 'n',
      describe: 'Name of the data series',
      type: 'string',
      demandOption: true
    })
    // Normal distribution params
    .option('mean', {
      describe: 'Mean for normal distribution',
      type: 'number'
    })
    .option('stdDev', {
      describe: 'Standard deviation for normal distribution',
      type: 'number'
    })
    // Uniform distribution params
    .option('min', {
      describe: 'Minimum value for uniform distribution',
      type: 'number'
    })
    .option('max', {
      describe: 'Maximum value for uniform distribution',
      type: 'number'
    })
    // Exponential and Poisson distribution param
    .option('lambda', {
      describe: 'Lambda parameter for exponential/poisson distribution',
      type: 'number'
    })
    // Lognormal distribution params
    .option('mu', {
      describe: 'Mu parameter for lognormal distribution',
      type: 'number'
    })
    .option('sigma', {
      describe: 'Sigma parameter for lognormal distribution',
      type: 'number'
    })
    .example([
      ['$0 -d normal -n test-data --mean 100 --stdDev 10', 'Use normal distribution'],
      ['$0 -d uniform -n test-data --min 80 --max 120', 'Use uniform distribution'],
      ['$0 -d exponential -n test-data --lambda 0.1', 'Use exponential distribution'],
      ['$0 -d poisson -n test-data --lambda 5', 'Use poisson distribution'],
      ['$0 -d lognormal -n test-data --mu 4.0 --sigma 0.5', 'Use lognormal distribution'],
      ['$0 -d normal -n test-data -f 500', 'Update every 500ms indefinitely'],
      ['$0 -d normal -n test-data -f 500 --duration 30', 'Update every 500ms for 30 seconds']
    ])
    .parseAsync()
    .then((argv) => runExample(argv as Args))
    .catch(console.error);
