import axios from 'axios';

import random from '@stdlib/random';

const {
  exponential,
  lognormal,
  normal,
  poisson,
  uniform,
} = random.base;

export interface TestSourceConfig {
  dataName: string;
  distribution: {
    type: 'normal' | 'uniform' | 'exponential' | 'poisson' | 'lognormal';
    params: {
      mean?: number;
      stdDev?: number;
      min?: number;
      max?: number;
      lambda?: number;
      mu?: number;    // mean of the log of the variable
      sigma?: number; // standard deviation of the log of the variable
    };
  };
  updateFrequencyMs: number;
}

export class TestSource {
  private config: TestSourceConfig;
  private intervalId?: NodeJS.Timeout;
  private apiUrl: string;

  constructor(config: TestSourceConfig, apiUrl = 'http://localhost:3000') {
    this.config = config;
    this.apiUrl = apiUrl;
  }

  private generateValue(): number {
    const { type, params } = this.config.distribution;

    switch (type) {
      case 'normal': {
        if (!params.mean || !params.stdDev) {
          throw new Error('Normal distribution requires mean and stdDev parameters');
        }
        return normal(params.mean, params.stdDev);
      }
      case 'uniform': {
        if (!params.min || !params.max) {
          throw new Error('Uniform distribution requires min and max parameters');
        }
        return uniform(params.min, params.max);
      }
      case 'exponential': {
        if (!params.lambda) {
          throw new Error('Exponential distribution requires lambda parameter');
        }
        return exponential(params.lambda);
      }
      case 'poisson': {
        if (!params.lambda) {
          throw new Error('Poisson distribution requires lambda parameter');
        }
        return poisson(params.lambda);
      }
      case 'lognormal': {
        if (!params.mu || !params.sigma) {
          throw new Error('Lognormal distribution requires mu and sigma parameters');
        }
        return lognormal(params.mu, params.sigma);
      }
      default:
        throw new Error(`Unsupported distribution type: ${type}`);
    }
  }

  private async sendObservation() {
    try {
      const value = this.generateValue();
      await axios.post(`${this.apiUrl}/addObservation`, {
        value,
        dataName: this.config.dataName,
        time: Date.now(),
      });
      console.log(`Sent observation: ${value}`);
    } catch (error) {
      console.error('Failed to send observation:', error);
    }
  }

  public start() {
    if (this.intervalId) {
      console.warn('Test source is already running');
      return;
    }
    this.intervalId = setInterval(() => {
      this.sendObservation();
    }, this.config.updateFrequencyMs);
    console.log(`Started test source with ${this.config.updateFrequencyMs}ms interval`);
  }

  public stop() {
    if (this.intervalId) {
      clearInterval(this.intervalId);
      this.intervalId = undefined;
      console.log('Stopped test source');
    }
  }

  public updateConfig(newConfig: Partial<TestSourceConfig>) {
    this.config = { ...this.config, ...newConfig };
    if (this.intervalId) {
      this.stop();
      this.start();
    }
  }
}
