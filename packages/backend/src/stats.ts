import math from '@stdlib/math';
import statistics from '@stdlib/stats';

import {
  ConfidenceIntervalTest,
  PValueTest,
} from './types/statistics.js';

const special = math.base.special;

const kstest = statistics.kstest;

export function countsChartSetupParams (
  setupValues: number[], 
  boundaryQuantile: number = 0.001
): ConfidenceIntervalTest {
  const poissonDistribution = statistics.base.dists.poisson;
  
  const mean = statistics.base.mean(setupValues.length, setupValues, 1);
  
  const upperControlLimit = poissonDistribution.quantile(mean, 1 - boundaryQuantile);
  const lowerControlLimit = poissonDistribution.quantile(mean, boundaryQuantile);

  return {statistic: mean, upper: upperControlLimit, lower: lowerControlLimit};
}

export function normalityTest (
  values: number[],
  alpha: number = 0.001
): PValueTest {
  const sampleMean = statistics.base.mean(values.length, values, 1);
  const sampleStandardDeviation = statistics.base.stdev(values.length, 1, values, 1);
  return kstest(
    values, 
    statistics.base.dists.normal.cdf, 
    sampleMean, 
    sampleStandardDeviation, 
    {alpha: alpha}
  );
}

export function sampleMedian (
  values: number[]
) {
  return statistics.base.mediansorted(
    values.length, 
    [...values].sort((a, b) => a - b), 
    1
  );
}

export function runsRandomnessTest (
  values: number[],
  boundaryQuantile: number = 0.001
): ConfidenceIntervalTest {
  const median = sampleMedian(values);
  
  const signs = values
    .map((value) => value - median)
    .filter((difference) => difference !== 0)
    .map((difference) => difference > 0);
    
  let numPositiveRuns = 0;
  let numNegativeRuns = 0;
  let runSign: boolean | undefined = undefined;
  
  for (const sign of signs) {
    if (! (sign === runSign)) {
      if (sign) {
        numPositiveRuns++;
      }
      else {
        numNegativeRuns++;
      }
      runSign = sign;
    }
  }
  
  const totalRuns = numPositiveRuns + numNegativeRuns;
  
  const mean = (2 * numPositiveRuns * numNegativeRuns / signs.length) + 1;
  const variance = ((mean - 1) * (mean - 2)) / (signs.length - 1);
  const standardDeviation = Math.sqrt(variance);
  
  const normalDistribution = statistics.base.dists.normal;
  const minumumExpectedRuns = normalDistribution.quantile(boundaryQuantile, mean, standardDeviation);
  const maximumExpectedRuns = normalDistribution.quantile(1 - boundaryQuantile, mean, standardDeviation);
  
  return {
    statistic: totalRuns,
    lower: minumumExpectedRuns,
    upper: maximumExpectedRuns
  };
}

export const c4 = (n: number) => {
  return Math.exp(
    .5 * Math.log(2 / (n - 1)) +
    special.gammaln(n / 2) -
    special.gammaln((n - 1) / 2)
  )
}
