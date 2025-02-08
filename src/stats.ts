import statistics from '@stdlib/stats';

export function individualsChartSetupParams (setupValues: number[], boundaryQuantile: number = 0.001) {
  const normalDistribution = statistics.base.dists.normal;

  const standardDeviation = statistics.base.stdev(setupValues.length, 1, setupValues, 1);

  const mean = statistics.base.mean(setupValues.length, setupValues, 1);
  const upperControlLimit = normalDistribution.quantile(1 - boundaryQuantile, mean, standardDeviation);
  const lowerControlLimit = normalDistribution.quantile(boundaryQuantile, mean, standardDeviation);
  return {mean: mean, upperControlLimit: upperControlLimit, lowerControlLimit: lowerControlLimit};
}

export function countsChartSetupParams (setupValues: number[], boundaryQuantile: number = 0.001) {
  const poissonDistribution = statistics.base.dists.poisson;
  
  const mean = statistics.base.mean(setupValues.length, setupValues, 1);
  
  const upperControlLimit = poissonDistribution.quantile(mean, 1 - boundaryQuantile);
  const lowerControlLimit = poissonDistribution.quantile(mean, boundaryQuantile);

  return {mean: mean, upperControlLimit: upperControlLimit, lowerControlLimit: lowerControlLimit};
}
