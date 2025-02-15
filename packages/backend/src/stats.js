import statistics from '@stdlib/stats';
import kstest from '@stdlib/stats/kstest';
;
export function individualsChartSetupParams(setupValues, boundaryQuantile = 0.001) {
    const normalDistribution = statistics.base.dists.normal;
    const standardDeviation = statistics.base.stdev(setupValues.length, 1, setupValues, 1);
    const mean = statistics.base.mean(setupValues.length, setupValues, 1);
    const upperControlLimit = normalDistribution.quantile(1 - boundaryQuantile, mean, standardDeviation);
    const lowerControlLimit = normalDistribution.quantile(boundaryQuantile, mean, standardDeviation);
    return { mean: mean, upperControlLimit: upperControlLimit, lowerControlLimit: lowerControlLimit };
}
export function countsChartSetupParams(setupValues, boundaryQuantile = 0.001) {
    const poissonDistribution = statistics.base.dists.poisson;
    const mean = statistics.base.mean(setupValues.length, setupValues, 1);
    const upperControlLimit = poissonDistribution.quantile(mean, 1 - boundaryQuantile);
    const lowerControlLimit = poissonDistribution.quantile(mean, boundaryQuantile);
    return { mean: mean, upperControlLimit: upperControlLimit, lowerControlLimit: lowerControlLimit };
}
export function normalityTest(values, alpha = 0.001) {
    const sampleMean = statistics.base.mean(values.length, values, 1);
    const sampleStandardDeviation = statistics.base.stdev(values.length, 1, values, 1);
    return kstest(values, statistics.base.dists.normal.cdf, sampleMean, sampleStandardDeviation, { alpha: alpha });
}
export function sampleMedian(values) {
    return statistics.base.mediansorted(values.length, [...values].sort((a, b) => a - b), 1);
}
export function runsRandomnessTest(values, boundaryQuantile) {
    const median = sampleMedian(values);
    const signs = values
        .map((value) => value - median)
        .filter((difference) => difference !== 0)
        .map((difference) => difference > 0);
    let numPositiveRuns = 0;
    let numNegativeRuns = 0;
    let runSign = undefined;
    for (const sign of signs) {
        if (!(sign === runSign)) {
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
        totalRuns: totalRuns,
        lowerRunsLimit: minumumExpectedRuns,
        upperRunsLimit: maximumExpectedRuns
    };
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoic3RhdHMuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyJzdGF0cy50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQSxPQUFPLFVBQVUsTUFBTSxlQUFlLENBQUM7QUFDdkMsT0FBTyxNQUFNLE1BQU0sc0JBQXNCLENBQUM7QUFNekMsQ0FBQztBQUVGLE1BQU0sVUFBVSwyQkFBMkIsQ0FDekMsV0FBcUIsRUFDckIsbUJBQTJCLEtBQUs7SUFFaEMsTUFBTSxrQkFBa0IsR0FBRyxVQUFVLENBQUMsSUFBSSxDQUFDLEtBQUssQ0FBQyxNQUFNLENBQUM7SUFFeEQsTUFBTSxpQkFBaUIsR0FBRyxVQUFVLENBQUMsSUFBSSxDQUFDLEtBQUssQ0FBQyxXQUFXLENBQUMsTUFBTSxFQUFFLENBQUMsRUFBRSxXQUFXLEVBQUUsQ0FBQyxDQUFDLENBQUM7SUFFdkYsTUFBTSxJQUFJLEdBQUcsVUFBVSxDQUFDLElBQUksQ0FBQyxJQUFJLENBQUMsV0FBVyxDQUFDLE1BQU0sRUFBRSxXQUFXLEVBQUUsQ0FBQyxDQUFDLENBQUM7SUFDdEUsTUFBTSxpQkFBaUIsR0FBRyxrQkFBa0IsQ0FBQyxRQUFRLENBQUMsQ0FBQyxHQUFHLGdCQUFnQixFQUFFLElBQUksRUFBRSxpQkFBaUIsQ0FBQyxDQUFDO0lBQ3JHLE1BQU0saUJBQWlCLEdBQUcsa0JBQWtCLENBQUMsUUFBUSxDQUFDLGdCQUFnQixFQUFFLElBQUksRUFBRSxpQkFBaUIsQ0FBQyxDQUFDO0lBQ2pHLE9BQU8sRUFBQyxJQUFJLEVBQUUsSUFBSSxFQUFFLGlCQUFpQixFQUFFLGlCQUFpQixFQUFFLGlCQUFpQixFQUFFLGlCQUFpQixFQUFDLENBQUM7QUFDbEcsQ0FBQztBQUVELE1BQU0sVUFBVSxzQkFBc0IsQ0FDcEMsV0FBcUIsRUFDckIsbUJBQTJCLEtBQUs7SUFFaEMsTUFBTSxtQkFBbUIsR0FBRyxVQUFVLENBQUMsSUFBSSxDQUFDLEtBQUssQ0FBQyxPQUFPLENBQUM7SUFFMUQsTUFBTSxJQUFJLEdBQUcs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