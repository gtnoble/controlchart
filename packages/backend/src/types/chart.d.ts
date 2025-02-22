import {
  ConfidenceIntervalTest,
  PValueTest,
} from './statistics.js';

export type ChartType = "individuals" | "counts";
export interface ControlLimitsType {
  individualsMean: number,
  upperIndividualsLimit: number,
  lowerIndividualsLimit: number,
  cusumLimit: number
}

export interface Observation {
  id: number;
  individualsValue: number;
  cusum?: {
    upperStatistic: number,
    lowerStatistic: number
  };
  time: number;
  isSetup: boolean;
  annotations: string[];
}

export interface ChartData {
  type: ChartType;
  controlLimits?: ControlLimitsType;
  observations: Observation[];
  tests: {
    runsRandom: ConfidenceIntervalTest,
    ksNormal: PValueTest
  }
}
