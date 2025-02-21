import {
  ConfidenceIntervalTest,
  ControlLimitsType,
  PValueTest,
} from './statistics.js';

export type ChartType = "individuals" | "counts";
export interface ControlLimitsType {
  mean: ChartValue,
  upperControlLimit: ChartValue,
  lowerControlLimit: ChartValue
};

export interface ChartValue {
  value: number,
  transformedValue: number
}

export interface Observation {
  id: number;
  individualsValue: ChartValue;
  cusumValue?: ChartValue;
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
