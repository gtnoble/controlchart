export interface Chart {
  type: import("../chartDb").ChartType;
  controlLimits?: {
    mean: number;
    upperControlLimit: number;
    lowerControlLimit: number;
  };
  observations: {
    value: number;
    time: number;
    isSetup: boolean;
  }[];
}

export type ChartType = import("../chartDb").ChartType;
