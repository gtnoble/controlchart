export interface ChartData {
  type: import("../chartDb").ChartType;
  controlLimits?: {
    mean: number;
    upperControlLimit: number;
    lowerControlLimit: number;
  };
  observations: {
    id: number;
    value: number;
    time: number;
    isSetup: boolean;
    annotations: string[];
  }[];
}
