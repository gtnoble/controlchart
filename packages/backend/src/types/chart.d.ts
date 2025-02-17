export type ChartType = "individuals" | "counts";
export interface ChartData {
  type: ChartType;
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
