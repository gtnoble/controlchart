import axios from 'axios';
import Chart, {
  ChartDataset,
  Point,
} from 'chart.js/auto';

const DATA_URL = location.pathname + "/data";

const CONTROL_LIMIT_COLOR = "#FF0000";
const MEAN_COLOR = "#00FF00";
const OBSERVATION_COLOR = "#000000";

const CONTROL_LIMIT_STYLE = {
  borderColor: CONTROL_LIMIT_COLOR,
  backgroundColor: CONTROL_LIMIT_COLOR,
  pointBorderColor: CONTROL_LIMIT_COLOR,
  pointBackgroundColor: CONTROL_LIMIT_COLOR,
  tension: 0,
  spanGaps: true,
  borderDash: [2,2],
  pointStyle: false
};

const MEAN_STYLE = {
  borderColor: MEAN_COLOR,
  backgroundColor: MEAN_COLOR,
  pointBorderColor: MEAN_COLOR,
  pointBackgroundColor: MEAN_COLOR,
  tension: 0,
  spanGaps: true,
  pointStyle: false
};

const OBSERVATION_STYLE = {
  borderColor: OBSERVATION_COLOR,
  backgroundColor: OBSERVATION_COLOR,
  pointBorderColor: OBSERVATION_COLOR,
  pointBackgroundColor: OBSERVATION_COLOR,
  tension: 0,
  pointStyle: false
};

interface ChartData {
  chartName: string;
  upperControlLimit: number;
  lowerControlLimit: number;
  mean: number;
  observations: {time: number, value: number, isSetup: boolean}[];
}

function makeEdgePoints(value: number, observations: {x: number, y:number}[]) {
  const points = observations.map((point, index) => ({x: point.x, y: value}));
  return points;
}

(async function () {

  const chartData: ChartData = (await axios.get(DATA_URL)).data;
  
  const observations: Point[] = chartData.observations.map((observation) => ({x: observation.time, y: observation.value}));
  const labels: string[] = observations.map((observation) => (new Date(observation.x)).toLocaleString());
  const upperControlLimits = makeEdgePoints(chartData.upperControlLimit, observations);
  const lowerControlLimits  = makeEdgePoints(chartData.lowerControlLimit, observations);
  const mean = makeEdgePoints(chartData.mean, observations);
  
  const dataSets: ChartDataset[] =  [
    {
      label: chartData.chartName,
      order: 0, 
      data: observations,
      ...OBSERVATION_STYLE
    },
    {
      label: "Mean",
      order: 1, 
      data: mean,
      ...MEAN_STYLE
    },
    {
      label: "Upper Control Limit",
      order: 2, 
      data: upperControlLimits,
      ...CONTROL_LIMIT_STYLE
    },
    {
      label: "Lower Control Limit",
      order: 3, 
      data: lowerControlLimits,
      ...CONTROL_LIMIT_STYLE
    }
  ];
  
  const chartElement = document.getElementById('control_chart');
  if (! chartElement) {
    throw new Error(`Could not find element ${chartElement} to attach control chart plot`);
  }
  if (! (chartElement instanceof HTMLCanvasElement)) {
    throw new Error(`${chartElement} must refer to a canvas tag`);
  }

  new Chart(
    chartElement,
    {
      type: 'line',
      data: {datasets: dataSets, labels: labels}
    }
  )
})()