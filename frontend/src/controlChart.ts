import 'daterangepicker';
import 'bootstrap';

import axios from 'axios';
import Chart, {
  ChartDataset,
  Point,
} from 'chart.js/auto';
import jQuery from 'jquery';

const DATA_URL = location.pathname + "/data";
const AVAILABLE_CHARTS_URL = "/availableCharts";

const CONTROL_LIMIT_COLOR = "#FF0000";
const MEAN_COLOR = "#00FF00";
const OBSERVATION_COLOR = "#000000";

const setColors = (color: string) => ({
  borderColor: color,
  backgroundColor: color,
  pointBorderColor: color,
  pointBackgroundColor: color
})

const CONTROL_LIMIT_STYLE = {
  ...setColors(CONTROL_LIMIT_COLOR),
  tension: 0,
  spanGaps: true,
  borderDash: [2,2],
  pointStyle: false
};

const MEAN_STYLE = {
  ...setColors(MEAN_COLOR),
  tension: 0,
  spanGaps: true,
  pointStyle: false
};

const MONITORED_STYLE = {
  ...setColors(OBSERVATION_COLOR),
  tension: 0,
  pointStyle: false,
  spanGaps: false
};

const SETUP_STYLE = {
  ...MONITORED_STYLE,
  borderDash: [2, 2]
}

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

async function updateChartDropdown () {
  const chartDropdownElements = document.getElementById('select-chart-items');
  if (! chartDropdownElements) {
    throw new Error("Unable to find chart dropdown elements DOM element");
  }

  const availableCharts: string[] = (await axios.get(AVAILABLE_CHARTS_URL)).data;
  const newChartDropdownElements = availableCharts.map((chartName: string) => {
    const listItem = document.createElement('li');
    const chartLink = document.createElement('a');
    chartLink.href = `/chart/${chartName}`;
    chartLink.className = "dropdown-item";
    chartLink.innerText = chartName;
    listItem.appendChild(chartLink);
    return listItem;
  })
  chartDropdownElements.replaceChildren(...newChartDropdownElements);
}

const chartSelectDropdown = document.getElementById('chart-select-dropdown');
if (! chartSelectDropdown) {
  throw new Error ("unable to locate chart select dropdown menu");
}
chartSelectDropdown.addEventListener('show.bs.dropdown', updateChartDropdown);

(async function () {
  const chartElement = document.getElementById('control_chart');
  if (! chartElement) {
    throw new Error(`Could not find element ${chartElement} to attach control chart plot`);
  }
  if (! (chartElement instanceof HTMLCanvasElement)) {
    throw new Error(`${chartElement} must refer to a canvas tag`);
  }
  
  const queryOptions = new URLSearchParams(window.location.search);
  const chart = new Chart(
      chartElement,
      {
        type: 'line',
        data: await getData()
      }
    )

  const currentURLComponents = new URL(window.location.href).pathname.split('/');
  const chartName = currentURLComponents[2];
  const currentStartTime = Number(currentURLComponents[4]);
  const currentEndTime = Number(currentURLComponents[6]);

  jQuery(() => {
    jQuery("#chart-range").daterangepicker(
      {
        timePicker: true,
        startDate: new Date(currentStartTime),
        endDate: new Date(currentEndTime),
      },
      async (start, stop) => {
        const currentURL = new URL(window.location.href);
        const startTime = start.toDate().valueOf().toString();
        const endTime = stop.toDate().valueOf().toString();
        
        const updatedPlotURL = `/chart/${chartName}/startTime/${startTime}/endTime/${endTime}`

        window.location.replace(updatedPlotURL);
      }
    )
  })


  async function getData (startDate?: string, endDate?: string) {

    const chartData: ChartData = (await axios.get(
      DATA_URL)
    ).data;
    
    const observations: Point[] = chartData.observations.map(
      (observation) => ({x: observation.time, y: observation.value}));
    const setupPoints: Point[] = chartData.observations.map(
      (observation) => ({x: observation.time, y: observation.isSetup ? observation.value : null})) as Point[];
    const monitoredPoints: Point[] = chartData.observations.map(
      (observation) => ({x: observation.time, y: ! observation.isSetup ? observation.value : null})) as Point[];

    const labels: string[] = observations.map((observation) => (new Date(observation.x)).toLocaleString());
    const upperControlLimits = makeEdgePoints(chartData.upperControlLimit, observations);
    const lowerControlLimits  = makeEdgePoints(chartData.lowerControlLimit, observations);
    const mean = makeEdgePoints(chartData.mean, observations);
    
    const dataSets: ChartDataset[] =  [
      {
        label: "Monitored Observations",
        order: 0, 
        data: monitoredPoints,
        ...MONITORED_STYLE
      },
      {
        label: "Setup Observations",
        order: 1, 
        data: setupPoints,
        ...SETUP_STYLE
      },
      {
        label: "Mean",
        order: 2, 
        data: mean,
        ...MEAN_STYLE
      },
      {
        label: "Upper Control Limit",
        order: 3, 
        data: upperControlLimits,
        ...CONTROL_LIMIT_STYLE
      },
      {
        label: "Lower Control Limit",
        order: 4, 
        data: lowerControlLimits,
        ...CONTROL_LIMIT_STYLE
      }
    ];
    

    return {datasets: dataSets, labels: labels}
  }

})()