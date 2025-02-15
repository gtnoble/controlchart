import 'daterangepicker';
import 'bootstrap';

import axios from 'axios';
import Chart, {
  ChartDataset,
  Point,
} from 'chart.js/auto';
import zoomPlugin from 'chartjs-plugin-zoom';
import jQuery from 'jquery';

import type { ChartData } from '../../backend/src/types/chart';

Chart.register(zoomPlugin);

const DATA_URL = "./data";
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

export type ChartType = "individuals" | "counts";

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
  
  let currentChartType: 'control' | 'histogram' = 'control';
  
interface HistogramData {
  labels: string[];
  data: number[];
}

function createHistogramData(observations: Point[], binCount = 20): HistogramData {
    const values = observations.map(o => o.y).filter(y => y !== null) as number[];
    const min = Math.min(...values);
    const max = Math.max(...values);
    const binSize = (max - min) / binCount;
    
    const bins = Array(binCount).fill(0);
    values.forEach(value => {
      const binIndex = Math.floor((value - min) / binSize);
      bins[Math.min(binIndex, binCount-1)]++;
    });
    
    return {
      labels: Array.from({length: binCount}, (_, i) => 
        `${(min + i*binSize).toFixed(1)}-${(min + (i+1)*binSize).toFixed(1)}`),
      data: bins
    };
  }

  let chart = new Chart(
      chartElement,
    {
      type: 'line',
      data: await getData(currentChartType),
      options: {
        interaction: {
          mode: 'nearest',
          intersect: true
        },
        onClick: (event, elements) => {
          if (elements.length > 0) {
            const { datasetIndex, index } = elements[0];
            const point = chart.data.datasets[datasetIndex].data[index] as any;
            if (point?.id) {
              const annotation = prompt('Enter annotation for this observation:');
              if (annotation) {
                axios.post(`/dataPoint/${point.id}/annotate`, { annotation })
                  .catch(error => alert(`Error saving annotation: ${error.message}`));
              }
            }
          }
        },
        responsive: true,
        scales: {
          y: {type: 'linear'}
        },
        plugins: {
          zoom: {
            pan: {
              enabled: true,
              mode: 'xy',
            },
            zoom: {
              wheel: {
                enabled: true
              },
              pinch: {
                enabled: true
              },
              mode: 'xy'
            }
          },
          tooltip: {
            callbacks: {
              afterBody: (context) => {
                const point = context[0].raw as any;
                return point?.annotations ? [`Annotation: ${point.annotations}`] : [];
              }
            }
          }
        },
      },
    }
    )
    
  const toggleLogButton = document.getElementById('toggleLog');
  if (! toggleLogButton) {
    throw new Error("missing log toggle button!");
  }
  toggleLogButton.addEventListener('click', function () {
    if (chart.options.scales?.y?.type !== 'logarithmic') {
      chart.options.scales = {y: {type: 'logarithmic'}};
    }
    else {
      chart.options.scales = {y: {type: 'linear'}};
    }
    chart.update();
  })

  // Add zoom reset functionality
  document.getElementById('resetZoom')?.addEventListener('click', () => {
    chart.resetZoom();
  });

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
        
        const updatedPlotURL = `/chart/${chartName}/startTime/${startTime}/endTime/${endTime}/chart`

        window.location.replace(updatedPlotURL);
      }
    )
  })


  // Add chart type dropdown handler
  document.getElementById('chartType')?.addEventListener('change', async (e) => {
    currentChartType = (e.target as HTMLSelectElement).value as 'control' | 'histogram';
    // Destroy old chart and create new one with correct type
    chart.destroy();
      chart = new Chart(chartElement, {
        type: currentChartType === 'histogram' ? 'bar' : 'line',
      data: await getData(currentChartType),
      options: {
        responsive: true,
        plugins: {
          zoom: {
            pan: {
              enabled: true,
              mode: 'xy',
            },
            zoom: {
              wheel: {
                enabled: true
              },
              pinch: {
                enabled: true
              },
              mode: 'xy'
            }
          }
        },
        scales: {
          y: {type: 'linear'}
        }
      },
    });
  });

  async function getData (chartType: 'control' | 'histogram', startDate?: string, endDate?: string): Promise<{datasets: ChartDataset[], labels: string[]}> {

    const chartData: ChartData = (await axios.get(
      DATA_URL)
    ).data;
    
    const observations = chartData.observations.map(observation => ({
      x: observation.time,
      y: observation.value,
      id: observation.id,
      annotations: observation.annotations
    }));
    const setupPoints = chartData.observations.map(observation => ({
      x: observation.time,
      y: observation.isSetup ? observation.value : NaN,
      id: observation.id,
      annotations: observation.annotations
    })) as Point[];
    const monitoredPoints = chartData.observations.map(observation => ({
      x: observation.time,
      y: !observation.isSetup ? observation.value : NaN,
      id: observation.id,
      annotations: observation.annotations
    })) as Point[];

    let labels: string[];
    if (chartType === 'histogram') {
      const { labels: binLabels } = createHistogramData(observations);
      labels = binLabels;
    } else {
      labels = observations.map((obs: Point) => new Date(obs.x).toLocaleString());
    }

    let controlLimitsDatasets: ChartDataset[] = [];
    if (chartData.controlLimits) {
      const upperControlLimits = makeEdgePoints(chartData.controlLimits.upperControlLimit, observations);
      const lowerControlLimits  = makeEdgePoints(chartData.controlLimits.lowerControlLimit, observations);
      const mean = makeEdgePoints(chartData.controlLimits.mean, observations);
      controlLimitsDatasets = [
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
      ]

    }
    
    
    let dataSets: ChartDataset[];
    if (chartType === 'histogram') {
      const histogramData = createHistogramData(observations);
      
      dataSets = [
        {
          label: "Histogram Data",
          order: 0,
          data: histogramData.data,
          ...SETUP_STYLE
        }
      ];
    } else {
      dataSets =  [
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
        ...controlLimitsDatasets
      ];
    }
    

    return {datasets: dataSets, labels: labels}
  }

})()
