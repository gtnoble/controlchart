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
const TRANSFORMED_DATA_URL = "./transformedData";
const AVAILABLE_CHARTS_URL = "/availableCharts";

/**
 * Chart display configuration
 */
/**
 * Chart display configuration
 */
const CONFIG = {
  COLORS: {
    CONTROL_LIMIT: "#FF0000",
    MEAN: "#00FF00",
    OBSERVATION: "#000000"
  }
}

const STYLES = {
  CONTROL_LIMIT_STYLE: {
    borderColor: CONFIG.COLORS.CONTROL_LIMIT,
    backgroundColor: CONFIG.COLORS.CONTROL_LIMIT,
    pointBorderColor: CONFIG.COLORS.CONTROL_LIMIT,
    pointBackgroundColor: CONFIG.COLORS.CONTROL_LIMIT,
    tension: 0,
    spanGaps: true,
    borderDash: [2, 2],
    pointStyle: false
  },
  MEAN_STYLE: {
    borderColor: CONFIG.COLORS.MEAN,
    backgroundColor: CONFIG.COLORS.MEAN,
    pointBorderColor: CONFIG.COLORS.MEAN,
    pointBackgroundColor: CONFIG.COLORS.MEAN,
    tension: 0,
    spanGaps: true,
    pointStyle: false
  },
  MONITORED_STYLE: {
    borderColor: CONFIG.COLORS.OBSERVATION,
    backgroundColor: CONFIG.COLORS.OBSERVATION,
    pointBorderColor: CONFIG.COLORS.OBSERVATION,
    pointBackgroundColor: CONFIG.COLORS.OBSERVATION,
    tension: 0,
    pointStyle: false,
    spanGaps: false
  },
}
  ;
const SETUP_STYLE = {
  ...STYLES.MONITORED_STYLE,
  borderDash: [2, 2]
}

export type ChartType = "individuals" | "counts";

/**
 * Creates an array of points at a consistent y-value for drawing control limits
 * @param value The y-value for all points
 * @param observations Array of observation points to match x-values from
 * @returns Array of points with matching x-values from observations and consistent y-value
 */
function makeEdgePoints(value: number, observations: { x: number, y: number }[]): Point[] {
  return observations.map((point) => ({ x: point.x, y: value }));
}

/**
 * Updates the chart dropdown with available chart options
 */
async function updateChartDropdown(): Promise<void> {
  const chartDropdownElements = document.getElementById('select-chart-items');
  if (!chartDropdownElements) {
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
  });
  chartDropdownElements.replaceChildren(...newChartDropdownElements);
}

/**
 * Initializes and configures the chart
 */
(async function () {
  const chartElement = document.getElementById('control_chart');
  if (!chartElement) {
    throw new Error(`Could not find element ${chartElement} to attach control chart plot`);
  }
  if (!(chartElement instanceof HTMLCanvasElement)) {
    throw new Error(`${chartElement} must refer to a canvas tag`);
  }

  let currentChartType: 'control' | 'histogram' = 'control';
  let isTransformedData = false;

  // Add toggle handler
  document.getElementById('dataTransformToggle')?.addEventListener('change', async (e) => {
    isTransformedData = (e.target as HTMLInputElement).checked;
    const data = await getData(currentChartType, isTransformedData);
    chart.data = data;
    chart.update();
  });

  // Initialize chart
  let chart = new Chart(chartElement, {
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
        y: { type: 'linear' }
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
      }
    }
  });

  // Configure dropdown and event listeners
  const chartSelectDropdown = document.getElementById('chart-select-dropdown');
  if (chartSelectDropdown) {
    chartSelectDropdown.addEventListener('show.bs.dropdown', updateChartDropdown);
  }

  // Toggle log scale
  const toggleLogButton = document.getElementById('toggleLog');
  if (toggleLogButton) {
    toggleLogButton.addEventListener('click', function () {
      if (chart.options.scales && chart.options.scales.y) {
        if (chart.options.scales && chart.options.scales.y) {
          chart.options.scales.y.type = chart.options.scales.y.type === 'logarithmic'
            ? 'linear'
            : 'logarithmic';
        }
      }
      chart.update();
    });
  }

  // Reset zoom
  document.getElementById('resetZoom')?.addEventListener('click', () => {
    chart.resetZoom();
  });

  // Date range picker
  const currentURL = new URL(window.location.href);
  const [, , chartName, , currentStartTime, , currentEndTime] = currentURL.pathname.split('/');

  jQuery(() => {
    jQuery("#chart-range").daterangepicker({
      timePicker: true,
      startDate: new Date(Number(currentStartTime)),
      endDate: new Date(Number(currentEndTime)),
    }, async (start, end) => {
      const startTime = start.toDate().valueOf().toString();
      const endTime = end.toDate().valueOf().toString();
      const updatedPlotURL = `/chart/${chartName}/startTime/${startTime}/endTime/${endTime}/chart`;
      window.location.replace(updatedPlotURL);
    });
  });

  // Chart type dropdown handler
  document.getElementById('chartType')?.addEventListener('change', async (e) => {
    currentChartType = (e.target as HTMLSelectElement).value as 'control' | 'histogram';
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
          y: { type: 'linear' }
        }
      }
    });
  });
})();

/**
 * Creates histogram data from observations
 * @param observations Array of observation points
 * @param binCount Number of bins or 'auto' for automatic calculation
 * @returns Histogram data including labels and data array
 */
function createHistogramData(observations: Point[], binCount: number | 'auto' = 'auto'): { labels: string[]; data: number[] } {
  const values = observations.map(o => o.y).filter(y => y !== null) as number[];
  if (values.length < 2) return { labels: [], data: [] };

  // Calculate optimal bins using Freedman-Diaconis rule
  let calculatedBinCount: number;
  if (binCount === 'auto') {
    const sorted = [...values].sort((a, b) => a - b);
    const q1 = sorted[Math.floor(sorted.length * 0.25)];
    const q3 = sorted[Math.floor(sorted.length * 0.75)];
    const iqr = q3 - q1;
    const binWidth = 2 * iqr / Math.pow(values.length, 1 / 3);
    const minVal = Math.min(...values);
    const maxVal = Math.max(...values);
    calculatedBinCount = Math.ceil((maxVal - minVal) / binWidth) || 1;
  } else {
    calculatedBinCount = binCount;
  }

  const min = Math.min(...values);
  const max = Math.max(...values);
  const binSize = (max - min) / calculatedBinCount;

  const bins = Array(calculatedBinCount).fill(0);
  values.forEach(value => {
    const binIndex = Math.floor((value - min) / binSize);
    bins[Math.min(binIndex, calculatedBinCount - 1)]++;
  });

  return {
    labels: Array.from({ length: calculatedBinCount }, (_, i) =>
      `${(min + i * binSize).toFixed(1)}-${(min + (i + 1) * binSize).toFixed(1)}`),
    data: bins
  };
}

/**
 * Gets chart data based on the current chart type
 * @param chartType Type of chart to display
 * @returns Chart data including datasets and labels
 */
async function getData(chartType: 'control' | 'histogram', transformed = false): Promise<{ datasets: ChartDataset[]; labels: string[] }> {
  const url = transformed ? TRANSFORMED_DATA_URL : DATA_URL;
  const chartData: ChartData = (await axios.get(url)).data;

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
    const lowerControlLimits = makeEdgePoints(chartData.controlLimits.lowerControlLimit, observations);
    const mean = makeEdgePoints(chartData.controlLimits.mean, observations);
    controlLimitsDatasets = [
      {
        label: "Mean",
        order: 2,
        data: mean,
        ...STYLES.MEAN_STYLE
      },
      {
        label: "Upper Control Limit",
        order: 3,
        data: upperControlLimits,
        ...STYLES.CONTROL_LIMIT_STYLE
      },
      {
        label: "Lower Control Limit",
        order: 4,
        data: lowerControlLimits,
        ...STYLES.CONTROL_LIMIT_STYLE
      }
    ];
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
    dataSets = [
      {
        label: "Monitored Observations",
        order: 0,
        data: monitoredPoints,
        ...STYLES.MONITORED_STYLE
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

  return { datasets: dataSets, labels: labels };
}
