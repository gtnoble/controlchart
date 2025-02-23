import 'daterangepicker';
import 'bootstrap';

import axios from 'axios';
import Chart, {
  ChartDataset,
  Point as ChartPoint,
  ScriptableContext,
} from 'chart.js/auto';
import zoomPlugin from 'chartjs-plugin-zoom';
import jQuery from 'jquery';

import type { ChartData } from '../../backend/src/types/chart';

interface Point extends ChartPoint {
  id?: number;
  annotations?: string[];
}

Chart.register(zoomPlugin);

const DATA_URL = "./data";
const TRANSFORMED_DATA_URL = "./transformedData";
const AVAILABLE_CHARTS_URL = "/availableCharts";

type ViewType = 'control' | 'histogram' | 'statistics';

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
    spanGaps: false
  },
}

const SETUP_STYLE = {
  ...STYLES.MONITORED_STYLE,
  borderDash: [2, 2]
}

/**
 * Creates an array of points at a consistent y-value for drawing control limits
 * @param value The y-value for all points
 * @param observations Array of observation points to match x-values from
 * @returns Array of points with matching x-values from observations and consistent y-value
 */
function makeEdgePoints(value: number, observations: { x: number, y: number }[]): Point[] {
  return observations.map((point) => ({ 
    x: point.x, 
    y: value,
    annotations: [] 
  }));
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

  let currentChartType: ViewType = 'control';
  let isTransformedData = false;
  let showCusum = false;

  // Initialize checkbox states from localStorage
  function initializeCheckboxStates() {
    const transformCheckbox = document.getElementById('dataTransformToggle') as HTMLInputElement;
    const cusumCheckbox = document.getElementById('cusumToggle') as HTMLInputElement;

    // Set default states if not in localStorage
    isTransformedData = localStorage.getItem('isTransformedData') === 'true';
    showCusum = localStorage.getItem('showCusum') === 'true';

    if (transformCheckbox) {
      transformCheckbox.checked = isTransformedData;
    }
    if (cusumCheckbox) {
      cusumCheckbox.checked = showCusum;
    }
  }

  // Save checkbox state to localStorage
  function saveCheckboxState(checkbox: HTMLInputElement, key: string) {
    localStorage.setItem(key, checkbox.checked.toString());
  }

  // Initialize checkbox states
  initializeCheckboxStates();

  // Add toggle handlers
  document.getElementById('dataTransformToggle')?.addEventListener('change', async (e) => {
    const checkbox = e.target as HTMLInputElement;
    isTransformedData = checkbox.checked;
    saveCheckboxState(checkbox, 'isTransformedData');
    const data = await getData(currentChartType, isTransformedData, showCusum);
    if (data) {
      chart.data = data;
      chart.update();
    }
  });

  document.getElementById('cusumToggle')?.addEventListener('change', async (e) => {
    const checkbox = e.target as HTMLInputElement;
    showCusum = checkbox.checked;
    saveCheckboxState(checkbox, 'showCusum');
    const data = await getData(currentChartType, isTransformedData, showCusum);
    if (data) {
      chart.data = data;
      chart.update();
    }
  });

  // Add lockXAxis toggle handler
  const lockXAxisToggle = document.getElementById('lockXAxisToggle') as HTMLInputElement;
  lockXAxisToggle.checked = localStorage.getItem('lockXAxis') === 'true';

  lockXAxisToggle.addEventListener('change', () => {
    saveCheckboxState(lockXAxisToggle, 'lockXAxis');
    updateZoomMode();
  });

  // Get current chart name from URL
  const currentURL = new URL(window.location.href);
  const [, , chartName] = currentURL.pathname.split('/');

  // Update chart dropdown button text with current chart name
  const dropdownButton = document.querySelector('#chart-select-dropdown .btn');
  if (dropdownButton) {
    dropdownButton.textContent = chartName;
  }

  // Initialize chart
  let chart = new Chart(chartElement, {
    type: 'line',
    data: await getData(currentChartType, isTransformedData, showCusum) || { datasets: [], labels: [] },
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
            onPanComplete: saveZoomState
          },
          zoom: {
            wheel: {
              enabled: true
            },
            pinch: {
              enabled: true
            },
            mode: 'xy',
            onZoomComplete: saveZoomState
          }
        },
        tooltip: {
          callbacks: {
            afterBody: (context) => {
              const point = context[0].raw as Point;
              return point?.annotations?.map(annotation => `Annotation: ${annotation}`) ?? [];
            }
          }
        }
      }
    }
  });

  // Load saved zoom state
  function initChart() {
    const savedState = localStorage.getItem('chartZoomState');
    if (savedState) {
      const state = JSON.parse(savedState);
      if (state.scales) {
        const xAxis = chart.scales.x;
        const yAxis = chart.scales.y;
        xAxis.options.min = state.scales.x.min;
        xAxis.options.max = state.scales.x.max;
        yAxis.options.min = state.scales.y.min;
        yAxis.options.max = state.scales.y.max;
        chart.update();
      }
    }
  }

  // Save zoom state when zooming or panning completes
  function saveZoomState() {
    const scales = chart.scales;
    const state = {
      scales: {
        x: {
          min: scales.x.min,
          max: scales.x.max
        },
        y: {
          min: scales.y.min,
          max: scales.y.max
        }
      }
    };
    localStorage.setItem('chartZoomState', JSON.stringify(state));
  }

  // Initialize chart and set up zoom/pan handlers
  if (chart.options.plugins?.zoom) {
    chart.options.plugins.zoom.zoom = {
      ...chart.options.plugins.zoom.zoom,
      onZoomComplete: saveZoomState
    };
    chart.options.plugins.zoom.pan = {
      ...chart.options.plugins.zoom.pan,
      onPanComplete: saveZoomState
    };
  }
  
  // Initialize chart with saved state after a short delay to ensure proper rendering
  setTimeout(initChart, 100);

  const updateZoomMode = () => {
    if (chart.options.plugins && chart.options.plugins.zoom && chart.options.plugins.zoom.pan) {
      chart.options.plugins.zoom.pan.mode = lockXAxisToggle.checked ? 'y' : 'xy';
    }
    if (chart.options.plugins && chart.options.plugins.zoom && chart.options.plugins.zoom.zoom) {
      chart.options.plugins.zoom.zoom.mode = lockXAxisToggle.checked ? 'y' : 'xy';
    }
    chart.update();
  };

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

  // Optimize vertical zoom
  document.getElementById('optimizeZoom')?.addEventListener('click', () => {
    // Find individuals datasets
    const individualsDatasets = chart.data.datasets.filter(ds => 
      ds.label === "Monitored Observations" || ds.label === "Setup Observations"
    );
    
    if (individualsDatasets.length === 0) return;

    // Get all valid y values from both datasets
    const values = individualsDatasets.flatMap(dataset => 
      dataset.data
        .map((point: any) => point.y)
        .filter((y: number) => !isNaN(y))
    );

    if (values.length === 0) return;

    // Calculate optimal range with 10% padding
    const min = Math.min(...values);
    const max = Math.max(...values);
    const range = max - min;
    const padding = range * 0.1;

    // Update y axis scale
    if (chart.options.scales?.y) {
      chart.options.scales.y.min = min - padding;
      chart.options.scales.y.max = max + padding;
      chart.update();
      saveZoomState();
    }
  });

  // Reset zoom
  document.getElementById('resetZoom')?.addEventListener('click', () => {
    // Clear min/max settings to show full dataset
    if (chart.options.scales?.x) {
      chart.options.scales.x.min = undefined;
      chart.options.scales.x.max = undefined;
    }
    if (chart.options.scales?.y) {
      chart.options.scales.y.min = undefined;
      chart.options.scales.y.max = undefined;
    }
    chart.update();
    localStorage.removeItem('chartZoomState');
  });

  // Load setup data
  document.getElementById('loadSetup')?.addEventListener('click', () => {
    const currentURL = new URL(window.location.href);
    const [, , chartName] = currentURL.pathname.split('/');
    window.location.href = `/chart/${chartName}/setup/chart`;
  });

  // Load all data
  document.getElementById('loadAllData')?.addEventListener('click', () => {
    const currentURL = new URL(window.location.href);
    const [, , chartName] = currentURL.pathname.split('/');
    window.location.href = `/chart/${chartName}`;
  });

  // Date range picker
  const isSetupMode = currentURL.pathname.includes('/setup/chart');
  
  async function initializeDateRangePicker() {
    let startDate, endDate;
    
    const pathParts = currentURL.pathname.split('/');
    const isRecentMode = pathParts.includes('recent');
    
    if (isSetupMode) {
      // Get setup time range from backend
      try {
        const setupData = await axios.get(`/chart/${chartName}/setup/data`);
        if (setupData.data.observations && setupData.data.observations.length > 0) {
          const times = setupData.data.observations.map((obs: { time: number }) => obs.time);
          startDate = new Date(Math.min(...times));
          endDate = new Date(Math.max(...times));
        }
      } catch (error) {
        console.error('Failed to fetch setup time range:', error);
      }
    } else if (isRecentMode) {
      // For recent data, get the data to determine the time range
      try {
        const recentData = await axios.get(DATA_URL);
        if (recentData.data.observations && recentData.data.observations.length > 0) {
          const times = recentData.data.observations.map((obs: { time: number }) => obs.time);
          startDate = new Date(Math.min(...times));
          endDate = new Date(Math.max(...times));
        }
      } catch (error) {
        console.error('Failed to fetch recent data time range:', error);
      }
    } else {
      // Use URL parameters for regular chart view
      const [, , , , currentStartTime, , currentEndTime] = pathParts;
      startDate = new Date(Number(currentStartTime));
      endDate = new Date(Number(currentEndTime));
    }

    jQuery("#chart-range").daterangepicker({
      timePicker: true,
      startDate,
      endDate,
    }, async (start, end) => {
      if (isRecentMode) {
        // For recent mode, calculate the count based on the selected time range
        try {
          const allData = await axios.get(`/chart/${chartName}/data`);
          const observations = allData.data.observations;
          if (observations && observations.length > 0) {
            const startTime = start.toDate().valueOf();
            const endTime = end.toDate().valueOf();
            const count = observations.filter((obs: { time: number }) => 
              obs.time >= startTime && obs.time <= endTime
            ).length;
            window.location.replace(`/chart/${chartName}/recent/${count}/chart`);
          }
        } catch (error) {
          console.error('Failed to calculate count for recent data:', error);
        }
      } else {
        const startTime = start.toDate().valueOf().toString();
        const endTime = end.toDate().valueOf().toString();
        const updatedPlotURL = `/chart/${chartName}/startTime/${startTime}/endTime/${endTime}/chart`;
        window.location.replace(updatedPlotURL);
      }
    });
  }

  jQuery(initializeDateRangePicker);

  // Function to update view visibility
  function updateViewVisibility(chartType: ViewType) {
    const chartView = document.getElementById('chart-view') as HTMLElement;
    const statisticsView = document.getElementById('statistics-view') as HTMLElement;
    const chartOptions = document.querySelector('.chart-options') as HTMLElement;

    if (chartView && statisticsView && chartOptions) {
      if (chartType === 'statistics') {
        chartView.style.display = 'none';
        statisticsView.style.display = 'block';
        chartOptions.style.display = 'none';
      } else {
        chartView.style.display = 'block';
        statisticsView.style.display = 'none';
        chartOptions.style.display = 'block';
      }
    }
  }

  // Chart type change handler
  document.getElementById('chartType')?.addEventListener('change', async (e) => {
    currentChartType = (e.target as HTMLSelectElement).value as ViewType;
    
    updateViewVisibility(currentChartType);

    if (currentChartType !== 'statistics') {
      // Clear zoom state and destroy old chart
      localStorage.removeItem('chartZoomState');
      chart.destroy();
      
      // Create new chart with auto-scaling enabled
      chart = new Chart(chartElement, {
        type: currentChartType === 'histogram' ? 'bar' : 'line',
        data: await getData(currentChartType, isTransformedData, showCusum) || { datasets: [], labels: [] },
        options: {
          responsive: true,
          plugins: {
            zoom: {
              pan: {
                enabled: true,
                mode: 'xy',
                onPanComplete: saveZoomState
              },
              zoom: {
                wheel: {
                  enabled: true
                },
                pinch: {
                  enabled: true
                },
                mode: 'xy',
                onZoomComplete: saveZoomState
              }
            }
          },
          scales: {
            y: { type: 'linear' }
          }
        }
      });
    } else {
      // Just fetch data to update statistics
      await getData(currentChartType, isTransformedData, showCusum);
    }
  });

  // Initialize view visibility
  updateViewVisibility(currentChartType);
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
 * @param transformed Whether to use transformed data
 * @param showCusum Whether to show CUSUM data
 * @returns Chart data including datasets and labels, or null for statistics view
 */
async function getData(chartType: ViewType, transformed = false, showCusum = true): Promise<{ datasets: ChartDataset[]; labels: string[] } | null> {
  const url = transformed ? TRANSFORMED_DATA_URL : DATA_URL;
  
  try {
    const response = await axios.get(url);
    const chartData: ChartData = response.data;
    
    if (!chartData.observations || !chartData.observations.length) {
      throw new Error(`No observations found in data response from ${url}`);
    }
    
    console.log('Data fetch success:', {
      url,
      dataSize: chartData.observations.length,
      firstDataPoint: chartData.observations[0]
    });
    
    const observations = chartData.observations.map(obs => ({
      x: obs.time,
      y: obs.individualsValue,
      id: obs.id,
      annotations: obs.annotations
    }));

    // Update statistical test displays
    updateStatisticalTests(chartData);

    if (chartType === 'statistics') {
      return null;
    }

    if (chartType === 'histogram') {
      const histogramData = createHistogramData(observations);
      return {
        datasets: [{
          label: "Histogram Data",
          data: histogramData.data,
          backgroundColor: CONFIG.COLORS.OBSERVATION,
          borderColor: CONFIG.COLORS.OBSERVATION,
          borderWidth: 1
        }],
        labels: histogramData.labels
      };
    }
    
    return {
      datasets: createChartDatasets(chartData, showCusum),
      labels: observations.map(obs => new Date(obs.x).toLocaleString())
    };
  } catch (error) {
    console.error('Data fetch failed:', error);
    throw error;
  }
}

/**
 * Updates the statistical test displays with the latest data
 */
function updateStatisticalTests(chartData: ChartData) {
  if (!chartData.tests) return;

  // Update normality test (Kolmogorov-Smirnov test)
  const normalityPValue = document.getElementById('normality-pvalue');
  const normalityStatistic = document.getElementById('normality-statistic');
  if (normalityPValue && normalityStatistic) {
    normalityPValue.textContent = chartData.tests.ksNormal.pValue.toFixed(4);
    normalityStatistic.textContent = chartData.tests.ksNormal.statistic.toFixed(4);
  }

  // Update randomness test (Runs test)
  const randomnessPValue = document.getElementById('randomness-pvalue');
  const randomnessStatistic = document.getElementById('randomness-statistic');
  if (randomnessPValue && randomnessStatistic) {
    const { lower, upper, statistic } = chartData.tests.runsRandom;
    // For confidence interval test, we'll show the interval range as p-value
    randomnessPValue.textContent = `${lower.toFixed(4)} - ${upper.toFixed(4)}`;
    randomnessStatistic.textContent = statistic.toFixed(4);
  }
}

function createLabels(observations: { time: number }[]): string[] {
  return observations.map(obs => new Date(obs.time).toLocaleString());
}

function createChartDatasets(chartData: ChartData, showCusum: boolean): ChartDataset[] {
  const observations = chartData.observations.map(obs => ({
    x: obs.time,
    y: obs.individualsValue,
    id: obs.id,
    annotations: obs.annotations
  }));
  
  const setupPoints = chartData.observations.map(obs => ({
    x: obs.time,
    y: obs.isSetup ? obs.individualsValue : NaN,
    id: obs.id,
    annotations: obs.annotations
  })) as Point[];

  const monitoredPoints = chartData.observations.map(obs => ({
    x: obs.time,
    y: !obs.isSetup ? obs.individualsValue : NaN,
    id: obs.id,
    annotations: obs.annotations
  })) as Point[];

  const datasets: ChartDataset[] = [
    {
      label: "Monitored Observations",
      order: 0,
      data: monitoredPoints,
      ...STYLES.MONITORED_STYLE,
      pointStyle: (ctx: ScriptableContext<"line">) => {
        const point = monitoredPoints[ctx.dataIndex] as Point;
        return point?.annotations?.length ? 'circle' : false;
      },
      pointRadius: (ctx: ScriptableContext<"line">) => {
        const point = monitoredPoints[ctx.dataIndex] as Point;
        return point?.annotations?.length ? 6 : 0;
      },
      pointBackgroundColor: (ctx: ScriptableContext<"line">) => {
        const point = monitoredPoints[ctx.dataIndex] as Point;
        return point?.annotations?.length ? '#FFFF00' : CONFIG.COLORS.OBSERVATION;
      },
      pointBorderColor: (ctx: ScriptableContext<"line">) => {
        const point = monitoredPoints[ctx.dataIndex] as Point;
        return point?.annotations?.length ? '#FFFF00' : CONFIG.COLORS.OBSERVATION;
      }
    },
    {
      label: "Setup Observations",
      order: 1,
      data: setupPoints,
      ...SETUP_STYLE,
      pointStyle: (ctx: ScriptableContext<"line">) => {
        const point = setupPoints[ctx.dataIndex] as Point;
        return point?.annotations?.length ? 'circle' : false;
      },
      pointRadius: (ctx: ScriptableContext<"line">) => {
        const point = setupPoints[ctx.dataIndex] as Point;
        return point?.annotations?.length ? 6 : 0;
      },
      pointBackgroundColor: (ctx: ScriptableContext<"line">) => {
        const point = setupPoints[ctx.dataIndex] as Point;
        return point?.annotations?.length ? '#FFFF00' : CONFIG.COLORS.OBSERVATION;
      },
      pointBorderColor: (ctx: ScriptableContext<"line">) => {
        const point = setupPoints[ctx.dataIndex] as Point;
        return point?.annotations?.length ? '#FFFF00' : CONFIG.COLORS.OBSERVATION;
      }
    }
  ];

  if (chartData.controlLimits) {
    const upperControlLimits = makeEdgePoints(chartData.controlLimits.upperIndividualsLimit, observations);
    const lowerControlLimits = makeEdgePoints(chartData.controlLimits.lowerIndividualsLimit, observations);
    const mean = makeEdgePoints(chartData.controlLimits.individualsMean, observations);

    datasets.push({
      label: "Mean",
      order: 2,
      data: mean,
      ...STYLES.MEAN_STYLE
    });

    datasets.push({
      label: "Upper Control Limit",
      order: 3,
      data: upperControlLimits,
      ...STYLES.CONTROL_LIMIT_STYLE
    });

    datasets.push({
      label: "Lower Control Limit", 
      order: 4,
      data: lowerControlLimits,
      ...STYLES.CONTROL_LIMIT_STYLE
    });
  }

  if (showCusum && chartData.controlLimits) {
    const upperCusumPoints = makeEdgePoints(chartData.controlLimits.cusumLimit, observations);

    const upperCusumStatistic = chartData.observations.map(obs => ({
      x: obs.time,
      y: obs.cusum?.upperStatistic ?? NaN,
      id: obs.id,
      annotations: obs.annotations
    })) as Point[];

    const lowerCusumStatistic = chartData.observations.map(obs => ({
      x: obs.time,
      y: obs.cusum?.lowerStatistic ?? NaN,
      id: obs.id,
      annotations: obs.annotations
    })) as Point[];

    datasets.push({
      label: "Upper CUSUM Statistic",
      order: 5,
      data: upperCusumStatistic,
      borderColor: "#0000FF",
      backgroundColor: "#0000FF",
      pointBorderColor: "#0000FF",
      pointBackgroundColor: "#0000FF",
      tension: 0,
      pointStyle: false,
      spanGaps: false
    });

    datasets.push({
      label: "Lower CUSUM Statistic",
      order: 6,
      data: lowerCusumStatistic,
      borderColor: "#800080",
      backgroundColor: "#800080",
      pointBorderColor: "#800080",
      pointBackgroundColor: "#800080",
      tension: 0,
      pointStyle: false,
      spanGaps: false
    });

    datasets.push({
      label: "CUSUM Control Limit",
      order: 7,
      data: upperCusumPoints,
      borderColor: "#0000FF",
      backgroundColor: "#0000FF",
      pointBorderColor: "#0000FF",
      pointBackgroundColor: "#0000FF",
      tension: 0,
      spanGaps: true,
      borderDash: [2, 2],
      pointStyle: false
    });
  }

  return datasets;
}
