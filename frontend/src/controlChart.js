var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
import axios from 'axios';
import Chart from 'chart.js/auto';
const DATA_URL = "data";
const CONTROL_LIMIT_COLOR = "#FF0000";
const MEAN_COLOR = "#00FF00";
const OBSERVATION_COLOR = "#000000";
const CONTROL_LIMIT_STYLE = {
    borderColor: CONTROL_LIMIT_COLOR,
    backgroundColor: CONTROL_LIMIT_COLOR,
    pointBorderColor: CONTROL_LIMIT_COLOR,
    pointBackgroundColor: CONTROL_LIMIT_COLOR,
    tension: 0,
    pointStyle: false
};
const MEAN_STYLE = {
    borderColor: MEAN_COLOR,
    backgroundColor: MEAN_COLOR,
    pointBorderColor: MEAN_COLOR,
    pointBackgroundColor: MEAN_COLOR,
    tension: 0,
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
function makeEdgePoints(value, observations) {
    var _a;
    const firstX = observations[0].x;
    const lastX = (_a = observations.at(-1)) === null || _a === void 0 ? void 0 : _a.x;
    if (!lastX) {
        throw new Error(`Observations array doesn't have a last element! Maybe it's empty?`);
    }
    return [{ x: firstX, y: value }, { x: lastX, y: value }];
}
(function () {
    return __awaiter(this, void 0, void 0, function* () {
        const chartData = yield axios.get(DATA_URL);
        const observations = chartData.observations;
        const upperControlLimits = makeEdgePoints(chartData.upperControlLimit, observations);
        const lowerControlLimits = makeEdgePoints(chartData.lowerControlLimit, observations);
        const mean = makeEdgePoints(chartData.mean, observations);
        const dataSets = [
            Object.assign({ order: 0, data: mean }, MEAN_STYLE),
            Object.assign({ label: chartData.chartName, order: 1, data: observations }, OBSERVATION_STYLE),
            Object.assign({ order: 2, data: upperControlLimits }, CONTROL_LIMIT_STYLE),
            Object.assign({ order: 3, data: lowerControlLimits }, CONTROL_LIMIT_STYLE)
        ];
        const chartElement = document.getElementById('control_chart');
        if (!chartElement) {
            throw new Error(`Could not find element ${chartElement} to attach control chart plot`);
        }
        if (!(chartElement instanceof HTMLCanvasElement)) {
            throw new Error(`${chartElement} must refer to a canvas tag`);
        }
        new Chart(chartElement, {
            type: 'line',
            data: { datasets: dataSets, labels: chartData.labels }
        });
    });
})();
