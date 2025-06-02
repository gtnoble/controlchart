# Control Chart Application

## Project Overview
The Control Chart Application is a comprehensive system designed for real-time statistical process monitoring. It enables users to collect, analyze, and visualize data using various control charts, helping to identify process anomalies and maintain quality control. The application supports data transformations, statistical tests, and interactive visualization features through a web-based interface and a command-line utility.

## Features
- **Real-time Data Collection**: Collect data points via a command-line interface or HTTP POST requests.
- **Dynamic Control Charts**: Visualize process data using Individuals charts, Histograms, and CUSUM charts.
- **Statistical Process Control**: Automatically calculate and display Upper Control Limits (UCL), Lower Control Limits (LCL), and Mean, with support for setup periods.
- **Data Transformations**: Apply logarithmic, Anscombe, and Freeman-Tukey transformations to normalize data for accurate analysis.
- **Interactive Visualization**: Zoom, pan, and filter data on charts.
- **Anomaly Detection & Annotation**: Highlight data points outside control limits and allow users to add custom annotations for context.
- **Statistical Tests**: Display results for Normality (Kolmogorov-Smirnov) and Randomness (Runs Test) to assess process behavior.
- **Web-based Interface**: User-friendly web UI for chart selection, creation, and detailed viewing.
- **Monorepo Structure**: Organized into separate backend and frontend packages for modular development.

## Technical Stack

### Backend
- **Node.js**: Runtime environment.
- **Fastify**: High-performance web framework for REST APIs.
- **better-sqlite3**: Synchronous SQLite database driver.
- **@stdlib/stats**: Comprehensive JavaScript statistical library.
- **yargs**: Command-line argument parser.
- **TypeScript**: For type-safe and maintainable code.

### Frontend
- **Chart.js**: Flexible charting library for data visualization.
- **chartjs-plugin-zoom**: Enables interactive chart zooming and panning.
- **axios**: Promise-based HTTP client.
- **Bootstrap**: CSS framework for responsive UI.
- **Daterangepicker**: jQuery plugin for date range selection.
- **jQuery**: General DOM manipulation.
- **TypeScript**: For type-safe and maintainable code.

### Build System
- **npm workspaces**: Manages the monorepo.
- **TypeScript Compiler (tsc)**: Compiles TypeScript to JavaScript.

## Architecture
The application follows a monorepo structure, separating concerns into `backend` and `frontend` packages.

- **Backend**: Provides a CLI for data collection and chart initialization, and a Fastify web server exposing REST APIs for data management, statistical calculations, and serving frontend assets.
- **Database**: Uses SQLite for persistent storage of observations, chart configurations, and annotations. Leverages SQL temporary views for efficient on-the-fly statistical computations.
- **Frontend**: A web-based interface built with HTML, CSS, and JavaScript (TypeScript). It uses Chart.js for interactive data visualization, allowing users to select, view, and create control charts.

## Setup and Installation

1.  **Clone the repository**:
    ```bash
    git clone [repository-url]
    cd control_chart
    ```
2.  **Install dependencies**:
    The project uses npm workspaces.
    ```bash
    npm install
    ```
3.  **Build the project**:
    ```bash
    npm run build
    ```

## Usage

### Command Line Interface (CLI)
The backend provides a CLI for various operations. Navigate to the project root and run commands using `npm run start --workspace=backend -- [command]`.

-   **Initialize a new chart**:
    ```bash
    npm run start --workspace=backend -- init -d mychart.db -c my_process_chart -n process_data -t individuals -g 86400000
    ```
    (This initializes an 'individuals' chart named 'my_process_chart' using 'process_data' from 'mychart.db', aggregating daily.)

-   **Collect data (from command output)**:
    ```bash
    npm run start --workspace=backend -- collect -d mychart.db -n process_data --command "your_data_generating_command" -i 5000
    # Runs "your_data_generating_command" every 5 seconds and collects its output
    ```

-   **Start the web UI**:
    ```bash
    npm run start --workspace=backend -- server -d mychart.db -p 3000
    ```
    (Starts the web server on port 3000, serving data from `mychart.db`.)

### Web Interface
Once the web UI is running (e.g., on `http://localhost:3000`), open your browser and navigate to the specified address.
-   **Select Chart**: Choose an existing chart from the dropdown on the landing page.
-   **Create New Chart**: Click "Create New Chart" to define a new control chart.
-   **View Chart**: Explore interactive charts with zoom, pan, data transformations, and statistical tests.
-   **Annotate Data Points**: Click on a data point to add contextual annotations.

## Contributing
Contributions are welcome! Please feel free to submit issues or pull requests.

## License
MIT
