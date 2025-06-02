# Control Chart Application

A comprehensive **Statistical Process Control (SPC)** system for quality control and process monitoring. This application provides both command-line and web-based tools for real-time process monitoring, quality control analysis, and data-driven decision making using industry-standard statistical methods.

## Features

### 📊 Statistical Control Charts
- **Individual/X Charts**: Monitor individual measurements with 3-sigma control limits
- **CUSUM Charts**: Detect small but persistent shifts in process mean
- **Count Charts**: Monitor discrete events and defect rates
- **Setup Intervals**: Establish control limits from stable baseline periods

### 🔬 Advanced Analytics
- **Data Transformations**: Log, Anscombe, Freeman-Tukey for non-normal data
- **Normality Testing**: Kolmogorov-Smirnov test for distribution validation
- **Randomness Testing**: Runs test for pattern detection
- **Statistical Summaries**: Comprehensive process statistics

### 💻 Interactive Web Interface
- **Professional UI**: Bootstrap-based responsive design
- **Zoomable Charts**: Interactive Chart.js-based plotting with zoom/pan
- **Real-time Updates**: Dynamic chart updates and data refresh
- **Annotation System**: Click-to-annotate data points with context
- **Multiple Views**: Control charts, histograms, statistical dashboards

### ⚡ CLI Automation Tools
- **Automated Data Collection**: Collect data from external commands
- **Manual Data Entry**: Interactive stdin data input
- **Chart Management**: Initialize, configure, and export charts
- **Scriptable Interface**: JSON output for programmatic consumption

### 🗄️ Robust Data Management
- **SQLite Database**: Efficient time-series data storage
- **Custom Functions**: Statistical calculations at database level
- **Data Reduction**: Handle millions of data points efficiently
- **Time-stamped Storage**: Millisecond precision data tracking

## Quick Start

### Prerequisites
- **Node.js** 18+ 
- **npm** 8+

### Installation

```bash
# Clone the repository
git clone <repository-url>
cd control_chart

# Install dependencies and build
npm install
npm run build
```

### Start the Web Interface

```bash
# Start the server (default port 42069)
npm start -- --database charts.db

# Or specify custom port and host
npm start -- --database charts.db --port 8080 --host 0.0.0.0
```

Access the web interface at `http://localhost:42069`

### Create Your First Chart

```bash
# Initialize a new control chart
npx control_chart init \
  --database charts.db \
  --chart_name "production_line_1" \
  --data_name "part_diameter" \
  --type "individuals"

# Start collecting data (example: measuring temperature every 5 seconds)
npx control_chart collect \
  --database charts.db \
  --name "part_diameter" \
  --interval 5000 \
  --command "sensors | grep 'Core 0' | awk '{print $3}' | sed 's/+//;s/°C//'"
```

## Installation & Setup

### Development Setup

```bash
# Install dependencies for all packages
npm install

# Build all packages
npm run build

# Start development server
npm start -- --database development.db
```

### Production Deployment

```bash
# Install and build
npm install
npm run build

# Start server with production settings
npx control_chart server \
  --database /path/to/production.db \
  --port 8080 \
  --host 0.0.0.0
```

## CLI Usage

The `control_chart` command provides comprehensive CLI tools for automation and scripting.

### Chart Initialization

```bash
# Create an individuals control chart
npx control_chart init \
  --database charts.db \
  --chart_name "quality_metric" \
  --data_name "measurement" \
  --type "individuals" \
  --aggregation_interval 86400000  # 1 day in milliseconds

# Create a count chart for defects
npx control_chart init \
  --database charts.db \
  --chart_name "defect_count" \
  --data_name "defects" \
  --type "counts"
```

### Data Collection

```bash
# Automated data collection from external command
npx control_chart collect \
  --database charts.db \
  --name "temperature" \
  --interval 1000 \
  --command "cat /sys/class/thermal/thermal_zone0/temp"

# Manual data entry via stdin
npx control_chart collect \
  --database charts.db \
  --name "manual_measurements"
# Then type values, one per line
```

### Data Export

```bash
# Export chart data as JSON
npx control_chart chart \
  --database charts.db \
  --chart_name "production_line_1" \
  --start_time "2024-01-01T00:00:00Z" \
  --end_time "2024-01-31T23:59:59Z"
```

### Data Transformations

```bash
# Apply logarithmic transformation
npx control_chart transform \
  --database charts.db \
  --chart_name "production_line_1" \
  --transformation "log"

# Remove transformation
npx control_chart transform \
  --database charts.db \
  --chart_name "production_line_1" \
  --transformation "none"
```

### Server Management

```bash
# Start web server
npx control_chart server \
  --database charts.db \
  --port 42069 \
  --host localhost
```

## Web Interface

### Chart Creation
1. Navigate to the web interface
2. Click "Create New Chart"
3. Configure chart parameters:
   - Chart name and data source
   - Chart type (individuals/counts)
   - Aggregation interval
   - Optional data transformations

### Interactive Features
- **Zoom & Pan**: Mouse wheel to zoom, drag to pan
- **Data Annotation**: Click data points to add contextual notes
- **Time Range Selection**: Use date picker for focused analysis
- **Real-time Updates**: Charts refresh automatically with new data
- **Multiple Views**: Switch between control chart, histogram, and statistics

### Chart Management
- View all available charts
- Configure setup intervals for control limits
- Apply data transformations in real-time
- Export chart data and images

## API Reference

### REST Endpoints

#### Chart Management
```http
GET /availableCharts
GET /availableDataNames
GET /chart/{chartName}
POST /createChart/{chartName}
```

#### Data Access
```http
GET /chart/{chartName}/startTime/{startTime}/endTime/{endTime}/data
GET /chart/{chartName}/recent/{count}/data
GET /chart/{chartName}/setup/data
```

#### Data Collection
```http
POST /addObservation
POST /dataPoint/{dataPointId}/annotate
GET /dataPoint/{dataPointId}/annotation
```

### Example API Usage

```javascript
// Add a new observation
fetch('/addObservation', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({
    value: 23.5,
    dataName: 'temperature',
    annotations: 'Measured after calibration'
  })
});

// Get recent chart data
const response = await fetch('/chart/production_line_1/recent/100/data');
const chartData = await response.json();
```

## Architecture

### Project Structure
```
control_chart/
├── packages/
│   ├── backend/           # Node.js/TypeScript server & CLI
│   │   ├── src/
│   │   │   ├── index.ts   # CLI entry point
│   │   │   ├── server.ts  # Web server
│   │   │   ├── chartDb.ts # Database layer
│   │   │   └── stats.ts   # Statistical functions
│   │   └── dist/          # Compiled JavaScript
│   └── frontend/          # TypeScript/Chart.js web interface
│       ├── src/
│       │   ├── index.html      # Landing page
│       │   ├── chart.html      # Chart viewer
│       │   ├── create-chart.html # Chart creation
│       │   └── controlChart.ts  # Chart logic
│       └── dist/          # Built assets
├── memory-bank/           # Project documentation
└── package.json          # Workspace configuration
```

### Technology Stack

**Backend:**
- **Node.js** + **TypeScript** - Modern JavaScript runtime with type safety
- **Fastify** - High-performance web framework
- **SQLite** + **better-sqlite3** - Embedded database with custom functions
- **@stdlib/stats** - Comprehensive statistical computing library
- **yargs** - Command-line interface framework

**Frontend:**
- **Chart.js** - Canvas-based interactive charting
- **Bootstrap** - Responsive UI framework
- **Parcel** - Zero-configuration build tool
- **TypeScript** - Type-safe frontend development

**Database:**
- **SQLite** with custom aggregate and scalar functions
- Optimized time-series storage and indexing
- Window functions for statistical calculations
- WAL mode for concurrent access

## Development

### Building from Source

```bash
# Install dependencies
npm install

# Build all packages
npm run build

# Build individual packages
npm run build --workspace=backend
npm run build --workspace=frontend
```

### Development Workflow

```bash
# Start development server with auto-reload
npm start -- --database dev.db

# Build and watch for changes (in separate terminals)
cd packages/backend && npx tsc --watch
cd packages/frontend && npx parcel watch
```

### Testing

```bash
# Run example data collection
cd packages/backend/src/tests
node example.ts

# Test API endpoints
curl http://localhost:42069/availableCharts
```

## Use Cases

### Manufacturing Quality Control
```bash
# Monitor part dimensions
npx control_chart init -d factory.db -c "part_diameter" -n "diameter" -t "individuals"
npx control_chart collect -d factory.db -n "diameter" -i 30000 -c "measure_part.sh"
```

### Service Performance Monitoring
```bash
# Track response times
npx control_chart init -d service.db -c "api_response" -n "response_time" -t "individuals"
npx control_chart collect -d service.db -n "response_time" -i 60000 -c "curl -w '%{time_total}' -s api.example.com"
```

### Process Capability Studies
1. Collect baseline data during stable operation
2. Set setup intervals to establish control limits
3. Monitor ongoing process performance
4. Detect shifts and trends using CUSUM charts

## Configuration

### Environment Variables
- `NODE_ENV` - Development/production mode
- `PORT` - Server port override
- `HOST` - Server bind address
- `DATABASE_PATH` - Default database location

### Database Configuration
- Automatic table creation on first run
- Custom SQL functions for statistical calculations
- Configurable aggregation intervals
- Efficient indexing for time-series queries

## Performance

### Scalability
- **Data Points**: Handles millions of observations efficiently
- **Charts**: Supports hundreds of concurrent charts
- **Users**: Single-writer database suitable for team use
- **Real-time**: Sub-second response times for interactive features

### Optimization Features
- **Data Reduction**: 1000-point sampling for large datasets
- **Efficient Queries**: Window functions and prepared statements
- **Client Caching**: Smart frontend data caching
- **Lazy Loading**: On-demand chart data loading

## License

ISC License - See LICENSE file for details.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes with tests
4. Submit a pull request

## Support

- **Documentation**: See memory-bank/ directory for detailed technical docs
- **Issues**: Report bugs and feature requests via GitHub issues
- **Examples**: Check packages/backend/src/tests/ for usage examples

---

**Built for quality control professionals who demand statistical rigor and operational excellence.**
