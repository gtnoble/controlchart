package Chart_DB.SQL_Statements is
   
   -- Base tables
   Create_Chart_Data_Table : constant String := 
     "CREATE TABLE IF NOT EXISTS chart_data (" &
     "id INTEGER PRIMARY KEY ASC AUTOINCREMENT," &
     "time INTEGER," &
     "value REAL," &
     "data_name STRING)";
      
   Create_Chart_Annotations_Table : constant String := 
     "CREATE TABLE IF NOT EXISTS chart_annotations (" &
     "id INTEGER PRIMARY KEY ASC AUTOINCREMENT," &
     "chart_data_id INTEGER REFERENCES chart_data(id)," &
     "annotation TEXT," &
     "created_time INTEGER)";
      
   Create_Chart_Table : constant String := 
     "CREATE TABLE IF NOT EXISTS chart (" &
     "chart_name STRING PRIMARY KEY," &
     "chart_type STRING," &
     "data_name STRING," &
     "aggregation_interval INTEGER)";
      
   Create_Setup_Table : constant String := 
     "CREATE TABLE IF NOT EXISTS setup (" &
     "id INTEGER PRIMARY KEY ASC AUTOINCREMENT," &
     "chart_name STRING REFERENCES chart (chart_name)," &
     "setup_start_time INTEGER," &
     "setup_end_time INTEGER," &
     "creation_time INTEGER)";
      
   Create_Transformations_Table : constant String := 
     "CREATE TABLE IF NOT EXISTS transformations (" &
     "chart_name STRING REFERENCES chart (chart_name)," &
     "transformation STRING)";
      
   -- Indexes
   Create_Chart_Data_Index : constant String := 
     "CREATE INDEX IF NOT EXISTS chart_data_lookup " &
     "ON chart_data (id, time, value, data_name)";
      
   Create_Chart_Index : constant String := 
     "CREATE INDEX IF NOT EXISTS chart_lookup " &
     "ON chart (chart_name, chart_type, data_name)";
      
   Create_Setup_Index : constant String := 
     "CREATE INDEX IF NOT EXISTS setup_lookup " &
     "ON setup (id, chart_name, setup_start_time, setup_end_time)";
      
   Create_Annotations_Index : constant String := 
     "CREATE INDEX IF NOT EXISTS annotations_lookup " &
     "ON chart_annotations (id, chart_data_id)";
      
   Create_Transformations_Index : constant String := 
     "CREATE INDEX IF NOT EXISTS transformations_lookup " &
     "ON transformations (chart_name, transformation)";
      
   -- Core Views
   Create_Latest_Setup_View : constant String := 
     "CREATE TEMP VIEW IF NOT EXISTS latest_setup AS " &
     "WITH numbered_setups AS (" &
     "SELECT " &
     "s.chart_name," &
     "s.setup_start_time," &
     "s.setup_end_time," &
     "ROW_NUMBER() OVER (PARTITION BY s.chart_name ORDER BY s.id DESC) AS rn " &
     "FROM setup s) " &
     "SELECT " &
     "chart_name," &
     "setup_start_time," &
     "setup_end_time," &
     "rn " &
     "FROM numbered_setups " &
     "WHERE rn = 1";
      
   Create_Base_Data_View : constant String := 
     "CREATE TEMP VIEW IF NOT EXISTS base_data AS " &
     "SELECT " &
     "cd.id," &
     "cd.time," &
     "cd.value," &
     "cd.data_name," &
     "GROUP_CONCAT(ca.annotation) AS annotations," &
     "cp.chart_name," &
     "cp.transformation," &
     "transform(cp.transformation, cd.value) AS transformed_value " &
     "FROM chart_data cd " &
     "LEFT JOIN chart_annotations ca ON cd.id = ca.chart_data_id " &
     "JOIN chart_parameters cp ON cd.data_name = cp.data_name " &
     "GROUP BY cd.id, cd.time, cd.value, cd.data_name";
      
   Create_Setup_Stats_View : constant String := 
     "CREATE TEMP VIEW IF NOT EXISTS chart_summary AS " &
     "SELECT " &
     "bd.chart_name," &
     "AVG(bd.transformed_value) AS transformed_mean," &
     "standardDeviation(bd.transformed_value) AS transformed_standard_deviation " &
     "FROM base_data bd " &
     "JOIN latest_setup ls ON bd.chart_name = ls.chart_name " &
     "WHERE bd.time BETWEEN ls.setup_start_time AND ls.setup_end_time " &
     "GROUP BY bd.chart_name";

   -- Operation Statements
   Insert_Observation : constant String := 
     "INSERT INTO chart_data (time, value, data_name) VALUES (?, ?, ?)";
      
   Insert_Annotation : constant String := 
     "INSERT INTO chart_annotations (chart_data_id, annotation, created_time) " &
     "VALUES (?, ?, ?)";
      
   Initialize_Chart : constant String := 
     "INSERT OR REPLACE INTO chart " &
     "(chart_name, chart_type, data_name, aggregation_interval) " &
     "VALUES (?, ?, ?, ?)";
      
   Insert_Setup : constant String := 
     "INSERT INTO setup " &
     "(chart_name, setup_start_time, setup_end_time, creation_time) " &
     "VALUES (?, ?, ?, ?)";
      
   Set_Transformation : constant String := 
     "INSERT OR REPLACE INTO transformations " &
     "(chart_name, transformation) " &
     "VALUES (?, ?)";
      
   Get_Chart_Points : constant String := 
     "SELECT " &
     "cp.id," &
     "cp.time," &
     "cp.transformed_value AS value," &
     "cp.transformedUpperStatistic AS cusum_upper_statistic," &
     "cp.transformedLowerStatistic AS cusum_lower_statistic," &
     "cp.annotations " &
     "FROM chart_points cp " &
     "WHERE " &
     "cp.chart_name = ? " &
     "AND cp.time BETWEEN ? AND ? " &
     "ORDER BY cp.time, cp.id";
      
   Get_Control_Limits : constant String := 
     "SELECT " &
     "cl.chart_name," &
     "cl.mean," &
     "cl.upper_control_limit," &
     "cl.lower_control_limit," &
     "cl.cusum_control_limit " &
     "FROM control_limits cl " &
     "WHERE cl.chart_name = ?";

end Chart_DB.SQL_Statements;
