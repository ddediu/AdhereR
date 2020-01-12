###############################################################################################
#
#    STUpump: using AdhereR for offline scripted processing.
#    This file implements various SQL-related tasks independently of the 
#    particular SQL database used.
#    Copyright (C) 2019-2020  Dan Dediu
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
###############################################################################################

##
## Libraries and settings ####
##

library(configr); # read YAML config files

options(warn=1); # show warning as they occur (and allow their capture with capture.output)

##
## The SQL_db class that ecapsulates all SQL-related things ####
##

SQL_db <- function(spec_file=NA,                                                 # the file containing the database specification (or NA for the defaults)
                   connect_to_db=TRUE,                                           # try to connect to the database?
                   truncate_results=TRUE,                                        # remove any pre-existing rows from the results tables?
                   check_db=connect_to_db,                                       # check the consistency of the database?
                   preprocess_db=TRUE,                                           # do the various pre-processings?
                   log_file="./log.txt", log_file_append=FALSE,                  # the logfile
                   stop_on_database_errors=TRUE, stop_on_processing_errors=FALSE # what type(s) of errors to stop on
                  )
{
  # Init the log:
  if( !log_file_append )
  {
    try(file.remove(log_file), silent=TRUE);
  }
  try(cat(paste0("################################################### \n",
                 "# \n",
                 "# STUpump ",ifelse(exists("STUpump_version"),paste0("v.",STUpump_version," "),""),"log file \n",
                 "# generated on ",Sys.time(),"\n",
                 "# using config file '",spec_file,"' \n",
                 "# \n",
                 "################################################### \n",
                 "\n\n"), 
          file=log_file), silent=TRUE);
  
  if( !is.na(spec_file) )
  {
    # The connection
    db_connection <- NULL; 
    
    .msg(paste0("## Reading and parsing the config file '",spec_file,"' ##\n\n"), log_file, "m");
    
    # Load the actual database specification:
    #db_info <- read.table(spec_file, header=TRUE, sep="\t", quote="", fill=TRUE, strip.white=TRUE, blank.lines.skip=TRUE, stringsAsFactors=FALSE);
    db_info <- configr::read.config(spec_file);
    if( is.null(db_info) ) .msg(paste0("Error reading the config file '",spec_file,"'!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));

    
    if( !("database" %in% names(db_info)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'database' section is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    
    if( !("type" %in% names(db_info$database)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'database:type' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    if( !(tolower(db_info$database$type) %in% c("mariadb", "mysql", "sqlite", "mssql")) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'database:type' entry has an unknown value!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_type  <- tolower(db_info$database$type);
    .msg(paste0("Config: read 'database:type' = '",db_type,"'.\n"), log_file, "m");
    
    if( !("host" %in% names(db_info$database)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'database:host' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_host  <- db_info$database$host;
    .msg(paste0("Config: read 'database:host' = '",db_host,"'.\n"), log_file, "m");
    
    if( !("DSN" %in% names(db_info$database)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'database:DSN' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_dsn   <- db_info$database$DSN;
    .msg(paste0("Config: read 'database:DSN' = '",db_dsn,"'.\n"), log_file, "m");
    
    if( !("user" %in% names(db_info$database)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'database:user' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_user  <- db_info$database$user;
    .msg(paste0("Config: read 'database:user' = '",db_user,"'.\n"), log_file, "m");
    
    if( !("psswd" %in% names(db_info$database)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'database:psswd' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_psswd <- db_info$database$psswd;
    .msg(paste0("Config: read 'database:psswd' = '",paste0(rep("*",nchar(db_psswd)),collapse=""),"'.\n"), log_file, "m");
    
    if( !("name" %in% names(db_info$database)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'database:name' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_name  <- db_info$database$name;
    .msg(paste0("Config: read 'database:name' = '",db_name,"'.\n"), log_file, "m");
    
    .msg("\n", log_file, "m"); # aesthetic newline in the log file
    
    
    # The tables:
    if( !("tables" %in% names(db_info)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'tables' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));

    # The table prefix:
    if( !("table_prefix" %in% names(db_info$tables)) ) 
    {
      .msg(paste0("Warning in the config file '",spec_file,"': 'table_prefix' is not defined: taking it as empty!\n"), log_file, "w");
      db_table_prefix <- "";
    } else
    {
      db_table_prefix <- db_info$tables$table_prefix;
      .msg(paste0("Config: read 'table:table_prefix' = '",db_table_prefix,"'.\n"), log_file, "m");
    }
  
        
    # The events table:
    if( !("events" %in% names(db_info$tables)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'events' table is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    
    if( !("name" %in% names(db_info$tables$events)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:events:name' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_evtable_name <- db_info$tables$events$name;
    .msg(paste0("Config: read 'table:events:name' = '",db_evtable_name,"'.\n"), log_file, "m");
    
    db_evtable_cols <- c();
    
    if( !("patient_id" %in% names(db_info$tables$events)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:events:patient_id' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_evtable_cols <- c(db_evtable_cols, "ID"=db_info$tables$events$patient_id);
    .msg(paste0("Config: read 'table:events:patient_id' = '",db_evtable_cols["ID"],"'.\n"), log_file, "m");
    
    if( !("date" %in% names(db_info$tables$events)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:events:date' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_evtable_cols <- c(db_evtable_cols, "DATE"=db_info$tables$events$date);
    .msg(paste0("Config: read 'table:events:date' = '",db_evtable_cols["DATE"],"'.\n"), log_file, "m");
    
    if( !("perday" %in% names(db_info$tables$events)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:events:perday' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_evtable_cols <- c(db_evtable_cols, "PERDAY"=db_info$tables$events$perday);
    .msg(paste0("Config: read 'table:events:perday' = '",db_evtable_cols["PERDAY"],"'.\n"), log_file, "m");
    
    if( !("category" %in% names(db_info$tables$events)) ) 
      .msg(paste0("Warning in the config file '",spec_file,"': 'table:events:category' entry is not defined, assuming it is not present in the events database...\n"), log_file, "w");
    db_evtable_cols <- c(db_evtable_cols, "CATEGORY"=db_info$tables$events$category);
    .msg(paste0("Config: read 'table:events:category' = '",db_evtable_cols["CATEGORY"],"'.\n"), log_file, "m");
    
    if( !("duration" %in% names(db_info$tables$events)) ) 
      .msg(paste0("Warning in the config file '",spec_file,"': 'table:events:duration' entry is not defined, assuming it is not present in the events database...\n"), log_file, "w");
    db_evtable_cols <- c(db_evtable_cols, "DURATION"=db_info$tables$events$duration);
    .msg(paste0("Config: read 'table:events:duration' = '",db_evtable_cols["DURATION"],"'.\n"), log_file, "m");
    
    .msg("\n", log_file, "m"); # aesthetic newline in the log file
    
    
    # The actions table:
    if( !("actions" %in% names(db_info$tables)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'actions' table is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    
    if( !("name" %in% names(db_info$tables$actions)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:actions:name' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_actable_name <- db_info$tables$actions$name;
    .msg(paste0("Config: read 'table:actions:name' = '",db_actable_name,"'.\n"), log_file, "m");
    
    db_actable_cols <- c();
    
    if( !("action_id" %in% names(db_info$tables$actions)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:actions:action_id' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_actable_cols <- c(db_actable_cols, "ID"=db_info$tables$actions$action_id);
    .msg(paste0("Config: read 'table:actions:action_id' = '",db_actable_cols["ID"],"'.\n"), log_file, "m");
    
    if( !("action" %in% names(db_info$tables$actions)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:actions:action' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_actable_cols <- c(db_actable_cols, "ACTION"=db_info$tables$actions$action);
    .msg(paste0("Config: read 'table:actions:action' = '",db_actable_cols["ACTION"],"'.\n"), log_file, "m");
    
    if( !("params" %in% names(db_info$tables$actions)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:actions:params' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_actable_cols <- c(db_actable_cols, "PARAMS"=db_info$tables$actions$params);
    .msg(paste0("Config: read 'table:actions:params' = '",db_actable_cols["PARAMS"],"'.\n"), log_file, "m");
    
    .msg("\n", log_file, "m"); # aesthetic newline in the log file
    
    
    # The medication classes table:
    if( !("med_classes" %in% names(db_info$tables)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'med_classes' table is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    
    if( !("name" %in% names(db_info$tables$med_classes)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:med_classes:name' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_mctable_name <- db_info$tables$med_classes$name;
    .msg(paste0("Config: read 'table:med_classes:name' = '",db_mctable_name,"'.\n"), log_file, "m");
    
    db_mctable_cols <- c();
    
    if( !("medclass_id" %in% names(db_info$tables$med_classes)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:med_classes:medclass_id' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_mctable_cols <- c(db_mctable_cols, "ID"=db_info$tables$med_classes$medclass_id);
    .msg(paste0("Config: read 'table:med_classes:medclass_id' = '",db_mctable_cols["ID"],"'.\n"), log_file, "m");
    
    if( !("class" %in% names(db_info$tables$med_classes)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:med_classes:class' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_mctable_cols <- c(db_mctable_cols, "CLASS"=db_info$tables$med_classes$class);
    .msg(paste0("Config: read 'table:med_classes:class' = '",db_mctable_cols["CLASS"],"'.\n"), log_file, "m");
    
    .msg("\n", log_file, "m"); # aesthetic newline in the log file
    
    # The processing to be done table:
    if( !("processes" %in% names(db_info$tables)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'processes' table is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    
    if( !("name" %in% names(db_info$tables$processes)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:processes:name' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_prtable_name <- db_info$tables$processes$name;
    .msg(paste0("Config: read 'table:processes:name' = '",db_mctable_name,"'.\n"), log_file, "m");
    
    db_prtable_cols <- c();
    
    if( !("procid" %in% names(db_info$tables$processes)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:processes:procid' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_prtable_cols <- c(db_prtable_cols, "ID"=db_info$tables$processes$procid);
    .msg(paste0("Config: read 'table:processes:procid' = '",db_prtable_cols["ID"],"'.\n"), log_file, "m");
    
    .msg("\n", log_file, "m"); # aesthetic newline in the log file

    
    # The main results table:
    if( !("results" %in% names(db_info$tables)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'results' table is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    
    if( !("name" %in% names(db_info$tables$results)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:results:name' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_retable_name <- db_info$tables$results$name;
    .msg(paste0("Config: read 'table:results:name' = '",db_retable_name,"'.\n"), log_file, "m");
    
    db_retable_cols <- c();
    
    if( !("result_id" %in% names(db_info$tables$results)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:results:result_id' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_retable_cols <- c(db_retable_cols, "ID"=db_info$tables$results$result_id);
    .msg(paste0("Config: read 'table:results:result_id' = '",db_retable_cols["ID"],"'.\n"), log_file, "m");
    
    if( !("estimate" %in% names(db_info$tables$results)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:results:estimate' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_retable_cols <- c(db_retable_cols, "ESTIMATE"=db_info$tables$results$estimate);
    .msg(paste0("Config: read 'table:results:estimate' = '",db_retable_cols["ESTIMATE"],"'.\n"), log_file, "m");
    
    if( !("estimate_type" %in% names(db_info$tables$results)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:results:estimate_type' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_retable_cols <- c(db_retable_cols, "ESTIMATE_TYPE"=db_info$tables$results$estimate_type);
    .msg(paste0("Config: read 'table:results:estimate_type' = '",db_retable_cols["ESTIMATE_TYPE"],"'.\n"), log_file, "m");
    
    if( !("plot_jpg" %in% names(db_info$tables$results)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:results:plot_jpg' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_retable_cols <- c(db_retable_cols, "PLOT_JPG"=db_info$tables$results$plot_jpg);
    .msg(paste0("Config: read 'table:results:plot_jpg' = '",db_retable_cols["PLOT_JPG"],"'.\n"), log_file, "m");
    
    if( !("plot_html" %in% names(db_info$tables$results)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:results:plot_html' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_retable_cols <- c(db_retable_cols, "PLOT_HTML"=db_info$tables$results$plot_html);
    .msg(paste0("Config: read 'table:results:plot_html' = '",db_retable_cols["PLOT_HTML"],"'.\n"), log_file, "m");
    
    .msg("\n", log_file, "m"); # aesthetic newline in the log file
    
    
    # The sliding windows results table:
    if( !("sliding_windows_results" %in% names(db_info$tables)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'sliding_windows_results' table is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    
    if( !("name" %in% names(db_info$tables$sliding_windows_results)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:sliding_windows_results:name' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_swtable_name <- db_info$tables$sliding_windows_results$name;
    .msg(paste0("Config: read 'table:sliding_windows_results:name' = '",db_swtable_name,"'.\n"), log_file, "m");
    
    db_swtable_cols <- c();
    
    if( !("window_id" %in% names(db_info$tables$sliding_windows_results)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:sliding_windows_results:window_id' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_swtable_cols <- c(db_swtable_cols, "WINDOW_ID"=db_info$tables$sliding_windows_results$window_id);
    .msg(paste0("Config: read 'table:sliding_windows_results:window_id' = '",db_swtable_cols["WINDOW_ID"],"'.\n"), log_file, "m");
    
    if( !("window_start" %in% names(db_info$tables$sliding_windows_results)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:sliding_windows_results:window_start' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_swtable_cols <- c(db_swtable_cols, "WINDOW_START"=db_info$tables$sliding_windows_results$window_start);
    .msg(paste0("Config: read 'table:sliding_windows_results:window_start' = '",db_swtable_cols["WINDOW_START"],"'.\n"), log_file, "m");
    
    if( !("window_end" %in% names(db_info$tables$sliding_windows_results)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:sliding_windows_results:window_end' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_swtable_cols <- c(db_swtable_cols, "WINDOW_END"=db_info$tables$sliding_windows_results$window_end);
    .msg(paste0("Config: read 'table:sliding_windows_results:window_end' = '",db_swtable_cols["WINDOW_END"],"'.\n"), log_file, "m");
    
    if( !("estimate" %in% names(db_info$tables$sliding_windows_results)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:sliding_windows_results:estimate' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_swtable_cols <- c(db_swtable_cols, "WINDOW_ESTIMATE"=db_info$tables$sliding_windows_results$estimate);
    .msg(paste0("Config: read 'table:sliding_windows_results:estimate' = '",db_swtable_cols["WINDOW_ESTIMATE"],"'.\n"), log_file, "m");
    
    .msg("\n", log_file, "m"); # aesthetic newline in the log file
    
        
    # The per episode results table:
    if( !("per_episode_results" %in% names(db_info$tables)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'per_episode_results' table is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    
    if( !("name" %in% names(db_info$tables$per_episode_results)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:per_episode_results:name' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_petable_name <- db_info$tables$per_episode_results$name;
    .msg(paste0("Config: read 'table:per_episode_results:name' = '",db_petable_name,"'.\n"), log_file, "m");
    
    db_petable_cols <- c();
    
    if( !("episode_id" %in% names(db_info$tables$per_episode_results)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:per_episode_results:episode_id' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_petable_cols <- c(db_petable_cols, "EPISODE_ID"=db_info$tables$per_episode_results$episode_id);
    .msg(paste0("Config: read 'table:per_episode_results:episode_id' = '",db_petable_cols["EPISODE_ID"],"'.\n"), log_file, "m");
    
    if( !("episode_start" %in% names(db_info$tables$per_episode_results)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:per_episode_results:episode_start' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_petable_cols <- c(db_petable_cols, "EPISODE_START"=db_info$tables$per_episode_results$episode_start);
    .msg(paste0("Config: read 'table:per_episode_results:episode_start' = '",db_petable_cols["EPISODE_START"],"'.\n"), log_file, "m");
    
    if( !("gap_days" %in% names(db_info$tables$per_episode_results)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:per_episode_results:gap_days' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_petable_cols <- c(db_petable_cols, "GAP_DAYS"=db_info$tables$per_episode_results$gap_days);
    .msg(paste0("Config: read 'table:per_episode_results:gap_days' = '",db_petable_cols["GAP_DAYS"],"'.\n"), log_file, "m");
    
    if( !("episode_duration" %in% names(db_info$tables$per_episode_results)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:per_episode_results:episode_duration' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_petable_cols <- c(db_petable_cols, "ESPISODE_DURATION"=db_info$tables$per_episode_results$episode_duration);
    .msg(paste0("Config: read 'table:per_episode_results:episode_duration' = '",db_petable_cols["ESPISODE_DURATION"],"'.\n"), log_file, "m");
    
    if( !("episode_end" %in% names(db_info$tables$per_episode_results)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:per_episode_results:episode_end' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_petable_cols <- c(db_petable_cols, "EPISODE_END"=db_info$tables$per_episode_results$episode_end);
    .msg(paste0("Config: read 'table:per_episode_results:episode_end' = '",db_petable_cols["EPISODE_END"],"'.\n"), log_file, "m");
    
    if( !("estimate" %in% names(db_info$tables$per_episode_results)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:per_episode_results:estimate' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_petable_cols <- c(db_petable_cols, "EPISODE_ESTIMATE"=db_info$tables$per_episode_results$estimate);
    .msg(paste0("Config: read 'table:per_episode_results:estimate' = '",db_petable_cols["EPISODE_ESTIMATE"],"'.\n"), log_file, "m");
    
    .msg("\n", log_file, "m"); # aesthetic newline in the log file
    
    
    # The updated info table:
    if( !("updated_info" %in% names(db_info$tables)) ) 
      .msg(paste0("Warning in the config file '",spec_file,"': 'updated_info' table is not defined: assuming all patients are to be estimated...\n"), log_file, "w");
    
    if( !("name" %in% names(db_info$tables$updated_info)) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'table:updated_info:name' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_uptable_name <- db_info$tables$updated_info$name;
    .msg(paste0("Config: read 'table:updated_info:name' = '",db_uptable_name,"'.\n"), log_file, "m");

    db_uptable_cols <- c();
    
    .msg("\n", log_file, "m"); # aesthetic newline in the log file
    
    .msg(paste0("## Finished parsing the config file ## \n\n"), log_file, "m");
    
    
    
    # The object:
    ret_val <- structure(list(# the database specification file and its contents:
                              "db_spec_file"=spec_file,
                              "db_info"     =db_info,
                              
                              # the database info:
                              "db_type"     =db_type,
                              "db_host"     =db_host,
                              "db_dsn"      =db_dsn,
                              "db_user"     =db_user,
                              "db_psswd"    =db_psswd,
                              "db_name"     =db_name,
                              "db_quote_characters"=switch(db_type, 
                                                           "mariadb"=, 
                                                           "mysql"=,
                                                           "sqlite"=c("`","`"),
                                                           "mssql"=c("[","]"),
                                                           c("`","`")),
                              "db_table_prefix"=db_table_prefix,
                              
                              # the important tables:
                              "db_evtable_name"=db_evtable_name,
                              "db_evtable_cols"=db_evtable_cols,
                              "db_actable_name"=db_actable_name,
                              "db_actable_cols"=db_actable_cols,
                              "db_mctable_name"=db_mctable_name,
                              "db_mctable_cols"=db_mctable_cols,
                              "db_mctable_use_temp_table"=NULL, # if not NULL, the name of the temporary medication class table with the {} references solved
                              "db_prtable_name"=db_prtable_name,
                              "db_prtable_cols"=db_prtable_cols,
                              "db_prtable_use_temp_table"=NULL, # if not NULL, the name of the temporary processing table with the default actions "*" solved
                              "db_retable_name"=db_retable_name,
                              "db_retable_cols"=db_retable_cols,
                              "db_swtable_name"=db_swtable_name,
                              "db_swtable_cols"=db_swtable_cols,
                              "db_petable_name"=db_petable_name,
                              "db_petable_cols"=db_petable_cols,
                              "db_uptable_name"=db_uptable_name,
                              "db_uptable_cols"=db_uptable_cols,
                              
                              # the actual connection:
                              "db_connection"  =db_connection,
                              
                              # other info:
                              "log_file"                 =log_file,
                              "stop_on_database_errors"  =stop_on_database_errors,
                              "stop_on_processing_errors"=stop_on_processing_errors
                            ),
      class="SQL_db");
    
    # Connect to the database?
    if( connect_to_db )
    {
      # Attempt connection:
      ret_val <- connect(ret_val);
      
      # Check if the tables exist and contain the expected columns and are not empty:
      if( check_db && !check_tables(ret_val) ) return (NULL);

      # Reset the results tables:
      if( truncate_results && !reset_results(ret_val) ) return (NULL);
      
      # Pre-process:
      if( preprocess_db )
      {
        ret_val <- preprocess(ret_val);
      }
    }
    
    return (ret_val);
  } else
  { 
    return (NULL);
  }
}

##
## Error/warning/message reporting ####
##

.msg <- function(msg, log_file, type=c("m","w","e")[1]) 
{
  try(cat(msg, file=log_file, append=TRUE), silent=TRUE);
  switch(type,
         "m"=cat(msg),
         "w"=warning(msg),
         "s"=,
         stop(msg));
}


##
## Get configurable attributes ####
##

# Get various attributes either for the whole database (table=NULL) or for a specific table:
get <- function(x, variable, table=NULL, append_prefix_to_table_name=TRUE, df.compat=FALSE) UseMethod("get")
get.SQL_db <- function(x, variable, 
                       table=NULL, append_prefix_to_table_name=TRUE,
                       df.compat=FALSE) # ensure these are valid data.frame names?
{
  if( is.null(table) )
  {
    # Globals:
    return (switch(tolower(variable),
                   "file"     =,
                   "spec_file"=x$db_spec_file,
                   "info"     =x$db_info,
                   "type"     =x$db_type,
                   "host"     =x$db_host,
                   "dsn"      =x$db_dsn,
                   "user"     =x$db_user,
                   "psswd"    =x$db_psswd,
                   "name"     =x$db_name,
                   "table_prefix"=,
                   "prefix"   =,
                   "pre"      =ifelse(!is.null(x$db_table_prefix) && !is.na(x$db_table_prefix) && x$db_table_prefix != "", x$db_table_prefix, ""),
                   .msg(paste0("Undefined global attribute '",variable,"'.\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"))
    ));
  } else
  {
    # A specific table was requested:
    return (switch(tolower(table),
                   # Events:
                   "events"=,
                   "ev"=switch(tolower(variable),
                               "name"      =ifelse(append_prefix_to_table_name && get(x,"pre") != "", paste0(get(x,"pre"),x$db_evtable_name), x$db_evtable_name),
                               "patient_id"=,
                               "patid"     =,
                               "id"        =x$db_evtable_cols["ID"],
                               "date"      =x$db_evtable_cols["DATE"],
                               "perday"    =x$db_evtable_cols["PERDAY"],
                               "cat"       =,
                               "category"  =x$db_evtable_cols["CATEGORY"],
                               "duration"  =x$db_evtable_cols["DURATION"],
                               .msg(paste0("Undefined attribute '",variable,"' for table '",table,"'.\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"))
                   ),
                   
                   # Actions:
                   "actions"=,
                   "ac"=switch(tolower(variable),
                               "name"     =ifelse(append_prefix_to_table_name && get(x,"pre") != "", paste0(get(x,"pre"),x$db_actable_name), x$db_actable_name),
                               "action_id"=,
                               "actid"    =,
                               "id"       =x$db_actable_cols["ID"],
                               "action"   =x$db_actable_cols["ACTION"],
                               "params"   =x$db_actable_cols["PARAMS"],
                               .msg(paste0("Undefined attribute '",variable,"' for table '",table,"'.\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"))
                   ),
                   
                   # Medication classes:
                   "medication classes"=,
                   "medclass"          =,
                   "classes"           =,
                   "medications"       =,
                   "mc"=switch(tolower(variable),
                               "name"        =ifelse(is.null(x$db_mctable_use_temp_table), 
                                                     ifelse(append_prefix_to_table_name && get(x,"pre") != "", paste0(get(x,"pre"),x$db_mctable_name), x$db_mctable_name),
                                                     x$db_mctable_use_temp_table), # use the solved default temp table?,
                               "med_class_id"=,
                               "mcid"        =,
                               "id"          =x$db_mctable_cols["ID"],
                               "class"       =x$db_mctable_cols["CLASS"],
                               .msg(paste0("Undefined attribute '",variable,"' for table '",table,"'.\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"))
                   ),
                   
                   # Processings:
                   "processings"=,
                   "procs"      =,
                   "pr"=switch(tolower(variable),
                               "name"         =ifelse(is.null(x$db_prtable_use_temp_table), 
                                                      ifelse(append_prefix_to_table_name && get(x,"pre") != "", paste0(get(x,"pre"),x$db_prtable_name), x$db_prtable_name), 
                                                      x$db_prtable_use_temp_table), # use the solved default temp table?
                               "processing_id"=,
                               "procid"       =,
                               "id"           =x$db_prtable_cols["ID"],
                               "patient_id"   =,
                               "patid"        =get(x, "patid", "ev"),
                               "cat"          =,
                               "category"     =get(x, "mcid", "mc"),
                               "action"       =get(x, "actid", "ac"),
                               .msg(paste0("Undefined attribute '",variable,"' for table '",table,"'.\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"))
                   ),
                   
                   # Main results:
                   "main results"=,
                   "results"     =,
                   "re"=switch(tolower(variable),
                               "name"         =ifelse(append_prefix_to_table_name && get(x,"pre") != "", paste0(get(x,"pre"),x$db_retable_name), x$db_retable_name),
                               "result_id"    =,
                               "resid"        =,
                               "id"           =x$db_retable_cols["ID"],
                               "processing_id"=,
                               "procid"       =get(x, "procid", "pr"),
                               "patient_id"   =,
                               "patid"        =get(x, "patid", "ev"),
                               "estim"        =,
                               "estimate"     =x$db_retable_cols["ESTIMATE"],
                               "estim_type"   =,
                               "estimate_type"=x$db_retable_cols["ESTIMATE_TYPE"],
                               "jpg"          =,
                               "plot_jpg"     =x$db_retable_cols["PLOT_JPG"],
                               "html"         =,
                               "plot_html"    =x$db_retable_cols["PLOT_HTML"],
                               .msg(paste0("Undefined attribute '",variable,"' for table '",table,"'.\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"))
                   ),
                   
                   # Sliding window results:
                   "sliding windows results"=,
                   "sliding windows"        =,
                   "sw"=switch(tolower(variable),
                               "name"           =ifelse(append_prefix_to_table_name && get(x,"pre") != "", paste0(get(x,"pre"),x$db_swtable_name), x$db_swtable_name),
                               "result_id"      =,
                               "resid"          =get(x, "resid", "re"),
                               "patid"          =,
                               "id"             =,
                               "patient_id"     =get(x, "patid", "ev"),
                               "wndid"          =,
                               "window_id"      =x$db_swtable_cols["WINDOW_ID"],
                               "wndstart"       =,
                               "start"          =,
                               "window_start"   =x$db_swtable_cols["WINDOW_START"],
                               "wndend"         =,
                               "end"            =,
                               "window_end"     =x$db_swtable_cols["WINDOW_END"],
                               "wndestim"       =,
                               "estimate"       =,
                               "estim"          =,
                               "window_estimate"=x$db_swtable_cols["WINDOW_ESTIMATE"],
                               .msg(paste0("Undefined attribute '",variable,"' for table '",table,"'.\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"))
                   ),
                   
                   # Per episode results:
                   "per episode results"=,
                   "per episode"        =,
                   "pe"=switch(tolower(variable),
                               "name"            =ifelse(append_prefix_to_table_name && get(x,"pre") != "", paste0(get(x,"pre"),x$db_petable_name), x$db_petable_name),
                               "result_id"       =,
                               "resid"           =get(x, "resid", "re"),
                               "patid"           =,
                               "id"              =,
                               "patient_id"      =get(x, "patid", "ev"),
                               "epid"            =,
                               "episode_id"      =x$db_petable_cols["EPISODE_ID"],
                               "epstart"         =,
                               "start"           =,
                               "episode_start"   =x$db_petable_cols["EPISODE_START"],
                               "gap"             =,
                               "gap_days"        =x$db_petable_cols["GAP_DAYS"],
                               "epduration"      =,
                               "duration"        =,
                               "episode_duration"=x$db_petable_cols["ESPISODE_DURATION"],
                               "epend"           =,
                               "end"             =,
                               "episode_end"     =x$db_petable_cols["EPISODE_END"],
                               "epestim"         =,
                               "estimate"        =,
                               "estim"           =,
                               "episode_estimate"=x$db_petable_cols["EPISODE_ESTIMATE"],
                               .msg(paste0("Undefined attribute '",variable,"' for table '",table,"'.\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"))
                   ),
                   
                   # Updated info:
                   "updated info"=,
                   "updated"     =,
                   "up"=switch(tolower(variable),
                               "name"            =ifelse(append_prefix_to_table_name && get(x,"pre") != "", paste0(get(x,"pre"),x$db_uptable_name), x$db_uptable_name),
                               "patid"           =,
                               "id"              =,
                               "patient_id"      =get(x, "patid", "ev"),
                               .msg(paste0("Undefined attribute '",variable,"' for table '",table,"'.\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"))
                   ),
                   
                   .msg(paste0("Undefined table '",table,"'.\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w")))
    );
  }
}

# Quoted get:
qs_get <- function(x, variable, table=NULL, append_prefix_to_table_name=TRUE, df.compat=FALSE) UseMethod("qs_get")
qs_get.SQL_db <- function(x, variable, 
                          table=NULL, append_prefix_to_table_name=TRUE,
                          df.compat=FALSE) # ensure these are valid data.frame names?
{
  qs(x, get(x, variable, table, append_prefix_to_table_name, df.compat));
}

# Get a quoted and fully qualified (qfq) attributes either for the whole database (table=NULL) or for a specific table:
qfq_get <- function(x, variable, table=NULL, table_name=NULL) UseMethod("qfq_get")
qfq_get.SQL_db <- function(x, variable, table=NULL, table_name=NULL)
{
  ret_val <- c();
  
  if( TRUE || x$db_type == "mssql" )
  {
    ret_val <- c(ret_val, qs(x,get(x,"name"))); # must be prefixed by the database's name
  }
  
  if( !is.null(table_name) )
  {
    # this must be treated as an already-solved table name (used for temporary tables, for example):
    ret_val <- c(ret_val, qs(x, table_name));
  } else
  {
    if( !is.null(table) && variable != "name" )
    {
      ret_val <- c(ret_val, qs(x,get(x,"name",table))); # table-level columns (so, not the table's name) must be precede by the table's name
    }
    
    ret_val <- c(ret_val, qs(x,get(x,variable,table))); # the attribute's value
  }
  
  return (paste0(ret_val, collapse=".")); # these are all separated by "."s
}
  

##
## Quoting strings appropriately ####
##

lquote <- function(x) UseMethod("lquote")
lquote.SQL_db <- function(x)
{
  x$db_quote_characters[1];
}

rquote <- function(x) UseMethod("rquote")
rquote.SQL_db <- function(x)
{
  x$db_quote_characters[2];
}

# Quote string approriately:
qs <- function(x, s) UseMethod("qs")
qs.SQL_db <- function(x, s)
{
  # The '.' has a special meaning --> each '.'-separated substring must be quoted separately:
  tmp <- strsplit(s,".",fixed=TRUE)[[1]];
  tmp <- paste0(lquote(x),tmp[tmp!=""],rquote(x));
  if(substr(s,1,1) == ".") tmp <- c("",tmp);
  if(substr(s,nchar(s),nchar(s)) == ".") tmp <- c(tmp,"");
  paste0(tmp,collapse=".");
}


##
## Connect/disconnect to/from database ####
##

# Conect of a given type of database and return the connection or stop with an error:
connect <- function(x) UseMethod("connect")
connect.SQL_db <- function(x)
{
  # Pre-emptively try to disconnect a pre-existing connection:
  disconnect(x);

  if( x$db_type %in% c("mariadb", "mysql") )
  {
    # MariaDB or MySQL:
    require(RMariaDB);
    x$db_connection <- DBI::dbConnect(RMariaDB::MariaDB(), user=x$db_user, password=x$db_psswd, dbname=x$db_name, host=x$db_host);
  } else if( x$db_type == "sqlite" )
  {
    # SQLite:
    require(RSQLite);
    x$db_connection <- DBI::dbConnect(RSQLite::SQLite(), host=x$db_host);
  } else if( x$db_type == "mssql" )
  {
    #  Microsoft SQL Server:
    require(RODBC);
    x$db_connection <- RODBC::odbcConnect(dsn=x$db_dsn, uid=x$db_user, pwd=x$db_psswd);
    # Check if the database exists:
    db_list <- RODBC::sqlQuery(x$db_connection, "SELECT name FROM master.sys.databases");
    if( !(x$db_name %in% db_list$name) )
    {
      x$db_connection <- NULL;
      .msg(paste0("Connect: the required database '",x$db_name,"' does not exist on this server!\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"));
    }
  } else
  {
    x$db_connection <- NULL;
    .msg(paste0("Connect: don't know how to use an SQL database of type '",x$db_type,"': please specify a MariaDB, MySQL, SQLite or Microsoft SQL Server database!\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"));
  }
  if( is.null(x$db_connection) )
  {
    # Something bad happened:
    .msg("Connect: error connecting to the specified database!\n", x$log_file, ifelse(x$stop_on_database_errors,"e","w"));
  } else
  {
    # All seems fine:
    .msg("Connect: successful connection to the database...\n", x$log_file, "m");
  }
  
  # Return the connection:
  return (invisible(x));
}

# Disconnect from database:
disconnect <- function(x) UseMethod("disconnect")
disconnect.SQL_db <- function(x)
{
  if( !is.null(x$db_connection) )
  {
    # Drop the temporary tables (if any):
    if( !is.null(x$db_prtable_use_temp_table) &&
        (x$db_prtable_use_temp_table %in% list_tables(x)) ) table_drop(x, x$db_prtable_use_temp_table);
    if( !is.null(x$db_mctable_use_temp_table) &&
        (x$db_mctable_use_temp_table %in% list_tables(x)) ) table_drop(x, x$db_mctable_use_temp_table);
    
    if( x$db_type %in% c("mariadb", "mysql", "sqlite") )
    {
      # Disconect from it:
      try(DBI::dbDisconnect(x$db_connection), silent=TRUE);
    } else if( x$db_type == "mssql" )
    {
      # Disconnect from it:
      try(RODBC::odbcClose(x$db_connection), silent=TRUE);
    }
    
    x$db_connection <- NULL;
    .msg("Disconnect: connection stopped...\n", x$log_file, "m");
  } else
  {
    .msg("Disconnect: database connection already stopped or never made...\n", x$log_file, "m");
  }
  
  return (invisible(x));
}

# Initialise the results tables:
reset_results <- function(x) UseMethod("reset_results")
reset_results.SQL_db <- function(x)
{
  table_clear(x, get(x, 'name', 're'));
  table_clear(x, get(x, 'name', 'sw'));
  table_clear(x, get(x, 'name', 'pe'));

  .msg("Reset: database results tables truncated...\n", x$log_file, "m");
  
  return (TRUE);
}

##
## SQL queries and actions ####
##

sqlQ <- function(x, query, err_msg=NA, just_execute=FALSE) UseMethod("sqlQ")
sqlQ.SQL_db <- function(x, query, err_msg=NA, just_execute=FALSE)
{
  ret_val <- NULL;
  
  if( x$db_type %in% c("mariadb", "mysql", "sqlite") )
  {
    if( just_execute )
    {
      if( !is.na(err_msg) )
      {
        try(ret_val <- DBI::dbExecute(x$db_connection, query), silent=TRUE);
      } else
      {
        ret_val <- DBI::dbExecute(x$db_connection, query);
      }
    } else
    {
      ret_val <- DBI::dbGetQuery(x$db_connection, query);
    }
  } else if( x$db_type == "mssql" )
  {
    ret_val <- RODBC::sqlQuery(x$db_connection, query);
  }
  
  if( !is.na(err_msg) && is.null(ret_val) )
  {
    .msg(err_msg, x$log_file, ifelse(x$stop_on_database_errors,"e","w"));
    return (NULL)
  } else
  {
    return (ret_val);
  }
}
  

##
## Database pre-processing ####
##

preprocess <- function(x, patient_id) UseMethod("preprocess")
preprocess.SQL_db <- function(x)
{
  .msg(paste0("Preprocessing of the database started...\n"), x$log_file, "m");
  
  # The processings table
  # Solve the "*" action (meaning the default ones, defined as the ones with * for both patid and category in the same table
  
  # Do we need to do anything about this?
  default_actions <- sqlQ(x, query=paste0("SELECT COUNT(*)",
                                          " FROM ",qfq_get(x, 'name', 'pr'),
                                          " WHERE ",qs_get(x, 'action', 'pr')," = '*'",
                                          " ;"),
                          err_msg=paste0("Error retreiving the number of default actions '*' from the processings table '",get(x, 'name', 'pr'),"'!\n"), just_execute=FALSE);
  if( is.null(default_actions) || nrow(default_actions) == 0 )
  {
    return (NULL);
  } else
  {
    default_actions <- (default_actions[1,1] > 0);
  }
  
  if( default_actions )
  {
    # Ok, are there defaults defined?
    default_actions_defined <- sqlQ(x, query=paste0("SELECT COUNT(*)",
                                                    " FROM ",qfq_get(x, 'name', 'pr'),
                                                    " WHERE ",qs_get(x, 'patid', 'pr')," = '*'",
                                                    " AND ",qs_get(x, 'category', 'pr')," = '*'",
                                                    " ;"),
                                    err_msg=paste0("Error retreiving the defaults for the processings table '",get(x, 'name', 'pr'),"'!\n"), just_execute=FALSE);
    if( is.null(default_actions_defined) || nrow(default_actions_defined) == 0 )
    {
      return (NULL);
    } else
    {
      default_actions_defined <- (default_actions_defined[1,1] > 0);
    }
    
    if( default_actions_defined )
    {
      # Ok: create (if necessary) a new processings table and replace the default actions by the defaults:
      tmp_procs_table <- paste0(get(x, 'prefix'),'tmp_',get(x, 'name', 'pr', append_prefix_to_table_name=FALSE));
      
      if( !(tmp_procs_table %in% list_tables(x)) )
      {
        # Seems not to exist: create it:
        if( !table_create(x, tmp_procs_table, duplicate_from=get(x, 'name', 'pr'), clear_if_exists=TRUE) ) return (NULL);
      } else
      {
        # Seems to already exist: delete any entries it might have:
        if( !table_clear(x, tmp_procs_table) ) return (NULL);
      }
      
      # Copy the non-"*" entries from the processing table:
      if( is.null(sqlQ(x, query=paste0("INSERT INTO ",qfq_get(x,table_name=tmp_procs_table),
                                       " (",qs_get(x, 'patid', 'pr'),", ",qs_get(x, 'category', 'pr'),", ",qs_get(x, 'action', 'pr'),")",
                                       " SELECT ",qs_get(x, 'patid', 'pr'),", ",qs_get(x, 'category', 'pr'),", ",qs_get(x, 'action', 'pr'),
                                       " FROM ",qfq_get(x, 'name', 'pr'),
                                       " WHERE ",qs_get(x, 'action', 'pr')," <> '*'",
                                       " ;"),
                       err_msg=paste0("Error copying the non-defaults from the processings into the temporary database '",tmp_procs_table,"'!\n"), just_execute=TRUE)) ) return (NULL);
      
      # Insert the defaults corresponsind to the "*" entries from the processing table:
      if( is.null(sqlQ(x, query=paste0("INSERT INTO ",qfq_get(x,table_name=tmp_procs_table),
                                       " (",qs_get(x, 'patid', 'pr'),", ",qs_get(x, 'category', 'pr'),", ",qs_get(x, 'action', 'pr'),")",
                                       " SELECT ",
                                       qs(x,'a'),".",qs_get(x, 'patid', 'pr'),",",
                                       qs(x,'a'),".",qs_get(x, 'category', 'pr'),",",
                                       qs(x,'b'),".",qs_get(x, 'action', 'pr'),
                                       " FROM ",qfq_get(x, 'name', 'pr')," ",qs(x,'a'),", ",qfq_get(x, 'name', 'pr')," ",qs(x,'b'),
                                       " WHERE ",qs(x,'a'),".",qs_get(x, 'action', 'pr')," = '*'",
                                       " AND ",qs(x,'b'),".",qs_get(x, 'patid', 'pr')," = '*'",
                                       " AND ",qs(x,'b'),".",qs_get(x, 'category', 'pr')," = '*'",
                                       " ;"),
                       err_msg=paste0("Error solving the default actions in the temporary database '",tmp_procs_table,"'!\n"), just_execute=TRUE)) ) return (NULL);
      
      # All good: use this temporary table as the processing table:
      x$db_prtable_use_temp_table <- tmp_procs_table;
    }
  }
  
  
  # The classes table
  # Solve the {} referencing to another classes within a class definition
  
  # Do we need to do anything about this?
  ref_classes <- sqlQ(x, query=paste0("SELECT *",
                                      " FROM ",qfq_get(x, 'name', 'mc'),
                                      " WHERE ",qs_get(x, 'class', 'mc')," LIKE '%{%}%'",
                                      " ;"),
                      err_msg=NA, just_execute=FALSE);
  if( !is.null(ref_classes) && nrow(ref_classes) > 0 )
  {
    # There's at least one {} reference!
    # Ok: create (if necessary) a new table and copy everything in it:
    tmp_class_table <- paste0(get(x, 'prefix'),'tmp_',get(x, 'name', 'mc', append_prefix_to_table_name=FALSE));
    
    if( !(tmp_class_table %in% list_tables(x)) )
    {
      # Seems not to exist: create it:
      if( !table_create(x, tmp_class_table, duplicate_from=get(x, 'name', 'mc'), clear_if_exists=TRUE) ) return (NULL);
    } else
    {
      # Seems to already exist: delete any entries it might have:
      if( !table_clear(x, tmp_class_table) ) return (NULL);
    }
    
    # Copy everything from the classes table:
    if( is.null(sqlQ(x, query=paste0("INSERT INTO ",qfq_get(x,table_name=tmp_class_table),
                                     "SELECT * FROM ",qfq_get(x, 'name', 'mc'),
                                     " ;"),
                     err_msg=paste0("Error compying the non-defaults from the classes table into the temporary database '",tmp_class_table,"'!\n"), just_execute=TRUE)) ) return (NULL);

    # Replace the references by their definitions:
    max_iterations <- 256; # the maximum depth of references to be solved
    while( !is.null(ref_classes) && nrow(ref_classes) > 0 && max_iterations > 0 )
    {
      ref_classes$solved <- as.character(ref_classes[,get(x, 'class', 'mc')]);
      for( i in 1:nrow(ref_classes) )
      {
        updated_class <- FALSE;
        
        s <- as.character(ref_classes[i,get(x, 'class', 'mc')]);
        
        # Find the references to classes and extract their names (if any):
        n <- gregexpr("\\{[^\\}]+\\}", s)[[1]];
        if( length(n) == 1 && n == (-1) )
        {
          # No match -- what's going on?
          .msg(paste0("Error finding class match {} where one should have been: '",s,"'!\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"));
        } else
        {
          # Extract the names:
          class_names <- substring(s, n+1, n+attr(n,"match.length")-2);
          # Check for recursions:
          if( any(class_names == ref_classes[i,get(x, 'mcid', 'mc')]) )
          {
            # Recursion detected!
            .msg(paste0("Medication class definitions cannot be recursive, but '",as.character(ref_classes[i,get(x, 'mcid', 'mc')]),"' seems to be!\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"));
            return (NULL);
          } else
          {
            # Replace the references by their definitions:
            for( cn in class_names )
            {
              tmp <- sqlQ(x, query=paste0("SELECT ",qs_get(x, 'class', 'mc'),
                                          " FROM ",qfq_get(x,table_name=tmp_class_table),
                                          " WHERE ",qs_get(x, 'mcid', 'mc')," = '",cn,"'",
                                          " ;"),
                          err_msg=paste0("Cannot find the definition of medication class '",cn,"'!\n"), just_execute=FALSE);
              if( !is.null(tmp) )
              {
                if( nrow(tmp) > 1 )
                {
                  .msg(paste0("The definition of medication class '",cn,"' is not unique!\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"));
                } else
                {
                  ref_classes$solved[i] <- gsub(paste0("{",cn,"}"), paste0("(",tmp[1,1],")"), ref_classes$solved[i], fixed=TRUE);
                  updated_class <- TRUE;
                }
              }
            }
          }
        }
        
        # If updated, write it back to the SQL database:
        if( updated_class )
        {
          if( is.null(sqlQ(x, query=paste0("UPDATE ",qfq_get(x,table_name=tmp_class_table),
                                           " SET ",qs_get(x, 'class', 'mc')," = '",ref_classes$solved[i],"'",
                                           " WHERE ",qs_get(x, 'mcid', 'mc')," = '",ref_classes[i,get(x, 'mcid', 'mc')],"'",
                                           " AND ",qs_get(x, 'class', 'mc')," = '",ref_classes[i,get(x, 'class', 'mc')],"'",
                                           " ;"),
                           err_msg=paste0("Error updating the temporary database '",tmp_class_table,"'!\n"), just_execute=TRUE)) ) return (NULL);
        }
      }
      
      # Redo the whole thing again until there's no more {} refs left:
      ref_classes <- NULL;
      try(ref_classes <- sqlQ(x, query=paste0("SELECT *",
                                              " FROM ",qfq_get(x,table_name=tmp_class_table),
                                              " WHERE ",qs_get(x, 'class', 'mc')," LIKE '%{%}%'",
                                              " ;"),
                              err_msg=NA, just_execute=FALSE), silent=TRUE);
      
      max_iterations <- (max_iterations - 1);
    }
    if( max_iterations == 0 )
    {
      .msg(paste0("Too deep medication class references {}: not all have been solved, please reduce this referencing depth!\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"));
      return (NULL);
    }
    
    # All good: use this temporary table as the medication classes table:
    x$db_mctable_use_temp_table <- tmp_class_table;
  }  
  
  
  .msg(paste0("Preprocessing of the database ended.\n\n"), x$log_file, "m");
  
  # Return this (possibly modified) object:
  return (x);
}


##
## Table manipulation ####
##

# Check if table exists in the database:
table_exists <- function(x, tbname) UseMethod("table_exists")
table_exists.SQL_db <- function(x, tbname)
{
  return (tbname %in% list_tables(x));
}


# Create table (if not already there):
table_create <- function(x, tbname, duplicate_from=NA, clear_if_exists=TRUE) UseMethod("table_create")
table_create.SQL_db <- function(x, tbname, duplicate_from=NA, clear_if_exists=TRUE)
{
  if( !table_exists(x, tbname) )
  {
    # Does not exist yet: create it de novo:
    if( is.na(duplicate_from) )
    {
      # Create a new table:
      if( is.null(sqlQ(x, query=paste0("CREATE TABLE ",qfq_get(x, 'name', 'ev')," ;"),
                       err_msg=paste0("Error creating the table '",tbname,"'!\n"), just_execute=TRUE)) ) return (FALSE);
    } else
    {
      # Duplicate an existing table:
      if( !table_exists(x, duplicate_from) )
      {
        .msg(paste0("Error creating the table '",tbname,"': the table that should be duplicated '",duplicate_from,"' does not seem to exist!\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"));
        return (FALSE);
      } else
      {
        # Duplication is database-dependent:
        if( x$db_type %in% c("mariadb", "mysql") )
        {
          try(tmp <- DBI::dbExecute(x$db_connection, 
                                    paste0("CREATE TABLE ",qs(x,tbname)," LIKE ",qs(x,duplicate_from)," ;")),
              silent=TRUE);
          if( is.null(tmp) )
          {
            .msg(paste0("Error duplicating table '",tbname,"' from table '",duplicate_from,"'!\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"));
            return (NULL);
          }
        } else  if( x$db_type == "mssql" )
        {
          try(tmp <- RODBC::sqlQuery(x$db_connection, 
                                     paste0("SELECT * INTO ",qfq_get(x,table_name=tbname),
                                            " FROM ",qfq_get(x,table_name=duplicate_from),
                                            " WHERE 1 = 0 ;")),
              silent=TRUE);
          if( is.null(tmp) )
          {
            .msg(paste0("Error duplicating table '",tbname,"' from table '",duplicate_from,"'!\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"));
            return (NULL);
          }
        }
      }
    }
  }
  
  # Clear it:
  if( clear_if_exists ) return (table_clear(x,tbname));
  
  return (TRUE); # all seems fine...
}


# Clear table:
table_clear <- function(x, tbname) UseMethod("table_clear")
table_clear.SQL_db <- function(x, tbname)
{
  if( table_exists(x, tbname) )
  {
    # It does exist:
    return( !is.null(sqlQ(x, query=paste0("TRUNCATE TABLE ",qfq_get(x,table_name=tbname)," ;"),
                          err_msg=paste0("Error clearing the table '",tbname,"'!\n"), just_execute=TRUE)) );
  } else
  {
    # It does not exist:
    .msg(paste0("Can't clear the non-existing table '",tbname,"'!\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"));
    return (FALSE);
  }
}


# Remove (drop) table:
table_drop <- function(x, tbname) UseMethod("table_drop")
table_drop.SQL_db <- function(x, tbname)
{
  if( table_exists(x, tbname) )
  {
    # It does exist:
    return( !is.null(sqlQ(x, query=paste0("DROP TABLE ",qfq_get(x,table_name=tbname)," ;"),
                          err_msg=paste0("Error removing the table '",tbname,"'!\n"), just_execute=TRUE)) );
  } else
  {
    # It does not exist:
    .msg(paste0("Can't remove the non-existing table '",tbname,"'!\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"));
    return (FALSE);
  }
}


# List tables in the database:
list_tables <- function(x) UseMethod("list_tables")
list_tables.SQL_db <- function(x)
{
  db_tables <- NULL;
  
  if( x$db_type %in% c("mariadb", "mysql", "sqlite") )
  {
    db_tables <- DBI::dbListTables(x$db_connection);
  } else if( x$db_type == "mssql" )
  {
    db_list <- RODBC::sqlQuery(x$db_connection, paste0("SELECT * FROM ",qs(x,x$db_name),".information_schema.tables;")); # list the tables
    db_tables <- paste0(db_list$TABLE_SCHEMA,".",db_list$TABLE_NAME); # reconstruct the tables' names
  }

  # Return the list tables' names:
  return (db_tables);
}

# Get the column info for a given table:
get_cols_info <- function(x, db_table) UseMethod("get_cols_info")
get_cols_info.SQL_db <- function(x, db_table)
{
  db_cols_info <- NULL;
  
  if( x$db_type %in% c("mariadb", "mysql") )
  {
    # Get columns:
    tmp <- NULL;
    try(tmp <- DBI::dbGetQuery(x$db_connection, paste0("SHOW COLUMNS FROM ",qs(x,db_table),";")), silent=TRUE);
    if( !is.null(tmp) && inherits(tmp, "data.frame") )
    {
      n <- NULL;
      # Get number of rows:
      try(n <- DBI::dbGetQuery(x$db_connection, paste0("SELECT COUNT(*) FROM ",qs(x,db_table),";")), silent=TRUE);
      if( is.null(n) || !inherits(n, "data.frame") || nrow(n) != 1 || ncol(n) != 1 )
      {
        # Error retreiving the number of rows:
        n <- NA;
        .msg(paste0("Error getting the number of rows for table '",db_table,"'!\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"));
      } else
      {
        n <- as.numeric(n[1,1]);
      }
      
      db_cols_info <- data.frame("table"=db_table, "nrow"=n, "column"=tmp$Field, "type"=tmp$Type, "null"=tmp$Null, "key"=tmp$Key);
    }
  } else if( x$db_type == "sqlite" )
  {
    # Get columns:
    tmp <- NULL;
    try(tmp <- DBI::dbGetQuery(x$db_connection, paste0("PRAGMA table_info(",qs(x,db_table),");")), silent=TRUE);
    if( !is.null(tmp) && inherits(tmp, "data.frame") )
    {
      n <- NULL;
      try(n <- DBI::dbGetQuery(x$db_connection, paste0("SELECT COUNT(*) FROM ",qs(x,db_table),";")), silent=TRUE);
      if( is.null(n) || !inherits(n, "data.frame") || nrow(n) != 1 || ncol(n) != 1 )
      {
        # Error retreiving the number of rows:
        n <- NA;
        .msg(paste0("Error getting the number of rows for table '",db_table,"'!\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"));
      } else
      {
        n <- as.numeric(n[1,1]);
      }
      
      db_cols_info <- data.frame("table"=db_table, "nrow"=n, "column"=tmp$name, "type"=tmp$type, "null"=(tmp$notnull == 0), "key"=(tmp$pk != 0));
    }
  } else if( x$db_type == "mssql" )
  {
    n <- NULL;
    try(n <- RODBC::sqlQuery(x$db_connection, paste0("SELECT COUNT(*) FROM ",qfq_get(x,table_name=db_table),";")), silent=TRUE);
    if( is.null(n) || !inherits(n, "data.frame") || nrow(n) != 1 || ncol(n) != 1 )
    {
      # Error retreiving the number of rows:
      n <- NA;
      .msg(paste0("Error getting the number of rows for table '",db_table,"'!\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"));
    } else
    {
      n <- as.numeric(n[1,1]);
    }
    db_list <- RODBC::sqlQuery(x$db_connection, paste0("SELECT * FROM ",qs_get(x, 'name'),".INFORMATION_SCHEMA.COLUMNS ORDER BY ORDINAL_POSITION;"));
    if( !is.null(db_list) && nrow(db_list) > 0 )
    {
      db_list <- db_list[ paste0(db_list$TABLE_SCHEMA,".",db_list$TABLE_NAME) == db_table, ];
      if( !is.null(db_list) && nrow(db_list) > 0 )
      {
        db_cols_info <- data.frame("table"=db_table, "nrow"=n, "column"=db_list$COLUMN_NAME, "type"=db_list$DATA_TYPE, "null"=db_list$IS_NULLABLE, "key"=NA);
      }
    }
  }
  
  if( is.null(db_cols_info) ) .msg(paste0("Error getting the columns info for table '",db_table,"'!\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"));
  
  # Return the column info:
  return (db_cols_info);
}

# Get the column names for a given table:
get_col_names <- function(x, db_table) UseMethod("get_col_names")
get_col_names.SQL_db <- function(x, db_table)
{
  db_cols_info <- get_cols_info(x, db_table);
  if( !is.null(db_cols_info) && nrow(db_cols_info) > 0 )
  {
    return (as.character(db_cols_info$column));
  } else
  {
    return (NULL);
    .msg(paste0("Error getting the column names!\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"));
  }
}


##
## Results tables ####
##

write_retable_entry <- function(x, id, procid, class="", type="", proc="", params="", estimate=NA, estimate_type=NA, plot_jpg="", plot_html="") UseMethod("write_retable_entry")
write_retable_entry.SQL_db <- function(x, id, procid, class="", type="", proc="", params="", estimate=NA, estimate_type=NA, plot_jpg="", plot_html="")
{
  # File to blob:
  file2blob <- function(x, file_name)
  {
    if( is.na(file_name)  || !file.exists(file_name) )
    {
      return ("NULL");
    } else
    {
      # Read the file as raw:
      file_raw <- readBin(file_name, n=file.size(file_name)+64, what="raw"); # allocate a bit more memory just in case...
      
      if( x$db_type == "mssql" )
      {
        return (paste0("CONVERT(VARBINARY(MAX), 0x",paste0(file_raw,collapse=""),")"));
      } else
      {
        return (paste0("X'",paste0(file_raw,collapse=""),"'"));
      }
    }
  }
  
  # Write all these info into the retable:
  result <- sqlQ(x, query=paste0("INSERT INTO ",qfq_get(x, 'name', 're'),
                                 "(",
                                 qs_get(x, 'patid', 'ev'),", ",
                                 qs_get(x, 'procid', 're'),", ",
                                 qs_get(x, 'estim', 're'),", ",
                                 qs_get(x, 'estim_type', 're'),", ",
                                 qs_get(x, 'jpg', 're'),", ",
                                 qs_get(x, 'html', 're'),
                                 ")",
                                 " VALUES (",
                                 "'",id,"', ", # id
                                 "'",procid,"', ", # reference to the processing key
                                 ifelse(is.na(estimate),"NULL",paste0("'",estimate,"'")),", ", # estimate
                                 ifelse(is.na(estimate_type),"NULL",paste0("'",estimate_type,"'")),", ", # estimate type
                                 file2blob(x,plot_jpg),", ", # the JPEG file as a blob
                                 file2blob(x,plot_html), # the HTML+SVG file as a blob
                                 ");"),
                 err_msg=paste0("Error writing into the events table '",get(x, 'name', 'ev'),"'!\n"), just_execute=TRUE);
  if( is.null(result) )
  {
    .msg(paste0("Error writing row to the results table '",get(x, 'name', 're'),"'!\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"));
    return (FALSE);
  } else
  {
    return (TRUE);
  }
  
  return (TRUE);
}

get_retable_resid_for_results <- function(x, id, procid, estimate_type) UseMethod("get_retable_resid_for_results")
get_retable_resid_for_results.SQL_db <- function(x, id, procid, estimate_type)
{
  # Get the resid of the previous insertion in the retable:
  resid_ref <- sqlQ(x, query=paste0("SELECT ", qs_get(x, 'resid', 're'),
                                    " FROM ", qfq_get(x, 'name', 're'), 
                                    " WHERE ", qs_get(x, 'patid', 're'), " = '", id, "'", 
                                    " AND ", qs_get(x, 'procid', 're'), " = '", procid, "'",
                                    " AND ", qs_get(x, 'estim_type', 're'), " = '", estimate_type, "'",
                                    ";"),
                    err_msg=paste0("Error writing into the events table '",get(x, 'name', 'ev'),"'!\n"), just_execute=FALSE);
  if( is.null(resid_ref) || nrow(resid_ref) != 1 )
  {
    # Error identifying the last inserted row!
    .msg(paste0("Error identifying the last row written to the results table '",get(x, 'name', 're'),"'!\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"));
    return (NULL);
  } else
  {
    return (resid_ref[1,1]);
  }
  
  return (NULL);
}

write_swtable_entry <- function(x, resid=-1, cma=NULL) UseMethod("write_swtable_entry")
write_swtable_entry.SQL_db <- function(x, resid=-1, cma=NULL)
{
  if( is.null(cma) || nrow(cma) == 0 )
  {
    return (FALSE);
  }
  # Convert the dates to the expected format for SQL's DATE:
  cma$window.start <- as.character(as.Date(cma$window.start, format="%m/%d/%Y"), format="%Y-%m-%d");
  cma$window.end   <- as.character(as.Date(cma$window.end,   format="%m/%d/%Y"), format="%Y-%m-%d");

  # Write all these info into the swtable:
  result <- sqlQ(x, query=paste0("INSERT INTO ",qfq_get(x, 'name', 'sw'),
                                 " (",
                                 qs_get(x, 'resid', 'sw'),", ",
                                 qs_get(x, 'patid', 'sw'),", ",
                                 qs_get(x, 'wndid', 'sw'),", ",
                                 qs_get(x, 'start', 'sw'),", ",
                                 qs_get(x, 'end', 'sw'),", ",
                                 qs_get(x, 'estim', 'sw'),
                                 ")",
                                 " VALUES ",
                                 paste0("(",
                                        vapply(1:nrow(cma), 
                                               function(i) 
                                                 paste0("'", resid, "', ",
                                                        "'", cma[i,get(x, 'patid', 'ev')], "', ",
                                                        "'", cma$window.ID[i], "', ",
                                                        "'", cma$window.start[i], "', ",
                                                        "'", cma$window.end[i], "', ",
                                                        ifelse(!is.na(cma$CMA[i]), round(cma$CMA[i],4), "NULL")), 
                                               character(1)),
                                        ")",
                                        collapse=", "),
                                 ";"),
                 err_msg=paste0("Error writing into the events table '",get(x, 'name', 'ev'),"'!\n"), just_execute=TRUE);
  if( is.null(result) )
  {
    .msg(paste0("Error writing row to the sliding windows results table '",get(x, 'name', 'sw'),"'!\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"));
    return (FALSE);
  } else
  {
    return (TRUE);
  }
  
  return (TRUE);
}

write_petable_entry <- function(x, resid=-1, cma=NULL) UseMethod("write_petable_entry")
write_petable_entry.SQL_db <- function(x, resid=-1, cma=NULL)
{
  if( is.null(cma) || nrow(cma) == 0 )
  {
    return (FALSE);
  }
  # Convert the dates to the expected format for SQL's DATE:
  cma$episode.start <- as.character(as.Date(cma$episode.start, format="%m/%d/%Y"), format="%Y-%m-%d");
  cma$episode.end   <- as.character(as.Date(cma$episode.end,   format="%m/%d/%Y"), format="%Y-%m-%d");
  
  # Write all these info into the swtable:
  result <- sqlQ(x, query=paste0("INSERT INTO ",qfq_get(x, 'name', 'pe'),
                                 " (",
                                 qs_get(x, 'resid', 'pe'),", ",
                                 qs_get(x, 'patid', 'pe'),", ",
                                 qs_get(x, 'epid', 'pe'),", ",
                                 qs_get(x, 'start', 'pe'),", ",
                                 qs_get(x, 'gap', 'pe'),", ",
                                 qs_get(x, 'duration', 'pe'),", ",
                                 qs_get(x, 'end', 'pe'),", ",
                                 qs_get(x, 'estim', 'pe'),
                                 ")",
                                 " VALUES ",
                                 paste0("(",
                                        vapply(1:nrow(cma), 
                                               function(i) 
                                                 paste0("'", resid, "', ",
                                                        "'", cma[i,get(x, 'patid', 'ev')], "', ",
                                                        "'", cma$episode.ID[i], "', ",
                                                        "'", cma$episode.start[i], "', ",
                                                        "'", cma$end.episode.gap.days[i], "', ",
                                                        "'", cma$episode.duration[i], "', ",
                                                        "'", cma$episode.end[i], "', ",
                                                        ifelse(!is.na(cma$CMA[i]), round(cma$CMA[i],4), "NULL")), 
                                               character(1)),
                                        ")",
                                        collapse=", "),
                                 ";"),
                 err_msg=paste0("Error writing row to the per episode results table '",get(x, 'name', 'pe'),"'!\n"), just_execute=TRUE);
  if( is.null(result) )
  {
    .msg(paste0("Error writing row to the per episode results table '",get(x, 'name', 'pe'),"'!\n"), x$log_file, ifelse(x$stop_on_database_errors,"e","w"));
    return (FALSE);
  } else
  {
    return (TRUE);
  }
  
  return (TRUE);
}


##
## List and get patients from events table ####
##

list_patients <- function(x, with_updated_info_only=TRUE) UseMethod("list_patients")
list_patients.SQL_db <- function(x, with_updated_info_only=TRUE)
{
  patient_ids <- NULL;
  if( !with_updated_info_only || # specifically requested to use all patients, or
      is.null(up_info <- get_cols_info(x, get(x, 'name', 'up'))) || up_info$nrow == 0 ) # the updated_info table is not defined or empty
  {
    # List all patients in the events table:
    tmp <- sqlQ(x, query=paste0("INSERT INTO ",qfq_get(x, 'name', 'ev')," VALUES (",
                                paste0("'",as.character(d[i,]),"'",collapse=","),
                                ");"),
                err_msg=paste0("Error retrieving the list of patients to processes!\n"), just_execute=FALSE);
    if( !is.null(tmp) && inherits(tmp, "data.frame") && nrow(tmp) > 0 ) patient_ids <- as.character(tmp[,1]);
  } else
  {
    # List only those in the events table that are also mentioned in the updated_info table:
    tmp <- sqlQ(x, query=paste0("SELECT DISTINCT ",qfq_get(x, 'patid', 'ev'),
                                " FROM ",qfq_get(x, 'name', 'ev'),
                                " INNER JOIN ",qfq_get(x, 'name', 'up'),
                                " ON ",qfq_get(x, 'patid', 'ev')," = ",qfq_get(x, 'patid', 'up'),
                                ";"),
                err_msg=paste0("Error retrieving the list of patients to processes!\n"), just_execute=FALSE);
    if( !is.null(tmp) && inherits(tmp, "data.frame") && nrow(tmp) > 0 ) patient_ids <- as.character(tmp[,1]);
  }

  # Return patient ids:
  return (patient_ids);
}


get_evtable_patients_info <- function(x, patient_id, cols=NA, maxrows=NA) UseMethod("get_evtable_patients_info")
get_evtable_patients_info.SQL_db <- function(x, patient_id, cols=NA, maxrows=NA)
{
  db_pat_info <- NULL;
  
  tmp <- sqlQ(x, query=paste0("SELECT ",
                                   ifelse(is.na(cols), "*", paste0(qs(x,cols),collapse=",")),
                                   " FROM ",qfq_get(x, 'name', 'ev'),
                                   " WHERE ",qs_get(x, 'patid', 'ev'),
                                   " IN (",paste0("'",patient_id,"'",collapse=","),")",
                                   ifelse(is.na(maxrows), "",paste0("LIMIT ",maxrows)),
                                   " ;"),
                   err_msg=paste0("Error retreiving info for patient(s) ",paste0("'",patient_id,"'",collapse=", "),"!\n"), just_execute=FALSE);
  if( !is.null(tmp) && inherits(tmp, "data.frame") && nrow(tmp) > 0 ) db_pat_info <- tmp;

  # Return the patient info:
  return (db_pat_info);
}


##
## Processings ####
##

# Get the list of possible processing for a list patient ids:
get_processings_for_patient <- function(x, patient_id) UseMethod("get_processings_for_patient")
get_processings_for_patient.SQL_db <- function(x, patient_id)
{
  # Select the actions specific to the patient(s) but also the defaults:
  db_procs <- sqlQ(x, query=paste0("SELECT *",
                                   " FROM ",qfq_get(x, 'name', 'pr'),
                                   " INNER JOIN ",qfq_get(x, 'name', 'mc'),
                                   " ON ",qfq_get(x, 'category', 'pr')," = ",qfq_get(x, 'mcid', 'mc'),
                                   " INNER JOIN ",qfq_get(x, 'name', 'ac'),
                                   " ON ",qfq_get(x, 'action', 'pr')," = ",qfq_get(x, 'actid', 'ac'),
                                   " AND ",qfq_get(x, 'patid', 'pr')," IN ('*', ",paste0("'",patient_id,"'",collapse=","),")",
                                   ";"),
                   err_msg=paste0("Error retreiving the default processings for patient(s) ",paste0("'",patient_id,"'",collapse=", "),"!\n"), just_execute=FALSE);
  if( is.null(db_procs) || nrow(db_procs) == 0 ) return (NULL);
    
  # Re-arrange it in the "procid", "patid", "mcid", "actid", "class", "type", "action" & "params" format:
  db_procs <- db_procs[, c(get(x, 'procid', 'pr'), get(x, 'patid', 'pr'), get(x, 'category', 'pr'), get(x, 'action', 'pr'),
                           get(x, 'class', 'mc'), get(x, 'action', 'ac'), get(x, 'params', 'ac'))];
  names(db_procs) <- c('procid', 'patid', 'mcid', 'actid', 'class', 'action', 'params');
  
  # If a specific processing is defined, discard the defaults:
  if( any(s <- (db_procs$patid != "*")) ) db_procs <- db_procs[s,];
  
  # Parse the action into a process and its type:
  db_procs <- cbind(db_procs, 
                    do.call(rbind, lapply(db_procs$action, function(s)
                      {
                        tmp2 <- trimws(s);
                        # Type of processing and name:
                        if( substr(tmp2,1,nchar("plot.")) == "plot." )
                        {
                          return (c("type"="plot", "proc"=substr(tmp2,nchar("plot.")+1,nchar(tmp2))));
                        } else
                        {
                          return (c("type"="CMA", "proc"=tmp2));
                        }
                      })));
  
  # Remove the trailing spaces:
  db_procs$class  <- trimws(db_procs$class);
  db_procs$action <- trimws(db_procs$action);
  db_procs$params[ is.na(db_procs$params) ] <- ""; # make sure the empty params are represented by emty strings
  db_procs$params <- trimws(db_procs$params);

  return (db_procs);
}

# Apply the required selection to this patient:
select_events_for_procs_class <- function(x, patient_info, procs_classs, proc_classes_for_patient) UseMethod("select_events_for_procs_class")
select_events_for_procs_class.SQL_db <- function(x, patient_info, procs_class, proc_classes_for_patient)
{
  if( is.null(patient_info) || nrow(patient_info) == 0 || 
      is.null(procs_class) || length(procs_class) != 1 )
  {
    # Empty patient info or processing class: nothing to do
    return (NULL);
  }
  
  # Special case for full selection?
  if( is.na(procs_class) || procs_class %in% c("", "*") )
  {
    # Select all!
    return (rep(TRUE, nrow(patient_info)));
  }
  
  # Special case for "all others"?
  if( procs_class == "@" )
  {
    # All medications not otherwise selected by the other class definitions for this patient (or all, if not classes defined):
    if( all(proc_classes_for_patient$class == "@") )
    {
      # Select all!
      return (rep(TRUE, nrow(patient_info)));
    } else
    {
      # Put together all the other class definitions:
      all_other_classes <- paste("(", proc_classes_for_patient$class[ !(proc_classes_for_patient$class %in% c("*", "@")) ], ")", collapse=" | ");
      # and negate it:
      procs_class <- paste0("!(", all_other_classes, ")");
    }
  }
  
  # Do the specific selection:
  # Get the medication classes for this patient:
  if( is.na(get(x, 'category', 'ev')) )
  {
    # No medication classes:
    .msg(paste0("Warning: medication classes not defined: selecting all events...\n"), x$log_file, "w");
    return (rep(TRUE, nrow(patient_info))); # select all
  }
  pat_classes <- patient_info[ , get(x, 'category', 'ev') ]; 
  if( is.null(pat_classes) )
  {
    # No medication classes:
    .msg(paste0("Warning: medication classes not defined: selecting all events...\n"), x$log_file, "w");
    return (rep(TRUE, nrow(patient_info))); # select all
  }
  
  # Transform the procs_class specification into a logical expression to be evaluated on pat_classes:
  procs_class_expr <- NULL;
  procs_class2 <- procs_class;
  procs_class2 <- gsub("]", "')", # replace "[ XX ]" by the actual R test "(pat_classes == 'XX')"
                       gsub("[", "(pat_classes == '",
                            procs_class2, 
                            fixed=TRUE), 
                       fixed=TRUE);
  try(procs_class_expr <- parse(text=procs_class2), silent=TRUE);
  if( is.null(procs_class_expr) )
  {
    .msg(paste0("Error parsing the medication class definition '",procs_class,"'!\n"), x$log_file, ifelse(x$stop_on_processing_errors,"e","w"));
    return (NULL);
  }
  
  # Evaluate the expression:
  s <- eval(procs_class_expr);
  if( (!is.na(s) || !is.null(s)) && !is.logical(s) )
  {
    .msg(paste0("Error applying the medication class definition '",procs_class,"' to the data: the result should be logical!\n"), x$log_file, ifelse(x$stop_on_processing_errors,"e","w"));
    return (NULL);
  }
  
  # Return it:
  return (s);
}

# Apply the required processing to this selection:
apply_procs_action_for_class <- function(x, patient_info, procs_action) UseMethod("apply_procs_action_for_class")
apply_procs_action_for_class.SQL_db <- function(x, patient_info, procs_action)
{
  if( is.null(patient_info) || nrow(patient_info) == 0 || 
      is.null(procs_action) || nrow(procs_action) != 1 )
  {
    # Empty patient info or processing actions: nothing to do
    return (NULL);
  }
  
  # Transform the action procs_action into an expression to be evaluated on patient_info
  # simply create a function call to the CMA (and possibly, the plotting) using the fact that the irelevant params will be ignored:
  procs_action_expr <- NULL;
  procs_action_call <- paste0(procs_action$proc[1], 
                              "(",
                              "data=patient_info, ",
                              "ID.colname='",get(x, 'patid', 'ev'),"', ",
                              "event.date.colname='",get(x, 'date', 'ev'),"', ",
                              "event.duration.colname='",get(x, 'duration', 'ev'),"', ",
                              ifelse(!is.na(get(x, 'perday', 'ev')), paste0("event.daily.dose.colname='",get(x, 'perday', 'ev'),"', "), ""),
                              ifelse(!is.na(get(x, 'category', 'ev')), paste0("medication.class.colname='",get(x, 'category', 'ev'),"' "), ""),
                              ifelse(procs_action$params[1] != "", paste0(", ",procs_action$params[1]),""), 
                              ")");
  try(procs_action_expr <- parse(text=procs_action_call), silent=TRUE);
  if( is.null(procs_action_expr) )
  {
    .msg(paste0("Error parsing the action '",procs_action_call,"'!\n"), x$log_file, ifelse(x$stop_on_processing_errors,"e","w"));
    return (NULL);
  }
  
  # Evaluate the expression:
  cma <- NULL; msg <- NULL;
  try(msg <- capture.output(cma <- eval(procs_action_expr), type="message"), silent=TRUE);
  if( is.null(cma) || is.na(cma) || !(inherits(cma, "CMA0") || inherits(cma, "CMA_sliding_window") || inherits(cma, "CMA_per_episode")) )
  {
    # Serious error:
    .msg(paste0("Error applying the action definition '",procs_action_call,"' to the data!\n"), x$log_file, ifelse(x$stop_on_processing_errors,"e","w"));
    return (NULL);
  }
  if( !is.null(msg) && length(msg) > 0 )
  {
    # Potential warnings generated by CMA:
    .msg(paste0("Warning(s) generated by the computation of the CMA:\n",paste0("  >>  ",msg,collapse="\n"),"\n"), x$log_file, "m");
  }
  
  # Should we plot it?
  cma_plots <- NULL;
  if( tolower(procs_action$type[1]) == "plot" )
  {
    procs_action_expr <- NULL;
    procs_action_call <- paste0("plot(cma, export.formats=c('html'), generate.R.plot=FALSE",
                                ifelse(procs_action$params[1] != "", paste0(", ",procs_action$params[1]),""), 
                                ")");
    try(procs_action_expr <- parse(text=procs_action_call), silent=TRUE);
    if( is.null(procs_action_expr) )
    {
      .msg(paste0("Error parsing the action '",procs_action_call,"'!\n"), x$log_file, ifelse(x$stop_on_processing_errors,"e","w"));
      return (NULL);
    }
    
    # Evaluate the expression:
    plot_file_names <- NULL; msg <- NULL;
    try(msg <- capture.output(plot_file_names <- eval(procs_action_expr), type="message"), silent=TRUE);
    if( is.null(plot_file_names) || length(plot_file_names) < 2 )
    {
      # Issues generating the plots:
      .msg(paste0("Error applying the action definition '",procs_action_call,"' to the data: the result should be a valid plot!\n"), x$log_file, "w");
      return (NULL);
    } else
    {
      # Save the plots:
      # Create the ZIP holding the HTML document and JPG placeholder:
      zip_file_name <- paste0(plot_file_names["html"],".zip");
      if( utils::zip(zipfile=zip_file_name, files=plot_file_names, flags="-9Xjq") # max compression, don't store the path, suppress messages
          != 0 ) # the return code for OK should be 0
      {
        # Errors zipping:
        .msg(paste0("Error creating the zip containing the HTML document and the JPG placeholder!\n"), x$log_file, ifelse(x$stop_on_processing_errors,"e","w"));
        return (NULL);
      }
      
      # Store these files:
      cma_plots <- list("jpg"=plot_file_names["jpg-placeholder"], "html"=zip_file_name);
    }
    if( !is.null(msg) && length(msg) > 0 )
    {
      # Potential warnings generated by plotting:
      .msg(paste0("Warning(s) generated by the plotting of the CMA:\n",paste0("  >>  ",msg,collapse="\n"),"\n"), x$log_file, "m");
    }
  }
  
  # Return the results:
  return (list("id"=patient_info[ 1, get(x, 'patid', 'ev') ],
               "procid"=procs_action$procid[1], "class"=procs_action$class[1], "type"=procs_action$type[1], "proc"=procs_action$proc[1], "params"=procs_action$params[1], 
               "cma"=cma, 
               "plots"=cma_plots));
}

# Apply the required processing to this selection:
upload_procs_results <- function(x, procs_results) UseMethod("upload_procs_results")
upload_procs_results.SQL_db <- function(x, procs_results)
{
  if( is.null(procs_results) || length(procs_results) != 8 )
  {
    # Nothing to do:
    return (FALSE);
  }
  
  # Write the results info:
  id  <- ifelse(is.na(procs_results$id), "", as.character(procs_results$id));
  procid <- procs_results$procid; # the procid must be defined! 
  estimate_type <- ifelse(is.null(procs_results$cma) || is.null(getCMA(procs_results$cma)), 
                          NA,
                          ifelse(inherits(procs_results$cma, "CMA0"), 
                                 "simple", 
                                 ifelse(inherits(procs_results$cma, "CMA_sliding_window"), 
                                        "sliding window", 
                                        ifelse(inherits(procs_results$cma, "CMA_per_episode"),
                                               "per episode",
                                               "unknown")
                                 )
                          ));
  ret_val <- write_retable_entry(x, 
                                 id        =id, 
                                 procid    =procid,
                                 class     =ifelse(is.na(procs_results$class),      "", as.character(procs_results$class)), 
                                 type      =ifelse(is.na(procs_results$type),       "", as.character(procs_results$type)), 
                                 proc      =ifelse(is.na(procs_results$proc),       "", as.character(procs_results$proc)), 
                                 params    =ifelse(is.na(procs_results$params),     "", as.character(procs_results$params)),
                                 estimate  =ifelse(is.null(procs_results$cma) || is.null(getCMA(procs_results$cma)) || !inherits(procs_results$cma, "CMA0"), 
                                                   NA, # not estimated or not a simple CMA -> mark it as NULL
                                                   round(getCMA(procs_results$cma)$CMA[1],4) # simple CMA -> use the first numeric value
                                                   ),
                                 estimate_type=estimate_type,
                                 plot_jpg  =if(!is.null(procs_results$plots)) procs_results$plots$jpg  else NA,
                                 plot_html =if(!is.null(procs_results$plots)) procs_results$plots$html else NA);
  # Clean the temporary files (if the case):
  if( !is.null(pat_procs_results$plots) )
  {
    if( !!is.null(procs_results$plots) && !is.null(pat_procs_results$plots$jpg)  && file.exists(pat_procs_results$plots$jpg) )  file.remove(pat_procs_results$plots$jpg);
    if( !!is.null(procs_results$plots) && !is.null(pat_procs_results$plots$html) && file.exists(pat_procs_results$plots$html) ) file.remove(pat_procs_results$plots$html);
  }
  if( !ret_val ) return (FALSE); # some error occured so return
  
  # Is this is a "complex" CMA?
  if( !is.null(procs_results$cma) && !is.null(getCMA(procs_results$cma)) && 
      (inherits(procs_results$cma, "CMA_sliding_window") || inherits(procs_results$cma, "CMA_per_episode")) )
  {
    # Get the resid of the previous insertion in the retable:
    resid_ref <- get_retable_resid_for_results(x, id, procid, estimate_type);
    if( is.null(resid_ref) )
    {
      # Error identifying the last inserted row!
      return (FALSE);
    }
    
    if( inherits(procs_results$cma, "CMA_sliding_window") )
    {
      # A sliding windows CMA: write it into the swtable:
      ret_val <- write_swtable_entry(x, resid_ref, getCMA(procs_results$cma));
      if( !ret_val ) return (FALSE); # some error occured so return
    } else if( inherits(procs_results$cma, "CMA_per_episode") )
    {
      # A per episodes CMA!
      ret_val <- write_petable_entry(x, resid_ref, getCMA(procs_results$cma));
      if( !ret_val ) return (FALSE); # some error occured so return
    }
  }
  
  return (TRUE);
}


##
## Database checks ####
##

# Check if the events table exists, contains the expected columns, and is not empty:
check_tables <- function(x) UseMethod("check_tables")
check_tables.SQL_db <- function(x)
{
  # Get the list of tables:
  db_tables <- list_tables(x);
  
  check_table <- function(x, tbname="events", tbcolumns=c("name", "patient_id", "date", "perday", "category", "duration"), 
                          check_empty=FALSE, stop_on_error=TRUE)
  {
    if( !(get(x, 'name', tbname) %in% db_tables) )
    {
      msg <- paste0("The required table '",get(x, 'name', tbname),"' does not seem to exist in the database!\n");
      .msg(msg, x$log_file, ifelse(stop_on_error,"e","w"));
      return (FALSE);
    }
    table_info <- get_cols_info(x, get(x, 'name', tbname));
    if( is.null(table_info) || nrow(table_info) == 0 )
    {
      msg <- paste0("Cannot get the information about the table '",get(x, 'name', tbname),"'!\n");
      .msg(msg, x$log_file, ifelse(stop_on_error,"e","w"));
      return (FALSE);
    }
    if( check_empty && table_info$nrow[1] == 0 )
    {
      msg <- paste0("The table '",get(x, 'name', tbname),"' seems empty!\n");
      .msg(msg, x$log_file, ifelse(stop_on_error,"e","w"));
      return (FALSE);
    }
    for( tbcol in setdiff(tbcolumns, "name") ) # "name" is not a column
    {
      if( !(get(x, tbcol, tbname) %in% table_info$column) )
      {
        msg <- paste0("The required column '",get(x, tbcol, tbname),"' does not seem to exist in the table '",get(x, 'name', tbname),"'!\n");
        .msg(msg, x$log_file, ifelse(stop_on_error,"e","w"));
        return (FALSE);
      }
    }
    return (TRUE);
  }
  
  default_class_defined <- function(x) 
  {
    tmp <- sqlQ(x, query=paste0("SELECT COUNT(*) FROM ",qfq_get(x, 'name', 'mc'),
                                          " WHERE ",qs_get(x, 'mcid', 'mc')," = '*' AND ",qs_get(x, 'class', 'mc')," = '*' ",
                                          ";"),
                          err_msg=paste0("The default class ('*','*') is not defined in the classes table '",get(x, 'name', 'mc'),"'!\n"), just_execute=FALSE);
    return (!is.null(tmp) && tmp[1,1] > 0 );
  }
  
  # Events:
  if( !check_table(x, tbname="events", tbcolumns=c("name", "patient_id", "date", "perday", "category", "duration"), check_empty=TRUE) )
  {
    return (FALSE);
  }
  
  # Actions:
  if( !check_table(x, tbname="actions", tbcolumns=c("name", "action_id", "action", "params"), check_empty=TRUE) )
  {
    return (FALSE);
  }
  
  # Classes:
  if( !check_table(x, tbname="medication classes", tbcolumns=c("name", "med_class_id", "class"), check_empty=FALSE) )
  {
    return (FALSE);
  }
  # In particular, the special default row ('*', '*'), must be defined in the classes table:
  if( !default_class_defined(x) )
  {
    return (FALSE);
  }
  
  # Processings:
  if( !check_table(x, tbname="processings", tbcolumns=c("name", "processing_id", "patient_id", "category", "action"), check_empty=TRUE) )
  {
    return (FALSE);
  }
  
  # Main results:
  if( !check_table(x, tbname="main results", tbcolumns=c("name", "result_id", "processing_id", "patient_id", "estimate", "estimate_type", "plot_jpg", "plot_html"), check_empty=FALSE) )
  {
    return (FALSE);
  }
  
  # Sliding window results:
  if( !check_table(x, tbname="sliding windows results", tbcolumns=c("name", "result_id", "patient_id", "window_id", "window_start", "window_end", "estimate"), check_empty=FALSE) )
  {
    return (FALSE);
  }
  
  # Per episode results:
  if( !check_table(x, tbname="per episode results", tbcolumns=c("name", "result_id", "patient_id", "episode_id", "episode_start", "gap_days", "episode_duration", "episode_end", "estimate"), check_empty=FALSE) )
  {
    return (FALSE);
  }
  
  # Udated info:
  if( !check_table(x, tbname="updated info", tbcolumns=c("name", "patient_id"), check_empty=FALSE, stop_on_error=FALSE) )
  {
    return (FALSE);
  }
  
  return (TRUE);
}


##
## Create the test database and table using the standard AdhereR::med.events dataset ####
##

create_test_database <- function(x) UseMethod("create_test_database")
create_test_database.SQL_db <- function(x)
{
  .msg(paste0("Creating the test database dervied from AdhereR::med.events...\n"), x$log_file, "m");
  
  # Make sure the dates are in the right format:
  d <- AdhereR::med.events;
  d$DATE <- as.character(as.Date(d$DATE, format="%m/%d/%Y"), format="%Y-%m-%d"); # use the expected format for SQL's DATE
  
  # Create the database:
  # Delete and (re)create the needed tables:
  db_tables <- list_tables(x);
  
  # The events table:
  try(table_drop(x, get(x, 'name', 'ev')), silent=TRUE);
  if( is.null(sqlQ(x, query=paste0("CREATE TABLE ",qfq_get(x, 'name', 'ev')," ( ",
                                   qs_get(x, 'patid', 'ev'),    " VARCHAR(256) NOT NULL, ",
                                   qs_get(x, 'date', 'ev'),     " DATE NOT NULL, ",
                                   qs_get(x, 'perday', 'ev'),   " INT NOT NULL, ",
                                   qs_get(x, 'category', 'ev'), " VARCHAR(1024) NULL DEFAULT NULL, ",
                                   qs_get(x, 'duration', 'ev'), " INT NULL DEFAULT NULL );"),
                   err_msg=paste0("Error creating the events table '",get(x, 'name', 'ev'),"'!\n"), just_execute=TRUE)) ) return (NULL);
  # Fill it in one by one (for some reason, saving the whole data.frame doesn't seems to be working):
  for( i in 1:nrow(d) )
  {
    if( is.null(sqlQ(x, query=paste0("INSERT INTO ",qfq_get(x, 'name', 'ev'),
                                     " VALUES (",paste0("'",as.character(d[i,]),"'",collapse=","),");"),
                     err_msg=paste0("Error writing into the events table '",get(x, 'name', 'ev'),"'!\n"), just_execute=TRUE)) ) return (NULL);
  }
  
  # The actions table: 
  try(table_drop(x, get(x, 'name', 'ac')), silent=TRUE);
  if( is.null(sqlQ(x, query=paste0("CREATE TABLE ",qfq_get(x, 'name', 'ac')," ( ",
                                   qs_get(x, 'actid', 'ac'),  " VARCHAR(256) NOT NULL, ",
                                   qs_get(x, 'action', 'ac'), " VARCHAR(128) NULL DEFAULT NULL, ",
                                   qs_get(x, 'params', 'ac'), " VARCHAR(5000) NULL DEFAULT NULL, ", 
                                   "PRIMARY KEY (", get(x, 'actid', 'ac'), ") );"),
                   err_msg=paste0("Error creating the actions table '",get(x, 'name', 'ac'),"'!\n"), just_execute=TRUE)) ) return (NULL);
  # Fill it in one by one:
  tmp <- matrix(c('CMA1',               'CMA1',                    '',
                  'CMA2',               'CMA2',                    '',
                  'CMA9',               'CMA9',                    '',
                  'pCMA0',              'plot.CMA0',               '',
                  'pCMA7',              'plot.CMA7',               '',
                  'pCMA9',              'plot.CMA9',               '',
                  'pSW(CMA1,d=90,n=5)', 'plot.CMA_sliding_window', 'CMA.to.apply=\"CMA1\", sliding.window.duration=90, sliding.window.no.steps=5',
                  'pPE(CMA1,gap=90)',   'plot.CMA_per_episode',    'CMA.to.apply=\"CMA1\", maximum.permissible.gap=90'), 
                ncol=3, byrow=TRUE);
  colnames(tmp) <- c(qs_get(x, 'actid', 'ac'), qs_get(x, 'action', 'ac'), qs_get(x, 'params', 'ac'));
  for( i in 1:nrow(tmp) )
  {
    if( is.null(sqlQ(x, query=paste0("INSERT INTO ",qfq_get(x, 'name', 'ac'),
                                     "(",paste0(colnames(tmp),collapse=","),")",
                                     " VALUES (",paste0("'",tmp[i,],"'",collapse=", "),");"),
                     err_msg=paste0("Error writing into the actions table '",get(x, 'name', 'ac'),"'!\n"), just_execute=TRUE)) ) return (NULL);
  }
  
  # The medication classes table: 
  try(table_drop(x, get(x, 'name', 'mc')), silent=TRUE);
  if( is.null(sqlQ(x, query=paste0("CREATE TABLE ",qfq_get(x, 'name', 'mc')," ( ",
                                   qs_get(x, 'mcid', 'mc'),  " VARCHAR(256) NOT NULL, ",
                                   qs_get(x, 'class', 'mc'), " VARCHAR(5000) NULL DEFAULT NULL, ",
                                   "PRIMARY KEY (", qs_get(x, 'mcid', 'mc'), ") );"),
                   err_msg=paste0("Error creating the medication classes table '",get(x, 'name', 'mc'),"'!\n"), just_execute=TRUE)) ) return (NULL);
  # Fill it in one by one:
  tmp <- matrix(c('*',      '*', # the special default * rule must be defined!
                  'A',      '[medA]',
                  'A | B',  '[medA] | [medB]',
                  '!A',     '![medA]',
                  'A & !A', '[medA] & {!A}', # refer to another class using {})
                  'else',   '@'), # @ means all other not otherwise matched medications for a given patient
                ncol=2, byrow=TRUE);
  colnames(tmp) <- c(qs_get(x, 'mcid', 'mc'), qs_get(x, 'class', 'mc'));
  for( i in 1:nrow(tmp) )
  {
    if( is.null(sqlQ(x, query=paste0("INSERT INTO ",qfq_get(x, 'name', 'mc'),
                                     "(",paste0(colnames(tmp),collapse=","),")",
                                     " VALUES (",paste0("'",tmp[i,],"'",collapse=", "),");"),
                     err_msg=paste0("Error writing into the medication classes table '",get(x, 'name', 'mc'),"'!\n"), just_execute=TRUE)) ) return (NULL);
  }
  
  # The processings table specifying what to do to which entries: 
  try(table_drop(x, get(x, 'name', 'pr')), silent=TRUE);
  if( is.null(sqlQ(x, query=paste0("CREATE TABLE ",qfq_get(x, 'name', 'pr')," ( ",
                                   qs_get(x, 'procid', 'pr'),   ifelse(x$db_type == "mssql", " INT IDENTITY(1,1), " ," INT NOT NULL AUTO_INCREMENT, "),
                                   qs_get(x, 'patid', 'pr'),    " VARCHAR(256) NOT NULL, ",
                                   qs_get(x, 'category', 'pr'), " VARCHAR(256) NOT NULL, ",
                                   qs_get(x, 'action', 'pr'),   " VARCHAR(256) NOT NULL, ",
                                   "PRIMARY KEY (", qs_get(x, 'procid', 'pr'), ") );"),
                   err_msg=paste0("Error creating the processings table '",get(x, 'name', 'pr'),"'!\n"), just_execute=TRUE)) ) return (NULL);
  # Fill it in one by one:
  tmp <- matrix(c('*', '*',      'pCMA0', # the default actions
                  '*', '*',      'CMA9',
                  '1', 'A',      'CMA2', # specific overrides
                  '1', 'A',      'pCMA0',
                  '1', 'A | B',  'pCMA7',
                  '2', '*',      'pCMA0',
                  '2', '*',      'pCMA9',
                  '3', '*',      'pCMA0',
                  '3', '*',      'CMA1',
                  '3', '*',      'pSW(CMA1,d=90,n=5)',
                  '4', '*',      'pCMA0',
                  '4', '*',      'CMA1',
                  '4', '*',      'pPE(CMA1,gap=90)',
                  '5', '!A',     'CMA2',
                  '6', 'A',      'CMA9',
                  '6', 'else',   'CMA2', # for all other medications for patient 6
                  '7', 'A',      '*',    # * means use the default actions (i.e., those with ('*', '*'))
                  '8', 'A & !A', 'CMA2',
                  '9', '*',      'CMA9'), 
                ncol=3, byrow=TRUE);
  colnames(tmp) <- c(qs_get(x, 'patid', 'pr'), qs_get(x, 'category', 'pr'), qs_get(x, 'action', 'pr'));
  for( i in 1:nrow(tmp) )
  {
    if( is.null(sqlQ(x, query=paste0("INSERT INTO ",qfq_get(x, 'name', 'pr'),
                                     "(",paste0(colnames(tmp),collapse=","),")",
                                     " VALUES (",paste0("'",tmp[i,],"'",collapse=", "),");"),
                     err_msg=paste0("Error writing into the processings table '",get(x, 'name', 'pr'),"'!\n"), just_execute=TRUE)) ) return (NULL);
  }
  
  # The main results table: 
  try(table_drop(x, get(x, 'name', 're')), silent=TRUE);
  if( is.null(sqlQ(x, query=paste0("CREATE TABLE ",qfq_get(x, 'name', 're')," ( ",
                                   qs_get(x, 'resid', 're'),      ifelse(x$db_type == "mssql", " INT IDENTITY(1,1), " ," INT NOT NULL AUTO_INCREMENT, "),
                                   qs_get(x, 'patid', 'ev'),      " VARCHAR(256) NOT NULL, ",
                                   qs_get(x, 'procid', 're'),     " INT NULL DEFAULT -1, ",
                                   qs_get(x, 'estim', 're'),      " FLOAT NULL DEFAULT NULL, ",
                                   qs_get(x, 'estim_type', 're'), " VARCHAR(256) NULL DEFAULT NULL, ",
                                   qs_get(x, 'jpg', 're'),        ifelse(x$db_type == "mssql", " VARBINARY(MAX), " ," LONGBLOB NULL DEFAULT NULL, "),
                                   qs_get(x, 'html', 're'),       ifelse(x$db_type == "mssql", " VARBINARY(MAX), " ," LONGBLOB NULL DEFAULT NULL, "), 
                                   "PRIMARY KEY (", qs_get(x, 'resid', 're'), ") );"),
                   err_msg=paste0("Error creating the main results table '",get(x, 'name', 're'),"'!\n"), just_execute=TRUE)) ) return (NULL);

  # The sliding windows results table: 
  try(table_drop(x, get(x, 'name', 'sw')), silent=TRUE);
  if( is.null(sqlQ(x, query=paste0("CREATE TABLE ",qfq_get(x, 'name', 'sw')," ( ",
                                   qs_get(x, 'resid', 'sw'), " INT NULL DEFAULT NULL, ",
                                   qs_get(x, 'patid', 'sw'), " VARCHAR(256) NOT NULL, ",
                                   qs_get(x, 'wndid', 'sw'), " INT NOT NULL, ",
                                   qs_get(x, 'start', 'sw'), " DATE NOT NULL, ",
                                   qs_get(x, 'end', 'sw'),   " DATE NOT NULL, ",
                                   qs_get(x, 'estim', 'sw'), " FLOAT NULL DEFAULT NULL );"),
                   err_msg=paste0("Error creating the sliding windows results table '",get(x, 'name', 'sw'),"'!\n"), just_execute=TRUE)) ) return (NULL);

  # The per episode results table: 
  try(table_drop(x, get(x, 'name', 'pe')), silent=TRUE);
  if( is.null(sqlQ(x, query=paste0("CREATE TABLE ",qfq_get(x, 'name', 'pe')," ( ",
                                   qs_get(x, 'resid', 'pe'),    " INT NULL DEFAULT NULL, ",
                                   qs_get(x, 'patid', 'pe'),    " VARCHAR(256) NOT NULL, ",
                                   qs_get(x, 'epid', 'pe'),     " INT NOT NULL, ",
                                   qs_get(x, 'start', 'pe'),    " DATE NOT NULL, ",
                                   qs_get(x, 'gap', 'pe'),      " INT NULL DEFAULT NULL, ",
                                   qs_get(x, 'duration', 'pe'), " INT NULL DEFAULT NULL, ",
                                   qs_get(x, 'end', 'pe'),      " DATE NOT NULL, ",
                                   qs_get(x, 'estim', 'pe'),    " FLOAT NULL DEFAULT NULL );"),
                   err_msg=paste0("Error creating the per episode results table '",get(x, 'name', 'pe'),"'!\n"), just_execute=TRUE)) ) return (NULL);

  # The updated info table: 
  try(table_drop(x, get(x, 'name', 'up')), silent=TRUE);
  if( is.null(sqlQ(x, query=paste0("CREATE TABLE ",qfq_get(x, 'name', 'up')," ( ",
                                   qs_get(x, 'patid', 'up'), " VARCHAR(256) NOT NULL );"),
                   err_msg=paste0("Error creating the updated info table '",get(x, 'name', 'up'),"'!\n"), just_execute=TRUE)) ) return (NULL);
  # Fill it in:
  if( is.null(sqlQ(x, query=paste0("INSERT INTO ",qfq_get(x, 'name', 'up'),
                                   " (",qs_get(x, 'patid', 'up'),") ",
                                   " VALUES ",paste0("('",1:20,"')",collapse=", ")," ;"), # just the first 20 patients have updated info
                   err_msg=paste0("Error writing into the updated info table '",get(x, 'name', 'up'),"'!\n"), just_execute=TRUE)) ) return (NULL);

  .msg(paste0("Finished creating the test database...\n\n"), x$log_file, "m");
}
                                    
