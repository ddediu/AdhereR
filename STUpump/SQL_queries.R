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

##
## The SQL_db class that ecapsulates all SQL-related things ####
##

SQL_db <- function(spec_file=NA,                                                 # the file containing the database specification (or NA for the defaults)
                   connect_to_db=TRUE,                                           # try to connect to the database?
                   truncate_results=TRUE,                                        # remove any pre-existing rows from the results tables?
                   check_db=connect_to_db,                                       # check the consistency of the database?
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
    
    # Load the actual database specification:
    #db_info <- read.table(spec_file, header=TRUE, sep="\t", quote="", fill=TRUE, strip.white=TRUE, blank.lines.skip=TRUE, stringsAsFactors=FALSE);
    db_info <- configr::read.config(spec_file);
    if( is.null(db_info) ) .msg(paste0("Error reading the config file '",spec_file,"'!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));

    if( is.null(db_info$database) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'database' section is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    
    if( is.null(db_info$database$type) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'database:type' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    if( !(tolower(db_info$database$type) %in% c("mariadb", "mysql", "sqlite", "mssql")) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'database:type' entry has an unknown value!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_type  <- tolower(db_info$database$type);
    .msg(paste0("Config: read 'database:type' = '",db_type,"'.\n"), log_file, "m");
    
    if( is.null(db_info$database$host) ) 
      .msg(paste0("Error in the config file '",spec_file,"': 'database:host' entry is not defined!\n"), log_file, ifelse(stop_on_database_errors,"e","w"));
    db_host  <- db_info$database$host;
    .msg(paste0("Config: read 'database:host' = '",db_host,"'.\n"), log_file, "m");
    
    db_dsn   <- db_info$database$DSN;
    db_user  <- db_info$database$user;
    db_psswd <- db_info$database$psswd;
    db_name  <- db_info$database$name;
    
    # The events table:
    db_evtable_name <- db_info$tables$events$name;
    db_evtable_cols <- c("ID"              =db_info$tables$events$patient_id,
                         "DATE"            =db_info$tables$events$date,
                         "PERDAY"          =db_info$tables$events$perday,
                         "CATEGORY"        =db_info$tables$events$category,
                         "DURATION"        =db_info$tables$events$duration);
    
    # The actions table:
    db_actable_name <- db_info$tables$actions$name;
    db_actable_cols <- c("ID"             =db_info$tables$actions$action_id,
                         "ACTION"         =db_info$tables$actions$action,
                         "PARAMS"         =db_info$tables$actions$params);
    
    # The medication classes table:
    db_mctable_name <- db_info$tables$med_classes$name;
    db_mctable_cols <- c("ID"             =db_info$tables$med_classes$medclass_id,
                         "CLASS"          =db_info$tables$med_classes$class);
    
    # The processing to be done table:
    db_prtable_name <- db_info$tables$processes$name;
    db_prtable_cols <- c("ID"             =db_info$tables$processes$procid);
    
    # The main results table:
    db_retable_name <- db_info$tables$results$name;
    db_retable_cols <- c("ID"             =db_info$tables$results$result_id,
                         "ESTIMATE"       =db_info$tables$results$estimate,
                         "ESTIMATE_TYPE"  =db_info$tables$results$estimate_type,
                         "PLOT_JPG"       =db_info$tables$results$plot_jpg,
                         "PLOT_HTML"      =db_info$tables$results$plot_html);
    
    # The sliding windows results table:
    db_swtable_name <- db_info$tables$sliding_windows_results$name;
    db_swtable_cols <- c("WINDOW_ID"      =db_info$tables$sliding_windows_results$window_id,
                         "WINDOW_START"   =db_info$tables$sliding_windows_results$window_start,
                         "WINDOW_END"     =db_info$tables$sliding_windows_results$window_end,
                         "WINDOW_ESTIMATE"=db_info$tables$sliding_windows_results$estimate);
    
    # The per episode results table:
    db_petable_name <- db_info$tables$per_episode_results$name;
    db_petable_cols <- c("EPISODE_ID"       =db_info$tables$per_episode_results$episode_id,
                         "EPISODE_START"    =db_info$tables$per_episode_results$episode_start,
                         "GAP_DAYS"         =db_info$tables$per_episode_results$gap_days,
                         "ESPISODE_DURATION"=db_info$tables$per_episode_results$episode_duration,
                         "EPISODE_END"      =db_info$tables$per_episode_results$episode_end,
                         "EPISODE_ESTIMATE" =db_info$tables$per_episode_results$estimate);
    
    # The updated info table:
    db_uptable_name <- db_info$tables$updated_info$name;
    db_uptable_cols <- c();
    
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
                              
                              # the important tables:
                              "db_evtable_name"=db_evtable_name,
                              "db_evtable_cols"=db_evtable_cols,
                              "db_actable_name"=db_actable_name,
                              "db_actable_cols"=db_actable_cols,
                              "db_mctable_name"=db_mctable_name,
                              "db_mctable_cols"=db_mctable_cols,
                              "db_prtable_name"=db_prtable_name,
                              "db_prtable_cols"=db_prtable_cols,
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
      stop(paste0("The required database '",x$db_name,"' does not exist on this server!\n"));
    }
  } else
  {
    x$db_connection <- NULL;
    stop(paste0("Don't know how to use an SQL database of type '",x$db_type,"': please specify a MariaDB, MySQL, SQLite or Microsoft SQL Server database!\n"));
  }
  if( is.null(x$db_connection) )
  {
    # Something bad happened:
    stop("Error connecting to the specified database!\n");
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
    if( x$db_type %in% c("mariadb", "mysql", "sqlite") )
    {
      try(DBI::dbDisconnect(x$db_connection), silent=TRUE);
    } else if( x$db_type == "mssql" )
    {
      try(RODBC::odbcClose(x$db_connection), silent=TRUE);
    }
    
    x$db_connection <- NULL;
  }
  
  return (invisible(x));
}

# Initialise the results tables:
reset_results <- function(x) UseMethod("reset_results")
reset_results.SQL_db <- function(x)
{
  if( x$db_type %in% c("mariadb", "mysql", "sqlite") )
  {
    if( get(x, 'name', 're') %in% list_tables(x) ) try(DBI::dbExecute(x$db_connection, paste0("TRUNCATE ",get(x, 'name', 're')," ;")), silent=TRUE);
    if( get(x, 'name', 'sw') %in% list_tables(x) ) try(DBI::dbExecute(x$db_connection, paste0("TRUNCATE ",get(x, 'name', 'sw')," ;")), silent=TRUE);
    if( get(x, 'name', 'pe') %in% list_tables(x) ) try(DBI::dbExecute(x$db_connection, paste0("TRUNCATE ",get(x, 'name', 'pe')," ;")), silent=TRUE);
  } else if( x$db_type == "mssql" )
  {
    if( get(x, 'name', 're') %in% list_tables(x) ) try(RODBC::sqlQuery(x$db_connection, paste0("TRUNCATE ",get(x, 'name', 're')," ;")), silent=TRUE);
    if( get(x, 'name', 'sw') %in% list_tables(x) ) try(RODBC::sqlQuery(x$db_connection, paste0("TRUNCATE ",get(x, 'name', 'sw')," ;")), silent=TRUE);
    if( get(x, 'name', 'pe') %in% list_tables(x) ) try(RODBC::sqlQuery(x$db_connection, paste0("TRUNCATE ",get(x, 'name', 'pe')," ;")), silent=TRUE);
  }
  
  return (TRUE);
}


##
## Retreive various database info ####
##

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
      } else
      {
        n <- as.numeric(n[1,1]);
      }
      
      db_cols_info <- data.frame("table"=db_table, "nrow"=n, "column"=tmp$name, "type"=tmp$type, "null"=(tmp$notnull == 0), "key"=(tmp$pk != 0));
    }
  } else if( x$db_type == "mssql" )
  {
    n <- NULL;
    try(n <- RODBC::sqlQuery(x$db_connection, paste0("SELECT COUNT(*) FROM ",qs(x,get(x, 'name')),".",qs(x,db_table),";")), silent=TRUE);
    if( is.null(n) || !inherits(n, "data.frame") || nrow(n) != 1 || ncol(n) != 1 )
    {
      # Error retreiving the number of rows:
      n <- NA;
    } else
    {
      n <- as.numeric(n[1,1]);
    }
    db_list <- RODBC::sqlQuery(x$db_connection, paste0("SELECT * FROM ",qs(x,get(x, 'name')),".INFORMATION_SCHEMA.COLUMNS ORDER BY ORDINAL_POSITION;"));
    if( !is.null(db_list) && nrow(db_list) > 0 )
    {
      db_list <- db_list[ paste0(db_list$TABLE_SCHEMA,".",db_list$TABLE_NAME) == db_table, ];
      if( !is.null(db_list) && nrow(db_list) > 0 )
      {
        db_cols_info <- data.frame("table"=db_table, "nrow"=n, "column"=db_list$COLUMN_NAME, "type"=db_list$DATA_TYPE, "null"=db_list$IS_NULLABLE, "key"=NA);
      }
    }
  }
  
  # Return the column info:
  return (db_cols_info);
}

# Get the column names for a given table:
get_col_names <- function(x) UseMethod("get_col_names")
get_col_names.SQL_db <- function(x)
{
  db_cols_info <- get_cols_info(x);
  if( !is.null(db_cols_info) && nrow(db_cols_info) > 0 )
  {
    return (as.character(db_cols_info$column));
  } else
  {
    return (NULL);
  }
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

qs <- function(x, s) UseMethod("qs") # quote string
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
## Getters ####
##

# Get various attributes either for the whole database (table=NULL) or for a specific table:
get <- function(x, variable, table=NULL) UseMethod("get")
get.SQL_db <- function(x, variable, table=NULL)
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
                   stop(paste0("Undefined global attribute '",variable,"'."))));
  } else
  {
    # A specific table was requested:
    return (switch(tolower(table),
                   # Events:
                   "events"=,
                   "ev"=switch(tolower(variable),
                               "name"      =x$db_evtable_name,
                               "patient_id"=,
                               "patid"     =,
                               "id"        =x$db_evtable_cols["ID"],
                               "date"      =x$db_evtable_cols["DATE"],
                               "perday"    =x$db_evtable_cols["PERDAY"],
                               "cat"       =,
                               "category"  =x$db_evtable_cols["CATEGORY"],
                               "duration"  =x$db_evtable_cols["DURATION"],
                               stop(paste0("Undefined attribute '",variable,"' for table '",table,"'."))),
                   
                   # Actions:
                   "actions"=,
                   "ac"=switch(tolower(variable),
                               "name"     =x$db_actable_name,
                               "action_id"=,
                               "actid"    =,
                               "id"       =x$db_actable_cols["ID"],
                               "action"   =x$db_actable_cols["ACTION"],
                               "params"   =x$db_actable_cols["PARAMS"],
                               stop(paste0("Undefined attribute '",variable,"' for table '",table,"'."))),
                   
                   # Medication classes:
                   "medication classes"=,
                   "medclass"          =,
                   "classes"           =,
                   "medications"       =,
                   "mc"=switch(tolower(variable),
                               "name"        =x$db_mctable_name,
                               "med_class_id"=,
                               "mcid"        =,
                               "id"          =x$db_mctable_cols["ID"],
                               "class"       =x$db_mctable_cols["CLASS"],
                               stop(paste0("Undefined attribute '",variable,"' for table '",table,"'."))),
                   
                   # Processings:
                   "processings"=,
                   "procs"      =,
                   "pr"=switch(tolower(variable),
                               "name"         =x$db_prtable_name,  
                               "processing_id"=,
                               "procid"       =,
                               "id"           =x$db_prtable_cols["ID"],
                               "patient_id"   =,
                               "patid"        =get(x, "patid", "ev"),
                               "cat"          =,
                               "category"     =get(x, "mcid", "mc"),
                               "action"       =get(x, "actid", "ac"),
                               stop(paste0("Undefined attribute '",variable,"' for table '",table,"'."))),
                   
                   # Main results:
                   "main results"=,
                   "results"     =,
                   "re"=switch(tolower(variable),
                               "name"         =x$db_retable_name,
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
                               stop(paste0("Undefined attribute '",variable,"' for table '",table,"'."))),
                   
                   # Sliding window results:
                   "sliding windows results"=,
                   "sliding windows"        =,
                   "sw"=switch(tolower(variable),
                               "name"           =x$db_swtable_name,
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
                               stop(paste0("Undefined attribute '",variable,"' for table '",table,"'."))),
                   
                   # Per episode results:
                   "per episode results"=,
                   "per episode"        =,
                   "pe"=switch(tolower(variable),
                               "name"            =x$db_petable_name,
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
                               stop(paste0("Undefined attribute '",variable,"' for table '",table,"'."))),
                   
                   # Updated info:
                   "updated info"=,
                   "updated"     =,
                   "up"=switch(tolower(variable),
                               "name"            =x$db_uptable_name,
                               "patid"           =,
                               "id"              =,
                               "patient_id"      =get(x, "patid", "ev"),
                               stop(paste0("Undefined attribute '",variable,"' for table '",table,"'."))),
                   
                   stop(paste0("Undefined table '",table,"'."))));
  }
}


# The defaults:
get_default_processing <- function(x) UseMethod("get_default_processing")
get_default_processing.SQL_db <- function(x)
{
  return (x$db_default_proc);
}


##
## Results tables ####
##

write_retable_entry <- function(x, id, procid, class="", type="", proc="", params="", estimate=NA, estimate_type=NA, plot_jpg="", plot_html="") UseMethod("write_retable_entry")
write_retable_entry.SQL_db <- function(x, id, procid, class="", type="", proc="", params="", estimate=NA, estimate_type=NA, plot_jpg="", plot_html="")
{
  # Write all these info into the retable:
  if( x$db_type %in% c("mariadb", "mysql", "sqlite") )
  {
    result <- DBI::dbExecute(x$db_connection, 
                             paste0("INSERT INTO ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 're')),
                                    "(",
                                    qs(x,get(x, 'patid', 'ev')),", ",
                                    qs(x,get(x, 'procid', 're')),", ",
                                    qs(x,get(x, 'estim', 're')),", ",
                                    qs(x,get(x, 'estim_type', 're')),", ",
                                    qs(x,get(x, 'jpg', 're')),", ",
                                    qs(x,get(x, 'html', 're')),
                                    ")",
                                    " VALUES (",
                                    "'",id,"', ", # id
                                    "'",procid,"', ", # reference to the processing key
                                    #"'",class,"', ", # class
                                    #"'",ifelse(tolower(type) == "plot",paste0(type,"."),""),proc,"', ", # type & proc 
                                    #"'",params,"', ", # params
                                    ifelse(is.na(estimate),"NULL",paste0("'",estimate,"'")),", ", # estimate
                                    ifelse(is.na(estimate_type),"NULL",paste0("'",estimate_type,"'")),", ", # estimate type
                                    ifelse(is.na(plot_jpg)  || !file.exists(plot_jpg),  
                                           "NULL", 
                                           paste0("X'",paste0(readBin(plot_jpg,  n=file.size(plot_jpg) +1024, what="raw"),collapse=""),"'")),", ", # the JPEG file as a blob
                                    ifelse(is.na(plot_html) || !file.exists(plot_html), 
                                           "NULL", 
                                           paste0("X'",paste0(readBin(plot_html, n=file.size(plot_html)+1024, what="raw"),collapse=""),"'")), # the HTML+SVG file as a blob
                                    ");"));
    return (result == 1); # should've written exactly one line
  } else if( x$db_type == "mssql" )
  {
  }
  
  return (TRUE);
}

get_retable_resid_for_results <- function(x, id, procid, estimate_type) UseMethod("get_retable_resid_for_results")
get_retable_resid_for_results.SQL_db <- function(x, id, procid, estimate_type)
{
  # Get the matching info:
  if( x$db_type %in% c("mariadb", "mysql", "sqlite") )
  {
    # Get the resid of the previous insertion in the retable:
    resid_ref <- DBI::dbGetQuery(x$db_connection, 
                                 paste0("SELECT ", qs(x,get(x, 'resid', 're')),
                                        " FROM ", qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 're')), 
                                        " WHERE ", qs(x,get(x, 'patid', 're')), " = '", id, "'", 
                                        " AND ", qs(x,get(x, 'procid', 're')), " = '", procid, "'",
                                        " AND ", qs(x,get(x, 'estim_type', 're')), " = '", estimate_type, "'",
                                        ";"));
    if( is.null(resid_ref) || nrow(resid_ref) != 1 )
    {
      # Error identifying the last inserted row!
      return (NULL);
    } else
    {
      return (resid_ref[1,1]);
    }
  } else if( x$db_type == "mssql" )
  {s
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
  if( x$db_type %in% c("mariadb", "mysql", "sqlite") )
  {
    # Write at once as usually there's few sliding windows:
    result <- DBI::dbExecute(x$db_connection, 
                             paste0("INSERT INTO ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'sw')),
                                    " (",
                                    qs(x,get(x, 'resid', 'sw')),", ",
                                    qs(x,get(x, 'patid', 'sw')),", ",
                                    qs(x,get(x, 'wndid', 'sw')),", ",
                                    qs(x,get(x, 'start', 'sw')),", ",
                                    qs(x,get(x, 'end', 'sw')),", ",
                                    qs(x,get(x, 'estim', 'sw')),
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
                                    ";"));
    return (result == nrow(cma)); # should've written exactly as many lines as in the data.frame
  } else if( x$db_type == "mssql" )
  {
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
  if( x$db_type %in% c("mariadb", "mysql", "sqlite") )
  {
    # Write at once as usually there's few sliding windows:
    result <- DBI::dbExecute(x$db_connection, 
                             paste0("INSERT INTO ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'pe')),
                                    " (",
                                    qs(x,get(x, 'resid', 'pe')),", ",
                                    qs(x,get(x, 'patid', 'pe')),", ",
                                    qs(x,get(x, 'epid', 'pe')),", ",
                                    qs(x,get(x, 'start', 'pe')),", ",
                                    qs(x,get(x, 'gap', 'pe')),", ",
                                    qs(x,get(x, 'duration', 'pe')),", ",
                                    qs(x,get(x, 'end', 'pe')),", ",
                                    qs(x,get(x, 'estim', 'pe')),
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
                                    ";"));
    return (result == nrow(cma)); # should've written exactly as many lines as in the data.frame
  } else if( x$db_type == "mssql" )
  {
  }
  
  return (TRUE);
}


##
## List the patient ids in the events table ####
##

list_patients <- function(x, with_updated_info_only=TRUE) UseMethod("list_patients")
list_patients.SQL_db <- function(x, with_updated_info_only=TRUE)
{
  patient_ids <- NULL;
  
  if( x$db_type %in% c("mariadb", "mysql", "sqlite") )
  {
    tmp <- NULL;
    if( !with_updated_info_only || # specifically requested to use all patients, or
        is.null(up_info <- get_cols_info(x, get(x, 'name', 'up'))) || up_info$nrow == 0 ) # the updated_info table is not defined or empty
    {
      # List all patients in the events table:
      try(tmp <- DBI::dbGetQuery(x$db_connection, paste0("SELECT DISTINCT ",qs(x,get(x, 'patid', 'ev')),
                                                         " FROM ",qs(x,get(x, 'name', 'ev')),";
                                                         ")), 
          silent=TRUE);
      if( !is.null(tmp) && inherits(tmp, "data.frame") && nrow(tmp) > 0 ) patient_ids <- as.character(tmp[,1]);
    } else
    {
      # List only those in the events table that are also mentioned in the updated_info table:
      try(tmp <- DBI::dbGetQuery(x$db_connection, paste0("SELECT DISTINCT ",qs(x,get(x, 'name', 'ev')),".",qs(x,get(x, 'patid', 'ev')),
                                                         " FROM ",qs(x,get(x, 'name', 'ev')),
                                                         " INNER JOIN ",qs(x,get(x, 'name', 'up')),
                                                         " ON ",qs(x,get(x, 'name', 'ev')),".",qs(x,get(x, 'patid', 'ev')),
                                                         " = ",qs(x,get(x, 'name', 'up')),".",qs(x,get(x, 'patid', 'up')),
                                                         ";")), 
          silent=TRUE);
      if( !is.null(tmp) && inherits(tmp, "data.frame") && nrow(tmp) > 0 ) patient_ids <- as.character(tmp[,1]);
    }
  } else if( x$db_type == "mssql" )
  {
    tmp <- NULL;
    try(tmp <- RODBC::sqlQuery(x$db_connection, paste0("SELECT DISTINCT ",qs(x,get(x, 'patid', 'ev'))," FROM ",
                                                       qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'ev')),";")), 
        silent=TRUE);
    if( !is.null(tmp) && inherits(tmp, "data.frame") && nrow(tmp) > 0 ) patient_ids <- as.character(tmp[,1]);
  }
  
  # Return patient ids:
  return (patient_ids);
}


##
## Get info for a given set of patients in the events table ####
##

get_evtable_patients_info <- function(x, patient_id, cols=NA, maxrows=NA) UseMethod("get_evtable_patients_info")
get_evtable_patients_info.SQL_db <- function(x, patient_id, cols=NA, maxrows=NA)
{
  db_pat_info <- NULL;
  
  if( x$db_type %in% c("mariadb", "mysql", "sqlite") )
  {
    tmp <- NULL;
    try(tmp <- DBI::dbGetQuery(x$db_connection, 
                               paste0("SELECT ",
                                      if(is.na(cols)) "*" else paste0(qs(x,cols),collapse=","),
                                      " FROM ",qs(x,get(x, 'name', 'ev')),
                                      " WHERE ",qs(x,get(x, 'patid', 'ev')),
                                      " IN (",paste0("'",patient_id,"'",collapse=","),")",
                                      if(!is.na(maxrows)) paste0("LIMIT ",maxrows),
                                      ";")),
        silent=TRUE);
    if( !is.null(tmp) && inherits(tmp, "data.frame") && nrow(tmp) > 0 ) db_pat_info <- tmp;
  } else if( x$db_type == "mssql" )
  {
    tmp <- NULL;
    try(tmp <- RODBC::sqlQuery(x$db_connection, 
                               paste0("SELECT ",
                                      if(is.na(cols)) "*" else paste0(qs(x,cols),collapse=","),
                                      " FROM ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'ev')),
                                      " WHERE ",qs(x,get(x, 'patid', 'ev')),
                                      " IN (",paste0("'",patient_id,"'",collapse=","),")",
                                      if(!is.na(maxrows)) paste0("LIMIT ",maxrows),
                                      ";")),
        silent=TRUE);
    if( !is.null(tmp) && inherits(tmp, "data.frame") && nrow(tmp) > 0 ) db_pat_info <- tmp;
  }
  
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
  db_procs <- NULL;
  
  # For the given patient(s), select both the default and the specific classes and actions:
  if( x$db_type %in% c("mariadb", "mysql", "sqlite") )
  {
    tmp <- NULL;
    try(tmp <- DBI::dbGetQuery(x$db_connection, 
                               paste0("SELECT *",
                                      " FROM ",qs(x,get(x, 'name', 'pr')),
                                      " INNER JOIN ",qs(x,get(x, 'name', 'mc')),
                                      " ON ",qs(x,get(x, 'name', 'pr')),".",qs(x,get(x, 'category', 'pr'))," = ",qs(x,get(x, 'name', 'mc')),".",qs(x,get(x, 'mcid', 'mc')),
                                      " INNER JOIN ",qs(x,get(x, 'name', 'ac')),
                                      " ON ",qs(x,get(x, 'name', 'pr')),".",qs(x,get(x, 'action', 'pr'))," = ",qs(x,get(x, 'name', 'ac')),".",qs(x,get(x, 'actid', 'ac')),
                                      " AND ",qs(x,get(x, 'patid', 'pr'))," IN ('*', ",paste0("'",patient_id,"'",collapse=","),")",
                                      ";")),
        silent=TRUE);
    if( is.null(tmp) || nrow(tmp) == 0 )
    {
      # We can't get even the defaults: error!
      return (NULL);
    } else
    {
      db_procs <- tmp;
    }
  } else if( x$db_type == "mssql" )
  {
    tmp <- NULL;
    try(tmp <- RODBC::sqlQuery(x$db_connection, 
                               paste0("SELECT *",
                                      " FROM ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'pr')),
                                      " WHERE ",qs(x,get(x, 'patid', 'pr')),
                                      " IN (",paste0("'",patient_id,"'",collapse=","),")",
                                      ";")),
        silent=TRUE);
    if( !is.null(tmp) && inherits(tmp, "data.frame") && nrow(tmp) > 0 ) db_procs <- tmp;
  }
  
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

  return (db_procs);
}

# Apply the required selection to this patient:
select_events_for_procs_class <- function(x, patient_info, procs_classs) UseMethod("select_events_for_procs_class")
select_events_for_procs_class.SQL_db <- function(x, patient_info, procs_class)
{
  if( is.null(patient_info) || nrow(patient_info) == 0 || 
      is.null(procs_class) || length(procs_class) != 1 )
  {
    # Empty patient info or processing class: nothing to do
    return (NULL);
  }
  
  if( is.na(procs_class) || procs_class %in% c("", "*") )
  {
    # Select all!
    return (rep(TRUE, nrow(patient_info)));
  } else
  {
    # Specific selection may be needed:
    
    # Get the medication classes for this patient:
    if( is.na(get(x, 'category', 'ev')) )
    {
      # No medication classes:
      return (NULL);
    }
    pat_classes <- patient_info[ , get(x, 'category', 'ev') ]; 
    if( is.null(pat_classes) )
    {
      # No medication classes:
      return (NULL);
    }
    
    # Transform the procs_class specification into a logical expression to be evaluated on pat_classes:
    procs_class_expr <- NULL;
    try(procs_class_expr <- parse(text=gsub("]", "')", # replace "[ XX ]" by the actual R test "(pat_classes == 'XX')"
                                            gsub("[", "(pat_classes == '",
                                                      gsub("&", "&&", 
                                                           procs_class, 
                                                           fixed=TRUE), 
                                                 fixed=TRUE), 
                                            fixed=TRUE)), 
        silent=TRUE);
    if( is.null(procs_class_expr) )
    {
      stop(paste0("Error parsing the medication class definition '",procs_class,"'!\n"));
      return (NULL);
    }
    
    # Evaluate the expression:
    s <- eval(procs_class_expr);
    if( (!is.na(s) || !is.null(s)) && !is.logical(s) )
    {
      stop(paste0("Error applying the medication class definition '",procs_class,"' to the data: the result should be logical!\n"));
      return (NULL);
    }
    
    # Return it:
    return (s);
  }
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
    stop(paste0("Error parsing the action '",procs_action_call,"'!\n"));
    return (NULL);
  }
  
  # Evaluate the expression:
  cma <- eval(procs_action_expr);
  if( is.null(cma) || is.na(cma) || !(inherits(cma, "CMA0") || inherits(cma, "CMA_sliding_window") || inherits(cma, "CMA_per_episode")) )
  {
    # Serious error:
    stop(paste0("Error applying the action definition '",procs_action_call,"' to the data!\n"));
    return (NULL);
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
      stop(paste0("Error parsing the action '",procs_action_call,"'!\n"));
      return (NULL);
    }
    
    # Evaluate the expression:
    plot_file_names <- eval(procs_action_expr);
    if( is.null(plot_file_names) || length(plot_file_names) < 2 )
    {
      # Issues generating the plots:
      warning(paste0("Error applying the action definition '",procs_action_call,"' to the data: the result should be a valid plot!\n"));
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
        warning(paste0(pat_msgs, "Error creating the zip containing the HTML document and the JPG placeholder!"));
        return (NULL);
      }
      
      # Store these files:
      cma_plots <- list("jpg"=plot_file_names["jpg-placeholder"], "html"=zip_file_name);
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
      if( stop_on_error ) stop(msg) else warning(msg);
      return (FALSE);
    }
    table_info <- get_cols_info(x, get(x, 'name', tbname));
    if( is.null(table_info) || nrow(table_info) == 0 )
    {
      msg <- paste0("Cannot get the information about the table '",get(x, 'name', tbname),"'!\n");
      if( stop_on_error ) stop(msg) else warning(msg);
      return (FALSE);
    }
    if( check_empty && table_info$nrow[1] == 0 )
    {
      msg <- paste0("The events table '",get(x, 'name', 'ev'),"' seems empty!\n");
      if( stop_on_error ) stop(msg) else warning(msg);
      return (FALSE);
    }
    for( tbcol in tbcolumns )
    {
      if( !(get(x, tbcol, tbname) %in% table_info$column) )
      {
        msg <- paste0("The required column '",get(x, tbcol, tbname),"' does not seem to exist in the table '",get(x, 'name', tbname),"'!\n");
        if( stop_on_error ) stop(msg) else warning(msg);
        return (FALSE);
      }
    }
    return (TRUE);
  }
  
  default_class_defined <- function(x) 
  {
    if( x$db_type %in% c("mariadb", "mysql", "sqlite") )
    {
      result <- DBI::dbGetQuery(x$db_connection, 
                               paste0("SELECT COUNT(*) FROM ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'mc')),
                                      " WHERE ",qs(x,get(x, 'mcid', 'mc'))," = '*' AND ",qs(x,get(x, 'class', 'mc'))," = '*' ",
                                      ";"));
      return (!is.null(result) && result[1,1] > 0);
    } else if( x$db_type == "mssql" )
    {
    }
    
    return (FALSE);
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
  # Make sure the dates are in the right format:
  d <- AdhereR::med.events;
  d$DATE <- as.character(as.Date(d$DATE, format="%m/%d/%Y"), format="%Y-%m-%d"); # use the expected format for SQL's DATE
  
  # Create the database:
  if( x$db_type %in% c("mariadb", "mysql", "sqlite") )
  {
    # Delete and (re)create the needed tables:
    db_tables <- list_tables(x);
    
    # The events table:
    if( get(x, 'name', 'ev') %in% db_tables ) DBI::dbExecute(x$db_connection, paste0("DROP TABLE ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'ev')),";"));
    DBI::dbExecute(x$db_connection, paste0("CREATE TABLE ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'ev'))," ( ",
                                           qs(x,get(x, 'patid', 'ev')),    " VARCHAR(256) NOT NULL, ",
                                           qs(x,get(x, 'date', 'ev')),     " DATE NOT NULL, ",
                                           qs(x,get(x, 'perday', 'ev')),   " INT NOT NULL, ",
                                           qs(x,get(x, 'category', 'ev')), " VARCHAR(1024) NULL DEFAULT NULL, ",
                                           qs(x,get(x, 'duration', 'ev')), " INT NULL DEFAULT NULL );"));
    # Fill it in one by one (for some reason, saving the whole data.frame doesn't seems to be working):
    for( i in 1:nrow(d) )
    {
      DBI::dbExecute(x$db_connection, paste0("INSERT INTO ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'ev'))," VALUES (",
                                             paste0("'",as.character(d[i,]),"'",collapse=","),
                                             ");"));
    }
    
    # The actions table: 
    if( get(x, 'name', 'ac') %in% db_tables ) DBI::dbExecute(x$db_connection, paste0("DROP TABLE ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'ac')),";"));
    DBI::dbExecute(x$db_connection, paste0("CREATE TABLE ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'ac'))," ( ",
                                           qs(x,get(x, 'actid', 'ac')),  " VARCHAR(256) NOT NULL, ",
                                           qs(x,get(x, 'action', 'ac')), " VARCHAR(128) NULL DEFAULT NULL, ",
                                           qs(x,get(x, 'params', 'ac')), " VARCHAR(10240) NULL DEFAULT NULL, ", 
                                           "PRIMARY KEY (", get(x, 'actid', 'ac'), ") );"));
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
    colnames(tmp) <- c(qs(x,get(x, 'actid', 'ac')), qs(x,get(x, 'action', 'ac')), qs(x,get(x, 'params', 'ac')));
    for( i in 1:nrow(tmp) )
    {
      DBI::dbExecute(x$db_connection, paste0("INSERT INTO ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'ac')),
                                             "(",paste0(colnames(tmp),collapse=","),")",
                                             " VALUES (",paste0("'",tmp[i,],"'",collapse=", "),");"));
    }
    
    # The medication classes table: 
    if( get(x, 'name', 'mc') %in% db_tables ) DBI::dbExecute(x$db_connection, paste0("DROP TABLE ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'mc')),";"));
    DBI::dbExecute(x$db_connection, paste0("CREATE TABLE ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'mc'))," ( ",
                                           qs(x,get(x, 'mcid', 'mc')),  " VARCHAR(256) NOT NULL, ",
                                           qs(x,get(x, 'class', 'mc')), " VARCHAR(10240) NULL DEFAULT NULL, ",
                                           "PRIMARY KEY (", get(x, 'mcid', 'mc'), ") );"));
    # Fill it in one by one:
    tmp <- matrix(c('*',     '*', # the special default * rule must be defined!
                    'A',     '[medA]',
                    'A | B', '[medA] | [medB]'), 
                  ncol=2, byrow=TRUE);
    colnames(tmp) <- c(qs(x,get(x, 'mcid', 'mc')), qs(x,get(x, 'class', 'mc')));
    for( i in 1:nrow(tmp) )
    {
      DBI::dbExecute(x$db_connection, paste0("INSERT INTO ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'mc')),
                                             "(",paste0(colnames(tmp),collapse=","),")",
                                             " VALUES (",paste0("'",tmp[i,],"'",collapse=", "),");"));
    }
    
    # The processings table specifying what to do to which entries: 
    if( get(x, 'name', 'pr') %in% db_tables ) DBI::dbExecute(x$db_connection, paste0("DROP TABLE ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'pr')),";"));
    DBI::dbExecute(x$db_connection, paste0("CREATE TABLE ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'pr'))," ( ",
                                           qs(x,get(x, 'procid', 'pr')),   " INT NOT NULL AUTO_INCREMENT, ",
                                           qs(x,get(x, 'patid', 'pr')),    " VARCHAR(256) NOT NULL, ",
                                           qs(x,get(x, 'category', 'pr')), " VARCHAR(256) NOT NULL, ",
                                           qs(x,get(x, 'action', 'pr')),   " VARCHAR(256) NOT NULL, ",
                                           "PRIMARY KEY (", get(x, 'procid', 'pr'), ") );"));
    # Fill it in one by one:
    tmp <- matrix(c('*', '*',     'pCMA0', # the default actions
                    '*', '*',     'CMA9',
                    '1', 'A',     'CMA2', # specific overrides
                    '1', 'A',     'pCMA0',
                    '1', 'A | B', 'pCMA7',
                    '2', '*',     'pCMA0',
                    '2', '*',     'pCMA9',
                    '3', '*',     'pCMA0',
                    '3', '*',     'CMA1',
                    '3', '*',     'pSW(CMA1,d=90,n=5)',
                    '4', '*',     'pCMA0',
                    '4', '*',     'CMA1',
                    '4', '*',     'pPE(CMA1,gap=90)'), 
                  ncol=3, byrow=TRUE);
    colnames(tmp) <- c(qs(x,get(x, 'patid', 'pr')), qs(x,get(x, 'category', 'pr')), qs(x,get(x, 'action', 'pr')));
    for( i in 1:nrow(tmp) )
    {
      DBI::dbExecute(x$db_connection, paste0("INSERT INTO ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'pr')),
                                             "(",paste0(colnames(tmp),collapse=","),")",
                                             " VALUES (",paste0("'",tmp[i,],"'",collapse=", "),");"));
    }

    # The main results table: 
    if( get(x, 'name', 're') %in% db_tables ) DBI::dbExecute(x$db_connection, paste0("DROP TABLE ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 're')),";"));
    DBI::dbExecute(x$db_connection, paste0("CREATE TABLE ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 're'))," ( ",
                                           qs(x,get(x, 'resid', 're')),      " INT NOT NULL AUTO_INCREMENT, ",
                                           qs(x,get(x, 'patid', 'ev')),      " VARCHAR(256) NOT NULL, ",
                                           qs(x,get(x, 'procid', 're')),     " INT NULL DEFAULT -1, ",
                                           qs(x,get(x, 'estim', 're')),      " FLOAT NULL DEFAULT NULL, ",
                                           qs(x,get(x, 'estim_type', 're')), " VARCHAR(256) NULL DEFAULT NULL, ",
                                           qs(x,get(x, 'jpg', 're')),        " LONGBLOB NULL DEFAULT NULL, ",
                                           qs(x,get(x, 'html', 're')),       " LONGBLOB NULL DEFAULT NULL, ", 
                                           "PRIMARY KEY (", get(x, 'resid', 're'), ") );"));
    
    # The sliding windows results table: 
    if( get(x, 'name', 'sw') %in% db_tables ) DBI::dbExecute(x$db_connection, paste0("DROP TABLE ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'sw')),";"));
    DBI::dbExecute(x$db_connection, paste0("CREATE TABLE ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'sw'))," ( ",
                                           qs(x,get(x, 'resid', 'sw')), " INT NULL DEFAULT NULL, ",
                                           qs(x,get(x, 'patid', 'sw')), " VARCHAR(256) NOT NULL, ",
                                           qs(x,get(x, 'wndid', 'sw')), " INT NOT NULL, ",
                                           qs(x,get(x, 'start', 'sw')), " DATE NOT NULL, ",
                                           qs(x,get(x, 'end', 'sw')),   " DATE NOT NULL, ",
                                           qs(x,get(x, 'estim', 'sw')), " FLOAT NULL DEFAULT NULL );"));
    
    # The per episode results table: 
    if( get(x, 'name', 'pe') %in% db_tables ) DBI::dbExecute(x$db_connection, paste0("DROP TABLE ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'pe')),";"));
    DBI::dbExecute(x$db_connection, paste0("CREATE TABLE ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'pe'))," ( ",
                                           qs(x,get(x, 'resid', 'pe')),    " INT NULL DEFAULT NULL, ",
                                           qs(x,get(x, 'patid', 'pe')),    " VARCHAR(256) NOT NULL, ",
                                           qs(x,get(x, 'epid', 'pe')),     " INT NOT NULL, ",
                                           qs(x,get(x, 'start', 'pe')),    " DATE NOT NULL, ",
                                           qs(x,get(x, 'gap', 'pe')),      " INT NULL DEFAULT NULL, ",
                                           qs(x,get(x, 'duration', 'pe')), " INT NULL DEFAULT NULL, ",
                                           qs(x,get(x, 'end', 'pe')),      " DATE NOT NULL, ",
                                           qs(x,get(x, 'estim', 'pe')),    " FLOAT NULL DEFAULT NULL );"));
    
    # The updated info table: 
    if( get(x, 'name', 'up') %in% db_tables ) DBI::dbExecute(x$db_connection, paste0("DROP TABLE ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'up')),";"));
    DBI::dbExecute(x$db_connection, paste0("CREATE TABLE ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'up'))," ( ",
                                           qs(x,get(x, 'patid', 'up')), " VARCHAR(256) NOT NULL );"));
    # Fill it in:
    DBI::dbExecute(x$db_connection, paste0("INSERT INTO ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'up')),
                                           " (",qs(x,get(x, 'patid', 'up')),") ",
                                           " VALUES ",paste0("('",1:20,"')",collapse=", ")," ;")); # just the first 20 patients have updated info
    
  } else if( x$db_type == "mssql" )
  {
    # Delete and (re)create the needed tables:
    db_tables <- list_tables(x);
    
    # The events table:
    if( get(x, 'name', 'ev') %in% db_tables ) RODBC::sqlQuery(x$db_connection, paste0("DROP TABLE ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'ev')),";"));
    RODBC::sqlQuery(x$db_connection, paste0("CREATE TABLE ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'ev'))," ( ",
                                            qs(x,get(x, 'patid', 'ev'))," VARCHAR(256) NOT NULL, ",
                                            qs(x,get(x, 'date', 'ev'))," DATE NOT NULL, ",
                                            qs(x,get(x, 'perday', 'ev'))," INT NOT NULL, ",
                                            qs(x,get(x, 'category', 'ev'))," VARCHAR(1024) NOT NULL, ",
                                            qs(x,get(x, 'duration', 'ev'))," INT NOT NULL);"));
    # Fill it in one by one (for some reason, saving the whole data.frame doesn't seems to be working):
    for( i in 1:nrow(d) )
    {
      RODBC::sqlQuery(x$db_connection, paste0("INSERT INTO ",qs(x,get(x, 'name')),".",qs(x,get(x, 'name', 'ev'))," VALUES (",
                                              paste0("'",as.character(d[i,]),"'",collapse=","),
                                              ");"));
    }
    
  }
}
                                    
