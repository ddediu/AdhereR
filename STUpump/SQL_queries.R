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
## The SQL_db class that ecapsulates all SQL-related things ####
##

SQL_db <- function(db_spec_file=NA, # the file containing the database specification (or NA for the defaults)
                   connect_to_db=TRUE # try to connect to the database?
                  )
{
  if( !is.na(db_spec_file) )
  {
    # The connection
    db_connection <- NULL; 
    
    # Load the actual database specification:
    db_info <- read.table(db_spec_file, header=TRUE, sep="\t", quote="", stringsAsFactors=FALSE);
    
    db_type  <- tolower(db_info$Value[ tolower(db_info$Variable) == "dbtype" ]);
    db_host  <- db_info$Value[ tolower(db_info$Variable) == "host" ];
    db_dsn   <- db_info$Value[ tolower(db_info$Variable) == "dsn" ];
    db_user  <- db_info$Value[ tolower(db_info$Variable) == "user" ];
    db_psswd <- db_info$Value[ tolower(db_info$Variable) == "psswd" ];
    db_name  <- db_info$Value[ tolower(db_info$Variable) == "dbname" ];
    
    db_evtable_name <- db_info$Value[ tolower(db_info$Variable) == "evtable" ];
    db_evtable_cols <- c("PATIENT_ID"=db_info$Value[ tolower(db_info$Variable) == "evtable_patient_id" ],
                         "DATE"      =db_info$Value[ tolower(db_info$Variable) == "evtable_date" ],
                         "PERDAY"    =db_info$Value[ tolower(db_info$Variable) == "evtable_perday" ],
                         "CATEGORY"  =db_info$Value[ tolower(db_info$Variable) == "evtable_category" ],
                         "DURATION"  =db_info$Value[ tolower(db_info$Variable) == "evtable_duration" ]);
    
    db_prtable_name <- db_info$Value[ tolower(db_info$Variable) == "prtable" ];
    db_prtable_cols <- c("PATIENT_ID"=db_info$Value[ tolower(db_info$Variable) == "prtable_patient_id" ],
                         "CATEGORIES"=db_info$Value[ tolower(db_info$Variable) == "prtable_categories" ],
                         "PROCESS"   =db_info$Value[ tolower(db_info$Variable) == "prtable_process" ],
                         "PARAMS"    =db_info$Value[ tolower(db_info$Variable) == "prtable_params" ]);
    
    db_retable_name <- db_info$Value[ tolower(db_info$Variable) == "retable" ];
    db_retable_cols <- c(db_prtable_cols, # reuse the columns from the prtable
                         "ESTIMATE"=db_info$Value[ tolower(db_info$Variable) == "retable_estimate" ],
                         "PLOT"    =db_info$Value[ tolower(db_info$Variable) == "retable_plot" ]);
    
    # The object:
    ret_val <- structure(list(# the database specification file and its contents:
                              "db_spec_file"=db_spec_file,
                              "db_info"=db_info,
                              # the database info:
                              "db_type"=db_type,
                              "db_host"=db_host,
                              "db_dsn"=db_dsn,
                              "db_user"=db_user,
                              "db_psswd"=db_psswd,
                              "db_name"=db_name,
                              "db_quote_characters"=switch(db_type, 
                                                           "mariadb"=, 
                                                           "mysql"=,
                                                           "sqlite"=c("`","`"),
                                                           "mssql"=c("[","]"),
                                                           c("`","`")),
                              # the important tables:
                              "db_evtable_name"=db_evtable_name,
                              "db_evtable_cols"=db_evtable_cols,
                              "db_prtable_name"=db_prtable_name,
                              "db_prtable_cols"=db_prtable_cols,
                              "db_retable_name"=db_retable_name,
                              "db_retable_cols"=db_retable_cols,
                              # the actual connection:
                              "db_connection"=db_connection),
      class="SQL_db");
    
    if( connect_to_db )
    {
      ret_val <- connect(ret_val);
      reset_results(ret_val);
    }
    
    return (ret_val);
  } else
  { 
    return (NULL);
  }
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

# Initialise the results table:
reset_results <- function(x) UseMethod("reset_results")
reset_results.SQL_db <- function(x)
{
  if( x$db_type %in% c("mariadb", "mysql", "sqlite") )
  {
    if( get_retable(x) %in% list_tables(x) ) try(DBI::dbExecute(x$db_connection, paste0("TRUNCATE ",get_retable(x)," ;")), silent=TRUE);
  } else if( x$db_type == "mssql" )
  {
    if( get_retable(x) %in% list_tables(x) ) try(RODBC::sqlQuery(x$db_connection, paste0("TRUNCATE ",get_retable(x)," ;")), silent=TRUE);
  }
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
    try(n <- RODBC::sqlQuery(x$db_connection, paste0("SELECT COUNT(*) FROM ",qs(x,get_name(x)),".",qs(x,db_table),";")), silent=TRUE);
    if( is.null(n) || !inherits(n, "data.frame") || nrow(n) != 1 || ncol(n) != 1 )
    {
      # Error retreiving the number of rows:
      n <- NA;
    } else
    {
      n <- as.numeric(n[1,1]);
    }
    db_list <- RODBC::sqlQuery(x$db_connection, paste0("SELECT * FROM ",qs(x,get_name(x)),".INFORMATION_SCHEMA.COLUMNS ORDER BY ORDINAL_POSITION;"));
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

get_spec_file <- function(x) UseMethod("get_spec_file")
get_spec_file.SQL_db <- function(x)
{
  return (x$db_spec_file);
}

get_info <- function(x) UseMethod("get_info")
get_info.SQL_db <- function(x)
{
  return (x$db_info);
}

get_type <- function(x) UseMethod("get_type")
get_type.SQL_db <- function(x)
{
  return (x$db_type);
}

get_host <- function(x) UseMethod("get_host")
get_host.SQL_db <- function(x)
{
  return (x$db_host);
}

get_dsn <- function(x) UseMethod("get_dsn")
get_dsn.SQL_db <- function(x)
{
  return (x$db_dsn);
}

get_user <- function(x) UseMethod("get_user")
get_user.SQL_db <- function(x)
{
  return (x$db_user);
}

get_psswd <- function(x) UseMethod("get_psswd")
get_psswd.SQL_db <- function(x)
{
  return (x$db_psswd);
}

get_name <- function(x) UseMethod("get_name")
get_name.SQL_db <- function(x)
{
  return (x$db_name);
}

get_evtable <- function(x) UseMethod("get_evtable")
get_evtable.SQL_db <- function(x)
{
  return (x$db_evtable_name);
}

get_evtable_id_col <- function(x) UseMethod("get_evtable_id_col")
get_evtable_id_col.SQL_db <- function(x)
{
  return (x$db_evtable_cols["PATIENT_ID"]);
}

get_evtable_date_col <- function(x) UseMethod("get_evtable_date_col")
get_evtable_date_col.SQL_db <- function(x)
{
  return (x$db_evtable_cols["DATE"]);
}

get_evtable_perday_col <- function(x) UseMethod("get_evtable_perday_col")
get_evtable_perday_col.SQL_db <- function(x)
{
  return (x$db_evtable_cols["PERDAY"]);
}

get_evtable_category_col <- function(x) UseMethod("get_evtable_category_col")
get_evtable_category_col.SQL_db <- function(x)
{
  return (x$db_evtable_cols["CATEGORY"]);
}

get_evtable_duration_col <- function(x) UseMethod("get_evtable_duration_col")
get_evtable_duration_col.SQL_db <- function(x)
{
  return (x$db_evtable_cols["DURATION"]);
}

get_prtable <- function(x) UseMethod("get_prtable")
get_prtable.SQL_db <- function(x)
{
  return (x$db_prtable_name);
}

get_prtable_id_col <- function(x) UseMethod("get_prtable_id_col")
get_prtable_id_col.SQL_db <- function(x)
{
  return (x$db_prtable_cols["PATIENT_ID"]);
}

get_prtable_categories_col <- function(x) UseMethod("get_prtable_categories_col")
get_prtable_categories_col.SQL_db <- function(x)
{
  return (x$db_prtable_cols["CATEGORIES"]);
}

get_prtable_process_col <- function(x) UseMethod("get_prtable_process_col")
get_prtable_process_col.SQL_db <- function(x)
{
  return (x$db_prtable_cols["PROCESS"]);
}

get_prtable_params_col <- function(x) UseMethod("get_prtable_params_col")
get_prtable_params_col.SQL_db <- function(x)
{
  return (x$db_prtable_cols["PARAMS"]);
}

get_retable <- function(x) UseMethod("get_retable")
get_retable.SQL_db <- function(x)
{
  return (x$db_retable_name);
}

get_retable_id_col <- function(x) UseMethod("get_retable_id_col")
get_retable_id_col.SQL_db <- function(x)
{
  return (get_prtable_id_col(x));
}

get_retable_categories_col <- function(x) UseMethod("get_retable_categories_col")
get_retable_categories_col.SQL_db <- function(x)
{
  return (get_prtable_categories_col(x));
}

get_retable_process_col <- function(x) UseMethod("get_retable_process_col")
get_retable_process_col.SQL_db <- function(x)
{
  return (get_prtable_process_col(x));
}

get_retable_params_col <- function(x) UseMethod("get_retable_params_col")
get_retable_params_col.SQL_db <- function(x)
{
  return (get_prtable_params_col(x));
}

get_retable_estimate_col <- function(x) UseMethod("get_retable_estimate_col")
get_retable_estimate_col.SQL_db <- function(x)
{
  return (x$db_retable_cols["ESTIMATE"]);
}

get_retable_plot_col <- function(x) UseMethod("get_retable_plot_col")
get_retable_plot_col.SQL_db <- function(x)
{
  return (x$db_retable_cols["PLOT"]);
}


##
## List the patient ids in the events table ####
##

list_evtable_patients <- function(x) UseMethod("list_evtable_patients")
list_evtable_patients.SQL_db <- function(x)
{
  patient_ids <- NULL;
  
  if( x$db_type %in% c("mariadb", "mysql", "sqlite") )
  {
    tmp <- NULL;
    try(tmp <- DBI::dbGetQuery(x$db_connection, paste0("SELECT DISTINCT ",qs(x,get_evtable_id_col(x))," FROM ",
                                                       qs(x,get_evtable(x)),";")), silent=TRUE);
    if( !is.null(tmp) && inherits(tmp, "data.frame") && nrow(tmp) > 0 ) patient_ids <- as.character(tmp[,1]);
  } else if( x$db_type == "mssql" )
  {
    tmp <- NULL;
    try(tmp <- RODBC::sqlQuery(x$db_connection, paste0("SELECT DISTINCT ",qs(x,get_evtable_id_col(x))," FROM ",
                                                       qs(x,get_name(x)),".",qs(x,get_evtable(x)),";")), 
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
                                      " FROM ",qs(x,get_evtable(x)),
                                      " WHERE ",qs(x,get_evtable_id_col(x)),
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
                                      " FROM ",qs(x,get_name(x)),".",qs(x,get_evtable(x)),
                                      " WHERE ",qs(x,get_evtable_id_col(x)),
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
## Database checks ####
##

# Check if the events table exists, contains the expected columns, and is not empty:
check_evtable <- function(x) UseMethod("check_evtable")
check_evtable.SQL_db <- function(x)
{
  db_tables <- list_tables(x);
  if( !(get_evtable(x) %in% db_tables) )
  {
    stop(paste0("The required events table '",get_evtable(x),"' does not seem to exist in the database!\n"));
    return (FALSE);
  }
  
  db_evtable_info <- get_cols_info(x, get_evtable(x));
  if( is.null(db_evtable_info) || nrow(db_evtable_info) == 0 )
  {
    stop(paste0("Cannot get the information about the events table '",get_evtable(x),"'!\n"));
    return (FALSE);
  }
  if( !(get_evtable_id_col(x) %in% db_evtable_info$column) )
  {
    stop(paste0("The required column PATIENT_ID ('",get_evtable_id_col(x),"') does not seem to exist in the events table '",get_evtable(x),"'!\n"));
    return (FALSE);
  }
  if( !(get_evtable_date_col(x) %in% db_evtable_info$column) )
  {
    stop(paste0("The required column DATE ('",get_evtable_date_col(x),"') does not seem to exist in the events table '",get_evtable(x),"'!\n"));
    return (FALSE);
  }
  if( !(get_evtable_perday_col(x) %in% db_evtable_info$column) )
  {
    stop(paste0("The required column PERDAY ('",get_evtable_perday_col(x),"') does not seem to exist in the events table '",get_evtable(x),"'!\n"));
    return (FALSE);
  }
  if( db_evtable_info$nrow[1] == 0 )
  {
    stop(paste0("The events table '",get_evtable(x),"' seems empty!\n"));
    return (FALSE);
  }
  
  return (TRUE);
}

# Check if the processing table exists, and if so, if it contains the expected columns, and is not empty:
check_prtable <- function(x) UseMethod("check_prtable")
check_prtable.SQL_db <- function(x)
{
  db_tables <- list_tables(x);
  if( !(get_prtable(x) %in% db_tables) )
  {
    # The processing table does not exist: using the default for everybody
    warning("The processing table does not exist: using the defaults for all patients...\n")
    return (TRUE);
  }
  
  **HERE!!!**
  
  db_evtable_info <- get_cols_info(x, get_evtable(x));
  if( is.null(db_evtable_info) || nrow(db_evtable_info) == 0 )
  {
    stop(paste0("Cannot get the information about the events table '",get_evtable(x),"'!\n"));
    return (FALSE);
  }
  if( !(get_evtable_id_col(x) %in% db_evtable_info$column) )
  {
    stop(paste0("The required column PATIENT_ID ('",get_evtable_id_col(x),"') does not seem to exist in the events table '",get_evtable(x),"'!\n"));
    return (FALSE);
  }
  if( !(get_evtable_date_col(x) %in% db_evtable_info$column) )
  {
    stop(paste0("The required column DATE ('",get_evtable_date_col(x),"') does not seem to exist in the events table '",get_evtable(x),"'!\n"));
    return (FALSE);
  }
  if( !(get_evtable_perday_col(x) %in% db_evtable_info$column) )
  {
    stop(paste0("The required column PERDAY ('",get_evtable_perday_col(x),"') does not seem to exist in the events table '",get_evtable(x),"'!\n"));
    return (FALSE);
  }
  if( db_evtable_info$nrow[1] == 0 )
  {
    stop(paste0("The events table '",get_evtable(x),"' seems empty!\n"));
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
    if( get_evtable(x) %in% db_tables ) DBI::dbExecute(x$db_connection, paste0("DROP TABLE ",qs(x,get_name(x)),".",qs(x,get_evtable(x)),";"));
    DBI::dbExecute(x$db_connection, paste0("CREATE TABLE ",qs(x,get_name(x)),".",qs(x,get_evtable(x))," ( ",
                                           qs(x,get_evtable_id_col(x)),      " INT NOT NULL, ",
                                           qs(x,get_evtable_date_col(x)),    " DATE NOT NULL, ",
                                           qs(x,get_evtable_perday_col(x)),  " INT NOT NULL, ",
                                           qs(x,get_evtable_category_col(x))," VARCHAR(1024) NOT NULL, ",
                                           qs(x,get_evtable_duration_col(x))," INT NOT NULL);"));
    # Fill it in one by one (for some reason, saving the whole data.frame doesn't seems to be working):
    for( i in 1:nrow(d) )
    {
      DBI::dbExecute(x$db_connection, paste0("INSERT INTO ",qs(x,get_name(x)),".",qs(x,get_evtable(x))," VALUES (",
                                             paste0("'",as.character(d[i,]),"'",collapse=","),
                                             ");"));
    }
    
    # The table specifying what to do to which entries: 
    if( get_prtable(x) %in% db_tables ) DBI::dbExecute(x$db_connection, paste0("DROP TABLE ",qs(x,get_name(x)),".",qs(x,get_prtable(x)),";"));
    DBI::dbExecute(x$db_connection, paste0("CREATE TABLE ",qs(x,get_name(x)),".",qs(x,get_prtable(x))," ( ",
                                           qs(x,get_prtable_id_col(x)),        " INT NOT NULL, ",
                                           qs(x,get_prtable_categories_col(x))," VARCHAR(1024) NOT NULL, ",
                                           qs(x,get_prtable_process_col(x)),   " VARCHAR(128) NOT NULL, ",
                                           qs(x,get_prtable_params_col(x)),    " VARCHAR(10240) NOT NULL);"));
    # Fill it in one by one:
    DBI::dbExecute(x$db_connection, paste0("INSERT INTO ",qs(x,get_name(x)),".",qs(x,get_prtable(x))," VALUES ('1', 'medA', 'CMA2',      '');"));
    DBI::dbExecute(x$db_connection, paste0("INSERT INTO ",qs(x,get_name(x)),".",qs(x,get_prtable(x))," VALUES ('1', 'medA', 'CMA7',      '');"));
    DBI::dbExecute(x$db_connection, paste0("INSERT INTO ",qs(x,get_name(x)),".",qs(x,get_prtable(x))," VALUES ('1', 'medA', 'plot.CMA0', '');"));
    DBI::dbExecute(x$db_connection, paste0("INSERT INTO ",qs(x,get_name(x)),".",qs(x,get_prtable(x))," VALUES ('1', 'medA', 'plot.CMA7', '');"));
    DBI::dbExecute(x$db_connection, paste0("INSERT INTO ",qs(x,get_name(x)),".",qs(x,get_prtable(x))," VALUES ('2', '',     'CMA9',      '');"));
    DBI::dbExecute(x$db_connection, paste0("INSERT INTO ",qs(x,get_name(x)),".",qs(x,get_prtable(x))," VALUES ('2', '',     'plot.CMA0', '');"));
    DBI::dbExecute(x$db_connection, paste0("INSERT INTO ",qs(x,get_name(x)),".",qs(x,get_prtable(x))," VALUES ('2', '',     'plot.CMA9', '');"));
    
    # The results table: 
    if( get_retable(x) %in% db_tables ) DBI::dbExecute(x$db_connection, paste0("DROP TABLE ",qs(x,get_name(x)),".",qs(x,get_retable(x)),";"));
    DBI::dbExecute(x$db_connection, paste0("CREATE TABLE ",qs(x,get_name(x)),".",qs(x,get_retable(x))," ( ",
                                           qs(x,get_retable_id_col(x)),        " INT NOT NULL, ",
                                           qs(x,get_retable_categories_col(x))," VARCHAR(1024) NOT NULL, ",
                                           qs(x,get_retable_process_col(x)),   " VARCHAR(128) NOT NULL, ",
                                           qs(x,get_retable_params_col(x)),    " VARCHAR(10240) NOT NULL, ",
                                           qs(x,get_retable_estimate_col(x)),  " VARCHAR(10240) NOT NULL, ",
                                           qs(x,get_retable_plot_col(x)),      " BLOB NOT NULL);"));
    
  } else if( x$db_type == "mssql" )
  {
    # Delete and (re)create the needed tables:
    db_tables <- list_tables(x);
    
    # The events table:
    if( get_evtable(x) %in% db_tables ) RODBC::sqlQuery(x$db_connection, paste0("DROP TABLE ",qs(x,get_name(x)),".",qs(x,get_evtable(x)),";"));
    RODBC::sqlQuery(x$db_connection, paste0("CREATE TABLE ",qs(x,get_name(x)),".",qs(x,get_evtable(x))," ( ",
                                            qs(x,get_evtable_id_col(x))," INT NOT NULL, ",
                                            qs(x,get_evtable_date_col(x))," DATE NOT NULL, ",
                                            qs(x,get_evtable_perday_col(x))," INT NOT NULL, ",
                                            qs(x,get_evtable_category_col(x))," VARCHAR(1024) NOT NULL, ",
                                            qs(x,get_evtable_duration_col(x))," INT NOT NULL);"));
    # Fill it in one by one (for some reason, saving the whole data.frame doesn't seems to be working):
    for( i in 1:nrow(d) )
    {
      RODBC::sqlQuery(x$db_connection, paste0("INSERT INTO ",qs(x,get_name(x)),".",qs(x,get_evtable(x))," VALUES (",
                                              paste0("'",as.character(d[i,]),"'",collapse=","),
                                              ");"));
    }
    
  }
}
                                    
