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

SQL_db <- function(db_spec_file=NA,       # the file containing the database specification (or NA for the defaults)
                   connect_to_db=TRUE,    # try to connect to the database?
                   check_db=connect_to_db # check the consistency of the database?
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
    
    # The events table:
    db_evtable_name <- db_info$Value[ tolower(db_info$Variable) == "evtable" ];
    db_evtable_cols <- c("PATIENT_ID"     =db_info$Value[ tolower(db_info$Variable) == "evtable_patient_id" ],
                         "DATE"           =db_info$Value[ tolower(db_info$Variable) == "evtable_date" ],
                         "PERDAY"         =db_info$Value[ tolower(db_info$Variable) == "evtable_perday" ],
                         "CATEGORY"       =db_info$Value[ tolower(db_info$Variable) == "evtable_category" ],
                         "DURATION"       =db_info$Value[ tolower(db_info$Variable) == "evtable_duration" ]);
    
    # The processing to be done table:
    db_prtable_name <- db_info$Value[ tolower(db_info$Variable) == "prtable" ];
    db_prtable_cols <- c("KEY"            =db_info$Value[ tolower(db_info$Variable) == "prtable_key" ],
                         "PATIENT_ID"     =db_info$Value[ tolower(db_info$Variable) == "prtable_patient_id" ],
                         "CATEGORIES"     =db_info$Value[ tolower(db_info$Variable) == "prtable_categories" ],
                         "PROCESS"        =db_info$Value[ tolower(db_info$Variable) == "prtable_process" ],
                         "PARAMS"         =db_info$Value[ tolower(db_info$Variable) == "prtable_params" ]);
    
    # The main results table:
    db_retable_name <- db_info$Value[ tolower(db_info$Variable) == "retable" ];
    db_retable_cols <- c("KEY"            =db_info$Value[ tolower(db_info$Variable) == "retable_key" ],
                         "PROC_KEY_REF"   =db_info$Value[ tolower(db_info$Variable) == "retable_proc_key_ref" ],
                         "ESTIMATE"       =db_info$Value[ tolower(db_info$Variable) == "retable_estimate" ],
                         "ESTIMATE_TYPE"  =db_info$Value[ tolower(db_info$Variable) == "retable_estimate_type" ],
                         "PLOT_JPG"       =db_info$Value[ tolower(db_info$Variable) == "retable_plot_jpg" ],
                         "PLOT_HTML"      =db_info$Value[ tolower(db_info$Variable) == "retable_plot_html" ]);
    
    # The sliding windows results table:
    db_swtable_name <- db_info$Value[ tolower(db_info$Variable) == "swtable" ];
    db_swtable_cols <- c("RES_KEY_REF"    =db_info$Value[ tolower(db_info$Variable) == "swtable_res_key_ref" ],
                         "PATIENT_ID"     =db_info$Value[ tolower(db_info$Variable) == "swtable_patient_id" ],
                         "WINDOW_ID"      =db_info$Value[ tolower(db_info$Variable) == "swtable_window_id" ],
                         "WINDOW_START"   =db_info$Value[ tolower(db_info$Variable) == "swtable_window_start" ],
                         "WINDOW_END"     =db_info$Value[ tolower(db_info$Variable) == "swtable_window_end" ],
                         "WINDOW_ESTIMATE"=db_info$Value[ tolower(db_info$Variable) == "swtable_window_estimate" ]);
    
    # The per episode results table:
    db_petable_name <- db_info$Value[ tolower(db_info$Variable) == "petable" ];
    db_petable_cols <- c("RES_KEY_REF"      =db_info$Value[ tolower(db_info$Variable) == "petable_res_key_ref" ],
                         "PATIENT_ID"       =db_info$Value[ tolower(db_info$Variable) == "petable_patient_id" ],
                         "EPISODE_ID"       =db_info$Value[ tolower(db_info$Variable) == "petable_episode_id" ],
                         "EPISODE_START"    =db_info$Value[ tolower(db_info$Variable) == "petable_episode_start" ],
                         "GAP_DAYS"         =db_info$Value[ tolower(db_info$Variable) == "petable_gap_days" ],
                         "ESPISODE_DURATION"=db_info$Value[ tolower(db_info$Variable) == "petable_episode_duration" ],
                         "EPISODE_END"      =db_info$Value[ tolower(db_info$Variable) == "petable_episode_end" ],
                         "EPISODE_ESTIMATE" =db_info$Value[ tolower(db_info$Variable) == "petable_episode_estimate" ]);
    
    # Parse the default processing into a data.frame wth format "cats", "type", "proc" and "params":
    tmp <- trimws(strsplit(db_info$Value[ tolower(db_info$Variable) == "default_processing_and_plotting" ], ";", fixed=TRUE)[[1]]);
    if( length(tmp) < 1 )
    {
      # No defaults define, fall-bak to the built-in defaults:
      db_default_proc <- data.frame("key"=-1, "cats"=c(NA), "type"=c("plot"), "proc"=c("CMA0"), "params"=c(NA));
    } else
    {
      # Parse it:
      db_default_proc <- as.data.frame(do.call(rbind, lapply(tmp, function(s)
        {
          # Return value:
          ret_val = c("key"=-1, "cats"=NA, "type"=NA, "proc"=NA, "params"=NA);
          
          # Split the params:
          tmp2 <- trimws(strsplit(s, "(", fixed=TRUE)[[1]]);
          
          # Type of processing and name:
          if( substr(tmp2,1,nchar("plot.")) == "plot." )
          {
            ret_val["type"] <- "plot"; ret_val["proc"] <- substr(tmp2,nchar("plot.")+1,nchar(tmp2));
          } else
          {
            ret_val["type"] <- "CMA"; ret_val["proc"] <- tmp2;
          }
          
          # The params:
          if( length(tmp2) > 1 )
          {
            ret_val["params"] <- tmp2[2];
          }
          
          return (ret_val);
        })));
    }

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
                              "db_swtable_name"=db_swtable_name,
                              "db_swtable_cols"=db_swtable_cols,
                              "db_petable_name"=db_petable_name,
                              "db_petable_cols"=db_petable_cols,
                              # defaults:
                              "db_default_proc"=db_default_proc,
                              "use_default_proc_for_all"=FALSE, # should the default processing be used for all patients?
                              # the actual connection:
                              "db_connection"=db_connection),
      class="SQL_db");
    
    # Connect to the database?
    if( connect_to_db )
    {
      # Attempt connection:
      ret_val <- connect(ret_val);
      
      if( check_db )
      {
        tmp <- ret_val;
        ## Check if the tables exist and contain the expected columns and are not empty:
        if( is.null(tmp <- check_evtable(ret_val)) )
        {
          stop(paste0("The events table '",get_evtable(ret_val),"' failed the safety checks!\n"));
          return (NULL);
        }
        
        if( is.null(tmp <- check_prtable(ret_val)) )
        {
          stop(paste0("The processing table '",get_prtable(ret_val),"' failed the safety checks!\n"));
          return (NULL);
        }
        
        ret_val <- tmp; # make sure we keep the various check results
      }
      
      # Reset the results table:
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

# Initialise the results tables:
reset_results <- function(x) UseMethod("reset_results")
reset_results.SQL_db <- function(x)
{
  if( x$db_type %in% c("mariadb", "mysql", "sqlite") )
  {
    if( get_retable(x) %in% list_tables(x) ) try(DBI::dbExecute(x$db_connection, paste0("TRUNCATE ",get_retable(x)," ;")), silent=TRUE);
    if( get_swtable(x) %in% list_tables(x) ) try(DBI::dbExecute(x$db_connection, paste0("TRUNCATE ",get_swtable(x)," ;")), silent=TRUE);
  } else if( x$db_type == "mssql" )
  {
    if( get_retable(x) %in% list_tables(x) ) try(RODBC::sqlQuery(x$db_connection, paste0("TRUNCATE ",get_retable(x)," ;")), silent=TRUE);
    if( get_swtable(x) %in% list_tables(x) ) try(RODBC::sqlQuery(x$db_connection, paste0("TRUNCATE ",get_swtable(x)," ;")), silent=TRUE);
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

get_prtable_key_col <- function(x) UseMethod("get_prtable_key_col")
get_prtable_key_col.SQL_db <- function(x)
{
  return (x$db_prtable_cols["KEY"]);
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

get_retable_key_col <- function(x) UseMethod("get_retable_key_col")
get_retable_key_col.SQL_db <- function(x)
{
  return (x$db_retable_cols["KEY"]);
}

get_retable <- function(x) UseMethod("get_retable")
get_retable.SQL_db <- function(x)
{
  return (x$db_retable_name);
}

get_retable_proc_key_ref_col <- function(x) UseMethod("get_retable_proc_key_ref_col")
get_retable_proc_key_ref_col.SQL_db <- function(x)
{
  return (x$db_retable_cols["PROC_KEY_REF"]);
}

get_retable_id_col <- function(x) UseMethod("get_retable_id_col")
get_retable_id_col.SQL_db <- function(x)
{
  return (get_evtable_id_col(x));
}

# get_retable_categories_col <- function(x) UseMethod("get_retable_categories_col")
# get_retable_categories_col.SQL_db <- function(x)
# {
#   return (get_prtable_categories_col(x));
# }
# 
# get_retable_process_col <- function(x) UseMethod("get_retable_process_col")
# get_retable_process_col.SQL_db <- function(x)
# {
#   return (get_prtable_process_col(x));
# }
# 
# get_retable_params_col <- function(x) UseMethod("get_retable_params_col")
# get_retable_params_col.SQL_db <- function(x)
# {
#   return (get_prtable_params_col(x));
# }

get_retable_estimate_col <- function(x) UseMethod("get_retable_estimate_col")
get_retable_estimate_col.SQL_db <- function(x)
{
  return (x$db_retable_cols["ESTIMATE"]);
}

get_retable_estimate_type_col <- function(x) UseMethod("get_retable_estimate_type_col")
get_retable_estimate_type_col.SQL_db <- function(x)
{
  return (x$db_retable_cols["ESTIMATE_TYPE"]);
}

get_retable_plot_jpg_col <- function(x) UseMethod("get_retable_plot_jpg_col")
get_retable_plot_jpg_col.SQL_db <- function(x)
{
  return (x$db_retable_cols["PLOT_JPG"]);
}

get_retable_plot_html_col <- function(x) UseMethod("get_retable_plot_html_col")
get_retable_plot_html_col.SQL_db <- function(x)
{
  return (x$db_retable_cols["PLOT_HTML"]);
}

get_swtable <- function(x) UseMethod("get_swtable")
get_swtable.SQL_db <- function(x)
{
  return (x$db_swtable_name);
}

get_swtable_res_key_ref_col <- function(x) UseMethod("get_swtable_res_key_ref_col")
get_swtable_res_key_ref_col.SQL_db <- function(x)
{
  return (x$db_swtable_cols["RES_KEY_REF"]);
}

get_swtable_patient_id_col <- function(x) UseMethod("get_swtable_patient_id_col")
get_swtable_patient_id_col.SQL_db <- function(x)
{
  return (x$db_swtable_cols["PATIENT_ID"]);
}

get_swtable_window_id_col <- function(x) UseMethod("get_swtable_window_id_col")
get_swtable_window_id_col.SQL_db <- function(x)
{
  return (x$db_swtable_cols["WINDOW_ID"]);
}

get_swtable_window_start_col <- function(x) UseMethod("get_swtable_window_start_col")
get_swtable_window_start_col.SQL_db <- function(x)
{
  return (x$db_swtable_cols["WINDOW_START"]);
}

get_swtable_window_end_col <- function(x) UseMethod("get_swtable_window_end_col")
get_swtable_window_end_col.SQL_db <- function(x)
{
  return (x$db_swtable_cols["WINDOW_END"]);
}

get_swtable_window_estimate_col <- function(x) UseMethod("get_swtable_window_estimate_col")
get_swtable_window_estimate_col.SQL_db <- function(x)
{
  return (x$db_swtable_cols["WINDOW_ESTIMATE"]);
}

get_petable <- function(x) UseMethod("get_petable")
get_petable.SQL_db <- function(x)
{
  return (x$db_petable_name);
}

get_petable_res_key_ref_col <- function(x) UseMethod("get_petable_res_key_ref_col")
get_petable_res_key_ref_col.SQL_db <- function(x)
{
  return (x$db_petable_cols["RES_KEY_REF"]);
}

get_petable_patient_id_col <- function(x) UseMethod("get_petable_patient_id_col")
get_petable_patient_id_col.SQL_db <- function(x)
{
  return (x$db_petable_cols["PATIENT_ID"]);
}

get_petable_episode_id_col <- function(x) UseMethod("get_petable_episode_id_col")
get_petable_episode_id_col.SQL_db <- function(x)
{
  return (x$db_petable_cols["EPISODE_ID"]);
}

get_petable_episode_start_col <- function(x) UseMethod("get_petable_episode_start_col")
get_petable_episode_start_col.SQL_db <- function(x)
{
  return (x$db_petable_cols["EPISODE_START"]);
}

get_petable_gap_days_col <- function(x) UseMethod("get_petable_gap_days_col")
get_petable_gap_days_col.SQL_db <- function(x)
{
  return (x$db_petable_cols["GAP_DAYS"]);
}

get_petable_episode_duration_col <- function(x) UseMethod("get_petable_episode_duration_col")
get_petable_episode_duration_col.SQL_db <- function(x)
{
  return (x$db_petable_cols["ESPISODE_DURATION"]);
}

get_petable_episode_end_col <- function(x) UseMethod("get_petable_episode_end_col")
get_petable_episode_end_col.SQL_db <- function(x)
{
  return (x$db_petable_cols["EPISODE_END"]);
}

get_petable_episode_estimate_col <- function(x) UseMethod("get_petable_episode_estimate_col")
get_petable_episode_estimate_col.SQL_db <- function(x)
{
  return (x$db_petable_cols["EPISODE_ESTIMATE"]);
}

get_default_processing <- function(x) UseMethod("get_default_processing")
get_default_processing.SQL_db <- function(x)
{
  return (x$db_default_proc);
}


##
## Results tables ####
##

write_retable_entry <- function(x, id, key_proc, categories="", type="", proc="", params="", estimate=NA, estimate_type=NA, plot_jpg="", plot_html="") UseMethod("write_retable_entry")
write_retable_entry.SQL_db <- function(x, id, key_proc, categories="", type="", proc="", params="", estimate=NA, estimate_type=NA, plot_jpg="", plot_html="")
{
  # Write all these info into the retable:
  if( x$db_type %in% c("mariadb", "mysql", "sqlite") )
  {
    result <- DBI::dbExecute(x$db_connection, 
                             paste0("INSERT INTO ",qs(x,get_name(x)),".",qs(x,get_retable(x)),
                                    "(",
                                    qs(x,get_evtable_id_col(x)),", ",
                                    qs(x,get_retable_proc_key_ref_col(x)),", ",
                                    qs(x,get_retable_estimate_col(x)),", ",
                                    qs(x,get_retable_estimate_type_col(x)),", ",
                                    qs(x,get_retable_plot_jpg_col(x)),", ",
                                    qs(x,get_retable_plot_html_col(x)),
                                    ")",
                                    " VALUES (",
                                    "'",id,"', ", # id
                                    "'",key_proc,"', ", # reference to the processing key
                                    #"'",categories,"', ", # categories
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

get_retable_key_for_results <- function(x, id, key, estimate_type) UseMethod("get_retable_key_for_results")
get_retable_key_for_results.SQL_db <- function(x, id, key, estimate_type)
{
  # Get the matching info:
  if( x$db_type %in% c("mariadb", "mysql", "sqlite") )
  {
    # Get the res_key of the previous insertion in the retable:
    res_key_ref <- DBI::dbGetQuery(x$db_connection, 
                                   paste0("SELECT ", qs(x,get_retable_key_col(x)),
                                          " FROM ", qs(x,get_name(x)),".",qs(x,get_retable(x)), 
                                          " WHERE ", qs(x,get_retable_id_col(x)), " = '", id, "'", 
                                          " AND ", qs(x,get_retable_proc_key_ref_col(x)), " = '", key, "'",
                                          " AND ", qs(x,get_retable_estimate_type_col(x)), " = '", estimate_type, "'",
                                          ";"));
    if( is.null(res_key_ref) || nrow(res_key_ref) != 1 )
    {
      # Error identifying the last inserted row!
      return (NULL);
    } else
    {
      return (res_key_ref[1,1]);
    }
  } else if( x$db_type == "mssql" )
  {s
  }
  
  return (NULL);
}

write_swtable_entry <- function(x, res_key_ref=-1, cma=NULL) UseMethod("write_swtable_entry")
write_swtable_entry.SQL_db <- function(x, res_key_ref=-1, cma=NULL)
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
                             paste0("INSERT INTO ",qs(x,get_name(x)),".",qs(x,get_swtable(x)),
                                    " (",
                                    qs(x,get_swtable_res_key_ref_col(x)),", ",
                                    qs(x,get_swtable_patient_id_col(x)),", ",
                                    qs(x,get_swtable_window_id_col(x)),", ",
                                    qs(x,get_swtable_window_start_col(x)),", ",
                                    qs(x,get_swtable_window_end_col(x)),", ",
                                    qs(x,get_swtable_window_estimate_col(x)),
                                    ")",
                                    " VALUES ",
                                    paste0("(",
                                           vapply(1:nrow(cma), 
                                                  function(i) 
                                                    paste0("'", res_key_ref, "', ",
                                                           "'", cma[i,get_evtable_id_col(x)], "', ",
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

write_petable_entry <- function(x, res_key_ref=-1, cma=NULL) UseMethod("write_petable_entry")
write_petable_entry.SQL_db <- function(x, res_key_ref=-1, cma=NULL)
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
                             paste0("INSERT INTO ",qs(x,get_name(x)),".",qs(x,get_petable(x)),
                                    " (",
                                    qs(x,get_petable_res_key_ref_col(x)),", ",
                                    qs(x,get_petable_patient_id_col(x)),", ",
                                    qs(x,get_petable_episode_id_col(x)),", ",
                                    qs(x,get_petable_episode_start_col(x)),", ",
                                    qs(x,get_petable_gap_days_col(x)),", ",
                                    qs(x,get_petable_episode_duration_col(x)),", ",
                                    qs(x,get_petable_episode_end_col(x)),", ",
                                    qs(x,get_petable_episode_estimate_col(x)),
                                    ")",
                                    " VALUES ",
                                    paste0("(",
                                           vapply(1:nrow(cma), 
                                                  function(i) 
                                                    paste0("'", res_key_ref, "', ",
                                                           "'", cma[i,get_evtable_id_col(x)], "', ",
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
## Processings ####
##

# Get the list of possible processing for a list patient ids:
get_processings_for_patient <- function(x, patient_id) UseMethod("get_processings_for_patient")
get_processings_for_patient.SQL_db <- function(x, patient_id)
{
  # Are we using the defaults for everybody?
  if( x$use_default_proc_for_all )
  {
    return (x$db_default_proc);
  }

  db_procs <- NULL;
  
  if( x$db_type %in% c("mariadb", "mysql", "sqlite") )
  {
    tmp <- NULL;
    try(tmp <- DBI::dbGetQuery(x$db_connection, 
                               paste0("SELECT *",
                                      " FROM ",qs(x,get_prtable(x)),
                                      " WHERE ",qs(x,get_prtable_id_col(x)),
                                      " IN (",paste0("'",patient_id,"'",collapse=","),")",
                                      ";")),
        silent=TRUE);
    if( !is.null(tmp) && inherits(tmp, "data.frame") && nrow(tmp) > 0 ) db_procs <- tmp;
  } else if( x$db_type == "mssql" )
  {
    tmp <- NULL;
    try(tmp <- RODBC::sqlQuery(x$db_connection, 
                               paste0("SELECT *",
                                      " FROM ",qs(x,get_name(x)),".",qs(x,get_prtable(x)),
                                      " WHERE ",qs(x,get_prtable_id_col(x)),
                                      " IN (",paste0("'",patient_id,"'",collapse=","),")",
                                      ";")),
        silent=TRUE);
    if( !is.null(tmp) && inherits(tmp, "data.frame") && nrow(tmp) > 0 ) db_procs <- tmp;
  }
  
  if( is.null(db_procs) || nrow(db_procs) < 1 )
  {
    # Use the defaults for this patient:
    return (x$db_default_proc);
  } else
  {
    # Re-arrange in the "key", "cats", "type", "proc", "params" format:
    col_key  <- which(get_prtable_key_col(x) == names(db_procs));        if( length(col_key)  != 1 ) stop(paste0("Error retreiving the processing!\n"));
    col_cats <- which(get_prtable_categories_col(x) == names(db_procs)); if( length(col_cats) != 1 ) stop(paste0("Error retreiving the processing!\n")); 
    col_proc <- which(get_prtable_process_col(x)    == names(db_procs)); if( length(col_proc) != 1 ) stop(paste0("Error retreiving the processing!\n")); 
    col_parm <- which(get_prtable_params_col(x)     == names(db_procs)); if( length(col_parm) != 1 ) stop(paste0("Error retreiving the processing!\n"));
    db_procs <- db_procs[,c(col_key, col_cats, col_proc, col_parm)];
    names(db_procs) <- c(      "key",   "cats",   "proc", "params");
    
    tmp <- do.call(rbind, lapply(1:nrow(db_procs), function(i)
      {
        # Return value:
        ret_val = c("key"=as.character(db_procs$key[i]), "cats"=as.character(db_procs$cats[i]), "type"=NA, "proc"=NA, "params"=NA);
        
        tmp2 <- trimws(as.character(db_procs$proc[i]));
        
        # Type of processing and name:
        if( substr(tmp2,1,nchar("plot.")) == "plot." )
        {
          ret_val["type"] <- "plot"; ret_val["proc"] <- substr(tmp2,nchar("plot.")+1,nchar(tmp2));
        } else
        {
          ret_val["type"] <- "CMA"; ret_val["proc"] <- tmp2;
        }
        
        # The params:
        ret_val["params"] <- as.character(db_procs$params[i]);

        return (ret_val);
      }));
    tmp[tmp==""] <- NA;
      
    return (as.data.frame(tmp));
  }
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
  
  if( procs_class == "" || is.na(procs_class) )
  {
    # Select all!
    return (rep(TRUE, nrow(patient_info)));
  } else
  {
    # Specific selection may be needed:
    
    # Get the medication classes for this patient:
    if( is.na(get_evtable_category_col(x)) )
    {
      # No medication classes:
      return (NULL);
    }
    pat_classes <- patient_info[ , get_evtable_category_col(x) ]; 
    if( is.null(pat_classes) )
    {
      # No medication classes:
      return (NULL);
    }
    
    # Transform the procs_class specification into a logical expression to be evaluated on pat_classes:
    procs_class_expr <- NULL;
    try(procs_class_expr <- parse(text=gsub("]", "')", # replace "[ XX ]" by the actual R test "(pat_classes == 'XX')"
                                            gsub("[", "(pat_classes == '",
                                                 gsub("|", "||", # replace the logical connectors "|" ans "&" by their actual R vectorized counterparts "||" and "&&"
                                                      gsub("&", "&&", 
                                                           procs_class, 
                                                           fixed=TRUE), 
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
                              "ID.colname='",get_evtable_id_col(x),"', ",
                              "event.date.colname='",get_evtable_date_col(x),"', ",
                              "event.duration.colname='",get_evtable_duration_col(x),"', ",
                              ifelse(!is.na(get_evtable_perday_col(x)), paste0("event.daily.dose.colname='",get_evtable_perday_col(x),"', "), ""),
                              ifelse(!is.na(get_evtable_category_col(x)), paste0("medication.class.colname='",get_evtable_category_col(x),"' "), ""),
                              ifelse(!is.na(procs_action$params[1]), paste0(", ",procs_action$params[1]),""), 
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
                                ifelse(!is.na(procs_action$params[1]), paste0(", ",procs_action$params[1]),""), 
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
  return (list("id"=patient_info[ 1, get_evtable_id_col(x) ],
               "key"=procs_action$key[1], "categories"=procs_action$cats[1], "type"=procs_action$type[1], "proc"=procs_action$proc[1], "params"=procs_action$params[1], 
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
  key <- procs_results$key; # the key must be defined! 
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
                                 key       =key,
                                 categories=ifelse(is.na(procs_results$categories), "", as.character(procs_results$categories)), 
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
    # Get the res_key of the previous insertion in the retable:
    res_key_ref <- get_retable_key_for_results(x, id, key, estimate_type);
    if( is.null(res_key_ref) )
    {
      # Error identifying the last inserted row!
      return (FALSE);
    }
    
    if( inherits(procs_results$cma, "CMA_sliding_window") )
    {
      # A sliding windows CMA: write it into the swtable:
      ret_val <- write_swtable_entry(x, res_key_ref, getCMA(procs_results$cma));
      if( !ret_val ) return (FALSE); # some error occured so return
    } else if( inherits(procs_results$cma, "CMA_per_episode") )
    {
      # A per episodes CMA!
      ret_val <- write_petable_entry(x, res_key_ref, getCMA(procs_results$cma));
      if( !ret_val ) return (FALSE); # some error occured so return
    }
  }
  
  return (TRUE);
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
    return (NULL);
  }
  
  db_evtable_info <- get_cols_info(x, get_evtable(x));
  if( is.null(db_evtable_info) || nrow(db_evtable_info) == 0 )
  {
    stop(paste0("Cannot get the information about the events table '",get_evtable(x),"'!\n"));
    return (NULL);
  }
  if( !(get_evtable_id_col(x) %in% db_evtable_info$column) )
  {
    stop(paste0("The required column PATIENT_ID ('",get_evtable_id_col(x),"') does not seem to exist in the events table '",get_evtable(x),"'!\n"));
    return (NULL);
  }
  if( !(get_evtable_date_col(x) %in% db_evtable_info$column) )
  {
    stop(paste0("The required column DATE ('",get_evtable_date_col(x),"') does not seem to exist in the events table '",get_evtable(x),"'!\n"));
    return (NULL);
  }
  if( !(get_evtable_perday_col(x) %in% db_evtable_info$column) )
  {
    stop(paste0("The required column PERDAY ('",get_evtable_perday_col(x),"') does not seem to exist in the events table '",get_evtable(x),"'!\n"));
    return (NULL);
  }
  if( db_evtable_info$nrow[1] == 0 )
  {
    stop(paste0("The events table '",get_evtable(x),"' seems empty!\n"));
    return (NULL);
  }
  
  return (x);
}

# Check if the processing table exists, and if so, if it contains the expected columns, and is not empty:
check_prtable <- function(x) UseMethod("check_prtable")
check_prtable.SQL_db <- function(x)
{
  db_tables <- list_tables(x);
  if( !(get_prtable(x) %in% db_tables) )
  {
    # The processing table does not exist: using the default for everybody
    warning(paste0("The processing table '",get_evtable(x),"' does not exist: using the defaults for all patients...\n"));
    x$use_default_proc_for_all <- TRUE;
    return (x);
  }
  
  db_prtable_info <- get_cols_info(x, get_prtable(x));
  if( db_prtable_info$nrow[1] == 0 )
  {
    # The processing table is empty: using the default for everybody
    warning(paste0("The processing table '",get_evtable(x),"' is empty: using the defaults for all patients...\n"));
    x$use_default_proc_for_all <- TRUE;
    return (x);
  }
  
  if( is.null(db_prtable_info) || nrow(db_prtable_info) == 0 )
  {
    stop(paste0("Cannot get the information about the processing table '",get_prtable(x),"'!\n"));
    return (NULL);
  }
  if( !(get_prtable_id_col(x) %in% db_prtable_info$column) )
  {
    stop(paste0("The required column PATIENT_ID ('",get_prtable_id_col(x),"') does not seem to exist in the processing table '",get_prtable(x),"'!\n"));
    return (NULL);
  }
  if( !(get_prtable_categories_col(x) %in% db_prtable_info$column) )
  {
    stop(paste0("The required column CATEGORIES ('",get_prtable_categories_col(x),"') does not seem to exist in the processing table '",get_prtable(x),"'!\n"));
    return (NULL);
  }
  if( !(get_prtable_params_col(x) %in% db_prtable_info$column) )
  {
    stop(paste0("The required column PARAMS ('",get_prtable_params_col(x),"') does not seem to exist in the processing table '",get_evtable(x),"'!\n"));
    return (NULL);
  }
  
  # Return the updated SQL_db object:
  x$use_default_proc_for_all <- FALSE;
  return (x);
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
                                           qs(x,get_evtable_id_col(x)),      " VARCHAR(256) NOT NULL, ",
                                           qs(x,get_evtable_date_col(x)),    " DATE NOT NULL, ",
                                           qs(x,get_evtable_perday_col(x)),  " INT NOT NULL, ",
                                           qs(x,get_evtable_category_col(x))," VARCHAR(1024) NULL DEFAULT NULL, ",
                                           qs(x,get_evtable_duration_col(x))," INT NULL DEFAULT NULL );"));
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
                                           qs(x,get_prtable_key_col(x)),       " INT NOT NULL AUTO_INCREMENT, ",
                                           qs(x,get_prtable_id_col(x)),        " VARCHAR(256) NOT NULL, ",
                                           qs(x,get_prtable_categories_col(x))," VARCHAR(1024) NULL DEFAULT NULL, ",
                                           qs(x,get_prtable_process_col(x)),   " VARCHAR(128) NULL DEFAULT NULL, ",
                                           qs(x,get_prtable_params_col(x)),    " VARCHAR(10240) NULL DEFAULT NULL, ", 
                                           "PRIMARY KEY (", get_prtable_key_col(x), ") );"));
    # Fill it in one by one:
    tmp <- matrix(c('1', '[medA]',          'CMA2',      '',
                    '1', '[medA]',          'plot.CMA0', '',
                    '1', '[medA] | [medB]', 'plot.CMA7', '',
                    '2', '',                'plot.CMA0', '',
                    '2', '',                'plot.CMA9', '',
                    '3', '',                'plot.CMA0', '',
                    '3', '',                'CMA1',      '',
                    '3', '',                'plot.CMA_sliding_window', 'CMA.to.apply=\"CMA1\", sliding.window.duration=90, sliding.window.no.steps=5',
                    '4', '',                'plot.CMA0', '',
                    '4', '',                'CMA1',      '',
                    '4', '',                'plot.CMA_per_episode', 'CMA.to.apply=\"CMA1\", maximum.permissible.gap=90'), 
                  ncol=4, byrow=TRUE);
    colnames(tmp) <- c(qs(x,get_prtable_id_col(x)), qs(x,get_prtable_categories_col(x)), qs(x,get_prtable_process_col(x)), qs(x,get_prtable_params_col(x)));
    for( i in 1:nrow(tmp) )
    {
      DBI::dbExecute(x$db_connection, paste0("INSERT INTO ",qs(x,get_name(x)),".",qs(x,get_prtable(x)),
                                             "(",paste0(colnames(tmp),collapse=","),")",
                                             " VALUES (",paste0("'",tmp[i,],"'",collapse=", "),");"));
    }

    # The results table: 
    if( get_retable(x) %in% db_tables ) DBI::dbExecute(x$db_connection, paste0("DROP TABLE ",qs(x,get_name(x)),".",qs(x,get_retable(x)),";"));
    DBI::dbExecute(x$db_connection, paste0("CREATE TABLE ",qs(x,get_name(x)),".",qs(x,get_retable(x))," ( ",
                                           qs(x,get_retable_key_col(x)),          " INT NOT NULL AUTO_INCREMENT, ",
                                           qs(x,get_evtable_id_col(x)),           " VARCHAR(256) NOT NULL, ",
                                           qs(x,get_retable_proc_key_ref_col(x)), " INT NULL DEFAULT -1, ",
                                           qs(x,get_retable_estimate_col(x)),     " FLOAT NULL DEFAULT NULL, ",
                                           qs(x,get_retable_estimate_type_col(x))," VARCHAR(256) NULL DEFAULT NULL, ",
                                           qs(x,get_retable_plot_jpg_col(x)),     " LONGBLOB NULL DEFAULT NULL, ",
                                           qs(x,get_retable_plot_html_col(x)),    " LONGBLOB NULL DEFAULT NULL, ", 
                                           "PRIMARY KEY (", get_retable_key_col(x), ") );"));
    
    # The sliding windows results table: 
    if( get_swtable(x) %in% db_tables ) DBI::dbExecute(x$db_connection, paste0("DROP TABLE ",qs(x,get_name(x)),".",qs(x,get_swtable(x)),";"));
    DBI::dbExecute(x$db_connection, paste0("CREATE TABLE ",qs(x,get_name(x)),".",qs(x,get_swtable(x))," ( ",
                                           qs(x,get_swtable_res_key_ref_col(x)),    " INT NULL DEFAULT NULL, ",
                                           qs(x,get_swtable_patient_id_col(x)),     " VARCHAR(256) NOT NULL, ",
                                           qs(x,get_swtable_window_id_col(x)),      " INT NOT NULL, ",
                                           qs(x,get_swtable_window_start_col(x)),   " DATE NOT NULL, ",
                                           qs(x,get_swtable_window_end_col(x)),     " DATE NOT NULL, ",
                                           qs(x,get_swtable_window_estimate_col(x))," FLOAT NULL DEFAULT NULL );"));
    
    # The per episode results table: 
    if( get_petable(x) %in% db_tables ) DBI::dbExecute(x$db_connection, paste0("DROP TABLE ",qs(x,get_name(x)),".",qs(x,get_petable(x)),";"));
    DBI::dbExecute(x$db_connection, paste0("CREATE TABLE ",qs(x,get_name(x)),".",qs(x,get_petable(x))," ( ",
                                           qs(x,get_petable_res_key_ref_col(x)),     " INT NULL DEFAULT NULL, ",
                                           qs(x,get_petable_patient_id_col(x)),      " VARCHAR(256) NOT NULL, ",
                                           qs(x,get_petable_episode_id_col(x)),      " INT NOT NULL, ",
                                           qs(x,get_petable_episode_start_col(x)),   " DATE NOT NULL, ",
                                           qs(x,get_petable_gap_days_col(x)),        " INT NULL DEFAULT NULL, ",
                                           qs(x,get_petable_episode_duration_col(x))," INT NULL DEFAULT NULL, ",
                                           qs(x,get_petable_episode_end_col(x)),     " DATE NOT NULL, ",
                                           qs(x,get_petable_episode_estimate_col(x))," FLOAT NULL DEFAULT NULL );"));
    
  } else if( x$db_type == "mssql" )
  {
    # Delete and (re)create the needed tables:
    db_tables <- list_tables(x);
    
    # The events table:
    if( get_evtable(x) %in% db_tables ) RODBC::sqlQuery(x$db_connection, paste0("DROP TABLE ",qs(x,get_name(x)),".",qs(x,get_evtable(x)),";"));
    RODBC::sqlQuery(x$db_connection, paste0("CREATE TABLE ",qs(x,get_name(x)),".",qs(x,get_evtable(x))," ( ",
                                            qs(x,get_evtable_id_col(x))," VARCHAR(256) NOT NULL, ",
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
                                    
