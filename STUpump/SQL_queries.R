###############################################################################################
#
#    STUpump: using AdhereR for offline scripted processing.
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
## This file implements various SQL-related tasks independently of the particular SQL database used ####
##


##
## Connect/disconnect to/from database ####
##

# Conect of a given type of database and return the connection or stop with an error:
SQL_connect <- function(db_type=c("mariadb", "mysql", "sqlite", "mssql")[1], db_host="localhost", db_dsn=NA, db_user="user", db_psswd="password", db_name=NULL)
{
  db_connection <- NULL;
  
  if( db_type %in% c("mariadb", "mysql") )
  {
    # MariaDB or MySQL:
    require(RMariaDB);
    db_connection <- DBI::dbConnect(RMariaDB::MariaDB(), user=db_user, password=db_psswd, dbname=db_name, host=db_host);
  } else if( db_type == "sqlite" )
  {
    # SQLite:
    require(RSQLite);
    db_connection <- DBI::dbConnect(RSQLite::SQLite(), host=db_host);
  } else if( db_type == "mssql" )
  {
    #  Microsoft SQL Server:
    require(RODBC);
    db_connection <- RODBC::odbcConnect(dsn=db_dsn, uid=db_user, pwd=db_psswd);
    # Check if the database exists:
    db_list <- RODBC::sqlQuery(db_connection, "SELECT name FROM master.sys.databases");
    if( !(db_name %in% db_list$name) )
    {
      stop(paste0("The required database '",db_name,"' does not exist on this server!\n"));
    }
  } else
  {
    db_connection <- NULL;
    stop(paste0("Don't know how to use an SQL database of type '",db_type,"': please specify a MariaDB, MySQL, SQLite or Microsoft SQL Server database!\n"));
  }
  if( is.null(db_connection) )
  {
    # Something bad happened:
    stop("Error connecting to the specified database!\n");
  }
  
  # Return the connection:
  return (db_connection);
}

# Disconnect from database:
SQL_disconnect <- function(db_type=c("mariadb", "mysql", "sqlite", "mssql")[1], db_connection)
{
  if( db_type %in% c("mariadb", "mysql", "sqlite") )
  {
    try(DBI::dbDisconnect(db_connection), silent=TRUE);
  } else if( db_type == "mssql" )
  {
    try(RODBC::odbcClose(db_connection), silent=TRUE);
  }
}


##
## Retreive various database info ####
##

# List tables in the database:
SQL_list_tables <- function(db_type=c("mariadb", "mysql", "sqlite", "mssql")[1], db_connection, db_name=NULL)
{
  db_tables <- NULL;
  
  if( db_type %in% c("mariadb", "mysql", "sqlite") )
  {
    db_tables <- DBI::dbListTables(d);
  } else if( db_type == "mssql" )
  {
    db_list <- RODBC::sqlQuery(db_connection, paste0("SELECT * FROM ",db_name,".information_schema.tables;")); # list the tables
    db_tables <- paste0(db_list$TABLE_SCHEMA,".",db_list$TABLE_NAME); # reconstruct the tables' names
  }
  
  # Return the list tables' names:
  return (db_tables);
}

# Get the column info for a given table:
SQL_get_cols_info <- function(db_type=c("mariadb", "mysql", "sqlite", "mssql")[1], db_connection, db_table, db_name=NULL)
{
  db_cols_info <- NULL;
  
  if( db_type %in% c("mariadb", "mysql") )
  {
    # Get columns:
    x <- NULL;
    try(x <- DBI::dbGetQuery(db_connection, paste0("SHOW COLUMNS FROM ",db_table,";")), silent=TRUE);
    if( !is.null(x) && inherits(x, "data.frame") )
    {
      n <- NULL;
      # Get number of rows:
      try(n <- DBI::dbGetQuery(db_connection, paste0("SELECT COUNT(*) FROM ",db_table,";")), silent=TRUE);
      if( is.null(n) || !inherits(n, "data.frame") || nrow(n) != 1 || ncol(n) != 1 )
      {
        # Error retreiving the number of rows:
        n <- NA;
      } else
      {
        n <- as.numeric(n[1,1]);
      }
      
      db_cols_info <- data.frame("table"=db_table, "nrow"=n, "column"=x$Field, "type"=x$Type, "null"=x$Null, "key"=x$Key);
    }
  } else if( db_type == "sqlite" )
  {
    # Get columns:
    x <- NULL;
    try(x <- DBI::dbGetQuery(db_connection, paste0("PRAGMA table_info(",db_table,");")), silent=TRUE);
    if( !is.null(x) && inherits(x, "data.frame") )
    {
      n <- NULL;
      try(n <- DBI::dbGetQuery(db_connection, paste0("SELECT COUNT(*) FROM ",db_table,";")), silent=TRUE);
      if( is.null(n) || !inherits(n, "data.frame") || nrow(n) != 1 || ncol(n) != 1 )
      {
        # Error retreiving the number of rows:
        n <- NA;
      } else
      {
        n <- as.numeric(n[1,1]);
      }
      
      db_cols_info <- data.frame("table"=db_table, "nrow"=n, "column"=x$name, "type"=x$type, "null"=(x$notnull == 0), "key"=(x$pk != 0));
    }
  } else if( db_type == "mssql" )
  {
    n <- NULL;
    try(n <- RODBC::sqlQuery(db_connection, paste0("SELECT COUNT(*) FROM ",db_name,".",db_table,";")), silent=TRUE);
    if( is.null(n) || !inherits(n, "data.frame") || nrow(n) != 1 || ncol(n) != 1 )
    {
      # Error retreiving the number of rows:
      n <- NA;
    } else
    {
      n <- as.numeric(n[1,1]);
    }
    db_list <- RODBC::sqlQuery(db_connection, paste0("SELECT * FROM ",db_name,".INFORMATION_SCHEMA.COLUMNS ORDER BY ORDINAL_POSITION;"));
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






##
## Create the test database and table using the AdhereR::med.events datset ####
##

SQL_create_test_database <- function(db_type=c("mariadb", "mysql", "sqlite", "mssql")[1], db_host="localhost", db_dsn=NA, db_user="user", db_psswd="password", db_name=NULL,
                                     db_evtable_name=NULL, db_evtable_cols=NULL)
{
}
                                    
