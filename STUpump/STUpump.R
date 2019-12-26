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
## Global variables and settings ####
##
server_info <- "./server_credentials_STUpump.tsv"; # the file storing the database server info and credentials


##
## Check if the needed packages are installed and with the correct version ####
##
.needed_packages <- data.frame(names  =c("AdhereR"),
                               version=c(    "0.5"), # use NA if version doesn't matter
                               stringsAsFactors=FALSE); 
for( i in 1:nrow(.needed_packages) )
{
  .package_name    <- .needed_packages$names[i];
  .package_version <- .needed_packages$version[i];
  if( !require(.package_name, character.only=TRUE) || # try to load the package
      (!is.na(.package_version) &&                    # check version requirements (if any)
       compareVersion(.package_version, as.character(packageVersion(.package_name))) > 0) )
  { 
    # Package not present or too old -> stop!
    stop(paste0("Please make sure package '",.package_name,"' is installed",
                ifelse(is.na(.package_version),
                       "",
                       paste0(" and at least version ",.package_version)),
                ": ABORTING NOW...\n"));
  }
}


##
## Connect to the database ####
##

# Load data access and description
db_info <- read.table(server_info, header=TRUE, sep="\t", quote="", stringsAsFactors=FALSE);

# Connect to the database:
db_con <- NULL;

db_type  <- tolower(db_info$Value[ tolower(db_info$Variable) == "dbtype" ]);
db_host  <- db_info$Value[ tolower(db_info$Variable) == "host" ];
db_dsn   <- db_info$Value[ tolower(db_info$Variable) == "dsn" ];
db_user  <- db_info$Value[ tolower(db_info$Variable) == "user" ];
db_psswd <- db_info$Value[ tolower(db_info$Variable) == "psswd" ];
db_name  <- db_info$Value[ tolower(db_info$Variable) == "dbname" ];

if( db_type %in% c("mariadb", "mysql") )
{
  # MariaDB or MySQL:
  library(RMariaDB);
  db_con <- dbConnect(RMariaDB::MariaDB(), user=db_user, password=db_psswd, dbname=db_name, host=db_host);
} else if( db_type == "sqlite" )
{
  # SQLite:
  library(RSQLite);
  db_con <- dbConnect(RSQLite::SQLite(), host=db_host);
} else if( db_type == "mssql" )
{
  #  Microsoft SQL Server:
  library(RODBC);
  db_con <- odbcConnect(dsn=db_dsn, uid=db_user, pwd=db_psswd);
  # Check if the database exists:
  db_list <- sqlQuery(db_con, "SELECT name FROM master.sys.databases");
  if( !(db_name %in% db_list$name) )
  {
    stop(paste0("The required database '",db_name,"' does not exist on this server!\n"));
  }
} else
{
  db_con <- NULL;
  stop(paste0("Don't know how to use an SQL database of type '",db_type,"': please specify a MariaDB, MySQL, SQLite or Microsoft SQL Server database!\n"));
}
if( is.null(db_con) )
{
  # Something bad happened:
  stop("Error connecting to the specified database!\n");
}


##
## Check if the tables exist and contain the expected columns and are not empty ####
##

db_evtable_name <- db_info$Value[ tolower(db_info$Variable) == "evtable" ];
db_evtable_cols <- c("PATIENT_ID"=db_info$Value[ tolower(db_info$Variable) == "evtable_patient_id" ],
                     "DATE"      =db_info$Value[ tolower(db_info$Variable) == "evtable_date" ],
                     "PERDAY"    =db_info$Value[ tolower(db_info$Variable) == "evtable_perday" ],
                     "CATEGORY"  =db_info$Value[ tolower(db_info$Variable) == "evtable_category" ],
                     "DURATION"  =db_info$Value[ tolower(db_info$Variable) == "evtable_duration" ]);

if( db_type %in% c("mariadb", "mysql", "sqlite") )
{
  # MariaDB or MySQL:
  if( !(db_evtable_name %in% dbListTables(db_con)) )
  {
    stop(paste0("The required events table '",db_evtable_name,"' does not seem to exist in the database!\n"));
  }
} else if( db_type == "mssql" )
{
  #  Microsoft SQL Server:
  db_list <- sqlQuery(db_con, paste0("SELECT * FROM ",db_name,".information_schema.tables;")); # list the tables
  if( !(db_evtable_name %in% paste0(db_list$TABLE_SCHEMA,".",db_list$TABLE_NAME)) )
  {
    stop(paste0("The required events table '",db_evtable_name,"' does not seem to exist in the database!\n"));
  }
}









