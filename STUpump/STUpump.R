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

# Load the SQL-stuff:
source("./SQL_queries.R", echo=FALSE);


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

# If needed, create the example database:
if( FALSE )
{
  # Create and use it:
  db_type  <- tolower(db_info$Value[ tolower(db_info$Variable) == "dbtype" ]);
  db_host  <- db_info$Value[ tolower(db_info$Variable) == "host" ];
  db_dsn   <- db_info$Value[ tolower(db_info$Variable) == "dsn" ];
  db_user  <- db_info$Value[ tolower(db_info$Variable) == "user" ];
  db_psswd <- db_info$Value[ tolower(db_info$Variable) == "psswd" ];
  db_name  <- "STUpumpTests";
  
  db_evtable_name <- ifelse(db_type == "mssql", "dbo.med_events", "med_events");
  db_evtable_cols <- c("PATIENT_ID"="id",
                       "DATE"      ="date",
                       "PERDAY"    ="perday",
                       "CATEGORY"  ="category",
                       "DURATION"  ="duration");
  
  # CREATE IT!
  SQL_create_test_database(db_type, db_host, db_dsn, db_user, db_psswd, db_name, 
                           db_evtable_name, db_evtable_cols);
} else
{
  # Use the database requested by the user:
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
}

# Connect to the database:
db_connection <- SQL_connect(db_type, db_host, db_dsn, db_user, db_psswd, db_name);


##
## Check if the tables exist and contain the expected columns and are not empty ####
##

db_tables <- SQL_list_tables(db_type, db_connection, db_name);
if( !(db_evtable_name %in% db_tables) )
{
  stop(paste0("The required events table '",db_evtable_name,"' does not seem to exist in the database!\n"));
}

db_evtable_info <- SQL_get_cols_info(db_type, db_connection, db_table=db_evtable_name, db_name);
if( is.null(db_evtable_info) || nrow(db_evtable_info) == 0 )
{
  stop(paste0("Cannot get the information about the events table '",db_evtable_name,"'!\n"));
}
if( !(db_evtable_cols["PATIENT_ID"] %in% db_evtable_info$column) )
{
  stop(paste0("The required column PATIENT_ID ('",db_evtable_cols["PATIENT_ID"],"') does not seem to exist in the events table '",db_evtable_name,"'!\n"));
}
if( !(db_evtable_cols["DATE"] %in% db_evtable_info$column) )
{
  stop(paste0("The required column DATE ('",db_evtable_cols["DATE"],"') does not seem to exist in the events table '",db_evtable_name,"'!\n"));
}
if( !(db_evtable_cols["PERDAY"] %in% db_evtable_info$column) )
{
  stop(paste0("The required column PERDAY ('",db_evtable_cols["PERDAY"],"') does not seem to exist in the events table '",db_evtable_name,"'!\n"));
}
if( db_evtable_info$nrow[1] == 0 )
{
  stop(paste0("The events table '",db_evtable_name,"' seems empty!\n"));
}




##
## Disconnect from the database ####
##

SQL_disconnect(db_type, db_connection);







