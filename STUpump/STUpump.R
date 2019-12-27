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

stu_db <- SQL_db(server_info);

# If needed, create the example database:
if( FALSE )
{
  # Create the test database:
  stu_db <- SQL_create_test_database(stu_db);
}


##
## Check if the tables exist and contain the expected columns and are not empty ####
##

db_tables <- SQL_list_tables(stu_db);
if( !(stu_db$db_evtable_name %in% db_tables) )
{
  stop(paste0("The required events table '",stu_db$db_evtable_name,"' does not seem to exist in the database!\n"));
}

db_evtable_info <- SQL_get_cols_info(stu_db, stu_db$db_evtable_name);
if( is.null(db_evtable_info) || nrow(db_evtable_info) == 0 )
{
  stop(paste0("Cannot get the information about the events table '",stu_db$db_evtable_name,"'!\n"));
}
if( !(stu_db$db_evtable_cols["PATIENT_ID"] %in% db_evtable_info$column) )
{
  stop(paste0("The required column PATIENT_ID ('",stu_db$db_evtable_cols["PATIENT_ID"],"') does not seem to exist in the events table '",stu_db$db_evtable_name,"'!\n"));
}
if( !(stu_db$db_evtable_cols["DATE"] %in% db_evtable_info$column) )
{
  stop(paste0("The required column DATE ('",stu_db$db_evtable_cols["DATE"],"') does not seem to exist in the events table '",stu_db$db_evtable_name,"'!\n"));
}
if( !(stu_db$db_evtable_cols["PERDAY"] %in% db_evtable_info$column) )
{
  stop(paste0("The required column PERDAY ('",stu_db$db_evtable_cols["PERDAY"],"') does not seem to exist in the events table '",stu_db$db_evtable_name,"'!\n"));
}
if( db_evtable_info$nrow[1] == 0 )
{
  stop(paste0("The events table '",stu_db$db_evtable_name,"' seems empty!\n"));
}


ids <- SQL_retreive_patient_ids(stu_db, db_table=stu_db$db_evtable_name);
SQL_retreive_patient_info(stu_db, db_table=stu_db$db_evtable_name, patient_id=ids[1]);



##
## Disconnect from the database ####
##

SQL_disconnect(stu_db);







