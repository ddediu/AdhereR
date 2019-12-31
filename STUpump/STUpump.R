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
#server_info <- "./server_credentials_STUpump.tsv"; # the file storing the database server info and credentials for MS SQL server
server_info <- "./server_credentials_STUpump2.tsv"; # the file storing the database server info and credentials for MySQL server

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
  create_test_database(stu_db);
}


##
## Process the patients one by one ####
##

# Get the list of all patients:
patient_ids <- list_evtable_patients(stu_db);

# Process them individually:
for( i in seq_along(patient_ids) )
{
  # The patient ID:
  pat_id <- patient_ids[i];
  
  # The possible message(s) related to processing this patient:
  pat_msgs <- "";
  
  # The values to return for this patient:
  pat_retval <- list("id"=pat_id);
  
  # Get the patient's info:
  pat_info <- get_evtable_patients_info(stu_db, pat_id);
  if( is.null(pat_info) || !inherits(pat_info, "data.frame") || nrow(pat_info) < 1 )
  {
    # Empty patient:
    pat_msgs <- paste0(pat_msgs, "W: this patient has no data!");
  } else
  {
    # Get the processing(s) and plotting(s) for this patient:
    pat_procs <- get_processings_for_patient(stu_db, pat_id);
    
    stop("*** IMPLEMENT THE PROCESSING!!! ***");
    
    # Compute the requested CMA(s):
    cma1 <- AdhereR::CMA1(data=pat_info,
                          ID.colname=get_evtable_id_col(stu_db),
                          event.date.colname=get_evtable_date_col(stu_db),
                          event.duration.colname=get_evtable_duration_col(stu_db));
    if( is.null(cma1) )
    {
      # Issues computing this CMA:
      pat_msgs <- paste0(pat_msgs, "W: error computing CMA1!");
      pat_retval$CMA1 <- NA;
    } else
    {
      pat_retval$CMA1 <- getCMA(cma1)$CMA[1];
    }
    
    # Do the requested plot(s):
    plot_file_names <- plot(cma1, export.formats=c("html"), generate.R.plot=FALSE); # plot_file_names contains the path to the generated plots
    if( is.null(plot_file_names) || length(plot_file_names) < 2 )
    {
      # Issues generating the plots:
      pat_msgs <- paste0(pat_msgs, "W: error plotting CMA1!");
      pat_retval$CMA1_plot <- NULL;
    } else
    {
      # Save the plots:
      
      # Create the ZIP holding the HTML document and JPG placeholder:
      zip_file_name <- paste0(plot_file_names["html"],".zip");
      if( utils::zip(zipfile=zip_file_name, files=plot_file_names, flags="-9Xj") != 0 )
      {
        # Errors zipping:
        pat_msgs <- paste0(pat_msgs, "W: error creating the zip containing the HTML document and the JPG placeholder!");
        zip_file_name <- NULL;
      }
      
      # Store these files:
      pat_retval$CMA1_plot <- list("jpg"=plot_file_names["jpg-placeholder"], 
                                   "html"=zip_file_name);
    }
    
    # Upload them in the database:
  }
  
  # Store the messages:
  pat_retval$msgs <- pat_msgs;
}



##
## Disconnect from the database ####
##

disconnect(stu_db);







