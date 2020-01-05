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

# If needed, create the example database:
if( FALSE )
{
  # Create the test database:
  stu_db <- SQL_db(server_info, check_db=FALSE);
  create_test_database(stu_db);
  disconnect(stu_db);
}

# Connect to the pre-existing database:
stu_db <- SQL_db(server_info);



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
    
    pat_procs_classes <- as.character(unique(pat_procs$cats)); # the classes of medication
    for( procs_class in pat_procs_classes )
    {
      # The actions for this class:
      if( is.na(procs_class) )
      {
        pat_procs_actions <- unique(pat_procs[ is.na(pat_procs$cats), ]);
      } else
      {
        pat_procs_actions <- unique(pat_procs[ pat_procs$cats == procs_class, ]);
      }
      
      # Select the events corresponding to this class:
      s <- select_events_for_procs_class(stu_db, pat_info, procs_class);
      
      if( !is.na(s) && !is.null(s) && length(s) > 0 && is.logical(s) && any(s) )
      {
        # Apply the specified processing(s) to this selection:
        if( !is.null(pat_procs_actions) && nrow(pat_procs_actions) > 0 )
        {
          # Ok, there's actions to apply:
          for( j in 1:nrow(pat_procs_actions) )
          {
            # Apply the action:
            pat_procs_results <- apply_procs_action_for_class(stu_db, pat_info[s,], pat_procs_actions[j,]);
            
            # Upload the results:
            if( !upload_procs_results(stu_db, pat_procs_results) )
            {
              # Oops: error writing these results to the database
              stop("Error writing results to the database!\n");
            }
          }
        }
      }
    }
  }   
}



##
## Disconnect from the database ####
##

disconnect(stu_db);







