#!/usr/bin/env Rscript

###############################################################################################
#
#    callAdhereRfromSTATA.R: an R wrapper script to allow AdhereR to be transparently
#    called from, and return results to, STATA.
#    Copyright (C) 2018  Dan Dediu & Maria Rubio Valera
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

#######
#
# Protocol:
# 0. all communication is via files in a given folder (e.g., C:\Temp\AdhereR)
# 1. STATA prepares two files in this folder, one containing the parameters (parameters.log) and one containing the data (dataset.csv)
# 2. STATA calls R using the "shell" mechanism: . shell Rscript --vanilla PATH_TO_ADHERER/callAdhereRfromSTATA.R PATH_TO_WHERE_DATA_AND_RESULTS_ARE
# 3. R returns, leaving two files in the given folder, "results.txt" (containes either "OK" or a warning/error message) and the "result.csv" (the actual results)
#
#######

# get the command line arguments
args <- commandArgs(trailingOnly=TRUE);

# process the arguments:
folder.path <- "./"; # implicitely work in the current folder
if( length(args)==0 )
{
  warning("AdhereR: No path to where the input and output files should be: assuming current directory...\n", call.=FALSE);
} else if( length(args)==1 )
{
  folder.path <- args[1];
}

# test to see if the folder exists:
if( !file.exists(folder.path) )
{
  sink(paste0("./Adherer-errors.txt"));
  msg <- paste0("AdhereR: The given directory '",folder.path,"' does not seem to exist: ABORTING...\n");
  cat(msg); sink();
  stop(msg, call.=FALSE);
}
# test to see if we have read and write access to the folder:
if( file.access(folder.path, 4) != 0 )
{
  sink(paste0("./Adherer-errors.txt"));
  msg <- paste0("AdhereR: I don't have read access to given directory '",folder.path,"': ABORTING...\n");
  cat(msg); sink();
  stop(msg, call.=FALSE);
}
if( file.access(folder.path, 2) != 0 )
{
  sink(paste0("./Adherer-errors.txt"));
  msg <- paste0("AdhereR: I don't have write access to given directory '",folder.path,"': ABORTING...\n");
  cat(msg); sink();
  stop(msg, call.=FALSE);
}

# The results file:
sink(paste0(folder.path,"/results.txt"));



# test to see if the folder contains the "parameters.log" file:
parameters.file <- paste0(folder.path,"/parameters.log");
if( !file.exists(parameters.file) )
{
  msg <- paste0("AdhereR: The parameters file 'parameters.log' does not seem to exist in the given folder '",folder.path,"': ABORTING...\n");
  cat(msg); sink();
  stop(msg, call.=FALSE);
}
# Try to parse it:
parameters <- readLines(parameters.file);
if( parameters[1] != "Parameters" )
{
  msg <- paste0("AdhereR: The parameters file 'parameters.log' should start with a 'Parameters' line: ABORTING...\n");
  cat(msg); sink();
  stop(msg, call.=FALSE);
}
if( parameters[length(parameters)] != "end_parameters" )
{
  msg <- paste0("AdhereR: The parameters file 'parameters.log' should end with a 'end_parameters' line: ABORTING...\n");
  cat(msg); sink();
  stop(msg, call.=FALSE);
}
if( length(parameters) < 3 )
{
  msg <- paste0("AdhereR: The parameters file 'parameters.log' should at least some parameters...\n");
  cat(msg); sink();
  stop(msg, call.=FALSE);
}
# get the parameters block nicely parsed:
.remove.spaces.and.quotes <- function(s)
{
  if( is.null(s) || is.na(s) || !is.character(s) || nchar(s)==0 ) return (""); # not a proper string value
  s <- trimws(s); # get rid of the trailng spaces
  if( substr(s,1,1) == "\"" || substr(s,1,1) == "'" ) s <- substr(s,2,nchar(s));
  if( substr(s,nchar(s),nchar(s)) == "\"" || substr(s,nchar(s),nchar(s)) == "'" ) s <- substr(s,1,nchar(s)-1);
  return (s);
}
parameters.block <- do.call(rbind, lapply( 2:(length(parameters)-1), function(i)
{
  s <- strsplit(parameters[i],"=",fixed=TRUE)[[1]];
  if( length(s) == 1 )
  {
    return (data.frame("param"=.remove.spaces.and.quotes(s[1]), "value"=NA));
  } else if( length(s) == 2 )
  {
    return (data.frame("param"=.remove.spaces.and.quotes(s[1]), "value"=.remove.spaces.and.quotes(s[2])));
  } else
  {
    msg <- paste0("AdhereR: Error in the parameters file 'parameters.log' line ",i," there should be at most one "=" sign...\n");
    cat(msg); sink();
    stop(msg, call.=FALSE);
  }
}));
parameters.block$param <- as.character(parameters.block$param); parameters.block$value <- as.character(parameters.block$value); # make sure these are strings!




# Check the input data:
data.file <- paste0(folder.path,"/dataset.csv");
if( !file.exists(data.file) )
{
  msg <- paste0("AdhereR: The data file 'dataset.csv' does not seem to exist in the given folder '",folder.path,"': ABORTING...\n");
  cat(msg); sink();
  stop(msg, call.=FALSE);
}
# try to load the data:
data <- NULL;
try(data <- read.table(data.file, header=TRUE, sep="\t", quote="", stringsAsFactors=FALSE));
if( is.null(data) || nrow(data)==0 || ncol(data)==0 )
{
  msg <- paste0("AdhereR: Cannot load the data file 'dataset.csv' or it is empty: ABORTING...\n");
  cat(msg); sink();
  stop(msg, call.=FALSE);
}


# try to get the value of a given parameter:
.get.param.value <- function(param.name,                              # the parameter's name
                             type=c("character","numeric","Date")[1], # the expected type
                             default.value=NA,                        # the default value (if not defined)
                             date.format="%d/%m/%y",                  # the format of the Date (if so requested)
                             required=TRUE                            # is the param required (i.e., stop everything if not defined)?
)
{
  if( is.na(param.name) || is.null(param.name) || !is.character(param.name) || length(param.name) != 1 ) return (NA);

  s <- which(parameters.block$param == param.name);
  if( length(s) == 0 )
  {
    if( required )
    {
      msg <- paste0("AdhereR: The parameters file 'parameters.log' must contain a single value for required parameter '",param.name,"': ABORTING...\n");
      cat(msg); sink();
      stop(msg, call.=FALSE);
    } else
    {
      return (default.value); # otherwise return the default value
    }
  }
  s <- s[1]; # use the first defined value...

  return (switch(type, # return the value (with conversion to request type)
                 "character"=parameters.block$value[s],
                 "numeric"=as.numeric(parameters.block$value[s]),
                 "Date"=as.Date(parameters.block$value[s], format=date.format)));
}


# load AdhereR:
if( !library("AdhereR", verbose=FALSE, quietly=TRUE, logical.return=TRUE) )
{
  msg <- paste0("AdhereR: failed loading the 'AdhereR' R package. Please check that it is corretly installed in R! ABORTING...\n");
  cat(msg); sink();
  stop(msg, call.=FALSE);
}

# try to call the appropriate AdhereR function:
if( sum(parameters.block$param == "function", na.rm=TRUE) != 1 )
{
  msg <- paste0("AdhereR: The parameters file 'parameters.log' should contain a single 'function=' line...\n");
  cat(msg); sink();
  stop(msg, call.=FALSE);
}
results <- switch(toupper(parameters.block$value[ parameters.block$param == "function" ]),
                  "CMA1"={
                    tmp <- AdhereR::CMA1(data=data,
                                         ID.colname=.get.param.value("ID.colname", type="character", required=TRUE),
                                         event.date.colname=.get.param.value("event.date.colname", type="character", required=TRUE),
                                         event.duration.colname=.get.param.value("event.duration.colname", type="character", required=TRUE)
                    );
                    if( !is.null(tmp) && !is.null(tmp$CMA) ){ tmp$CMA; } else { NULL; }
                  },
                  NULL # oops!
);

if( !is.null(results) && inherits(results,"data.frame") && nrow(results) > 0 && ncol(results) > 0 )
{
  write.table(results, paste0(folder.path,"./results.csv"), row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE);
  cat("OK\n");
} else
{
  file.remove(paste0(folder.path,"./results.csv"), showWarnings=FALSE);
  cat("\nSOME ERROR HAS OCCURED (maybe there's some error messages above?)\n");
}


# Stop writing to the results file
sink();








