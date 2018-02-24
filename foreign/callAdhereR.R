#!/usr/bin/env Rscript

###############################################################################################
#
#    callAdhereR.R: an R wrapper script to allow AdhereR to be transparently
#    called from, and return results to an external caller using the standard
#    shell mechanism; this should work for, e.g., STATA, SAS, SPSS, python...
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
# The communication protocol is fully described in the 'communication-protocol.txt' file, but essentially:
# 0. all communication is via files in a given folder (by default the current one)
# 1. the external caller prepares two files in this folder, one containing the parameters (parameters.log) and one containing the data (dataset.csv)
# 2. the external calls R using the "shell" mechanism: shell Rscript --vanilla PATH_TO_ADHERER/callAdhereR.R PATH_TO_WHERE_DATA_AND_RESULTS_ARE
# 3. R returns, leaving one or more files in the given folder, usually "Adherer-results.txt", "CMA.csv" (the actual results), and plots (if requested)
#
#######


# Auxiliary function: Interactive plotting:
.do.interactive.plotting <- function(params.as.list)
{
  # pre-process the parameters:
  if( length(s <- which("patient_to_plot" == names(params.as.list))) > 0 )
  {
    names(params.as.list)[s] <- "ID";
    params.as.list[[s]] <- .get.param.value("patient_to_plot", type="character", default.value=NULL, required=FALSE);
  }

  # Interactive plotting:
  do.call("plot_interactive_cma", c(list("print.full.params"=FALSE), # DEBUG
                                    list("backend"="shiny"), list("use.system.browser"=TRUE), # force using shiny in the system browser
                                    params.as.list));

  return (NULL); # all ok
}


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
  msg.file <- file(paste0("./Adherer-results.txt"), "wt");
  sink(msg.file, type="message");
  msg <- paste0("AdhereR: The given directory '",folder.path,"' does not seem to exist: ABORTING...\n");
  #cat(msg); sink();
  stop(msg, call.=FALSE);
}
# test to see if we have read and write access to the folder:
if( file.access(folder.path, 4) != 0 )
{
  msg.file <- file(paste0("./Adherer-results.txt"), "wt");
  sink(msg.file, type="message");
  msg <- paste0("AdhereR: I don't have read access to given directory '",folder.path,"': ABORTING...\n");
  #cat(msg); sink();
  stop(msg, call.=FALSE);
}
if( file.access(folder.path, 2) != 0 )
{
  msg.file <- file(paste0("./Adherer-results.txt"), "wt");
  sink(msg.file, type="message");
  msg <- paste0("AdhereR: I don't have write access to given directory '",folder.path,"': ABORTING...\n");
  #cat(msg); sink();
  stop(msg, call.=FALSE);
}

# The results file:
msg.file <- file(paste0(folder.path,"/Adherer-results.txt"), "wt");
sink(msg.file, type="message");



# test to see if the folder contains the "parameters.log" file:
parameters.file <- paste0(folder.path,"/parameters.log");
if( !file.exists(parameters.file) )
{
  msg <- paste0("AdhereR: The parameters file 'parameters.log' does not seem to exist in the given folder '",folder.path,"': ABORTING...\n");
  #cat(msg); sink();
  stop(msg, call.=FALSE);
}
# Try to parse it:
parameters <- readLines(parameters.file);
if( parameters[1] != "Parameters" )
{
  msg <- paste0("AdhereR: The parameters file 'parameters.log' should start with a 'Parameters' line: ABORTING...\n");
  #cat(msg); sink();
  stop(msg, call.=FALSE);
}
if( parameters[length(parameters)] != "end_parameters" )
{
  msg <- paste0("AdhereR: The parameters file 'parameters.log' should end with a 'end_parameters' line: ABORTING...\n");
  #cat(msg); sink();
  stop(msg, call.=FALSE);
}
if( length(parameters) < 3 )
{
  msg <- paste0("AdhereR: The parameters file 'parameters.log' should at least some parameters...\n");
  #cat(msg); sink();
  stop(msg, call.=FALSE);
}
# get the parameters block nicely parsed:
.remove.spaces.and.quotes <- function(s)
{
  if( is.null(s) || is.na(s) || !is.character(s) || nchar(s)==0 ) return (""); # not a proper string value
  s <- trimws(s); # get rid of the trailing spaces
  if( substr(s,1,1) == "\"" || substr(s,1,1) == "'" ) s <- substr(s,2,nchar(s));
  if( substr(s,nchar(s),nchar(s)) == "\"" || substr(s,nchar(s),nchar(s)) == "'" ) s <- substr(s,1,nchar(s)-1);
  return (s);
}
parameters.block <- do.call(rbind, lapply( 2:(length(parameters)-1), function(i)
{
  # quick and dirty parser (it does not catch all possible cases but should generally work):
  s <- parameters[i];
  # get rid of any comments (preceded by # or ///):
  s <- strsplit(s,"#",fixed=TRUE)[[1]][1]; s <- strsplit(s,"///",fixed=TRUE)[[1]][1];

  # get the param name and possible value:
  s <- strsplit(s,"=",fixed=TRUE)[[1]];
  if( length(s) == 1 )
  {
    return (data.frame("param"=.remove.spaces.and.quotes(s[1]), "value"=NA));
  } else if( length(s) == 2 )
  {
    return (data.frame("param"=.remove.spaces.and.quotes(s[1]), "value"=.remove.spaces.and.quotes(s[2])));
  } else
  {
    msg <- paste0("AdhereR: Error in the parameters file 'parameters.log' line ",i," there should be at most one "=" sign...\n");
    #cat(msg); sink();
    stop(msg, call.=FALSE);
  }
}));
parameters.block$param <- as.character(parameters.block$param); parameters.block$value <- as.character(parameters.block$value); # make sure these are strings!




# Check the input data:
data.file <- paste0(folder.path,"/dataset.csv");
if( !file.exists(data.file) )
{
  msg <- paste0("AdhereR: The data file 'dataset.csv' does not seem to exist in the given folder '",folder.path,"': ABORTING...\n");
  #cat(msg); sink();
  stop(msg, call.=FALSE);
}
# try to load the data:
data <- NULL;
try(data <- read.table(data.file, header=TRUE, sep="\t", quote="", stringsAsFactors=FALSE));
if( is.null(data) || nrow(data)==0 || ncol(data)==0 )
{
  msg <- paste0("AdhereR: Cannot load the data file 'dataset.csv' or it is empty: ABORTING...\n");
  #cat(msg); sink();
  stop(msg, call.=FALSE);
}


# try to get the value of a given parameter:
.get.param.value <- function(param.name,                                                 # the parameter's name
                             type=c("character","numeric","Date","character.vector")[1], # the expected type (vector means a list of values of the given type separated by ";")
                             default.value=NA,                                           # the default value (if not defined)
                             date.format="%d/%m/%y",                                     # the format of the Date (if so requested)
                             required=TRUE                                               # is the param required (i.e., stop everything if not defined)?
)
{
  if( is.na(param.name) || is.null(param.name) || !is.character(param.name) || length(param.name) != 1 ) return (NA);

  s <- which(parameters.block$param == param.name);
  if( length(s) == 0 )
  {
    if( required )
    {
      msg <- paste0("AdhereR: The parameters file 'parameters.log' must contain a single value for required parameter '",param.name,"': ABORTING...\n");
      #cat(msg); sink();
      stop(msg, call.=FALSE);
    } else
    {
      return (default.value); # otherwise return the default value
    }
  }
  s <- s[1]; # use the first defined value...
  if( is.na(parameters.block$value[s]) || parameters.block$value[s] == "" )
  {
    # empty string "" also means *not given* -> use the default value:
    return (default.value);
  } else
  {
    # return the value (with conversion to requested type):
    return (switch(type,
                   "character"=parameters.block$value[s],
                   "numeric"=as.numeric(parameters.block$value[s]),
                   "Date"=as.Date(parameters.block$value[s], format=date.format),
                   "character.vector"=vapply(strsplit(parameters.block$value[s],";",fixed=TRUE)[[1]], function(s) .remove.spaces.and.quotes(s), character(1))));
  }
}


# load AdhereR:
#cat("ATTENTION: change AdhereR.devel to AdhereR in the final version!!!\n");
#if( !library("AdhereR.devel", verbose=FALSE, quietly=TRUE, logical.return=TRUE) )
if( !library("AdhereR", verbose=FALSE, quietly=TRUE, logical.return=TRUE) )
{
  msg <- paste0("AdhereR: failed loading the 'AdhereR' R package. Please check that it is corretly installed in R! ABORTING...\n");
  #cat(msg);
  sink();
  stop(msg, call.=FALSE);
}

# try to call the appropriate AdhereR function:
if( sum(parameters.block$param == "function", na.rm=TRUE) != 1 )
{
  msg <- paste0("AdhereR: The parameters file 'parameters.log' should contain a single 'function=' line...\n");
  #cat(msg); sink();
  stop(msg, call.=FALSE);
}

# collect the parameters in a list:
NON_CMA_PARAMS <- c("NA.SYMBOL.NUMERIC", "NA.SYMBOL.STRING", "LOGICAL.SYMBOL.TRUE", "LOGICAL.SYMBOL.FALSE", "COLNAMES.DOT.SYMBOL", "COLNAMES.START.DOT", "function");
PARAMS <- setdiff(unique(parameters.block$param), NON_CMA_PARAMS);
params.as.list <- lapply(PARAMS, function(s){ x <- .get.param.value(s, type="character", default.value=NA, required=FALSE); if( is.na(x) ) return (NULL) else return (x); }); names(params.as.list) <- PARAMS;
params.as.list <- Filter(Negate(is.null), params.as.list); # get rid of the NULL ("default") elemens

# some params have special meaning and should be processed as such:
# various window types:
.cast.param.to.type <- function(value.param, type.param)
{
  if( !is.null(params.as.list[[value.param]]) )
  {
    # set its type appropriately:
    tmp <- switch(.get.param.value(type.param, type="character", default.value="numeric", required=FALSE),
                  "character"=params.as.list[[value.param]], # nothing to covert to
                  "numeric"=as.numeric(params.as.list[[value.param]]), # try to make it a number
                  "date"=as.Date(params.as.list[[value.param]], format=.get.param.value("date.format", type="character", default.value="%m/%d/%Y", required=FALSE)), # try to make it a Date
                  NA);
    if( is.na(tmp) )
    {
      msg <- paste0("AdhereR: Cannot convert '",value.param,"' to the desired type '",type.param,"'...\n");
      #cat(msg); sink();
      stop(msg, call.=FALSE);
    } else
    {
      params.as.list[[value.param]] <<- tmp; # write it back directly!
    }
  }
}
.cast.param.to.type("followup.window.start",        "followup.window.start.type");
.cast.param.to.type("followup.window.duration",     "followup.window.duration.type");
.cast.param.to.type("observation.window.start",     "observation.window.start.type");
.cast.param.to.type("observation.window.duration",  "observation.window.duration.type");
.cast.param.to.type("sliding.window.start",         "sliding.window.start.type");
.cast.param.to.type("sliding.window.duration",      "sliding.window.duration.type");
.cast.param.to.type("sliding.window.step.duration", "sliding.window.step.duration.type");

# add the data to the list of params as well:
params.as.list <- c(list("data"=data), params.as.list);

# call the appropriate function:
function.to.call <- .get.param.value("function", type="character", required=TRUE);
results <- switch(function.to.call,
                  "CMA0"=,
                  "CMA1"=,
                  "CMA2"=,
                  "CMA3"=,
                  "CMA4"=,
                  "CMA5"=,
                  "CMA6"=,
                  "CMA7"=,
                  "CMA8"=,
                  "CMA9"=,
                  "CMA_per_episode"=,
                  "CMA_sliding_window"=,
                  "compute.event.int.gaps"=,
                  "compute.treatment.episodes"= do.call(function.to.call, params.as.list), # call a CMA (or CMA-like) function
                  "plot_interactive_cma"=.do.interactive.plotting(params.as.list), # call the interactive plotting function
                  NULL # oops!
);

if( is.null(results) ) # OOPS! some error occured: make it known and quit!
{
  #file.remove(paste0(folder.path,"./results.csv"), showWarnings=FALSE); # clean-up?

  if( function.to.call == "plot_interactive_cma" )
  {
    # interactive plotting is a special case where NULL is just a sign of ending the call...
      cat("OK: interactive plotting is over  (but still, there might be warnings and messages above worth paying attention to)!\n", file=stderr());
  } else
  {
    msg <- "\nSOME ERROR HAS OCCURED (maybe there's some helpful messages above?)\n";
    stop(msg, call.=FALSE); # force stopping!
  }
} else
{
  # Otherwise, continue....

  # Save the results (possibly applying conversions):
  .apply.export.conversions <- function(df)
  {
    # logical symbols (may require converssion to numeric or string):
    LOGICAL.SYMBOL.TRUE  <- .get.param.value("LOGICAL.SYMBOL.TRUE",  type="character", default.value=NA, required=FALSE);
    LOGICAL.SYMBOL.FALSE <- .get.param.value("LOGICAL.SYMBOL.FALSE", type="character", default.value=NA, required=FALSE);
    if( !is.na(LOGICAL.SYMBOL.TRUE) || !is.na(LOGICAL.SYMBOL.FALSE) )
    {
      for( i in 1:ncol(df) ) if( is.logical(df[,i]) )
      {
        tmp <- as.character(df[,i]);
        if( !is.na(LOGICAL.SYMBOL.TRUE) )  tmp[tmp=="TRUE"]  <- LOGICAL.SYMBOL.TRUE;
        if( !is.na(LOGICAL.SYMBOL.FALSE) ) tmp[tmp=="FALSE"] <- LOGICAL.SYMBOL.FALSE;
        df[,i] <- tmp;
      }
    }

    # NA.SYMBOL.NUMERIC (applies to numeric columns):
    NA.SYMBOL.NUMERIC <- .get.param.value("NA.SYMBOL.NUMERIC", type="character", default.value=NA, required=FALSE);
    if( !is.na(NA.SYMBOL.NUMERIC) )
    {
      for( i in 1:ncol(df) ) if( is.numeric(df[,i]) ){ tmp <- as.character(df[,i]); tmp[is.na(tmp)] <- NA.SYMBOL.NUMERIC; df[,i] <- tmp; }
    }

    # NA.SYMBOL.STRING (applies to character and Date columns):
    NA.SYMBOL.STRING <- .get.param.value("NA.SYMBOL.STRING", type="character", default.value=NA, required=FALSE);
    if( !is.na(NA.SYMBOL.STRING) )
    {
      for( i in 1:ncol(df) )
      {
        if( is.character(df[,i]) || is.factor(df[,i]) ){ tmp <- as.character(df[,i]); tmp[is.na(tmp)] <- NA.SYMBOL.STRING; df[,i] <- tmp; }
        if( inherits(df[,i],"Date") ){ tmp <- as.character(df[,i], format=.get.param.value("date.format", type="character", default.value="%m/%d/%Y", required=FALSE)); tmp[is.na(tmp)] <- NA.SYMBOL.STRING; df[,i] <- tmp; }
      }
    }

    # column name stuff:
    COLNAMES.START.DOT <- .get.param.value("COLNAMES.START.DOT", type="character", default.value=NA, required=FALSE);
    if( !is.na(COLNAMES.START.DOT) ) names(df) <- sub("^\\.", COLNAMES.START.DOT, names(df));
    COLNAMES.DOT.SYMBOL <- .get.param.value("COLNAMES.DOT.SYMBOL", type="character", default.value=NA, required=FALSE);
    if( !is.na(COLNAMES.DOT.SYMBOL) ) names(df) <- gsub(".", COLNAMES.DOT.SYMBOL, names(df), fixed=TRUE);

    # return the new df for exporting:
    return (df);
  }

  # Depending on the computation, we may export different things:
  if( inherits(results, "CMA0") )
  {
    # Special case: for plot.show == TRUE, add the "-plotted" suffix to the saved files!
    file.name.suffix <- ifelse(.get.param.value("plot.show", type="character", default.value="FALSE", required=FALSE) == "TRUE", "-plotted", "" );

    # CMAs:
    write.table(.apply.export.conversions(results$CMA), paste0(folder.path,"./CMA",file.name.suffix,".csv"), row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE);
    # event info:
    if( !is.na(save.event.info <- .get.param.value("save.event.info", type="character", default.value=NA, required=FALSE)) && save.event.info=="TRUE" )
    {
      write.table(.apply.export.conversions(results$event.info), paste0(folder.path,"./EVENTINFO",file.name.suffix,".csv"), row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE);
    }
  } else if( function.to.call == "compute.event.int.gaps" && inherits(results, "data.frame") && nrow(results) > 0 && ncol(results) > 0 )
  {
    # event info:
    write.table(.apply.export.conversions(results), paste0(folder.path,"./EVENTINFO.csv"), row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE);
  } else if( function.to.call == "compute.treatment.episodes" && inherits(results, "data.frame") && nrow(results) > 0 && ncol(results) > 0 )
  {
    # treatment episodes:
    write.table(.apply.export.conversions(results), paste0(folder.path,"./TREATMENTEPISODES.csv"), row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE);
  }


  # Plotting might have been required:
  if( .get.param.value("plot.show", type="character", default.value="FALSE", required=FALSE) == "TRUE" )
  {
    # OK, plotting it too!

    # Get the list of relevant parameters:
    plotting.params <- params.as.list[grep("^plot\\.", names(params.as.list))];
    plotting.params[["plot.show"]] <- NULL;
    plotting.params[["plot.save.to"]] <- NULL;
    plotting.params[["plot.save.as"]] <- NULL;
    plotting.params[["plot.width"]] <- NULL;
    plotting.params[["plot.height"]] <- NULL;
    plotting.params[["plot.quality"]] <- NULL;
    plotting.params[["plot.dpi"]] <- NULL;
    names(plotting.params) <- substring(names(plotting.params), nchar("plot.")+1);

    # patients.to.plot is special:
    if( "patients.to.plot" %in% names(plotting.params) ) plotting.params[["patients.to.plot"]] <- .get.param.value("plot.patients.to.plot", type="character.vector", default.value=NULL, required=FALSE);

    # Get the info about the plot exporting process:
    plot.file.name <- paste0(.get.param.value("plot.save.to", type="character", default.value=folder.path, required=FALSE),"/adherer-plot.");
    plot.file.type <- .get.param.value("plot.save.as", type="character", default.value="jpg", required=FALSE);
    if( plot.file.type %in% c("jpg", "jpeg") )
    {
      jpeg(paste0(plot.file.name,"jpg"),
           width=.get.param.value("plot.width", type="numeric", default.value=7, required=FALSE),
           height=.get.param.value("plot.height", type="numeric", default.value=7, required=FALSE),
           units="in",
           quality=.get.param.value("plot.quality", type="numeric", default.value=90, required=FALSE),
           res=.get.param.value("plot.dpi", type="numeric", default.value=150, required=FALSE));
    } else if( plot.file.type %in% c("png") )
    {
      png(paste0(plot.file.name,"png"),
           width=.get.param.value("plot.width", type="numeric", default.value=7, required=FALSE),
           height=.get.param.value("plot.height", type="numeric", default.value=7, required=FALSE),
           units="in",
           res=.get.param.value("plot.dpi", type="numeric", default.value=150, required=FALSE));
    } else if( plot.file.type %in% c("tif", "tiff") )
    {
      tiff(paste0(plot.file.name,"tiff"),
           width=.get.param.value("plot.width", type="numeric", default.value=7, required=FALSE),
           height=.get.param.value("plot.height", type="numeric", default.value=7, required=FALSE),
           units="in", compression="lzw",
           res=.get.param.value("plot.dpi", type="numeric", default.value=150, required=FALSE));
    } else if( plot.file.type %in% c("eps") )
    {
      postscript(paste0(plot.file.name,"eps"),
           width=.get.param.value("plot.width", type="numeric", default.value=7, required=FALSE),
           height=.get.param.value("plot.height", type="numeric", default.value=7, required=FALSE),
           units="in", horizontal=FALSE, onefile=FALSE, paper="special"); # make sure the output is EPS
    } else if( plot.file.type %in% c("pdf") )
    {
      pdf(paste0(plot.file.name,"pdf"),
           width=.get.param.value("plot.width", type="numeric", default.value=7, required=FALSE),
           height=.get.param.value("plot.height", type="numeric", default.value=7, required=FALSE),
           units="in", horizontal=FALSE, onefile=FALSE, paper="special");
    }

    # attemt to plot:
    do.call("plot", c(list(results), plotting.params));

    # close the plotting device:
    dev.off();
  }

  # everything seems ok:
  cat("OK: the results were exported successfully (but still, there might be warnings and messages above worth paying attention to)!\n", file=stderr());
}

# ... and finish...








