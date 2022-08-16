###############################################################################################
#
#    This allows AdhereR to be called from outside R using a generic `shell` +
#    shared files mechanism.
#    Copyright (C) 2015-2018  Dan Dediu & Alexandra Dima
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



#' callAdhereR.
#'
#' The function encapsulating all the logics that allows AdhereR to be called
#' from any platform using the generic \code{shell} mechanism.
#'
#' In most cases this should not be done directly by the user,
#' but instead used by an appropriate \code{wrapper} on the client platform.
#' It allows transparent use of \code{AdhereR} from virtually any platform or
#' programming language for which an appropriate wrapper is provided.
#' For more details see the vignette describing the included reference
#' \code{Python 3} wrapper.
#'
#' @param shared.data.directory A \emph{string} containing the path to the
#' directory where all the exchanged (shared) data (both input and output) is.
#' \code{AdhereR} needs read and write access to this directory.
#' @return This function displays any messages to the console, tries to also
#' write them to the \code{Adherer-results.txt} file in the
#' \code{shared.data.directory} directory, and, when finished, forces \code{R}
#' to quit with a given shell error code:
#' \itemize{
#'  \item \code{0} The processing ended without major errors;
#'  \item \code{1} General error (hopefully there are messages in the
#'                 \code{Adherer-results.txt} file;
#'  \item \code{10} The directory \code{shared.data.directory} does not exit;
#'  \item \code{11} \code{AdhereR} does not have read access to the
#'                  \code{shared.data.directory} directory;
#'  \item \code{12} \code{AdhereR} does not have write access to the
#'                  \code{shared.data.directory} directory;
#'  \item \code{13} issues with the parameters file \code{parameters.log};
#'  \item \code{14} issues with the data file \code{dataset.csv};
#'  \item \code{15} plotting issues;
#'  \item \code{16} interactive plotting issues;
#'  \item \code{17} issues exporting the results.
#' }
#' @export
callAdhereR <- function(shared.data.directory) # the directory where the shared data (input and output) is found
{
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


  # Check to see if the folder exists:
  if( !dir.exists(shared.data.directory) )
  {
    msg <- paste0("AdhereR: The given directory '",shared.data.directory,"' does not seem to exist: ABORTING...\n");
    cat(msg);
    quit(save="no", status=10, runLast=FALSE);
  }
  # Check to see if we have read and write access to the folder:
  if( file.access(shared.data.directory, 4) != 0 )
  {
    msg <- paste0("AdhereR: I don't have read access to given directory '",shared.data.directory,"': ABORTING...\n");
    cat(msg);
    quit(save="no", status=11, runLast=FALSE);
  }
  if( file.access(shared.data.directory, 2) != 0 )
  {
    msg <- paste0("AdhereR: I don't have write access to given directory '",shared.data.directory,"': ABORTING...\n");
    cat(msg);
    quit(save="no", status=12, runLast=FALSE);
  }

  # The errors/warnings/messages file:
  msg.file <- file(paste0(shared.data.directory,"/Adherer-results.txt"), "wt");
  cat(paste0("AdhereR ",packageVersion("AdhereR")," on R ",getRversion()," started at ",Sys.time(),":\n"), file=msg.file, append=FALSE); # initial message

  # Check to see if the directory contains the "parameters.log" file:
  parameters.file <- paste0(shared.data.directory,"/parameters.log");
  if( !file.exists(parameters.file) )
  {
    msg <- paste0("AdhereR: The parameters file 'parameters.log' does not seem to exist in the given folder '",shared.data.directory,"': ABORTING...\n");
    cat(msg); cat(msg, file=msg.file, append=TRUE);
    quit(save="no", status=13, runLast=FALSE);
  }
  # Try to parse it:
  parameters <- readLines(parameters.file);
  if( parameters[1] != "Parameters" )
  {
    msg <- paste0("AdhereR: The parameters file 'parameters.log' should start with a 'Parameters' line: ABORTING...\n");
    cat(msg); cat(msg, file=msg.file, append=TRUE);
    quit(save="no", status=13, runLast=FALSE);
  }
  if( parameters[length(parameters)] != "end_parameters" )
  {
    msg <- paste0("AdhereR: The parameters file 'parameters.log' should end with a 'end_parameters' line: ABORTING...\n");
    cat(msg); cat(msg, file=msg.file, append=TRUE);
    quit(save="no", status=13, runLast=FALSE);
  }
  if( length(parameters) < 3 )
  {
    msg <- paste0("AdhereR: The parameters file 'parameters.log' should at least some parameters...\n");
    cat(msg); cat(msg, file=msg.file, append=TRUE);
    quit(save="no", status=13, runLast=FALSE);
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
    } else
    {
      return (data.frame("param"=.remove.spaces.and.quotes(s[1]), "value"=.remove.spaces.and.quotes(paste(s[-1],collapse="=")))); # put back the '=' signs in the value
    }
  }));
  parameters.block$param <- as.character(parameters.block$param); parameters.block$value <- as.character(parameters.block$value); # make sure these are strings!

  # Check the input data:
  data.file <- paste0(shared.data.directory,"/dataset.csv");
  if( !file.exists(data.file) )
  {
    msg <- paste0("AdhereR: The data file 'dataset.csv' does not seem to exist in the given folder '",shared.data.directory,"': ABORTING...\n");
    cat(msg); cat(msg, file=msg.file, append=TRUE);
    quit(save="no", status=14, runLast=FALSE);
  }
  # try to load the data:
  data <- NULL;
  try(data <- read.table(data.file, header=TRUE, sep="\t", quote="", stringsAsFactors=FALSE));
  if( is.null(data) || nrow(data)==0 || ncol(data)==0 )
  {
    msg <- paste0("AdhereR: Cannot load the data file 'dataset.csv' or it is empty: ABORTING...\n");
    cat(msg); cat(msg, file=msg.file, append=TRUE);
    quit(save="no", status=14, runLast=FALSE);
  }

  # try to get the value of a given parameter:
  .get.param.value <- function(param.name,                                                           # the parameter's name
                               type=c("character","numeric","logical","Date","character.vector")[1], # the expected type (vector = list of values of given type separated by ";")
                               default.value=NA,                                                     # the default value (if not defined)
                               date.format="%d/%m/%y",                                               # the format of the Date (if so requested)
                               required=TRUE                                                         # is the param required (i.e., stop everything if not defined)?
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
                     "logical"=as.logical(parameters.block$value[s]),
                     "Date"=as.Date(parameters.block$value[s], format=date.format),
                     "character.vector"=vapply(strsplit(parameters.block$value[s],";",fixed=TRUE)[[1]], function(s) .remove.spaces.and.quotes(s), character(1))));
    }
  }

  # try to call the appropriate AdhereR function:
  if( sum(parameters.block$param == "function", na.rm=TRUE) != 1 )
  {
    msg <- paste0("AdhereR: The parameters file 'parameters.log' should contain a single 'function=' line...\n");
    cat(msg); cat(msg, file=msg.file, append=TRUE);
    quit(save="no", status=13, runLast=FALSE);
  }

  # collect the parameters in a list:
  NON_CMA_PARAMS <- c("NA.SYMBOL.NUMERIC", "NA.SYMBOL.STRING", "LOGICAL.SYMBOL.TRUE", "LOGICAL.SYMBOL.FALSE", "COLNAMES.DOT.SYMBOL", "COLNAMES.START.DOT", "function");
  PARAMS <- setdiff(unique(parameters.block$param), NON_CMA_PARAMS);
  params.as.list <- lapply(PARAMS, function(s){ x <- .get.param.value(s, type="character", default.value=NA, required=FALSE); if( is.na(x) ) return (NULL) else return (x); }); names(params.as.list) <- PARAMS;
  params.as.list <- Filter(Negate(is.null), params.as.list); # get rid of the NULL ("default") elemens

  # some params have special meaning and should be processed as such:
  .cast.param.to.type <- function(value.param, type.param, is.type.param.fixed=FALSE)
  {
    if( !is.null(params.as.list[[value.param]]) )
    {
      # set its type appropriately:
      tmp <- switch(ifelse(is.type.param.fixed, type.param, .get.param.value(type.param, type="character", default.value="numeric", required=FALSE)),
                    "character"=params.as.list[[value.param]], # nothing to covert to
                    "numeric"=as.numeric(params.as.list[[value.param]]), # try to make it a number
                    "logical"=as.logical(params.as.list[[value.param]]), # try to make it a boolean
                    "date"=as.Date(params.as.list[[value.param]], format=.get.param.value("date.format", type="character", default.value="%m/%d/%Y", required=FALSE)), # try to make it a Date
                    NA);
      if( is.na(tmp) )
      {
        msg <- paste0("AdhereR: Cannot convert '",value.param,"' to the desired type '",type.param,"'...\n");
        cat(msg); cat(msg, file=msg.file, append=TRUE);
        quit(save="no", status=13, runLast=FALSE);
      } else
      {
        params.as.list[[value.param]] <<- tmp; # write it back directly!
      }
    }
  }
  # Force type for params with known type:
  .cast.param.to.type("followup.window.start",        "followup.window.start.type");
  .cast.param.to.type("followup.window.duration",     "followup.window.duration.type");
  .cast.param.to.type("observation.window.start",     "observation.window.start.type");
  .cast.param.to.type("observation.window.duration",  "observation.window.duration.type");
  .cast.param.to.type("sliding.window.start",         "sliding.window.start.type");
  .cast.param.to.type("sliding.window.duration",      "sliding.window.duration.type");
  .cast.param.to.type("sliding.window.step.duration", "sliding.window.step.duration.type");
  if( .get.param.value("sliding.window.no.steps", type="numeric", default.value=-1, required=FALSE) == -1 ) params.as.list[["sliding.window.no.steps"]] <- NA;

  .cast.param.to.type("plot.show",                         "logical", TRUE);
  .cast.param.to.type("plot.align.all.patients",           "logical", TRUE);
  .cast.param.to.type("plot.align.first.event.at.zero",    "logical", TRUE);
  .cast.param.to.type("plot.show.legend",                  "logical", TRUE);
  .cast.param.to.type("plot.show.cma",                     "logical", TRUE);
  .cast.param.to.type("plot.show.event.intervals",         "logical", TRUE);
  .cast.param.to.type("plot.print.CMA",                    "logical", TRUE);
  .cast.param.to.type("plot.plot.CMA",                     "logical", TRUE);
  .cast.param.to.type("plot.plot.CMA.as.histogram",        "logical", TRUE);
  .cast.param.to.type("plot.highlight.followup.window",    "logical", TRUE);
  .cast.param.to.type("plot.highlight.observation.window", "logical", TRUE);
  .cast.param.to.type("plot.show.real.obs.window.start",   "logical", TRUE);
  .cast.param.to.type("plot.bw.plot",                      "logical", TRUE);
  .cast.param.to.type("force.NA.CMA.for.failed.patients",  "logical", TRUE);
  .cast.param.to.type("suppress.warnings",                 "logical", TRUE);
  .cast.param.to.type("save.event.info",                   "logical", TRUE);
  .cast.param.to.type("keep.event.interval.for.all.events","logical", TRUE);
  .cast.param.to.type("keep.window.start.end.dates",       "logical", TRUE);

  .cast.param.to.type("plot.width",                      "numeric", TRUE);
  .cast.param.to.type("plot.height",                     "numeric", TRUE);
  .cast.param.to.type("plot.quality",                    "numeric", TRUE);
  .cast.param.to.type("plot.dpi",                        "numeric", TRUE);
  .cast.param.to.type("plot.period.in.days",             "numeric", TRUE);
  .cast.param.to.type("plot.legend.bkg.opacity",         "numeric", TRUE);
  .cast.param.to.type("plot.legend.cex",                 "numeric", TRUE);
  .cast.param.to.type("plot.legend.cex.title",           "numeric", TRUE);
  .cast.param.to.type("plot.cex",                        "numeric", TRUE);
  .cast.param.to.type("plot.cex.axis",                   "numeric", TRUE);
  .cast.param.to.type("plot.cex.lab",                    "numeric", TRUE);
  .cast.param.to.type("plot.cex.title",                  "numeric", TRUE);
  .cast.param.to.type("plot.lwd.event",                  "numeric", TRUE);
  .cast.param.to.type("plot.pch.start.event",            "numeric", TRUE);
  .cast.param.to.type("plot.pch.end.event",              "numeric", TRUE);
  .cast.param.to.type("plot.lwd.continuation",           "numeric", TRUE);
  .cast.param.to.type("plot.CMA.plot.ratio",             "numeric", TRUE);
  .cast.param.to.type("plot.observation.window.density", "numeric", TRUE);
  .cast.param.to.type("plot.observation.window.angle",   "numeric", TRUE);
  .cast.param.to.type("plot.real.obs.window.density",    "numeric", TRUE);
  .cast.param.to.type("plot.real.obs.window.angle",      "numeric", TRUE);

  .cast.param.to.type("carryover.within.obs.window",     "logical", TRUE);
  .cast.param.to.type("carryover.into.obs.window",       "logical", TRUE);
  .cast.param.to.type("carry.only.for.same.medication",  "logical", TRUE);
  .cast.param.to.type("consider.dosage.change",          "logical", TRUE);
  .cast.param.to.type("medication.change.means.new.treatment.episode", "logical", TRUE);
  .cast.param.to.type("dosage_change_means_new_treatment_episode",     "logical", TRUE);

  .cast.param.to.type("plot.medication.groups.separator.show", "logical", TRUE);
  .cast.param.to.type("plot.medication.groups.separator.lwd",  "numeric", TRUE);
  .cast.param.to.type("plot.plot.events.vertically.displaced", "logical", TRUE);
  .cast.param.to.type("plot.print.dose",                       "logical", TRUE);
  .cast.param.to.type("plot.cex.dose",                         "numeric", TRUE);
  .cast.param.to.type("plot.print.dose.centered",              "logical", TRUE);
  .cast.param.to.type("plot.plot.dose",                        "logical", TRUE);
  .cast.param.to.type("plot.lwd.event.max.dose",               "numeric", TRUE);
  .cast.param.to.type("plot.plot.dose.lwd.across.medication.classes",    "logical", TRUE);
  .cast.param.to.type("plot.cma.cex",                                    "numeric", TRUE);
  .cast.param.to.type("plot.plot.partial.CMAs.as.timeseries.vspace",          "numeric", TRUE);
  .cast.param.to.type("plot.plot.partial.CMAs.as.timeseries.start.from.zero", "logical", TRUE);
  .cast.param.to.type("plot.plot.partial.CMAs.as.timeseries.lwd.interval",    "numeric", TRUE);
  .cast.param.to.type("plot.plot.partial.CMAs.as.timeseries.alpha.interval",  "numeric", TRUE);
  .cast.param.to.type("plot.plot.partial.CMAs.as.timeseries.show.0perc",      "logical", TRUE);
  .cast.param.to.type("plot.plot.partial.CMAs.as.timeseries.show.100perc",    "logical", TRUE);
  .cast.param.to.type("plot.plot.partial.CMAs.as.overlapping.alternate",      "logical", TRUE);
  .cast.param.to.type("plot.observation.window.opacity",                 "numeric", TRUE);
  .cast.param.to.type("plot.rotate.text",                                "numeric", TRUE);
  .cast.param.to.type("plot.force.draw.text",                            "logical", TRUE);
  .cast.param.to.type("plot.min.plot.size.in.characters.horiz",          "numeric", TRUE);
  .cast.param.to.type("plot.min.plot.size.in.characters.vert",           "numeric", TRUE);
  .cast.param.to.type("plot.max.patients.to.plot",                       "numeric", TRUE);
  .cast.param.to.type("plot.do.not.draw.plot",                           "logical", TRUE);
  .cast.param.to.type("return.inner.event.info",                         "logical", TRUE);

  # col.cats is special in that it can be a function name or a color name:
  col.cats <- trimws(.get.param.value("plot.col.cats", type="character", required=FALSE));
  if( is.na(col.cats) )
  {
    # Go for the default:
    col.cats <- rainbow;
  } else if( substring(col.cats, nchar(col.cats)-1, nchar(col.cats)) == "()" )
  {
    # it seems to be a function name, so match it to the ones we currently support:
    col.cats <- switch(col.cats,
                       "rainbow()"=rainbow,
                       "heat.colors()"=heat.colors,
                       "terrain.colors()"=terrain.colors,
                       "topo.colors()"=topo.colors,
                       "cm.colors()"=cm.colors,
                       "viridis()"=viridisLite::viridis,
                       "magma()"=viridisLite::magma,
                       "inferno()"=viridisLite::inferno,
                       "plasma()"=viridisLite::plasma,
                       "cividis()"=viridisLite::cividis,
                       "rocket()"=viridisLite::rocket,
                       "mako"=viridisLite::mako,
                       "turbo"=viridisLite::turbo,
                       rainbow); # defaults to rainbow
  } # otherwise it is a color name, so use it as such

  # xlab.* are special in that they need assembly into a single named vector:
  xlab.dates <- trimws(.get.param.value("plot.xlab.dates", type="character", required=FALSE));
  xlab.days  <- trimws(.get.param.value("plot.xlab.days",  type="character", required=FALSE));
  xlab <- c("dates"=xlab.dates, "days"=xlab.days);

  # ylab.* are special in that they need assembly into a single named vector:
  ylab.withoutcma <- trimws(.get.param.value("plot.ylab.withoutcma", type="character", required=FALSE));
  ylab.withcma    <- trimws(.get.param.value("plot.ylab.withcma",    type="character", required=FALSE));
  ylab <- c("withoutCMA"=ylab.withoutcma, "withCMA"=ylab.withcma);

  # title.* are special in that they need assembly into a single named vector:
  title.aligned    <- trimws(.get.param.value("plot.title.aligned",    type="character", required=FALSE));
  title.notaligned <- trimws(.get.param.value("plot.title.notaligned", type="character", required=FALSE));
  title.main <- c("aligned"=title.aligned, "notaligned"=title.notaligned);

  # medication.groups.to.plot is a bit special:
  if( (medication.groups.to.plot <- trimws(.get.param.value("plot.medication.groups.to.plot", type="character", default.value="", required=FALSE))) == "" ) medication.groups.to.plot <- NA; # NA and NULL are equivalent

  # plot.partial.CMAs.as is a bit special:
  if( (plot.partial.CMAs.as <- trimws(.get.param.value("plot.plot.partial.CMAs.as", type="character", default.value="", required=FALSE))) == "" ) plot.partial.CMAs.as <- NULL;

  # plot.partial.CMAs.as is special in that it might be a vector of strings:
  alternating.bands.cols <- trimws(.get.param.value("plot.alternating.bands.cols", type="character", default.value="", required=FALSE));
  if( alternating.bands.cols == "" )
  {
    alternating.bands.cols <- NA;
  } else
  {
    # see if it is a list of strings:
    alternating.bands.cols <- strsplit(alternating.bands.cols, ",", fixed=TRUE)[[1]];
    if( length(alternating.bands.cols) == 1 )
    {
      alternating.bands.cols <- .remove.spaces.and.quotes(alternating.bands.cols);
    } else
    {
      alternating.bands.cols <- vapply(alternating.bands.cols, .remove.spaces.and.quotes, character(1));
    }
  }

  # medication.groups is a bit special:
  if( (medication.groups <- trimws(.get.param.value("medication.groups", type="character", default.value="", required=FALSE))) == "" ) params.as.list[["medication.groups"]] <- NULL;

  if( suppressWarnings(!is.na(as.numeric(params.as.list[["parallel.threads"]]))) )
  {
    params.as.list[["parallel.threads"]] <- as.numeric(params.as.list[["parallel.threads"]]);
  } else if( is.character(params.as.list[["parallel.threads"]]) && params.as.list[["parallel.threads"]] == "auto" )
  {
    # nothing to pre-process
  } else
  {
    # try to eval it:
    tmp <- NULL;
    tmp <- try(eval(parse(text=as.character(params.as.list[["parallel.threads"]]))), silent=FALSE);
    if( is.null(tmp) )
    {
      msg <- paste0("AdhereR: I don't understand parallel.threads=\"",as.character(params.as.list[["parallel.threads"]]),"\"!\n");
      cat(msg); cat(msg, file=msg.file, append=TRUE);
      quit(save="no", status=13, runLast=FALSE);
    } else
    {
      params.as.list[["parallel.threads"]] <- tmp;
    }
  }

  # special case for plotting: don't compute the CMA for all patients but only for those to be plotted:
  if( .get.param.value("plot.show", type="logical", default.value=FALSE, required=FALSE) &&
      !is.null(patients.to.plot <- .get.param.value("plot.patients.to.plot", type="character.vector", default.value=NULL, required=FALSE)) )
  {
    data <- data[ data[,.get.param.value("ID.colname", type="character", required=TRUE)] %in% patients.to.plot, ];
    if( is.null(data) || nrow(data) == 0 )
    {
      msg <- paste0("AdhereR: No patients to plot!...\n");
      cat(msg); cat(msg, file=msg.file, append=TRUE);
      quit(save="no", status=15, runLast=FALSE);
    }
  }


  # add the data to the list of params as well:
  params.as.list <- c(list("data"=data), params.as.list);

  # medication groups: always flatten them:
  params.as.list[["flatten.medication.groups"]] <- TRUE; params.as.list[["medication.groups.colname"]] <- "__MED_GROUP_ID";

  # call the appropriate function:
  function.to.call <- .get.param.value("function", type="character", required=TRUE);

  # avoid warnings about the arguments.that.should.not.be.defined:
  if(function.to.call %in% c("CMA0", "CMA1", "CMA2", "CMA3", "CMA4", "CMA5", "CMA6", "CMA7", "CMA8", "CMA9", "CMA_per_episode", "CMA_sliding_window") )
  {
    # get the arguments.that.should.not.be.defined directly from the function definition:
    arguments.to.undefine <- names(formals(function.to.call)[["arguments.that.should.not.be.defined"]]);
    if( !is.null(arguments.to.undefine) && length(arguments.to.undefine) > 0 )
    {
      arguments.to.undefine <- arguments.to.undefine[ arguments.to.undefine != "" ]; # keep only the named arguments
      params.as.list <- params.as.list[!(names(params.as.list) %in% arguments.to.undefine)]; # simply remove these arguments from the list (if already there)
    }
  }
  # call the function:
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
                    "CMA_sliding_window"=do.call(function.to.call, params.as.list), # call a CMA function
                    "compute_event_int_gaps"=do.call("compute.event.int.gaps", params.as.list), # call compute.event.int.gaps()
                    "compute_treatment_episodes"= do.call("compute.treatment.episodes", params.as.list), # call compute.treatment.episodes()
                    "plot_interactive_cma"=.do.interactive.plotting(params.as.list), # call the interactive plotting function
                    NULL # oops!
  );


  if( is.null(results) ) # OOPS! some error occurred: make it known and quit!
  {
    if( function.to.call == "plot_interactive_cma" )
    {
      # interactive plotting is a special case where NULL is just a sign of ending the call...
      cat("OK: interactive plotting is over (but still, there might be warnings and messages above worth paying attention to)!\n", file=stderr());
    } else
    {
      msg <- "\nSOME ERROR HAS OCCURED (maybe there's some helpful messages above?)\n";
      cat(msg); cat(msg, file=msg.file, append=TRUE);
      quit(save="no", status=16, runLast=FALSE);
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
    if( length(cl_res <- class(results)) == 1 && cl_res == "CMA0" )
    {
      # Nothing to export....
    } else if( inherits(results, "CMA0") || inherits(results, "CMA_per_episode") || inherits(results, "CMA_sliding_window") )
    {
      # Special case: for plot.show == TRUE, add the "-plotted" suffix to the saved files!
      file.name.suffix <- ifelse(.get.param.value("plot.show", type="character", default.value="FALSE", required=FALSE) == "TRUE", "-plotted", "" );

      # CMAs:
      write.table(.apply.export.conversions(results$CMA), paste0(shared.data.directory,"/CMA",file.name.suffix,".csv"), row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE);
      # event info:
      if( !is.na(save.event.info <- .get.param.value("save.event.info", type="character", default.value=NA, required=FALSE)) && save.event.info=="TRUE" )
      {
        write.table(.apply.export.conversions(results$event.info), paste0(shared.data.directory,"/EVENTINFO",file.name.suffix,".csv"), row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE);
      }
      # inner event info:
      if( !is.na(return.inner.event.info <- .get.param.value("return.inner.event.info", type="character", default.value=NA, required=FALSE)) && return.inner.event.info=="TRUE" &&
          (inherits(results, "CMA_per_episode") || inherits(results, "CMA_sliding_window")) )
      {
        write.table(.apply.export.conversions(results$inner.event.info), paste0(shared.data.directory,"/INNEREVENTINFO",file.name.suffix,".csv"), row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE);
      }
    } else if( function.to.call == "compute_event_int_gaps" && inherits(results, "data.frame") && nrow(results) > 0 && ncol(results) > 0 )
    {
      # event info:
      write.table(.apply.export.conversions(results), paste0(shared.data.directory,"/EVENTINFO.csv"), row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE);
    } else if( function.to.call == "compute_treatment_episodes" && inherits(results, "data.frame") && nrow(results) > 0 && ncol(results) > 0 )
    {
      # treatment episodes:
      write.table(.apply.export.conversions(results), paste0(shared.data.directory,"/TREATMENTEPISODES.csv"), row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE);
    } else
    {
      # how did we get here?
      msg <- "\nDon't know how to export this type of results!\n";
      cat(msg); cat(msg, file=msg.file, append=TRUE);
      quit(save="no", status=17, runLast=FALSE);
    }


    # Plotting might have been required:
    if( .get.param.value("plot.show", type="logical", default.value="FALSE", required=FALSE) )
    {
      # OK, plotting it too!

      # Get the list of relevant parameters (ignore plot.show):
      plotting.params <- params.as.list[grep("^plot\\.", names(params.as.list))];
      plotting.params <- plotting.params[ names(plotting.params) != "plot.show" ];
      names(plotting.params) <- substring(names(plotting.params), nchar("plot.")+1);

      # patients.to.plot has already been parsed:
      if( "patients.to.plot" %in% names(plotting.params) ) plotting.params[["patients.to.plot"]] <- patients.to.plot;

      # col.cats has already been parsed:
      if( "col.cats" %in% names(plotting.params) ) plotting.params[["col.cats"]] <- col.cats;

      # xlab has already been parsed:
      if( "xlab.dates" %in% names(plotting.params) && "xlab.days" %in% names(plotting.params) ){ plotting.params[["xlab"]] <- xlab; plotting.params["xlab.dates"] <- NULL; plotting.params["xlab.days"] <- NULL; }

      # ylab has already been parsed:
      if( "ylab.withoutcma" %in% names(plotting.params) && "ylab.withcma" %in% names(plotting.params) ){ plotting.params[["ylab"]] <- ylab; plotting.params["ylab.withoutcma"] <- NULL; plotting.params["ylab.withcma"] <- NULL; }

      # title has already been parsed:
      if( "title.aligned" %in% names(plotting.params) && "title.notaligned" %in% names(plotting.params) ){ plotting.params[["title"]] <- title.main; plotting.params["title.aligned"] <- NULL; plotting.params["title.notaligned"] <- NULL; }

      # medication.groups.to.plot has already been parsed:
      if( "medication.groups.to.plot" %in% names(plotting.params) ) plotting.params[["medication.groups.to.plot"]] <- medication.groups.to.plot;

      # plot.partial.CMAs.as has already been parsed:
      if( "plot.partial.CMAs.as" %in% names(plotting.params) ) plotting.params[["plot.partial.CMAs.as"]] <- plot.partial.CMAs.as;

      # alternating.bands.cols has already been parsed:
      plotting.params[["alternating.bands.cols"]] <- alternating.bands.cols;

      # Get the info about the plot exporting process:
      plot.file.dir <- .get.param.value("plot.save.to", type="character", default.value=shared.data.directory, required=FALSE);
      # Check if the directory exists and is writtable:
      if( !dir.exists(plot.file.dir) || file.access(plot.file.dir, 2) != 0 )
      {
        msg <- paste0("\nThe destination directory for plots '",plot.file.dir,"' does not exist or does not have write access!\n");
        cat(msg); cat(msg, file=msg.file, append=TRUE);
        quit(save="no", status=15, runLast=FALSE);
      }
      plot.file.name <- paste0(plot.file.dir,"/adherer-plot.");
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
                   horizontal=FALSE, onefile=FALSE, paper="special"); # make sure the output is EPS
      } else if( plot.file.type %in% c("pdf") )
      {
        pdf(paste0(plot.file.name,"pdf"),
            width=.get.param.value("plot.width", type="numeric", default.value=7, required=FALSE),
            height=.get.param.value("plot.height", type="numeric", default.value=7, required=FALSE),
            onefile=FALSE, paper="special");
      }

      # attempt to plot:
      msg <- capture.output(do.call("plot", c(list(results), plotting.params)), file=NULL, type="output");
      if( length(msg) > 0 )
      {
        dev.off(); # close the plotting device anyway
        cat(msg); cat(paste0(msg,"\n"), file=msg.file, append=TRUE);
        quit(save="no", status=0, runLast=FALSE); # Some plotting error seems to have occurred
      }

      # close the plotting device:
      dev.off();
    }
  }


  msg <- "OK: the results were exported successfully (but there might be warnings and messages above worth paying attention to)!\n";
  cat(msg); cat(msg, file=msg.file, append=TRUE);
  quit(save="no", status=0, runLast=FALSE); # everything seems OK
}



#' getCallerWrapperLocation.
#'
#' This function returns the full path to where the various \code{wrappers} that
#' can call \code{AdhereR} are installed.
#'
#' In most cases, these wrappers are one or more files in the calling language
#' that may be directly used as such.
#' For more details see the vignette describing the included reference
#' \code{Python 3} wrapper.
#'
#' @param callig.platform A \emph{string} specifying the desired wrapper.
#' Currently it can be "python3".
#' @param full.path A \emph{logical} specifying if the returned path should
#' also include the wrapper's main file name.
#' @return The full path to the requested wrapper or NULL if none exists.
#' @export
getCallerWrapperLocation <- function(callig.platform=c("python3", "stata")[1], full.path=FALSE)
{
  switch(tolower(callig.platform),
         "python3" = file.path(system.file(package="AdhereR"), "wrappers", "python3", ifelse(full.path,"adherer.py","")),
         "stata" = file.path(system.file(package="AdhereR"), "wrappers", "stata", ifelse(full.path,"adherer.ado","")),
         NULL);
}
