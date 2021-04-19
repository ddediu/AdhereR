###############################################################################################
#
#    AdhereR: an R package for computing various estimates of medication adherence.
#    Copyright (C) 2015-2018  Dan Dediu & Alexandra Dima
#    Copyright (C) 2018-2019  Dan Dediu, Alexandra Dima & Samuel Allemann
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

#' @import grDevices
#' @import graphics
#' @import stats
#' @import data.table
#' @import utils
#' @import methods
NULL

# Declare some variables as global to avoid NOTEs during package building:
globalVariables(c(".OBS.START.DATE", ".OBS.START.DATE.PRECOMPUTED", ".OBS.START.DATE.UPDATED", ".OBS.WITHIN.FU",
                  ".PROCESSING.CHUNK", ".START.BEFORE.END.WITHIN.OBS.WND", ".WND.ID", ".WND.START.DATE", "CMA",
                  ".CARRY.OVER.FROM.BEFORE", ".DATE.as.Date", ".END.EVENT.DATE", ".EVENT.STARTS.AFTER.OBS.WINDOW",
                  ".EVENT.STARTS.BEFORE.OBS.WINDOW", ".EVENT.WITHIN.FU.WINDOW", ".FU.START.DATE", ".FU.START.DATE.UPDATED",
                  ".INTERSECT.EPISODE.OBS.WIN.END", ".INTERSECT.EPISODE.OBS.WIN.START", ".OBS.DURATION.UPDATED",
                  ".OBS.END.DATE", ".OBS.END.DATE.PRECOMPUTED",
                  "episode.ID", "episode.duration", "end.episode.gap.days"));

## Package private info ####
# Store various info relevant to the package (such as info about the last plot or the last error).
# As this is inside a package and we must avoid locking, we ned to use an environment (see, e.g. https://www.r-bloggers.com/package-wide-variablescache-in-r-packages/)
.adherer.env <- new.env();

# Info about the last plot (initially, none):
assign(".last.cma.plot.info", NULL, envir=.adherer.env);

# Info about errors, warnings and messages (initially, none):
assign(".ewms", NULL, envir=.adherer.env);
assign(".record.ewms", FALSE, envir=.adherer.env); # initially, do not record the errors, warnings and message, but just display them

# Clear the info about errors, warnings and messages:
.clear.ewms <- function() { assign(".ewms", NULL, envir=.adherer.env); }

# Get the info about errors, warnings and messages (basically, a data.frame contaning all the important info, one thing per row):
.get.ewms <- function() { return (get(".ewms", envir=.adherer.env)); }

# Are we recording the errors, warnings and messages?
.is.recording.ewms <- function() { return (!is.null(.record.ewms <- get(".record.ewms", envir=.adherer.env)) && .record.ewms); }

# Start/stop recording the errors, warnings and messages:
.record.ewms <- function(record=TRUE) { .clear.ewms(); assign(".record.ewms", record, envir=.adherer.env); }

# Reporting an error, warning or message:
.report.ewms <- function(text, # the text
                        type=c("error", "warning", "message")[1], # the type
                        fnc.name=NA, pkg.name=NA, # which function and package generated it
                        generate.exception=TRUE, # should we throw an actual error or watning using R's exceptions mechanism?
                        error.as.warning=TRUE # when reporting an error, should it stop() the whole process or be thrown as a warning() so the processes continues?
)
{
  # Make sure text is really a text:
  text <- as.character(text);
  if( length(text) > 1 ) text <- paste0("[ ", paste0("'", as.character(text), "'", collapse="; "), " ]");

  # Add this new ewms info:
  if( .is.recording.ewms() )
  {
    assign(".ewms",
           rbind(.get.ewms(),
                 data.frame("text"=text, "type"=type, "function"=fnc.name, "package"=pkg.name, "exception.was.thrown"=generate.exception)),
           envir=.adherer.env);
  }

  # Throw an exception (if the case):
  if( generate.exception )
  {
    if( type == "error" )
    {
      if( !error.as.warning )
      {
        stop(text);
      } else
      {
        warning(text);
      }
    } else if( type == "warning" )
    {
      warning(text);
    } else
    {
      print(paste0("Info: ",text,
                   if(!is.na(fnc.name)) paste0(" [from function ",fnc.name,
                                               if(!is.na(pkg.name)) paste0(", package ",pkg.name),
                                               "]"),
                   collapse=""));
    }
  }
}


#' Example medication events records for 100 patients.
#'
#' An artificial dataset containing medication events (one per row) for 100
#' patients (1080 events in total). This is the dataset format appropriate for
#' medication adherence analyses performed with the R package AdhereR.
#' Medication events represent individual records of prescribing or dispensing
#' a specific medication for a patient at a given date. Dosage and medication
#' type is optional (only needed if calculation of adherence or persistence
#' takes into account changes in dosage and type of medication).
#'
#' @format A data frame with 1080 rows and 5 variables:
#' \describe{
#'   \item{PATIENT_ID}{\emph{integer} here; patient unique identifier. Can also
#'   be \emph{string}}.
#'   \item{DATE}{\emph{character};the medication event date, by default in the
#'   mm/dd/yyyy format. It may represent a prescribing or dispensing date.}
#'   \item{PERDAY}{\emph{integer}; the daily dosage prescribed for the
#'   medication supplied at this medication event (i.e. how many doses should
#'   be taken daily according to the prescription). This column is optional,
#'   as it is not considered in all functions but may be relevant for specific
#'   research or clinical contexts. All values should be > 0.}
#'   \item{CATEGORY}{\emph{character}; the medication type, here two placeholder
#'   labels, 'medA' and 'medB'. This is a researcher-defined classification
#'   depending on study aims (e.g., based on therapeutic use, mechanism of
#'   action, chemical molecule, or pharmaceutical formulation). This column is
#'   optional, as it is not considered in all functions but may be relevant for
#'   specific research or clinical contexts.}
#'   \item{DURATION}{\emph{integer}; the medication event duration in days (i.e.
#'   how many days the mediation supplied would last if used as prescribed);
#'   may be available in the extraction or computed based on quantity supplied
#'   (the number of doses prescribed or dispensed on that occasion) and daily
#'   dosage. All values should be > 0.}
#' }
"med.events"


# Check, parse and evaluate the medication groups:
# The idea is to transform each group into a function and let R do the heavy lifting:
# Returns a list with two components:
#  - defs (contains the group names and definitions) and
#  - obs (logical matrix saying for each observation (row) if it belongs to a group (column))
# or NULL if error occurred:
.apply.medication.groups <- function(medication.groups, # the medication groups
                                     data, # the data on which to apply them
                                     suppress.warnings=FALSE)
{
  if( is.null(medication.groups) || length(medication.groups) == 0 )
  {
    # by definition, there's nothing wrong with NULL
    return (list("groups"=NULL,
                 "obs_in_groups"=NULL,
                 "errors"=NULL));
  }

  # Basic checks:
  if( is.null(data) || !inherits(data, "data.frame") || nrow(data) == 0 )
  {
    if( !suppress.warnings ) .report.ewms("The data must be a non-emtpy data.frame (or derived) object!\n", "error", ".apply.medication.groups", "AdhereR");
    return (NULL);
  }
  data <- as.data.frame(data); # make sure we convert it to a data.frame

  if( !(is.character(medication.groups) || is.factor(medication.groups)) )
  {
    if( !suppress.warnings ) .report.ewms("The medication groups must be a vector of characters!\n", "error", ".apply.medication.groups", "AdhereR");
    return (NULL);
  }

  if( length(medication.groups) == 1 && (medication.groups %in% names(data)) )
  {
    # It is a column in the data: transform it into the corresponding explicit definitions:
    mg.vals <- unique(data[,medication.groups]); mg.vals <- mg.vals[!is.na(mg.vals)]; # the unique non-NA values
    if( is.null(mg.vals) || length(mg.vals) == 0 )
    {
      if( !suppress.warnings ) .report.ewms("The column '",medication.groups,"' in the data must contain at least one non-missing value!\n", "error", ".apply.medication.groups", "AdhereR");
      return (NULL);
    }
    mg.vals <- sort(mg.vals); # makes it easier to view if ordered
    medication.groups.defs <- paste0('(',medication.groups,' == "',mg.vals,'")'); names(medication.groups.defs) <- mg.vals;
    medication.groups <- medication.groups.defs;
  }

  if( length(names(medication.groups)) != length(medication.groups) || any(duplicated(names(medication.groups))) || ("" %in% names(medication.groups)) )
  {
    if( !suppress.warnings ) .report.ewms("The medication groups must be a named list with unique and non-empty names!\n", "error", ".apply.medication.groups", "AdhereR");
    return (NULL);
  }


  # The safe environment for evaluating the medication groups (no parent) containing only the needed variables and functions (as per https://stackoverflow.com/a/18391779):
  safe_env <- new.env(parent = emptyenv());
  # ... add the safe functions:
  for( .f in c(getGroupMembers("Math"),
               getGroupMembers("Arith"),
               getGroupMembers("Logic"),
               getGroupMembers("Compare"),
               "(", "[", "!") )
  {
    safe_env[[.f]] <- get(.f, "package:base");
  }
  # ... and variables:
  assign(".data", data, envir=safe_env); # the data
  assign(".res", matrix(NA, nrow=nrow(data), ncol=length(medication.groups)), envir=safe_env); # matrix of results from evaluating the medication classes
  assign(".evald", rep(FALSE, length(medication.groups)), envir=safe_env); # records which the medication groups were already evaluated (to speed up things)
  assign(".errs", NULL, envir=safe_env); # possible errors encountered during the evaluation
  # The safe eval function (use evalq to avoid searching in the current environment): evalq(expr, env = safe_env);

  # Check, transform, parse and add the group definitions as functions to the .mg_safe_env_original environment to be later used in the safe evaluation environment:
  mg <- data.frame("name"=names(medication.groups),
                   "def"=medication.groups,
                   "uid"=make.names(names(medication.groups), unique=TRUE, allow_=TRUE),
                   "fnc_name"=NA); # convert the names to valid identifiers
  mg$fnc_name <- paste0(".mg_fnc_", mg$uid);
  for( i in 1:nrow(mg) )
  {
    # Cache the definition:
    s <- mg$def[i];
    if( is.na(s) || is.null(s) || s == "" )
    {
      # Empty definition!
      if( !suppress.warnings ) .report.ewms(paste0("Error parsing the medication class definition '",mg$name[i],"': this definition is empty!\n"), "error", ".apply.medication.groups", "AdhereR");
      return (NULL);
    }

    # Search for medication group references of the form {name} :
    ss <- gregexpr("\\{[^(\\})]+\\}",s); #gregexpr("\\{[[:alpha:]]+\\}",s);
    if( ss[[1]][1] != (-1) )
    {
      # There's at least one group reference: replace it by the corresponding function call:
      ss_calls <- regmatches(s, ss)[[1]];
      ss_calls_replaced <- vapply(ss_calls, function(x)
      {
        if( length(ii <- which(mg$name == substr(x, 2, nchar(x)-1))) != 1 )
        {
          if( !suppress.warnings ) .report.ewms(paste0("Error parsing the medication class definition '",mg$name[i],"': there is a call to the undefined medication class '",substr(x, 2, nchar(x)-1),"'!\n"), "error", ".apply.medication.groups", "AdhereR");
          return (NA);
        } else
        {
          return (paste0("safe_env$",mg$fnc_name[ii],"()"));
        }
      }, character(1));
      if( anyNA(ss_calls_replaced) )
      {
        # Error parsing medication class definitions (the message should be already generated):
        return (NULL);
      }
      regmatches(s, ss)[[1]] <- ss_calls_replaced;
    }

    # Make it into a function definition:
    s_fnc <- paste0("function()
                    {
                      if( !safe_env$.evald[",i,"] )
                      {
                        # not already evaluated:
                        tmp <- try(with(safe_env$.data, ",s,"), silent=TRUE);
                        if( inherits(tmp, 'try-error') )
                        {
                          # fail gracefully and informatively:
                          safe_env$.errs <- rbind(safe_env$.errs,
                                                  data.frame('fnc'='",mg$fnc_name[i],"', 'error'=as.character(tmp)));
                          stop('Error in ",mg$fnc_name[i],"');
                        }
                        # sanity checks:
                        if( is.null(tmp) || !is.logical(tmp) || length(tmp) != nrow(safe_env$.data) )
                        {
                          # fail gracefully and informatively:
                          safe_env$.errs <- rbind(safe_env$.errs,
                                                  data.frame('fnc'='",mg$fnc_name[i],"', 'error'='Error: evaluation did not produce logical results'));
                          stop('Error in ",mg$fnc_name[i],"');
                        }
                        safe_env$.res[,",i,"] <- tmp; safe_env$.evald[",i,"] <- TRUE;
                      } # else, it should have already been evaluated!
                      # return the value
                      return (safe_env$.res[,",i,"]);
                    }");

    # Parse it:
    s_parsed <- NULL;
    try(s_parsed <- parse(text=s_fnc), silent=TRUE);
    if( is.null(s_parsed) )
    {
      if( !suppress.warnings ) .report.ewms(paste0("Error parsing the medication class definition '",mg$name[i],"'!\n"), "error", ".apply.medication.groups", "AdhereR");
      return (NULL);
    }

    # Add the function:
    safe_env[[mg$fnc_name[i]]] <- eval(s_parsed);
  }


  # Evaluate the medication groups on the data:
  for( i in 1:nrow(mg) )
  {
    # Call the corresponding function:
    res <- NULL;
    try(res <- eval(parse(text=paste0(mg$fnc_name[i],"()")), envir=safe_env), silent=TRUE); # the errors will be anyway recorded in the .errs variable in safe_env
    if( is.null(res) || inherits(res, 'try-error') || !safe_env$.evald[i] )
    {
      # Evaluation error:
      if( !is.null(safe_env$.errs) )
      {
        err_msgs <- unique(safe_env$.errs);
        err_msgs$error <- vapply(err_msgs$error, function(s) trimws(strsplit(s,":",fixed=TRUE)[[1]][2]), character(1));
        err_msgs$fnc <- vapply(err_msgs$fnc, function(s) mg$name[ mg$fnc_name == s ], character(1));
        ss <- gregexpr(".mg_fnc_",err_msgs$error); regmatches(err_msgs$error, ss) <- "";
        if( !suppress.warnings ) .report.ewms(paste0("Error(s) during the evaluation of medication class(es): ",paste0("'",err_msgs$error," (for ",err_msgs$fnc,")'",colapse=", "),"!\n"), "warning", ".apply.medication.groups", "AdhereR");
      }
      return (NULL);
    }
  }

  # Retrieve the results and compute __ALL_OTHERS__:
  groups_info <- cbind(rbind(mg[,c("name", "def")], c("__ALL_OTHERS__", "*all observations not included in the defined groups*")),
                       "evaluated"=c(safe_env$.evald, TRUE)); rownames(groups_info) <- NULL;
  obs_in_groups <- cbind(safe_env$.res, rowSums(!is.na(safe_env$.res) & safe_env$.res) == 0); colnames(obs_in_groups) <- c(mg$name, "__ALL_OTHERS__");

  # ... and return them:
  return (list("defs"=groups_info[,c("name", "def")], "obs"=obs_in_groups));
}



#' CMA0 constructor.
#'
#' Constructs a basic CMA (continuous multiple-interval measures of medication
#' availability/gaps) object.
#'
#' In most cases this should not be done directly by the user,
#' but it is used internally by the other CMAs.
#'
#' @param data A \emph{\code{data.frame}} containing the medication events
#' (prescribing or dispensing) used to compute the CMA. Must contain, at a
#' minimum, the patient unique ID, the event date and duration, and might also
#' contain the daily dosage and medication type (the actual column names are
#' defined in the following four parameters).
#' @param ID.colname A \emph{string}, the name of the column in \code{data}
#' containing the unique patient ID, or \code{NA} if not defined.
#' @param event.date.colname A \emph{string}, the name of the column in
#' \code{data} containing the start date of the event (in the format given in
#' the \code{date.format} parameter), or \code{NA} if not defined.
#' @param event.duration.colname A \emph{string}, the name of the column in
#' \code{data} containing the event duration (in days), or \code{NA} if not
#' defined.
#' @param event.daily.dose.colname A \emph{string}, the name of the column in
#' \code{data} containing the prescribed daily dose, or \code{NA} if not defined.
#' @param medication.class.colname A \emph{string}, the name of the column in
#' \code{data} containing the classes/types/groups of medication, or \code{NA}
#' if not defined.
#' @param medication.groups A \emph{vector} of characters defining medication
#' groups or the name of a column in \code{data} that defines such groups.
#' The names of the vector are the medication group unique names, while
#' the content defines them as logical expressions. While the names can be any
#' string of characters except "\}", it is recommended to stick to the rules for
#' defining vector names in \code{R}. For example,
#' \code{c("A"="CATEGORY == 'medA'", "AA"="{A} & PERDAY < 4"} defines two
#' medication groups: \emph{A} which selects all events of type "medA", and
#' \emph{B} which selects all events already defined by "A" but with a daily
#' dose lower than 4. If \code{NULL}, no medication groups are defined. If
#' medication groups are defined, there is one CMA estimate for each group;
#' moreover, there is a special group \emph{__ALL_OTHERS__} automatically defined
#' containing all observations \emph{not} covered by any of the explicitly defined
#' groups.
#' @param flatten.medication.groups \emph{Logical}, if \code{FALSE} (the default)
#' then the \code{CMA} and \code{event.info} components of the object are lists
#' with one medication group per element; otherwise, they are \code{data.frame}s
#' with an extra column containing the medication group (its name is given by
#' \code{medication.groups.colname}).
#' @param medication.groups.colname a \emph{string} (defaults to ".MED_GROUP_ID")
#' giving the name of the column storing the group name when
#' \code{flatten.medication.groups} is \code{TRUE}.
#' @param carryover.within.obs.window \emph{Logical}, if \code{TRUE} consider
#' the carry-over within the observation window, or \code{NA} if not defined.
#' @param carryover.into.obs.window \emph{Logical}, if \code{TRUE} consider the
#' carry-over from before the starting date of the observation window, or
#' \code{NA} if not defined.
#' @param carry.only.for.same.medication \emph{Logical}, if \code{TRUE} the
#' carry-over applies only across medications of the same type, or \code{NA}
#' if not defined.
#' @param consider.dosage.change \emph{Logical}, if \code{TRUE} the carry-over
#' is adjusted to reflect changes in dosage, or \code{NA} if not defined.
#' @param followup.window.start If a \emph{\code{Date}} object, it represents
#' the actual start date of the follow-up window; if a \emph{string} it is the
#' name of the column in \code{data} containing the start date of the follow-up
#' window either as the numbers of \code{followup.window.start.unit} units
#' after the first event (the column must be of type \code{numeric}) or as
#' actual dates (in which case the column must be of type \code{Date} or a string
#' that conforms to the format specified in \code{date.format}); if a
#' \emph{number} it is the number of time units defined in the
#' \code{followup.window.start.unit} parameter after the begin of the
#' participant's first event; or \code{NA} if not defined.
#' @param followup.window.start.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.start} refers to (when a number), or
#' \code{NA} if not defined.
#' @param followup.window.start.per.medication.group a \emph{logical}: if there are
#' medication groups defined and this is \code{TRUE}, then the first event
#' considered for the follow-up window start is relative to each medication group
#' separately, otherwise (the default) it is relative to the patient.
#' @param followup.window.duration either a \emph{number} representing the
#' duration of the follow-up window in the time units given in
#' \code{followup.window.duration.unit}, or a \emph{string} giving the column
#' containing these numbers. Should represent a period for which relevant
#' medication events are recorded accurately (e.g. not extend after end of
#' relevant treatment, loss-to-follow-up or change to a health care provider
#' not covered by the database).
#' @param followup.window.duration.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.duration} refers to, or \code{NA} if not
#' defined.
#' @param observation.window.start,observation.window.start.unit,observation.window.duration,observation.window.duration.unit the definition of the observation window
#' (see the follow-up window parameters above for details).
#' @param date.format A \emph{string} giving the format of the dates used in
#' the \code{data} and the other parameters; see the \code{format} parameters
#' of the \code{\link[base]{as.Date}} function for details (NB, this concerns
#' only the dates given as strings and not as \code{Date} objects).
#' @param summary Metadata as a \emph{string}, briefly describing this CMA.
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any
#' warnings.
#' @param arguments.that.should.not.be.defined a \emph{list} of argument names
#' and pre-defined valuesfor which a warning should be thrown if passed to the
#' function.
#' @param ... other possible parameters
#' @return An \code{S3} object of class \code{CMA0} with the following fields:
#' \itemize{
#'  \item \code{data} The actual event (prescribing or dispensing) data, as
#'  given by the \code{data} parameter.
#'  \item \code{ID.colname} the name of the column in \code{data} containing
#'  the unique patient ID, as given by the \code{ID.colname} parameter.
#'  \item \code{event.date.colname} the name of the column in \code{data}
#'  containing the start date of the event (in the format given in the
#'  \code{date.format} parameter), as given by the \code{event.date.colname}
#'  parameter.
#'  \item \code{event.duration.colname} the name of the column in \code{data}
#'  containing the event duration (in days), as given by the
#'  \code{event.duration.colname} parameter.
#'  \item \code{event.daily.dose.colname} the name of the column in \code{data}
#'  containing the prescribed daily dose, as given by the
#'  \code{event.daily.dose.colname} parameter.
#'  \item \code{medication.class.colname} the name of the column in \code{data}
#'  containing the classes/types/groups of medication, as given by the
#'  \code{medication.class.colname} parameter.
#'  \item \code{carryover.within.obs.window} whether to consider the carry-over
#'  within the observation window, as given by the
#'  \code{carryover.within.obs.window} parameter.
#'  \item \code{carryover.into.obs.window} whether to consider the carry-over
#'  from before the starting date of the observation window, as given by the
#'  \code{carryover.into.obs.window} parameter.
#'  \item \code{carry.only.for.same.medication} whether the carry-over applies
#'  only across medication of the same type, as given by the
#'  \code{carry.only.for.same.medication} parameter.
#'  \item \code{consider.dosage.change} whether the carry-over is adjusted to
#'  reflect changes in dosage, as given by the \code{consider.dosage.change}
#'  parameter.
#'  \item \code{followup.window.start} the beginning of the follow-up window,
#'  as given by the \code{followup.window.start} parameter.
#'  \item \code{followup.window.start.unit} the time unit of the
#'  \code{followup.window.start}, as given by the
#'  \code{followup.window.start.unit} parameter.
#'  \item \code{followup.window.duration} the duration of the follow-up window,
#'  as given by the \code{followup.window.duration} parameter.
#'  \item \code{followup.window.duration.unit} the time unit of the
#'  \code{followup.window.duration}, as given by the
#'  \code{followup.window.duration.unit} parameter.
#'  \item \code{observation.window.start} the beginning of the observation
#'  window, as given by the \code{observation.window.start} parameter.
#'  \item \code{observation.window.start.unit} the time unit of the
#'  \code{observation.window.start}, as given by the
#'  \code{observation.window.start.unit} parameter.
#'  \item \code{observation.window.duration} the duration of the observation
#'  window, as given by the \code{observation.window.duration} parameter.
#'  \item \code{observation.window.duration.unit} the time unit of the
#'  \code{observation.window.duration}, as given by the \code{observation.window.duration.unit} parameter.
#'  \item \code{date.format} the format of the dates, as given by the
#'  \code{date.format} parameter.
#'  \item \code{summary} the metadata, as given by the \code{summary}
#'  parameter.
#' }
#' @examples
#' cma0 <- CMA0(data=med.events,
#'              ID.colname="PATIENT_ID",
#'              event.date.colname="DATE",
#'              event.duration.colname="DURATION",
#'              event.daily.dose.colname="PERDAY",
#'              medication.class.colname="CATEGORY",
#'              followup.window.start=0,
#'              followup.window.start.unit="days",
#'              followup.window.duration=2*365,
#'              followup.window.duration.unit="days",
#'              observation.window.start=30,
#'              observation.window.start.unit="days",
#'              observation.window.duration=365,
#'              observation.window.duration.unit="days",
#'              date.format="%m/%d/%Y",
#'              summary="Base CMA");
#' @export
CMA0 <- function(data=NULL, # the data used to compute the CMA on
                 # Important columns in the data
                 ID.colname=NA, # the name of the column containing the unique patient ID (NA = undefined)
                 event.date.colname=NA, # the start date of the event in the date.format format (NA = undefined)
                 event.duration.colname=NA, # the event duration in days (NA = undefined)
                 event.daily.dose.colname=NA, # the prescribed daily dose (NA = undefined)
                 medication.class.colname=NA, # the classes/types/groups of medication (NA = undefined)
                 # Groups of medication classes:
                 medication.groups=NULL, # a named vector of medication group definitions, the name of a column in the data that defines the groups, or NULL
                 flatten.medication.groups=FALSE, medication.groups.colname=".MED_GROUP_ID", # if medication.groups were defined, return CMAs and event.info as single data.frame?
                 # Various types methods of computing gaps:
                 carryover.within.obs.window=NA, # if TRUE consider the carry-over within the observation window (NA = undefined)
                 carryover.into.obs.window=NA, # if TRUE consider the carry-over from before the starting date of the observation window (NA = undefined)
                 carry.only.for.same.medication=NA, # if TRUE the carry-over applies only across medication of same type (NA = undefined)
                 consider.dosage.change=NA, # if TRUE carry-over is adjusted to reflect changes in dosage (NA = undefined)
                 # The follow-up window:
                 followup.window.start=0, # if a number is the earliest event per participant date plus number of units, or a Date object, or a column name in data (NA = undefined)
                 followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                 followup.window.start.per.medication.group=FALSE, # if there are medication groups and this is TRUE, then the first event is relative to each medication group separately, otherwise is relative to the patient
                 followup.window.duration=365*2, # the duration of the follow-up window in the time units given below (NA = undefined)
                 followup.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)  (NA = undefined)
                 # The observation window (embedded in the follow-up window):
                 observation.window.start=0, # the number of time units relative to followup.window.start (NA = undefined)
                 observation.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                 observation.window.duration=365*2, # the duration of the observation window in time units (NA = undefined)
                 observation.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                 # Date format:
                 date.format="%m/%d/%Y", # the format of the dates used in this function (NA = undefined)
                 # Comments and metadata:
                 summary="Base CMA object",
                 # Misc:
                 suppress.warnings=FALSE,
                 arguments.that.should.not.be.defined=NULL, # the list of argument names and values for which a warning should be thrown if passed to the function
                 ...
)
{
  # Preconditions:
  if( !is.null(data) )
  {
    # data's class and dimensions:
    if( inherits(data, "matrix") || inherits(data, "tbl") || inherits(data, "data.table") ) data <- as.data.frame(data); # convert various things to data.frame
    if( !inherits(data, "data.frame") )
    {
      if( !suppress.warnings ) .report.ewms("The 'data' for a CMA must be of type 'data.frame'!\n", "error", "CMA0", "AdhereR");
      return (NULL);
    }
    if( nrow(data) < 1 )
    {
      if( !suppress.warnings ) .report.ewms("The 'data' for a CMA must have at least one row!\n", "error", "CMA0", "AdhereR");
      return (NULL);
    }
    # the column names must exist in data:
    if( !is.na(ID.colname) && !(ID.colname %in% names(data)) )
    {
      if( !suppress.warnings ) .report.ewms(paste0("Column ID.colname='",ID.colname,"' must appear in the 'data'!\n"), "error", "CMA0", "AdhereR");
      return (NULL);
    }
    if( !is.na(event.date.colname) && !(event.date.colname %in% names(data)) )
    {
      if( !suppress.warnings ) .report.ewms(paste0("Column event.date.colname='",event.date.colname,"' must appear in the 'data'!\n"), "error", "CMA0", "AdhereR");
      return (NULL);
    }
    if( !is.na(event.duration.colname) && !(event.duration.colname %in% names(data)) )
    {
      if( !suppress.warnings ) .report.ewms(paste0("Column event.duration.colname='",event.duration.colname,"' must appear in the 'data'!\n"), "error", "CMA0", "AdhereR");
      return (NULL);
    }
    if( !is.na(event.daily.dose.colname) && !(event.daily.dose.colname %in% names(data)) )
    {
      if( !suppress.warnings ) .report.ewms(paste0("Column event.daily.dose.colname='",event.daily.dose.colname,"' must appear in the 'data'!\n"), "error", "CMA0", "AdhereR");
      return (NULL);
    }
    if( !is.na(medication.class.colname) && !(medication.class.colname %in% names(data)) )
    {
      if( !suppress.warnings ) .report.ewms(paste0("Column medication.class.colname='",medication.class.colname,"' must appear in the 'data'!\n"), "error", "CMA0", "AdhereR");
      return (NULL);
    }
    # carry-over conditions:
    if( !is.na(carryover.within.obs.window) && !is.logical(carryover.within.obs.window) )
    {
      if( !suppress.warnings ) .report.ewms(paste0("Parameter 'carryover.within.obs.window' must be logical!\n"), "error", "CMA0", "AdhereR");
      return (NULL);
    }
    if( !is.na(carryover.into.obs.window) && !is.logical(carryover.into.obs.window) )
    {
      if( !suppress.warnings ) .report.ewms(paste0("Parameter 'carryover.into.obs.window' must be logical!\n"), "error", "CMA0", "AdhereR");
      return (NULL);
    }
    if( !is.na(carry.only.for.same.medication) && !is.logical(carry.only.for.same.medication) )
    {
      if( !suppress.warnings ) .report.ewms(paste0("Parameter 'carry.only.for.same.medication' must be logical!\n"), "error", "CMA0", "AdhereR");
      return (NULL);
    }
    if( !is.na(consider.dosage.change) && !is.logical(consider.dosage.change) )
    {
      if( !suppress.warnings ) .report.ewms(paste0("Parameter 'consider.dosage.change' must be logical!\n"), "error", "CMA0", "AdhereR");
      return (NULL);
    }
    if( (!is.na(carryover.within.obs.window) && !is.na(carryover.into.obs.window) && !is.na(carry.only.for.same.medication)) &&
          (!carryover.within.obs.window && !carryover.into.obs.window && carry.only.for.same.medication) )
    {
      if( !suppress.warnings ) .report.ewms("Cannot carry over only for same medication when no carry over at all is considered!\n", "error", "CMA0", "AdhereR")
      return (NULL);
    }
    # the follow-up window:
    if( !is.na(followup.window.start) && !inherits(followup.window.start,"Date") && !is.numeric(followup.window.start) && !(followup.window.start %in% names(data)) )
    {
      # See if it can be forced to a valid Date:
      if( !is.na(followup.window.start) && (is.character(followup.window.start) || is.factor(followup.window.start)) && !is.na(followup.window.start <- as.Date(followup.window.start, format=date.format, optional=TRUE)) )
      {
        # Ok, it was apparently successfully converted to Date: nothing else to do...
      } else
      {
        if( !suppress.warnings ) .report.ewms("The follow-up window start must be either a number, a Date, or a valid column name in 'data'!\n", "error", "CMA0", "AdhereR")
        return (NULL);
      }
    }
    if( !inherits(followup.window.start,"Date") && !is.na(followup.window.start.unit) && !(followup.window.start.unit %in% c("days", "weeks", "months", "years") ) )
    {
      if( !suppress.warnings ) .report.ewms("The follow-up window start unit is not recognized!\n", "error", "CMA0", "AdhereR")
      return (NULL);
    }
    if( !is.na(followup.window.duration) && is.numeric(followup.window.duration) && followup.window.duration < 0 )
    {
      if( !suppress.warnings ) .report.ewms("The follow-up window duration must be a positive number of time units after the first event!\n", "error", "CMA0", "AdhereR")
      return (NULL);
    } else if( !is.na(followup.window.duration) && !is.numeric(followup.window.duration) && !(followup.window.duration %in% names(data)) )
    {
      if( !suppress.warnings ) .report.ewms("The follow-up window duration must be either a positive number or a valid column name in 'data'!\n", "error", "CMA0", "AdhereR")
      return (NULL);
    }
    if( !is.na(followup.window.duration.unit) && !(followup.window.duration.unit %in% c("days", "weeks", "months", "years") ) )
    {
      if( !suppress.warnings ) .report.ewms("The follow-up window duration unit is not recognized!\n", "error", "CMA0", "AdhereR")
      return (NULL);
    }
    # the observation window:
    if( !is.na(observation.window.start) && is.numeric(observation.window.start) && observation.window.start < 0 )
    {
      if( !suppress.warnings ) .report.ewms("The observation window start must be a positive number of time units after the first event!\n", "error", "CMA0", "AdhereR")
      return (NULL);
    } else if( !is.na(observation.window.start) && !inherits(observation.window.start,"Date") && !is.numeric(observation.window.start) && !(observation.window.start %in% names(data)) )
    {
      # See if it can be forced to a valid Date:
      if( !is.na(observation.window.start) && (is.character(observation.window.start) || is.factor(observation.window.start)) && !is.na(observation.window.start <- as.Date(observation.window.start, format=date.format, optional=TRUE)) )
      {
        # Ok, it was apparently successfully converted to Date: nothing else to do...
      } else
      {
        if( !suppress.warnings ) .report.ewms("The observation window start must be either a positive number, a Date, or a valid column name in 'data'!\n", "error", "CMA0", "AdhereR")
        return (NULL);
      }
    }
    if( !inherits(observation.window.start,"Date") && !is.na(observation.window.start.unit) && !(observation.window.start.unit %in% c("days", "weeks", "months", "years") ) )
    {
      if( !suppress.warnings ) .report.ewms("The observation window start unit is not recognized!\n", "error", "CMA0", "AdhereR")
      return (NULL);
    }
    if( !is.na(observation.window.duration) && is.numeric(observation.window.duration) && observation.window.duration < 0 )
    {
      if( !suppress.warnings ) .report.ewms("The observation window duration must be a positive number of time units after the first event!\n", "error", "CMA0", "AdhereR")
      return (NULL);
    } else if( !is.na(observation.window.duration) && !is.numeric(observation.window.duration) && !(observation.window.duration %in% names(data)) )
    {
      if( !suppress.warnings ) .report.ewms("The observation window duration must be either a positive number or a valid column name in 'data'!\n", "error", "CMA0", "AdhereR")
      return (NULL);
    }
    if( !is.na(observation.window.duration.unit) && !(observation.window.duration.unit %in% c("days", "weeks", "months", "years") ) )
    {
      if( !suppress.warnings ) .report.ewms("The observation window duration unit is not recognized!\n", "error", "CMA0", "AdhereR")
      return (NULL);
    }

    # Arguments that should not have been passed:
    if( !suppress.warnings && !is.null(arguments.that.should.not.be.defined) )
    {
      # Get the actual list of arguments (including in the ...); the first is the function's own name:
      args.list <- as.list(match.call(expand.dots = TRUE));
      args.mathing <- (names(arguments.that.should.not.be.defined) %in% names(args.list)[-1]);
      if( any(args.mathing) )
      {
        for( i in which(args.mathing) )
        {
          .report.ewms(paste0("Please note that '",args.list[[1]],"' overrides argument '",names(arguments.that.should.not.be.defined)[i],"' with value '",arguments.that.should.not.be.defined[i],"'!\n"), "warning", "CMA0", "AdhereR");
        }
      }
    }
  } else
  {
    return (NULL);
  }

  # Parse, check and evaluate the medication groups (if any):
  if( !is.null(medication.groups) )
  {
    if( is.null(mg <- .apply.medication.groups(medication.groups=medication.groups, data=data, suppress.warnings=suppress.warnings)) )
    {
      return (NULL);
    }
  } else
  {
    mg <- NULL;
  }

  structure(list("data"=data,
                 "ID.colname"=ID.colname,
                 "event.date.colname"=event.date.colname,
                 "event.duration.colname"=event.duration.colname,
                 "event.daily.dose.colname"=event.daily.dose.colname,
                 "medication.class.colname"=medication.class.colname,
                 "medication.groups"=mg,
                 "flatten.medication.groups"=flatten.medication.groups,
                 "medication.groups.colname"=medication.groups.colname,
                 "carryover.within.obs.window"=carryover.within.obs.window,
                 "carryover.into.obs.window"=carryover.into.obs.window,
                 "carry.only.for.same.medication"=carry.only.for.same.medication,
                 "consider.dosage.change"=consider.dosage.change,
                 "followup.window.start"=followup.window.start,
                 "followup.window.start.unit"=followup.window.start.unit,
                 "followup.window.start.per.medication.group"=followup.window.start.per.medication.group,
                 "followup.window.duration"=followup.window.duration,
                 "followup.window.duration.unit"=followup.window.duration.unit,
                 "observation.window.start"=observation.window.start,
                 "observation.window.start.unit"=observation.window.start.unit,
                 "observation.window.duration"=observation.window.duration,
                 "observation.window.duration.unit"=observation.window.duration.unit,
                 "date.format"=date.format,
                 "summary"=summary),
            class="CMA0");
}


#' Print CMA0 (and derived) objects.
#'
#' Prints and summarizes a basic CMA0, or derived, object.
#'
#' Can produce output for the console (text), R Markdown or LaTeX,
#' showing various types of information.
#'
#' @param x A \emph{\code{CMA0}} or derived object, representing the CMA to
#' print.
#' @param inline \emph{Logical}, should print inside a line of text or as a
#' separate, extended object?
#' @param format A \emph{string}, the type of output: plain text ("text";
#' default), LaTeX ("latex") or R Markdown ("markdown").
#' @param print.params \emph{Logical}, should print the parameters?
#' @param print.data \emph{Logical}, should print a summary of the data?
#' @param exclude.params A vector of \emph{strings}, the names of the object
#' fields to exclude from printing (usually, internal information irrelevant to
#' the end-user).
#' @param skip.header \emph{Logical}, should the header be printed?
#' @param cma.type A \emph{string}, used to override the reported object's class.
#' @param ... other possible parameters
#' @examples
#' cma0 <- CMA0(data=med.events,
#'              ID.colname="PATIENT_ID",
#'              event.date.colname="DATE",
#'              event.duration.colname="DURATION",
#'              event.daily.dose.colname="PERDAY",
#'              medication.class.colname="CATEGORY",
#'              followup.window.start=0,
#'              followup.window.start.unit="days",
#'              followup.window.duration=2*365,
#'              followup.window.duration.unit="days",
#'              observation.window.start=30,
#'              observation.window.start.unit="days",
#'              observation.window.duration=365,
#'              observation.window.duration.unit="days",
#'              date.format="%m/%d/%Y",
#'              summary="Base CMA");
#' cma0;
#' print(cma0, format="markdown");
#' cma1 <- CMA1(data=med.events,
#'              ID.colname="PATIENT_ID",
#'              event.date.colname="DATE",
#'              event.duration.colname="DURATION",
#'              followup.window.start=30,
#'              observation.window.start=30,
#'              observation.window.duration=365,
#'              date.format="%m/%d/%Y"
#'             );
#' cma1;
#' @export
print.CMA0 <- function(x,                                                  # the CMA0 (or derived) object
                       ...,                                                # required for S3 consistency
                       inline=FALSE,                                       # print inside a line of text or as a separate, extended object?
                       format=c("text", "latex", "markdown"),              # the format to print to
                       print.params=TRUE,                                  # show the parameters?
                       print.data=TRUE,                                    # show the summary of the data?
                       exclude.params=c("event.info", "real.obs.windows"), # if so, should I not print some?
                       skip.header=FALSE,                                  # should I print the generic header?
                       cma.type=class(cma)[1]
)
{
  cma <- x; # parameter x is required for S3 consistency, but I like cma more
  if( is.null(cma) || !inherits(cma, "CMA0") ) return (invisible(NULL));

  if( format[1] == "text" )
  {
    # Output text:
    if( !inline )
    {
      # Extended print:
      if( !skip.header ) cat(paste0(cma.type,":\n"));
      if( print.params )
      {
        params <- names(cma); params <- params[!(params %in% c("data",exclude.params))]; # exlude the 'data' (and any other requested) params from printing
        if( length(params) > 0 )
        {
          if( "summary" %in% params )
          {
            cat(paste0("  \"",cma$summary,"\"\n"));
            params <- params[!(params %in% "summary")];
          }
          cat("  [\n");
          for( p in params )
          {
            if( p == "CMA" )
            {
              cat(paste0("    ",p," = CMA results for ",nrow(cma[[p]])," patients\n"));
            } else if( p == "medication.groups" )
            {
              if( !is.null(cma[[p]]) )
              {
                cat(paste0("    ", p, " = ", nrow(cma[[p]]$defs), " [", ifelse(nrow(cma[[p]]$defs)<4, paste0("'",cma[[p]]$defs$name,"'", collapse=", "), paste0(paste0("'",cma[[p]]$defs$name[1:4],"'", collapse=", ")," ...")), "]\n"));
              } else
              {
                cat(paste0("    ", p, " = <NONE>\n"));
              }
            } else if( !is.null(cma[[p]]) && length(cma[[p]]) > 0 && !is.na(cma[[p]]) )
            {
              cat(paste0("    ",p," = ",cma[[p]],"\n"));
            }
          }
          cat("  ]\n");
        }
        if( print.data && !is.null(cma$data) )
        {
          # Data summary:
          cat(paste0("  DATA: ",nrow(cma$data)," (rows) x ",ncol(cma$data)," (columns)"," [",length(unique(cma$data[,cma$ID.colname]))," patients]",".\n"));
        }
      }
    } else
    {
      # Inline print:
      cat(paste0(cma$summary,ifelse(print.data && !is.null(cma$data),paste0(" (on ",nrow(cma$data)," rows x ",ncol(cma$data)," columns",", ",length(unique(cma$data[,cma$ID.colname]))," patients",")"),"")));
    }
  } else if( format[1] == "latex" )
  {
    # Output LaTeX: no difference between inline and not inline:
    cat(paste0("\\textbf{",cma$summary,"} (",cma.type,"):",
               ifelse(print.data && !is.null(cma$data),paste0(" (on ",nrow(cma$data)," rows x ",ncol(cma$data)," columns",", ",length(unique(cma$data[,cma$ID.colname]))," patients",")"),"")));
  } else if( format[1] == "markdown" )
  {
    # Output Markdown: no difference between inline and not inline:
    cat(paste0("**",cma$summary,"** (",cma.type,"):",
               ifelse(print.data && !is.null(cma$data),paste0(" (on ",nrow(cma$data)," rows x ",ncol(cma$data)," columns",", ",length(unique(cma$data[,cma$ID.colname]))," patients",")"),"")));
  } else
  {
    .report.ewms("Unknown format for printing!\n", "error", "print.CMA0", "AdhereR");
    return (invisible(NULL));
  }
}

#' Plot CMA0 objects.
#'
#' Plots the events (prescribing or dispensing) data encapsulated in a basic
#' CMA0 object.
#'
#' The x-axis represents time (either in days since the earliest date or as
#' actual dates), with consecutive events represented as ascending on the y-axis.
#'
#' Each event is represented as a segment with style \code{lty.event} and line
#' width \code{lwd.event} starting with a \code{pch.start.event} and ending with
#' a \code{pch.end.event} character, coloured with a unique color as given by
#' \code{col.cats}, extending from its start date until its end date.
#' Consecutive events are thus represented on consecutive levels of the y-axis
#' and are connected by a "continuation" line with \code{col.continuation}
#' colour, \code{lty.continuation} style and \code{lwd.continuation} width;
#' these continuation lines are purely visual guides helping to perceive the
#' sequence of events, and carry no information about the availability of
#' medication in this interval.
#'
#' When several patients are displayed on the same plot, they are organized
#' vertically, and alternating bands (white and gray) help distinguish
#' consecutive patients.
#' Implicitly, all patients contained in the \code{cma} object will be plotted,
#' but the \code{patients.to.plot} parameter allows the selection of a subset
#' of patients.
#'
#' @param x A \emph{\code{CMA0}} or derived object, representing the CMA to
#' plot
#' @param patients.to.plot A vector of \emph{strings} containing the list of
#' patient IDs to plot (a subset of those in the \code{cma} object), or
#' \code{NULL} for all
#' @param duration A \emph{number}, the total duration (in days) of the whole
#' period to plot; in \code{NA} it is automatically determined from the event
#' data such that the whole dataset fits.
#' @param align.all.patients \emph{Logical}, should all patients be aligned
#' (i.e., the actual dates are discarded and all plots are relative to the
#' earliest date)?
#' @param align.first.event.at.zero \emph{Logical}, should the first event be
#' placed at the origin of the time axis (at 0)?
#' @param show.period A \emph{string}, if "dates" show the actual dates at the
#' regular grid intervals, while for "days" (the default) shows the days since
#' the beginning; if \code{align.all.patients == TRUE}, \code{show.period} is
#' taken as "days".
#' @param period.in.days The \emph{number} of days at which the regular grid is
#' drawn (or 0 for no grid).
#' @param show.legend \emph{Logical}, should the legend be drawn?
#' @param legend.x The position of the legend on the x axis; can be "left",
#' "right" (default), or a \emph{numeric} value.
#' @param legend.y The position of the legend on the y axis; can be "bottom"
#' (default), "top", or a \emph{numeric} value.
#' @param legend.bkg.opacity A \emph{number} between 0.0 and 1.0 specifying the
#' opacity of the legend background.
#' @param cex,cex.axis,cex.lab,legend.cex,legend.cex.title \emph{numeric} values
#' specifying the cex of the various types of text.
#' @param xlab Named vector of x-axis labels to show for the two types of periods
#' ("days" and "dates"), or a single value for both, or \code{NULL} for nothing.
#' @param ylab Named vector of y-axis labels to show without and with CMA estimates,
#' or a single value for both, or \code{NULL} for nonthing.
#' @param title Named vector of titles to show for and without alignment, or a
#' single value for both, or \code{NULL} for nonthing.
#' @param col.cats A \emph{color} or a \emph{function} that specifies the single
#' colour or the colour palette used to plot the different medication; by
#' default \code{rainbow}, but we recommend, whenever possible, a
#' colorblind-friendly palette such as \code{viridis} or \code{colorblind_pal}.
#' @param unspecified.category.label A \emph{string} giving the name of the
#' unspecified (generic) medication category.
#' @param medication.groups.to.plot the names of the medication groups to plot or
#' \code{NULL} (the default) for all.
#' @param medication.groups.separator.show a \emph{boolean}, if \code{TRUE} (the
#' default) visually mark the medication groups the belong to the same patient,
#' using horizontal lines and alternating vertical lines.
#' @param medication.groups.separator.lty,medication.groups.separator.lwd,medication.groups.separator.color
#' graphical parameters (line type, line width and colour describing the visual
#' marking og medication groups as beloning to the same patient.
#' @param medication.groups.allother.label a \emph{string} giving the label to
#' use for the implicit \code{__ALL_OTHERS__} medication group (defaults to "*").
#' @param lty.event,lwd.event,pch.start.event,pch.end.event The style of the
#' event (line style, width, and start and end symbols).
#' @param plot.events.vertically.displaced Should consecutive events be plotted
#' on separate rows (i.e., separated vertically, the default) or on the same row?
#' @param print.dose \emph{Logical}, should the daily dose be printed as text?
#' @param cex.dose \emph{Numeric}, if daily dose is printed, what text size
#' to use?
#' @param print.dose.outline.col If \emph{\code{NA}}, don't print dose text with
#' outline, otherwise a color name/code for the outline.
#' @param print.dose.centered \emph{Logical}, print the daily dose centered on
#' the segment or slightly below it?
#' @param plot.dose \emph{Logical}, should the daily dose be indicated through
#' segment width?
#' @param lwd.event.max.dose \emph{Numeric}, the segment width corresponding to
#' the maximum daily dose (must be >= lwd.event but not too big either).
#' @param plot.dose.lwd.across.medication.classes \emph{Logical}, if \code{TRUE},
#' the line width of the even is scaled relative to all medication classes (i.e.,
#' relative to the global minimum and maximum doses), otherwise it is scale
#' relative only to its medication class.
#' @param col.continuation,lty.continuation,lwd.continuation The style of the
#' "continuation" lines connecting consecutive events (colour, line style and
#' width).
#' @param col.na The colour used for missing event data.
#' @param highlight.followup.window \emph{Logical}, should the follow-up window
#' be plotted?
#' @param followup.window.col The follow-up window's colour.
#' @param highlight.observation.window \emph{Logical}, should the observation
#' window be plotted?
#' @param observation.window.col,observation.window.density,observation.window.angle,observation.window.opacity
#' Attributes of the observation window (colour, shading density, angle and
#' opacity).
#' @param alternating.bands.cols The colors of the alternating vertical bands
#' distinguishing the patients; can be \code{NULL} = don't draw the bandes;
#' or a vector of colors.
#' @param bw.plot \emph{Logical}, should the plot use grayscale only (i.e., the
#' \code{\link[grDevices]{gray.colors}} function)?
#' @param force.draw.text \emph{Logical}, if \code{TRUE}, always draw text even
#' if too big or too small
#' @param min.plot.size.in.characters.horiz,min.plot.size.in.characters.vert
#' \emph{Numeric}, the minimum size of the plotting surface in characters;
#' horizontally (min.plot.size.in.characters.horiz) refers to the the whole
#' duration of the events to plot; vertically (min.plot.size.in.characters.vert)
#' refers to a single event. If the plotting is too small, possible solutions
#' might be: if within \code{RStudio}, try to enlarge the "Plots" panel, or
#' (also valid outside \code{RStudio} but not if using \code{RStudio server}
#' start a new plotting device (e.g., using \code{X11()}, \code{quartz()}
#' or \code{windows()}, depending on OS) or (works always) save to an image
#' (e.g., \code{jpeg(...); ...; dev.off()}) and display it in a viewer.
#' @param suppress.warnings \emph{Logical}: show or hide the warnings?
#' @param max.patients.to.plot \emph{Numeric}, the maximum patients to attempt
#' to plot.
#' @param export.formats a \emph{string} giving the formats to export the figure
#' to (by default \code{NULL}, meaning no exporting); can be any combination of
#' "svg" (just an \code{SVG} file), "html" (\code{SVG} + \code{HTML} + \code{CSS}
#' + \code{JavaScript}, all embedded within one \code{HTML} document), "jpg",
#' "png", "webp", "ps" or "pdf".
#' @param export.formats.fileprefix a \emph{string} giving the file name prefix
#' for the exported formats (defaults to "AdhereR-plot").
#' @param export.formats.height,export.formats.width \emph{numbers} giving the
#' desired dimensions (in pixels) for the exported figure (defaults to sane
#' values if \code{NA}).
#' @param export.formats.save.svg.placeholder a \emph{logical}, if TRUE, save an
#' image placeholder of type given by \code{export.formats.svg.placeholder.type}
#'for the \code{SVG} image.
#' @param export.formats.svg.placeholder.type a \emph{string}, giving the type of
#' placeholder for the \code{SVG} image to save; can be "jpg",
#' "png" (the default) or "webp".
#' @param export.formats.svg.placeholder.embed a \emph{logical}, if \code{TRUE},
#' embed the placeholder image in the HTML document (if any) using \code{base64}
#' encoding, otherwise (the default) leave it as an external image file (works
#' only when an \code{HTML} document is exported and only for \code{JPEG} or
#' \code{PNG} images.
#' @param export.formats.html.template,export.formats.html.javascript,export.formats.html.css
#' \emph{character strings} or \code{NULL} (the default) giving the path to the
#' \code{HTML}, \code{JavaScript} and \code{CSS} templates, respectively, to be
#' used when generating the HTML+CSS semi-interactive plots; when \code{NULL},
#' the default ones included with the package will be used. If you decide to define
#' new templates please use the default ones for inspiration and note that future
#' version are not guaranteed to be backwards compatible!
#' @param export.formats.directory a \emph{string}; if exporting, which directory
#' to export to; if \code{NA} (the default), creates the files in a temporary
#' directory.
#' @param generate.R.plot a \emph{logical}, if \code{TRUE} (the default),
#' generate the standard (base \code{R}) plot for plotting within \code{R}.
#' @param ... other possible parameters
#' @examples
#' cma0 <- CMA0(data=med.events,
#'              ID.colname="PATIENT_ID",
#'              event.date.colname="DATE",
#'              event.duration.colname="DURATION",
#'              event.daily.dose.colname="PERDAY",
#'              medication.class.colname="CATEGORY",
#'              followup.window.start=0,
#'              followup.window.start.unit="days",
#'              followup.window.duration=2*365,
#'              followup.window.duration.unit="days",
#'              observation.window.start=30,
#'              observation.window.start.unit="days",
#'              observation.window.duration=365,
#'              observation.window.duration.unit="days",
#'              date.format="%m/%d/%Y",
#'              summary="Base CMA");
#' plot(cma0, patients.to.plot=c("1","2"));
#' @export
plot.CMA0 <- function(x,                                     # the CMA0 (or derived) object
                      ...,                                   # required for S3 consistency
                      patients.to.plot=NULL,                 # list of patient IDs to plot or NULL for all
                      duration=NA,                           # duration to plot in days (if missing, determined from the data)
                      align.all.patients=FALSE, align.first.event.at.zero=TRUE, # should all patients be aligned? and, if so, place the first event as the horizontal 0?
                      show.period=c("dates","days")[2],      # draw vertical bars at regular interval as dates or days?
                      period.in.days=90,                     # the interval (in days) at which to draw veritcal lines
                      show.legend=TRUE, legend.x="right", legend.y="bottom", legend.bkg.opacity=0.5, legend.cex=0.75, legend.cex.title=1.0, # legend params and position
                      cex=1.0, cex.axis=0.75, cex.lab=1.0,   # various graphical params
                      xlab=c("dates"="Date", "days"="Days"), # Vector of x labels to show for the two types of periods, or a single value for both, or NULL for nothing
                      ylab=c("withoutCMA"="patient", "withCMA"="patient (& CMA)"), # Vector of y labels to show without and with CMA estimates, or a single value for both, or NULL ofr nonthing
                      title=c("aligned"="Event patterns (all patients aligned)", "notaligned"="Event patterns"), # Vector of titles to show for and without alignment, or a single value for both, or NULL for nonthing
                      col.cats=rainbow,                      # single color or a function mapping the categories to colors
                      unspecified.category.label="drug",     # the label of the unspecified category of medication
                      medication.groups.to.plot=NULL,        # the names of the medication groups to plot (by default, all)
                      medication.groups.separator.show=TRUE, medication.groups.separator.lty="solid", medication.groups.separator.lwd=2, medication.groups.separator.color="blue", # group medication events by patient?
                      medication.groups.allother.label="*",  # the label to use for the __ALL_OTHERS__ medication class (defaults to *)
                      lty.event="solid", lwd.event=2, pch.start.event=15, pch.end.event=16, # event style
                      plot.events.vertically.displaced=TRUE, # display the events on different lines (vertical displacement) or not (defaults to TRUE)?
                      print.dose=FALSE, cex.dose=0.75, print.dose.outline.col="white", print.dose.centered=FALSE, # print daily dose
                      plot.dose=FALSE, lwd.event.max.dose=8, plot.dose.lwd.across.medication.classes=FALSE, # draw daily dose as line width
                      col.continuation="black", lty.continuation="dotted", lwd.continuation=1, # style of the contuniation lines connecting consecutive events
                      col.na="lightgray",                    # color for missing data
                      highlight.followup.window=TRUE, followup.window.col="green",
                      highlight.observation.window=TRUE, observation.window.col="yellow", observation.window.density=35, observation.window.angle=-30, observation.window.opacity=0.3,
                      alternating.bands.cols=c("white", "gray95"), # the colors of the alternating vertical bands across patients (NULL=don't draw any; can be >= 1 color)
                      force.draw.text=FALSE,                 # if true, always draw text even if too big or too small
                      bw.plot=FALSE,                         # if TRUE, override all user-given colors and replace them with a scheme suitable for grayscale plotting
                      min.plot.size.in.characters.horiz=0, min.plot.size.in.characters.vert=0, # the minimum plot size (in characters: horizontally, for the whole duration, vertically, per event)
                      suppress.warnings=FALSE,               # suppress warnings?
                      max.patients.to.plot=100,              # maximum number of patients to plot
                      export.formats=NULL,                   # the formats to export the figure to (by default, none); can be any subset of "svg" (just SVG file), "html" (SVG + HTML + CSS + JavaScript all embedded within the HTML document), "jpg", "png", "webp", "ps" and "pdf"
                      export.formats.fileprefix="AdhereR-plot", # the file name prefix for the exported formats
                      export.formats.height=NA, export.formats.width=NA, # desired dimensions (in pixels) for the exported figure (defaults to sane values)
                      export.formats.save.svg.placeholder=TRUE,
                      export.formats.svg.placeholder.type=c("jpg", "png", "webp")[2],
                      export.formats.svg.placeholder.embed=FALSE, # save a placeholder for the SVG image?
                      export.formats.html.template=NULL, export.formats.html.javascript=NULL, export.formats.html.css=NULL, # HTML, JavaScript and CSS templates for exporting HTML+SVG
                      export.formats.directory=NA,           # if exporting, which directory to export to (if not give, creates files in the temporary directory)
                      generate.R.plot=TRUE                   # generate standard (base R) plot for plotting within R?
)
{
  .plot.CMAs(x,
             patients.to.plot=patients.to.plot,
             duration=duration,
             align.all.patients=align.all.patients,
             align.first.event.at.zero=align.first.event.at.zero,
             show.period=show.period,
             period.in.days=period.in.days,
             show.legend=show.legend,
             legend.x=legend.x,
             legend.y=legend.y,
             legend.bkg.opacity=legend.bkg.opacity,
             legend.cex=legend.cex,
             legend.cex.title=legend.cex.title,
             cex=cex,
             cex.axis=cex.axis,
             cex.lab=cex.lab,
             show.cma=FALSE, # for CMA0, there's no CMA to show by definition
             xlab=xlab,
             ylab=ylab,
             title=title,
             col.cats=col.cats,
             unspecified.category.label=unspecified.category.label,
             medication.groups.to.plot=medication.groups.to.plot,
             medication.groups.separator.show=medication.groups.separator.show,
             medication.groups.separator.lty=medication.groups.separator.lty,
             medication.groups.separator.lwd=medication.groups.separator.lwd,
             medication.groups.separator.color=medication.groups.separator.color,
             medication.groups.allother.label=medication.groups.allother.label,
             lty.event=lty.event,
             lwd.event=lwd.event,
             show.event.intervals=FALSE, # not for CMA0
             plot.events.vertically.displaced=plot.events.vertically.displaced,
             pch.start.event=pch.start.event,
             pch.end.event=pch.end.event,
             print.dose=print.dose,
             cex.dose=cex.dose,
             print.dose.outline.col=print.dose.outline.col,
             print.dose.centered=print.dose.centered,
             plot.dose=plot.dose,
             lwd.event.max.dose=lwd.event.max.dose,
             plot.dose.lwd.across.medication.classes=plot.dose.lwd.across.medication.classes,
             col.na=col.na,
             col.continuation=col.continuation,
             lty.continuation=lty.continuation,
             lwd.continuation=lwd.continuation,
             print.CMA=FALSE, # for CMA0, there's no CMA to show by definition
             plot.CMA=FALSE, # for CMA0, there's no CMA to show by definition
             plot.partial.CMAs.as=NULL, # for CMA0, there's no "partial" CMAs by definition
             highlight.followup.window=highlight.followup.window,
             followup.window.col=followup.window.col,
             highlight.observation.window=highlight.observation.window,
             observation.window.col=observation.window.col,
             observation.window.density=observation.window.density,
             observation.window.angle=observation.window.angle,
             observation.window.opacity=observation.window.opacity,
             alternating.bands.cols=alternating.bands.cols,
             bw.plot=bw.plot,
             force.draw.text=force.draw.text,
             min.plot.size.in.characters.horiz=min.plot.size.in.characters.horiz,
             min.plot.size.in.characters.vert=min.plot.size.in.characters.vert,
             max.patients.to.plot=max.patients.to.plot,
             export.formats=export.formats,
             export.formats.fileprefix=export.formats.fileprefix,
             export.formats.height=export.formats.height,
             export.formats.width=export.formats.width,
             export.formats.save.svg.placeholder=export.formats.save.svg.placeholder,
             export.formats.svg.placeholder.type=export.formats.svg.placeholder.type,
             export.formats.svg.placeholder.embed=export.formats.svg.placeholder.embed,
             export.formats.html.template=export.formats.html.template,
             export.formats.html.javascript=export.formats.html.javascript,
             export.formats.html.css=export.formats.html.css,
             export.formats.directory=export.formats.directory,
             generate.R.plot=generate.R.plot,
             suppress.warnings=suppress.warnings);
}


#' Access the medication groups of a CMA object.
#'
#' Retrieve the medication groups and the observations they refer to (if any).
#'
#' @param x a CMA object.
#' @return a \emph{list} with two members:
#' \itemize{
#'  \item \code{defs} A \code{data.frame} containing the names and definitions of
#'  the medication classes; please note that there is an extra class
#'  \emph{__ALL_OTHERS__} containing all the observations not selected by any of
#'  the explicitly given medication classes.
#'  \item \code{obs} A \code{logical matrix} where the columns are the medication
#'  classes (the last being \emph{__ALL_OTHERS__}), and the rows the observations in
#'  the x's data; element \eqn{(i,j)} is \code{TRUE} iff observation \eqn{j} was
#'  selected by medication class \eqn{i}.
#' }
#' @export
getMGs <- function(x) UseMethod("getMGs")
#' @export
getMGs.CMA0 <- function(x)
{
  cma <- x; # parameter x is required for S3 consistency, but I like cma more
  if( is.null(cma) || !inherits(cma, "CMA0") || is.null(cma$medication.groups) ) return (NULL);
  return (cma$medication.groups);
}


#' Access the actual CMA estimate from a CMA object.
#'
#' Retrieve the actual CMA estimate(s) encapsulated in a simple, per episode,
#' or sliding window CMA object.
#'
#' @param x a CMA object.
#' @param flatten.medication.groups \emph{Logical}, if \code{TRUE} and there are
#' medication groups defined, then the return value is flattened to a single
#' \code{data.frame} with an extra column containing the medication group (its
#' name is given by \code{medication.groups.colname}).
#' @param medication.groups.colname a \emph{string} (defaults to ".MED_GROUP_ID")
#' giving the name of the column storing the group name when
#' \code{flatten.medication.groups} is \code{TRUE}.
#' @return a \emph{data.frame} containing the CMA estimate(s).
#' @examples
#' cma1 <- CMA1(data=med.events,
#'              ID.colname="PATIENT_ID",
#'              event.date.colname="DATE",
#'              event.duration.colname="DURATION",
#'              followup.window.start=30,
#'              observation.window.start=30,
#'              observation.window.duration=365,
#'              date.format="%m/%d/%Y"
#'             );
#' getCMA(cma1);
#' \dontrun{
#' cmaE <- CMA_per_episode(CMA="CMA1",
#'                         data=med.events,
#'                         ID.colname="PATIENT_ID",
#'                         event.date.colname="DATE",
#'                         event.duration.colname="DURATION",
#'                         event.daily.dose.colname="PERDAY",
#'                         medication.class.colname="CATEGORY",
#'                         carry.only.for.same.medication=FALSE,
#'                         consider.dosage.change=FALSE,
#'                         followup.window.start=0,
#'                         observation.window.start=0,
#'                         observation.window.duration=365,
#'                         date.format="%m/%d/%Y"
#'                        );
#' getCMA(cmaE);}
#' @export
getCMA <- function(x, flatten.medication.groups=FALSE, medication.groups.colname=".MED_GROUP_ID") UseMethod("getCMA")
#' @export
getCMA.CMA0 <- function(x, flatten.medication.groups=FALSE, medication.groups.colname=".MED_GROUP_ID")
{
  cma <- x; # parameter x is required for S3 consistency, but I like cma more
  if( is.null(cma) || !inherits(cma, "CMA0") || !("CMA" %in% names(cma)) || is.null(cma$CMA) ) return (NULL);
  if( inherits(cma$CMA, "data.frame") || !flatten.medication.groups )
  {
    return (cma$CMA);
  } else
  {
    # Flatten the medication groups into a single data.frame:
    ret.val <- do.call(rbind, cma$CMA);
    if( is.null(ret.val) || nrow(ret.val) == 0 ) return (NULL);
    ret.val <- cbind(ret.val, unlist(lapply(1:length(cma$CMA), function(i) if(!is.null(cma$CMA[[i]])){rep(names(cma$CMA)[i], nrow(cma$CMA[[i]]))}else{NULL})));
    names(ret.val)[ncol(ret.val)] <- medication.groups.colname; rownames(ret.val) <- NULL;
    return (ret.val);
  }
}




#' Access the event info from a CMA object.
#'
#' Retrieve the event info encapsulated in a simple, per episode,
#' or sliding window CMA object.
#'
#' @param x a CMA object.
#' @param flatten.medication.groups \emph{Logical}, if \code{TRUE} and there are
#' medication groups defined, then the return value is flattened to a single
#' \code{data.frame} with an extra column containing the medication group (its
#' name is given by \code{medication.groups.colname}).
#' @param medication.groups.colname a \emph{string} (defaults to ".MED_GROUP_ID")
#' giving the name of the column storing the group name when
#' \code{flatten.medication.groups} is \code{TRUE}.
#' @return a \emph{data.frame} containing the CMA estimate(s).
#' @examples
#' cma1 <- CMA1(data=med.events,
#'              ID.colname="PATIENT_ID",
#'              event.date.colname="DATE",
#'              event.duration.colname="DURATION",
#'              followup.window.start=30,
#'              observation.window.start=30,
#'              observation.window.duration=365,
#'              date.format="%m/%d/%Y"
#'             );
#' getEventInfo(cma1);
#' @export
getEventInfo <- function(x, flatten.medication.groups=FALSE, medication.groups.colname=".MED_GROUP_ID") UseMethod("getEventInfo")
#' @export
getEventInfo.CMA0 <- function(x, flatten.medication.groups=FALSE, medication.groups.colname=".MED_GROUP_ID")
{
  cma <- x; # parameter x is required for S3 consistency, but I like cma more
  if( is.null(cma) || !inherits(cma, "CMA0") || !("event.info" %in% names(cma)) || is.null(cma$event.info) ) return (NULL);
  if( inherits(cma$event.info, "data.frame") || !flatten.medication.groups )
  {
    return (cma$event.info);
  } else
  {
    # Flatten the medication groups into a single data.frame:
    ret.val <- do.call(rbind, cma$event.info);
    if( is.null(ret.val) || nrow(ret.val) == 0 ) return (NULL);
    ret.val <- cbind(ret.val, unlist(lapply(1:length(cma$event.info), function(i) if(!is.null(cma$event.info[[i]])){rep(names(cma$event.info)[i], nrow(cma$event.info[[i]]))}else{NULL})));
    names(ret.val)[ncol(ret.val)] <- medication.groups.colname; rownames(ret.val) <- NULL;
    return (ret.val);
  }
}


#' Restrict a CMA object to a subset of patients.
#'
#' Restrict a CMA object to a subset of patients.
#'
#' @param cma a CMA object.
#' @param patients a list of patient IDs to keep.
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any
#' warnings.
#' @return a CMA object containing only the information for the given patients.
#' @examples
#' cma1 <- CMA1(data=med.events,
#'              ID.colname="PATIENT_ID",
#'              event.date.colname="DATE",
#'              event.duration.colname="DURATION",
#'              followup.window.start=30,
#'              observation.window.start=30,
#'              observation.window.duration=365,
#'              date.format="%m/%d/%Y"
#'             );
#' getCMA(cma1);
#' cma1a <- subsetCMA(cma1, patients=c(1:3,7));
#' cma1a; getCMA(cma1a);
#' @export
subsetCMA <- function(cma, patients, suppress.warnings) UseMethod("subsetCMA")
#' @export
subsetCMA.CMA0 <- function(cma, patients, suppress.warnings=FALSE)
{
  if( inherits(patients, "factor") ) patients <- as.character(patients);
  all.patients <- unique(cma$data[,cma$ID.colname]);
  patients.to.keep <- intersect(patients, all.patients);
  if( length(patients.to.keep) == length(all.patients) )
  {
    # Keep all patients:
    return (cma);
  }
  if( length(patients.to.keep) == 0 )
  {
    if( !suppress.warnings ) .report.ewms("No patients to subset on!\n", "error", "subsetCMA.CMA0", "AdhereR");
    return (NULL);
  }
  if( length(patients.to.keep) < length(patients) && !suppress.warnings ) .report.ewms("Some patients in the subsetting set are not in the CMA itself and are ignored!\n", "warning", "subsetCMA.CMA0", "AdhereR");

  ret.val <- cma;
  ret.val$data <- ret.val$data[ ret.val$data[,ret.val$ID.colname] %in% patients.to.keep, ];
  if( !is.null(ret.val$event.info) )
  {
    if( inherits(ret.val$event.info, "data.frame") )
    {
      ret.val$event.info <- ret.val$event.info[ ret.val$event.info[,ret.val$ID.colname] %in% patients.to.keep, ]; if( nrow(ret.val$event.info) == 0 ) ret.val$event.info <- NULL;
    } else if( is.list(ret.val$event.info) && length(ret.val$event.info) > 0 )
    {
      ret.val$event.info <- lapply(ret.val$event.info, function(x){tmp <- x[ x[,ret.val$ID.colname] %in% patients.to.keep, ]; if(!is.null(tmp) && nrow(tmp) > 0){tmp}else{NULL}});
    }
  }
  if( ("CMA" %in% names(ret.val)) && !is.null(ret.val$CMA) )
  {
    if( inherits(ret.val$CMA, "data.frame") )
    {
      ret.val$CMA <- ret.val$CMA[ ret.val$CMA[,ret.val$ID.colname] %in% patients.to.keep, ];
    } else if( is.list(ret.val$CMA) && length(ret.val$CMA) > 0 )
    {
      ret.val$CMA <- lapply(ret.val$CMA, function(x){tmp <- x[ x[,ret.val$ID.colname] %in% patients.to.keep, ]; if(!is.null(tmp) && nrow(tmp) > 0){tmp}else{NULL}});
    }
  }
  return (ret.val);
}


# Auxiliary function: add time units to date:
.add.time.interval.to.date <- function( start.date, # a Date object
                                        time.interval=0, # the number of "units" to add to the start.date (must be numeric and is rounded)
                                        unit="days", # can be "days", "weeks", "months", "years"
                                        suppress.warnings=FALSE
)
{
  # Checks
  if( !inherits(start.date,"Date") )
  {
    if( !suppress.warnings ) .report.ewms("Parameter start.date of .add.time.interval.to.date() must be a Date() object.\n", "error", ".add.time.interval.to.date", "AdhereR");
    return (NA);
  }
  if( !is.numeric(time.interval) )
  {
    if( inherits(time.interval, "difftime") ) # check if a difftime and, if so, attempt conversion to number of units
    {
      # Time difference:
      if( units(time.interval) == as.character(unit) )
      {
        time.interval <- as.numeric(time.interval);
      } else
      {
        # Try to convert it:
        time.interval <- as.numeric(time.interval, unit="days");
        time.interval <- switch( as.character(unit),
                                 "days"  = time.interval,
                                 "weeks" = time.interval/7,
                                 "months" = {if( !suppress.warnings ) .report.ewms("Converting to months assuming a 30-days month!", "warning", ".add.time.interval.to.date", "AdhereR"); time.interval/30;},
                                 "years"  = {if( !suppress.warnings ) .report.ewms("Converting to years assuming a 365-days year!", "warning", ".add.time.interval.to.date", "AdhereR"); time.interval/365;},
                                 {if( !suppress.warnings ) .report.ewms(paste0("Unknown unit '",unit,"' to '.add.time.interval.to.date'.\n"), "error", ".add.time.interval.to.date", "AdhereR"); return(NA);} # default
        );
      }
    } else
    {
      if( !suppress.warnings ) .report.ewms("Parameter start.date of .add.time.interval.to.date() must be a number.\n", "error", ".add.time.interval.to.date", "AdhereR");
      return (NA);
    }
  }

  # time.interval <- round(time.interval);
  # return (switch( as.character(unit),
  #                 "days"  = (start.date + time.interval),
  #                 "weeks" = (start.date + time.interval*7),
  #                 "months" = {tmp <- (start.date + months(time.interval));
  #                             i <- which(is.na(tmp));
  #                             if( length(i) > 0 ) tmp[i] <- start.date[i] + lubridate::days(1) + months(time.interval);
  #                             tmp;},
  #                 "years"  = {tmp <- (start.date + lubridate::years(time.interval));
  #                             i <- which(is.na(tmp));
  #                             if( length(i) > 0 ) tmp[i] <- start.date[i] + lubridate::days(1) + lubridate::years(time.interval);
  #                             tmp;},
  #                 {if( !suppress.warnings ) .report.ewms(paste0("Unknown unit '",unit,"' to '.add.time.interval.to.date'.\n"), "error", ".add.time.interval.to.date", "AdhereR"); NA;} # default
  # ));

  # Faster but assumes that the internal representation of "Date" object is in number of days since the begining of time (probably stably true):
  return (switch( as.character(unit),
                            "days"  = structure(unclass(start.date) + round(time.interval), class="Date"),
                            "weeks" = structure(unclass(start.date) + round(time.interval*7), class="Date"),
                            "months" = lubridate::add_with_rollback(start.date,
                                                                    lubridate::period(round(time.interval),"months"),
                                                                    roll_to_first=TRUE), # take care of cases such as 2001/01/29 + 1 month
                            "years"  = lubridate::add_with_rollback(start.date,
                                                                    lubridate::period(round(time.interval),"years"),
                                                                    roll_to_first=TRUE), # take care of cases such as 2000/02/29 + 1 year
                            {if( !suppress.warnings ) .report.ewms(paste0("Unknown unit '",unit,"' to '.add.time.interval.to.date'.\n"), "error", ".add.time.interval.to.date", "AdhereR"); NA;} # default
  ));
}

# Auxiliary function: subtract two dates to obtain the number of days in between:
# WARNING! Faster than difftime() but makes the assumption that the internal representation of Date objects is the number of days since a given beginning of time
# (true as of R 3.4 and probably conserved in future versions)
.difftime.Dates.as.days <- function( start.dates, end.dates, suppress.warnings=FALSE )
{
  # Checks
  if( !inherits(start.dates,"Date") || !inherits(end.dates,"Date") )
  {
    if( !suppress.warnings ) .report.ewms("start.dates and end.dates to '.difftime.Dates.as.days' must be a Date() objects.\n", "error", ".difftime.Dates.as.days", "AdhereR");
    return (NA);
  }
  return (unclass(start.dates) - unclass(end.dates)); # the difference between the raw internal representations of Date objects is in days
}


# Auxiliary function: call a given function sequentially or in parallel:
.compute.function <- function(fnc, # the function to compute
                              fnc.ret.vals=1, # how many distinct values (as elements in a list) does fnc return (really useful for binding results from multi-cpu processing)?
                              # Parallel processing:
                              parallel.backend=c("none","multicore","snow","snow(SOCK)","snow(MPI)","snow(NWS)")[1], # parallel backend to use
                              parallel.threads="auto", # specification (or number) of parallel threads
                              # The parameters with which to call the function:
                              # - NULL indicates that they are not used at all; the values, including defaults, must be fed by the caller
                              # - all consistency checks have been already been done in the caller (except for the parallel procssing params: these are checked here)
                              data=NULL, # this is a per-event *data.table* already keyed by patient ID and event date!
                              ID.colname=NULL, # the name of the column containing the unique patient ID
                              event.date.colname=NULL, # the start date of the event in the date.format format
                              event.duration.colname=NULL, # the event duration in days
                              event.daily.dose.colname=NULL, # the prescribed daily dose
                              medication.class.colname=NULL, # the classes/types/groups of medication
                              event.interval.colname=NULL, # contains number of days between the start of current event and the start of the next
                              gap.days.colname=NULL, # contains the number of days when medication was not available
                              carryover.within.obs.window=NULL, # if TRUE consider the carry-over within the observation window
                              carryover.into.obs.window=NULL, # if TRUE consider the carry-over from before the starting date of the observation window
                              carry.only.for.same.medication=NULL, # if TRUE the carry-over applies only across medication of same type
                              consider.dosage.change=NULL, # if TRUE carry-over is adjusted to reflect changes in dosage
                              followup.window.start=NULL, # if a number, date earliest event per participant + number of units, otherwise a date.format date or variable date
                              followup.window.start.unit=NULL, # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)
                              followup.window.duration=NULL, # the duration of the follow-up window in the time units given below
                              followup.window.duration.unit=NULL, # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)
                              observation.window.start=NULL, # the number of time units relative to followup.window.start, otherwise a date.format date or variable date
                              observation.window.start.unit=NULL, # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)
                              observation.window.duration=NULL, # the duration of the observation window in time units
                              observation.window.duration.unit=NULL, # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)
                              date.format=NULL, # the format of the dates used in this function
                              suppress.warnings=NULL
)
{
  # Quick decision for sequential processing:
  if( parallel.backend == "none" || (is.numeric(parallel.threads) && parallel.threads == 1) )
  {
    # Single threaded: simply call the function with the given data:
    return (fnc(data=data,
                ID.colname=ID.colname,
                event.date.colname=event.date.colname,
                event.duration.colname=event.duration.colname,
                event.daily.dose.colname=event.daily.dose.colname,
                medication.class.colname=medication.class.colname,
                event.interval.colname=event.interval.colname,
                gap.days.colname=gap.days.colname,
                carryover.within.obs.window=carryover.within.obs.window,
                carryover.into.obs.window=carryover.into.obs.window,
                carry.only.for.same.medication=carry.only.for.same.medication,
                consider.dosage.change=consider.dosage.change,
                followup.window.start=followup.window.start,
                followup.window.start.unit=followup.window.start.unit,
                followup.window.duration=followup.window.duration,
                followup.window.duration.unit=followup.window.duration.unit,
                observation.window.start=observation.window.start,
                observation.window.start.unit=observation.window.start.unit,
                observation.window.duration=observation.window.duration,
                observation.window.duration.unit=observation.window.duration.unit,
                date.format=date.format,
                suppress.warnings=suppress.warnings
           ));
  }


  # Could be Multicore:
  if( parallel.backend == "multicore" )
  {
    if( .Platform$OS.type == "windows" )
    {
      # Can't do multicore on Windows!
      if( !suppress.warnings ) .report.ewms(paste0("Parallel processing backend \"multicore\" is not currently supported on Windows: will use SNOW instead.\n"), "warning", ".compute.function", "AdhereR");
      parallel.backend <- "snow"; # try to do SNOW...
    } else
    {
      # Okay, seems we're on some sort of *NIX, so can do multicore!

      # Pre-process parallel.threads:
      if( parallel.threads == "auto" )
      {
        parallel.threads <- getOption("mc.cores", 2L);
      } else if( is.na(parallel.threads) || !is.numeric(parallel.threads) || (parallel.threads < 1) || (parallel.threads %% 1 != 0) )
      {
        if( !suppress.warnings ) .report.ewms(paste0("Number of parallel processing threads \"",parallel.threads,"\" must be either \"auto\" or a positive integer; forcing \"auto\".\n"), "warning", ".compute.function", "AdhereR");
        parallel.threads <- getOption("mc.cores", 2L);
      }

      # Pre-split the participants into a number of chunks equal to the number of threads to reduce paying the overheads multiple times
      # and call the function for each thread in parallel:
      patids <- unique(data[,get(ID.colname)]); # data is a data.table, so careful with column selection!
      if( length(patids) < 1 ) return (NULL);
      tmp <- parallel::mclapply( lapply(parallel::splitIndices(length(patids), min(parallel.threads, length(patids))), function(i) patids[i]), # simulate snow::clusterSplit() to split patients by thread
                                 function(IDs) fnc(data=data[list(IDs),], # call the function sequentially for the patients in the current chunk
                                                   ID.colname=ID.colname,
                                                   event.date.colname=event.date.colname,
                                                   event.duration.colname=event.duration.colname,
                                                   event.daily.dose.colname=event.daily.dose.colname,
                                                   medication.class.colname=medication.class.colname,
                                                   event.interval.colname=event.interval.colname,
                                                   gap.days.colname=gap.days.colname,
                                                   carryover.within.obs.window=carryover.within.obs.window,
                                                   carryover.into.obs.window=carryover.into.obs.window,
                                                   carry.only.for.same.medication=carry.only.for.same.medication,
                                                   consider.dosage.change=consider.dosage.change,
                                                   followup.window.start=followup.window.start,
                                                   followup.window.start.unit=followup.window.start.unit,
                                                   followup.window.duration=followup.window.duration,
                                                   followup.window.duration.unit=followup.window.duration.unit,
                                                   observation.window.start=observation.window.start,
                                                   observation.window.start.unit=observation.window.start.unit,
                                                   observation.window.duration=observation.window.duration,
                                                   observation.window.duration.unit=observation.window.duration.unit,
                                                   date.format=date.format,
                                                   suppress.warnings=suppress.warnings),
                                 mc.cores=parallel.threads, # as many cores as threads
                                 mc.preschedule=FALSE # don't preschedule as we know that we have relatively few jobs to do
                               );

      # Combine the results (there may be multiple returned data.frames intermingled!)
      if( fnc.ret.vals == 1 )
      {
        # Easy: just one!
        ret.val <- data.table::rbindlist(tmp);
      } else
      {
        # Combine them in turn:
        ret.val <- lapply(1:fnc.ret.vals, function(i)
        {
          x <- data.table::rbindlist(lapply(tmp, function(x) x[[i]]));
        });
        if( length(tmp) > 0 ) names(ret.val) <- names(tmp[[1]]);
      }
      return (ret.val);
    }
  }

  # Could be SNOW:
  cluster.type <- switch(parallel.backend,
                         "snow"=, "snow(SOCK)"="SOCK", # socket cluster
                         "snow(MPI)"="MPI",            # MPI cluster (required Rmpi)
                         "snow(NWS)"="NWS",            # NWS cluster (requires nws)
                         NA                            # unknown type of cluster
                        );
  if( is.na(cluster.type) )
  {
    if( !suppress.warnings ) .report.ewms(paste0("Unknown parallel processing backend \"",parallel.backend,"\": will force sequential (\"none\").\n"), "warning", ".compute.function", "AdhereR");
    # Force single threaded: simply call the function with the given data:
    return (fnc(data=data,
                ID.colname=ID.colname,
                event.date.colname=event.date.colname,
                event.duration.colname=event.duration.colname,
                event.daily.dose.colname=event.daily.dose.colname,
                medication.class.colname=medication.class.colname,
                event.interval.colname=event.interval.colname,
                gap.days.colname=gap.days.colname,
                carryover.within.obs.window=carryover.within.obs.window,
                carryover.into.obs.window=carryover.into.obs.window,
                carry.only.for.same.medication=carry.only.for.same.medication,
                consider.dosage.change=consider.dosage.change,
                followup.window.start=followup.window.start,
                followup.window.start.unit=followup.window.start.unit,
                followup.window.duration=followup.window.duration,
                followup.window.duration.unit=followup.window.duration.unit,
                observation.window.start=observation.window.start,
                observation.window.start.unit=observation.window.start.unit,
                observation.window.duration=observation.window.duration,
                observation.window.duration.unit=observation.window.duration.unit,
                date.format=date.format,
                suppress.warnings=suppress.warnings
           ));
  }

  patids <- unique(data[,get(ID.colname)]); # data is a data.table, so careful with column selection!
  if( length(patids) < 1 ) return (NULL);

  # Pre-process parallel.threads:
  if( length(parallel.threads) == 1 )
  {
    if( parallel.threads == "auto" )
    {
      parallel.threads <- 2L;
    } else if( is.na(parallel.threads) || (is.numeric(parallel.threads) && ((parallel.threads < 1) || (parallel.threads %% 1 != 0))) )
    {
      if( !suppress.warnings ) .report.ewms(paste0("Number of parallel processing threads \"",parallel.threads,"\", if numeric, must be either a positive integer; forcing \"auto\".\n"), "warning", ".compute.function", "AdhereR");
      parallel.threads <- 2L;
    }
    if( is.numeric(parallel.threads) ) parallel.threads <- min(parallel.threads, length(patids)); # make sure we're not starting more threads than patients
  }

  # Attempt to create the SNOW cluster:
  cluster <- parallel::makeCluster(parallel.threads, # process only "auto", otherwise trust makeCluster() to interpret the parameters
                                   type = cluster.type);
  if( is.null(cluster) )
  {
    if( !suppress.warnings ) .report.ewms(paste0("Failed to create the cluster \"",parallel.backend,"\" with parallel.threads \"",parallel.threads,"\": will force sequential (\"none\").\n"), "warning", ".compute.function", "AdhereR");
    # Force single threaded: simply call the function with the given data:
    return (fnc(data=data,
                ID.colname=ID.colname,
                event.date.colname=event.date.colname,
                event.duration.colname=event.duration.colname,
                event.daily.dose.colname=event.daily.dose.colname,
                medication.class.colname=medication.class.colname,
                event.interval.colname=event.interval.colname,
                gap.days.colname=gap.days.colname,
                carryover.within.obs.window=carryover.within.obs.window,
                carryover.into.obs.window=carryover.into.obs.window,
                carry.only.for.same.medication=carry.only.for.same.medication,
                consider.dosage.change=consider.dosage.change,
                followup.window.start=followup.window.start,
                followup.window.start.unit=followup.window.start.unit,
                followup.window.duration=followup.window.duration,
                followup.window.duration.unit=followup.window.duration.unit,
                observation.window.start=observation.window.start,
                observation.window.start.unit=observation.window.start.unit,
                observation.window.duration=observation.window.duration,
                observation.window.duration.unit=observation.window.duration.unit,
                date.format=date.format,
                suppress.warnings=suppress.warnings
           ));
  }

  # Pre-split the participants into a number of chunks equal to the number of created cluster nodes to reduce paying the overheads multiple times
  # and call the function for each cluster in parallel:
  tmp <- parallel::parLapply(cluster,
                             parallel::clusterSplit(cluster, patids),
                             function(IDs) fnc(data=data[list(IDs),], # call the function sequentially for the patients in the current chunk
                                               ID.colname=ID.colname,
                                               event.date.colname=event.date.colname,
                                               event.duration.colname=event.duration.colname,
                                               event.daily.dose.colname=event.daily.dose.colname,
                                               medication.class.colname=medication.class.colname,
                                               event.interval.colname=event.interval.colname,
                                               gap.days.colname=gap.days.colname,
                                               carryover.within.obs.window=carryover.within.obs.window,
                                               carryover.into.obs.window=carryover.into.obs.window,
                                               carry.only.for.same.medication=carry.only.for.same.medication,
                                               consider.dosage.change=consider.dosage.change,
                                               followup.window.start=followup.window.start,
                                               followup.window.start.unit=followup.window.start.unit,
                                               followup.window.duration=followup.window.duration,
                                               followup.window.duration.unit=followup.window.duration.unit,
                                               observation.window.start=observation.window.start,
                                               observation.window.start.unit=observation.window.start.unit,
                                               observation.window.duration=observation.window.duration,
                                               observation.window.duration.unit=observation.window.duration.unit,
                                               date.format=date.format,
                                               suppress.warnings=suppress.warnings));

  parallel::stopCluster(cluster); # stop the cluster

  # Combine the results (there may be multiple return data.frames intermingled!)
  if( fnc.ret.vals == 1 )
  {
    # Easy: just one!
    ret.val <- data.table::rbindlist(tmp);
  } else
  {
    # Combine them in turn:
    ret.val <- lapply(1:fnc.ret.vals, function(i)
    {
      x <- data.table::rbindlist(lapply(tmp, function(x) x[[i]]));
    });
    if( length(tmp) > 0 ) names(ret.val) <- names(tmp[[1]]);
  }
  return (ret.val);
}


#' Gap Days and Event (prescribing or dispensing) Intervals.
#'
#' For a given event (prescribing or dispensing) database, compute the gap days
#' and event intervals in various scenarious.
#'
#' This should in general not be called directly by the user, but is provided as
#' a basis for the extension to new CMAs.
#'
#' @param data A \emph{\code{data.frame}} containing the events used to
#' compute the CMA. Must contain, at a minimum, the patient unique ID, the event
#' date and duration, and might also contain the daily dosage and medication
#' type (the actual column names are defined in the following four parameters);
#' the \code{CMA} constructors call this parameter \code{data}.
#' @param ID.colname A \emph{string}, the name of the column in \code{data}
#' containing the unique patient ID; must be present.
#' @param event.date.colname A \emph{string}, the name of the column in
#' \code{data} containing the start date of the event (in the format given in
#' the \code{date.format} parameter); must be present.
#' @param event.duration.colname A \emph{string}, the name of the column in
#' \code{data} containing the event duration (in days); must be present.
#' @param event.daily.dose.colname A \emph{string}, the name of the column in
#' \code{data} containing the prescribed daily dose, or \code{NA} if not defined.
#' @param medication.class.colname A \emph{string}, the name of the column in
#' \code{data} containing the classes/types/groups of medication, or \code{NA}
#' if not defined.
#' @param event.interval.colname A \emph{string}, the name of a newly-created
#' column storing the number of days between the start of the current event and
#' the start of the next one; the default value "event.interval" should be
#' changed only if there is a naming conflict with a pre-existing
#' "event.interval" column in \code{event.info}.
#' @param gap.days.colname A \emph{string}, the name of a newly-created column
#' storing the number of days when medication was not available (i.e., the
#' "gap days"); the default value "gap.days" should be changed only if there is
#' a naming conflict with a pre-existing "gap.days" column in \code{event.info}.
#' @param carryover.within.obs.window \emph{Logical}, if \code{TRUE} consider
#' the carry-over within the observation window, or \code{NA} if not defined.
#' @param carryover.into.obs.window \emph{Logical}, if \code{TRUE} consider the
#' carry-over from before the starting date of the observation window, or
#' \code{NA} if not defined.
#' @param carry.only.for.same.medication \emph{Logical}, if \code{TRUE} the
#' carry-over applies only across medication of the same type, or \code{NA}
#' if not defined.
#' @param consider.dosage.change \emph{Logical}, if \code{TRUE} the carry-over
#' is adjusted to reflect changes in dosage, or \code{NA} if not defined.
#' @param followup.window.start If a \emph{\code{Date}} object, it represents
#' the actual start date of the follow-up window; if a \emph{string} it is the
#' name of the column in \code{data} containing the start date of the follow-up
#' window either as the numbers of \code{followup.window.start.unit} units after
#' the first event (the column must be of type \code{numeric}) or as actual
#' dates (in which case the column must be of type \code{Date}); if a
#' \emph{number} it is the number of time units defined in the
#' \code{followup.window.start.unit} parameter after the begin of the
#' participant's first event.
#' @param followup.window.start.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.start} refers to (when a number), or
#' \code{NA} if not defined.
#' @param followup.window.duration either a \emph{number} representing the
#' duration of the follow-up window in the time units given in
#' \code{followup.window.duration.unit}, or a \emph{string} giving the column
#' containing these numbers. Should represent a period for which relevant
#' medication events are recorded accurately (e.g. not extend after end of
#' relevant treatment, loss-to-follow-up or change to a health care provider
#' not covered by the database).
#' @param followup.window.duration.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.duration} refers to, or \code{NA} if not
#' defined.
#' @param observation.window.start,observation.window.start.unit,observation.window.duration,observation.window.duration.unit the definition of the observation window
#' (see the follow-up window parameters above for details).
#' @param date.format A \emph{string} giving the format of the dates used in the
#' \code{data} and the other parameters; see the \code{format} parameters of the
#' \code{\link[base]{as.Date}} function for details (NB, this concerns only the
#' dates given as strings and not as \code{Date} objects).
#' @param keep.window.start.end.dates \emph{Logical}, should the computed start
#' and end dates of the windows be kept?
#' @param keep.event.interval.for.all.events \emph{Logical}, should the computed
#' event intervals be kept for all events, or \code{NA}'ed for those outside the
#' OW?
#' @param remove.events.outside.followup.window \emph{Logical}, should the
#' events that fall outside the follo-wup window be removed from the results?
#' @param parallel.backend Can be "none" (the default) for single-threaded
#' execution, "multicore"  (using \code{mclapply} in package \code{parallel})
#' for multicore processing (NB. not currently implemented on MS Windows and
#' automatically falls back on "snow" on this platform),  or "snow",
#' "snow(SOCK)" (equivalent to "snow"), "snow(MPI)" or "snow(NWS)" specifying
#' various types of SNOW clusters (can be on the local machine or more complex
#' setups -- please see the documentation of package \code{snow} for details;
#' the last two require packages \code{Rmpi} and \code{nws}, respectively, not
#' automatically installed with \code{AdhereR}).
#' @param parallel.threads Can be "auto" (for \code{parallel.backend} ==
#' "multicore", defaults to the number of cores in the system as given by
#' \code{options("cores")}, while for \code{parallel.backend} == "snow",
#' defaults to 2), a strictly positive integer specifying the number of parallel
#' threads, or a more complex specification of the SNOW cluster nodes for
#' \code{parallel.backend} == "snow" (see the documentation of package
#' \code{snow} for details).
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any
#' warnings.
#' @param return.data.table \emph{Logical}, if \code{TRUE} return a
#' \code{data.table} object, otherwise a \code{data.frame}.
#' @param ... extra arguments.
#' @return A \code{data.frame} or \code{data.table} extending the
#' \code{event.info} parameter with:
#' \itemize{
#'  \item \code{event.interval} Or any other name given in
#'  \code{event.interval.colname}, containing the number of days between the
#'  start of the current event and the start of the next one.
#'  \item \code{gap.days} Or any other name given in \code{gap.days.colname},
#'  containing the number of days when medication was not available for the
#'  current event (i.e., the "gap days").
#'  \item \code{.FU.START.DATE,.FU.END.DATE} if kept, the actual start and end
#'  dates of the follow-up window (after adjustments due to the various
#'  parameters).
#'  \item \code{.OBS.START.DATE,.OBS.END.DATE} if kept, the actual start and end
#'  dates of the observation window (after adjustments due to the various
#'  parameters).
#'  \item \code{.EVENT.STARTS.BEFORE.OBS.WINDOW} if kept, \code{TRUE} if the
#'  current event starts before the start of the observation window.
#'  \item \code{.TDIFF1,.TDIFF2} if kept, various auxiliary time differences
#'  (in days).
#'  \item \code{.EVENT.STARTS.AFTER.OBS.WINDOW} if kept, \code{TRUE} if the
#'  current event starts after the end of the observation window.
#'  \item \code{.CARRY.OVER.FROM.BEFORE} if kept, the carry-over (if any) from
#'  the previous events.
#'  \item \code{.EVENT.WITHIN.FU.WINDOW} if kept, \code{TRUE} if the current
#'  event is within the follow-up window.
#' }
#' @export
compute.event.int.gaps <- function(data, # this is a per-event data.frame with columns:
                                   ID.colname=NA, # the name of the column containing the unique patient ID
                                   event.date.colname=NA, # the start date of the event in the date.format format
                                   event.duration.colname=NA, # the event duration in days
                                   event.daily.dose.colname=NA, # the prescribed daily dose
                                   medication.class.colname=NA, # the classes/types/groups of medication
                                   # The description of the output (added) columns:
                                   event.interval.colname="event.interval", # contains number of days between the start of current event and the start of the next
                                   gap.days.colname="gap.days", # contains the number of days when medication was not available
                                   # Various types methods of computing gaps:
                                   carryover.within.obs.window=FALSE, # if TRUE consider the carry-over within the observation window
                                   carryover.into.obs.window=FALSE, # if TRUE consider the carry-over from before the starting date of the observation window
                                   carry.only.for.same.medication=FALSE, # if TRUE the carry-over applies only across medication of same type
                                   consider.dosage.change=FALSE, # if TRUE carry-over is adjusted to reflect changes in dosage
                                   # The follow-up window:
                                   followup.window.start=0, # if a number, date earliest event per participant + number of units, otherwise a date.format date or variable date
                                   followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)
                                   followup.window.duration=365*2, # the duration of the follow-up window in the time units given below
                                   followup.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)
                                   # The observation window (embedded in the follow-up window):
                                   observation.window.start=0, # the number of time units relative to followup.window.start, otherwise a date.format date or variable date
                                   observation.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)
                                   observation.window.duration=365*2, # the duration of the observation window in time units
                                   observation.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)
                                   # Date format:
                                   date.format="%m/%d/%Y", # the format of the dates used in this function
                                   # Keep the follow-up and observation window start and end dates?
                                   keep.window.start.end.dates=FALSE,
                                   remove.events.outside.followup.window=TRUE, # remove events outside the follow-up window?
                                   keep.event.interval.for.all.events=FALSE, # keep the event.interval estimates for all events
                                   # Parallel processing:
                                   parallel.backend=c("none","multicore","snow","snow(SOCK)","snow(MPI)","snow(NWS)")[1], # parallel backend to use
                                   parallel.threads="auto", # specification (or number) of parallel threads
                                   # Misc:
                                   suppress.warnings=FALSE,
                                   return.data.table=FALSE,  # should the result be converted to data.frame (default) or returned as a data.table (keyed by patient ID and event date)?
                                   ... # other stuff
)
{
  # preconditions concerning column names:
  if( is.null(data) || !inherits(data,"data.frame") || nrow(data) < 1 )
  {
    if( !suppress.warnings ) .report.ewms("Event data must be a non-empty data frame!\n", "error", "compute.event.int.gaps", "AdhereR")
    return (NULL);
  }
  data.names <- names(data); # cache names(data) as it is used a lot
  if( is.null(ID.colname) || is.na(ID.colname) ||                                           # avoid empty stuff
      !(is.character(ID.colname) ||                                                         # it must be a character...
        (is.factor(ID.colname) && is.character(ID.colname <- as.character(ID.colname)))) || # ...or a factor (forced to character)
      length(ID.colname) != 1 ||                                                            # make sure it's a single value
      !(ID.colname %in% data.names)                                                         # make sure it's a valid column name
      )
  {
    if( !suppress.warnings ) .report.ewms(paste0("The patient ID column \"",ID.colname,"\" cannot be empty, must be a single value, and must be present in the event data!\n"), "error", "compute.event.int.gaps", "AdhereR")
    return (NULL);
  }
  if( is.null(event.date.colname) || is.na(event.date.colname) ||                                                   # avoid empty stuff
      !(is.character(event.date.colname) ||                                                                         # it must be a character...
        (is.factor(event.date.colname) && is.character(event.date.colname <- as.character(event.date.colname)))) || # ...or a factor (forced to character)
      length(event.date.colname) != 1 ||                                                                            # make sure it's a single value
      !(event.date.colname %in% data.names)                                                                         # make sure it's a valid column name
      )
  {
    if( !suppress.warnings ) .report.ewms(paste0("The event date column \"",event.date.colname,"\" cannot be empty, must be a single value, and must be present in the event data!\n"), "error", "compute.event.int.gaps", "AdhereR")
    return (NULL);
  }
  if( is.null(event.duration.colname) || is.na(event.duration.colname) ||                                                       # avoid empty stuff
      !(is.character(event.duration.colname) ||                                                                                 # it must be a character...
        (is.factor(event.duration.colname) && is.character(event.duration.colname <- as.character(event.duration.colname)))) || # ...or a factor (forced to character)
      length(event.duration.colname) != 1 ||                                                                                    # make sure it's a single value
      !(event.duration.colname %in% data.names)                                                                                 # make sure it's a valid column name
      )
  {
    if( !suppress.warnings ) .report.ewms(paste0("The event duration column \"",event.duration.colname,"\" cannot be empty, must be a single value, and must be present in the event data!\n"), "error", "compute.event.int.gaps", "AdhereR")
    return (NULL);
  }
  if( (!is.null(event.daily.dose.colname) && !is.na(event.daily.dose.colname)) &&                                                      # if actually given:
      (!(is.character(event.daily.dose.colname) ||                                                                                     # it must be a character...
         (is.factor(event.daily.dose.colname) && is.character(event.daily.dose.colname <- as.character(event.daily.dose.colname)))) || # ...or a factor (forced to character)
       length(event.daily.dose.colname) != 1 ||                                                                                        # make sure it's a single value
       !(event.daily.dose.colname %in% data.names))                                                                                    # make sure it's a valid column name
      )
  {
    if( !suppress.warnings ) .report.ewms(paste0("If given, the event daily dose column \"",event.daily.dose.colname,"\" must be a single value and must be present in the event data!\n"), "error", "compute.event.int.gaps", "AdhereR")
    return (NULL);
  }
  if( (!is.null(medication.class.colname) && !is.na(medication.class.colname)) &&                                                      # if actually given:
      (!(is.character(medication.class.colname) ||                                                                                     # it must be a character...
         (is.factor(medication.class.colname) && is.character(medication.class.colname <- as.character(medication.class.colname)))) || # ...or a factor (forced to character)
       length(medication.class.colname) != 1 ||                                                                                        # make sure it's a single value
       !(medication.class.colname %in% data.names))                                                                                    # make sure it's a valid column name
      )
  {
    if( !suppress.warnings ) .report.ewms(paste0("If given, the event type column \"",medication.class.colname,"\" must be a single value and must be present in the event data!\n"), "error", "compute.event.int.gaps", "AdhereR")
    return (NULL);
  }

  # preconditions concerning carry-over:
  if( !is.logical(carryover.within.obs.window)    || is.na(carryover.within.obs.window)    || length(carryover.within.obs.window) != 1    ||
      !is.logical(carryover.into.obs.window)      || is.na(carryover.into.obs.window)      || length(carryover.into.obs.window) != 1      ||
      !is.logical(carry.only.for.same.medication) || is.na(carry.only.for.same.medication) || length(carry.only.for.same.medication) != 1 )
  {
    if( !suppress.warnings ) .report.ewms("Carry over arguments must be single value logicals!\n", "error", "compute.event.int.gaps", "AdhereR")
    return (NULL);
  }
  if( !carryover.within.obs.window && !carryover.into.obs.window && carry.only.for.same.medication )
  {
    if( !suppress.warnings ) .report.ewms("Cannot carry over only for same medication when no carry over at all is considered!\n", "error", "compute.event.int.gaps", "AdhereR")
    return (NULL);
  }

  # preconditions concerning dosage change:
  if( !is.logical(consider.dosage.change) || is.na(consider.dosage.change) || length(consider.dosage.change) != 1 )
  {
    if( !suppress.warnings ) .report.ewms("Consider dosage change must be single value logical!\n", "error", "compute.event.int.gaps", "AdhereR")
    return (NULL);
  }

  # preconditions concerning follow-up window (as all violations result in the same error, aggregate them in a single if):
  if( (is.null(followup.window.start) || is.na(followup.window.start) || length(followup.window.start) != 1) ||                   # cannot be missing or have more than one values
      (!inherits(followup.window.start,"Date") && !is.numeric(followup.window.start) &&                                           # not a Date or number:
          (!(is.character(followup.window.start) ||                                                                               # it must be a character...
             (is.factor(followup.window.start) && is.character(followup.window.start <- as.character(followup.window.start)))) || # ...or a factor (forced to character)
           !(followup.window.start %in% data.names))) )                                                                           # make sure it's a valid column name
  {
    if( !suppress.warnings ) .report.ewms("The follow-up window start must be a single value, either a number, a Date object, or a string giving a column name in the data!\n", "error", "compute.event.int.gaps", "AdhereR")
    return (NULL);
  }
  if( is.null(followup.window.start.unit) ||
      (is.na(followup.window.start.unit) && !(is.factor(followup.window.start) || is.character(followup.window.start))) ||
      length(followup.window.start.unit) != 1 ||
      ((is.factor(followup.window.start.unit) || is.character(followup.window.start.unit)) && !(followup.window.start.unit %in% c("days", "weeks", "months", "years"))) )
  {
    if( !suppress.warnings ) .report.ewms("The follow-up window start unit must be a single value, one of \"days\", \"weeks\", \"months\" or \"years\"!\n", "error", "compute.event.int.gaps", "AdhereR")
    return (NULL);
  }
  if( is.numeric(followup.window.duration) && (followup.window.duration <= 0 || length(followup.window.duration) != 1) ||               # cannot be missing or have more than one values
      (!is.numeric(followup.window.duration) &&
       (!(is.character(followup.window.duration) ||                                                                                     # it must be a character...
          (is.factor(followup.window.duration) && is.character(followup.window.duration <- as.character(followup.window.duration)))) || # ...or a factor (forced to character)
        !(followup.window.duration %in% data.names))))                                                                                  # make sure it's a valid column name
  {
    if( !suppress.warnings ) .report.ewms("The follow-up window duration must be a single value, either a positive number, or a string giving a column name in the data!\n", "error", "compute.event.int.gaps", "AdhereR")
    return (NULL);
  }
  if( is.null(followup.window.duration.unit) || is.na(followup.window.duration.unit) ||
      length(followup.window.duration.unit) != 1 ||
      !(followup.window.duration.unit %in% c("days", "weeks", "months", "years") ) )
  {
    if( !suppress.warnings ) .report.ewms("The follow-up window duration unit must be a single value, one of \"days\", \"weeks\", \"months\" or \"years\"!\n", "error", "compute.event.int.gaps", "AdhereR")
    return (NULL);
  }

  # preconditions concerning observation window (as all violations result in the same error, aggregate them in a single if):
  if( (is.null(observation.window.start) || is.na(observation.window.start) || length(observation.window.start) != 1) ||                   # cannot be missing or have more than one values
      (is.numeric(observation.window.start) && (observation.window.start < 0)) ||                                                          # if a number, must be a single positive one
      (!inherits(observation.window.start,"Date") && !is.numeric(observation.window.start) &&                                              # not a Date or number:
          (!(is.character(observation.window.start) ||                                                                                     # it must be a character...
             (is.factor(observation.window.start) && is.character(observation.window.start <- as.character(observation.window.start)))) || # ...or a factor (forced to character)
           !(observation.window.start %in% data.names))) )                                                                                 # make sure it's a valid column name
  {
    if( !suppress.warnings ) .report.ewms("The observation window start must be a single value, either a positive number, a Date object, or a string giving a column name in the data!\n", "error", "compute.event.int.gaps", "AdhereR")
    return (NULL);
  }
  if( is.null(observation.window.start.unit) ||
      (is.na(observation.window.start.unit) && !(is.factor(observation.window.start) || is.character(observation.window.start))) ||
      length(observation.window.start.unit) != 1 ||
      ((is.factor(observation.window.start.unit) || is.character(observation.window.start.unit)) && !(observation.window.start.unit %in% c("days", "weeks", "months", "years"))) )
  {
    if( !suppress.warnings ) .report.ewms("The observation window start unit must be a single value, one of \"days\", \"weeks\", \"months\" or \"years\"!\n", "error", "compute.event.int.gaps", "AdhereR")
    return (NULL);
  }
  if( is.numeric(observation.window.duration) && (observation.window.duration <= 0 || length(observation.window.duration) != 1) ||               # cannot be missing or have more than one values
      (!is.numeric(observation.window.duration) &&
       (!(is.character(observation.window.duration) ||                                                                                           # it must be a character...
          (is.factor(observation.window.duration) && is.character(observation.window.duration <- as.character(observation.window.duration)))) || # ...or a factor (forced to character)
        !(observation.window.duration %in% data.names))))                                                                                        # make sure it's a valid column name
  {
    if( !suppress.warnings ) .report.ewms("The observation window duration must be a single value, either a positive number, or a string giving a column name in the data!\n", "error", "compute.event.int.gaps", "AdhereR")
    return (NULL);
  }
  if( is.null(observation.window.duration.unit) || is.na(observation.window.duration.unit) ||
      length(observation.window.duration.unit) != 1 ||
      !(observation.window.duration.unit %in% c("days", "weeks", "months", "years") ) )
  {
    if( !suppress.warnings ) .report.ewms("The observation window duration unit must be a single value, one of \"days\", \"weeks\", \"months\" or \"years\"!\n", "error", "compute.event.int.gaps", "AdhereR")
    return (NULL);
  }

  # Check the patient IDs:
  if( anyNA(data[,ID.colname]) )
  {
    if( !suppress.warnings ) .report.ewms(paste0("The patient unique identifiers in the \"",ID.colname,"\" column must not contain NAs; the first occurs on row ",min(which(is.na(data[,ID.colname]))),"!\n"), "error", "compute.event.int.gaps", "AdhereR");
    return (NULL);
  }

  # Check the date format (and save the conversion to Date() for later use):
  if( is.na(date.format) || is.null(date.format) || length(date.format) != 1 || !is.character(date.format) )
  {
    if( !suppress.warnings ) .report.ewms(paste0("The date format must be a single string!\n"), "error", "compute.event.int.gaps", "AdhereR");
    return (NULL);
  }
  if( anyNA(Date.converted.to.DATE <- as.Date(data[,event.date.colname],format=date.format)) )
  {
    if( !suppress.warnings ) .report.ewms(paste0("Not all entries in the event date \"",event.date.colname,"\" column are valid dates or conform to the date format \"",date.format,"\"; first issue occurs on row ",min(which(is.na(Date.converted.to.DATE))),"!\n"), "error", "compute.event.int.gaps", "AdhereR");
    return (NULL);
  }

  # Check the duration:
  tmp <- data[,event.duration.colname]; # caching for speed
  if( !is.numeric(tmp) || any(is.na(tmp) | tmp <= 0) )
  {
    if( !suppress.warnings ) .report.ewms(paste0("The event durations in the \"",event.duration.colname,"\" column must be non-missing strictly positive numbers!\n"), "error", "compute.event.int.gaps", "AdhereR");
    return (NULL);
  }

  # Check the event daily dose:
  if( !is.na(event.daily.dose.colname) && !is.null(event.daily.dose.colname) &&             # if actually given:
      (!is.numeric(tmp <- data[,event.daily.dose.colname]) || any(is.na(tmp) | tmp <= 0)) ) # must be a non-missing strictly positive number (and cache it for speed)
  {
    if( !suppress.warnings ) .report.ewms(paste0("If given, the event daily dose in the \"",event.daily.dose.colname,"\" column must be a non-missing strictly positive numbers!\n"), "error", "compute.event.int.gaps", "AdhereR");
    return (NULL);
  }

  # Check the newly created columns:
  if( is.na(event.interval.colname) || is.null(event.interval.colname) || !is.character(event.interval.colname) || (event.interval.colname %in% data.names) )
  {
    if( !suppress.warnings ) .report.ewms(paste0("The column name where the event interval will be stored \"",event.interval.colname,"\" cannot be missing nor already present in the event data!\n"), "error", "compute.event.int.gaps", "AdhereR");
    return (NULL);
  }
  if( is.na(gap.days.colname) || is.null(gap.days.colname) || !is.character(gap.days.colname) || (gap.days.colname %in% data.names) )
  {
    if( !suppress.warnings ) .report.ewms(paste0("The column name where the gap days will be stored \"",gap.days.colname,"\" cannot be mising nor already present in the event data.\n"), "error", "compute.event.int.gaps", "AdhereR");
    return (NULL);
  }

  # Make a data.table copy of data so we can alter it without altering the original input data:
  ret.val <- data.table(data);

  event.date2.colname <- ".DATE.as.Date"; # name of column caching the event dates
  ret.val[,c(event.interval.colname,            # output column event.interval.colname
             gap.days.colname,                  # output column gap.days.colname
             ".DATE.as.Date",                   # cache column .DATE.as.Date
             ".FU.START.DATE",                  # cache FUW start date
             ".FU.END.DATE",                    # cache FUW end date
             ".OBS.START.DATE",                 # cache OW start date
             ".OBS.END.DATE",                   # cache OW end date
             ".OBS.WITHIN.FU",                  # cache if the OW falls within the FUW
             ".EVENT.STARTS.BEFORE.OBS.WINDOW", # cache if the event starts before the OW begins
             ".EVENT.STARTS.AFTER.OBS.WINDOW",  # cache if the event starts after the OW ends
             ".EVENT.WITHIN.FU.WINDOW",         # cache if the event is within the FUW
             ".TDIFF1",                         # cache time difference betwen the event duration and (OW start - event start)
             ".TDIFF2",                         # cache time difference betwen the next event start date and the current event start date
             ".CARRY.OVER.FROM.BEFORE"          # cache if there is carry over from before the current event
            ) :=
            list(NA_real_,    # event.interval.colname: initially NA (numeric)
                 NA_real_,    # gap.days.colname: initially NA (numeric)
                 Date.converted.to.DATE, # .DATE.as.Date: convert event.date.colname from formatted string to Date (speeding calendar computations)
                 as.Date(NA), # .FU.START.DATE: initially NA (of type Date)
                 as.Date(NA), # .FU.END.DATE: initially NA (of type Date)
                 as.Date(NA), # .OBS.START.DATE: initially NA (of type Date)
                 as.Date(NA), # .OBS.END.DATE: initially NA (of type Date)
                 NA,          # .OBS.WITHIN.FU: initially NA (logical)
                 NA,          # .EVENT.STARTS.BEFORE.OBS.WINDOW: initially NA (logical)
                 NA,          # .EVENT.STARTS.AFTER.OBS.WINDOW: initially NA (logical)
                 NA,          # .EVENT.WITHIN.FU.WINDOW: initially NA (logical)
                 NA_real_,    # .TDIFF1: initially NA (numeric)
                 NA_real_,    # .TDIFF2: initially NA (numeric)
                 NA_real_     # .CARRY.OVER.FROM.BEFORE: initially NA (numeric)
            )];

  # Cache types of followup.window.start and observation.window.start (1 = numeric, 2 = column with Dates, 3 = column with units, 4 = a Date), as well as durations:
  followup.window.start.type <- ifelse(is.numeric(followup.window.start),
                                       1, # a number in the appropriate units
                                       ifelse(followup.window.start %in% data.names,
                                              ifelse(inherits(data[,followup.window.start],"Date"),
                                                     2,  # column of Date objects
                                                     3), # column of numbers in the appropriate units
                                              4)); # a Date object
  followup.window.duration.is.number <- is.numeric(followup.window.duration)
  observation.window.start.type <- ifelse(is.numeric(observation.window.start),
                                          1, # a number in the appropriate units
                                          ifelse(observation.window.start %in% data.names,
                                                 ifelse(inherits(data[,observation.window.start],"Date"),
                                                        2,  # column of Date objects
                                                        3), # column of numbers in the appropriate units
                                                 4)); # a Date object
  observation.window.duration.is.number <- is.numeric(observation.window.duration)

  setkeyv(ret.val, c(ID.colname, ".DATE.as.Date")); # key (and sorting) by patient ID and event date

  # The workhorse auxiliary function: For a given (subset) of data, compute the event intervals and gaps:
  .workhorse.function <- function(data=NULL,
                                  ID.colname=NULL,
                                  event.date.colname=NULL,
                                  event.duration.colname=NULL,
                                  event.daily.dose.colname=NULL,
                                  medication.class.colname=NULL,
                                  event.interval.colname=NULL,
                                  gap.days.colname=NULL,
                                  carryover.within.obs.window=NULL,
                                  carryover.into.obs.window=NULL,
                                  carry.only.for.same.medication=NULL,
                                  consider.dosage.change=NULL,
                                  followup.window.start=NULL,
                                  followup.window.start.unit=NULL,
                                  followup.window.duration=NULL,
                                  followup.window.duration.unit=NULL,
                                  observation.window.start=NULL,
                                  observation.window.start.unit=NULL,
                                  observation.window.duration=NULL,
                                  observation.window.duration.unit=NULL,
                                  date.format=NULL,
                                  suppress.warnings=NULL
  )
  {
    # Auxiliary internal function: For a given patient, compute the gaps and return the required columns:
    .process.patient <- function(data4ID)
    {
      # Number of events:
      n.events <- nrow(data4ID);

      # Force the selection, evaluation of promises and caching of the needed columns:
      # ... which columns to select (with their indices):
      columns.to.cache <- c(event.date2.colname, event.duration.colname); event.date2.colname.index <- 1; event.duration.colname.index <- 2;
      curr.index <- 3;
      if( !is.na(medication.class.colname) ){ columns.to.cache <- c(columns.to.cache, medication.class.colname); medication.class.colname.index <- curr.index; curr.index <- curr.index + 1;}
      if( !is.na(event.daily.dose.colname) ){ columns.to.cache <- c(columns.to.cache, event.daily.dose.colname); event.daily.dose.colname.index <- curr.index; curr.index <- curr.index + 1;}
      if( followup.window.start.type %in% c(2,3) ){ columns.to.cache <- c(columns.to.cache, followup.window.start); followup.window.start.index <- curr.index; curr.index <- curr.index + 1;}
      if( !followup.window.duration.is.number ){ columns.to.cache <- c(columns.to.cache, followup.window.duration); followup.window.duration.index <- curr.index; curr.index <- curr.index + 1;}
      if( observation.window.start.type %in% c(2,3) ){ columns.to.cache <- c(columns.to.cache, observation.window.start); observation.window.start.index <- curr.index; curr.index <- curr.index + 1;}
      if( !observation.window.duration.is.number ){ columns.to.cache <- c(columns.to.cache, observation.window.duration); observation.window.duration.index <- curr.index; curr.index <- curr.index + 1;}
      # ... select these columns:
      data4ID.selected.columns <- data4ID[, columns.to.cache, with=FALSE]; # alternative to: data4ID[,..columns.to.cache]
      # ... cache the columns based on their indices:
      event.date2.column <- data4ID.selected.columns[[event.date2.colname.index]]; event.duration.column <- data4ID.selected.columns[[event.duration.colname.index]];
      if( !is.na(medication.class.colname) ) medication.class.column <- data4ID.selected.columns[[medication.class.colname.index]];
      if( !is.na(event.daily.dose.colname) ) event.daily.dose.column <- data4ID.selected.columns[[event.daily.dose.colname.index]];
      if( followup.window.start.type %in% c(2,3) ) followup.window.start.column <- data4ID.selected.columns[[followup.window.start.index]];
      if( !followup.window.duration.is.number ) followup.window.duration.column <- data4ID.selected.columns[[followup.window.duration.index]];
      if( observation.window.start.type %in% c(2,3) ) observation.window.start.column <- data4ID.selected.columns[[observation.window.start.index]];
      if( !observation.window.duration.is.number ) observation.window.duration.column <- data4ID.selected.columns[[observation.window.duration.index]];

      # Cache also follow-up window start and end dates:
      # start dates
      .FU.START.DATE <- switch(followup.window.start.type,
                               .add.time.interval.to.date(event.date2.column[1], followup.window.start, followup.window.start.unit, suppress.warnings),                 # 1
                               followup.window.start.column[1],                                                                                                         # 2
                               .add.time.interval.to.date(event.date2.column[1], followup.window.start.column[1], followup.window.start.unit, suppress.warnings),       # 3
                               followup.window.start);                                                                                                                  # 4
      if( is.na(.FU.START.DATE) )
      {
        # Return a valid but empty object:
        return (list(".FU.START.DATE"=as.Date(NA),
                     ".FU.END.DATE"=as.Date(NA),
                     ".OBS.START.DATE"=as.Date(NA),
                     ".OBS.END.DATE"=as.Date(NA),
                     ".EVENT.STARTS.BEFORE.OBS.WINDOW"=NA,
                     ".EVENT.STARTS.AFTER.OBS.WINDOW"=NA,
                     ".EVENT.WITHIN.FU.WINDOW"=NA,
                     ".TDIFF1"=NA_real_,
                     ".TDIFF2"=NA_real_,
                     ".OBS.WITHIN.FU"=FALSE,
                     ".EVENT.INTERVAL"=NA_real_,
                     ".GAP.DAYS"=NA_real_,
                     ".CARRY.OVER.FROM.BEFORE"=NA_real_));
      }
      # end dates
      .FU.END.DATE <- .add.time.interval.to.date(.FU.START.DATE,
                                                 ifelse(followup.window.duration.is.number, followup.window.duration, followup.window.duration.column[1]),
                                                 followup.window.duration.unit,
                                                 suppress.warnings);
      if( is.na(.FU.END.DATE) )
      {
        # Return a valid but empty object:
        return (list(".FU.START.DATE"=.FU.START.DATE,
                     ".FU.END.DATE"=as.Date(NA),
                     ".OBS.START.DATE"=as.Date(NA),
                     ".OBS.END.DATE"=as.Date(NA),
                     ".EVENT.STARTS.BEFORE.OBS.WINDOW"=NA,
                     ".EVENT.STARTS.AFTER.OBS.WINDOW"=NA,
                     ".EVENT.WITHIN.FU.WINDOW"=NA,
                     ".TDIFF1"=NA_real_,
                     ".TDIFF2"=NA_real_,
                     ".OBS.WITHIN.FU"=FALSE,
                     ".EVENT.INTERVAL"=NA_real_,
                     ".GAP.DAYS"=NA_real_,
                     ".CARRY.OVER.FROM.BEFORE"=NA_real_));
      }
      # Is the event within the FU window?
      .EVENT.WITHIN.FU.WINDOW <- (event.date2.column >= .FU.START.DATE) & (event.date2.column <= .FU.END.DATE);

      # Cache also observation window start and end dates:
      # start dates
      .OBS.START.DATE <- switch(observation.window.start.type,
                                .add.time.interval.to.date(.FU.START.DATE, observation.window.start, observation.window.start.unit, suppress.warnings),                 # 1
                                observation.window.start.column[1],                                                                                                     # 2
                                .add.time.interval.to.date(.FU.START.DATE, observation.window.start.column[1], observation.window.start.unit, suppress.warnings),       # 3
                                observation.window.start);                                                                                                              # 4
      if( is.na(.OBS.START.DATE) )
      {
        # Return a valid but empty object:
        return (list(".FU.START.DATE"=.FU.START.DATE,
                     ".FU.END.DATE"=.FU.END.DATE,
                     ".OBS.START.DATE"=as.Date(NA),
                     ".OBS.END.DATE"=as.Date(NA),
                     ".EVENT.STARTS.BEFORE.OBS.WINDOW"=NA,
                     ".EVENT.STARTS.AFTER.OBS.WINDOW"=NA,
                     ".EVENT.WITHIN.FU.WINDOW"=.EVENT.WITHIN.FU.WINDOW,
                     ".TDIFF1"=NA_real_,
                     ".TDIFF2"=NA_real_,
                     ".OBS.WITHIN.FU"=FALSE,
                     ".EVENT.INTERVAL"=NA_real_,
                     ".GAP.DAYS"=NA_real_,
                     ".CARRY.OVER.FROM.BEFORE"=NA_real_));
      }
      # end dates
      .OBS.END.DATE <- .add.time.interval.to.date(.OBS.START.DATE,
                                                  ifelse(observation.window.duration.is.number, observation.window.duration, observation.window.duration.column[1]),
                                                  observation.window.duration.unit,
                                                  suppress.warnings);
      if( is.na(.OBS.END.DATE) )
      {
        # Return a valid but empty object:
        return (list(".FU.START.DATE"=.FU.START.DATE,
                     ".FU.END.DATE"=.FU.END.DATE,
                     ".OBS.START.DATE"=.OBS.START.DATE,
                     ".OBS.END.DATE"=as.Date(NA),
                     ".EVENT.STARTS.BEFORE.OBS.WINDOW"=NA,
                     ".EVENT.STARTS.AFTER.OBS.WINDOW"=NA,
                     ".EVENT.WITHIN.FU.WINDOW"=.EVENT.WITHIN.FU.WINDOW,
                     ".TDIFF1"=NA_real_,
                     ".TDIFF2"=NA_real_,
                     ".OBS.WITHIN.FU"=FALSE,
                     ".EVENT.INTERVAL"=NA_real_,
                     ".GAP.DAYS"=NA_real_,
                     ".CARRY.OVER.FROM.BEFORE"=NA_real_));
      }

      # For each event, starts before/after the observation window start date?
      .EVENT.STARTS.BEFORE.OBS.WINDOW <- (event.date2.column < .OBS.START.DATE);
      .EVENT.STARTS.AFTER.OBS.WINDOW  <- (event.date2.column > .OBS.END.DATE);

      # Cache some time differences:
      # event.duration.colname - (.OBS.START.DATE - event.date2.colname):
      .TDIFF1 <- (event.duration.column - .difftime.Dates.as.days(.OBS.START.DATE, event.date2.column));
      # event.date2.colname[current+1] - event.date2.colname[current]:
      if( n.events > 1 )
      {
        .TDIFF2 <- c(.difftime.Dates.as.days(event.date2.column[-1], event.date2.column[-n.events]),NA_real_);
      } else
      {
        .TDIFF2 <- NA_real_;
      }

      # Make sure the observation window is included in the follow-up window:
      if( (.FU.START.DATE > .OBS.START.DATE) || (.FU.END.DATE < .OBS.END.DATE) )
      {
        # Return a valid but empty object:
        return (list(".FU.START.DATE"=.FU.START.DATE,
                     ".FU.END.DATE"=.FU.END.DATE,
                     ".OBS.START.DATE"=.OBS.START.DATE,
                     ".OBS.END.DATE"=.OBS.END.DATE,
                     ".EVENT.STARTS.BEFORE.OBS.WINDOW"=.EVENT.STARTS.BEFORE.OBS.WINDOW,
                     ".EVENT.STARTS.AFTER.OBS.WINDOW"=.EVENT.STARTS.AFTER.OBS.WINDOW,
                     ".EVENT.WITHIN.FU.WINDOW"=.EVENT.WITHIN.FU.WINDOW,
                     ".TDIFF1"=.TDIFF1,
                     ".TDIFF2"=.TDIFF2,
                     ".OBS.WITHIN.FU"=FALSE,
                     ".EVENT.INTERVAL"=NA_real_,
                     ".GAP.DAYS"=NA_real_,
                     ".CARRY.OVER.FROM.BEFORE"=NA_real_));
      } else
      {
        .OBS.WITHIN.FU <- TRUE;
      }

      .CARRY.OVER.FROM.BEFORE <- .EVENT.INTERVAL <- .GAP.DAYS <- rep(NA_real_,n.events); # initialize to NA
      # Select only those events within the FUW and OW (depending on carryover into OW):
      if( !carryover.into.obs.window )
      {
        s <- which(.EVENT.WITHIN.FU.WINDOW & !.EVENT.STARTS.BEFORE.OBS.WINDOW & !.EVENT.STARTS.AFTER.OBS.WINDOW); # select all events for this patient within the observation window
      } else
      {
        s <- which(.EVENT.WITHIN.FU.WINDOW & !.EVENT.STARTS.AFTER.OBS.WINDOW); # select all events for patient ID within the follow-up window
      }
      slen <- length(s); # cache it up
      if( slen == 1 ) # only one event in the observation window
      {
        # Computations happen within the observation window
        .EVENT.INTERVAL[s] <- .difftime.Dates.as.days(.OBS.END.DATE, event.date2.column[s]); # for last event, the interval ends at the end of OW
        .CARRY.OVER.FROM.BEFORE[s] <- 0.0; # no carry-over into this unique event
        .GAP.DAYS[s] <- max(0.0, (.EVENT.INTERVAL[s] - event.duration.column[s])); # the actual gap cannot be negative
      } else if( slen > 1 ) # at least one event in the observation window
      {
        # Computations happen within the observation window
        if( carryover.within.obs.window )
        {
          # do carry over within the observation window:

          # was there a change in medication?
          if( !is.na(medication.class.colname) )
          {
            medication.changed <- c((medication.class.column[s[-slen]] != medication.class.column[s[-1]]), FALSE);
          } else
          {
            medication.changed <- rep(FALSE,slen);
          }
          # was there a change in dosage?
          if( !is.na(event.daily.dose.colname) )
          {
            dosage.change.ratio <- c((event.daily.dose.column[s[-slen]] / event.daily.dose.column[s[-1]]), 1.0);
          } else
          {
            dosage.change.ratio <- rep(1.0,slen);
          }
          # event intervals:
          .EVENT.INTERVAL[s]       <- .TDIFF2[s]; # save the time difference between next and current event start dates, but
          .EVENT.INTERVAL[s[slen]] <- .difftime.Dates.as.days(.OBS.END.DATE, event.date2.column[s[slen]]); # for last event, the interval ends at the end of OW
          # event.interval - event.duration:
          .event.interval.minus.duration <- (.EVENT.INTERVAL[s] - event.duration.column[s]);
          # cache various medication and dosage change conditions:
          cond1 <- (carry.only.for.same.medication & medication.changed);
          cond2 <- ((carry.only.for.same.medication & consider.dosage.change & !medication.changed) | (!carry.only.for.same.medication & consider.dosage.change));

          carry.over <- 0; # Initially, no carry-over into the first event
          # for each event:
          for( i in seq_along(s) ) # this for loop is not a performance bottleneck!
          {
            si <- s[i]; # caching s[i] as it's used a lot

            # Save the carry-over into the event:
            .CARRY.OVER.FROM.BEFORE[si] <- carry.over;

            # Computing the gap between events:
            gap <- (.event.interval.minus.duration[i] - carry.over); # subtract the event duration and carry-over from event interval
            if( gap < 0.0 ) # the actual gap cannot be negative
            {
              .GAP.DAYS[si] <- 0.0; carry.over <- (-gap);
            } else
            {
              .GAP.DAYS[si] <- gap; carry.over <- 0.0;
            }

            if( cond1[i] )
            {
              # Do not carry over across medication changes:
              carry.over <- 0;
            } else if( cond2[i] )
            {
              # Apply the dosage change ratio:
              carry.over <- carry.over * dosage.change.ratio[i]; # adjust the carry-over relative to dosage change
            }
          }
        } else
        {
          # do not consider carry over within the observation window:

          # event intervals:
          .EVENT.INTERVAL[s]       <- .TDIFF2[s]; # save the time difference between next and current event start dates, but
          .EVENT.INTERVAL[s[slen]] <- .difftime.Dates.as.days(.OBS.END.DATE, event.date2.column[s[slen]]); # for last event, the interval ends at the end of OW
          # event.interval - event.duration:
          .event.interval.minus.duration <- (.EVENT.INTERVAL[s] - event.duration.column[s]);
          # no carry over in this case:
          .CARRY.OVER.FROM.BEFORE[s] <- 0.0;

          # for each event:
          for( i in seq_along(s) ) # this for loop is not a performance bottleneck!
          {
            si <- s[i]; # caching s[i] as it's used a lot

            # Computing the gap between events:
            gap <- .event.interval.minus.duration[i]; # the event duration
            if( gap < 0.0 ) # the actual gap cannot be negative
            {
              .GAP.DAYS[si] <- 0.0;
            } else
            {
              .GAP.DAYS[si] <- gap;
            }
          }
        }
      }

      # Return the computed columns:
      return (list(".FU.START.DATE"=.FU.START.DATE,
                   ".FU.END.DATE"=.FU.END.DATE,
                   ".OBS.START.DATE"=.OBS.START.DATE,
                   ".OBS.END.DATE"=.OBS.END.DATE,
                   ".EVENT.STARTS.BEFORE.OBS.WINDOW"=.EVENT.STARTS.BEFORE.OBS.WINDOW,
                   ".EVENT.STARTS.AFTER.OBS.WINDOW"=.EVENT.STARTS.AFTER.OBS.WINDOW,
                   ".EVENT.WITHIN.FU.WINDOW"=.EVENT.WITHIN.FU.WINDOW,
                   ".TDIFF1"=.TDIFF1,
                   ".TDIFF2"=.TDIFF2,
                   ".OBS.WITHIN.FU"=.OBS.WITHIN.FU,
                   ".EVENT.INTERVAL"=.EVENT.INTERVAL,
                   ".GAP.DAYS"=.GAP.DAYS,
                   ".CARRY.OVER.FROM.BEFORE"=.CARRY.OVER.FROM.BEFORE));
    }

    # Don't attempt to proces an empty dataset:
    if( is.null(data) || nrow(data) == 0 ) return (NULL);

    data[, c(".FU.START.DATE", ".FU.END.DATE",
             ".OBS.START.DATE", ".OBS.END.DATE",
             ".EVENT.STARTS.BEFORE.OBS.WINDOW", ".EVENT.STARTS.AFTER.OBS.WINDOW", ".EVENT.WITHIN.FU.WINDOW",
             ".TDIFF1", ".TDIFF2",
             ".OBS.WITHIN.FU",
             event.interval.colname, gap.days.colname,
             ".CARRY.OVER.FROM.BEFORE") :=
           .process.patient(.SD), # for each patient, compute the various columns and assign them back into the data.table
           by=ID.colname # group by patients
        ];
    return (data);
  }

  # Compute the workhorse function:
  ret.val <- .compute.function(.workhorse.function,
                               parallel.backend=parallel.backend,
                               parallel.threads=parallel.threads,
                               data=ret.val,
                               ID.colname=ID.colname,
                               event.date.colname=event.date.colname,
                               event.duration.colname=event.duration.colname,
                               event.daily.dose.colname=event.daily.dose.colname,
                               medication.class.colname=medication.class.colname,
                               event.interval.colname=event.interval.colname,
                               gap.days.colname=gap.days.colname,
                               carryover.within.obs.window=carryover.within.obs.window,
                               carryover.into.obs.window=carryover.into.obs.window,
                               carry.only.for.same.medication=carry.only.for.same.medication,
                               consider.dosage.change=consider.dosage.change,
                               followup.window.start=followup.window.start,
                               followup.window.start.unit=followup.window.start.unit,
                               followup.window.duration=followup.window.duration,
                               followup.window.duration.unit=followup.window.duration.unit,
                               observation.window.start=observation.window.start,
                               observation.window.start.unit=observation.window.start.unit,
                               observation.window.duration=observation.window.duration,
                               observation.window.duration.unit=observation.window.duration.unit,
                               date.format=date.format,
                               suppress.warnings=suppress.warnings);

  if( is.null(ret.val) || nrow(ret.val) < 1 )
  {
    if( !suppress.warnings ) .report.ewms("Computing event intervals and gap days failed!\n", "error", "compute.event.int.gaps", "AdhereR");
    return (NULL);
  }
  if( any(!ret.val$.OBS.WITHIN.FU) )
  {
    if( !suppress.warnings ) .report.ewms(paste0("The observation window is not within the follow-up window for participant(s) ",paste0(unique(ret.val[!ret.val$.OBS.WITHIN.FU,get(ID.colname)]),collapse=", ")," !\n"), "error", "compute.event.int.gaps", "AdhereR");
    return (NULL);
  }

  # Make sure events outside the observation window are NA'ed:
  if( !keep.event.interval.for.all.events ) ret.val[ .OBS.WITHIN.FU & (.EVENT.STARTS.BEFORE.OBS.WINDOW | .EVENT.STARTS.AFTER.OBS.WINDOW), c(event.interval.colname) := NA_real_ ];

  # Remove the events that fall outside the follow-up window:
  if( remove.events.outside.followup.window ) ret.val <- ret.val[ which(.OBS.WITHIN.FU & .EVENT.WITHIN.FU.WINDOW), ];
  # If the results are empty return NULL:
  if( is.null(ret.val) || nrow(ret.val) < 1 )
  {
    if( !suppress.warnings ) .report.ewms("No observations fall within the follow-up and observation windows!\n", "error", "compute.event.int.gaps", "AdhereR");
    return (NULL);
  }

  # Final clean-up: delete temporary columns:
  if( !keep.window.start.end.dates )
  {
    ret.val[,c(".DATE.as.Date",
               ".FU.START.DATE",
               ".FU.END.DATE",
               ".OBS.START.DATE",
               ".OBS.END.DATE",
               ".OBS.WITHIN.FU",
               ".EVENT.STARTS.BEFORE.OBS.WINDOW",
               ".TDIFF1",
               ".TDIFF2",
               ".EVENT.STARTS.AFTER.OBS.WINDOW",
               ".CARRY.OVER.FROM.BEFORE",
               ".EVENT.WITHIN.FU.WINDOW") := NULL];
  }

  # If the results are empty return NULL:
  if( is.null(ret.val) || nrow(ret.val) < 1 ) return (NULL);

  if( !return.data.table )
  {
    return (as.data.frame(ret.val));
  } else
  {
    if( ".DATE.as.Date" %in% names(ret.val) )
    {
      setkeyv(ret.val, c(ID.colname, ".DATE.as.Date")); # make sure it is keyed by patient ID and event date
    } else
    {
      setkeyv(ret.val, c(ID.colname)); # make sure it is keyed by patient ID (as event date was removed)
    }
    return (ret.val);
  }
}



#' Compute Treatment Episodes.
#'
#' For a given event (prescribing or dispensing) database, compute the treatment
#' episodes for each patient in various scenarious.
#'
#' This should in general not be called directly by the user, but is provided as
#' a basis for the extension to new CMAs.
#'
#' For the last treatment episode, the gap is considered only when longer than
#' the maximum permissible gap.
#' Please note the following:
#' \itemize{
#'  \item episode starts at first medication event for a particular medication,
#'  \item episode ends on the day when the last supply of that medication
#'  finished or if a period longer than the permissible gap preceded the next
#'  medication event, or at the end of the FUW,
#'  \item end episode gap days represents either the number of days after the
#'  end of the treatment episode (if medication changed, or if a period longer
#'  than the permissible gap preceded the next medication event) or at the
#'  end of (and within) the episode, i.e. the number of days after the last
#'  supply finished (if no other medication event followed until the end of the
#'  FUW),
#'  \item the duration of the episode is the interval between the episode start
#'  and episode end (and may include the gap days at the end, in the latter
#'  condition described above),
#'  \item the number of gap days after the end of the episode can be computed
#'  as all values larger than the permissible gap and 0 otherwise,
#'  \item if medication change starts new episode, then previous episode ends
#'  when the last supply is finished (irrespective of the length of gap compared
#'  to a maximum permissible gap); any days before the date of the new
#'  medication supply are considered a gap; this maintains consistency with gaps
#'  between episodes (whether they are constructed based on the maximum
#'  permissible gap rule or the medication change rule).
#' }
#'
#' @param data A \emph{\code{data.frame}} containing the events used to
#' compute the CMA. Must contain, at a minimum, the patient unique ID, the event
#' date and duration, and might also contain the daily dosage and medication
#' type (the actual column names are defined in the following four parameters);
#' the \code{CMA} constructors call this parameter \code{data}.
#' @param ID.colname A \emph{string}, the name of the column in \code{data}
#' containing the unique patient ID, or \code{NA} if not defined.
#' @param event.date.colname A \emph{string}, the name of the column in
#' \code{data} containing the start date of the event (in the format given in
#' the \code{date.format} parameter), or \code{NA} if not defined.
#' @param event.duration.colname A \emph{string}, the name of the column in
#' \code{data} containing the event duration (in days), or \code{NA} if not
#' defined.
#' @param event.daily.dose.colname A \emph{string}, the name of the column in
#' \code{data} containing the prescribed daily dose, or \code{NA} if not defined.
#' @param medication.class.colname A \emph{string}, the name of the column in
#' \code{data} containing the classes/types/groups of medication, or \code{NA}
#' if not defined.
#' @param carryover.within.obs.window \emph{Logical}, if \code{TRUE} consider
#' the carry-over within the observation window, or \code{NA} if not defined.
#' @param carry.only.for.same.medication \emph{Logical}, if \code{TRUE} the
#' carry-over applies only across medication of the same type, or \code{NA} if
#' not defined.
#' @param consider.dosage.change \emph{Logical}, if \code{TRUE} the carry-over
#' is adjusted to reflect changes in dosage, or \code{NA} if not defined.
#' @param medication.change.means.new.treatment.episode \emph{Logical}, should
#' a change in medication automatically start a new treatment episode?
#' @param dosage.change.means.new.treatment.episode \emph{Logical}, should
#' a change in dosage automatically start a new treatment episode?
#' @param maximum.permissible.gap The \emph{number} of units given by
#' \code{maximum.permissible.gap.unit} representing the maximum duration of
#' permissible gaps between treatment episodes (can also be a percent, see
#' \code{maximum.permissible.gap.unit} for details).
#' @param maximum.permissible.gap.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"}, \emph{"years"} or \emph{"percent"}, and
#' represents the time units that \code{maximum.permissible.gap} refers to;
#' if \emph{percent}, then  \code{maximum.permissible.gap} is interpreted as a
#' percent (can be greater than 100\%) of the duration of the current
#' prescription.
#' @param maximum.permissible.gap.append.to.episode.proportion a \emph{number}
#' giving the proportion of the \code{maximum.permissible.gap} to append at the
#' end of an episode with a gap larger than the \code{maximum.permissible.gap};
#' varies between 0.0 (no addition, the default) and 1.0 (the full
#' \code{maximum.permissible.gap} is added).
#' @param followup.window.start If a \emph{\code{Date}} object it is the actual
#' start date of the follow-up window; if a \emph{string} it is the name of the
#' column in \code{data} containing the start date of the follow-up window; if a
#' \emph{number} it is the number of time units defined in the
#' \code{followup.window.start.unit} parameter after the begin of the
#' participant's first event; or \code{NA} if not defined.
#' @param followup.window.start.unit can be either \emph{"days"}, \emph{"weeks"},
#' \emph{"months"} or \emph{"years"}, and represents the time units that
#' \code{followup.window.start} refers to (when a number), or \code{NA} if not
#' defined.
#' @param followup.window.duration a \emph{number} representing the duration of
#' the follow-up window in the time units given in
#' \code{followup.window.duration.unit}, or \code{NA} if not defined.
#' @param followup.window.duration.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.duration} refers to, or \code{NA} if not
#' defined.
#' @param event.interval.colname A \emph{string}, the name of a newly-created
#' column storing the number of days between the start of the current event and
#' the start of the next one; the default value "event.interval" should be
#' changed only if there is a naming conflict with a pre-existing
#' "event.interval" column in \code{event.info}.
#' @param gap.days.colname A \emph{string}, the name of a newly-created column
#' storing the number of days when medication was not available (i.e., the
#' "gap days"); the default value "gap.days" should be changed only if there is
#' a naming conflict with a pre-existing "gap.days" column in \code{event.info}.
#' @param date.format A \emph{string} giving the format of the dates used in the
#' \code{data} and the other parameters; see the \code{format} parameters of the
#' \code{\link[base]{as.Date}} function for details (NB, this concerns only the
#' dates given as strings and not as \code{Date} objects).
#' @param parallel.backend Can be "none" (the default) for single-threaded
#' execution, "multicore"  (using \code{mclapply} in package \code{parallel})
#' for multicore processing (NB. not currently implemented on MS Windows and
#' automatically falls back on "snow" on this platform),  or "snow",
#' "snow(SOCK)" (equivalent to "snow"), "snow(MPI)" or "snow(NWS)" specifying
#' various types of SNOW clusters (can be on the local machine or more complex
#' setups -- please see the documentation of package \code{snow} for details;
#' the last two require packages \code{Rmpi} and \code{nws}, respectively, not
#' automatically installed with \code{AdhereR}).
#' @param parallel.threads Can be "auto" (for \code{parallel.backend} ==
#' "multicore", defaults to the number of cores in the system as given by
#' \code{options("cores")}, while for \code{parallel.backend} == "snow",
#' defaults to 2), a strictly positive integer specifying the number of parallel
#' threads, or a more complex specification of the SNOW cluster nodes for
#' \code{parallel.backend} == "snow" (see the documentation of package
#' \code{snow} for details).
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any
#' warnings.
#' @param return.data.table \emph{Logical}, if \code{TRUE} return a
#' \code{data.table} object, otherwise a \code{data.frame}.
#' @param ... extra arguments.
#' @return A \code{data.frame} or \code{data.table} with the following columns
#' (or \code{NULL} if no
#' treatment episodes could be computed):
#' \itemize{
#'  \item \code{patid} the patient ID.
#'  \item \code{episode.ID} the episode unique ID (increasing sequentially).
#'  \item \code{episode.start} the episode start date.
#'  \item \code{end.episode.gap.days} the corresponding gap days of the last event in this episode.
#'  \item \code{episode.duration} the episode duration in days.
#'  \item \code{episode.end} the episode end date.
#' }
#' @export
compute.treatment.episodes <- function( data, # this is a per-event data.frame with columns:
                                        ID.colname=NA, # the name of the column containing the unique patient ID
                                        event.date.colname=NA, # the start date of the event in the date.format format
                                        event.duration.colname=NA, # the event duration in days
                                        event.daily.dose.colname=NA, # the prescribed daily dose
                                        medication.class.colname=NA, # the classes/types/groups of medication
                                        # Various types methods of computing gaps:
                                        carryover.within.obs.window=TRUE, # if TRUE consider the carry-over within the observation window
                                        carry.only.for.same.medication=TRUE, # if TRUE the carry-over applies only across medication of same type
                                        consider.dosage.change=TRUE, # if TRUE carry-over is adjusted to reflect changes in dosage
                                        # Treatment episodes:
                                        medication.change.means.new.treatment.episode=TRUE, # does a change in medication automatically start a new treatment episode?
                                        dosage.change.means.new.treatment.episode=FALSE, # does a change in dosage automatically start a new treatment episode?
                                        maximum.permissible.gap=90, # if a number, is the duration in units of max. permissible gaps between treatment episodes
                                        maximum.permissible.gap.unit=c("days", "weeks", "months", "years", "percent")[1], # time units; can be "days", "weeks" (fixed at 7 days), "months" (fixed at 30 days), "years" (fixed at 365 days), or "percent", in which case maximum.permissible.gap is interpreted as a percent (can be > 100%) of the duration of the current prescription
                                        maximum.permissible.gap.append.to.episode.proportion=0.0, # the proportion of the maximum permissible gap to append at the end of an episode with a gap larger than the maximum permissible gap, between 0.0 (no addition, the default) and 1.0 (the full maximum permissible gap is added)
                                        # The follow-up window:
                                        followup.window.start=0, # if a number is the earliest event per participant date + number of units, otherwise a date.format date
                                        followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)
                                        followup.window.duration=365*2, # the duration of the follow-up window in the time units given below
                                        followup.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)
                                        # The description of the output (added) columns:
                                        event.interval.colname="event.interval", # contains number of days between the start of current event and the start of the next
                                        gap.days.colname="gap.days", # contains the number of days when medication was not available
                                        # Date format:
                                        date.format="%m/%d/%Y", # the format of the dates used in this function
                                        # Parallel processing:
                                        parallel.backend=c("none","multicore","snow","snow(SOCK)","snow(MPI)","snow(NWS)")[1], # parallel backend to use
                                        parallel.threads="auto", # specification (or number) of parallel threads
                                        # Misc:
                                        suppress.warnings=FALSE,
                                        return.data.table=FALSE,
                                        ... # other stuff
)
{
  # Consistency checks:
  if( is.null(CMA0(data=data,
                   ID.colname=ID.colname,
                   event.date.colname=event.date.colname,
                   event.duration.colname=event.duration.colname,
                   event.daily.dose.colname=event.daily.dose.colname,
                   medication.class.colname=medication.class.colname,
                   carryover.within.obs.window=carryover.within.obs.window,
                   carry.only.for.same.medication=carry.only.for.same.medication,
                   consider.dosage.change=consider.dosage.change,
                   medication.change.means.new.treatment.episode=medication.change.means.new.treatment.episode,
                   dosage.change.means.new.treatment.episode=dosage.change.means.new.treatment.episode,
                   maximum.permissible.gap=maximum.permissible.gap,
                   maximum.permissible.gap.unit=maximum.permissible.gap.unit,
                   followup.window.start=followup.window.start,
                   followup.window.start.unit=followup.window.start.unit,
                   followup.window.duration=followup.window.duration,
                   followup.window.duration.unit=followup.window.duration.unit,
                   event.interval.colname=event.interval.colname,
                   gap.days.colname=gap.days.colname,
                   date.format=date.format,
                   parallel.backend=parallel.backend,
                   parallel.threads=parallel.threads,
                   suppress.warnings=suppress.warnings,
                   return.data.table=return.data.table,
                   ...)) ) # delegate default checks to CMA0!
  {
    return (NULL);
  }
  # Episode-specific stuff:
  if( is.na(medication.class.colname) && medication.change.means.new.treatment.episode )
  {
    if( !suppress.warnings ) .report.ewms("When 'medication.class.colname' is NA, 'medication.change.means.new.treatment.episode' must be FALSE!\n", "error", "compute.treatment.episodes", "AdhereR");
    return (NULL);
  }
  if( is.na(event.daily.dose.colname) && dosage.change.means.new.treatment.episode )
  {
    if( !suppress.warnings ) .report.ewms("When 'event.daily.dose.colname' is NA, 'dosage.change.means.new.treatment.episode' must be FALSE!\n", "error", "compute.treatment.episodes", "AdhereR");
    return (NULL);
  }

  # Convert maximum permissible gap units into days or proportion:
  maximum.permissible.gap.as.percent <- FALSE; # is the maximum permissible gap a percent of the current duration?
  maximum.permissible.gap <- switch(maximum.permissible.gap.unit,
                                    "days" = maximum.permissible.gap,
                                    "weeks" = maximum.permissible.gap * 7,
                                    "months" = maximum.permissible.gap * 30,
                                    "years" = maximum.permissible.gap * 365,
                                    "percent" = {maximum.permissible.gap.as.percent <- TRUE; maximum.permissible.gap / 100;}, # transform it into a proportion for faster computation
                                    {if(!suppress.warnings) .report.ewms(paste0("Unknown maximum.permissible.gap.unit '",maximum.permissible.gap.unit,"': assuming you meant 'days'."), "warning", "compute.treatment.episodes", "AdhereR");  maximum.permissible.gap;} # otherwise force it to "days"
                                   );
  if( maximum.permissible.gap < 0 ) maximum.permissible.gap <- 0; # make sure this is positive

  # Check maximum.permissible.gap.append.to.episode.proportion:
  if( !is.numeric(maximum.permissible.gap.append.to.episode.proportion) || length(maximum.permissible.gap.append.to.episode.proportion) != 1 ||
      maximum.permissible.gap.append.to.episode.proportion < 0.0 || maximum.permissible.gap.append.to.episode.proportion > 1.0 )
  {
    if( !suppress.warnings ) .report.ewms("'maximum.permissible.gap.append.to.episode.proportion' must be a number between 0.0 and 1.0!\n", "error", "compute.treatment.episodes", "AdhereR");
    return (NULL);
  }

  # The workhorse auxiliary function: For a given (subset) of data, compute the event intervals and gaps:
  .workhorse.function <- function(data=NULL,
                                  ID.colname=NULL,
                                  event.date.colname=NULL,
                                  event.duration.colname=NULL,
                                  event.daily.dose.colname=NULL,
                                  medication.class.colname=NULL,
                                  event.interval.colname=NULL,
                                  gap.days.colname=NULL,
                                  carryover.within.obs.window=NULL,
                                  carryover.into.obs.window=NULL,
                                  carry.only.for.same.medication=NULL,
                                  consider.dosage.change=NULL,
                                  followup.window.start=NULL,
                                  followup.window.start.unit=NULL,
                                  followup.window.duration=NULL,
                                  followup.window.duration.unit=NULL,
                                  observation.window.start=NULL,
                                  observation.window.start.unit=NULL,
                                  observation.window.duration=NULL,
                                  observation.window.duration.unit=NULL,
                                  date.format=NULL,
                                  suppress.warnings=NULL
  )
  {
    # Auxiliary internal function: Compute the CMA for a given patient:
    .process.patient <- function(data4ID)
    {
      # Cache things up:
      n.events                <- nrow(data4ID);
      last.event              <- max(which(data4ID$.EVENT.WITHIN.FU.WINDOW), na.rm=TRUE); # the last event in the follow-up window
      event.duration.column   <- data4ID[,get(event.duration.colname)];
      gap.days.column         <- data4ID[,get(gap.days.colname)];
      if( !is.na(medication.class.colname) ) medication.class.column <- data4ID[,get(medication.class.colname)];
      MAX.PERMISSIBLE.GAP     <- switch(as.numeric(maximum.permissible.gap.as.percent)+1,
                                        rep(maximum.permissible.gap,n.events), # FALSE: maximum.permissible.gap is fixed in days
                                        maximum.permissible.gap * event.duration.column); # TRUE: maximum.permissible.gap is a percent of the duration

      # Select those gaps bigger than the maximum.permissible.gap:
      s <- (gap.days.column > MAX.PERMISSIBLE.GAP);
      if( medication.change.means.new.treatment.episode && n.events > 1 )
      {
        # If medication change triggers a new episode and there is more than one event, consider these changes as well:
        s <- (s | c(medication.class.column[1:(n.events-1)] != medication.class.column[2:n.events], TRUE));
      }
      if( dosage.change.means.new.treatment.episode && n.events > 1 )
      {
        # If dosage change triggers a new episode and there is more than one event, consider these changes as well:
        s <- (s | c(event.daily.dose.colname[1:(n.events-1)] != event.daily.dose.colname[2:n.events], TRUE));
      }
      s <- which(s); s.len <- length(s);

      if( n.events == 1 || s.len == 0 || (s.len==1 && s==n.events) )
      {
        # One single treatment episode starting with the first event within the follow-up window and the end of the follow-up window:
        treatment.episodes <- data.table("episode.ID"=as.numeric(1),
                                         "episode.start"=data4ID$.DATE.as.Date[1],
                                         "end.episode.gap.days"=gap.days.column[last.event]);
        gap.correction <- (maximum.permissible.gap.append.to.episode.proportion * MAX.PERMISSIBLE.GAP[last.event]);
        n.episodes <- nrow(treatment.episodes);
        treatment.episodes[, episode.duration := as.numeric(data4ID$.FU.END.DATE[1] - episode.start[n.episodes]) -
          ifelse(end.episode.gap.days[n.episodes] < MAX.PERMISSIBLE.GAP[last.event], # duration of the last event of the last episode
                 0,
                 end.episode.gap.days[n.episodes] + gap.correction)]; # the last episode duration is the end date of the follow-up window minus the start date of the last episode, minus the gap after the last episode plus a proportion of the maximum permissible gap only if the gap is longer than the maximum permissible gap
        treatment.episodes[, episode.end := (episode.start + episode.duration)];
      } else
      {
        # Define the treatment episodes:
        if( s[s.len] != n.events )
        {
          # The last event with gap > maximum permissible is not the last event for this patient:
          treatment.episodes <- data.table("episode.ID"=as.numeric(1:(s.len+1)),
                                           "episode.start"=c(data4ID$.DATE.as.Date[1],               # the 1st event in the follow-up window
                                                             data4ID$.DATE.as.Date[s+1]),            # the next event
                                           "end.episode.gap.days"=c(gap.days.column[s],              # the corresponding gap.days of the last event in this episode
                                                                    gap.days.column[last.event]));   # the corresponding gap.days of the last event in this follow-up window
          episode.gap.smaller.than.max <- c(gap.days.column[s] <= MAX.PERMISSIBLE.GAP[s],
                                            gap.days.column[last.event] <= MAX.PERMISSIBLE.GAP[last.event]);
          gap.correction <- (maximum.permissible.gap.append.to.episode.proportion * MAX.PERMISSIBLE.GAP[c(s, last.event)]);
        } else
        {
          # The last event with gap > maximum permissible is the last event for this patient:
          treatment.episodes <- data.table("episode.ID"=as.numeric(1:s.len),
                                           "episode.start"=c(data4ID$.DATE.as.Date[1],                # the 1st event in the follow-up window
                                                             data4ID$.DATE.as.Date[s[-s.len]+1]),     # the next event
                                           "end.episode.gap.days"=c(gap.days.column[s]));             # the corresponding gap.days of the last event in this follow-up window
          episode.gap.smaller.than.max <- c(gap.days.column[s] <= MAX.PERMISSIBLE.GAP[s]);
          gap.correction <- (maximum.permissible.gap.append.to.episode.proportion * MAX.PERMISSIBLE.GAP[s]);
        }
        n.episodes <- nrow(treatment.episodes);
        treatment.episodes[, episode.duration := c(as.numeric(episode.start[2:n.episodes] - episode.start[1:(n.episodes-1)]) - # start date of next episode minus start date of current episode
                                                     ifelse(episode.gap.smaller.than.max[1:(n.episodes-1)],
                                                            0,
                                                            end.episode.gap.days[1:(n.episodes-1)] - gap.correction[1:(n.episodes-1)]), # minus the start date of the current episode, minus the gap after the current episode plus a proportion of the maximum permissible gap only if the gap is larger than the maximum permissible gap (i.e., not for changes of medication or dosage)
                                                   as.numeric(data4ID$.FU.END.DATE[1] - episode.start[n.episodes]) -
                                                     ifelse(episode.gap.smaller.than.max[n.episodes], # duration of the last event of the last episode
                                                            0,
                                                            end.episode.gap.days[n.episodes] - gap.correction[n.episodes]))]; # the last episode duration is the end date of the follow-up window minus the start date of the last episode minus the gap after the last episode plus a proportion of the maximum permissible gap only if the gap is longer than the maximum permissible gap
        treatment.episodes[, episode.end := (episode.start + episode.duration)];
      }
      return (treatment.episodes);
    }

    # Call the compute.event.int.gaps() function and use the results:
    event.info <- compute.event.int.gaps(data=as.data.frame(data),
                                         ID.colname=ID.colname,
                                         event.date.colname=event.date.colname,
                                         event.duration.colname=event.duration.colname,
                                         event.daily.dose.colname=event.daily.dose.colname,
                                         medication.class.colname=medication.class.colname,
                                         event.interval.colname=event.interval.colname,
                                         gap.days.colname=gap.days.colname,
                                         carryover.within.obs.window=carryover.within.obs.window,
                                         carryover.into.obs.window=carryover.into.obs.window,
                                         carry.only.for.same.medication=carry.only.for.same.medication,
                                         consider.dosage.change=consider.dosage.change,
                                         followup.window.start=followup.window.start,
                                         followup.window.start.unit=followup.window.start.unit,
                                         followup.window.duration=followup.window.duration,
                                         followup.window.duration.unit=followup.window.duration.unit,
                                         observation.window.start=observation.window.start,
                                         observation.window.start.unit=observation.window.start.unit,
                                         observation.window.duration=observation.window.duration,
                                         observation.window.duration.unit=observation.window.duration.unit,
                                         date.format=date.format,
                                         keep.window.start.end.dates=TRUE,
                                         parallel.backend="none",
                                         parallel.threads=1,
                                         suppress.warnings=suppress.warnings,
                                         return.data.table=TRUE);
    if( is.null(event.info) ) return (NULL);

    episodes <- event.info[!is.na(get(event.interval.colname)) & !is.na(get(gap.days.colname)), # only for those events that have non-NA interval and gap estimates
                           .process.patient(.SD),
                           by=ID.colname];
    setnames(episodes, 1, ID.colname);
    return (episodes);
  }

  # Convert to data.table, cache event dat as Date objects, and key by patient ID and event date
  data.copy <- data.table(data);
  data.copy[, .DATE.as.Date := as.Date(get(event.date.colname),format=date.format)]; # .DATE.as.Date: convert event.date.colname from formatted string to Date
  setkeyv(data.copy, c(ID.colname, ".DATE.as.Date")); # key (and sorting) by patient ID and event date

  # Compute the workhorse function:
  tmp <- .compute.function(.workhorse.function,
                           parallel.backend=parallel.backend,
                           parallel.threads=parallel.threads,
                           data=data.copy,
                           ID.colname=ID.colname,
                           event.date.colname=event.date.colname,
                           event.duration.colname=event.duration.colname,
                           event.daily.dose.colname=event.daily.dose.colname,
                           medication.class.colname=medication.class.colname,
                           event.interval.colname=event.interval.colname,
                           gap.days.colname=gap.days.colname,
                           carryover.within.obs.window=carryover.within.obs.window,
                           carryover.into.obs.window=FALSE,
                           carry.only.for.same.medication=carry.only.for.same.medication,
                           consider.dosage.change=consider.dosage.change,
                           followup.window.start=followup.window.start,
                           followup.window.start.unit=followup.window.start.unit,
                           followup.window.duration=followup.window.duration,
                           followup.window.duration.unit=followup.window.duration.unit,
                           observation.window.start=0,
                           observation.window.start.unit="days",
                           observation.window.duration=followup.window.duration,
                           observation.window.duration.unit=followup.window.duration.unit,
                           date.format=date.format,
                           suppress.warnings=suppress.warnings);
  if( is.null(tmp) ) return (NULL);
  tmp[,end.episode.gap.days := as.integer(end.episode.gap.days)]; # force end.episode.gap.days to be integer to make for pretty printing...
  if( !return.data.table ) return (as.data.frame(tmp)) else return (tmp);
}


# Shared code between multiple CMAs
.cma.skeleton <- function(data,
                          ret.val,
                          cma.class.name,
                          ID.colname,
                          event.date.colname,
                          event.duration.colname,
                          event.daily.dose.colname,
                          medication.class.colname,
                          medication.groups.colname,
                          flatten.medication.groups,
                          followup.window.start.per.medication.group,
                          event.interval.colname,
                          gap.days.colname,
                          carryover.within.obs.window,
                          carryover.into.obs.window,
                          carry.only.for.same.medication,
                          consider.dosage.change,
                          followup.window.start,
                          followup.window.start.unit,
                          followup.window.duration,
                          followup.window.duration.unit,
                          observation.window.start,
                          observation.window.start.unit,
                          observation.window.duration,
                          observation.window.duration.unit,
                          date.format,
                          suppress.warnings,
                          force.NA.CMA.for.failed.patients,
                          parallel.backend,
                          parallel.threads,
                          .workhorse.function)
{
  # Convert to data.table, cache event date as Date objects, and key by patient ID and event date
  data.copy <- data.table(data);
  data.copy[, .DATE.as.Date := as.Date(get(event.date.colname),format=date.format)]; # .DATE.as.Date: convert event.date.colname from formatted string to Date
  data.copy$..ORIGINAL.ROW.ORDER.. <- 1:nrow(data.copy); # preserve the original order of the rows (needed for medication groups)
  setkeyv(data.copy, c(ID.colname, ".DATE.as.Date")); # key (and sorting) by patient ID and event date

  # Are there medication groups?
  if( is.null(mg <- getMGs(ret.val)) )
  {
    # Nope: do a single estimation on the whole dataset:

    # Compute the workhorse function:
    tmp <- .compute.function(.workhorse.function, fnc.ret.vals=2,
                             parallel.backend=parallel.backend,
                             parallel.threads=parallel.threads,
                             data=data.copy,
                             ID.colname=ID.colname,
                             event.date.colname=event.date.colname,
                             event.duration.colname=event.duration.colname,
                             event.daily.dose.colname=event.daily.dose.colname,
                             medication.class.colname=medication.class.colname,
                             event.interval.colname=event.interval.colname,
                             gap.days.colname=gap.days.colname,
                             carryover.within.obs.window=carryover.within.obs.window,
                             carryover.into.obs.window=carryover.into.obs.window,
                             carry.only.for.same.medication=carry.only.for.same.medication,
                             consider.dosage.change=consider.dosage.change,
                             followup.window.start=followup.window.start,
                             followup.window.start.unit=followup.window.start.unit,
                             followup.window.duration=followup.window.duration,
                             followup.window.duration.unit=followup.window.duration.unit,
                             observation.window.start=observation.window.start,
                             observation.window.start.unit=observation.window.start.unit,
                             observation.window.duration=observation.window.duration,
                             observation.window.duration.unit=observation.window.duration.unit,
                             date.format=date.format,
                             suppress.warnings=suppress.warnings);
    if( is.null(tmp) || is.null(tmp$CMA) || !inherits(tmp$CMA,"data.frame") || is.null(tmp$event.info) ) return (NULL);

    # Convert to data.frame and return:
    if( force.NA.CMA.for.failed.patients )
    {
      # Make sure patients with failed CMA estimations get an NA estimate!
      patids <- unique(data.copy[,get(ID.colname)]);
      if( length(patids) > nrow(tmp$CMA) )
      {
        setnames(tmp$CMA, 1, ".ID"); tmp$CMA <- merge(data.table(".ID"=patids, key=".ID"), tmp$CMA, all.x=TRUE);
      }
    }
    setnames(tmp$CMA, c(ID.colname,"CMA")); ret.val[["CMA"]] <- as.data.frame(tmp$CMA);
    ret.val[["event.info"]] <- as.data.frame(tmp$event.info);
    class(ret.val) <- c(cma.class.name, class(ret.val));
    return (ret.val);

  } else
  {
    # Yes

    # Make sure the group's observations reflect the potentially new order of the observations in the data:
    mb.obs <- mg$obs[data.copy$..ORIGINAL.ROW.ORDER.., ];

    # Focus only on the non-trivial ones:
    mg.to.eval <- (colSums(!is.na(mb.obs) & mb.obs) > 0);
    if( sum(mg.to.eval) == 0 )
    {
      # None selects not even one observation!
      .report.ewms(paste0("None of the medication classes (included __ALL_OTHERS__) selects any observation!\n"), "warning", cma.class.name, "AdhereR");
      return (NULL);
    }
    mb.obs <- mb.obs[,mg.to.eval]; # keep only the non-trivial ones

    # How is the FUW to be estimated?
    if( !followup.window.start.per.medication.group )
    {
      # The FUW and OW are estimated once per patient (i.e., all medication groups share the same FUW and OW):
      # Call the compute.event.int.gaps() function and use the results:
      event.info <- compute.event.int.gaps(data=as.data.frame(data.copy),
                                           ID.colname=ID.colname,
                                           event.date.colname=event.date.colname,
                                           event.duration.colname=event.duration.colname,
                                           event.daily.dose.colname=event.daily.dose.colname,
                                           medication.class.colname=medication.class.colname,
                                           event.interval.colname=event.interval.colname,
                                           gap.days.colname=gap.days.colname,
                                           carryover.within.obs.window=carryover.within.obs.window,
                                           carryover.into.obs.window=carryover.into.obs.window,
                                           carry.only.for.same.medication=carry.only.for.same.medication,
                                           consider.dosage.change=consider.dosage.change,
                                           followup.window.start=followup.window.start,
                                           followup.window.start.unit=followup.window.start.unit,
                                           followup.window.duration=followup.window.duration,
                                           followup.window.duration.unit=followup.window.duration.unit,
                                           observation.window.start=observation.window.start,
                                           observation.window.start.unit=observation.window.start.unit,
                                           observation.window.duration=observation.window.duration,
                                           observation.window.duration.unit=observation.window.duration.unit,
                                           date.format=date.format,
                                           keep.window.start.end.dates=TRUE,
                                           parallel.backend="none", # make sure this runs sequentially!
                                           parallel.threads=1,
                                           suppress.warnings=suppress.warnings,
                                           return.data.table=FALSE);
      if( is.null(event.info) ) return (list("CMA"=NA, "event.info"=NULL));

      # Add the FUW and OW start dates to the data:
      data.copy <- merge(data.copy, unique(event.info[,c(ID.colname, ".FU.START.DATE", ".OBS.START.DATE")]), by=ID.colname, all.x=TRUE, all.y=FALSE);
      names(data.copy)[ names(data.copy) == ".FU.START.DATE" ]  <- ".FU.START.DATE.PER.PATIENT.ACROSS.MGS";
      names(data.copy)[ names(data.copy) == ".OBS.START.DATE" ] <- ".OBS.START.DATE.PER.PATIENT.ACROSS.MGS";

      # Adjust the corresponding params:
      actual.followup.window.start         <- ".FU.START.DATE.PER.PATIENT.ACROSS.MGS";
      actual.followup.window.start.unit    <- NA;
      actual.observation.window.start      <- ".OBS.START.DATE.PER.PATIENT.ACROSS.MGS";
      actual.observation.window.start.unit <- NA;
    } else
    {
      # The FUW and OW are estimated separately for each medication group -- nothing to do...
      actual.followup.window.start         <- followup.window.start;
      actual.followup.window.start.unit    <- followup.window.start.unit;
      actual.observation.window.start      <- observation.window.start;
      actual.observation.window.start.unit <- observation.window.start.unit;
    }

    # Check if there are medication classes that refer to the same observations (they would result in the same estimates):
    mb.obs.dupl <- duplicated(mb.obs, MARGIN=2);

    # Estimate each separately:
    tmp <- lapply(1:nrow(mg$defs), function(i)
    {
      # Check if these are to be evaluated:
      if( !mg.to.eval[i] )
      {
        return (list("CMA"=NULL, "event.info"=NULL));
      }

      # Translate into the index of the classes to be evaluated:
      ii <- sum(mg.to.eval[1:i]);

      # Cache the selected observations:
      mg.sel.obs <- mb.obs[,ii];

      # Check if this is a duplicated medication class:
      if( mb.obs.dupl[ii] )
      {
        # Find which one is the original:
        for( j in 1:(ii-1) ) # ii=1 never should be TRUE
        {
          if( identical(mb.obs[,j], mg.sel.obs) )
          {
            # This is the original: return it and stop
            return (c("identical.to"=j));
          }
        }
      }

      # Compute the workhorse function:
      tmp <- .compute.function(.workhorse.function, fnc.ret.vals=2,
                               parallel.backend=parallel.backend,
                               parallel.threads=parallel.threads,
                               data=data.copy[mg.sel.obs,], # apply it on the subset of observations covered by this medication class
                               ID.colname=ID.colname,
                               event.date.colname=event.date.colname,
                               event.duration.colname=event.duration.colname,
                               event.daily.dose.colname=event.daily.dose.colname,
                               medication.class.colname=medication.class.colname,
                               event.interval.colname=event.interval.colname,
                               gap.days.colname=gap.days.colname,
                               carryover.within.obs.window=carryover.within.obs.window,
                               carryover.into.obs.window=carryover.into.obs.window,
                               carry.only.for.same.medication=carry.only.for.same.medication,
                               consider.dosage.change=consider.dosage.change,
                               followup.window.start=actual.followup.window.start,
                               followup.window.start.unit=actual.followup.window.start.unit,
                               followup.window.duration=followup.window.duration,
                               followup.window.duration.unit=followup.window.duration.unit,
                               observation.window.start=actual.observation.window.start,
                               observation.window.start.unit=actual.observation.window.start.unit,
                               observation.window.duration=observation.window.duration,
                               observation.window.duration.unit=observation.window.duration.unit,
                               date.format=date.format,
                               suppress.warnings=suppress.warnings);
      if( is.null(tmp) || (!is.null(tmp$CMA) && !inherits(tmp$CMA,"data.frame")) || is.null(tmp$event.info) ) return (NULL);

      if( !is.null(tmp$CMA) )
      {
        # Convert to data.frame and return:
        if( force.NA.CMA.for.failed.patients )
        {
          # Make sure patients with failed CMA estimations get an NA estimate!
          patids <- unique(data.copy[,get(ID.colname)]);
          if( length(patids) > nrow(tmp$CMA) )
          {
            setnames(tmp$CMA, 1, ".ID"); tmp$CMA <- merge(data.table(".ID"=patids, key=".ID"), tmp$CMA, all.x=TRUE);
          }
        }

        setnames(tmp$CMA, c(ID.colname,"CMA")); tmp$CMA <- as.data.frame(tmp$CMA);
      }
      if( !is.null(tmp$event.info) )
      {
        tmp$event.info <- as.data.frame(tmp$event.info);
      }
      return (tmp);

    });

    # Set the names:
    names(tmp) <- mg$defs$name;

    # Solve the duplicates:
    for( i in seq_along(tmp) )
    {
      if( is.numeric(tmp[[i]]) && length(tmp[[i]]) == 1 && names(tmp[[i]]) == "identical.to" ) tmp[[i]] <- tmp[[ tmp[[i]] ]];
    }

    # Rearrange these and return:
    ret.val[["CMA"]]        <- lapply(tmp, function(x) x$CMA);
    ret.val[["event.info"]] <- lapply(tmp, function(x) x$event.info);
    if( flatten.medication.groups && !is.na(medication.groups.colname) )
    {
      # Flatten the CMA:
      tmp <- do.call(rbind, ret.val[["CMA"]]);
      if( is.null(tmp) || nrow(tmp) == 0 )
      {
        ret.val[["CMA"]] <- NULL;
      } else
      {
        tmp <- cbind(tmp, unlist(lapply(1:length(ret.val[["CMA"]]), function(i) if(!is.null(ret.val[["CMA"]][[i]])){rep(names(ret.val[["CMA"]])[i], nrow(ret.val[["CMA"]][[i]]))}else{NULL})));
        names(tmp)[ncol(tmp)] <- medication.groups.colname; rownames(tmp) <- NULL;
        ret.val[["CMA"]] <- tmp;
      }

      # ... and the event.info:
      tmp <- do.call(rbind, ret.val[["event.info"]]);
      if( is.null(tmp) || nrow(tmp) == 0 )
      {
        ret.val[["event.info"]] <- NULL;
      } else
      {
        tmp <- cbind(tmp, unlist(lapply(1:length(ret.val[["event.info"]]), function(i) if(!is.null(ret.val[["event.info"]][[i]])){rep(names(ret.val[["event.info"]])[i], nrow(ret.val[["event.info"]][[i]]))}else{NULL})));
        names(tmp)[ncol(tmp)] <- medication.groups.colname; rownames(tmp) <- NULL;
        ret.val[["event.info"]] <- tmp;
      }
    }
    class(ret.val) <- c(cma.class.name, class(ret.val));
    return (ret.val);

  }
}


############################################################################################
#
# Plot CMAs greater or equal to 1 (smart enough to know what to do with specific info)
#
############################################################################################
.plot.CMA1plus <- function(cma,                                   # the CMA1 (or derived) object
                           patients.to.plot=NULL,                 # list of patient IDs to plot or NULL for all
                           duration=NA,                           # duration to plot in days (if missing, determined from the data)
                           align.all.patients=FALSE, align.first.event.at.zero=TRUE, # should all patients be aligned? and, if so, place the first event as the horizintal 0?
                           show.period=c("dates","days")[2],      # draw vertical bars at regular interval as dates or days?
                           period.in.days=90,                     # the interval (in days) at which to draw veritcal lines
                           show.legend=TRUE, legend.x="right", legend.y="bottom", legend.bkg.opacity=0.5, legend.cex=0.75, legend.cex.title=1.0, # legend params and position
                           cex=1.0, cex.axis=0.75, cex.lab=1.0,   # various graphical params
                           show.cma=TRUE,                         # show the CMA type
                           xlab=c("dates"="Date", "days"="Days"), # Vector of x labels to show for the two types of periods, or a single value for both, or NULL for nothing
                           ylab=c("withoutCMA"="patient", "withCMA"="patient (& CMA)"), # Vector of y labels to show without and with CMA estimates, or a single value for both, or NULL ofr nonthing
                           title=c("aligned"="Event patterns (all patients aligned)", "notaligned"="Event patterns"), # Vector of titles to show for and without alignment, or a single value for both, or NULL for nothing
                           col.cats=rainbow,                      # single color or a function mapping the categories to colors
                           unspecified.category.label="drug",     # the label of the unspecified category of medication
                           medication.groups.to.plot=NULL,        # the names of the medication groups to plot (by default, all)
                           medication.groups.separator.show=TRUE, medication.groups.separator.lty="solid", medication.groups.separator.lwd=2, medication.groups.separator.color="blue", # group medication events by patient?
                           medication.groups.allother.label="*",  # the label to use for the __ALL_OTHERS__ medication class (defaults to *)
                           lty.event="solid", lwd.event=2, pch.start.event=15, pch.end.event=16, # event style
                           show.event.intervals=TRUE,             # show the actual prescription intervals
                           plot.events.vertically.displaced=TRUE, # display the events on different lines (vertical displacement) or not (defaults to TRUE)?
                           col.na="lightgray",                    # color for missing data
                           print.CMA=TRUE, CMA.cex=0.50,           # print CMA next to the participant's ID?
                           plot.CMA=TRUE,                   # plot the CMA next to the participant ID?
                           CMA.plot.ratio=0.10,             # the proportion of the total horizontal plot to be taken by the CMA plot
                           CMA.plot.col="lightgreen", CMA.plot.border="darkgreen", CMA.plot.bkg="aquamarine", CMA.plot.text=CMA.plot.border, # attributes of the CMA plot
                           highlight.followup.window=TRUE, followup.window.col="green",
                           highlight.observation.window=TRUE, observation.window.col="yellow", observation.window.density=35, observation.window.angle=-30, observation.window.opacity=0.3,
                           show.real.obs.window.start=TRUE, real.obs.window.density=35, real.obs.window.angle=30, # for some CMAs, the real observation window starts at a different date
                           print.dose=FALSE, cex.dose=0.75, print.dose.outline.col="white", print.dose.centered=FALSE, # print daily dose
                           plot.dose=FALSE, lwd.event.max.dose=8, plot.dose.lwd.across.medication.classes=FALSE, # draw daily dose as line width
                           alternating.bands.cols=c("white", "gray95"), # the colors of the alternating vertical bands across patients (NULL=don't draw any; can be >= 1 color)
                           bw.plot=FALSE,                         # if TRUE, override all user-given colors and replace them with a scheme suitable for grayscale plotting
                           force.draw.text=FALSE,                 # if true, always draw text even if too big or too small
                           min.plot.size.in.characters.horiz=0, min.plot.size.in.characters.vert=0, # the minimum plot size (in characters: horizontally, for the whole duration, vertically, per event)
                           suppress.warnings=FALSE,               # suppress warnings?
                           max.patients.to.plot=100,              # maximum number of patients to plot
                           export.formats=NULL,                   # the formats to export the figure to (by default, none); can be any subset of "svg" (just SVG file), "html" (SVG + HTML + CSS + JavaScript all embedded within the HTML document), "jpg", "png", "webp", "ps" and "pdf"
                           export.formats.fileprefix="AdhereR-plot", # the file name prefix for the exported formats
                           export.formats.height=NA, export.formats.width=NA, # desired dimensions (in pixels) for the exported figure (defaults to sane values)
                           export.formats.save.svg.placeholder=TRUE,
                           export.formats.svg.placeholder.type=c("jpg", "png", "webp")[2],
                           export.formats.svg.placeholder.embed=FALSE, # save a placeholder for the SVG image?
                           export.formats.html.template=NULL, export.formats.html.javascript=NULL, export.formats.html.css=NULL, # HTML, JavaScript and CSS templates for exporting HTML+SVG
                           export.formats.directory=NA,           # if exporting, which directory to export to (if not give, creates files in the temporary directory)
                           generate.R.plot=TRUE,                  # generate standard (base R) plot for plotting within R?
                           ...
)
{
  .plot.CMAs(cma,
             patients.to.plot=patients.to.plot,
             duration=duration,
             align.all.patients=align.all.patients,
             align.first.event.at.zero=align.first.event.at.zero,
             show.period=show.period,
             period.in.days=period.in.days,
             show.legend=show.legend,
             legend.x=legend.x,
             legend.y=legend.y,
             legend.bkg.opacity=legend.bkg.opacity,
             legend.cex=legend.cex,
             legend.cex.title=legend.cex.title,
             cex=cex,
             cex.axis=cex.axis,
             cex.lab=cex.lab,
             show.cma=show.cma,
             xlab=xlab,
             ylab=ylab,
             title=title,
             col.cats=col.cats,
             unspecified.category.label=unspecified.category.label,
             medication.groups.to.plot=medication.groups.to.plot,
             medication.groups.separator.show=medication.groups.separator.show,
             medication.groups.separator.lty=medication.groups.separator.lty,
             medication.groups.separator.lwd=medication.groups.separator.lwd,
             medication.groups.separator.color=medication.groups.separator.color,
             medication.groups.allother.label=medication.groups.allother.label,
             lty.event=lty.event,
             lwd.event=lwd.event,
             show.event.intervals=show.event.intervals,
             plot.events.vertically.displaced=plot.events.vertically.displaced,
             pch.start.event=pch.start.event,
             pch.end.event=pch.end.event,
             print.dose=print.dose,
             cex.dose=cex.dose,
             print.dose.outline.col=print.dose.outline.col,
             print.dose.centered=print.dose.centered,
             plot.dose=plot.dose,
             lwd.event.max.dose=lwd.event.max.dose,
             plot.dose.lwd.across.medication.classes=plot.dose.lwd.across.medication.classes,
             col.na=col.na,
             print.CMA=print.CMA,
             CMA.cex=CMA.cex,
             plot.CMA=plot.CMA,
             CMA.plot.ratio=CMA.plot.ratio,
             CMA.plot.col=CMA.plot.col,
             CMA.plot.border=CMA.plot.border,
             CMA.plot.bkg=CMA.plot.bkg,
             CMA.plot.text=CMA.plot.text,
             plot.partial.CMAs.as=FALSE, # CMA1+ do not have partial CMAs
             highlight.followup.window=highlight.followup.window,
             followup.window.col=followup.window.col,
             highlight.observation.window=highlight.observation.window,
             observation.window.col=observation.window.col,
             observation.window.density=observation.window.density,
             observation.window.angle=observation.window.angle,
             observation.window.opacity=observation.window.opacity,
             show.real.obs.window.start=show.real.obs.window.start,
             real.obs.window.density=real.obs.window.density,
             real.obs.window.angle=real.obs.window.angle,
             alternating.bands.cols=alternating.bands.cols,
             bw.plot=bw.plot,
             force.draw.text=force.draw.text,
             min.plot.size.in.characters.horiz=min.plot.size.in.characters.horiz,
             min.plot.size.in.characters.vert=min.plot.size.in.characters.vert,
             max.patients.to.plot=max.patients.to.plot,
             export.formats=export.formats,
             export.formats.fileprefix=export.formats.fileprefix,
             export.formats.height=export.formats.height,
             export.formats.width=export.formats.width,
             export.formats.save.svg.placeholder=export.formats.save.svg.placeholder,
             export.formats.svg.placeholder.type=export.formats.svg.placeholder.type,
             export.formats.svg.placeholder.embed=export.formats.svg.placeholder.embed,
             export.formats.html.template=export.formats.html.template,
             export.formats.html.javascript=export.formats.html.javascript,
             export.formats.html.css=export.formats.html.css,
             export.formats.directory=export.formats.directory,
             generate.R.plot=generate.R.plot,
             suppress.warnings=suppress.warnings);
}



#' CMA1 and CMA3 constructors.
#'
#' Constructs a CMA (continuous multiple-interval measures of medication
#' availability/gaps) type 1 or type 3 object.
#'
#' \code{CMA1} considers the total number of days with medication supplied in
#' all medication events in the observation window, excluding the last event.
#' \code{CMA3} is identical to \code{CMA1} except that it is capped at 100\%.
#'
#' The formula is
#' \deqn{(number of days supply excluding last) / (first to last event)}
#' Thus, the durations of all events are added up, possibly resulting in an CMA
#' estimate (much) bigger than 1.0 (100\%).
#'
#' \code{\link{CMA2}} and \code{CMA1} differ in the inclusion or not of the last
#' event.
#'
#' @param data A \emph{\code{data.frame}} containing the events used to compute
#' the CMA. Must contain, at a minimum, the patient unique ID, the event date
#' and duration, and might also contain the daily dosage and medication type
#' (the actual column names are defined in the following four parameters).
#' @param ID.colname A \emph{string}, the name of the column in \code{data}
#' containing the unique patient ID; must be present.
#' @param event.date.colname A \emph{string}, the name of the column in
#' \code{data} containing the start date of the event (in the format given in
#' the \code{date.format} parameter); must be present.
#' @param event.duration.colname A \emph{string}, the name of the column in
#' \code{data} containing the event duration (in days); must be present.
#' @param medication.groups A \emph{vector} of characters defining medication
#' groups or the name of a column in \code{data} that defines such groups.
#' The names of the vector are the medication group unique names, while
#' the content defines them as logical expressions. While the names can be any
#' string of characters except "\}", it is recommended to stick to the rules for
#' defining vector names in \code{R}. For example,
#' \code{c("A"="CATEGORY == 'medA'", "AA"="{A} & PERDAY < 4"} defines two
#' medication groups: \emph{A} which selects all events of type "medA", and
#' \emph{B} which selects all events already defined by "A" but with a daily
#' dose lower than 4. If \code{NULL}, no medication groups are defined. If
#' medication groups are defined, there is one CMA estimate for each group;
#' moreover, there is a special group \emph{__ALL_OTHERS__} automatically defined
#' containing all observations \emph{not} covered by any of the explicitly defined
#' groups.
#' @param flatten.medication.groups \emph{Logical}, if \code{FALSE} (the default)
#' then the \code{CMA} and \code{event.info} components of the object are lists
#' with one medication group per element; otherwise, they are \code{data.frame}s
#' with an extra column containing the medication group (its name is given by
#' \code{medication.groups.colname}).
#' @param medication.groups.colname a \emph{string} (defaults to ".MED_GROUP_ID")
#' giving the name of the column storing the group name when
#' \code{flatten.medication.groups} is \code{TRUE}.
#' @param followup.window.start If a \emph{\code{Date}} object, it represents
#' the actual start date of the follow-up window; if a \emph{string} it is the
#' name of the column in \code{data} containing the start date of the follow-up
#' window either as the numbers of \code{followup.window.start.unit} units after
#' the first event (the column must be of type \code{numeric}) or as actual
#' dates (in which case the column must be of type \code{Date} or a string
#' that conforms to the format specified in \code{date.format}); if a
#' \emph{number} it is the number of time units defined in the
#' \code{followup.window.start.unit} parameter after the begin of the
#' participant's first event; or \code{NA} if not defined.
#' @param followup.window.start.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.start} refers to (when a number), or
#' \code{NA} if not defined.
#' @param followup.window.start.per.medication.group a \emph{logical}: if there are
#' medication groups defined and this is \code{TRUE}, then the first event
#' considered for the follow-up window start is relative to each medication group
#' separately, otherwise (the default) it is relative to the patient.
#' @param followup.window.duration either a \emph{number} representing the
#' duration of the follow-up window in the time units given in
#' \code{followup.window.duration.unit}, or a \emph{string} giving the column
#' containing these numbers. Should represent a period for which relevant
#' medication events are recorded accurately (e.g. not extend after end of
#' relevant treatment, loss-to-follow-up or change to a health care provider
#' not covered by the database).
#' @param followup.window.duration.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.duration} refers to, or \code{NA} if not
#' defined.
#' @param observation.window.start,observation.window.start.unit,observation.window.duration,observation.window.duration.unit the definition of the observation window
#' (see the follow-up window parameters above for details).
#' @param date.format A \emph{string} giving the format of the dates used in the
#' \code{data} and the other parameters; see the \code{format} parameters of the
#' \code{\link[base]{as.Date}} function for details (NB, this concerns only the
#' dates given as strings and not as \code{Date} objects).
#' @param summary Metadata as a \emph{string}, briefly describing this CMA.
#' @param event.interval.colname A \emph{string}, the name of a newly-created
#' column storing the number of days between the start of the current event and
#' the start of the next one; the default value "event.interval" should be
#' changed only if there is a naming conflict with a pre-existing
#' "event.interval" column in \code{event.info}.
#' @param gap.days.colname A \emph{string}, the name of a newly-created column
#' storing the number of days when medication was not available (i.e., the
#' "gap days"); the default value "gap.days" should be changed only if there is
#' a naming conflict with a pre-existing "gap.days" column in \code{event.info}.
#' @param force.NA.CMA.for.failed.patients \emph{Logical} describing how the
#' patients for which the CMA estimation fails are treated: if \code{TRUE}
#' they are returned with an \code{NA} CMA estimate, while for
#' \code{FALSE} they are omitted.
#' @param parallel.backend Can be "none" (the default) for single-threaded
#' execution, "multicore"  (using \code{mclapply} in package \code{parallel})
#' for multicore processing (NB. not currently implemented on MS Windows and
#' automatically falls back on "snow" on this platform),  or "snow",
#' "snow(SOCK)" (equivalent to "snow"), "snow(MPI)" or "snow(NWS)" specifying
#' various types of SNOW clusters (can be on the local machine or more complex
#' setups -- please see the documentation of package \code{snow} for details;
#' the last two require packages \code{Rmpi} and \code{nws}, respectively, not
#' automatically installed with \code{AdhereR}).
#' @param parallel.threads Can be "auto" (for \code{parallel.backend} ==
#' "multicore", defaults to the number of cores in the system as given by
#' \code{options("cores")}, while for \code{parallel.backend} == "snow",
#' defaults to 2), a strictly positive integer specifying the number of parallel
#' threads, or a more complex specification of the SNOW cluster nodes for
#' \code{parallel.backend} == "snow" (see the documentation of package
#' \code{snow} for details).
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any
#' warnings.
#' @param arguments.that.should.not.be.defined a \emph{list} of argument names
#' and pre-defined valuesfor which a warning should be thrown if passed to the
#' function.
#' @param ... other possible parameters
#' @return An \code{S3} object of class \code{CMA1} (derived from \code{CMA0})
#' with the following fields:
#' \itemize{
#'  \item \code{data} The actual event data, as given by the \code{data}
#'  parameter.
#'  \item \code{ID.colname} the name of the column in \code{data} containing the
#'  unique patient ID, as given by the \code{ID.colname} parameter.
#'  \item \code{event.date.colname} the name of the column in \code{data}
#'  containing the start date of the event (in the format given in the
#'  \code{date.format} parameter), as given by the \code{event.date.colname}
#'  parameter.
#'  \item \code{event.duration.colname} the name of the column in \code{data}
#'  containing the event duration (in days), as given by the
#'  \code{event.duration.colname} parameter.
#'  \item \code{event.daily.dose.colname} the name of the column in \code{data}
#'  containing the prescribed daily dose, as given by the
#'  \code{event.daily.dose.colname} parameter.
#'  \item \code{medication.class.colname} the name of the column in \code{data}
#'  containing the classes/types/groups of medication, as given by the
#'  \code{medication.class.colname} parameter.
#'  \item \code{followup.window.start} the beginning of the follow-up window, as
#'  given by the \code{followup.window.start} parameter.
#'  \item \code{followup.window.start.unit} the time unit of the
#'  \code{followup.window.start}, as given by the
#'  \code{followup.window.start.unit} parameter.
#'  \item \code{followup.window.duration} the duration of the follow-up window,
#'  as given by the \code{followup.window.duration} parameter.
#'  \item \code{followup.window.duration.unit} the time unit of the
#'  \code{followup.window.duration}, as given by the
#'  \code{followup.window.duration.unit} parameter.
#'  \item \code{observation.window.start} the beginning of the observation
#'  window, as given by the \code{observation.window.start} parameter.
#'  \item \code{observation.window.start.unit} the time unit of the
#'  \code{observation.window.start}, as given by the
#'  \code{observation.window.start.unit} parameter.
#'  \item \code{observation.window.duration} the duration of the observation
#'  window, as given by the \code{observation.window.duration} parameter.
#'  \item \code{observation.window.duration.unit} the time unit of the
#'  \code{observation.window.duration}, as given by the
#'  \code{observation.window.duration.unit} parameter.
#'  \item \code{date.format} the format of the dates, as given by the
#'  \code{date.format} parameter.
#'  \item \code{summary} the metadata, as given by the \code{summary}
#'  parameter.
#'  \item \code{event.info} the \code{data.frame} containing the event info
#'  (irrelevant for most users; see \code{\link{compute.event.int.gaps}} for
#'  details).
#'  \item \code{CMA} the \code{data.frame} containing the actual \code{CMA}
#'  estimates for each participant (the \code{ID.colname} column).
#' }
#' Please note that if \code{medication.groups} are defined and
#' \code{flatten.medication.groups} is \code{FALSE}, then the \code{CMA}
#' and \code{event.info} are named lists, each element containing the CMA and
#' event.info corresponding to a single medication group (the element's name),
#' but if \code{flatten.medication.groups} is \code{FALSE} then they are
#' \code{data.frame}s with an extra column giving the medication group (the
#' column's name is given by \code{medication.groups.colname}).
#' @seealso CMAs 1 to 8 are described in:
#'
#' Vollmer, W. M., Xu, M., Feldstein, A., Smith, D., Waterbury, A., & Rand, C.
#' (2012). Comparison of pharmacy-based measures of medication adherence.
#' \emph{BMC Health Services Research}, \strong{12}, 155.
#' \url{http://doi.org/10.1186/1472-6963-12-155}.
#'
#' @examples
#' cma1 <- CMA1(data=med.events,
#'              ID.colname="PATIENT_ID",
#'              event.date.colname="DATE",
#'              event.duration.colname="DURATION",
#'              followup.window.start=30,
#'              observation.window.start=30,
#'              observation.window.duration=365,
#'              date.format="%m/%d/%Y"
#'             );
#' cma3 <- CMA3(data=med.events,
#'              ID.colname="PATIENT_ID",
#'              event.date.colname="DATE",
#'              event.duration.colname="DURATION",
#'              followup.window.start=30,
#'              observation.window.start=30,
#'              observation.window.duration=365,
#'              date.format="%m/%d/%Y"
#'             );
#' @export
CMA1 <- function( data=NULL, # the data used to compute the CMA on
                  # Important columns in the data
                  ID.colname=NA, # the name of the column containing the unique patient ID (NA = undefined)
                  event.date.colname=NA, # the start date of the event in the date.format format (NA = undefined)
                  event.duration.colname=NA, # the event duration in days (NA = undefined)
                  # Groups of medication classes:
                  medication.groups=NULL, # a named vector of medication group definitions, the name of a column in the data that defines the groups, or NULL
                  flatten.medication.groups=FALSE, medication.groups.colname=".MED_GROUP_ID", # if medication.groups were defined, return CMAs and event.info as single data.frame?
                  # The follow-up window:
                  followup.window.start=0, # if a number is the earliest event per participant date plus number of units, or a Date object, or a column name in data (NA = undefined)
                  followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  followup.window.start.per.medication.group=FALSE, # if there are medication groups and this is TRUE, then the first event is relative to each medication group separately, otherwise is relative to the patient
                  followup.window.duration=365*2, # the duration of the follow-up window in the time units given below (NA = undefined)
                  followup.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)  (NA = undefined)
                  # The observation window (embedded in the follow-up window):
                  observation.window.start=0, # the number of time units relative to followup.window.start (NA = undefined)
                  observation.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  observation.window.duration=365*2, # the duration of the observation window in time units (NA = undefined)
                  observation.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  # Date format:
                  date.format="%m/%d/%Y", # the format of the dates used in this function (NA = undefined)
                  # Comments and metadata:
                  summary=NA,
                  # The description of the output (added) columns:
                  event.interval.colname="event.interval", # contains number of days between the start of current event and the start of the next
                  gap.days.colname="gap.days", # contains the number of days when medication was not available
                  # Dealing with failed estimates:
                  force.NA.CMA.for.failed.patients=TRUE, # force the failed patients to have NA CMA estimates?
                  # Parallel processing:
                  parallel.backend=c("none","multicore","snow","snow(SOCK)","snow(MPI)","snow(NWS)")[1], # parallel backend to use
                  parallel.threads="auto", # specification (or number) of parallel threads
                  # Misc:
                  suppress.warnings=FALSE,
                  arguments.that.should.not.be.defined=c("carryover.within.obs.window"=FALSE,
                                                         "carryover.into.obs.window"=FALSE,
                                                         "carry.only.for.same.medication"=FALSE,
                                                         "consider.dosage.change"=FALSE), # the list of argument names and values for which a warning should be thrown if passed to the function
                  ...
                )
{
  # The summary:
  if( is.na(summary) ) summary <- "The ratio of days with medication available in the observation window excluding the last event; durations of all events added up and divided by number of days from first to last event, possibly resulting in a value >1.0";

  # Arguments that should not have been passed:
  if( !suppress.warnings && !is.null(arguments.that.should.not.be.defined) )
  {
    # Get the actual list of arguments (including in the ...); the first is the function's own name:
    args.list <- as.list(match.call(expand.dots = TRUE));
    args.mathing <- (names(arguments.that.should.not.be.defined) %in% names(args.list)[-1]);
    if( any(args.mathing) )
    {
      for( i in which(args.mathing) )
      {
        .report.ewms(paste0("Please note that '",args.list[[1]],"' overrides argument '",names(arguments.that.should.not.be.defined)[i],"' with value '",arguments.that.should.not.be.defined[i],"'!\n"), "warning", "CMA1", "AdhereR");
      }
    }
  }

  # Create the CMA0 object:
  ret.val <- CMA0(data=data,
                  ID.colname=ID.colname,
                  event.date.colname=event.date.colname,
                  event.duration.colname=event.duration.colname,
                  medication.groups=medication.groups,
                  flatten.medication.groups=flatten.medication.groups,
                  medication.groups.colname=medication.groups.colname,
                  followup.window.start=followup.window.start,
                  followup.window.start.unit=followup.window.start.unit,
                  followup.window.start.per.medication.group=followup.window.start.per.medication.group,
                  followup.window.duration=followup.window.duration,
                  followup.window.duration.unit=followup.window.duration.unit,
                  observation.window.start=observation.window.start,
                  observation.window.start.unit=observation.window.start.unit,
                  observation.window.duration=observation.window.duration,
                  observation.window.duration.unit=observation.window.duration.unit,
                  date.format=date.format,
                  summary=summary,
                  suppress.warnings=suppress.warnings);
  if( is.null(ret.val) ) return (NULL); # some serious error upstream
  # The followup.window.start and observation.window.start might have been converted to Date:
  followup.window.start <- ret.val$followup.window.start; observation.window.start <- ret.val$observation.window.start;

  # The workhorse auxiliary function:
  .workhorse.function <- function(data=NULL,
                                  ID.colname=NULL,
                                  event.date.colname=NULL,
                                  event.duration.colname=NULL,
                                  event.daily.dose.colname=NULL,
                                  medication.class.colname=NULL,
                                  event.interval.colname=NULL,
                                  gap.days.colname=NULL,
                                  carryover.within.obs.window=NULL,
                                  carryover.into.obs.window=NULL,
                                  carry.only.for.same.medication=NULL,
                                  consider.dosage.change=NULL,
                                  followup.window.start=NULL,
                                  followup.window.start.unit=NULL,
                                  followup.window.duration=NULL,
                                  followup.window.duration.unit=NULL,
                                  observation.window.start=NULL,
                                  observation.window.start.unit=NULL,
                                  observation.window.duration=NULL,
                                  observation.window.duration.unit=NULL,
                                  date.format=NULL,
                                  suppress.warnings=NULL
  )
  {
    # Auxiliary internal function: Compute the CMA for a given patient:
    .process.patient <- function(data4ID)
    {
      # Force the selection, evaluation of promises and caching of the needed columns:
      # ... which columns to select (with their indices):
      columns.to.cache <- c(".EVENT.STARTS.BEFORE.OBS.WINDOW", ".EVENT.STARTS.AFTER.OBS.WINDOW", ".DATE.as.Date", event.duration.colname);
      # ... select these columns:
      data4ID.selected.columns <- data4ID[, columns.to.cache, with=FALSE]; # alternative to: data4ID[,..columns.to.cache];
      # ... cache the columns based on their indices:
      .EVENT.STARTS.BEFORE.OBS.WINDOW <- data4ID.selected.columns[[1]];
      .EVENT.STARTS.AFTER.OBS.WINDOW <- data4ID.selected.columns[[2]];
      .DATE.as.Date <- data4ID.selected.columns[[3]];
      event.duration.column <- data4ID.selected.columns[[4]];

      # which data to consider:
      s <- which(!(.EVENT.STARTS.BEFORE.OBS.WINDOW | .EVENT.STARTS.AFTER.OBS.WINDOW));
      s.len <- length(s); s1 <- s[1]; ss.len <- s[s.len];

      if( s.len < 2 || (.date.diff <- .difftime.Dates.as.days(.DATE.as.Date[ss.len], .DATE.as.Date[s1])) == 0 )
      {
        # For less than two events or when the first and the last events are on the same day, CMA1 does not make sense
        return (NA_real_);
      } else
      {
        # Otherwise, the sum of durations of the events excluding the last divided by the number of days between the first and the last event
        return (as.numeric(sum(event.duration.column[s[-s.len]],na.rm=TRUE) / .date.diff));
      }
    }

    # Call the compute.event.int.gaps() function and use the results:
    event.info <- compute.event.int.gaps(data=as.data.frame(data),
                                         ID.colname=ID.colname,
                                         event.date.colname=event.date.colname,
                                         event.duration.colname=event.duration.colname,
                                         event.daily.dose.colname=event.daily.dose.colname,
                                         medication.class.colname=medication.class.colname,
                                         event.interval.colname=event.interval.colname,
                                         gap.days.colname=gap.days.colname,
                                         carryover.within.obs.window=carryover.within.obs.window,
                                         carryover.into.obs.window=carryover.into.obs.window,
                                         carry.only.for.same.medication=carry.only.for.same.medication,
                                         consider.dosage.change=consider.dosage.change,
                                         followup.window.start=followup.window.start,
                                         followup.window.start.unit=followup.window.start.unit,
                                         followup.window.duration=followup.window.duration,
                                         followup.window.duration.unit=followup.window.duration.unit,
                                         observation.window.start=observation.window.start,
                                         observation.window.start.unit=observation.window.start.unit,
                                         observation.window.duration=observation.window.duration,
                                         observation.window.duration.unit=observation.window.duration.unit,
                                         date.format=date.format,
                                         keep.window.start.end.dates=TRUE,
                                         parallel.backend="none", # make sure this runs sequentially!
                                         parallel.threads=1,
                                         suppress.warnings=suppress.warnings,
                                         return.data.table=TRUE);
    if( is.null(event.info) ) return (list("CMA"=NA, "event.info"=NULL));

    CMA <- event.info[, .process.patient(.SD), by=ID.colname];
    return (list("CMA"=CMA, "event.info"=event.info));
  }

  ret.val <- .cma.skeleton(data=data,
                           ret.val=ret.val,
                           cma.class.name="CMA1",

                           ID.colname=ID.colname,
                           event.date.colname=event.date.colname,
                           event.duration.colname=event.duration.colname,
                           event.daily.dose.colname=NA, # not relevant
                           medication.class.colname=NA, # not relevant
                           event.interval.colname=event.interval.colname,
                           gap.days.colname=gap.days.colname,
                           carryover.within.obs.window=FALSE, # if TRUE consider the carry-over within the observation window
                           carryover.into.obs.window=FALSE, # if TRUE consider the carry-over from before the starting date of the observation window
                           carry.only.for.same.medication=FALSE, # if TRUE the carry-over applies only across medication of same type
                           consider.dosage.change=FALSE, # if TRUE carry-over is adjusted to reflect changes in dosage
                           followup.window.start=followup.window.start,
                           followup.window.start.unit=followup.window.start.unit,
                           followup.window.duration=followup.window.duration,
                           followup.window.duration.unit=followup.window.duration.unit,
                           observation.window.start=observation.window.start,
                           observation.window.start.unit=observation.window.start.unit,
                           observation.window.duration=observation.window.duration,
                           observation.window.duration.unit=observation.window.duration.unit,
                           date.format=date.format,

                           flatten.medication.groups=flatten.medication.groups,
                           followup.window.start.per.medication.group=followup.window.start.per.medication.group,

                           suppress.warnings=suppress.warnings,
                           force.NA.CMA.for.failed.patients=force.NA.CMA.for.failed.patients,
                           parallel.backend=parallel.backend,
                           parallel.threads=parallel.threads,
                           .workhorse.function=.workhorse.function);

  return (ret.val);
}


#' @rdname print.CMA0
#' @export
print.CMA1 <- function(...) print.CMA0(...)

#' Plot CMA0-derived objects.
#'
#' Plots the event data and estimated CMA encapsulated in objects derived from
#' \code{CMA0}.
#'
#' Please note that this function plots objects inheriting from \code{CMA0} but
#' not objects of type \code{CMA0} itself (these are plotted by
#' \code{\link{plot.CMA0}}).
#'
#' The x-axis represents time (either in days since the earliest date or as
#' actual dates), with consecutive events represented as ascending on the y-axis.
#'
#' Each event is represented as a segment with style \code{lty.event} and line
#' width \code{lwd.event} starting with a \code{pch.start.event} and ending with
#' a \code{pch.end.event} character, coloured with a unique color as given by
#' \code{col.cats}, extending from its start date until its end date.
#' Superimposed on these are shown the event intervals and gap days as estimated
#' by the particular CMA method, more precisely plotting the start and end of
#' the available events as solid filled-in rectangles, and the event gaps as
#' shaded rectangles.
#'
#' The follow-up and the observation windows are plotted as an empty rectangle
#' and as shaded rectangle, respectively (for some CMAs the observation window
#' might be adjusted in which case the adjustment may also be plotted using a
#'  different shading).
#'
#' The CMA estimates can be visually represented as well in the left side of the
#' figure using bars (sometimes the estimates can go above 100\%, in which case
#' the maximum possible bar filling is adjusted to reflect this).
#'
#' When several patients are displayed on the same plot, they are organized
#' vertically, and alternating bands (white and gray) help distinguish
#' consecutive patients.
#' Implicitely, all patients contained in the \code{cma} object will be plotted,
#' but the \code{patients.to.plot} parameter allows the selection of a subset of
#' patients.
#'
#' Finally, the y-axis shows the patient ID and possibly the CMA estimate as
#' well.
#'
#' @param x A \emph{\code{CMA0}} or derived object, representing the CMA to
#' plot
#' @param patients.to.plot A vector of \emph{strings} containing the list of
#' patient IDs to plot (a subset of those in the \code{cma} object), or
#' \code{NULL} for all
#' @param duration A \emph{number}, the total duration (in days) of the whole
#' period to plot; in \code{NA} it is automatically determined from the event
#' data such that the whole dataset fits.
#' @param align.all.patients \emph{Logical}, should all patients be aligned
#' (i.e., the actual dates are discarded and all plots are relative to the
#' earliest date)?
#' @param align.first.event.at.zero \emph{Logical}, should the first event be
#' placed at the origin of the time axis (at 0)?
#' @param show.period A \emph{string}, if "dates" show the actual dates at the
#' regular grid intervals, while for "days" (the default) shows the days since
#' the beginning; if \code{align.all.patients == TRUE}, \code{show.period} is
#' taken as "days".
#' @param period.in.days The \emph{number} of days at which the regular grid is
#' drawn (or 0 for no grid).
#' @param show.legend \emph{Logical}, should the legend be drawn?
#' @param legend.x The position of the legend on the x axis; can be "left",
#' "right" (default), or a \emph{numeric} value.
#' @param legend.y The position of the legend on the y axis; can be "bottom"
#' (default), "top", or a \emph{numeric} value.
#' @param legend.bkg.opacity A \emph{number} between 0.0 and 1.0 specifying the
#' opacity of the legend background.
#' @param cex,cex.axis,cex.lab,legend.cex,legend.cex.title,CMA.cex \emph{numeric}
#' values specifying the \code{cex} of the various types of text.
#' @param show.cma \emph{Logical}, should the CMA type be shown in the title?
#' @param col.cats A \emph{color} or a \emph{function} that specifies the single
#' colour or the colour palette used to plot the different medication; by
#' default \code{rainbow}, but we recommend, whenever possible, a
#' colorblind-friendly palette such as \code{viridis} or \code{colorblind_pal}.
#' @param unspecified.category.label A \emph{string} giving the name of the
#' unspecified (generic) medication category.
#' @param medication.groups.to.plot the names of the medication groups to plot or
#' \code{NULL} (the default) for all.
#' @param medication.groups.separator.show a \emph{boolean}, if \code{TRUE} (the
#' default) visually mark the medication groups the belong to the same patient,
#' using horizontal lines and alternating vertical lines.
#' @param medication.groups.separator.lty,medication.groups.separator.lwd,medication.groups.separator.color
#' graphical parameters (line type, line width and colour describing the visual
#' marking og medication groups as beloning to the same patient.
#' @param medication.groups.allother.label a \emph{string} giving the label to
#' use for the implicit \code{__ALL_OTHERS__} medication group (defaults to "*").
#' @param lty.event,lwd.event,pch.start.event,pch.end.event The style of the
#' event (line style, width, and start and end symbols).
#' @param show.event.intervals \emph{Logical}, should the actual event intervals
#' be shown?
#' @param col.na The colour used for missing event data.
#' @param bw.plot \emph{Logical}, should the plot use grayscale only (i.e., the
#' \code{\link[grDevices]{gray.colors}} function)?
#' @param force.draw.text \emph{Logical}, if \code{TRUE}, always draw text even
#' if too big or too small
#' @param print.CMA \emph{Logical}, should the CMA values be printed?
#' @param plot.CMA \emph{Logical}, should the CMA values be represented
#' graphically?
#' @param CMA.plot.ratio A \emph{number}, the proportion of the total horizontal
#' plot space to be allocated to the CMA plot.
#' @param CMA.plot.col,CMA.plot.border,CMA.plot.bkg,CMA.plot.text \emph{Strings}
#' giving the colours of the various components of the CMA plot.
#' @param highlight.followup.window \emph{Logical}, should the follow-up window
#' be plotted?
#' @param followup.window.col The follow-up window's colour.
#' @param highlight.observation.window \emph{Logical}, should the observation
#' window be plotted?
#' @param observation.window.col,observation.window.density,observation.window.angle,observation.window.opacity
#' Attributes of the observation window (colour, shading density, angle and
#' opacity).
#' @param show.real.obs.window.start,real.obs.window.density,real.obs.window.angle For some CMAs, the observation window might
#' be adjusted, in which case should it be plotted and with that attributes?
#' @param print.dose \emph{Logical}, should the daily dose be printed as text?
#' @param cex.dose \emph{Numeric}, if daily dose is printed, what text size
#' to use?
#' @param print.dose.outline.col If \emph{\code{NA}}, don't print dose text with
#' outline, otherwise a color name/code for the outline.
#' @param print.dose.centered \emph{Logical}, print the daily dose centered on
#' the segment or slightly below it?
#' @param plot.dose \emph{Logical}, should the daily dose be indicated through
#' segment width?
#' @param lwd.event.max.dose \emph{Numeric}, the segment width corresponding to
#' the maximum daily dose (must be >= lwd.event but not too big either).
#' @param plot.dose.lwd.across.medication.classes \emph{Logical}, if \code{TRUE},
#' the line width of the even is scaled relative to all medication classes (i.e.,
#' relative to the global minimum and maximum doses), otherwise it is scale
#' relative only to its medication class.
#' @param min.plot.size.in.characters.horiz,min.plot.size.in.characters.vert
#' \emph{Numeric}, the minimum size of the plotting surface in characters;
#' horizontally (min.plot.size.in.characters.horiz) refers to the the whole
#' duration of the events to plot; vertically (min.plot.size.in.characters.vert)
#' refers to a single event. If the plotting is too small, possible solutions
#' might be: if within \code{RStudio}, try to enlarge the "Plots" panel, or
#' (also valid outside \code{RStudio} but not if using \code{RStudio server}
#' start a new plotting device (e.g., using \code{X11()}, \code{quartz()}
#' or \code{windows()}, depending on OS) or (works always) save to an image
#' (e.g., \code{jpeg(...); ...; dev.off()}) and display it in a viewer.
#' @param max.patients.to.plot \emph{Numeric}, the maximum patients to attempt
#' to plot.
#' @param export.formats a \emph{string} giving the formats to export the figure
#' to (by default \code{NULL}, meaning no exporting); can be any combination of
#' "svg" (just an \code{SVG} file), "html" (\code{SVG} + \code{HTML} + \code{CSS}
#' + \code{JavaScript}, all embedded within one \code{HTML} document), "jpg",
#' "png", "webp", "ps" or "pdf".
#' @param export.formats.fileprefix a \emph{string} giving the file name prefix
#' for the exported formats (defaults to "AdhereR-plot").
#' @param export.formats.height,export.formats.width \emph{numbers} giving the
#' desired dimensions (in pixels) for the exported figure (defaults to sane
#' values if \code{NA}).
#' @param export.formats.save.svg.placeholder a \emph{logical}, if TRUE, save an
#' image placeholder of type given by \code{export.formats.svg.placeholder.type}
#'for the \code{SVG} image.
#' @param export.formats.svg.placeholder.type a \emph{string}, giving the type of
#' placeholder for the \code{SVG} image to save; can be "jpg",
#' "png" (the default) or "webp".
#' @param export.formats.svg.placeholder.embed a \emph{logical}, if \code{TRUE},
#' embed the placeholder image in the HTML document (if any) using \code{base64}
#' encoding, otherwise (the default) leave it as an external image file (works
#' only when an \code{HTML} document is exported and only for \code{JPEG} or
#' \code{PNG} images.
#' @param export.formats.html.template,export.formats.html.javascript,export.formats.html.css
#' \emph{character strings} or \code{NULL} (the default) giving the path to the
#' \code{HTML}, \code{JavaScript} and \code{CSS} templates, respectively, to be
#' used when generating the HTML+CSS semi-interactive plots; when \code{NULL},
#' the default ones included with the package will be used. If you decide to define
#' new templates please use the default ones for inspiration and note that future
#' version are not guaranteed to be backwards compatible!
#' @param export.formats.directory a \emph{string}; if exporting, which directory
#' to export to; if \code{NA} (the default), creates the files in a temporary
#' directory.
#' @param generate.R.plot a \emph{logical}, if \code{TRUE} (the default),
#' generate the standard (base \code{R}) plot for plotting within \code{R}.
#' @param ... other possible parameters
#' @examples
#' cma1 <- CMA1(data=med.events,
#'              ID.colname="PATIENT_ID",
#'              event.date.colname="DATE",
#'              event.duration.colname="DURATION",
#'              followup.window.start=30,
#'              observation.window.start=30,
#'              observation.window.duration=365,
#'              date.format="%m/%d/%Y"
#'             );
#' plot(cma1, patients.to.plot=c("1","2"));
#' @export
plot.CMA1 <- function(x,                                     # the CMA1 (or derived) object
                      ...,                                   # required for S3 consistency
                      patients.to.plot=NULL,                 # list of patient IDs to plot or NULL for all
                      duration=NA,                           # duration to plot in days (if missing, determined from the data)
                      align.all.patients=FALSE, align.first.event.at.zero=TRUE, # should all patients be aligned? and, if so, place the first event as the horizintal 0?
                      show.period=c("dates","days")[2],      # draw vertical bars at regular interval as dates or days?
                      period.in.days=90,                     # the interval (in days) at which to draw veritcal lines
                      show.legend=TRUE, legend.x="right", legend.y="bottom", legend.bkg.opacity=0.5, legend.cex=0.75, legend.cex.title=1.0, # legend params and position
                      cex=1.0, cex.axis=0.75, cex.lab=1.0,   # various graphical params
                      show.cma=TRUE,                         # show the CMA type
                      col.cats=rainbow,                      # single color or a function mapping the categories to colors
                      unspecified.category.label="drug",     # the label of the unspecified category of medication
                      medication.groups.to.plot=NULL,        # the names of the medication groups to plot (by default, all)
                      medication.groups.separator.show=TRUE, medication.groups.separator.lty="solid", medication.groups.separator.lwd=2, medication.groups.separator.color="blue", # group medication events by patient?
                      medication.groups.allother.label="*",  # the label to use for the __ALL_OTHERS__ medication class (defaults to *)
                      lty.event="solid", lwd.event=2, pch.start.event=15, pch.end.event=16, # event style
                      show.event.intervals=TRUE,             # show the actual rpescription intervals
                      col.na="lightgray",                    # color for mising data
                      #col.continuation="black", lty.continuation="dotted", lwd.continuation=1, # style of the contuniation lines connecting consecutive events
                      print.CMA=TRUE, CMA.cex=0.50,           # print CMA next to the participant's ID?
                      plot.CMA=TRUE,                   # plot the CMA next to the participant ID?
                      CMA.plot.ratio=0.10,             # the proportion of the total horizontal plot to be taken by the CMA plot
                      CMA.plot.col="lightgreen", CMA.plot.border="darkgreen", CMA.plot.bkg="aquamarine", CMA.plot.text=CMA.plot.border, # attributes of the CMA plot
                      highlight.followup.window=TRUE, followup.window.col="green",
                      highlight.observation.window=TRUE, observation.window.col="yellow", observation.window.density=35, observation.window.angle=-30, observation.window.opacity=0.3,
                      show.real.obs.window.start=TRUE, real.obs.window.density=35, real.obs.window.angle=30, # for some CMAs, the real observation window starts at a different date
                      print.dose=FALSE, cex.dose=0.75, print.dose.outline.col="white", print.dose.centered=FALSE, # print daily dose
                      plot.dose=FALSE, lwd.event.max.dose=8, plot.dose.lwd.across.medication.classes=FALSE, # draw daily dose as line width
                      bw.plot=FALSE,                         # if TRUE, override all user-given colors and replace them with a scheme suitable for grayscale plotting
                      force.draw.text=FALSE,                 # if true, always draw text even if too big or too small
                      min.plot.size.in.characters.horiz=0, min.plot.size.in.characters.vert=0, # the minimum plot size (in characters: horizontally, for the whole duration, vertically, per event)
                      max.patients.to.plot=100,              # maximum number of patients to plot
                      export.formats=NULL,                   # the formats to export the figure to (by default, none); can be any subset of "svg" (just SVG file), "html" (SVG + HTML + CSS + JavaScript all embedded within the HTML document), "jpg", "png", "webp", "ps" and "pdf"
                      export.formats.fileprefix="AdhereR-plot", # the file name prefix for the exported formats
                      export.formats.height=NA, export.formats.width=NA, # desired dimensions (in pixels) for the exported figure (defaults to sane values)
                      export.formats.save.svg.placeholder=TRUE,
                      export.formats.svg.placeholder.type=c("jpg", "png", "webp")[2],
                      export.formats.svg.placeholder.embed=FALSE, # save a placeholder for the SVG image?
                      export.formats.directory=NA,           # if exporting, which directory to export to (if not give, creates files in the temporary directory)
                      export.formats.html.template=NULL, export.formats.html.javascript=NULL, export.formats.html.css=NULL, # HTML, JavaScript and CSS templates for exporting HTML+SVG
                      generate.R.plot=TRUE                   # generate standard (base R) plot for plotting within R?
)
{
  .plot.CMA1plus(cma=x,
                 patients.to.plot=patients.to.plot,
                 duration=duration,
                 align.all.patients=align.all.patients,
                 align.first.event.at.zero=align.first.event.at.zero,
                 show.period=show.period,
                 period.in.days=period.in.days,
                 show.legend=show.legend,
                 legend.x=legend.x,
                 legend.y=legend.y,
                 legend.bkg.opacity=legend.bkg.opacity,
                 legend.cex=legend.cex,
                 legend.cex.title=legend.cex.title,
                 cex=cex,
                 cex.axis=cex.axis,
                 cex.lab=cex.lab,
                 show.cma=show.cma,
                 col.cats=col.cats,
                 unspecified.category.label=unspecified.category.label,
                 medication.groups.to.plot=medication.groups.to.plot,
                 medication.groups.separator.show=medication.groups.separator.show,
                 medication.groups.separator.lty=medication.groups.separator.lty,
                 medication.groups.separator.lwd=medication.groups.separator.lwd,
                 medication.groups.separator.color=medication.groups.separator.color,
                 medication.groups.allother.label=medication.groups.allother.label,
                 lty.event=lty.event,
                 lwd.event=lwd.event,
                 pch.start.event=pch.start.event,
                 pch.end.event=pch.end.event,
                 show.event.intervals=show.event.intervals,
                 col.na=col.na,
                 print.CMA=print.CMA,
                 CMA.cex=CMA.cex,
                 plot.CMA=plot.CMA,
                 CMA.plot.ratio=CMA.plot.ratio,
                 CMA.plot.col=CMA.plot.col,
                 CMA.plot.border=CMA.plot.border,
                 CMA.plot.bkg=CMA.plot.bkg,
                 CMA.plot.text=CMA.plot.text,
                 highlight.followup.window=highlight.followup.window,
                 followup.window.col=followup.window.col,
                 highlight.observation.window=highlight.observation.window,
                 observation.window.col=observation.window.col,
                 observation.window.density=observation.window.density,
                 observation.window.angle=observation.window.angle,
                 observation.window.opacity=observation.window.opacity,
                 show.real.obs.window.start=show.real.obs.window.start,
                 real.obs.window.density=real.obs.window.density,
                 real.obs.window.angle=real.obs.window.angle,
                 print.dose=print.dose,
                 cex.dose=cex.dose,
                 print.dose.outline.col=print.dose.outline.col,
                 print.dose.centered=print.dose.centered,
                 plot.dose=plot.dose,
                 lwd.event.max.dose=lwd.event.max.dose,
                 plot.dose.lwd.across.medication.classes=plot.dose.lwd.across.medication.classes,
                 bw.plot=bw.plot,
                 force.draw.text=force.draw.text,
                 min.plot.size.in.characters.horiz=min.plot.size.in.characters.horiz,
                 min.plot.size.in.characters.vert=min.plot.size.in.characters.vert,
                 max.patients.to.plot=max.patients.to.plot,
                 export.formats=export.formats,
                 export.formats.fileprefix=export.formats.fileprefix,
                 export.formats.height=export.formats.height,
                 export.formats.width=export.formats.width,
                 export.formats.save.svg.placeholder=export.formats.save.svg.placeholder,
                 export.formats.svg.placeholder.type=export.formats.svg.placeholder.type,
                 export.formats.svg.placeholder.embed=export.formats.svg.placeholder.embed,
                 export.formats.html.template=export.formats.html.template,
                 export.formats.html.javascript=export.formats.html.javascript,
                 export.formats.html.css=export.formats.html.css,
                 export.formats.directory=export.formats.directory,
                 generate.R.plot=generate.R.plot,
                 ...)
}




#' CMA2 and CMA4 constructors.
#'
#' Constructs a CMA (continuous multiple-interval measures of medication
#' availability/gaps) type 2 or type 4 object.
#'
#' \code{CMA2} considers the total number of days with medication supplied in
#' all medication events in the observation window, including the last event.
#' \code{CMA4} is identical to \code{CMA2} except that it is capped at 100\%.
#'
#' The formula is
#' \deqn{(number of days supply including last event) / (first to last event)}
#' Thus, the durations of all events are added up, possibly resulting in an CMA
#' estimate (much) bigger than 1.0 (100\%)
#'
#' \code{CMA2} and \code{\link{CMA1}} differ in the inclusion or not of the last
#' event.
#'
#' @param data A \emph{\code{data.frame}} containing the events used to compute
#' the CMA. Must contain, at a minimum, the patient unique ID, the event date
#' and duration, and might also contain the daily dosage and medication type
#' (the actual column names are defined in the following four parameters).
#' @param ID.colname A \emph{string}, the name of the column in \code{data}
#' containing the unique patient ID; must be present.
#' @param event.date.colname A \emph{string}, the name of the column in
#' \code{data} containing the start date of the event (in the format given in
#' the \code{date.format} parameter); must be present.
#' @param event.duration.colname A \emph{string}, the name of the column in
#' \code{data} containing the event duration (in days); must be present.
#' @param medication.groups A \emph{vector} of characters defining medication
#' groups or the name of a column in \code{data} that defines such groups.
#' The names of the vector are the medication group unique names, while
#' the content defines them as logical expressions. While the names can be any
#' string of characters except "\}", it is recommended to stick to the rules for
#' defining vector names in \code{R}. For example,
#' \code{c("A"="CATEGORY == 'medA'", "AA"="{A} & PERDAY < 4"} defines two
#' medication groups: \emph{A} which selects all events of type "medA", and
#' \emph{B} which selects all events already defined by "A" but with a daily
#' dose lower than 4. If \code{NULL}, no medication groups are defined. If
#' medication groups are defined, there is one CMA estimate for each group;
#' moreover, there is a special group \emph{__ALL_OTHERS__} automatically defined
#' containing all observations \emph{not} covered by any of the explicitly defined
#' groups.
#' @param flatten.medication.groups \emph{Logical}, if \code{FALSE} (the default)
#' then the \code{CMA} and \code{event.info} components of the object are lists
#' with one medication group per element; otherwise, they are \code{data.frame}s
#' with an extra column containing the medication group (its name is given by
#' \code{medication.groups.colname}).
#' @param medication.groups.colname a \emph{string} (defaults to ".MED_GROUP_ID")
#' giving the name of the column storing the group name when
#' \code{flatten.medication.groups} is \code{TRUE}.
#' @param followup.window.start If a \emph{\code{Date}} object, it represents
#' the actual start date of the follow-up window; if a \emph{string} it is the
#' name of the column in \code{data} containing the start date of the follow-up
#' window either as the numbers of \code{followup.window.start.unit} units after
#' the first event (the column must be of type \code{numeric}) or as actual
#' dates (in which case the column must be of type \code{Date} or a string
#' that conforms to the format specified in \code{date.format}); if a
#' \emph{number} it is the number of time units defined in the
#' \code{followup.window.start.unit} parameter after the begin of the
#' participant's first event; or \code{NA} if not defined.
#' @param followup.window.start.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.start} refers to (when a number), or
#' \code{NA} if not defined.
#' @param followup.window.start.per.medication.group a \emph{logical}: if there are
#' medication groups defined and this is \code{TRUE}, then the first event
#' considered for the follow-up window start is relative to each medication group
#' separately, otherwise (the default) it is relative to the patient.
#' @param followup.window.duration either a \emph{number} representing the
#' duration of the follow-up window in the time units given in
#' \code{followup.window.duration.unit}, or a \emph{string} giving the column
#' containing these numbers. Should represent a period for which relevant
#' medication events are recorded accurately (e.g. not extend after end of
#' relevant treatment, loss-to-follow-up or change to a health care provider
#' not covered by the database).
#' @param followup.window.duration.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.duration} refers to, or \code{NA} if not
#' defined.
#' @param observation.window.start,observation.window.start.unit,observation.window.duration,observation.window.duration.unit the definition of the observation window
#' (see the follow-up window parameters above for details).
#' @param date.format A \emph{string} giving the format of the dates used in the
#' \code{data} and the other parameters; see the \code{format} parameters of the
#' \code{\link[base]{as.Date}} function for details (NB, this concerns only the
#' dates given as strings and not as \code{Date} objects).
#' @param summary Metadata as a \emph{string}, briefly describing this CMA.
#' @param event.interval.colname A \emph{string}, the name of a newly-created
#' column storing the number of days between the start of the current event and
#' the start of the next one; the default value "event.interval" should be
#' changed only if there is a naming conflict with a pre-existing
#' "event.interval" column in \code{event.info}.
#' @param gap.days.colname A \emph{string}, the name of a newly-created column
#' storing the number of days when medication was not available (i.e., the
#' "gap days"); the default value "gap.days" should be changed only if there is
#' a naming conflict with a pre-existing "gap.days" column in \code{event.info}.
#' @param force.NA.CMA.for.failed.patients \emph{Logical} describing how the
#' patients for which the CMA estimation fails are treated: if \code{TRUE}
#' they are returned with an \code{NA} CMA estimate, while for
#' \code{FALSE} they are omitted.
#' @param parallel.backend Can be "none" (the default) for single-threaded
#' execution, "multicore"  (using \code{mclapply} in package \code{parallel})
#' for multicore processing (NB. not currently implemented on MS Windows and
#' automatically falls back on "snow" on this platform),  or "snow",
#' "snow(SOCK)" (equivalent to "snow"), "snow(MPI)" or "snow(NWS)" specifying
#' various types of SNOW clusters (can be on the local machine or more complex
#' setups -- please see the documentation of package \code{snow} for details;
#' the last two require packages \code{Rmpi} and \code{nws}, respectively, not
#' automatically installed with \code{AdhereR}).
#' @param parallel.threads Can be "auto" (for \code{parallel.backend} ==
#' "multicore", defaults to the number of cores in the system as given by
#' \code{options("cores")}, while for \code{parallel.backend} == "snow",
#' defaults to 2), a strictly positive integer specifying the number of parallel
#' threads, or a more complex specification of the SNOW cluster nodes for
#' \code{parallel.backend} == "snow" (see the documentation of package
#' \code{snow} for details).
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any
#' warnings.
#' @param arguments.that.should.not.be.defined a \emph{list} of argument names
#' and pre-defined valuesfor which a warning should be thrown if passed to the
#' function.
#' @param ... other possible parameters
#' @return An \code{S3} object of class \code{CMA2} (derived from \code{CMA0})
#' with the following fields:
#' \itemize{
#'  \item \code{data} The actual event data, as given by the \code{data}
#'  parameter.
#'  \item \code{ID.colname} the name of the column in \code{data} containing the
#'  unique patient ID, as given by the \code{ID.colname} parameter.
#'  \item \code{event.date.colname} the name of the column in \code{data}
#'  containing the start date of the event (in the format given in the
#'  \code{date.format} parameter), as given by the \code{event.date.colname}
#'  parameter.
#'  \item \code{event.duration.colname} the name of the column in \code{data}
#'  containing the event duration (in days), as given by the
#'  \code{event.duration.colname} parameter.
#'  \item \code{event.daily.dose.colname} the name of the column in \code{data}
#'  containing the prescribed daily dose, as given by the
#'  \code{event.daily.dose.colname} parameter.
#'  \item \code{medication.class.colname} the name of the column in \code{data}
#'  containing the classes/types/groups of medication, as given by the
#'  \code{medication.class.colname} parameter.
#'  \item \code{followup.window.start} the beginning of the follow-up window, as
#'  given by the \code{followup.window.start} parameter.
#'  \item \code{followup.window.start.unit} the time unit of the
#'  \code{followup.window.start}, as given by the
#'  \code{followup.window.start.unit} parameter.
#'  \item \code{followup.window.duration} the duration of the follow-up window,
#'  as given by the \code{followup.window.duration} parameter.
#'  \item \code{followup.window.duration.unit} the time unit of the
#'  \code{followup.window.duration}, as given by the
#'  \code{followup.window.duration.unit} parameter.
#'  \item \code{observation.window.start} the beginning of the observation
#'  window, as given by the \code{observation.window.start} parameter.
#'  \item \code{observation.window.start.unit} the time unit of the
#'  \code{observation.window.start}, as given by the
#'  \code{observation.window.start.unit} parameter.
#'  \item \code{observation.window.duration} the duration of the observation
#'  window, as given by the \code{observation.window.duration} parameter.
#'  \item \code{observation.window.duration.unit} the time unit of the
#'  \code{observation.window.duration}, as given by the
#'  \code{observation.window.duration.unit} parameter.
#'  \item \code{date.format} the format of the dates, as given by the
#'  \code{date.format} parameter.
#'  \item \code{summary} the metadata, as given by the \code{summary} parameter.
#'  \item \code{event.info} the \code{data.frame} containing the event info
#'  (irrelevant for most users; see \code{\link{compute.event.int.gaps}} for
#'  details).
#'  \item \code{CMA} the \code{data.frame} containing the actual \code{CMA}
#'  estimates for each participant (the \code{ID.colname} column).
#' }
#' Please note that if \code{medication.groups} are defined, then the \code{CMA}
#' and \code{event.info} are named lists, each element containing the CMA and
#' event.info corresponding to a single medication group (the element's name).
#' @seealso CMAs 1 to 8 are defined in:
#'
#' Vollmer, W. M., Xu, M., Feldstein, A., Smith, D., Waterbury, A., & Rand, C.
#' (2012). Comparison of pharmacy-based measures of medication adherence.
#' \emph{BMC Health Services Research}, \strong{12}, 155.
#' \url{http://doi.org/10.1186/1472-6963-12-155}.
#'
#' @examples
#' \dontrun{
#' cma2 <- CMA2(data=med.events,
#'              ID.colname="PATIENT_ID",
#'              event.date.colname="DATE",
#'              event.duration.colname="DURATION",
#'              followup.window.start=30,
#'              observation.window.start=30,
#'              observation.window.duration=365,
#'              date.format="%m/%d/%Y"
#'             );
#' cma4 <- CMA4(data=med.events,
#'              ID.colname="PATIENT_ID",
#'              event.date.colname="DATE",
#'              event.duration.colname="DURATION",
#'              followup.window.start=30,
#'              observation.window.start=30,
#'              observation.window.duration=365,
#'              date.format="%m/%d/%Y"
#'             );}
#' @export
CMA2 <- function( data=NULL, # the data used to compute the CMA on
                  # Important columns in the data
                  ID.colname=NA, # the name of the column containing the unique patient ID (NA = undefined)
                  event.date.colname=NA, # the start date of the event in the date.format format (NA = undefined)
                  event.duration.colname=NA, # the event duration in days (NA = undefined)
                  # Groups of medication classes:
                  medication.groups=NULL, # a named vector of medication group definitions, the name of a column in the data that defines the groups, or NULL
                  flatten.medication.groups=FALSE, medication.groups.colname=".MED_GROUP_ID", # if medication.groups were defined, return CMAs and event.info as single data.frame?
                  # The follow-up window:
                  followup.window.start=0, # if a number is the earliest event per participant date plus number of units, or a Date object, or a column name in data (NA = undefined)
                  followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  followup.window.start.per.medication.group=FALSE, # if there are medication groups and this is TRUE, then the first event is relative to each medication group separately, otherwise is relative to the patient
                  followup.window.duration=365*2, # the duration of the follow-up window in the time units given below (NA = undefined)
                  followup.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)  (NA = undefined)
                  # The observation window (embedded in the follow-up window):
                  observation.window.start=0, # the number of time units relative to followup.window.start (NA = undefined)
                  observation.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  observation.window.duration=365*2, # the duration of the observation window in time units (NA = undefined)
                  observation.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  # Date format:
                  date.format="%m/%d/%Y", # the format of the dates used in this function (NA = undefined)
                  # Comments and metadata:
                  summary=NA,
                  # The description of the output (added) columns:
                  event.interval.colname="event.interval", # contains number of days between the start of current event and the start of the next
                  gap.days.colname="gap.days", # contains the number of days when medication was not available
                  # Dealing with failed estimates:
                  force.NA.CMA.for.failed.patients=TRUE, # force the failed patients to have NA CM estimate?
                  # Parallel processing:
                  parallel.backend=c("none","multicore","snow","snow(SOCK)","snow(MPI)","snow(NWS)")[1], # parallel backend to use
                  parallel.threads="auto", # specification (or number) of parallel threads
                  # Misc:
                  suppress.warnings=FALSE,
                  arguments.that.should.not.be.defined=c("carryover.within.obs.window"=FALSE,
                                                         "carryover.into.obs.window"=FALSE,
                                                         "carry.only.for.same.medication"=FALSE,
                                                         "consider.dosage.change"=FALSE), # the list of argument names and values for which a warning should be thrown if passed to the function
                  ...
                )
{
  # The summary:
  if( is.na(summary) ) summary <- "The ratio of days with medication available in the observation window including the last event; durations of all events added up and divided by number of days from first event to end of observation window, possibly resulting in a value >1.0";

  # Arguments that should not have been passed:
  if( !suppress.warnings && !is.null(arguments.that.should.not.be.defined) )
  {
    # Get the actual list of arguments (including in the ...); the first is the function's own name:
    args.list <- as.list(match.call(expand.dots = TRUE));
    args.mathing <- (names(arguments.that.should.not.be.defined) %in% names(args.list)[-1]);
    if( any(args.mathing) )
    {
      for( i in which(args.mathing) )
      {
        .report.ewms(paste0("Please note that '",args.list[[1]],"' overrides argument '",names(arguments.that.should.not.be.defined)[i],"' with value '",arguments.that.should.not.be.defined[i],"'!\n"), "warning", "CMA2", "AdhereR");
      }
    }
  }

  # Create the CMA0 object:
  ret.val <- CMA0(data=data,
                  ID.colname=ID.colname,
                  event.date.colname=event.date.colname,
                  event.duration.colname=event.duration.colname,
                  medication.groups=medication.groups,
                  flatten.medication.groups=flatten.medication.groups,
                  medication.groups.colname=medication.groups.colname,
                  followup.window.start=followup.window.start,
                  followup.window.start.unit=followup.window.start.unit,
                  followup.window.start.per.medication.group=followup.window.start.per.medication.group,
                  followup.window.duration=followup.window.duration,
                  followup.window.duration.unit=followup.window.duration.unit,
                  observation.window.start=observation.window.start,
                  observation.window.start.unit=observation.window.start.unit,
                  observation.window.duration=observation.window.duration,
                  observation.window.duration.unit=observation.window.duration.unit,
                  date.format=date.format,
                  summary=summary,
                  suppress.warnings=suppress.warnings);
  if( is.null(ret.val) ) return (NULL); # some error upstream
  # The followup.window.start and observation.window.start might have been converted to Date:
  followup.window.start <- ret.val$followup.window.start; observation.window.start <- ret.val$observation.window.start;

  # The workhorse auxiliary function: For a given (subset) of data, compute the event intervals and gaps:
  .workhorse.function <- function(data=NULL,
                                  ID.colname=NULL,
                                  event.date.colname=NULL,
                                  event.duration.colname=NULL,
                                  event.daily.dose.colname=NULL,
                                  medication.class.colname=NULL,
                                  event.interval.colname=NULL,
                                  gap.days.colname=NULL,
                                  carryover.within.obs.window=NULL,
                                  carryover.into.obs.window=NULL,
                                  carry.only.for.same.medication=NULL,
                                  consider.dosage.change=NULL,
                                  followup.window.start=NULL,
                                  followup.window.start.unit=NULL,
                                  followup.window.duration=NULL,
                                  followup.window.duration.unit=NULL,
                                  observation.window.start=NULL,
                                  observation.window.start.unit=NULL,
                                  observation.window.duration=NULL,
                                  observation.window.duration.unit=NULL,
                                  date.format=NULL,
                                  suppress.warnings=NULL
  )
  {
    # Auxiliary internal function: Compute the CMA for a given patient:
    .process.patient <- function(data4ID)
    {
      sel.data4ID <- data4ID[ !.EVENT.STARTS.BEFORE.OBS.WINDOW & !.EVENT.STARTS.AFTER.OBS.WINDOW, ]; # select the events within the observation window only
      n.events <- nrow(sel.data4ID); # cache number of events
      if( n.events < 1 || sel.data4ID$.DATE.as.Date[1] > sel.data4ID$.OBS.END.DATE[1] )
      {
        # For less than one event or when the first event is on the last day of the observation window, CMA2 does not make sense
        return (NA_real_);
      } else
      {
        # Otherwise, the sum of durations of the events divided by the number of days between the first event and the end of the observation window
        return (as.numeric(sum(sel.data4ID[, get(event.duration.colname)],na.rm=TRUE) /
                                 (as.numeric(difftime(sel.data4ID$.OBS.END.DATE[1], sel.data4ID$.DATE.as.Date[1], units="days")))));
      }
    }

    # Call the compute.event.int.gaps() function and use the results:
    event.info <- compute.event.int.gaps(data=as.data.frame(data),
                                         ID.colname=ID.colname,
                                         event.date.colname=event.date.colname,
                                         event.duration.colname=event.duration.colname,
                                         event.daily.dose.colname=event.daily.dose.colname,
                                         medication.class.colname=medication.class.colname,
                                         event.interval.colname=event.interval.colname,
                                         gap.days.colname=gap.days.colname,
                                         carryover.within.obs.window=carryover.within.obs.window,
                                         carryover.into.obs.window=carryover.into.obs.window,
                                         carry.only.for.same.medication=carry.only.for.same.medication,
                                         consider.dosage.change=consider.dosage.change,
                                         followup.window.start=followup.window.start,
                                         followup.window.start.unit=followup.window.start.unit,
                                         followup.window.duration=followup.window.duration,
                                         followup.window.duration.unit=followup.window.duration.unit,
                                         observation.window.start=observation.window.start,
                                         observation.window.start.unit=observation.window.start.unit,
                                         observation.window.duration=observation.window.duration,
                                         observation.window.duration.unit=observation.window.duration.unit,
                                         date.format=date.format,
                                         keep.window.start.end.dates=TRUE,
                                         parallel.backend="none", # make sure this runs sequentially!
                                         parallel.threads=1,
                                         suppress.warnings=suppress.warnings,
                                         return.data.table=TRUE);
    if( is.null(event.info) ) return (list("CMA"=NA, "event.info"=NULL));

    CMA <- event.info[, .process.patient(.SD), by=ID.colname];
    return (list("CMA"=CMA, "event.info"=event.info));
  }

  ret.val <- .cma.skeleton(data=data,
                           ret.val=ret.val,
                           cma.class.name=c("CMA2","CMA1"),

                           ID.colname=ID.colname,
                           event.date.colname=event.date.colname,
                           event.duration.colname=event.duration.colname,
                           event.daily.dose.colname=NA, # not relevant
                           medication.class.colname=NA, # not relevant
                           event.interval.colname=event.interval.colname,
                           gap.days.colname=gap.days.colname,
                           carryover.within.obs.window=FALSE, # if TRUE consider the carry-over within the observation window
                           carryover.into.obs.window=FALSE, # if TRUE consider the carry-over from before the starting date of the observation window
                           carry.only.for.same.medication=FALSE, # if TRUE the carry-over applies only across medication of same type
                           consider.dosage.change=FALSE, # if TRUE carry-over is adjusted to reflect changes in dosage
                           followup.window.start=followup.window.start,
                           followup.window.start.unit=followup.window.start.unit,
                           followup.window.duration=followup.window.duration,
                           followup.window.duration.unit=followup.window.duration.unit,
                           observation.window.start=observation.window.start,
                           observation.window.start.unit=observation.window.start.unit,
                           observation.window.duration=observation.window.duration,
                           observation.window.duration.unit=observation.window.duration.unit,
                           date.format=date.format,

                           flatten.medication.groups=flatten.medication.groups,
                           followup.window.start.per.medication.group=followup.window.start.per.medication.group,

                           suppress.warnings=suppress.warnings,
                           force.NA.CMA.for.failed.patients=force.NA.CMA.for.failed.patients,
                           parallel.backend=parallel.backend,
                           parallel.threads=parallel.threads,
                           .workhorse.function=.workhorse.function);

  return (ret.val);
}

#' @rdname print.CMA0
#' @export
print.CMA2 <- function(...) print.CMA0(...)

#' @rdname plot.CMA1
#' @export
plot.CMA2 <- function(...) .plot.CMA1plus(...)






#' @rdname CMA1
#' @export
CMA3 <- function( data=NULL, # the data used to compute the CMA on
                  # Important columns in the data
                  ID.colname=NA, # the name of the column containing the unique patient ID (NA = undefined)
                  event.date.colname=NA, # the start date of the event in the date.format format (NA = undefined)
                  event.duration.colname=NA, # the event duration in days (NA = undefined)
                  # Groups of medication classes:
                  medication.groups=NULL, # a named vector of medication group definitions, the name of a column in the data that defines the groups, or NULL
                  flatten.medication.groups=FALSE, medication.groups.colname=".MED_GROUP_ID", # if medication.groups were defined, return CMAs and event.info as single data.frame?
                  # The follow-up window:
                  followup.window.start=0, # if a number is the earliest event per participant date + number of units, or a Date object, or a column name in data (NA = undefined)
                  followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  followup.window.start.per.medication.group=FALSE, # if there are medication groups and this is TRUE, then the first event is relative to each medication group separately, otherwise is relative to the patient
                  followup.window.duration=365*2, # the duration of the follow-up window in the time units given below (NA = undefined)
                  followup.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)  (NA = undefined)
                  # The observation window (embedded in the follow-up window):
                  observation.window.start=0, # the number of time units relative to followup.window.start (NA = undefined)
                  observation.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  observation.window.duration=365*2, # the duration of the observation window in time units (NA = undefined)
                  observation.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  # Date format:
                  date.format="%m/%d/%Y", # the format of the dates used in this function (NA = undefined)
                  # Comments and metadata:
                  summary=NA,
                  # The description of the output (added) columns:
                  event.interval.colname="event.interval", # contains number of days between the start of current event and the start of the next
                  gap.days.colname="gap.days", # contains the number of days when medication was not available
                  # Dealing with failed estimates:
                  force.NA.CMA.for.failed.patients=TRUE, # force the failed patients to have NA CM estimate?
                  # Parallel processing:
                  parallel.backend=c("none","multicore","snow","snow(SOCK)","snow(MPI)","snow(NWS)")[1], # parallel backend to use
                  parallel.threads="auto", # specification (or number) of parallel threads
                  # Misc:
                  suppress.warnings=FALSE,
                  arguments.that.should.not.be.defined=c("carryover.within.obs.window"=FALSE,
                                                         "carryover.into.obs.window"=FALSE,
                                                         "carry.only.for.same.medication"=FALSE,
                                                         "consider.dosage.change"=FALSE), # the list of argument names and values for which a warning should be thrown if passed to the function
                  ...
                )
{
  # The summary:
  if( is.na(summary) ) summary <- "The ratio of days with medication available in the observation window including the last event; durations of all events added up and divided by number of days from first to last event, then capped at 1.0";

  # Arguments that should not have been passed:
  if( !suppress.warnings && !is.null(arguments.that.should.not.be.defined) )
  {
    # Get the actual list of arguments (including in the ...); the first is the function's own name:
    args.list <- as.list(match.call(expand.dots = TRUE));
    args.mathing <- (names(arguments.that.should.not.be.defined) %in% names(args.list)[-1]);
    if( any(args.mathing) )
    {
      for( i in which(args.mathing) )
      {
        .report.ewms(paste0("Please note that '",args.list[[1]],"' overrides argument '",names(arguments.that.should.not.be.defined)[i],"' with value '",arguments.that.should.not.be.defined[i],"'!\n"), "warning", "CMA3", "AdhereR");
      }
    }
  }

  # Create the CMA1 object:
  ret.val <- CMA1(data=data,
                  ID.colname=ID.colname,
                  event.date.colname=event.date.colname,
                  event.duration.colname=event.duration.colname,
                  medication.groups=medication.groups,
                  flatten.medication.groups=flatten.medication.groups,
                  medication.groups.colname=medication.groups.colname,
                  followup.window.start=followup.window.start,
                  followup.window.start.unit=followup.window.start.unit,
                  followup.window.start.per.medication.group=followup.window.start.per.medication.group,
                  followup.window.duration=followup.window.duration,
                  followup.window.duration.unit=followup.window.duration.unit,
                  observation.window.start=observation.window.start,
                  observation.window.start.unit=observation.window.start.unit,
                  observation.window.duration=observation.window.duration,
                  observation.window.duration.unit=observation.window.duration.unit,
                  date.format=date.format,
                  summary=summary,
                  event.interval.colname=event.interval.colname,
                  gap.days.colname=gap.days.colname,
                  force.NA.CMA.for.failed.patients=force.NA.CMA.for.failed.patients,
                  parallel.backend=parallel.backend,
                  parallel.threads=parallel.threads,
                  suppress.warnings=suppress.warnings,
                  arguments.that.should.not.be.defined=arguments.that.should.not.be.defined);
  if( is.null(ret.val) ) return (NULL); # some error upstream

  # Cap the CMA at 1.0:
  if( !is.null(ret.val$CMA) )
  {
    if( inherits(ret.val$CMA, "data.frame") )
    {
      ret.val$CMA$CMA <- pmin(1, ret.val$CMA$CMA);
    } else if( is.list(ret.val$CMA) && length(ret.val$CMA) > 0 )
    {
      for( i in 1:length(ret.val$CMA) ) if( !is.null(ret.val$CMA[[i]]) ) ret.val$CMA[[i]]$CMA <- pmin(1, ret.val$CMA[[i]]$CMA);
    }
  }

  # Convert to data.frame and return:
  class(ret.val) <- c("CMA3", class(ret.val));
  return (ret.val);
}

#' @rdname print.CMA0
#' @export
print.CMA3 <- function(...) print.CMA0(...)

#' @rdname plot.CMA1
#' @export
plot.CMA3 <- function(...) .plot.CMA1plus(...)





#' @rdname CMA2
#' @export
CMA4 <- function( data=NULL, # the data used to compute the CMA on
                  # Important columns in the data
                  ID.colname=NA, # the name of the column containing the unique patient ID (NA = undefined)
                  event.date.colname=NA, # the start date of the event in the date.format format (NA = undefined)
                  event.duration.colname=NA, # the event duration in days (NA = undefined)
                  # Groups of medication classes:
                  medication.groups=NULL, # a named vector of medication group definitions, the name of a column in the data that defines the groups, or NULL
                  flatten.medication.groups=FALSE, medication.groups.colname=".MED_GROUP_ID", # if medication.groups were defined, return CMAs and event.info as single data.frame?
                  # The follow-up window:
                  followup.window.start=0, # if a number is the earliest event per participant date plus number of units, or a Date object, or a column name in data (NA = undefined)
                  followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  followup.window.start.per.medication.group=FALSE, # if there are medication groups and this is TRUE, then the first event is relative to each medication group separately, otherwise is relative to the patient
                  followup.window.duration=365*2, # the duration of the follow-up window in the time units given below (NA = undefined)
                  followup.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)  (NA = undefined)
                  # The observation window (embedded in the follow-up window):
                  observation.window.start=0, # the number of time units relative to followup.window.start (NA = undefined)
                  observation.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  observation.window.duration=365*2, # the duration of the observation window in time units (NA = undefined)
                  observation.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  # Date format:
                  date.format="%m/%d/%Y", # the format of the dates used in this function (NA = undefined)
                  # Comments and metadata:
                  summary=NA,
                  # The description of the output (added) columns:
                  event.interval.colname="event.interval", # contains number of days between the start of current event and the start of the next
                  gap.days.colname="gap.days", # contains the number of days when medication was not available
                  # Dealing with failed estimates:
                  force.NA.CMA.for.failed.patients=TRUE, # force the failed patients to have NA CM estimate?
                  # Parallel processing:
                  parallel.backend=c("none","multicore","snow","snow(SOCK)","snow(MPI)","snow(NWS)")[1], # parallel backend to use
                  parallel.threads="auto", # specification (or number) of parallel threads
                  # Misc:
                  suppress.warnings=FALSE,
                  arguments.that.should.not.be.defined=c("carryover.within.obs.window"=FALSE,
                                                         "carryover.into.obs.window"=FALSE,
                                                         "carry.only.for.same.medication"=FALSE,
                                                         "consider.dosage.change"=FALSE), # the list of argument names and values for which a warning should be thrown if passed to the function
                  ...
                )
{
  # The summary:
  if( is.na(summary) ) summary <- "The ratio of days with medication available in the observation window including the last event; durations of all events added up and divided by number of days from first event to end of observation window, then capped at 1.0";

  # Arguments that should not have been passed:
  if( !suppress.warnings && !is.null(arguments.that.should.not.be.defined) )
  {
    # Get the actual list of arguments (including in the ...); the first is the function's own name:
    args.list <- as.list(match.call(expand.dots = TRUE));
    args.mathing <- (names(arguments.that.should.not.be.defined) %in% names(args.list)[-1]);
    if( any(args.mathing) )
    {
      for( i in which(args.mathing) )
      {
        .report.ewms(paste0("Please note that '",args.list[[1]],"' overrides argument '",names(arguments.that.should.not.be.defined)[i],"' with value '",arguments.that.should.not.be.defined[i],"'!\n"), "warning", "CMA4", "AdhereR");
      }
    }
  }

  # Create the CMA2 object:
  ret.val <- CMA2(data=data,
                  ID.colname=ID.colname,
                  event.date.colname=event.date.colname,
                  event.duration.colname=event.duration.colname,
                  medication.groups=medication.groups,
                  flatten.medication.groups=flatten.medication.groups,
                  medication.groups.colname=medication.groups.colname,
                  followup.window.start=followup.window.start,
                  followup.window.start.unit=followup.window.start.unit,
                  followup.window.start.per.medication.group=followup.window.start.per.medication.group,
                  followup.window.duration=followup.window.duration,
                  followup.window.duration.unit=followup.window.duration.unit,
                  observation.window.start=observation.window.start,
                  observation.window.start.unit=observation.window.start.unit,
                  observation.window.duration=observation.window.duration,
                  observation.window.duration.unit=observation.window.duration.unit,
                  date.format=date.format,
                  summary=summary,
                  event.interval.colname=event.interval.colname,
                  gap.days.colname=gap.days.colname,
                  force.NA.CMA.for.failed.patients=force.NA.CMA.for.failed.patients,
                  parallel.backend=parallel.backend,
                  parallel.threads=parallel.threads,
                  suppress.warnings=suppress.warnings,
                  arguments.that.should.not.be.defined=arguments.that.should.not.be.defined);
  if( is.null(ret.val) ) return (NULL); # some error upstream

  # Cap the CMA at 1.0:
  if( !is.null(ret.val$CMA) )
  {
    if( inherits(ret.val$CMA, "data.frame") )
    {
      ret.val$CMA$CMA <- pmin(1, ret.val$CMA$CMA);
    } else if( is.list(ret.val$CMA) && length(ret.val$CMA) > 0 )
    {
      for( i in 1:length(ret.val$CMA) ) if( !is.null(ret.val$CMA[[i]]) ) ret.val$CMA[[i]]$CMA <- pmin(1, ret.val$CMA[[i]]$CMA);
    }
  }

  # Convert to data.frame and return:
  class(ret.val) <- c("CMA4", class(ret.val));
  return (ret.val);
}

#' @rdname print.CMA0
#' @export
print.CMA4 <- function(...) print.CMA0(...)

#' @rdname plot.CMA1
#' @export
plot.CMA4 <- function(...) .plot.CMA1plus(...)





#' CMA5 constructor.
#'
#' Constructs a CMA (continuous multiple-interval measures of medication
#' availability/gaps) type 5 object.
#'
#' \code{CMA5} assumes that, within the observation window, the medication is
#' used as prescribed and new medication is "banked" until needed (oversupply
#' from previous events is used first, followed new medication supply).
#' It computes days of theoretical use by extracting the total number of gap
#' days from the total time interval between the first and the last event,
#' accounting for carry over for all medication events within the observation
#' window.
#' Thus, it accounts for timing within the observation window, and excludes the
#' remaining supply at the start of the last event within the observation window.
#'
#' The formula is
#' \deqn{(number of days of theoretical use) / (first to last event)}
#'
#' Observations:
#' \itemize{
#'  \item the \code{carry.only.for.same.medication} parameter controls the
#'  transmission of carry-over across medication changes, producing a "standard"
#'  \code{CMA5} (default value is FALSE), and an "alternative" \code{CMA5b},
#'  respectively;
#'  \item the \code{consider.dosage.change} parameter controls if dosage changes
#'  are taken into account, i.e. if set as TRUE and a new medication event has a
#'  different daily dosage recommendation, carry-over is recomputed assuming
#'  medication use according to the new prescribed dosage (default value is FALSE).
#' }
#'
#' @param data A \emph{\code{data.frame}} containing the medication events used
#' to compute the CMA. Must contain, at a minimum, the patient unique ID, the
#' event date and duration, and might also contain the daily dosage and
#' medication type (the actual column names are defined in the following four
#' parameters).
#' @param ID.colname A \emph{string}, the name of the column in \code{data}
#' containing the unique patient ID; must be present.
#' @param event.date.colname A \emph{string}, the name of the column in
#' \code{data} containing the start date of the event (in the format given in
#' the \code{date.format} parameter); must be present.
#' @param event.duration.colname A \emph{string}, the name of the column in
#' \code{data} containing the event duration (in days); must be present.
#' @param event.daily.dose.colname A \emph{string}, the name of the column in
#' \code{data} containing the prescribed daily dose, or \code{NA} if not defined.
#' @param medication.class.colname A \emph{string}, the name of the column in
#' \code{data} containing the medication type, or \code{NA} if not defined.
#' @param medication.groups A \emph{vector} of characters defining medication
#' groups or the name of a column in \code{data} that defines such groups.
#' The names of the vector are the medication group unique names, while
#' the content defines them as logical expressions. While the names can be any
#' string of characters except "\}", it is recommended to stick to the rules for
#' defining vector names in \code{R}. For example,
#' \code{c("A"="CATEGORY == 'medA'", "AA"="{A} & PERDAY < 4"} defines two
#' medication groups: \emph{A} which selects all events of type "medA", and
#' \emph{B} which selects all events already defined by "A" but with a daily
#' dose lower than 4. If \code{NULL}, no medication groups are defined. If
#' medication groups are defined, there is one CMA estimate for each group;
#' moreover, there is a special group \emph{__ALL_OTHERS__} automatically defined
#' containing all observations \emph{not} covered by any of the explicitly defined
#' groups.
#' @param flatten.medication.groups \emph{Logical}, if \code{FALSE} (the default)
#' then the \code{CMA} and \code{event.info} components of the object are lists
#' with one medication group per element; otherwise, they are \code{data.frame}s
#' with an extra column containing the medication group (its name is given by
#' \code{medication.groups.colname}).
#' @param medication.groups.colname a \emph{string} (defaults to ".MED_GROUP_ID")
#' giving the name of the column storing the group name when
#' \code{flatten.medication.groups} is \code{TRUE}.
#' @param carry.only.for.same.medication \emph{Logical}, if \code{TRUE}, the
#' carry-over applies only across medication of the same type.
#' @param consider.dosage.change \emph{Logical}, if \code{TRUE}, the carry-over
#' is adjusted to also reflect changes in dosage.
#' @param followup.window.start If a \emph{\code{Date}} object, it represents
#' the actual start date of the follow-up window; if a \emph{string} it is the
#' name of the column in \code{data} containing the start date of the follow-up
#' window either as the numbers of \code{followup.window.start.unit} units after
#' the first event (the column must be of type \code{numeric}) or as actual
#' dates (in which case the column must be of type \code{Date} or a string
#' that conforms to the format specified in \code{date.format}); if a
#' \emph{number} it is the number of time units defined in the
#' \code{followup.window.start.unit} parameter after the begin of the
#' participant's first event; or \code{NA} if not defined.
#' @param followup.window.start.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.start} refers to (when a number), or
#' \code{NA} if not defined.
#' @param followup.window.start.per.medication.group a \emph{logical}: if there are
#' medication groups defined and this is \code{TRUE}, then the first event
#' considered for the follow-up window start is relative to each medication group
#' separately, otherwise (the default) it is relative to the patient.
#' @param followup.window.duration either a \emph{number} representing the
#' duration of the follow-up window in the time units given in
#' \code{followup.window.duration.unit}, or a \emph{string} giving the column
#' containing these numbers. Should represent a period for which relevant
#' medication events are recorded accurately (e.g. not extend after end of
#' relevant treatment, loss-to-follow-up or change to a health care provider not
#' covered by the database).
#' @param followup.window.duration.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.duration} refers to, or \code{NA} if not
#' defined.
#' @param observation.window.start,observation.window.start.unit,observation.window.duration,observation.window.duration.unit the definition of the observation window
#' (see the follow-up window parameters above for details).
#' @param date.format A \emph{string} giving the format of the dates used in the
#' \code{data} and the other parameters; see the \code{format} parameters of the
#' \code{\link[base]{as.Date}} function for details (NB, this concerns only the
#' dates given as strings and not as \code{Date} objects).
#' @param summary Metadata as a \emph{string}, briefly describing this CMA.
#' @param event.interval.colname A \emph{string}, the name of a newly-created
#' column storing the number of days between the start of the current event and
#' the start of the next one; the default value "event.interval" should be
#' changed only if there is a naming conflict with a pre-existing
#' "event.interval" column in \code{event.info}.
#' @param gap.days.colname A \emph{string}, the name of a newly-created column
#' storing the number of days when medication was not available (i.e., the
#' "gap days"); the default value "gap.days" should be changed only if there is
#' a naming conflict with a pre-existing "gap.days" column in \code{event.info}.
#' @param force.NA.CMA.for.failed.patients \emph{Logical} describing how the
#' patients for which the CMA estimation fails are treated: if \code{TRUE}
#' they are returned with an \code{NA} CMA estimate, while for
#' \code{FALSE} they are omitted.
#' @param parallel.backend Can be "none" (the default) for single-threaded
#' execution, "multicore"  (using \code{mclapply} in package \code{parallel})
#' for multicore processing (NB. not currently implemented on MS Windows and
#' automatically falls back on "snow" on this platform),  or "snow",
#' "snow(SOCK)" (equivalent to "snow"), "snow(MPI)" or "snow(NWS)" specifying
#' various types of SNOW clusters (can be on the local machine or more complex
#' setups -- please see the documentation of package \code{snow} for details;
#' the last two require packages \code{Rmpi} and \code{nws}, respectively, not
#' automatically installed with \code{AdhereR}).
#' @param parallel.threads Can be "auto" (for \code{parallel.backend} ==
#' "multicore", defaults to the number of cores in the system as given by
#' \code{options("cores")}, while for \code{parallel.backend} == "snow",
#' defaults to 2), a strictly positive integer specifying the number of parallel
#' threads, or a more complex specification of the SNOW cluster nodes for
#' \code{parallel.backend} == "snow" (see the documentation of package
#' \code{snow} for details).
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any
#' warnings.
#' @param arguments.that.should.not.be.defined a \emph{list} of argument names
#' and pre-defined valuesfor which a warning should be thrown if passed to the
#' function.
#' @param ... other possible parameters
#' @return An \code{S3} object of class \code{CMA5} (derived from \code{CMA0})
#' with the following fields:
#' \itemize{
#'  \item \code{data} The actual event data, as given by the \code{data}
#'  parameter.
#'  \item \code{ID.colname} the name of the column in \code{data} containing the
#'  unique patient ID, as given by the \code{ID.colname} parameter.
#'  \item \code{event.date.colname} the name of the column in \code{data}
#'  containing the start date of the event (in the format given in the
#'  \code{date.format} parameter), as given by the \code{event.date.colname}
#'  parameter.
#'  \item \code{event.duration.colname} the name of the column in \code{data}
#'  containing the event duration (in days), as given by the
#'  \code{event.duration.colname} parameter.
#'  \item \code{event.daily.dose.colname} the name of the column in \code{data}
#'  containing the prescribed daily dose, as given by the
#'  \code{event.daily.dose.colname} parameter.
#'  \item \code{medication.class.colname} the name of the column in \code{data}
#'  containing the classes/types/groups of medication, as given by the
#'  \code{medication.class.colname} parameter.
#'  \item \code{carry.only.for.same.medication} whether the carry-over applies
#'  only across medication of the same type, as given by the
#'  \code{carry.only.for.same.medication} parameter.
#'  \item \code{consider.dosage.change} whether the carry-over is adjusted to
#'  reflect changes in dosage, as given by the \code{consider.dosage.change}
#'  parameter.
#'  \item \code{followup.window.start} the beginning of the follow-up window, as
#'  given by the \code{followup.window.start} parameter.
#'  \item \code{followup.window.start.unit} the time unit of the
#'  \code{followup.window.start}, as given by the
#'  \code{followup.window.start.unit} parameter.
#'  \item \code{followup.window.duration} the duration of the follow-up window,
#'  as given by the \code{followup.window.duration} parameter.
#'  \item \code{followup.window.duration.unit} the time unit of the
#'  \code{followup.window.duration}, as given by the
#'  \code{followup.window.duration.unit} parameter.
#'  \item \code{observation.window.start} the beginning of the observation
#'  window, as given by the \code{observation.window.start} parameter.
#'  \item \code{observation.window.start.unit} the time unit of the
#'  \code{observation.window.start}, as given by the
#'  \code{observation.window.start.unit} parameter.
#'  \item \code{observation.window.duration} the duration of the observation
#'  window, as given by the \code{observation.window.duration} parameter.
#'  \item \code{observation.window.duration.unit} the time unit of the
#'  \code{observation.window.duration}, as given by the
#'  \code{observation.window.duration.unit} parameter.
#'  \item \code{date.format} the format of the dates, as given by the
#'  \code{date.format} parameter.
#'  \item \code{summary} the metadata, as given by the \code{summary} parameter.
#'  \item \code{event.info} the \code{data.frame} containing the event info
#'  (irrelevant for most users; see \code{\link{compute.event.int.gaps}} for
#'  details).
#'  \item \code{CMA} the \code{data.frame} containing the actual \code{CMA}
#'  estimates for each participant (the \code{ID.colname} column).
#' }
#' Please note that if \code{medication.groups} are defined, then the \code{CMA}
#' and \code{event.info} are named lists, each element containing the CMA and
#' event.info corresponding to a single medication group (the element's name).
#' @seealso CMAs 1 to 8 are defined in:
#'
#' Vollmer, W. M., Xu, M., Feldstein, A., Smith, D., Waterbury, A., & Rand, C.
#' (2012). Comparison of pharmacy-based measures of medication adherence.
#' \emph{BMC Health Services Research}, \strong{12}, 155.
#' \url{http://doi.org/10.1186/1472-6963-12-155}.
#'
#' @examples
#' cma5 <- CMA5(data=med.events,
#'              ID.colname="PATIENT_ID",
#'              event.date.colname="DATE",
#'              event.duration.colname="DURATION",
#'              event.daily.dose.colname="PERDAY",
#'              medication.class.colname="CATEGORY",
#'              carry.only.for.same.medication=FALSE,
#'              consider.dosage.change=FALSE,
#'              followup.window.start=30,
#'              observation.window.start=30,
#'              observation.window.duration=365,
#'              date.format="%m/%d/%Y"
#'             );
#' @export
CMA5 <- function( data=NULL, # the data used to compute the CMA on
                  # Important columns in the data
                  ID.colname=NA, # the name of the column containing the unique patient ID (NA = undefined)
                  event.date.colname=NA, # the start date of the event in the date.format format (NA = undefined)
                  event.duration.colname=NA, # the event duration in days (NA = undefined)
                  event.daily.dose.colname=NA, # the prescribed daily dose (NA = undefined)
                  medication.class.colname=NA, # the classes/types/groups of medication (NA = undefined)
                  # Groups of medication classes:
                  medication.groups=NULL, # a named vector of medication group definitions, the name of a column in the data that defines the groups, or NULL
                  flatten.medication.groups=FALSE, medication.groups.colname=".MED_GROUP_ID", # if medication.groups were defined, return CMAs and event.info as single data.frame?
                  # Various types methods of computing gaps:
                  carry.only.for.same.medication=FALSE, # if TRUE the carry-over applies only across medication of same type (NA = undefined)
                  consider.dosage.change=FALSE, # if TRUE carry-over is adjusted to reflect changes in dosage (NA = undefined)
                  # The follow-up window:
                  followup.window.start=0, # if a number is the earliest event per participant date plus number of units, or a Date object, or a column name in data (NA = undefined)
                  followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  followup.window.start.per.medication.group=FALSE, # if there are medication groups and this is TRUE, then the first event is relative to each medication group separately, otherwise is relative to the patient
                  followup.window.duration=365*2, # the duration of the follow-up window in the time units given below (NA = undefined)
                  followup.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)  (NA = undefined)
                  # The observation window (embedded in the follow-up window):
                  observation.window.start=0, # the number of time units relative to followup.window.start (NA = undefined)
                  observation.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  observation.window.duration=365*2, # the duration of the observation window in time units (NA = undefined)
                  observation.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  # Date format:
                  date.format="%m/%d/%Y", # the format of the dates used in this function (NA = undefined)
                  # Comments and metadata:
                  summary=NA,
                  # The description of the output (added) columns:
                  event.interval.colname="event.interval", # contains number of days between the start of current event and the start of the next
                  gap.days.colname="gap.days", # contains the number of days when medication was not available
                  # Dealing with failed estimates:
                  force.NA.CMA.for.failed.patients=TRUE, # force the failed patients to have NA CM estimate?
                  # Parallel processing:
                  parallel.backend=c("none","multicore","snow","snow(SOCK)","snow(MPI)","snow(NWS)")[1], # parallel backend to use
                  parallel.threads="auto", # specification (or number) of parallel threads
                  # Misc:
                  suppress.warnings=FALSE,
                  arguments.that.should.not.be.defined=c("carryover.within.obs.window"=TRUE,
                                                         "carryover.into.obs.window"=FALSE), # the list of argument names and values for which a warning should be thrown if passed to the function
                  ...
                )
{
  # The summary:
  if( is.na(summary) ) summary <- "The ratio of days with medication available from first to last event; total number of gap days extracted from this time interval, then divided by the time interval, accounting for carry-over within observation window and excluding remaining supply";

  # Arguments that should not have been passed:
  if( !suppress.warnings && !is.null(arguments.that.should.not.be.defined) )
  {
    # Get the actual list of arguments (including in the ...); the first is the function's own name:
    args.list <- as.list(match.call(expand.dots = TRUE));
    args.mathing <- (names(arguments.that.should.not.be.defined) %in% names(args.list)[-1]);
    if( any(args.mathing) )
    {
      for( i in which(args.mathing) )
      {
        .report.ewms(paste0("Please note that '",args.list[[1]],"' overrides argument '",names(arguments.that.should.not.be.defined)[i],"' with value '",arguments.that.should.not.be.defined[i],"'!\n"), "warning", "CMA5", "AdhereR");
      }
    }
  }

  # Create the CMA0 object:
  ret.val <- CMA0(data=data,
                  ID.colname=ID.colname,
                  event.date.colname=event.date.colname,
                  event.duration.colname=event.duration.colname,
                  event.daily.dose.colname=event.daily.dose.colname,
                  medication.class.colname=medication.class.colname,
                  carry.only.for.same.medication=carry.only.for.same.medication,
                  consider.dosage.change=consider.dosage.change,
                  medication.groups=medication.groups,
                  flatten.medication.groups=flatten.medication.groups,
                  medication.groups.colname=medication.groups.colname,
                  followup.window.start=followup.window.start,
                  followup.window.start.unit=followup.window.start.unit,
                  followup.window.start.per.medication.group=followup.window.start.per.medication.group,
                  followup.window.duration=followup.window.duration,
                  followup.window.duration.unit=followup.window.duration.unit,
                  observation.window.start=observation.window.start,
                  observation.window.start.unit=observation.window.start.unit,
                  observation.window.duration=observation.window.duration,
                  observation.window.duration.unit=observation.window.duration.unit,
                  date.format=date.format,
                  summary=summary,
                  suppress.warnings=suppress.warnings);
  if( is.null(ret.val) ) return (NULL); # some error upstream
  # The followup.window.start and observation.window.start might have been converted to Date:
  followup.window.start <- ret.val$followup.window.start; observation.window.start <- ret.val$observation.window.start;

  # The workhorse auxiliary function: For a given (subset) of data, compute the event intervals and gaps:
  .workhorse.function <- function(data=NULL,
                                  ID.colname=NULL,
                                  event.date.colname=NULL,
                                  event.duration.colname=NULL,
                                  event.daily.dose.colname=NULL,
                                  medication.class.colname=NULL,
                                  event.interval.colname=NULL,
                                  gap.days.colname=NULL,
                                  carryover.within.obs.window=NULL,
                                  carryover.into.obs.window=NULL,
                                  carry.only.for.same.medication=NULL,
                                  consider.dosage.change=NULL,
                                  followup.window.start=NULL,
                                  followup.window.start.unit=NULL,
                                  followup.window.duration=NULL,
                                  followup.window.duration.unit=NULL,
                                  observation.window.start=NULL,
                                  observation.window.start.unit=NULL,
                                  observation.window.duration=NULL,
                                  observation.window.duration.unit=NULL,
                                  date.format=NULL,
                                  suppress.warnings=NULL
  )
  {
    # Auxiliary internal function: Compute the CMA for a given patient:
    .process.patient <- function(data4ID)
    {
      sel.data4ID <- data4ID[ !.EVENT.STARTS.BEFORE.OBS.WINDOW & !.EVENT.STARTS.AFTER.OBS.WINDOW, ]; # select the events within the observation window only
      n.events <- nrow(sel.data4ID); # cache number of events
      if( n.events < 2 || sel.data4ID$.DATE.as.Date[1] == sel.data4ID$.DATE.as.Date[n.events] )
      {
        # For less than two events or when the first and the last events are on the same day, CMA5 does not make sense
        return (NA_real_);
      } else
      {
        # Otherwise, the sum of durations of the events excluding the last divided by the number of days between the first and the last event
        return (1 - as.numeric(sum(sel.data4ID[-n.events, get(gap.days.colname)],na.rm=TRUE) /
                                     (as.numeric(difftime(sel.data4ID$.DATE.as.Date[n.events], sel.data4ID$.DATE.as.Date[1], units="days")))));
      }
    }

    # Call the compute.event.int.gaps() function and use the results:
    event.info <- compute.event.int.gaps(data=as.data.frame(data),
                                         ID.colname=ID.colname,
                                         event.date.colname=event.date.colname,
                                         event.duration.colname=event.duration.colname,
                                         event.daily.dose.colname=event.daily.dose.colname,
                                         medication.class.colname=medication.class.colname,
                                         event.interval.colname=event.interval.colname,
                                         gap.days.colname=gap.days.colname,
                                         carryover.within.obs.window=carryover.within.obs.window,
                                         carryover.into.obs.window=carryover.into.obs.window,
                                         carry.only.for.same.medication=carry.only.for.same.medication,
                                         consider.dosage.change=consider.dosage.change,
                                         followup.window.start=followup.window.start,
                                         followup.window.start.unit=followup.window.start.unit,
                                         followup.window.duration=followup.window.duration,
                                         followup.window.duration.unit=followup.window.duration.unit,
                                         observation.window.start=observation.window.start,
                                         observation.window.start.unit=observation.window.start.unit,
                                         observation.window.duration=observation.window.duration,
                                         observation.window.duration.unit=observation.window.duration.unit,
                                         date.format=date.format,
                                         keep.window.start.end.dates=TRUE,
                                         parallel.backend="none", # make sure this runs sequentially!
                                         parallel.threads=1,
                                         suppress.warnings=suppress.warnings,
                                         return.data.table=TRUE);
    if( is.null(event.info) ) return (list("CMA"=NA, "event.info"=NULL));

    CMA <- event.info[, .process.patient(.SD), by=ID.colname];
    return (list("CMA"=CMA, "event.info"=event.info));
  }

  ret.val <- .cma.skeleton(data=data,
                           ret.val=ret.val,
                           cma.class.name=c("CMA5","CMA1"),

                           ID.colname=ID.colname,
                           event.date.colname=event.date.colname,
                           event.duration.colname=event.duration.colname,
                           event.daily.dose.colname=event.daily.dose.colname,
                           medication.class.colname=medication.class.colname,
                           event.interval.colname=event.interval.colname,
                           gap.days.colname=gap.days.colname,
                           carryover.within.obs.window=TRUE,
                           carryover.into.obs.window=FALSE, # if TRUE consider the carry-over from before the starting date of the observation window
                           carry.only.for.same.medication=carry.only.for.same.medication,
                           consider.dosage.change=consider.dosage.change,
                           followup.window.start=followup.window.start,
                           followup.window.start.unit=followup.window.start.unit,
                           followup.window.duration=followup.window.duration,
                           followup.window.duration.unit=followup.window.duration.unit,
                           observation.window.start=observation.window.start,
                           observation.window.start.unit=observation.window.start.unit,
                           observation.window.duration=observation.window.duration,
                           observation.window.duration.unit=observation.window.duration.unit,
                           date.format=date.format,

                           flatten.medication.groups=flatten.medication.groups,
                           followup.window.start.per.medication.group=followup.window.start.per.medication.group,

                           suppress.warnings=suppress.warnings,
                           force.NA.CMA.for.failed.patients=force.NA.CMA.for.failed.patients,
                           parallel.backend=parallel.backend,
                           parallel.threads=parallel.threads,
                           .workhorse.function=.workhorse.function);

  return (ret.val);
}

#' @rdname print.CMA0
#' @export
print.CMA5 <- function(...) print.CMA0(...)

#' @rdname plot.CMA1
#' @export
plot.CMA5 <- function(...) .plot.CMA1plus(...)




#' CMA6 constructor.
#'
#' Constructs a CMA (continuous multiple-interval measures of medication
#' availability/gaps) type 6 object.
#'
#' \code{CMA6} assumes that, within the observation window, the medication is
#' used as prescribed and new medication is "banked" until needed (oversupply
#' from previous events is used first, followed new medication supply).
#' It computes days of theoretical use by extracting the total number of gap
#' days from the total time interval between the first event and the end of the
#' observation window, accounting for carry over for all medication events
#' within the observation window.
#' Thus, it accounts for timing within the observation window, and excludes the
#' remaining supply at the end of the observation window.
#'
#' The formula is
#' \deqn{(number of days of theoretical use) / (first event to end of
#' observation window)}
#'
#' Observations:
#' \itemize{
#'  \item the \code{carry.only.for.same.medication} parameter controls the
#'  transmission of carry-over across medication changes, producing a
#'  "standard" \code{CMA6} (default value is FALSE), and an "alternative"
#'  \code{CMA6b}, respectively;
#'  \item the \code{consider.dosage.change} parameter controls if dosage changes
#'  are taken into account, i.e. if set as TRUE and a new medication event has
#'  a different daily dosage recommendation, carry-over is recomputed assuming
#'  medication use according to the new prescribed dosage (default value is FALSE).
#' }
#'
#' @param data A \emph{\code{data.frame}} containing the events used to compute
#' the CMA. Must contain, at a minimum, the patient unique ID, the event date
#' and duration, and might also contain the daily dosage and medication type
#' (the actual column names are defined in the following four parameters).
#' @param ID.colname A \emph{string}, the name of the column in \code{data}
#' containing the unique patient ID; must be present.
#' @param event.date.colname A \emph{string}, the name of the column in
#' \code{data} containing the start date of the event (in the format given in
#' the \code{date.format} parameter); must be present.
#' @param event.duration.colname A \emph{string}, the name of the column in
#' \code{data} containing the event duration (in days); must be present.
#' @param event.daily.dose.colname A \emph{string}, the name of the column in
#' \code{data} containing the prescribed daily dose, or \code{NA} if not defined.
#' @param medication.class.colname A \emph{string}, the name of the column in
#' \code{data} containing the medication type, or \code{NA} if not defined.
#' @param medication.groups A \emph{vector} of characters defining medication
#' groups or the name of a column in \code{data} that defines such groups.
#' The names of the vector are the medication group unique names, while
#' the content defines them as logical expressions. While the names can be any
#' string of characters except "\}", it is recommended to stick to the rules for
#' defining vector names in \code{R}. For example,
#' \code{c("A"="CATEGORY == 'medA'", "AA"="{A} & PERDAY < 4"} defines two
#' medication groups: \emph{A} which selects all events of type "medA", and
#' \emph{B} which selects all events already defined by "A" but with a daily
#' dose lower than 4. If \code{NULL}, no medication groups are defined. If
#' medication groups are defined, there is one CMA estimate for each group;
#' moreover, there is a special group \emph{__ALL_OTHERS__} automatically defined
#' containing all observations \emph{not} covered by any of the explicitly defined
#' groups.
#' @param flatten.medication.groups \emph{Logical}, if \code{FALSE} (the default)
#' then the \code{CMA} and \code{event.info} components of the object are lists
#' with one medication group per element; otherwise, they are \code{data.frame}s
#' with an extra column containing the medication group (its name is given by
#' \code{medication.groups.colname}).
#' @param medication.groups.colname a \emph{string} (defaults to ".MED_GROUP_ID")
#' giving the name of the column storing the group name when
#' \code{flatten.medication.groups} is \code{TRUE}.
#' @param carry.only.for.same.medication \emph{Logical}, if \code{TRUE}, the
#' carry-over applies only across medication of the same type.
#' @param consider.dosage.change \emph{Logical}, if \code{TRUE}, the carry-over
#' is adjusted to also reflect changes in dosage.
#' @param followup.window.start If a \emph{\code{Date}} object, it represents
#' the actual start date of the follow-up window; if a \emph{string} it is the
#' name of the column in \code{data} containing the start date of the follow-up
#' window either as the numbers of \code{followup.window.start.unit} units after
#' the first event (the column must be of type \code{numeric}) or as actual
#' dates (in which case the column must be of type \code{Date} or a string
#' that conforms to the format specified in \code{date.format}); if a
#' \emph{number} it is the number of time units defined in the
#' \code{followup.window.start.unit} parameter after the begin of the
#' participant's first event; or \code{NA} if not defined.
#' @param followup.window.start.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.start} refers to (when a number), or
#' \code{NA} if not defined.
#' @param followup.window.start.per.medication.group a \emph{logical}: if there are
#' medication groups defined and this is \code{TRUE}, then the first event
#' considered for the follow-up window start is relative to each medication group
#' separately, otherwise (the default) it is relative to the patient.
#' @param followup.window.duration either a \emph{number} representing the
#' duration of the follow-up window in the time units given in
#' \code{followup.window.duration.unit}, or a \emph{string} giving the column
#' containing these numbers. Should represent a period for which relevant
#' medication events are recorded accurately (e.g. not extend after end of
#' relevant treatment, loss-to-follow-up or change to a health care provider
#' not covered by the database).
#' @param followup.window.duration.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.duration} refers to, or \code{NA} if not
#' defined.
#' @param observation.window.start,observation.window.start.unit,observation.window.duration,observation.window.duration.unit the definition of the observation window
#' (see the follow-up window parameters above for details).
#' @param date.format A \emph{string} giving the format of the dates used in the
#' \code{data} and the other parameters; see the \code{format} parameters of the
#' \code{\link[base]{as.Date}} function for details (NB, this concerns only the
#' dates given as strings and not as \code{Date} objects).
#' @param summary Metadata as a \emph{string}, briefly describing this CMA.
#' @param event.interval.colname A \emph{string}, the name of a newly-created
#' column storing the number of days between the start of the current event and
#' the start of the next one; the default value "event.interval" should be
#' changed only if there is a naming conflict with a pre-existing
#' "event.interval" column in \code{event.info}.
#' @param gap.days.colname A \emph{string}, the name of a newly-created column
#' storing the number of days when medication was not available (i.e., the
#' "gap days"); the default value "gap.days" should be changed only if there is
#' a naming conflict with a pre-existing "gap.days" column in \code{event.info}.
#' @param force.NA.CMA.for.failed.patients \emph{Logical} describing how the
#' patients for which the CMA estimation fails are treated: if \code{TRUE}
#' they are returned with an \code{NA} CMA estimate, while for
#' \code{FALSE} they are omitted.
#' @param parallel.backend Can be "none" (the default) for single-threaded
#' execution, "multicore"  (using \code{mclapply} in package \code{parallel})
#' for multicore processing (NB. not currently implemented on MS Windows and
#' automatically falls back on "snow" on this platform),  or "snow",
#' "snow(SOCK)" (equivalent to "snow"), "snow(MPI)" or "snow(NWS)" specifying
#' various types of SNOW clusters (can be on the local machine or more complex
#' setups -- please see the documentation of package \code{snow} for details;
#' the last two require packages \code{Rmpi} and \code{nws}, respectively, not
#' automatically installed with \code{AdhereR}).
#' @param parallel.threads Can be "auto" (for \code{parallel.backend} ==
#' "multicore", defaults to the number of cores in the system as given by
#' \code{options("cores")}, while for \code{parallel.backend} == "snow",
#' defaults to 2), a strictly positive integer specifying the number of parallel
#' threads, or a more complex specification of the SNOW cluster nodes for
#' \code{parallel.backend} == "snow" (see the documentation of package
#' \code{snow} for details).
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any
#' warnings.
#' @param arguments.that.should.not.be.defined a \emph{list} of argument names
#' and pre-defined valuesfor which a warning should be thrown if passed to the
#' function.
#' @param ... other possible parameters
#' @return An \code{S3} object of class \code{CMA6} (derived from \code{CMA0})
#' with the following fields:
#' \itemize{
#'  \item \code{data} The actual event data, as given by the \code{data}
#'  parameter.
#'  \item \code{ID.colname} the name of the column in \code{data} containing the
#'  unique patient ID, as given by the \code{ID.colname} parameter.
#'  \item \code{event.date.colname} the name of the column in \code{data}
#'  containing the start date of the event (in the format given in the
#'  \code{date.format} parameter), as given by the \code{event.date.colname}
#'  parameter.
#'  \item \code{event.duration.colname} the name of the column in \code{data}
#'  containing the event duration (in days), as given by the
#'  \code{event.duration.colname} parameter.
#'  \item \code{event.daily.dose.colname} the name of the column in \code{data}
#'  containing the prescribed daily dose, as given by the
#'  \code{event.daily.dose.colname} parameter.
#'  \item \code{medication.class.colname} the name of the column in \code{data}
#'  containing the classes/types/groups of medication, as given by the
#'  \code{medication.class.colname} parameter.
#'  \item \code{carry.only.for.same.medication} whether the carry-over applies
#'  only across medication of the same type, as given by the
#'  \code{carry.only.for.same.medication} parameter.
#'  \item \code{consider.dosage.change} whether the carry-over is adjusted to
#'  reflect changes in dosage, as given by the \code{consider.dosage.change}
#'  parameter.
#'  \item \code{followup.window.start} the beginning of the follow-up window, as
#'  given by the \code{followup.window.start} parameter.
#'  \item \code{followup.window.start.unit} the time unit of the
#'  \code{followup.window.start}, as given by the
#'  \code{followup.window.start.unit} parameter.
#'  \item \code{followup.window.duration} the duration of the follow-up window,
#'  as given by the \code{followup.window.duration} parameter.
#'  \item \code{followup.window.duration.unit} the time unit of the
#'  \code{followup.window.duration}, as given by the
#'  \code{followup.window.duration.unit} parameter.
#'  \item \code{observation.window.start} the beginning of the observation
#'  window, as given by the \code{observation.window.start} parameter.
#'  \item \code{observation.window.start.unit} the time unit of the
#'  \code{observation.window.start}, as given by the
#'  \code{observation.window.start.unit} parameter.
#'  \item \code{observation.window.duration} the duration of the observation
#'  window, as given by the \code{observation.window.duration} parameter.
#'  \item \code{observation.window.duration.unit} the time unit of the
#'  \code{observation.window.duration}, as given by the
#'  \code{observation.window.duration.unit} parameter.
#'  \item \code{date.format} the format of the dates, as given by the
#'  \code{date.format} parameter.
#'  \item \code{summary} the metadata, as given by the \code{summary} parameter.
#'  \item \code{event.info} the \code{data.frame} containing the event info
#'  (irrelevant for most users; see \code{\link{compute.event.int.gaps}} for
#'  details).
#'  \item \code{CMA} the \code{data.frame} containing the actual \code{CMA}
#'  estimates for each participant (the \code{ID.colname} column).
#' }
#' Please note that if \code{medication.groups} are defined, then the \code{CMA}
#' and \code{event.info} are named lists, each element containing the CMA and
#' event.info corresponding to a single medication group (the element's name).
#' @seealso CMAs 1 to 8 are defined in:
#'
#' Vollmer, W. M., Xu, M., Feldstein, A., Smith, D., Waterbury, A., & Rand, C.
#' (2012). Comparison of pharmacy-based measures of medication adherence.
#' \emph{BMC Health Services Research}, \strong{12}, 155.
#' \url{http://doi.org/10.1186/1472-6963-12-155}.
#'
#' @examples
#' cma6 <- CMA6(data=med.events,
#'              ID.colname="PATIENT_ID",
#'              event.date.colname="DATE",
#'              event.duration.colname="DURATION",
#'              event.daily.dose.colname="PERDAY",
#'              medication.class.colname="CATEGORY",
#'              carry.only.for.same.medication=FALSE,
#'              consider.dosage.change=FALSE,
#'              followup.window.start=30,
#'              observation.window.start=30,
#'              observation.window.duration=365,
#'              date.format="%m/%d/%Y"
#'             );
#' @export
CMA6 <- function( data=NULL, # the data used to compute the CMA on
                  # Important columns in the data
                  ID.colname=NA, # the name of the column containing the unique patient ID (NA = undefined)
                  event.date.colname=NA, # the start date of the event in the date.format format (NA = undefined)
                  event.duration.colname=NA, # the event duration in days (NA = undefined)
                  event.daily.dose.colname=NA, # the prescribed daily dose (NA = undefined)
                  medication.class.colname=NA, # the classes/types/groups of medication (NA = undefined)
                  # Groups of medication classes:
                  medication.groups=NULL, # a named vector of medication group definitions, the name of a column in the data that defines the groups, or NULL
                  flatten.medication.groups=FALSE, medication.groups.colname=".MED_GROUP_ID", # if medication.groups were defined, return CMAs and event.info as single data.frame?
                  # Various types methods of computing gaps:
                  carry.only.for.same.medication=FALSE, # if TRUE the carry-over applies only across medication of same type (NA = undefined)
                  consider.dosage.change=FALSE, # if TRUE carry-over is adjusted to reflect changes in dosage (NA = undefined)
                  # The follow-up window:
                  followup.window.start=0, # if a number is the earliest event per participant date plus number of units, or a Date object, or a column name in data (NA = undefined)
                  followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  followup.window.start.per.medication.group=FALSE, # if there are medication groups and this is TRUE, then the first event is relative to each medication group separately, otherwise is relative to the patient
                  followup.window.duration=365*2, # the duration of the follow-up window in the time units given below (NA = undefined)
                  followup.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)  (NA = undefined)
                  # The observation window (embedded in the follow-up window):
                  observation.window.start=0, # the number of time units relative to followup.window.start (NA = undefined)
                  observation.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  observation.window.duration=365*2, # the duration of the observation window in time units (NA = undefined)
                  observation.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  # Date format:
                  date.format="%m/%d/%Y", # the format of the dates used in this function (NA = undefined)
                  # Comments and metadata:
                  summary=NA,
                  # The description of the output (added) columns:
                  event.interval.colname="event.interval", # contains number of days between the start of current event and the start of the next
                  gap.days.colname="gap.days", # contains the number of days when medication was not available
                  # Dealing with failed estimates:
                  force.NA.CMA.for.failed.patients=TRUE, # force the failed patients to have NA CM estimate?
                  # Parallel processing:
                  parallel.backend=c("none","multicore","snow","snow(SOCK)","snow(MPI)","snow(NWS)")[1], # parallel backend to use
                  parallel.threads="auto", # specification (or number) of parallel threads
                  # Misc:
                  suppress.warnings=FALSE,
                  arguments.that.should.not.be.defined=c("carryover.within.obs.window"=TRUE,
                                                         "carryover.into.obs.window"=FALSE), # the list of argument names and values for which a warning should be thrown if passed to the function
                  ...
                )
{
  # The summary:
  if( is.na(summary) ) summary <- "The ratio of days with medication available from the first event to the end of the observation window; total number of gap days extracted from this time interval, then divided by the time interval, accounting for carry-over within observation window and excluding remaining supply";

  # Arguments that should not have been passed:
  if( !suppress.warnings && !is.null(arguments.that.should.not.be.defined) )
  {
    # Get the actual list of arguments (including in the ...); the first is the function's own name:
    args.list <- as.list(match.call(expand.dots = TRUE));
    args.mathing <- (names(arguments.that.should.not.be.defined) %in% names(args.list)[-1]);
    if( any(args.mathing) )
    {
      for( i in which(args.mathing) )
      {
        .report.ewms(paste0("Please note that '",args.list[[1]],"' overrides argument '",names(arguments.that.should.not.be.defined)[i],"' with value '",arguments.that.should.not.be.defined[i],"'!\n"), "warning", "CMA6", "AdhereR");
      }
    }
  }

  # Create the CMA0 object:
  ret.val <- CMA0(data=data,
                  ID.colname=ID.colname,
                  event.date.colname=event.date.colname,
                  event.duration.colname=event.duration.colname,
                  event.daily.dose.colname=event.daily.dose.colname,
                  medication.class.colname=medication.class.colname,
                  carry.only.for.same.medication=carry.only.for.same.medication,
                  consider.dosage.change=consider.dosage.change,
                  medication.groups=medication.groups,
                  flatten.medication.groups=flatten.medication.groups,
                  medication.groups.colname=medication.groups.colname,
                  followup.window.start=followup.window.start,
                  followup.window.start.unit=followup.window.start.unit,
                  followup.window.start.per.medication.group=followup.window.start.per.medication.group,
                  followup.window.duration=followup.window.duration,
                  followup.window.duration.unit=followup.window.duration.unit,
                  observation.window.start=observation.window.start,
                  observation.window.start.unit=observation.window.start.unit,
                  observation.window.duration=observation.window.duration,
                  observation.window.duration.unit=observation.window.duration.unit,
                  date.format=date.format,
                  summary=summary,
                  suppress.warnings=suppress.warnings);
  if( is.null(ret.val) ) return (NULL); # some error upstream
  # The followup.window.start and observation.window.start might have been converted to Date:
  followup.window.start <- ret.val$followup.window.start; observation.window.start <- ret.val$observation.window.start;

  # The workhorse auxiliary function: For a given (subset) of data, compute the event intervals and gaps:
  .workhorse.function <- function(data=NULL,
                                  ID.colname=NULL,
                                  event.date.colname=NULL,
                                  event.duration.colname=NULL,
                                  event.daily.dose.colname=NULL,
                                  medication.class.colname=NULL,
                                  event.interval.colname=NULL,
                                  gap.days.colname=NULL,
                                  carryover.within.obs.window=NULL,
                                  carryover.into.obs.window=NULL,
                                  carry.only.for.same.medication=NULL,
                                  consider.dosage.change=NULL,
                                  followup.window.start=NULL,
                                  followup.window.start.unit=NULL,
                                  followup.window.duration=NULL,
                                  followup.window.duration.unit=NULL,
                                  observation.window.start=NULL,
                                  observation.window.start.unit=NULL,
                                  observation.window.duration=NULL,
                                  observation.window.duration.unit=NULL,
                                  date.format=NULL,
                                  suppress.warnings=NULL
  )
  {
    # Auxiliary internal function: Compute the CMA for a given patient:
    .process.patient <- function(data4ID)
    {
      sel.data4ID <- data4ID[ !.EVENT.STARTS.BEFORE.OBS.WINDOW & !.EVENT.STARTS.AFTER.OBS.WINDOW, ]; # select the events within the observation window only
      n.events <- nrow(sel.data4ID); # cache number of events
      if( n.events < 1 || sel.data4ID$.DATE.as.Date[1] > sel.data4ID$.OBS.END.DATE[1] )
      {
        # For less than one event or when the first event is on the last day of the observation window, CMA6 does not make sense
        return (NA_real_);
      } else
      {
        # Otherwise, 1 - (total gap days divided by the number of days between the first event and the end of the observation window)
        return (1 - as.numeric(sum(sel.data4ID[, get(gap.days.colname)],na.rm=TRUE) /
                                     (as.numeric(sel.data4ID$.OBS.END.DATE[1] - sel.data4ID$.DATE.as.Date[1]))));
      }
    }

    # Call the compute.event.int.gaps() function and use the results:
    event.info <- compute.event.int.gaps(data=as.data.frame(data),
                                         ID.colname=ID.colname,
                                         event.date.colname=event.date.colname,
                                         event.duration.colname=event.duration.colname,
                                         event.daily.dose.colname=event.daily.dose.colname,
                                         medication.class.colname=medication.class.colname,
                                         event.interval.colname=event.interval.colname,
                                         gap.days.colname=gap.days.colname,
                                         carryover.within.obs.window=carryover.within.obs.window,
                                         carryover.into.obs.window=carryover.into.obs.window,
                                         carry.only.for.same.medication=carry.only.for.same.medication,
                                         consider.dosage.change=consider.dosage.change,
                                         followup.window.start=followup.window.start,
                                         followup.window.start.unit=followup.window.start.unit,
                                         followup.window.duration=followup.window.duration,
                                         followup.window.duration.unit=followup.window.duration.unit,
                                         observation.window.start=observation.window.start,
                                         observation.window.start.unit=observation.window.start.unit,
                                         observation.window.duration=observation.window.duration,
                                         observation.window.duration.unit=observation.window.duration.unit,
                                         date.format=date.format,
                                         keep.window.start.end.dates=TRUE,
                                         parallel.backend="none", # make sure this runs sequentially!
                                         parallel.threads=1,
                                         suppress.warnings=suppress.warnings,
                                         return.data.table=TRUE);
    if( is.null(event.info) ) return (list("CMA"=NA, "event.info"=NULL));

    CMA <- event.info[, .process.patient(.SD), by=ID.colname];
    return (list("CMA"=CMA, "event.info"=event.info));
  }

  ret.val <- .cma.skeleton(data=data,
                           ret.val=ret.val,
                           cma.class.name=c("CMA6","CMA1"),

                           ID.colname=ID.colname,
                           event.date.colname=event.date.colname,
                           event.duration.colname=event.duration.colname,
                           event.daily.dose.colname=event.daily.dose.colname,
                           medication.class.colname=medication.class.colname,
                           event.interval.colname=event.interval.colname,
                           gap.days.colname=gap.days.colname,
                           carryover.within.obs.window=TRUE,
                           carryover.into.obs.window=FALSE, # if TRUE consider the carry-over from before the starting date of the observation window
                           carry.only.for.same.medication=carry.only.for.same.medication,
                           consider.dosage.change=consider.dosage.change,
                           followup.window.start=followup.window.start,
                           followup.window.start.unit=followup.window.start.unit,
                           followup.window.duration=followup.window.duration,
                           followup.window.duration.unit=followup.window.duration.unit,
                           observation.window.start=observation.window.start,
                           observation.window.start.unit=observation.window.start.unit,
                           observation.window.duration=observation.window.duration,
                           observation.window.duration.unit=observation.window.duration.unit,
                           date.format=date.format,

                           flatten.medication.groups=flatten.medication.groups,
                           followup.window.start.per.medication.group=followup.window.start.per.medication.group,

                           suppress.warnings=suppress.warnings,
                           force.NA.CMA.for.failed.patients=force.NA.CMA.for.failed.patients,
                           parallel.backend=parallel.backend,
                           parallel.threads=parallel.threads,
                           .workhorse.function=.workhorse.function);

  return (ret.val);
}

#' @rdname print.CMA0
#' @export
print.CMA6 <- function(...) print.CMA0(...)

#' @rdname plot.CMA1
#' @export
plot.CMA6 <- function(...) .plot.CMA1plus(...)




#' CMA7 constructor.
#'
#' Constructs a CMA (continuous multiple-interval measures of medication
#' availability/gaps) type 7 object.
#'
#' \code{CMA7} assumes that, within and before the observation window, the
#' medication is used as prescribed and new medication is "banked" until needed
#' (oversupply from previous events is used first, followed new medication
#' supply).
#' It computes days of theoretical use by extracting the total number of gap
#' days from the total time interval between the start and the end of the
#' observation window, accounting for carry over for all medication events
#' within and before the observation window. All medication events in the
#' follow up window before observation window are considered for carry-over
#' calculation.
#' Thus, it accounts for timing within and before the observation window, and
#' excludes the remaining supply at the end of the observation window.
#'
#' The formula is
#' \deqn{(number of days of theoretical use) / (start to end of observation
#'  window)}
#'
#' Observations:
#' \itemize{
#'  \item the \code{carry.only.for.same.medication} parameter controls the
#'  transmission of carry-over across medication changes, producing a "standard"
#'  \code{CMA7} (default value is FALSE), and an "alternative" \code{CMA7b},
#'  respectively;
#'  \item the \code{consider.dosage.change} parameter controls if dosage
#'  changes are taken into account, i.e. if set as TRUE and a new medication
#'  event has a different daily dosage recommendation, carry-over is recomputed
#'  assuming medication use according to the new prescribed dosage (default
#'  value is FALSE).
#' }
#'
#' @param data A \emph{\code{data.frame}} containing the events used to compute
#' the CMA. Must contain, at a minimum, the patient unique ID, the event date
#' and duration, and might also contain the daily dosage and medication type
#' (the actual column names are defined in the following four parameters).
#' @param ID.colname A \emph{string}, the name of the column in \code{data}
#' containing the unique patient ID; must be present.
#' @param event.date.colname A \emph{string}, the name of the column in
#' \code{data} containing the start date of the event (in the format given in
#' the \code{date.format} parameter); must be present.
#' @param event.duration.colname A \emph{string}, the name of the column in
#' \code{data} containing the event duration (in days); must be present.
#' @param event.daily.dose.colname A \emph{string}, the name of the column in
#' \code{data} containing the prescribed daily dose, or \code{NA} if not defined.
#' @param medication.class.colname A \emph{string}, the name of the column in
#' \code{data} containing the medication type, or \code{NA} if not defined.
#' @param medication.groups A \emph{vector} of characters defining medication
#' groups or the name of a column in \code{data} that defines such groups.
#' The names of the vector are the medication group unique names, while
#' the content defines them as logical expressions. While the names can be any
#' string of characters except "\}", it is recommended to stick to the rules for
#' defining vector names in \code{R}. For example,
#' \code{c("A"="CATEGORY == 'medA'", "AA"="{A} & PERDAY < 4"} defines two
#' medication groups: \emph{A} which selects all events of type "medA", and
#' \emph{B} which selects all events already defined by "A" but with a daily
#' dose lower than 4. If \code{NULL}, no medication groups are defined. If
#' medication groups are defined, there is one CMA estimate for each group;
#' moreover, there is a special group \emph{__ALL_OTHERS__} automatically defined
#' containing all observations \emph{not} covered by any of the explicitly defined
#' groups.
#' @param flatten.medication.groups \emph{Logical}, if \code{FALSE} (the default)
#' then the \code{CMA} and \code{event.info} components of the object are lists
#' with one medication group per element; otherwise, they are \code{data.frame}s
#' with an extra column containing the medication group (its name is given by
#' \code{medication.groups.colname}).
#' @param medication.groups.colname a \emph{string} (defaults to ".MED_GROUP_ID")
#' giving the name of the column storing the group name when
#' \code{flatten.medication.groups} is \code{TRUE}.
#' @param carry.only.for.same.medication \emph{Logical}, if \code{TRUE}, the
#' carry-over applies only across medication of the same type.
#' @param consider.dosage.change \emph{Logical}, if \code{TRUE}, the carry-over
#' is adjusted to also reflect changes in dosage.
#' @param followup.window.start If a \emph{\code{Date}} object, it represents
#' the actual start date of the follow-up window; if a \emph{string} it is the
#' name of the column in \code{data} containing the start date of the follow-up
#' window either as the numbers of \code{followup.window.start.unit} units after
#' the first event (the column must be of type \code{numeric}) or as actual
#' dates (in which case the column must be of type \code{Date} or a string
#' that conforms to the format specified in \code{date.format}); if a
#' \emph{number} it is the number of time units defined in the
#' \code{followup.window.start.unit} parameter after the begin of the
#' participant's first event; or \code{NA} if not defined.
#' @param followup.window.start.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.start} refers to (when a number), or
#' \code{NA} if not defined.
#' @param followup.window.start.per.medication.group a \emph{logical}: if there are
#' medication groups defined and this is \code{TRUE}, then the first event
#' considered for the follow-up window start is relative to each medication group
#' separately, otherwise (the default) it is relative to the patient.
#' @param followup.window.duration either a \emph{number} representing the
#' duration of the follow-up window in the time units given in
#' \code{followup.window.duration.unit}, or a \emph{string} giving the column
#' containing these numbers. Should represent a period for which relevant
#' medication events are recorded accurately (e.g. not extend after end of
#' relevant treatment, loss-to-follow-up or change to a health care provider not
#' covered by the database).
#' @param followup.window.duration.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.duration} refers to, or \code{NA} if not
#' defined.
#' @param observation.window.start,observation.window.start.unit,observation.window.duration,observation.window.duration.unit the definition of the observation window
#' (see the follow-up window parameters above for details).
#' @param date.format A \emph{string} giving the format of the dates used in the
#' \code{data} and the other parameters; see the \code{format} parameters of the
#' \code{\link[base]{as.Date}} function for details (NB, this concerns only the
#' dates given as strings and not as \code{Date} objects).
#' @param summary Metadata as a \emph{string}, briefly describing this CMA.
#' @param event.interval.colname A \emph{string}, the name of a newly-created
#' column storing the number of days between the start of the current event and
#' the start of the next one; the default value "event.interval" should be
#' changed only if there is a naming conflict with a pre-existing
#' "event.interval" column in \code{event.info}.
#' @param gap.days.colname A \emph{string}, the name of a newly-created column
#' storing the number of days when medication was not available (i.e., the
#' "gap days"); the default value "gap.days" should be changed only if there is
#' a naming conflict with a pre-existing "gap.days" column in \code{event.info}.
#' @param force.NA.CMA.for.failed.patients \emph{Logical} describing how the
#' patients for which the CMA estimation fails are treated: if \code{TRUE}
#' they are returned with an \code{NA} CMA estimate, while for
#' \code{FALSE} they are omitted.
#' @param parallel.backend Can be "none" (the default) for single-threaded
#' execution, "multicore"  (using \code{mclapply} in package \code{parallel})
#' for multicore processing (NB. not currently implemented on MS Windows and
#' automatically falls back on "snow" on this platform),  or "snow",
#' "snow(SOCK)" (equivalent to "snow"), "snow(MPI)" or "snow(NWS)" specifying
#' various types of SNOW clusters (can be on the local machine or more complex
#' setups -- please see the documentation of package \code{snow} for details;
#' the last two require packages \code{Rmpi} and \code{nws}, respectively, not
#' automatically installed with \code{AdhereR}).
#' @param parallel.threads Can be "auto" (for \code{parallel.backend} ==
#' "multicore", defaults to the number of cores in the system as given by
#' \code{options("cores")}, while for \code{parallel.backend} == "snow",
#' defaults to 2), a strictly positive integer specifying the number of parallel
#' threads, or a more complex specification of the SNOW cluster nodes for
#' \code{parallel.backend} == "snow" (see the documentation of package
#' \code{snow} for details).
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any
#' warnings.
#' @param arguments.that.should.not.be.defined a \emph{list} of argument names
#' and pre-defined valuesfor which a warning should be thrown if passed to the
#' function.
#' @param ... other possible parameters
#' @return An \code{S3} object of class \code{CMA7} (derived from \code{CMA0})
#' with the following fields:
#' \itemize{
#'  \item \code{data} The actual event data, as given by the \code{data}
#'  parameter.
#'  \item \code{ID.colname} the name of the column in \code{data} containing the
#'  unique patient ID, as given by the \code{ID.colname} parameter.
#'  \item \code{event.date.colname} the name of the column in \code{data}
#'  containing the start date of the event (in the format given in the
#'  \code{date.format} parameter), as given by the \code{event.date.colname}
#'  parameter.
#'  \item \code{event.duration.colname} the name of the column in \code{data}
#'  containing the event duration (in days), as given by the
#'  \code{event.duration.colname} parameter.
#'  \item \code{event.daily.dose.colname} the name of the column in \code{data}
#'  containing the prescribed daily dose, as given by the
#'  \code{event.daily.dose.colname} parameter.
#'  \item \code{medication.class.colname} the name of the column in \code{data}
#'  containing the classes/types/groups of medication, as given by the
#'  \code{medication.class.colname} parameter.
#'  \item \code{carry.only.for.same.medication} whether the carry-over applies
#'  only across medication of the same type, as given by the
#'  \code{carry.only.for.same.medication} parameter.
#'  \item \code{consider.dosage.change} whether the carry-over is adjusted to
#'  reflect changes in dosage, as given by the \code{consider.dosage.change}
#'  parameter.
#'  \item \code{followup.window.start} the beginning of the follow-up window,
#'  as given by the \code{followup.window.start} parameter.
#'  \item \code{followup.window.start.unit} the time unit of the
#'  \code{followup.window.start}, as given by the
#'  \code{followup.window.start.unit} parameter.
#'  \item \code{followup.window.duration} the duration of the follow-up window,
#'  as given by the \code{followup.window.duration} parameter.
#'  \item \code{followup.window.duration.unit} the time unit of the
#'  \code{followup.window.duration}, as given by the
#'  \code{followup.window.duration.unit} parameter.
#'  \item \code{observation.window.start} the beginning of the observation
#'  window, as given by the \code{observation.window.start} parameter.
#'  \item \code{observation.window.start.unit} the time unit of the
#'  \code{observation.window.start}, as given by the
#'  \code{observation.window.start.unit} parameter.
#'  \item \code{observation.window.duration} the duration of the observation
#'  window, as given by the \code{observation.window.duration} parameter.
#'  \item \code{observation.window.duration.unit} the time unit of the
#'  \code{observation.window.duration}, as given by the
#'  \code{observation.window.duration.unit} parameter.
#'  \item \code{date.format} the format of the dates, as given by the
#'  \code{date.format} parameter.
#'  \item \code{summary} the metadata, as given by the \code{summary} parameter.
#'  \item \code{event.info} the \code{data.frame} containing the event info
#'  (irrelevant for most users; see \code{\link{compute.event.int.gaps}} for
#'  details).
#'  \item \code{CMA} the \code{data.frame} containing the actual \code{CMA}
#'  estimates for each participant (the \code{ID.colname} column).
#' }
#' Please note that if \code{medication.groups} are defined, then the \code{CMA}
#' and \code{event.info} are named lists, each element containing the CMA and
#' event.info corresponding to a single medication group (the element's name).
#' @seealso CMAs 1 to 8 are defined in:
#'
#' Vollmer, W. M., Xu, M., Feldstein, A., Smith, D., Waterbury, A., & Rand, C.
#' (2012). Comparison of pharmacy-based measures of medication adherence.
#' \emph{BMC Health Services Research}, \strong{12}, 155.
#' \url{http://doi.org/10.1186/1472-6963-12-155}.
#'
#' @examples
#' cma7 <- CMA7(data=med.events,
#'              ID.colname="PATIENT_ID",
#'              event.date.colname="DATE",
#'              event.duration.colname="DURATION",
#'              event.daily.dose.colname="PERDAY",
#'              medication.class.colname="CATEGORY",
#'              carry.only.for.same.medication=FALSE,
#'              consider.dosage.change=FALSE,
#'              followup.window.start=30,
#'              observation.window.start=30,
#'              observation.window.duration=365,
#'              date.format="%m/%d/%Y"
#'             );
#' @export
CMA7 <- function( data=NULL, # the data used to compute the CMA on
                  # Important columns in the data
                  ID.colname=NA, # the name of the column containing the unique patient ID (NA = undefined)
                  event.date.colname=NA, # the start date of the event in the date.format format (NA = undefined)
                  event.duration.colname=NA, # the event duration in days (NA = undefined)
                  event.daily.dose.colname=NA, # the prescribed daily dose (NA = undefined)
                  medication.class.colname=NA, # the classes/types/groups of medication (NA = undefined)
                  # Groups of medication classes:
                  medication.groups=NULL, # a named vector of medication group definitions, the name of a column in the data that defines the groups, or NULL
                  flatten.medication.groups=FALSE, medication.groups.colname=".MED_GROUP_ID", # if medication.groups were defined, return CMAs and event.info as single data.frame?
                  # Various types methods of computing gaps:
                  carry.only.for.same.medication=FALSE, # if TRUE the carry-over applies only across medication of same type (NA = undefined)
                  consider.dosage.change=FALSE, # if TRUE carry-over is adjusted to reflect changes in dosage (NA = undefined)
                  # The follow-up window:
                  followup.window.start=0, # if a number is the earliest event per participant date + number of units, or a Date object, or a column name in data (NA = undefined)
                  followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  followup.window.start.per.medication.group=FALSE, # if there are medication groups and this is TRUE, then the first event is relative to each medication group separately, otherwise is relative to the patient
                  followup.window.duration=365*2, # the duration of the follow-up window in the time units given below (NA = undefined)
                  followup.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)  (NA = undefined)
                  # The observation window (embedded in the follow-up window):
                  observation.window.start=0, # the number of time units relative to followup.window.start (NA = undefined)
                  observation.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  observation.window.duration=365*2, # the duration of the observation window in time units (NA = undefined)
                  observation.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  # Date format:
                  date.format="%m/%d/%Y", # the format of the dates used in this function (NA = undefined)
                  # Comments and metadata:
                  summary=NA,
                  # The description of the output (added) columns:
                  event.interval.colname="event.interval", # contains number of days between the start of current event and the start of the next
                  gap.days.colname="gap.days", # contains the number of days when medication was not available
                  # Dealing with failed estimates:
                  force.NA.CMA.for.failed.patients=TRUE, # force the failed patients to have NA CM estimate?
                  # Parallel processing:
                  parallel.backend=c("none","multicore","snow","snow(SOCK)","snow(MPI)","snow(NWS)")[1], # parallel backend to use
                  parallel.threads="auto", # specification (or number) of parallel threads
                  # Misc:
                  suppress.warnings=FALSE,
                  arguments.that.should.not.be.defined=c("carryover.within.obs.window"=TRUE,
                                                         "carryover.into.obs.window"=TRUE), # the list of argument names and values for which a warning should be thrown if passed to the function
                  ...
                )
{
  # The summary:
  if( is.na(summary) ) summary <- "The ratio of days with medication available in the whole observation window; total number of gap days extracted from this time interval, then divided by the time interval, accounting for carry-over both before and within observation window and excluding remaining supply";

  # Arguments that should not have been passed:
  if( !suppress.warnings && !is.null(arguments.that.should.not.be.defined) )
  {
    # Get the actual list of arguments (including in the ...); the first is the function's own name:
    args.list <- as.list(match.call(expand.dots = TRUE));
    args.mathing <- (names(arguments.that.should.not.be.defined) %in% names(args.list)[-1]);
    if( any(args.mathing) )
    {
      for( i in which(args.mathing) )
      {
        .report.ewms(paste0("Please note that '",args.list[[1]],"' overrides argument '",names(arguments.that.should.not.be.defined)[i],"' with value '",arguments.that.should.not.be.defined[i],"'!\n"), "warning", "CMA7", "AdhereR");
      }
    }
  }

  # Create the CMA0 object:
  ret.val <- CMA0(data=data,
                  ID.colname=ID.colname,
                  event.date.colname=event.date.colname,
                  event.duration.colname=event.duration.colname,
                  event.daily.dose.colname=event.daily.dose.colname,
                  medication.class.colname=medication.class.colname,
                  carry.only.for.same.medication=carry.only.for.same.medication,
                  consider.dosage.change=consider.dosage.change,
                  medication.groups=medication.groups,
                  flatten.medication.groups=flatten.medication.groups,
                  medication.groups.colname=medication.groups.colname,
                  followup.window.start=followup.window.start,
                  followup.window.start.unit=followup.window.start.unit,
                  followup.window.start.per.medication.group=followup.window.start.per.medication.group,
                  followup.window.duration=followup.window.duration,
                  followup.window.duration.unit=followup.window.duration.unit,
                  observation.window.start=observation.window.start,
                  observation.window.start.unit=observation.window.start.unit,
                  observation.window.duration=observation.window.duration,
                  observation.window.duration.unit=observation.window.duration.unit,
                  date.format=date.format,
                  summary=summary,
                  suppress.warnings=suppress.warnings);
  if( is.null(ret.val) ) return (NULL); # some error upstream
  # The followup.window.start and observation.window.start might have been converted to Date:
  followup.window.start <- ret.val$followup.window.start; observation.window.start <- ret.val$observation.window.start;

  # The workhorse auxiliary function: For a given (subset) of data, compute the event intervals and gaps:
  .workhorse.function <- function(data=NULL,
                                  ID.colname=NULL,
                                  event.date.colname=NULL,
                                  event.duration.colname=NULL,
                                  event.daily.dose.colname=NULL,
                                  medication.class.colname=NULL,
                                  event.interval.colname=NULL,
                                  gap.days.colname=NULL,
                                  carryover.within.obs.window=NULL,
                                  carryover.into.obs.window=NULL,
                                  carry.only.for.same.medication=NULL,
                                  consider.dosage.change=NULL,
                                  followup.window.start=NULL,
                                  followup.window.start.unit=NULL,
                                  followup.window.duration=NULL,
                                  followup.window.duration.unit=NULL,
                                  observation.window.start=NULL,
                                  observation.window.start.unit=NULL,
                                  observation.window.duration=NULL,
                                  observation.window.duration.unit=NULL,
                                  date.format=NULL,
                                  suppress.warnings=NULL
  )
  {
    # Auxiliary internal function: Compute the CMA for a given patient:
    .process.patient <- function(data4ID)
    {
      # Select the events within the observation window:
      s <- which(!data4ID$.EVENT.STARTS.BEFORE.OBS.WINDOW & !data4ID$.EVENT.STARTS.AFTER.OBS.WINDOW);
      # Cache used things:
      n.events <- nrow(data4ID); s1 <- s[1]; slen <- length(s); slast <- s[slen];
      gap.days.column <- data4ID[, get(gap.days.colname)];

      # For less than one event or when the first event is on the last day of the observation window,
      if( slen < 1 || (data4ID$.DATE.as.Date[s1] > data4ID$.OBS.END.DATE[s1]) )
      {
        if ( sum(data4ID$.EVENT.STARTS.BEFORE.OBS.WINDOW)==0 )
        {
          # If there are no prior events, CMA7 does not make sense:
          return (NA_real_);
        } else
        {
          # Select the events that start before the observation window begins but continue within the observation window:
          event.start.before.continue.in <- max(which(data4ID$.EVENT.STARTS.BEFORE.OBS.WINDOW));
          if( data4ID$gap.days[event.start.before.continue.in] <= as.numeric(data4ID$.OBS.END.DATE[n.events] - data4ID$.OBS.START.DATE[n.events]) )
          {
            # Otherwise, CMA7 is the gap of the last event entering the observation window minus any days between the end of observation window and date of next event or end of follow-up window, divided by the observation window:
            return (1.0 - as.numeric(gap.days.column[event.start.before.continue.in] /
                                       (as.numeric(data4ID$.OBS.END.DATE[event.start.before.continue.in] - data4ID$.OBS.START.DATE[event.start.before.continue.in]))));
          } else
          {
            # If there is no event before that enter into the observation window, CMA7 is 0:
            return (0.0);
          }
        }
      } else
      {
        # for all other situations compute the gap days for the last event in the observation window
        event.after.obs.window <- which(data4ID$.EVENT.STARTS.AFTER.OBS.WINDOW); # the events after the obsevration window
        gap.days.obswin.last.event <- gap.days.column[slast];

        # Compute the gap days within the observation window but before the first event:
        if( s1 == 1 )
        {
          gap.days.obswin.before.first.event <- as.numeric(difftime(data4ID$.DATE.as.Date[s1], data4ID$.OBS.START.DATE[s1], units="days")); # if it is the first event, there are gap days from start of observation window to first event
        } else
        {
          gap.days.obswin.before.first.event <- min(gap.days.column[s1-1], as.numeric(difftime(data4ID$.DATE.as.Date[s1], data4ID$.OBS.START.DATE[s1], units="days")));
        }

        # Return 1 - (total gap days in observation window divided by the number of days between the start and the end of the observation window)
        if( slen == 1  )
        {
          # if there is only one event, there are no "gap.days" to consider
          return (1 - as.numeric((gap.days.obswin.last.event + gap.days.obswin.before.first.event) /
                                    (as.numeric(difftime(data4ID$.OBS.END.DATE[s1], data4ID$.OBS.START.DATE[s1], units="days")))));
        } else
        {
          # if there are more events, gap days are the ones from before, the ones for the last event, and the ones in between
          return (1 - as.numeric((sum(gap.days.column[s[-slen]],na.rm=TRUE) + gap.days.obswin.last.event + gap.days.obswin.before.first.event) /
                                     (as.numeric(difftime(data4ID$.OBS.END.DATE[s1], data4ID$.OBS.START.DATE[s1], units="days")))));
        }
      }
    }

    # Call the compute.event.int.gaps() function and use the results:
    event.info <- compute.event.int.gaps(data=as.data.frame(data),
                                         ID.colname=ID.colname,
                                         event.date.colname=event.date.colname,
                                         event.duration.colname=event.duration.colname,
                                         event.daily.dose.colname=event.daily.dose.colname,
                                         medication.class.colname=medication.class.colname,
                                         event.interval.colname=event.interval.colname,
                                         gap.days.colname=gap.days.colname,
                                         carryover.within.obs.window=carryover.within.obs.window,
                                         carryover.into.obs.window=carryover.into.obs.window,
                                         carry.only.for.same.medication=carry.only.for.same.medication,
                                         consider.dosage.change=consider.dosage.change,
                                         followup.window.start=followup.window.start,
                                         followup.window.start.unit=followup.window.start.unit,
                                         followup.window.duration=followup.window.duration,
                                         followup.window.duration.unit=followup.window.duration.unit,
                                         observation.window.start=observation.window.start,
                                         observation.window.start.unit=observation.window.start.unit,
                                         observation.window.duration=observation.window.duration,
                                         observation.window.duration.unit=observation.window.duration.unit,
                                         date.format=date.format,
                                         keep.window.start.end.dates=TRUE,
                                         parallel.backend="none", # make sure this runs sequentially!
                                         parallel.threads=1,
                                         suppress.warnings=suppress.warnings,
                                         return.data.table=TRUE);
    if( is.null(event.info) ) return (list("CMA"=NA, "event.info"=NULL));

    CMA <- event.info[, .process.patient(.SD), by=ID.colname];
    return (list("CMA"=CMA, "event.info"=event.info));
  }

  ret.val <- .cma.skeleton(data=data,
                           ret.val=ret.val,
                           cma.class.name=c("CMA7","CMA1"),

                           ID.colname=ID.colname,
                           event.date.colname=event.date.colname,
                           event.duration.colname=event.duration.colname,
                           event.daily.dose.colname=event.daily.dose.colname,
                           medication.class.colname=medication.class.colname,
                           event.interval.colname=event.interval.colname,
                           gap.days.colname=gap.days.colname,
                           carryover.within.obs.window=TRUE,
                           carryover.into.obs.window=TRUE, # if TRUE consider the carry-over from before the starting date of the observation window
                           carry.only.for.same.medication=carry.only.for.same.medication,
                           consider.dosage.change=consider.dosage.change,
                           followup.window.start=followup.window.start,
                           followup.window.start.unit=followup.window.start.unit,
                           followup.window.duration=followup.window.duration,
                           followup.window.duration.unit=followup.window.duration.unit,
                           observation.window.start=observation.window.start,
                           observation.window.start.unit=observation.window.start.unit,
                           observation.window.duration=observation.window.duration,
                           observation.window.duration.unit=observation.window.duration.unit,
                           date.format=date.format,

                           flatten.medication.groups=flatten.medication.groups,
                           followup.window.start.per.medication.group=followup.window.start.per.medication.group,

                           suppress.warnings=suppress.warnings,
                           force.NA.CMA.for.failed.patients=force.NA.CMA.for.failed.patients,
                           parallel.backend=parallel.backend,
                           parallel.threads=parallel.threads,
                           .workhorse.function=.workhorse.function);

  return (ret.val);
}

#' @rdname print.CMA0
#' @export
print.CMA7 <- function(...) print.CMA0(...)

#' @rdname plot.CMA1
#' @export
plot.CMA7 <- function(...) .plot.CMA1plus(...)




#' CMA8 constructor.
#'
#' Constructs a CMA (continuous multiple-interval measures of medication
#' availability/gaps) type 8 object.
#'
#' \code{CMA8} is similar to CMA6 in that it assumes that, within the
#' observation window, the medication is used as prescribed and new medication
#' is "banked" until needed (oversupply from previous events is used first,
#' followed new medication supply). Unlike \code{CMA6} it accounts for
#' carry-over from before the window - but in a different way from \code{CMA7}:
#' by adding a time lag at the start of the observation window equal to the
#' duration of carry-over from before. It is designed for situations when an
#' event with a hypothesized causal effect on adherence happens at the start of
#' the observation window (e.g. enrolment in an intervention study); in this
#' case, it may be that the existing supply is not part of the relationship
#' under study (e.g. it delays the actual start of the study for that
#' participant) and needs to be excluded by shortening the time interval
#' examined. The end of the observation window remains the same.
#' Thus, \code{CMA8} computes days of theoretical use by extracting the total
#' number of gap days from the total time interval between the lagged start and
#' the end of the observation window, accounting for carry over for all
#' medication events within the observation window. All medication events in the
#' follow up window before observation window are considered for carry-over
#' calculation.
#' Thus, as \code{CMA7}, it accounts for timing within the observation window,
#' as well as before (different adjustment than \code{CMA7}), and excludes the
#' remaining supply at the end of the observation window.
#'
#' The formula is
#' \deqn{(number of days of theoretical use) / (lagged start to end of
#' observation window)}
#'
#' Observations:
#' \itemize{
#'  \item the \code{carry.only.for.same.medication} parameter controls the
#'  transmission of carry-over across medication changes, producing a "standard"
#'  \code{CMA8} (default value is FALSE), and an "alternative" \code{CMA8b},
#'  respectively;
#'  \item the \code{consider.dosage.change} parameter controls if dosage changes
#'  are taken into account, i.e. if set as TRUE and a new medication event has
#'  a different daily dosage recommendation, carry-over is recomputed assuming
#'  medication use according to the new prescribed dosage (default value is
#'  FALSE).
#' }
#'
#' @param data A \emph{\code{data.frame}} containing the events used to compute
#' the CMA. Must contain, at a minimum, the patient unique ID, the event date
#' and duration, and might also contain the daily dosage and medication type
#' (the actual column names are defined in the following four parameters).
#' @param ID.colname A \emph{string}, the name of the column in \code{data}
#' containing the unique patient ID; must be present.
#' @param event.date.colname A \emph{string}, the name of the column in
#' \code{data} containing the start date of the event (in the format given in
#' the \code{date.format} parameter); must be present.
#' @param event.duration.colname A \emph{string}, the name of the column in
#' \code{data} containing the event duration (in days); must be present.
#' @param event.daily.dose.colname A \emph{string}, the name of the column in
#' \code{data} containing the prescribed daily dose, or \code{NA} if not defined.
#' @param medication.class.colname A \emph{string}, the name of the column in
#' \code{data} containing the medication type, or \code{NA} if not defined.
#' @param medication.groups A \emph{vector} of characters defining medication
#' groups or the name of a column in \code{data} that defines such groups.
#' The names of the vector are the medication group unique names, while
#' the content defines them as logical expressions. While the names can be any
#' string of characters except "\}", it is recommended to stick to the rules for
#' defining vector names in \code{R}. For example,
#' \code{c("A"="CATEGORY == 'medA'", "AA"="{A} & PERDAY < 4"} defines two
#' medication groups: \emph{A} which selects all events of type "medA", and
#' \emph{B} which selects all events already defined by "A" but with a daily
#' dose lower than 4. If \code{NULL}, no medication groups are defined. If
#' medication groups are defined, there is one CMA estimate for each group;
#' moreover, there is a special group \emph{__ALL_OTHERS__} automatically defined
#' containing all observations \emph{not} covered by any of the explicitly defined
#' groups.
#' @param flatten.medication.groups \emph{Logical}, if \code{FALSE} (the default)
#' then the \code{CMA} and \code{event.info} components of the object are lists
#' with one medication group per element; otherwise, they are \code{data.frame}s
#' with an extra column containing the medication group (its name is given by
#' \code{medication.groups.colname}).
#' @param medication.groups.colname a \emph{string} (defaults to ".MED_GROUP_ID")
#' giving the name of the column storing the group name when
#' \code{flatten.medication.groups} is \code{TRUE}.
#' @param carry.only.for.same.medication \emph{Logical}, if \code{TRUE}, the
#' carry-over applies only across medication of the same type.
#' @param consider.dosage.change \emph{Logical}, if \code{TRUE}, the carry-over
#' is adjusted to also reflect changes in dosage.
#' @param followup.window.start If a \emph{\code{Date}} object, it represents
#' the actual start date of the follow-up window; if a \emph{string} it is the
#' name of the column in \code{data} containing the start date of the follow-up
#' window either as the numbers of \code{followup.window.start.unit} units after
#' the first event (the column must be of type \code{numeric}) or as actual
#' dates (in which case the column must be of type \code{Date} or a string
#' that conforms to the format specified in \code{date.format}); if a
#' \emph{number} it is the number of time units defined in the
#' \code{followup.window.start.unit} parameter after the begin of the
#' participant's first event; or \code{NA} if not defined.
#' @param followup.window.start.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.start} refers to (when a number), or
#' \code{NA} if not defined.
#' @param followup.window.start.per.medication.group a \emph{logical}: if there are
#' medication groups defined and this is \code{TRUE}, then the first event
#' considered for the follow-up window start is relative to each medication group
#' separately, otherwise (the default) it is relative to the patient.
#' @param followup.window.duration either a \emph{number} representing the
#' duration of the follow-up window in the time units given in
#' \code{followup.window.duration.unit}, or a \emph{string} giving the column
#' containing these numbers. Should represent a period for which relevant
#' medication events are recorded accurately (e.g. not extend after end of
#' relevant treatment, loss-to-follow-up or change to a health care provider
#' not covered by the database).
#' @param followup.window.duration.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.duration} refers to, or \code{NA} if not
#' defined.
#' @param observation.window.start,observation.window.start.unit,observation.window.duration,observation.window.duration.unit the definition of the observation window
#' (see the follow-up window parameters above for details).
#' @param date.format A \emph{string} giving the format of the dates used in the
#' \code{data} and the other parameters; see the \code{format} parameters of the
#' \code{\link[base]{as.Date}} function for details (NB, this concerns only the
#' dates given as strings and not as \code{Date} objects).
#' @param summary Metadata as a \emph{string}, briefly describing this CMA.
#' @param event.interval.colname A \emph{string}, the name of a newly-created
#' column storing the number of days between the start of the current event and
#' the start of the next one; the default value "event.interval" should be
#' changed only if there is a naming conflict with a pre-existing
#' "event.interval" column in \code{event.info}.
#' @param gap.days.colname A \emph{string}, the name of a newly-created column
#' storing the number of days when medication was not available (i.e., the
#' "gap days"); the default value "gap.days" should be changed only if there is
#' a naming conflict with a pre-existing "gap.days" column in \code{event.info}.
#' @param force.NA.CMA.for.failed.patients \emph{Logical} describing how the
#' patients for which the CMA estimation fails are treated: if \code{TRUE}
#' they are returned with an \code{NA} CMA estimate, while for
#' \code{FALSE} they are omitted.
#' @param parallel.backend Can be "none" (the default) for single-threaded
#' execution, "multicore"  (using \code{mclapply} in package \code{parallel})
#' for multicore processing (NB. not currently implemented on MS Windows and
#' automatically falls back on "snow" on this platform),  or "snow",
#' "snow(SOCK)" (equivalent to "snow"), "snow(MPI)" or "snow(NWS)" specifying
#' various types of SNOW clusters (can be on the local machine or more complex
#' setups -- please see the documentation of package \code{snow} for details;
#' the last two require packages \code{Rmpi} and \code{nws}, respectively, not
#' automatically installed with \code{AdhereR}).
#' @param parallel.threads Can be "auto" (for \code{parallel.backend} ==
#' "multicore", defaults to the number of cores in the system as given by
#' \code{options("cores")}, while for \code{parallel.backend} == "snow",
#' defaults to 2), a strictly positive integer specifying the number of parallel
#' threads, or a more complex specification of the SNOW cluster nodes for
#' \code{parallel.backend} == "snow" (see the documentation of package
#' \code{snow} for details).
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any
#' warnings.
#' @param arguments.that.should.not.be.defined a \emph{list} of argument names
#' and pre-defined valuesfor which a warning should be thrown if passed to the
#' function.
#' @param ... other possible parameters
#' @return An \code{S3} object of class \code{CMA8} (derived from \code{CMA0})
#' with the following fields:
#' \itemize{
#'  \item \code{data} The actual event data, as given by the \code{data}
#'  parameter.
#'  \item \code{ID.colname} the name of the column in \code{data} containing the
#'  unique patient ID, as given by the \code{ID.colname} parameter.
#'  \item \code{event.date.colname} the name of the column in \code{data}
#'  containing the start date of the event (in the format given in the
#'  \code{date.format} parameter), as given by the \code{event.date.colname}
#'  parameter.
#'  \item \code{event.duration.colname} the name of the column in \code{data}
#'  containing the event duration (in days), as given by the
#'  \code{event.duration.colname} parameter.
#'  \item \code{event.daily.dose.colname} the name of the column in \code{data}
#'  containing the prescribed daily dose, as given by the
#'  \code{event.daily.dose.colname} parameter.
#'  \item \code{medication.class.colname} the name of the column in \code{data}
#'  containing the classes/types/groups of medication, as given by the
#'  \code{medication.class.colname} parameter.
#'  \item \code{carry.only.for.same.medication} whether the carry-over applies
#'  only across medication of the same type, as given by the
#'  \code{carry.only.for.same.medication} parameter.
#'  \item \code{consider.dosage.change} whether the carry-over is adjusted to
#'  reflect changes in dosage, as given by the \code{consider.dosage.change}
#'  parameter.
#'  \item \code{followup.window.start} the beginning of the follow-up window, as
#'  given by the \code{followup.window.start} parameter.
#'  \item \code{followup.window.start.unit} the time unit of the
#'  \code{followup.window.start}, as given by the
#'  \code{followup.window.start.unit} parameter.
#'  \item \code{followup.window.duration} the duration of the follow-up window,
#'  as given by the \code{followup.window.duration} parameter.
#'  \item \code{followup.window.duration.unit} the time unit of the
#'  \code{followup.window.duration}, as given by the
#'  \code{followup.window.duration.unit} parameter.
#'  \item \code{observation.window.start} the beginning of the observation
#'  window, as given by the \code{observation.window.start} parameter.
#'  \item \code{observation.window.start.unit} the time unit of the
#'  \code{observation.window.start}, as given by the
#'  \code{observation.window.start.unit} parameter.
#'  \item \code{observation.window.duration} the duration of the observation
#'  window, as given by the \code{observation.window.duration} parameter.
#'  \item \code{observation.window.duration.unit} the time unit of the
#'  \code{observation.window.duration}, as given by the
#'  \code{observation.window.duration.unit} parameter.
#'  \item \code{date.format} the format of the dates, as given by the
#'  \code{date.format} parameter.
#'  \item \code{summary} the metadata, as given by the \code{summary} parameter.
#'  \item \code{event.info} the \code{data.frame} containing the event info
#'  (irrelevant for most users; see \code{\link{compute.event.int.gaps}} for
#'  details).
#'  \item \code{CMA} the \code{data.frame} containing the actual \code{CMA}
#'  estimates for each participant (the \code{ID.colname} column).
#' }
#' Please note that if \code{medication.groups} are defined, then the \code{CMA}
#' and \code{event.info} are named lists, each element containing the CMA and
#' event.info corresponding to a single medication group (the element's name).
#' @seealso CMAs 1 to 8 are defined in:
#'
#' Vollmer, W. M., Xu, M., Feldstein, A., Smith, D., Waterbury, A., & Rand, C.
#' (2012). Comparison of pharmacy-based measures of medication adherence.
#' \emph{BMC Health Services Research}, \strong{12}, 155.
#' \url{http://doi.org/10.1186/1472-6963-12-155}.
#'
#' @examples
#' cma8 <- CMA8(data=med.events,
#'              ID.colname="PATIENT_ID",
#'              event.date.colname="DATE",
#'              event.duration.colname="DURATION",
#'              event.daily.dose.colname="PERDAY",
#'              medication.class.colname="CATEGORY",
#'              carry.only.for.same.medication=FALSE,
#'              consider.dosage.change=FALSE,
#'              followup.window.start=30,
#'              observation.window.start=30,
#'              observation.window.duration=365,
#'              date.format="%m/%d/%Y"
#'             );
#' @export
CMA8 <- function( data=NULL, # the data used to compute the CMA on
                  # Important columns in the data
                  ID.colname=NA, # the name of the column containing the unique patient ID (NA = undefined)
                  event.date.colname=NA, # the start date of the event in the date.format format (NA = undefined)
                  event.duration.colname=NA, # the event duration in days (NA = undefined)
                  event.daily.dose.colname=NA, # the prescribed daily dose (NA = undefined)
                  medication.class.colname=NA, # the classes/types/groups of medication (NA = undefined)
                  # Groups of medication classes:
                  medication.groups=NULL, # a named vector of medication group definitions, the name of a column in the data that defines the groups, or NULL
                  flatten.medication.groups=FALSE, medication.groups.colname=".MED_GROUP_ID", # if medication.groups were defined, return CMAs and event.info as single data.frame?
                  # Various types methods of computing gaps:
                  carry.only.for.same.medication=FALSE, # if TRUE the carry-over applies only across medication of same type (NA = undefined)
                  consider.dosage.change=FALSE, # if TRUE carry-over is adjusted to reflect changes in dosage (NA = undefined)
                  # The follow-up window:
                  followup.window.start=0, # if a number is the earliest event per participant date plus number of units, or a Date object, or a column name in data (NA = undefined)
                  followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  followup.window.start.per.medication.group=FALSE, # if there are medication groups and this is TRUE, then the first event is relative to each medication group separately, otherwise is relative to the patient
                  followup.window.duration=365*2, # the duration of the follow-up window in the time units given below (NA = undefined)
                  followup.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)  (NA = undefined)
                  # The observation window (embedded in the follow-up window):
                  observation.window.start=0, # the number of time units relative to followup.window.start (NA = undefined)
                  observation.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  observation.window.duration=365*2, # the duration of the observation window in time units (NA = undefined)
                  observation.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  # Date format:
                  date.format="%m/%d/%Y", # the format of the dates used in this function (NA = undefined)
                  # Comments and metadata:
                  summary=NA,
                  # The description of the output (added) columns:
                  event.interval.colname="event.interval", # contains number of days between the start of current event and the start of the next
                  gap.days.colname="gap.days", # contains the number of days when medication was not available
                  # Dealing with failed estimates:
                  force.NA.CMA.for.failed.patients=TRUE, # force the failed patients to have NA CM estimate?
                  # Parallel processing:
                  parallel.backend=c("none","multicore","snow","snow(SOCK)","snow(MPI)","snow(NWS)")[1], # parallel backend to use
                  parallel.threads="auto", # specification (or number) of parallel threads
                  # Misc:
                  suppress.warnings=FALSE,
                  arguments.that.should.not.be.defined=c("carryover.within.obs.window"=TRUE,
                                                         "carryover.into.obs.window"=TRUE), # the list of argument names and values for which a warning should be thrown if passed to the function
                  ...
                )
{
  # The summary:
  if( is.na(summary) ) summary <- "The ratio of days with medication available in the whole observation window, with lagged start until previous supply is finished; total number of gap days extracted from this time interval, then divided by the time interval, accounting for carry-over within lagged observation window and excluding remaining supply";

  # Arguments that should not have been passed:
  if( !suppress.warnings && !is.null(arguments.that.should.not.be.defined) )
  {
    # Get the actual list of arguments (including in the ...); the first is the function's own name:
    args.list <- as.list(match.call(expand.dots = TRUE));
    args.mathing <- (names(arguments.that.should.not.be.defined) %in% names(args.list)[-1]);
    if( any(args.mathing) )
    {
      for( i in which(args.mathing) )
      {
        .report.ewms(paste0("Please note that '",args.list[[1]],"' overrides argument '",names(arguments.that.should.not.be.defined)[i],"' with value '",arguments.that.should.not.be.defined[i],"'!\n"), "warning", "CMA8", "AdhereR");
      }
    }
  }

  # Create the CMA0 object:
  ret.val <- CMA0(data=data,
                  ID.colname=ID.colname,
                  event.date.colname=event.date.colname,
                  event.duration.colname=event.duration.colname,
                  event.daily.dose.colname=event.daily.dose.colname,
                  medication.class.colname=medication.class.colname,
                  carry.only.for.same.medication=carry.only.for.same.medication,
                  consider.dosage.change=consider.dosage.change,
                  medication.groups=medication.groups,
                  flatten.medication.groups=flatten.medication.groups,
                  medication.groups.colname=medication.groups.colname,
                  followup.window.start=followup.window.start,
                  followup.window.start.unit=followup.window.start.unit,
                  followup.window.start.per.medication.group=followup.window.start.per.medication.group,
                  followup.window.duration=followup.window.duration,
                  followup.window.duration.unit=followup.window.duration.unit,
                  observation.window.start=observation.window.start,
                  observation.window.start.unit=observation.window.start.unit,
                  observation.window.duration=observation.window.duration,
                  observation.window.duration.unit=observation.window.duration.unit,
                  date.format=date.format,
                  summary=summary,
                  suppress.warnings=suppress.warnings);
  if( is.null(ret.val) ) return (NULL); # some error upstream
  # The followup.window.start and observation.window.start might have been converted to Date:
  followup.window.start <- ret.val$followup.window.start; observation.window.start <- ret.val$observation.window.start;

  # The workhorse auxiliary function: For a given (subset) of data, compute the event intervals and gaps:
  .workhorse.function <- function(data=NULL,
                                  ID.colname=NULL,
                                  event.date.colname=NULL,
                                  event.duration.colname=NULL,
                                  event.daily.dose.colname=NULL,
                                  medication.class.colname=NULL,
                                  event.interval.colname=NULL,
                                  gap.days.colname=NULL,
                                  carryover.within.obs.window=NULL,
                                  carryover.into.obs.window=NULL,
                                  carry.only.for.same.medication=NULL,
                                  consider.dosage.change=NULL,
                                  followup.window.start=NULL,
                                  followup.window.start.unit=NULL,
                                  followup.window.duration=NULL,
                                  followup.window.duration.unit=NULL,
                                  observation.window.start=NULL,
                                  observation.window.start.unit=NULL,
                                  observation.window.duration=NULL,
                                  observation.window.duration.unit=NULL,
                                  date.format=NULL,
                                  suppress.warnings=NULL
  )
  {
    # Auxiliary internal function: Compute the new OW start date:
    .new.OW.start <- function(data4ID)
    {
      # Select the events that start before the observation window but end within it:
      s <- which(data4ID$.START.BEFORE.END.WITHIN.OBS.WND);
      if( length(s) > 0 ) return (max(data4ID$.END.EVENT.DATE[s])) else return (data4ID$.OBS.START.DATE[1]); # new (or old) OW start date
    }

    # Call the compute.event.int.gaps() function and use the results:
    event.info <- compute.event.int.gaps(data=as.data.frame(data),
                                         ID.colname=ID.colname,
                                         event.date.colname=event.date.colname,
                                         event.duration.colname=event.duration.colname,
                                         event.daily.dose.colname=event.daily.dose.colname,
                                         medication.class.colname=medication.class.colname,
                                         event.interval.colname=event.interval.colname,
                                         gap.days.colname=gap.days.colname,
                                         carryover.within.obs.window=carryover.within.obs.window,
                                         carryover.into.obs.window=carryover.into.obs.window,
                                         carry.only.for.same.medication=carry.only.for.same.medication,
                                         consider.dosage.change=consider.dosage.change,
                                         followup.window.start=followup.window.start,
                                         followup.window.start.unit=followup.window.start.unit,
                                         followup.window.duration=followup.window.duration,
                                         followup.window.duration.unit=followup.window.duration.unit,
                                         observation.window.start=observation.window.start,
                                         observation.window.start.unit=observation.window.start.unit,
                                         observation.window.duration=observation.window.duration,
                                         observation.window.duration.unit=observation.window.duration.unit,
                                         date.format=date.format,
                                         keep.window.start.end.dates=TRUE,
                                         parallel.backend="none", # make sure this runs sequentially!
                                         parallel.threads=1,
                                         suppress.warnings=suppress.warnings,
                                         return.data.table=TRUE);
    if( is.null(event.info) ) return (list("CMA"=NA, "event.info"=NULL));

    # Add auxiliary columns to the event.info data.table:
    event.info[, .END.EVENT.DATE := (.DATE.as.Date + get(event.duration.colname) + .CARRY.OVER.FROM.BEFORE) ]; # compute the end date of the events
    event.info[, .START.BEFORE.END.WITHIN.OBS.WND := (.EVENT.STARTS.BEFORE.OBS.WINDOW & (.END.EVENT.DATE > .OBS.START.DATE)) ]; # events that start before the OW but end within it

    # Update the FU start, OW start and OW duration (make sure there is no naming conflict!):
    event.info[, .FU.START.DATE.UPDATED := .FU.START.DATE ]; # FU start date as Date object
    event.info[, .OBS.START.DATE.UPDATED := .new.OW.start(.SD), by=ID.colname ]; # OW start date as Date object
    event.info[, .OBS.DURATION.UPDATED := as.numeric(.OBS.END.DATE - .OBS.START.DATE.UPDATED) ]; # OW duration as number

    # Calling CMA7:
    include.columns <- which(!(names(event.info) %in% c(event.interval.colname, gap.days.colname))); # make sure we don't send these two columns as they'll produce an error in CMA7
    CMA <- CMA7(data=as.data.frame(event.info[, include.columns, with=FALSE]),
                ID.colname=ID.colname,
                event.date.colname=event.date.colname,
                event.duration.colname=event.duration.colname,
                event.daily.dose.colname=event.daily.dose.colname,
                medication.class.colname=medication.class.colname,
                carry.only.for.same.medication=carry.only.for.same.medication,
                consider.dosage.change=consider.dosage.change,
                followup.window.start=".FU.START.DATE.UPDATED",
                followup.window.start.unit="days",
                followup.window.duration=followup.window.duration,
                followup.window.duration.unit=followup.window.duration.unit,
                observation.window.start=".OBS.START.DATE.UPDATED",
                observation.window.start.unit="days",
                observation.window.duration=".OBS.DURATION.UPDATED",
                observation.window.duration.unit="days", # fix it to days
                date.format=date.format,
                parallel.backend="none", # make sure this runs sequentially!
                parallel.threads=1,
                suppress.warnings=suppress.warnings, # suppress warnings from CMA7
                force.NA.CMA.for.failed.patients=FALSE # may enforce this afterwards...
    );
    return (list("CMA"=CMA$CMA, "event.info"=event.info)); # make sure to return the non-adjusted event.info prior to computing CMA7!
  }

  ret.val <- .cma.skeleton(data=data,
                           ret.val=ret.val,
                           cma.class.name=c("CMA8","CMA1"),

                           ID.colname=ID.colname,
                           event.date.colname=event.date.colname,
                           event.duration.colname=event.duration.colname,
                           event.daily.dose.colname=event.daily.dose.colname,
                           medication.class.colname=medication.class.colname,
                           event.interval.colname=event.interval.colname,
                           gap.days.colname=gap.days.colname,
                           carryover.within.obs.window=TRUE,
                           carryover.into.obs.window=TRUE, # if TRUE consider the carry-over from before the starting date of the observation window
                           carry.only.for.same.medication=carry.only.for.same.medication,
                           consider.dosage.change=consider.dosage.change,
                           followup.window.start=followup.window.start,
                           followup.window.start.unit=followup.window.start.unit,
                           followup.window.duration=followup.window.duration,
                           followup.window.duration.unit=followup.window.duration.unit,
                           observation.window.start=observation.window.start,
                           observation.window.start.unit=observation.window.start.unit,
                           observation.window.duration=observation.window.duration,
                           observation.window.duration.unit=observation.window.duration.unit,
                           date.format=date.format,

                           flatten.medication.groups=flatten.medication.groups,
                           followup.window.start.per.medication.group=followup.window.start.per.medication.group,

                           suppress.warnings=suppress.warnings,
                           force.NA.CMA.for.failed.patients=force.NA.CMA.for.failed.patients,
                           parallel.backend=parallel.backend,
                           parallel.threads=parallel.threads,
                           .workhorse.function=.workhorse.function);

  # Save the real observation window as well:
  if( !is.null(ret.val$event.info) )
  {
    if( inherits(ret.val$event.info, "data.frame") )
    {
      ret.val[["real.obs.windows"]] <- unique(data.frame( ret.val$event.info[, ID.colname],
                                                          "window.start"=ret.val$event.info$.OBS.START.DATE.UPDATED,
                                                          "window.end"=NA));
      setnames(ret.val$real.obs.windows, 1, ID.colname);
    } else if( is.list(ret.val$event.info) && length(ret.val$event.info) > 0 )
    {
      ret.val[["real.obs.windows"]] <- lapply(ret.val$event.info, function(x)
        {
          if( is.null(x) ) return (NULL);
          tmp <- unique(data.frame( x[, ID.colname],
                                    "window.start"=x$.OBS.START.DATE.UPDATED,
                                    "window.end"=NA));
          setnames(tmp, 1, ID.colname);
          return (tmp);
        });
      names(ret.val[["real.obs.windows"]]) <- names(ret.val$event.info);
    } else
    {
      ret.val[["real.obs.windows"]] <- NULL;
    }
  } else
  {
    ret.val[["real.obs.windows"]] <- NULL;
  }

  return (ret.val);
}

#' @rdname print.CMA0
#' @export
print.CMA8 <- function(...) print.CMA0(...)

#' @rdname plot.CMA1
#' @export
plot.CMA8 <- function(...) .plot.CMA1plus(...)




#' CMA9 constructor.
#'
#' Constructs a CMA (continuous multiple-interval measures of medication
#' availability/gaps) type 9 object.
#'
#'
#' \code{CMA9} is similar to \code{CMA7} and \code{CMA8} in that it accounts for
#' carry-over within and before the observation window assuming that new
#' medication is "banked" until needed (oversupply from previous events is used
#' first, followed new medication supply). Yet, unlike these previous CMAs, it
#' does not assume the medication is used as prescribed; in longitudinal studies
#' with multiple CMA measures, this assumption may introduce additional
#' variation in CMA estimates depending on when the observation window starts
#' in relation to the previous medication event. A shorter time distance from
#' the previous event (and longer to the first event in the observation window)
#' results in higher values even if the number of gap days is the same, and it
#' may also be that the patient has had a similar use pattern for that time
#' interval, rather than perfect adherence followed by no medication use.
#' \code{CMA9} applies a different adjustment: it computes a ratio of days
#' supply over each interval between two prescriptions and considers this
#' applies for each day of that interval, up to 100\% (moving oversupply to the
#' next event interval). All medication events in the follow up window before
#' observation window are considered for carry-over calculation. The last
#' interval ends at the end of the follow-up window.
#' Thus, it accounts for timing within  the observation window, as well as
#' before (but differently from \code{CMA7} and \code{CMA8}), and excludes the
#' remaining supply at the end of the observation window, if any.
#'
#' The formula is
#' \deqn{(number of days in the observation window, each weighted by the ratio
#' of days supply applicable to their event interval) / (start to end of
#' observation window)}
#'
#' Observations:
#' \itemize{
#'  \item the \code{carry.only.for.same.medication} parameter controls the
#'  transmission of carry-over across medication changes, producing a "standard"
#'  \code{CMA7} (default value is FALSE), and an "alternative" \code{CMA7b},
#'  respectively;
#'  \item the \code{consider.dosage.change} parameter controls if dosage changes
#'  are taken into account, i.e. if set as TRUE and a new medication event has a
#'  different daily dosage recommendation, carry-over is recomputed assuming
#'  medication use according to the new prescribed dosage (default value is
#'  FALSE).
#' }
#'
#' @param data A \emph{\code{data.frame}} containing the events used to compute
#' the CMA. Must contain, at a minimum, the patient unique ID, the event date
#' and duration, and might also contain the daily dosage and medication type
#' (the actual column names are defined in the following four parameters).
#' @param ID.colname A \emph{string}, the name of the column in \code{data}
#' containing the unique patient ID; must be present.
#' @param event.date.colname A \emph{string}, the name of the column in
#' \code{data} containing the start date of the event (in the format given in
#' the \code{date.format} parameter); must be present.
#' @param event.duration.colname A \emph{string}, the name of the column in
#' \code{data} containing the event duration (in days); must be present.
#' @param event.daily.dose.colname A \emph{string}, the name of the column in
#' \code{data} containing the prescribed daily dose, or \code{NA} if not defined.
#' @param medication.class.colname A \emph{string}, the name of the column in
#' \code{data} containing the medication type, or \code{NA} if not defined.
#' @param medication.groups A \emph{vector} of characters defining medication
#' groups or the name of a column in \code{data} that defines such groups.
#' The names of the vector are the medication group unique names, while
#' the content defines them as logical expressions. While the names can be any
#' string of characters except "\}", it is recommended to stick to the rules for
#' defining vector names in \code{R}. For example,
#' \code{c("A"="CATEGORY == 'medA'", "AA"="{A} & PERDAY < 4"} defines two
#' medication groups: \emph{A} which selects all events of type "medA", and
#' \emph{B} which selects all events already defined by "A" but with a daily
#' dose lower than 4. If \code{NULL}, no medication groups are defined. If
#' medication groups are defined, there is one CMA estimate for each group;
#' moreover, there is a special group \emph{__ALL_OTHERS__} automatically defined
#' containing all observations \emph{not} covered by any of the explicitly defined
#' groups.
#' @param flatten.medication.groups \emph{Logical}, if \code{FALSE} (the default)
#' then the \code{CMA} and \code{event.info} components of the object are lists
#' with one medication group per element; otherwise, they are \code{data.frame}s
#' with an extra column containing the medication group (its name is given by
#' \code{medication.groups.colname}).
#' @param medication.groups.colname a \emph{string} (defaults to ".MED_GROUP_ID")
#' giving the name of the column storing the group name when
#' \code{flatten.medication.groups} is \code{TRUE}.
#' @param carry.only.for.same.medication \emph{Logical}, if \code{TRUE}, the
#' carry-over applies only across medication of the same type.
#' @param consider.dosage.change \emph{Logical}, if \code{TRUE}, the carry-over
#' is adjusted to also reflect changes in dosage.
#' @param followup.window.start If a \emph{\code{Date}} object, it represents
#' the actual start date of the follow-up window; if a \emph{string} it is the
#' name of the column in \code{data} containing the start date of the follow-up
#' window either as the numbers of \code{followup.window.start.unit} units after
#' the first event (the column must be of type \code{numeric}) or as actual
#' dates (in which case the column must be of type \code{Date} or a string
#' that conforms to the format specified in \code{date.format}); if a
#' \emph{number} it is the number of time units defined in the
#' \code{followup.window.start.unit} parameter after the begin of the
#' participant's first event; or \code{NA} if not defined.
#' @param followup.window.start.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.start} refers to (when a number), or
#' \code{NA} if not defined.
#' @param followup.window.start.per.medication.group a \emph{logical}: if there are
#' medication groups defined and this is \code{TRUE}, then the first event
#' considered for the follow-up window start is relative to each medication group
#' separately, otherwise (the default) it is relative to the patient.
#' @param followup.window.duration either a \emph{number} representing the
#' duration of the follow-up window in the time units given in
#' \code{followup.window.duration.unit}, or a \emph{string} giving the column
#' containing these numbers. Should represent a period for which relevant
#' medication events are recorded accurately (e.g. not extend after end of
#' relevant treatment, loss-to-follow-up or change to a health care provider not
#' covered by the database).
#' @param followup.window.duration.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.duration} refers to, or \code{NA} if not
#' defined.
#' @param observation.window.start,observation.window.start.unit,observation.window.duration,observation.window.duration.unit the definition of the observation window
#' (see the follow-up window parameters above for details).
#' @param date.format A \emph{string} giving the format of the dates used in the
#' \code{data} and the other parameters; see the \code{format} parameters of the
#' \code{\link[base]{as.Date}} function for details (NB, this concerns only the
#' dates given as strings and not as \code{Date} objects).
#' @param summary Metadata as a \emph{string}, briefly describing this CMA.
#' @param event.interval.colname A \emph{string}, the name of a newly-created
#' column storing the number of days between the start of the current event and
#' the start of the next one; the default value "event.interval" should be
#' changed only if there is a naming conflict with a pre-existing
#' "event.interval" column in \code{event.info}.
#' @param gap.days.colname A \emph{string}, the name of a newly-created column
#' storing the number of days when medication was not available (i.e., the
#' "gap days"); the default value "gap.days" should be changed only if there is
#' a naming conflict with a pre-existing "gap.days" column in \code{event.info}.
#' @param force.NA.CMA.for.failed.patients \emph{Logical} describing how the
#' patients for which the CMA estimation fails are treated: if \code{TRUE}
#' they are returned with an \code{NA} CMA estimate, while for
#' \code{FALSE} they are omitted.
#' @param parallel.backend Can be "none" (the default) for single-threaded
#' execution, "multicore"  (using \code{mclapply} in package \code{parallel})
#' for multicore processing (NB. not currently implemented on MS Windows and
#' automatically falls back on "snow" on this platform),  or "snow",
#' "snow(SOCK)" (equivalent to "snow"), "snow(MPI)" or "snow(NWS)" specifying
#' various types of SNOW clusters (can be on the local machine or more complex
#' setups -- please see the documentation of package \code{snow} for details;
#' the last two require packages \code{Rmpi} and \code{nws}, respectively, not
#' automatically installed with \code{AdhereR}).
#' @param parallel.threads Can be "auto" (for \code{parallel.backend} ==
#' "multicore", defaults to the number of cores in the system as given by
#' \code{options("cores")}, while for \code{parallel.backend} == "snow",
#' defaults to 2), a strictly positive integer specifying the number of parallel
#' threads, or a more complex specification of the SNOW cluster nodes for
#' \code{parallel.backend} == "snow" (see the documentation of package
#' \code{snow} for details).
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any
#' warnings.
#' @param arguments.that.should.not.be.defined a \emph{list} of argument names
#' and pre-defined valuesfor which a warning should be thrown if passed to the
#' function.
#' @param ... other possible parameters
#' @return An \code{S3} object of class \code{CMA9} (derived from \code{CMA0})
#' with the following fields:
#' \itemize{
#'  \item \code{data} The actual event data, as given by the \code{data}
#'  parameter.
#'  \item \code{ID.colname} the name of the column in \code{data} containing the
#'  unique patient ID, as given by the \code{ID.colname} parameter.
#'  \item \code{event.date.colname} the name of the column in \code{data}
#'  containing the start date of the event (in the format given in the
#'  \code{date.format} parameter), as given by the \code{event.date.colname}
#'  parameter.
#'  \item \code{event.duration.colname} the name of the column in \code{data}
#'  containing the event duration (in days), as given by the
#'  \code{event.duration.colname} parameter.
#'  \item \code{event.daily.dose.colname} the name of the column in \code{data}
#'  containing the prescribed daily dose, as given by the
#'  \code{event.daily.dose.colname} parameter.
#'  \item \code{medication.class.colname} the name of the column in \code{data}
#'  containing the classes/types/groups of medication, as given by the
#'  \code{medication.class.colname} parameter.
#'  \item \code{carry.only.for.same.medication} whether the carry-over applies
#'  only across medication of the same type, as given by the
#'  \code{carry.only.for.same.medication} parameter.
#'  \item \code{consider.dosage.change} whether the carry-over is adjusted to
#'  reflect changes in dosage, as given by the \code{consider.dosage.change}
#'  parameter.
#'  \item \code{followup.window.start} the beginning of the follow-up window,
#'  as given by the \code{followup.window.start} parameter.
#'  \item \code{followup.window.start.unit} the time unit of the
#'  \code{followup.window.start}, as given by the
#'  \code{followup.window.start.unit} parameter.
#'  \item \code{followup.window.duration} the duration of the follow-up window,
#'  as given by the \code{followup.window.duration} parameter.
#'  \item \code{followup.window.duration.unit} the time unit of the
#'  \code{followup.window.duration}, as given by the
#'  \code{followup.window.duration.unit} parameter.
#'  \item \code{observation.window.start} the beginning of the observation
#'  window, as given by the \code{observation.window.start} parameter.
#'  \item \code{observation.window.start.unit} the time unit of the
#'  \code{observation.window.start}, as given by the
#'  \code{observation.window.start.unit} parameter.
#'  \item \code{observation.window.duration} the duration of the observation
#'  window, as given by the \code{observation.window.duration} parameter.
#'  \item \code{observation.window.duration.unit} the time unit of the
#'  \code{observation.window.duration}, as given by the
#'  \code{observation.window.duration.unit} parameter.
#'  \item \code{date.format} the format of the dates, as given by the
#'  \code{date.format} parameter.
#'  \item \code{summary} the metadata, as given by the \code{summary} parameter.
#'  \item \code{event.info} the \code{data.frame} containing the event info
#'  (irrelevant for most users; see \code{\link{compute.event.int.gaps}} for
#'  details).
#'  \item \code{CMA} the \code{data.frame} containing the actual \code{CMA}
#'  estimates for each participant (the \code{ID.colname} column).
#' }
#' Please note that if \code{medication.groups} are defined, then the \code{CMA}
#' and \code{event.info} are named lists, each element containing the CMA and
#' event.info corresponding to a single medication group (the element's name).
#' @examples
#' cma9 <- CMA9(data=med.events,
#'              ID.colname="PATIENT_ID",
#'              event.date.colname="DATE",
#'              event.duration.colname="DURATION",
#'              event.daily.dose.colname="PERDAY",
#'              medication.class.colname="CATEGORY",
#'              carry.only.for.same.medication=FALSE,
#'              consider.dosage.change=FALSE,
#'              followup.window.start=30,
#'              observation.window.start=30,
#'              observation.window.duration=365,
#'              date.format="%m/%d/%Y"
#'             );
#' @export
CMA9 <- function( data=NULL, # the data used to compute the CMA on
                  # Important columns in the data
                  ID.colname=NA, # the name of the column containing the unique patient ID (NA = undefined)
                  event.date.colname=NA, # the start date of the event in the date.format format (NA = undefined)
                  event.duration.colname=NA, # the event duration in days (NA = undefined)
                  event.daily.dose.colname=NA, # the prescribed daily dose (NA = undefined)
                  medication.class.colname=NA, # the classes/types/groups of medication (NA = undefined)
                  # Groups of medication classes:
                  medication.groups=NULL, # a named vector of medication group definitions, the name of a column in the data that defines the groups, or NULL
                  flatten.medication.groups=FALSE, medication.groups.colname=".MED_GROUP_ID", # if medication.groups were defined, return CMAs and event.info as single data.frame?
                  # Various types methods of computing gaps:
                  carry.only.for.same.medication=FALSE, # if TRUE the carry-over applies only across medication of same type (NA = undefined)
                  consider.dosage.change=FALSE, # if TRUE carry-over is adjusted to reflect changes in dosage (NA = undefined)
                  # The follow-up window:
                  followup.window.start=0, # if a number is the earliest event per participant date + number of units, or a Date object, or a column name in data (NA = undefined)
                  followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  followup.window.start.per.medication.group=FALSE, # if there are medication groups and this is TRUE, then the first event is relative to each medication group separately, otherwise is relative to the patient
                  followup.window.duration=365*2, # the duration of the follow-up window in the time units given below (NA = undefined)
                  followup.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)  (NA = undefined)
                  # The observation window (embedded in the follow-up window):
                  observation.window.start=0, # the number of time units relative to followup.window.start (NA = undefined)
                  observation.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  observation.window.duration=365*2, # the duration of the observation window in time units (NA = undefined)
                  observation.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                  # Date format:
                  date.format="%m/%d/%Y", # the format of the dates used in this function (NA = undefined)
                  # Comments and metadata:
                  summary=NA,
                  # The description of the output (added) columns:
                  event.interval.colname="event.interval", # contains number of days between the start of current event and the start of the next
                  gap.days.colname="gap.days", # contains the number of days when medication was not available
                  # Dealing with failed estimates:
                  force.NA.CMA.for.failed.patients=TRUE, # force the failed patients to have NA CM estimate?
                  # Parallel processing:
                  parallel.backend=c("none","multicore","snow","snow(SOCK)","snow(MPI)","snow(NWS)")[1], # parallel backend to use
                  parallel.threads="auto", # specification (or number) of parallel threads
                  # Misc:
                  suppress.warnings=FALSE,
                  arguments.that.should.not.be.defined=c("carryover.within.obs.window"=TRUE,
                                                         "carryover.into.obs.window"=TRUE), # the list of argument names and values for which a warning should be thrown if passed to the function
                  ...
                )
{
  # The summary:
  if( is.na(summary) ) summary <- "The ratio of days with medication available in the observation window; the supply of each event is evenly spread until the next event (ratio days supply up to 1), then oversupply carried over to the next event; the each day in the observation window is weighted by its ratio of days supply, and the sum divided by the duration of the observation window";

  # Arguments that should not have been passed:
  if( !suppress.warnings && !is.null(arguments.that.should.not.be.defined) )
  {
    # Get the actual list of arguments (including in the ...); the first is the function's own name:
    args.list <- as.list(match.call(expand.dots = TRUE));
    args.mathing <- (names(arguments.that.should.not.be.defined) %in% names(args.list)[-1]);
    if( any(args.mathing) )
    {
      for( i in which(args.mathing) )
      {
        .report.ewms(paste0("Please note that '",args.list[[1]],"' overrides argument '",names(arguments.that.should.not.be.defined)[i],"' with value '",arguments.that.should.not.be.defined[i],"'!\n"), "warning", "CMA9", "AdhereR");
      }
    }
  }

  # Create the CMA0 object:
  ret.val <- CMA0(data=data,
                  ID.colname=ID.colname,
                  event.date.colname=event.date.colname,
                  event.duration.colname=event.duration.colname,
                  event.daily.dose.colname=event.daily.dose.colname,
                  medication.class.colname=medication.class.colname,
                  carry.only.for.same.medication=carry.only.for.same.medication,
                  consider.dosage.change=consider.dosage.change,
                  medication.groups=medication.groups,
                  flatten.medication.groups=flatten.medication.groups,
                  medication.groups.colname=medication.groups.colname,
                  followup.window.start=followup.window.start,
                  followup.window.start.unit=followup.window.start.unit,
                  followup.window.start.per.medication.group=followup.window.start.per.medication.group,
                  followup.window.duration=followup.window.duration,
                  followup.window.duration.unit=followup.window.duration.unit,
                  observation.window.start=observation.window.start,
                  observation.window.start.unit=observation.window.start.unit,
                  observation.window.duration=observation.window.duration,
                  observation.window.duration.unit=observation.window.duration.unit,
                  date.format=date.format,
                  summary=summary,
                  suppress.warnings=suppress.warnings);
  if( is.null(ret.val) ) return (NULL); # some error upstream
  # The followup.window.start and observation.window.start might have been converted to Date:
  followup.window.start <- ret.val$followup.window.start; observation.window.start <- ret.val$observation.window.start;

  # The workhorse auxiliary function: For a given (subset) of data, compute the event intervals and gaps:
  .workhorse.function <- function(data=NULL,
                                  ID.colname=NULL,
                                  event.date.colname=NULL,
                                  event.duration.colname=NULL,
                                  event.daily.dose.colname=NULL,
                                  medication.class.colname=NULL,
                                  event.interval.colname=NULL,
                                  gap.days.colname=NULL,
                                  carryover.within.obs.window=NULL,
                                  carryover.into.obs.window=NULL,
                                  carry.only.for.same.medication=NULL,
                                  consider.dosage.change=NULL,
                                  followup.window.start=NULL,
                                  followup.window.start.unit=NULL,
                                  followup.window.duration=NULL,
                                  followup.window.duration.unit=NULL,
                                  observation.window.start=NULL,
                                  observation.window.start.unit=NULL,
                                  observation.window.duration=NULL,
                                  observation.window.duration.unit=NULL,
                                  date.format=NULL,
                                  suppress.warnings=NULL
  )
  {
    # Auxiliary internal function: Compute the CMA for a given patient:
    .process.patient <- function(data4ID)
    {
      n.events <- nrow(data4ID); # cache number of events
      # Caching;
      .obs.start.actual <- data4ID$.OBS.START.DATE.ACTUAL[1]; .obs.end.actual <- data4ID$.OBS.END.DATE.ACTUAL[1];
      # Select the events within the observation window:
      s <- which(data4ID$.DATE.as.Date <= .obs.end.actual);
      if( length(s) == 0 )
      {
        return (NA_real_);
      } else
      {
        # Compute the ratios of CMA within the FUW:
        .CMA.PER.INTERVAL <- (1 - data4ID[,get(gap.days.colname)] / data4ID[,get(event.interval.colname)]);
        event.start <- data4ID$.DATE.as.Date; event.end <- (event.start + data4ID[,get(event.interval.colname)]);
        int.start <- pmax(event.start, .obs.start.actual); int.end <- pmin(event.end, .obs.end.actual);
        .EVENT.INTERVAL.IN.OBS.WIN <- pmax(0, as.numeric(int.end - int.start));
        # Weight the days by the ratio:
        return (sum(.CMA.PER.INTERVAL * .EVENT.INTERVAL.IN.OBS.WIN, na.rm=TRUE) /
                      as.numeric(.obs.end.actual - .obs.start.actual));
      }
    }

    # ATTENTION: this is done for an observation window that is identical to the follow-up window to accommodate events that overshoot the observation window!
    event.info <- compute.event.int.gaps(data=as.data.frame(data),
                                         ID.colname=ID.colname,
                                         event.date.colname=event.date.colname,
                                         event.duration.colname=event.duration.colname,
                                         event.daily.dose.colname=event.daily.dose.colname,
                                         medication.class.colname=medication.class.colname,
                                         event.interval.colname=event.interval.colname,
                                         gap.days.colname=gap.days.colname,
                                         carryover.within.obs.window=TRUE,
                                         carryover.into.obs.window=TRUE,
                                         carry.only.for.same.medication=carry.only.for.same.medication,
                                         consider.dosage.change=consider.dosage.change,
                                         followup.window.start=followup.window.start,
                                         followup.window.start.unit=followup.window.start.unit,
                                         followup.window.duration=followup.window.duration,
                                         followup.window.duration.unit=followup.window.duration.unit,
                                         observation.window.start=0, # force follow-up window start at 0!
                                         observation.window.start.unit="days",
                                         observation.window.duration=followup.window.duration,
                                         observation.window.duration.unit=followup.window.duration.unit,
                                         date.format=date.format,
                                         keep.window.start.end.dates=TRUE,
                                         keep.event.interval.for.all.events=TRUE,
                                         parallel.backend="none", # make sure this runs sequentially!
                                         parallel.threads=1,
                                         suppress.warnings=suppress.warnings,
                                         return.data.table=TRUE);
    if( is.null(event.info) ) return (list("CMA"=NA, "event.info"=NULL));

    # Also compute the actual observation window start and end:
    #patinfo.cols <- which(names(event.info) %in% c(ID.colname, event.date.colname, event.duration.colname, event.daily.dose.colname, medication.class.colname));
    #tmp <- as.data.frame(data); tmp <- tmp[!duplicated(tmp[,ID.colname]),patinfo.cols]; # the reduced dataset for comuting the actual OW:
    tmp <- as.data.frame(data); tmp <- tmp[!duplicated(tmp[,ID.colname]),names(data)]; # the reduced dataset for comuting the actual OW:
    actual.obs.win <- compute.event.int.gaps(data=tmp,
                                             ID.colname=ID.colname,
                                             event.date.colname=event.date.colname,
                                             event.duration.colname=event.duration.colname,
                                             event.daily.dose.colname=event.daily.dose.colname,
                                             medication.class.colname=medication.class.colname,
                                             event.interval.colname=event.interval.colname,
                                             gap.days.colname=gap.days.colname,
                                             carryover.within.obs.window=FALSE,
                                             carryover.into.obs.window=FALSE,
                                             carry.only.for.same.medication=FALSE,
                                             consider.dosage.change=FALSE,
                                             followup.window.start=followup.window.start,
                                             followup.window.start.unit=followup.window.start.unit,
                                             followup.window.duration=followup.window.duration,
                                             followup.window.duration.unit=followup.window.duration.unit,
                                             observation.window.start=observation.window.start,
                                             observation.window.start.unit=observation.window.start.unit,
                                             observation.window.duration=observation.window.duration,
                                             observation.window.duration.unit=observation.window.duration.unit,
                                             date.format=date.format,
                                             keep.window.start.end.dates=TRUE,
                                             remove.events.outside.followup.window=FALSE,
                                             parallel.backend="none", # make sure this runs sequentially!
                                             parallel.threads=1,
                                             suppress.warnings=suppress.warnings,
                                             return.data.table=TRUE);
    if( is.null(actual.obs.win) ) return (list("CMA"=NA, "event.info"=NULL));

    # Add the actual OW dates to event.info:
    actual.obs.win <- actual.obs.win[,c(ID.colname,".OBS.START.DATE",".OBS.END.DATE"),with=FALSE]
    setkeyv(actual.obs.win, ID.colname);
    event.info <- merge(event.info, actual.obs.win, all.x=TRUE); setnames(event.info, ncol(event.info)-c(1,0), c(".OBS.START.DATE.ACTUAL", ".OBS.END.DATE.ACTUAL"));

    CMA <- event.info[, .process.patient(.SD), by=ID.colname];

    # Make sure the information reflects the real observation window:
    event.info <- compute.event.int.gaps(data=as.data.frame(data),
                                         ID.colname=ID.colname,
                                         event.date.colname=event.date.colname,
                                         event.duration.colname=event.duration.colname,
                                         event.daily.dose.colname=event.daily.dose.colname,
                                         medication.class.colname=medication.class.colname,
                                         event.interval.colname=event.interval.colname,
                                         gap.days.colname=gap.days.colname,
                                         carryover.within.obs.window=TRUE,
                                         carryover.into.obs.window=TRUE, # if TRUE consider the carry-over from before the starting date of the observation window
                                         carry.only.for.same.medication=carry.only.for.same.medication,
                                         consider.dosage.change=consider.dosage.change,
                                         followup.window.start=followup.window.start,
                                         followup.window.start.unit=followup.window.start.unit,
                                         followup.window.duration=followup.window.duration,
                                         followup.window.duration.unit=followup.window.duration.unit,
                                         observation.window.start=observation.window.start,
                                         observation.window.start.unit=observation.window.start.unit,
                                         observation.window.duration=observation.window.duration,
                                         observation.window.duration.unit=observation.window.duration.unit,
                                         date.format=date.format,
                                         keep.window.start.end.dates=TRUE,
                                         keep.event.interval.for.all.events=TRUE,
                                         parallel.backend="none", # make sure this runs sequentially!
                                         parallel.threads=1,
                                         suppress.warnings=suppress.warnings,
                                         return.data.table=TRUE);

    return (list("CMA"=CMA, "event.info"=event.info));
  }

  ret.val <- .cma.skeleton(data=data,
                           ret.val=ret.val,
                           cma.class.name=c("CMA9","CMA1"),

                           ID.colname=ID.colname,
                           event.date.colname=event.date.colname,
                           event.duration.colname=event.duration.colname,
                           event.daily.dose.colname=event.daily.dose.colname,
                           medication.class.colname=medication.class.colname,
                           event.interval.colname=event.interval.colname,
                           gap.days.colname=gap.days.colname,
                           carryover.within.obs.window=TRUE,
                           carryover.into.obs.window=TRUE,
                           carry.only.for.same.medication=carry.only.for.same.medication,
                           consider.dosage.change=consider.dosage.change,
                           followup.window.start=followup.window.start,
                           followup.window.start.unit=followup.window.start.unit,
                           followup.window.duration=followup.window.duration,
                           followup.window.duration.unit=followup.window.duration.unit,
                           observation.window.start=observation.window.start,
                           observation.window.start.unit=observation.window.start.unit,
                           observation.window.duration=observation.window.duration,
                           observation.window.duration.unit=observation.window.duration.unit,
                           date.format=date.format,

                           flatten.medication.groups=flatten.medication.groups,
                           followup.window.start.per.medication.group=followup.window.start.per.medication.group,

                           suppress.warnings=suppress.warnings,
                           force.NA.CMA.for.failed.patients=force.NA.CMA.for.failed.patients,
                           parallel.backend=parallel.backend,
                           parallel.threads=parallel.threads,
                           .workhorse.function=.workhorse.function);

  return (ret.val);
}

#' @rdname print.CMA0
#' @export
print.CMA9 <- function(...) print.CMA0(...)

#' @rdname plot.CMA1
#' @export
plot.CMA9 <- function(...) .plot.CMA1plus(...)




#' CMA_per_episode constructor.
#'
#' Applies a given CMA to each treatment episode and constructs a
#' CMA_per_episode object.
#'
#' \code{CMA_per_episode} first identifies the treatment episodes for the whole
#' follo-up window (using the \code{\link{compute.treatment.episodes}} function),
#' and then computes the given "simple" CMA for each treatment episode that
#' intersects with the observation window. NB: the CMA is computed for the
#' period of the episode that is part of the observations window; thus, if an
#' episode starts earlier or ends later than the observation window, CMA will
#' be computed for a section of that episode.
#' Thus, as opposed to the "simple" CMAs 1 to 9, it returns a set of CMAs, with
#' possibly more than one element.
#'
#' It is highly similar to \code{\link{CMA_sliding_window}} which computes a CMA
#' for a set of sliding windows.
#'
#' @param CMA.to.apply A \emph{string} giving the name of the CMA function (1 to
#' 9) that will be computed for each treatment episode.
#' @param data A \emph{\code{data.frame}} containing the events (prescribing or
#' dispensing) used to compute the CMA. Must contain, at a minimum, the patient
#' unique ID, the event date and duration, and might also contain the daily
#' dosage and medication type (the actual column names are defined in the
#' following four parameters).
#' @param treat.epi A \emph{\code{data.frame}} containing the treatment episodes.
#' Must contain the patient ID (as given in \code{ID.colname}), the episode unique ID
#' (increasing sequentially, \code{episode.ID}), the episode start date
#' (\code{episode.start}), the episode duration in days (\code{episode.duration}),
#' and the episode end date (\code{episode.end}).
#' @param ID.colname A \emph{string}, the name of the column in \code{data}
#' containing the unique patient ID; must be present.
#' @param event.date.colname A \emph{string}, the name of the column in
#' \code{data} containing the start date of the event (in the format given in
#' the \code{date.format} parameter); must be present.
#' @param event.duration.colname A \emph{string}, the name of the column in
#' \code{data} containing the event duration (in days); must be present.
#' @param event.daily.dose.colname A \emph{string}, the name of the column in
#' \code{data} containing the prescribed daily dose, or \code{NA} if not defined.
#' @param medication.class.colname A \emph{string}, the name of the column in
#' \code{data} containing the medication type, or \code{NA} if not defined.
#' @param medication.groups A \emph{vector} of characters defining medication
#' groups or the name of a column in \code{data} that defines such groups.
#' The names of the vector are the medication group unique names, while
#' the content defines them as logical expressions. While the names can be any
#' string of characters except "\}", it is recommended to stick to the rules for
#' defining vector names in \code{R}. For example,
#' \code{c("A"="CATEGORY == 'medA'", "AA"="{A} & PERDAY < 4"} defines two
#' medication groups: \emph{A} which selects all events of type "medA", and
#' \emph{B} which selects all events already defined by "A" but with a daily
#' dose lower than 4. If \code{NULL}, no medication groups are defined. If
#' medication groups are defined, there is one CMA estimate for each group;
#' moreover, there is a special group \emph{__ALL_OTHERS__} automatically defined
#' containing all observations \emph{not} covered by any of the explicitly defined
#' groups.
#' @param flatten.medication.groups \emph{Logical}, if \code{FALSE} (the default)
#' then the \code{CMA} and \code{event.info} components of the object are lists
#' with one medication group per element; otherwise, they are \code{data.frame}s
#' with an extra column containing the medication group (its name is given by
#' \code{medication.groups.colname}).
#' @param medication.groups.colname a \emph{string} (defaults to ".MED_GROUP_ID")
#' giving the name of the column storing the group name when
#' \code{flatten.medication.groups} is \code{TRUE}.
#' @param carry.only.for.same.medication \emph{Logical}, if \code{TRUE}, the
#' carry-over applies only across medication of the same type; valid only for
#' CMAs 5 to 9, in which case it is coupled (i.e., the same value is used for
#' computing the treatment episodes and the CMA on each treatment episode).
#' @param consider.dosage.change \emph{Logical}, if \code{TRUE}, the carry-over
#' is adjusted to also reflect changes in dosage; valid only for CMAs 5 to 9, in
#' which case it is coupled (i.e., the same value is used for computing the
#' treatment episodes and the CMA on each treatment episode).
#' @param medication.change.means.new.treatment.episode \emph{Logical}, should a
#' change in medication automatically start a new treatment episode?
#' @param dosage.change.means.new.treatment.episode \emph{Logical}, should a
#' change in dosage automatically start a new treatment episode?
#' @param maximum.permissible.gap The \emph{number} of units given by
#' \code{maximum.permissible.gap.unit} representing the maximum duration of
#' permissible gaps between treatment episodes (can also be a percent, see
#' \code{maximum.permissible.gap.unit} for details).
#' @param maximum.permissible.gap.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"}, \emph{"years"} or \emph{"percent"}, and
#' represents the time units that \code{maximum.permissible.gap} refers to;
#' if \emph{percent}, then  \code{maximum.permissible.gap} is interpreted as a
#' percent (can be greater than 100\%) of the duration of the current
#' prescription.
#' @param maximum.permissible.gap.append.to.episode.proportion a \emph{number}
#' giving the proportion of the \code{maximum.permissible.gap} to append at the
#' end of an episode with a gap larger than the \code{maximum.permissible.gap};
#' varies between 0.0 (no addition, the default) and 1.0 (the full
#' \code{maximum.permissible.gap} is added).
#' @param followup.window.start If a \emph{\code{Date}} object, it represents
#' the actual start date of the follow-up window; if a \emph{string} it is the
#' name of the column in \code{data} containing the start date of the follow-up
#' window either as the numbers of \code{followup.window.start.unit} units after
#' the first event (the column must be of type \code{numeric}) or as actual
#' dates (in which case the column must be of type \code{Date} or a string
#' that conforms to the format specified in \code{date.format}); if a
#' \emph{number} it is the number of time units defined in the
#' \code{followup.window.start.unit} parameter after the begin of the
#' participant's first event; or \code{NA} if not defined.
#' @param followup.window.start.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.start} refers to (when a number), or
#' \code{NA} if not defined.
#' @param followup.window.start.per.medication.group a \emph{logical}: if there are
#' medication groups defined and this is \code{TRUE}, then the first event
#' considered for the follow-up window start is relative to each medication group
#' separately, otherwise (the default) it is relative to the patient.
#' @param followup.window.duration either a \emph{number} representing the
#' duration of the follow-up window in the time units given in
#' \code{followup.window.duration.unit}, or a \emph{string} giving the column
#' containing these numbers. Should represent a period for which relevant
#' medication events are recorded accurately (e.g. not extend after end of
#' relevant treatment, loss-to-follow-up or change to a health care provider
#' not covered by the database).
#' @param followup.window.duration.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.duration} refers to, or \code{NA} if not
#' defined.
#' @param observation.window.start,observation.window.start.unit,observation.window.duration,observation.window.duration.unit the definition of the observation window
#' (see the follow-up window parameters above for details).
#' @param date.format A \emph{string} giving the format of the dates used in the
#' \code{data} and the other parameters; see the \code{format} parameters of the
#' \code{\link[base]{as.Date}} function for details (NB, this concerns only the
#' dates given as strings and not as \code{Date} objects).
#' @param summary Metadata as a \emph{string}, briefly describing this CMA.
#' @param event.interval.colname A \emph{string}, the name of a newly-created
#' column storing the number of days between the start of the current event and
#' the start of the next one; the default value "event.interval" should be
#' changed only if there is a naming conflict with a pre-existing
#' "event.interval" column in \code{event.info}.
#' @param gap.days.colname A \emph{string}, the name of a newly-created column
#' storing the number of days when medication was not available (i.e., the
#' "gap days"); the default value "gap.days" should be changed only if there is
#' a naming conflict with a pre-existing "gap.days" column in \code{event.info}.
#' @param force.NA.CMA.for.failed.patients \emph{Logical} describing how the
#' patients for which the CMA estimation fails are treated: if \code{TRUE}
#' they are returned with an \code{NA} CMA estimate, while for
#' \code{FALSE} they are omitted.
#' @param parallel.backend Can be "none" (the default) for single-threaded
#' execution, "multicore"  (using \code{mclapply} in package \code{parallel})
#' for multicore processing (NB. not currently implemented on MS Windows and
#' automatically falls back on "snow" on this platform),  or "snow",
#' "snow(SOCK)" (equivalent to "snow"), "snow(MPI)" or "snow(NWS)" specifying
#' various types of SNOW clusters (can be on the local machine or more complex
#' setups -- please see the documentation of package \code{snow} for details;
#' the last two require packages \code{Rmpi} and \code{nws}, respectively, not
#' automatically installed with \code{AdhereR}).
#' @param parallel.threads Can be "auto" (for \code{parallel.backend} ==
#' "multicore", defaults to the number of cores in the system as given by
#' \code{options("cores")}, while for \code{parallel.backend} == "snow",
#' defaults to 2), a strictly positive integer specifying the number of parallel
#' threads, or a more complex specification of the SNOW cluster nodes for
#' \code{parallel.backend} == "snow" (see the documentation of package
#' \code{snow} for details).
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any
#' warnings.
#' @param ... other possible parameters
#' @return An \code{S3} object of class \code{CMA_per_episode} with the
#' following fields:
#' \itemize{
#'  \item \code{data} The actual event data, as given by the \code{data}
#'  parameter.
#'  \item \code{ID.colname} the name of the column in \code{data} containing the
#'  unique patient ID, as given by the \code{ID.colname} parameter.
#'  \item \code{event.date.colname} the name of the column in \code{data}
#'  containing the start date of the event (in the format given in the
#'  \code{date.format} parameter), as given by the \code{event.date.colname}
#'  parameter.
#'  \item \code{event.duration.colname} the name of the column in \code{data}
#'  containing the event duration (in days), as given by the
#'  \code{event.duration.colname} parameter.
#'  \item \code{event.daily.dose.colname} the name of the column in \code{data}
#'  containing the prescribed daily dose, as given by the
#'  \code{event.daily.dose.colname} parameter.
#'  \item \code{medication.class.colname} the name of the column in \code{data}
#'  containing the classes/types/groups of medication, as given by the
#'  \code{medication.class.colname} parameter.
#'  \item \code{carry.only.for.same.medication} whether the carry-over applies
#'  only across medication of the same type, as given by the
#'  \code{carry.only.for.same.medication} parameter.
#'  \item \code{consider.dosage.change} whether the carry-over is adjusted to
#'  reflect changes in dosage, as given by the \code{consider.dosage.change}
#'  parameter.
#'  \item \code{followup.window.start} the beginning of the follow-up window, as
#'  given by the \code{followup.window.start} parameter.
#'  \item \code{followup.window.start.unit} the time unit of the
#'  \code{followup.window.start}, as given by the
#'  \code{followup.window.start.unit} parameter.
#'  \item \code{followup.window.duration} the duration of the follow-up window,
#'  as given by the \code{followup.window.duration} parameter.
#'  \item \code{followup.window.duration.unit} the time unit of the
#'  \code{followup.window.duration}, as given by the
#'  \code{followup.window.duration.unit} parameter.
#'  \item \code{observation.window.start} the beginning of the observation
#'  window, as given by the \code{observation.window.start} parameter.
#'  \item \code{observation.window.start.unit} the time unit of the
#'  \code{observation.window.start}, as given by the
#'  \code{observation.window.start.unit} parameter.
#'  \item \code{observation.window.duration} the duration of the observation
#'  window, as given by the \code{observation.window.duration} parameter.
#'  \item \code{observation.window.duration.unit} the time unit of the
#'  \code{observation.window.duration}, as given by the
#'  \code{observation.window.duration.unit} parameter.
#'  \item \code{date.format} the format of the dates, as given by the
#'  \code{date.format} parameter.
#'  \item \code{summary} the metadata, as given by the \code{summary} parameter.
#'  \item \code{event.info} the \code{data.frame} containing the event info
#'  (irrelevant for most users; see \code{\link{compute.event.int.gaps}} for
#'  details).
#'  \item \code{computed.CMA} the class name of the computed CMA.
#'  \item \code{CMA} the \code{data.frame} containing the actual \code{CMA}
#'  estimates for each participant (the \code{ID.colname} column) and treatment
#'  episode, with columns:
#'    \itemize{
#'      \item \code{ID.colname} the patient ID as given by the \code{ID.colname}
#'      parameter.
#'      \item \code{episode.ID} the unique treatment episode ID (within
#'      patients).
#'      \item \code{episode.start} the treatment episode's start date (as a
#'      \code{Date} object).
#'      \item \code{end.episode.gap.days} the corresponding gap days of the last
#'      event in this episode.
#'      \item \code{episode.duration} the treatment episode's duration in days.
#'      \item \code{episode.end} the treatment episode's end date (as a
#'      \code{Date} object).
#'      \item \code{CMA} the treatment episode's estimated CMA.
#'    }
#' }
#' Please note that if \code{medication.groups} are defined, then the \code{CMA}
#' and \code{event.info} are named lists, each element containing the CMA and
#' event.info corresponding to a single medication group (the element's name).
#' @seealso \code{\link{CMA_sliding_window}} is very similar, computing a
#' "simple" CMA for each of a set of same-size sliding windows.
#' The "simple" CMAs that can be computed comprise \code{\link{CMA1}},
#' \code{\link{CMA2}}, \code{\link{CMA3}}, \code{\link{CMA4}},
#' \code{\link{CMA5}}, \code{\link{CMA6}}, \code{\link{CMA7}},
#' \code{\link{CMA8}}, \code{\link{CMA9}}, as well as user-defined classes
#' derived from \code{\link{CMA0}} that have a \code{CMA} component giving the
#' estimated CMA per patient as a \code{data.frame}.
#' @examples
#' \dontrun{
#' cmaE <- CMA_per_episode(CMA="CMA1",
#'                         data=med.events,
#'                         ID.colname="PATIENT_ID",
#'                         event.date.colname="DATE",
#'                         event.duration.colname="DURATION",
#'                         event.daily.dose.colname="PERDAY",
#'                         medication.class.colname="CATEGORY",
#'                         carry.only.for.same.medication=FALSE,
#'                         consider.dosage.change=FALSE,
#'                         followup.window.start=0,
#'                         observation.window.start=0,
#'                         observation.window.duration=365,
#'                         date.format="%m/%d/%Y"
#'                        );}
#' @export
CMA_per_episode <- function( CMA.to.apply,  # the name of the CMA function (e.g., "CMA1") to be used
                             data, # the data used to compute the CMA on
                             treat.epi=NULL, # the treatment episodes, if available
                             # Important columns in the data
                             ID.colname=NA, # the name of the column containing the unique patient ID (NA = undefined)
                             event.date.colname=NA, # the start date of the event in the date.format format (NA = undefined)
                             event.duration.colname=NA, # the event duration in days (NA = undefined)
                             event.daily.dose.colname=NA, # the prescribed daily dose (NA = undefined)
                             medication.class.colname=NA, # the classes/types/groups of medication (NA = undefined)
                             # Groups of medication classes:
                             medication.groups=NULL, # a named vector of medication group definitions, the name of a column in the data that defines the groups, or NULL
                             flatten.medication.groups=FALSE, medication.groups.colname=".MED_GROUP_ID", # if medication.groups were defined, return CMAs and event.info as single data.frame?
                             # Various types methods of computing gaps:
                             carry.only.for.same.medication=NA, # if TRUE the carry-over applies only across medication of same type (NA = use the CMA's values)
                             consider.dosage.change=NA, # if TRUE carry-over is adjusted to reflect changes in dosage (NA = use the CMA's values)
                             # Treatment episodes:
                             medication.change.means.new.treatment.episode=TRUE, # does a change in medication automatically start a new treatment episode?
                             dosage.change.means.new.treatment.episode=FALSE, # does a change in dosage automatically start a new treatment episode?
                             maximum.permissible.gap=180, # if a number, is the duration in units of max. permissible gaps between treatment episodes
                             maximum.permissible.gap.unit=c("days", "weeks", "months", "years", "percent")[1], # time units; can be "days", "weeks" (fixed at 7 days), "months" (fixed at 30 days), "years" (fixed at 365 days), or "percent", in which case maximum.permissible.gap is interpreted as a percent (can be > 100%) of the duration of the current prescription
                             maximum.permissible.gap.append.to.episode.proportion=0.0, # the proportion of the maximum permissible gap to append at the end of an episode with a gap larger than the maximum permissible gap, between 0.0 (no addition, the default) and 1.0 (the full maximum permissible gap is added)
                             # The follow-up window:
                             followup.window.start=0, # if a number is the earliest event per participant date + number of units, or a Date object, or a column name in data (NA = undefined)
                             followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                             followup.window.start.per.medication.group=FALSE, # if there are medication groups and this is TRUE, then the first event is relative to each medication group separately, otherwise is relative to the patient
                             followup.window.duration=365*2, # the duration of the follow-up window in the time units given below (NA = undefined)
                             followup.window.duration.unit=c("days", "weeks", "months", "years")[1],# the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)  (NA = undefined)
                             # The observation window (embedded in the follow-up window):
                             observation.window.start=0, # the number of time units relative to followup.window.start (NA = undefined)
                             observation.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                             observation.window.duration=365*2, # the duration of the observation window in time units (NA = undefined)
                             observation.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                             # Date format:
                             date.format="%m/%d/%Y", # the format of the dates used in this function (NA = undefined)
                             # Comments and metadata:
                             summary="CMA per treatment episode",
                             # The description of the output (added) columns:
                             event.interval.colname="event.interval", # contains number of days between the start of current event and the start of the next
                             gap.days.colname="gap.days", # contains the number of days when medication was not available
                             # Dealing with failed estimates:
                             force.NA.CMA.for.failed.patients=TRUE, # force the failed patients to have NA CM estimate?
                             # Parallel processing:
                             parallel.backend=c("none","multicore","snow","snow(SOCK)","snow(MPI)","snow(NWS)")[1], # parallel backend to use
                             parallel.threads="auto", # specification (or number) of parallel threads
                             # Misc:
                             suppress.warnings=FALSE,
                             # extra parameters to be sent to the CMA function:
                             ...
)
{
  # Get the CMA function corresponding to the name:
  if( !(is.character(CMA.to.apply) || is.factor(CMA.to.apply)) )
  {
    if( !suppress.warnings ) .report.ewms(paste0("'CMA.to.apply' must be a string contining the name of the simple CMA to apply!\n)"), "error", "CMA_per_episode", "AdhereR");
    return (NULL);
  }
  CMA.FNC <- switch(as.character(CMA.to.apply), # if factor, force it to string
                    "CMA1" = CMA1,
                    "CMA2" = CMA2,
                    "CMA3" = CMA3,
                    "CMA4" = CMA4,
                    "CMA5" = CMA5,
                    "CMA6" = CMA6,
                    "CMA7" = CMA7,
                    "CMA8" = CMA8,
                    "CMA9" = CMA9,
                    {if( !suppress.warnings ) .report.ewms(paste0("Unknown 'CMA.to.apply' '",CMA.to.apply,"': defaulting to CMA0!\n)"), "warning", "CMA_per_episode", "AdhereR"); CMA0;}); # by default, fall back to CMA0

  # Default argument values and overrides:
  def.vals <- formals(CMA.FNC);
  if( CMA.to.apply %in% c("CMA1", "CMA2", "CMA3", "CMA4") )
  {
    carryover.into.obs.window <- carryover.within.obs.window <- FALSE;
    if( !is.na(carry.only.for.same.medication) && carry.only.for.same.medication && !suppress.warnings ) .report.ewms("'carry.only.for.same.medication' cannot be defined for CMAs 1-4!\n", "warning", "CMA_per_episode", "AdhereR");
    carry.only.for.same.medication <- FALSE;
    if( !is.na(consider.dosage.change) && consider.dosage.change && !suppress.warnings ) .report.ewms("'consider.dosage.change' cannot be defined for CMAs 1-4!\n", "warning", "CMA_per_episode", "AdhereR");
    consider.dosage.change <- FALSE;
  } else if( CMA.to.apply %in% c("CMA5", "CMA6") )
  {
    carryover.into.obs.window <- FALSE;
    carryover.within.obs.window <- TRUE;
    if( is.na(carry.only.for.same.medication) ) carry.only.for.same.medication <- def.vals[["carry.only.for.same.medication"]]; # use the default value from CMA
    if( is.na(consider.dosage.change) ) consider.dosage.change <- def.vals[["consider.dosage.change"]]; # use the default value from CMA
  } else if( CMA.to.apply %in% c("CMA7", "CMA8", "CMA9") )
  {
    carryover.into.obs.window <- carryover.within.obs.window <- TRUE;
    if( is.na(carry.only.for.same.medication) ) carry.only.for.same.medication <- def.vals[["carry.only.for.same.medication"]]; # use the default value from CMA
    if( is.na(consider.dosage.change) ) consider.dosage.change <- def.vals[["consider.dosage.change"]]; # use the default value from CMA
  } else
  {
    if( !suppress.warnings ) .report.ewms("I know how to do CMA per episodes only for CMAs 1 to 9!\n", "error", "CMA_per_episode", "AdhereR");
    return (NULL);
  }

  ## Force data to data.table
  #if( !inherits(data,"data.table") ) data <- as.data.table(data);

  # Create the return value skeleton and check consistency:
  ret.val <- CMA0(data,
                  ID.colname=ID.colname,
                  event.date.colname=event.date.colname,
                  event.duration.colname=event.duration.colname,
                  event.daily.dose.colname=event.daily.dose.colname,
                  medication.class.colname=medication.class.colname,
                  medication.groups=medication.groups,
                  flatten.medication.groups=flatten.medication.groups,
                  medication.groups.colname=medication.groups.colname,
                  carryover.within.obs.window=carryover.within.obs.window,
                  carryover.into.obs.window=carryover.into.obs.window,
                  carry.only.for.same.medication=carry.only.for.same.medication,
                  consider.dosage.change=consider.dosage.change,
                  followup.window.start=followup.window.start,
                  followup.window.start.unit=followup.window.start.unit,
                  followup.window.start.per.medication.group=followup.window.start.per.medication.group,
                  followup.window.duration=followup.window.duration,
                  followup.window.duration.unit=followup.window.duration.unit,
                  observation.window.start=observation.window.start,
                  observation.window.start.unit=observation.window.start.unit,
                  observation.window.duration=observation.window.duration,
                  observation.window.duration.unit=observation.window.duration.unit,
                  date.format=date.format,
                  suppress.warnings=suppress.warnings,
                  summary=NA);
  if( is.null(ret.val) ) return (NULL);
  # The followup.window.start and observation.window.start might have been converted to Date:
  followup.window.start <- ret.val$followup.window.start; observation.window.start <- ret.val$observation.window.start;

  ## retain only necessary columns of data
  #data <- data[,c(ID.colname,
  #                event.date.colname,
  #                event.duration.colname,
  #                event.daily.dose.colname,
  #                medication.class.colname),
  #             with=FALSE];

  # The workhorse auxiliary function: For a given (subset) of data, compute the event intervals and gaps:
  .workhorse.function <- function(data=NULL,
                                  ID.colname=NULL,
                                  event.date.colname=NULL,
                                  event.duration.colname=NULL,
                                  event.daily.dose.colname=NULL,
                                  medication.class.colname=NULL,
                                  event.interval.colname=NULL,
                                  gap.days.colname=NULL,
                                  carryover.within.obs.window=NULL,
                                  carryover.into.obs.window=NULL,
                                  carry.only.for.same.medication=NULL,
                                  consider.dosage.change=NULL,
                                  followup.window.start=NULL,
                                  followup.window.start.unit=NULL,
                                  followup.window.duration=NULL,
                                  followup.window.duration.unit=NULL,
                                  observation.window.start=NULL,
                                  observation.window.start.unit=NULL,
                                  observation.window.duration=NULL,
                                  observation.window.duration.unit=NULL,
                                  date.format=NULL,
                                  suppress.warnings=NULL
  )
  {
    if(is.null(treat.epi)) {

      # Compute the treatment espisodes:
      treat.epi <- compute.treatment.episodes( data=data,
                                               ID.colname=ID.colname,
                                               event.date.colname=event.date.colname,
                                               event.duration.colname=event.duration.colname,
                                               event.daily.dose.colname=event.daily.dose.colname,
                                               medication.class.colname=medication.class.colname,
                                               carryover.within.obs.window=carryover.within.obs.window,
                                               carry.only.for.same.medication=carry.only.for.same.medication,
                                               consider.dosage.change=consider.dosage.change,
                                               medication.change.means.new.treatment.episode=medication.change.means.new.treatment.episode,
                                               dosage.change.means.new.treatment.episode=dosage.change.means.new.treatment.episode,
                                               maximum.permissible.gap=maximum.permissible.gap,
                                               maximum.permissible.gap.unit=maximum.permissible.gap.unit,
                                               maximum.permissible.gap.append.to.episode.proportion=maximum.permissible.gap.append.to.episode.proportion,
                                               followup.window.start=followup.window.start,
                                               followup.window.start.unit=followup.window.start.unit,
                                               followup.window.duration=followup.window.duration,
                                               followup.window.duration.unit=followup.window.duration.unit,
                                               date.format=date.format,
                                               parallel.backend="none", # make sure this runs sequentially!
                                               parallel.threads=1,
                                               suppress.warnings=suppress.warnings,
                                               return.data.table=TRUE);

    } else {

      # various checks

      # Convert treat.epi to data.table, cache event dat as Date objects, and key by patient ID and event date
      treat.epi <- as.data.table(treat.epi);
      treat.epi[, `:=` (episode.start = as.Date(episode.start,format=date.format),
                        episode.end = as.Date(episode.end,format=date.format)
                        )]; # .DATE.as.Date: convert event.date.colname from formatted string to Date
      setkeyv(treat.epi, c(ID.colname, "episode.ID")); # key (and sorting) by patient and episode ID

    }

    if( is.null(treat.epi) || nrow(treat.epi) == 0 ) return (NULL);

    # Compute the real observation windows (might differ per patient) only once per patient (speed things up & the observation window is the same for all events within a patient):
    tmp <- as.data.frame(data); tmp <- tmp[!duplicated(tmp[,ID.colname]),]; # the reduced dataset for computing the actual OW:
    event.info2 <- compute.event.int.gaps(data=tmp,
                                          ID.colname=ID.colname,
                                          event.date.colname=event.date.colname,
                                          event.duration.colname=event.duration.colname,
                                          event.daily.dose.colname=event.daily.dose.colname,
                                          medication.class.colname=medication.class.colname,
                                          event.interval.colname=event.interval.colname,
                                          gap.days.colname=gap.days.colname,
                                          carryover.within.obs.window=FALSE,
                                          carryover.into.obs.window=FALSE,
                                          carry.only.for.same.medication=FALSE,
                                          consider.dosage.change=FALSE,
                                          followup.window.start=followup.window.start,
                                          followup.window.start.unit=followup.window.start.unit,
                                          followup.window.duration=followup.window.duration,
                                          followup.window.duration.unit=followup.window.duration.unit,
                                          observation.window.start=observation.window.start,
                                          observation.window.start.unit=observation.window.start.unit,
                                          observation.window.duration=observation.window.duration,
                                          observation.window.duration.unit=observation.window.duration.unit,
                                          date.format=date.format,
                                          keep.window.start.end.dates=TRUE,
                                          remove.events.outside.followup.window=FALSE,
                                          parallel.backend="none", # make sure this runs sequentially!
                                          parallel.threads=1,
                                          suppress.warnings=suppress.warnings,
                                          return.data.table=TRUE);
    if( is.null(event.info2) ) return (NULL);

    # Merge the observation window start and end dates back into the treatment episodes:
    treat.epi <- merge(treat.epi, event.info2[,c(ID.colname, ".OBS.START.DATE", ".OBS.END.DATE"),with=FALSE],
                       all.x=TRUE,
                       by = c(ID.colname));
    setnames(treat.epi, ncol(treat.epi)-c(1,0), c(".OBS.START.DATE.PRECOMPUTED", ".OBS.END.DATE.PRECOMPUTED"));
    # Get the intersection between the episode and the observation window:
    treat.epi[, c(".INTERSECT.EPISODE.OBS.WIN.START",
                  ".INTERSECT.EPISODE.OBS.WIN.END")
                := list(max(episode.start, .OBS.START.DATE.PRECOMPUTED),
                        min(episode.end,   .OBS.END.DATE.PRECOMPUTED)),
              by=c(ID.colname,"episode.ID")];
    treat.epi <- treat.epi[ .INTERSECT.EPISODE.OBS.WIN.START < .INTERSECT.EPISODE.OBS.WIN.END, ]; # keep only the episodes which fall within the OW
    treat.epi[, c("episode.duration",
                  ".INTERSECT.EPISODE.OBS.WIN.DURATION",
                  ".PATIENT.EPISODE.ID")
                := list(as.numeric(episode.end - episode.start),
                        as.numeric(.INTERSECT.EPISODE.OBS.WIN.END - .INTERSECT.EPISODE.OBS.WIN.START),
                        paste(get(ID.colname),episode.ID,sep="*"))];

    # Merge the data and the treatment episodes info:
    data.epi <- merge(treat.epi, data, allow.cartesian=TRUE);
    setkeyv(data.epi, c(".PATIENT.EPISODE.ID", ".DATE.as.Date"));

    # compute end.episode.gap.days, if treat.epi are supplied
    if(!"end.episode.gap.days" %in% colnames(treat.epi)) {
      data.epi2 <- compute.event.int.gaps(data=as.data.frame(data.epi),
                                          ID.colname=".PATIENT.EPISODE.ID",
                                          event.date.colname=event.date.colname,
                                          event.duration.colname=event.duration.colname,
                                          event.daily.dose.colname=event.daily.dose.colname,
                                          medication.class.colname=medication.class.colname,
                                          carryover.within.obs.window=carryover.within.obs.window,
                                          carryover.into.obs.window=carryover.into.obs.window,
                                          carry.only.for.same.medication=carry.only.for.same.medication,
                                          consider.dosage.change=consider.dosage.change,
                                          followup.window.start="episode.start",
                                          followup.window.start.unit=followup.window.start.unit,
                                          followup.window.duration="episode.duration",
                                          followup.window.duration.unit=followup.window.duration.unit,
                                          observation.window.start=".INTERSECT.EPISODE.OBS.WIN.START",
                                          observation.window.duration=".INTERSECT.EPISODE.OBS.WIN.DURATION",
                                          observation.window.duration.unit="days",
                                          date.format=date.format,
                                          keep.window.start.end.dates=TRUE,
                                          remove.events.outside.followup.window=FALSE,
                                          parallel.backend="none", # make sure this runs sequentially!
                                          parallel.threads=1,
                                          suppress.warnings=suppress.warnings,
                                          return.data.table=TRUE);

      episode.gap.days <- data.epi2[which(.EVENT.WITHIN.FU.WINDOW), c(ID.colname, "episode.ID", gap.days.colname), by = c(ID.colname, "episode.ID"), with = FALSE]; # gap days during the follow-up window
      end.episode.gap.days <- episode.gap.days[,last(get(gap.days.colname)), by = c(ID.colname, "episode.ID")]; # gap days during the last event

      setnames(end.episode.gap.days, old = "V1", new = "end.episode.gap.days")

      treat.epi <- merge(treat.epi, end.episode.gap.days, all.x = TRUE, by = c(ID.colname, "episode.ID")); # merge end.episode.gap.days back to data.epi

      treat.epi[, episode.duration := as.numeric(.INTERSECT.EPISODE.OBS.WIN.END-.INTERSECT.EPISODE.OBS.WIN.START)];
    }

    # Compute the required CMA on this new combined database:
    cma <- CMA.FNC(data=as.data.frame(data.epi),
                   ID.colname=".PATIENT.EPISODE.ID",
                   event.date.colname=event.date.colname,
                   event.duration.colname=event.duration.colname,
                   event.daily.dose.colname=event.daily.dose.colname,
                   medication.class.colname=medication.class.colname,
                   carryover.within.obs.window=carryover.within.obs.window,
                   carryover.into.obs.window=carryover.into.obs.window,
                   carry.only.for.same.medication=carry.only.for.same.medication,
                   consider.dosage.change=consider.dosage.change,
                   followup.window.start="episode.start",
                   followup.window.start.unit=followup.window.start.unit,
                   followup.window.duration="episode.duration",
                   followup.window.duration.unit=followup.window.duration.unit,
                   observation.window.start=".INTERSECT.EPISODE.OBS.WIN.START",
                   observation.window.duration=".INTERSECT.EPISODE.OBS.WIN.DURATION",
                   observation.window.duration.unit="days",
                   date.format=date.format,
                   parallel.backend="none", # make sure this runs sequentially!
                   parallel.threads=1,
                   suppress.warnings=suppress.warnings,
                   ...);

    # adjust episode start- and end dates
    treat.epi[, `:=` (episode.start = .INTERSECT.EPISODE.OBS.WIN.START,
                      episode.end = .INTERSECT.EPISODE.OBS.WIN.END)]

    # Add back the patient and episode IDs:
    tmp <- as.data.table(merge(cma$CMA, treat.epi)[,c(ID.colname, "episode.ID", "episode.start", "end.episode.gap.days", "episode.duration", "episode.end", "CMA")]);
    setkeyv(tmp, c(ID.colname,"episode.ID"));
    return (list("CMA"=as.data.frame(tmp), "event.info"=as.data.frame(event.info2)[,c(ID.colname, ".FU.START.DATE", ".FU.END.DATE", ".OBS.START.DATE", ".OBS.END.DATE")]));
  }

  # Convert to data.table, cache event dat as Date objects, and key by patient ID and event date
  data.copy <- data.table(data);
  data.copy[, .DATE.as.Date := as.Date(get(event.date.colname),format=date.format)]; # .DATE.as.Date: convert event.date.colname from formatted string to Date
  data.copy$..ORIGINAL.ROW.ORDER.. <- 1:nrow(data.copy); # preserve the original order of the rows (needed for medication groups)
  setkeyv(data.copy, c(ID.colname, ".DATE.as.Date")); # key (and sorting) by patient ID and event date


  # Are there medication groups?
  if( is.null(mg <- getMGs(ret.val)) )
  {
    # Nope: do a single estimation on the whole dataset:

    # Compute the workhorse function:
    tmp <- .compute.function(.workhorse.function, fnc.ret.vals=2,
                             parallel.backend=parallel.backend,
                             parallel.threads=parallel.threads,
                             data=data.copy,
                             ID.colname=ID.colname,
                             event.date.colname=event.date.colname,
                             event.duration.colname=event.duration.colname,
                             event.daily.dose.colname=event.daily.dose.colname,
                             medication.class.colname=medication.class.colname,
                             event.interval.colname=event.interval.colname,
                             gap.days.colname=gap.days.colname,
                             carryover.within.obs.window=carryover.within.obs.window,
                             carryover.into.obs.window=carryover.into.obs.window,
                             carry.only.for.same.medication=carry.only.for.same.medication,
                             consider.dosage.change=consider.dosage.change,
                             followup.window.start=followup.window.start,
                             followup.window.start.unit=followup.window.start.unit,
                             followup.window.duration=followup.window.duration,
                             followup.window.duration.unit=followup.window.duration.unit,
                             observation.window.start=observation.window.start,
                             observation.window.start.unit=observation.window.start.unit,
                             observation.window.duration=observation.window.duration,
                             observation.window.duration.unit=observation.window.duration.unit,
                             date.format=date.format,
                             suppress.warnings=suppress.warnings);
    if( is.null(tmp) || is.null(tmp$CMA) || !inherits(tmp$CMA,"data.frame") || is.null(tmp$event.info) ) return (NULL);

    # Construct the return object:
    class(ret.val) <- "CMA_per_episode";
    ret.val$event.info <- as.data.frame(tmp$event.info);
    ret.val$computed.CMA <- CMA.to.apply;
    ret.val$summary <- summary;
    ret.val$CMA <- as.data.frame(tmp$CMA);
    setnames(ret.val$CMA, 1, ID.colname);

    return (ret.val);

  } else
  {
    # Yes

    # Make sure the group's observations reflect the potentially new order of the observations in the data:
    mb.obs <- mg$obs[data.copy$..ORIGINAL.ROW.ORDER.., ];

    # Focus only on the non-trivial ones:
    mg.to.eval <- (colSums(!is.na(mb.obs) & mb.obs) > 0);
    if( sum(mg.to.eval) == 0 )
    {
      # None selects not even one observation!
      .report.ewms(paste0("None of the medication classes (included __ALL_OTHERS__) selects any observation!\n"), "warning", "CMA1", "AdhereR");
      return (NULL);
    }
    mb.obs <- mb.obs[,mg.to.eval]; # keep only the non-trivial ones

    # Check if there are medication classes that refer to the same observations (they would result in the same estimates):
    mb.obs.dupl <- duplicated(mb.obs, MARGIN=2);

    # Estimate each separately:
    tmp <- lapply(1:nrow(mg$defs), function(i)
    {
      # Check if these are to be evaluated:
      if( !mg.to.eval[i] )
      {
        return (list("CMA"=NULL, "event.info"=NULL));
      }

      # Translate into the index of the classes to be evaluated:
      ii <- sum(mg.to.eval[1:i]);

      # Cache the selected observations:
      mg.sel.obs <- mb.obs[,ii];

      # Check if this is a duplicated medication class:
      if( mb.obs.dupl[ii] )
      {
        # Find which one is the original:
        for( j in 1:(ii-1) ) # ii=1 never should be TRUE
        {
          if( identical(mb.obs[,j], mg.sel.obs) )
          {
            # This is the original: return it and stop
            return (c("identical.to"=j));
          }
        }
      }

      # Compute the workhorse function:
      tmp <- .compute.function(.workhorse.function, fnc.ret.vals=2,
                               parallel.backend=parallel.backend,
                               parallel.threads=parallel.threads,
                               data=data.copy[mg.sel.obs,], # apply it on the subset of observations covered by this medication class
                               ID.colname=ID.colname,
                               event.date.colname=event.date.colname,
                               event.duration.colname=event.duration.colname,
                               event.daily.dose.colname=event.daily.dose.colname,
                               medication.class.colname=medication.class.colname,
                               event.interval.colname=event.interval.colname,
                               gap.days.colname=gap.days.colname,
                               carryover.within.obs.window=carryover.within.obs.window,
                               carryover.into.obs.window=carryover.into.obs.window,
                               carry.only.for.same.medication=carry.only.for.same.medication,
                               consider.dosage.change=consider.dosage.change,
                               followup.window.start=followup.window.start,
                               followup.window.start.unit=followup.window.start.unit,
                               followup.window.duration=followup.window.duration,
                               followup.window.duration.unit=followup.window.duration.unit,
                               observation.window.start=observation.window.start,
                               observation.window.start.unit=observation.window.start.unit,
                               observation.window.duration=observation.window.duration,
                               observation.window.duration.unit=observation.window.duration.unit,
                               date.format=date.format,
                               suppress.warnings=suppress.warnings);
      if( is.null(tmp) || is.null(tmp$CMA) || !inherits(tmp$CMA,"data.frame") || is.null(tmp$event.info) ) return (NULL);

      # Convert to data.frame and return:
      tmp$CMA <- as.data.frame(tmp$CMA); setnames(tmp$CMA, 1, ID.colname);
      tmp$event.info <- as.data.frame(tmp$event.info);
      return (tmp);

    });

    # Set the names:
    names(tmp) <- mg$defs$name;

    # Solve the duplicates:
    for( i in seq_along(tmp) )
    {
      if( is.numeric(tmp[[i]]) && length(tmp[[i]]) == 1 && names(tmp[[i]]) == "identical.to" ) tmp[[i]] <- tmp[[ tmp[[i]] ]];
    }

    # Rearrange these and return:
    ret.val[["CMA"]]        <- lapply(tmp, function(x) x$CMA);
    ret.val[["event.info"]] <- lapply(tmp, function(x) x$event.info);
    ret.val$computed.CMA <- CMA.to.apply;
    if( flatten.medication.groups && !is.na(medication.groups.colname) )
    {
      # Flatten the CMA:
      tmp <- do.call(rbind, ret.val[["CMA"]]);
      if( is.null(tmp) || nrow(tmp) == 0 )
      {
        ret.val[["CMA"]] <- NULL;
      } else
      {
        tmp <- cbind(tmp, unlist(lapply(1:length(ret.val[["CMA"]]), function(i) if(!is.null(ret.val[["CMA"]][[i]])){rep(names(ret.val[["CMA"]])[i], nrow(ret.val[["CMA"]][[i]]))}else{NULL})));
        names(tmp)[ncol(tmp)] <- medication.groups.colname; rownames(tmp) <- NULL;
        ret.val[["CMA"]] <- tmp;
      }

      # ... and the event.info:
      tmp <- do.call(rbind, ret.val[["event.info"]]);
      if( is.null(tmp) || nrow(tmp) == 0 )
      {
        ret.val[["event.info"]] <- NULL;
      } else
      {
        tmp <- cbind(tmp, unlist(lapply(1:length(ret.val[["event.info"]]), function(i) if(!is.null(ret.val[["event.info"]][[i]])){rep(names(ret.val[["event.info"]])[i], nrow(ret.val[["event.info"]][[i]]))}else{NULL})));
        names(tmp)[ncol(tmp)] <- medication.groups.colname; rownames(tmp) <- NULL;
        ret.val[["event.info"]] <- tmp;
      }
    }
    class(ret.val) <- "CMA_per_episode";
    ret.val$summary <- summary;
    return (ret.val);

  }
}

#' @export
getMGs.CMA_per_episode <- function(x)
{
  cma <- x; # parameter x is required for S3 consistency, but I like cma more
  if( is.null(cma) || !inherits(cma, "CMA_per_episode") || is.null(cma$medication.groups) ) return (NULL);
  return (cma$medication.groups);
}

#' @export
getCMA.CMA_per_episode <- function(x, flatten.medication.groups=FALSE, medication.groups.colname=".MED_GROUP_ID")
{
  cma <- x; # parameter x is required for S3 consistency, but I like cma more
  if( is.null(cma) || !inherits(cma, "CMA_per_episode") || !("CMA" %in% names(cma)) || is.null(cma$CMA) ) return (NULL);
  if( inherits(cma$CMA, "data.frame") || !flatten.medication.groups )
  {
    return (cma$CMA);
  } else
  {
    # Flatten the medication groups into a single data.frame:
    ret.val <- do.call(rbind, cma$CMA);
    if( is.null(ret.val) || nrow(ret.val) == 0 ) return (NULL);
    ret.val <- cbind(ret.val, unlist(lapply(1:length(cma$CMA), function(i) if(!is.null(cma$CMA[[i]])){rep(names(cma$CMA)[i], nrow(cma$CMA[[i]]))}else{NULL})));
    names(ret.val)[ncol(ret.val)] <- medication.groups.colname; rownames(ret.val) <- NULL;
    return (ret.val);
  }
}

#' @export
getEventInfo.CMA_per_episode <- function(x, flatten.medication.groups=FALSE, medication.groups.colname=".MED_GROUP_ID")
{
  cma <- x; # parameter x is required for S3 consistency, but I like cma more
  if( is.null(cma) || !inherits(cma, "CMA_per_episode") || !("event.info" %in% names(cma)) || is.null(cma$event.info) ) return (NULL);
  if( inherits(cma$event.info, "data.frame") || !flatten.medication.groups )
  {
    return (cma$event.info);
  } else
  {
    # Flatten the medication groups into a single data.frame:
    ret.val <- do.call(rbind, cma$event.info);
    if( is.null(ret.val) || nrow(ret.val) == 0 ) return (NULL);
    ret.val <- cbind(ret.val, unlist(lapply(1:length(cma$event.info), function(i) if(!is.null(cma$event.info[[i]])){rep(names(cma$event.info)[i], nrow(cma$event.info[[i]]))}else{NULL})));
    names(ret.val)[ncol(ret.val)] <- medication.groups.colname; rownames(ret.val) <- NULL;
    return (ret.val);
  }
}

#' @export
subsetCMA.CMA_per_episode <- function(cma, patients, suppress.warnings=FALSE)
{
  if( inherits(patients, "factor") ) patients <- as.character(patients);
  all.patients <- unique(cma$data[,cma$ID.colname]);
  patients.to.keep <- intersect(patients, all.patients);
  if( length(patients.to.keep) == length(all.patients) )
  {
    # Keep all patients:
    return (cma);
  }
  if( length(patients.to.keep) == 0 )
  {
    if( !suppress.warnings ) .report.ewms("No patients to subset on!\n", "error", "subsetCMA.CMA_per_episode", "AdhereR");
    return (NULL);
  }
  if( length(patients.to.keep) < length(patients) && !suppress.warnings ) .report.ewms("Some patients in the subsetting set are not in the CMA itself and are ignored!\n", "warning", "subsetCMA.CMA_per_episode", "AdhereR");

  ret.val <- cma;
  ret.val$data <- ret.val$data[ ret.val$data[,ret.val$ID.colname] %in% patients.to.keep, ];
  if( !is.null(ret.val$event.info) )
  {
    if( inherits(ret.val$event.info, "data.frame") )
    {
      ret.val$event.info <- ret.val$event.info[ ret.val$event.info[,ret.val$ID.colname] %in% patients.to.keep, ]; if( nrow(ret.val$event.info) == 0 ) ret.val$event.info <- NULL;
    } else if( is.list(ret.val$event.info) && length(ret.val$event.info) > 0 )
    {
      ret.val$event.info <- lapply(ret.val$event.info, function(x){tmp <- x[ x[,ret.val$ID.colname] %in% patients.to.keep, ]; if(!is.null(tmp) && nrow(tmp) > 0){tmp}else{NULL}});
    }
  }
  if( ("CMA" %in% names(ret.val)) && !is.null(ret.val$CMA) )
  {
    if( inherits(ret.val$CMA, "data.frame") )
    {
      ret.val$CMA <- ret.val$CMA[ ret.val$CMA[,ret.val$ID.colname] %in% patients.to.keep, ];
    } else if( is.list(ret.val$CMA) && length(ret.val$CMA) > 0 )
    {
      ret.val$CMA <- lapply(ret.val$CMA, function(x){tmp <- x[ x[,ret.val$ID.colname] %in% patients.to.keep, ]; if(!is.null(tmp) && nrow(tmp) > 0){tmp}else{NULL}});
    }
  }
  return (ret.val);
}

#' @rdname print.CMA0
#' @export
print.CMA_per_episode <- function(x,                                     # the CMA_per_episode (or derived) object
                                  ...,                                   # required for S3 consistency
                                  inline=FALSE,                          # print inside a line of text or as a separate, extended object?
                                  format=c("text", "latex", "markdown"), # the format to print to
                                  print.params=TRUE,                     # show the parameters?
                                  print.data=TRUE,                       # show the summary of the data?
                                  exclude.params=c("event.info"),        # if so, should I not print some?
                                  skip.header=FALSE,                     # should I print the generic header?
                                  cma.type=class(x)[1]
)
{
  cma <- x; # parameter x is required for S3 consistency, but I like cma more
  if( is.null(cma) ) return (invisible(NULL));

  if( format[1] == "text" )
  {
    # Output text:
    if( !inline )
    {
      # Extended print:
      if( !skip.header ) cat(paste0(cma.type,":\n"));
      if( print.params )
      {
        params <- names(cma); params <- params[!(params %in% c("data",exclude.params))]; # exlude the 'data' (and any other requested) params from printing
        if( length(params) > 0 )
        {
          if( "summary" %in% params )
          {
            cat(paste0("  \"",cma$summary,"\"\n"));
            params <- params[!(params %in% "summary")];
          }
          cat("  [\n");
          for( p in params )
          {
            if( p == "CMA" )
            {
              cat(paste0("    ",p," = CMA results for ",nrow(cma[[p]])," patients\n"));
            } else if( p == "medication.groups" )
            {
              if( !is.null(cma[[p]]) )
              {
                cat(paste0("    ", p, " = ", nrow(cma[[p]]$defs), " [", ifelse(nrow(cma[[p]]$defs)<4, paste0("'",cma[[p]]$defs$name,"'", collapse=", "), paste0(paste0("'",cma[[p]]$defs$name[1:4],"'", collapse=", ")," ...")), "]\n"));
              } else
              {
                cat(paste0("    ", p, " = <NONE>\n"));
              }
            } else if( !is.null(cma[[p]]) && length(cma[[p]]) > 0 && !is.na(cma[[p]]) )
            {
              cat(paste0("    ",p," = ",cma[[p]],"\n"));
            }
          }
          cat("  ]\n");
        }
        if( print.data && !is.null(cma$data) )
        {
          # Data summary:
          cat(paste0("  DATA: ",nrow(cma$data)," (rows) x ",ncol(cma$data)," (columns)"," [",length(unique(cma$data[,cma$ID.colname]))," patients]",".\n"));
        }
      }
    } else
    {
      # Inline print:
      cat(paste0(cma$summary,ifelse(print.data && !is.null(cma$data),paste0(" (on ",nrow(cma$data)," rows x ",ncol(cma$data)," columns",", ",length(unique(cma$data[,cma$ID.colname]))," patients",")"),"")));
    }
  } else if( format[1] == "latex" )
  {
    # Output LaTeX: no difference between inline and not inline:
    cat(paste0("\\textbf{",cma$summary,"} (",cma.type,"):",
               ifelse(print.data && !is.null(cma$data),paste0(" (on ",nrow(cma$data)," rows x ",ncol(cma$data)," columns",", ",length(unique(cma$data[,cma$ID.colname]))," patients",")"),"")));
  } else if( format[1] == "markdown" )
  {
    # Output Markdown: no difference between inline and not inline:
    cat(paste0("**",cma$summary,"** (",cma.type,"):",
               ifelse(print.data && !is.null(cma$data),paste0(" (on ",nrow(cma$data)," rows x ",ncol(cma$data)," columns",", ",length(unique(cma$data[,cma$ID.colname]))," patients",")"),"")));
  } else
  {
    .report.ewms("Unknown format for printing!\n", "error", "print.CMA_per_episode", "AdhereR");
    return (invisible(NULL));
  }
}


#' Plot CMA_per_episode and CMA_sliding_window objects.
#'
#' Plots the event data and the estimated CMA per treatment episode and sliding
#' window, respectively.
#'
#' The x-axis represents time (either in days since the earliest date or as
#' actual dates), with consecutive events represented as ascending on the y-axis.
#'
#' Each event is represented as a segment with style \code{lty.event} and line
#' width \code{lwd.event} starting with a \code{pch.start.event} and ending with
#' a \code{pch.end.event} character, coloured with a unique color as given by
#' \code{col.cats}, extending from its start date until its end date.
#' Consecutive events are thus represented on consecutive levels of the y-axis
#' and are connected by a "continuation" line with \code{col.continuation}
#' colour, \code{lty.continuation} style and \code{lwd.continuation} width;
#' these continuation lines are purely visual guides helping to perceive the
#' sequence of events, and carry no information about the avilability of
#' medicine in this interval.
#'
#' Above these, the treatment episodes or the sliding windows are represented in
#' a stacked manner from the earlieast (left, bottom of the stack) to the latest
#' (right, top of the stack), each showing the CMA as percent fill (capped at
#' 100\% even if CMA values may be higher) and also as text.
#'
#' The follow-up and the observation windows are plotted as empty an rectangle
#' and as shaded rectangle, respectively (for some CMAs the observation window
#' might be adjusted in which case the adjustment may also be plotted using a
#' different shading).
#'
#' The kernel density ("smoothed histogram") of the CMA estimates across
#' treatment episodes/sliding windows (if more than 2) can be visually
#' represented as well in the left side of the figure (NB, their horizontal
#' scales may be different across patients).
#'
#' When several patients are displayed on the same plot, they are organized
#' vertically, and alternating bands (white and gray) help distinguish
#' consecutive patients.
#' Implicitely, all patients contained in the \code{cma} object will be plotted,
#' but the \code{patients.to.plot} parameter allows the selection of a subset
#' of patients.
#'
#' Finally, the y-axis shows the patient ID and possibly the CMA estimate as
#' well.
#'
#' Any not explicitely defined arguments are passed to the simple CMA estimation
#' and plotting function; therefore, for more info about possible estimation
#' parameters plese see the help for the appropriate simple CMA, and for possible
#' aesthetic tweaks, please see the help for their plotting.
#'
#' @param x A \emph{\code{CMA0}} or derived object, representing the CMA to
#' plot
#' @param patients.to.plot A vector of \emph{strings} containing the list of
#' patient IDs to plot (a subset of those in the \code{cma} object), or
#' \code{NULL} for all
#' @param duration A \emph{number}, the total duration (in days) of the whole
#' period to plot; in \code{NA} it is automatically determined from the event
#' data such that the whole dataset fits.
#' @param align.all.patients \emph{Logical}, should all patients be aligned
#' (i.e., the actual dates are discarded and all plots are relative to the
#' earliest date)?
#' @param align.first.event.at.zero \emph{Logical}, should the first event be
#' placed at the origin of the time axis (at 0)?
#' @param show.period A \emph{string}, if "dates" show the actual dates at the
#' regular grid intervals, while for "days" (the default) shows the days since
#' the beginning; if \code{align.all.patients == TRUE}, \code{show.period} is
#' taken as "days".
#' @param period.in.days The \emph{number} of days at which the regular grid is
#' drawn (or 0 for no grid).
#' @param show.legend \emph{Logical}, should the legend be drawn?
#' @param legend.x The position of the legend on the x axis; can be "left",
#' "right" (default), or a \emph{numeric} value.
#' @param legend.y The position of the legend on the y axis; can be "bottom"
#' (default), "top", or a \emph{numeric} value.
#' @param legend.bkg.opacity A \emph{number} between 0.0 and 1.0 specifying the
#' opacity of the legend background.
#' @param legend.cex,legend.cex.title The legend and legend title font sizes.
#' @param cex,cex.axis,cex.lab \emph{numeric} values specifying the cex of the
#' various types of text.
#' @param show.cma \emph{Logical}, should the CMA type be shown in the title?
#' @param xlab Named vector of x-axis labels to show for the two types of periods
#' ("days" and "dates"), or a single value for both, or \code{NULL} for nothing.
#' @param ylab Named vector of y-axis labels to show without and with CMA estimates,
#' or a single value for both, or \code{NULL} for nonthing.
#' @param title Named vector of titles to show for and without alignment, or a
#' single value for both, or \code{NULL} for nonthing.
#' @param col.cats A \emph{color} or a \emph{function} that specifies the single
#' colour or the colour palette used to plot the different medication; by
#' default \code{rainbow}, but we recommend, whenever possible, a
#' colorblind-friendly palette such as \code{viridis} or \code{colorblind_pal}.
#' @param unspecified.category.label A \emph{string} giving the name of the
#' unspecified (generic) medication category.
#' @param medication.groups.to.plot the names of the medication groups to plot or
#' \code{NULL} (the default) for all.
#' @param medication.groups.separator.show a \emph{boolean}, if \code{TRUE} (the
#' default) visually mark the medication groups the belong to the same patient,
#' using horizontal lines and alternating vertical lines.
#' @param medication.groups.separator.lty,medication.groups.separator.lwd,medication.groups.separator.color
#' graphical parameters (line type, line width and colour describing the visual
#' marking og medication groups as beloning to the same patient.
#' @param medication.groups.allother.label a \emph{string} giving the label to
#' use for the implicit \code{__ALL_OTHERS__} medication group (defaults to "*").
#' @param lty.event,lwd.event,pch.start.event,pch.end.event The style of the
#' event (line style, width, and start and end symbols).
#' @param plot.events.vertically.displaced Should consecutive events be plotted
#' on separate rows (i.e., separated vertically, the default) or on the same row?
#' @param print.dose,cex.dose,print.dose.outline.col,print.dose.centered Print daily
#' dose as a number and, if so, how (color, size, position...).
#' @param plot.dose,lwd.event.max.dose,plot.dose.lwd.across.medication.classes
#' Show dose through the width of the event lines and, if so, what the maximum
#' width should be, and should this maximum be by medication class or overall.
#' @param col.na The colour used for missing event data.
#' @param col.continuation,lty.continuation,lwd.continuation The color, style
#' and width of the contuniation lines connecting consecutive events.
#' @param alternating.bands.cols The colors of the alternating vertical bands
#' distinguishing the patients; can be \code{NULL} = don't draw the bandes;
#' or a vector of colors.
#' @param bw.plot \emph{Logical}, should the plot use grayscale only (i.e., the
#' \code{\link[grDevices]{gray.colors}} function)?
#' @param force.draw.text \emph{Logical}, if \code{TRUE}, always draw text even
#' if too big or too small
#' @param print.CMA \emph{Logical}, should the CMA values be printed?
#' @param CMA.cex ... and, if printed, what cex (\emph{numeric}) to use?
#' @param plot.CMA \emph{Logical}, should the distribution of the CMA values
#' across episodes/sliding windows be plotted? If \code{TRUE} (the default), the
#' distribution is shown on the left-hand side of the plot, otherwise it is not.
#' @param plot.CMA.as.histogram \emph{Logical}, should the CMA plot be a
#' histogram or a (truncated) density plot? Please note that it is TRUE by
#' deafult for CMA_per_episode and FALSE for CMA_sliding_window, because
#' usually there are more sliding windows than episodes. Also, the density
#' estimate cannot be estimated for less than three different values.
#' @param plot.partial.CMAs.as Should the partial CMAs be plotted? Possible values
#' are "stacked", "overlapping" or "timeseries", or \code{NULL} for no partial
#' CMA plots. Please note that \code{plot.CMA} and \code{plot.partial.CMAs.as}
#' are independent of each other.
#' @param plot.partial.CMAs.as.stacked.col.bars,plot.partial.CMAs.as.stacked.col.border,plot.partial.CMAs.as.stacked.col.text
#' If plotting the partial CMAs as stacked bars, define their graphical attributes.
#' @param plot.partial.CMAs.as.timeseries.vspace,plot.partial.CMAs.as.timeseries.start.from.zero,plot.partial.CMAs.as.timeseries.col.dot,plot.partial.CMAs.as.timeseries.col.interval,plot.partial.CMAs.as.timeseries.col.text,plot.partial.CMAs.as.timeseries.interval.type,plot.partial.CMAs.as.timeseries.lwd.interval,plot.partial.CMAs.as.timeseries.alpha.interval,plot.partial.CMAs.as.timeseries.show.0perc,plot.partial.CMAs.as.timeseries.show.100perc
#' If plotting the partial CMAs as imeseries, these are their graphical attributes.
#' @param plot.partial.CMAs.as.overlapping.alternate,plot.partial.CMAs.as.overlapping.col.interval,plot.partial.CMAs.as.overlapping.col.text
#' If plotting the partial CMAs as overlapping segments, these are their
#' graphical attributes.
#' @param CMA.plot.ratio A \emph{number}, the proportion of the total horizontal
#' plot space to be allocated to the CMA plot.
#' @param CMA.plot.col,CMA.plot.border,CMA.plot.bkg,CMA.plot.text \emph{Strings}
#' giving the colours of the various components of the CMA plot.
#' @param highlight.followup.window \emph{Logical}, should the follow-up window
#' be plotted?
#' @param followup.window.col The follow-up window colour.
#' @param highlight.observation.window \emph{Logical}, should the observation
#' window be plotted?
#' @param observation.window.col,observation.window.opacity
#' Attributes of the observation window (colour, transparency).
#' @param min.plot.size.in.characters.horiz,min.plot.size.in.characters.vert
#' \emph{Numeric}, the minimum size of the plotting surface in characters;
#' horizontally (min.plot.size.in.characters.horiz) refers to the the whole
#' duration of the events to plot; vertically (min.plot.size.in.characters.vert)
#' refers to a single event. If the plotting is too small, possible solutions
#' might be: if within \code{RStudio}, try to enlarge the "Plots" panel, or
#' (also valid outside \code{RStudio} but not if using \code{RStudio server}
#' start a new plotting device (e.g., using \code{X11()}, \code{quartz()}
#' or \code{windows()}, depending on OS) or (works always) save to an image
#' (e.g., \code{jpeg(...); ...; dev.off()}) and display it in a viewer.
#' @param max.patients.to.plot \emph{Numeric}, the maximum patients to attempt
#' to plot.
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any
#' warnings.
#' @param export.formats a \emph{string} giving the formats to export the figure
#' to (by default \code{NULL}, meaning no exporting); can be any combination of
#' "svg" (just an \code{SVG} file), "html" (\code{SVG} + \code{HTML} + \code{CSS}
#' + \code{JavaScript}, all embedded within one \code{HTML} document), "jpg",
#' "png", "webp", "ps" or "pdf".
#' @param export.formats.fileprefix a \emph{string} giving the file name prefix
#' for the exported formats (defaults to "AdhereR-plot").
#' @param export.formats.height,export.formats.width \emph{numbers} giving the
#' desired dimensions (in pixels) for the exported figure (defaults to sane
#' values if \code{NA}).
#' @param export.formats.save.svg.placeholder a \emph{logical}, if TRUE, save an
#' image placeholder of type given by \code{export.formats.svg.placeholder.type}
#'for the \code{SVG} image.
#' @param export.formats.svg.placeholder.type a \emph{string}, giving the type of
#' placeholder for the \code{SVG} image to save; can be "jpg",
#' "png" (the default) or "webp".
#' @param export.formats.svg.placeholder.embed a \emph{logical}, if \code{TRUE},
#' embed the placeholder image in the HTML document (if any) using \code{base64}
#' encoding, otherwise (the default) leave it as an external image file (works
#' only when an \code{HTML} document is exported and only for \code{JPEG} or
#' \code{PNG} images.
#' @param export.formats.html.template,export.formats.html.javascript,export.formats.html.css
#' \emph{character strings} or \code{NULL} (the default) giving the path to the
#' \code{HTML}, \code{JavaScript} and \code{CSS} templates, respectively, to be
#' used when generating the HTML+CSS semi-interactive plots; when \code{NULL},
#' the default ones included with the package will be used. If you decide to define
#' new templates please use the default ones for inspiration and note that future
#' version are not guaranteed to be backwards compatible!
#' @param export.formats.directory a \emph{string}; if exporting, which directory
#' to export to; if \code{NA} (the default), creates the files in a temporary
#' directory.
#' @param generate.R.plot a \emph{logical}, if \code{TRUE} (the default),
#' generate the standard (base \code{R}) plot for plotting within \code{R}.
#' @param ... other parameters (to be passed to the estimation and plotting of
#' the simple CMA)
#'
#' @seealso See the simple CMA estimation \code{\link[AdhereR]{CMA1}}
#' to \code{\link[AdhereR]{CMA9}} and plotting \code{\link[AdhereR]{plot.CMA1}}
#' functions for extra parameters.
#'
#' @examples
#' \dontrun{
#' cmaW <- CMA_sliding_window(CMA=CMA1,
#'                         data=med.events,
#'                         ID.colname="PATIENT_ID",
#'                         event.date.colname="DATE",
#'                         event.duration.colname="DURATION",
#'                         event.daily.dose.colname="PERDAY",
#'                         medication.class.colname="CATEGORY",
#'                         carry.only.for.same.medication=FALSE,
#'                         consider.dosage.change=FALSE,
#'                         followup.window.start=0,
#'                         observation.window.start=0,
#'                         observation.window.duration=365,
#'                         sliding.window.start=0,
#'                         sliding.window.start.unit="days",
#'                         sliding.window.duration=90,
#'                         sliding.window.duration.unit="days",
#'                         sliding.window.step.duration=7,
#'                         sliding.window.step.unit="days",
#'                         sliding.window.no.steps=NA,
#'                         date.format="%m/%d/%Y"
#'                        );
#' plot(cmaW, patients.to.plot=c("1","2"));
#' cmaE <- CMA_per_episode(CMA=CMA1,
#'                         data=med.events,
#'                         ID.colname="PATIENT_ID",
#'                         event.date.colname="DATE",
#'                         event.duration.colname="DURATION",
#'                         event.daily.dose.colname="PERDAY",
#'                         medication.class.colname="CATEGORY",
#'                         carry.only.for.same.medication=FALSE,
#'                         consider.dosage.change=FALSE,
#'                         followup.window.start=0,
#'                         observation.window.start=0,
#'                         observation.window.duration=365,
#'                         date.format="%m/%d/%Y"
#'                        );
#' plot(cmaE, patients.to.plot=c("1","2"));}
#' @export
plot.CMA_per_episode <- function(x,                                     # the CMA_per_episode or CMA_sliding_window (or derived) object
                                 patients.to.plot=NULL,                 # list of patient IDs to plot or NULL for all
                                 duration=NA,                           # duration and end period to plot in days (if missing, determined from the data)
                                 align.all.patients=FALSE, align.first.event.at.zero=TRUE, # should all patients be aligned? and, if so, place the first event as the horizintal 0?
                                 show.period=c("dates","days")[2],      # draw vertical bars at regular interval as dates or days?
                                 period.in.days=90,                     # the interval (in days) at which to draw veritcal lines
                                 show.legend=TRUE, legend.x="right", legend.y="bottom", legend.bkg.opacity=0.5, legend.cex=0.75, legend.cex.title=1.0, # legend params and position
                                 cex=1.0, cex.axis=0.75, cex.lab=1.0,   # various graphical params
                                 show.cma=TRUE,                         # show the CMA type
                                 xlab=c("dates"="Date", "days"="Days"), # Vector of x labels to show for the two types of periods, or a single value for both, or NULL for nothing
                                 ylab=c("withoutCMA"="patient", "withCMA"="patient (& CMA)"), # Vector of y labels to show without and with CMA estimates, or a single value for both, or NULL ofr nonthing
                                 title=c("aligned"="Event patterns (all patients aligned)", "notaligned"="Event patterns"), # Vector of titles to show for and without alignment, or a single value for both, or NULL for nonthing
                                 col.cats=rainbow,                      # single color or a function mapping the categories to colors
                                 unspecified.category.label="drug",     # the label of the unspecified category of medication
                                 medication.groups.to.plot=NULL,        # the names of the medication groups to plot (by default, all)
                                 medication.groups.separator.show=TRUE, medication.groups.separator.lty="solid", medication.groups.separator.lwd=2, medication.groups.separator.color="blue", # group medication events by patient?
                                 medication.groups.allother.label="*",  # the label to use for the __ALL_OTHERS__ medication class (defaults to *)
                                 lty.event="solid", lwd.event=2, pch.start.event=15, pch.end.event=16, # event style
                                 plot.events.vertically.displaced=TRUE, # display the events on different lines (vertical displacement) or not (defaults to TRUE)?
                                 print.dose=FALSE, cex.dose=0.75, print.dose.outline.col="white", print.dose.centered=FALSE, # print daily dose
                                 plot.dose=FALSE, lwd.event.max.dose=8, plot.dose.lwd.across.medication.classes=FALSE, # draw daily dose as line width
                                 col.na="lightgray",                    # color for mising data
                                 col.continuation="black", lty.continuation="dotted", lwd.continuation=1, # style of the contuniation lines connecting consecutive events
                                 print.CMA=TRUE, CMA.cex=0.50,    # print CMA next to the participant's ID?
                                 plot.CMA=TRUE,                   # plot the CMA next to the participant ID?
                                 plot.CMA.as.histogram=TRUE,      # plot CMA as a histogram or as a density plot?
                                 plot.partial.CMAs.as=c("stacked", "overlapping", "timeseries")[1], # how to plot the "partial" (i.e., intervals/episodes) CMAs (NULL for none)?
                                 plot.partial.CMAs.as.stacked.col.bars="gray90", plot.partial.CMAs.as.stacked.col.border="gray30", plot.partial.CMAs.as.stacked.col.text="black",
                                 plot.partial.CMAs.as.timeseries.vspace=7, # how much vertical space to reserve for the timeseries plot (in character lines)
                                 plot.partial.CMAs.as.timeseries.start.from.zero=TRUE, #show the vertical axis start at 0 or at the minimum actual value (if positive)?
                                 plot.partial.CMAs.as.timeseries.col.dot="darkblue", plot.partial.CMAs.as.timeseries.col.interval="gray70", plot.partial.CMAs.as.timeseries.col.text="firebrick", # setting any of these to NA results in them not being plotted
                                 plot.partial.CMAs.as.timeseries.interval.type=c("none", "segments", "arrows", "lines", "rectangles")[2], # how to show the covered intervals
                                 plot.partial.CMAs.as.timeseries.lwd.interval=1, # line width for some types of intervals
                                 plot.partial.CMAs.as.timeseries.alpha.interval=0.25, # the transparency of the intervales (when drawn as rectangles)
                                 plot.partial.CMAs.as.timeseries.show.0perc=TRUE, plot.partial.CMAs.as.timeseries.show.100perc=FALSE, #show the 0% and 100% lines?
                                 plot.partial.CMAs.as.overlapping.alternate=TRUE, # should successive intervals be plotted low/high?
                                 plot.partial.CMAs.as.overlapping.col.interval="gray70", plot.partial.CMAs.as.overlapping.col.text="firebrick", # setting any of these to NA results in them not being plotted
                                 CMA.plot.ratio=0.10,             # the proportion of the total horizontal plot to be taken by the CMA plot
                                 CMA.plot.col="lightgreen", CMA.plot.border="darkgreen", CMA.plot.bkg="aquamarine", CMA.plot.text=CMA.plot.border, # attributes of the CMA plot
                                 highlight.followup.window=TRUE, followup.window.col="green",
                                 highlight.observation.window=TRUE, observation.window.col="yellow", observation.window.opacity=0.3,
                                 alternating.bands.cols=c("white", "gray95"), # the colors of the alternating vertical bands across patients (NULL=don't draw any; can be >= 1 color)
                                 bw.plot=FALSE,                   # if TRUE, override all user-given colors and replace them with a scheme suitable for grayscale plotting
                                 force.draw.text=FALSE,           # if true, always draw text even if too big or too small
                                 min.plot.size.in.characters.horiz=0, min.plot.size.in.characters.vert=0, # the minimum plot size (in characters: horizontally, for the whole duration, vertically, per event (and, if shown, per episode/sliding window))
                                 max.patients.to.plot=100,        # maximum number of patients to plot
                                 export.formats=NULL,                   # the formats to export the figure to (by default, none); can be any subset of "svg" (just SVG file), "html" (SVG + HTML + CSS + JavaScript all embedded within the HTML document), "jpg", "png", "webp", "ps" and "pdf"
                                 export.formats.fileprefix="AdhereR-plot", # the file name prefix for the exported formats
                                 export.formats.height=NA, export.formats.width=NA, # desired dimensions (in pixels) for the exported figure (defaults to sane values)
                                 export.formats.save.svg.placeholder=TRUE,
                                 export.formats.svg.placeholder.type=c("jpg", "png", "webp")[2],
                                 export.formats.svg.placeholder.embed=FALSE, # save a placeholder for the SVG image?
                                 export.formats.html.template=NULL, export.formats.html.javascript=NULL, export.formats.html.css=NULL, # HTML, JavaScript and CSS templates for exporting HTML+SVG
                                 export.formats.directory=NA,           # if exporting, which directory to export to (if not give, creates files in the temporary directory)
                                 generate.R.plot=TRUE,                  # generate standard (base R) plot for plotting within R?
                                 suppress.warnings=FALSE,         # suppress warnings?
                                 ...
)
{
  .plot.CMAs(x,
             patients.to.plot=patients.to.plot,
             duration=duration,
             align.all.patients=align.all.patients,
             align.first.event.at.zero=align.first.event.at.zero,
             show.period=show.period,
             period.in.days=period.in.days,
             show.legend=show.legend,
             legend.x=legend.x,
             legend.y=legend.y,
             legend.bkg.opacity=legend.bkg.opacity,
             legend.cex=legend.cex,
             legend.cex.title=legend.cex.title,
             cex=cex,
             cex.axis=cex.axis,
             cex.lab=cex.lab,
             show.cma=show.cma,
             xlab=xlab,
             ylab=ylab,
             title=title,
             col.cats=col.cats,
             unspecified.category.label=unspecified.category.label,
             medication.groups.to.plot=medication.groups.to.plot,
             medication.groups.separator.show=medication.groups.separator.show,
             medication.groups.separator.lty=medication.groups.separator.lty,
             medication.groups.separator.lwd=medication.groups.separator.lwd,
             medication.groups.separator.color=medication.groups.separator.color,
             medication.groups.allother.label=medication.groups.allother.label,
             lty.event=lty.event,
             lwd.event=lwd.event,
             show.event.intervals=FALSE, # per-episode and sliding windows might have overlapping intervals, so better not to show them at all
             plot.events.vertically.displaced=plot.events.vertically.displaced,
             pch.start.event=pch.start.event,
             pch.end.event=pch.end.event,
             print.dose=print.dose,
             cex.dose=cex.dose,
             print.dose.outline.col=print.dose.outline.col,
             print.dose.centered=print.dose.centered,
             plot.dose=plot.dose,
             lwd.event.max.dose=lwd.event.max.dose,
             plot.dose.lwd.across.medication.classes=plot.dose.lwd.across.medication.classes,
             col.na=col.na,
             print.CMA=print.CMA,
             CMA.cex=CMA.cex,
             plot.CMA=plot.CMA,
             plot.CMA.as.histogram=plot.CMA.as.histogram,
             CMA.plot.ratio=CMA.plot.ratio,
             CMA.plot.col=CMA.plot.col,
             CMA.plot.border=CMA.plot.border,
             CMA.plot.bkg=CMA.plot.bkg,
             CMA.plot.text=CMA.plot.text,
             plot.partial.CMAs.as=plot.partial.CMAs.as,
             plot.partial.CMAs.as.stacked.col.bars=plot.partial.CMAs.as.stacked.col.bars,
             plot.partial.CMAs.as.stacked.col.border=plot.partial.CMAs.as.stacked.col.border,
             plot.partial.CMAs.as.stacked.col.text=plot.partial.CMAs.as.stacked.col.text,
             plot.partial.CMAs.as.timeseries.vspace=plot.partial.CMAs.as.timeseries.vspace,
             plot.partial.CMAs.as.timeseries.start.from.zero=plot.partial.CMAs.as.timeseries.start.from.zero,
             plot.partial.CMAs.as.timeseries.col.dot=plot.partial.CMAs.as.timeseries.col.dot,
             plot.partial.CMAs.as.timeseries.col.interval=plot.partial.CMAs.as.timeseries.col.interval,
             plot.partial.CMAs.as.timeseries.col.text=plot.partial.CMAs.as.timeseries.col.text,
             plot.partial.CMAs.as.timeseries.interval.type=plot.partial.CMAs.as.timeseries.interval.type,
             plot.partial.CMAs.as.timeseries.lwd.interval=plot.partial.CMAs.as.timeseries.lwd.interval,
             plot.partial.CMAs.as.timeseries.alpha.interval=plot.partial.CMAs.as.timeseries.alpha.interval,
             plot.partial.CMAs.as.timeseries.show.0perc=plot.partial.CMAs.as.timeseries.show.0perc,
             plot.partial.CMAs.as.timeseries.show.100perc=plot.partial.CMAs.as.timeseries.show.100perc,
             plot.partial.CMAs.as.overlapping.alternate=plot.partial.CMAs.as.overlapping.alternate,
             plot.partial.CMAs.as.overlapping.col.interval=plot.partial.CMAs.as.overlapping.col.interval,
             plot.partial.CMAs.as.overlapping.col.text=plot.partial.CMAs.as.overlapping.col.text,
             highlight.followup.window=highlight.followup.window,
             followup.window.col=followup.window.col,
             highlight.observation.window=highlight.observation.window,
             observation.window.col=observation.window.col,
             observation.window.opacity=observation.window.opacity,
             alternating.bands.cols=alternating.bands.cols,
             bw.plot=bw.plot,
             force.draw.text=force.draw.text,
             min.plot.size.in.characters.horiz=min.plot.size.in.characters.horiz,
             min.plot.size.in.characters.vert=min.plot.size.in.characters.vert,
             max.patients.to.plot=max.patients.to.plot,
             export.formats=export.formats,
             export.formats.fileprefix=export.formats.fileprefix,
             export.formats.height=export.formats.height,
             export.formats.width=export.formats.width,
             export.formats.save.svg.placeholder=export.formats.save.svg.placeholder,
             export.formats.svg.placeholder.type=export.formats.svg.placeholder.type,
             export.formats.svg.placeholder.embed=export.formats.svg.placeholder.embed,
             export.formats.html.template=export.formats.html.template,
             export.formats.html.javascript=export.formats.html.javascript,
             export.formats.html.css=export.formats.html.css,
             export.formats.directory=export.formats.directory,
             generate.R.plot=generate.R.plot,
             suppress.warnings=suppress.warnings);
}



#' CMA_sliding_window constructor.
#'
#' Applies a given CMA to each sliding window and constructs a
#' CMA_sliding_window object.
#'
#' \code{CMA_sliding_window} first computes a set of fixed-size (possibly partly
#' overlapping) sliding windows,
#' each sliding to the right by a fixed timelag,
#' and then, for each of them, it computes the given "simple" CMA.
#' Thus, as opposed to the "simple" CMAs 1 to 9, it returns a set of CMAs, with
#' possibly more than one element.
#'
#' It is highly similar to \code{\link{CMA_per_episode}} which computes a CMA
#' for a set of treatment episodes.
#'
#' @param CMA.to.apply A \emph{string} giving the name of the CMA function (1 to
#' 9) that will be computed for each treatment episode.
#' @param data A \emph{\code{data.frame}} containing the events used to compute
#' the CMA. Must contain, at a minimum, the patient unique ID, the event date
#' and duration, and might also contain the daily dosage and medication type
#' (the actual column names are defined in the following four parameters).
#' @param ID.colname A \emph{string}, the name of the column in \code{data}
#' containing the unique patient ID; must be present.
#' @param event.date.colname A \emph{string}, the name of the column in
#' \code{data} containing the start date of the event (in the format given in
#' the \code{date.format} parameter); must be present.
#' @param event.duration.colname A \emph{string}, the name of the column in
#' \code{data} containing the event duration (in days); must be present.
#' @param event.daily.dose.colname A \emph{string}, the name of the column in
#' \code{data} containing the prescribed daily dose, or \code{NA} if not defined.
#' @param medication.class.colname A \emph{string}, the name of the column in
#' \code{data} containing the medication type, or \code{NA} if not defined.
#' @param medication.groups A \emph{vector} of characters defining medication
#' groups or the name of a column in \code{data} that defines such groups.
#' The names of the vector are the medication group unique names, while
#' the content defines them as logical expressions. While the names can be any
#' string of characters except "\}", it is recommended to stick to the rules for
#' defining vector names in \code{R}. For example,
#' \code{c("A"="CATEGORY == 'medA'", "AA"="{A} & PERDAY < 4"} defines two
#' medication groups: \emph{A} which selects all events of type "medA", and
#' \emph{B} which selects all events already defined by "A" but with a daily
#' dose lower than 4. If \code{NULL}, no medication groups are defined. If
#' medication groups are defined, there is one CMA estimate for each group;
#' moreover, there is a special group \emph{__ALL_OTHERS__} automatically defined
#' containing all observations \emph{not} covered by any of the explicitly defined
#' groups.
#' @param flatten.medication.groups \emph{Logical}, if \code{FALSE} (the default)
#' then the \code{CMA} and \code{event.info} components of the object are lists
#' with one medication group per element; otherwise, they are \code{data.frame}s
#' with an extra column containing the medication group (its name is given by
#' \code{medication.groups.colname}).
#' @param medication.groups.colname a \emph{string} (defaults to ".MED_GROUP_ID")
#' giving the name of the column storing the group name when
#' \code{flatten.medication.groups} is \code{TRUE}.
#' @param carry.only.for.same.medication \emph{Logical}, if \code{TRUE}, the
#' carry-over applies only across medication of the same type.
#' @param consider.dosage.change \emph{Logical}, if \code{TRUE}, the carry-over
#' is adjusted to also reflect changes in dosage.
#' @param followup.window.start If a \emph{\code{Date}} object, it represents
#' the actual start date of the follow-up window; if a \emph{string} it is the
#' name of the column in \code{data} containing the start date of the follow-up
#' window either as the numbers of \code{followup.window.start.unit} units after
#' the first event (the column must be of type \code{numeric}) or as actual
#' dates (in which case the column must be of type \code{Date} or a string
#' that conforms to the format specified in \code{date.format}); if a
#' \emph{number} it is the number of time units defined in the
#' \code{followup.window.start.unit} parameter after the begin of the
#' participant's first event; or \code{NA} if not defined.
#' @param followup.window.start.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.start} refers to (when a number), or
#' \code{NA} if not defined.
#' @param followup.window.start.per.medication.group a \emph{logical}: if there are
#' medication groups defined and this is \code{TRUE}, then the first event
#' considered for the follow-up window start is relative to each medication group
#' separately, otherwise (the default) it is relative to the patient.
#' @param followup.window.duration either a \emph{number} representing the
#' duration of the follow-up window in the time units given in
#' \code{followup.window.duration.unit}, or a \emph{string} giving the column
#' containing these numbers. Should represent a period for which relevant
#' medication events are recorded accurately (e.g. not extend after end of
#' relevant treatment, loss-to-follow-up or change to a health care provider not
#' covered by the database).
#' @param followup.window.duration.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.duration} refers to, or \code{NA} if not
#' defined.
#' @param observation.window.start,observation.window.start.unit,observation.window.duration,observation.window.duration.unit the definition of the observation window
#' (see the follow-up window parameters above for details).
#' @param sliding.window.start,sliding.window.start.unit,sliding.window.duration,sliding.window.duration.unit the definition of the first sliding window
#' (see the follow-up window parameters above for details).
#' @param sliding.window.step.duration,sliding.window.step.unit if not missing
#' (\code{NA}), these give the step (or "jump") to the right of the sliding
#' window in time units.
#' @param sliding.window.no.steps a \emph{integer} specifying the desired number
#' of sliding windows to cover the observation window (if possible); trumps
#' \code{sliding.window.step.duration} and \code{sliding.window.step.unit}.
#' @param date.format A \emph{string} giving the format of the dates used in the
#' \code{data} and the other parameters; see the \code{format} parameters of the
#' \code{\link[base]{as.Date}} function for details (NB, this concerns only the
#' dates given as strings and not as \code{Date} objects).
#' @param summary Metadata as a \emph{string}, briefly describing this CMA.
#' @param event.interval.colname A \emph{string}, the name of a newly-created
#' column storing the number of days between the start of the current event and
#' the start of the next one; the default value "event.interval" should be
#' changed only if there is a naming conflict with a pre-existing
#' "event.interval" column in \code{event.info}.
#' @param gap.days.colname A \emph{string}, the name of a newly-created column
#' storing the number of days when medication was not available (i.e., the
#' "gap days"); the default value "gap.days" should be changed only if there is
#' a naming conflict with a pre-existing "gap.days" column in \code{event.info}.
#' @param force.NA.CMA.for.failed.patients \emph{Logical} describing how the
#' patients for which the CMA estimation fails are treated: if \code{TRUE}
#' they are returned with an \code{NA} CMA estimate, while for
#' \code{FALSE} they are omitted.
#' @param parallel.backend Can be "none" (the default) for single-threaded
#' execution, "multicore"  (using \code{mclapply} in package \code{parallel})
#' for multicore processing (NB. not currently implemented on MS Windows and
#' automatically falls back on "snow" on this platform),  or "snow",
#' "snow(SOCK)" (equivalent to "snow"), "snow(MPI)" or "snow(NWS)" specifying
#' various types of SNOW clusters (can be on the local machine or more complex
#' setups -- please see the documentation of package \code{snow} for details;
#' the last two require packages \code{Rmpi} and \code{nws}, respectively, not
#' automatically installed with \code{AdhereR}).
#' @param parallel.threads Can be "auto" (for \code{parallel.backend} ==
#' "multicore", defaults to the number of cores in the system as given by
#' \code{options("cores")}, while for \code{parallel.backend} == "snow",
#' defaults to 2), a strictly positive integer specifying the number of parallel
#' threads, or a more complex specification of the SNOW cluster nodes for
#' \code{parallel.backend} == "snow" (see the documentation of package
#' \code{snow} for details).
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any
#' warnings.
#' @param ... other possible parameters
#' @return An \code{S3} object of class \code{CMA_sliding_window} with the
#' following fields:
#' \itemize{
#'  \item \code{data} The actual event data, as given by the \code{data}
#'  parameter.
#'  \item \code{ID.colname} the name of the column in \code{data} containing the
#'  unique patient ID, as given by the \code{ID.colname} parameter.
#'  \item \code{event.date.colname} the name of the column in \code{data}
#'  containing the start date of the event (in the format given in the
#'  \code{date.format} parameter), as given by the \code{event.date.colname}
#'  parameter.
#'  \item \code{event.duration.colname} the name of the column in \code{data}
#'  containing the event duration (in days), as given by the
#'  \code{event.duration.colname} parameter.
#'  \item \code{event.daily.dose.colname} the name of the column in \code{data}
#'  containing the prescribed daily dose, as given by the
#'  \code{event.daily.dose.colname} parameter.
#'  \item \code{medication.class.colname} the name of the column in \code{data}
#'  containing the classes/types/groups of medication, as given by the
#'  \code{medication.class.colname} parameter.
#'  \item \code{carry.only.for.same.medication} whether the carry-over applies
#'  only across medication of the same type, as given by the
#'  \code{carry.only.for.same.medication} parameter.
#'  \item \code{consider.dosage.change} whether the carry-over is adjusted to
#'  reflect changes in dosage, as given by the \code{consider.dosage.change} parameter.
#'  \item \code{followup.window.start} the beginning of the follow-up window,
#'   as given by the \code{followup.window.start} parameter.
#'  \item \code{followup.window.start.unit} the time unit of the
#'  \code{followup.window.start}, as given by the
#'  \code{followup.window.start.unit} parameter.
#'  \item \code{followup.window.duration} the duration of the follow-up window,
#'  as given by the \code{followup.window.duration} parameter.
#'  \item \code{followup.window.duration.unit} the time unit of the
#'  \code{followup.window.duration}, as given by the
#'  \code{followup.window.duration.unit} parameter.
#'  \item \code{observation.window.start} the beginning of the observation
#'  window, as given by the \code{observation.window.start} parameter.
#'  \item \code{observation.window.start.unit} the time unit of the
#'  \code{observation.window.start}, as given by the
#'  \code{observation.window.start.unit} parameter.
#'  \item \code{observation.window.duration} the duration of the observation
#'  window, as given by the \code{observation.window.duration} parameter.
#'  \item \code{observation.window.duration.unit} the time unit of the
#'  \code{observation.window.duration}, as given by the
#'  \code{observation.window.duration.unit} parameter.
#'  \item \code{date.format} the format of the dates, as given by the
#'  \code{date.format} parameter.
#'  \item \code{summary} the metadata, as given by the \code{summary} parameter.
#'  \item \code{event.info} the \code{data.frame} containing the event info
#'  (irrelevant for most users; see \code{\link{compute.event.int.gaps}} for
#'  details).
#'  \item \code{computed.CMA} the class name of the computed CMA.
#'  \item \code{CMA} the \code{data.frame} containing the actual \code{CMA}
#'  estimates for each participant (the \code{ID.colname} column) and sliding
#'  window, with columns:
#'    \itemize{
#'      \item \code{ID.colname} the patient ID as given by the \code{ID.colname}
#'      parameter.
#'      \item \code{window.ID} the unique window ID (within patients).
#'      \item \code{window.start} the window's start date (as a \code{Date}
#'      object).
#'      \item \code{window.end} the window's end date (as a \code{Date} object).
#'      \item \code{CMA} the window's estimated CMA.
#'    }
#' }
#' Please note that if \code{medication.groups} are defined, then the \code{CMA}
#' and \code{event.info} are named lists, each element containing the CMA and
#' event.info corresponding to a single medication group (the element's name).
#' @seealso \code{\link{CMA_per_episode}} is very similar, computing a "simple"
#' CMA for each of the treatment episodes.
#' The "simple" CMAs that can be computed comprise \code{\link{CMA1}},
#' \code{\link{CMA2}}, \code{\link{CMA3}}, \code{\link{CMA4}},
#' \code{\link{CMA5}}, \code{\link{CMA6}}, \code{\link{CMA7}},
#' \code{\link{CMA8}}, \code{\link{CMA9}}, as well as user-defined classes
#' derived from \code{\link{CMA0}} that have a \code{CMA} component giving the
#' estimated CMA per patient as a \code{data.frame}.
#' @examples
#' \dontrun{
#' cmaW <- CMA_sliding_window(CMA="CMA1",
#'                            data=med.events,
#'                            ID.colname="PATIENT_ID",
#'                            event.date.colname="DATE",
#'                            event.duration.colname="DURATION",
#'                            event.daily.dose.colname="PERDAY",
#'                            medication.class.colname="CATEGORY",
#'                            carry.only.for.same.medication=FALSE,
#'                            consider.dosage.change=FALSE,
#'                            followup.window.start=0,
#'                            observation.window.start=0,
#'                            observation.window.duration=365,
#'                            sliding.window.start=0,
#'                            sliding.window.start.unit="days",
#'                            sliding.window.duration=90,
#'                            sliding.window.duration.unit="days",
#'                            sliding.window.step.duration=7,
#'                            sliding.window.step.unit="days",
#'                            sliding.window.no.steps=NA,
#'                            date.format="%m/%d/%Y"
#'                           );}
#' @export
CMA_sliding_window <- function( CMA.to.apply,  # the name of the CMA function (e.g., "CMA1") to be used
                                data, # the data used to compute the CMA on
                                # Important columns in the data
                                ID.colname=NA, # the name of the column containing the unique patient ID (NA = undefined)
                                event.date.colname=NA, # the start date of the event in the date.format format (NA = undefined)
                                event.duration.colname=NA, # the event duration in days (NA = undefined)
                                event.daily.dose.colname=NA, # the prescribed daily dose (NA = undefined)
                                medication.class.colname=NA, # the classes/types/groups of medication (NA = undefined)
                                # Groups of medication classes:
                                medication.groups=NULL, # a named vector of medication group definitions, the name of a column in the data that defines the groups, or NULL
                                flatten.medication.groups=FALSE, medication.groups.colname=".MED_GROUP_ID", # if medication.groups were defined, return CMAs and event.info as single data.frame?
                                # Various types methods of computing gaps:
                                carry.only.for.same.medication=NA, # if TRUE the carry-over applies only across medication of same type (NA = undefined)
                                consider.dosage.change=NA, # if TRUE carry-over is adjusted to reflect changes in dosage (NA = undefined)
                                # The follow-up window:
                                followup.window.start=0, # if a number is the earliest event per participant date + number of units, or a Date object, or a column name in data (NA = undefined)
                                followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                                followup.window.start.per.medication.group=FALSE, # if there are medication groups and this is TRUE, then the first event is relative to each medication group separately, otherwise is relative to the patient
                                followup.window.duration=365*2, # the duration of the follow-up window in the time units given below (NA = undefined)
                                followup.window.duration.unit=c("days", "weeks", "months", "years")[1],# the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)  (NA = undefined)
                                # The observation window (embedded in the follow-up window):
                                observation.window.start=0, # the number of time units relative to followup.window.start (NA = undefined)
                                observation.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                                observation.window.duration=365*2, # the duration of the observation window in time units (NA = undefined)
                                observation.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                                # Sliding window:
                                sliding.window.start=0, # if a number is the earliest event per participant date + number of units, or a Date object, or a column name in data (NA = undefined)
                                sliding.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                                sliding.window.duration=90,  # the duration of the sliding window in time units (NA = undefined)
                                sliding.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                                sliding.window.step.duration=30, # the step ("jump") of the sliding window in time units (NA = undefined)
                                sliding.window.step.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                                sliding.window.no.steps=NA, # the number of steps to jump; has priority over setp specification
                                # Date format:
                                date.format="%m/%d/%Y", # the format of the dates used in this function (NA = undefined)
                                # Comments and metadata:
                                summary="CMA per sliding window",
                                # The description of the output (added) columns:
                                event.interval.colname="event.interval", # contains number of days between the start of current event and the start of the next
                                gap.days.colname="gap.days", # contains the number of days when medication was not available
                                # Dealing with failed estimates:
                                force.NA.CMA.for.failed.patients=TRUE, # force the failed patients to have NA CM estimate?
                                # Parallel processing:
                                parallel.backend=c("none","multicore","snow","snow(SOCK)","snow(MPI)","snow(NWS)")[1], # parallel backend to use
                                parallel.threads="auto", # specification (or number) of parallel threads
                                # Misc:
                                suppress.warnings=FALSE,
                                # extra parameters to be sent to the CMA function:
                                ...
)
{
  # Preconditions:
  if( is.numeric(sliding.window.start) && sliding.window.start < 0 )
  {
    if( !suppress.warnings ) .report.ewms("The sliding window must start a positive number of time units after the start of the observation window!\n", "error", "CMA_sliding_window", "AdhereR")
    return (NULL);
  }
  if( !inherits(sliding.window.start,"Date") && !is.numeric(sliding.window.start) && !(sliding.window.start %in% names(data)) )
  {
    if( !suppress.warnings ) .report.ewms("The sliding window start must be a valid column name!\n", "error", "CMA_sliding_window", "AdhereR")
    return (NULL);
  }
  if( !(sliding.window.start.unit %in% c("days", "weeks", "months", "years") ) )
  {
    if( !suppress.warnings ) .report.ewms("The sliding window start unit is not recognized!\n", "error", "CMA_sliding_window", "AdhereR")
    return (NULL);
  }
  if( !is.numeric(sliding.window.duration) || sliding.window.duration <= 0 )
  {
    if( !suppress.warnings ) .report.ewms("The sliding window duration must be greater than 0!\n", "error", "CMA_sliding_window", "AdhereR")
    return (NULL);
  }
  if( !(sliding.window.duration.unit %in% c("days", "weeks", "months", "years") ) )
  {
    if( !suppress.warnings ) .report.ewms("The sliding window duration unit is not recognized!\n", "error", "CMA_sliding_window", "AdhereR")
    return (NULL);
  }
  if( is.numeric(sliding.window.step.duration) && sliding.window.step.duration < 1 )
  {
    if( !suppress.warnings ) .report.ewms("The sliding window's step duration must be at least 1 unit!\n", "error", "CMA_sliding_window", "AdhereR")
    return (NULL);
  }
  if( !(sliding.window.step.unit %in% c("days", "weeks", "months", "years") ) )
  {
    if( !suppress.warnings ) .report.ewms("The sliding window's step duration unit is not recognized!\n", "error", "CMA_sliding_window", "AdhereR")
    return (NULL);
  }
  if( is.numeric(sliding.window.no.steps) && sliding.window.no.steps < 1 )
  {
    if( !suppress.warnings ) .report.ewms("The sliding window must move at least once!\n", "error", "CMA_sliding_window", "AdhereR")
    return (NULL);
  }

  # Get the CMA function corresponding to the name:
  if( !(is.character(CMA.to.apply) || is.factor(CMA.to.apply)) )
  {
    if( !suppress.warnings ) .report.ewms(paste0("'CMA.to.apply' must be a string contining the name of the simple CMA to apply!\n)"), "error", "CMA_sliding_window", "AdhereR");
    return (NULL);
  }
  CMA.FNC <- switch(as.character(CMA.to.apply), # if factor, force it to string
                    "CMA1" = CMA1,
                    "CMA2" = CMA2,
                    "CMA3" = CMA3,
                    "CMA4" = CMA4,
                    "CMA5" = CMA5,
                    "CMA6" = CMA6,
                    "CMA7" = CMA7,
                    "CMA8" = CMA8,
                    "CMA9" = CMA9,
                    {if( !suppress.warnings ) .report.ewms(paste0("Unknown 'CMA.to.apply' '",CMA.to.apply,"': defaulting to CMA0!\n)"), "warning", "CMA_sliding_window", "AdhereR"); CMA0;}); # by default, fall back to CMA0

  # Default argument values and overrides:
  def.vals <- formals(CMA.FNC);
  if( CMA.to.apply %in% c("CMA1", "CMA2", "CMA3", "CMA4") )
  {
    carryover.into.obs.window <- carryover.within.obs.window <- FALSE;
    if( !is.na(carry.only.for.same.medication) && carry.only.for.same.medication && !suppress.warnings ) .report.ewms("'carry.only.for.same.medication' cannot be defined for CMAs 1-4!\n", "warning", "CMA_sliding_window", "AdhereR");
    carry.only.for.same.medication <- FALSE;
    if( !is.na(consider.dosage.change) && consider.dosage.change && !suppress.warnings ) .report.ewms("'consider.dosage.change' cannot be defined for CMAs 1-4!\n", "warning", "CMA_sliding_window", "AdhereR");
    consider.dosage.change <- FALSE;
  } else if( CMA.to.apply %in% c("CMA5", "CMA6") )
  {
    carryover.into.obs.window <- FALSE;
    carryover.within.obs.window <- TRUE;
    if( is.na(carry.only.for.same.medication) ) carry.only.for.same.medication <- def.vals[["carry.only.for.same.medication"]]; # use the default value from CMA
    if( is.na(consider.dosage.change) ) consider.dosage.change <- def.vals[["consider.dosage.change"]]; # use the default value from CMA
  } else if( CMA.to.apply %in% c("CMA7", "CMA8", "CMA9") )
  {
    carryover.into.obs.window <- carryover.within.obs.window <- TRUE;
    if( is.na(carry.only.for.same.medication) ) carry.only.for.same.medication <- def.vals[["carry.only.for.same.medication"]]; # use the default value from CMA
    if( is.na(consider.dosage.change) ) consider.dosage.change <- def.vals[["consider.dosage.change"]]; # use the default value from CMA
  } else
  {
    if( !suppress.warnings ) .report.ewms("I know how to do CMA sliding windows only for CMAs 1 to 9!\n", "error", "CMA_sliding_window", "AdhereR");
    return (NULL);
  }

  # Create the return value skeleton and check consistency:
  ret.val <- CMA0(data,
                  ID.colname=ID.colname,
                  event.date.colname=event.date.colname,
                  event.duration.colname=event.duration.colname,
                  event.daily.dose.colname=event.daily.dose.colname,
                  medication.class.colname=medication.class.colname,
                  medication.groups=medication.groups,
                  flatten.medication.groups=flatten.medication.groups,
                  medication.groups.colname=medication.groups.colname,
                  carryover.within.obs.window=carryover.within.obs.window,
                  carryover.into.obs.window=carryover.into.obs.window,
                  carry.only.for.same.medication=carry.only.for.same.medication,
                  consider.dosage.change=consider.dosage.change,
                  followup.window.start=followup.window.start,
                  followup.window.start.unit=followup.window.start.unit,
                  followup.window.start.per.medication.group=followup.window.start.per.medication.group,
                  followup.window.duration=followup.window.duration,
                  followup.window.duration.unit=followup.window.duration.unit,
                  observation.window.start=observation.window.start,
                  observation.window.start.unit=observation.window.start.unit,
                  observation.window.duration=observation.window.duration,
                  observation.window.duration.unit=observation.window.duration.unit,
                  date.format=date.format,
                  suppress.warnings=suppress.warnings,
                  summary=NA);
  if( is.null(ret.val) ) return (NULL);
  # The followup.window.start and observation.window.start might have been converted to Date:
  followup.window.start <- ret.val$followup.window.start; observation.window.start <- ret.val$observation.window.start;

  # The workhorse auxiliary function: For a given (subset) of data, compute the event intervals and gaps:
  .workhorse.function <- function(data=NULL,
                                  ID.colname=NULL,
                                  event.date.colname=NULL,
                                  event.duration.colname=NULL,
                                  event.daily.dose.colname=NULL,
                                  medication.class.colname=NULL,
                                  event.interval.colname=NULL,
                                  gap.days.colname=NULL,
                                  carryover.within.obs.window=NULL,
                                  carryover.into.obs.window=NULL,
                                  carry.only.for.same.medication=NULL,
                                  consider.dosage.change=NULL,
                                  followup.window.start=NULL,
                                  followup.window.start.unit=NULL,
                                  followup.window.duration=NULL,
                                  followup.window.duration.unit=NULL,
                                  observation.window.start=NULL,
                                  observation.window.start.unit=NULL,
                                  observation.window.duration=NULL,
                                  observation.window.duration.unit=NULL,
                                  date.format=NULL,
                                  suppress.warnings=NULL
  )
  {
    # Auxiliary internal function: Compute the CMA for a given patient:
    .process.patient <- function(data4ID)
    {
      n.events <- nrow(data4ID); # cache number of events
      if( n.events < 1 ) return (NULL);

      # Compute the sliding windows for this patient:
      start.date <- .add.time.interval.to.date(data4ID$.OBS.START.DATE[1], sliding.window.start, sliding.window.start.unit, suppress.warnings); # when do the windows start?
      sliding.duration <- as.numeric(data4ID$.OBS.END.DATE[1] - start.date) - sliding.window.duration.in.days; # the effective duration to be covered with sliding windows
      if( sliding.duration < 0)  return (NULL); # the sliding window is longer than the available time in the observation window
      if( is.na(sliding.window.no.steps) )
      {
        # Compute the number of steps required from the step size:
        sliding.window.no.steps <- (sliding.duration / sliding.window.step.duration.in.days) + 1;
      } else if( sliding.window.no.steps > 1 )
      {
        # Compute the step size to optimally cover the duration (possibly adjust the number of steps, too):
        sliding.window.step.duration.in.days <- (sliding.duration / (sliding.window.no.steps - 1));
        sliding.window.step.duration.in.days <- max(1, min(sliding.window.duration.in.days, sliding.window.step.duration.in.days)); # make sure we don't overdue it
        sliding.window.no.steps <- min(((sliding.duration / sliding.window.step.duration.in.days) + 1), sliding.duration); # adjust the number of steps just in case
      } else
      {
        # Only one sliding window:
        sliding.window.step.duration.in.days <- 0;
      }
      if( sliding.window.no.steps < 1 || sliding.window.step.duration.in.days < 0 ) return (NULL);

      # Expand the participant data by replicating it for each consecutive sliding window:
      data4ID.wnds <- cbind(".WND.ID"        =rep(1:sliding.window.no.steps, each=n.events),
                            data4ID,
                            ".WND.START.DATE"=rep(as.Date(vapply(1:sliding.window.no.steps,
                                                                 function(i) .add.time.interval.to.date(start.date, (i-1)*sliding.window.step.duration.in.days, "days", suppress.warnings),
                                                                 numeric(1)),
                                                          origin=lubridate::origin),
                                                      each=n.events),
                            ".WND.DURATION"  =sliding.window.duration.in.days);
      setkeyv(data4ID.wnds, ".WND.ID");

      # Apply the desired CMA to all the windows:
      cma <- CMA.FNC(data=as.data.frame(data4ID.wnds),
                     ID.colname=".WND.ID",
                     event.date.colname=event.date.colname,
                     event.duration.colname=event.duration.colname,
                     event.daily.dose.colname=event.daily.dose.colname,
                     medication.class.colname=medication.class.colname,
                     carry.only.for.same.medication=carry.only.for.same.medication,
                     consider.dosage.change=consider.dosage.change,
                     followup.window.start=followup.window.start,
                     followup.window.start.unit=followup.window.start.unit,
                     followup.window.duration=followup.window.duration,
                     followup.window.duration.unit=followup.window.duration.unit,
                     observation.window.start=".WND.START.DATE",
                     observation.window.duration=".WND.DURATION",
                     observation.window.duration.unit="days",
                     date.format=date.format,
                     parallel.backend="none", # make sure this runs sequentially!
                     parallel.threads=1,
                     suppress.warnings=suppress.warnings,
                     ...);
      if( is.null(cma) ) return (NULL);

      # Unpack the data fo returning:
      wnd.info <- data4ID.wnds[,
                               list(".WND.START"=.WND.START.DATE[1], # the start
                                    ".WND.END"=.add.time.interval.to.date(.WND.START.DATE[1], sliding.window.duration.in.days, "days", suppress.warnings)), # and end
                               by=.WND.ID]; # for each window
      wnd.info <- cbind(merge(wnd.info, cma$CMA, by=".WND.ID", all=TRUE),
                        "CMA.to.apply"=class(cma)[1]);
      setnames(wnd.info, c("window.ID", "window.start", "window.end", "CMA", "CMA.to.apply"));
      return (as.data.frame(wnd.info));
    }

    # Compute the real observation windows (might differ per patient) only once per patient (speed things up & the observation window is the same for all events within a patient):
    #patinfo.cols <- which(names(data) %in% c(ID.colname, event.date.colname, event.duration.colname, event.daily.dose.colname, medication.class.colname));
    #tmp <- as.data.frame(data); tmp <- tmp[!duplicated(tmp[,ID.colname]),patinfo.cols]; # the reduced dataset for computing the actual OW:
    tmp <- as.data.frame(data); tmp <- tmp[!duplicated(tmp[,ID.colname]),]; # the reduced dataset for computing the actual OW:
    event.info2 <- compute.event.int.gaps(data=tmp,
                                          ID.colname=ID.colname,
                                          event.date.colname=event.date.colname,
                                          event.duration.colname=event.duration.colname,
                                          event.daily.dose.colname=event.daily.dose.colname,
                                          medication.class.colname=medication.class.colname,
                                          event.interval.colname=event.interval.colname,
                                          gap.days.colname=gap.days.colname,
                                          carryover.within.obs.window=FALSE,
                                          carryover.into.obs.window=FALSE,
                                          carry.only.for.same.medication=FALSE,
                                          consider.dosage.change=FALSE,
                                          followup.window.start=followup.window.start,
                                          followup.window.start.unit=followup.window.start.unit,
                                          followup.window.duration=followup.window.duration,
                                          followup.window.duration.unit=followup.window.duration.unit,
                                          observation.window.start=observation.window.start,
                                          observation.window.start.unit=observation.window.start.unit,
                                          observation.window.duration=observation.window.duration,
                                          observation.window.duration.unit=observation.window.duration.unit,
                                          date.format=date.format,
                                          keep.window.start.end.dates=TRUE,
                                          remove.events.outside.followup.window=FALSE,
                                          parallel.backend="none", # make sure this runs sequentially!
                                          parallel.threads=1,
                                          suppress.warnings=suppress.warnings,
                                          return.data.table=TRUE);
    if( is.null(event.info2) ) return (NULL);
    # Merge the observation window start and end dates back into the data:
    data <- merge(data, event.info2[,c(ID.colname, ".OBS.START.DATE", ".OBS.END.DATE"),with=FALSE], all.x=TRUE);
    setnames(data, ncol(data)-c(1,0), c(".OBS.START.DATE.PRECOMPUTED", ".OBS.END.DATE.PRECOMPUTED"));

    CMA <- data[, .process.patient(.SD), by=ID.colname ];
    return (list("CMA"=CMA, "event.info"=event.info2[,c(ID.colname, ".FU.START.DATE", ".FU.END.DATE", ".OBS.START.DATE", ".OBS.END.DATE"), with=FALSE]));
  }

  # Convert the sliding window duration and step in days:
  sliding.window.duration.in.days <- switch(sliding.window.duration.unit,
                                            "days"=sliding.window.duration,
                                            "weeks"=sliding.window.duration * 7,
                                            "months"=sliding.window.duration * 30,
                                            "years"=sliding.window.duration * 365,
                                            sliding.window.duration);
  sliding.window.step.duration.in.days <- switch(sliding.window.step.unit,
                                                 "days"=sliding.window.step.duration,
                                                 "weeks"=sliding.window.step.duration * 7,
                                                 "months"=sliding.window.step.duration * 30,
                                                 "years"=sliding.window.step.duration * 365,
                                                 sliding.window.step.duration);

  # Convert to data.table, cache event date as Date objects, and key by patient ID and event date
  data.copy <- data.table(data);
  data.copy[, .DATE.as.Date := as.Date(get(event.date.colname),format=date.format)]; # .DATE.as.Date: convert event.date.colname from formatted string to Date
  data.copy$..ORIGINAL.ROW.ORDER.. <- 1:nrow(data.copy); # preserve the original order of the rows (needed for medication groups)
  setkeyv(data.copy, c(ID.colname, ".DATE.as.Date")); # key (and sorting) by patient ID and event date


  # Are there medication groups?
  if( is.null(mg <- getMGs(ret.val)) )
  {
    # Nope: do a single estimation on the whole dataset:

    # Compute the workhorse function:
    tmp <- .compute.function(.workhorse.function, fnc.ret.vals=2,
                             parallel.backend=parallel.backend,
                             parallel.threads=parallel.threads,
                             data=data.copy,
                             ID.colname=ID.colname,
                             event.date.colname=event.date.colname,
                             event.duration.colname=event.duration.colname,
                             event.daily.dose.colname=event.daily.dose.colname,
                             medication.class.colname=medication.class.colname,
                             event.interval.colname=event.interval.colname,
                             gap.days.colname=gap.days.colname,
                             carryover.within.obs.window=carryover.within.obs.window,
                             carryover.into.obs.window=carryover.into.obs.window,
                             carry.only.for.same.medication=carry.only.for.same.medication,
                             consider.dosage.change=consider.dosage.change,
                             followup.window.start=followup.window.start,
                             followup.window.start.unit=followup.window.start.unit,
                             followup.window.duration=followup.window.duration,
                             followup.window.duration.unit=followup.window.duration.unit,
                             observation.window.start=observation.window.start,
                             observation.window.start.unit=observation.window.start.unit,
                             observation.window.duration=observation.window.duration,
                             observation.window.duration.unit=observation.window.duration.unit,
                             date.format=date.format,
                             suppress.warnings=suppress.warnings);
    if( is.null(tmp) || is.null(tmp$CMA) || !inherits(tmp$CMA,"data.frame") || is.null(tmp$event.info) ) return (NULL);

    # Construct the return object:
    class(ret.val) <- "CMA_sliding_window";
    ret.val$event.info <- as.data.frame(tmp$event.info);
    ret.val$computed.CMA <- as.character(tmp$CMA$CMA.to.apply[1]);
    ret.val$sliding.window.start <- sliding.window.start;
    ret.val$sliding.window.start.unit <- sliding.window.start.unit;
    ret.val$sliding.window.duration <- sliding.window.duration;
    ret.val$sliding.window.duration.unit <- sliding.window.duration.unit;
    ret.val$sliding.window.step.duration <- sliding.window.step.duration;
    ret.val$sliding.window.step.unit <- sliding.window.step.unit;
    ret.val$sliding.window.no.steps <- sliding.window.no.steps;
    ret.val$summary <- summary;
    ret.val$CMA <- as.data.frame(tmp$CMA); setnames(ret.val$CMA, 1, ID.colname); ret.val$CMA <- ret.val$CMA[,-ncol(ret.val$CMA)];
    return (ret.val);

  } else
  {
    # Yes

    # Make sure the group's observations reflect the potentially new order of the observations in the data:
    mb.obs <- mg$obs[data.copy$..ORIGINAL.ROW.ORDER.., ];

    # Focus only on the non-trivial ones:
    mg.to.eval <- (colSums(!is.na(mb.obs) & mb.obs) > 0);
    if( sum(mg.to.eval) == 0 )
    {
      # None selects not even one observation!
      .report.ewms(paste0("None of the medication classes (included __ALL_OTHERS__) selects any observation!\n"), "warning", "CMA1", "AdhereR");
      return (NULL);
    }
    mb.obs <- mb.obs[,mg.to.eval]; # keep only the non-trivial ones

    # Check if there are medication classes that refer to the same observations (they would result in the same estimates):
    mb.obs.dupl <- duplicated(mb.obs, MARGIN=2);

    # Estimate each separately:
    tmp <- lapply(1:nrow(mg$defs), function(i)
    {
      # Check if these are to be evaluated:
      if( !mg.to.eval[i] )
      {
        return (list("CMA"=NULL, "event.info"=NULL, "CMA.to.apply"=NA));
      }

      # Translate into the index of the classes to be evaluated:
      ii <- sum(mg.to.eval[1:i]);

      # Cache the selected observations:
      mg.sel.obs <- mb.obs[,ii];

      # Check if this is a duplicated medication class:
      if( mb.obs.dupl[ii] )
      {
        # Find which one is the original:
        for( j in 1:(ii-1) ) # ii=1 never should be TRUE
        {
          if( identical(mb.obs[,j], mg.sel.obs) )
          {
            # This is the original: return it and stop
            return (c("identical.to"=j));
          }
        }
      }

      # Compute the workhorse function:
      tmp <- .compute.function(.workhorse.function, fnc.ret.vals=2,
                               parallel.backend=parallel.backend,
                               parallel.threads=parallel.threads,
                               data=data.copy[mg.sel.obs,], # apply it on the subset of observations covered by this medication class
                               ID.colname=ID.colname,
                               event.date.colname=event.date.colname,
                               event.duration.colname=event.duration.colname,
                               event.daily.dose.colname=event.daily.dose.colname,
                               medication.class.colname=medication.class.colname,
                               event.interval.colname=event.interval.colname,
                               gap.days.colname=gap.days.colname,
                               carryover.within.obs.window=carryover.within.obs.window,
                               carryover.into.obs.window=carryover.into.obs.window,
                               carry.only.for.same.medication=carry.only.for.same.medication,
                               consider.dosage.change=consider.dosage.change,
                               followup.window.start=followup.window.start,
                               followup.window.start.unit=followup.window.start.unit,
                               followup.window.duration=followup.window.duration,
                               followup.window.duration.unit=followup.window.duration.unit,
                               observation.window.start=observation.window.start,
                               observation.window.start.unit=observation.window.start.unit,
                               observation.window.duration=observation.window.duration,
                               observation.window.duration.unit=observation.window.duration.unit,
                               date.format=date.format,
                               suppress.warnings=suppress.warnings);
      if( is.null(tmp) || is.null(tmp$CMA) || !inherits(tmp$CMA,"data.frame") || is.null(tmp$event.info) ) return (NULL);

      # Convert to data.frame and return:
      tmp$CMA.to.apply <- tmp$CMA$CMA.to.apply[1];
      tmp$CMA <- as.data.frame(tmp$CMA); setnames(tmp$CMA, 1, ID.colname); tmp$CMA <- tmp$CMA[,-ncol(tmp$CMA)];
      tmp$event.info <- as.data.frame(tmp$event.info);
      return (tmp);

    });

    # Set the names:
    names(tmp) <- mg$defs$name;

    # Solve the duplicates:
    for( i in seq_along(tmp) )
    {
      if( is.numeric(tmp[[i]]) && length(tmp[[i]]) == 1 && names(tmp[[i]]) == "identical.to" ) tmp[[i]] <- tmp[[ tmp[[i]] ]];
    }

    # Rearrange these and return:
    ret.val[["CMA"]]        <- lapply(tmp, function(x) x$CMA);
    ret.val[["event.info"]] <- lapply(tmp, function(x) x$event.info);
    ret.val$computed.CMA <- unique(vapply(tmp, function(x) if(is.null(x) || is.na(x$CMA.to.apply)){return (NA_character_)}else{return(x$CMA.to.apply)}, character(1))); ret.val$computed.CMA <- ret.val$computed.CMA[ !is.na(ret.val$computed.CMA) ];
    if( flatten.medication.groups && !is.na(medication.groups.colname) )
    {
      # Flatten the CMA:
      tmp <- do.call(rbind, ret.val[["CMA"]]);
      if( is.null(tmp) || nrow(tmp) == 0 )
      {
        ret.val[["CMA"]] <- NULL;
      } else
      {
        tmp <- cbind(tmp, unlist(lapply(1:length(ret.val[["CMA"]]), function(i) if(!is.null(ret.val[["CMA"]][[i]])){rep(names(ret.val[["CMA"]])[i], nrow(ret.val[["CMA"]][[i]]))}else{NULL})));
        names(tmp)[ncol(tmp)] <- medication.groups.colname; rownames(tmp) <- NULL;
        ret.val[["CMA"]] <- tmp;
      }

      # ... and the event.info:
      tmp <- do.call(rbind, ret.val[["event.info"]]);
      if( is.null(tmp) || nrow(tmp) == 0 )
      {
        ret.val[["event.info"]] <- NULL;
      } else
      {
        tmp <- cbind(tmp, unlist(lapply(1:length(ret.val[["event.info"]]), function(i) if(!is.null(ret.val[["event.info"]][[i]])){rep(names(ret.val[["event.info"]])[i], nrow(ret.val[["event.info"]][[i]]))}else{NULL})));
        names(tmp)[ncol(tmp)] <- medication.groups.colname; rownames(tmp) <- NULL;
        ret.val[["event.info"]] <- tmp;
      }
    }
    class(ret.val) <- "CMA_sliding_window";
    ret.val$sliding.window.start.unit <- sliding.window.start.unit;
    ret.val$sliding.window.duration <- sliding.window.duration;
    ret.val$sliding.window.duration.unit <- sliding.window.duration.unit;
    ret.val$sliding.window.step.duration <- sliding.window.step.duration;
    ret.val$sliding.window.step.unit <- sliding.window.step.unit;
    ret.val$sliding.window.no.steps <- sliding.window.no.steps;
    ret.val$summary <- summary;
    return (ret.val);

  }
}

#' @export
getMGs.CMA_sliding_window <- function(x)
{
  cma <- x; # parameter x is required for S3 consistency, but I like cma more
  if( is.null(cma) || !inherits(cma, "CMA_sliding_window") || is.null(cma$medication.groups) ) return (NULL);
  return (cma$medication.groups);
}

#' @export
getCMA.CMA_sliding_window <- function(x, flatten.medication.groups=FALSE, medication.groups.colname=".MED_GROUP_ID")
{
  cma <- x; # parameter x is required for S3 consistency, but I like cma more
  if( is.null(cma) || !inherits(cma, "CMA_sliding_window") || !("CMA" %in% names(cma)) || is.null(cma$CMA) ) return (NULL);
  if( inherits(cma$CMA, "data.frame") || !flatten.medication.groups )
  {
    return (cma$CMA);
  } else
  {
    # Flatten the medication groups into a single data.frame:
    ret.val <- do.call(rbind, cma$CMA);
    if( is.null(ret.val) || nrow(ret.val) == 0 ) return (NULL);
    ret.val <- cbind(ret.val, unlist(lapply(1:length(cma$CMA), function(i) if(!is.null(cma$CMA[[i]])){rep(names(cma$CMA)[i], nrow(cma$CMA[[i]]))}else{NULL})));
    names(ret.val)[ncol(ret.val)] <- medication.groups.colname; rownames(ret.val) <- NULL;
    return (ret.val);
  }
}

#' @export
getEventInfo.CMA_sliding_window <- function(x, flatten.medication.groups=FALSE, medication.groups.colname=".MED_GROUP_ID")
{
  cma <- x; # parameter x is required for S3 consistency, but I like cma more
  if( is.null(cma) || !inherits(cma, "CMA_sliding_window") || !("event.info" %in% names(cma)) || is.null(cma$event.info) ) return (NULL);
  if( inherits(cma$event.info, "data.frame") || !flatten.medication.groups )
  {
    return (cma$event.info);
  } else
  {
    # Flatten the medication groups into a single data.frame:
    ret.val <- do.call(rbind, cma$event.info);
    if( is.null(ret.val) || nrow(ret.val) == 0 ) return (NULL);
    ret.val <- cbind(ret.val, unlist(lapply(1:length(cma$event.info), function(i) if(!is.null(cma$event.info[[i]])){rep(names(cma$event.info)[i], nrow(cma$event.info[[i]]))}else{NULL})));
    names(ret.val)[ncol(ret.val)] <- medication.groups.colname; rownames(ret.val) <- NULL;
    return (ret.val);
  }
}

#' @export
subsetCMA.CMA_sliding_window <- function(cma, patients, suppress.warnings=FALSE)
{
  if( inherits(patients, "factor") ) patients <- as.character(patients);
  patients.to.keep <- intersect(patients, unique(cma$data[,cma$ID.colname]));
  if( length(patients.to.keep) == 0 )
  {
    if( !suppress.warnings ) .report.ewms("No patients to subset on!\n", "error", "subsetCMA.CMA_sliding_window", "AdhereR");
    return (NULL);
  }
  if( length(patients.to.keep) < length(patients) && !suppress.warnings ) .report.ewms("Some patients in the subsetting set are not in the CMA itsefl and are ignored!\n", "warning", "subsetCMA.CMA_sliding_window", "AdhereR");

  ret.val <- cma;
  ret.val$data <- ret.val$data[ ret.val$data[,ret.val$ID.colname] %in% patients.to.keep, ];
  if( !is.null(ret.val$event.info) )
  {
    if( inherits(ret.val$event.info, "data.frame") )
    {
      ret.val$event.info <- ret.val$event.info[ ret.val$event.info[,ret.val$ID.colname] %in% patients.to.keep, ]; if( nrow(ret.val$event.info) == 0 ) ret.val$event.info <- NULL;
    } else if( is.list(ret.val$event.info) && length(ret.val$event.info) > 0 )
    {
      ret.val$event.info <- lapply(ret.val$event.info, function(x){tmp <- x[ x[,ret.val$ID.colname] %in% patients.to.keep, ]; if(!is.null(tmp) && nrow(tmp) > 0){tmp}else{NULL}});
    }
  }
  if( ("CMA" %in% names(ret.val)) && !is.null(ret.val$CMA) )
  {
    if( inherits(ret.val$CMA, "data.frame") )
    {
      ret.val$CMA <- ret.val$CMA[ ret.val$CMA[,ret.val$ID.colname] %in% patients.to.keep, ];
    } else if( is.list(ret.val$CMA) && length(ret.val$CMA) > 0 )
    {
      ret.val$CMA <- lapply(ret.val$CMA, function(x){tmp <- x[ x[,ret.val$ID.colname] %in% patients.to.keep, ]; if(!is.null(tmp) && nrow(tmp) > 0){tmp}else{NULL}});
    }
  }
  return (ret.val);
}


#' @rdname print.CMA0
#' @export
print.CMA_sliding_window <- function(...) print.CMA_per_episode(...)

#' @rdname plot.CMA_per_episode
#' @export
plot.CMA_sliding_window <- plot.CMA_per_episode;



#' Interactive exploration and CMA computation.
#'
#' Interactively plot a given patient's data, allowing the real-time exploration
#' of the various CMAs and their parameters.
#' It can use \code{Rstudio}'s \code{manipulate} library or \code{Shiny}.
#'
#' This is merely a stub for the actual implementation in package
#' \code{AdhereRViz}: it just checks if this package is installed and functional,
#' in which case it calls the actual implementation, otherwise warns the user that
#' \code{AdhereRViz} must be instaled.
#'
#' @seealso Function \code{\link[AdhereR]{plot_interactive_cma}} in package
#' \code{AdhereRViz}.
#'
#' @param ... Parameters to be passed to \code{plot_interactive_cma()} in package
#' \code{AdhereRViz}.
#'
#' @return Nothing
#' @examples
#' \dontrun{
#' plot_interactive_cma(med.events,
#'                      ID.colname="PATIENT_ID",
#'                      event.date.colname="DATE",
#'                      event.duration.colname="DURATION",
#'                      event.daily.dose.colname="PERDAY",
#'                      medication.class.colname="CATEGORY");}
#' @export
plot_interactive_cma <- function(...)
{
  if( requireNamespace("AdhereRViz", quietly=TRUE) )
  {
    # Pass the parameters to AdhereRViz:
    AdhereRViz::plot_interactive_cma(...);
  } else {
    .report.ewms("Package 'AdhereRViz' must be installed for the interactive plotting to work! Please either install it or use the 'normal' plotting functions provided by 'AdhereR'...\n", "error", "plot_interactive_cma", "AdhereR");
    if( interactive() )
    {
      if( menu(c("Yes", "No"), graphics=FALSE, title="Do you want to install 'AdhereRViz' now?") == 1 )
      {
        # Try to install AdhereRViz:
        install.packages("AdhereRViz", dependencies=TRUE);
        if( requireNamespace("AdhereRViz", quietly=TRUE) )
        {
          # Pass the parameters to AdhereRViz:
          AdhereRViz::plot_interactive_cma(...);
        } else
        {
          .report.ewms("Failed to install 'AdhereRViz'!\n", "error", "plot_interactive_cma", "AdhereR");
          return (invisible(NULL));
        }
      } else
      {
        return (invisible(NULL));
      }
    }
  }
}



# # Create the medication groups example dataset med.groups from the drcomp dataset:
# # DON'T RUN!
# event_durations <- compute_event_durations(disp.data = durcomp.dispensing,
#                                            presc.data = durcomp.prescribing,
#                                            special.periods.data = durcomp.hospitalisation,
#                                            ID.colname = "ID",
#                                            presc.date.colname = "DATE.PRESC",
#                                            disp.date.colname = "DATE.DISP",
#                                            medication.class.colnames = c("ATC.CODE", "UNIT", "FORM"),
#                                            total.dose.colname = "TOTAL.DOSE",
#                                            presc.daily.dose.colname = "DAILY.DOSE",
#                                            presc.duration.colname = "PRESC.DURATION",
#                                            visit.colname = "VISIT",
#                                            split.on.dosage.change = TRUE,
#                                            force.init.presc = TRUE,
#                                            force.presc.renew = TRUE,
#                                            trt.interruption = "continue",
#                                            special.periods.method = "continue",
#                                            date.format = "%Y-%m-%d",
#                                            suppress.warnings = FALSE,
#                                            return.data.table = FALSE);
# med.events.ATC <- event_durations$event_durations[ !is.na(event_durations$event_durations$DURATION) & event_durations$event_durations$DURATION > 0,
#                                                    c("ID", "DISP.START", "DURATION", "DAILY.DOSE", "ATC.CODE")];
# names(med.events.ATC) <- c("PATIENT_ID", "DATE", "DURATION", "PERDAY", "CATEGORY");
# # Groups from the ATC codes:
# sort(unique(med.events.ATC$CATEGORY)); # all the ATC codes in the data
# # Level 1:
# med.events.ATC$CATEGORY_L1 <- vapply(substr(med.events.ATC$CATEGORY,1,1), switch, character(1),
#                                      "A"="ALIMENTARY TRACT AND METABOLISM",
#                                      "B"="BLOOD AND BLOOD FORMING ORGANS",
#                                      "J"="ANTIINFECTIVES FOR SYSTEMIC USE",
#                                      "R"="RESPIRATORY SYSTEM",
#                                      "OTHER");
# # Level 2:
# med.events.ATC$CATEGORY_L2 <- vapply(substr(med.events.ATC$CATEGORY,1,3), switch, character(1),
#                                      "A02"="DRUGS FOR ACID RELATED DISORDERS",
#                                      "A05"="BILE AND LIVER THERAPY",
#                                      "A09"="DIGESTIVES, INCL. ENZYMES",
#                                      "A10"="DRUGS USED IN DIABETES",
#                                      "A11"="VITAMINS",
#                                      "A12"="MINERAL SUPPLEMENTS",
#                                      "B02"="ANTIHEMORRHAGICS",
#                                      "J01"="ANTIBACTERIALS FOR SYSTEMIC USE",
#                                      "J02"="ANTIMYCOTICS FOR SYSTEMIC USE",
#                                      "R03"="DRUGS FOR OBSTRUCTIVE AIRWAY DISEASES",
#                                      "R05"="COUGH AND COLD PREPARATIONS",
#                                      "OTHER");
#
# # Define groups of medications:
# med.groups <- c("Vitamins"  = "(CATEGORY_L2 == 'VITAMINS')",
#                 "VitaResp"  = "({Vitamins} | CATEGORY_L1 == 'RESPIRATORY SYSTEM')",
#                 "VitaShort" = "({Vitamins} & DURATION <= 30)",
#                 "VitELow"   = "(CATEGORY == 'A11HA03' & PERDAY <= 500)",
#                 "VitaComb"  = "({VitaShort} | {VitELow})",
#                 "NotVita"   = "(!{Vitamins})");
# save(med.events.ATC, med.groups, file="./data/medgroups.rda", version=2); # save it backwards compatible with R >= 1.4.0

#' Example of medication events with ATC codes.
#'
#' An artificial dataset containing medication events (one per row) for 16
#' patients (1564 events in total), containing ATC codes. This dataset is
#' derived from the \code{durcomp} datasets using the \code{compute_event_durations}
#' function. See @med.events for more details.
#'
#' @format A data frame with 1564 rows and 7 variables:
#' \describe{
#'   \item{PATIENT_ID}{the patient unique identifier.}
#'   \item{DATE}{the medication event date.}
#'   \item{DURATION}{the duration in days.}
#'   \item{PERDAY}{the daily dosage.}
#'   \item{CATEGORY}{the ATC code.}
#'   \item{CATEGORY_L1}{explicitation of the first field of the ATC code (e.g.,
#'   "A"="ALIMENTARY TRACT AND METABOLISM").}
#'   \item{CATEGORY_L2}{explicitation of the first and second fields of the ATC
#'   code (e.g., "A02"="DRUGS FOR ACID RELATED DISORDERS").}
#' }
"med.events.ATC"

#' Example of medication groups.
#'
#' An example defining 6 medication groups for \code{med.events.ATC}.
#' It is a \emph{named character vector}, where the names are the medication
#' group unique \emph{names} (e.g., "Vitamines") and the elements are the medication
#' group \emph{definitions} (e.g., "(CATEGORY_L2 == 'VITAMINS')").
#' The definitions are \code{R} logical expressions using \emph{column names} and
#' \emph{values} that appear in the dataset, as well as references to other
#' medication groups using the construction \emph{"{NAME}"}.
#'
#' In the above example, "CATEGORY_L2" is a column name in the \code{med.events.ATC}
#' dataset, and 'VITAMINS' one of its possible values, and which selects all events
#' that have prescribed ATC codes "A11" (aka "VITAMINS").
#' Another example is "NotVita" defined as "(!{Vitamines})", which selects all
#' events that do not have Vitamines prescribed.
#'
#' For more details, please see the acompanying vignette.
"med.groups"


