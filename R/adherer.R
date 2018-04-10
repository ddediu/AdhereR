###############################################################################################
#
#    AdhereR: an R package for computing various estimates of medication adherence.
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

#' @import grDevices
#' @import graphics
#' @import stats
#' @import data.table
NULL

# Declare some variables as global to avoid NOTEs during package building:
globalVariables(c(".OBS.START.DATE", ".OBS.START.DATE.PRECOMPUTED", ".OBS.START.DATE.UPDATED", ".OBS.WITHIN.FU",
                  ".PROCESSING.CHUNK", ".START.BEFORE.END.WITHIN.OBS.WND", ".WND.ID", ".WND.START.DATE", "CMA",
                  ".CARRY.OVER.FROM.BEFORE", ".DATE.as.Date", ".END.EVENT.DATE", ".EVENT.STARTS.AFTER.OBS.WINDOW",
                  ".EVENT.STARTS.BEFORE.OBS.WINDOW", ".EVENT.WITHIN.FU.WINDOW", ".FU.START.DATE", ".FU.START.DATE.UPDATED",
                  ".INTERSECT.EPISODE.OBS.WIN.END", ".INTERSECT.EPISODE.OBS.WIN.START", ".OBS.DURATION.UPDATED",
                  ".OBS.END.DATE", ".OBS.END.DATE.PRECOMPUTED", "carry.only.for.same.medication", "chunk",
                  "consider.dosage.change", "end.episode.gap.days", "episode.ID", "episode.duration", "episode.end",
                  "episode.start", "followup.window.duration", "followup.window.start", "maximum.permissible.gap",
                  "maximum.permissible.gap.as.percent", "medication.change.means.new.treatment.episode",
                  "observation.window.duration", "observation.window.start", "patientID", "plot.CMA.as.histogram", "selectedCMA",
                  "show.legend", "sliding.window.duration", "sliding.window.start", "sliding.window.step.duration"));


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
#' actual dates (in which case the column must be of type \code{Date}); if a
#' \emph{number} it is the number of time units defined in the
#' \code{followup.window.start.unit} parameter after the begin of the
#' participant's first event; or \code{NA} if not defined.
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
#' @param date.format A \emph{string} giving the format of the dates used in
#' the \code{data} and the other parameters; see the \code{format} parameters
#' of the \code{\link[base]{as.Date}} function for details (NB, this concerns
#' only the dates given as strings and not as \code{Date} objects).
#' @param summary Metadata as a \emph{string}, briefly describing this CMA.
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any
#' warnings.
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
                 # Various types medhods of computing gaps:
                 carryover.within.obs.window=NA, # if TRUE consider the carry-over within the observation window (NA = undefined)
                 carryover.into.obs.window=NA, # if TRUE consider the carry-over from before the starting date of the observation window (NA = undefined)
                 carry.only.for.same.medication=NA, # if TRUE the carry-over applies only across medication of same type (NA = undefined)
                 consider.dosage.change=NA, # if TRUE carry-over is adjusted to reflect changes in dosage (NA = undefined)
                 # The follow-up window:
                 followup.window.start=NA, # if a number is the earliest event per participant date plus number of units, or a Date object, or a column name in data (NA = undefined)
                 followup.window.start.unit=NA, # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                 followup.window.duration=NA, # the duration of the follow-up window in the time units given below, or a column name in data (NA = undefined)
                 followup.window.duration.unit=NA, # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)  (NA = undefined)
                 # The observation window (embedded in the follow-up window):
                 observation.window.start=NA, # the number of time units relative to followup.window.start, or a column name in data (NA = undefined)
                 observation.window.start.unit=NA, # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                 observation.window.duration=NA, # the duration of the observation window in time units, or a column name in data (NA = undefined)
                 observation.window.duration.unit=NA, # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                 # Date format:
                 date.format=NA, # the format of the dates used in this function (NA = undefined)
                 # Comments and metadata:
                 summary="Base CMA object",
                 # Misc:
                 suppress.warnings=FALSE,
                 ...
)
{
  # Preconditions:
  if( !is.null(data) )
  {
    # data's class and dimensions:
    if( inherits(data, "matrix") ) data <- as.data.frame(data); # convert matrix to data.frame
    if( !inherits(data, "data.frame") )
    {
      if( !suppress.warnings ) warning("The 'data' for a CMA must be of type 'data.frame'!\n");
      return (NULL);
    }
    if( nrow(data) < 1 )
    {
      if( !suppress.warnings ) warning("The 'data' for a CMA must have at least one row!\n");
      return (NULL);
    }
    # the column names must exist in data:
    if( !is.na(ID.colname) && !(ID.colname %in% names(data)) )
    {
      if( !suppress.warnings ) warning(paste0("Column ID.colname='",ID.colname,"' must appear in the 'data'!\n"));
      return (NULL);
    }
    if( !is.na(event.date.colname) && !(event.date.colname %in% names(data)) )
    {
      if( !suppress.warnings ) warning(paste0("Column event.date.colname='",event.date.colname,"' must appear in the 'data'!\n"));
      return (NULL);
    }
    if( !is.na(event.duration.colname) && !(event.duration.colname %in% names(data)) )
    {
      if( !suppress.warnings ) warning(paste0("Column event.duration.colname='",event.duration.colname,"' must appear in the 'data'!\n"));
      return (NULL);
    }
    if( !is.na(event.daily.dose.colname) && !(event.daily.dose.colname %in% names(data)) )
    {
      if( !suppress.warnings ) warning(paste0("Column event.daily.dose.colname='",event.daily.dose.colname,"' must appear in the 'data'!\n"));
      return (NULL);
    }
    if( !is.na(medication.class.colname) && !(medication.class.colname %in% names(data)) )
    {
      if( !suppress.warnings ) warning(paste0("Column medication.class.colname='",medication.class.colname,"' must appear in the 'data'!\n"));
      return (NULL);
    }
    # carry-over conditions:
    if( !is.na(carryover.within.obs.window) && !is.logical(carryover.within.obs.window) )
    {
      if( !suppress.warnings ) warning(paste0("Parameter 'carryover.within.obs.window' must be logical!\n"));
      return (NULL);
    }
    if( !is.na(carryover.into.obs.window) && !is.logical(carryover.into.obs.window) )
    {
      if( !suppress.warnings ) warning(paste0("Parameter 'carryover.into.obs.window' must be logical!\n"));
      return (NULL);
    }
    if( !is.na(carry.only.for.same.medication) && !is.logical(carry.only.for.same.medication) )
    {
      if( !suppress.warnings ) warning(paste0("Parameter 'carry.only.for.same.medication' must be logical!\n"));
      return (NULL);
    }
    if( !is.na(consider.dosage.change) && !is.logical(consider.dosage.change) )
    {
      if( !suppress.warnings ) warning(paste0("Parameter 'consider.dosage.change' must be logical!\n"));
      return (NULL);
    }
    if( (!is.na(carryover.within.obs.window) && !is.na(carryover.into.obs.window) && !is.na(carry.only.for.same.medication)) &&
          (!carryover.within.obs.window && !carryover.into.obs.window && carry.only.for.same.medication) )
    {
      if( !suppress.warnings ) warning("Cannot carry over only for same medication when no carry over at all is considered!\n")
      return (NULL);
    }
    # the follow-up window:
    if( !is.na(followup.window.start) && is.numeric(followup.window.start) && followup.window.start < 0 )
    {
      if( !suppress.warnings ) warning("The follow-up window start must be a positive number of time units after the first event!\n")
      return (NULL);
    } else if( !is.na(followup.window.start) && !inherits(followup.window.start,"Date") && !is.numeric(followup.window.start) && !(followup.window.start %in% names(data)) )
    {
      if( !suppress.warnings ) warning("The follow-up window start must be either a positive number, a Date, or a valid column name in 'data'!\n")
      return (NULL);
    }
    if( !is.na(followup.window.start.unit) && !(followup.window.start.unit %in% c("days", "weeks", "months", "years") ) )
    {
      if( !suppress.warnings ) warning("The follow-up window start unit is not recognized!\n")
      return (NULL);
    }
    if( !is.na(followup.window.duration) && is.numeric(followup.window.duration) && followup.window.duration < 0 )
    {
      if( !suppress.warnings ) warning("The follow-up window duration must be a positive number of time units after the first event!\n")
      return (NULL);
    } else if( !is.na(followup.window.duration) && !is.numeric(followup.window.duration) && !(followup.window.duration %in% names(data)) )
    {
      if( !suppress.warnings ) warning("The follow-up window duration must be either a positive number or a valid column name in 'data'!\n")
      return (NULL);
    }
    if( !is.na(followup.window.duration.unit) && !(followup.window.duration.unit %in% c("days", "weeks", "months", "years") ) )
    {
      if( !suppress.warnings ) warning("The follow-up window duration unit is not recognized!\n")
      return (NULL);
    }
    # the observation window:
    if( !is.na(observation.window.start) && is.numeric(observation.window.start) && observation.window.start < 0 )
    {
      if( !suppress.warnings ) warning("The observation window start must be a positive number of time units after the first event!\n")
      return (NULL);
    } else if( !is.na(observation.window.start) && !inherits(observation.window.start,"Date") && !is.numeric(observation.window.start) && !(observation.window.start %in% names(data)) )
    {
      if( !suppress.warnings ) warning("The observation window start must be either a positive number, a Date, or a valid column name in 'data'!\n")
      return (NULL);
    }
    if( !is.na(observation.window.start.unit) && !(observation.window.start.unit %in% c("days", "weeks", "months", "years") ) )
    {
      if( !suppress.warnings ) warning("The observation window start unit is not recognized!\n")
      return (NULL);
    }
    if( !is.na(observation.window.duration) && is.numeric(observation.window.duration) && observation.window.duration < 0 )
    {
      if( !suppress.warnings ) warning("The observation window duration must be a positive number of time units after the first event!\n")
      return (NULL);
    } else if( !is.na(observation.window.duration) && !is.numeric(observation.window.duration) && !(observation.window.duration %in% names(data)) )
    {
      if( !suppress.warnings ) warning("The observation window duration must be either a positive number or a valid column name in 'data'!\n")
      return (NULL);
    }
    if( !is.na(observation.window.duration.unit) && !(observation.window.duration.unit %in% c("days", "weeks", "months", "years") ) )
    {
      if( !suppress.warnings ) warning("The observation window duration unit is not recognized!\n")
      return (NULL);
    }
  } else
  {
    return (NULL);
  }

  structure(list("data"=data,
                 "ID.colname"=ID.colname,
                 "event.date.colname"=event.date.colname,
                 "event.duration.colname"=event.duration.colname,
                 "event.daily.dose.colname"=event.daily.dose.colname,
                 "medication.class.colname"=medication.class.colname,
                 "carryover.within.obs.window"=carryover.within.obs.window,
                 "carryover.into.obs.window"=carryover.into.obs.window,
                 "carry.only.for.same.medication"=carry.only.for.same.medication,
                 "consider.dosage.change"=consider.dosage.change,
                 "followup.window.start"=followup.window.start,
                 "followup.window.start.unit"=followup.window.start.unit,
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
print.CMA0 <- function(x,                                     # the CMA0 (or derived) object
                       ...,                                   # required for S3 consistency
                       inline=FALSE,                          # print inside a line of text or as a separate, extended object?
                       format=c("text", "latex", "markdown"), # the format to print to
                       print.params=TRUE,                     # show the parameters?
                       print.data=TRUE,                       # show the summary of the data?
                       exclude.params=c("event.info"),        # if so, should I not print some?
                       skip.header=FALSE,                     # should I print the generic header?
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
            if( !is.null(cma[[p]]) && !is.na(cma[[p]]) )
            {
              if( p != "CMA" )
              {
                cat(paste0("    ",p," = ",cma[[p]],"\n"));
              } else
              {
                cat(paste0("    ",p," = CMA results for ",nrow(cma[[p]])," patients\n"));
              }
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
    warning("Unknown format for printing!\n");
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
#' @param show.period A \emph{string}, if "dates" show the actual dates at the
#' regular grid intervals, while for "days" (the default) shows the days since
#' the beginning; if \code{align.all.patients == TRUE}, \code{show.period} is
#' taken as "days".
#' @param period.in.days The \emph{number} of days at which the regular grid is
#' drawn.
#' @param show.legend \emph{Logical}, should the legend be drawn?
#' @param cex,cex.axis,cex.lab \emph{numeric} values specifying the cex of the
#' various types of text.
#' @param col.cats A \emph{color} or a \emph{function} that specifies the single
#' colour or the colour palette used to plot the different medication; by
#' default \code{\link[grDevices]{cm.colors}}.
#' @param lty.event,lwd.event,pch.start.event,pch.end.event The style of the
#' event (line style, width, and start and end symbols).
#' @param col.continuation,lty.continuation,lwd.continuation The style of the
#' "continuation" lines connecting consecutive events (colour, line style and
#' width).
#' @param col.na The colour used for missing event data.
#' @param bw.plot \emph{Logical}, should the plot use grayscale only (i.e., the
#' \code{\link[grDevices]{gray.colors}} function)?
#' @param print.CMA \emph{Logical}, should the CMA values be printed?
#' @param plot.CMA \emph{Logical}, should the CMA values be represented
#' graphically?
#' @param CMA.plot.ratio A \emph{number}, the proportion of the total horizontal
#' plot space to be allocated to the CMA plot.
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
                      align.all.patients=FALSE,              # should all patients be aligned?
                      show.period=c("dates","days")[2],      # draw vertical bars at regular interval as dates or days?
                      period.in.days=90,                     # the interval (in days) at which to draw veritcal lines
                      show.legend=TRUE,                      # show the legend?
                      cex=1.0, cex.axis=0.75, cex.lab=1.0,   # various graphical params
                      col.cats=cm.colors,         # single color or a function mapping the categories to colors
                      lty.event="solid", lwd.event=2, pch.start.event=15, pch.end.event=16, # event style
                      col.continuation="black", lty.continuation="dotted", lwd.continuation=1, # style of the contuniation lines connecting consecutive events
                      col.na="lightgray",                    # color for mising data
                      bw.plot=FALSE,                         # if TRUE, override all user-given colors and replace them with a scheme suitable for grayscale plotting
                      print.CMA=TRUE,                  # print CMA next to the participant's ID?
                      plot.CMA=TRUE,                   # plot the CMA next to the participant ID?
                      CMA.plot.ratio=0.10              # the proportion of the total horizontal plot to be taken by the CMA plot
)
{
  cma <- x; # parameter x is required for S3 consistency, but I like cma more
  if( is.null(cma) || !inherits(cma, "CMA0") || is.null(cma$data) || nrow(cma$data) < 1 ||
      is.na(cma$ID.colname) || !(cma$ID.colname %in% names(cma$data)) ||
      is.na(cma$event.date.colname) || !(cma$event.date.colname %in% names(cma$data)) ) return (invisible(NULL));

  # Check compatibility between subtypes of plots:
  if( align.all.patients && show.period != "days" ){ show.period <- "days"; warning("When aligning all patients, cannot show actual dates: showing days instead!\n"); }

  # Make sure the dates are strings in the right format:
  if( inherits(cma$data[,cma$event.date.colname], "Date") )
  {
    cma$date.format <- "%m/%d/%Y"; # use the default format
    cma$data[,cma$event.date.colname] <- as.character(cma$data[,cma$event.date.colname], format=cma$date.format);
  }

  # The patients:
  patids <- unique(as.character(cma$data[,cma$ID.colname])); patids <- patids[!is.na(patids)];
  if( !is.null(patients.to.plot) ) patids <- intersect(as.character(patids), as.character(patients.to.plot));
  if( length(patids) == 0 ) return (invisible(NULL));
  # Select only the patients to display:
  cma$data <- cma$data[ cma$data[,cma$ID.colname] %in% patids, ];
  # Make sure the patients are ordered by ID and date:
  cma$data <- cma$data[ order( cma$data[,cma$ID.colname], as.Date(cma$data[,cma$event.date.colname],format=cma$date.format)), ];

  # Grayscale plotting:
  if( bw.plot )
  {
      if( is.function(col.cats) ) col.cats <- .bw.colors else col.cats <- gray(0.1);
  }

  # The colors for the categories:
  if( is.na(cma$medication.class.colname) || !(cma$medication.class.colname %in% names(cma$data)) )
  {
    categories <- "unspec. type";
  } else
  {
    categories <- sort(unique(as.character(cma$data[,cma$medication.class.colname])), na.last=FALSE); # all categories making sure NA is first
  }
  if( is.na(categories[1]) )
  {
    if( is.function(col.cats) ) cols <- c(col.na, col.cats(length(categories)-1)) else cols <- c(col.na, rep(col.cats,length(categories)-1));
  } else
  {
    if( is.function(col.cats) ) cols <- col.cats(length(categories)) else cols <- rep(col.cats,length(categories));
  }
  names(cols) <- categories;
  .map.category.to.color <- function( category ) ifelse( is.na(category), cols[1], ifelse( category %in% names(cols), cols[category], "black") );

  # Find the earliest date:
  earliest.date <- min(as.Date(cma$data[,cma$event.date.colname],format=cma$date.format));

  # If aligning all participants to the same date, simply relocate all dates relative to the earliest date:
  if( align.all.patients )
  {
    for( i in 1:nrow(cma$data) )
    {
      if( i == 1 || cma$data[i,cma$ID.colname] != cma$data[i-1,cma$ID.colname] ) align.to <- as.Date(cma$data[i,cma$event.date.colname],format=cma$date.format);
      cma$data[i,cma$event.date.colname] <- as.character(earliest.date + (as.Date(cma$data[i,cma$event.date.colname], format=cma$date.format) - align.to), format=cma$date.format);
    }
  }

  # Compute the duration if not given:
  if( is.na(duration) )
  {
    latest.date <- max(as.Date(cma$data[,cma$event.date.colname],format=cma$date.format) + cma$data[,cma$event.duration.colname]);
    duration <- as.numeric(difftime(latest.date, earliest.date, "days"));
  }
  endperiod <- duration;

  # Reserve space for the CMA plotting:
  adh.plot.space <- c(0, ifelse( print.CMA && !is.null(getCMA(cma)), duration*CMA.plot.ratio, 0) );
  duration.total <- duration + adh.plot.space[2];

  # The actual plotting:
  plot( 0, 1, xlim=c(0,duration.total), ylim=c(0,nrow(cma$data)+1), type="n",
        main=ifelse(align.all.patients, "Event patterns (all patients aligned)", "Event patterns"),
        axes=FALSE, xlab=ifelse(show.period=="dates","","days"), ylab=ifelse(print.CMA && !is.null(getCMA(cma)),"patient (& CMA)","patient"), cex.lab=cex.lab ); box();
  curpat <- TRUE;
  for( i in 1:nrow(cma$data) )
  {
    start <- as.numeric(difftime(as.Date(cma$data[i,cma$event.date.colname],format=cma$date.format), earliest.date, "days" ) );
    end <- start + cma$data[i,cma$event.duration.colname];
    col <- .map.category.to.color(ifelse(is.na(cma$medication.class.colname) || !(cma$medication.class.colname %in% names(cma$data)),"unspec. type",cma$data[i,cma$medication.class.colname]));
    points( adh.plot.space[2]+start, i, pch=pch.start.event, col=col, cex=cex); points(adh.plot.space[2]+end, i, pch=pch.end.event, col=col, cex=cex);
    segments( adh.plot.space[2]+start, i, adh.plot.space[2]+end, i, col=col, lty=lty.event, lwd=lwd.event);

    if( i < nrow(cma$data) )
    {
      if( cma$data[i,cma$ID.colname] == cma$data[i+1,cma$ID.colname] )
      {
        start.next <- as.numeric(difftime(as.Date(cma$data[i+1,cma$event.date.colname],format=cma$date.format), earliest.date, "days"));
        segments( adh.plot.space[2]+end, i, adh.plot.space[2]+start.next, i, col=col.continuation, lty=lty.continuation, lwd=lwd.continuation);
        segments( adh.plot.space[2]+start.next, i, adh.plot.space[2]+start.next, i+1, col=col.continuation, lty=lty.continuation, lwd=lwd.continuation);
      } else
      {
        # Now the patient is changing:
        if( curpat )
        {
          y <- which(cma$data[,cma$ID.colname] == cma$data[i+1,cma$ID.colname]);
          rect( 0-1, min(y)-0.5, duration.total+1, max(y)+0.5, col=gray(0.9), border=NA );
        }
        curpat <- !curpat;
      }
    }
  }
  # The grid at regular dates:
  abline( v=adh.plot.space[2]+seq(0,endperiod,by=period.in.days), lty="dotted", col=gray(0.5) );
  abline( v=adh.plot.space[2]+endperiod, lty="solid", col=gray(0.5) );

  # The patient axis and CMA plots:
  if( print.CMA && !is.null(getCMA(cma)) )
  {
    # Maximum achieved CMA:
    adh.max <- max(getCMA(cma)$CMA,na.rm=TRUE);
    # Function mapping the CMA values to the appropriate x-coordinates:
    .rescale.xcoord.for.CMA.plot <- function(x,pfree=0.20) return (adh.plot.space[1] + x/adh.max*(adh.plot.space[2]*(1-pfree) - adh.plot.space[1]));
    # Mark the drawing area:
    if( adh.max > 1.0 )
    {
      rect(.rescale.xcoord.for.CMA.plot(0), par("usr")[3], .rescale.xcoord.for.CMA.plot(adh.max), par("usr")[4], col=rgb(1.0,1.0,0.0,0.25), border=NA);
      abline(v=c(.rescale.xcoord.for.CMA.plot(0), .rescale.xcoord.for.CMA.plot(1.0), .rescale.xcoord.for.CMA.plot(adh.max)), col="blue", lty=c("solid","dotted","solid"), lwd=1);
      mtext( c("0%","100%",sprintf("%.1f%%",adh.max*100)), 3, line=0.5, at=c(.rescale.xcoord.for.CMA.plot(0), .rescale.xcoord.for.CMA.plot(1.0), .rescale.xcoord.for.CMA.plot(adh.max)), las=2, cex=cex.axis, col="blue" );
    } else
    {
      rect(.rescale.xcoord.for.CMA.plot(0), par("usr")[3], .rescale.xcoord.for.CMA.plot(1.0), par("usr")[4], col=rgb(1.0,1.0,0.0,0.25), border=NA);
      abline(v=c(.rescale.xcoord.for.CMA.plot(0), .rescale.xcoord.for.CMA.plot(1.0)), col="blue", lty="solid", lwd=1);
      mtext( c("0%","100%"), 3, line=0.5, at=c(.rescale.xcoord.for.CMA.plot(0), .rescale.xcoord.for.CMA.plot(1.0)), las=2, cex=cex.axis, col="blue" );
    }
  }
  for( p in as.character(patids) )
  {
    # The participant axis text:
    s <- which(cma$data[,cma$ID.colname] == p);
    pid <- ifelse( print.CMA && !is.null(getCMA(cma)) && length(x<-which(getCMA(cma)[cma$ID.colname] == p))==1, paste0(p,"\n",sprintf("%.1f%%",getCMA(cma)[x,"CMA"]*100)), p);
    mtext( pid, 2, line=0.5, at=mean(s), las=2, cex=cex.axis );

    # The participant CMA plot:
    if( print.CMA && !is.null(getCMA(cma)) )
    {
      adh <- getCMA(cma)[x,"CMA"];
      rect(.rescale.xcoord.for.CMA.plot(0), mean(s)-1, .rescale.xcoord.for.CMA.plot(min(adh,adh.max)), mean(s)+1, col="lightblue", border=NA);
      rect(.rescale.xcoord.for.CMA.plot(0), mean(s)-1, .rescale.xcoord.for.CMA.plot(max(1.0,adh.max)), mean(s)+1, col=NA, border="blue");
      text(x=(.rescale.xcoord.for.CMA.plot(0) + .rescale.xcoord.for.CMA.plot(max(1.0,adh.max)))/2, y=mean(s), labels=sprintf("%.1f%%",adh*100), col="darkblue", cex=cex.axis);
    }
  }

  # The days/dates axis
  axis( 1, at=adh.plot.space[2]+seq(0,endperiod,by=period.in.days),
        labels=if(show.period=="dates"){as.character(earliest.date + round(seq(0,endperiod,by=period.in.days),1), format=cma$date.format)} else {as.character(round(seq(0,endperiod,by=period.in.days),1))},
        las=3, cex.axis=cex.axis);

  # The legend:
  if( show.legend )
  {
    legend("bottomright", bty="o", bg=rgb(0.9,0.9,0.9,0.6),
           legend=c("event","no event",vapply(names(cols), function(s) ifelse(is.na(s),"<missing>",s), character(1))),
           col=c("black","black",cols),
           lty=c("solid","dotted",rep("solid",length(cols))),
           lwd=c(2,1,rep(2,length(cols))));
  }
}

#' Access the actual CMA estimate from a CMA object.
#'
#' Retreive the actual CMA estimate(s) encapsulated in a simple, per episode,
#' or sliding window CMA object.
#'
#' @param x a CMA object.
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
getCMA <- function(x) UseMethod("getCMA")
#' @export
getCMA.CMA0 <- function(x)
{
  cma <- x; # parameter x is required for S3 consistency, but I like cma more
  if( is.null(cma) || !inherits(cma, "CMA0") || !("CMA" %in% names(cma)) || is.null(cma$CMA) ) return (NULL);
  return (cma$CMA);
}


# Auxiliary function: add time units to date:
.add.time.interval.to.date <- function( start.date, # a Date object
                                        time.interval=0, # the number of "units" to add to the start.date (must be positive and is rounded)
                                        unit="days", # can be "days", "weeks", "months", "years"
                                        suppress.warnings=FALSE
)
{
  # Checks
  if( !inherits(start.date,"Date") )
  {
    if( !suppress.warnings ) warning("start.date to '.add.time.interval.to.date' must be a Date() object.\n");
    return (NA);
  }
  if( !is.numeric(time.interval) || time.interval < 0 )
  {
    if( !suppress.warnings ) warning("time.interval to '.add.time.interval.to.date' must be a positive integer.\n");
    return (NA);
  }
  time.interval <- round(time.interval);

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
  #                 {if( !suppress.warnings ) warning(paste0("Unknown unit '",unit,"' to '.add.time.interval.to.date'.\n")); NA;} # default
  # ));

  # Faster but assumes that the internal representation of "Date" object is in number of days since the begining of time (probably stably true):
  return (switch( as.character(unit),
                            "days"  = structure(unclass(start.date) + time.interval, class="Date"),
                            "weeks" = structure(unclass(start.date) + time.interval*7, class="Date"),
                            "months" = lubridate::add_with_rollback(start.date, lubridate::period(time.interval,"months"), roll_to_first=TRUE), # take care of cases such as 2001/01/29 + 1 month
                            "years"  = lubridate::add_with_rollback(start.date, lubridate::period(time.interval,"years"),  roll_to_first=TRUE), # take care of cases such as 2000/02/29 + 1 year
                            {if( !suppress.warnings ) warning(paste0("Unknown unit '",unit,"' to '.add.time.interval.to.date'.\n")); NA;} # default
  ));
}

# Auxiliary function: subtract two dates to obtain the number of days in between:
# WARNING! Faster than difftime() but makes the assumption that the internal representation of Date objects is the number of days since a given begining of time
# (true as of R 3.4 and probably conserved in the future versions)
.difftime.Dates.as.days <- function( start.dates, end.dates, suppress.warnings=FALSE )
{
  # Checks
  if( !inherits(start.dates,"Date") || !inherits(end.dates,"Date") )
  {
    if( !suppress.warnings ) warning("start.dates and end.dates to '.difftime.Dates.as.days' must be a Date() objects.\n");
    return (NA);
  }
  return (unclass(start.dates) - unclass(end.dates)); # the difference between the raw internal representations of Date objects is in days
}

# Auxiliary function generating colors for bw plotting:
.bw.colors <- function(n) gray.colors(n, start=0, end=0.5);


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
      if( !suppress.warnings ) warning(paste0("Parallel processing backend \"multicore\" is not currently supported on Windows: will use SNOW instead.\n"));
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
        if( !suppress.warnings ) warning(paste0("Number of parallel processing threads \"",parallel.threads,"\" must be either \"auto\" or a positive integer; forcing \"auto\".\n"));
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
    if( !suppress.warnings ) warning(paste0("Unknown parallel processing backend \"",parallel.backend,"\": will force sequential (\"none\").\n"));
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
      if( !suppress.warnings ) warning(paste0("Number of parallel processing threads \"",parallel.threads,"\", if numeric, must be either a positive integer; forcing \"auto\".\n"));
      parallel.threads <- 2L;
    }
    if( is.numeric(parallel.threads) ) parallel.threads <- min(parallel.threads, length(patids)); # make sure we're not starting more threads than patients
  }

  # Attempt to create the SNOW cluster:
  cluster <- snow::makeCluster(parallel.threads, # process only "auto", otherwise trust makeCluster() to interpret the parameters
                               type = cluster.type);
  if( is.null(cluster) )
  {
    if( !suppress.warnings ) warning(paste0("Failed to create the cluster \"",parallel.backend,"\" with parallel.threads \"",parallel.threads,"\": will force sequential (\"none\").\n"));
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
  tmp <- snow::parLapply(cluster,
                         snow::clusterSplit(cluster, patids),
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

  snow::stopCluster(cluster); # stop the cluster

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
                                   # Various types medhods of computing gaps:
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
    if( !suppress.warnings ) warning("Event data must be a non-empty data frame!\n")
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
    if( !suppress.warnings ) warning("The patient ID column \"",ID.colname,"\" cannot be empty, must be a single value, and must be present in the event data!\n")
    return (NULL);
  }
  if( is.null(event.date.colname) || is.na(event.date.colname) ||                                                   # avoid empty stuff
      !(is.character(event.date.colname) ||                                                                         # it must be a character...
        (is.factor(event.date.colname) && is.character(event.date.colname <- as.character(event.date.colname)))) || # ...or a factor (forced to character)
      length(event.date.colname) != 1 ||                                                                            # make sure it's a single value
      !(event.date.colname %in% data.names)                                                                         # make sure it's a valid column name
      )
  {
    if( !suppress.warnings ) warning("The event date column \"",event.date.colname,"\" cannot be empty, must be a single value, and must be present in the event data!\n")
    return (NULL);
  }
  if( is.null(event.duration.colname) || is.na(event.duration.colname) ||                                                       # avoid empty stuff
      !(is.character(event.duration.colname) ||                                                                                 # it must be a character...
        (is.factor(event.duration.colname) && is.character(event.duration.colname <- as.character(event.duration.colname)))) || # ...or a factor (forced to character)
      length(event.duration.colname) != 1 ||                                                                                    # make sure it's a single value
      !(event.duration.colname %in% data.names)                                                                                 # make sure it's a valid column name
      )
  {
    if( !suppress.warnings ) warning("The event duration column \"",event.duration.colname,"\" cannot be empty, must be a single value, and must be present in the event data!\n")
    return (NULL);
  }
  if( (!is.null(event.daily.dose.colname) && !is.na(event.daily.dose.colname)) &&                                                      # if actually given:
      (!(is.character(event.daily.dose.colname) ||                                                                                     # it must be a character...
         (is.factor(event.daily.dose.colname) && is.character(event.daily.dose.colname <- as.character(event.daily.dose.colname)))) || # ...or a factor (forced to character)
       length(event.daily.dose.colname) != 1 ||                                                                                        # make sure it's a single value
       !(event.daily.dose.colname %in% data.names))                                                                                    # make sure it's a valid column name
      )
  {
    if( !suppress.warnings ) warning("If given, the event daily dose column \"",event.daily.dose.colname,"\" must be a single value and must be present in the event data!\n")
    return (NULL);
  }
  if( (!is.null(medication.class.colname) && !is.na(medication.class.colname)) &&                                                      # if actually given:
      (!(is.character(medication.class.colname) ||                                                                                     # it must be a character...
         (is.factor(medication.class.colname) && is.character(medication.class.colname <- as.character(medication.class.colname)))) || # ...or a factor (forced to character)
       length(medication.class.colname) != 1 ||                                                                                        # make sure it's a single value
       !(medication.class.colname %in% data.names))                                                                                    # make sure it's a valid column name
      )
  {
    if( !suppress.warnings ) warning("If given, the event type column \"",medication.class.colname,"\" must be a single value and must be present in the event data!\n")
    return (NULL);
  }

  # preconditions concerning carry-over:
  if( !is.logical(carryover.within.obs.window)    || is.na(carryover.within.obs.window)    || length(carryover.within.obs.window) != 1    ||
      !is.logical(carryover.into.obs.window)      || is.na(carryover.into.obs.window)      || length(carryover.into.obs.window) != 1      ||
      !is.logical(carry.only.for.same.medication) || is.na(carry.only.for.same.medication) || length(carry.only.for.same.medication) != 1 )
  {
    if( !suppress.warnings ) warning("Carry over arguments must be single value logicals!\n")
    return (NULL);
  }
  if( !carryover.within.obs.window && !carryover.into.obs.window && carry.only.for.same.medication )
  {
    if( !suppress.warnings ) warning("Cannot carry over only for same medication when no carry over at all is considered!\n")
    return (NULL);
  }

  # preconditions concerning dosage change:
  if( !is.logical(consider.dosage.change) || is.na(consider.dosage.change) || length(consider.dosage.change) != 1 )
  {
    if( !suppress.warnings ) warning("Consider dosage change must be single value logical!\n")
    return (NULL);
  }

  # preconditions concerning follow-up window (as all violations result in the same error, aggregate them in a single if):
  if( (is.null(followup.window.start) || is.na(followup.window.start) || length(followup.window.start) != 1) ||                   # cannot be missing or have more than one values
      (is.numeric(followup.window.start) && (followup.window.start < 0)) ||                                                       # if a number, must be a single positive one
      (!inherits(followup.window.start,"Date") && !is.numeric(followup.window.start) &&                                           # not a Date or number:
          (!(is.character(followup.window.start) ||                                                                               # it must be a character...
             (is.factor(followup.window.start) && is.character(followup.window.start <- as.character(followup.window.start)))) || # ...or a factor (forced to character)
           !(followup.window.start %in% data.names))) )                                                                           # make sure it's a valid column name
  {
    if( !suppress.warnings ) warning("The follow-up window start must be a single value, either a positive number, a Date object, or a string giving a column name in the data!\n")
    return (NULL);
  }
  if( is.null(followup.window.start.unit) || is.na(followup.window.start.unit) ||
      length(followup.window.start.unit) != 1 ||
      !(followup.window.start.unit %in% c("days", "weeks", "months", "years") ) )
  {
    if( !suppress.warnings ) warning("The follow-up window start unit must be a single value, one of \"days\", \"weeks\", \"months\" or \"years\"!\n")
    return (NULL);
  }
  if( is.numeric(followup.window.duration) && (followup.window.duration <= 0 || length(followup.window.duration) != 1) ||               # cannot be missing or have more than one values
      (!is.numeric(followup.window.duration) &&
       (!(is.character(followup.window.duration) ||                                                                                     # it must be a character...
          (is.factor(followup.window.duration) && is.character(followup.window.duration <- as.character(followup.window.duration)))) || # ...or a factor (forced to character)
        !(followup.window.duration %in% data.names))))                                                                                  # make sure it's a valid column name
  {
    if( !suppress.warnings ) warning("The follow-up window duration must be a single value, either a positive number, or a string giving a column name in the data!\n")
    return (NULL);
  }
  if( is.null(followup.window.duration.unit) || is.na(followup.window.duration.unit) ||
      length(followup.window.duration.unit) != 1 ||
      !(followup.window.duration.unit %in% c("days", "weeks", "months", "years") ) )
  {
    if( !suppress.warnings ) warning("The follow-up window duration unit must be a single value, one of \"days\", \"weeks\", \"months\" or \"years\"!\n")
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
    if( !suppress.warnings ) warning("The observation window start must be a single value, either a positive number, a Date object, or a string giving a column name in the data!\n")
    return (NULL);
  }
  if( is.null(observation.window.start.unit) || is.na(observation.window.start.unit) ||
      length(observation.window.start.unit) != 1 ||
      !(observation.window.start.unit %in% c("days", "weeks", "months", "years") ) )
  {
    if( !suppress.warnings ) warning("The observation window start unit must be a single value, one of \"days\", \"weeks\", \"months\" or \"years\"!\n")
    return (NULL);
  }
  if( is.numeric(observation.window.duration) && (observation.window.duration <= 0 || length(observation.window.duration) != 1) ||               # cannot be missing or have more than one values
      (!is.numeric(observation.window.duration) &&
       (!(is.character(observation.window.duration) ||                                                                                           # it must be a character...
          (is.factor(observation.window.duration) && is.character(observation.window.duration <- as.character(observation.window.duration)))) || # ...or a factor (forced to character)
        !(observation.window.duration %in% data.names))))                                                                                        # make sure it's a valid column name
  {
    if( !suppress.warnings ) warning("The observation window duration must be a single value, either a positive number, or a string giving a column name in the data!\n")
    return (NULL);
  }
  if( is.null(observation.window.duration.unit) || is.na(observation.window.duration.unit) ||
      length(observation.window.duration.unit) != 1 ||
      !(observation.window.duration.unit %in% c("days", "weeks", "months", "years") ) )
  {
    if( !suppress.warnings ) warning("The observation window duration unit must be a single value, one of \"days\", \"weeks\", \"months\" or \"years\"!\n")
    return (NULL);
  }

  # Check the patient IDs:
  if( anyNA(data[,ID.colname]) )
  {
    if( !suppress.warnings ) warning(paste0("The patient unique identifiers in the \"",ID.colname,"\" column must not contain NAs; the first occurs on row ",min(which(is.na(data[,ID.colname]))),"!\n"));
    return (NULL);
  }

  # Check the date format (and save the conversion to Date() for later use):
  if( is.na(date.format) || is.null(date.format) || length(date.format) != 1 || !is.character(date.format) )
  {
    if( !suppress.warnings ) warning(paste0("The date format must be a single string!\n"));
    return (NULL);
  }
  if( anyNA(Date.converted.to.DATE <- as.Date(data[,event.date.colname],format=date.format)) )
  {
    if( !suppress.warnings ) warning(paste0("Not all entries in the event date \"",event.date.colname,"\" column are valid dates or conform to the date format \"",date.format,"\"; first issue occurs on row ",min(which(is.na(Date.converted.to.DATE))),"!\n"));
    return (NULL);
  }

  # Check the duration:
  tmp <- data[,event.duration.colname]; # caching for speed
  if( !is.numeric(tmp) || any(is.na(tmp) | tmp <= 0) )
  {
    if( !suppress.warnings ) warning(paste0("The event durations in the \"",event.duration.colname,"\" column must be non-missing strictly positive numbers!\n"));
    return (NULL);
  }

  # Check the event daily dose:
  if( !is.na(event.daily.dose.colname) && !is.null(event.daily.dose.colname) &&             # if actually given:
      (!is.numeric(tmp <- data[,event.daily.dose.colname]) || any(is.na(tmp) | tmp <= 0)) ) # must be a non-missing strictly positive number (and cache it for speed)
  {
    if( !suppress.warnings ) warning(paste0("If given, the event daily dose in the \"",event.daily.dose.colname,"\" column must be a non-missing strictly positive numbers!\n"));
    return (NULL);
  }

  # Check the newly created columns:
  if( is.na(event.interval.colname) || is.null(event.interval.colname) || !is.character(event.interval.colname) || (event.interval.colname %in% data.names) )
  {
    if( !suppress.warnings ) warning(paste0("The column name where the event interval will be stored \"",event.interval.colname,"\" cannot be missing nor already present in the event data!\n"));
    return (NULL);
  }
  if( is.na(gap.days.colname) || is.null(gap.days.colname) || !is.character(gap.days.colname) || (gap.days.colname %in% data.names) )
  {
    if( !suppress.warnings ) warning(paste0("The column name where the gap days will be stored \"",gap.days.colname,"\" cannot be mising nor already present in the event data.\n"));
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
    # Auxliary internal function: For a given patient, compute the gaps and return the required columns:
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
      data4ID.selected.columns <- data4ID[,..columns.to.cache];
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
    if( !suppress.warnings ) warning("Computing event intervals and gap days failed!\n");
    return (NULL);
  }
  if( any(!ret.val$.OBS.WITHIN.FU) )
  {
    if( !suppress.warnings ) warning("The observation window is not within the follow-up window for participant(s) ",paste0(unique(ret.val[!ret.val$.OBS.WITHIN.FU,get(ID.colname)]),collpase=", ")," !\n");
  }

  # Make sure events outside the observation window are NA'ed:
  if( !keep.event.interval.for.all.events ) ret.val[ .OBS.WITHIN.FU & (.EVENT.STARTS.BEFORE.OBS.WINDOW | .EVENT.STARTS.AFTER.OBS.WINDOW), c(event.interval.colname) := NA_real_ ];

  # Remove the events that fall outside the follow-up window:
  if( remove.events.outside.followup.window ) ret.val <- ret.val[ which(.OBS.WITHIN.FU & .EVENT.WITHIN.FU.WINDOW), ];
  # If the results are empty return NULL:
  if( is.null(ret.val) || nrow(ret.val) < 1 )
  {
    if( !suppress.warnings ) warning("No observations fall within the follow-up and observation windows!\n");
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
    setkeyv(ret.val, c(ID.colname, ".DATE.as.Date")); # make sure it is keyed by patient ID and event date
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
                                        # Various types medhods of computing gaps:
                                        carryover.within.obs.window=TRUE, # if TRUE consider the carry-over within the observation window
                                        carry.only.for.same.medication=TRUE, # if TRUE the carry-over applies only across medication of same type
                                        consider.dosage.change=TRUE, # if TRUE carry-over is adjusted to reflect changes in dosage
                                        # Treatment episodes:
                                        medication.change.means.new.treatment.episode=TRUE, # does a change in medication automatically start a new treatment episode?
                                        maximum.permissible.gap=90, # if a number, is the duration in units of max. permissible gaps between treatment episodes
                                        maximum.permissible.gap.unit=c("days", "weeks", "months", "years", "percent")[1], # time units; can be "days", "weeks" (fixed at 7 days), "months" (fixed at 30 days), "years" (fixed at 365 days), or "percent", in which case maximum.permissible.gap is interpreted as a percent (can be > 100%) of the duration of the current prescription
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
                                        return.data.table=FALSE
)
{
  # Convert maximum permissible gap units into days or proprtion:
  maximum.permissible.gap.as.percent <- FALSE; # is the maximum permissible gap a percent of the current duration?
  maximum.permissible.gap <- switch(maximum.permissible.gap.unit,
                                    "days" = maximum.permissible.gap,
                                    "weeks" = maximum.permissible.gap * 7,
                                    "months" = maximum.permissible.gap * 30,
                                    "years" = maximum.permissible.gap * 365,
                                    "percent" = {maximum.permissible.gap.as.percent <- TRUE; maximum.permissible.gap / 100;}, # transform it into a proportion for faster computation
                                    {if(!suppress.warnings) warning(paste0("Unknown maximum.permissible.gap.unit '",maximum.permissible.gap.unit,"': assuming you meant 'days'."));  maximum.permissible.gap;} # otherwise force it to "days"
                                   );
  if( maximum.permissible.gap < 0 ) maximum.permissible.gap <- 0; # make sure this is positive

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
    # Auxliary internal function: Compute the CMA for a given patient:
    .process.patient <- function(data4ID)
    {
      # Cache things up:
      n.events                <- nrow(data4ID);
      last.event              <- max(which(data4ID$.EVENT.WITHIN.FU.WINDOW), na.rm=TRUE); # the last event in the follow-up window
      event.duration.column   <- data4ID[,get(event.duration.colname)];
      gap.days.column         <- data4ID[,get(gap.days.colname)];
      medication.class.column <- data4ID[,get(medication.class.colname)];
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
      s <- which(s); s.len <- length(s);

      if( n.events == 1 || s.len == 0 || (s.len==1 && s==n.events) )
      {
        # One single treatment episode starting with the first event within the follow-up window and the end of the follow-up window:
        treatment.episodes <- data.table("episode.ID"=as.numeric(1),
                                         "episode.start"=data4ID$.DATE.as.Date[1],
                                         "end.episode.gap.days"=gap.days.column[last.event]);
        n.episodes <- nrow(treatment.episodes);
        treatment.episodes[, episode.duration := as.numeric(data4ID$.FU.END.DATE[1] - episode.start[n.episodes]) -
          ifelse(end.episode.gap.days[n.episodes] < MAX.PERMISSIBLE.GAP[last.event], # duration of the last event of the last episode
                 0,
                 end.episode.gap.days[n.episodes])]; # the last episode duration is the end date of the follow-up window minus the start date of the last episode minus the gap after the last episode only if the gap is longer than the maximum persmissible gap
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
        } else
        {
          # The last event with gap > maximum permissible is the last event for this patient:
          treatment.episodes <- data.table("episode.ID"=as.numeric(1:s.len),
                                           "episode.start"=c(data4ID$.DATE.as.Date[1],                # the 1st event in the follow-up window
                                                             data4ID$.DATE.as.Date[s[-s.len]+1]), # the next event
                                           "end.episode.gap.days"=c(gap.days.column[s]));             # the corresponding gap.days of the last event in this follow-up window
        }
        n.episodes <- nrow(treatment.episodes);
        treatment.episodes[, episode.duration := c(as.numeric(episode.start[2:n.episodes] - episode.start[1:(n.episodes-1)]) - end.episode.gap.days[1:(n.episodes-1)], # the episode duration is the start date of the next episode minus the start date of the current episode minus the gap after the current episode
                                                   as.numeric(data4ID$.FU.END.DATE[1] - episode.start[n.episodes]) -
                                                     ifelse(end.episode.gap.days[n.episodes] < MAX.PERMISSIBLE.GAP[last.event], # duration of the last event of the last episode
                                                            0,
                                                            end.episode.gap.days[n.episodes]))]; # the last episode duration is the episode duration is the end date of the follow-up window minus the start date of the last episode minus the gap after the last episode only if the gap is longer than the maximum persmissible gap
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
                           show.legend=TRUE, legend.x="right", legend.y="bottom", legend.bkg.opacity=0.5, # legend params and position
                           cex=1.0, cex.axis=0.75, cex.lab=1.0,   # various graphical params
                           show.cma=TRUE,                         # show the CMA type
                           col.cats=cm.colors,         # single color or a function mapping the categories to colors
                           unspecified.category.label="drug",     # the label of the unspecified category of medication
                           lty.event="solid", lwd.event=2, pch.start.event=15, pch.end.event=16, # event style
                           show.event.intervals=TRUE,             # show the actual rpescription intervals
                           col.na="lightgray",                    # color for mising data
                           #col.continuation="black", lty.continuation="dotted", lwd.continuation=1, # style of the contuniation lines connecting consecutive events
                           print.CMA=TRUE,                  # print CMA next to the participant's ID?
                           plot.CMA=TRUE,                   # plot the CMA next to the participant ID?
                           CMA.plot.ratio=0.10,             # the proportion of the total horizontal plot to be taken by the CMA plot
                           CMA.plot.col="lightgreen", CMA.plot.border="darkgreen", CMA.plot.bkg="aquamarine", CMA.plot.text=CMA.plot.border, # attributes of the CMA plot
                           highlight.followup.window=TRUE, followup.window.col="green",
                           highlight.observation.window=TRUE, observation.window.col="yellow", observation.window.density=35, observation.window.angle=-30,
                           show.real.obs.window.start=TRUE, real.obs.window.density=35, real.obs.window.angle=30, # for some CMAs, the real observation window starts at a different date
                           bw.plot=FALSE,                         # if TRUE, override all user-given colors and replace them with a scheme suitable for grayscale plotting
                           min.plot.size.in.characters.horiz=20, min.plot.size.in.characters.vert=15,  # the minimum plot size (in character)
                           max.patients.to.plot=100,        # maximum number of patients to plot
                           ...
)
{
  if( is.null(cma) || !inherits(cma, "CMA1") || is.null(cma$data) || nrow(cma$data) < 1 ||
      is.na(cma$ID.colname) || !(cma$ID.colname %in% names(cma$data)) ||
      is.na(cma$event.date.colname) || !(cma$event.date.colname %in% names(cma$data)) ||
      !("event.info" %in% names(cma)) || is.null(cma$event.info) ) return (plot.CMA0(cma,...));

  # Check compatibility between subtypes of plots:
  if( align.all.patients && show.period != "days" ){ show.period <- "days"; warning("When aligning all patients, cannot show actual dates: showing days instead!\n"); }

  # Make sure the dates are strings of the right fomat:
  if( inherits(cma$data[,cma$event.date.colname], "Date") )
  {
    cma$date.format <- "%m/%d/%Y"; # use the default format
    cma$data[,cma$event.date.colname] <- as.character(cma$data[,cma$event.date.colname], format=cma$date.format);
  }

  # The patients (use event.info as it contains all the info required, plus the specifics of the CMA):
  patids <- unique(cma$event.info[,cma$ID.colname]); patids <- patids[!is.na(patids)];
  if( !is.null(patients.to.plot) ) patids <- intersect(as.character(patids), as.character(patients.to.plot));
  if( length(patids) == 0 )
  {
    cat("No patients to plot!\n");
    return (invisible(NULL));
  } else if( length(patids) > max.patients.to.plot )
  {
    cat(paste0("Too many patients to plot (",length(patids),
               ")! If you really want that, please change the 'max.patients.to.plot' parameter value (now set at ",
               max.patients.to.plot,"!\n"));
    return (invisible(NULL));
  }
  # Select only the patients to display:
  cma$data <- cma$data[ cma$data[,cma$ID.colname] %in% patids, ];
  cma$event.info <- cma$event.info[ cma$event.info[,cma$ID.colname] %in% patids, ];
  # Make sure the patients are ordered by ID and date:
  cma$event.info <- cma$event.info[ order( cma$event.info[,cma$ID.colname], as.Date(cma$event.info[,cma$event.date.colname],format=cma$date.format)), ];

  # Grayscale plotting:
  if( bw.plot )
  {
      if( is.function(col.cats) ) col.cats <- .bw.colors else col.cats <- gray(0.1);
      followup.window.col <- "black";
      observation.window.col <- gray(0.3);
      CMA.plot.col <- gray(0.8);
      CMA.plot.border <- gray(0.2);
      CMA.plot.bkg <- gray(0.5);
      CMA.plot.text <- CMA.plot.border;
  }

  # The colors for the categories:
  if( is.na(cma$medication.class.colname) || !(cma$medication.class.colname %in% names(cma$event.info)) )
  {
    categories <- unspecified.category.label;
  } else
  {
    categories <- sort(unique(as.character(cma$event.info[,cma$medication.class.colname])), na.last=FALSE); # all categories making sure NA is first
  }
  if( is.na(categories[1]) )
  {
    if( is.function(col.cats) ) cols <- c(col.na, col.cats(length(categories)-1)) else cols <- c(col.na, rep(col.cats,length(categories)-1));
  } else
  {
    if( is.function(col.cats) ) cols <- col.cats(length(categories)) else cols <- rep(col.cats,length(categories));
  }
  names(cols) <- categories;
  .map.category.to.color <- function( category ) ifelse( is.na(category), cols[1], ifelse( category %in% names(cols), cols[category], "black") );

  # Find the earliest date:
  earliest.date <- min(cma$event.info$.DATE.as.Date, cma$event.info$.OBS.START.DATE, cma$event.info$.FU.START.DATE);

  # If aligning all participants to the same date, simply relocate all dates relative to the earliest date:
  if( align.all.patients )
  {
    for( i in 1:nrow(cma$event.info) )
    {
      if( i == 1 || cma$event.info[i,cma$ID.colname] != cma$event.info[i-1,cma$ID.colname] ) align.to <- cma$event.info$.DATE.as.Date[i];
      cma$event.info$.DATE.as.Date[i] <- earliest.date + (cma$event.info$.DATE.as.Date[i] - align.to);
      cma$event.info$.FU.START.DATE[i] <- earliest.date + (cma$event.info$.FU.START.DATE[i] - align.to);
      cma$event.info$.FU.END.DATE[i] <- earliest.date + (cma$event.info$.FU.END.DATE[i] - align.to);
      cma$event.info$.OBS.START.DATE[i] <- earliest.date + (cma$event.info$.OBS.START.DATE[i] - align.to);
      cma$event.info$.OBS.END.DATE[i] <- earliest.date + (cma$event.info$.OBS.END.DATE[i] - align.to);
    }
    correct.earliest.followup.window <- min(cma$event.info$.DATE.as.Date - min(cma$event.info$.FU.START.DATE,na.rm=TRUE),na.rm=TRUE);
  } else
  {
    correct.earliest.followup.window <- 0;
  }

  # Compute the duration if not given:
  if( is.na(duration) )
  {
    latest.date <- max(c(cma$event.info$.DATE.as.Date + cma$event.info$event.interval,
                         cma$event.info$.DATE.as.Date + cma$event.info[,cma$event.duration.colname],
                         cma$event.info$.FU.END.DATE),na.rm=TRUE);
    duration <- as.numeric(latest.date - earliest.date) + correct.earliest.followup.window;
  }
  endperiod <- duration;

  # Reserve space for the CMA plotting:
  adh.plot.space <- c(0, ifelse( plot.CMA && !is.null(getCMA(cma)), duration*CMA.plot.ratio, 0) );
  duration.total <- duration + adh.plot.space[2];

  # The actual plotting:
  if(inherits(msg <- try(plot( 0, 1,
                               xlim=c(0-2*duration.total/100,duration.total), xaxs="i",
                               ylim=c(0,nrow(cma$event.info)+1), yaxs="i", type="n",
                               axes=FALSE,
                               xlab="", ylab=""),
                         silent=TRUE),
              "try-error"))
  {
    # Some error occured when creatig the plot...
    cat(msg);
    return (invisible(NULL));
  }

  # Character height and width in the current plotting system:
  char.width <- strwidth("O",cex=cex); char.height <- strheight("O",cex=cex);

  # Minimum plot dimensions:
  if( abs(par("usr")[2] - par("usr")[1]) <= char.width * min.plot.size.in.characters.horiz ||
      abs(par("usr")[4] - par("usr")[3]) <= char.height * min.plot.size.in.characters.vert * length(patids))
  {
    cat(paste0("Plotting area is too small (it must be at least ",
               min.plot.size.in.characters.horiz,
               " x ",
               min.plot.size.in.characters.vert,
               " characters per patient, but now it is only ",
               round(abs(par("usr")[2] - par("usr")[1]) / char.width,1),
               " x ",
               round(abs(par("usr")[4] - par("usr")[3]) / (char.height * length(patids)),1),
               ")!\n"));
    #segments(x0=c(par("usr")[1], par("usr")[1]),
    #         y0=c(par("usr")[3], par("usr")[4]),
    #         x1=c(par("usr")[2], par("usr")[2]),
    #         y1=c(par("usr")[4], par("usr")[3]),
    #         col="red", lwd=3);
    return (invisible(NULL));
  }

  # Continue plotting:
  box();
  title(main=paste0(ifelse(align.all.patients, "Event patterns (all patients aligned)", "Event patterns"),
                    ifelse(show.cma,paste0(" (",class(cma)[1],")"),"")),
        xlab=ifelse(show.period=="dates","","days"),
        ylab=ifelse((print.CMA || plot.CMA) && !is.null(getCMA(cma)),"patient (& CMA)","patient"),
        cex.lab=cex.lab);

  # The patient axis and CMA plots:
  if( plot.CMA && !is.null(getCMA(cma)) )
  {
    # Maximum achieved CMA:
    adh.max <- max(c(getCMA(cma)$CMA, 1.0),na.rm=TRUE);
    # Function mapping the CMA values to the appropriate x-coordinates:
    .rescale.xcoord.for.CMA.plot <- function(x,pfree=0.20) return (adh.plot.space[1] + x/adh.max*(adh.plot.space[2]*(1-pfree) - adh.plot.space[1]));
  }
  draw.gray.band <- FALSE;
  for( p in as.character(patids) )
  {
    # The participant axis text:
    s <- which(cma$event.info[,cma$ID.colname] == p);
    x <- which(getCMA(cma)[cma$ID.colname] == p);
    pid <- ifelse( print.CMA && !is.null(getCMA(cma)) && length(x)==1 && !is.na(getCMA(cma)[x,"CMA"]), paste0(p,"\n",sprintf("%.1f%%",getCMA(cma)[x,"CMA"]*100)), p);
    mtext( pid, 2, line=0.5, at=mean(s), las=2, cex=cex.axis );

    # The alternating gray bands:
    if( draw.gray.band )
      rect( 0-1, min(s)-0.5, duration.total+1, max(s)+0.5, col=gray(0.95), border=NA );
    draw.gray.band <- !draw.gray.band;

    # The participant CMA plot:
    if( plot.CMA && !is.null(getCMA(cma)) )
    {
      adh <- getCMA(cma)[x,"CMA"];
      rect(.rescale.xcoord.for.CMA.plot(0), mean(s)-1, .rescale.xcoord.for.CMA.plot(min(adh,adh.max)), mean(s)+1, col=CMA.plot.col, border=NA);
      rect(.rescale.xcoord.for.CMA.plot(0), mean(s)-1, .rescale.xcoord.for.CMA.plot(max(1.0,adh.max)), mean(s)+1, col=NA, border=CMA.plot.border);
      if( !is.na(adh) )
      {
        cma.string <- sprintf("%.1f%%",adh*100); available.x.space <- abs(.rescale.xcoord.for.CMA.plot(max(1.0,adh.max)) - .rescale.xcoord.for.CMA.plot(0));
        if( strwidth(cma.string, cex=cex.axis) <= available.x.space )
        { # horizontal writing of the CMA:
          text(x=(.rescale.xcoord.for.CMA.plot(0) + .rescale.xcoord.for.CMA.plot(max(1.0,adh.max)))/2, y=mean(s),
               labels=cma.string, col=CMA.plot.text, cex=cex.axis);
        } else if( strheight(cma.string, cex=cex.axis) <= available.x.space )
        { # vertical writing of the CMA:
          text(x=(.rescale.xcoord.for.CMA.plot(0) + .rescale.xcoord.for.CMA.plot(max(1.0,adh.max)))/2, y=mean(s),
               labels=cma.string, col=CMA.plot.text, cex=cex.axis, srt=90);
        } # otherwise, theres' no space for showing the CMA here
      }
    }

    # The follow-up and observation windows:
    if( highlight.followup.window )
      rect(adh.plot.space[2] + as.numeric(cma$event.info$.FU.START.DATE[s[1]] - earliest.date) + correct.earliest.followup.window, s[1]-0.25,
           adh.plot.space[2] + as.numeric(cma$event.info$.FU.END.DATE[s[1]] - earliest.date) + correct.earliest.followup.window, s[length(s)]+0.25,
           col=NA, border=followup.window.col, lty="dashed", lwd=2);
    if( highlight.observation.window )
    {
      if( inherits(cma,"CMA8") && !is.null(cma$real.obs.window) && show.real.obs.window.start )
      {
        # The given observation window:
        rect(adh.plot.space[2] + as.numeric(cma$event.info$.OBS.START.DATE[s[1]] - earliest.date) + correct.earliest.followup.window, s[1]-0.25,
             adh.plot.space[2] + as.numeric(cma$event.info$.OBS.END.DATE[s[1]] - earliest.date) + correct.earliest.followup.window, s[length(s)]+0.25,
             col=adjustcolor(observation.window.col,alpha.f=0.3), border=NA, density=observation.window.density, angle=observation.window.angle);
        # For some CMAs, also show the real observation window:
        ss <- which(cma$real.obs.window[,cma$ID.colname]==p);
        if( length(ss) == 1)
        {
          if( !is.null(cma$real.obs.windows$window.start) && !is.na(cma$real.obs.windows$window.start[ss]) )
          {
            real.obs.window.start <- cma$real.obs.windows$window.start[ss];
          } else
          {
            real.obs.window.start <- cma$event.info$.OBS.START.DATE[s[1]];
          }
          if( !is.null(cma$real.obs.windows$window.end) && !is.na(cma$real.obs.windows$window.end[ss]) )
          {
            real.obs.window.end <- cma$real.obs.windows$window.end[ss];
          } else
          {
            real.obs.window.end <- cma$event.info$.OBS.END.DATE[s[1]];
          }
          rect(adh.plot.space[2] + as.numeric(real.obs.window.start - earliest.date) + correct.earliest.followup.window, s[1]-0.25,
               adh.plot.space[2] + as.numeric(real.obs.window.end - earliest.date) + correct.earliest.followup.window, s[length(s)]+0.25,
               col=adjustcolor(observation.window.col,alpha.f=0.3), border=NA, density=real.obs.window.density, angle=real.obs.window.angle);
        }
      } else
      {
        # The given observation window:
        rect(adh.plot.space[2] + as.numeric(cma$event.info$.OBS.START.DATE[s[1]] - earliest.date) + correct.earliest.followup.window, s[1]-0.25,
             adh.plot.space[2] + as.numeric(cma$event.info$.OBS.END.DATE[s[1]] - earliest.date) + correct.earliest.followup.window, s[length(s)]+0.25,
             col=adjustcolor(observation.window.col,alpha.f=0.3), border=NA, density=observation.window.density, angle=observation.window.angle);
      }
    }
  }
  if( plot.CMA && !is.null(getCMA(cma)) )
  {
    # Mark the drawing area:
    if( adh.max > 1.0 )
    {
      rect(.rescale.xcoord.for.CMA.plot(0), par("usr")[3], .rescale.xcoord.for.CMA.plot(adh.max), par("usr")[4], col=adjustcolor(CMA.plot.bkg,alpha.f=0.25), border=NA);
      abline(v=c(.rescale.xcoord.for.CMA.plot(0), .rescale.xcoord.for.CMA.plot(1.0), .rescale.xcoord.for.CMA.plot(adh.max)), col=CMA.plot.border, lty=c("solid","dotted","solid"), lwd=1);
      mtext( c("0%",sprintf("%.1f%%",adh.max*100)), 3, line=0.5, at=c(.rescale.xcoord.for.CMA.plot(0), .rescale.xcoord.for.CMA.plot(adh.max)), las=2, cex=cex.axis, col=CMA.plot.border );
      if( (.rescale.xcoord.for.CMA.plot(adh.max) - .rescale.xcoord.for.CMA.plot(1.0)) > 1.5*strwidth("0", cex=cex.axis) ) # Don't overcrowd the 100% and maximum CMA by omitting 100%
      {
        mtext( c("100%"), 3, line=0.5, at=c(.rescale.xcoord.for.CMA.plot(1.0)), las=2, cex=cex.axis, col=CMA.plot.border );
      }
    } else
    {
      rect(.rescale.xcoord.for.CMA.plot(0), par("usr")[3], .rescale.xcoord.for.CMA.plot(1.0), par("usr")[4], col=adjustcolor(CMA.plot.bkg,alpha.f=0.25), border=NA);
      abline(v=c(.rescale.xcoord.for.CMA.plot(0), .rescale.xcoord.for.CMA.plot(1.0)), col=CMA.plot.border, lty="solid", lwd=1);
      mtext( c("0%","100%"), 3, line=0.5, at=c(.rescale.xcoord.for.CMA.plot(0), .rescale.xcoord.for.CMA.plot(1.0)), las=2, cex=cex.axis, col=CMA.plot.border );
    }
  }

  # Plot each event:
  curpat <- TRUE;
  for( i in 1:nrow(cma$event.info) )
  {
    start <- as.numeric(cma$event.info$.DATE.as.Date[i] - earliest.date);
    end <- start + cma$event.info[i,cma$event.duration.colname];
    col <- .map.category.to.color(ifelse(is.na(cma$medication.class.colname) || !(cma$medication.class.colname %in% names(cma$data)),unspecified.category.label,cma$event.info[i,cma$medication.class.colname]));
    points( adh.plot.space[2]+start+correct.earliest.followup.window, i, pch=pch.start.event, col=col, cex=cex);
    points(adh.plot.space[2]+end+correct.earliest.followup.window, i, pch=pch.end.event, col=col, cex=cex);
    segments( adh.plot.space[2]+start+correct.earliest.followup.window, i, adh.plot.space[2]+end+correct.earliest.followup.window, i, col=col, lty=lty.event, lwd=lwd.event);

    if( show.event.intervals && !is.na(cma$event.info$event.interval[i]) )
    {
      end.pi <- start + cma$event.info$event.interval[i] - cma$event.info$gap.days[i];
      rect(adh.plot.space[2]+start+correct.earliest.followup.window, i-char.height/2,
           adh.plot.space[2]+end.pi+correct.earliest.followup.window, i+char.height/2,
           col=adjustcolor(col,alpha.f=0.2), border=col);
      if( cma$event.info$gap.days[i] > 0 )
        rect(adh.plot.space[2]+end.pi+correct.earliest.followup.window, i-char.height/2,
             adh.plot.space[2]+end.pi+cma$event.info$gap.days[i]+correct.earliest.followup.window, i+char.height/2,
             density=25, col=adjustcolor(col,alpha.f=0.5), border=col);
    }

    if( i < nrow(cma$event.info) )
    {
      if( cma$event.info[i,cma$ID.colname] == cma$event.info[i+1,cma$ID.colname] )
      {
        if( show.event.intervals && !is.na(cma$event.info$event.interval[i]) )
        {
          #start.next <- as.numeric(cma$event.info$.DATE.as.Date[i+1] - earliest.date);
          #segments( adh.plot.space[2]+end.pi, i, adh.plot.space[2]+start.next, i, col=col.continuation, lty=lty.continuation, lwd=lwd.continuation);
          #segments( adh.plot.space[2]+start.next, i, adh.plot.space[2]+start.next, i+1, col=col.continuation, lty=lty.continuation, lwd=lwd.continuation);
        }
      } else
      {
        # Now the patient is changing:
        if( curpat )
        {
          #y <- which(cma$event.info[,cma$ID.colname] == cma$event.info[i+1,cma$ID.colname]);
          #rect( 0-1, min(y)-0.5, duration.total+1, max(y)+0.5, col=gray(0.95), border=NA );
        }
        curpat <- !curpat;
      }
    }
  }

  # The days/dates axis and the grid at those important days/dates:
  if( show.period=="dates" )
  {
      axis( 1, at=adh.plot.space[2]+seq(0,as.numeric(endperiod),by=period.in.days),
            labels=as.character(earliest.date + round(seq(0,as.numeric(endperiod),by=period.in.days),1), format=cma$date.format),
            las=3, cex.axis=cex.axis);
      abline( v=adh.plot.space[2]+seq(0,as.numeric(endperiod),by=period.in.days), lty="dotted", col=gray(0.5) );
      abline( v=adh.plot.space[2]+endperiod, lty="solid", col=gray(0.5) );
  } else
  {
      if( align.first.event.at.zero )
      {
          xpos <- c(correct.earliest.followup.window-seq(0,as.numeric(correct.earliest.followup.window),by=period.in.days),
                    seq(0,as.numeric(endperiod),by=period.in.days)+correct.earliest.followup.window);
          xpos <- xpos[ xpos >= 0 & xpos <= endperiod ];
          axis( 1, at=adh.plot.space[2]+xpos,
                labels=as.character(round(xpos-correct.earliest.followup.window,1)),
                las=3, cex.axis=cex.axis);
          abline( v=adh.plot.space[2]+xpos, lty="dotted", col=gray(0.5) );
          abline( v=adh.plot.space[2]+endperiod, lty="solid", col=gray(0.5) );
      } else
      {
          axis( 1, at=adh.plot.space[2]+seq(0,as.numeric(endperiod),by=period.in.days),
                labels=as.character(round(seq(0,as.numeric(endperiod),by=period.in.days),1)),
                las=3, cex.axis=cex.axis);
          abline( v=adh.plot.space[2]+seq(0,as.numeric(endperiod),by=period.in.days), lty="dotted", col=gray(0.5) );
          abline( v=adh.plot.space[2]+endperiod, lty="solid", col=gray(0.5) );
      }
  }

  # The legend:
  .legend <- function(x=0, y=0, width=1, height=1, do.plot=TRUE)
  {
    # Legend rectangle:
    if( do.plot ) rect(x, y, x + width, y + height, border=gray(0.6), lwd=2, col=rgb(0.99,0.99,0.99,legend.bkg.opacity));

    cur.y <- y + height; # current y
    max.width <- width; # maximum width

    # Legend title:
    if( do.plot ) text(x + width/2, cur.y, "Legend", pos=1, col=gray(0.3), cex=1.0);
    cur.y <- cur.y - 4*char.height; max.width <- max(max.width, strwidth("Legend", cex=1.0));

    # Event:
    if( do.plot ) segments(x + 1.0*char.width, cur.y, x + 4.0*char.width, cur.y, lty=lty.event, lwd=lwd.event, col="black");
    if( do.plot ) points(x + 1.0*char.width, cur.y, pch=pch.start.event, cex=1.0, col="black");
    if( do.plot ) points(x + 4.0*char.width, cur.y, pch=pch.end.event, cex=1.0, col="black");
    if( do.plot ) text(x + 5.0*char.width, cur.y, "duration", col="black", cex=0.75, pos=4);
    cur.y <- cur.y - 1.5*char.height; max.width <- max(max.width, 5.0*char.width + strwidth("duration", cex=0.75));

    # Event intervals:
    if( show.event.intervals )
    {
      if( do.plot ) rect(x + 1.0*char.width, cur.y, x + 4.0*char.width, cur.y - 1.0*char.height, border="black", col=adjustcolor("black",alpha.f=0.5));
      if( do.plot ) text(x + 5.0*char.width, cur.y - 0.5*char.height, "days covered", col="black", cex=0.75, pos=4);
      cur.y <- cur.y - 1.5*char.height; max.width <- max(max.width, 5.0*char.width + strwidth("days covered", cex=0.75));
      if( do.plot ) rect(x + 1.0*char.width, cur.y, x + 4.0*char.width, cur.y - 1.0*char.height, border="black", col="black", density=25);
      if( do.plot ) text(x + 5.0*char.width, cur.y - 0.5*char.height, "gap days", col="black", cex=0.75, pos=4);
      cur.y <- cur.y - 2.0*char.height; max.width <- max(max.width, 5.0*char.width + strwidth("gap days", cex=0.75));
    }

    # Events with names:
    for( i in 1:length(cols) )
    {
      if( do.plot ) rect(x + 1.0*char.width, cur.y, x + 4.0*char.width, cur.y - 1.0*char.height, border="black", col=adjustcolor(cols[i],alpha.f=0.5));
      if( do.plot ) text(x + 5.0*char.width, cur.y - 0.5*char.height, names(cols)[i], col="black", cex=0.75, pos=4);
      cur.y <- cur.y - 1.5*char.height; max.width <- max(max.width, 5.0*char.width + strwidth(names(cols)[i], cex=0.75));
    }
    cur.y <- cur.y - 0.5*char.height;

    # Follow-up window:
    if( highlight.followup.window )
    {
      if( do.plot ) rect(x + 1.0*char.width, cur.y, x + 4.0*char.width, cur.y - 1.0*char.height, border=followup.window.col, lty="dotted", lwd=2, col=rgb(1,1,1,0.0));
      if( do.plot ) text(x + 5.0*char.width, cur.y - 0.5*char.height, "follow-up wnd.", col="black", cex=0.75, pos=4);
      cur.y <- cur.y - 2.0*char.height; max.width <- max(max.width, 5.0*char.width + strwidth("follow-up wnd.", cex=0.75));
    }

    # Observation window:
    if( highlight.observation.window )
    {
      if( inherits(cma,"CMA8") && !is.null(cma$real.obs.windows) && show.real.obs.window.start )
      {
        if( do.plot ) rect(x + 1.0*char.width, cur.y, x + 4.0*char.width, cur.y - 1.0*char.height, border=rgb(1,1,1,0.0), col=adjustcolor(observation.window.col,alpha.f=0.3), density=observation.window.density, angle=observation.window.angle);
        if( do.plot ) text(x + 5.0*char.width, cur.y - 0.5*char.height, "theor. obs. wnd.", col="black", cex=0.75, pos=4);
        cur.y <- cur.y - 1.5*char.height; max.width <- max(max.width, 5.0*char.width + strwidth("theor. obs. wnd.", cex=0.75));
        if( do.plot ) rect(x + 1.0*char.width, cur.y, x + 4.0*char.width, cur.y - 1.0*char.height, border=rgb(1,1,1,0.0), col=adjustcolor(observation.window.col,alpha.f=0.3), density=real.obs.window.density, angle=real.obs.window.angle);
        if( do.plot ) text(x + 5.0*char.width, cur.y - 0.5*char.height, "real obs.wnd.", col="black", cex=0.75, pos=4);
        cur.y <- cur.y - 2.0*char.height; max.width <- max(max.width, 5.0*char.width + strwidth("real obs.wnd.", cex=0.75));
      } else
      {
        if( do.plot ) rect(x + 1.0*char.width, cur.y, x + 4.0*char.width, cur.y - 1.0*char.height, border=rgb(1,1,1,0.0), col=adjustcolor(observation.window.col,alpha.f=0.3), density=observation.window.density, angle=observation.window.angle);
        if( do.plot ) text(x + 5.0*char.width, cur.y - 0.5*char.height, "observation wnd.", col="black", cex=0.75, pos=4);
        cur.y <- cur.y - 2.0*char.height; max.width <- max(max.width, 5.0*char.width + strwidth("observation wnd.", cex=0.75));
      }
    }

    # Required size:
    return (c("width" =max.width + 5.0*char.width,
              "height"=(y + height - cur.y) + 1.0*char.height));
  }
  if( show.legend )
  {
    legend.size <- .legend(do.plot=FALSE);
    if( is.na(legend.x) || legend.x == "right" )
    {
      legend.x <- par("usr")[2] - legend.size["width"] - char.width;
    } else if( legend.x == "left" )
    {
      legend.x <- par("usr")[1] + char.width;
    } else if( !is.numeric(legend.x) && length(legend.x) != 1 )
    {
      legend.x <- par("usr")[2] - legend.size["width"] - char.width;
    }
    if( is.na(legend.y) || legend.y == "bottom" )
    {
      legend.y <- par("usr")[3] + char.height;
    } else if( legend.y == "top" )
    {
      legend.y <- par("usr")[4] - legend.size["height"] - char.height;
    } else if( !is.numeric(legend.y) && length(legend.y) != 1 )
    {
      legend.y <- par("usr")[3] + char.height;
    }
    invisible(.legend(legend.x, legend.y, as.numeric(legend.size["width"]), as.numeric(legend.size["height"])));
  }
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
#' @param followup.window.start If a \emph{\code{Date}} object, it represents
#' the actual start date of the follow-up window; if a \emph{string} it is the
#' name of the column in \code{data} containing the start date of the follow-up
#' window either as the numbers of \code{followup.window.start.unit} units after
#' the first event (the column must be of type \code{numeric}) or as actual
#' dates (in which case the column must be of type \code{Date}); if a
#' \emph{number} it is the number of time units defined in the
#' \code{followup.window.start.unit} parameter after the begin of the
#' participant's first event; or \code{NA} if not defined.
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
                  # The follow-up window:
                  followup.window.start=0, # if a number is the earliest event per participant date plus number of units, or a Date object, or a column name in data (NA = undefined)
                  followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
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
                  ...
                )
{
  # The summary:
  if( is.na(summary) ) summary <- "The ratio of days with medication available in the observation window excluding the last event; durations of all events added up and divided by number of days from first to last event, possibly resulting in a value >1.0";
  # Create the CMA0 object:
  ret.val <- CMA0(data=data,
                  ID.colname=ID.colname,
                  event.date.colname=event.date.colname,
                  event.duration.colname=event.duration.colname,
                  followup.window.start=followup.window.start,
                  followup.window.start.unit=followup.window.start.unit,
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
    # Auxliary internal function: Compute the CMA for a given patient:
    .process.patient <- function(data4ID)
    {
      # Force the selection, evaluation of promises and caching of the needed columns:
      # ... which columns to select (with their indices):
      columns.to.cache <- c(".EVENT.STARTS.BEFORE.OBS.WINDOW", ".EVENT.STARTS.AFTER.OBS.WINDOW", ".DATE.as.Date", event.duration.colname);
      # ... select these columns:
      data4ID.selected.columns <- data4ID[,..columns.to.cache];
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

  # Convert to data.table, cache event dat as Date objects, and key by patient ID and event date
  data.copy <- data.table(data);
  data.copy[, .DATE.as.Date := as.Date(get(event.date.colname),format=date.format)]; # .DATE.as.Date: convert event.date.colname from formatted string to Date
  setkeyv(data.copy, c(ID.colname, ".DATE.as.Date")); # key (and sorting) by patient ID and event date

  # Compute the workhorse function:
  tmp <- .compute.function(.workhorse.function, fnc.ret.vals=2,
                           parallel.backend=parallel.backend,
                           parallel.threads=parallel.threads,
                           data=data.copy,
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
                           suppress.warnings=suppress.warnings);
  if( is.null(tmp) || is.null(tmp$CMA) || is.null(tmp$event.info) ) return (NULL);

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
  class(ret.val) <- c("CMA1", class(ret.val));
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
#' drawn.
#' @param show.legend \emph{Logical}, should the legend be drawn?
#' @param legend.x The position of the legend on the x axis; can be "left",
#' "right" (default), or a \emph{numeric} value.
#' @param legend.y The position of the legend on the y axis; can be "bottom"
#' (default), "top", or a \emph{numeric} value.
#' @param legend.bkg.opacity A \emph{number} between 0.0 and 1.0 specifying the
#' opacity of the legend background.
#' @param cex,cex.axis,cex.lab \emph{numeric} values specifying the \code{cex}
#' of the various types of text.
#' @param show.cma \emph{Logical}, should the CMA type be shown in the title?
#' @param col.cats A \emph{color} or a \emph{function} that specifies the single
#' colour or the colour palette used to plot the different medication; by
#' default \code{\link[grDevices]{cm.colors}}.
#' @param unspecified.category.label A \emph{string} giving the name of the
#' unspecified (generic) medication category.
#' @param lty.event,lwd.event,pch.start.event,pch.end.event The style of the
#' event (line style, width, and start and end symbols).
#' @param show.event.intervals \emph{Logical}, should the actual event intervals
#' be shown?
#' @param col.na The colour used for missing event data.
#' @param bw.plot \emph{Logical}, should the plot use grayscale only (i.e., the
#' \code{\link[grDevices]{gray.colors}} function)?
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
#' @param observation.window.col,observation.window.density,observation.window.angle Attributes of the observation window
#' (colour, shading density and angle).
#' @param show.real.obs.window.start,real.obs.window.density,real.obs.window.angle For some CMAs, the observation window might
#' be adjusted, in which case should it be plotted and with that attributes?
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
                      show.legend=TRUE, legend.x="right", legend.y="bottom", legend.bkg.opacity=0.5, # legend params and position
                      cex=1.0, cex.axis=0.75, cex.lab=1.0,   # various graphical params
                      show.cma=TRUE,                         # show the CMA type
                      col.cats=cm.colors,         # single color or a function mapping the categories to colors
                      unspecified.category.label="drug",     # the label of the unspecified category of medication
                      lty.event="solid", lwd.event=2, pch.start.event=15, pch.end.event=16, # event style
                      show.event.intervals=TRUE,             # show the actual rpescription intervals
                      col.na="lightgray",                    # color for mising data
                      print.CMA=TRUE,                  # print CMA next to the participant ID?
                      plot.CMA=TRUE,                   # plot the CMA next to the participant ID?
                      CMA.plot.ratio=0.10,             # the proportion of the total horizontal plot to be taken by the CMA plot
                      CMA.plot.col="lightgreen", CMA.plot.border="darkgreen", CMA.plot.bkg="aquamarine", CMA.plot.text=CMA.plot.border, # attributes of the CMA plot
                      highlight.followup.window=TRUE, followup.window.col="green",
                      highlight.observation.window=TRUE, observation.window.col="yellow", observation.window.density=35, observation.window.angle=-30,
                      show.real.obs.window.start=TRUE, real.obs.window.density=35, real.obs.window.angle=30, # for some CMAs, the real observation window starts at a different date
                      bw.plot=FALSE                          # if TRUE, override all user-given colors and replace them with a scheme suitable for grayscale plotting
                     )
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
               cex=cex, cex.axis=cex.axis, cex.lab=cex.lab,
               show.cma=show.cma,
               col.cats=col.cats,
               unspecified.category.label=unspecified.category.label,
               lty.event=lty.event, lwd.event=lwd.event, pch.start.event=pch.start.event, pch.end.event=pch.end.event,
               show.event.intervals=show.event.intervals,
               col.na=col.na,
               print.CMA=print.CMA,
               plot.CMA=plot.CMA,
               CMA.plot.ratio=CMA.plot.ratio,
               CMA.plot.col=CMA.plot.col,
               CMA.plot.border=CMA.plot.border,
               CMA.plot.bkg=CMA.plot.bkg,
               CMA.plot.text=CMA.plot.text,
               highlight.followup.window=highlight.followup.window, followup.window.col=followup.window.col,
               highlight.observation.window=highlight.observation.window,
               observation.window.col=observation.window.col,
               observation.window.density=observation.window.density,
               observation.window.angle=observation.window.angle,
               show.real.obs.window.start=show.real.obs.window.start,
               real.obs.window.density=real.obs.window.density, real.obs.window.angle=real.obs.window.angle,
               bw.plot=bw.plot,
               ...)




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
#' @param followup.window.start If a \emph{\code{Date}} object, it represents
#' the actual start date of the follow-up window; if a \emph{string} it is the
#' name of the column in \code{data} containing the start date of the follow-up
#' window either as the numbers of \code{followup.window.start.unit} units after
#' the first event (the column must be of type \code{numeric}) or as actual
#' dates (in which case the column must be of type \code{Date}); if a
#' \emph{number} it is the number of time units defined in the
#' \code{followup.window.start.unit} parameter after the begin of the
#' participant's first event; or \code{NA} if not defined.
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
#' @seealso CMAs 1 to 8 are defined in:
#'
#' Vollmer, W. M., Xu, M., Feldstein, A., Smith, D., Waterbury, A., & Rand, C.
#' (2012). Comparison of pharmacy-based measures of medication adherence.
#' \emph{BMC Health Services Research}, \strong{12}, 155.
#' \url{http://doi.org/10.1186/1472-6963-12-155}.
#'
#' @examples
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
#'             );
#' @export
CMA2 <- function( data=NULL, # the data used to compute the CMA on
                  # Important columns in the data
                  ID.colname=NA, # the name of the column containing the unique patient ID (NA = undefined)
                  event.date.colname=NA, # the start date of the event in the date.format format (NA = undefined)
                  event.duration.colname=NA, # the event duration in days (NA = undefined)
                  # The follow-up window:
                  followup.window.start=0, # if a number is the earliest event per participant date plus number of units, or a Date object, or a column name in data (NA = undefined)
                  followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
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
                  ...
                )
{
  # The summary:
  if( is.na(summary) ) summary <- "The ratio of days with medication available in the observation window including the last event; durations of all events added up and divided by number of days from first event to end of observation window, possibly resulting in a value >1.0";
  # Create the CMA0 object:
  ret.val <- CMA0(data=data,
                  ID.colname=ID.colname,
                  event.date.colname=event.date.colname,
                  event.duration.colname=event.duration.colname,
                  followup.window.start=followup.window.start,
                  followup.window.start.unit=followup.window.start.unit,
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
    # Auxliary internal function: Compute the CMA for a given patient:
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

  # Convert to data.table, cache event dat as Date objects, and key by patient ID and event date
  data.copy <- data.table(data);
  data.copy[, .DATE.as.Date := as.Date(get(event.date.colname),format=date.format)]; # .DATE.as.Date: convert event.date.colname from formatted string to Date
  setkeyv(data.copy, c(ID.colname, ".DATE.as.Date")); # key (and sorting) by patient ID and event date

  # Compute the workhorse function:
  tmp <- .compute.function(.workhorse.function, fnc.ret.vals=2,
                           parallel.backend=parallel.backend,
                           parallel.threads=parallel.threads,
                           data=data.copy,
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
                           suppress.warnings=suppress.warnings);
  if( is.null(tmp) || is.null(tmp$CMA) || is.null(tmp$event.info) ) return (NULL);

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
  class(ret.val) <- c("CMA2","CMA1", class(ret.val));
  return (ret.val);
  if( is.null(CMA) ) return (NULL);
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
                  # The follow-up window:
                  followup.window.start=0, # if a number is the earliest event per participant date + number of units, or a Date object, or a column name in data (NA = undefined)
                  followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
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
                  ...
                )
{
  # The summary:
  if( is.na(summary) ) summary <- "The ratio of days with medication available in the observation window including the last event; durations of all events added up and divided by number of days from first to last event, then capped at 1.0";
  # Create the CMA1 object:
  ret.val <- CMA1(data=data,
                  ID.colname=ID.colname,
                  event.date.colname=event.date.colname,
                  event.duration.colname=event.duration.colname,
                  followup.window.start=followup.window.start,
                  followup.window.start.unit=followup.window.start.unit,
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
                  suppress.warnings=suppress.warnings);
  if( is.null(ret.val) ) return (NULL); # some error upstream

  # Cap CMA at 1.0:
  ret.val$CMA$CMA <- pmin(1, ret.val$CMA$CMA);

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
                  # The follow-up window:
                  followup.window.start=0, # if a number is the earliest event per participant date plus number of units, or a Date object, or a column name in data (NA = undefined)
                  followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
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
                  ...
                )
{
  # The summary:
  if( is.na(summary) ) summary <- "The ratio of days with medication available in the observation window including the last event; durations of all events added up and divided by number of days from first event to end of observation window, then capped at 1.0";
  # Create the CMA2 object:
  ret.val <- CMA2(data=data,
                  ID.colname=ID.colname,
                  event.date.colname=event.date.colname,
                  event.duration.colname=event.duration.colname,
                  followup.window.start=followup.window.start,
                  followup.window.start.unit=followup.window.start.unit,
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
                  suppress.warnings=suppress.warnings);
  if( is.null(ret.val) ) return (NULL); # some error upstream

  # Cap CMA at 1.0:
  ret.val$CMA$CMA <- pmin(1, ret.val$CMA$CMA);

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
#' @param carry.only.for.same.medication \emph{Logical}, if \code{TRUE}, the
#' carry-over applies only across medication of the same type.
#' @param consider.dosage.change \emph{Logical}, if \code{TRUE}, the carry-over
#' is adjusted to also reflect changes in dosage.
#' @param followup.window.start If a \emph{\code{Date}} object, it represents
#' the actual start date of the follow-up window; if a \emph{string} it is the
#' name of the column in \code{data} containing the start date of the follow-up
#' window either as the numbers of \code{followup.window.start.unit} units after
#' the first event (the column must be of type \code{numeric}) or as actual
#' dates (in which case the column must be of type \code{Date}); if a
#' \emph{number} it is the number of time units defined in the
#' \code{followup.window.start.unit} parameter after the begin of the
#' participant's first event; or \code{NA} if not defined.
#' @param followup.window.start.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.start} refers to (when a number), or
#' \code{NA} if not defined.
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
                  # Various types medhods of computing gaps:
                  carry.only.for.same.medication=FALSE, # if TRUE the carry-over applies only across medication of same type (NA = undefined)
                  consider.dosage.change=FALSE, # if TRUE carry-over is adjusted to reflect changes in dosage (NA = undefined)
                  # The follow-up window:
                  followup.window.start=0, # if a number is the earliest event per participant date plus number of units, or a Date object, or a column name in data (NA = undefined)
                  followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
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
                  ...
                )
{
  # The summary:
  if( is.na(summary) ) summary <- "The ratio of days with medication available from first to last event; total number of gap days extracted from this time interval, then divided by the time interval, accounting for carry-over within observation window and excluding remaining supply";
  # Create the CMA0 object:
  ret.val <- CMA0(data=data,
                  ID.colname=ID.colname,
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
                  observation.window.start=observation.window.start,
                  observation.window.start.unit=observation.window.start.unit,
                  observation.window.duration=observation.window.duration,
                  observation.window.duration.unit=observation.window.duration.unit,
                  date.format=date.format,
                  summary=summary,
                  suppress.warnings=suppress.warnings);
  if( is.null(ret.val) ) return (NULL); # some error upstream

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
    # Auxliary internal function: Compute the CMA for a given patient:
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

  # Convert to data.table, cache event dat as Date objects, and key by patient ID and event date
  data.copy <- data.table(data);
  data.copy[, .DATE.as.Date := as.Date(get(event.date.colname),format=date.format)]; # .DATE.as.Date: convert event.date.colname from formatted string to Date
  setkeyv(data.copy, c(ID.colname, ".DATE.as.Date")); # key (and sorting) by patient ID and event date

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
                           suppress.warnings=suppress.warnings);
  if( is.null(tmp) || is.null(tmp$CMA) || is.null(tmp$event.info) ) return (NULL);

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
  class(ret.val) <- c("CMA5","CMA1", class(ret.val));
  return (ret.val);
  if( is.null(CMA) ) return (NULL);
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
#' @param carry.only.for.same.medication \emph{Logical}, if \code{TRUE}, the
#' carry-over applies only across medication of the same type.
#' @param consider.dosage.change \emph{Logical}, if \code{TRUE}, the carry-over
#' is adjusted to also reflect changes in dosage.
#' @param followup.window.start If a \emph{\code{Date}} object, it represents
#' the actual start date of the follow-up window; if a \emph{string} it is the
#' name of the column in \code{data} containing the start date of the follow-up
#' window either as the numbers of \code{followup.window.start.unit} units after
#' the first event (the column must be of type \code{numeric}) or as actual
#' dates (in which case the column must be of type \code{Date}); if a
#' \emph{number} it is the number of time units defined in the
#' \code{followup.window.start.unit} parameter after the begin of the
#' participant's first event; or \code{NA} if not defined.
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
                  # Various types medhods of computing gaps:
                  carry.only.for.same.medication=FALSE, # if TRUE the carry-over applies only across medication of same type (NA = undefined)
                  consider.dosage.change=FALSE, # if TRUE carry-over is adjusted to reflect changes in dosage (NA = undefined)
                  # The follow-up window:
                  followup.window.start=0, # if a number is the earliest event per participant date plus number of units, or a Date object, or a column name in data (NA = undefined)
                  followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
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
                  ...
                )
{
  # The summary:
  if( is.na(summary) ) summary <- "The ratio of days with medication available from the first event to the end of the observation window; total number of gap days extracted from this time interval, then divided by the time interval, accounting for carry-over within observation window and excluding remaining supply";
  # Create the CMA0 object:
  ret.val <- CMA0(data=data,
                  ID.colname=ID.colname,
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
                  observation.window.start=observation.window.start,
                  observation.window.start.unit=observation.window.start.unit,
                  observation.window.duration=observation.window.duration,
                  observation.window.duration.unit=observation.window.duration.unit,
                  date.format=date.format,
                  summary=summary,
                  suppress.warnings=suppress.warnings);
  if( is.null(ret.val) ) return (NULL); # some error upstream


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
    # Auxliary internal function: Compute the CMA for a given patient:
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

  # Convert to data.table, cache event dat as Date objects, and key by patient ID and event date
  data.copy <- data.table(data);
  data.copy[, .DATE.as.Date := as.Date(get(event.date.colname),format=date.format)]; # .DATE.as.Date: convert event.date.colname from formatted string to Date
  setkeyv(data.copy, c(ID.colname, ".DATE.as.Date")); # key (and sorting) by patient ID and event date

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
                           suppress.warnings=suppress.warnings);
  if( is.null(tmp) || is.null(tmp$CMA) || is.null(tmp$event.info) ) return (NULL);

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
  class(ret.val) <- c("CMA6","CMA1", class(ret.val));
  return (ret.val);
  if( is.null(CMA) ) return (NULL);
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
#' @param carry.only.for.same.medication \emph{Logical}, if \code{TRUE}, the
#' carry-over applies only across medication of the same type.
#' @param consider.dosage.change \emph{Logical}, if \code{TRUE}, the carry-over
#' is adjusted to also reflect changes in dosage.
#' @param followup.window.start If a \emph{\code{Date}} object, it represents
#' the actual start date of the follow-up window; if a \emph{string} it is the
#' name of the column in \code{data} containing the start date of the follow-up
#' window either as the numbers of \code{followup.window.start.unit} units after
#' the first event (the column must be of type \code{numeric}) or as actual
#' dates (in which case the column must be of type \code{Date}); if a
#' \emph{number} it is the number of time units defined in the
#' \code{followup.window.start.unit} parameter after the begin of the
#' participant's first event; or \code{NA} if not defined.
#' @param followup.window.start.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.start} refers to (when a number), or
#' \code{NA} if not defined.
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
                  # Various types medhods of computing gaps:
                  carry.only.for.same.medication=FALSE, # if TRUE the carry-over applies only across medication of same type (NA = undefined)
                  consider.dosage.change=FALSE, # if TRUE carry-over is adjusted to reflect changes in dosage (NA = undefined)
                  # The follow-up window:
                  followup.window.start=0, # if a number is the earliest event per participant date + number of units, or a Date object, or a column name in data (NA = undefined)
                  followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
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
                  ...
                )
{
  # The summary:
  if( is.na(summary) ) summary <- "The ratio of days with medication available in the whole observation window; total number of gap days extracted from this time interval, then divided by the time interval, accounting for carry-over both before and within observation window and excluding remaining supply";
  # Create the CMA0 object:
  ret.val <- CMA0(data=data,
                  ID.colname=ID.colname,
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
                  observation.window.start=observation.window.start,
                  observation.window.start.unit=observation.window.start.unit,
                  observation.window.duration=observation.window.duration,
                  observation.window.duration.unit=observation.window.duration.unit,
                  date.format=date.format,
                  summary=summary,
                  suppress.warnings=suppress.warnings);
  if( is.null(ret.val) ) return (NULL); # some error upstream


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
    # Auxliary internal function: Compute the CMA for a given patient:
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

  # Convert to data.table, cache event dat as Date objects, and key by patient ID and event date
  data.copy <- data.table(data);
  data.copy[, .DATE.as.Date := as.Date(get(event.date.colname),format=date.format)]; # .DATE.as.Date: convert event.date.colname from formatted string to Date
  setkeyv(data.copy, c(ID.colname, ".DATE.as.Date")); # key (and sorting) by patient ID and event date

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
                           suppress.warnings=suppress.warnings);
  if( is.null(tmp) || is.null(tmp$CMA) || is.null(tmp$event.info) ) return (NULL);

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
  class(ret.val) <- c("CMA7","CMA1", class(ret.val));
  return (ret.val);
  if( is.null(CMA) ) return (NULL);
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
#' @param carry.only.for.same.medication \emph{Logical}, if \code{TRUE}, the
#' carry-over applies only across medication of the same type.
#' @param consider.dosage.change \emph{Logical}, if \code{TRUE}, the carry-over
#' is adjusted to also reflect changes in dosage.
#' @param followup.window.start If a \emph{\code{Date}} object, it represents
#' the actual start date of the follow-up window; if a \emph{string} it is the
#' name of the column in \code{data} containing the start date of the follow-up
#' window either as the numbers of \code{followup.window.start.unit} units after
#' the first event (the column must be of type \code{numeric}) or as actual
#' dates (in which case the column must be of type \code{Date}); if a
#' \emph{number} it is the number of time units defined in the
#' \code{followup.window.start.unit} parameter after the begin of the
#' participant's first event; or \code{NA} if not defined.
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
                  # Various types medhods of computing gaps:
                  carry.only.for.same.medication=FALSE, # if TRUE the carry-over applies only across medication of same type (NA = undefined)
                  consider.dosage.change=FALSE, # if TRUE carry-over is adjusted to reflect changes in dosage (NA = undefined)
                  # The follow-up window:
                  followup.window.start=0, # if a number is the earliest event per participant date plus number of units, or a Date object, or a column name in data (NA = undefined)
                  followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
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
                  ...
                )
{
  # The summary:
  if( is.na(summary) ) summary <- "The ratio of days with medication available in the whole observation window, with lagged start until previous supply is finished; total number of gap days extracted from this time interval, then divided by the time interval, accounting for carry-over within lagged observation window and excluding remaining supply";
  # Create the CMA0 object:
  ret.val <- CMA0(data=data,
                  ID.colname=ID.colname,
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
                  observation.window.start=observation.window.start,
                  observation.window.start.unit=observation.window.start.unit,
                  observation.window.duration=observation.window.duration,
                  observation.window.duration.unit=observation.window.duration.unit,
                  date.format=date.format,
                  summary=summary,
                  suppress.warnings=suppress.warnings);
  if( is.null(ret.val) ) return (NULL); # some error upstream

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
    # Auxliary internal function: Compute the new OW start date:
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

  # Convert to data.table, cache event dat as Date objects, and key by patient ID and event date
  data.copy <- data.table(data);
  data.copy[, .DATE.as.Date := as.Date(get(event.date.colname),format=date.format)]; # .DATE.as.Date: convert event.date.colname from formatted string to Date
  setkeyv(data.copy, c(ID.colname, ".DATE.as.Date")); # key (and sorting) by patient ID and event date

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
                           suppress.warnings=suppress.warnings);
  if( is.null(tmp) || is.null(tmp$CMA) || is.null(tmp$event.info) ) return (NULL);

  # Convert to data.frame and return:
  if( force.NA.CMA.for.failed.patients )
  {
    # Make sure patients with failed CMA estimations get an NA estimate!
    patids <- unique(data.copy[,get(ID.colname)]);
    if( length(patids) > nrow(tmp$CMA) )
    {
      setnames(tmp$CMA, 1, ".ID"); tmp$CMA <- merge(data.table(".ID"=patids, key=".ID"), tmp$CMA, all.x=TRUE); setnames(tmp$CMA, 1, ID.colname);
    }
  }
  ret.val[["CMA"]] <- as.data.frame(tmp$CMA); # names are fine cause this come directly from CMA7
  ret.val[["event.info"]] <- as.data.frame(tmp$event.info);
  ret.val[["real.obs.windows"]] <- unique(data.frame( ret.val$event.info[, ID.colname], "window.start"=ret.val$event.info$.OBS.START.DATE.UPDATED, "window.end"=NA)); setnames(ret.val$real.obs.windows, 1, ID.colname);
  class(ret.val) <- c("CMA8","CMA1", class(ret.val));
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
#' @param carry.only.for.same.medication \emph{Logical}, if \code{TRUE}, the
#' carry-over applies only across medication of the same type.
#' @param consider.dosage.change \emph{Logical}, if \code{TRUE}, the carry-over
#' is adjusted to also reflect changes in dosage.
#' @param followup.window.start If a \emph{\code{Date}} object, it represents
#' the actual start date of the follow-up window; if a \emph{string} it is the
#' name of the column in \code{data} containing the start date of the follow-up
#' window either as the numbers of \code{followup.window.start.unit} units after
#' the first event (the column must be of type \code{numeric}) or as actual
#' dates (in which case the column must be of type \code{Date}); if a
#' \emph{number} it is the number of time units defined in the
#' \code{followup.window.start.unit} parameter after the begin of the
#' participant's first event; or \code{NA} if not defined.
#' @param followup.window.start.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.start} refers to (when a number), or
#' \code{NA} if not defined.
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
                  # Various types medhods of computing gaps:
                  carry.only.for.same.medication=FALSE, # if TRUE the carry-over applies only across medication of same type (NA = undefined)
                  consider.dosage.change=FALSE, # if TRUE carry-over is adjusted to reflect changes in dosage (NA = undefined)
                  # The follow-up window:
                  followup.window.start=0, # if a number is the earliest event per participant date + number of units, or a Date object, or a column name in data (NA = undefined)
                  followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
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
                  ...
                )
{
  # The summary:
  if( is.na(summary) ) summary <- "The ratio of days with medication available in the observation window; the supply of each event is evenly spread until the next event (ratio days supply up to 1), then oversupply carried over to the next event; the each day in the observation window is weighted by its ratio of days supply, and the sum divided by the duration of the observation window";
  # Create the CMA0 object:
  ret.val <- CMA0(data=data,
                  ID.colname=ID.colname,
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
                  observation.window.start=observation.window.start,
                  observation.window.start.unit=observation.window.start.unit,
                  observation.window.duration=observation.window.duration,
                  observation.window.duration.unit=observation.window.duration.unit,
                  date.format=date.format,
                  summary=summary,
                  suppress.warnings=suppress.warnings);
  if( is.null(ret.val) ) return (NULL); # some error upstream

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
    # Auxliary internal function: Compute the CMA for a given patient:
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

    # ATTENTION: this is done for an observation window that is identical to the follow-up window to accomodate events that overshoot the observation window!
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
                                         observation.window.start.unit=followup.window.start.unit,
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

  # Convert to data.table, cache event dat as Date objects, and key by patient ID and event date
  data.copy <- data.table(data);
  data.copy[, .DATE.as.Date := as.Date(get(event.date.colname),format=date.format)]; # .DATE.as.Date: convert event.date.colname from formatted string to Date
  setkeyv(data.copy, c(ID.colname, ".DATE.as.Date")); # key (and sorting) by patient ID and event date

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
                           suppress.warnings=suppress.warnings);
  if( is.null(tmp) || is.null(tmp$CMA) || is.null(tmp$event.info) ) return (NULL);

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
  class(ret.val) <- c("CMA9","CMA1", class(ret.val));
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
#' @param followup.window.start If a \emph{\code{Date}} object, it represents
#' the actual start date of the follow-up window; if a \emph{string} it is the
#' name of the column in \code{data} containing the start date of the follow-up
#' window either as the numbers of \code{followup.window.start.unit} units after
#' the first event (the column must be of type \code{numeric}) or as actual
#' dates (in which case the column must be of type \code{Date}); if a
#' \emph{number} it is the number of time units defined in the
#' \code{followup.window.start.unit} parameter after the begin of the
#' participant's first event; or \code{NA} if not defined.
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
                             # Important columns in the data
                             ID.colname=NA, # the name of the column containing the unique patient ID (NA = undefined)
                             event.date.colname=NA, # the start date of the event in the date.format format (NA = undefined)
                             event.duration.colname=NA, # the event duration in days (NA = undefined)
                             event.daily.dose.colname=NA, # the prescribed daily dose (NA = undefined)
                             medication.class.colname=NA, # the classes/types/groups of medication (NA = undefined)
                             # Various types medhods of computing gaps:
                             carry.only.for.same.medication=NA, # if TRUE the carry-over applies only across medication of same type (NA = use the CMA's values)
                             consider.dosage.change=NA, # if TRUE carry-over is adjusted to reflect changes in dosage (NA = use the CMA's values)
                             # Treatment episodes:
                             medication.change.means.new.treatment.episode=TRUE, # does a change in medication automatically start a new treatment episode?
                             maximum.permissible.gap=180, # if a number, is the duration in units of max. permissible gaps between treatment episodes
                             maximum.permissible.gap.unit=c("days", "weeks", "months", "years", "percent")[1], # time units; can be "days", "weeks" (fixed at 7 days), "months" (fixed at 30 days), "years" (fixed at 365 days), or "percent", in which case maximum.permissible.gap is interpreted as a percent (can be > 100%) of the duration of the current prescription
                             # The follow-up window:
                             followup.window.start=0, # if a number is the earliest event per participant date + number of units, or a Date object, or a column name in data (NA = undefined)
                             followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
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
    if( !suppress.warnings ) warning(paste0("'CMA.to.apply' must be a string contining the name of the simple CMA to apply!\n)"));
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
                    {if( !suppress.warnings ) warning(paste0("Unknown 'CMA.to.apply' '",CMA.to.apply,"': defaulting to CMA0!\n)")); CMA0;}); # by default, fall back to CMA0

  # Default argument values and overrides:
  def.vals <- formals(CMA.FNC);
  if( CMA.to.apply %in% c("CMA1", "CMA2", "CMA3", "CMA4") )
  {
    carryover.into.obs.window <- carryover.within.obs.window <- FALSE;
    if( !is.na(carry.only.for.same.medication) && carry.only.for.same.medication && !suppress.warnings ) cat("Warning: 'carry.only.for.same.medication' cannot be defined for CMAs 1-4!\n");
    carry.only.for.same.medication <- FALSE;
    if( !is.na(consider.dosage.change) && consider.dosage.change && !suppress.warnings ) cat("Warning: 'consider.dosage.change' cannot be defined for CMAs 1-4!\n");
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
    if( !suppress.warnings ) warning("I know how to do CMA per episodes only for CMAs 1 to 9!\n");
    return (NULL);
  }

  # Create the return value skeleton and check consistency:
  ret.val <- CMA0(data,
                  ID.colname=ID.colname,
                  event.date.colname=event.date.colname,
                  event.duration.colname=event.duration.colname,
                  event.daily.dose.colname=event.daily.dose.colname,
                  medication.class.colname=medication.class.colname,
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
                  suppress.warnings=suppress.warnings,
                  summary=NA);
  if( is.null(ret.val) ) return (NULL);


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
                                             maximum.permissible.gap=maximum.permissible.gap,
                                             maximum.permissible.gap.unit=maximum.permissible.gap.unit,
                                             followup.window.start=followup.window.start,
                                             followup.window.start.unit=followup.window.start.unit,
                                             followup.window.duration=followup.window.duration,
                                             followup.window.duration.unit=followup.window.duration.unit,
                                             date.format=date.format,
                                             parallel.backend="none", # make sure this runs sequentially!
                                             parallel.threads=1,
                                             suppress.warnings=suppress.warnings,
                                             return.data.table=TRUE);
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
    treat.epi <- merge(treat.epi, event.info2[,c(ID.colname, ".OBS.START.DATE", ".OBS.END.DATE"),with=FALSE], all.x=TRUE);
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

    # Add back the patient and episode IDs:
    tmp <- as.data.table(merge(cma$CMA, treat.epi)[,c(ID.colname, "episode.ID", "episode.start", "end.episode.gap.days", "episode.duration", "episode.end", "CMA")]);
    setkeyv(tmp, c(ID.colname,"episode.ID"));
    return (list("CMA"=as.data.frame(tmp), "event.info"=as.data.frame(event.info2)[,c(ID.colname, ".FU.START.DATE", ".FU.END.DATE", ".OBS.START.DATE", ".OBS.END.DATE")]));
  }

  # Convert to data.table, cache event dat as Date objects, and key by patient ID and event date
  data.copy <- data.table(data);
  data.copy[, .DATE.as.Date := as.Date(get(event.date.colname),format=date.format)]; # .DATE.as.Date: convert event.date.colname from formatted string to Date
  setkeyv(data.copy, c(ID.colname, ".DATE.as.Date")); # key (and sorting) by patient ID and event date

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
  if( is.null(tmp) || is.null(tmp$CMA) || is.null(tmp$event.info) ) return (NULL);

  # Construct the return object:
  class(ret.val) <- "CMA_per_episode";
  ret.val$event.info <- as.data.frame(tmp$event.info);
  ret.val$computed.CMA <- CMA.to.apply;
  ret.val$summary <- summary;
  ret.val$CMA <- as.data.frame(tmp$CMA);
  setnames(ret.val$CMA, 1, ID.colname);

  return (ret.val);
}

#' @export
getCMA.CMA_per_episode <- function(x)
{
  cma <- x; # parameter x is required for S3 consistency, but I like cma more
  if( is.null(cma) || !inherits(cma, "CMA_per_episode") || !("CMA" %in% names(cma)) || is.null(cma$CMA) ) return (NULL);
  return (cma$CMA);
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
                                  cma.type=class(cma)[1]
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
            if( !is.null(cma[[p]]) && !is.na(cma[[p]]) )
            {
              if( p != "CMA" )
              {
                cat(paste0("    ",p," = ",cma[[p]],"\n"));
              } else
              {
                cat(paste0("    ",p," = CMA results for ",nrow(cma[[p]])," patients\n"));
              }
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
    warning("Unknown format for printing!\n");
    return (invisible(NULL));
  }
}

.plot.CMAintervals <- function(cma,                                   # the CMA_per_episode or CMA_sliding_window (or derived) object
                               patients.to.plot=NULL,                 # list of patient IDs to plot or NULL for all
                               duration=NA,                           # duration and end period to plot in days (if missing, determined from the data)
                               align.all.patients=FALSE, align.first.event.at.zero=TRUE, # should all patients be aligned? and, if so, place the first event as the horizontal 0?
                               show.period=c("dates","days")[2],      # draw vertical bars at regular interval as dates or days?
                               period.in.days=90,                     # the interval (in days) at which to draw veritcal lines
                               show.legend=TRUE, legend.x="right", legend.y="bottom", legend.bkg.opacity=0.5, # legend params and position
                               cex=1.0, cex.axis=0.75, cex.lab=1.0,   # various graphical params
                               show.cma=TRUE,                         # show the CMA type
                               col.cats=cm.colors,         # single color or a function mapping the categories to colors
                               unspecified.category.label="drug",     # the label of the unspecified category of medication
                               lty.event="solid", lwd.event=2, pch.start.event=15, pch.end.event=16, # event style
                               show.event.intervals=TRUE,             # show the actual rpescription intervals
                               col.na="lightgray",                    # color for mising data
                               col.continuation="black", lty.continuation="dotted", lwd.continuation=1, # style of the contuniation lines connecting consecutive events
                               print.CMA=TRUE, CMA.cex=0.50, # print CMA next to the participant's ID?
                               plot.CMA=TRUE,                   # plot the CMA next to the participant ID?
                               plot.CMA.as.histogram=TRUE,      # plot CMA as a histogram or as a density plot?
                               CMA.plot.ratio=0.10,             # the proportion of the total horizontal plot to be taken by the CMA plot
                               CMA.plot.col="lightgreen", CMA.plot.border="darkgreen", CMA.plot.bkg="aquamarine", CMA.plot.text=CMA.plot.border, # attributes of the CMA plot
                               highlight.followup.window=TRUE, followup.window.col="green",
                               highlight.observation.window=TRUE, observation.window.col="yellow", observation.window.density=35, observation.window.angle=-30,
                               show.real.obs.window.start=TRUE, real.obs.window.density=35, real.obs.window.angle=30, # for some CMAs, the real observation window starts at a different date
                               bw.plot=FALSE,                         # if TRUE, override all user-given colors and replace them with a scheme suitable for grayscale plotting
                               min.plot.size.in.characters.horiz=20, min.plot.size.in.characters.vert=15,  # the minimum plot size (in character)
                               max.patients.to.plot=100,        # maximum number of patients to plot
                               ...
)
{
  if( is.null(cma) || !(inherits(cma, "CMA_per_episode") || inherits(cma, "CMA_sliding_window")) || is.null(cma$data) || nrow(cma$data) < 1 ||
      is.na(cma$ID.colname) || !(cma$ID.colname %in% names(cma$data)) ||
      is.na(cma$event.date.colname) || !(cma$event.date.colname %in% names(cma$data)) ||
      !("event.info" %in% names(cma)) || is.null(cma$event.info) ) return (plot.CMA0(cma,...));

  # Check compatibility between subtypes of plots:
  if( align.all.patients && show.period != "days" ){ show.period <- "days"; warning("When aligning all patients, cannot show actual dates: showing days instead!\n"); }

  # Depeding on the cma's exact type, the relevant columns might be different: homogenize them for later use
  cmas <- cma$CMA;
  if( inherits(cma, "CMA_per_episode") )
  {
    names(cmas)[2:ncol(cmas)] <- c("WND.ID", "start", "gap.days", "duration", "end", "CMA"); # avoid possible conflict with patients being called "ID"
  } else if( inherits(cma, "CMA_sliding_window") )
  {
    cmas <- cbind(cmas[,1:3], "gap.days"=NA, "duration"=cma$sliding.window.duration, cmas[,4:ncol(cmas)]);
    names(cmas)[2:ncol(cmas)] <- c("WND.ID", "start", "gap.days", "duration", "end", "CMA"); # avoid possible conflict with patients being called "ID"
  }
  # add also the follow-up and observation window infor as well to have everything in one place:
  cmas <- cbind(cmas, do.call(rbind, lapply(1:nrow(cmas), function(i)
  {
    s <- which(cma$event.info[,cma$ID.colname] == cmas[i,cma$ID.colname]);
    if( length(s) != 1 ) return (NULL);
    cma$event.info[s,c(".FU.START.DATE", ".FU.END.DATE", ".OBS.START.DATE", ".OBS.END.DATE")];
  })));

  # Make sure the dates are strings of the right fomat:
  if( inherits(cma$data[,cma$event.date.colname], "Date") )
  {
    cma$date.format <- "%m/%d/%Y"; # use the default format
    cma$data[,cma$event.date.colname] <- as.character(cma$data[,cma$event.date.colname], format=cma$date.format);
  }

  # The patients:
  patids <- unique(cmas[,cma$ID.colname]); patids <- patids[!is.na(patids)];
  if( !is.null(patients.to.plot) ) patids <- intersect(as.character(patids), as.character(patients.to.plot));
  if( length(patids) == 0 )
  {
    cat("No patients to plot!\n");
    return (invisible(NULL));
  } else if( length(patids) > max.patients.to.plot )
  {
    cat(paste0("Too many patients to plot (",length(patids),
               ")! If you really want that, please change the 'max.patients.to.plot' parameter value (now set at ",
               max.patients.to.plot,"!\n"));
    return (invisible(NULL));
  }
  # Select only the patients to display:
  cma$data <- cma$data[ cma$data[,cma$ID.colname] %in% patids, ];
  cmas <- cmas[ cmas[,cma$ID.colname] %in% patids, ];
  # Make sure the patients are ordered by ID and date:
  cma$data <- cma$data[ order( cma$data[,cma$ID.colname], as.Date(cma$data[,cma$event.date.colname],format=cma$date.format)), ];
  cmas <- cmas[ order( cmas[,cma$ID.colname], cmas$WND.ID, cmas$start), ];

  # Grayscale plotting:
  if( bw.plot )
  {
      if( is.function(col.cats) ) col.cats <- .bw.colors else col.cats <- gray(0.1);
      followup.window.col <- "black";
      observation.window.col <- gray(0.3);
      CMA.plot.col <- gray(0.8);
      CMA.plot.border <- gray(0.2);
      CMA.plot.bkg <- gray(0.5);
      CMA.plot.text <- CMA.plot.border;
  }

  # The colors for the categories:
  if( is.na(cma$medication.class.colname) || !(cma$medication.class.colname %in% names(cma$data)) )
  {
    categories <- unspecified.category.label;
  } else
  {
    categories <- sort(unique(as.character(cma$data[,cma$medication.class.colname])), na.last=FALSE); # all categories making sure NA is first
  }
  if( is.na(categories[1]) )
  {
    if( is.function(col.cats) ) cols <- c(col.na, col.cats(length(categories)-1)) else cols <- c(col.na, rep(col.cats,length(categories)-1));
  } else
  {
    if( is.function(col.cats) ) cols <- col.cats(length(categories)) else cols <- rep(col.cats,length(categories));
  }
  names(cols) <- categories;
  .map.category.to.color <- function( category ) ifelse( is.na(category), cols[1], ifelse( category %in% names(cols), cols[category], "black") );

  # Make sure we are using actual dates:
  if( !inherits(cma$data[,cma$event.date.colname], "Date") )
  {
    cma$data$.DATE.as.Date <- as.Date(cma$data[,cma$event.date.colname],format=cma$date.format);
  } else
  {
    cma$data$.DATE.as.Date <- cma$data[,cma$event.date.colname];
  }
  # Find the earliest date:
  earliest.date <- min(cma$data$.DATE.as.Date, cmas$start, cmas$.OBS.START.DATE, cmas$.FU.START.DATE);

  # If aligning all participants to the same date, simply relocate all dates relative to the earliest date:
  if( align.all.patients )
  {
    for( i in 1:nrow(cma$data) )
    {
      if( i == 1 || cma$data[i,cma$ID.colname] != cma$data[i-1,cma$ID.colname] )
      {
        align.to <- cma$data$.DATE.as.Date[i];
        for( j in which(cmas[,cma$ID.colname] == cma$data[i,cma$ID.colname]) )
        {
          cmas$start[j]           <- earliest.date + (cmas$start[j]           - align.to);
          cmas$end[j]             <- earliest.date + (cmas$end[j]             - align.to);
          cmas$.FU.START.DATE[j]  <- earliest.date + (cmas$.FU.START.DATE[j]  - align.to);
          cmas$.FU.END.DATE[j]    <- earliest.date + (cmas$.FU.END.DATE[j]    - align.to);
          cmas$.OBS.START.DATE[j] <- earliest.date + (cmas$.OBS.START.DATE[j] - align.to);
          cmas$.OBS.END.DATE[j]   <- earliest.date + (cmas$.OBS.END.DATE[j]   - align.to);
        }
      }
      cma$data$.DATE.as.Date[i] <- earliest.date + (cma$data$.DATE.as.Date[i] - align.to);
    }
    correct.earliest.followup.window <- min(cma$data$.DATE.as.Date - min(cmas$.FU.START.DATE,na.rm=TRUE),na.rm=TRUE);
  } else
  {
    correct.earliest.followup.window <- 0;
  }

  # Compute the duration if not given:
  if( is.na(duration) )
  {
    latest.date <- max(c(cmas$end, cmas$.FU.END.DATE, cmas$.OBS.END.DATE),na.rm=TRUE);
    duration <- as.numeric(latest.date - earliest.date) + correct.earliest.followup.window;
  }
  endperiod <- duration;

  # Reserve space for the CMA plotting:
  adh.plot.space <- c(0, ifelse( plot.CMA && !is.null(getCMA(cma)), duration*CMA.plot.ratio, 0) );
  duration.total <- duration + adh.plot.space[2];

  # The actual plotting:
  if(inherits(msg <- try(plot( 0, 1,
                               xlim=c(0-2*duration.total/100,duration.total), xaxs="i",
                               ylim=c(0,nrow(cma$data)+ifelse(plot.CMA && !is.null(getCMA(cma)), nrow(cmas), 0)+1), yaxs="i", type="n",
                               axes=FALSE,
                               xlab="", ylab="" ),
                         silent=TRUE),
              "try-error"))
  {
    # Some error occured when creatig the plot...
    cat(msg);
    return (invisible(NULL));
  }

  # Character width and height in the current plotting system:
  char.width <- strwidth("O",cex=cex); char.height <- strheight("O",cex=cex);
  char.height.CMA <- strheight("0",cex=CMA.cex);

  # Minimum plot dimensions:
  if( abs(par("usr")[2] - par("usr")[1]) <= char.width * min.plot.size.in.characters.horiz ||
      abs(par("usr")[4] - par("usr")[3]) <= char.height * min.plot.size.in.characters.vert * length(patids))
  {
    cat(paste0("Plotting area is too small (it must be at least ",
               min.plot.size.in.characters.horiz,
               " x ",
               min.plot.size.in.characters.vert,
               " characters per patient, but now it is only ",
               round(abs(par("usr")[2] - par("usr")[1]) / char.width,1),
               " x ",
               round(abs(par("usr")[4] - par("usr")[3]) / (char.height * length(patids)),1),
               ")!\n"));
    #segments(x0=c(par("usr")[1], par("usr")[1]),
    #         y0=c(par("usr")[3], par("usr")[4]),
    #         x1=c(par("usr")[2], par("usr")[2]),
    #         y1=c(par("usr")[4], par("usr")[3]),
    #         col="red", lwd=3);
    return (invisible(NULL));
  }

  # Continue plotting:
  box();
  title(main=paste0(ifelse(align.all.patients, "Event patterns (all patients aligned)", "Event patterns"),
                    ifelse(show.cma,paste0(" ",
                                           switch(class(cma)[1],
                                                  "CMA_sliding_window"="sliding window",
                                                  "CMA_per_episode"="per episode"),
                                           " (",cma$computed.CMA,")"),"")),
        xlab=ifelse(show.period=="dates","","days"),
        ylab=ifelse((print.CMA || plot.CMA) && !is.null(getCMA(cma)),"patient (& CMA)","patient"),
        cex.lab=cex.lab);

  # The patient axis and CMA plots:
  if( plot.CMA && !is.null(getCMA(cma)) )
  {
    # Maximum achieved CMA:
    adh.max <- 1.0;
    # Function mapping the CMA values to the appropriate x-coordinates:
    .rescale.xcoord.for.CMA.plot <- function(x,pfree=0.20) return (adh.plot.space[1] + x/adh.max*(adh.plot.space[2]*(1-pfree) - adh.plot.space[1]));
  }
  draw.gray.band <- FALSE;
  y.cur <- 1;
  for( p in as.character(patids) )
  {
    # The participant axis text:
    s <- which(cma$data[,cma$ID.colname] == p);
    x <- which(cmas[cma$ID.colname] == p);
    pid <- p;
    mtext( pid, 2, line=0.5, at=y.cur+length(s)/2+ifelse(plot.CMA && !is.null(getCMA(cma)),length(x),0)/2, las=2, cex=cex.axis );

    # The alternating gray bands:
    if( draw.gray.band )
      rect( 0-1, y.cur-0.5, duration.total+1, y.cur+length(s)+ifelse(plot.CMA && !is.null(getCMA(cma)),length(x),0)-0.5, col=gray(0.95), border=NA );
    draw.gray.band <- !draw.gray.band;

    # The participant CMA plot:
    if( plot.CMA && !is.null(getCMA(cma)) )
    {
      y.mean <- y.cur+length(s)/2+ifelse(plot.CMA && !is.null(getCMA(cma)),length(x),0)/2;
      segments(.rescale.xcoord.for.CMA.plot(0), y.mean-2, .rescale.xcoord.for.CMA.plot(1), y.mean-2, lty="solid", col=CMA.plot.col);
      segments(.rescale.xcoord.for.CMA.plot(0), y.mean+2, .rescale.xcoord.for.CMA.plot(1), y.mean+2, lty="solid", col=CMA.plot.col);
      adh <- na.omit(getCMA(cma)[x,"CMA"]);
      # Scale the CMA (itself or density) in such a way that if within 0..1 stays within 0..1 but scales if it goes outside this interval to accomodate it
      if( plot.CMA.as.histogram )
      {
        # Plot CMA as histogram:
        if( length(adh) > 0 )
        {
          adh.hist <- hist(adh, plot=FALSE);
          adh.x <- adh.hist$breaks[-1]; adh.x.0 <- min(adh.x,0); adh.x.1 <- max(adh.x,1); adh.x <- (adh.x - adh.x.0) / (adh.x.1 - adh.x.0);
          adh.y <- adh.hist$counts; adh.y <- adh.y / max(adh.y);
          adh.x.max <- adh.x[which.max(adh.hist$counts)];
          segments(.rescale.xcoord.for.CMA.plot(adh.x), y.mean-2, .rescale.xcoord.for.CMA.plot(adh.x), y.mean-2 + 4*adh.y, lty="solid", lwd=1, col=CMA.plot.border);
          if( char.height.CMA <= abs(.rescale.xcoord.for.CMA.plot(1) - .rescale.xcoord.for.CMA.plot(0)) )
          {
            # enough space for vertical writing all three of them:
            text(x=.rescale.xcoord.for.CMA.plot(0),         y.mean-2-char.height.CMA/2,
                 sprintf("%.1f%%",100*min(adh.x.0,na.rm=TRUE)), srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
            text(x=.rescale.xcoord.for.CMA.plot(1),         y.mean-2-char.height.CMA/2,
                 sprintf("%.1f%%",100*max(adh.x.1,na.rm=TRUE)), srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
            text(x=.rescale.xcoord.for.CMA.plot(adh.x.max), y.mean+2+char.height.CMA/2,
                 sprintf("%d",max(adh.hist$counts,an.rm=TRUE)), srt=90, pos=3, cex=CMA.cex, col=CMA.plot.text);
          }
        }
      } else
      {
        if( length(adh) > 2 )
        {
          # Plot CMA as density plot:
          #adh.density <- density(adh, from=min(adh,na.rm=TRUE), to=max(adh,na.rm=TRUE));
          adh.density <- density(adh);
          ss <- (adh.density$x >= min(adh,na.rm=TRUE) & adh.density$x <= max(adh,na.rm=TRUE));
          if( sum(ss) == 0 )
          {
            # probably constant numbers?
            # Plot the individual lines:
            #if( length(adh) == 1 || isTRUE(all.equal(min(adh), max(adh))) ) adh.x <- pmax(pmin(adh,1.0),0.0) else adh.x <- (adh - min(adh)) / (max(adh) - min(adh));
            adh.x.0 <- min(adh,0); adh.x.1 <- max(adh,1); adh.x <- (adh - adh.x.0) / (adh.x.1 - adh.x.0);
            segments(.rescale.xcoord.for.CMA.plot(adh.x), y.mean-2, .rescale.xcoord.for.CMA.plot(adh.x), y.mean-2 + 4, lty="solid", lwd=2, col=CMA.plot.border);
            if( char.height.CMA*length(adh) <= abs(.rescale.xcoord.for.CMA.plot(1) - .rescale.xcoord.for.CMA.plot(0)) )
            {
              # enough space for vertical writing all of them:
              for( i in 1:length(adh) )
              {
                text(x=.rescale.xcoord.for.CMA.plot(adh.x[i]), y.mean+ifelse(i %% 2==0,2+char.height.CMA/2,-2-char.height.CMA/2),
                     sprintf("%.1f%%",100*adh[i]), srt=90, pos=ifelse(i %% 2==0,3,1), cex=CMA.cex, col=CMA.plot.text);
              }
            } else if( char.height.CMA <= abs(.rescale.xcoord.for.CMA.plot(1) - .rescale.xcoord.for.CMA.plot(0)) )
            {
              # enough space for vertical writing only the extremes:
              text(x=.rescale.xcoord.for.CMA.plot(adh.x[1]),           y.mean-2-char.height.CMA/2,
                   sprintf("%.1f%%",100*adh[1]),           srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
              text(x=.rescale.xcoord.for.CMA.plot(adh.x[length(adh)]), y.mean-2-char.height.CMA/2,
                   sprintf("%.1f%%",100*adh[length(adh)]), srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
            }
          } else
          {
            adh.density$x <- adh.density$x[ss]; adh.density$y <- adh.density$y[ss];
            #adh.x <- adh.density$x; adh.x <- (adh.x - min(adh.x)) / (max(adh.x) - min(adh.x));
            adh.x <- adh.density$x; adh.x.0 <- min(adh.x,0); adh.x.1 <- max(adh.x,1); adh.x <- (adh.x - adh.x.0) / (adh.x.1 - adh.x.0);
            adh.y <- adh.density$y; adh.y <- (adh.y - min(adh.y)) / (max(adh.y) - min(adh.y));
            points(.rescale.xcoord.for.CMA.plot(adh.x), y.mean-2 + 4*adh.y, type="l", col=CMA.plot.border);
            if( char.height.CMA <= abs(.rescale.xcoord.for.CMA.plot(1) - .rescale.xcoord.for.CMA.plot(0)) )
            {
              # enough space for vertical writing:
              text(x=.rescale.xcoord.for.CMA.plot(0), y.mean-2-char.height.CMA/2, sprintf("%.1f%%",100*adh.x.0), srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
              text(x=.rescale.xcoord.for.CMA.plot(1), y.mean-2-char.height.CMA/2, sprintf("%.1f%%",100*adh.x.1), srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
            }
          }
        } else
        {
          #rect(.rescale.xcoord.for.CMA.plot(0), y.mean-2, .rescale.xcoord.for.CMA.plot(1), y.mean+2, border=NA, col=CMA.plot.col, density=25);
          if( length(adh) == 0 )
          {
            # No points: nothing to plot!
          } else
          {
            # Plot the individual lines:
            #if( length(adh) == 1 || isTRUE(all.equal(min(adh), max(adh))) ) adh.x <- pmax(pmin(adh,1.0),0.0) else adh.x <- (adh - min(adh)) / (max(adh) - min(adh));
            adh.x.0 <- min(adh,0); adh.x.1 <- max(adh,1); adh.x <- (adh - adh.x.0) / (adh.x.1 - adh.x.0);
            segments(.rescale.xcoord.for.CMA.plot(adh.x), y.mean-2, .rescale.xcoord.for.CMA.plot(adh.x), y.mean-2 + 4, lty="solid", lwd=2, col=CMA.plot.border);
            if( char.height.CMA*length(adh) <= abs(.rescale.xcoord.for.CMA.plot(1) - .rescale.xcoord.for.CMA.plot(0)) )
            {
              # enough space for vertical writing all of them:
              for( i in 1:length(adh) )
              {
                text(x=.rescale.xcoord.for.CMA.plot(adh.x[i]), y.mean+ifelse(i %% 2==0,2+char.height.CMA/2,-2-char.height.CMA/2),
                     sprintf("%.1f%%",100*adh[i]), srt=90, pos=ifelse(i %% 2==0,3,1), cex=CMA.cex, col=CMA.plot.text);
              }
            } else if( char.height.CMA <= abs(.rescale.xcoord.for.CMA.plot(1) - .rescale.xcoord.for.CMA.plot(0)) )
            {
              # enough space for vertical writing only the extremes:
              text(x=.rescale.xcoord.for.CMA.plot(adh.x[1]),           y.mean-2-char.height.CMA/2,
                   sprintf("%.1f%%",100*adh[1]),           srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
              text(x=.rescale.xcoord.for.CMA.plot(adh.x[length(adh)]), y.mean-2-char.height.CMA/2,
                   sprintf("%.1f%%",100*adh[length(adh)]), srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
            }
          }
        }
      }
    }

    # The follow-up and observation windows:
    if( highlight.followup.window )
      rect(adh.plot.space[2] + as.numeric(cmas$.FU.START.DATE[x[1]] - earliest.date) + correct.earliest.followup.window, y.cur-0.25,
           adh.plot.space[2] + as.numeric(cmas$.FU.END.DATE[x[1]] - earliest.date) + correct.earliest.followup.window, y.cur+length(s)+0.25,
           col=NA, border=followup.window.col, lty="dashed", lwd=2);
    if( highlight.observation.window )
    {
      # The given observation window:
      rect(adh.plot.space[2] + as.numeric(cmas$.OBS.START.DATE[x[1]] - earliest.date) + correct.earliest.followup.window, y.cur-0.25,
           adh.plot.space[2] + as.numeric(cmas$.OBS.END.DATE[x[1]] - earliest.date) + correct.earliest.followup.window, y.cur+length(s)+0.25,
           col=adjustcolor(observation.window.col,alpha.f=0.3), border=NA, density=observation.window.density, angle=observation.window.angle);
    }

    y.cur <- y.cur + length(s) + ifelse(plot.CMA && !is.null(getCMA(cma)),length(x),0);
  }
  if( plot.CMA && !is.null(getCMA(cma)) )
  {
    # Mark the drawing area:
    #rect(.rescale.xcoord.for.CMA.plot(0), par("usr")[3], .rescale.xcoord.for.CMA.plot(1.0), par("usr")[4], col=adjustcolor(CMA.plot.bkg,alpha.f=0.25), border=NA);
    abline(v=c(.rescale.xcoord.for.CMA.plot(0), .rescale.xcoord.for.CMA.plot(1.0)), col=CMA.plot.col, lty="dotted", lwd=1);
    #mtext( c("0%","100%"), 3, line=0.5, at=c(.rescale.xcoord.for.CMA.plot(0), .rescale.xcoord.for.CMA.plot(1.0)), las=2, cex=cex.axis, col=CMA.plot.border );
  }

  # Plot each event:
  curpat <- TRUE;
  y.cur <- 1;
  for( i in 1:nrow(cma$data) )
  {
    start <- as.numeric(cma$data$.DATE.as.Date[i] - earliest.date);
    end <- start + cma$data[i,cma$event.duration.colname];
    col <- .map.category.to.color(ifelse(is.na(cma$medication.class.colname) || !(cma$medication.class.colname %in% names(cma$data)),unspecified.category.label,cma$data[i,cma$medication.class.colname]));
    points( adh.plot.space[2]+start+correct.earliest.followup.window, y.cur, pch=pch.start.event, col=col, cex=cex);
    points(adh.plot.space[2]+end+correct.earliest.followup.window, y.cur, pch=pch.end.event, col=col, cex=cex);
    segments( adh.plot.space[2]+start+correct.earliest.followup.window, y.cur, adh.plot.space[2]+end+correct.earliest.followup.window, y.cur, col=col, lty=lty.event, lwd=lwd.event);
    y.cur <- y.cur + 1;

    if( i < nrow(cma$data) )
    {
      if( cma$data[i,cma$ID.colname] == cma$data[i+1,cma$ID.colname] )
      {
        # Extend the line
        start.next <- as.numeric(cma$data$.DATE.as.Date[i+1] - earliest.date);
        segments( adh.plot.space[2]+end+correct.earliest.followup.window, y.cur-1, adh.plot.space[2]+start.next+correct.earliest.followup.window, y.cur-1,
                  col=col.continuation, lty=lty.continuation, lwd=lwd.continuation);
        segments( adh.plot.space[2]+start.next+correct.earliest.followup.window, y.cur-1, adh.plot.space[2]+start.next+correct.earliest.followup.window, y.cur,
                  col=col.continuation, lty=lty.continuation, lwd=lwd.continuation);
      } else
      {
        # Now the patient is changing:
        # Draw its subperiods:
        if( plot.CMA && !is.null(getCMA(cma)) )
        {
          s <- which(cmas[,cma$ID.colname] == cma$data[i,cma$ID.colname]);
          if( length(s) > 0 )
          {
            for( j in 1:length(s) )
            {
              start <- as.numeric(cmas$start[s[j]] - earliest.date);
              end <- as.numeric(cmas$end[s[j]] - earliest.date);
              rect( adh.plot.space[2]+start+correct.earliest.followup.window, y.cur+0.10, adh.plot.space[2]+end+correct.earliest.followup.window, y.cur+0.90, border=gray(0.7), col="white");
              if( !is.na(cmas$CMA[s[j]]) )
              {
                h <- start + (end - start)*max(c(min(c(cmas$CMA[s[j]],1.0)),0.0));
                rect( adh.plot.space[2]+start+correct.earliest.followup.window, y.cur+0.10,
                      adh.plot.space[2]+h+correct.earliest.followup.window, y.cur+0.90, border=gray(0.3), col=gray(0.9));
                if( print.CMA && char.height.CMA <= 0.80 )
                {
                  text( adh.plot.space[2]+(start+end)/2+correct.earliest.followup.window, y.cur+0.5, sprintf("%.1f%%",100*cmas$CMA[s[j]]), cex=CMA.cex);
                }
              }
              y.cur <- y.cur+1;
            }
          }
        }
        curpat <- !curpat;
      }
    }
  }
  # Draw the last participant's subperiods:
  if( plot.CMA && !is.null(getCMA(cma)) )
  {
    s <- which(cmas[,cma$ID.colname] == cma$data[i,cma$ID.colname]);
    if( length(s) > 0 )
    {
      for( j in 1:length(s) )
      {
        start <- as.numeric(cmas$start[s[j]] - earliest.date);
        end <- as.numeric(cmas$end[s[j]] - earliest.date);
        rect( adh.plot.space[2]+start+correct.earliest.followup.window, y.cur+0.10, adh.plot.space[2]+end+correct.earliest.followup.window, y.cur+0.90, border=gray(0.7), col="white");
        if( !is.na(cmas$CMA[s[j]]) )
        {
          h <- start + (end - start)*max(c(min(c(cmas$CMA[s[j]],1.0)),0.0));
          rect( adh.plot.space[2]+start+correct.earliest.followup.window, y.cur+0.10, adh.plot.space[2]+h+correct.earliest.followup.window, y.cur+0.90, border=gray(0.3), col=gray(0.9));
          if( print.CMA && char.height.CMA <= .80 )
          {
            text( adh.plot.space[2]+(start+end)/2+correct.earliest.followup.window, y.cur+0.5, sprintf("%.1f%%",100*cmas$CMA[s[j]]), cex=CMA.cex);
          }
        }
        y.cur <- y.cur+1;
      }
    }
  }

  # The days/dates axis and the grid at those important days/dates:
  if( show.period=="dates" )
  {
      axis( 1, at=adh.plot.space[2]+seq(0,as.numeric(endperiod),by=period.in.days),
            labels=as.character(earliest.date + round(seq(0,as.numeric(endperiod),by=period.in.days),1), format=cma$date.format),
            las=3, cex.axis=cex.axis);
      abline( v=adh.plot.space[2]+seq(0,as.numeric(endperiod),by=period.in.days), lty="dotted", col=gray(0.5) );
      abline( v=adh.plot.space[2]+endperiod, lty="solid", col=gray(0.5) );
  } else
  {
      if( align.first.event.at.zero )
      {
          xpos <- c(correct.earliest.followup.window-seq(0,as.numeric(correct.earliest.followup.window),by=period.in.days),
                    seq(0,as.numeric(endperiod),by=period.in.days)+correct.earliest.followup.window);
          xpos <- xpos[ xpos >= 0 & xpos <= endperiod ];
          axis( 1, at=adh.plot.space[2]+xpos,
                labels=as.character(round(xpos-correct.earliest.followup.window,1)),
                las=3, cex.axis=cex.axis);
          abline( v=adh.plot.space[2]+xpos, lty="dotted", col=gray(0.5) );
          abline( v=adh.plot.space[2]+endperiod, lty="solid", col=gray(0.5) );
      } else
      {
          axis( 1, at=adh.plot.space[2]+seq(0,as.numeric(endperiod),by=period.in.days),
                labels=as.character(round(seq(0,as.numeric(endperiod),by=period.in.days),1)),
                las=3, cex.axis=cex.axis);
          abline( v=adh.plot.space[2]+seq(0,as.numeric(endperiod),by=period.in.days), lty="dotted", col=gray(0.5) );
          abline( v=adh.plot.space[2]+endperiod, lty="solid", col=gray(0.5) );
      }
  }

  # The legend:
  .legend <- function(x=0, y=0, width=1, height=1, do.plot=TRUE)
  {
    # Legend rectangle:
    if( do.plot ) rect(x, y, x + width, y + height, border=gray(0.6), lwd=2, col=rgb(0.99,0.99,0.99,legend.bkg.opacity));

    cur.y <- y + height; # current y
    max.width <- width; # maximum width

    # Legend title:
    if( do.plot ) text(x + width/2, cur.y, "Legend", pos=1, col=gray(0.3), cex=1.0);
    cur.y <- cur.y - 4*char.height; max.width <- max(max.width, strwidth("Legend", cex=1.0));

    # Event:
    if( do.plot ) segments(x + 1.0*char.width, cur.y, x + 4.0*char.width, cur.y, lty=lty.event, lwd=lwd.event, col="black");
    if( do.plot ) points(x + 1.0*char.width, cur.y, pch=pch.start.event, cex=1.0, col="black");
    if( do.plot ) points(x + 4.0*char.width, cur.y, pch=pch.end.event, cex=1.0, col="black");
    if( do.plot ) text(x + 5.0*char.width, cur.y, "duration", col="black", cex=0.75, pos=4);
    cur.y <- cur.y - 1.5*char.height; max.width <- max(max.width, 5.0*char.width + strwidth("duration", cex=0.75));
    if( do.plot ) segments(x + 1.0*char.width, cur.y - 0.5*char.height, x + 3.0*char.width, cur.y - 0.5*char.height, lty=lty.continuation, lwd=lwd.continuation, col=col.continuation);
    if( do.plot ) segments(x + 3.0*char.width, cur.y - 0.5*char.height, x + 3.0*char.width, cur.y + 0.5*char.height, lty=lty.continuation, lwd=lwd.continuation, col=col.continuation);
    if( do.plot ) text(x + 5.0*char.width, cur.y, "connector", col="black", cex=0.75, pos=4);
    cur.y <- cur.y - 1.5*char.height; max.width <- max(max.width, 5.0*char.width + strwidth("connector", cex=0.75));

    for( i in 1:length(cols) )
    {
      if( do.plot ) rect(x + 1.0*char.width, cur.y, x + 4.0*char.width, cur.y - 1.0*char.height, border="black", col=adjustcolor(cols[i],alpha.f=0.5));
      if( do.plot ) text(x + 5.0*char.width, cur.y - 0.5*char.height, names(cols)[i], col="black", cex=0.75, pos=4);
      cur.y <- cur.y - 1.5*char.height; max.width <- max(max.width, 5.0*char.width + strwidth(names(cols)[i], cex=0.75));
    }
    cur.y <- cur.y - 0.5*char.height;

    # Follow-up window:
    if( highlight.followup.window )
    {
      if( do.plot ) rect(x + 1.0*char.width, cur.y, x + 4.0*char.width, cur.y - 1.0*char.height, border=followup.window.col, lty="dotted", lwd=2, col=rgb(1,1,1,0.0));
      if( do.plot ) text(x + 5.0*char.width, cur.y - 0.5*char.height, "follow-up wnd.", col="black", cex=0.75, pos=4);
      cur.y <- cur.y - 2.0*char.height; max.width <- max(max.width, 5.0*char.width + strwidth("follow-up wnd.", cex=0.75));
    }

    # Observation window:
    if( highlight.observation.window )
    {
      if( inherits(cma,"CMA8") && !is.null(cma$real.obs.windows) && show.real.obs.window.start )
      {
        if( do.plot ) rect(x + 1.0*char.width, cur.y, x + 4.0*char.width, cur.y - 1.0*char.height, border=rgb(1,1,1,0.0), col=adjustcolor(observation.window.col,alpha.f=0.3), density=observation.window.density, angle=observation.window.angle);
        if( do.plot ) text(x + 5.0*char.width, cur.y - 0.5*char.height, "theor. obs. wnd.", col="black", cex=0.75, pos=4);
        cur.y <- cur.y - 1.5*char.height; max.width <- max(max.width, 5.0*char.width + strwidth("theor. obs. wnd.", cex=0.75));
        if( do.plot ) rect(x + 1.0*char.width, cur.y, x + 4.0*char.width, cur.y - 1.0*char.height, border=rgb(1,1,1,0.0), col=adjustcolor(observation.window.col,alpha.f=0.3), density=real.obs.window.density, angle=real.obs.window.angle);
        if( do.plot ) text(x + 5.0*char.width, cur.y - 0.5*char.height, "real obs.wnd.", col="black", cex=0.75, pos=4);
        cur.y <- cur.y - 2.0*char.height; max.width <- max(max.width, 5.0*char.width + strwidth("real obs.wnd.", cex=0.75));
      } else
      {
        if( do.plot ) rect(x + 1.0*char.width, cur.y, x + 4.0*char.width, cur.y - 1.0*char.height, border=rgb(1,1,1,0.0), col=adjustcolor(observation.window.col,alpha.f=0.3), density=observation.window.density, angle=observation.window.angle);
        if( do.plot ) text(x + 5.0*char.width, cur.y - 0.5*char.height, "observation wnd.", col="black", cex=0.75, pos=4);
        cur.y <- cur.y - 2.0*char.height; max.width <- max(max.width, 5.0*char.width + strwidth("observation wnd.", cex=0.75));
      }
    }

    # Required size:
    return (c("width" =max.width + 5.0*char.width,
              "height"=(y + height - cur.y) + 1.0*char.height));
  }
  if( show.legend )
  {
    legend.size <- .legend(do.plot=FALSE);
    if( is.na(legend.x) || legend.x == "right" )
    {
      legend.x <- par("usr")[2] - legend.size["width"] - char.width;
    } else if( legend.x == "left" )
    {
      legend.x <- par("usr")[1] + char.width;
    } else if( !is.numeric(legend.x) && length(legend.x) != 1 )
    {
      legend.x <- par("usr")[2] - legend.size["width"] - char.width;
    }
    if( is.na(legend.y) || legend.y == "bottom" )
    {
      legend.y <- par("usr")[3] + char.height;
    } else if( legend.y == "top" )
    {
      legend.y <- par("usr")[4] - legend.size["height"] - char.height;
    } else if( !is.numeric(legend.y) && length(legend.y) != 1 )
    {
      legend.y <- par("usr")[3] + char.height;
    }
    invisible(.legend(legend.x, legend.y, as.numeric(legend.size["width"]), as.numeric(legend.size["height"])));
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
#' drawn.
#' @param show.legend \emph{Logical}, should the legend be drawn?
#' @param legend.x The position of the legend on the x axis; can be "left",
#' "right" (default), or a \emph{numeric} value.
#' @param legend.y The position of the legend on the y axis; can be "bottom"
#' (default), "top", or a \emph{numeric} value.
#' @param legend.bkg.opacity A \emph{number} between 0.0 and 1.0 specifying the
#' opacity of the legend background.
#' @param cex,cex.axis,cex.lab \emph{numeric} values specifying the cex of the
#' various types of text.
#' @param show.cma \emph{Logical}, should the CMA type be shown in the title?
#' @param col.cats A \emph{color} or a \emph{function} that specifies the single
#' colour or the colour palette used to plot the different medication; by
#' default \code{\link[grDevices]{cm.colors}}.
#' @param unspecified.category.label A \emph{string} giving the name of the
#' unspecified (generic) medication category.
#' @param lty.event,lwd.event,pch.start.event,pch.end.event The style of the
#' event (line style, width, and start and end symbols).
#' @param show.event.intervals \emph{Logical}, should the actual event intervals
#' be shown?
#' @param col.na The colour used for missing event data.
#' @param col.continuation,lty.continuation,lwd.continuation The color, style
#' and width of the contuniation lines connecting consecutive events.
#' @param bw.plot \emph{Logical}, should the plot use grayscale only (i.e., the
#' \code{\link[grDevices]{gray.colors}} function)?
#' @param print.CMA \emph{Logical}, should the CMA values be printed?
#' @param CMA.cex ... and, if printed, what cex (\emph{numeric}) to use?
#' @param plot.CMA \emph{Logical}, should the CMA values be represented
#' graphically?
#' @param plot.CMA.as.histogram \emph{Logical}, should the CMA plot be a
#' histogram or a (truncated) density plot? Please note that it is TRUE by
#' deafult for CMA_per_episode and FALSE for CMA_sliding_window, because
#' usually there are more sliding windows than episodes. Also, the density
#' estimate canot be estimated for less than three different values.
#' @param CMA.plot.ratio A \emph{number}, the proportion of the total horizontal
#' plot space to be allocated to the CMA plot.
#' @param CMA.plot.col,CMA.plot.border,CMA.plot.bkg,CMA.plot.text \emph{Strings}
#' giving the colours of the various components of the CMA plot.
#' @param highlight.followup.window \emph{Logical}, should the follow-up window
#' be plotted?
#' @param followup.window.col The follow-up window colour.
#' @param highlight.observation.window \emph{Logical}, should the observation
#' window be plotted?
#' @param observation.window.col,observation.window.density,observation.window.angle Attributes of the observation window
#' (colour, shading density and angle).
#' @param show.real.obs.window.start,real.obs.window.density,real.obs.window.angle For some CMAs, the observation window
#' might be adjusted, in which case should it be plotted and with that attributes?
#' @param ... other possible parameters
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
                                 ...,                                   # required for S3 consistency
                                 patients.to.plot=NULL,                 # list of patient IDs to plot or NULL for all
                                 duration=NA,                           # duration and end period to plot in days (if missing, determined from the data)
                                 align.all.patients=FALSE, align.first.event.at.zero=TRUE, # should all patients be aligned? and, if so, place the first event as the horizintal 0?
                                 show.period=c("dates","days")[2],      # draw vertical bars at regular interval as dates or days?
                                 period.in.days=90,                     # the interval (in days) at which to draw veritcal lines
                                 show.legend=TRUE, legend.x="right", legend.y="bottom", legend.bkg.opacity=0.5, # legend params and position
                                 cex=1.0, cex.axis=0.75, cex.lab=1.0,   # various graphical params
                                 show.cma=TRUE,                         # show the CMA type
                                 col.cats=cm.colors,         # single color or a function mapping the categories to colors
                                 unspecified.category.label="drug",     # the label of the unspecified category of medication
                                 lty.event="solid", lwd.event=2, pch.start.event=15, pch.end.event=16, # event style
                                 show.event.intervals=TRUE,             # show the actual rpescription intervals
                                 col.na="lightgray",                    # color for mising data
                                 col.continuation="black", lty.continuation="dotted", lwd.continuation=1, # style of the contuniation lines connecting consecutive events
                                 print.CMA=TRUE, CMA.cex=0.50,    # print CMA next to the participant's ID?
                                 plot.CMA=TRUE,                   # plot the CMA next to the participant ID?
                                 plot.CMA.as.histogram=TRUE,      # plot CMA as a histogram or as a density plot?
                                 CMA.plot.ratio=0.10,             # the proportion of the total horizontal plot to be taken by the CMA plot
                                 CMA.plot.col="lightgreen", CMA.plot.border="darkgreen", CMA.plot.bkg="aquamarine", CMA.plot.text=CMA.plot.border, # attributes of the CMA plot
                                 highlight.followup.window=TRUE, followup.window.col="green",
                                 highlight.observation.window=TRUE, observation.window.col="yellow", observation.window.density=35, observation.window.angle=-30,
                                 show.real.obs.window.start=TRUE, real.obs.window.density=35, real.obs.window.angle=30, # for some CMAs, the real observation window starts at a different date
                                 bw.plot=FALSE                          # if TRUE, override all user-given colors and replace them with a scheme suitable for grayscale plotting
)
.plot.CMAintervals(cma=x,
                   patients.to.plot=patients.to.plot,
                   duration=duration,
                   align.all.patients=align.all.patients, align.first.event.at.zero=align.first.event.at.zero,
                   show.period=show.period,
                   period.in.days=period.in.days,
                   show.legend=show.legend, legend.x=legend.x, legend.y=legend.y, legend.bkg.opacity=legend.bkg.opacity,
                   cex=cex, cex.axis=cex.axis, cex.lab=cex.lab,
                   show.cma=show.cma,
                   col.cats=col.cats,
                   unspecified.category.label=unspecified.category.label,
                   lty.event=lty.event, lwd.event=lwd.event, pch.start.event=pch.start.event, pch.end.event=pch.end.event,
                   show.event.intervals=show.event.intervals,
                   col.na=col.na,
                   col.continuation=col.continuation, lty.continuation=lty.continuation, lwd.continuation=lwd.continuation,
                   print.CMA=print.CMA, CMA.cex=CMA.cex,
                   plot.CMA=plot.CMA,
                   CMA.plot.ratio=CMA.plot.ratio,
                   CMA.plot.col=CMA.plot.col, CMA.plot.border=CMA.plot.border,
                   CMA.plot.bkg=CMA.plot.bkg, CMA.plot.text=CMA.plot.text,
                   highlight.followup.window=highlight.followup.window, followup.window.col=followup.window.col,
                   highlight.observation.window=highlight.observation.window,
                   observation.window.col=observation.window.col,
                   observation.window.density=observation.window.density,
                   observation.window.angle=observation.window.angle,
                   show.real.obs.window.start=show.real.obs.window.start,
                   real.obs.window.density=real.obs.window.density,
                   real.obs.window.angle=real.obs.window.angle,
                   bw.plot=bw.plot,
                   ...)





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
#' @param carry.only.for.same.medication \emph{Logical}, if \code{TRUE}, the
#' carry-over applies only across medication of the same type.
#' @param consider.dosage.change \emph{Logical}, if \code{TRUE}, the carry-over
#' is adjusted to also reflect changes in dosage.
#' @param followup.window.start If a \emph{\code{Date}} object, it represents
#' the actual start date of the follow-up window; if a \emph{string} it is the
#' name of the column in \code{data} containing the start date of the follow-up
#' window either as the numbers of \code{followup.window.start.unit} units after
#' the first event (the column must be of type \code{numeric}) or as actual
#' dates (in which case the column must be of type \code{Date}); if a
#' \emph{number} it is the number of time units defined in the
#' \code{followup.window.start.unit} parameter after the begin of the
#' participant's first event; or \code{NA} if not defined.
#' @param followup.window.start.unit can be either \emph{"days"},
#' \emph{"weeks"}, \emph{"months"} or \emph{"years"}, and represents the time
#' units that \code{followup.window.start} refers to (when a number), or
#' \code{NA} if not defined.
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
                                # Various types medhods of computing gaps:
                                carry.only.for.same.medication=NA, # if TRUE the carry-over applies only across medication of same type (NA = undefined)
                                consider.dosage.change=NA, # if TRUE carry-over is adjusted to reflect changes in dosage (NA = undefined)
                                # The follow-up window:
                                followup.window.start=0, # if a number is the earliest event per participant date + number of units, or a Date object, or a column name in data (NA = undefined)
                                followup.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
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
    if( !suppress.warnings ) warning("The sliding window must start a positive number of time units after the start of the observation window!\n")
    return (NULL);
  }
  if( !inherits(sliding.window.start,"Date") && !is.numeric(sliding.window.start) && !(sliding.window.start %in% names(data)) )
  {
    if( !suppress.warnings ) warning("The sliding window start must be a valid column name!\n")
    return (NULL);
  }
  if( !(sliding.window.start.unit %in% c("days", "weeks", "months", "years") ) )
  {
    if( !suppress.warnings ) warning("The sliding window start unit is not recognized!\n")
    return (NULL);
  }
  if( !is.numeric(sliding.window.duration) || sliding.window.duration <= 0 )
  {
    if( !suppress.warnings ) warning("The sliding window duration must be greater than 0!\n")
    return (NULL);
  }
  if( !(sliding.window.duration.unit %in% c("days", "weeks", "months", "years") ) )
  {
    if( !suppress.warnings ) warning("The sliding window duration unit is not recognized!\n")
    return (NULL);
  }
  if( is.numeric(sliding.window.step.duration) && sliding.window.step.duration < 1 )
  {
    if( !suppress.warnings ) warning("The sliding window's step duration must be at least 1 unit!\n")
    return (NULL);
  }
  if( !(sliding.window.step.unit %in% c("days", "weeks", "months", "years") ) )
  {
    if( !suppress.warnings ) warning("The sliding window's step duration unit is not recognized!\n")
    return (NULL);
  }
  if( is.numeric(sliding.window.no.steps) && sliding.window.no.steps < 1 )
  {
    if( !suppress.warnings ) warning("The sliding window must move at least once!\n")
    return (NULL);
  }

  # Get the CMA function corresponding to the name:
  if( !(is.character(CMA.to.apply) || is.factor(CMA.to.apply)) )
  {
    if( !suppress.warnings ) warning(paste0("'CMA.to.apply' must be a string contining the name of the simple CMA to apply!\n)"));
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
                    {if( !suppress.warnings ) warning(paste0("Unknown 'CMA.to.apply' '",CMA.to.apply,"': defaulting to CMA0!\n)")); CMA0;}); # by default, fall back to CMA0

  # Default argument values and overrides:
  def.vals <- formals(CMA.FNC);
  if( CMA.to.apply %in% c("CMA1", "CMA2", "CMA3", "CMA4") )
  {
    carryover.into.obs.window <- carryover.within.obs.window <- FALSE;
    if( !is.na(carry.only.for.same.medication) && carry.only.for.same.medication && !suppress.warnings ) cat("Warning: 'carry.only.for.same.medication' cannot be defined for CMAs 1-4!\n");
    carry.only.for.same.medication <- FALSE;
    if( !is.na(consider.dosage.change) && consider.dosage.change && !suppress.warnings ) cat("Warning: 'consider.dosage.change' cannot be defined for CMAs 1-4!\n");
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
    if( !suppress.warnings ) warning("I know how to do CMA per episodes only for CMAs 1 to 9!\n");
    return (NULL);
  }

  # Create the return value skeleton and check consistency:
  ret.val <- CMA0(data,
                  ID.colname=ID.colname,
                  event.date.colname=event.date.colname,
                  event.duration.colname=event.duration.colname,
                  event.daily.dose.colname=event.daily.dose.colname,
                  medication.class.colname=medication.class.colname,
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
                  suppress.warnings=suppress.warnings,
                  summary=NA);
  if( is.null(ret.val) ) return (NULL);


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
    # Auxliary internal function: Compute the CMA for a given patient:
    .process.patient <- function(data4ID)
    {
      n.events <- nrow(data4ID); # cache number of events
      if( n.events < 1 ) return (NULL);

      # Compute the sliding windows for this patient:
      start.date <- .add.time.interval.to.date(data4ID$.OBS.START.DATE[1], sliding.window.start, sliding.window.start.unit, suppress.warnings); # when do the windows start?
      sliding.duration <- as.numeric(data4ID$.OBS.END.DATE[1] - start.date) - sliding.window.duration.in.days; # the effective duration to be covered with sliding windows
      if( is.na(sliding.window.no.steps) )
      {
        # Compute the number of steps required from the step size:
        sliding.window.no.steps <- (sliding.duration / sliding.window.step.duration.in.days) + 1;
      } else if( sliding.window.no.steps > 1 )
      {
        # Compute the step size to optimally cover the duration (possibly adjust the number of steps, too):
        sliding.window.step.duration.in.days <- (sliding.duration / (sliding.window.no.steps - 1));
        sliding.window.step.duration.in.days <- max(1, min(sliding.window.duration.in.days, sliding.window.step.duration.in.days)); # make sure we don't overdue it
        sliding.window.no.steps <- min(((sliding.duration / sliding.window.step.duration.in.days) + 1), sliding.duration); # ajust the number of steps just in case
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

  # Convert to data.table, cache event dat as Date objects, and key by patient ID and event date
  data.copy <- data.table(data);
  data.copy[, .DATE.as.Date := as.Date(get(event.date.colname),format=date.format)]; # .DATE.as.Date: convert event.date.colname from formatted string to Date
  setkeyv(data.copy, c(ID.colname, ".DATE.as.Date")); # key (and sorting) by patient ID and event date

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
  if( is.null(tmp) || is.null(tmp$CMA) || is.null(tmp$event.info) ) return (NULL);

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
}

#' @export
getCMA.CMA_sliding_window <- function(x)
{
  cma <- x; # parameter x is required for S3 consistency, but I like cma more
  if( is.null(cma) || !inherits(cma, "CMA_sliding_window") || !("CMA" %in% names(cma)) || is.null(cma$CMA) ) return (NULL);
  return (cma$CMA);
}

#' @rdname print.CMA0
#' @export
print.CMA_sliding_window <- function(...) print.CMA_per_episode(...)

#' @rdname plot.CMA_per_episode
#' @export
plot.CMA_sliding_window <- function(...) .plot.CMAintervals(...)




#' Interactive exploration CMA computation.
#'
#' Interactively plot a given patient's data, allowing the real-time exploration
#' of the various CMAs and their parameters.
#' It is not intended for heavy use but simply for the exploration of various
#' parameters and ways of computing the CMA. The interface is constrained by the
#' current limitations of the \code{manipulate} library.
#'
#' This function must be run within \code{RStudio} (as it uses the
#' \code{manipulate} library), and it allows the interactive manipulation of
#' various parameters and ways of computing the CMA for a participant's event
#' data.
#' Please note that:
#' \itemize{
#'   \item the graphical interface may allow the user to change certain
#'   parameters (such as considering carry over) even in the case of simple CMAs
#'   that do not consider these parameters (such as CMA1 for carry over) -- in
#'   this case changing these parameters has no effect;
#'   \item for CMA-per-episode only, additional parameters can be accessed in
#'   the interface regarding permissible gap length (days or percent) and
#'   whether medication changes indicate new episodes
#'   \item for sliding-window-CMA only, additional parameters can be accessed
#'   in the interface regarding the sliding window start, duration and step.
#' }
#'
#' @param data A \emph{\code{data.frame}} containing the events (prescribing or
#' dispensing) used to compute the CMA. Must contain, at a minimum, the patient
#' unique ID, the event date and duration, and might also contain the daily
#' dosage and medication type (the actual column names are defined in the
#' following four parameters).
#' @param ID The ID (as given in the \code{ID.colname} column) of the patient
#' whose data to interactively plot (if absent, pick the first one); please not
#' that this an be interactively selected during plotting.
#' @param cma.class The type of CMAs to plot; can be "simple" (CMA0 to CMA9),
#' "per episode", or "sliding window".
#' @param print.full.params A \emph{logical} specifying if the values of all the
#' parameters used to generate the current plot should be printed in the console
#' (if \emph{TRUE}, it can generate extremely verbose output!).
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
#' @param date.format A \emph{string} giving the format of the dates used in the
#' \code{data} and the other parameters; see the \code{format} parameters of the
#' \code{\link[base]{as.Date}} function for details (NB, this concerns only the
#' dates given as strings and not as \code{Date} objects).
#' @param followup.window.start.max The maximum number of days when the
#' follow-up window can start.
#' @param followup.window.duration.max The maximum duration of the follow-up
#' window in days.
#' @param observation.window.start.max The maximum number of days when the
#' observation window can start.
#' @param observation.window.duration.max The maximum duration of the
#' observation window in days.
#' @param maximum.permissible.gap.max The maximum permissible gap in days.
#' @param sliding.window.start.max The maximum number of days when the sliding
#' windows can start.
#' @param sliding.window.duration.max The maximum duration of the sliding
#' windows in days.
#' @param sliding.window.step.duration.max The maximum sliding window step in
#' days.
#' @param backend The plotting backend to use; "shiny" (the default) tries to
#' use the Shiny framework, while "rstudio" uses the manipulate RStudio
#' capability.
#' @return Nothing
#' @examples
#' \dontrun{
#' plot.interactive.cma(med.events);}
#' @export
plot_interactive_cma <- function( data=NULL, # the data used to compute the CMA on
                                  ID=NULL, # the ID of the patient to be plotted (automatically taken to be the first)
                                  cma.class=c("simple","per episode","sliding window")[1], # the CMA class to plot
                                  print.full.params=FALSE, # should the parameter values for the currently plotted plot be printed?
                                  # Important columns in the data
                                  ID.colname=NA, # the name of the column containing the unique patient ID (NA = undefined)
                                  event.date.colname=NA, # the start date of the event in the date.format format (NA = undefined)
                                  event.duration.colname=NA, # the event duration in days (NA = undefined)
                                  event.daily.dose.colname=NA, # the prescribed daily dose (NA = undefined)
                                  medication.class.colname=NA, # the classes/types/groups of medication (NA = undefined)
                                  # Date format:
                                  date.format="%m/%d/%Y", # the format of the dates used in this function (NA = undefined)
                                  # Parameter ranges:
                                  followup.window.start.max=5*365, # in days
                                  followup.window.duration.max=5*365, # in days
                                  observation.window.start.max=followup.window.start.max, # in days
                                  observation.window.duration.max=followup.window.duration.max, # in days
                                  align.all.patients=FALSE, align.first.event.at.zero=TRUE, # should all patients be aligned? if so, place the first event as the horizontal 0?
                                  maximum.permissible.gap.max=2*365, # in days
                                  sliding.window.start.max=followup.window.start.max, # in days
                                  sliding.window.duration.max=2*365, # in days
                                  sliding.window.step.duration.max=2*365, # in days
                                  backend=c("shiny","rstudio"), # the interactive backend to use
                                  use.system.browser=FALSE, # if shiny backend, use the system browser?
                                  ...
)
{
  if( backend == "shiny" )
  {
    .plot_interactive_cma_shiny(data=data,
                                ID=ID,
                                cma.class=cma.class,
                                print.full.params=print.full.params,
                                ID.colname=ID.colname,
                                event.date.colname=event.date.colname,
                                event.duration.colname=event.duration.colname,
                                event.daily.dose.colname=event.daily.dose.colname,
                                medication.class.colname=medication.class.colname,
                                date.format=date.format,
                                followup.window.start.max=followup.window.start.max,
                                followup.window.duration.max=followup.window.duration.max,
                                observation.window.start.max=observation.window.start.max,
                                observation.window.duration.max=observation.window.duration.max,
                                align.all.patients=align.all.patients,
                                align.first.event.at.zero=align.first.event.at.zero,
                                maximum.permissible.gap.max=maximum.permissible.gap.max,
                                sliding.window.start.max=sliding.window.start.max,
                                sliding.window.duration.max=sliding.window.duration.max,
                                sliding.window.step.duration.max=sliding.window.step.duration.max,
                                use.system.browser=use.system.browser,
                                ...
    );
  } else if( backend == "rstudio" )
  {
    .plot_interactive_cma_rstudio(data=data,
                                  ID=ID,
                                  cma.class=cma.class,
                                  print.full.params=print.full.params,
                                  ID.colname=ID.colname,
                                  event.date.colname=event.date.colname,
                                  event.duration.colname=event.duration.colname,
                                  event.daily.dose.colname=event.daily.dose.colname,
                                  medication.class.colname=medication.class.colname,
                                  date.format=date.format,
                                  followup.window.start.max=followup.window.start.max,
                                  followup.window.duration.max=followup.window.duration.max,
                                  observation.window.start.max=observation.window.start.max,
                                  observation.window.duration.max=observation.window.duration.max,
                                  #align.all.patients=align.all.patients,
                                  #align.first.event.at.zero=align.first.event.at.zero,
                                  maximum.permissible.gap.max=maximum.permissible.gap.max,
                                  sliding.window.start.max=sliding.window.start.max,
                                  sliding.window.duration.max=sliding.window.duration.max,
                                  sliding.window.step.duration.max=sliding.window.step.duration.max,
                                  ...
    );
  } else
  {
    warning("Interactive plotting: dont' know backend '",backend,"'; only use 'shiny' or 'rstudio'.\n");
  }
}



# Auxiliary function: interactive plot using RStudio's manipulate:
.plot_interactive_cma_rstudio <- function( data=NULL, # the data used to compute the CMA on
                                           ID=NULL, # the ID of the patient to be plotted (automatically taken to be the first)
                                           cma.class=c("simple","per episode","sliding window")[1], # the CMA class to plot
                                           print.full.params=FALSE, # should the parameter values for the currently plotted plot be printed?
                                           # Important columns in the data
                                           ID.colname=NA, # the name of the column containing the unique patient ID (NA = undefined)
                                           event.date.colname=NA, # the start date of the event in the date.format format (NA = undefined)
                                           event.duration.colname=NA, # the event duration in days (NA = undefined)
                                           event.daily.dose.colname=NA, # the prescribed daily dose (NA = undefined)
                                           medication.class.colname=NA, # the classes/types/groups of medication (NA = undefined)
                                           # Date format:
                                           date.format=NA, # the format of the dates used in this function (NA = undefined)
                                           # Parameter ranges:
                                           followup.window.start.max=5*365, # in days
                                           followup.window.duration.max=5*365, # in days
                                           observation.window.start.max=followup.window.start.max, # in days
                                           observation.window.duration.max=followup.window.duration.max, # in days
                                           maximum.permissible.gap.max=2*365, # in days
                                           sliding.window.start.max=followup.window.start.max, # in days
                                           sliding.window.duration.max=2*365, # in days
                                           sliding.window.step.duration.max=2*365, # in days
                                           ...
)
{
  # Check if manipulate can be run:
  if( !manipulate::isAvailable() )
  {
    stop("Interactive plotting is currently only possible within RStudio (as we use the manipulate package)!\n");
    return (NULL);
  }

  if( !(cma.class %in% c("simple","per episode","sliding window")) )
  {
    warning(paste0("Only know how to interactively plot 'cma.class' of type 'simple', 'per episode' and 'sliding window', but you requested '",cma.class,"': assuming 'simple'."));
    cma.class <- "simple"
  }

  # Preconditions:
  if( !is.null(data) )
  {
    # data's class and dimensions:
    if( class(data) == "matrix" ) data <- as.data.frame(data); #make it a data.frame
    if( !inherits(data, "data.frame") )
    {
      stop("The 'data' must be of type 'data.frame'!\n");
      return (NULL);
    }
    if( nrow(data) < 1 )
    {
      stop("The 'data' must have at least one row!\n");
      return (NULL);
    }
    # the column names must exist in data:
    if( !is.na(ID.colname) && !(ID.colname %in% names(data)) )
    {
      stop(paste0("Column ID.colname='",ID.colname,"' must appear in the 'data'!\n"));
      return (NULL);
    }
    if( !is.na(event.date.colname) && !(event.date.colname %in% names(data)) )
    {
      stop(paste0("Column event.date.colname='",event.date.colname,"' must appear in the 'data'!\n"));
      return (NULL);
    }
    if( !is.na(event.duration.colname) && !(event.duration.colname %in% names(data)) )
    {
      stop(paste0("Column event.duration.colname='",event.duration.colname,"' must appear in the 'data'!\n"));
      return (NULL);
    }
    if( !is.na(event.daily.dose.colname) && !(event.daily.dose.colname %in% names(data)) )
    {
      stop(paste0("Column event.daily.dose.colname='",event.daily.dose.colname,"' must appear in the 'data'!\n"));
      return (NULL);
    }
    if( !is.na(medication.class.colname) && !(medication.class.colname %in% names(data)) )
    {
      stop(paste0("Column medication.class.colname='",medication.class.colname,"' must appear in the 'data'!\n"));
      return (NULL);
    }
  } else
  {
    stop("The 'data' cannot be empty!\n");
    return (NULL);
  }

  # The function encapsulating the plotting:
  .plotting.fnc <- function(data=NULL, # the data used to compute the CMA on
                            ID=NULL, # the ID of the patient to plot
                            cma="none", # the CMA to use for plotting
                            cma.to.apply="none", # cma to compute per episode or sliding window
                            # Various types medhods of computing gaps:
                            carryover.within.obs.window=NA, # if TRUE consider the carry-over within the observation window (NA = undefined)
                            carryover.into.obs.window=NA, # if TRUE consider the carry-over from before the starting date of the observation window (NA = undefined)
                            carry.only.for.same.medication=NA, # if TRUE the carry-over applies only across medication of same type (NA = undefined)
                            consider.dosage.change=NA, # if TRUE carry-over is adjusted to reflect changes in dosage (NA = undefined)
                            # The follow-up window:
                            followup.window.start=NA, # if a number is the earliest event per participant date + number of units, or a Date object, or a column name in data (NA = undefined)
                            followup.window.start.unit=NA, # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                            followup.window.duration=NA, # the duration of the follow-up window in the time units given below (NA = undefined)
                            followup.window.duration.unit=NA, # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)  (NA = undefined)
                            # The observation window (embedded in the follow-up window):
                            observation.window.start=NA, # the number of time units relative to followup.window.start (NA = undefined)
                            observation.window.start.unit=NA, # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                            observation.window.duration=NA, # the duration of the observation window in time units (NA = undefined)
                            observation.window.duration.unit=NA, # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                            # Treatment episodes:
                            medication.change.means.new.treatment.episode=TRUE, # does a change in medication automatically start a new treatment episode?
                            maximum.permissible.gap=180, # if a number, is the duration in units of max. permissible gaps between treatment episodes
                            maximum.permissible.gap.unit="days", # time units; can be "days", "weeks" (fixed at 7 days), "months" (fixed at 30 days) or "years" (fixed at 365 days)
                            # Sliding window:
                            sliding.window.start=0, # if a number is the earliest event per participant date + number of units, or a Date object, or a column name in data (NA = undefined)
                            sliding.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                            sliding.window.duration=90,  # the duration of the sliding window in time units (NA = undefined)
                            sliding.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                            sliding.window.step.duration=7, # the step ("jump") of the sliding window in time units (NA = undefined)
                            sliding.window.step.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                            sliding.window.no.steps=NA, # the number of steps to jump; if both sliding.win.no.steps & sliding.win.duration are NA, fill the whole observation window
                            plot.CMA.as.histogram=TRUE, # plot the CMA as historgram or density plot?
                            show.legend=TRUE # show the legend?
  )
  {
    # Progress messages:
    cat(paste0("Plotting patient ID '",ID,"' with CMA '",cma,"'",ifelse(cma.to.apply != "none",paste0(" ('",cma.to.apply,"')"),"")));
    if( print.full.params )
    {
      cat(paste0(" with params: ",
                 "carryover.within.obs.window=",carryover.within.obs.window,", ",
                 "carryover.into.obs.window=",carryover.into.obs.window,", ",
                 "carry.only.for.same.medication=",carry.only.for.same.medication,", ",
                 "consider.dosage.change=",consider.dosage.change,", ",
                 "followup.window.start=",followup.window.start,", ",
                 "followup.window.start.unit=",followup.window.start.unit,", ",
                 "followup.window.duration=",followup.window.duration,", ",
                 "followup.window.duration.unit=",followup.window.duration.unit,", ",
                 "observation.window.start=",observation.window.start,", ",
                 "observation.window.start.unit=",observation.window.start.unit,", ",
                 "observation.window.duration=",observation.window.duration,", ",
                 "observation.window.duration.unit=",observation.window.duration.unit,", ",
                 "medication.change.means.new.treatment.episode=",medication.change.means.new.treatment.episode,", ",
                 "maximum.permissible.gap=",maximum.permissible.gap,", ",
                 "maximum.permissible.gap.unit=",maximum.permissible.gap.unit,", ",
                 "sliding.window.start=",sliding.window.start,", ",
                 "sliding.window.start.unit=",sliding.window.start.unit,", ",
                 "sliding.window.duration=",sliding.window.duration,", ",
                 "sliding.window.duration.unit=",sliding.window.duration.unit,", ",
                 "sliding.window.step.duration=",sliding.window.step.duration,", ",
                 "sliding.window.step.unit=",sliding.window.step.unit,", ",
                 "sliding.window.no.steps=",sliding.window.no.steps
      ));
    }
    cat("\n");

    # Preconditions:
    if( is.null(ID) || is.null(data <- data[data[,ID.colname] %in% ID,]) || nrow(data)==0 )
    {
      plot(-10:10,-10:10,type="n",axes=FALSE,xlab="",ylab=""); text(0,0,paste0("Error: cannot display the data for patient '",ID,"'!"),col="red");
      return (invisible(NULL));
    }

    # Compute the CMA:
    cma.fnc <- switch(cma,
                      "CMA1" = CMA1,
                      "CMA2" = CMA2,
                      "CMA3" = CMA3,
                      "CMA4" = CMA4,
                      "CMA5" = CMA5,
                      "CMA6" = CMA6,
                      "CMA7" = CMA7,
                      "CMA8" = CMA8,
                      "CMA9" = CMA9,
                      "per episode" = CMA_per_episode,
                      "sliding window" = CMA_sliding_window,
                      CMA0); # by default, fall back to CMA0
    # Try to catch errors and warnings for nice displaying:
    results <- NULL;
    full.results <- tryCatch( results <- cma.fnc( data,
                                                  CMA=cma.to.apply,
                                                  ID.colname=ID.colname,
                                                  event.date.colname=event.date.colname,
                                                  event.duration.colname=event.duration.colname,
                                                  event.daily.dose.colname=event.daily.dose.colname,
                                                  medication.class.colname=medication.class.colname,
                                                  date.format=date.format,
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
                                                  medication.change.means.new.treatment.episode=medication.change.means.new.treatment.episode,
                                                  maximum.permissible.gap=maximum.permissible.gap,
                                                  maximum.permissible.gap.unit=maximum.permissible.gap.unit,
                                                  sliding.window.start=sliding.window.start,
                                                  sliding.window.start.unit=sliding.window.start.unit,
                                                  sliding.window.duration=sliding.window.duration,
                                                  sliding.window.duration.unit=sliding.window.duration.unit,
                                                  sliding.window.step.duration=sliding.window.step.duration,
                                                  sliding.window.step.unit=sliding.window.step.unit,
                                                  sliding.window.no.steps=sliding.window.no.steps),
                              error  =function(e) return(list(results=results,error=conditionMessage(e))),
                              warning=function(w) return(list(results=results,warning=conditionMessage(w))));
    if( is.null(results) )
    {
      # Plot an error message:
      plot(-10:10,-10:10,type="n",axes=FALSE,xlab="",ylab="");
      text(0,0,paste0("Error computing '",cma,"' for patient '",ID,"'\n(see console for possible warnings or errors)!"),col="red");
      if( !is.null(full.results$error) )   cat(paste0("Error(s): ",paste0(full.results$error,collapse="\n")));
      if( !is.null(full.results$warning) ) cat(paste0("Warning(s): ",paste0(full.results$warning,collapse="\n")));
    } else
    {
      # Plot the results:
      plot(results, show.legend=show.legend, plot.CMA.as.histogram=plot.CMA.as.histogram);
    }
  }

  if( is.null(ID) ) ID <- data[1,ID.colname];

  if( cma.class == "simple" )
  {
    # CMA0 to CMA9:
    manipulate::manipulate(.plotting.fnc(data=data,
                                         ID=patientID,
                                         cma=selectedCMA,
                                         carryover.within.obs.window=NA, # carryover.within.obs.window,
                                         carryover.into.obs.window=NA, # carryover.into.obs.window,
                                         carry.only.for.same.medication=carry.only.for.same.medication,
                                         consider.dosage.change=consider.dosage.change,
                                         followup.window.start=followup.window.start,
                                         followup.window.start.unit="days", # followup.window.start.unit,
                                         followup.window.duration=followup.window.duration,
                                         followup.window.duration.unit="days", # followup.window.duration.unit,
                                         observation.window.start=observation.window.start,
                                         observation.window.start.unit="days", # observation.window.start.unit,
                                         observation.window.duration=observation.window.duration,
                                         observation.window.duration.unit="days", # observation.window.duration.unit,
                                         medication.change.means.new.treatment.episode=NA, #medication.change.means.new.treatment.episode,
                                         maximum.permissible.gap=NA, #maximum.permissible.gap,
                                         maximum.permissible.gap.unit="days", # maximum.permissible.gap.unit,
                                         sliding.window.start=NA, #sliding.window.start,
                                         sliding.window.start.unit="days", # sliding.window.start.unit,
                                         sliding.window.duration=NA, #sliding.window.duration,
                                         sliding.window.duration.unit="days", # sliding.window.duration.unit,
                                         sliding.window.step.duration=NA, #sliding.window.step.duration,
                                         sliding.window.step.unit="days", # sliding.window.step.unit,
                                         sliding.window.no.steps=NA, # sliding.window.no.steps
                                         plot.CMA.as.histogram=NA, # plot CMA as historgram?
                                         show.legend=show.legend # show the legend?
                                        ),
                           patientID = manipulate::picker(as.list(unique(data[,ID.colname])), initial=as.character(ID), label="Patient ID"),
                           selectedCMA = manipulate::picker(list("CMA0", "CMA1", "CMA2", "CMA3", "CMA4", "CMA5", "CMA6", "CMA7", "CMA8", "CMA9"), initial="CMA1", label="CMA to compute"),
                           #carryover.within.obs.window = manipulate::checkbox(FALSE, "Carry-over within obs. wnd.?"),
                           #carryover.into.obs.window = manipulate::checkbox(FALSE, "Carry-over into obs. wnd.?"),
                           carry.only.for.same.medication = manipulate::checkbox(FALSE, "Carry-over for same treat only? (CMAs 5-9)"),
                           consider.dosage.change = manipulate::checkbox(FALSE, "Consider dosage changes? (CMAs 5-9)"),
                           followup.window.start = manipulate::slider(0, followup.window.start.max, 0, "Follow-up wnd. start (days)", step=1),
                           #followup.window.start.unit = manipulate::picker(list("days", "weeks", "months", "years"), initial="days", label="Follow-up wnd. start unit"),
                           followup.window.duration = manipulate::slider(0, followup.window.duration.max, 2*365, "Follow-up wnd. duration (days)", 1),
                           #followup.window.duration.unit = manipulate::picker(list("days", "weeks", "months", "years"), initial="days", label="Follow-up wnd. duration unit"),
                           observation.window.start = manipulate::slider(0, observation.window.start.max, 0, "Obs. wnd. start (days)", step=1),
                           #observation.window.start.unit = manipulate::picker(list("days", "weeks", "months", "years"), initial="days", label="Obs. wnd. start unit"),
                           observation.window.duration = manipulate::slider(0, observation.window.duration.max, 2*365, "Obs. wnd. duration (days)", 1),
                           #observation.window.duration.unit = manipulate::picker(list("days", "weeks", "months", "years"), initial="days", label="Obs. wnd. duration unit"),
                           #medication.change.means.new.treatment.episode = manipulate::checkbox(TRUE, "Treat. change starts new episode?"),
                           #maximum.permissible.gap = manipulate::slider(0, 500, 0, "Max. permissible gap (days)", 1),
                           #maximum.permissible.gap.unit = manipulate::picker(list("days", "weeks", "months", "years"), initial="days", label="Max. permis. gap duration unit"),
                           #maximum.permissible.gap.as.percent = manipulate::checkbox(FALSE, "Max. permissible gap as percent?"),
                           #sliding.window.start = manipulate::slider(0, 5000, 0, "Sliding wnd. start (days)", step=1),
                           #sliding.window.start.unit = manipulate::picker(list("days", "weeks", "months", "years"), initial="days", label="Sliding. wnd. start unit"),
                           #sliding.window.duration = manipulate::slider(0, 2*365, 90, "Sliding wnd. duration (days)", 1),
                           #sliding.window.duration.unit = manipulate::picker(list("days", "weeks", "months", "years"), initial="days", label="Sliding wnd. duration unit"),
                           #sliding.window.step.duration = manipulate::slider(0, 2*365, 7, "Sliding wnd. step (days)", 1)
                           #sliding.window.step.unit = manipulate::picker(list("days", "weeks", "months", "years"), initial="days", label="Sliding wnd. step unit"),
                           #sliding.window.no.steps = manipulate::slider(0, 100, 10, "# sliding wnd. steps", 1),
                           show.legend = manipulate::checkbox(TRUE, "Show the legend?")
                           );
  } else if( cma.class == "per episode" )
  {
    # per episode:
    manipulate::manipulate(.plotting.fnc(data=data,
                                         ID=patientID,
                                         cma="per episode", cma.to.apply=selectedCMA,
                                         carryover.within.obs.window=NA, # carryover.within.obs.window,
                                         carryover.into.obs.window=NA, # carryover.into.obs.window,
                                         carry.only.for.same.medication=carry.only.for.same.medication,
                                         consider.dosage.change=consider.dosage.change,
                                         followup.window.start=followup.window.start,
                                         followup.window.start.unit="days", # followup.window.start.unit,
                                         followup.window.duration=followup.window.duration,
                                         followup.window.duration.unit="days", # followup.window.duration.unit,
                                         observation.window.start=observation.window.start,
                                         observation.window.start.unit="days", # observation.window.start.unit,
                                         observation.window.duration=observation.window.duration,
                                         observation.window.duration.unit="days", # observation.window.duration.unit,
                                         medication.change.means.new.treatment.episode=medication.change.means.new.treatment.episode,
                                         maximum.permissible.gap=maximum.permissible.gap,
                                         maximum.permissible.gap.unit=ifelse(!maximum.permissible.gap.as.percent,"days","percent"), # maximum.permissible.gap.unit,
                                         sliding.window.start=NA, #sliding.window.start,
                                         sliding.window.start.unit="days", # sliding.window.start.unit,
                                         sliding.window.duration=NA, #sliding.window.duration,
                                         sliding.window.duration.unit="days", # sliding.window.duration.unit,
                                         sliding.window.step.duration=NA, #sliding.window.step.duration,
                                         sliding.window.step.unit="days", # sliding.window.step.unit,
                                         sliding.window.no.steps=NA, # sliding.window.no.steps
                                         plot.CMA.as.histogram=plot.CMA.as.histogram, # plot CMA as histogram?
                                         show.legend=show.legend # show the legend?
                                        ),
                           patientID = manipulate::picker(as.list(unique(data[,ID.colname])), initial=as.character(ID), label="Patient ID"),
                           selectedCMA = manipulate::picker(list("CMA1", "CMA2", "CMA3", "CMA4", "CMA5", "CMA6", "CMA7", "CMA8", "CMA9"), initial="CMA1", label="CMA to compute per episode"),
                           #carryover.within.obs.window = manipulate::checkbox(FALSE, "Carry-over within obs. wnd.? (coupled)"),
                           #carryover.into.obs.window = manipulate::checkbox(FALSE, "Carry-over into obs. wnd.? (coupled)"),
                           carry.only.for.same.medication = manipulate::checkbox(FALSE, "Carry-over for same treat only? (coupled, CMAs 5-9)"),
                           consider.dosage.change = manipulate::checkbox(FALSE, "Consider dosage changes? (coupled, CMAs 5-9)"),
                           followup.window.start = manipulate::slider(0, followup.window.start.max, 0, "Follow-up wnd. start (days)", step=1),
                           #followup.window.start.unit = manipulate::picker(list("days", "weeks", "months", "years"), initial="days", label="Follow-up wnd. start unit"),
                           followup.window.duration = manipulate::slider(0, followup.window.duration.max, 2*365, "Follow-up wnd. duration (days)", 1),
                           #followup.window.duration.unit = manipulate::picker(list("days", "weeks", "months", "years"), initial="days", label="Follow-up wnd. duration unit"),
                           observation.window.start = manipulate::slider(0, observation.window.start.max, 0, "Obs. wnd. start (days)", step=1),
                           #observation.window.start.unit = manipulate::picker(list("days", "weeks", "months", "years"), initial="days", label="Obs. wnd. start unit"),
                           observation.window.duration = manipulate::slider(0, observation.window.duration.max, 2*365, "Obs. wnd. duration (days)", 1),
                           #observation.window.duration.unit = manipulate::picker(list("days", "weeks", "months", "years"), initial="days", label="Obs. wnd. duration unit"),
                           medication.change.means.new.treatment.episode = manipulate::checkbox(TRUE, "Treat. change starts new episode?"),
                           maximum.permissible.gap = manipulate::slider(0, maximum.permissible.gap.max, 180, "Max. permissible gap (days or %)", 1),
                           maximum.permissible.gap.as.percent = manipulate::checkbox(FALSE, "Max. permissible gap as percent?"),
                           #maximum.permissible.gap.unit = manipulate::picker(list("days", "weeks", "months", "years"), initial="days", label="Max. permis. gap duration unit"),
                           #sliding.window.start = manipulate::slider(0, 5000, 0, "Sliding wnd. start (days)", step=1),
                           #sliding.window.start.unit = manipulate::picker(list("days", "weeks", "months", "years"), initial="days", label="Sliding. wnd. start unit"),
                           #sliding.window.duration = manipulate::slider(0, 2*365, 90, "Sliding wnd. duration (days)", 1),
                           #sliding.window.duration.unit = manipulate::picker(list("days", "weeks", "months", "years"), initial="days", label="Sliding wnd. duration unit"),
                           #sliding.window.step.duration = manipulate::slider(0, 2*365, 7, "Sliding wnd. step (days)", 1)
                           #sliding.window.step.unit = manipulate::picker(list("days", "weeks", "months", "years"), initial="days", label="Sliding wnd. step unit"),
                           #sliding.window.no.steps = manipulate::slider(0, 100, 10, "# sliding wnd. steps", 1),
                           plot.CMA.as.histogram = manipulate::checkbox(TRUE, "Plot CMA as histogram?"),
                           show.legend = manipulate::checkbox(TRUE, "Show the legend?")
                          );
  } else if( cma.class == "sliding window" )
  {
    # sliding window:
    manipulate::manipulate(.plotting.fnc(data=data,
                                         ID=patientID,
                                         cma="sliding window", cma.to.apply=selectedCMA,
                                         carryover.within.obs.window=NA, # carryover.within.obs.window,
                                         carryover.into.obs.window=NA, # carryover.into.obs.window,
                                         carry.only.for.same.medication=carry.only.for.same.medication,
                                         consider.dosage.change=consider.dosage.change,
                                         followup.window.start=followup.window.start,
                                         followup.window.start.unit="days", # followup.window.start.unit,
                                         followup.window.duration=followup.window.duration,
                                         followup.window.duration.unit="days", # followup.window.duration.unit,
                                         observation.window.start=observation.window.start,
                                         observation.window.start.unit="days", # observation.window.start.unit,
                                         observation.window.duration=observation.window.duration,
                                         observation.window.duration.unit="days", # observation.window.duration.unit,
                                         medication.change.means.new.treatment.episode=NA,
                                         maximum.permissible.gap=NA, #maximum.permissible.gap,
                                         maximum.permissible.gap.unit="days", # maximum.permissible.gap.unit,
                                         sliding.window.start=sliding.window.start,
                                         sliding.window.start.unit="days", # sliding.window.start.unit,
                                         sliding.window.duration=sliding.window.duration,
                                         sliding.window.duration.unit="days", # sliding.window.duration.unit,
                                         sliding.window.step.duration=sliding.window.step.duration,
                                         sliding.window.step.unit="days", # sliding.window.step.unit,
                                         sliding.window.no.steps=NA, # sliding.window.no.steps
                                         plot.CMA.as.histogram=plot.CMA.as.histogram, # plot CMA as histogram?
                                         show.legend=show.legend # show the legend?
                                        ),
                           patientID = manipulate::picker(as.list(unique(data[,ID.colname])), initial=as.character(ID), label="Patient ID"),
                           selectedCMA = manipulate::picker(list("CMA1", "CMA2", "CMA3", "CMA4", "CMA5", "CMA6", "CMA7", "CMA8", "CMA9"), initial="CMA1" , label="CMA to compute per sliding window"),
                           #carryover.within.obs.window = manipulate::checkbox(FALSE, "Carry-over within obs. wnd.?"),
                           #carryover.into.obs.window = manipulate::checkbox(FALSE, "Carry-over into obs. wnd.?"),
                           carry.only.for.same.medication = manipulate::checkbox(FALSE, "Carry-over for same treat only? (CMAs 5-9)"),
                           consider.dosage.change = manipulate::checkbox(FALSE, "Consider dosage changes? (CMAs 5-9)"),
                           followup.window.start = manipulate::slider(0, followup.window.start.max, 0, "Follow-up wnd. start (days)", step=1),
                           #followup.window.start.unit = manipulate::picker(list("days", "weeks", "months", "years"), initial="days", label="Follow-up wnd. start unit"),
                           followup.window.duration = manipulate::slider(0, followup.window.duration.max, 2*365, "Follow-up wnd. duration (days)", 1),
                           #followup.window.duration.unit = manipulate::picker(list("days", "weeks", "months", "years"), initial="days", label="Follow-up wnd. duration unit"),
                           observation.window.start = manipulate::slider(0, observation.window.start.max, 0, "Obs. wnd. start (days)", step=1),
                           #observation.window.start.unit = manipulate::picker(list("days", "weeks", "months", "years"), initial="days", label="Obs. wnd. start unit"),
                           observation.window.duration = manipulate::slider(0, observation.window.duration.max, 2*365, "Obs. wnd. duration (days)", 1),
                           #observation.window.duration.unit = manipulate::picker(list("days", "weeks", "months", "years"), initial="days", label="Obs. wnd. duration unit"),
                           #medication.change.means.new.treatment.episode = manipulate::checkbox(TRUE, "Treat. change starts new episode?"),
                           #maximum.permissible.gap = manipulate::slider(0, 500, 0, "Max. permissible gap (days)", 1),
                           #maximum.permissible.gap.unit = manipulate::picker(list("days", "weeks", "months", "years"), initial="days", label="Max. permis. gap duration unit"),
                           #maximum.permissible.gap.as.percent = manipulate::checkbox(FALSE, "Max. permissible gap as percent?"),
                           sliding.window.start = manipulate::slider(0, sliding.window.start.max, 0, "Sliding wnd. start (days)", step=1),
                           #sliding.window.start.unit = manipulate::picker(list("days", "weeks", "months", "years"), initial="days", label="Sliding. wnd. start unit"),
                           sliding.window.duration = manipulate::slider(0, sliding.window.duration.max, 90, "Sliding wnd. duration (days)", 1),
                           #sliding.window.duration.unit = manipulate::picker(list("days", "weeks", "months", "years"), initial="days", label="Sliding wnd. duration unit"),
                           sliding.window.step.duration = manipulate::slider(0, sliding.window.step.duration.max, 30, "Sliding wnd. step (days)", 1),
                           #sliding.window.step.unit = manipulate::picker(list("days", "weeks", "months", "years"), initial="days", label="Sliding wnd. step unit"),
                           #sliding.window.no.steps = manipulate::slider(0, 100, 10, "# sliding wnd. steps", 1),
                           plot.CMA.as.histogram = manipulate::checkbox(FALSE, "Plot CMA as histogram?"),
                           show.legend = manipulate::checkbox(TRUE, "Show the legend?")
                          );
  }
}


# Auxiliary function: interactive plot using shiny
.plot_interactive_cma_shiny <- function(data=NULL, # the data used to compute the CMA on
                                        ID=NULL, # the ID of the patient to be plotted (automatically taken to be the first)
                                        cma.class=c("simple","per episode","sliding window")[1], # the CMA class to plot
                                        print.full.params=FALSE, # should the parameter values for the currently plotted plot be printed?
                                        # Important columns in the data
                                        ID.colname=NA, # the name of the column containing the unique patient ID (NA = undefined)
                                        event.date.colname=NA, # the start date of the event in the date.format format (NA = undefined)
                                        event.duration.colname=NA, # the event duration in days (NA = undefined)
                                        event.daily.dose.colname=NA, # the prescribed daily dose (NA = undefined)
                                        medication.class.colname=NA, # the classes/types/groups of medication (NA = undefined)
                                        # Date format:
                                        date.format=NA, # the format of the dates used in this function (NA = undefined)
                                        # Parameter ranges:
                                        followup.window.start.max=5*365, # in days
                                        followup.window.duration.max=5*365, # in days
                                        observation.window.start.max=followup.window.start.max, # in days
                                        observation.window.duration.max=followup.window.duration.max, # in days
                                        align.all.patients=FALSE, align.first.event.at.zero=TRUE, # should all patients be aligned? if so, place first event the horizontal 0?
                                        maximum.permissible.gap.max=2*365, # in days
                                        sliding.window.start.max=followup.window.start.max, # in days
                                        sliding.window.duration.max=2*365, # in days
                                        sliding.window.step.duration.max=2*365, # in days
                                        use.system.browser=FALSE, # by default, don't necessarily use the system browser
                                        ...
)
{
  # pass things to shiny using the global environment (as discussed at https://github.com/rstudio/shiny/issues/440):

  # checks:
  if( !(cma.class %in% c("simple","per episode","sliding window")) )
  {
    warning(paste0("Only know how to interactively plot 'cma.class' of type 'simple', 'per episode' and 'sliding window', but you requested '",cma.class,"': assuming 'simple'."));
    cma.class <- "simple";
  }

  # Preconditions:
  if( !is.null(data) )
  {
    # data's class and dimensions:
    if( class(data) == "matrix" ) data <- as.data.frame(data); #make it a data.frame
    if( !inherits(data, "data.frame") )
    {
      stop("The 'data' must be of type 'data.frame'!\n");
      return (NULL);
    }
    if( nrow(data) < 1 )
    {
      stop("The 'data' must have at least one row!\n");
      return (NULL);
    }
    # the column names must exist in data:
    if( !is.na(ID.colname) && !(ID.colname %in% names(data)) )
    {
      stop(paste0("Column ID.colname='",ID.colname,"' must appear in the 'data'!\n"));
      return (NULL);
    }
    if( !is.na(event.date.colname) && !(event.date.colname %in% names(data)) )
    {
      stop(paste0("Column event.date.colname='",event.date.colname,"' must appear in the 'data'!\n"));
      return (NULL);
    }
    if( !is.na(event.duration.colname) && !(event.duration.colname %in% names(data)) )
    {
      stop(paste0("Column event.duration.colname='",event.duration.colname,"' must appear in the 'data'!\n"));
      return (NULL);
    }
    if( !is.na(event.daily.dose.colname) && !(event.daily.dose.colname %in% names(data)) )
    {
      stop(paste0("Column event.daily.dose.colname='",event.daily.dose.colname,"' must appear in the 'data'!\n"));
      return (NULL);
    }
    if( !is.na(medication.class.colname) && !(medication.class.colname %in% names(data)) )
    {
      stop(paste0("Column medication.class.colname='",medication.class.colname,"' must appear in the 'data'!\n"));
      return (NULL);
    }
  } else
  {
    stop("The 'data' cannot be empty!\n");
    return (NULL);
  }

  # All patient IDs:
  all.IDs <- sort(unique(data[,ID.colname]));
  if( is.null(ID) || is.na(ID) || !(ID %in% all.IDs) ) ID <- all.IDs[1];

  # The function encapsulating the plotting:
  .plotting.fnc <- function(data=NULL, # the data used to compute the CMA on
                            ID=NULL, # the ID of the patient to plot
                            cma="none", # the CMA to use for plotting
                            cma.to.apply="none", # cma to compute per episode or sliding window
                            # Various types medhods of computing gaps:
                            carryover.within.obs.window=NA, # if TRUE consider the carry-over within the observation window (NA = undefined)
                            carryover.into.obs.window=NA, # if TRUE consider the carry-over from before the starting date of the observation window (NA = undefined)
                            carry.only.for.same.medication=NA, # if TRUE the carry-over applies only across medication of same type (NA = undefined)
                            consider.dosage.change=NA, # if TRUE carry-over is adjusted to reflect changes in dosage (NA = undefined)
                            # The follow-up window:
                            followup.window.start=NA, # if a number is the earliest event per participant date + number of units, or a Date object, or a column name in data (NA = undefined)
                            followup.window.start.unit=NA, # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                            followup.window.duration=NA, # the duration of the follow-up window in the time units given below (NA = undefined)
                            followup.window.duration.unit=NA, # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!)  (NA = undefined)
                            # The observation window (embedded in the follow-up window):
                            observation.window.start=NA, # the number of time units relative to followup.window.start (NA = undefined)
                            observation.window.start.unit=NA, # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                            observation.window.duration=NA, # the duration of the observation window in time units (NA = undefined)
                            observation.window.duration.unit=NA, # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                            # Treatment episodes:
                            medication.change.means.new.treatment.episode=TRUE, # does a change in medication automatically start a new treatment episode?
                            maximum.permissible.gap=180, # if a number, is the duration in units of max. permissible gaps between treatment episodes
                            maximum.permissible.gap.unit="days", # time units; can be "days", "weeks" (fixed at 7 days), "months" (fixed at 30 days) or "years" (fixed at 365 days)
                            # Sliding window:
                            sliding.window.start=0, # if a number is the earliest event per participant date + number of units, or a Date object, or a column name in data (NA = undefined)
                            sliding.window.start.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                            sliding.window.duration=90,  # the duration of the sliding window in time units (NA = undefined)
                            sliding.window.duration.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                            sliding.window.step.duration=7, # the step ("jump") of the sliding window in time units (NA = undefined)
                            sliding.window.step.unit=c("days", "weeks", "months", "years")[1], # the time units; can be "days", "weeks", "months" or "years" (if months or years, using an actual calendar!) (NA = undefined)
                            sliding.window.no.steps=NA, # the number of steps to jump; if both sliding.win.no.steps & sliding.win.duration are NA, fill the whole observation window
                            plot.CMA.as.histogram=TRUE, # plot the CMA as historgram or density plot?
                            align.all.patients=FALSE, align.first.event.at.zero=TRUE, # should all patients be aligned? if so, place first event the horizontal 0?

                            # Legend:
                            show.legend=TRUE # show the legend?
  )
  {
    # Progress messages:
    cat(paste0("Plotting patient ID '",ID,"' with CMA '",cma,"'",ifelse(cma.to.apply != "none",paste0(" ('",cma.to.apply,"')"),"")));
    if( print.full.params )
    {
      cat(paste0(" with params: ",
                 "carryover.within.obs.window=",carryover.within.obs.window,", ",
                 "carryover.into.obs.window=",carryover.into.obs.window,", ",
                 "carry.only.for.same.medication=",carry.only.for.same.medication,", ",
                 "consider.dosage.change=",consider.dosage.change,", ",
                 "followup.window.start=",followup.window.start,", ",
                 "followup.window.start.unit=",followup.window.start.unit,", ",
                 "followup.window.duration=",followup.window.duration,", ",
                 "followup.window.duration.unit=",followup.window.duration.unit,", ",
                 "observation.window.start=",observation.window.start,", ",
                 "observation.window.start.unit=",observation.window.start.unit,", ",
                 "observation.window.duration=",observation.window.duration,", ",
                 "observation.window.duration.unit=",observation.window.duration.unit,", ",
                 "medication.change.means.new.treatment.episode=",medication.change.means.new.treatment.episode,", ",
                 "maximum.permissible.gap=",maximum.permissible.gap,", ",
                 "maximum.permissible.gap.unit=",maximum.permissible.gap.unit,", ",
                 "sliding.window.start=",sliding.window.start,", ",
                 "sliding.window.start.unit=",sliding.window.start.unit,", ",
                 "sliding.window.duration=",sliding.window.duration,", ",
                 "sliding.window.duration.unit=",sliding.window.duration.unit,", ",
                 "sliding.window.step.duration=",sliding.window.step.duration,", ",
                 "sliding.window.step.unit=",sliding.window.step.unit,", ",
                 "sliding.window.no.steps=",sliding.window.no.steps,", ",
                 "align.all.patients=",align.all.patients,", ",
                 "align.first.event.at.zero=",align.first.event.at.zero
      ));
    }
    cat("\n");

    # Preconditions:
    if( is.null(ID) || is.null(data <- data[data[,ID.colname] %in% ID,]) || nrow(data)==0 )
    {
      plot(-10:10,-10:10,type="n",axes=FALSE,xlab="",ylab=""); text(0,0,paste0("Error: cannot display the data for patient '",ID,"'!"),col="red");
      return (invisible(NULL));
    }

    # Compute the CMA:
    cma.fnc <- switch(cma,
                      "CMA1" = CMA1,
                      "CMA2" = CMA2,
                      "CMA3" = CMA3,
                      "CMA4" = CMA4,
                      "CMA5" = CMA5,
                      "CMA6" = CMA6,
                      "CMA7" = CMA7,
                      "CMA8" = CMA8,
                      "CMA9" = CMA9,
                      "per episode" = CMA_per_episode,
                      "sliding window" = CMA_sliding_window,
                      CMA0); # by default, fall back to CMA0
    # Try to catch errors and warnings for nice displaying:
    results <- NULL;
    full.results <- tryCatch( results <- cma.fnc( data,
                                                  CMA=cma.to.apply,
                                                  ID.colname=ID.colname,
                                                  event.date.colname=event.date.colname,
                                                  event.duration.colname=event.duration.colname,
                                                  event.daily.dose.colname=event.daily.dose.colname,
                                                  medication.class.colname=medication.class.colname,
                                                  date.format=date.format,
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
                                                  medication.change.means.new.treatment.episode=medication.change.means.new.treatment.episode,
                                                  maximum.permissible.gap=maximum.permissible.gap,
                                                  maximum.permissible.gap.unit=maximum.permissible.gap.unit,
                                                  sliding.window.start=sliding.window.start,
                                                  sliding.window.start.unit=sliding.window.start.unit,
                                                  sliding.window.duration=sliding.window.duration,
                                                  sliding.window.duration.unit=sliding.window.duration.unit,
                                                  sliding.window.step.duration=sliding.window.step.duration,
                                                  sliding.window.step.unit=sliding.window.step.unit,
                                                  sliding.window.no.steps=sliding.window.no.steps),
                              error  =function(e) return(list(results=results,error=conditionMessage(e))),
                              warning=function(w) return(list(results=results,warning=conditionMessage(w))));
    if( is.null(results) )
    {
      # Plot an error message:
      plot(-10:10,-10:10,type="n",axes=FALSE,xlab="",ylab="");
      text(0,0,paste0("Error computing '",cma,"' for patient '",ID,"'\n(see console for possible warnings or errors)!"),col="red");
      if( !is.null(full.results$error) )   cat(paste0("Error(s): ",paste0(full.results$error,collapse="\n")));
      if( !is.null(full.results$warning) ) cat(paste0("Warning(s): ",paste0(full.results$warning,collapse="\n")));
    } else
    {
      # Plot the results:
      plot(results,
           show.legend=show.legend,
           plot.CMA.as.histogram=plot.CMA.as.histogram,
           align.all.patients=align.all.patients,
           align.first.event.at.zero=align.first.event.at.zero
          );
    }
  }


  # put things in the global environment for shiny:
  .GlobalEnv$.plotting.params <- list("data"=data,
                                      "ID"=ID, "all.IDs"=all.IDs,
                                      "cma.class"=cma.class,
                                      "print.full.params"=print.full.params,
                                      "ID.colname"=ID.colname,
                                      "event.date.colname"=event.date.colname,
                                      "event.duration.colname"=event.duration.colname,
                                      "event.daily.dose.colname"=event.daily.dose.colname,
                                      "medication.class.colname"=medication.class.colname,
                                      "date.format"=date.format,
                                      "followup.window.start.max"=followup.window.start.max,
                                      "followup.window.duration.max"=followup.window.duration.max,
                                      "observation.window.start.max"=observation.window.start.max,
                                      "observation.window.duration.max"=observation.window.duration.max,
                                      "maximum.permissible.gap.max"=maximum.permissible.gap.max,
                                      "sliding.window.start.max"=sliding.window.start.max,
                                      "sliding.window.duration.max"=sliding.window.duration.max,
                                      "sliding.window.step.duration.max"=sliding.window.step.duration.max,
                                      "align.all.patients"=align.all.patients,
                                      "align.first.event.at.zero"=align.first.event.at.zero,
                                      ".plotting.fnc"=.plotting.fnc
                                     );
  # make sure they are deleted on exit from shiny:
  on.exit(rm(.plotting.params, envir=.GlobalEnv));

  # call shiny:
  if( use.system.browser )
  {
    shiny::runApp(system.file('interactivePlotShiny', package='AdhereR.devel'), launch.browser=TRUE);
  } else
  {
    shiny::runApp(system.file('interactivePlotShiny', package='AdhereR.devel'));
  }
}

