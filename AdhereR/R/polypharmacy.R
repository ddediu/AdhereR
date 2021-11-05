###############################################################################################
#
#    This file is part of AdhereR.
#    Copyright (C) 2018  Samuel Allemann, Dan Dediu & Alexandra Dima
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

# Declare some variables as global to avoid NOTEs during package building:
globalVariables(c(".PATIENT.MED.ID", ".new.ID", ".obs.duration", "EVENT.ID", "ID.colname",
                  "PATIENT_ID", "carry.only.for.same.medication", "carryover.from.last",
                  "carryover.into.obs.window", "carryover.total", "carryover.within.obs.window",
                  "consider.dosage.change", "date.format", "event.daily.dose.colname",
                  "event.date.colname", "event.duration.colname", "event.interval.colname",
                  "followup.window.duration", "followup.window.duration.unit",
                  "followup.window.start", "followup.window.start.unit", "gap.days",
                  "gap.days.colname", "intersect.ID", "intersect.duration", "intersect.end",
                  "intersect.start", "medication.class.colname", "medication.group",
                  "observation.window.duration", "observation.window.duration.unit",
                  "observation.window.start", "observation.window.start.unit", "process.seq",
                  "process.seq.num", "prop.trt.groups.available", "suppress.warnings",
                  "group"));


################ function to calculate simple CMA for polypharmacy

#' CMA constructor for polypharmacy.
#'
#' Constructs a CMA (continuous multiple-interval measures of medication
#' availability/gaps) object for polypharmacy.
#'
#'
#' @param data A \emph{\code{data.frame}} containing the events (prescribing or
#' dispensing) used to compute the CMA. Must contain, at a minimum, the patient
#' unique ID, the event date and duration, medication type, and might also contain the daily
#' dosage (the actual column names are defined in the
#' following four parameters).
#' @param medication.groups A \emph{string} with the name of the column containing the medication
#' groups. If multiple medication classes should belong to the same treatment group, they can be
#' differentiated here (important to investigate treatment switches)
#' @param CMA.to.apply A \emph{string} giving the name of the CMA function (1 to
#' 9) that will be computed for each treatment group.
#' @param aggregate.first \emph{Logical}, if \code{TRUE}, aggregate across treatment groups before
#' summarizing over time during OW.
#' @param aggregation.method A \emph{string} giving the name of the function to
#' aggregate CMA values of medication group, or \code{NA} to return only raw CMA estimates
#' per medication group. Accepts summary functions such as "mean", "sd", "var", "min", "max",
#' and "median". Custom functions are possible as long as they take a numeric vector as input
#' and return a single numeric value.
#' @param aggregation.method.arguments optional, A \emph{named list} of additional arguments to the
#' function given in \code{aggregation method}, e.g. \code{na.rm = TRUE}.
#' @param thresholds optional, a \emph{number} to apply as threshold between aggregation and summarizing.
#' @param ID.colname A \emph{string}, the name of the column in
#' \code{data} containing the medication type. Defaults to
#' \code{medication.class.colname}.
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
#' (see the follow-up window parameters above for details). Can be defined separately
#' for each patient and treatment group.
#' @param date.format A \emph{string} giving the format of the dates used in the
#' \code{data} and the other parameters; see the \code{format} parameters of the
#' \code{\link[base]{as.Date}} function for details (NB, this concerns only the
#' dates given as strings and not as \code{Date} objects).
#' @param summary Metadata as a \emph{string}, briefly describing this CMA.
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
#' @return An \code{S3} object of class \code{CMA_polypharmacy} with the
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
#'  \item \code{aggregation.method} the aggregation method to combine CMA values
#'  from different groups.
#'  \item \code{computed.CMA} the class name of the computed CMA.
#'  \item \code{medication.groups} a \code{data.frame} with medication groups and
#'  classes
#'  \item \code{CMA} the \code{data.frame} containing the actual \code{CMA}
#'  estimates for each participant (the \code{ID.colname} column) and
#'  sometimes treatment group, with columns:
#'    \itemize{
#'      \item \code{ID.colname} the patient ID as given by the \code{ID.colname}
#'      parameter.
#'      \item \code{medication.groups} only when no aggregation method is used
#'      (\code{aggregation.method = NA}); the treatment group as given by the
#'      \code{medication.groups} parameter.
#'      \item \code{CMA} the treatment episode's estimated CMA.
#'    }
#' }
#' @examples
#' \dontrun{
#' CMA_PP <- CMA_polypharmacy(data = med.events.pp,
#' medication.groups = med.groups,
#' CMA.to.apply = "CMA7",
#' aggregate.first = TRUE, # aggregate before summarizing
#' aggregation.method = "mean", # compute mean of CMAs
#' aggregation.method.arguments = list(na.rm = TRUE), # remove NA's during calculation
#' thresholds = NA, # don't apply threshold
#' ID.colname="PATIENT_ID",
#' event.date.colname="DATE",
#' event.duration.colname="DURATION",
#' event.daily.dose.colname="PERDAY",
#' medication.class.colname="CATEGORY",
#' followup.window.start=0,
#' observation.window.start=180,
#' observation.window.duration=365,
#' carry.only.for.same.medication = TRUE);}
#' @export
CMA_polypharmacy <- function(data = data,
                             medication.groups = medication.class.colname, #if multiple medication classes should belong to the same treatment group, they can be differentiated here (important to investigate treatment switches)
                             CMA.to.apply = NA,
                             aggregate.first = TRUE, #if TRUE, aggregate across treatment groups before summarizing during OW
                             aggregation.method = NA, #custom function possible, NA for raw data only
                             aggregation.method.arguments = NA, #a named list of additional arguments to aggregation function
                             thresholds = NA, #threshold to apply between aggregation and summarizing
                             ID.colname = NA,
                             event.date.colname = NA,
                             event.duration.colname = NA,
                             event.daily.dose.colname = NA,
                             medication.class.colname = NA,
                             carry.only.for.same.medication=NA, # if TRUE the carry-over applies only across medication of same type (NA = use the CMA's values)
                             consider.dosage.change=NA, # if TRUE carry-over is adjusted to reflect changes in dosage (NA = use the CMA's values)
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
                             summary="CMA for polypharmacy",
                             # Dealing with failed estimates:
                             force.NA.CMA.for.failed.patients=TRUE, # force the failed patients to have NA CM estimate?
                             # Parallel processing:
                             parallel.backend=c("none","multicore","snow","snow(SOCK)","snow(MPI)","snow(NWS)")[1], # parallel backend to use
                             parallel.threads="auto", # specification (or number) of parallel threads
                             # Misc:
                             suppress.warnings=FALSE,
                             ...){

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

  # get aggregation function

  if(exists(aggregation.method, mode='function')) { # check if aggregation method exists as a function
    POLY.FNC <- get(aggregation.method);
  } else {
    POLY.FNC <- mean; # by default, fall back to mean
    if( !suppress.warnings ) warning(paste0("Unknown 'aggregation.method' '",aggregation.method,"': defaulting to mean!\n)"));
  }

  # # Get the CMA class corresponding to the name
  # if( !(is.character(CMA.class) || is.factor(CMA.class)) )
  # {
  #   if( !suppress.warnings ) warning(paste0("'CMA.class' must be a string contining the name of the CMA class to apply!\n)"));
  #   return (NULL);
  # }

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
    if( !suppress.warnings ) warning("I know how to do CMA for polypharmacy only for CMAs 1 to 9!\n");
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

  # The workhorse auxiliary function: For a given (subset) of data, compute the polypharmacy CMA and event information:
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
    # auxiliary function to compute intersection of episodes (when aggregate.first = TRUE)
      episodes.intersections <- function(episode.start, episode.end,
                                         #.OBS.START.DATE, .OBS.END.DATE, #not used
                                         intersect.name = "intersect"){

        episode.dates <- sort(unique(c(episode.start,episode.end
                                       #, .OBS.START.DATE, .OBS.END.DATE #not used
                                       )))

        episodes <- data.table(episode.dates[1:(length(episode.dates)-1)],
                               episode.dates[2:length(episode.dates)])

        intersect.start.name <- paste0(intersect.name,".start")
        intersect.end.name <- paste0(intersect.name, ".end")
        intersect.duration.name <- paste0(intersect.name, ".duration")

        setnames(episodes, c(intersect.start.name, intersect.end.name))

        episodes[,(intersect.duration.name) := as.numeric(get(intersect.end.name) - get(intersect.start.name))]

      }

    # auxiliary function to apply thresholds

      apply.thresholds <- function(x, thresholds, medication.groups){

        if(is.list(thresholds)) {

          medication.groups <- first(medication.groups)

          if(medication.groups %in% names(thresholds)) {

            thresholds <- thresholds[[medication.groups]]

          } else {

            if( !suppress.warnings ) warning(paste0("No thresholds specified for treatment group '",medication.groups,"'!\n"));
            return (x);

          }
        }

      # if thresholds is numeric, cut and scale between 0 and 1
        if(is.numeric(thresholds)){
          breaks <- sort(unique(c(ifelse(min(x, na.rm = TRUE) >= min(thresholds), min(thresholds)+0.00001, 0),
                                  thresholds,
                                  ifelse(max(x, na.rm = TRUE) <= max(thresholds), max(thresholds)+1, max(x, na.rm = TRUE))
                                  )
                                )
                         )
          ret <- cut(x, breaks, include.lowest = TRUE, right = FALSE)
          ret <- as.vector(scale(as.numeric(ret)-1, center = FALSE, scale = nlevels(ret)-1))
        } else
        {
          if( !suppress.warnings ) warning("Thresholds must be either numeric or a named list of numeric thresholds per treatment group!\n");
          return (x);
        }

        return(ret)

      }

      # cache data and various parameters
      data.2 <- copy(data)

      ID.colname.2 <- ID.colname
      followup.window.start.2 <- followup.window.start
      followup.window.duration.2 <- followup.window.duration
      observation.window.start.2 <- observation.window.start
      observation.window.duration.2 <- observation.window.duration
      observation.window.duration.unit.2 <- observation.window.duration.unit

      setnames(data.2, old = .orig.ID.colname, new = "PATIENT_ID")

      # # Add back the patient and episode IDs:
      # tmp <- as.data.table(merge(cma$CMA, treat.epi)[,c(ID.colname, "episode.ID", "episode.start", "end.episode.gap.days", "episode.duration", "episode.end", "CMA")]);
      # setkeyv(tmp, c(ID.colname,"episode.ID"));

    # When aggregate.first = FALSE

      if(aggregate.first == FALSE){

      # compute CMAs by medication group
      # CMA_all_by_group <- by(data.2, data.2[["group"]], CMA.FNC,
      #                        CMA.to.apply = CMA.to.apply,
      #                        ID.colname = ID.colname.2,
      #                        event.date.colname = event.date.colname,
      #                        event.duration.colname = event.duration.colname,
      #                        medication.class.colname = medication.class.colname,
      #                        event.daily.dose.colname = event.daily.dose.colname,
      #                        followup.window.start=followup.window.start,
      #                        followup.window.start.unit=followup.window.start.unit,
      #                        followup.window.duration=followup.window.duration,
      #                        followup.window.duration.unit=followup.window.duration.unit,
      #                        observation.window.start=observation.window.start.2,
      #                        observation.window.start.unit=observation.window.start.unit,
      #                        observation.window.duration=observation.window.duration.2,
      #                        observation.window.duration.unit=observation.window.duration.unit.2,
      #                        date.format=date.format,parallel.backend="none")

      CMA_all_by_group <- CMA.FNC(data.2,
                                  ID.colname = ID.colname.2,
                                  event.date.colname = event.date.colname,
                                  event.duration.colname = event.duration.colname,
                                  medication.class.colname = medication.class.colname,
                                  event.daily.dose.colname = event.daily.dose.colname,
                                  followup.window.start=followup.window.start,
                                  followup.window.start.unit=followup.window.start.unit,
                                  followup.window.duration=followup.window.duration,
                                  followup.window.duration.unit=followup.window.duration.unit,
                                  observation.window.start=observation.window.start.2,
                                  observation.window.start.unit=observation.window.start.unit,
                                  observation.window.duration=observation.window.duration.2,
                                  observation.window.duration.unit=observation.window.duration.unit.2,
                                  carry.only.for.same.medication=carry.only.for.same.medication,
                                  date.format=date.format,parallel.backend="none")

      # get CMA values
      CMA_per_group <- as.data.table(getCMA(CMA_all_by_group))

      # merge back original ID and group and sort by original ID
      CMA_per_group <- merge(unique(data.2[,c(ID.colname, "PATIENT_ID", "group"), with = FALSE]), CMA_per_group, by = ID.colname)
      setkeyv(CMA_per_group, "PATIENT_ID")

      # CMA_per_group <- lapply(CMA_all_by_group, FUN = function(x){
      #   cbind(group = first(x$data[["group"]]), x$CMA)
      # })
      # CMA_per_group <- rbindlist(CMA_per_group)
      #
      # setkeyv(CMA_per_group, ID.colname)

      # construct intermediate CMA object

      CMA_intermediate <- CMA_per_group[,c("PATIENT_ID", "group", "CMA"), with = FALSE]
      setkeyv(CMA_intermediate, "PATIENT_ID")

      # get event information
      event_info <- as.data.table(CMA_all_by_group$event.info)

      # event_info <- lapply(CMA_all_by_group, FUN = function(x){
      #   x$event.info
      # })
      # event_info <- rbindlist(event_info)

      # merge back original ID and group and sort by original ID
      setkeyv(event_info, "PATIENT_ID")

      # aggregate CMAs

      CMA <- copy(CMA_per_group)

      # if thresholds are provided
      if(length(thresholds) >= 1 && !is.na(thresholds)){
        CMA[,CMA := apply.thresholds(CMA, thresholds, as.character(group)), by = group]
      }

      CMA[,CMA := do.call(POLY.FNC, c(list(x=CMA), aggregation.method.arguments)), by = c("PATIENT_ID")]

      CMA <- CMA[,`:=` (group = NULL,
                        .new.ID = NULL)]

      # construct return object
      tmp <- list(CMA = unique(CMA), CMA.intermediate = CMA_intermediate, event.info = event_info[,c(ID.colname) := NULL])

      return(tmp)

    } else { # compute CMA if aggregate.first = TRUE


      # auxiliary function to summarize over time
      summarize_longitudinal <- function(data, ID){

        # if(anyNA(data$prop.trt.groups.available)){
        #
        #     if( !suppress.warnings ) warning(paste0("Aggregation of availability returns NA for patient with ID '",first(ID), "' for some intervals, only considering other intervals for final results!\n"));
        # }

        #rep(unique(na.omit(data, cols = "prop.trt.groups.available")
                   data[,sum(intersect.duration*prop.trt.groups.available)/.obs.duration]
        #          ), nrow(data))

      }

      # calculate CMA_per_episode with maximum permissible gap of 0 days
      CMA_full_per_day <- CMA_per_episode(data.2,
                                          treat.epi = NULL,
                                          maximum.permissible.gap = 0,
                                          maximum.permissible.gap.unit = "days",
                                          CMA.to.apply = CMA.to.apply,
                                          ID.colname = ID.colname.2,
                                          event.date.colname = event.date.colname,
                                          event.duration.colname = event.duration.colname,
                                          medication.class.colname = medication.class.colname,
                                          event.daily.dose.colname = event.daily.dose.colname,
                                          followup.window.start=followup.window.start,
                                          followup.window.start.unit=followup.window.start.unit,
                                          followup.window.duration=followup.window.duration,
                                          followup.window.duration.unit=followup.window.duration.unit,
                                          observation.window.start=observation.window.start.2,
                                          observation.window.start.unit=observation.window.start.unit,
                                          observation.window.duration=observation.window.duration.2,
                                          observation.window.duration.unit=observation.window.duration.unit.2,
                                          carry.only.for.same.medication = carry.only.for.same.medication,
                                          date.format=date.format,
                                          force.NA.CMA.for.failed.patients = TRUE,
                                          suppress.warnings = TRUE
                                          )

      # select ID.colnames
      # ifelse(ID.colname == ID.colname.2, ID.colnames <- ID.colname, ID.colnames <- c(ID.colname, ID.colname.2))

      # get CMA values
      CMA_per_day <- as.data.table(getCMA(CMA_full_per_day))

      # merge back original ID and group and sort by original ID
      CMA_per_day <- merge(unique(data.2[,c(ID.colname, "PATIENT_ID", "group"), with = FALSE]), CMA_per_day, by = ID.colname)
      setkeyv(CMA_per_day, "PATIENT_ID")

      if(CMA.to.apply == "CMA9") {

        CMA_per_day[,`:=` (episode.end = episode.end + end.episode.gap.days,
                           CMA = episode.duration / (episode.duration + end.episode.gap.days),
                           episode.duration = episode.duration + end.episode.gap.days
                           )
                    ]

      }

      # get event information
      event_info <- as.data.table(CMA_full_per_day$event.info)

      # merge back original ID and group and sort by original ID
      event_info <- merge(unique(data.2[,c(ID.colname, "PATIENT_ID", "group"), with = FALSE]), event_info, by = ID.colname)
      setkeyv(event_info, "PATIENT_ID")

      # merge event_info to CMA_per_day
      # CMA_per_day <- merge(CMA_per_day, event_info[,c(ID.colname, ".OBS.START.DATE", ".OBS.END.DATE"), with = FALSE], by = c(ID.colname)) #not used

      # create intersections of availability episodes
      episodes <- CMA_per_day[,episodes.intersections(episode.start, episode.end,
                                                      #.OBS.START.DATE, .OBS.END.DATE, # not used
                                                      "intersect"), by = "PATIENT_ID"]
      episodes[,intersect.ID := seq_len(.N), by = "PATIENT_ID"]

      CMA_per_day_intersect <- merge(CMA_per_day[,c("group",
                                                    "PATIENT_ID",
                                                    "episode.start",
                                                    "end.episode.gap.days",
                                                    "episode.duration",
                                                    "episode.end",
                                                    "CMA"), with = FALSE],
                                     episodes, by = "PATIENT_ID", allow.cartesian=TRUE)

      CMA_per_day_intersect <- merge(CMA_per_day_intersect, event_info, by = c("PATIENT_ID", "group"))

      # CMA_per_day_intersect[,CMA := 0] # set availability to 0

      # if intersection is not covered by an episode, set CMA to 0
      CMA_per_day_intersect[intersect.start >= episode.end | intersect.end <= episode.start, CMA := 0]


      # if the OW starts after the end of the intersection or ends before the start of an intersection, set CMA to NA
      CMA_per_day_intersect[intersect.end <= .OBS.START.DATE | intersect.start >= .OBS.END.DATE, CMA := NA]

      # keep only one row per intersection
      CMA <- CMA_per_day_intersect[, list(CMA = max(CMA)), by = c("PATIENT_ID",
                                                                  "group",
                                                                  "intersect.start",
                                                                  "intersect.end",
                                                                  "intersect.ID",
                                                                  "intersect.duration",
                                                                  ".OBS.START.DATE",
                                                                  ".OBS.END.DATE")]

      # # add number of medication groups per patient
      # CMA[!is.na(CMA),med.groups := length(unique(medication.groups)), by = c(ID.colname, "intersect.ID")]

      # aggregate availability of all medication group per patient and intersection
      CMA[,prop.trt.groups.available := do.call(POLY.FNC, c(list(x=CMA), aggregation.method.arguments)), by = c("PATIENT_ID", "intersect.ID")]

      CMA_intermediate <- CMA[,c("PATIENT_ID",
                                 "group",
                                 "intersect.start",
                                 "intersect.end",
                                 "intersect.ID",
                                 "intersect.duration",
                                 "CMA",
                                 "prop.trt.groups.available"), with = FALSE]

      CMA_intermediate <- dcast(CMA, PATIENT_ID + intersect.start + intersect.end + intersect.ID + intersect.duration + prop.trt.groups.available ~ group, value.var = "CMA")

      # if thresholds are provided
      if(!is.na(thresholds)){
        CMA[,prop.trt.groups.available := apply.thresholds(prop.trt.groups.available, thresholds, as.character(group)), by = group]
      }

      # select unique episodes per patient
      CMA[, `:=` (.OBS.START.DATE = min(.OBS.START.DATE),
                  .OBS.END.DATE = max(.OBS.END.DATE)),
          by = "PATIENT_ID"]
      CMA <- unique(na.omit(CMA, cols = "CMA"), by = c("PATIENT_ID", "intersect.ID"))

      # adjust end of last episode and intersect.duration
      CMA[intersect.end > .OBS.END.DATE, `:=` (intersect.end = .OBS.END.DATE,
                                               intersect.duration = as.numeric(.OBS.END.DATE-intersect.start))]
      # compute total duration of observation
      CMA[,.obs.duration := as.numeric(.OBS.END.DATE-.OBS.START.DATE), by = c("PATIENT_ID")]
      # CMA_ret <- switch(as.character(aggregation.method),
      #               "DPPR" = unique(CMA[,list(CMA = sum(intersect.duration*prop.med.groups.available)/.obs.duration), by = c(ID.colname)]),
      #               "any" = unique(CMA[prop.med.groups.available > 0,list(CMA = sum(intersect.duration)/.obs.duration), by = ID.colname]),
      #               "all" = unique(CMA[round(prop.med.groups.available,0) == 1,list(CMA = sum(intersect.duration)/.obs.duration), by = ID.colname]));

      CMA_ret <- unique(CMA[,list(CMA = summarize_longitudinal(data = .SD, ID = PATIENT_ID)), by = c("PATIENT_ID")])

      setnames(CMA_ret, old = "PATIENT_ID", .orig.ID.colname)
      setnames(CMA_intermediate, old = "PATIENT_ID", .orig.ID.colname)
      setnames(event_info, old = "PATIENT_ID", .orig.ID.colname)

      tmp <- list(CMA = CMA_ret, CMA.intermediate = CMA_intermediate, event.info = event_info)

      return(tmp)
    }

  }

  # Construct the return object:

  # Convert to data.table, cache event dat as Date objects, and key by patient ID and event date
  data.copy <- data.table(data);
  data.copy[, .DATE.as.Date := as.Date(get(event.date.colname),format=date.format)]; # .DATE.as.Date: convert event.date.colname from formatted string to Date

  # if medication.groups is a named list, merge medication.groups into data
  if(is.character(medication.groups)){

    if(medication.groups %in% names(data.copy)){

      med.groups.dt <- unique(data.copy[,c(medication.groups, medication.class.colname), with = FALSE])
      if(medication.groups == medication.class.colname){
        setnames(med.groups.dt, c("group", medication.class.colname))
        data.copy <- merge(data.copy, med.groups.dt, by = medication.class.colname)

      } else {setnames(data.copy, old = medication.groups, new = "group")}

    } else {

      if( !suppress.warnings ) warning(paste0("Column medication.groups = '",medication.groups,"' must appear in the data!\n)"));
      return (NULL);
    }

  # } else if(.check.medication.groups(medication.groups,
  #                                    list.of.medication.classes = unique(data[[medication.class.colname]])))
  # {
  # 
  #   if(is.null(medication.groups)){
  #     medication.groups <- unique(data[[medication.class.colname]])
  #   }
  # 
  #   med.groups.dt <- as.data.table(.fill.medication.groups(medication.groups,
  #                                                          list.of.medication.classes = unique(data[[medication.class.colname]]),
  #                                                          already.checked = TRUE))
  # 
  #   setnames(med.groups.dt, old = "class", new = medication.class.colname)
  # 
  #   data.copy <- merge(data.copy, med.groups.dt, by = medication.class.colname)

  } else {
    return (NULL);
  }

  # create new ID from Patient ID and medication group
  data.copy[,.new.ID := paste(get(ID.colname), group, sep = "_")]

  .orig.ID.colname = ID.colname

  # Compute the workhorse function:
  tmp <- .compute.function(.workhorse.function, fnc.ret.vals=3,
                           parallel.backend=parallel.backend,
                           parallel.threads=parallel.threads,
                           data=data.copy,
                           ID.colname=".new.ID",
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
  if( is.null(tmp) || is.null(tmp$CMA) || is.null(tmp$event.info) || is.null(tmp$CMA.intermediate)) return (NULL);

  # Construct the return object:
  class(ret.val) <- "CMA_polypharmacy";
  ret.val$event.info <- as.data.frame(tmp$event.info);
  ret.val$aggregation.method = aggregation.method;
  ret.val$computed.CMA <- CMA.to.apply;
  ret.val$medication.groups <- as.data.frame(med.groups.dt);
  ret.val$summary <- summary;
  ret.val$CMA <- as.data.frame(tmp$CMA);
  ret.val$CMA.intermediate <- as.data.frame(tmp$CMA.intermediate);

  return (ret.val);
}

#' @export
getCMA.CMA_polypharmacy <- function(x, flatten.medication.groups=FALSE, medication.groups.colname=".MED_GROUP_ID")
{
  cma <- x; # parameter x is required for S3 consistency, but I like cma more
  if( is.null(cma) || !inherits(cma, "CMA_polypharmacy") || !("CMA" %in% names(cma)) || is.null(cma$CMA) ) return (NULL);
  return (cma$CMA);
}

subsetCMA.CMA_polypharmacy <- function(cma, patients, suppress.warnings=FALSE)
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
    if( !suppress.warnings ) warning("No patients to subset on!\n");
    return (NULL);
  }
  if( length(patients.to.keep) < length(patients) && !suppress.warnings ) warning("Some patients in the subsetting set are not in the CMA itsefl and are ignored!\n");

  ret.val <- cma;
  ret.val$data <- ret.val$data[ ret.val$data[,ret.val$ID.colname] %in% patients.to.keep, ];
  if( !is.null(ret.val$event.info) ) ret.val$event.info <- ret.val$event.info[ ret.val$event.info[,ret.val$ID.colname] %in% patients.to.keep, ];
  if( ("CMA" %in% names(ret.val)) && !is.null(ret.val$CMA) ) ret.val$CMA <- ret.val$CMA[ ret.val$CMA[,ret.val$ID.colname] %in% patients.to.keep, ];
  return (ret.val);
}


# for CMA_per_episode:

{
  # auxiliary function to handle precomputed treatment episodes
  compute.treat.epi <- function(treat.epi = treat.epi){

    # Convert treat.epi to data.table, cache event dat as Date objects, and key by patient ID and event date
    treat.epi <- as.data.table(treat.epi);
    treat.epi[, `:=` (episode.start = as.Date(episode.start,format=date.format),
                      episode.end = as.Date(episode.end,format=date.format)
    )]; # .DATE.as.Date: convert event.date.colname from formatted string to Date
    setkeyv(treat.epi, c(ID.colname, group, "episode.ID")); # key (and sorting) by patient and episode ID

    # Compute the real observation windows (might differ per patient) only once per patient (speed things up & the observation window is the same for all events within a patient):
    tmp <- data[!duplicated(data[,c(ID.colname,group), with = FALSE]),]; # the reduced dataset for computing the actual OW:
    tmp[,.PATIENT.MED.ID := paste(get(ID.colname),get(group),sep="*")]
    event.info2 <- compute.event.int.gaps(data=as.data.frame(tmp),
                                          ID.colname=".PATIENT.MED.ID",
                                          event.date.colname=event.date.colname,
                                          event.duration.colname=event.duration.colname,
                                          event.daily.dose.colname=event.daily.dose.colname,
                                          medication.class.colname=medication.class.colname,
                                          event.interval.colname="event.interval",
                                          gap.days.colname="gap.days",
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
    treat.epi <- merge(treat.epi, event.info2[,c(ID.colname, "group", ".OBS.START.DATE", ".OBS.END.DATE"),with=FALSE],
                       all.x=TRUE,
                       by = c(ID.colname, "group"));
    setnames(treat.epi, ncol(treat.epi)-c(1,0), c(".OBS.START.DATE.PRECOMPUTED", ".OBS.END.DATE.PRECOMPUTED"));
    # Get the intersection between the episode and the observation window:
    treat.epi[, c(".INTERSECT.EPISODE.OBS.WIN.START",
                  ".INTERSECT.EPISODE.OBS.WIN.END")
              := list(max(episode.start, .OBS.START.DATE.PRECOMPUTED),
                      min(episode.end,   .OBS.END.DATE.PRECOMPUTED)),
              by=c(ID.colname,"episode.ID", "group")];
    treat.epi <- treat.epi[ .INTERSECT.EPISODE.OBS.WIN.START < .INTERSECT.EPISODE.OBS.WIN.END, ]; # keep only the episodes which fall within the OW
    treat.epi[, c("episode.duration",
                  ".INTERSECT.EPISODE.OBS.WIN.DURATION",
                  ".PATIENT.MED.EPISODE.ID")
              := list(as.numeric(episode.end - episode.start),
                      as.numeric(.INTERSECT.EPISODE.OBS.WIN.END - .INTERSECT.EPISODE.OBS.WIN.START),
                      paste(get(ID.colname),group,episode.ID,sep="*"))];

    # Merge the data and the treatment episodes info:
    data.epi <- merge(treat.epi, data, allow.cartesian=TRUE);
    setkeyv(data.epi, c(".PATIENT.MED.EPISODE.ID", ".DATE.as.Date"));

    # compute end.episode.gap.days

    data.epi2 <- compute.event.int.gaps(data=as.data.frame(data.epi),
                                        ID.colname=".PATIENT.MED.EPISODE.ID",
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


    episode.gap.days <- data.epi2[which(.EVENT.WITHIN.FU.WINDOW), c(ID.colname, medication.group, "episode.ID", "gap.days"), by = c(ID.colname, "episode.ID", "group"), with = FALSE]; # gap days during the follow-up window
    end.episode.gap.days <- episode.gap.days[,list(end.episode.gap.days = last(gap.days)), by = c(ID.colname, "episode.ID", "group")]; # gap days during the last event

    treat.epi <- merge(treat.epi, end.episode.gap.days, all.x = TRUE, by = c(ID.colname, "episode.ID", "group")); # merge end.episode.gap.days back to data.epi

    treat.epi[, episode.duration := as.numeric(.INTERSECT.EPISODE.OBS.WIN.END-.INTERSECT.EPISODE.OBS.WIN.START)];

    data.epi.ret <- data.epi[, c(ID.colname,
                                 "groups",
                                 event.date.colname,
                                 event.duration.colname,
                                 event.daily.dose.colname,
                                 medication.class.colname,
                                 "episode.start",
                                 "episode.duration",
                                 ".PATIENT.MED.EPISODE.ID",
                                 ".INTERSECT.EPISODE.OBS.WIN.START",
                                 ".INTERSECT.EPISODE.OBS.WIN.DURATION",
                                 ".INTERSECT.EPISODE.OBS.WIN.END"), with = FALSE]

    setnames(data.epi.ret,
             old = c("episode.start",
                     "episode.duration",
                     ".INTERSECT.EPISODE.OBS.WIN.START",
                     ".INTERSECT.EPISODE.OBS.WIN.DURATION",
                     ".INTERSECT.EPISODE.OBS.WIN.END"),
             new = c("med.episode.start",
                     "med.episode.duration",
                     ".MED.INTERSECT.EPISODE.OBS.WIN.START",
                     ".MED.INTERSECT.EPISODE.OBS.WIN.DURATION",
                     ".MED.INTERSECT.EPISODE.OBS.WIN.END"))

    treat.epi.ret <- treat.epi[,c(ID.colname,
                                  ".PATIENT.MED.EPISODE.ID",
                                  "group",
                                  "episode.ID",
                                  "episode.start",
                                  "episode.end",
                                  "episode.duration",
                                  "end.episode.gap.days"),
                               with = FALSE]

    # construct return object
    ret <- list(data.epi = data.epi.ret, treat.epi = treat.epi.ret)

    return(ret)

  }

}




