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


################ function to calculate simple CMA for polypharmacy

#' CMA constructor for polypharmacy.
#'
#' Constructs a CMA (continuous multiple-interval measures of medication
#' availability/gaps) object for polypharmacy.
#'
#'
#'#' @param data A \emph{\code{data.frame}} containing the events (prescribing or
#' dispensing) used to compute the CMA. Must contain, at a minimum, the patient
#' unique ID, the event date and duration, and might also contain the daily
#' dosage and medication type (the actual column names are defined in the
#' following four parameters).
#' @param polypharmacy.method A \emph{string} giving the name of the function to
#' aggregate CMA values of medication group, or \code{NA} to return raw CMA estimates
#' per medication group. Accepts custom options like "DPPR", "any", and "all", or
#' summary functions such as "mean", "sd", "var", "min", "max", and "median".
#' Custom functions are possible as long as they take a vector as input and return
#' a single value.
#' @param CMA.to.apply A \emph{string} giving the name of the CMA function (1 to
#' 9) that will be computed for each treatment group.
#' @param grouping A \emph{string} giving the name of the CMA function (1 to
#' 9) that will be computed for each treatment group.
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
#'  @param treat.epi A \emph{\code{data.frame}} containing the treatment episodes.
#'  Must contain the patient ID (\code{ID.colname}), the episode unique ID
#'  (increasing sequentially, \code{episode.ID}), the episode start date
#'  (\code{episode.start}), the episode duration in days (\code{episode.duration}),
#'  and the episode end date (\code{episode.end}).
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
#'  \item \code{polypharmacy.method} the aggregation method to combine CMA values
#'  from different groups.
#'  \item \code{computed.CMA} the class name of the computed CMA.
#'  \item \code{grouping} the column in \code{data} indicating the treatment group.
#'  \item \code{CMA} the \code{data.frame} containing the actual \code{CMA}
#'  estimates for each participant (the \code{ID.colname} column) and
#'  sometimes treatment group, with columns:
#'    \itemize{
#'      \item \code{ID.colname} the patient ID as given by the \code{ID.colname}
#'      parameter.
#'      \item \code{grouping} only when no aggregation method is used
#'      (\code{polypharmacy.method = NA}); the treatment group as given by the
#'      \code{grouping} parameter.
#'      \item \code{CMA} the treatment episode's estimated CMA.
#'    }
#' }
#' @examples
#' \dontrun{
#' CMA_PP <- CMA_polypharmacy(data = med.events,
#'                            polypharmacy.method = "DPPR",
#'                            CMA.to.apply = "CMA7",
#'                            grouping = "CATEGORY", #if multiple medication classes should belong to the same group, they can be differentiated here (important to investigate treatment switches)
#'                            ID.colname="PATIENT_ID",
#'                            event.date.colname="DATE",
#'                            event.duration.colname="DURATION",
#'                            event.daily.dose.colname="PERDAY",
#'                            medication.class.colname="CATEGORY",
#'                            date.format="%m/%d/%Y");}
#' @export
CMA_polypharmacy <- function(data = data,
                             polypharmacy.method = NA, #custom function possible, NA for raw data
                             CMA.to.apply = NA,
                             grouping = medication.class.colname, #if multiple medication classes should belong to the same group, they can be differentiated here (important to investigate treatment switches)
                             treat.epi=NULL, # the treatment episodes, if available
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

  # if grouping is a named list, merge grouping into data

  ### code to merge grouping information into rest of data

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
    # auxiliary function to compute intersection of episodes (for "DPPR", "any" and "all")
      episodes.intersections <- function(episode.start, episode.end, intersect.name = "intersect"){

        episode.dates <- sort(unique(c(episode.start,episode.end)))

        episodes <- data.table(episode.dates[1:(length(episode.dates)-1)],
                               episode.dates[2:length(episode.dates)])

        intersect.start.name <- paste0(intersect.name,".start")
        intersect.end.name <- paste0(intersect.name, ".end")
        intersect.duration.name <- paste0(intersect.name, ".duration")

        setnames(episodes, c(intersect.start.name, intersect.end.name))

        episodes[,(intersect.duration.name) := as.numeric(get(intersect.end.name) - get(intersect.start.name))]

      }

    # auxiliary function to handle precomputed treatment episodes
      compute.treat.epi <- function(treat.epi = treat.epi){

        # Convert treat.epi to data.table, cache event dat as Date objects, and key by patient ID and event date
        treat.epi <- as.data.table(treat.epi);
        treat.epi[, `:=` (episode.start = as.Date(episode.start,format=date.format),
                          episode.end = as.Date(episode.end,format=date.format)
        )]; # .DATE.as.Date: convert event.date.colname from formatted string to Date
        setkeyv(treat.epi, c(ID.colname, grouping, "episode.ID")); # key (and sorting) by patient and episode ID

        # Compute the real observation windows (might differ per patient) only once per patient (speed things up & the observation window is the same for all events within a patient):
        tmp <- data[!duplicated(data[,c(ID.colname,grouping), with = FALSE]),]; # the reduced dataset for computing the actual OW:
        tmp[,.PATIENT.MED.ID := paste(get(ID.colname),get(grouping),sep="*")]
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
        treat.epi <- merge(treat.epi, event.info2[,c(ID.colname, grouping, ".OBS.START.DATE", ".OBS.END.DATE"),with=FALSE],
                           all.x=TRUE,
                           by = c(ID.colname, grouping));
        setnames(treat.epi, ncol(treat.epi)-c(1,0), c(".OBS.START.DATE.PRECOMPUTED", ".OBS.END.DATE.PRECOMPUTED"));
        # Get the intersection between the episode and the observation window:
        treat.epi[, c(".INTERSECT.EPISODE.OBS.WIN.START",
                      ".INTERSECT.EPISODE.OBS.WIN.END")
                  := list(max(episode.start, .OBS.START.DATE.PRECOMPUTED),
                          min(episode.end,   .OBS.END.DATE.PRECOMPUTED)),
                  by=c(ID.colname,"episode.ID", grouping)];
        treat.epi <- treat.epi[ .INTERSECT.EPISODE.OBS.WIN.START < .INTERSECT.EPISODE.OBS.WIN.END, ]; # keep only the episodes which fall within the OW
        treat.epi[, c("episode.duration",
                      ".INTERSECT.EPISODE.OBS.WIN.DURATION",
                      ".PATIENT.MED.EPISODE.ID")
                  := list(as.numeric(episode.end - episode.start),
                          as.numeric(.INTERSECT.EPISODE.OBS.WIN.END - .INTERSECT.EPISODE.OBS.WIN.START),
                          paste(get(ID.colname),get(grouping),episode.ID,sep="*"))];

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


        episode.gap.days <- data.epi2[which(.EVENT.WITHIN.FU.WINDOW), c(ID.colname, grouping, "episode.ID", "gap.days"), by = c(ID.colname, "episode.ID", grouping), with = FALSE]; # gap days during the follow-up window
        end.episode.gap.days <- episode.gap.days[,.(end.episode.gap.days = last(gap.days)), by = c(ID.colname, "episode.ID", grouping)]; # gap days during the last event

        treat.epi <- merge(treat.epi, end.episode.gap.days, all.x = TRUE, by = c(ID.colname, "episode.ID", grouping)); # merge end.episode.gap.days back to data.epi

        treat.epi[, episode.duration := as.numeric(.INTERSECT.EPISODE.OBS.WIN.END-.INTERSECT.EPISODE.OBS.WIN.START)];

        browser()

        data.epi.ret <- data.epi[, c(ID.colname,
                                     grouping,
                                     event.date.colname,
                                     event.duration.colname,
                                     event.daily.dose.colname,
                                     medication.class.colname,
                                     ".PATIENT.MED.EPISODE.ID",
                                     "episode.start",
                                     "episode.duration",
                                     ".INTERSECT.EPISODE.OBS.WIN.START",
                                     ".INTERSECT.EPISODE.OBS.WIN.DURATION"), with = FALSE]

        setnames(data.epi.ret,
                 old = c(".PATIENT.MED.EPISODE.ID",
                         "episode.start",
                         "episode.duration",
                         ".INTERSECT.EPISODE.OBS.WIN.START",
                         ".INTERSECT.EPISODE.OBS.WIN.DURATION"),
                 new = c(".PATIENT.MED.EPISODE.ID",
                         "med.episode.start",
                         "med.episode.duration",
                         ".MED.INTERSECT.EPISODE.OBS.WIN.START",
                         ".MED.INTERSECT.EPISODE.OBS.WIN.DURATION"))

        treat.epi.ret <- treat.epi[,c(ID.colname,
                                      ".PATIENT.MED.EPISODE.ID",
                                      grouping,
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

      # cache data and various parameters
      data.2 <- copy(data)

      ID.colname.2 <- ID.colname
      followup.window.start.2 <- followup.window.start
      followup.window.duration.2 <- followup.window.duration
      observation.window.start.2 <- observation.window.start
      observation.window.duration.2 <- observation.window.duration
      observation.window.duration.unit.2 <- observation.window.duration.unit

      # adjust parameters if treat.epi is defined
      if(!is.null(treat.epi)){

        data.epi <- compute.treat.epi(treat.epi)

        data.2 <- copy(data.epi$data.epi)

        ID.colname.2 <- ".PATIENT.MED.EPISODE.ID"
        followup.window.start.2 <- "med.episode.start"
        followup.window.duration.2 <- "med.episode.duration"
        observation.window.start.2 <- ".MED.INTERSECT.EPISODE.OBS.WIN.START"
        observation.window.duration.2 <- ".MED.INTERSECT.EPISODE.OBS.WIN.DURATION"
        observation.window.duration.unit.2 <- "days"

      }

      # # Add back the patient and episode IDs:
      # tmp <- as.data.table(merge(cma$CMA, treat.epi)[,c(ID.colname, "episode.ID", "episode.start", "end.episode.gap.days", "episode.duration", "episode.end", "CMA")]);
      # setkeyv(tmp, c(ID.colname,"episode.ID"));

    # compute CMA per group for raw returns, simple (mean, median, min, max) or custom aggregation functions
browser()
    if(!polypharmacy.method %in% c("DPPR", "any", "all")){

      # compute CMAs by medication group
      CMA_all_by_group <- by(data.2, data.2[[grouping]], CMA.FNC,
                             CMA.to.apply = CMA.to.apply,
                             ID.colname = ID.colname.2,
                             event.date.colname = event.date.colname,
                             event.duration.colname = event.duration.colname,
                             medication.class.colname = medication.class.colname,
                             event.daily.dose.colname = event.daily.dose.colname,
                             followup.window.start=followup.window.start.2,
                             followup.window.start.unit=followup.window.start.unit,
                             followup.window.duration=followup.window.duration.2,
                             followup.window.duration.unit=followup.window.duration.unit,
                             observation.window.start=observation.window.start.2,
                             observation.window.start.unit=observation.window.start.unit,
                             observation.window.duration=observation.window.duration.2,
                             observation.window.duration.unit=observation.window.duration.unit.2,
                             date.format=date.format,parallel.backend="none")

      # get CMA values
      CMA_per_group <- lapply(CMA_all_by_group, FUN = function(x){
        cbind(grouping = first(x$data[[grouping]]), x$CMA)
      })
      CMA_per_group <- rbindlist(CMA_per_group)

      setkeyv(CMA_per_group, ID.colname)

      # get event information
      event_info <- lapply(CMA_all_by_group, FUN = function(x){
        x$event.info
      })
      event_info <- rbindlist(event_info)

      setkeyv(event_info, ID.colname)

      # return raw CMA values per medication group
      if(is.na(polypharmacy.method)) {

        CMA <- CMA_per_group[,c(ID.colname, "grouping", "CMA"), with = FALSE]

        setnames(CMA, old = "grouping", new = grouping)

        tmp <- list(CMA = CMA, event.info = event_info)

        return(tmp)

      }

      # aggregate CMAs for simple (mean, median, min, max) or custom aggregation functions

      if(exists(polypharmacy.method, mode='function')) { # check if aggregation method exists as a function
        POLY.FNC <- get(polypharmacy.method);
      } else {
        POLY.FNC <- mean; # by default, fall back to mean
        if( !suppress.warnings ) warning(paste0("Unknown 'polypharmacy.method' '",polypharmacy.method,"': defaulting to mean!\n)"));
      }

      CMA <- copy(CMA_per_group)
      CMA[,CMA := POLY.FNC(CMA, na.rm = TRUE), by = c(ID.colname)] # remove NA's to deal with missing CMA's

      CMA <- CMA[,grouping := NULL]

      # construct return object
      tmp <- list(CMA = unique(CMA), event.info = event_info)

      return(tmp)
    } else { # compute CMA for "DPPR", "any" and "all" aggregation functions

      # calculate CMA_per_episode with maximum permissible gap of 0 days
      CMA_full_per_day <- by(data.2, data.2[[grouping]],
                             treat.epi = NULL,
                             CMA_per_episode,
                             maximum.permissible.gap = 0,
                             maximum.permissible.gap.unit = "days",
                             CMA.to.apply = CMA.to.apply,
                             ID.colname = ID.colname.2,
                             event.date.colname = event.date.colname,
                             event.duration.colname = event.duration.colname,
                             medication.class.colname = medication.class.colname,
                             event.daily.dose.colname = event.daily.dose.colname,
                             followup.window.start=followup.window.start.2,
                             followup.window.start.unit=followup.window.start.unit,
                             followup.window.duration=followup.window.duration.2,
                             followup.window.duration.unit=followup.window.duration.unit,
                             observation.window.start=observation.window.start.2,
                             observation.window.start.unit=observation.window.start.unit,
                             observation.window.duration=observation.window.duration.2,
                             observation.window.duration.unit=observation.window.duration.unit.2,
                             carry.only.for.same.medication = carry.only.for.same.medication,
                             date.format=date.format,
                             suppress.warnings = TRUE)

      # get CMA values
      CMA_per_day <- lapply(CMA_full_per_day, FUN = function(x){
        cbind(grouping = first(x$data[[grouping]]), x$CMA)
      })
      CMA_per_day <- rbindlist(CMA_per_day)
      setkeyv(CMA_per_day, ID.colname)

      # get event information
      event_info <- lapply(CMA_full_per_day, FUN = function(x){
        cbind(grouping = first(x$data[[grouping]]), x$event.info)
      })
      event_info <- rbindlist(event_info)
      setkeyv(event_info, ID.colname)

      # create intersections of episodes
      episodes <- CMA_per_day[,episodes.intersections(episode.start, episode.end, "intersect"), by = ID.colname]
      episodes[,intersect.ID := seq_len(.N), by=ID.colname]

      CMA_per_day_intersect <- merge(CMA_per_day[,c("grouping",
                                                    ID.colname,
                                                    "episode.start",
                                                    "end.episode.gap.days",
                                                    "episode.duration",
                                                    "episode.end"), with = FALSE],
                                     episodes, by = ID.colname, allow.cartesian=TRUE)

      CMA_per_day_intersect <- merge(CMA_per_day_intersect, event_info, by = c(ID.colname, "grouping"))

      CMA_per_day_intersect[,CMA := 0] # set availability to 0

      # if intersection is covered by an episode, set CMA to 1
      CMA_per_day_intersect[intersect.start >= episode.start & intersect.end <= episode.end, CMA := 1]

      # if the OW starts after the end of the intersection or ends before the start of an intersection, set CMA to NA
      CMA_per_day_intersect[intersect.end <= .OBS.START.DATE | intersect.start >= .OBS.END.DATE, CMA := NA]

      # keep only one row per intersection
      CMA <- CMA_per_day_intersect[, .(CMA = max(CMA)), by = c(ID.colname,
                                                               "grouping",
                                                               "intersect.start",
                                                               "intersect.end",
                                                               "intersect.ID",
                                                               "intersect.duration",
                                                               ".OBS.START.DATE",
                                                               ".OBS.END.DATE")]

      # add number of medication groups per patient
      CMA[!is.na(CMA),med.groups := length(unique(grouping)), by = c(ID.colname, "intersect.ID")]

      # calculate availability of each medication group per patient and intersection
      CMA[,prop.med.groups.available := sum(CMA, na.rm = TRUE)/med.groups, by = c(ID.colname, "intersect.ID")]

      # select unique episodes per patient
      CMA <- unique(na.omit(CMA, cols = "CMA"), by = c(ID.colname, "intersect.ID"))

      # adjust end of last episode and intersect.duration
      CMA[intersect.end > .OBS.END.DATE, `:=` (intersect.end = .OBS.END.DATE,
                                               intersect.duration = as.numeric(.OBS.END.DATE-intersect.start))]

      CMA <- switch(as.character(polypharmacy.method),
                    "DPPR" = unique(CMA[,.(CMA = sum(intersect.duration*prop.med.groups.available)/(as.numeric(.OBS.END.DATE-.OBS.START.DATE))), by = ID.colname]),
                    "any" = unique(CMA[prop.med.groups.available > 0,.(CMA = sum(intersect.duration)/(as.numeric(.OBS.END.DATE-.OBS.START.DATE))), by = ID.colname]),
                    "all" = unique(CMA[round(prop.med.groups.available,0) == 1,.(CMA = sum(intersect.duration)/(as.numeric(.OBS.END.DATE-.OBS.START.DATE))), by = ID.colname]));

      tmp <- list(CMA = CMA, event.info = event_info)

      return(tmp)
    }

  }

  # Construct the return object:

  # Convert to data.table, cache event dat as Date objects, and key by patient ID and event date
  data.copy <- data.table(data);
  data.copy[, .DATE.as.Date := as.Date(get(event.date.colname),format=date.format)]; # .DATE.as.Date: convert event.date.colname from formatted string to Date
  setkeyv(data.copy, c(ID.colname, ".DATE.as.Date", grouping)); # key (and sorting) by patient ID and event date

  # Compute the workhorse function:
  tmp <- AdhereR:::.compute.function(.workhorse.function, fnc.ret.vals=2,
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
  class(ret.val) <- "CMA_polypharmacy";
  ret.val$event.info <- as.data.frame(tmp$event.info);
  ret.val$polypharmacy.method = polypharmacy.method;
  ret.val$computed.CMA <- CMA.to.apply;
  ret.val$grouping <- grouping;
  ret.val$summary <- summary;
  ret.val$CMA <- as.data.frame(tmp$CMA);
  setnames(ret.val$CMA, 1, ID.colname);

  return (ret.val);
}
