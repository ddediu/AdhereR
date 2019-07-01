###############################################################################################
#
#    AdhereRViz: interactive visualisations for AdhereR.
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

#' @import AdhereR
#' @import grDevices
#' @import graphics
#' @import stats
#' @import data.table
#' @import utils
#' @importFrom shinyjs useShinyjs extendShinyjs hidden disabled toggle onclick js enable disable
#' @importFrom shinyWidgets materialSwitch pickerInput updatePickerInput progressBar updateProgressBar
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable dbListTables dbGetQuery dbIsValid
#' @importFrom RMariaDB MariaDB
#' @importFrom RSQLite SQLite
#' @import V8
#' @importFrom clipr clipr_available write_clip
#' @importFrom colourpicker colourInput
#' @importFrom haven read_spss read_xpt read_sas read_stata
#' @importFrom highlight highlight renderer_html
#' @import knitr
#' @importFrom readODS read_ods
#' @importFrom readxl read_excel
#' @importFrom viridisLite magma inferno plasma viridis cividis
NULL

# Declare some variables as global to avoid NOTEs during package building:
globalVariables(c("patientID", "selectedCMA", "carry.only.for.same.medication", "consider.dosage.change", "followup.window.start",
                  "followup.window.start", "followup.window.duration", "observation.window.start", "observation.window.duration",
                  "show.legend", "medication.change.means.new.treatment.episode", "dosage.change.means.new.treatment.episode",
                  "maximum.permissible.gap", "maximum.permissible.gap.as.percent", "plot.CMA.as.histogram", "sliding.window.start",
                  "sliding.window.duration", "sliding.window.step.duration"));


#' Interactive exploration and CMA computation.
#'
#' Interactively plots the data for one of more patients, allowing the real-time
#' exploration of the various CMAs and their parameters.
#' It can use \code{Rstudio}'s \code{manipulate} library (deprecated) or
#' \code{Shiny} (recommended).
#'
#' The \code{manipulate} is kept for backward compatibility only, as it is much
#' more limited than \code{Shiny} and will receive no new development in the
#' future.
#' \code{Shiny} currently allows the use of any other data source besides a
#' default (and usual) \code{data.frame} (or derived), such a connection to an
#' \code{SQL} database. In this case, the user \emph{must} redefine the three
#' argument functions \code{get.colnames.fnc}, \code{get.patients.fnc} and
#' \code{get.data.for.patients.fnc} which collectively define an interface for
#' listing the column names, all the patient IDs, and for retreiving the actual
#' data for a (set of) patient ID(s). A fully worked example is described in
#' the vignette detailing the access to standard databases storaging the
#' patient information.
#' For more info please see the vignette.
#'
#' @param data Usually a \emph{\code{data.frame}} containing the events (prescribing
#' or dispensing) used to compute the CMA. Must contain, at a minimum, the patient
#' unique ID, the event date and duration, and might also contain the daily
#' dosage and medication type (the actual column names are defined in the
#' following four parameters). Alternatively, this can be any other data source
#' (for example, a connection to a database), in which case the user must redefine
#' the arguments \code{get.colnames.fnc}, \code{get.patients.fnc} and
#' \code{get.data.for.patients.fnc} appropriately. Currently, this works only when
#' using Shiny for interactive rendering. For a working example, please see
#' the vignette describing the interfacing with databases.
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
#' @param align.all.patients Should the patients be aligend?
#' @param align.first.event.at.zero Should the first event be put at zero?
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
#' @param use.system.browser For shiny, use the system browser?
#' @param get.colnames.fnc A \emph{function} taking as parameter the data source
#' and returning the column names. Must be overridden when the data source is
#' not derived from a \code{data.frame}.
#' @param get.patients.fnc A \emph{function} taking as parameter the data source
#' and the patient ID column name, and returns the list of all patient IDs.
#' Must be overridden when the data source is not derived from a \code{data.frame}.
#' @param get.data.for.patients.fnc A \emph{function} taking as parameter a (set
#' of) patient ID(s), the data source, and the patient ID column name, and returns
#' the list of all patient IDs. Must be overridden when the data source is not
#' derived from a \code{data.frame}.
#' @param ... Extra arguments.
#'
#' @seealso The vignette *AdhereR: Interactive plotting (and more) with Shiny*.
#'
#' @return Nothing
#' @examples
#' \dontrun{
#' library(AdhereR);
#' plot_interactive_cma(med.events,
#'                      ID.colname="PATIENT_ID",
#'                      event.date.colname="DATE",
#'                      event.duration.colname="DURATION",
#'                      event.daily.dose.colname="PERDAY",
#'                      medication.class.colname="CATEGORY");}
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
                                  backend=c("shiny","rstudio")[1], # the interactive backend to use
                                  use.system.browser=FALSE, # if shiny backend, use the system browser?
                                  get.colnames.fnc=function(d) names(d),
                                  get.patients.fnc=function(d, idcol) unique(d[[idcol]]),
                                  get.data.for.patients.fnc=function(patientid, d, idcol, cols=NA, maxrows=NA) d[ d[[idcol]] %in% patientid, ],
                                  ...
)
{
  # Clear any AdhereR errors, warning or messages, and start recording them:
  AdhereR:::.clear.ewms();
  AdhereR:::.record.ewms(record=TRUE);

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
                                get.colnames.fnc=get.colnames.fnc,
                                get.patients.fnc=get.patients.fnc,
                                get.data.for.patients.fnc=get.data.for.patients.fnc,
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

  # Clear any generated AdhereR errors, warning or messages, and stop recording them:
  AdhereR:::.clear.ewms();
  AdhereR:::.record.ewms(record=FALSE);
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
    if( inherits(data, "matrix") || inherits(data, "data.table") ) data <- as.data.frame(data); # make it a data.frame
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
                            dosage.change.means.new.treatment.episode=FALSE, # does a change in dosage automatically start a new treatment episode?
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
                 "dosage.change.means.new.treatment.episode=",dosage.change.means.new.treatment.episode,", ",
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
                                                  dosage.change.means.new.treatment.episode=dosage.change.means.new.treatment.episode,
                                                  maximum.permissible.gap=maximum.permissible.gap,
                                                  maximum.permissible.gap.unit=maximum.permissible.gap.unit,
                                                  sliding.window.start=sliding.window.start,
                                                  sliding.window.start.unit=sliding.window.start.unit,
                                                  sliding.window.duration=sliding.window.duration,
                                                  sliding.window.duration.unit=sliding.window.duration.unit,
                                                  sliding.window.step.duration=sliding.window.step.duration,
                                                  sliding.window.step.unit=sliding.window.step.unit,
                                                  sliding.window.no.steps=sliding.window.no.steps),
                              error  =function(e) return(list(results=results,error=conditionMessage(e))));
    if( is.null(results) )
    {
      # Plot an error message:
      plot(-10:10,-10:10,type="n",axes=FALSE,xlab="",ylab="");
      text(0,0,paste0("Error computing '",cma,"' for patient '",ID,"'\n(see console for possible warnings or errors)!"),col="red");
      if( !is.null(full.results$error) )   cat(paste0("Error(s): ",paste0(full.results$error,collapse="\n")));
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
                                         dosage.change.means.new.treatment.episode=NA, #dosage.change.means.new.treatment.episode
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
                           #dosage.change.means.new.treatment.episode = manipulate::checkbox(TRUE, "Dosage change starts new episode?"),
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
                                         dosage.change.means.new.treatment.episode=dosage.change.means.new.treatment.episode,
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
                           dosage.change.means.new.treatment.episode = manipulate::checkbox(TRUE, "Dosage change starts new episode?"),
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
                                         dosage.change.means.new.treatment.episode=NA,
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
                           #dosage.change.means.new.treatment.episode = manipulate::checkbox(TRUE, "Dosage change starts new episode?"),
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


# Auxiliary functions for interactive plot using shiny:
# The function encapsulating the actual plotting and CMA estimation:
.plotting.fnc.shiny <- function(data=NULL, # the data used to compute the CMA on
                                # Important columns in the data
                                ID.colname=NA, # the name of the column containing the unique patient ID (NA = undefined)
                                event.date.colname=NA, # the start date of the event in the date.format format (NA = undefined)
                                event.duration.colname=NA, # the event duration in days (NA = undefined)
                                event.daily.dose.colname=NA, # the prescribed daily dose (NA = undefined)
                                medication.class.colname=NA, # the classes/types/groups of medication (NA = undefined)
                                # Date format:
                                date.format=NA, # the format of the dates used in this function (NA = undefined)
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
                                dosage.change.means.new.treatment.episode=FALSE, # does a change in dosage automatically start a new treatment episode?
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
                                show.legend=TRUE, legend.x="right", legend.y="bottom", legend.bkg.opacity=0.5, legend.cex=0.75, legend.cex.title=1.0, # legend

                                # Labels and title:
                                xlab=c("dates"="Date", "days"="Days"),
                                ylab=c("withoutCMA"="patient", "withCMA"="patient (& CMA)"),
                                title=c("aligned"="Event patterns (all patients aligned)", "notaligned"="Event patterns"),

                                # Duration and period:
                                duration=NA, # duration to plot
                                show.period=c("dates","days")[2], period.in.days=90, # period on the x axis

                                # Colors and fonts:
                                bw.plot=FALSE,
                                show.cma=TRUE,
                                col.na="lightgray", col.cats=rainbow, unspecified.category.label="drug",
                                lty.event="solid", lwd.event=2, pch.start.event=15, pch.end.event=16,
                                col.continuation="black", lty.continuation="dotted", lwd.continuation=1,
                                cex=1.0, cex.axis=0.75, cex.lab=1.0,
                                highlight.followup.window=TRUE, followup.window.col="green",
                                highlight.observation.window=TRUE, observation.window.col="yellow", observation.window.density=35, observation.window.angle=-30, observation.window.opacity=0.3,
                                show.real.obs.window.start=TRUE, real.obs.window.density=35, real.obs.window.angle=30,
                                show.event.intervals=TRUE,
                                print.CMA=TRUE, CMA.cex=0.50,
                                plot.CMA=TRUE, CMA.plot.ratio=0.10, CMA.plot.col="lightgreen", CMA.plot.border="darkgreen", CMA.plot.bkg="aquamarine", CMA.plot.text="darkgreen",
                                plot.partial.CMAs.as=c("stacked"),
                                plot.partial.CMAs.as.stacked.col.bars="gray90",
                                plot.partial.CMAs.as.stacked.col.border="gray30",
                                plot.partial.CMAs.as.stacked.col.text="black",
                                plot.partial.CMAs.as.timeseries.vspace=7,
                                plot.partial.CMAs.as.timeseries.start.from.zero=TRUE,
                                plot.partial.CMAs.as.timeseries.col.dot="darkblue",
                                plot.partial.CMAs.as.timeseries.interval.type=c("none", "segments", "arrows", "lines", "rectangles")[2],
                                plot.partial.CMAs.as.timeseries.lwd.interval=1,
                                plot.partial.CMAs.as.timeseries.alpha.interval=0.25,
                                plot.partial.CMAs.as.timeseries.col.interval="gray70",
                                plot.partial.CMAs.as.timeseries.col.text="firebrick",
                                plot.partial.CMAs.as.timeseries.show.0perc=TRUE,
                                plot.partial.CMAs.as.timeseries.show.100perc=FALSE,
                                plot.partial.CMAs.as.overlapping.col.interval="gray70",
                                plot.partial.CMAs.as.overlapping.col.text="firebrick",

                                # Dose:
                                print.dose=FALSE, cex.dose=0.75, print.dose.outline.col="white", print.dose.centered=FALSE,
                                plot.dose=FALSE, lwd.event.max.dose=8, plot.dose.lwd.across.medication.classes=FALSE,

                                # Minimum plot size:
                                min.plot.size.in.characters.horiz=10, min.plot.size.in.characters.vert=0.5,

                                # Data accessor functions:
                                get.colnames.fnc=function(d) names(d),
                                get.patients.fnc=function(d, idcol) unique(d[[idcol]]),
                                get.data.for.patients.fnc=function(patientid, d, idcol, cols=NA, maxrows=NA) d[ d[[idcol]] %in% patientid, ],

                                # Plot the results or only compute the CMA and return it:
                                compute.cma.only=FALSE,

                                # Debugging
                                print.full.params=FALSE
)
{
  # Clear any AdhereR messages:
  AdhereR:::.clear.ewms();

  if( !compute.cma.only ) # for computing CMA only these messages are not very informative and positively distracting...
  {
    # Progress messages:
    AdhereR:::.write.ewms(paste0("Plotting patient ID '",ID,"' with CMA '",cma,"'",ifelse(cma.to.apply != "none",paste0(" ('",cma.to.apply,"')"),"")), "message", "plot_interactive_cma", "AdhereRViz");
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
                 "dosage.change.means.new.treatment.episode=",dosage.change.means.new.treatment.episode,", ",
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
  }


  # Do we need to recompute the CMA?
  recompute.CMA <- TRUE;
  # Check if anything really changed from the last time
  # (all the relevant stuff is stored in .GlobalEnv$.plotting.params$.recompute.CMA.old.params (which can be NULL or not defined the first time):
  if( compute.cma.only )
  {
    # Explicit computation of CMAs (not not for plotting): don't alter the saved parameter values or the cached CMA:
    recompute.CMA <- TRUE;
  } else if( # if not defined at all:
             is.null(pp <- .GlobalEnv$.plotting.params$.recompute.CMA.old.params) || # (and cache .GlobalEnv$.plotting.params$.recompute.CMA.old.params as "pp" for later use)
             # otherwise chaeck if anything meaningful has changed:
             # check if the data or any of its important attributed have changed:
             (!identical(pp$data, data) ||
              !identical(pp$ID.colname, ID.colname) ||
              !identical(pp$event.date.colname, event.date.colname) ||
              !identical(pp$event.duration.colname, event.duration.colname) ||
              !identical(pp$event.daily.dose.colname, event.daily.dose.colname) ||
              !identical(pp$medication.class.colname, medication.class.colname) ||
              !identical(pp$date.format, date.format) ||
              !identical(pp$get.colnames.fnc, get.colnames.fnc) ||
              !identical(pp$get.patients.fnc, get.patients.fnc) ||
              !identical(pp$get.data.for.patients.fnc, get.data.for.patients.fnc)) ||
             # check if the patients have changed:
             (!identical(pp$ID, ID)) ||
             # check if CMA or any of their relevant parameters have changed:
             (!identical(pp$cma, cma) ||
              (cma == "per episode" && # per episode specifically
               (!identical(pp$cma.to.apply, cma.to.apply) ||
                !identical(pp$medication.change.means.new.treatment.episode, medication.change.means.new.treatment.episode) ||
                !identical(pp$dosage.change.means.new.treatment.episode, dosage.change.means.new.treatment.episode) ||
                !identical(pp$maximum.permissible.gap.unit, maximum.permissible.gap.unit))) ||
              (cma == "siding window" && # sliding window specifically
               (!identical(pp$cma.to.apply, cma.to.apply) ||
                !identical(pp$sliding.window.start, sliding.window.start) ||
                !identical(pp$sliding.window.start.unit, sliding.window.start.unit) ||
                !identical(pp$sliding.window.duration, sliding.window.duration) ||
                !identical(pp$sliding.window.duration.unit, sliding.window.duration.unit) ||
                !identical(pp$sliding.window.step.duration, sliding.window.step.duration) ||
                !identical(pp$sliding.window.step.unit, sliding.window.step.unit) ||
                !identical(pp$sliding.window.no.steps, sliding.window.no.steps))) ||
              (!identical(pp$carryover.within.obs.window, carryover.within.obs.window) ||
               !identical(pp$carryover.into.obs.window, carryover.into.obs.window) ||
               !identical(pp$carry.only.for.same.medication, carry.only.for.same.medication) ||
               !identical(pp$consider.dosage.change, consider.dosage.change))) ||
             # check if the FUW or OW have changed:
             (!identical(pp$followup.window.start, followup.window.start) ||
              !identical(pp$followup.window.start.unit, followup.window.start.unit) ||
              !identical(pp$followup.window.duration, followup.window.duration) ||
              !identical(pp$followup.window.duration.unit, followup.window.duration.unit) ||
              !identical(pp$observation.window.start, observation.window.start) ||
              !identical(pp$observation.window.start.unit, observation.window.start.unit) ||
              !identical(pp$observation.window.duration, observation.window.duration) ||
              !identical(pp$observation.window.duration.unit, observation.window.duration.unit))
  )
  {
    # Create this structure :
    recompute.CMA <- TRUE;
    .GlobalEnv$.plotting.params$.recompute.CMA.old.params <- list( # changes to these params force the recomputation of the CMA (the CMA itself is cached in this structure as well
        "cached.CMA"=NULL, "cached.CMA.messages"=NULL, # the previously computed CMA and associated messages (if any)
        # The data:
        "data"=data,
        # Important columns in the data:
        "ID.colname"=ID.colname,
        "event.date.colname"=event.date.colname,
        "event.duration.colname"=event.duration.colname,
        "event.daily.dose.colname"=event.daily.dose.colname,
        "medication.class.colname"=medication.class.colname,
        # Date format:
        "date.format"=date.format,
        # The IDs and CMAs:
        "ID"=ID,
        "cma"=cma,
        "cma.to.apply"=cma.to.apply,
        # Various types medhods of computing gaps:
        "carryover.within.obs.window"=carryover.within.obs.window,
        "carryover.into.obs.window"=carryover.into.obs.window,
        "carry.only.for.same.medication"=carry.only.for.same.medication,
        "consider.dosage.change"=consider.dosage.change,
        # The follow-up window:
        "followup.window.start"=followup.window.start,
        "followup.window.start.unit"=followup.window.start.unit,
        "followup.window.duration"=followup.window.duration,
        "followup.window.duration.unit"=followup.window.duration.unit,
        # The observation window:
        "observation.window.start"=observation.window.start,
        "observation.window.start.unit"=observation.window.start.unit,
        "observation.window.duration"=observation.window.duration,
        "observation.window.duration.unit"=observation.window.duration.unit,
        # Treatment episodes:
        "medication.change.means.new.treatment.episode"=medication.change.means.new.treatment.episode,
        "dosage.change.means.new.treatment.episode"=dosage.change.means.new.treatment.episode,
        "maximum.permissible.gap.unit"=maximum.permissible.gap.unit,
        # Sliding window
        "sliding.window.start"=sliding.window.start,
        "sliding.window.start.unit"=sliding.window.start.unit,
        "sliding.window.duration"=sliding.window.duration,
        "sliding.window.duration.unit"=sliding.window.duration.unit,
        "sliding.window.step.duration"=sliding.window.step.duration,
        "sliding.window.step.unit"=sliding.window.step.unit,
        "sliding.window.no.steps"=sliding.window.no.steps,
        # Data accessor functions:
        "get.colnames.fnc"=get.colnames.fnc,
        "get.patients.fnc"=get.patients.fnc,
        "get.data.for.patients.fnc"=get.data.for.patients.fnc
      );
    pp <- .GlobalEnv$.plotting.params$.recompute.CMA.old.params; #make sure it's easier to access with a shorter name
  }

  # Preconditions (and data extraction):
  if( is.null(ID) ||
      is.null(data <- get.data.for.patients.fnc(ID, data, ID.colname)) || # extract the data for these IDs
      nrow(data)==0 )
  {
    if( compute.cma.only )
    {
      AdhereR:::.write.ewms(paste0("No data for patient ",ID), "warning", "plot_interactive_cma", "AdhereRViz");
    } else
    {
      plot(-10:10,-10:10,type="n",axes=FALSE,xlab="",ylab=""); text(0,0,paste0("Error: cannot display the data for patient '",ID,"'!"),col="red");
      AdhereR:::.write.ewms(paste0("Error: cannot display the data for patient '",ID,"'!"), "warning", "plot_interactive_cma", "AdhereRViz");
    }
    return (invisible(NULL));
  }

  # Compute the CMA:
  if( recompute.CMA )
  {
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
                                                  dosage.change.means.new.treatment.episode=dosage.change.means.new.treatment.episode,
                                                  maximum.permissible.gap=maximum.permissible.gap,
                                                  maximum.permissible.gap.unit=maximum.permissible.gap.unit,
                                                  sliding.window.start=sliding.window.start,
                                                  sliding.window.start.unit=sliding.window.start.unit,
                                                  sliding.window.duration=sliding.window.duration,
                                                  sliding.window.duration.unit=sliding.window.duration.unit,
                                                  sliding.window.step.duration=sliding.window.step.duration,
                                                  sliding.window.step.unit=sliding.window.step.unit,
                                                  sliding.window.no.steps=sliding.window.no.steps,
                                                  arguments.that.should.not.be.defined=NULL # avoid spurious warnings about overridden arguments
    ),
    error  =function(e) return(list(results=results,error=conditionMessage(e))),
    warning=function(w) return(list(results=results,warning=conditionMessage(w))));

    if( !compute.cma.only )
    {
      # Cache this new cma (and the associated messages, if any):
      pp$cached.CMA <- results;
      pp$cached.CMA.messages <- full.results;
    }
  } else
  {
    # Restore these from the case:
    results <- pp$cached.CMA;
    full.results <- pp$cached.CMA.messages;
  }

  if( is.null(results) )
  {
    if( compute.cma.only )
    {
      warning(paste0("Error computing '",cma,"' for patient '",ID,". ",
                     if( !is.null(full.results$error) )   paste0("Error(s): ",  paste0(full.results$error,collapse="; "),". "),
                     if( !is.null(full.results$warning) ) paste0("Warning(s): ",paste0(full.results$warning,collapse="; "),". ")));
      return (invisible(NULL));
    } else
    {
      # Plot an error message:
      plot(-10:10,-10:10,type="n",axes=FALSE,xlab="",ylab="");
      text(0,0,paste0("Error computing '",cma,"' for patient '",ID,"'\n(see console for possible warnings or errors)!"),col="red");
      if( !is.null(full.results$error) )   cat(paste0("Error(s): ",paste0(full.results$error,collapse="\n")));
      if( !is.null(full.results$warning) ) cat(paste0("Warning(s): ",paste0(full.results$warning,collapse="\n")));
    }
  } else
  {
    if( compute.cma.only )
    {
      return (invisible(results));
    } else
    {
      # Plot the results:
      plot(results,
           show.legend=show.legend, legend.x=legend.x, legend.y=legend.y, legend.bkg.opacity=legend.bkg.opacity, legend.cex=legend.cex, legend.cex.title=legend.cex.title,
           duration=duration,
           bw.plot=bw.plot,
           show.cma=show.cma,
           col.na=col.na, col.cats=col.cats, unspecified.category.label=unspecified.category.label,
           lty.event=lty.event, lwd.event=lwd.event, pch.start.event=pch.start.event, pch.end.event=pch.end.event,
           col.continuation=col.continuation, lty.continuation=lty.continuation, lwd.continuation=lwd.continuation,
           cex=cex, cex.axis=cex.axis, cex.lab=cex.lab,
           highlight.followup.window=highlight.followup.window, followup.window.col=followup.window.col,
           highlight.observation.window=highlight.observation.window, observation.window.col=observation.window.col,
           observation.window.density=observation.window.density, observation.window.angle=observation.window.angle, observation.window.opacity=observation.window.opacity,
           show.real.obs.window.start=show.real.obs.window.start, real.obs.window.density=real.obs.window.density, real.obs.window.angle=real.obs.window.angle,
           show.event.intervals=show.event.intervals,
           print.CMA=print.CMA, CMA.cex=CMA.cex,
           plot.CMA=plot.CMA, CMA.plot.ratio=CMA.plot.ratio, CMA.plot.col=CMA.plot.col, CMA.plot.border=CMA.plot.border, CMA.plot.bkg=CMA.plot.bkg, CMA.plot.text=CMA.plot.text,
           plot.partial.CMAs.as=plot.partial.CMAs.as,
           plot.partial.CMAs.as.stacked.col.bars=plot.partial.CMAs.as.stacked.col.bars,
           plot.partial.CMAs.as.stacked.col.border=plot.partial.CMAs.as.stacked.col.border,
           plot.partial.CMAs.as.stacked.col.text=plot.partial.CMAs.as.stacked.col.text,
           plot.partial.CMAs.as.timeseries.vspace=plot.partial.CMAs.as.timeseries.vspace,
           plot.partial.CMAs.as.timeseries.start.from.zero=plot.partial.CMAs.as.timeseries.start.from.zero,
           plot.partial.CMAs.as.timeseries.col.dot=plot.partial.CMAs.as.timeseries.col.dot,
           plot.partial.CMAs.as.timeseries.interval.type=plot.partial.CMAs.as.timeseries.interval.type,
           plot.partial.CMAs.as.timeseries.lwd.interval=plot.partial.CMAs.as.timeseries.lwd.interval,
           plot.partial.CMAs.as.timeseries.alpha.interval=plot.partial.CMAs.as.timeseries.alpha.interval,
           plot.partial.CMAs.as.timeseries.col.interval=plot.partial.CMAs.as.timeseries.col.interval,
           plot.partial.CMAs.as.timeseries.col.text=plot.partial.CMAs.as.timeseries.col.text,
           plot.partial.CMAs.as.timeseries.show.0perc=plot.partial.CMAs.as.timeseries.show.0perc,
           plot.partial.CMAs.as.timeseries.show.100perc=plot.partial.CMAs.as.timeseries.show.100perc,
           plot.partial.CMAs.as.overlapping.col.interval=plot.partial.CMAs.as.overlapping.col.interval,
           plot.partial.CMAs.as.overlapping.col.text=plot.partial.CMAs.as.overlapping.col.text,
           min.plot.size.in.characters.horiz=min.plot.size.in.characters.horiz, min.plot.size.in.characters.vert=min.plot.size.in.characters.vert,
           show.period=show.period, period.in.days=period.in.days,
           plot.CMA.as.histogram=plot.CMA.as.histogram,
           xlab=xlab,
           ylab=ylab,
           title=title,
           align.all.patients=align.all.patients,
           align.first.event.at.zero=align.first.event.at.zero,
           print.dose=print.dose, cex.dose=cex.dose, print.dose.outline.col=print.dose.outline.col, print.dose.centered=print.dose.centered,
           plot.dose=plot.dose, lwd.event.max.dose=lwd.event.max.dose, plot.dose.lwd.across.medication.classes=plot.dose.lwd.across.medication.classes
      );
    }
  }
}

# The shiny plotting itself:
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
                                        align.all.patients=FALSE, align.first.event.at.zero=TRUE, # should all patients be aligned? if so, place first event the horizontal 0?
                                        use.system.browser=FALSE, # by default, don't necessarily use the system browser
                                        get.colnames.fnc=function(d) names(d),
                                        get.patients.fnc=function(d, idcol) unique(d[[idcol]]),
                                        get.data.for.patients.fnc=function(patientid, d, idcol, cols=NA, maxrows=NA) d[ d[[idcol]] %in% patientid, ],
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
    # certain types of data must be coerced to data.frame:
    if( inherits(data, "matrix") || inherits(data, "data.table") ) data <- as.data.frame(data); # make it a data.frame
    ## Note: the following checks do not apply anymore as we could pass, for example, a database connection!!!
    # if( !inherits(data, "data.frame") )
    # {
    #   stop("The 'data' must be of type 'data.frame', 'matrix', or something derived from them!\n");
    #   return (NULL);
    # }
    # if( nrow(data) < 1 )
    # {
    #   stop("The 'data' must have at least one row!\n");
    #   return (NULL);
    # }
    # the column names must exist in data:
    column.names <- get.colnames.fnc(data);
    if( is.na(ID.colname) )
    {
      stop(paste0("Column ID.colname cannot be NA!\n"));
      return (NULL);
    }
    if( !is.na(ID.colname) && !(ID.colname %in% column.names) )
    {
      stop(paste0("Column ID.colname='",ID.colname,"' must appear in the 'data'!\n"));
      return (NULL);
    }
    # get the patients:
    all.IDs <- sort(get.patients.fnc(data, ID.colname));
    if( length(all.IDs) < 1 )
    {
      stop("The 'data' must contain at least one patient!\n");
      return (NULL);
    }
    if( !is.na(event.date.colname) && !(event.date.colname %in% column.names) )
    {
      stop(paste0("Column event.date.colname='",event.date.colname,"' must appear in the 'data'!\n"));
      return (NULL);
    }
    if( !is.na(event.duration.colname) && !(event.duration.colname %in% column.names) )
    {
      stop(paste0("Column event.duration.colname='",event.duration.colname,"' must appear in the 'data'!\n"));
      return (NULL);
    }
    if( !is.na(event.daily.dose.colname) && !(event.daily.dose.colname %in% column.names) )
    {
      stop(paste0("Column event.daily.dose.colname='",event.daily.dose.colname,"' must appear in the 'data'!\n"));
      return (NULL);
    }
    if( !is.na(medication.class.colname) && !(medication.class.colname %in% column.names) )
    {
      stop(paste0("Column medication.class.colname='",medication.class.colname,"' must appear in the 'data'!\n"));
      return (NULL);
    }

    # Check the requested ID (if any):
    if( is.null(ID) || is.na(ID) || !(ID %in% all.IDs) ) ID <- all.IDs[1];
  } else
  {
    #stop("The 'data' cannot be empty!\n");
    #return (NULL);

    all.IDs <- c("[not defined]");
    ID <- all.IDs[1];
    cma.class <- "simple";
    ID.colname <- event.date.colname <- event.duration.colname <- event.daily.dose.colname <- medication.class.colname <- NA;
    date.format <- NA;
  }

  # put things in the global environment for shiny:
  .GlobalEnv$.plotting.params <- list("data"=data,
                                      "cma.class"=cma.class,
                                      "ID.colname"=ID.colname,
                                      "event.date.colname"=event.date.colname,
                                      "event.duration.colname"=event.duration.colname,
                                      "event.daily.dose.colname"=event.daily.dose.colname,
                                      "medication.class.colname"=medication.class.colname,
                                      "date.format"=date.format,
                                      "align.all.patients"=align.all.patients,
                                      "align.first.event.at.zero"=align.first.event.at.zero,
                                      "ID"=ID, "all.IDs"=all.IDs,
                                      "max.number.patients.to.plot"=10, "max.number.events.to.plot"=500,
                                      "max.number.patients.to.compute"=100, "max.number.events.to.compute"=5000, "max.running.time.in.minutes.to.compute"=5,
                                      ".patients.to.compute"=NULL,
                                      "print.full.params"=print.full.params,
                                      "get.colnames.fnc"=get.colnames.fnc,
                                      "get.patients.fnc"=get.patients.fnc,
                                      "get.data.for.patients.fnc"=get.data.for.patients.fnc,
                                      ".plotting.fnc"=.plotting.fnc.shiny,
                                      ".dataset.type"=if(is.null(data)) NA else c("in memory", "from file", "SQL database")[2],
                                      ".dataset.comes.from.function.arguments"=!is.null(data),
                                      ".dataset.name"=NA,
                                      ".inmemory.dataset"=NULL,
                                      ".fromfile.dataset"=NULL,
                                      ".fromfile.dataset.filetype"=NULL,
                                      ".fromfile.dataset.header"=NULL,
                                      ".fromfile.dataset.sep"=NULL,
                                      ".fromfile.dataset.quote"=NULL,
                                      ".fromfile.dataset.dec"=NULL,
                                      ".fromfile.dataset.strip.white"=NULL,
                                      ".fromfile.dataset.na.strings"=NULL,
                                      ".fromfile.dataset.sheet"=NULL,
                                      ".db.connection.tables"=NULL,
                                      ".db.connection.selected.table"=NULL,
                                      ".db.connection"=NULL
                                     );

  # Make sure they are deleted on exit from shiny:
  on.exit({
            .GlobalEnv$.plotting.params <- NULL;
            # Clear any AdhereR errors, warning or messages, and start recording them:
            AdhereR:::.clear.ewms();
            AdhereR:::.record.ewms(record=FALSE);
          },
          add=TRUE);
  #on.exit(rm(list=c(".plotting.params"), envir=.GlobalEnv));

  # Clear any AdhereR errors, warning or messages, and start recording them:
  AdhereR:::.clear.ewms();
  AdhereR:::.record.ewms(record=TRUE);

  shiny.app.launcher <- system.file('interactivePlotShiny', package='AdhereRViz');

  # call shiny:
  if( use.system.browser )
  {
    shiny::runApp(shiny.app.launcher, launch.browser=TRUE);
  } else
  {
    shiny::runApp(shiny.app.launcher);
  }
}

