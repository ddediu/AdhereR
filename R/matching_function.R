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
globalVariables(c("DATE.IN", "DATE.OUT",
                  "DATE.PRESC", "START.PRESC", "END.PRESC",
                  "DURATION", "HOSP.DURATION",
                  "DAILY.DOSE", "TOTAL.DOSE",
                  "DISP.START", "DISP.END",
                  "cum.duration", ".episode", ".out", ".hosp", "time.to.initialization", "first.disp", "first.presc",
                  "debug.mode"));


#' Example prescription events for 16 patients.
#'
#' A sample dataset containing prescription events (one per row) for 16 patients
#' over a period of roughly 15 months (1502 events in total).
#' This is the appropriate format to compute event durations with the
#' \code{compute_event_durations} function. Each row represent an individual prescription
#' record for a specific dose of a specific medication for a patient at a given date.
#' Visit number and Duration are optional, and more than one column to group medications
#' can be supplied (such as Form or Unit).
#'
#' @format A data table with 1502 rows and 8 variables:
#' \describe{
#'   \item{ID}{\emph{integer} here; patient unique identifier. Can also
#'   be \emph{string}.}
#'   \item{DATE.PRESC}{\emph{Date} here;the prescription event date, by default in the
#'   yyyy-mm-dd format. Can also be \emph{string}.}
#'   \item{VISIT}{\emph{integer}; the consecutive number of the prescription instances.
#'   This column is optional and will be generated internally when not supplied. It is
#'   used to identify treatment interruptions.}
#'   \item{ATC.CODE}{\emph{character}; the medication type, according to the WHO ATC
#'   classification system. This can be a researcher-defined classification
#'   depending on study aims (e.g., based on therapeutic use, mechanism of
#'   action, chemical molecule, or pharmaceutical formulation). The \code{compute_event_durations}
#'   function will match prescribed medication to dispensed medications based on this variable.}
#'   \item{FORM}{\emph{character}; the galenic form of the prescribed preparation.
#'   This is optional and can be used as a separate variable to match between prescription and
#'   dispensing events.}
#'   \item{UNIT}{\emph{integer}; the unit of the prescribed dose. This is optional and can be used
#'   as a separate variable to match between prescription and dispensing events.}
#'   \item{PRESC.DURATION}{\emph{numeric}; the duration (in days) for which the prescription
#'   is intended. Can be \code{NA} if the prescription is continuous without a fixed end date.}
#'   \item{DAILY.DOSE}{\emph{numeric}; the daily dose prescribed during this event (e.g., \code{50} for 1 tablet
#'   of 50 mg per day or \code{25} for 1 tablet of 50 mg every two days).}
#' }
"durcomp.prescribing"

#' Example dispensing events for 16 patients.
#'
#' A sample dataset containing dispensing events (one per row) for 16 patients
#' over a period of roughly 24 months (1794 events in total).
#' This is the appropriate format to compute event durations with the
#' \code{compute_event_durations} function. Each row represent an individual dispensing
#' record for a specific dose of a specific medication for a patient at a given date.
#' Visit number and Duration are optional, and more than one column to group medications
#' can be supplied (such as Form or Unit).
#'
#' @format A data frame with 1794 rows and 6 variables:
#' \describe{
#'   \item{ID}{\emph{integer} here; patient unique identifier. Can also
#'   be \emph{string}.}
#'   \item{DATE.DISP}{\emph{Date} here;the dispensing event date, by default in the
#'   yyyy-mm-dd format. Can also be \emph{string}.}
#'   \item{ATC.CODE}{\emph{character}; the medication type, according to the WHO ATC
#'   classification system. This can be a researcher-defined classification
#'   depending on study aims (e.g., based on therapeutic use, mechanism of
#'   action, chemical molecule, or pharmaceutical formulation). The \code{compute_event_durations}
#'   function will match prescribed medication to dispensed medications based on this variable.}
#'   \item{UNIT}{\emph{integer}; the unit of the dispensed dose. This is optional and can be used
#'   as a separate variable to match between prescription and dispensing events.}
#'   \item{FORM}{\emph{character}; the galenic form of the dispensed preparation.
#'   This is optional and can be used as a separate variable to match between prescription and
#'   dispensing events.}
#'   \item{TOTAL.DOSE}{\emph{numeric}; the total dispensed dose supplied at this
#'   medication event (e.g., \code{5000} for 10 tables of 500 mg).}
#' }
"durcomp.dispensing"

#' Example hospitalization events for 10 patients.
#'
#' A sample dataset containing hospitalization periods (one per row) for 10 patients
#' over a period of roughly 18 months (28 events in total).
#' This is the appropriate format to compute event durations with the
#' \code{compute_event_durations} function. Each row represent an individual hospitalization period
#' of a patient for whom event durations should be calculated. Besides hospitalizations, this could
#' cover other situations where patients may not use their own supply, e.g. during incarcerations.
#' All column names must match the format provided in this example.
#'
#' @format A data frame with 28 rows and 3 variables:
#' \describe{
#'   \item{ID}{\emph{integer} here; patient unique identifier. Can also
#'   be \emph{string}}.
#'   \item{DATE.IN}{\emph{Date} here;the start of the hospitalization period, by default in the
#'   yyyy-mm-dd format.Can also be \emph{string}.}
#'   \item{DATE.OUT}{\emph{Date};the end of the hospitalization period, by default in the
#'   yyyy-mm-dd format. Can also be \emph{string}.}
#' }
"durcomp.hospitalisation"




################ function to construct treatment episodes from dispensing and prescription databases

#' Computation of event durations.
#'
#' Computes event durations based on dispensing, prescription, and hospitalization
#' data and returns a \code{data.frame} which can be used with the CMA constructors
#' in \code{AdhereR}.
#'
#' Computation of CMAs requires a supply duration for medications dispensed to
#' patients. If medications are not supplied for fixed durations but as a quantity
#' that may last for various durations based on the prescribed dose, the supply
#' duration has to be calculated based on dispensed and prescribed doses. Treatments
#' may be interrupted and resumed at later times, for which existing supplies may
#' or may not be taken into account. Patients may be hospitalized or incarcerated,
#' and may not use their own supplies during these periods. This function calculates
#' supply durations, taking into account the aforementioned situations and providing
#' various parameters for flexible adjustments.
#'
#' @param disp.data A \emph{\code{data.frame}} or \emph{\code{data.table}} containing
#' the dispensing events. Must contain, at a minimum, the patient unique ID, one
#' medication identifier, the dispensing date, and total dispensed dose, and might
#' also contain additional columns to identify and group medications (the actual
#' column names are defined in the \emph{\code{medication.class.colnames}} parameter).
#' @param presc.data A \emph{\code{data.frame}} containing the prescribing events.
#' Must contain, at a minimum, the same unique patient ID and medication identifier(s)
#' as the dispensing data, the prescription date, the daily prescribed dose, and the
#' prescription duration. Optionally, it might also contain a visit number.
#' @param hosp.data Optional, \emph{\code{NULL}} or a \emph{\code{data.frame}}
#' containing the hospitalization periods (or other situations where patients may not
#' use their own supply, e.g. during incarcerations). Must contain the same unique
#' patient ID as dispensing and prescription data, and the start and end dates of the
#' hospitalizations with the exact column names \emph{\code{DATE.IN}} and
#' \emph{\code{DATE.OUT}}.
#' @param ID.colname A \emph{string}, the name of the column in \code{disp.data},
#' \code{presc.data}, and \code{hosp.data} containing the unique patient ID, or
#' \code{NA} if not defined.
#' @param presc.date.colname A \emph{string}, the name of the column in
#' \code{presc.data} containing the prescription date (in the format given in
#' the \code{date.format} parameter), or \code{NA} if not defined.
#' @param disp.date.colname A \emph{string}, the name of the column in
#' \code{disp.data} containing the dispensing date (in the format given in
#' the \code{date.format} parameter), or \code{NA} if not defined.
#' @param date.format A \emph{string} giving the format of the dates used in
#' the \code{data} and the other parameters; see the \code{format} parameters
#' of the \code{\link[base]{as.Date}} function for details (NB, this concerns
#' only the dates given as strings and not as \code{Date} objects).
#' @param medication.class.colnames A \emph{\code{Vector}} of \emph{strings}, the
#' name(s) of the column(s) in \code{disp.data} and \code{presc.data} containing
#' the classes/types/groups of medication, or \code{NA} if not defined.
#' @param total.dose.colname A \emph{string}, the name of the column in
#' \code{disp.data} containing the total dispensed dose as \code{numeric} (e.g.
#' \code{500} for 10 tablets of 50 mg), or \code{NA} if not defined.
#' @param presc.daily.dose.colname A \emph{string}, the name of the column in
#' \code{presc.data} containing the daily prescribed dose as \code{numeric} (e.g.
#' \code{50} for 50 mg once per day, or 25 for 50 mg once ever 2 days), or \code{NA}
#' if not defined.
#' @param presc.duration.colname A \emph{string}, the name of the column in
#' \code{presc.data} containing the duration of the prescription as \code{numeric}
#' or \code{NA} if duration is unknown.
#' @param visit.colname A \emph{string}, the name of the column in
#' \code{presc.data} containing the number of the visit, or \code{NA} if not defined.
#' @param force.init.presc \emph{Logical}. If \code{TRUE} advance the date of the
#' first prescription event to the date of the first dispensing event, if the first
#' prescription event is after the first dispensing event for a specific medication.
#' Only if the first prescription event is not limited in duration (as indicated in
#' the \code{presc.duration.colname}).
#' @param force.presc.renew \emph{Logical} or \emph{string}. If \code{TRUE} require
#' a new prescription for all medications for every prescription event (visit),
#' otherwise prescriptions end on the first visit without renewal. If \emph{string},
#' the name of the column containing the \code{Logical} for each medication class
#' separatly.
#' @param split.on.dosage.change \emph{Logical} or \emph{string}. If \code{TRUE}
#' split the dispensing event on days with dosage change and create a new event with
#' the new dosage for the remaining supply. If \emph{string}, the name of the column
#' containing the \code{Logical} for each medication class separatly. Important if
#' carryover should be considered later on.
#' @param trt.interruption can be either \emph{continue}, \emph{discard}, or
#' \emph{carryover}, and indicates how to handle supplies affected by treatment
#' interruptions or hospitalizations. With \emph{continue}, interruptions have
#' no effect on durations and dispensing start dates. With \emph{discard}, supplies
#' are truncated at the beginning of an interruption or hospitalization and the
#' remaining supply is discarded. With \emph{carryover}, supplies are truncated
#' at the beginning of an interruption or hospitalization and a new dispensing
#' start event with the remaining supply is created after the end of the interruption
#' or hospitalization.
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any warnings.
#' @param return.data.table \emph{Logical}, if \code{TRUE} return a
#' \code{data.table} object, otherwise a \code{data.frame}.
#' @param ... other possible parameters.
#' @return A \code{data.frame} or \code{data.table} with the following columns:
#' \itemize{
#'  \item \code{ID.colname} the unique patient ID, as given by the \code{ID.colname}
#'  parameter.
#'  \item \code{medication.class.colnames} the column(s) with classes/types/groups
#'  of medication, as given by the \code{medication.class.colnames} parameter.
#'  \item \code{total.dose.colname} the total dispensed dose, as given by the
#'  \code{total.dose.colname} parameter.
#'  \item \code{disp.date.colname} the date of the dispensing event, as given by
#'  the \code{disp.date.colnema} parameter.
#'  \item \code{DISP.START} the start date of the dispensing event, either the
#'  same as in \code{disp.date.colnema} or a later date in case of dosage changes
#'  or treatment interruptions/hospitalizations.
#'  \item \code{presc.daily.dose.colname} the prescribed daily dose, as given by
#'  the \code{presc.daily.dose.colname} parameter.
#'  \item \code{DURATION} the calculated duration of the supply, based on the total
#'  dispensed dose and the prescribed daily dose, starting from the \code{DISP.START}
#'  date.
#'  \item \code{START.PRESC} the start date of the prescription episode.
#'  \item \code{END.PRESC} the end date of the prescription episode.
#'  \item \code{HOSP.DURATION} the number of days during the current supply period
#'  affected by hospitalizations.
#'  \item \code{tot.presc.interruptions} the total number of prescription interruptions
#'  per patient for a specific medication.
#'  \item \code{tot.dosage.changes} the total number of dosage changes per patient
#'  for a specific medication.
#' }
#' @examples
#' event_durations <- compute_event_durations(disp.data = durcomp.dispensing[1:3,],
#'                                            presc.data = durcomp.prescribing,
#'                                            hosp.data = durcomp.hospitalisation,
#'                                            ID.colname = "ID",
#'                                            presc.date.colname = "DATE.PRESC",
#'                                            disp.date.colname = "DATE.DISP",
#'                                            date.format = "%Y-%m-%d",
#'                                            medication.class.colnames = c("ATC.CODE",
#'                                            "UNIT", "FORM"),
#'                                            total.dose.colname = "TOTAL.DOSE",
#'                                            presc.daily.dose.colname = "DAILY.DOSE",
#'                                            presc.duration.colname = "PRESC.DURATION",
#'                                            visit.colname = "VISIT",
#'                                            force.init.presc = TRUE,
#'                                            force.presc.renew = TRUE,
#'                                            split.on.dosage.change = TRUE,
#'                                            trt.interruption = "continue",
#'                                            suppress.warnings = FALSE,
#'                                            return.data.table = TRUE);
#' @export
compute_event_durations <- function(disp.data = NULL,
                                    presc.data = NULL,
                                    hosp.data = NULL,
                                    ID.colname = NA,
                                    presc.date.colname = NA,
                                    disp.date.colname = NA,
                                    date.format = "%d.%m.%Y",
                                    medication.class.colnames = NA,
                                    total.dose.colname = NA,
                                    presc.daily.dose.colname = NA,
                                    presc.duration.colname = NA,
                                    visit.colname = NA,
                                    force.init.presc = FALSE,
                                    force.presc.renew = FALSE,
                                    split.on.dosage.change = TRUE,
                                    trt.interruption = c("continue", "discard", "carryover")[1],
                                    suppress.warnings = FALSE,
                                    return.data.table = FALSE,
                                    ...)
{
  # Preconditions:
  {
    # dispensing data class and dimensions:
    if( inherits(disp.data, "matrix") ) disp.data <- as.data.table(disp.data); # convert matrix to data.table
    if( !inherits(disp.data, "data.frame") )
    {
      if( !suppress.warnings ) warning("The dispensing data must be of type 'data.frame'!\n");
      return (NULL);
    }
    if( nrow(disp.data) < 1 )
    {
      if( !suppress.warnings ) warning("The dispensing data must have at least one row!\n");
      return (NULL);
    }
    # prescribing data class and dimensions:
    if( inherits(presc.data, "matrix") ) presc.data <- as.data.table(presc.data); # convert matrix to data.table
    if( !inherits(presc.data, "data.frame") )
    {
      if( !suppress.warnings ) warning("The prescribing data must be of type 'data.frame'!\n");
      return (NULL);
    }
    if( nrow(presc.data) < 1 )
    {
      if( !suppress.warnings ) warning("The prescribing data must have at least one row!\n");
      return (NULL);
    }
    # hospitalization data class and dimensions:
    if(!is.null(hosp.data))
    {
      if( inherits(hosp.data, "matrix") ) hosp.data <- as.data.table(hosp.data); # convert matrix to data.table
      if( !inherits(hosp.data, "data.frame") )
      {
        if( !suppress.warnings ) warning("The hospitalisation data must be of type 'data.frame'!\n");
        return (NULL);
      }
      if( nrow(hosp.data) < 1 )
      {
        if( !suppress.warnings ) warning("The hospitalisation data must have at least one row!\n");
        return (NULL);
      }
    }

    # the column names must exist in dispensing and prescription data:
    if( !is.na(ID.colname) && !(ID.colname %in% names(disp.data)) && !(ID.colname %in% names(presc.data)))
    {
      if( !suppress.warnings ) warning(paste0("Column ID.colname='",ID.colname,"' must appear in the dispensing and prescribing data!\n"));
      return (NULL);
    }
    if( !is.na(presc.date.colname) && !(presc.date.colname %in% names(presc.data)) )
    {
      if( !suppress.warnings ) warning(paste0("Column presc.date.colname='",presc.date.colname,"' must appear in the prescribing data!\n"));
      return (NULL);
    }
    if( !is.na(disp.date.colname) && !(disp.date.colname %in% names(disp.data)) )
    {
      if( !suppress.warnings ) warning(paste0("Column disp.date.colname='",disp.date.colname,"' must appear in the dispensing data!\n"));
      return (NULL);
    }
    if( !is.na(medication.class.colnames) && !(medication.class.colnames %in% names(disp.data))  && !(medication.class.colnames %in% names(presc.data)) )
    {
      if( !suppress.warnings ) warning(paste0("Column medication.class.colnames='",medication.class.colnames,"' must appear in the dispensing and prescribing data!\n"));
      return (NULL);
    }
    if( !is.na(total.dose.colname) && !(total.dose.colname %in% names(disp.data)) )
    {
      if( !suppress.warnings ) warning(paste0("Column total.dose.colname='",total.dose.colname,"' must appear in the dispensing data!\n"));
      return (NULL);
    }
    if( !is.na(presc.daily.dose.colname) && !(presc.daily.dose.colname %in% names(presc.data)) )
    {
      if( !suppress.warnings ) warning(paste0("Column presc.daily.dose.colname='",presc.daily.dose.colname,"' must appear in the prescribing data!\n"));
      return (NULL);
    }
    if( !is.na(presc.duration.colname) && !(presc.duration.colname %in% names(presc.data)) )
    {
      if( !suppress.warnings ) warning(paste0("Column presc.duration.colname='",presc.duration.colname,"' must appear in the prescribing data!\n"));
      return (NULL);
    }

    if(".episode" %in% colnames(presc.data)){
      {
        if( !suppress.warnings ) warning("The column name \'.episode\' is used internally, please use another column name.");
        return (NULL);
      }
    }

    # convert dates
    disp.data[,(disp.date.colname) := as.Date(get(disp.date.colname), format = date.format)];
    presc.data[,(presc.date.colname) := as.Date(get(presc.date.colname), format = date.format)];
    if(!is.null(hosp.data))
    {
      hosp.data[,`:=` (DATE.IN = as.Date(DATE.IN, format = date.format),
                       DATE.OUT = as.Date(DATE.OUT, format = date.format))];
    }

    # force medication class to character
    for(class.colname in medication.class.colnames)
    {
      if(inherits(disp.data[[class.colname]], "factor"))
      {
        disp.data[,(class.colname) := as.character(get(class.colname))];
      }

      if(inherits(presc.data[[class.colname]], "factor"))
      {
        presc.data[,(class.colname) := as.character(get(class.colname))];
      }
    }
  }

  # function to process each patient
  process_patient <- function(pat)
  {
    if(exists("debug.mode") && debug.mode==TRUE) print(paste("Patient:",pat));

    # function to process each medication
    process_medication <- function(med)
    {
      if(exists("debug.mode") && debug.mode==TRUE) print(paste("Medication:", med));

      process_dispensing_events <- function(event)
      {
        if(exists("debug.mode") && debug.mode==TRUE) print(paste("Event:", event));

        ## !Important: We assume that the prescribed dose can be accomodated with the dispensed medication

        #subset data to event
        curr_disp <- med_disp[event];
        orig.disp.date <- curr_disp[[disp.date.colname]];

        # if current dispensing event is before first prescription date, don't calculate a duration
        if(orig.disp.date < first_presc[[presc.date.colname]])
        {
          med_event <- cbind(curr_disp[,c(ID.colname, medication.class.colnames, total.dose.colname, disp.date.colname), with = FALSE],
                             DISP.START = orig.disp.date,
                             DURATION = 0,
                             DAILY.DOSE = NA,
                             START.PRESC = as.Date(NA, format = date.format),
                             END.PRESC = as.Date(NA, format = date.format),
                             HOSP.DURATION = NA);
          # if current dispensing event is after end of last prescription date, don't calculate a duration (only when last prescription indicates termination)
        } else
        {
          #select prescription episodes ending after the original dispensing date and add the one immediately before
          episodes <- med_presc[orig.disp.date < END.PRESC | is.na(END.PRESC), which = TRUE];

          # if the current dispensing event is after the last prescription episode, don't calculate a duration
          if(length(episodes) == 0)
          {
            med_event <- cbind(curr_disp[,c(ID.colname, medication.class.colnames, total.dose.colname, disp.date.colname), with = FALSE],
                               DISP.START = orig.disp.date,
                               DURATION = 0,
                               DAILY.DOSE = NA,
                               START.PRESC = as.Date(NA, format = date.format),
                               END.PRESC = as.Date(NA, format = date.format),
                               HOSP.DURATION = NA);
          } else
          {
            ## for each prescription episode, calculate the duration with the current dose
            total.dose.i <- curr_disp[[total.dose.colname]]; #dispensed dose
            presc.dose.i <- 0; # initialize prescibed dose as 0
            disp.start.date.i <- orig.disp.date; #start date of dispensing event

            rm.trt.episode <- FALSE; # will be set to TRUE in case of calculations during treatment interruptions

            stop <- 0;

            med_event <- NULL;
            for(episode in episodes)
            {
              presc.dose.i <- med_presc[[episode,presc.daily.dose.colname]]; # prescribed daily dose

              if(presc.dose.i == 0) # if event happens during treatment interruption (prescribed dose = 0), check what to do
              {
                if(trt.interruption == "continue") # if trt.interruption is set to continue, continue with last prescribed dose
                {
                  presc.dose.i <- med_presc[[episode-1,presc.daily.dose.colname]];

                  rm.trt.episode <- TRUE; # make sure that prescription start- and end date are removed at the end
                } else if(trt.interruption == "discard") # if trt.interruption is set to discard, don't calculate anything
                {
                  if(is.null(med_event))
                  {
                    med_event <- cbind(curr_disp[,c(ID.colname, medication.class.colnames, total.dose.colname, disp.date.colname), with = FALSE],
                                       DISP.START = disp.start.date.i,
                                       DURATION = 0,
                                       DAILY.DOSE = NA,
                                       START.PRESC = as.Date(NA, format = date.format),
                                       END.PRESC = as.Date(NA, format = date.format),
                                       HOSP.DURATION = NA);
                  }

                  break
                } else
                {
                  episode <- episode + 1; # else skip to next episode
                  next;
                }
              }

              start.episode <- med_presc[episode,START.PRESC];
              end.episode <- med_presc[episode,END.PRESC];

              # if it is not the first episode, adjust supply start date to prescription start date
              if(episode != episodes[1]) disp.start.date.i <- start.episode;

              duration.i <- total.dose.i/presc.dose.i; # calculate duration

              disp.end.date.i <- disp.start.date.i + duration.i; # calculate end date of supply

              # add hospitalizations during the supply period
              hosp.duration.i <- 0;
              if(nrow(hosp_events) != 0 & !is.na(duration.i))
              {
                # check for hospitalizations within the episode
                hosp_events_i <- hosp_events[(DATE.IN <= end.episode|is.na(end.episode)) & DATE.OUT > start.episode];

                # check end of supply date is after start of hospitalization and end of hospitalization is after start of dispsense
                curr_hosp_event <- hosp_events_i[DATE.IN <= disp.end.date.i & DATE.OUT > disp.start.date.i];

                if(nrow(curr_hosp_event) > 0)
                {
                  if(trt.interruption == "continue") # if trt.interruption is set to continue, calculate duration of hospitalization and continue
                  {
                    hosp.duration.i <- sum(curr_hosp_event$HOSP.DURATION);
                  } else if(trt.interruption == "discard") # if trt.interruption is set to discard, calculate until start of hospitalization and discard the rest
                  {
                    duration.i <- curr_hosp_event[[1,"DATE.IN"]] - disp.start.date.i; # calculate duration until start of hospitalization
                    if(duration.i < 0) duration.i <- 0;

                    stop <- 1;
                  } else #for carryover, construct medication events taking into account all hospitalizations within an episode
                  {
                    #hosp.durations <- as.numeric(curr_hosp_event$HOSP.DURATION) #get hospitalization periods
                    hosp_dates <- sort(c(curr_hosp_event$DATE.IN, curr_hosp_event$DATE.OUT)); # sort hostpitalization dates

                    # if the dispensing event starts before the hospitalization, add the dispensing date
                    if(disp.start.date.i <= hosp_dates[1])
                    {
                      episode.dates <- c(disp.start.date.i, hosp_dates);
                    } else #else remove the first hospitalization date and adjust hosp.durations
                    {
                      #hosp.durations[1] <- as.numeric(hosp_dates[2]- disp.start.date.i)
                      episode.dates <- hosp_dates[-1];
                    }

                    # if end of episode is before the last discharge, remove last discharge date and adjust hosp.durations
                    if(!is.na(end.episode) && end.episode < tail(hosp_dates,1))
                    {
                      episode.dates <- head(episode.dates,-1);
                      if(length(episode.dates) == 0) next;
                      #hosp.durations[length(hosp.durations)] <- as.numeric(end.episode-tail(episode.dates,1))
                    } else # add end of episode
                    {
                      episode.dates <- c(episode.dates,end.episode);
                      #hosp.durations <- c(hosp.durations, 0)
                    }

                    # create data table with all events in an episode
                    disp.start.dates <- episode.dates[seq(1, by = 2, length.out = length(episode.dates)/2)];
                    disp.end.dates <- episode.dates[seq(2, by = 2, length.out = length(episode.dates)/2)];
                    durations <- as.numeric(disp.end.dates - disp.start.dates);

                    all.events <- data.table(DISP.START = disp.start.dates,
                                             DURATION = durations,
                                             DAILY.DOSE = as.numeric(presc.dose.i),
                                             START.PRESC = start.episode,
                                             END.PRESC = end.episode,
                                             HOSP.DURATION = rep(0,length(durations)));

                    # calculate sum of all durations
                    sum.duration <- sum(all.events$DURATION,na.rm= TRUE);

                    # if episode is not terminated (last duration is NA)
                    if(is.na(tail(all.events$DURATION,1)))
                    {
                      all.events[nrow(all.events), DURATION := duration.i-sum.duration];

                      med_event <-  rbind(med_event,
                                          cbind(curr_disp[,c(ID.colname, medication.class.colnames, total.dose.colname, disp.date.colname), with = FALSE],
                                                all.events[,1:6]));
                      break;
                    } else if(duration.i <= sum.duration) # if the supply duration is shorter than the sum of the duration
                    {
                      # calculate cumulative sum of durations
                      all.events[,cum.duration := cumsum(DURATION)];
                      # subset to all rows until supply is exhaused and add 1
                      .rows <- all.events[cum.duration <= duration.i,which=TRUE];
                      if( length(.rows) == 0 ) .rows <- 0;
                      all.events <- all.events[c(.rows, tail(.rows,1)+1)];

                      # calculate remaining duration for last row
                      sum.duration <- sum(head(all.events,-1)$DURATION);
                      all.events[nrow(all.events), DURATION := duration.i-sum.duration];

                      med_event <-  rbind(med_event,
                                          cbind(curr_disp[,c(ID.colname, medication.class.colnames, total.dose.colname, disp.date.colname), with = FALSE],
                                                all.events[,1:6]));
                      break;
                    } else # if supply duration is longer than the sum of the durations
                    {
                      # calculate the carryover dose
                      oversupply <- duration.i-sum.duration; # calculate remaining days of oversupply
                      total.dose.i <- presc.dose.i*oversupply; # calculate remaining total dose

                      med_event <-  cbind(curr_disp[,c(ID.colname, medication.class.colnames, total.dose.colname, disp.date.colname), with = FALSE],
                                          all.events[,1:6]);
                      next;
                    }
                  }
                }
              }

              # check various parameters to decide wheter to stop or continue

              # check if end of supply is before end of episode OR last row of prescription episodes is reached
              if( disp.end.date.i < end.episode | episode == tail(episodes,1) )
              {
                stop <- 1;
              } else {
                episode <- episode + 1; # get next prescription episode
                next.presc.dose <- med_presc[[episode,presc.daily.dose.colname]]; # get next episode's dosage

                # if there is a treatment interruption and trt.interruption is set to continue, stop
                if( next.presc.dose == 0 & trt.interruption == "continue" ) stop <- 1;

                # if there is no treatment interruption, but a dosage change and split.on.dosage.change is set FALSE, stop
                if( next.presc.dose != 0 & next.presc.dose != presc.dose.i & split.on.dosage.change == FALSE ) stop <- 1;
              }

              if( stop == 1 )
              {
                if( rm.trt.episode == TRUE )
                {
                  start.episode <- as.Date(NA, format = date.format);
                  end.episode <- as.Date(NA, format = date.format);
                }

                med_event <- rbind(med_event,
                                   cbind(curr_disp[,c(ID.colname, medication.class.colnames, total.dose.colname, disp.date.colname), with = FALSE],
                                         data.table(DISP.START = disp.start.date.i,
                                                    DURATION = as.numeric(duration.i),
                                                    DAILY.DOSE = as.numeric(presc.dose.i),
                                                    START.PRESC = start.episode,
                                                    END.PRESC = end.episode,
                                                    HOSP.DURATION = as.numeric(hosp.duration.i))));
                break;
              } else
              {
                duration.i <- end.episode - disp.start.date.i; # calculate duration until end of episode
                oversupply <- disp.end.date.i - end.episode; # calculate remaining days of oversupply
                total.dose.i <- presc.dose.i*oversupply; # calculate remaining total dose

                if( rm.trt.episode == TRUE )
                {
                  start.episode <- as.Date(NA, format = date.format);
                  end.episode <- as.Date(NA, format = date.format);
                }

                #create medication event
                med_event <- rbind(med_event,
                                   cbind(curr_disp[,c(ID.colname, medication.class.colnames, total.dose.colname, disp.date.colname), with = FALSE],
                                         data.table(DISP.START = disp.start.date.i,
                                                    DURATION = as.numeric(duration.i),
                                                    DAILY.DOSE = as.numeric(presc.dose.i),
                                                    START.PRESC = start.episode,
                                                    END.PRESC = end.episode,
                                                    HOSP.DURATION = as.numeric(hosp.duration.i))));
              }
            }

            med_event;
          }
        }
      }

      ## subset data to medication

      setkeyv(pat_disp, medication.class.colnames);

      med_disp <- pat_disp[list(disp_presc[med, medication.class.colnames, with = FALSE])];

      med_presc <- pat_presc[list(disp_presc[med, medication.class.colnames, with = FALSE])];

      setkeyv(med_disp, cols = disp.date.colname);
      setkeyv(med_presc, cols = presc.date.colname);

      # determine date of initial prescription
      first_presc <- med_presc[1];

      # determine date of initial dispense
      first_disp <- med_disp[[disp.date.colname]][1];

      #if force.presc.renew and split.on.dosage.change are not set globally, set for medication based on first dispensing event
      if( !is.logical(force.presc.renew) )
      {
        force.presc.renew <- as.logical(first_disp[[force.presc.renew]]);
      }
      if( !is.logical(split.on.dosage.change) )
      {
        split.on.dosage.change <- as.logical(first_disp[[split.on.dosage.change]]);
      }

      ## calculate treatment interruptions and end of prescription date

      ## determine end of prescription and prescription interruptions if prescription reneval is enforced for each subsequent prescription event (requires the "visit" column)
      presc_interruptions <- data.table(NULL);
      if( force.presc.renew == TRUE )
      {
        presc_visit <- presc_events[[visit.colname]] %in% unique(med_presc[[visit.colname]]); # determine for each visit if medication was prescribed

        first_presc_event <- head(which(presc_visit),1); # extract first prescription event
        last_presc_event <- tail(which(presc_visit),1); # extract last prescription event

        presc_omit <- which(!presc_visit)[which(!presc_visit) > first_presc_event & which(!presc_visit) < last_presc_event]; # identify visits between first and last prescription with missing prescriptions

        interruption_dates <- presc_events[[presc.date.colname]][presc_omit]; # determine dates of treatment interruptions

        presc_interruptions <- med_presc[rep(1, length(presc_omit))]; # create table with one row for each interruption

        presc_interruptions[, c(visit.colname, presc.date.colname, presc.daily.dose.colname, presc.duration.colname) :=
                              list(presc_events[[visit.colname]][presc_omit], interruption_dates, 0, NA)]; # adjust variables

        med_presc <- rbind(med_presc, presc_interruptions); # bind to existing prescriptions
        setkeyv(med_presc, cols = presc.date.colname); # order by date

        med_presc[,.episode := rleidv(med_presc, cols = presc.daily.dose.colname)]; # add counter for treatment episodes
      }

      setorder(med_presc);

      ## construct treatment episodes
      # create new .episode counter
      med_presc[,.episode := rleidv(med_presc, cols = c(presc.daily.dose.colname, presc.duration.colname))];

      # if consecutive episodes with set end date, increase .episode counter
      if( nrow(med_presc) > 2 )
      {
        for( n in 2:(nrow(med_presc)-1) )
        {
          if( !is.na(med_presc[n,presc.duration.colname, with = FALSE]) & !is.na(med_presc[n-1,presc.duration.colname, with = FALSE]) )
          {
            med_presc[n:nrow(med_presc), .episode := as.integer(.episode + 1)];
          }
        }
      } else if( nrow(med_presc) == 2 )
      {
        med_presc[!is.na(shift(get(presc.duration.colname), type = "lag")) & !is.na(get(presc.duration.colname)), .episode := as.integer(.episode + 1)];
      }

      # add episodes with same dose but set end date to last episode
      .row <- med_presc[is.na(shift(get(presc.duration.colname), type = "lag")) & shift(get(presc.daily.dose.colname), type = "lag") == get(presc.daily.dose.colname) & !is.na(get(presc.duration.colname)), which = TRUE];
      if( length(.row)>0 )
      {
        med_presc[.row:nrow(med_presc),.episode := as.integer(.episode-1)];
      }

      ## set start and end of prescription dates per group
      med_presc[, `:=` (START.PRESC = get(presc.date.colname), # set prescription date as start date
                        END.PRESC = get(presc.date.colname))]; # set end date to prescription date ...

      # connect episodes with limited durations with following episodes if they have the same dosage and matching end- and start dates.
      med_presc[,END.PRESC := shift(END.PRESC, type = "lead")]; # ... and shift end dates up by one

      # adjust end date if prescription duration is provided and change start date of following prescriptions
      med_presc[!is.na(get(presc.duration.colname)), END.PRESC := DATE.PRESC + get(presc.duration.colname)];
      end.limited.presc <- head(med_presc,-1)[!is.na(get(presc.duration.colname))]$END.PRESC; #don't include last prescription episode
      med_presc[shift(!is.na(get(presc.duration.colname)), type = "lag"), START.PRESC := end.limited.presc];
      med_presc[DATE.PRESC>START.PRESC & get(presc.daily.dose.colname) != 0,START.PRESC:=DATE.PRESC];

      med_presc[shift(get(presc.daily.dose.colname),type="lag")==get(presc.daily.dose.colname) & !is.na(shift(get(presc.duration.colname),type="lag")) & shift(END.PRESC, type = "lag") == START.PRESC, .episode := as.integer(.episode-1)];

      # fill in start and end dates by group
      med_presc[,START.PRESC := head(START.PRESC,1), by = .episode]; # first start date per episode
      med_presc[,END.PRESC:= tail(END.PRESC,1), by = .episode]; # last end date per episode

      # collapse episodes
      med_presc <- unique(med_presc, by = ".episode", fromLast = TRUE);
      med_presc[,.episode := rleidv(med_presc, cols = c("START.PRESC", "END.PRESC"))];

      # remove episodes where end date is before start date
      rm.episode <- med_presc[END.PRESC <= START.PRESC, which = TRUE];
      if( length(rm.episode) > 0 )
      {
        med_presc <- med_presc[-rm.episode];
      }
      med_presc[,.episode := rleidv(med_presc)];

      # collapse consecutive episodes where end date of the former is before start date of the latter
      med_presc[shift(END.PRESC,type = "lag") > START.PRESC & shift(get(presc.daily.dose.colname),type = "lag") == get(presc.daily.dose.colname),
                .episode := as.integer(.episode-1)];
      med_presc[,START.PRESC := head(START.PRESC,1), by = .episode]; # first start date per episode
      med_presc[,END.PRESC:= tail(END.PRESC,1), by = .episode]; # last end date per episode
      med_presc <- unique(med_presc, by = ".episode");
      med_presc[,.episode := rleidv(med_presc, cols = c("START.PRESC", "END.PRESC"))];

      # add treatment interruptions
      med_presc <- rbind(med_presc,med_presc[shift(START.PRESC,type = "lead")!=END.PRESC][,c(presc.daily.dose.colname, "START.PRESC", ".episode") := list(0, END.PRESC, 0)]);
      setorder(med_presc, START.PRESC, END.PRESC);
      end.trt.interruptions <- med_presc[shift(END.PRESC,type = "lag")!=START.PRESC]$START.PRESC;
      med_presc[.episode == 0, END.PRESC := end.trt.interruptions];

      if( force.init.presc == TRUE )
      {
        # if initial dispense is before initial prescription, adjust date of initial prescription to match initial dispense
        # but only if first prescription is unlimited
        if( first_disp < head(med_presc[[presc.date.colname]],1) & is.na(head(med_presc[[presc.duration.colname]],1)) )
        {
          # adjust first prescription date
          first_presc[1, (presc.date.colname) := first_disp];
          med_presc[1, START.PRESC := first_disp];
        }
      }

      ## calculate medication events for "simple" events not extending over multiple episodes or affected by hospitalizations
      # add prescription events to dispensing events
      for( i in 1:nrow(med_presc) )
      {
        med_disp[get(disp.date.colname) >= med_presc[i,START.PRESC] & (get(disp.date.colname) < med_presc[i,END.PRESC] | is.na(med_presc[i,END.PRESC])),
                 c("START.PRESC", "END.PRESC", presc.daily.dose.colname) := list(med_presc[i,START.PRESC], med_presc[i,END.PRESC],med_presc[i,get(presc.daily.dose.colname)])];
      }
      med_disp[,DURATION := (TOTAL.DOSE)/(DAILY.DOSE)];
      med_disp[,`:=` (DISP.START = get(disp.date.colname),
                      DISP.END = get(disp.date.colname)+DURATION)];

      med_disp[DISP.END > END.PRESC, .out := 1];

      # add hospitalization events to dispensing events
      med_disp[,.hosp := as.numeric(NA)];
      if( nrow(hosp_events) != 0 )
      {
        for( i in 1:nrow(hosp_events) )
        {
          med_disp[(DISP.END >= hosp_events[i,DATE.IN] & DISP.START < hosp_events[i,DATE.OUT])|(DISP.START >= hosp_events[i,DATE.IN] & DISP.START < hosp_events[i,DATE.OUT]),
                   .hosp := 1];
        }
      }

      medication_events <- med_disp[DURATION != Inf & is.na(.out) & is.na(.hosp),
                                    c(ID.colname,
                                      medication.class.colnames,
                                      total.dose.colname,
                                      disp.date.colname,
                                      "DISP.START",
                                      "DURATION",
                                      presc.daily.dose.colname,
                                      "START.PRESC",
                                      "END.PRESC"), with = FALSE];
      medication_events[,HOSP.DURATION := 0];

      med_disp <- med_disp[DURATION == Inf | .out == 1 | .hosp == 1];

      ## apply process_dispensing_events to each dispensing event
      medication_events_rest <- NULL;
      if( nrow(med_disp) > 0 )
      {
        medication_events_rest <- do.call(rbindlist,
                                          list(l = lapply(1:nrow(med_disp), FUN = function(i) process_dispensing_events(event = i)),
                                               fill = TRUE));
      }

      medication_events <- rbind(medication_events, medication_events_rest, fill = TRUE);

      setorderv(medication_events,cols=c(disp.date.colname, "DISP.START"));

      if( force.presc.renew == TRUE )
      {
        tot.presc.interruptions <- nrow(med_presc[get(presc.daily.dose.colname)==0]);

        medication_events[,tot.presc.interruptions := tot.presc.interruptions];
      }

      if( split.on.dosage.change == TRUE )
      {
        tot.dosage.changes <- (nrow(med_presc) - 1 - 2*nrow(med_presc[get(presc.daily.dose.colname)==0]));

        medication_events[,tot.dosage.changes := tot.dosage.changes];
      }

      #med_presc <- med_presc[,`:=` (.episode=NULL,VISIT=NULL)]
      setnames(med_presc,
               old = c(presc.daily.dose.colname, presc.duration.colname),
               new = c("DAILY.DOSE", "PRESC.DURATION"));

      presc_episode_no_dispense <- med_presc[!medication_events[,c("DAILY.DOSE","START.PRESC","END.PRESC")],
                                             on = c("DAILY.DOSE","START.PRESC", "END.PRESC")];

      presc_episode_no_dispense[,c(".episode","VISIT", "PRESC.DURATION", presc.date.colname) := NULL];

      medication_events <- rbind(medication_events, presc_episode_no_dispense, fill = TRUE);

      medication_events;
    }

    # subset data to patient
    pat_disp <- disp.data[get(ID.colname) == pat, c(ID.colname,
                                                    disp.date.colname,
                                                    medication.class.colnames,
                                                    total.dose.colname), with = FALSE];

    pat_presc <- presc.data[get(ID.colname) == pat, c(ID.colname,
                                                      presc.date.colname,
                                                      visit.colname,
                                                      medication.class.colnames,
                                                      presc.daily.dose.colname,
                                                      presc.duration.colname), with = FALSE];

    # sort by DCI
    setkeyv(pat_disp, cols = medication.class.colnames);
    setkeyv(pat_presc, cols = medication.class.colnames);

    # extract unique dispensed/prescribed DCIs
    disp_unique <- unique(pat_disp[,c(medication.class.colnames), with = FALSE]);
    presc_unique <- unique(pat_presc[,c(medication.class.colnames), with = FALSE]);

    # extract medications present in both dispensing and prescription database (by DCI, Unit, and Form)
    disp_presc <- merge(disp_unique, presc_unique, by = c(medication.class.colnames), all=FALSE);

    # extract unique dispensed/prescribed DCIs not present in both databases
    disp_no_presc <- disp_unique[!presc_unique];
    presc_no_disp <- presc_unique[!disp_unique];

    #create visits if not supplied
    if( !visit.colname %in% colnames(presc.data) )
    {
      presc_events <- unique(pat_presc[,c(presc.date.colname), with = FALSE]);
      presc_events[,(visit.colname) := 0:(nrow(presc_events)-1)];
      pat_presc <- merge(pat_presc, presc_events, by = presc.date.colname);
      setorderv(pat_presc, medication.class.colnames);
    } else
    {
      presc_events <- unique(pat_presc[,c(presc.date.colname, visit.colname), with = FALSE]); # extract prescription instances
    }

    # if duplicate visit numbers for different dates or vice versa, throw an error
    if( length(unique(presc_events$DATE.PRESC)) != nrow(presc_events) )
    {
      {
        if( !suppress.warnings ) warning("Prescription dates and visit number don't match for patient Nr.", pat);
        return (NULL);
      }
    }

    # extract hospitalizations
    if( !is.null(hosp.data) )
    {
      hosp_events <- hosp.data[get(ID.colname) == pat];
    } else
    {
      hosp_events <- data.table(NULL);
    }

    setkeyv(presc_events, cols = presc.date.colname);

    # apply process_medication() function to each medication present in both databses
    patient_events <- NULL;
    if( nrow(disp_presc) != 0 )
    {
      patient_events <- do.call(rbindlist, list(l = lapply(1:nrow(disp_presc), FUN = function(i) process_medication(med = i)),
                                                fill = TRUE));
    }

    patient_events <- rbind(patient_events,
                            pat_disp[list(disp_no_presc[,medication.class.colnames, with = FALSE]), c(ID.colname, disp.date.colname, medication.class.colnames, total.dose.colname), with = FALSE],
                            pat_presc[list(presc_no_disp[,medication.class.colnames, with = FALSE]), c(ID.colname, medication.class.colnames, presc.daily.dose.colname), with = FALSE],
                            fill = TRUE);

    # update progress bar
    setTxtProgressBar(pb, pat);

    patient_events;
  }

  # extract IDs of all patients present in dispensing and prescription database
  disp_presc_IDs <- intersect(disp.data[[ID.colname]], presc.data[[ID.colname]]);

  # add duration of hospitalization
  if( !is.null(hosp.data) )
  {
    hosp.data[,HOSP.DURATION := as.numeric(DATE.OUT-DATE.IN)];
  }

  # progress bar
  pb <- txtProgressBar(min = 0, max = length(disp_presc_IDs), style = 3);

  # apply process_patient function
  setkeyv(disp.data, cols = ID.colname);
  setkeyv(presc.data, cols = ID.colname);

  treatment_episodes <- do.call(rbindlist, list(l = lapply(disp_presc_IDs, FUN = function(i) process_patient(pat = i)),
                                                fill = TRUE));

  # key by ID, medication class, and dispensing date
  setkeyv(treatment_episodes, cols = c(ID.colname, medication.class.colnames, disp.date.colname));

  if( !return.data.table )
  {
    return (as.data.frame(treatment_episodes));
  } else
  {
    return(treatment_episodes);
  }
}

############ function to compute time to initiation

#' Computation of initiation times.
#'
#' Computes the time between the first prescription event and the first dispensing
#' event for each medication class.
#'
#' The period between the first prescription event and the first dose administration
#' may impact health outcomes differently than omitting doses once on treatment or
#' interrupting medication for longer periods of time. Primary non-adherence (not
#' acquiring the first prescription) or delayed initiation may have a negative
#' impact on health outcomes. The function \code{time_to_initiation} calculates
#' the time between the first prescription and the first dispensing event, taking
#' into account multiple variables to differentiate between treatments.
#'
#' @param presc.data A \emph{\code{data.frame}} or \emph{\code{data.table}} containing
#' the prescription events. Must contain, at a minimum, the patient unique ID,
#' one medication identifier, and the start date of the prescription, and might
#' also contain additional columns to identify and group medications (the actual
#' column names are defined in the \emph{\code{medication.class.colnames}} parameter).
#' @param disp.data A \emph{\code{data.frame}} or \emph{\code{data.table}} containing
#' the dispensing events. Must contain, at a minimum, the patient unique ID, one
#' medication identifier, the dispensing date, and might also contain additional
#' columns to identify and group medications (the actual column names are defined
#' in the \emph{\code{medication.class.colnames}} parameter).
#' @param ID.colname A \emph{string}, the name of the column in \code{presc.data}
#' and \code{disp.data} containing the unique patient ID, or \code{NA} if not defined.
#' @param presc.start.colname A \emph{string}, the name of the column in
#' \code{presc.data} containing the prescription date (in the format given in
#' the \code{date.format} parameter), or \code{NA} if not defined.
#' @param disp.date.colname A \emph{string}, the name of the column in
#' \code{disp.data} containing the dispensing date (in the format given in
#' the \code{date.format} parameter), or \code{NA} if not defined.
#' @param date.format A \emph{string} giving the format of the dates used in
#' the \code{data} and the other parameters; see the \code{format} parameters
#' of the \code{\link[base]{as.Date}} function for details (NB, this concerns
#' only the dates given as strings and not as \code{Date} objects).
#' @param medication.class.colnames A \emph{\code{Vector}} of \emph{strings}, the
#' name(s) of the column(s) in \code{data} containing the classes/types/groups of
#' medication, or \code{NA} if not defined.
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any
#' warnings.
#' @param return.data.table \emph{Logical}, if \code{TRUE} return a
#' \code{data.table} object, otherwise a \code{data.frame}.
#' @param ... other possible parameters
#' @return A \code{data.frame} or \code{data.table} with the following columns:
#' \itemize{
#'  \item \code{ID.colname} the unique patient ID, as given by the \code{ID.colname}
#'  parameter.
#'  \item \code{medication.class.colnames} the column(s) with classes/types/groups
#'  of medication, as given by the  \code{medication.class.colnames} parameter.
#'  \item \code{first.presc} the date of the first prescription event.
#'  \item \code{first.disp} the date of the first dispensing event.
#'  \item \code{time.to.initialization} the difference in days between the first
#'  dispensing date and the  first prescription date.
#' }
#' @examples
#' time_init <- time_to_initiation(presc.data = durcomp.prescribing,
#'                                 disp.data = durcomp.dispensing,
#'                                 ID.colname = "ID",
#'                                 presc.start.colname = "DATE.PRESC",
#'                                 disp.date.colname = "DATE.DISP",
#'                                 medication.class.colnames = c("ATC.CODE", "FORM", "UNIT"),
#'                                 date.format = "%Y-%m-%d",
#'                                 suppress.warnings = FALSE,
#'                                 return.data.table = TRUE);
#' @export
time_to_initiation <- function(presc.data = NULL,
                               disp.data = NULL,
                               ID.colname = NA,
                               presc.start.colname = NA,
                               disp.date.colname = NA,
                               medication.class.colnames = NA,
                               date.format = "%d.%m.%Y",
                               suppress.warnings = FALSE,
                               return.data.table = FALSE,
                               ...)
{
  # Preconditions
  {
    # data class and dimensions:
    if( inherits(presc.data, "matrix") ) presc.data <- as.data.table(presc.data); # convert matrix to data.table
    if( !inherits(presc.data, "data.frame") )
    {
      if( !suppress.warnings ) warning("The presc.data must be of type 'data.frame'!\n");
      return (NULL);
    }

    if( inherits(disp.data, "matrix") ) disp.data <- as.data.table(disp.data); # convert matrix to data.table
    if( !inherits(disp.data, "data.frame") )
    {
      if( !suppress.warnings ) warning("The presc.data must be of type 'data.frame'!\n");
      return (NULL);
    }

    if( !is.na(ID.colname) && !(ID.colname %in% names(disp.data))  && !(ID.colname %in% names(presc.data)))
    {
      if( !suppress.warnings ) warning(paste0("Column ID.colname='",ID.colname,"' must appear in the data!\n"));
      return (NULL);
    }

    if( !is.na(medication.class.colnames) && !(medication.class.colnames %in% names(disp.data)) && !(medication.class.colnames %in% names(presc.data)))
    {
      if( !suppress.warnings ) warning(paste0("Column(s) medication.class.colnames='",medication.class.colnames,"' must appear in the data!\n"));
      return (NULL);
    }

    if( !is.na(disp.date.colname) && !(disp.date.colname %in% names(disp.data)) )
    {
      if( !suppress.warnings ) warning(paste0("Column disp.date.colname='",disp.date.colname,"' must appear in the data!\n"));
      return (NULL);
    }

    if( !is.na(presc.start.colname) && !(presc.start.colname %in% names(presc.data)) )
    {
      if( !suppress.warnings ) warning(paste0("Column presc.start.colname='",presc.start.colname,"' must appear in the data!\n"));
      return (NULL);
    }

    if( sum(is.na(disp.data[[disp.date.colname]])) > 0 )
    {
      if( !suppress.warnings ) warning(paste0("Dispensing dates in disp.date.colname='",disp.date.colname,"' cannot contain NAs!\n"));
      return (NULL);
    }
  }

  # convert dates
  presc.data[,(presc.start.colname) := as.Date(get(presc.start.colname), format = date.format)];
  disp.data[,(disp.date.colname) := as.Date(get(disp.date.colname), format = date.format)];

  first_presc <- presc.data[,list(first.presc = min(get(presc.start.colname),na.rm=TRUE)),
                            by = c(ID.colname, medication.class.colnames)];
  first_disp <- disp.data[,list(first.disp = min(get(disp.date.colname),na.rm=TRUE)),
                           by = c(ID.colname, medication.class.colnames)];

  dt_t2i <- merge(first_presc, first_disp, by = c(ID.colname, medication.class.colnames), all = TRUE);

  dt_t2i[,time.to.initialization := as.numeric(first.disp-first.presc)];

  # key by ID, medication class, and dispensing daste
  setkeyv(dt_t2i, cols = c(ID.colname, medication.class.colnames, "first.disp"));

  if( !return.data.table )
  {
    return (as.data.frame(dt_t2i));
  } else
  {
    return(dt_t2i);
  }
}


