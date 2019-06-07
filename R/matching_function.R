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
                  "DATE.PRESC", "episode.start", "episode.end",
                  "DURATION", "SPECIAL.DURATION",
                  "DAILY.DOSE", "TOTAL.DOSE",
                  "DISP.START", "DISP.END",
                  "cum.duration", ".episode", ".out", ".special.periods", "time.to.initiation", "first.disp", "episode.start",
                  "debug.mode"));


#' Example prescription events for 16 patients.
#'
#' A sample dataset containing prescription events (one per row) for 16 patients
#' over a period of roughly 15 months (1502 events in total).
#' This is the appropriate format to compute event durations with the
#' \code{compute_event_durations} function. Each row represents an individual prescription
#' record for a specific dose of a specific medication for a patient at a given date.
#' Visit number and Duration are optional, and more than one column to group medications
#' can be supplied (such as ATC Code, Form or Unit).
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
#'   \item{episode.duration}{\emph{numeric}; the duration (in days) for which the prescription
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
#' \code{compute_event_durations} function. Each row represents an individual dispensing
#' record for a specific dose of a specific medication for a patient at a given date.
#' More than one column to group medications can be supplied (such as ATC code, Form and Unit).
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

#' Example special periods for 10 patients.
#'
#' A sample dataset containing special periods (one per row) for 10 patients
#' over a period of roughly 18 months (28 events in total).
#' This is the appropriate format to compute event durations with the
#' \code{compute_event_durations} function. Each row represents an individual special period of type
#' "hospitalization" of a patient for whom event durations should be calculated.
#' Besides hospitalizations, this could cover other situations
#' where medication use may differ, e.g. during incarcerations or holidays.
#' All column names must match the format provided in this example.
#'
#' @format A data frame with 28 rows and 3 variables:
#' \describe{
#'   \item{ID}{\emph{Integer} here; patient unique identifier. Can also
#'   be \emph{string}.}
#'   \item{DATE.IN}{\emph{Date} here;the start of the hospitalization period, by default in the
#'   yyyy-mm-dd format.Can also be \emph{string}.}
#'   \item{DATE.OUT}{\emph{Date};the end of the hospitalization period, by default in the
#'   yyyy-mm-dd format. Can also be \emph{string}.}
#'   \item{TYPE}{\emph{Character}; describes the type of situation, e.g. "hospitalization"
#'   or "holiday". Used to categorize different types of situations where use might differ
#'   from "normal" periods.}
#'   \item{CUSTOM}{\emph{Character}; can be either \emph{continue}, \emph{discard}, or
#'   \emph{carryover} (see function arguments in \code{compute_event_duration}).}
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
#' @param special.periods.data Optional, \emph{\code{NULL}} or a \emph{\code{data.frame}}
#' containing the information about special periods (e.g., hospitalizations or other situations
#' where medication use may differ, e.g. during incarcerations or holidays). Must contain the same unique
#' patient ID as dispensing and prescription data, the start and end dates of the special
#' periods with the exact column names \emph{\code{DATE.IN}} and \emph{\code{DATE.OUT}}.
#' Optional columns are \emph{\code{TYPE}} (indicating the type of special situation),
#' customized instructions how to handle a specific period (see
#' \code{special.periods.method}), and any of those specified in \code{medication.class.colnames}.
#' @param ID.colname A \emph{string}, the name of the column in \code{disp.data},
#' \code{presc.data}, and \code{special.periods.data} containing the unique patient ID.
#' @param presc.date.colname A \emph{string}, the name of the column in
#' \code{presc.data} containing the prescription date (in the format given in
#' the \code{date.format} parameter).
#' @param disp.date.colname A \emph{string}, the name of the column in
#' \code{disp.data} containing the dispensing date (in the format given in
#' the \code{date.format} parameter).
#' @param medication.class.colnames A \emph{\code{Vector}} of \emph{strings}, the
#' name(s) of the column(s) in \code{disp.data} and \code{presc.data} containing
#' the classes/types/groups of medication.
#' @param total.dose.colname A \emph{string}, the name of the column in
#' \code{disp.data} containing the total dispensed dose as \code{numeric} (e.g.
#' \code{500} for 10 tablets of 50 mg).
#' @param presc.daily.dose.colname A \emph{string}, the name of the column in
#' \code{presc.data} containing the daily prescribed dose as \code{numeric} (e.g.
#' \code{50} for 50 mg once per day, or 25 for 50 mg once ever 2 days).
#' @param presc.duration.colname A \emph{string}, the name of the column in
#' \code{presc.data} containing the duration of the prescription as \code{numeric}
#' or \code{NA} if duration is unknown.
#' @param visit.colname A \emph{string}, the name of the column in
#' \code{presc.data} containing the number of the visit or a new column name if the
#' prescribing data does not contain such a column.
#' @param split.on.dosage.change \emph{Logical} or \emph{string}. If \code{TRUE}
#' split the dispensing event on days with dosage change and create a new event with
#' the new dosage for the remaining supply. If \emph{string}, the name of the column
#' containing the \code{Logical} in \emph{disp.data} for each medication class separatly.
#' Important if carryover should be considered later on.
#' @param force.init.presc \emph{Logical}. If \code{TRUE} advance the date of the
#' first prescription event to the date of the first dispensing event, if the first
#' prescription event is after the first dispensing event for a specific medication.
#' Only if the first prescription event is not limited in duration (as indicated in
#' the \code{presc.duration.colname}).
#' @param force.presc.renew \emph{Logical} or \emph{string}. If \code{TRUE} require
#' a new prescription for all medications for every prescription event (visit),
#' otherwise prescriptions end on the first visit without renewal. If \emph{string},
#' the name of the column in \emph{disp.data} containing the \code{Logical} for each
#' medication class separatly.
#' @param trt.interruption can be either of \emph{"continue"}, \emph{"discard"},
#' \emph{"carryover"}, or a \emph{string}. It indicates how to handle durations during
#' treatment interruptions (see \code{special.periods.method}).
#' If \emph{string}, the name of the (\emph{character}) column in \emph{disp.data}
#' containing the information (\emph{"continue"}, \emph{"discard"}, or \emph{"carryover"})
#' for each medication class separatly.
#' @param special.periods.method can be either of \emph{continue}, \emph{discard},
#' \emph{carryover}, or \emph{custom}. It indicates how to handle durations during special periods.
#' With \emph{continue}, special periods have no effect on durations and event start dates.
#' With \emph{discard}, durations are truncated at the beginning of special periods and the
#' remaining quantity is discarded. With \emph{carryover}, durations are truncated
#' at the beginning of a special period and a new event with the remaining duration
#' is created after the end of the end of the special period. With \emph{custom}, the
#' mapping has to be included in \emph{\code{special.periods.data}}.
#' @param date.format A \emph{string} giving the format of the dates used in
#' the \code{data} and the other parameters; see the \code{format} parameters
#' of the \code{\link[base]{as.Date}} function for details (NB, this concerns
#' only the dates given as strings and not as \code{Date} objects).
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any warnings.
#' @param return.data.table \emph{Logical}, if \code{TRUE} return a
#' \code{data.table} object, otherwise a \code{data.frame}.
#' @param progress.bar \emph{Logical}, if \code{TRUE} show a progress bar.
#' @param ... other possible parameters.
#' @return A \code{list} with the following elements:
#' \itemize{
#'  \item \code{event_durations}: A \code{data.table} or \code{data.frame} with the following columns:
#'    \itemize{
#'      \item \code{ID.colname} the unique patient ID, as given by the \code{ID.colname}
#'      parameter.
#'      \item \code{disp.date.colname} the date of the dispensing event, as given by
#'      the \code{disp.date.colnema} parameter.
#'      \item \code{medication.class.colnames} the column(s) with classes/types/groups
#'      of medication, as given by the \code{medication.class.colnames} parameter.
#'      \item \code{total.dose.colname} the total dispensed dose, as given by the
#'      \code{total.dose.colname} parameter.
#'      \item \code{presc.daily.dose.colname} the prescribed daily dose, as given by
#'      the \code{presc.daily.dose.colname} parameter.
#'      \item \code{DISP.START} the start date of the dispensing event, either the
#'      same as in \code{disp.date.colname} or a later date in case of dosage changes
#'      or treatment interruptions/hospitalizations.
#'      \item \code{DURATION} the calculated duration of the supply, based on the total
#'      dispensed dose and the prescribed daily dose, starting from the \code{DISP.START}
#'      date.
#'      \item \code{episode.start}: the start date of the current prescription episode.
#'      \item \code{episode.end}: the end date of the current prescription episode.
#'      Can be before the start date of the dispensing event if dispensed during a treatment interruption.
#'      \item \code{SPECIAL.DURATION} the number of days \emph{during} the current duration affected
#'      by special durations or treatment interruptions of type "continue".
#'      \item \code{CARRYOVER.DURATION} the number of days \emph{after} the current duration affected
#'      by special durations or treatment interruptions of type "carryover".
#'      \item \code{tot.presc.interruptions} the total number of prescription interruptions
#'      per patient for a specific medication.
#'      \item \code{tot.dosage.changes} the total number of dosage changes per patient
#'      for a specific medication.
#'      }
#'  \item \code{prescription_episodes}: A code{data.table} or code{data.frame} with the following columns:
#'    \itemize{
#'      \item \code{ID.colname}: the unique patient ID, as given by the \code{ID.colname} parameter.
#'      \item \code{medication.class.colnames}:  the column(s) with classes/types/groups of medication,
#'       as given by the \code{medication.class.colnames} parameter.
#'      \item \code{presc.daily.dose.colname}: the prescribed daily dose, as given by the
#'      \code{presc.daily.dose.colname} parameter.
#'      \item \code{episode.start}: the start date of the prescription episode.
#'      \item \code{episode.duration}: the duration of the prescription episode in days.
#'      \item \code{episode.end}: the end date of the prescription episode.
#'      }
#'  \item \code{special_periods}: A code{data.table} or code{data.frame}, the \code{special.periods.data}
#'   with an additional column \code{SPECIAL.DURATION}: the number of days
#'   between \code{DATE.IN} and \code{DATE.OUT}
#' \item \code{ID.colname} the name of the columns containing
#'  the unique patient ID, as given by the \code{ID.colname} parameter.
#' \item \code{presc.date.colname} the name of the column in
#' \code{presc.data} containing the prescription date, as given in the \code{presc.date.colname}
#' parameter.
#' \item \code{disp.date.colname}  the name of the column in
#' \code{disp.data} containing the dispensing date, as given in the \code{disp.date.colname}
#' parameter.
#' \item \code{date.format} the format of the dates, as given by the
#'  \code{date.format} parameter.
#' \item \code{medication.class.colnames} the name(s) of the column(s) in \code{disp.data}
#'  and \code{presc.data} containing the classes/types/groups of medication, as given by the
#'  \code{medication.class.colnames} parameter.
#' \item \code{total.dose.colname} the name of the column in
#' \code{disp.data} containing the total dispensed dose, as given by the
#' \code{total.dose.colname} parameter.
#' \item \code{presc.daily.dose.colname} the name of the column in
#' \code{presc.data} containing the daily prescribed dose, as given by the
#' \code{presc.daily.dose.colname} parameter.
#' \item \code{presc.duration.colname} the name of the column in
#' \code{presc.data} containing the duration of the prescription, as given by the
#' \code{presc.duration.colname} parameter.
#' \item \code{visit.colname} the name of the column containing the number of the visit,
#'  as given by the \code{visit.colname} parameter
#' \item \code{split.on.dosage.change} whether to split the dispensing event on days with dosage changes
#'  and create a new event with the new dosage for the remaining supply, as given by the
#' \code{split.on.dosage.change} parameter.
#' \item \code{force.init.presc} whether the date of the first prescription event was set back
#' to the date of the first dispensing event, when the first prescription event was after the
#' first dispensing event for a specific medication, as given by the \code{force.init.presc} parameter.
#' \item \code{force.presc.renew} whether a new prescription was required for all medications for every
#' prescription event (visit), as given by the \code{force.presc.renew} parameter.
#' \item \code{trt.interruption} how durations during treatment interruptions were handled, as given
#' by the \code{trt.interruption} parameter.
#' \item \code{special.periods.method} as given by the \code{special.periods.method} parameter.
#' }
#' @examples
#' event_durations <- compute_event_durations(disp.data = durcomp.dispensing[1:3,],
#'                                            presc.data = durcomp.prescribing,
#'                                            special.periods.data = durcomp.hospitalisation,
#'                                            ID.colname = "ID",
#'                                            presc.date.colname = "DATE.PRESC",
#'                                            disp.date.colname = "DATE.DISP",
#'                                            medication.class.colnames = c("ATC.CODE",
#'                                            "UNIT", "FORM"),
#'                                            total.dose.colname = "TOTAL.DOSE",
#'                                            presc.daily.dose.colname = "DAILY.DOSE",
#'                                            presc.duration.colname = "episode.duration",
#'                                            visit.colname = "VISIT",
#'                                            split.on.dosage.change = TRUE,
#'                                            force.init.presc = TRUE,
#'                                            force.presc.renew = TRUE,
#'                                            trt.interruption = "continue",
#'                                            special.periods.method = "continue",
#'                                            date.format = "%Y-%m-%d",
#'                                            suppress.warnings = FALSE,
#'                                            return.data.table = TRUE);
#' @export
compute_event_durations <- function(disp.data = NULL,
                                    presc.data = NULL,
                                    special.periods.data = NULL,
                                    ID.colname,
                                    presc.date.colname,
                                    disp.date.colname,
                                    medication.class.colnames,
                                    total.dose.colname,
                                    presc.daily.dose.colname,
                                    presc.duration.colname,
                                    visit.colname,
                                    split.on.dosage.change = TRUE,
                                    force.init.presc = FALSE,
                                    force.presc.renew = FALSE,
                                    trt.interruption = c("continue", "discard", "carryover")[1],
                                    special.periods.method = trt.interruption,
                                    date.format = "%d.%m.%Y",
                                    suppress.warnings = FALSE,
                                    return.data.table = FALSE,
                                    progress.bar = TRUE,
                                    ...)
{

  presc.data <- copy(presc.data)
  disp.data <- copy(disp.data)

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
    # special period data class and dimensions:
    if(!is.null(special.periods.data))
    {
      special.periods.data <- copy(special.periods.data)

      if( inherits(special.periods.data, "matrix") ) special.periods.data <- as.data.table(special.periods.data); # convert matrix to data.table
      if( !inherits(special.periods.data, "data.frame") )
      {
        if( !suppress.warnings ) warning("The special periods data must be of type 'data.frame'!\n");
        return (NULL);
      }
      if( nrow(special.periods.data) < 1 )
      {
        if( !suppress.warnings ) warning("The special periods data must have at least one row!\n");
        return (NULL);
      }
      if(!all(c(ID.colname, "DATE.IN", "DATE.OUT") %in% colnames(special.periods.data)))
      {
        if( !suppress.warnings ) warning(paste0("The special periods data must contain at least all
                                                columns with the names '", ID.colname, "', 'DATE.IN', and 'DATE.OUT'.\n
                                                Please refer to the documentation for more information.\n"));
        return (NULL);
      }

      # if(!all(colnames(special.periods.data) %in% c(ID.colname, "DATE.IN", "DATE.OUT", "TYPE", special.periods.method, medication.class.colnames)))
      # {
      #   if( !suppress.warnings ) warning(paste0("The special periods data can only contain columns
      #                                           with the names \"", ID.colname, "\", \"DATE.IN\", \"DATE.OUT\", \"TYPE\", ",
      #                                           paste(shQuote(medication.class.colnames), collapse = ", "), ", and a column with
      #                                           customized instructions how to handle a specific period.\n
      #                                           Please refer to the documentation for more information.\n"));
      #   return (NULL);
      # }

      if( !special.periods.method %in% c("continue", "discard", "carryover") && !special.periods.method %in% names(special.periods.data))
      {
        if( !suppress.warnings ) warning(paste0("special.periods.method must be either of 'continue', 'discard',
                                                'carryover', or a column name in the special periods data!\n"));
        return (NULL);
      }
      if(special.periods.method %in% names(special.periods.data) && any(!unique(special.periods.data[[special.periods.method]] %in% c("continue", "discard", "carryover"))))
      {
        unexpected.values <- unique(special.periods.data[[special.periods.method]][!special.periods.data[[special.periods.method]] %in% c("continue", "discard", "carryover")])

        if( !suppress.warnings ) warning(paste0("Column special.periods.method='",special.periods.method, "' in special periods data contains unexpected values: ",
                                                unexpected.values,"\n"));
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
    if(anyNA(presc.data[[presc.date.colname]])){
      if( !suppress.warnings ) warning(paste0("Column presc.date.colname='",presc.date.colname,"' cannot contain missing values!\n"));
      return (NULL);
    }
    if( !is.na(disp.date.colname) && !(disp.date.colname %in% names(disp.data)) )
    {
      if( !suppress.warnings ) warning(paste0("Column disp.date.colname='",disp.date.colname,"' must appear in the dispensing data!\n"));
      return (NULL);
    }
    if(anyNA(disp.data[[disp.date.colname]])){
      if( !suppress.warnings ) warning(paste0("Column disp.date.colname='",disp.date.colname,"' cannot contain missing values!\n"));
      return (NULL);
    }
    if( any(!is.na(medication.class.colnames) & !(medication.class.colnames %in% names(disp.data)) & !(medication.class.colnames %in% names(presc.data))) ) # deal with the possibility of multiple column names
    {
      if( !suppress.warnings ) warning(paste0("Column(s) medication.class.colnames=",paste0("'",medication.class.colnames,"'",collapse=",")," must appear in the dispensing and prescribing data!\n"));
      return (NULL);
    }
    if( !is.na(total.dose.colname) && !(total.dose.colname %in% names(disp.data)) )
    {
      if( !suppress.warnings ) warning(paste0("Column total.dose.colname='",total.dose.colname,"' must appear in the dispensing data!\n"));
      return (NULL);
    }
    if(anyNA(disp.data[[total.dose.colname]])){
      if( !suppress.warnings ) warning(paste0("Column total.dose.colname='",total.dose.colname,"' cannot contain missing values!\n"));
      return (NULL);
    }
    if( !is.na(presc.daily.dose.colname) && !(presc.daily.dose.colname %in% names(presc.data)) )
    {
      if( !suppress.warnings ) warning(paste0("Column presc.daily.dose.colname='",presc.daily.dose.colname,"' must appear in the prescribing data!\n"));
      return (NULL);
    }
    if(anyNA(presc.data[[presc.daily.dose.colname]])){
      if( !suppress.warnings ) warning(paste0("Column presc.daily.dose.colname='",presc.daily.dose.colname,"' cannot contain missing values!\n"));
      return (NULL);
    }
    if( !is.na(presc.duration.colname) && !(presc.duration.colname %in% names(presc.data)) )
    {
      if( !suppress.warnings ) warning(paste0("Column presc.duration.colname='",presc.duration.colname,"' must appear in the prescribing data!\n"));
      return (NULL);
    }

    if( !is.logical(force.presc.renew) && !force.presc.renew %in% names(disp.data) )
    {
      if( !suppress.warnings ) warning(paste0("Column force.presc.renew='",force.presc.renew,"' must appear in the dispensing data!\n"));
      return (NULL);
    }

    if( !is.logical(split.on.dosage.change) && !split.on.dosage.change %in% names(disp.data) )
    {
      if( !suppress.warnings ) warning(paste0("Column split.on.dosage.change='",split.on.dosage.change,"' must appear in the dispensing data!\n"));
      return (NULL);
    }

    if( !trt.interruption %in% c("continue", "discard", "carryover") && !trt.interruption %in% names(disp.data))
    {
      if( !suppress.warnings ) warning(paste0("trt.interruption must be either of 'continue', 'discard',
                                              'carryover', or a column name in the dispensing data!\n"));
      return (NULL);
    }
    if(trt.interruption %in% names(disp.data) && any(!unique(disp.data[[trt.interruption]]) %in% c("continue", "discard", "carryover")))
    {
      unexpected.values <- unique(disp.data[[trt.interruption]][disp.data[[trt.interruption]] %in% c("continue", "discard", "carryover")])

      if( !suppress.warnings ) warning(paste0("Column trt.interruption='",trt.interruption, "' contains unexpected values: ",
                                              unexpected.values,"\n"));
      return (NULL);
    }

    if(".episode" %in% colnames(presc.data)){
      {
        if( !suppress.warnings ) warning("The column name \'.episode\' is used internally, please use another column name.");
        return (NULL);
      }
    }

    if( is.na(date.format) || is.null(date.format) || length(date.format) != 1 || !is.character(date.format) )
    {
      if( !suppress.warnings ) warning(paste0("The date format must be a single string!\n"));
      return (NULL);
    }

    # convert column names
    setnames(presc.data,
             old = c(ID.colname,
                     presc.date.colname,
                     presc.daily.dose.colname,
                     presc.duration.colname),
             new = c("ID",
                     "PRESC.DATE",
                     "DAILY.DOSE",
                     "episode.duration"))

    setnames(disp.data,
             old = c(ID.colname,
                     disp.date.colname,
                     total.dose.colname),
             new = c("ID",
                     "DISP.DATE",
                     "TOTAL.DOSE"))

    # convert dates
    disp.data[,DISP.DATE := as.Date(DISP.DATE, format = date.format)];
    presc.data[,PRESC.DATE := as.Date(PRESC.DATE, format = date.format)];
    if(!is.null(special.periods.data))
    {
      setnames(special.periods.data,
               old = c(ID.colname),
               new = c("ID"))

      special.periods.data[,`:=` (DATE.IN = as.Date(DATE.IN, format = date.format),
                                  DATE.OUT = as.Date(DATE.OUT, format = date.format))];

      special.periods.data[,SPECIAL.DURATION := as.numeric(DATE.OUT-DATE.IN)];
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

  # helper function to process each patient
  process_patient <- function(pat)
  {
    # helper function to process each medication
    process_medication <- function(med)
    {

      # helper function to process each dispensing event
      process_dispensing_events <- function(event)
      {
        # helper function to compute special intervals
        compute.special.intervals <- function(data,
                                              DATE.IN.colname = "DATE.IN",
                                              DATE.OUT.colname = "DATE.OUT",
                                              TYPE.colname = "TYPE",
                                              CUSTOM.colname = special.periods.method)
          {

          if(CUSTOM.colname %in% colnames(data)){
            setnames(data, old = CUSTOM.colname, new = "CUSTOM")
          } else { data[,CUSTOM := special.periods.method]}


          # convert dates
          data[, (DATE.IN.colname) := as.Date(get(DATE.IN.colname), format = date.format)]
          data[, (DATE.OUT.colname) := as.Date(get(DATE.OUT.colname), format = date.format)]

          # add durations
          data[,DURATION := as.numeric(get(DATE.OUT.colname) - get(DATE.IN.colname))]

          # add episodes
          data[,.episode := seq_len(.N)]

          # melt special episodes
          data.melt <- melt(data,
                            measure.vars = c(DATE.IN.colname, DATE.OUT.colname),
                            variable.name = "EVENT",
                            value.name = "DATE")

          # sort by DATE.IN
          setkeyv(data.melt, cols = c("DATE", ".episode"))

          # add dispensing event
          data.melt <- rbind(data.melt,
                             data.table(ID = pat,
                                        DATE = disp.start.date.i,
                                        EVENT = "DISP.DATE",
                                        .episode = 0),
                             fill = TRUE)

          # find row with end of episode
          data.melt <- rbind(data.melt,
                             data.table(ID = pat,
                                        DATE = end.episode,
                                        EVENT = "episode.end",
                                        .episode = -1),
                             fill = TRUE)

          data.melt[, EVENT := factor(EVENT, levels = c("DATE.OUT", "DISP.DATE", "DATE.IN", "episode.end"))]

          setorderv(data.melt, cols = c("DATE", "EVENT", ".episode"), na.last = TRUE)

          # calculate durations of intersections
          data.melt[,`:=` (DISP.EVENT = 0,
                           CARRYOVER.DURATION = 0,
                           INT.DURATION = as.numeric(shift(DATE, n = 1, type = "lead")-DATE))]

          # find active period
          data.melt[,active.episode := sapply(seq(nrow(data.melt)), function(x) {

            dt <- data.melt[seq(x)]

            closed.episodes <- dt[duplicated(dt[,.episode]),.episode]

            active.episode <- dt[!.episode %in% closed.episodes, suppressWarnings(max(.episode))]

          })]

          # indicate intersections that should be counted
          data.melt[active.episode %in% unique(data.melt[CUSTOM == "continue", .episode]),
                         `:=` (SPECIAL.PERIOD = 1,
                               DISP.EVENT = 1)]
          data.melt[active.episode == 0, DISP.EVENT := 1]

          # calculat durations during carryover
          if( "carryover" %in% unique(data.melt$CUSTOM) ){


            data.melt[active.episode %in% unique(data.melt[CUSTOM == "carryover", .episode]),
                           CARRYOVER.DURATION := INT.DURATION]

            # remove duration during carryover
            data.melt[CARRYOVER.DURATION != 0,
                           INT.DURATION := 0]

         }

          # remove events before dispensing date and after end date
          first.row <- data.melt[EVENT == "DISP.DATE", which = TRUE]

          last.row <- data.melt[EVENT == "episode.end", which = TRUE]

          data.melt <- data.melt[first.row:last.row]

          # identify rows after discard
          data.melt[, .drop := 0]
          if("discard" %in% data$CUSTOM){
            data.melt[,DISP.EVENT := 0]
            data.melt[CUSTOM == "discard", DISP.EVENT := 1]
            data.melt[,.drop := cumsum(DISP.EVENT)]
            data.melt[CUSTOM == "discard" & EVENT == DATE.IN.colname, .drop := .drop-1]

            # remove durations after discard
            data.melt[CUSTOM == "discard", `:=` (DISP.EVENT = 0,
                                                      INT.DURATION = 0)]
          }

          # drop rows after discard
          data.melt.drop <- data.melt[.drop == 0]

          # create intervals of continuous use
          data.melt.drop[,.interval := rleidv(data.melt.drop, cols = "DISP.EVENT")]

          data.melt.drop[DISP.EVENT == 1,.interval := as.integer(.interval+1)]

          # calculate sum of all durations
          sum.duration <- sum(data.melt.drop$INT.DURATION, na.rm = TRUE);

          # if the supply duration is shorter than the sum of the duration
          if(duration.i <= sum.duration)
          {
            # calculate cumulative sum of durations
            data.melt.drop[,cum.duration := cumsum(INT.DURATION)];
            # subset to all rows until supply is exhaused and add 1
            .rows <- data.melt.drop[cum.duration <= duration.i,which=TRUE];
            if( length(.rows) == 0 ) {.rows <- 0};
            data.melt.drop <- data.melt.drop[c(.rows, tail(.rows,1)+1)];

            # calculate remaining duration for last row
            sum.duration <- sum(head(data.melt.drop,-1)$INT.DURATION);
            data.melt.drop[nrow(data.melt.drop), INT.DURATION := duration.i-sum.duration];
          }

          # calculate total duration
          data.melt.drop[,DURATION := sum(INT.DURATION, na.rm = TRUE), by = .interval]

          # calculate duration covered during special intervals
          data.melt.drop[,SPECIAL.DURATION := 0]

          data.melt.drop[SPECIAL.PERIOD == 1, SPECIAL.DURATION := sum(INT.DURATION, na.rm = TRUE), by = .interval]
          # data.melt.drop[(CUSTOM == "continue" & EVENT == DATE.IN.colname) | (shift(CUSTOM, n = 1, type = "lead") == "continue" & shift(EVENT, n = 1, type = "lead") == DATE.OUT.colname), # | (shift(CUSTOM, n = 1, type = "lag") == "continue" & shift(EVENT, n = 1, type = "lag") == DATE.IN.colname),
          #                SPECIAL.DURATION := sum(INT.DURATION, na.rm = TRUE), by = .interval]
          data.melt.drop[,SPECIAL.DURATION := max(SPECIAL.DURATION, na.rm = TRUE), by = .interval]

          # calculate duration NOT covered during special intervals
          data.melt.drop[,CARRYOVER.DURATION := sum(CARRYOVER.DURATION, na.rm = TRUE), by = .interval]

          # subset to first and last row of interval
          events <- data.melt.drop[ data.melt.drop[, .I[c(1L,.N)], by=.interval]$V1 ]
          events[,CUSTOM := last(CUSTOM), by = .interval]

          # convert to wide format with start and end date of intervals
          events[,EVENT := rep(c("DISP.START", "DISP.END"), nrow(events)/2)]

          all.events <- dcast(events, ID + CUSTOM + DURATION + SPECIAL.DURATION + CARRYOVER.DURATION + .interval ~ EVENT, value.var = "DATE")
          setorderv(all.events, cols = ".interval")

          # create all events table
          all.events <- cbind(all.events[,c("ID",
                                            "CUSTOM",
                                            "DISP.START",
                                            "DURATION",
                                            "SPECIAL.DURATION",
                                            "CARRYOVER.DURATION"), with = FALSE],
                              data.table(DAILY.DOSE = as.numeric(presc.dose.i),
                                         episode.start = start.episode,
                                         episode.end = end.episode))

          return(all.events)
        }

        if(exists("debug.mode") && debug.mode==TRUE) print(paste("Event:", event));
        ## !Important: We assume that the prescribed dose can be accomodated with the dispensed medication

        #subset data to event
        curr_disp <- med_disp[event];
        orig.disp.date <- curr_disp[["DISP.DATE"]]

        # if current dispensing event is before first prescription date, don't calculate a duration
        if(orig.disp.date < first_presc[["PRESC.DATE"]])
        {
          med_event <- cbind(curr_disp[,c("ID", medication.class.colnames, "TOTAL.DOSE", "DISP.DATE"), with = FALSE],
                             DISP.START = orig.disp.date,
                             DURATION = 0,
                             DAILY.DOSE = NA,
                             SPECIAL.DURATION = NA);
          # if current dispensing event is after end of last prescription date, don't calculate a duration (only when last prescription indicates termination)
        } else
        {
          #select prescription episodes ending after the original dispensing date
          episodes <- med_presc[orig.disp.date < episode.end | is.na(episode.end), which = TRUE];

          # if the current dispensing event is after the last prescription episode, don't calculate a duration
          if(length(episodes) == 0)
          {
            med_event <- cbind(curr_disp[,c("ID", medication.class.colnames, "TOTAL.DOSE", "DISP.DATE"), with = FALSE],
                               DISP.START = orig.disp.date,
                               DURATION = 0,
                               DAILY.DOSE = NA,
                               SPECIAL.DURATION = NA);
          } else
          {
            ## for each prescription episode, calculate the duration with the current dose
            total.dose.i <- curr_disp[["TOTAL.DOSE"]]; #dispensed dose
            presc.dose.i <- 0; # initialize prescibed dose as 0
            disp.start.date.i <- orig.disp.date; #start date of dispensing event

            #select prescription episodes ending after the original dispensing date and add the one immediately before
            curr_med_presc <- copy(med_presc)

            # if supply should be finished with original dose, collapse consecutive episodes with dosage > 0
            if(split.on.dosage.change == FALSE){

              curr_med_presc[(orig.disp.date < episode.end | is.na(episode.end)) & DAILY.DOSE > 0,POS.DOSE := 1]
              curr_med_presc[,.episode := rleidv(.SD, cols = "POS.DOSE")]

              curr_med_presc[POS.DOSE == 1,episode.start := head(episode.start,1), by = .episode]; # first start date per episode
              curr_med_presc[POS.DOSE == 1,episode.end:= tail(episode.end,1), by = .episode]; # last end date per episode
              curr_med_presc[POS.DOSE == 1,DAILY.DOSE:= head(DAILY.DOSE,1), by = .episode]; # first dosage per episode

              curr_med_presc <- unique(curr_med_presc, by = c("episode.start", "episode.end"), fromLast = TRUE);
              curr_med_presc[,.episode := rleidv(curr_med_presc, cols = c("episode.start", "episode.end"))];

              #select prescription episodes ending after the original dispensing date
              episodes <- curr_med_presc[orig.disp.date < episode.end | is.na(episode.end), which = TRUE];

              }

            # rm.trt.episode <- FALSE; # will be set to TRUE in case of calculations during treatment interruptions

            stop <- 0;

            med_event <- NULL;
            for(episode in episodes)
            {

              presc.dose.i <- curr_med_presc[[episode,"DAILY.DOSE"]]; # prescribed daily dose
              start.episode <- curr_med_presc[episode,episode.start];
              end.episode <- curr_med_presc[episode,episode.end];

              if(presc.dose.i == 0) # if event happens during treatment interruption (prescribed dose = 0), check what to do
              {
                if(trt.interruption == "continue") # if trt.interruption is set to continue, continue with last prescribed dose
                {
                  presc.dose.i <- curr_med_presc[[episode-1,"DAILY.DOSE"]];

                  # adjust prescription episode to previous episode
                  start.episode <- curr_med_presc[episode-1,episode.start];
                  end.episode <- curr_med_presc[episode-1,episode.end];

                  stop <- 1;

                  # rm.trt.episode <- TRUE; # make sure that prescription start- and end date are removed at the end
                } else if(trt.interruption == "discard") # if trt.interruption is set to discard, don't calculate anything
                {
                  if(is.null(med_event))
                  {
                    med_event <- cbind(curr_disp[,c("ID", medication.class.colnames, "TOTAL.DOSE", "DISP.DATE"), with = FALSE],
                                       DISP.START = disp.start.date.i,
                                       DURATION = 0,
                                       DAILY.DOSE = NA,
                                       SPECIAL.DURATION = NA);
                  }

                  break
                } else
                {
                  episode <- episode + 1; # else skip to next episode
                  next;
                }
              }

              #if(curr_disp$DISP.DATE == as.Date("2057-10-20")) browser()
              # if disp.start.date.i is after end.episode date, go to next episode.
              if( !is.na(curr_med_presc[episode,episode.end]) & disp.start.date.i >= curr_med_presc[episode,episode.end] ) {
                next;
              }

              # if it is not the first episode, adjust supply start date to prescription start date
              if(episode != episodes[1]) disp.start.date.i <- curr_med_presc[episode,episode.start];

              duration.i <- total.dose.i/presc.dose.i; # calculate duration

              disp.end.date.i <- disp.start.date.i + duration.i; # calculate end date of supply

              # add special durations during the supply period
              special.periods.duration.i <- 0;
              if(nrow(med_special.periods_events) != 0 & !is.na(duration.i))
              {
                # check for special durations within the episode
                med_special.periods_events_i <- med_special.periods_events[(DATE.IN <= end.episode|is.na(end.episode)) & DATE.OUT > start.episode];

                if(nrow(med_special.periods_events_i) > 0)
                {
                  all.events <- compute.special.intervals(med_special.periods_events_i);

                  sum.duration <- sum(all.events$DURATION, na.rm = TRUE)

                  # if last line is "discard", create med_event
                  if(!is.na(last(all.events$CUSTOM)) && last(all.events$CUSTOM) == "discard") {

                    med_event <-  rbind(med_event,
                                        cbind(curr_disp[,c("ID", medication.class.colnames, "TOTAL.DOSE", "DISP.DATE"), with = FALSE],
                                              all.events[,3:9]),
                                        fill = TRUE);

                    break;
                  } else if( duration.i == sum.duration ) # if supply is equal to the sum of durations
                  {
                    med_event <-  rbind(med_event,
                                        cbind(curr_disp[,c("ID", medication.class.colnames, "TOTAL.DOSE", "DISP.DATE"), with = FALSE],
                                              all.events[,3:9]),
                                        fill = TRUE);

                    break;

                  } else if(is.na(last(all.events$episode.end))) # if last event is not terminated
                  {
                    all.events[nrow(all.events), DURATION := DURATION + (duration.i-sum.duration)];

                    med_event <-  rbind(med_event,
                                        cbind(curr_disp[,c("ID", medication.class.colnames, "TOTAL.DOSE", "DISP.DATE"), with = FALSE],
                                              all.events[,3:9]),
                                        fill = TRUE);
                    break;
                  } else # if supply duration is longer than the sum of the durations
                  {
                    # calculate the carryover dose
                    oversupply <- duration.i-sum.duration; # calculate remaining days of oversupply
                    total.dose.i <- presc.dose.i*oversupply; # calculate remaining total dose

                    med_event <-  rbind(med_event,
                                        cbind(curr_disp[,c("ID", medication.class.colnames, "TOTAL.DOSE", "DISP.DATE"), with = FALSE],
                                              all.events[,3:9]),
                                        fill = TRUE);
                    next;
                  }
                }
              }

              # check various parameters to decide wheter to stop or continue

              # check if end of supply is before end of episode OR last row of prescription episodes is reached
              if( disp.end.date.i < curr_med_presc[episode,episode.end] | episode == last(episodes) )
              {
                stop <- 1;
              } else {
                episode <- episode + 1; # get next prescription episode
                next.presc.dose <- curr_med_presc[[episode,"DAILY.DOSE"]]; # get next episode's dosage

                # if there is a treatment interruption and trt.interruption is set to continue, stop
                if( next.presc.dose == 0 & trt.interruption == "continue" ) stop <- 1;

                # if there is no treatment interruption, but a dosage change and split.on.dosage.change is set FALSE, stop
                if( next.presc.dose != 0 & next.presc.dose != presc.dose.i & split.on.dosage.change == FALSE ) stop <- 1;
              }

              if( stop == 1 )
              {
                # if( rm.trt.episode == TRUE )
                # {
                #   start.episode <- as.Date(NA, format = date.format);
                #   end.episode <- as.Date(NA, format = date.format);
                # }

                med_event <- rbind(med_event,
                                   cbind(curr_disp[,c("ID", medication.class.colnames, "TOTAL.DOSE", "DISP.DATE"), with = FALSE],
                                         data.table(DISP.START = disp.start.date.i,
                                                    DURATION = as.numeric(duration.i),
                                                    episode.start = start.episode,
                                                    episode.end = end.episode,
                                                    DAILY.DOSE = as.numeric(presc.dose.i),
                                                    SPECIAL.DURATION = as.numeric(special.periods.duration.i))),
                                   fill = TRUE);
                break;
              } else
              {
                duration.i <- end.episode - disp.start.date.i; # calculate duration until end of episode
                oversupply <- disp.end.date.i - end.episode; # calculate remaining days of oversupply
                total.dose.i <- presc.dose.i*oversupply; # calculate remaining total dose

                # if( rm.trt.episode == TRUE )
                # {
                #   start.episode <- as.Date(NA, format = date.format);
                #   end.episode <- as.Date(NA, format = date.format);
                # }

                #create medication event
                med_event <- rbind(med_event,
                                   cbind(curr_disp[,c("ID", medication.class.colnames, "TOTAL.DOSE", "DISP.DATE"), with = FALSE],
                                         data.table(DISP.START = disp.start.date.i,
                                                    DURATION = as.numeric(duration.i),
                                                    episode.start = start.episode,
                                                    episode.end = end.episode,
                                                    DAILY.DOSE = as.numeric(presc.dose.i),
                                                    SPECIAL.DURATION = as.numeric(special.periods.duration.i))),
                                   fill = TRUE);
              }
            }

            med_event;
          }
        }
      }

      if(exists("debug.mode") && debug.mode==TRUE) print(paste("Medication:", med));

      ## subset data to medication

      setkeyv(pat_disp, medication.class.colnames);
      setkeyv(pat_presc, medication.class.colnames);

      med_disp <- pat_disp[list(disp_presc[med, medication.class.colnames, with = FALSE])];

      med_presc <- pat_presc[list(disp_presc[med, medication.class.colnames, with = FALSE])];

      setkeyv(med_disp, cols = "DISP.DATE");
      setkeyv(med_presc, cols = "PRESC.DATE");

      med_special.periods_events <- copy(special.periods_events)
      if( !is.null(special.periods.data) )
      {
        special.colnames <- intersect(medication.class.colnames, colnames(special.periods.data))

        if( length(special.colnames) > 0 ) {
          setkeyv(special.periods_events, special.colnames);
          med_special.periods_events <- special.periods_events[list(disp_presc[med, special.colnames, with = FALSE])];
        }

        setkeyv(med_special.periods_events, cols = "DATE.IN")
      }

      # determine date of initial prescription
      first_presc <- med_presc[1];

      # determine date of initial dispense
      first_disp <- med_disp[["DISP.DATE"]][1];

      #if force.presc.renew, trt.interruption, and split.on.dosage.change are not set globally, set for medication based on first dispensing event
      if( !is.logical(force.presc.renew) )
      {
        force.presc.renew <- as.logical(first_disp[[force.presc.renew]]);
      }
      if( !trt.interruption %in% c("continue", "discard", "carryover") )
      {
        trt.interruption <- as.logical(first_disp[[trt.interruption]]);
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

        interruption_dates <- presc_events[["PRESC.DATE"]][presc_omit]; # determine dates of treatment interruptions

        presc_interruptions <- med_presc[rep(1, length(presc_omit))]; # create table with one row for each interruption

        presc_interruptions[, c(visit.colname, "PRESC.DATE", "DAILY.DOSE", "episode.duration") :=
                              list(presc_events[[visit.colname]][presc_omit], interruption_dates, 0, NA)]; # adjust variables

        med_presc <- rbind(med_presc, presc_interruptions); # bind to existing prescriptions
        setkeyv(med_presc, cols = "PRESC.DATE"); # order by date

        med_presc[,.episode := rleidv(med_presc, cols = "DAILY.DOSE")]; # add counter for treatment episodes
      }

      setorder(med_presc);

      ## construct treatment episodes
      # create new .episode counter
      med_presc[,.episode := rleidv(med_presc, cols = c("DAILY.DOSE", "episode.duration"))];

      # if consecutive episodes with set end date, increase .episode counter

      if( nrow(med_presc) > 2 )
      {
        for( n in 2:(nrow(med_presc)))
        {
          if( !is.na(med_presc[n,"episode.duration", with = FALSE]) & !is.na(med_presc[n-1,"episode.duration", with = FALSE]) )
          {
            med_presc[n:nrow(med_presc), .episode := as.integer(.episode + 1)];
          }
        }
      } else if( nrow(med_presc) == 2 )
      {
        med_presc[!is.na(shift(episode.duration, type = "lag")) & !is.na(episode.duration), .episode := as.integer(.episode + 1)];
      }

      # add episodes with same dose but set end date to last episode
      .row <- med_presc[is.na(shift(episode.duration, type = "lag")) & shift(DAILY.DOSE, type = "lag") == DAILY.DOSE & !is.na(episode.duration), which = TRUE];
      if( length(.row)>0 )
      {
        med_presc[.row:nrow(med_presc),.episode := as.integer(.episode-1)];
      }

      ## set start and end of prescription dates per group
      med_presc[, `:=` (episode.start = PRESC.DATE, # set prescription date as start date
                        episode.end = PRESC.DATE)]; # set end date to prescription date ...

      med_presc[,episode.end := shift(episode.end, type = "lead")]; # ... and shift end dates up by one

      # adjust end date if prescription duration is provided and change start date of following prescriptions
      med_presc[!is.na(episode.duration) & ((PRESC.DATE + episode.duration) <= episode.end | is.na(episode.end)), episode.end := PRESC.DATE + episode.duration]; # only if prescription ends before the current end prescription date!
      end.limited.presc <- head(med_presc,-1)[!is.na(episode.duration) & ((PRESC.DATE + episode.duration) <= episode.end | is.na(episode.end))]$episode.end; #don't include last prescription episode
      med_presc[shift(!is.na(episode.duration), type = "lag") & shift((PRESC.DATE + episode.duration) <= episode.end, type = "lag"), episode.start := end.limited.presc];
      med_presc[PRESC.DATE>episode.start & DAILY.DOSE != 0,episode.start:=PRESC.DATE];

      # combine episodes with set durations with previous episodes of same dosage but unrestricted duration
      med_presc[shift(DAILY.DOSE,type="lag")==DAILY.DOSE & !is.na(shift(episode.duration,type="lag")) & shift(episode.end, type = "lag") == episode.start, .episode := as.integer(.episode-1)];

      # fill in start and end dates by group
      med_presc[,episode.start := head(episode.start,1), by = .episode]; # first start date per episode
      med_presc[,episode.end:= tail(episode.end,1), by = .episode]; # last end date per episode
      med_presc[,PRESC.DATE := min(PRESC.DATE), by = .episode]; # set PRESC.DATE to first start date

      # collapse episodes
      med_presc <- unique(med_presc, by = ".episode", fromLast = TRUE);
      med_presc[,.episode := rleidv(med_presc, cols = c("episode.start", "episode.end"))];

      # remove episodes where end date is before start date
      rm.episode <- med_presc[episode.end <= episode.start, which = TRUE];
      if( length(rm.episode) > 0 )
      {
        med_presc <- med_presc[-rm.episode];
      }
      med_presc[,.episode := rleidv(med_presc)];

      # collapse consecutive episodes where end date of the former is before start date of the latter
      med_presc[shift(episode.end,type = "lag") > episode.start & shift(DAILY.DOSE,type = "lag") == DAILY.DOSE,
                .episode := as.integer(.episode-1)];
      med_presc[,episode.start := head(episode.start,1), by = .episode]; # first start date per episode
      med_presc[,episode.end:= tail(episode.end,1), by = .episode]; # last end date per episode
      med_presc <- unique(med_presc, by = ".episode");
      med_presc[,.episode := rleidv(med_presc, cols = c("episode.start", "episode.end"))];

      # add treatment interruptions

      med_presc <- rbind(med_presc,med_presc[shift(episode.start,type = "lead")!=episode.end][,c("DAILY.DOSE", "episode.start", ".episode") := list(0, episode.end, 0)]);
      setorder(med_presc, episode.start, episode.end);
      end.trt.interruptions <- med_presc[shift(episode.end,type = "lag")!=episode.start]$episode.start;
      med_presc[.episode == 0, episode.end := end.trt.interruptions];

      if( force.init.presc == TRUE )
      {
        # if initial dispense is before initial prescription, adjust date of initial prescription to match initial dispense
        # but only if first prescription is unlimited
        if( first_disp < first(med_presc[["episode.start"]]) & is.na(head(med_presc[["episode.duration"]],1)) )
        {
          # adjust first prescription date
          first_presc[1, PRESC.DATE := first_disp];
          med_presc[1, episode.start := first_disp];
        }
      }

      ## calculate medication events for "simple" events not extending over multiple episodes or affected by special periods
      # add prescription events to dispensing events

      for( i in 1:nrow(med_presc) )
      {
        med_disp[DISP.DATE >= med_presc[i,episode.start] & (DISP.DATE < med_presc[i,episode.end] | is.na(med_presc[i,episode.end])),
                 c("episode.start", "episode.end", "DAILY.DOSE") := list(med_presc[i,episode.start], med_presc[i,episode.end],med_presc[i,DAILY.DOSE])];
      }

      med_disp[,DURATION := (TOTAL.DOSE)/(DAILY.DOSE)];
      med_disp[,`:=` (DISP.START = DISP.DATE,
                      DISP.END = DISP.DATE+DURATION)];

      med_disp[DISP.END > episode.end, .out := 1];

      # add special periods to dispensing events
      med_disp[,.special.periods := as.numeric(NA)];

      if( nrow(med_special.periods_events) != 0 ){

         for( i in 1:nrow(med_special.periods_events) )
        {
          med_disp[(DISP.END >= med_special.periods_events[i,DATE.IN] & DISP.START < med_special.periods_events[i,DATE.OUT])|(DISP.START >= med_special.periods_events[i,DATE.IN] & DISP.START < med_special.periods_events[i,DATE.OUT]),
                   .special.periods := 1];
        }
      }

      medication_events <- med_disp[DURATION != Inf & is.na(.out) & is.na(.special.periods),
                                    c("ID",
                                      medication.class.colnames,
                                      "TOTAL.DOSE",
                                      "DISP.DATE",
                                      "DISP.START",
                                      "DURATION",
                                      "DAILY.DOSE",
                                      "episode.start",
                                      "episode.end"), with = FALSE];
      medication_events[,SPECIAL.DURATION := 0];

      med_disp <- med_disp[DURATION == Inf | .out == 1 | .special.periods == 1];

      ## apply process_dispensing_events to each dispensing event
      medication_events_rest <- NULL;
      if( nrow(med_disp) > 0 )
      {
        medication_events_rest <- do.call(rbindlist,
                                          list(l = lapply(1:nrow(med_disp), FUN = function(i) process_dispensing_events(event = i)),
                                               fill = TRUE));
      }

      medication_events <- rbind(medication_events, medication_events_rest, fill = TRUE);

      setorderv(medication_events,cols=c("DISP.DATE", "DISP.START"));

      if( force.presc.renew == TRUE ) # add number of prescription interruptions
      {
        tot.presc.interruptions <- nrow(med_presc[DAILY.DOSE==0]);

        medication_events[,tot.presc.interruptions := tot.presc.interruptions];
      }

      if( split.on.dosage.change == TRUE ) # add number of dosage changes
      {
        tot.dosage.changes <- (nrow(med_presc) - 1 - 2*nrow(med_presc[DAILY.DOSE==0]));

        medication_events[,tot.dosage.changes := tot.dosage.changes];
      }

      # presc_episode_no_dispense <- med_presc[!medication_events[,c("DAILY.DOSE","episode.start","episode.end")],
      #                                         on = c("DAILY.DOSE","episode.start", "episode.end")];
      #
      # presc_episode_no_dispense[,c(".episode","VISIT", "episode.duration", "PRESC.DATE") := NULL];
      #
      # medication_events <- rbind(medication_events, presc_episode_no_dispense, fill = TRUE);

      # add episode number
      med_presc <- med_presc[DAILY.DOSE != 0, episode.ID := seq(.N)];

      # calculate duration
      med_presc[,episode.duration := as.numeric(episode.end-episode.start)];

      # compute prescription events
      prescription_events <- med_presc[DAILY.DOSE != 0,
                                       c("ID",
                                         medication.class.colnames,
                                         "DAILY.DOSE",
                                         "episode.ID",
                                         "episode.start",
                                         "episode.duration",
                                         "episode.end"), with = FALSE]

      return(list(DURATIONS = medication_events,
                  PRESCRIPTION_EPISODES = prescription_events));

      # medication_events;
    }

    if(exists("debug.mode") && debug.mode==TRUE) print(paste("Patient:",pat));

    # subset data to patient
    pat_disp <- disp.data[ID == pat, c("ID",
                                       "DISP.DATE",
                                       medication.class.colnames,
                                       "TOTAL.DOSE"), with = FALSE];

    pat_presc <- presc.data[ID == pat, c("ID",
                                         "PRESC.DATE",
                                         medication.class.colnames,
                                         "DAILY.DOSE",
                                         "episode.duration"), with = FALSE];
    if(visit.colname %in% colnames(presc.data)){
      pat_presc <- cbind(presc.data[ID == pat, visit.colname, with = FALSE], pat_presc);
    };
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
      presc_events <- unique(pat_presc[,"PRESC.DATE"]);
      presc_events[,(visit.colname) := 0:(nrow(presc_events)-1)];
      pat_presc <- merge(pat_presc, presc_events, by = "PRESC.DATE");
      setorderv(pat_presc, medication.class.colnames);
    } else
    {
      presc_events <- unique(pat_presc[,.(PRESC.DATE, VISIT)]); # extract prescription instances
    }

    # if duplicate visit numbers for different dates or vice versa, throw an error
    if( length(unique(presc_events[["PRESC.DATE"]])) != nrow(presc_events) )
    {
      {
        if( !suppress.warnings ) warning("Prescription dates and visit number don't match for patient Nr.", pat);
        return (NULL);
      }
    }

    # extract special periods
    if( !is.null(special.periods.data) )
    {
      special.periods_events <- special.periods.data[ID == pat];
    } else
    {
      special.periods_events <- data.table(NULL);
    }

    setkeyv(presc_events, cols = "PRESC.DATE");

    # apply process_medication() function to each medication present in both databses
    patient_events <- NULL;
    if( nrow(disp_presc) != 0 )
    {
      patient_events <- lapply(1:nrow(disp_presc), FUN = function(i) process_medication(med = i));

      # patient_events <- do.call(rbindlist, list(l = lapply(1:nrow(disp_presc), FUN = function(i) process_medication(med = i)),
      #                                           fill = TRUE));
    }

    setkeyv(pat_disp, cols = medication.class.colnames);
    setkeyv(pat_presc, cols = medication.class.colnames);
    #
    patient_events[[1]][[1]] <- rbind(pat_disp[list(disp_no_presc[,medication.class.colnames, with = FALSE]), c("ID", "DISP.DATE", medication.class.colnames, "TOTAL.DOSE"), with = FALSE],
                                      pat_presc[list(presc_no_disp[,medication.class.colnames, with = FALSE]), c("ID", medication.class.colnames, "DAILY.DOSE"), with = FALSE],
                                      patient_events[[1]][[1]],
                                      fill = TRUE);

    # update progress bar
    if(progress.bar == TRUE) { setTxtProgressBar(pb, getTxtProgressBar(pb)+1) };

    patient_events;
  }

  # extract IDs of all patients present in dispensing and prescription database
  disp_presc_IDs <- sort(intersect(disp.data[["ID"]], presc.data[["ID"]]));

  # progress bar
  if(progress.bar == TRUE) {
    pb <- txtProgressBar(min = 0, max = length(disp_presc_IDs), style = 3);
  }

  # apply process_patient function
  setkeyv(disp.data, cols = "ID");
  setkeyv(presc.data, cols = "ID");

  events_output_list <- lapply(disp_presc_IDs, FUN = function(i) process_patient(pat = i));

  events_output_durations <- do.call(rbindlist, list(l = lapply(events_output_list, FUN = function(i) {
                             do.call(rbindlist, list(l = lapply(i, FUN = function(j) {
                                     j[[1]] }), fill = TRUE)) }), fill = TRUE));

  events_output_prescriptions <- do.call(rbindlist, list(l = lapply(events_output_list, FUN = function(i) {
                                 do.call(rbindlist, list(l = lapply(i, FUN = function(j) {
                                         j[[2]] }), fill = TRUE)) }), fill = TRUE));


  # events_output <- do.call(rbindlist, list(l = lapply(disp_presc_IDs, FUN = function(i) process_patient(pat = i)),
  #                                          fill = TRUE));

  # key by ID, medication class, and dispensing date
  setkeyv(events_output_durations, cols = c("ID", medication.class.colnames, "DISP.DATE"));
  setkeyv(events_output_prescriptions, cols = c("ID", medication.class.colnames));

  # convert column names
  setnames(events_output_durations,
           old = c("ID",
                   "DISP.DATE",
                   "DAILY.DOSE",
                   "TOTAL.DOSE"),
           new = c(ID.colname,
                   disp.date.colname,
                   presc.daily.dose.colname,
                   total.dose.colname)
  )

  setnames(events_output_prescriptions,
           old = c("ID",
                   "DAILY.DOSE"),
           new = c(ID.colname,
                   presc.daily.dose.colname)
  )
if(progress.bar == TRUE)  close(pb)

  if( !return.data.table )
  {
    events_output_durations <- as.data.frame(events_output_durations);
    events_output_prescriptions <- as.data.frame(events_output_prescriptions)
  }

  # only return special periods for selected patients
  if(!is.null(special.periods.data)) {
    special.periods.data <- special.periods.data[get(ID.colname) %in% disp_presc_IDs]
  }

  # set order of column names
  opt_cols <- c("SPECIAL.DURATION","tot.presc.interruptions","tot.dosage.changes","CARRYOVER.DURATION")
  opt_cols <- opt_cols[opt_cols %in% names(events_output_durations)]

  colorder <- c(ID.colname,
                disp.date.colname,
                medication.class.colnames,
                total.dose.colname,
                presc.daily.dose.colname,
                "DISP.START",
                "DURATION",
                "episode.start",
                "episode.end",
                opt_cols)

  setcolorder(events_output_durations, colorder)

  summary <- "Event durations based on dispensing, prescription, and other data, which can be used with the CMA constructors in AdhereR."

  list("event_durations" = events_output_durations,
       "prescription_episodes" = events_output_prescriptions,
       "special_periods" = special.periods.data,
       "ID.colname" = ID.colname,
       "presc.date.colname" = presc.date.colname,
       "disp.date.colname" = disp.date.colname,
       "medication.class.colnames" = medication.class.colnames,
       "total.dose.colname" = total.dose.colname,
       "presc.daily.dose.colname"  = presc.daily.dose.colname,
       "presc.duration.colname" = presc.duration.colname,
       "visit.colname"  = visit.colname,
       "split.on.dosage.change" = split.on.dosage.change,
       "force.init.presc" = force.init.presc,
       "force.presc.renew" = force.presc.renew,
       "trt.interruption" = trt.interruption,
       "special.periods.method" = special.periods.method,
       "date.format" = date.format);

}

############ function to prune event durations

#' Prune event durations.
#'
#' Flags or removes leftover supply durations after dosage changes, the end of a special period,
#' or treatment interruption.
#' The function accepts the raw list output of \code{compute_event_durations} and additional arguments
#' to specify event durations that need to be removed.
#'
#' Dosage changes, special periods, and treatment interruptions may lead to overestimation of
#' implementation, e.g. if patients get a refill after discharge from hospital and don't continue to use
#' their previous supply. Likewise, it may also lead to overestimation of persistence, e.g. when
#' patients discontinue treatments after the end of a special period or treatment interruption.
#'
#' @param data A \emph{\code{list}}, the output of `compute_event_durations`.
#' @param include A \emph{\code{Vector}} of \emph{strings} indicating whether to include
#' dosage changes, special periods, and/or treatment interruptions.
#' @param medication.class.colnames A \emph{\code{Vector}} of \emph{strings}, the
#' name(s) of the column(s) in the \code{event_durations} element of \code{data} to
#' identify medication classes. Defaults to the columns used in \code{compute_event_durations}.
#' @param days.within.out.date.1 event durations from before the dosage change, special period, or
#' treatment interruptions are removed if there is a new dispensing event within the
#' number of days specified as \emph{integer} after the dosage change or end of the special
#' period/treatment interruption.
#' @param days.within.out.date.2 event durations from before dosage change, special period,
#' or treatment interruption are removed if there is \emph{NO} new dispensing event within the
#' number of days specified as \emph{integer} after the dosage change or end of the special
#' period/treatment interruption.
#' @param keep.all \emph{Logical}, should events be kept and marked for removal?
#' If \code{TRUE}, a new column \code{.prune.event} will be added to \code{event_durations},
#' if \code{FALSE} the events will be removed from the output.
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any
#' warnings.
#' @param return.data.table \emph{Logical}, if \code{TRUE} return a
#' \code{data.table} object, otherwise a \code{data.frame}.
#' @param ... other possible parameters.
#' @return A \code{data.frame} or \code{data.table}, the pruned event_durations.
#' @examples
#' # select medication class of interest and compute event durations
#' event_durations_list <- compute_event_durations(disp.data = durcomp.dispensing[ID == 3 & grepl("J01EE01", ATC.CODE)],
#'                                                 presc.data = durcomp.prescribing[ID == 3 & grepl("J01EE01", ATC.CODE)],
#'                                                 special.periods.data = durcomp.hospitalisation[ID == 3],
#'                                                 ID.colname = "ID",
#'                                                 presc.date.colname = "DATE.PRESC",
#'                                                 disp.date.colname = "DATE.DISP",
#'                                                 date.format = "%Y-%m-%d",
#'                                                 medication.class.colnames = c("ATC.CODE","UNIT", "FORM"),
#'                                                 total.dose.colname = "TOTAL.DOSE",
#'                                                 presc.daily.dose.colname = "DAILY.DOSE",
#'                                                 presc.duration.colname = "PRESC.DURATION",
#'                                                 visit.colname = "VISIT",
#'                                                 force.init.presc = TRUE,
#'                                                 force.presc.renew = TRUE,
#'                                                 split.on.dosage.change = TRUE,
#'                                                 trt.interruption = "carryover",
#'                                                 special.periods.method = "carryover",
#'                                                 suppress.warnings = FALSE,
#'                                                 return.data.table = TRUE,
#'                                                 progress.bar = FALSE)
#'
#' event_durations <- prune_event_durations(event_durations_list,
#'                                          include = c("special periods"), # only consider special periods
#'                                          medication.class.colnames = "ATC.CODE",
#'                                          days.within.out.date.1 = 7, # flag carryover durations if there are new events within 7 days after the end of special periods
#'                                          days.within.out.date.2 = 30, # flag carryover durations if there are no new events within 30 days after the end of special periods
#'                                          keep.all = FALSE) # remove flagged events from dataset
#' @export
prune_event_durations <- function(data,
                                  include = c("special periods", "treatment interruptions", "dosage changes"),
                                  medication.class.colnames = data$medication.class.colnames,
                                  days.within.out.date.1,
                                  days.within.out.date.2,
                                  keep.all = TRUE,
                                  suppress.warnings = FALSE,
                                  return.data.table = FALSE,
                                  ...){

  ## Preconditions

  # data class and dimensions
  if( !inherits(data, "list") )
  {
    if( !suppress.warnings ) warning("The data must be a of type 'list'!\n");
    return (NULL);
  }

  if( !inherits(data$event_durations, "data.frame") )
  {
    if( !suppress.warnings ) warning("The event_durations element in data must be of type 'data.frame'!\n");
    return (NULL);
  }
  if( nrow(data$event_durations) < 1 )
  {
    if( !suppress.warnings ) warning("The event_durations element in data must have at least one row!\n");
    return (NULL);
  }

  if( !inherits(data$prescription_episodes, "data.frame") )
  {
    if( !suppress.warnings ) warning("The prescription_episodes element in data must be of type 'data.frame'!\n");
    return (NULL);
  }
  if( nrow(data$prescription_episodes) < 1 )
  {
    if( !suppress.warnings ) warning("The prescription_episodes element in data must have at least one row!\n");
    return (NULL);
  }

  if( !inherits(data$special_periods, "data.frame") )
    {
      if( !suppress.warnings ) warning("The special_periods element in data must be of type 'data.frame'!\n");
      return (NULL);
    }
  if( nrow(data$special_periods) < 1 )
    {
      if( !suppress.warnings ) warning("The special_periods element in data must have at least one row!\n");
      return (NULL);
    }
  if(!all(c(data$ID.colname, "DATE.IN", "DATE.OUT") %in% colnames(data$special_periods)))
    {
      if( !suppress.warnings ) warning(paste0("The special_periods element in data must contain at least all columns with the names '", data$ID.colname, "', 'DATE.IN', and 'DATE.OUT'.\n Please refer to the documentation for more information.\n"));
      return (NULL);
    }

  # include parameter valid

  if(any(!include %in% c("special periods", "treatment interruptions", "dosage changes")))
  {
    if( !suppress.warnings ) warning("The included elements in include = '", include, "' can only be 'special periods', 'treatment interruptions', and/or 'dosage changes'\n");    return (NULL);
  }

  # days.within.out.date parameters valid
  if( is.na(days.within.out.date.1) || is.null(days.within.out.date.1) || length(days.within.out.date.1) != 1 || !is.numeric(days.within.out.date.1) || days.within.out.date.1 < 0 )
  {
    if( !suppress.warnings ) warning("The 'days.within.out.date.1' argument must be a positive number of days after a special period!\n")
    return (NULL);
  }
  if( is.na(days.within.out.date.2) || is.null(days.within.out.date.2) || length(days.within.out.date.2) != 1 || !is.numeric(days.within.out.date.2) || days.within.out.date.2 < 0 )
  {
    if( !suppress.warnings ) warning("The 'days.within.out.date.2' argument must be a positive number of days after a special period!\n")
    return (NULL);
  }

  # extract data from output list
  event_durations <- data$event_durations

  # medication class colnames in dataset
  if( any(!is.na(medication.class.colnames) & !(medication.class.colnames %in% names(event_durations))) ) # deal with the possibility of multiple column names
  {
    if( !suppress.warnings ) warning(paste0("Column(s) medication.class.colnames=",paste0("'",medication.class.colnames,"'",collapse=",")," must appear in the event_durations data!\n"));
    return (NULL);
  }




  if(".prune.event" %in% colnames(event_durations)) {
    event_durations[,.prune.event := NULL]
  }

  end_dates <- NULL

  if("special periods" %in% include){
    special_periods <- data$special_periods

    # extract end dates
    end_dates <- unique(special_periods[,c(data$ID.colname, "DATE.OUT"), with = FALSE])

    # add medication classes
    unique_med <- unique(event_durations[,c(data$ID.colname, medication.class.colnames), with = FALSE])

    end_dates <- merge(end_dates, unique_med, by = data$ID.colname, allow.cartesian = TRUE)
    setnames(end_dates, old = c(data$ID.colname), new = "ID")
  }

  if("treatment interruptions" %in% include){
    presc_episodes <- data$prescription_episodes
    trt_interruptions <- presc_episodes[shift(episode.end, n = 1, type = "lag") < episode.start, .SD, by = c(data$ID.colname, medication.class.colnames)]
    trt_interruptions <- trt_interruptions[,c(data$ID.colname, "episode.start", medication.class.colnames), with = FALSE]
    setnames(trt_interruptions, old = c(data$ID.colname, "episode.start"), new = c("ID", "DATE.OUT"))

    # extract end dates
    end_dates <- unique(rbind(end_dates,
                              trt_interruptions))
  }
  if("dosage changes" %in% include) {
    presc_episodes <- data$prescription_episodes
    dosage_changes <- presc_episodes[shift(episode.start, n = 1, type = "lead") == episode.end, .SD, by = c(data$ID.colname, medication.class.colnames)]
    dosage_changes <- dosage_changes[,c(data$ID.colname, "episode.start", medication.class.colnames), with = FALSE]
    setnames(dosage_changes, old = c(data$ID.colname, "episode.start"), new = c("ID", "DATE.OUT"))

    # extract end dates
    end_dates <- unique(rbind(end_dates,
                              dosage_changes))

  }

  # create new variable for join date
  event_durations[, join_date := DISP.START]
  end_dates[, join_date := DATE.OUT]

  # key by ID, medication class, and join date
  setkeyv(event_durations, cols = c(data$ID.colname, medication.class.colnames, "join_date"))
  setkeyv(end_dates, cols = c(data$ID.colname, medication.class.colnames, "join_date"))

  # identify events to remove from the event_durations dataset
  disp.remove.1 <- NULL
  if(!is.na(days.within.out.date.1)) {

    # rolling join to select events starting within the specified number of days after the end date of special periods

    #if(is.numeric(days.within.out.date.1)){

      disp.within.1 <- na.omit(end_dates[event_durations, roll = days.within.out.date.1], cols = c("DURATION", "DATE.OUT", data$disp.date.colname))

    # } else {
    #
    #   disp.within.1 <- na.omit(end_dates[event_durations, roll = get(days.within.out.date.1)], cols = c("DURATION", "DATE.OUT", data$disp.date.colname))
    #
    # }

    # identify durations from previous events
    disp.within.1[get(data$disp.date.colname) < DATE.OUT & get(data$disp.date.colname) < DISP.START, .from.carryover := 1]

    # identify new events
    disp.within.1[, .new.events :=  .N - sum(.from.carryover, na.rm = TRUE), by = c(data$ID.colname, medication.class.colnames, "DATE.OUT")]

    # mark events for removal if they are from previous events and at least one new event is present within the specified period
    disp.remove.1 <- disp.within.1[.from.carryover == 1 & .new.events > 0, .prune.event := 1]

    disp.remove.1 <- disp.remove.1[.prune.event == 1]

  }

  disp.remove.2 <- NULL
  if(!is.na(days.within.out.date.2)) {

    # rolling join to select events starting within the specified number of days after the end date of special periods

    #if(is.numeric(days.within.out.date.2)){

      disp.within.2 <- na.omit(end_dates[event_durations, roll = days.within.out.date.2], cols = c("DURATION", "DATE.OUT", data$disp.date.colname))

    # } else {
    #
    #   disp.within.2 <- na.omit(end_dates[event_durations, roll = get(days.within.out.date.2)], cols = c("DURATION",
    #                                                                                                     "DATE.OUT",
    #                                                                                                     data$disp.date.colname))
    # }

    # identify durations from previous events
    disp.within.2[get(data$disp.date.colname) < DATE.OUT & get(data$disp.date.colname) < DISP.START, .from.carryover := 1]

    # identify new events
    disp.within.2[, .new.events :=  .N - sum(.from.carryover, na.rm = TRUE), by = c(data$ID.colname, medication.class.colnames, "DATE.OUT")]

    # mark events for removal if they are from previous and no new events are present
    disp.remove.2 <- disp.within.2[.from.carryover == 1 & .new.events == 0, .prune.event := 1]

    # in case of multiple new durations from the same dispensing event, mark previous new durations according to last new duration
    disp.remove.2[.from.carryover == 1,.prune.event := last(.prune.event), by = c(data$disp.date.colname)]

    disp.remove.2 <- disp.remove.2[.prune.event == 1]

  }

  # compine events to remove
  disp.remove <- rbind(disp.remove.1, disp.remove.2)

  # merge with event_durations
  event_durations_prune <- merge(event_durations[, join_date := NULL], disp.remove[, c(data$ID.colname, medication.class.colnames, data$disp.date.colname, "DISP.START", "DURATION", ".prune.event"), with = FALSE], by = c(data$ID.colname, medication.class.colnames, data$disp.date.colname, "DISP.START", "DURATION"), all.x = TRUE)

  # set order of column names
  opt_cols <- c("SPECIAL.DURATION",
                "tot.presc.interruptions",
                "tot.dosage.changes",
                "CARRYOVER.DURATION",
                ".prune.event")
  opt_cols <- opt_cols[opt_cols %in% names(event_durations_prune)]

  colorder <- c(data$ID.colname,
                data$disp.date.colname,
                data$medication.class.colnames,
                data$total.dose.colname,
                data$presc.daily.dose.colname,
                "DISP.START",
                "DURATION",
                "episode.start",
                "episode.end",
                opt_cols)

  setcolorder(event_durations_prune, colorder)

  if(keep.all == FALSE) {

    output  <- event_durations_prune[is.na(.prune.event)]

    output[, .prune.event := NULL]

  } else { output <- event_durations_prune }

  return(output)

}

############ function to consider special periods as covered

#' Cover special periods.
#'
#' Identifies special periods that are in proximity to already covered durations and adds additional
#' events for these durations.
#'
#' Special periods may appear as gaps, possibly leading to underestimation of implementation or even
#' assumption of discontinuation and non-persistence. To consider such periods as covered, this function
#' adds additional durations, for example when it is assumed that hospitalized patients are adherent
#' during the hospitalization period. This function should be used after pruning with
#' \code{prune_event_durations}.
#'
#' @param events.data A \emph{\code{data.frame}} or \emph{\code{data.table}} with the event durations.
#' @param special.periods.data a \emph{\code{data.frame}} or or \emph{\code{data.table}}
#' containing the information about special periods (e.g., hospitalizations or other situations
#' where medication use may differ, e.g. during incarcerations or holidays). Must contain the same unique
#' patient ID as dispensing and prescription data, the start and end dates of the special
#' periods with the exact column names \emph{\code{DATE.IN}} and \emph{\code{DATE.OUT}}.
#' @param ID.colname A \emph{string}, the name of the column in \code{disp.data},
#' \code{presc.data}, and \code{special.periods.data} containing the unique patient ID.
#' @param disp.start.colname A \emph{string}, the name of the column in
#' \code{events.data} containing the event start date (in the format given in
#' the \code{date.format} parameter).
#' @param duration.colname A \emph{string}, the name of the column in
#' \code{events.data} containing the duration of the medication event.
#' @param medication.class.colnames A \emph{\code{Vector}} of \emph{strings}, the
#' name(s) of the column(s) in the \code{event_durations} element of \code{data} to
#' identify medication classes. Defaults to the columns used in \code{compute_event_durations}.
#' @param days.before an \emph{integer}, the number of days before the start of a special period
#' within which an event duration must end to consider the special period as covered.
#' @param days.after an \emph{integer}, the number of days after a special period within
#' which an event duration must start to consider the special period as covered.
#' @param date.format A \emph{string} giving the format of the dates used in
#' the \code{data} and the other parameters; see the \code{format} parameters
#' of the \code{\link[base]{as.Date}} function for details (NB, this concerns
#' only the dates given as strings and not as \code{Date} objects).
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any
#' warnings.
#' @param return.data.table \emph{Logical}, if \code{TRUE} return a
#' \code{data.table} object, otherwise a \code{data.frame}.
#' @param ... other possible parameters.
#' @return A \code{data.frame} or \code{data.table}, the \code{events.data} with the additional
#' durations for special periods covered.
#' @examples
#' # select medication class of interest and compute event durations
#' event_durations_list <- compute_event_durations(disp.data = durcomp.dispensing[ID == 3 & grepl("J01EE01", ATC.CODE)],
#'                                                 presc.data = durcomp.prescribing[ID == 3 & grepl("J01EE01", ATC.CODE)],
#'                                                 special.periods.data = durcomp.hospitalisation[ID == 3],
#'                                                 special.periods.method = "carryover",
#'                                                 ID.colname = "ID",
#'                                                 presc.date.colname = "DATE.PRESC",
#'                                                 disp.date.colname = "DATE.DISP",
#'                                                 date.format = "%Y-%m-%d",
#'                                                 medication.class.colnames = c("ATC.CODE","UNIT", "FORM"),
#'                                                 total.dose.colname = "TOTAL.DOSE",
#'                                                 presc.daily.dose.colname = "DAILY.DOSE",
#'                                                 presc.duration.colname = "PRESC.DURATION",
#'                                                 visit.colname = "VISIT",
#'                                                 force.init.presc = TRUE,
#'                                                 force.presc.renew = TRUE,
#'                                                 split.on.dosage.change = TRUE,
#'                                                 trt.interruption = "carryover",
#'                                                 suppress.warnings = FALSE,
#'                                                 return.data.table = TRUE,
#'                                                 progress.bar = FALSE)
#'
#' event_durations <- prune_event_durations(event_durations_list,
#'                                          include = c("special periods"), # only consider special periods
#'                                          medication.class.colnames = "ATC.CODE",
#'                                          days.within.out.date.1 = 7, # flag carryover durations if there are new events within 7 days after the end of special periods
#'                                          days.within.out.date.2 = 30, # flag carryover durations if there are no new events within 30 days after the end of special periods
#'                                          keep.all = FALSE) # remove flagged events from dataset
#'
#' # cover special periods
#' event_durations_covered <- cover_special_periods(events.data = event_durations,
#'                                                  special.periods.data = event_durations_list$special_periods,
#'                                                  ID.colname = "ID",
#'                                                  disp.start.colname = "DISP.START",
#'                                                  duration.colname = "DURATION",
#'                                                  medication.class.colnames = "ATC.CODE",
#'                                                  days.before = 7,
#'                                                  days.after = 7,
#'                                                  date.format = "%Y-%m-%d")
#' @export
cover_special_periods <- function(events.data,
                                  special.periods.data,
                                  ID.colname,
                                  disp.start.colname,
                                  duration.colname,
                                  medication.class.colnames,
                                  days.before,
                                  days.after,
                                  date.format,
                                  suppress.warnings = FALSE,
                                  return.data.table = FALSE,
                                  ...){

  # Preconditions
  # events.data class and dimensions:
  if( inherits(events.data, "matrix") ) events.data <- as.data.table(events.data); # convert matrix to data.table
  if( !inherits(events.data, "data.frame") )
  {
    if( !suppress.warnings ) warning("The events data must be of type 'data.frame'!\n");
    return (NULL);
  }
  if( nrow(events.data) < 1 )
  {
    if( !suppress.warnings ) warning("The events data must have at least one row!\n");
    return (NULL);
  }

  # special period data class and dimensions:
  if( inherits(special.periods.data, "matrix") ) special.periods.data <- as.data.table(special.periods.data); # convert matrix to data.table
  if( !inherits(special.periods.data, "data.frame") )
  {
    if( !suppress.warnings ) warning("The special periods data must be of type 'data.frame'!\n");
    return (NULL);
  }
  if( nrow(special.periods.data) < 1 )
  {
    if( !suppress.warnings ) warning("The special periods data must have at least one row!\n");
    return (NULL);
  }
  if(!all(c(ID.colname, "DATE.IN", "DATE.OUT") %in% colnames(special.periods.data)))
  {
    if( !suppress.warnings ) warning(paste0("The special periods data must contain at least all
                                            columns with the names '", ID.colname, "', 'DATE.IN', and 'DATE.OUT'.\n
                                            Please refer to the documentation for more information.\n"));
    return (NULL);
  }

  # the column names must exist in dispensing and prescription data:
  if( !is.na(ID.colname) && !(ID.colname %in% names(events.data)) )
  {
    if( !suppress.warnings ) warning(paste0("Column ID.colname='",ID.colname,"' must appear in the events data!\n"));
    return (NULL);
  }

  if( !is.na(disp.start.colname) && !(disp.start.colname %in% names(events.data)) )
  {
    if( !suppress.warnings ) warning(paste0("Column disp.start.colname='",disp.start.colname,"' must appear in the events data!\n"));
    return (NULL);
  }
  if(anyNA(events.data[[disp.start.colname]])){
    if( !suppress.warnings ) warning(paste0("Column disp.start.colname='",disp.start.colname,"' cannot contain missing values!\n"));
    return (NULL);
  }

  if( any(!is.na(medication.class.colnames) & !(medication.class.colnames %in% names(events.data)) ) ) # deal with the possibility of multiple column names
  {
    if( !suppress.warnings ) warning(paste0("Column(s) medication.class.colnames=",paste0("'",medication.class.colnames,"'",collapse=",")," must appear in the events data!\n"));
    return (NULL);
  }

  if( !is.na(duration.colname) && !(duration.colname %in% names(events.data)) )
  {
    if( !suppress.warnings ) warning(paste0("Column duration.colname='",duration.colname,"' must appear in the events data!\n"));
    return (NULL);
  }

  # days before and after
  if( is.na(days.before) || is.null(days.before) || length(days.before) != 1 || !is.numeric(days.before) || days.before < 0 )
  {
    if( !suppress.warnings ) warning("The 'days.before' argument must be a positive number of days before a special period!\n")
    return (NULL);
  }
  if( is.na(days.after) || is.null(days.after) || length(days.after) != 1 || !is.numeric(days.after) || days.after < 0 )
  {
    if( !suppress.warnings ) warning("The 'days.after' argument must be a positive number of days after a special period!\n")
    return (NULL);
  }

  if( is.na(date.format) || is.null(date.format) || length(date.format) != 1 || !is.character(date.format) )
  {
    if( !suppress.warnings ) warning(paste0("The date format must be a single string!\n"));
    return (NULL);
  }

  events.data <- copy(events.data)
  special.periods.data <- copy(special.periods.data)

  setnames(events.data,
           old = c(ID.colname, disp.start.colname, duration.colname),
           new = c("ID", "DISP.START", "DURATION"))

  setnames(special.periods.data,
           old = c(ID.colname),
           new = c("ID"))

  # set join date to the beginning of special durations
  events.data[, join_date := DISP.START+DURATION]
  special.periods.data[, join_date := DATE.IN]

  # key by ID and join date
  setkeyv(events.data, cols = c("ID", "join_date"))
  setkeyv(special.periods.data, cols = c("ID", "join_date"))

  # select durations ending within x days before the start of a special period
  dt1 <- na.omit(special.periods.data[events.data, roll = -days.before], cols = "DATE.IN")
  dt1 <- dt1[,c("ID", "DATE.IN", "DATE.OUT", medication.class.colnames, "SPECIAL.DURATION" #, events.data_list$presc.daily.dose.colname
  ), with = FALSE] # only keep necessary columns

  # set join date to the end of special durations
  events.data[, join_date := DISP.START]
  special.periods.data[, join_date := DATE.OUT]

  # key by ID and join date
  setkeyv(events.data, cols = c("ID", "join_date"))
  setkeyv(special.periods.data, cols = c("ID", "join_date"))

  # select durations beginning within 7 days after the end of a special period
  dt2 <- na.omit(special.periods.data[events.data, roll = days.after], cols = "DATE.OUT")
  dt2 <- dt2[,c("ID", "DATE.IN", "DATE.OUT", medication.class.colnames, "SPECIAL.DURATION"), with = FALSE] # only keep necessary columns

  # merge dt1 and dt2 and select unique rows
  dt_merge <- unique(merge(dt1,
                           dt2,
                           all=FALSE,
                           by = c("ID", "DATE.IN", "DATE.OUT", "SPECIAL.DURATION", medication.class.colnames)))


  # change column names
  setnames(dt_merge,
           old = c("DATE.IN", "SPECIAL.DURATION"),
           new = c("DISP.START", "DURATION"))

  events.data <- rbind(events.data, dt_merge, fill = TRUE)

  events.data[,`:=`(join_date = NULL,
                    DATE.OUT = NULL)]

  # change back to original column names
  setnames(events.data,
           old = c("ID", "DISP.START", "DURATION"),
           new = c(ID.colname, disp.start.colname, duration.colname))

  return(events.data)

}

############ function to compute time to initiation

#' Computation of initiation times.
#'
#' Computes the time between the start of a prescription episode and the first dispensing
#' event for each medication class.
#'
#' The period between the start of a prescription episode and the first dose administration
#' may impact health outcomes differently than omitting doses once on treatment or
#' interrupting medication for longer periods of time. Primary non-adherence (not
#' acquiring the first prescription) or delayed initiation may have a negative
#' impact on health outcomes. The function \code{time_to_initiation} calculates
#' the time between the start of a prescription episode and the first dispensing event, taking
#' into account multiple variables to differentiate between treatments.
#'
#' @param presc.data A \emph{\code{data.frame}} or \emph{\code{data.table}} containing
#' the prescription episodes. Must contain, at a minimum, the patient unique ID,
#' one medication identifier, and the start date of the prescription episode, and might
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
#'  \item \code{episode.start} the date of the first prescription event.
#'  \item \code{first.disp} the date of the first dispensing event.
#'  \item \code{time.to.initiation} the difference in days between the first
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

    if( any(!is.na(medication.class.colnames) & !(medication.class.colnames %in% names(disp.data)) & !(medication.class.colnames %in% names(presc.data))) ) # deal with the possibility of multiple column names
    {
      if( !suppress.warnings ) warning(paste0("Column(s) medication.class.colnames=",paste0("'",medication.class.colnames,"'",collapse=",")," must appear in the dispensing and prescribing data!\n"));
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

  presc.data <- presc.data[,c(ID.colname, presc.start.colname, medication.class.colnames), with = FALSE]
  disp.data <- disp.data[,c(ID.colname, disp.date.colname, medication.class.colnames), with = FALSE]

  # convert dates
  presc.data[,(presc.start.colname) := as.Date(get(presc.start.colname), format = date.format)];
  disp.data[,(disp.date.colname) := as.Date(get(disp.date.colname), format = date.format)];

  # set join date to the beginning of special durations
  presc.data[, join_date := get(presc.start.colname)]
  disp.data[, join_date := get(disp.date.colname)]

  # key by ID and join date
  setkeyv(presc.data, cols = c(ID.colname, medication.class.colnames, "join_date"))
  setkeyv(disp.data, cols = c(ID.colname, medication.class.colnames, "join_date"))

  # rolling join first dispensing event for each prescription episode
  dt_t2i <- disp.data[presc.data, roll = -Inf]

  setnames(dt_t2i,
           old = c(disp.date.colname, presc.start.colname),
           new = c("first.disp", "episode.start"))

  dt_t2i <- dt_t2i[,c(ID.colname, medication.class.colnames, "episode.start", "first.disp"),
                   with = FALSE]

  # calculate time to initiation
  dt_t2i[,time.to.initiation := as.numeric(first.disp-episode.start)];

  # key by ID, medication class, and dispensing date
  setkeyv(dt_t2i, cols = c(ID.colname, medication.class.colnames, "first.disp"));

  if( !return.data.table )
  {
    return (as.data.frame(dt_t2i));
  } else
  {
    return(dt_t2i);
  }
}