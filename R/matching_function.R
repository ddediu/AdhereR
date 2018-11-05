#function to construct treatment episodes from dispensing and prescription databases

process_medication_events <- function(disp.data = disp_events,
                                        presc.data = presc_events,
                                        hosp.data = hosp_events,
                                        ID.colname = "ID",
                                        presc.date.colname = "DATE.PRESC",
                                        disp.date.colname = "DATE.DISP",
                                        date.format = "%d.%m.%Y",
                                        medication.class.colname = "DCI",
                                        total.dose.colname = "TOTAL.DOSE",
                                        presc.daily.dose.colname = "DAILY.DOSE",
                                        presc.duration.colname = "RYTHM.3",
                                        unit.colname = "Unit",
                                        form.colname = "Form",
                                        visit.colname = "VISIT",
                                        force.init.presc = TRUE,
                                        force.presc.renew = TRUE,
                                        consider.dosage.change = TRUE,
                                      suppress.warnings = FALSE){

  # Preconditions:

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
    if(!is.null(hosp.data)){
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
    if( !is.na(medication.class.colname) && !(medication.class.colname %in% names(disp.data))  && !(medication.class.colname %in% names(presc.data)) )
    {
      if( !suppress.warnings ) warning(paste0("Column medication.class.colname='",medication.class.colname,"' must appear in the dispensing and prescribing data!\n"));
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
    if( !is.na(unit.colname) && !(unit.colname %in% names(disp.data)) && !(unit.colname %in% names(presc.data)) )
    {
      if( !suppress.warnings ) warning(paste0("Column unit.colname='",unit.colname,"' must appear in the dispensing and prescribing data!\n"));
      return (NULL);
    }
    if( !is.na(form.colname) && !(form.colname %in% names(disp.data)) && !(form.colname %in% names(presc.data)) )
    {
      if( !suppress.warnings ) warning(paste0("Column form.colname='",form.colname,"' must appear in the dispensing and prescribing data!\n"));
      return (NULL);
    }
    if(".episode" %in% colnames(presc.data)){
      {
        if( !suppress.warnings ) warning("The column name \'.episode\' is used internally, please use another column name.");
        return (NULL);
      }
    }

  # convert dates
  disp.data[,(disp.date.colname) := as.Date(get(disp.date.colname), format = date.format)]
  presc.data[,(presc.date.colname) := as.Date(get(presc.date.colname), format = date.format)]
  if(!is.null(hosp.data)){
    hosp.data[,`:=` (DATE.IN = as.Date(DATE.IN, format = date.format),
             DATE.OUT = as.Date(DATE.OUT, format = date.format))]
  }

  # force medication class, unit, and form to character
  if(inherits(disp.data[[medication.class.colname]], "factor")){
    disp.data[,(medication.class.colname) := as.character(get(medication.class.colname))]
  }
  if(inherits(disp.data[[unit.colname]], "factor")){
    disp.data[,(unit.colname) := as.character(get(unit.colname))]
  }
  if(inherits(disp.data[[form.colname]], "factor")){
    disp.data[,(form.colname) := as.character(get(form.colname))]
  }

  if(inherits(presc.data[[medication.class.colname]], "factor")){
    presc.data[,(medication.class.colname) := as.character(get(medication.class.colname))]
  }
  if(inherits(presc.data[[unit.colname]], "factor")){
    presc.data[,(unit.colname) := as.character(get(unit.colname))]
  }
  if(inherits(presc.data[[form.colname]], "factor")){
    presc.data[,(form.colname) := as.character(get(form.colname))]
  }

  # function to process each patient
  process_patient <- function(pat){

    # function to process each medication
    process_medication <- function(med){

      # function to process each dispensing event
      process_dispensing_events <- function(event){

        ## !Important: We assume that the prescribed dose can be accomodated with the dispensed medication

        #identify last prescribed dosage before dispense
        curr_disp <- med_disp[event]
        curr_presc <- med_presc[1]
        curr_presc_date <- curr_presc[[presc.date.colname]]

        # if current dispensing event is before first prescription date, don't calculate a duration
        if(curr_disp[[disp.date.colname]] < first_presc[[presc.date.colname]]) {
          med_event <- cbind(curr_disp[,c(ID.colname, disp.date.colname, medication.class.colname, unit.colname, form.colname, total.dose.colname), with = FALSE],
                             PRESC.DURATION = NA,
                             DAILY.DOSE = NA,
                             START.PRESC = as.Date(NA, format = date.format),
                             END.PRESC = as.Date(NA, format = date.format))

        # if current dispensing event is after end of last prescription date, don't calculate a duration (only when last prescription indicates termination)
        } else if(!is.na(tail(med_presc$END.PRESC,1)) && curr_disp[[disp.date.colname]] > tail(med_presc$END.PRESC,1)){
          med_event <- cbind(curr_disp[,c(ID.colname, disp.date.colname, medication.class.colname, unit.colname, form.colname, total.dose.colname), with = FALSE],
                             PRESC.DURATION = NA,
                             DAILY.DOSE = NA,
                             START.PRESC = as.Date(NA, format = date.format),
                             END.PRESC = as.Date(NA, format = date.format))
        } else { # otherwise search last prescription before the current dispensing event

        curr_presc <- tail(med_presc[get(presc.date.colname) <= curr_disp[[disp.date.colname]]], 1)
        curr_presc_date <- curr_presc[[presc.date.colname]]

        # set current prescribed dose
        curr_presc_dose <- as.numeric(curr_presc[[presc.daily.dose.colname]])

        # calculate duration of dispensation with this dosage
        disp_duration <- curr_disp[[total.dose.colname]]/curr_presc_dose

     ## add hospitalizations
        curr_hosp_event <- 0
        if(nrow(hosp_events) != 0 & !is.na(disp_duration) ){

          # check if end date of dispensiung duration is after start and before end of hospitalization
          curr_disp_end <- curr_disp[[disp.date.colname]] + disp_duration
          for(i in 1:nrow(hosp_events)){
            if(curr_disp_end >= hosp_events[i, DATE.IN] & curr_disp_end <= hosp_events[i, DATE.OUT]){
              # adjust end of dispensing duration until no further hospitalization is encountered within this duration
              disp_duration <- disp_duration + as.numeric(hosp_events[i, HOSP.DURATION])
              curr_disp_end <- curr_disp_end + disp_duration
              curr_hosp_event <- curr_hosp_event + as.numeric(hosp_events[i, HOSP.DURATION])
            }
          }
        }

    ## check for dosage changes (including interruptions) within the duration of the dispensation
        if(!is.null(dosage_changes) && consider.dosage.change == TRUE){

          med_event <- NULL
          curr_dosage_change <- NULL
          presc_date_i <- curr_presc_date
          presc_dose_i <- curr_presc_dose
          total_dose_i <- curr_disp[[total.dose.colname]]
          date_i <- curr_disp[[disp.date.colname]]
          nb_dosage_change <- 0

          # while there is a dosage change within the calculated duration, recalculate duration accounting for dosage changes
          while (total_dose_i > presc_dose_i) {
            curr_dosage_change <- head(dosage_changes[dosage_changes[[presc.date.colname]] > date_i &
                                                        dosage_changes[[presc.date.colname]] < curr_disp[[disp.date.colname]] + disp_duration], 1)

            if(nrow(curr_dosage_change) > 0) {

              disp_duration_i <- as.numeric(curr_dosage_change[[1, presc.date.colname]] - date_i)
              total_dose_i <- total_dose_i - disp_duration_i*presc_dose_i

              med_event <- rbind(med_event,
                                 cbind(curr_disp[,c(ID.colname, disp.date.colname, medication.class.colname, unit.colname, form.colname, total.dose.colname), with = FALSE],
                                 data.table(DATE.PRESC = presc_date_i,
                                            DAILY.DOSE = presc_dose_i,
                                            DURATION = disp_duration_i,
                                            FIRST.PRESC = first_presc[[presc.date.colname]],
                                            START.PRESC = curr_presc$START.PRESC,
                                            END.PRESC = curr_presc$END.PRESC,
                                            PRESC.DURATION = curr_presc[[presc.duration.colname]],
                                            HOSP.DURATION = 0,
                                            DOSAGE.CHANGE = 1)))

              curr_presc <- tail(med_presc[START.PRESC <= (date_i + disp_duration_i)], 1)

              date_i <- curr_dosage_change[[1,presc.date.colname]]
              presc_date_i <- date_i
              presc_dose_i <- curr_dosage_change[[1,presc.daily.dose.colname]]
              disp_duration <- as.numeric(curr_dosage_change[[1, presc.date.colname]] - curr_disp[[disp.date.colname]]) + total_dose_i/presc_dose_i
              nb_dosage_change <- nb_dosage_change + 1

            } else { #if there is no further dosage change, calculate total dispensiung duration including hospitalization

              rest <- cbind(curr_disp[,c(ID.colname, disp.date.colname, medication.class.colname, unit.colname, form.colname, total.dose.colname), with = FALSE],
                            data.table(DATE.PRESC = presc_date_i,
                                       DAILY.DOSE = presc_dose_i,
                                       DURATION = (total_dose_i/presc_dose_i) + curr_hosp_event, #add hospitalization duration to the last row
                                       FIRST.PRESC = first_presc[[presc.date.colname]],
                                       START.PRESC = curr_presc$START.PRESC,
                                       END.PRESC = curr_presc$END.PRESC,
                                       PRESC.DURATION = curr_presc[[presc.duration.colname]],
                                       HOSP.DURATION = curr_hosp_event,
                                       DOSAGE.CHANGE = 1))
              rest[,(total.dose.colname) := total_dose_i]
              med_event <- rbind(med_event, rest)

              total_dose_i <- 0
            }
          }
        } else { # if there is no dosage change, return dispensing duration

          med_event <- cbind(curr_disp[,c(ID.colname, disp.date.colname, medication.class.colname, unit.colname, form.colname, total.dose.colname), with = FALSE],
                             data.table(DATE.PRESC = curr_presc[[presc.date.colname]],
                                        DAILY.DOSE = curr_presc[[presc.daily.dose.colname]],
                                        DURATION = disp_duration,
                                        FIRST.PRESC = first_presc[[presc.date.colname]],
                                        START.PRESC = curr_presc$START.PRESC,
                                        END.PRESC = curr_presc$END.PRESC,
                                        PRESC.DURATION = curr_presc[[presc.duration.colname]],
                                        HOSP.DURATION = curr_hosp_event,
                                        DOSAGE.CHANGE = NA
                             ))
        }
        }
        med_event
      }

  ## subset data to medication

      med_disp <- pat_disp[get(medication.class.colname) == disp_presc[[med, medication.class.colname]] & get(form.colname) == disp_presc[[med, form.colname]]]

      med_presc <- pat_presc[get(medication.class.colname) == disp_presc[[med, medication.class.colname]] & get(form.colname) == disp_presc[[med, form.colname]]]

      setkeyv(med_disp, cols = disp.date.colname)
      setkeyv(med_presc, cols = presc.date.colname)

   ## calculate treatment interruptions and end of prescription date

   ## determine end of prescription and prescription interruptions if prescription reneval is enforced for each subsequent prescription event (requires the "visit" column)
      presc_interruptions <- data.table(NULL)
      if(force.presc.renew == TRUE){

        presc_visit <- presc_events[[visit.colname]] %in% unique(med_presc[[visit.colname]]) #determine for each visit if medication was prescribed

        first_presc_event <- head(which(presc_visit),1) #extract first prescription event
        last_presc_event <- tail(which(presc_visit),1) #extract last prescription event

        presc_omit <- which(!presc_visit)[which(!presc_visit) > first_presc_event & which(!presc_visit) < last_presc_event] #identify visits between first and last prescription with missing prescriptions

        interruption_dates <- presc_events[[presc.date.colname]][presc_omit] #determine dates of treatment interruptions

        presc_interruptions <- med_presc[rep(1, length(presc_omit))] #create table with one row for each interruption

        presc_interruptions[, c(visit.colname, presc.date.colname, presc.daily.dose.colname, presc.duration.colname) :=
                              list(presc_events[[visit.colname]][presc_omit], interruption_dates, 0, NA)] #adjust variables

        med_presc <- rbind(med_presc, presc_interruptions) #bind to existing prescriptions
        setkeyv(med_presc, cols = presc.date.colname) #order by date

        med_presc[,.episode := rleidv(med_presc, cols = presc.daily.dose.colname)] # add counter for treatment episodes

        # remove interruptions after prescriptions with limited durations
        .episodes_rm <- med_presc[shift(!is.na(get(presc.duration.colname)), type = "lag") & get(presc.daily.dose.colname) == 0, .episode]
        med_presc <- med_presc[!.episode %in% .episodes_rm]
      }

   ## determine dates of dosage changes
      dosage_changes <- NULL
      if(consider.dosage.change == TRUE){
        if(nrow(med_presc) > 1 & diff(range(med_presc[[presc.daily.dose.colname]])) != 0){

    ## postpone date of dosage change to end of hospitalizations
        for(i in nrow(med_presc):2) {# go backwards to catch later dosage changes first; important for hospitalisations
          if(!identical(med_presc[[i, presc.daily.dose.colname]],med_presc[[i-1, presc.daily.dose.colname]])){# check if dosage has changed since last prescription
            if(nrow(hosp_events) != 0){# if patient has been hospitalized
              for(j in 1:nrow(hosp_events)){# check if dosage changes during hospitalizations and if yes, postpone the date of dosage change to the end of hospitalization
                if(med_presc[[i, presc.date.colname]] >= hosp_events[[j, "DATE.IN"]] & med_presc[[i, presc.date.colname]] <= hosp_events[[j, "DATE.OUT"]]){
                  med_presc[i, (presc.date.colname) := hosp_events[[j, "DATE.OUT"]]]
                }
              }
            }
            dosage_changes <- rbind(dosage_changes, med_presc[i, c(presc.date.colname, presc.daily.dose.colname, presc.duration.colname), with = FALSE])
            }
        }
        # make sure to have only one dosage change for each date
        dosage_changes <- unique(dosage_changes, by = presc.date.colname)
        setorderv(dosage_changes, presc.date.colname) # order again by ascending date

        # do the same for the med_presc
        setorderv(med_presc, visit.colname, order=-1)
        med_presc <- unique(med_presc, by = presc.date.colname)
        setorderv(med_presc, visit.colname, order=1) # order again by ascending date
        }
      }

   ## construct treatment episodes
      # create new .episode counter
      med_presc[,.episode := rleidv(med_presc, cols = c(presc.daily.dose.colname, presc.duration.colname))]

      # determine date of initial prescription
      first_presc <- med_presc[1]

      # determine date of initial dispense
      first_disp <- med_disp[[disp.date.colname]][1]
      if(force.init.presc == TRUE) {
        # if initial dispense is before initial prescription, adjust date of initial prescription to match initial dispense
        # but only if first prescription is unlimited
        if(first_disp < first_presc[[presc.date.colname]] & is.na(first_presc[[presc.duration.colname]])) {
          first_presc[[presc.date.colname]] = first_disp

          #adjust first prescription date
          med_presc[1, (presc.date.colname) := first_presc[[presc.date.colname]]]

          }
      }

   ## set start and end of prescription dates per group
      med_presc[, `:=` (START.PRESC = get(presc.date.colname), # set prescription date as start date
                                       END.PRESC = get(presc.date.colname)-1)] # set end date to one day before prescription date ...

      med_presc[,END.PRESC := shift(END.PRESC, type = "lead")] # ... and shift end dates up by one

      # fill in start and end dates by group
      med_presc[,START.PRESC := head(START.PRESC,1), by = .episode] # first prescription date per episode
      med_presc[,END.PRESC:= tail(END.PRESC,1), by = .episode] # last prescription date per episode

      med_presc[!is.na(get(presc.duration.colname)), `:=` (START.PRESC = DATE.PRESC,
                                                        END.PRESC = DATE.PRESC + get(presc.duration.colname) - 1)] # adjust end date if prescription duration is provided

  ## apply process_dispensing_events to each dispensing event
      medication_events <- do.call(rbindlist,
                                   list(l = lapply(1:nrow(med_disp), FUN = function(i) process_dispensing_events(event = i)),
                                        fill = TRUE))

      med_presc <- med_presc[,`:=` (.episode=NULL,VISIT=NULL)]
      setnames(med_presc,
               old = c(presc.daily.dose.colname, presc.duration.colname),
               new = c("DAILY.DOSE", "PRESC.DURATION"))

      presc_episode_no_dispense <- med_presc[!medication_events[,c("DAILY.DOSE","PRESC.DURATION","START.PRESC","END.PRESC")],
                                             on = c("DAILY.DOSE","PRESC.DURATION", "START.PRESC", "END.PRESC")]

      medication_events <- rbind(medication_events, presc_episode_no_dispense, fill = TRUE)

      medication_events
    }

    #subset data to patient
    pat_disp <- disp.data[get(ID.colname) == pat]

    pat_presc <- presc.data[get(ID.colname) == pat]

    #sort by DCI
    setkeyv(pat_disp, cols = medication.class.colname)
    setkeyv(pat_presc, cols = medication.class.colname)

    #extract unique dispensed/prescribed DCIs
    disp_unique <- unique(pat_disp[,c(medication.class.colname, unit.colname, form.colname), with = FALSE])
    presc_unique <- unique(pat_presc[,c(medication.class.colname, unit.colname, form.colname), with = FALSE])

    #extract medications present in both dispensing and prescription database (by DCI, Unit, and Form)
    disp_presc <- merge(disp_unique, presc_unique, by = c(medication.class.colname, unit.colname, form.colname), all=FALSE)

    #extract unique dispensed/prescribed DCIs not present in both databases
    disp_no_presc <- disp_unique[!presc_unique]
    presc_no_disp <- presc_unique[!disp_unique]

    #create visits if not supplied
    if(!visit.colname %in% colnames(presc.data)) {
      presc_events <- unique(pat_presc[,c(presc.date.colname), with = FALSE])
      presc_events[,(visit.colname) := 0:(nrow(presc_events)-1)]
      pat_presc <- merge(pat_presc, presc_events, by = presc.date.colname)
      setorderv(pat_presc, medication.class.colname)

    } else {presc_events <- unique(pat_presc[,c(presc.date.colname, visit.colname), with = FALSE])} #extract prescription instances

    #if duplicate visit numbers for different dates or vice versa, throw an error
    if(length(unique(presc_events$DATE.PRESC)) != nrow(presc_events)) {
      {
        if( !suppress.warnings ) warning("Prescription dates and visit number don't match for patient Nr.", pat);
        return (NULL);
      }
    }

    #extract hospitalizations
    if(!is.null(hosp.data)) {
      hosp_events <- hosp.data[get(ID.colname) == pat]
    } else hosp_events <- data.table(NULL)

    setkeyv(presc_events, cols = presc.date.colname)

    #apply process_medication() function to each medication present in both databses
    patient_events <- NULL
    if(nrow(disp_presc) != 0) {
      patient_events <- do.call(rbindlist, list(l = lapply(1:nrow(disp_presc), FUN = function(i) process_medication(med = i)),
                                                fill = TRUE))
    }

    patient_events <- rbind(patient_events,
                            pat_disp[get(medication.class.colname) %in% disp_no_presc[[medication.class.colname]], c(ID.colname, disp.date.colname, medication.class.colname, unit.colname, form.colname, total.dose.colname), with = FALSE],
                            pat_presc[get(medication.class.colname) %in% presc_no_disp[[medication.class.colname]], c(ID.colname, presc.date.colname, medication.class.colname, unit.colname, form.colname, presc.daily.dose.colname), with = FALSE],
                            fill = TRUE)

    patient_events

  }

  # extract IDs of all patients present in dispensing and prescription database
  disp_presc_IDs <- intersect(disp.data[[ID.colname]], presc.data[[ID.colname]])

  # apply process_patient function
  setkeyv(disp.data, cols = ID.colname)
  setkeyv(presc.data, cols = ID.colname)

  treatment_episodes <- do.call(rbindlist, list(l = lapply(disp_presc_IDs, FUN = function(i) process_patient(pat = i)),
                                                fill = TRUE))

  treatment_episodes
}

time_to_initiation <- function(data = NULL,
                               ID.colname = NA,
                               medication.class.colname = NA,
                               dispensing.date.colname = NA,
                               prescription.start.colname = NA){

  #Preconditions

  #
  data[,list(first.disp = min(get(dispensing.date.colname)),
                     time.to.initialization = as.numeric(min(get(dispensing.date.colname))-get(prescription.start.colname))),
               by = c(ID.colname, medication.class.colname, prescription.start.colname)]
}

