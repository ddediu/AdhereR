#function to construct treatment episodes from dispensing and prescription databases

process_medication_events <- function(DISP.DATA = disp_events,
                                        PRESC.DATA = presc_events,
                                        HOSP.DATA = hosp_events,
                                        ID.var = "ID",
                                        DATE.PRESC.var = "DATE.PRESC",
                                        DATE.DISP.var = "DATE.DISP",
                                        DATE.format = "%d.%m.%Y",
                                        CATEGORY.var = "DCI",
                                        TOTAL.DOSE.var = "TOTAL.DOSE",
                                        PERDAY.var = "DAILY.DOSE",
                                        PRESC.DURATION.var = "RYTHM.3",
                                        UNIT.var = "Unit",
                                        FORM.var = "Form",
                                        VISIT.var = "VISIT",
                                        force.init.presc = TRUE,
                                        force.presc.renew = TRUE,
                                        consider.dosage.change = TRUE){

  # Preconditions:

    # dispensing data class and dimensions:
    if( inherits(DISP.DATA, "matrix") ) DISP.DATA <- as.data.table(DISP.DATA); # convert matrix to data.table
    if( !inherits(DISP.DATA, "data.frame") )
    {
      if( !suppress.warnings ) warning("The dispensing data must be of type 'data.frame'!\n");
      return (NULL);
    }
    if( nrow(DISP.DATA) < 1 )
    {
      if( !suppress.warnings ) warning("The dispensing data must have at least one row!\n");
      return (NULL);
    }
    # prescribing data class and dimensions:
    if( inherits(PRESC.DATA, "matrix") ) PRESC.DATA <- as.data.table(PRESC.DATA); # convert matrix to data.table
    if( !inherits(PRESC.DATA, "data.frame") )
    {
      if( !suppress.warnings ) warning("The prescribing data must be of type 'data.frame'!\n");
      return (NULL);
    }
    if( nrow(PRESC.DATA) < 1 )
    {
      if( !suppress.warnings ) warning("The prescribing data must have at least one row!\n");
      return (NULL);
    }
    # hospitalization data class and dimensions:
    if(!is.null(HOSP.DATA)){
      if( inherits(HOSP.DATA, "matrix") ) HOSP.DATA <- as.data.table(HOSP.DATA); # convert matrix to data.table
      if( !inherits(HOSP.DATA, "data.frame") )
      {
        if( !suppress.warnings ) warning("The hospitalisation data must be of type 'data.frame'!\n");
        return (NULL);
      }
      if( nrow(HOSP.DATA) < 1 )
      {
        if( !suppress.warnings ) warning("The hospitalisation data must have at least one row!\n");
        return (NULL);
      }
    }

    # the column names must exist in dispensing and prescription data:
    if( !is.na(ID.var) && !(ID.var %in% names(DISP.DATA)) && !(ID.var %in% names(PRESC.DATA)))
    {
      if( !suppress.warnings ) warning(paste0("Column ID.var='",ID.var,"' must appear in the dispensing and prescribing data!\n"));
      return (NULL);
    }
    if( !is.na(DATE.PRESC.var) && !(DATE.PRESC.var %in% names(PRESC.DATA)) )
    {
      if( !suppress.warnings ) warning(paste0("Column DATE.PRESC.var='",DATE.PRESC.var,"' must appear in the prescribing data!\n"));
      return (NULL);
    }
    if( !is.na(DATE.DISP.var) && !(DATE.DISP.var %in% names(DISP.DATA)) )
    {
      if( !suppress.warnings ) warning(paste0("Column DATE.DISP.var='",DATE.DISP.var,"' must appear in the dispensing data!\n"));
      return (NULL);
    }
    if( !is.na(CATEGORY.var) && !(CATEGORY.var %in% names(DISP.DATA))  && !(CATEGORY.var %in% names(PRESC.DATA)) )
    {
      if( !suppress.warnings ) warning(paste0("Column CATEGORY.var='",CATEGORY.var,"' must appear in the dispensing and prescribing data!\n"));
      return (NULL);
    }
    if( !is.na(TOTAL.DOSE.var) && !(TOTAL.DOSE.var %in% names(DISP.DATA)) )
    {
      if( !suppress.warnings ) warning(paste0("Column TOTAL.DOSE.var='",TOTAL.DOSE.var,"' must appear in the dispensing data!\n"));
      return (NULL);
    }
    if( !is.na(PERDAY.var) && !(PERDAY.var %in% names(PRESC.DATA)) )
    {
      if( !suppress.warnings ) warning(paste0("Column PERDAY.var='",PERDAY.var,"' must appear in the prescribing data!\n"));
      return (NULL);
    }
    if( !is.na(PRESC.DURATION.var) && !(PRESC.DURATION.var %in% names(PRESC.DATA)) )
    {
      if( !suppress.warnings ) warning(paste0("Column PRESC.DURATION.var='",PRESC.DURATION.var,"' must appear in the prescribing data!\n"));
      return (NULL);
    }
    if( !is.na(UNIT.var) && !(UNIT.var %in% names(DISP.DATA)) && !(UNIT.var %in% names(PRESC.DATA)) )
    {
      if( !suppress.warnings ) warning(paste0("Column UNIT.var='",UNIT.var,"' must appear in the dispensing and prescribing data!\n"));
      return (NULL);
    }
    if( !is.na(FORM.var) && !(FORM.var %in% names(DISP.DATA)) && !(FORM.var %in% names(PRESC.DATA)) )
    {
      if( !suppress.warnings ) warning(paste0("Column FORM.var='",FORM.var,"' must appear in the dispensing and prescribing data!\n"));
      return (NULL);
    }


  # convert dates
  DISP.DATA[,(DATE.DISP.var) := as.Date(get(DATE.DISP.var), format = DATE.format)]
  PRESC.DATA[,(DATE.PRESC.var) := as.Date(get(DATE.PRESC.var), format = DATE.format)]
  if(!is.null(HOSP.DATA)){
    HOSP.DATA[,`:=` (DATE.IN = as.Date(DATE.IN, format = DATE.format),
             DATE.OUT = as.Date(DATE.OUT, format = DATE.format))]
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
        curr_presc_date <- curr_presc[[DATE.PRESC.var]]

        # if current dispensing event is before first prescription date, don't calculate a duration
        if(curr_disp[[DATE.DISP.var]] < first_presc[[DATE.PRESC.var]]) {
          med_event <- cbind(curr_disp[,c(ID.var, DATE.DISP.var, CATEGORY.var, UNIT.var, FORM.var, TOTAL.DOSE.var), with = FALSE],
                             PRESC.DURATION = NA,
                             DAILY.DOSE = NA,
                             START.PRESC = as.Date(NA, format = DATE.format),
                             END.PRESC = as.Date(NA, format = DATE.format))

        # if current dispensing event is after end of last prescription date, don't calculate a duration (only when last prescription indicates termination)
        } else if(!is.na(tail(med_presc$END.PRESC,1)) && curr_disp[[DATE.DISP.var]] > tail(med_presc$END.PRESC,1)){
          med_event <- cbind(curr_disp[,c(ID.var, DATE.DISP.var, CATEGORY.var, UNIT.var, FORM.var, TOTAL.DOSE.var), with = FALSE],
                             PRESC.DURATION = NA,
                             DAILY.DOSE = NA,
                             START.PRESC = as.Date(NA, format = DATE.format),
                             END.PRESC = as.Date(NA, format = DATE.format))
        } else { # otherwise search last prescription before the current dispensing event

        curr_presc <- tail(med_presc[get(DATE.PRESC.var) <= curr_disp[[DATE.DISP.var]]], 1)
        curr_presc_date <- curr_presc[[DATE.PRESC.var]]

        # set current prescribed dose
        curr_presc_dose <- as.numeric(curr_presc[[PERDAY.var]])

        # calculate duration of dispensation with this dosage
        disp_duration <- curr_disp[[TOTAL.DOSE.var]]/curr_presc_dose

     ## add hospitalizations
        curr_hosp_event <- 0
        if(nrow(hosp_events) != 0 & !is.na(disp_duration) ){

          # check if end date of dispensiung duration is after start and before end of hospitalization
          curr_disp_end <- curr_disp[[DATE.DISP.var]] + disp_duration
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
          total_dose_i <- curr_disp[[TOTAL.DOSE.var]]
          date_i <- curr_disp[[DATE.DISP.var]]
          nb_dosage_change <- 0

          # while there is a dosage change within the calculated duration, recalculate duration accounting for dosage changes
          while (total_dose_i > presc_dose_i) {
            curr_dosage_change <- head(dosage_changes[dosage_changes[[DATE.PRESC.var]] > date_i &
                                                        dosage_changes[[DATE.PRESC.var]] < curr_disp[[DATE.DISP.var]] + disp_duration], 1)

            if(nrow(curr_dosage_change) > 0) {

              disp_duration_i <- as.numeric(curr_dosage_change[[1, DATE.PRESC.var]] - date_i)
              total_dose_i <- total_dose_i - disp_duration_i*presc_dose_i

              med_event <- rbind(med_event,
                                 cbind(curr_disp[,c(ID.var, DATE.DISP.var, CATEGORY.var, UNIT.var, FORM.var, TOTAL.DOSE.var), with = FALSE],
                                 data.table(DATE.PRESC = presc_date_i,
                                            DAILY.DOSE = presc_dose_i,
                                            DURATION = disp_duration_i,
                                            FIRST.PRESC = first_presc[[DATE.PRESC.var]],
                                            START.PRESC = curr_presc$START.PRESC,
                                            END.PRESC = curr_presc$END.PRESC,
                                            PRESC.DURATION = curr_presc[[PRESC.DURATION.var]],
                                            HOSP.DURATION = 0,
                                            DOSAGE.CHANGE = 1)))

              curr_presc <- tail(med_presc[START.PRESC <= (date_i + disp_duration_i)], 1)

              date_i <- curr_dosage_change[[1,DATE.PRESC.var]]
              presc_date_i <- date_i
              presc_dose_i <- curr_dosage_change[[1,PERDAY.var]]
              disp_duration <- as.numeric(curr_dosage_change[[1, DATE.PRESC.var]] - curr_disp[[DATE.DISP.var]]) + total_dose_i/presc_dose_i
              nb_dosage_change <- nb_dosage_change + 1

            } else { #if there is no further dosage change, calculate total dispensiung duration including hospitalization

              rest <- cbind(curr_disp[,c(ID.var, DATE.DISP.var, CATEGORY.var, UNIT.var, FORM.var, TOTAL.DOSE.var), with = FALSE],
                            data.table(DATE.PRESC = presc_date_i,
                                       DAILY.DOSE = presc_dose_i,
                                       DURATION = (total_dose_i/presc_dose_i) + curr_hosp_event, #add hospitalization duration to the last row
                                       FIRST.PRESC = first_presc[[DATE.PRESC.var]],
                                       START.PRESC = curr_presc$START.PRESC,
                                       END.PRESC = curr_presc$END.PRESC,
                                       PRESC.DURATION = curr_presc[[PRESC.DURATION.var]],
                                       HOSP.DURATION = curr_hosp_event,
                                       DOSAGE.CHANGE = 1))
              rest[,(TOTAL.DOSE.var) := total_dose_i]
              med_event <- rbind(med_event, rest)

              total_dose_i <- 0
            }
          }
        } else { # if there is no dosage change, return dispensing duration

          med_event <- cbind(curr_disp[,c(ID.var, DATE.DISP.var, CATEGORY.var, UNIT.var, FORM.var, TOTAL.DOSE.var), with = FALSE],
                             data.table(DATE.PRESC = curr_presc[[DATE.PRESC.var]],
                                        DAILY.DOSE = curr_presc[[PERDAY.var]],
                                        DURATION = disp_duration,
                                        FIRST.PRESC = first_presc[[DATE.PRESC.var]],
                                        START.PRESC = curr_presc$START.PRESC,
                                        END.PRESC = curr_presc$END.PRESC,
                                        PRESC.DURATION = curr_presc[[PRESC.DURATION.var]],
                                        HOSP.DURATION = curr_hosp_event,
                                        DOSAGE.CHANGE = NA
                             ))
        }
        }
        med_event
      }

  ## subset data to medication

      med_disp <- pat_disp[get(CATEGORY.var) == disp_presc[[med, CATEGORY.var]] & get(FORM.var) == disp_presc[[med, FORM.var]]]

      med_presc <- pat_presc[get(CATEGORY.var) == disp_presc[[med, CATEGORY.var]] & get(FORM.var) == disp_presc[[med, FORM.var]]]

      setkeyv(med_disp, cols = DATE.DISP.var)
      setkeyv(med_presc, cols = DATE.PRESC.var)

   ## calculate treatment interruptions and end of prescription date

   ## determine end of prescription and prescription interruptions if prescription reneval is enforced for each subsequent prescription event (requires the "visit" column)
      presc_interruptions <- data.table(NULL)
      if(force.presc.renew == TRUE){

        presc_visit <- presc_events[[VISIT.var]] %in% unique(med_presc[[VISIT.var]]) #determine for each visit if medication was prescribed

        first_presc_event <- head(which(presc_visit),1) #extract first prescription event
        last_presc_event <- tail(which(presc_visit),1) #extract last prescription event

        presc_omit <- which(!presc_visit)[which(!presc_visit) > first_presc_event & which(!presc_visit) < last_presc_event] #identify visits between first and last prescription with missing prescriptions

        interruption_dates <- presc_events[[DATE.PRESC.var]][presc_omit] #determine dates of treatment interruptions

        presc_interruptions <- med_presc[rep(1, length(presc_omit))] #create table with one row for each interruption

        presc_interruptions[, c(VISIT.var, DATE.PRESC.var, PERDAY.var, PRESC.DURATION.var) :=
                              list(presc_events[[VISIT.var]][presc_omit], interruption_dates, 0, NA)] #adjust variables

        med_presc <- rbind(med_presc, presc_interruptions) #bind to existing prescriptions
        setkeyv(med_presc, cols = DATE.PRESC.var) #order by date

        med_presc[,.episode := rleidv(med_presc, cols = PERDAY.var)] # add counter for treatment episodes

        # remove interruptions after prescriptions with limited durations
        .episodes_rm <- med_presc[shift(!is.na(get(PRESC.DURATION.var)), type = "lag") & get(PERDAY.var) == 0, .episode]
        med_presc <- med_presc[!.episode %in% .episodes_rm]
      }

   ## determine dates of dosage changes
      dosage_changes <- NULL
      if(consider.dosage.change == TRUE){
        if(nrow(med_presc) > 1 & diff(range(med_presc[[PERDAY.var]])) != 0){

    ## postpone date of dosage change to end of hospitalizations
        for(i in nrow(med_presc):2) {# go backwards to catch later dosage changes first; important for hospitalisations
          if(!identical(med_presc[[i, PERDAY.var]],med_presc[[i-1, PERDAY.var]])){# check if dosage has changed since last prescription
            if(nrow(hosp_events) != 0){# if patient has been hospitalized
              for(j in 1:nrow(hosp_events)){# check if dosage changes during hospitalizations and if yes, postpone the date of dosage change to the end of hospitalization
                if(med_presc[[i, DATE.PRESC.var]] >= hosp_events[[j, "DATE.IN"]] & med_presc[[i, DATE.PRESC.var]] <= hosp_events[[j, "DATE.OUT"]]){
                  med_presc[i, (DATE.PRESC.var) := hosp_events[[j, "DATE.OUT"]]]
                }
              }
            }
            dosage_changes <- rbind(dosage_changes, med_presc[i, c(DATE.PRESC.var, PERDAY.var, PRESC.DURATION.var), with = FALSE])
            }
        }
        # make sure to have only one dosage change for each date
        dosage_changes <- unique(dosage_changes, by = DATE.PRESC.var)
        setorderv(dosage_changes, DATE.PRESC.var) # order again by ascending date

        # do the same for the med_presc
        setorderv(med_presc, VISIT.var, order=-1)
        med_presc <- unique(med_presc, by = DATE.PRESC.var)
        setorderv(med_presc, VISIT.var, order=1) # order again by ascending date
        }
      }

   ## construct treatment episodes
      # create new .episode counter
      med_presc[,.episode := rleidv(med_presc, cols = c(PERDAY.var, PRESC.DURATION.var))]

      # determine date of initial prescription
      first_presc <- med_presc[1]

      # determine date of initial dispense
      first_disp <- med_disp[[DATE.DISP.var]][1]
      if(force.init.presc == TRUE) {
        # if initial dispense is before initial prescription, adjust date of initial prescription to match initial dispense
        # but only if first prescription is unlimited
        if(first_disp < first_presc[[DATE.PRESC.var]] & is.na(first_presc[[PRESC.DURATION.var]])) {
          first_presc[[DATE.PRESC.var]] = first_disp

          #adjust first prescription date
          med_presc[1, (DATE.PRESC.var) := first_presc[[DATE.PRESC.var]]]

          }
      }

   ## set start and end of prescription dates per group
      med_presc[, `:=` (START.PRESC = get(DATE.PRESC.var), # set prescription date as start date
                                       END.PRESC = get(DATE.PRESC.var)-1), by = .episode] # set end date to one day before prescription date ...

      med_presc[,END.PRESC := shift(END.PRESC, type = "lead")] # ... and shift end dates up by one

      # fill in start and end dates by group
      med_presc[,START.PRESC := head(START.PRESC,1), by = .episode] # first prescription date per episode
      med_presc[,END.PRESC:= tail(END.PRESC,1), by = .episode] # last prescription date per episode

      med_presc[!is.na(get(PRESC.DURATION.var)), END.PRESC := START.PRESC + get(PRESC.DURATION.var) - 1] # adjust end date if prescription duration is provided

  ## apply process_dispensing_events to each dispensing event
      medication_events <- do.call(rbindlist,
                                   list(l = lapply(1:nrow(med_disp), FUN = function(i) process_dispensing_events(event = i)),
                                        fill = TRUE))

      med_presc <- med_presc[,`:=` (.episode=NULL,VISIT=NULL)]
      setnames(med_presc,
               old = c(PERDAY.var, PRESC.DURATION.var),
               new = c("DAILY.DOSE", "PRESC.DURATION"))

      presc_episode_no_dispense <- med_presc[!medication_events[,c("DAILY.DOSE","PRESC.DURATION","START.PRESC","END.PRESC")],
                                             on = c("DAILY.DOSE","PRESC.DURATION", "START.PRESC", "END.PRESC")]

      medication_events <- rbind(medication_events, presc_episode_no_dispense, fill = TRUE)

      medication_events
    }

    #subset data to patient
    pat_disp <- DISP.DATA[get(ID.var) == pat]

    pat_presc <- PRESC.DATA[get(ID.var) == pat]

    #sort by DCI
    setkeyv(pat_disp, cols = CATEGORY.var)
    setkeyv(pat_presc, cols = CATEGORY.var)

    #extract unique dispensed/prescribed DCIs
    disp_unique <- unique(pat_disp[,c(CATEGORY.var, UNIT.var, FORM.var), with = FALSE])
    presc_unique <- unique(pat_presc[,c(CATEGORY.var, UNIT.var, FORM.var), with = FALSE])

    #extract medications present in both dispensing and prescription database (by DCI, Unit, and Form)
    disp_presc <- merge(disp_unique, presc_unique, by = c(CATEGORY.var, UNIT.var, FORM.var), all=FALSE)

    #extract unique dispensed/prescribed DCIs not present in both databases
    disp_no_presc <- disp_unique[!presc_unique]
    presc_no_disp <- presc_unique[!disp_unique]

    #extract prescription instances
    presc_events <- unique(pat_presc[,c(DATE.PRESC.var, VISIT.var), with = FALSE])

    #create visits if not supplied
    if(is.null(VISIT.var)) {
      VISIT.var <- "VISIT"
      presc_events[,VISIT := 0:(nrow(presc_events)-1)]
    }

    #if duplicate visit numbers for different dates or vice versa, throw an error
    if(length(unique(presc_events$DATE.PRESC)) != nrow(presc_events)) stop(paste("Prescription dates and visit number don't match for patient Nr.", pat))

    #extract hospitalizations
    if(!is.null(HOSP.DATA)) {
      hosp_events <- HOSP.DATA[get(ID.var) == pat]
    } else hosp_events <- data.table(NULL)

    setkeyv(presc_events, cols = DATE.PRESC.var)

    #apply process_medication() function to each medication present in both databses
    patient_events <- NULL
    if(nrow(disp_presc) != 0) {
      patient_events <- do.call(rbindlist, list(l = lapply(1:nrow(disp_presc), FUN = function(i) process_medication(med = i)),
                                                fill = TRUE))
    }

    patient_events <- rbind(patient_events,
                            pat_disp[get(CATEGORY.var) %in% disp_no_presc[[CATEGORY.var]], c(ID.var, DATE.DISP.var, CATEGORY.var, UNIT.var, FORM.var, TOTAL.DOSE.var), with = FALSE],
                            pat_presc[get(CATEGORY.var) %in% presc_no_disp[[CATEGORY.var]], c(ID.var, DATE.PRESC.var, CATEGORY.var, UNIT.var, FORM.var, PERDAY.var), with = FALSE],
                            fill = TRUE)

    patient_events

  }

  # extract IDs of all patients present in dispensing and prescription database
  disp_presc_IDs <- intersect(DISP.DATA[[ID.var]], PRESC.DATA[[ID.var]])

  # apply process_patient function
  setkeyv(DISP.DATA, cols = ID.var)
  setkeyv(PRESC.DATA, cols = ID.var)

  treatment_episodes <- do.call(rbindlist, list(l = lapply(disp_presc_IDs, FUN = function(i) process_patient(pat = i)),
                                                fill = TRUE))

  treatment_episodes
}

