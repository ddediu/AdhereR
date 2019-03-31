###############################################################################################
#
#    This file is part of AdhereR.
#    Copyright (C) 2018  Samuel Allemann
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


#########################################################################################
#
#  Test matching function to combine dispensing, prescription, and hospitalization data
#
#########################################################################################
context("Matching functions")

# # Test arguments for illegal values
# test_that("dispensing data")

# Test output format
test_that("output format is correct", {
  test_results <- compute_event_durations(disp.data = durcomp.dispensing,
                                            presc.data = durcomp.prescribing,
                                            special.periods.data = durcomp.hospitalisation,
                                          special.periods.mapping = "continue",
                                            ID.colname = "ID",
                                            presc.date.colname = "DATE.PRESC",
                                            disp.date.colname = "DATE.DISP",
                                            date.format = "%Y-%m-%d",
                                            medication.class.colnames = c("ATC.CODE", "UNIT", "FORM"),
                                            total.dose.colname = "TOTAL.DOSE",
                                            presc.daily.dose.colname = "DAILY.DOSE",
                                            presc.duration.colname = "PRESC.DURATION",
                                            visit.colname = "VISIT",
                                            force.init.presc = TRUE,
                                            force.presc.renew = TRUE,
                                            split.on.dosage.change = TRUE,
                                            trt.interruption = "continue",
                                            suppress.warnings = FALSE,
                                            return.data.table = TRUE)

  expect_is(test_results, "data.table") # is a data.table
  expect_equal(names(test_results), c("ID",
                                      "ATC.CODE",
                                      "UNIT",
                                      "FORM",
                                      "TOTAL.DOSE",
                                      "DATE.DISP",
                                      "DISP.START",
                                      "DURATION",
                                      "DAILY.DOSE",
                                      "START.PRESC",
                                      "END.PRESC",
                                      "SPECIAL.DURATION",
                                      "tot.presc.interruptions",
                                      "tot.dosage.changes"))
  expect_is(test_results$ID, "numeric") # ID's are integers
  expect_is(test_results$ATC.CODE, "character") # DCIs are characters
  expect_is(test_results$UNIT, "character") # Units are characters
  expect_is(test_results$FORM, "character") # Forms are character
  expect_is(test_results$TOTAL.DOSE, "numeric") # TOTAL.DOSE is numeric
  expect_is(test_results$DATE.DISP, "Date") # DATE.DISP is a date
  expect_is(test_results$DISP.START, "Date") # DATE.DISP is a date
  expect_is(test_results$DURATION, "numeric") # DURATION is numeric
  expect_is(test_results$DAILY.DOSE, "numeric") # DAILY.DOSE is numeric
  expect_is(test_results$START.PRESC, "Date") # PRESC.START is a date
  expect_is(test_results$END.PRESC, "Date") # PRESC.END is a date
  expect_is(test_results$SPECIAL.DURATION, "numeric") # HOSP.DURATIONS are numeric
  expect_is(test_results$tot.presc.interruptions, "integer") # number of treatment interruptions are numeric
  expect_is(test_results$tot.dosage.changes, "numeric") # number of dosage changes are numeric
})

# Test process_patient function
test_that("all patients are processed", {
  test_results <- compute_event_durations(disp.data = durcomp.dispensing,
                                          presc.data = durcomp.prescribing,
                                          special.periods.data = durcomp.hospitalisation,
                                          special.periods.mapping = "continue",
                                          ID.colname = "ID",
                                          presc.date.colname = "DATE.PRESC",
                                          disp.date.colname = "DATE.DISP",
                                          date.format = "%Y-%m-%d",
                                          medication.class.colnames = c("ATC.CODE", "UNIT", "FORM"),
                                          total.dose.colname = "TOTAL.DOSE",
                                          presc.daily.dose.colname = "DAILY.DOSE",
                                          presc.duration.colname = "PRESC.DURATION",
                                          visit.colname = "VISIT",
                                          force.init.presc = TRUE,
                                          force.presc.renew = TRUE,
                                          split.on.dosage.change = TRUE,
                                          trt.interruption = "continue",
                                          suppress.warnings = FALSE,
                                          return.data.table = TRUE)
  expect_length(unique(test_results$ID), 16)
  expect_equal(unique(test_results$ID), c(1:16))
})

# Test process_medication function
test_that("all medications for one patient are processed", {
  test_results <- compute_event_durations(disp.data = durcomp.dispensing[ID == 3],
                                          presc.data = durcomp.prescribing[ID == 3],
                                          hosp.data = durcomp.hospitalisation,
                                          ID.colname = "ID",
                                          presc.date.colname = "DATE.PRESC",
                                          disp.date.colname = "DATE.DISP",
                                          date.format = "%Y-%m-%d",
                                          medication.class.colnames = c("ATC.CODE", "UNIT", "FORM"),
                                          total.dose.colname = "TOTAL.DOSE",
                                          presc.daily.dose.colname = "DAILY.DOSE",
                                          presc.duration.colname = "PRESC.DURATION",
                                          visit.colname = "VISIT",
                                          force.init.presc = TRUE,
                                          force.presc.renew = TRUE,
                                          split.on.dosage.change = TRUE,
                                          trt.interruption = "continue",
                                          suppress.warnings = FALSE,
                                          return.data.table = TRUE)
  expect_length(unique(test_results$ATC.CODE), 17)  #number of medications dispensed & prescribed
  expect_equal(unique(test_results$ATC.CODE), c("A05AA02",
                                                "A09AA02",
                                                "A11CA01",
                                                "A11CC05",
                                                "A11HA03",
                                                "A12AX",
                                                "B02BA01",
                                                "J01AA08",
                                                "J01CR02",
                                                "J01EE01",
                                                "J01FA10",
                                                "J01GB01",
                                                "J01MA02",
                                                "J01XB01",
                                                "J02AC02",
                                                "R03AK08",
                                                "R05CB13")) #identity of medications dispensed & prescribed
  expect_length(unique(na.omit(test_results[,1:10])$ATC.CODE), 16) #number of medications prescribed
  expect_equal(unique(na.omit(test_results[,1:10])$ATC.CODE), c("A05AA02",
                                                                "A09AA02",
                                                                "A11CA01",
                                                                "A11CC05",
                                                                "A11HA03",
                                                                "A12AX",
                                                                "B02BA01",
                                                                "J01AA08",
                                                                "J01EE01",
                                                                "J01FA10",
                                                                "J01GB01",
                                                                "J01MA02",
                                                                "J01XB01",
                                                                "J02AC02",
                                                                "R03AK08",
                                                                "R05CB13")) #identity of medications prescribed
  expect_equal(nrow(unique(test_results[,.(START.PRESC, END.PRESC)])), 23) #number of prescription instances + NA
  expect_equal(as.character(sort(unique(test_results$START.PRESC))), sort(c("2056-07-01",
                                                                 "2057-09-02",
                                                                 "2057-01-27",
                                                                 "2056-09-09",
                                                                 "2056-10-18",
                                                                 "2057-06-09",
                                                                 NA,
                                                                 "2057-05-12",
                                                                 "2057-02-26",
                                                                 "2057-03-04",
                                                                 "2057-10-05",
                                                                 "2057-12-10",
                                                                 "2057-03-03",
                                                                 "2057-08-04",
                                                                 "2056-09-16",
                                                                 "2057-01-28"))) #prescription instances + NA
    expect_equal(max(test_results$SPECIAL.DURATION, na.rm = T), 63) #maximal hospital duration


    durcomp.prescribing_2 <- data.table(ID = rep(10, 8),
                            DATE.PRESC = as.Date(c("2057-01-01",
                                                   "2057-02-01",
                                                   "2057-03-01",
                                                   "2057-05-01",
                                                   "2057-07-01",
                                                   "2057-09-01",
                                                   "2057-11-01",
                                                   "2058-01-01")),
                            VISIT = c(0,1,2,4,5,6,7,8),
                            ATC.CODE = rep("A", 8),
                            UNIT = rep("mg", 8),
                            FORM = rep("oral", 8),
                            DAILY.DOSE = c(1,1,1,2,1,1,1,1),
                            PRESC.DURATION = c(30,30,NA,NA,30,NA,90,NA))

    durcomp.dispensing_2 <- data.table(ID = rep(10, 13),
                              DATE.DISP = as.Date(c("2057-01-01",
                                                     "2057-02-01",
                                                     "2057-03-01",
                                                     "2057-04-01",
                                                     "2057-05-01",
                                                     "2057-06-01",
                                                     "2057-07-01",
                                                     "2057-08-01",
                                                     "2057-09-01",
                                                     "2057-10-01",
                                                     "2057-11-01",
                                                     "2057-12-01",
                                                     "2058-01-01")),
                              VISIT = c(0,1,2,3,4,5,6,7,8,9,10,11,12),
                              ATC.CODE = rep("A", 13),
                              UNIT = rep("mg", 13),
                              FORM = rep("oral", 13),
                              TOTAL.DOSE = rep(30,13))

    test_results <- compute_event_durations(disp.data = durcomp.dispensing_2,
                                            presc.data = durcomp.prescribing_2,
                                            hosp.data = NULL,
                                            ID.colname = "ID",
                                            presc.date.colname = "DATE.PRESC",
                                            disp.date.colname = "DATE.DISP",
                                            date.format = "%Y-%m-%d",
                                            medication.class.colnames = c("ATC.CODE", "UNIT", "FORM"),
                                            total.dose.colname = "TOTAL.DOSE",
                                            presc.daily.dose.colname = "DAILY.DOSE",
                                            presc.duration.colname = "PRESC.DURATION",
                                            visit.colname = "VISIT",
                                            force.init.presc = TRUE,
                                            force.presc.renew = TRUE,
                                            split.on.dosage.change = TRUE,
                                            trt.interruption = "continue",
                                            suppress.warnings = FALSE,
                                            return.data.table = TRUE)

    expect_equal(as.character(sort(unique(c(test_results$START.PRESC, test_results$END.PRESC)))),
                              c("2057-01-01","2057-01-31","2057-02-01","2057-05-01","2057-07-01","2057-07-31","2057-09-01"))

})

# Test process_dispensing events function
test_that("all dispensing events for one patient are processed", {
  test_results <- compute_event_durations(disp.data = durcomp.dispensing[ID == 3],
                                          presc.data = durcomp.prescribing[ID == 3],
                                          special.periods.data = durcomp.hospitalisation,
                                          special.periods.mapping = "continue",
                                          ID.colname = "ID",
                                          presc.date.colname = "DATE.PRESC",
                                          disp.date.colname = "DATE.DISP",
                                          date.format = "%Y-%m-%d",
                                          medication.class.colnames = c("ATC.CODE", "UNIT", "FORM"),
                                          total.dose.colname = "TOTAL.DOSE",
                                          presc.daily.dose.colname = "DAILY.DOSE",
                                          presc.duration.colname = "PRESC.DURATION",
                                          visit.colname = "VISIT",
                                          force.init.presc = TRUE,
                                          force.presc.renew = TRUE,
                                          split.on.dosage.change = TRUE,
                                          trt.interruption = "continue",
                                          suppress.warnings = FALSE,
                                          return.data.table = TRUE)

  expect_equal(dim(test_results), c(111,14)) #correct number of lines
  expect_equal(round(sum(test_results$DURATION, na.rm=TRUE), 3), 3269.214) #correct sum of durations
  expect_equal(round(min(test_results$DURATION, na.rm=TRUE), 0), 10) #correct minimum of durations
  expect_equal(round(mean(test_results$DURATION, na.rm=TRUE), 4), 32.0511) #correct mean of durations
  expect_equal(round(max(test_results$DURATION, na.rm=TRUE), 0), 120) #correct maximum of durations
})

# Test process without hospitalizations
test_that("events are processed without hospitalizations", {
  test_results <- compute_event_durations(disp.data = durcomp.dispensing[ID == 3],
                                          presc.data = durcomp.prescribing[ID == 3],
                                          special.periods.data = NULL,
                                          ID.colname = "ID",
                                          presc.date.colname = "DATE.PRESC",
                                          disp.date.colname = "DATE.DISP",
                                          date.format = "%Y-%m-%d",
                                          medication.class.colnames = c("ATC.CODE", "UNIT", "FORM"),
                                          total.dose.colname = "TOTAL.DOSE",
                                          presc.daily.dose.colname = "DAILY.DOSE",
                                          presc.duration.colname = "PRESC.DURATION",
                                          visit.colname = "VISIT",
                                          force.init.presc = TRUE,
                                          force.presc.renew = TRUE,
                                          split.on.dosage.change = TRUE,
                                          trt.interruption = "continue",
                                          suppress.warnings = FALSE,
                                          return.data.table = TRUE)
  expect_equal(max(test_results$SPECIAL.DURATION, na.rm = T), 0) #hospital duration
  expect_equal(round(sum(test_results$DURATION, na.rm=TRUE), 0), 3269) #correct sum of durations
})

# Test with force.init.presc = FALSE
test_that("enforcement of initial prescription can be turned off", {
  test_results_1 <- compute_event_durations(disp.data = durcomp.dispensing[ID == 3],
                                            presc.data = durcomp.prescribing[ID == 3],
                                            hosp.data = NULL,
                                            ID.colname = "ID",
                                            presc.date.colname = "DATE.PRESC",
                                            disp.date.colname = "DATE.DISP",
                                            date.format = "%Y-%m-%d",
                                            medication.class.colnames = c("ATC.CODE", "UNIT", "FORM"),
                                            total.dose.colname = "TOTAL.DOSE",
                                            presc.daily.dose.colname = "DAILY.DOSE",
                                            presc.duration.colname = "PRESC.DURATION",
                                            visit.colname = "VISIT",
                                            force.init.presc = FALSE,
                                            force.presc.renew = TRUE,
                                            split.on.dosage.change = TRUE,
                                            trt.interruption = "continue",
                                            suppress.warnings = FALSE,
                                            return.data.table = TRUE)

  expect_equal(as.character(min(test_results_1$START.PRESC, na.rm = T)), "2056-09-23")
  expect_equal(as.character(mean(test_results_1$START.PRESC, na.rm = T)), "2056-11-22")
  expect_equal(as.character(max(test_results_1$START.PRESC, na.rm = T)), "2057-12-10")
})

# Test with force.presc.renew = FALSE
test_that("enforcing of prescription reneval can be turned off", {
  test_results_2 <- compute_event_durations(disp.data = durcomp.dispensing[ID == 3],
                                            presc.data = durcomp.prescribing[ID == 3],
                                            hosp.data = NULL,
                                            ID.colname = "ID",
                                            presc.date.colname = "DATE.PRESC",
                                            disp.date.colname = "DATE.DISP",
                                            date.format = "%Y-%m-%d",
                                            medication.class.colnames = c("ATC.CODE", "UNIT", "FORM"),
                                            total.dose.colname = "TOTAL.DOSE",
                                            presc.daily.dose.colname = "DAILY.DOSE",
                                            presc.duration.colname = "PRESC.DURATION",
                                            visit.colname = "VISIT",
                                            force.init.presc = TRUE,
                                            force.presc.renew = FALSE,
                                            split.on.dosage.change = TRUE,
                                            trt.interruption = "continue",
                                            suppress.warnings = FALSE,
                                            return.data.table = TRUE)

  expect_equal(as.character(mean(test_results_2$START.PRESC, na.rm = T)), "2056-09-22")
  expect_equal(dim(test_results_2), c(110,13))
})

# Test with split.on.dosage.change = FALSE
test_that("consideration of dosage changes can be turned off", {
  test_results1 <- compute_event_durations(disp.data = durcomp.dispensing[ID == 3],
                                           presc.data = durcomp.prescribing[ID == 3],
                                           hosp.data = NULL,
                                           ID.colname = "ID",
                                           presc.date.colname = "DATE.PRESC",
                                           disp.date.colname = "DATE.DISP",
                                           date.format = "%Y-%m-%d",
                                           medication.class.colnames = c("ATC.CODE", "UNIT", "FORM"),
                                           total.dose.colname = "TOTAL.DOSE",
                                           presc.daily.dose.colname = "DAILY.DOSE",
                                           presc.duration.colname = "PRESC.DURATION",
                                           visit.colname = "VISIT",
                                           force.init.presc = TRUE,
                                           force.presc.renew = TRUE,
                                           split.on.dosage.change = FALSE,
                                           trt.interruption = "continue",
                                           suppress.warnings = FALSE,
                                           return.data.table = TRUE)
  expect_equal(dim(test_results1), c(111,13))
  expect_equal(round(sum(test_results1$DURATION, na.rm=TRUE),0), 3289) #correct sum of durations
})

# Test with trt.interruption = discard
test_that("consideration of dosage changes can be turned off", {
  test_results1 <- compute_event_durations(disp.data = durcomp.dispensing[ID == 3],
                                           presc.data = durcomp.prescribing[ID == 3],
                                           hosp.data = NULL,
                                           ID.colname = "ID",
                                           presc.date.colname = "DATE.PRESC",
                                           disp.date.colname = "DATE.DISP",
                                           date.format = "%Y-%m-%d",
                                           medication.class.colnames = c("ATC.CODE", "UNIT", "FORM"),
                                           total.dose.colname = "TOTAL.DOSE",
                                           presc.daily.dose.colname = "DAILY.DOSE",
                                           presc.duration.colname = "PRESC.DURATION",
                                           visit.colname = "VISIT",
                                           force.init.presc = TRUE,
                                           force.presc.renew = TRUE,
                                           split.on.dosage.change = FALSE,
                                           trt.interruption = "discard",
                                           suppress.warnings = FALSE,
                                           return.data.table = TRUE)

  expect_equal(round(sum(test_results1$DURATION, na.rm=TRUE),0), 3269) #correct sum of durations
})

# Test with trt.interruption = carryover
test_that("consideration of dosage changes can be turned off", {
  test_results1 <- compute_event_durations(disp.data = durcomp.dispensing[ID == 3],
                                           presc.data = durcomp.prescribing[ID == 3],
                                           hosp.data = NULL,
                                           ID.colname = "ID",
                                           presc.date.colname = "DATE.PRESC",
                                           disp.date.colname = "DATE.DISP",
                                           date.format = "%Y-%m-%d",
                                           medication.class.colnames = c("ATC.CODE", "UNIT", "FORM"),
                                           total.dose.colname = "TOTAL.DOSE",
                                           presc.daily.dose.colname = "DAILY.DOSE",
                                           presc.duration.colname = "PRESC.DURATION",
                                           visit.colname = "VISIT",
                                           force.init.presc = TRUE,
                                           force.presc.renew = TRUE,
                                           split.on.dosage.change = FALSE,
                                           trt.interruption = "carryover",
                                           suppress.warnings = FALSE,
                                           return.data.table = TRUE)

  expect_equal(round(sum(test_results1$DURATION, na.rm=TRUE),0), 3286) #correct sum of durations
})