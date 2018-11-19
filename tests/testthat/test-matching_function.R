#########################################################################################
#
#  Test matching function to combine dispensing, prescription, and hospitalization data
#
#########################################################################################
load("data/matching_data.rda")

context("Matching functions")

# # Test arguments for illegal values
# test_that("dispensing data")

# Test output format
test_that("output format is correct", {
  test_results <- compute_event_durations(disp.data = test_disp,
                                            presc.data = test_presc,
                                            hosp.data = test_hosp,
                                            ID.colname = "ID",
                                            presc.date.colname = "DATE.PRESC",
                                            disp.date.colname = "DATE.DISP",
                                            date.format = "%Y-%m-%d",
                                            medication.class.colnames = c("ATC.CODE", "Unit", "Form"),
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
                                      "Unit",
                                      "Form",
                                      "TOTAL.DOSE",
                                      "DATE.DISP",
                                      "DISP.START",
                                      "DURATION",
                                      "DAILY.DOSE",
                                      "START.PRESC",
                                      "END.PRESC",
                                      "HOSP.DURATION",
                                      "tot.presc.interruptions",
                                      "tot.dosage.changes"))
  expect_is(test_results$ID, "numeric") # ID's are integers
  expect_is(test_results$ATC.CODE, "character") # DCIs are characters
  expect_is(test_results$Unit, "character") # Units are characters
  expect_is(test_results$Form, "character") # Forms are character
  expect_is(test_results$TOTAL.DOSE, "numeric") # TOTAL.DOSE is numeric
  expect_is(test_results$DATE.DISP, "Date") # DATE.DISP is a date
  expect_is(test_results$DISP.START, "Date") # DATE.DISP is a date
  expect_is(test_results$DURATION, "numeric") # DURATION is numeric
  expect_is(test_results$DAILY.DOSE, "numeric") # DAILY.DOSE is numeric
  expect_is(test_results$START.PRESC, "Date") # PRESC.START is a date
  expect_is(test_results$END.PRESC, "Date") # PRESC.END is a date
  expect_is(test_results$HOSP.DURATION, "numeric") # HOSP.DURATIONS are numeric
  expect_is(test_results$tot.presc.interruptions, "integer") # number of treatment interruptions are numeric
  expect_is(test_results$tot.dosage.changes, "numeric") # number of dosage changes are numeric
})

# Test process_patient function
test_that("all patients are processed", {
  test_results <- compute_event_durations(disp.data = test_disp,
                                          presc.data = test_presc,
                                          hosp.data = test_hosp,
                                          ID.colname = "ID",
                                          presc.date.colname = "DATE.PRESC",
                                          disp.date.colname = "DATE.DISP",
                                          date.format = "%Y-%m-%d",
                                          medication.class.colnames = c("ATC.CODE", "Unit", "Form"),
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
  test_results <- compute_event_durations(disp.data = test_disp[ID == 3],
                                          presc.data = test_presc[ID == 3],
                                          hosp.data = test_hosp,
                                          ID.colname = "ID",
                                          presc.date.colname = "DATE.PRESC",
                                          disp.date.colname = "DATE.DISP",
                                          date.format = "%Y-%m-%d",
                                          medication.class.colnames = c("ATC.CODE", "Unit", "Form"),
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
  expect_equal(as.character(unique(test_results$START.PRESC)), c("2056-07-01",
                                                                 "2056-09-23",
                                                                 "2057-01-27",
                                                                 "2056-09-09",
                                                                 "2057-06-09",
                                                                 NA,
                                                                 "2057-05-12",
                                                                 "2057-02-26",
                                                                 "2057-04-02",
                                                                 "2057-04-03",
                                                                 "2057-10-05",
                                                                 "2057-12-10",
                                                                 "2057-03-03",
                                                                 "2057-08-04",
                                                                 "2056-09-16",
                                                                 "2057-01-28")) #prescription instances + NA
    expect_equal(max(test_results$HOSP.DURATION, na.rm = T), 63) #maximal hospital duration
})

# Test process_dispensing events function
test_that("all dispensing events for one patient are processed", {
  test_results <- compute_event_durations(disp.data = test_disp[ID == 3],
                                          presc.data = test_presc[ID == 3],
                                          hosp.data = test_hosp,
                                          ID.colname = "ID",
                                          presc.date.colname = "DATE.PRESC",
                                          disp.date.colname = "DATE.DISP",
                                          date.format = "%Y-%m-%d",
                                          medication.class.colnames = c("ATC.CODE", "Unit", "Form"),
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
  expect_equal(dim(test_results), c(112,14)) #correct number of lines
  expect_equal(round(sum(test_results$DURATION, na.rm=TRUE), 3), 3255.214) #correct sum of durations
  expect_equal(round(min(test_results$DURATION, na.rm=TRUE), 0), 10) #correct minimum of durations
  expect_equal(round(mean(test_results$DURATION, na.rm=TRUE), 4), 31.9139) #correct mean of durations
  expect_equal(round(max(test_results$DURATION, na.rm=TRUE), 0), 120) #correct maximum of durations
})

# Test process without hospitalizations
test_that("events are processed without hospitalizations", {
  test_results <- process_medication_events(DISP.DATA = disp_events[ID == 1],
                                              PRESC.DATA = presc_events[ID == 1],
                                              HOSP.DATA = NULL)
  expect_equal(max(test_results$HOSP.DURATION, na.rm = T), 0) #hospital duration
  expect_equal(round(sum(test_results$DURATION, na.rm=TRUE), 4), 763.6589) #correct sum of durations
})

# Test with force.init.presc = FALSE
test_that("enforcement of initial prescription can be turned off", {
  test_results_1 <- process_medication_events(force.init.presc = FALSE)
  expect_equal(as.character(min(test_results_1$FIRST.PRESC, na.rm = T)), "2014-09-22")
  expect_equal(as.character(mean(test_results_1$FIRST.PRESC, na.rm = T)), "2014-12-27")
  expect_equal(as.character(max(test_results_1$FIRST.PRESC, na.rm = T)), "2015-11-17")
})

# Test with force.presc.renew = FALSE
test_that("enforcing of prescription reneval can be turned off", {
  test_results_2 <- process_medication_events(force.presc.renew = FALSE)
  expect_equal(as.character(mean(test_results_2$FIRST.PRESC, na.rm = T)), "2014-08-29")
  expect_equal(dim(test_results_2), c(396,15))
  expect_equal(round(sum(test_results_2$DURATION, na.rm=TRUE), 3), 9590.872) #correct sum of durations
})

# Test with consider.dosage.change = FALSE
test_that("consideration of dosage changes can be turned off", {
  test_results1 <- process_medication_events(consider.dosage.change = FALSE,
                                              DISP.DATA = disp_events[ID == 1],
                                              PRESC.DATA = presc_events[ID == 1],
                                              HOSP.DATA = hosp_events[ID == 1])
  expect_equal(dim(test_results1), c(42,15))
  expect_equal(mean(test_results1$DOSAGE.CHANGE, na.rm=TRUE), NaN) #dosage change should be NA for all entries
  expect_equal(sum(test_results1$DURATION, na.rm=TRUE), Inf) #correct sum of durations
})