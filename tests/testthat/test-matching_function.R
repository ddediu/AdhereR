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
  test_results <- process_medication_events()
  expect_is(test_results, "data.table") # is a data.table
  expect_equal(names(test_results), c("ID",
                                      "DATE.DISP",
                                      "DCI",
                                      "Unit",
                                      "Form",
                                      "TOTAL.DOSE",
                                      "DATE.PRESC",
                                      "DAILY.DOSE",
                                      "DURATION",
                                      "FIRST.PRESC",
                                      "START.PRESC",
                                      "END.PRESC",
                                      "PRESC.DURATION",
                                      "HOSP.DURATION",
                                      "DOSAGE.CHANGE"))
  expect_is(test_results$ID, "integer") # ID's are integers
  expect_is(test_results$DATE.DISP, "Date") # DATE.DISP is a date
  expect_is(test_results$DCI, "character") # DCIs are characters
  expect_is(test_results$Unit, "character") # Units are characters
  expect_is(test_results$Form, "character") # Forms are character
  expect_is(test_results$TOTAL.DOSE, "numeric") # TOTAL.DOSE is numeric
  expect_is(test_results$DATE.PRESC, "Date") # DATE.PRESC is a date
  expect_is(test_results$DAILY.DOSE, "numeric") # DAILY.DOSE is numeric
  expect_is(test_results$DURATION, "numeric") # DURATION is numeric
  expect_is(test_results$FIRST.PRESC, "Date") # FIRST.PRESC is a date
  expect_is(test_results$START.PRESC, "Date") # PRESC.START is a date
  expect_is(test_results$END.PRESC, "Date") # PRESC.END is a date
  expect_is(test_results$PRESC.DURATION, "integer") # PRESC.DURATIONs are integer
  expect_is(test_results$HOSP.DURATION, "numeric") # HOSP.DURATIONS are numeric
  expect_is(test_results$DOSAGE.CHANGE, "numeric") # DOSAGE.CHANGEs are numeric
})

# Test process_patient function
test_that("all patients are processed", {
  test_results <- process_medication_events()
  expect_length(unique(test_results$ID), 6)
  expect_equal(unique(test_results$ID), c(1,5,46,75,135,153))
})

# Test process_medication function
test_that("all medications for one patient are processed", {
  test_results <- process_medication_events(DISP.DATA = disp_events[ID == 1],
                                              PRESC.DATA = presc_events[ID == 1],
                                              HOSP.DATA = hosp_events[ID == 1])
  expect_length(unique(test_results$DCI), 8)  #number of medications dispensed & prescribed
  expect_equal(unique(test_results$DCI), c("MULTIENZYMES (LIPASE. PROTEASE...)",
                                           "PANTOPRAZOLE",
                                           "SALMETEROL",
                                           "AMOXICILLINE ET INHIBITEUR D'ENZYME",
                                           "COLECALCIFEROL",
                                           "ESOMEPRAZOLE",
                                           "PRISTINAMYCINE",
                                           "SULFAMETHOXAZOLE ET TRIMETHOPRIME")) #identity of medications dispensed & prescribed
  expect_length(unique(na.omit(test_results[,1:11])$DCI), 3) #number of medications prescribed
  expect_equal(unique(na.omit(test_results[,1:11])$DCI), c("MULTIENZYMES (LIPASE. PROTEASE...)",
                                                           "PANTOPRAZOLE",
                                                           "SALMETEROL")) #identity of medications prescribed
  expect_length(unique(test_results$DATE.PRESC), 10) #number of prescription instances + NA
  expect_equal(as.character(unique(test_results$DATE.PRESC)), c("2014-01-01",
                                                               "2015-02-01",
                                                               "2015-05-01",
                                                               "2015-09-01",
                                                               "2015-10-01",
                                                               "2015-03-01",
                                                               "2015-01-01",
                                                               "2015-04-01",
                                                               "2015-06-01",
                                                               NA )) #prescription instances + NA
  expect_equal(as.character(unique(test_results$FIRST.PRESC)), c("2014-01-01",
                                                                 NA,
                                                                 "2015-01-01")) #first date of prescription
  expect_equal(max(test_results$HOSP.DURATION, na.rm = T), 3) #maximal hospital duration
})

# Test process_dispensing events function
test_that("all dispensing events for one patient are processed", {
  test_results <- process_medication_events(DISP.DATA = disp_events[ID == 1],
                                              PRESC.DATA = presc_events[ID == 1],
                                              HOSP.DATA = hosp_events[ID == 1])
  expect_equal(dim(test_results), c(44,15)) #correct number of lines
  expect_equal(round(sum(test_results$DURATION, na.rm=TRUE), 4), 766.6589) #correct sum of durations
  expect_equal(round(min(test_results$DURATION, na.rm=TRUE), 0), 6) #correct minimum of durations
  expect_equal(round(mean(test_results$DURATION, na.rm=TRUE), 4), 38.3329) #correct mean of durations
  expect_equal(round(max(test_results$DURATION, na.rm=TRUE), 0), 111) #correct maximum of durations
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