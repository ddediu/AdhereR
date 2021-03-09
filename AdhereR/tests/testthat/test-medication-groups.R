############################################################
#
# Test the medication groups
#
############################################################

context("Medication groups")


test_that("CMA1 without medication groups", {
  # compute the return value, to be used throughout:
  cma1 <- CMA1(data=med.events.ATC,
               ID.colname="PATIENT_ID",
               event.date.colname="DATE",
               event.duration.colname="DURATION",
               event.daily.dose.colname="PERDAY",
               medication.class.colname="CATEGORY",
               followup.window.start=0,
               observation.window.start=0,
               observation.window.duration=365,
               date.format="%m/%d/%Y");

  expect_is(cma1, "CMA1")
  expect_false(is.null(cma1))

  cma1_CMA <- getCMA(cma1);
  expect_is(cma1_CMA, "data.frame")
  expect_equal(nrow(cma1_CMA), 16)
  expect_equal(round(cma1_CMA$CMA[2],2), 1.76)
})

test_that("CMA1 with medication groups", {
  # compute the return value, to be used throughout:
  cma1mg <- CMA1(data=med.events.ATC,
                 ID.colname="PATIENT_ID",
                 event.date.colname="DATE",
                 event.duration.colname="DURATION",
                 event.daily.dose.colname="PERDAY",
                 medication.class.colname="CATEGORY",
                 medication.groups=med.groups,
                 followup.window.start=0,
                 observation.window.start=0,
                 observation.window.duration=365,
                 date.format="%m/%d/%Y");

  expect_is(cma1mg, "CMA1")
  expect_false(is.null(cma1mg))

  cma1mg_CMA <- getCMA(cma1mg);
  expect_is(cma1mg_CMA, "list")
  expect_equal(length(cma1mg_CMA), 7)
  expect_equal(names(cma1mg_CMA)[[1]], "Vitamins")

  expect_is(cma1mg_CMA$Vitamins, "data.frame")
  expect_equal(nrow(cma1mg_CMA$Vitamins), 16)
  expect_equal(round(cma1mg_CMA$Vitamins$CMA[2],2), 1.76)
})









