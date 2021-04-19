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

test_that("CMA1 with medication groups (fixed FUW start date)", {
  # compute the return value, to be used throughout:
  cma1mg <- CMA1(data=med.events.ATC,
                 ID.colname="PATIENT_ID",
                 event.date.colname="DATE",
                 event.duration.colname="DURATION",
                 event.daily.dose.colname="PERDAY",
                 medication.class.colname="CATEGORY",
                 medication.groups=med.groups,
                 followup.window.start="01/07/2056",
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

  # compute the expected value by selecting specifically the observations in the target medication group ("Vitamins"):
  cma1vitamines <- CMA1(data=med.events.ATC[ med.events.ATC$CATEGORY_L2 == 'VITAMINS', ],
                        ID.colname="PATIENT_ID",
                        event.date.colname="DATE",
                        event.duration.colname="DURATION",
                        event.daily.dose.colname="PERDAY",
                        medication.class.colname="CATEGORY",
                        #medication.groups=med.groups,
                        followup.window.start="01/07/2056",
                        observation.window.start=0,
                        observation.window.duration=365,
                        date.format="%m/%d/%Y");

  expect_is(cma1vitamines, "CMA1")
  expect_false(is.null(cma1vitamines))

  cma1vitamines_CMA <- getCMA(cma1vitamines);
  expect_is(cma1vitamines_CMA, "data.frame")
  expect_equal(nrow(cma1vitamines_CMA), 14)

  # compare the results:
  expect_equal( sort(intersect(cma1vitamines_CMA$PATIENT_ID, cma1mg_CMA$Vitamins$PATIENT_ID)), c(2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16) )
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 2 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 2 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 3 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 3 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 4 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 4 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 6 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 6 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 7 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 7 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 8 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 8 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 9 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 9 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 10 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 10 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 11 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 11 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 12 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 12 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 13 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 13 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 14 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 14 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 15 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 15 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 16 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 16 ])
})


test_that("CMA1 with medication groups (per medication group FUW start date)", {
  # compute the return value, to be used throughout:
  cma1mg <- CMA1(data=med.events.ATC,
                 ID.colname="PATIENT_ID",
                 event.date.colname="DATE",
                 event.duration.colname="DURATION",
                 event.daily.dose.colname="PERDAY",
                 medication.class.colname="CATEGORY",
                 medication.groups=med.groups,
                 followup.window.start=0,
                 followup.window.start.per.medication.group=TRUE,
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

  # compute the expected value by selecting specifically the observations in the target medication group ("Vitamins"):
  cma1vitamines <- CMA1(data=med.events.ATC[ med.events.ATC$CATEGORY_L2 == 'VITAMINS', ],
                        ID.colname="PATIENT_ID",
                        event.date.colname="DATE",
                        event.duration.colname="DURATION",
                        event.daily.dose.colname="PERDAY",
                        medication.class.colname="CATEGORY",
                        #medication.groups=med.groups,
                        followup.window.start=0,
                        observation.window.start=0,
                        observation.window.duration=365,
                        date.format="%m/%d/%Y");

  expect_is(cma1vitamines, "CMA1")
  expect_false(is.null(cma1vitamines))

  cma1vitamines_CMA <- getCMA(cma1vitamines);
  expect_is(cma1vitamines_CMA, "data.frame")
  expect_equal(nrow(cma1vitamines_CMA), 14)

  # compare the results:
  expect_equal( sort(intersect(cma1vitamines_CMA$PATIENT_ID, cma1mg_CMA$Vitamins$PATIENT_ID)), c(2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16) )
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 2 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 2 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 3 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 3 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 4 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 4 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 6 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 6 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 7 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 7 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 8 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 8 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 9 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 9 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 10 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 10 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 11 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 11 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 12 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 12 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 13 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 13 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 14 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 14 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 15 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 15 ])
  expect_equal(cma1vitamines_CMA$CMA[ cma1vitamines_CMA$PATIENT_ID == 16 ], cma1mg_CMA$Vitamins$CMA[ cma1mg_CMA$Vitamins$PATIENT_ID == 16 ])
})








