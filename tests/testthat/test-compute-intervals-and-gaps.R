############################################################
#
# Test computation of event intervals and gaps
#
############################################################

context("Event intervals and gaps")

test_that("wrong data", {
  expect_warning(compute.event.int.gaps(data=NULL), "Event data must be a non-empty data.frame!")
  expect_warning(compute.event.int.gaps(data=NA), "Event data must be a non-empty data.frame!")
  expect_warning(compute.event.int.gaps(data=1), "Event data must be a non-empty data.frame!")
  expect_warning(compute.event.int.gaps(data="1"), "Event data must be a non-empty data.frame!")
})

test_that("wrong ID.colname", {
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname=NA), "The patient ID column \"NA\" cannot be empty, must be a single value, and must be present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname=NULL), "The patient ID column \"\" cannot be empty, must be a single value, and must be present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname=1), "The patient ID column \"1\" cannot be empty, must be a single value, and must be present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname=c(1,2)), "The patient ID column \"12\" cannot be empty, must be a single value, and must be present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname=as.Date("2001-01-01")), "The patient ID column \"2001-01-01\" cannot be empty, must be a single value, and must be present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname=c("PATIENT_ID","DATE")), "The patient ID column \"PATIENT_IDDATE\" cannot be empty, must be a single value, and must be present in the event data!")
})

test_that("wrong event.date.colname", {
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname=NA), "The event date column \"NA\" cannot be empty, must be a single value, and must be present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname=NULL), "The event date column \"\" cannot be empty, must be a single value, and must be present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname=1), "The event date column \"1\" cannot be empty, must be a single value, and must be present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname=c(1,2)), "The event date column \"12\" cannot be empty, must be a single value, and must be present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname=as.Date("2001-01-01")), "The event date column \"2001-01-01\" cannot be empty, must be a single value, and must be present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname=c("PATIENT_ID","DATE")), "The event date column \"PATIENT_IDDATE\" cannot be empty, must be a single value, and must be present in the event data!")
})

test_that("wrong event.duration.colname", {
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname=NA), "The event duration column \"NA\" cannot be empty, must be a single value, and must be present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname=NULL), "The event duration column \"\" cannot be empty, must be a single value, and must be present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname=1), "The event duration column \"1\" cannot be empty, must be a single value, and must be present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname=c(1,2)), "The event duration column \"12\" cannot be empty, must be a single value, and must be present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname=as.Date("2001-01-01")), "The event duration column \"2001-01-01\" cannot be empty, must be a single value, and must be present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname=c("PATIENT_ID","DATE")), "The event duration column \"PATIENT_IDDATE\" cannot be empty, must be a single value, and must be present in the event data!")
})

test_that("wrong event.daily.dose.colname", {
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname=1), "If given, the event daily dose column \"1\" must be a single value and must be present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="a"), "If given, the event daily dose column \"a\" must be a single value and must be present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname=c(1,2)), "If given, the event daily dose column \"12\" must be a single value and must be present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname=as.Date("2001-01-01")), "If given, the event daily dose column \"2001-01-01\" must be a single value and must be present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname=c("PATIENT_ID","DATE")), "If given, the event daily dose column \"PATIENT_IDDATE\" must be a single value and must be present in the event data!")
})

test_that("wrong medication.class.colname", {
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname=1), "If given, the event type column \"1\" must be a single value and must be present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="a"), "If given, the event type column \"a\" must be a single value and must be present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname=c(1,2)), "If given, the event type column \"12\" must be a single value and must be present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname=as.Date("2001-01-01")), "If given, the event type column \"2001-01-01\" must be a single value and must be present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname=c("PATIENT_ID","DATE")), "If given, the event type column \"PATIENT_IDDATE\" must be a single value and must be present in the event data!")
})

test_that("wrong carry overs", {
})

