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
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", carryover.within.obs.window=NA), "Carry over arguments must be single value logicals!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", carryover.within.obs.window=NULL), "Carry over arguments must be single value logicals!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", carryover.within.obs.window="a"), "Carry over arguments must be single value logicals!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", carryover.within.obs.window=1), "Carry over arguments must be single value logicals!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", carryover.within.obs.window=c(TRUE,FALSE)), "Carry over arguments must be single value logicals!")

  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", carryover.into.obs.window=NA), "Carry over arguments must be single value logicals!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", carryover.into.obs.window=NULL), "Carry over arguments must be single value logicals!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", carryover.into.obs.window="a"), "Carry over arguments must be single value logicals!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", carryover.into.obs.window=1), "Carry over arguments must be single value logicals!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", carryover.into.obs.window=c(TRUE,FALSE)), "Carry over arguments must be single value logicals!")

  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", carry.only.for.same.medication=NA), "Carry over arguments must be single value logicals!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", carry.only.for.same.medication=NULL), "Carry over arguments must be single value logicals!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", carry.only.for.same.medication="a"), "Carry over arguments must be single value logicals!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", carry.only.for.same.medication=1), "Carry over arguments must be single value logicals!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", carry.only.for.same.medication=c(TRUE,FALSE)), "Carry over arguments must be single value logicals!")

  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", carryover.within.obs.window=FALSE, carryover.into.obs.window=FALSE, carry.only.for.same.medication=TRUE), "Cannot carry over only for same medication when no carry over at all is considered!")
})

test_that("wrong consider.dosage.change", {
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", consider.dosage.change=NA), "Consider dosage change must be single value logical!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", consider.dosage.change=NULL), "Consider dosage change must be single value logical!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", consider.dosage.change="a"), "Consider dosage change must be single value logical!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", consider.dosage.change=1), "Consider dosage change must be single value logical!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", consider.dosage.change=c(TRUE,FALSE)), "Consider dosage change must be single value logical!")
})

test_that("follow-up window start", {
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=NULL), "The follow-up window start must be a single value, either a positive number, a Date object, or a string giving a column name in the data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=NA), "The follow-up window start must be a single value, either a positive number, a Date object, or a string giving a column name in the data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=factor(c("a"))), "The follow-up window start must be a single value, either a positive number, a Date object, or a string giving a column name in the data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=data.frame("a"=1)), "The follow-up window start must be a single value, either a positive number, a Date object, or a string giving a column name in the data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start="2001-01-01"), "The follow-up window start must be a single value, either a positive number, a Date object, or a string giving a column name in the data!")
})

test_that("follow-up window start unit", {
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit=NA), "The follow-up window start unit must be a single value, one of \"days\", \"weeks\", \"months\" or \"years\"!")
})

test_that("follow-up window duration", {
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=NA), "The follow-up window duration must be a single value, either a positive number, or a string giving a column name in the data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=NULL), "The follow-up window duration must be a single value, either a positive number, or a string giving a column name in the data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=-3), "The follow-up window duration must be a single value, either a positive number, or a string giving a column name in the data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=c(1,2)), "The follow-up window duration must be a single value, either a positive number, or a string giving a column name in the data!")
})

test_that("follow-up window duration unit", {
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=100, followup.window.duration.unit="années"), "The follow-up window duration unit must be a single value, one of \"days\", \"weeks\", \"months\" or \"years\"!")
})

test_that("observation window start", {
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, observation.window.start=NULL), "The observation window start must be a single value, either a positive number, a Date object, or a string giving a column name in the data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, observation.window.start=NA), "The observation window start must be a single value, either a positive number, a Date object, or a string giving a column name in the data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, observation.window.start=factor(c("a"))), "The observation window start must be a single value, either a positive number, a Date object, or a string giving a column name in the data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, observation.window.start=data.frame("a"=1)), "The observation window start must be a single value, either a positive number, a Date object, or a string giving a column name in the data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, observation.window.start="2001-01-01"), "The observation window start must be a single value, either a positive number, a Date object, or a string giving a column name in the data!")
})

test_that("observation window start unit", {
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", observation.window.start=0, observation.window.start.unit=NA), "The observation window start unit must be a single value, one of \"days\", \"weeks\", \"months\" or \"years\"!")
})

test_that("observation window duration", {
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, observation.window.start=0, observation.window.start.unit="days", observation.window.duration=NA), "The observation window duration must be a single value, either a positive number, or a string giving a column name in the data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, observation.window.start=0, observation.window.start.unit="days", observation.window.duration=NULL), "The observation window duration must be a single value, either a positive number, or a string giving a column name in the data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, observation.window.start=0, observation.window.start.unit="days", observation.window.duration=-3), "The observation window duration must be a single value, either a positive number, or a string giving a column name in the data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, observation.window.start=0, observation.window.start.unit="days", observation.window.duration=c(1,2)), "The observation window duration must be a single value, either a positive number, or a string giving a column name in the data!")
})

test_that("observation window duration unit", {
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, followup.window.duration.unit="days", observation.window.start=0, observation.window.start.unit="days", observation.window.duration=100, observation.window.duration.unit="années"), "The observation window duration unit must be a single value, one of \"days\", \"weeks\", \"months\" or \"years\"!")
})





