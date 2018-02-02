############################################################
#
# Test computation of event intervals and gaps
#
############################################################

context("Event intervals and gaps")


# Test argument illegal values:
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

test_that("Patient IDs contain NAs", {
  expect_warning({tmp <- med.events; tmp$PATIENT_ID[5] <- NA; compute.event.int.gaps(data=tmp, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, followup.window.duration.unit="days", observation.window.start=0, observation.window.start.unit="days", observation.window.duration=100, observation.window.duration.unit="days")}, "The patient unique identifiers in the \"PATIENT_ID\" column must not contain NAs; the first occurs on row 5!")
})

test_that("event date and date format", {
  expect_warning({tmp <- med.events; tmp$DATE[5] <- "13/10/2033"; compute.event.int.gaps(data=tmp, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, followup.window.duration.unit="days", observation.window.start=0, observation.window.start.unit="days", observation.window.duration=100, observation.window.duration.unit="days")}, "Not all entries in the event date \"DATE\" column are valid dates or conform to the date format \"%m/%d/%Y\"; first issue occurs on row 5!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, followup.window.duration.unit="days", observation.window.start=0, observation.window.start.unit="days", observation.window.duration=100, observation.window.duration.unit="days", date.format="%m-%d;%y"), "Not all entries in the event date \"DATE\" column are valid dates or conform to the date format \"%m-%d;%y\"; first issue occurs on row 1!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, followup.window.duration.unit="days", observation.window.start=0, observation.window.start.unit="days", observation.window.duration=100, observation.window.duration.unit="days", date.format="%z-%k"), "Not all entries in the event date \"DATE\" column are valid dates or conform to the date format \"%z-%k\"; first issue occurs on row 1!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, followup.window.duration.unit="days", observation.window.start=0, observation.window.start.unit="days", observation.window.duration=100, observation.window.duration.unit="days", date.format="wrong!"), "Not all entries in the event date \"DATE\" column are valid dates or conform to the date format \"wrong!\"; first issue occurs on row 1!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, followup.window.duration.unit="days", observation.window.start=0, observation.window.start.unit="days", observation.window.duration=100, observation.window.duration.unit="days", date.format=NA), "The date format must be a single string!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, followup.window.duration.unit="days", observation.window.start=0, observation.window.start.unit="days", observation.window.duration=100, observation.window.duration.unit="days", date.format=NULL), "The date format must be a single string!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, followup.window.duration.unit="days", observation.window.start=0, observation.window.start.unit="days", observation.window.duration=100, observation.window.duration.unit="days", date.format=1), "The date format must be a single string!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, followup.window.duration.unit="days", observation.window.start=0, observation.window.start.unit="days", observation.window.duration=100, observation.window.duration.unit="days", date.format=c("a","b")), "The date format must be a single string!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, followup.window.duration.unit="days", observation.window.start=0, observation.window.start.unit="days", observation.window.duration=100, observation.window.duration.unit="days", date.format=data.frame("a"="a","b"=1)), "The date format must be a single string!")
})

test_that("event duration", {
  expect_warning({tmp <- med.events; tmp$DURATION[5] <- NA; compute.event.int.gaps(data=tmp, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, followup.window.duration.unit="days", observation.window.start=0, observation.window.start.unit="days", observation.window.duration=100, observation.window.duration.unit="days")}, "The event durations in the \"DURATION\" column must be non-missing strictly positive numbers!")
  expect_warning({tmp <- med.events; tmp$DURATION[5] <- -3; compute.event.int.gaps(data=tmp, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, followup.window.duration.unit="days", observation.window.start=0, observation.window.start.unit="days", observation.window.duration=100, observation.window.duration.unit="days")}, "The event durations in the \"DURATION\" column must be non-missing strictly positive numbers!")
  expect_warning({tmp <- med.events; tmp$DURATION[5] <- "a"; compute.event.int.gaps(data=tmp, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, followup.window.duration.unit="days", observation.window.start=0, observation.window.start.unit="days", observation.window.duration=100, observation.window.duration.unit="days")}, "The event durations in the \"DURATION\" column must be non-missing strictly positive numbers!")
})

test_that("event dose", {
  expect_warning({tmp <- med.events; tmp$PERDAY[5] <- NA; compute.event.int.gaps(data=tmp, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, followup.window.duration.unit="days", observation.window.start=0, observation.window.start.unit="days", observation.window.duration=100, observation.window.duration.unit="days")}, "If given, the event daily dose in the \"PERDAY\" column must be a non-missing strictly positive numbers!")
  expect_warning({tmp <- med.events; tmp$PERDAY[5] <- -3; compute.event.int.gaps(data=tmp, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, followup.window.duration.unit="days", observation.window.start=0, observation.window.start.unit="days", observation.window.duration=100, observation.window.duration.unit="days")}, "If given, the event daily dose in the \"PERDAY\" column must be a non-missing strictly positive numbers!")
  expect_warning({tmp <- med.events; tmp$PERDAY[5] <- "a"; compute.event.int.gaps(data=tmp, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, followup.window.duration.unit="days", observation.window.start=0, observation.window.start.unit="days", observation.window.duration=100, observation.window.duration.unit="days")}, "If given, the event daily dose in the \"PERDAY\" column must be a non-missing strictly positive numbers!")
})

test_that("internally created columns for storing the event intervals and the gap days", {
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, followup.window.duration.unit="days", observation.window.start=0, observation.window.start.unit="days", observation.window.duration=100, observation.window.duration.unit="days", event.interval.colname=NA), "The column name where the event interval will be stored \"NA\" cannot be missing nor already present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, followup.window.duration.unit="days", observation.window.start=0, observation.window.start.unit="days", observation.window.duration=100, observation.window.duration.unit="days", event.interval.colname=NULL), "The column name where the event interval will be stored \"\" cannot be missing nor already present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, followup.window.duration.unit="days", observation.window.start=0, observation.window.start.unit="days", observation.window.duration=100, observation.window.duration.unit="days", event.interval.colname=1), "The column name where the event interval will be stored \"1\" cannot be missing nor already present in the event data!")
  expect_warning(compute.event.int.gaps(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY", followup.window.start=0, followup.window.start.unit="days", followup.window.duration=365, followup.window.duration.unit="days", observation.window.start=0, observation.window.start.unit="days", observation.window.duration=100, observation.window.duration.unit="days", event.interval.colname="PATIENT_ID"), "The column name where the event interval will be stored \"PATIENT_ID\" cannot be missing nor already present in the event data!")
})


# Test exected results for various parameter combinations using the included med.events data:
# Given that we expect to obtain large data.frames, we will test the major properties of the result (class, nrow, ncols, column names),
# the number/distribution of values per columns, and some rows chosen either at random or due to some interesting properties.
# General rules:
#  - integer columns: check number of unique values (if not too many), table (if not too big), and sample of values
#  - numeric columns: check number of NAs, min, max, mean, median, sd (all exluding NAs), number <0, ==0 and >0, and sample of values
#  - character columns: check MD5 (this is probably ok across platforms, internal representations, etc) and sample of values
library(digest); # for computing hashes

test_that("compute.event.int.gaps() on default values", {
  # compute the return value, to be used throughout:
  d <- compute.event.int.gaps(data=med.events,
  														ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION",
  														event.daily.dose.colname=NA, medication.class.colname=NA,
  														carryover.within.obs.window=FALSE, carryover.into.obs.window=FALSE, carry.only.for.same.medication=FALSE,
  														consider.dosage.change=FALSE,
  														followup.window.start=0, followup.window.start.unit=c("days", "weeks", "months", "years")[1],
  														followup.window.duration=365*2, followup.window.duration.unit=c("days", "weeks", "months", "years")[1],
  														observation.window.start=0, observation.window.start.unit=c("days", "weeks", "months", "years")[1],
  														observation.window.duration=365*2, observation.window.duration.unit=c("days", "weeks", "months", "years")[1],
  														date.format="%m/%d/%Y",
  														keep.window.start.end.dates=FALSE, remove.events.outside.followup.window=TRUE, keep.event.interval.for.all.events=FALSE,
  														parallel.backend=c("none","multicore","snow","snow(SOCK)","snow(MPI)","snow(NWS)")[1], parallel.threads="auto",
  														suppress.warnings=FALSE, return.data.table=FALSE);

  expect_is(d, "data.frame") # is a data.frame
  expect_equal(dim(d), c(1066,7)) # number of events and columns
  expect_equal(names(d), c("PATIENT_ID","DATE","PERDAY","CATEGORY","DURATION","event.interval","gap.days")) # the names of the columns
  # PATIENT_ID:
  expect_is(d$PATIENT_ID, "integer") # PATIENT_ID's are integers
  expect_equal(unique(d$PATIENT_ID), 1:100) # PATIENT_ID's are integers from 1 to 100
  expect_equal(as.numeric(table(d$PATIENT_ID)), c(24,8,28,6,8,9,6,14,9,14,6,7,5,24,12,22,9,6,6,11,10,6,11,9,10,12,13,10,14,11,3,6,4,11,11,11,8,9,9,11,6,7,20,7,10,18,10,12,6,9,6,7,4,5,5,11,4,16,18,7,10,8,6,11,19,20,14,8,6,9,11,6,9,7,13,11,6,24,22,19,7,9,8,12,9,13,10,14,6,12,6,18,7,11,11,22,12,8,5,16)) # the distribution of PATIENT_ID's
  expect_equal(d$PATIENT_ID[c(54,210,423,424,755,1050)], c(3,17,40,41,74,99)) # a selection of individual rows
  # DATE:
  expect_is(d$DATE, "character") # DATE's are characters
  expect_equal(digest(d$DATE,"md5",serialize=TRUE,ascii=TRUE), "378d1ede8d6699c84633c0ca5c757fc2") # digest is probably safe for characters
  expect_equal(d$DATE[c(54,210,423,424,755,1050)], c("11/10/2043", "07/07/2037", "06/22/2037", "04/11/2041", "08/03/2032", "08/01/2033")) # a selection of individual rows
  # PERDAY:
  expect_is(d$PERDAY, "integer") # PERDAY's are integers
  expect_equal(unique(d$PERDAY), c(4,2,8,6,20)) # PERDAY's are integers
  expect_equal(as.numeric(table(d$PERDAY)), c(374, 533, 123, 26, 10)) # the distribution of PERDAY's
  expect_equal(d$PERDAY[c(54,210,423,424,755,1050)], c(2, 4, 6, 4, 2, 4)) # a selection of individual rows
  # CATEGORY:
  expect_is(d$CATEGORY, "character") # CATEGORY's are characters
  expect_equal(digest(d$CATEGORY,"md5",serialize=TRUE,ascii=TRUE), "7411863b7ceb60f19f8a9b8480063a0a") # digest is probably safe for characters
  expect_equal(d$CATEGORY[c(54,210,423,424,755,1050)], c("medB", "medB", "medB", "medA", "medB", "medB")) # a selection of individual rows
  # DURATION:
  expect_is(d$DURATION, "integer") # DURATION's are integers
  expect_equal(unique(d$DURATION), c(50, 30, 60, 100, 20, 150)) # DURATION's are integers
  expect_equal(as.numeric(table(d$DURATION)), c(54, 404, 368, 131, 105, 4)) # the distribution of DURATION's
  expect_equal(d$DURATION[c(54,210,423,424,755,1050)], c(50, 60, 60, 50, 60, 30)) # a selection of individual rows
  # event.interval:
  expect_is(d$event.interval, "numeric") # event.interval's are numbers
  expect_equal(min(d$event.interval,na.rm=TRUE), 0) # event.interval's summaries
  expect_equal(max(d$event.interval,na.rm=TRUE), 544)
  expect_equal(sum(is.na(d$event.interval)), 0)
  expect_equal(round(mean(d$event.interval,na.rm=TRUE),5), 68.4803)
  expect_equal(round(median(d$event.interval,na.rm=TRUE),5), 47.5)
  expect_equal(round(sd(d$event.interval,na.rm=TRUE),5), 64.80363)
  expect_equal(sum(d$event.interval == 0,na.rm=TRUE), 3)
  expect_equal(sum(d$event.interval < 0,na.rm=TRUE), 0)
  expect_equal(sum(d$event.interval > 0,na.rm=TRUE), 1063)
  expect_equal(d$event.interval[c(54,210,423,424,755,1050)], c(2, 76, 62, 47, 48, 98)) # a selection of individual rows
  # gap.days
  expect_is(d$gap.days, "numeric") # gap.days's are numbers
  expect_equal(min(d$gap.days,na.rm=TRUE), 0) # gap.days's summaries
  expect_equal(max(d$gap.days,na.rm=TRUE), 491)
  expect_equal(sum(is.na(d$gap.days)), 0)
  expect_equal(round(mean(d$gap.days,na.rm=TRUE),5), 25.95779)
  expect_equal(round(median(d$gap.days,na.rm=TRUE),5), 0)
  expect_equal(round(sd(d$gap.days,na.rm=TRUE),5), 54.84098)
  expect_equal(sum(d$gap.days == 0,na.rm=TRUE), 666)
  expect_equal(sum(d$gap.days < 0,na.rm=TRUE), 0)
  expect_equal(sum(d$gap.days > 0,na.rm=TRUE), 400)
  expect_equal(d$gap.days[c(54,210,423,424,755,1050)], c(0, 16, 0, 0, 0, 68)) # a selection of individual rows
})

test_that("compute.event.int.gaps() on default values + window durations in weeks", {
  # compute the return value, to be used throughout:
  d <- compute.event.int.gaps(data=med.events,
  														ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION",
  														event.daily.dose.colname=NA, medication.class.colname=NA,
  														carryover.within.obs.window=FALSE, carryover.into.obs.window=FALSE, carry.only.for.same.medication=FALSE,
  														consider.dosage.change=FALSE,
  														followup.window.start=0, followup.window.start.unit=c("days", "weeks", "months", "years")[1],
  														followup.window.duration=50*2, followup.window.duration.unit=c("days", "weeks", "months", "years")[2],
  														observation.window.start=0, observation.window.start.unit=c("days", "weeks", "months", "years")[1],
  														observation.window.duration=50*2, observation.window.duration.unit=c("days", "weeks", "months", "years")[2],
  														date.format="%m/%d/%Y",
  														keep.window.start.end.dates=FALSE, remove.events.outside.followup.window=TRUE, keep.event.interval.for.all.events=FALSE,
  														parallel.backend=c("none","multicore","snow","snow(SOCK)","snow(MPI)","snow(NWS)")[1], parallel.threads="auto",
  														suppress.warnings=FALSE, return.data.table=FALSE);

  expect_is(d, "data.frame") # is a data.frame
  expect_equal(dim(d), c(1024,7)) # number of events and columns
  expect_equal(names(d), c("PATIENT_ID","DATE","PERDAY","CATEGORY","DURATION","event.interval","gap.days")) # the names of the columns
  # PATIENT_ID:
  expect_is(d$PATIENT_ID, "integer") # PATIENT_ID's are integers
  expect_equal(unique(d$PATIENT_ID), 1:100) # PATIENT_ID's are integers from 1 to 100
  expect_equal(as.numeric(table(d$PATIENT_ID)), c(23,8,27,5,8,8,6,14,8,14,5,7,5,23,12,22,9,6,6,10,9,6,11,8,10,12,13,10,12,10,3,5,4,10,11,10,8,9,9,11,5,7,19,6,10,17,10,12,6,7,6,7,3,5,5,10,4,16,17,6,10,8,6,11,18,19,14,8,5,9,11,5,9,7,12,11,6,23,21,18,6,9,8,12,9,13,10,13,5,12,6,17,7,11,11,21,11,7,5,15)) # the distribution of PATIENT_ID's
  expect_equal(d$PATIENT_ID[c(54,210,423,424,755,950)], c(3,18,43,43,78,94)) # a selection of individual rows
  # DATE:
  expect_is(d$DATE, "character") # DATE's are characters
  expect_equal(digest(d$DATE,"md5",serialize=TRUE,ascii=TRUE), "3e9494d155f182f84528d6ca58071f06") # digest is probably safe for characters
  expect_equal(d$DATE[c(54,210,423,424,755,950)], c("11/12/2043", "12/30/2035", "01/18/2036", "03/05/2036", "11/19/2039", "10/09/2039")) # a selection of individual rows
  # PERDAY:
  expect_is(d$PERDAY, "integer") # PERDAY's are integers
  expect_equal(unique(d$PERDAY), c(4,2,8,6,20)) # PERDAY's are integers
  expect_equal(as.numeric(table(d$PERDAY)), c(355, 513, 120, 26, 10)) # the distribution of PERDAY's
  expect_equal(d$PERDAY[c(54,210,423,424,755,950)], c(2, 4, 4, 4, 2, 4)) # a selection of individual rows
  # CATEGORY:
  expect_is(d$CATEGORY, "character") # CATEGORY's are characters
  expect_equal(digest(d$CATEGORY,"md5",serialize=TRUE,ascii=TRUE), "7c135fc992cc985ef12c984c58d7c18b") # digest is probably safe for characters
  expect_equal(d$CATEGORY[c(54,210,423,424,755,950)], c("medB", "medB", "medA", "medA", "medA", "medB")) # a selection of individual rows
  # DURATION:
  expect_is(d$DURATION, "integer") # DURATION's are integers
  expect_equal(unique(d$DURATION), c(50, 30, 60, 100, 20, 150)) # DURATION's are integers
  expect_equal(as.numeric(table(d$DURATION)), c(54, 377, 363, 124, 102, 4)) # the distribution of DURATION's
  expect_equal(d$DURATION[c(54,210,423,424,755,950)], c(50, 30, 50, 50, 50, 60)) # a selection of individual rows
  # event.interval:
  expect_is(d$event.interval, "numeric") # event.interval's are numbers
  expect_equal(min(d$event.interval,na.rm=TRUE), 0) # event.interval's summaries
  expect_equal(max(d$event.interval,na.rm=TRUE), 544)
  expect_equal(sum(is.na(d$event.interval)), 0)
  expect_equal(round(mean(d$event.interval,na.rm=TRUE),5), 68.35938)
  expect_equal(round(median(d$event.interval,na.rm=TRUE),5), 47.5)
  expect_equal(round(sd(d$event.interval,na.rm=TRUE),5), 64.86965)
  expect_equal(sum(d$event.interval == 0,na.rm=TRUE), 3)
  expect_equal(sum(d$event.interval < 0,na.rm=TRUE), 0)
  expect_equal(sum(d$event.interval > 0,na.rm=TRUE), 1021)
  expect_equal(d$event.interval[c(54,210,423,424,755,950)], c(43, 46, 47, 58, 56, 197)) # a selection of individual rows
  # gap.days
  expect_is(d$gap.days, "numeric") # gap.days's are numbers
  expect_equal(min(d$gap.days,na.rm=TRUE), 0) # gap.days's summaries
  expect_equal(max(d$gap.days,na.rm=TRUE), 491)
  expect_equal(sum(is.na(d$gap.days)), 0)
  expect_equal(round(mean(d$gap.days,na.rm=TRUE),5), 26.04883)
  expect_equal(round(median(d$gap.days,na.rm=TRUE),5), 0)
  expect_equal(round(sd(d$gap.days,na.rm=TRUE),5), 54.61476)
  expect_equal(sum(d$gap.days == 0,na.rm=TRUE), 638)
  expect_equal(sum(d$gap.days < 0,na.rm=TRUE), 0)
  expect_equal(sum(d$gap.days > 0,na.rm=TRUE), 386)
  expect_equal(d$gap.days[c(54,210,423,424,755,950)], c(0, 16, 0, 0, 6, 106)) # a selection of individual rows
})

test_that("compute.event.int.gaps() on default values + window durations in months", {
  # compute the return value, to be used throughout:
  d <- compute.event.int.gaps(data=med.events,
  														ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION",
  														event.daily.dose.colname=NA, medication.class.colname=NA,
  														carryover.within.obs.window=FALSE, carryover.into.obs.window=FALSE, carry.only.for.same.medication=FALSE,
  														consider.dosage.change=FALSE,
  														followup.window.start=0, followup.window.start.unit=c("days", "weeks", "months", "years")[1],
  														followup.window.duration=12*2, followup.window.duration.unit=c("days", "weeks", "months", "years")[3],
  														observation.window.start=0, observation.window.start.unit=c("days", "weeks", "months", "years")[1],
  														observation.window.duration=12*2, observation.window.duration.unit=c("days", "weeks", "months", "years")[3],
  														date.format="%m/%d/%Y",
  														keep.window.start.end.dates=FALSE, remove.events.outside.followup.window=TRUE, keep.event.interval.for.all.events=FALSE,
  														parallel.backend=c("none","multicore","snow","snow(SOCK)","snow(MPI)","snow(NWS)")[1], parallel.threads="auto",
  														suppress.warnings=FALSE, return.data.table=FALSE);

  expect_is(d, "data.frame") # is a data.frame
  expect_equal(dim(d), c(1066,7)) # number of events and columns
  expect_equal(names(d), c("PATIENT_ID","DATE","PERDAY","CATEGORY","DURATION","event.interval","gap.days")) # the names of the columns
  # PATIENT_ID:
  expect_is(d$PATIENT_ID, "integer") # PATIENT_ID's are integers
  expect_equal(unique(d$PATIENT_ID), 1:100) # PATIENT_ID's are integers from 1 to 100
  expect_equal(as.numeric(table(d$PATIENT_ID)), c(24,8,28,6,8,9,6,14,9,14,6,7,5,24,12,22,9,6,6,11,10,6,11,9,10,12,13,10,14,11,3,6,4,11,11,11,8,9,9,11,6,7,20,7,10,18,10,12,6,9,6,7,4,5,5,11,4,16,18,7,10,8,6,11,19,20,14,8,6,9,11,6,9,7,13,11,6,24,22,19,7,9,8,12,9,13,10,14,6,12,6,18,7,11,11,22,12,8,5,16)) # the distribution of PATIENT_ID's
  expect_equal(d$PATIENT_ID[c(54,210,423,424,755,1050)], c(3,17,40,41,74,99)) # a selection of individual rows
  # DATE:
  expect_is(d$DATE, "character") # DATE's are characters
  expect_equal(digest(d$DATE,"md5",serialize=TRUE,ascii=TRUE), "378d1ede8d6699c84633c0ca5c757fc2") # digest is probably safe for characters
  expect_equal(d$DATE[c(54,210,423,424,755,1050)], c("11/10/2043", "07/07/2037", "06/22/2037", "04/11/2041", "08/03/2032", "08/01/2033")) # a selection of individual rows
  # PERDAY:
  expect_is(d$PERDAY, "integer") # PERDAY's are integers
  expect_equal(unique(d$PERDAY), c(4,2,8,6,20)) # PERDAY's are integers
  expect_equal(as.numeric(table(d$PERDAY)), c(374, 533, 123, 26, 10)) # the distribution of PERDAY's
  expect_equal(d$PERDAY[c(54,210,423,424,755,1050)], c(2, 4, 6, 4, 2, 4)) # a selection of individual rows
  # CATEGORY:
  expect_is(d$CATEGORY, "character") # CATEGORY's are characters
  expect_equal(digest(d$CATEGORY,"md5",serialize=TRUE,ascii=TRUE), "7411863b7ceb60f19f8a9b8480063a0a") # digest is probably safe for characters
  expect_equal(d$CATEGORY[c(54,210,423,424,755,1050)], c("medB", "medB", "medB", "medA", "medB", "medB")) # a selection of individual rows
  # DURATION:
  expect_is(d$DURATION, "integer") # DURATION's are integers
  expect_equal(unique(d$DURATION), c(50, 30, 60, 100, 20, 150)) # DURATION's are integers
  expect_equal(as.numeric(table(d$DURATION)), c(54, 404, 368, 131, 105, 4)) # the distribution of DURATION's
  expect_equal(d$DURATION[c(54,210,423,424,755,1050)], c(50, 60, 60, 50, 60, 30)) # a selection of individual rows
  # event.interval:
  expect_is(d$event.interval, "numeric") # event.interval's are numbers
  expect_equal(min(d$event.interval,na.rm=TRUE), 0) # event.interval's summaries
  expect_equal(max(d$event.interval,na.rm=TRUE), 544)
  expect_equal(sum(is.na(d$event.interval)), 0)
  expect_equal(round(mean(d$event.interval,na.rm=TRUE),5), 68.52908)
  expect_equal(round(median(d$event.interval,na.rm=TRUE),5), 48)
  expect_equal(round(sd(d$event.interval,na.rm=TRUE),5), 64.81414)
  expect_equal(sum(d$event.interval == 0,na.rm=TRUE), 3)
  expect_equal(sum(d$event.interval < 0,na.rm=TRUE), 0)
  expect_equal(sum(d$event.interval > 0,na.rm=TRUE), 1063)
  expect_equal(d$event.interval[c(54,210,423,424,755,1050)], c(2, 76, 63, 47, 49, 99)) # a selection of individual rows
  # gap.days
  expect_is(d$gap.days, "numeric") # gap.days's are numbers
  expect_equal(min(d$gap.days,na.rm=TRUE), 0) # gap.days's summaries
  expect_equal(max(d$gap.days,na.rm=TRUE), 491)
  expect_equal(sum(is.na(d$gap.days)), 0)
  expect_equal(round(mean(d$gap.days,na.rm=TRUE),5), 25.9803)
  expect_equal(round(median(d$gap.days,na.rm=TRUE),5), 0)
  expect_equal(round(sd(d$gap.days,na.rm=TRUE),5), 54.86816)
  expect_equal(sum(d$gap.days == 0,na.rm=TRUE), 666)
  expect_equal(sum(d$gap.days < 0,na.rm=TRUE), 0)
  expect_equal(sum(d$gap.days > 0,na.rm=TRUE), 400)
  expect_equal(d$gap.days[c(54,210,423,424,755,1050)], c(0, 16, 0, 0, 0, 69)) # a selection of individual rows
})

test_that("compute.event.int.gaps() on default values + window durations in years", {
  # compute the return value, to be used throughout:
  d <- compute.event.int.gaps(data=med.events,
  														ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION",
  														event.daily.dose.colname=NA, medication.class.colname=NA,
  														carryover.within.obs.window=FALSE, carryover.into.obs.window=FALSE, carry.only.for.same.medication=FALSE,
  														consider.dosage.change=FALSE,
  														followup.window.start=0, followup.window.start.unit=c("days", "weeks", "months", "years")[1],
  														followup.window.duration=1*2, followup.window.duration.unit=c("days", "weeks", "months", "years")[4],
  														observation.window.start=0, observation.window.start.unit=c("days", "weeks", "months", "years")[1],
  														observation.window.duration=1*2, observation.window.duration.unit=c("days", "weeks", "months", "years")[4],
  														date.format="%m/%d/%Y",
  														keep.window.start.end.dates=FALSE, remove.events.outside.followup.window=TRUE, keep.event.interval.for.all.events=FALSE,
  														parallel.backend=c("none","multicore","snow","snow(SOCK)","snow(MPI)","snow(NWS)")[1], parallel.threads="auto",
  														suppress.warnings=FALSE, return.data.table=FALSE);

  expect_is(d, "data.frame") # is a data.frame
  expect_equal(dim(d), c(1066,7)) # number of events and columns
  expect_equal(names(d), c("PATIENT_ID","DATE","PERDAY","CATEGORY","DURATION","event.interval","gap.days")) # the names of the columns
  # PATIENT_ID:
  expect_is(d$PATIENT_ID, "integer") # PATIENT_ID's are integers
  expect_equal(unique(d$PATIENT_ID), 1:100) # PATIENT_ID's are integers from 1 to 100
  expect_equal(as.numeric(table(d$PATIENT_ID)), c(24,8,28,6,8,9,6,14,9,14,6,7,5,24,12,22,9,6,6,11,10,6,11,9,10,12,13,10,14,11,3,6,4,11,11,11,8,9,9,11,6,7,20,7,10,18,10,12,6,9,6,7,4,5,5,11,4,16,18,7,10,8,6,11,19,20,14,8,6,9,11,6,9,7,13,11,6,24,22,19,7,9,8,12,9,13,10,14,6,12,6,18,7,11,11,22,12,8,5,16)) # the distribution of PATIENT_ID's
  expect_equal(d$PATIENT_ID[c(54,210,423,424,755,1050)], c(3,17,40,41,74,99)) # a selection of individual rows
  # DATE:
  expect_is(d$DATE, "character") # DATE's are characters
  expect_equal(digest(d$DATE,"md5",serialize=TRUE,ascii=TRUE), "378d1ede8d6699c84633c0ca5c757fc2") # digest is probably safe for characters
  expect_equal(d$DATE[c(54,210,423,424,755,1050)], c("11/10/2043", "07/07/2037", "06/22/2037", "04/11/2041", "08/03/2032", "08/01/2033")) # a selection of individual rows
  # PERDAY:
  expect_is(d$PERDAY, "integer") # PERDAY's are integers
  expect_equal(unique(d$PERDAY), c(4,2,8,6,20)) # PERDAY's are integers
  expect_equal(as.numeric(table(d$PERDAY)), c(374, 533, 123, 26, 10)) # the distribution of PERDAY's
  expect_equal(d$PERDAY[c(54,210,423,424,755,1050)], c(2, 4, 6, 4, 2, 4)) # a selection of individual rows
  # CATEGORY:
  expect_is(d$CATEGORY, "character") # CATEGORY's are characters
  expect_equal(digest(d$CATEGORY,"md5",serialize=TRUE,ascii=TRUE), "7411863b7ceb60f19f8a9b8480063a0a") # digest is probably safe for characters
  expect_equal(d$CATEGORY[c(54,210,423,424,755,1050)], c("medB", "medB", "medB", "medA", "medB", "medB")) # a selection of individual rows
  # DURATION:
  expect_is(d$DURATION, "integer") # DURATION's are integers
  expect_equal(unique(d$DURATION), c(50, 30, 60, 100, 20, 150)) # DURATION's are integers
  expect_equal(as.numeric(table(d$DURATION)), c(54, 404, 368, 131, 105, 4)) # the distribution of DURATION's
  expect_equal(d$DURATION[c(54,210,423,424,755,1050)], c(50, 60, 60, 50, 60, 30)) # a selection of individual rows
  # event.interval:
  expect_is(d$event.interval, "numeric") # event.interval's are numbers
  expect_equal(min(d$event.interval,na.rm=TRUE), 0) # event.interval's summaries
  expect_equal(max(d$event.interval,na.rm=TRUE), 544)
  expect_equal(sum(is.na(d$event.interval)), 0)
  expect_equal(round(mean(d$event.interval,na.rm=TRUE),5), 68.52908)
  expect_equal(round(median(d$event.interval,na.rm=TRUE),5), 48)
  expect_equal(round(sd(d$event.interval,na.rm=TRUE),5), 64.81414)
  expect_equal(sum(d$event.interval == 0,na.rm=TRUE), 3)
  expect_equal(sum(d$event.interval < 0,na.rm=TRUE), 0)
  expect_equal(sum(d$event.interval > 0,na.rm=TRUE), 1063)
  expect_equal(d$event.interval[c(54,210,423,424,755,1050)], c(2, 76, 63, 47, 49, 99)) # a selection of individual rows
  # gap.days
  expect_is(d$gap.days, "numeric") # gap.days's are numbers
  expect_equal(min(d$gap.days,na.rm=TRUE), 0) # gap.days's summaries
  expect_equal(max(d$gap.days,na.rm=TRUE), 491)
  expect_equal(sum(is.na(d$gap.days)), 0)
  expect_equal(round(mean(d$gap.days,na.rm=TRUE),5), 25.9803)
  expect_equal(round(median(d$gap.days,na.rm=TRUE),5), 0)
  expect_equal(round(sd(d$gap.days,na.rm=TRUE),5), 54.86816)
  expect_equal(sum(d$gap.days == 0,na.rm=TRUE), 666)
  expect_equal(sum(d$gap.days < 0,na.rm=TRUE), 0)
  expect_equal(sum(d$gap.days > 0,na.rm=TRUE), 400)
  expect_equal(d$gap.days[c(54,210,423,424,755,1050)], c(0, 16, 0, 0, 0, 69)) # a selection of individual rows
})

test_that("compute.event.int.gaps() on default values + smaller observation window", {
  # compute the return value, to be used throughout:
  d <- compute.event.int.gaps(data=med.events,
  														ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION",
  														event.daily.dose.colname=NA, medication.class.colname=NA,
  														carryover.within.obs.window=FALSE, carryover.into.obs.window=FALSE, carry.only.for.same.medication=FALSE,
  														consider.dosage.change=FALSE,
  														followup.window.start=0, followup.window.start.unit=c("days", "weeks", "months", "years")[1],
  														followup.window.duration=2, followup.window.duration.unit=c("days", "weeks", "months", "years")[4],
  														observation.window.start=0, observation.window.start.unit=c("days", "weeks", "months", "years")[1],
  														observation.window.duration=1.5, observation.window.duration.unit=c("days", "weeks", "months", "years")[4],
  														date.format="%m/%d/%Y",
  														keep.window.start.end.dates=FALSE, remove.events.outside.followup.window=TRUE, keep.event.interval.for.all.events=FALSE,
  														parallel.backend=c("none","multicore","snow","snow(SOCK)","snow(MPI)","snow(NWS)")[1], parallel.threads="auto",
  														suppress.warnings=FALSE, return.data.table=FALSE);

  expect_is(d, "data.frame") # is a data.frame
  expect_equal(dim(d), c(1066,7)) # number of events and columns
  expect_equal(names(d), c("PATIENT_ID","DATE","PERDAY","CATEGORY","DURATION","event.interval","gap.days")) # the names of the columns
  # PATIENT_ID:
  expect_is(d$PATIENT_ID, "integer") # PATIENT_ID's are integers
  expect_equal(unique(d$PATIENT_ID), 1:100) # PATIENT_ID's are integers from 1 to 100
  expect_equal(as.numeric(table(d$PATIENT_ID)), c(24,8,28,6,8,9,6,14,9,14,6,7,5,24,12,22,9,6,6,11,10,6,11,9,10,12,13,10,14,11,3,6,4,11,11,11,8,9,9,11,6,7,20,7,10,18,10,12,6,9,6,7,4,5,5,11,4,16,18,7,10,8,6,11,19,20,14,8,6,9,11,6,9,7,13,11,6,24,22,19,7,9,8,12,9,13,10,14,6,12,6,18,7,11,11,22,12,8,5,16)) # the distribution of PATIENT_ID's
  expect_equal(d$PATIENT_ID[c(54,210,423,424,755,1050)], c(3,17,40,41,74,99)) # a selection of individual rows
  # DATE:
  expect_is(d$DATE, "character") # DATE's are characters
  expect_equal(digest(d$DATE,"md5",serialize=TRUE,ascii=TRUE), "378d1ede8d6699c84633c0ca5c757fc2") # digest is probably safe for characters
  expect_equal(d$DATE[c(54,210,423,424,755,1050)], c("11/10/2043", "07/07/2037", "06/22/2037", "04/11/2041", "08/03/2032", "08/01/2033")) # a selection of individual rows
  # PERDAY:
  expect_is(d$PERDAY, "integer") # PERDAY's are integers
  expect_equal(unique(d$PERDAY), c(4,2,8,6,20)) # PERDAY's are integers
  expect_equal(as.numeric(table(d$PERDAY)), c(374, 533, 123, 26, 10)) # the distribution of PERDAY's
  expect_equal(d$PERDAY[c(54,210,423,424,755,1050)], c(2, 4, 6, 4, 2, 4)) # a selection of individual rows
  # CATEGORY:
  expect_is(d$CATEGORY, "character") # CATEGORY's are characters
  expect_equal(digest(d$CATEGORY,"md5",serialize=TRUE,ascii=TRUE), "7411863b7ceb60f19f8a9b8480063a0a") # digest is probably safe for characters
  expect_equal(d$CATEGORY[c(54,210,423,424,755,1050)], c("medB", "medB", "medB", "medA", "medB", "medB")) # a selection of individual rows
  # DURATION:
  expect_is(d$DURATION, "integer") # DURATION's are integers
  expect_equal(unique(d$DURATION), c(50, 30, 60, 100, 20, 150)) # DURATION's are integers
  expect_equal(as.numeric(table(d$DURATION)), c(54, 404, 368, 131, 105, 4)) # the distribution of DURATION's
  expect_equal(d$DURATION[c(54,210,423,424,755,1050)], c(50, 60, 60, 50, 60, 30)) # a selection of individual rows
  # event.interval:
  expect_is(d$event.interval, "numeric") # event.interval's are numbers
  expect_equal(min(d$event.interval,na.rm=TRUE), 0) # event.interval's summaries
  expect_equal(max(d$event.interval,na.rm=TRUE), 544)
  expect_equal(sum(is.na(d$event.interval)), 0)
  expect_equal(round(mean(d$event.interval,na.rm=TRUE),5), 68.52908)
  expect_equal(round(median(d$event.interval,na.rm=TRUE),5), 48)
  expect_equal(round(sd(d$event.interval,na.rm=TRUE),5), 64.81414)
  expect_equal(sum(d$event.interval == 0,na.rm=TRUE), 3)
  expect_equal(sum(d$event.interval < 0,na.rm=TRUE), 0)
  expect_equal(sum(d$event.interval > 0,na.rm=TRUE), 1063)
  expect_equal(d$event.interval[c(54,210,423,424,755,1050)], c(2, 76, 63, 47, 49, 99)) # a selection of individual rows
  # gap.days
  expect_is(d$gap.days, "numeric") # gap.days's are numbers
  expect_equal(min(d$gap.days,na.rm=TRUE), 0) # gap.days's summaries
  expect_equal(max(d$gap.days,na.rm=TRUE), 491)
  expect_equal(sum(is.na(d$gap.days)), 0)
  expect_equal(round(mean(d$gap.days,na.rm=TRUE),5), 25.9803)
  expect_equal(round(median(d$gap.days,na.rm=TRUE),5), 0)
  expect_equal(round(sd(d$gap.days,na.rm=TRUE),5), 54.86816)
  expect_equal(sum(d$gap.days == 0,na.rm=TRUE), 666)
  expect_equal(sum(d$gap.days < 0,na.rm=TRUE), 0)
  expect_equal(sum(d$gap.days > 0,na.rm=TRUE), 400)
  expect_equal(d$gap.days[c(54,210,423,424,755,1050)], c(0, 16, 0, 0, 0, 69)) # a selection of individual rows
})

test_that("compute.event.int.gaps() on default values + smaller follow-up window", {
  # compute the return value, to be used throughout:
  d <- compute.event.int.gaps(data=med.events,
  														ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION",
  														event.daily.dose.colname=NA, medication.class.colname=NA,
  														carryover.within.obs.window=FALSE, carryover.into.obs.window=FALSE, carry.only.for.same.medication=FALSE,
  														consider.dosage.change=FALSE,
  														followup.window.start=1, followup.window.start.unit=c("days", "weeks", "months", "years")[3],
  														followup.window.duration=1.5, followup.window.duration.unit=c("days", "weeks", "months", "years")[4],
  														observation.window.start=1, observation.window.start.unit=c("days", "weeks", "months", "years")[2],
  														observation.window.duration=12, observation.window.duration.unit=c("days", "weeks", "months", "years")[3],
  														date.format="%m/%d/%Y",
  														keep.window.start.end.dates=FALSE, remove.events.outside.followup.window=TRUE, keep.event.interval.for.all.events=FALSE,
  														parallel.backend=c("none","multicore","snow","snow(SOCK)","snow(MPI)","snow(NWS)")[1], parallel.threads="auto",
  														suppress.warnings=FALSE, return.data.table=FALSE);

  expect_is(d, "data.frame") # is a data.frame
  expect_equal(dim(d), c(968,7)) # number of events and columns
  expect_equal(names(d), c("PATIENT_ID","DATE","PERDAY","CATEGORY","DURATION","event.interval","gap.days")) # the names of the columns
  # PATIENT_ID:
  expect_is(d$PATIENT_ID, "integer") # PATIENT_ID's are integers
  expect_equal(unique(d$PATIENT_ID), 1:100) # PATIENT_ID's are integers from 1 to 100
  expect_equal(as.numeric(table(d$PATIENT_ID)), c(23,7,27,5,7,8,5,13,8,13,5,6,4,23,12,20,9,5,5,10,9,6,10,8,10,11,11,9,13,10,2,5,3,10,10,10,7,7,9,10,5,6,18,6,10,17,9,11,4,8,5,6,3,4,4,9,3,15,17,6,9,7,5,10,18,19,12,7,4,9,10,5,8,7,12,10,5,24,21,18,6,8,8,11,8,11,9,13,5,11,6,17,6,10,10,21,11,7,4,15)) # the distribution of PATIENT_ID's
  expect_equal(d$PATIENT_ID[c(54,210,423,424,755,950)], c(3,20,45,45,80,99)) # a selection of individual rows
  # DATE:
  expect_is(d$DATE, "character") # DATE's are characters
  expect_equal(digest(d$DATE,"md5",serialize=TRUE,ascii=TRUE), "257149af1f47ab31a870a60d488584d0") # digest is probably safe for characters
  expect_equal(d$DATE[c(54,210,423,424,755,950)], c("01/03/2044", "10/06/2036", "01/02/2043", "02/04/2043", "10/02/2041", "06/16/2032")) # a selection of individual rows
  # PERDAY:
  expect_is(d$PERDAY, "integer") # PERDAY's are integers
  expect_equal(unique(d$PERDAY), c(4,2,8,6,20)) # PERDAY's are integers
  expect_equal(as.numeric(table(d$PERDAY)), c(347, 477, 115, 21, 8)) # the distribution of PERDAY's
  expect_equal(d$PERDAY[c(54,210,423,424,755,950)], c(4, 4, 2, 2, 6, 4)) # a selection of individual rows
  # CATEGORY:
  expect_is(d$CATEGORY, "character") # CATEGORY's are characters
  expect_equal(digest(d$CATEGORY,"md5",serialize=TRUE,ascii=TRUE), "decb0fcd157a3df42e8f3e98ea958474") # digest is probably safe for characters
  expect_equal(d$CATEGORY[c(54,210,423,424,755,950)], c("medB", "medB", "medA", "medA", "medA", "medA")) # a selection of individual rows
  # DURATION:
  expect_is(d$DURATION, "integer") # DURATION's are integers
  expect_equal(unique(d$DURATION), c(30, 50, 60, 100, 20, 150)) # DURATION's are integers
  expect_equal(as.numeric(table(d$DURATION)), c(52, 399, 298, 133, 84, 2)) # the distribution of DURATION's
  expect_equal(d$DURATION[c(54,210,423,424,755,950)], c(30, 30, 50, 50, 50, 50)) # a selection of individual rows
  # event.interval:
  expect_is(d$event.interval, "numeric") # event.interval's are numbers
  expect_equal(min(d$event.interval,na.rm=TRUE), 0) # event.interval's summaries
  expect_equal(max(d$event.interval,na.rm=TRUE), 355)
  expect_equal(sum(is.na(d$event.interval)), 524)
  expect_equal(round(mean(d$event.interval,na.rm=TRUE),5), 62.93468)
  expect_equal(round(median(d$event.interval,na.rm=TRUE),5), 44)
  expect_equal(round(sd(d$event.interval,na.rm=TRUE),5), 55.54351)
  expect_equal(sum(d$event.interval == 0,na.rm=TRUE), 2)
  expect_equal(sum(d$event.interval < 0,na.rm=TRUE), 0)
  expect_equal(sum(d$event.interval > 0,na.rm=TRUE), 442)
  expect_equal(d$event.interval[c(54,210,423,424,755,950)], c(NA, NA, 33, 74, 42, 80)) # a selection of individual rows
  # gap.days
  expect_is(d$gap.days, "numeric") # gap.days's are numbers
  expect_equal(min(d$gap.days,na.rm=TRUE), 0) # gap.days's summaries
  expect_equal(max(d$gap.days,na.rm=TRUE), 305)
  expect_equal(sum(is.na(d$gap.days)), 524)
  expect_equal(round(mean(d$gap.days,na.rm=TRUE),5), 22.49099)
  expect_equal(round(median(d$gap.days,na.rm=TRUE),5), 0)
  expect_equal(round(sd(d$gap.days,na.rm=TRUE),5), 44.65395)
  expect_equal(sum(d$gap.days == 0,na.rm=TRUE), 257)
  expect_equal(sum(d$gap.days < 0,na.rm=TRUE), 0)
  expect_equal(sum(d$gap.days > 0,na.rm=TRUE), 187)
  expect_equal(d$gap.days[c(54,210,423,424,755,950)], c(NA, NA, 0, 0, 0, 30)) # a selection of individual rows
})

test_that("compute.event.int.gaps() on default values + observation and follow-up windows starts as Dates", {
  # compute the return value, to be used throughout:
  d <- compute.event.int.gaps(data=med.events,
  														ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION",
  														event.daily.dose.colname=NA, medication.class.colname=NA,
  														carryover.within.obs.window=FALSE, carryover.into.obs.window=FALSE, carry.only.for.same.medication=FALSE,
  														consider.dosage.change=FALSE,
  														followup.window.start=as.Date("01/01/2034",format="%d/%m/%Y"), followup.window.start.unit=c("days", "weeks", "months", "years")[3],
  														followup.window.duration=1.5, followup.window.duration.unit=c("days", "weeks", "months", "years")[4],
  														observation.window.start=as.Date("01/02/2034",format="%d/%m/%Y"), observation.window.start.unit=c("days", "weeks", "months", "years")[2],
  														observation.window.duration=12, observation.window.duration.unit=c("days", "weeks", "months", "years")[3],
  														date.format="%m/%d/%Y",
  														keep.window.start.end.dates=FALSE, remove.events.outside.followup.window=TRUE, keep.event.interval.for.all.events=FALSE,
  														parallel.backend=c("none","multicore","snow","snow(SOCK)","snow(MPI)","snow(NWS)")[1], parallel.threads="auto",
  														suppress.warnings=FALSE, return.data.table=FALSE);

  expect_is(d, "data.frame") # is a data.frame
  expect_equal(dim(d), c(156,7)) # number of events and columns
  expect_equal(names(d), c("PATIENT_ID","DATE","PERDAY","CATEGORY","DURATION","event.interval","gap.days")) # the names of the columns
  # PATIENT_ID:
  expect_is(d$PATIENT_ID, "integer") # PATIENT_ID's are integers
  expect_equal(unique(d$PATIENT_ID), c(1,4,8,9,11,15,17,18,20,31,32,34,40,43,48,52,56,57,60,63,64,65,67,69,76,77,79,81,84,86,87,89,98,100)) # PATIENT_ID's
  expect_equal(as.numeric(table(d$PATIENT_ID)), c(17,1,12,1,1,12,1,6,2,3,3,6,2,3,2,1,6,1,4,1,4,3,7,5,1,3,17,3,1,2,9,5,4,7)) # the distribution of PATIENT_ID's
  expect_equal(d$PATIENT_ID[c(3,21,46,47,121,155)], c(1, 8, 18, 18, 79, 100)) # a selection of individual rows
  # DATE:
  expect_is(d$DATE, "character") # DATE's are characters
  expect_equal(digest(d$DATE,"md5",serialize=TRUE,ascii=TRUE), "486ace9425b9b710be1144163d1a805b") # digest is probably safe for characters
  expect_equal(d$DATE[c(3,21,46,47,121,155)], c("03/08/2034", "07/17/2034", "03/16/2034", "08/07/2034", "12/23/2034", "05/26/2034")) # a selection of individual rows
  # PERDAY:
  expect_is(d$PERDAY, "integer") # PERDAY's are integers
  expect_equal(unique(d$PERDAY), c(4,2,6,8,20)) # PERDAY's are integers
  expect_equal(as.numeric(table(d$PERDAY)), c(42, 80, 27, 6, 1)) # the distribution of PERDAY's
  expect_equal(d$PERDAY[c(3,21,46,47,121,155)], c(4, 6, 2, 4, 4, 6)) # a selection of individual rows
  # CATEGORY:
  expect_is(d$CATEGORY, "character") # CATEGORY's are characters
  expect_equal(digest(d$CATEGORY,"md5",serialize=TRUE,ascii=TRUE), "22ebfc5ceaee9c486589791f6ff6412a") # digest is probably safe for characters
  expect_equal(d$CATEGORY[c(3,21,46,47,121,155)], c("medB", "medA", "medA", "medA", "medB", "medB")) # a selection of individual rows
  # DURATION:
  expect_is(d$DURATION, "integer") # DURATION's are integers
  expect_equal(unique(d$DURATION), c(30, 50, 60, 20, 100)) # DURATION's are integers
  expect_equal(as.numeric(table(d$DURATION)), c(12, 66, 60, 13, 5)) # the distribution of DURATION's
  expect_equal(d$DURATION[c(3,21,46,47,121,155)], c(30, 50, 60, 30, 30, 30)) # a selection of individual rows
  # event.interval:
  expect_is(d$event.interval, "numeric") # event.interval's are numbers
  expect_equal(min(d$event.interval,na.rm=TRUE), 3) # event.interval's summaries
  expect_equal(max(d$event.interval,na.rm=TRUE), 343)
  expect_equal(sum(is.na(d$event.interval)), 84)
  expect_equal(round(mean(d$event.interval,na.rm=TRUE),5), 77.91667)
  expect_equal(round(median(d$event.interval,na.rm=TRUE),5), 39.5)
  expect_equal(round(sd(d$event.interval,na.rm=TRUE),5), 90.52519)
  expect_equal(sum(d$event.interval == 0,na.rm=TRUE), 0)
  expect_equal(sum(d$event.interval < 0,na.rm=TRUE), 0)
  expect_equal(sum(d$event.interval > 0,na.rm=TRUE), 72)
  expect_equal(d$event.interval[c(3,21,46,47,121,155)], c(31, 17, 144, 178, 32, 44)) # a selection of individual rows
  # gap.days
  expect_is(d$gap.days, "numeric") # gap.days's are numbers
  expect_equal(min(d$gap.days,na.rm=TRUE), 0) # gap.days's summaries
  expect_equal(max(d$gap.days,na.rm=TRUE), 290)
  expect_equal(sum(is.na(d$gap.days)), 84)
  expect_equal(round(mean(d$gap.days,na.rm=TRUE),5), 44.81944)
  expect_equal(round(median(d$gap.days,na.rm=TRUE),5), 0)
  expect_equal(round(sd(d$gap.days,na.rm=TRUE),5), 83.63996)
  expect_equal(sum(d$gap.days == 0,na.rm=TRUE), 42)
  expect_equal(sum(d$gap.days < 0,na.rm=TRUE), 0)
  expect_equal(sum(d$gap.days > 0,na.rm=TRUE), 30)
  expect_equal(d$gap.days[c(3,21,46,47,121,155)], c(0, 0, 84, 148, 0, 3)) # a selection of individual rows
})






