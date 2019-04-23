############################################################
#
# Test the computation of various CMAs
#
############################################################

context("CMAs")


# Test arguments that should not have been defined:
test_that("no overridden arguments", {
  expect_silent(CMA0(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                     arguments.that.should.not.be.defined=NULL,
                     date.format="%m/%d/%Y"))
})

test_that("overridden arguments defined but not given", {
  expect_silent(CMA0(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                     arguments.that.should.not.be.defined=c("overridden.argument1"=NA, "overridden.argument2"=NA),
                     date.format="%m/%d/%Y"))
})

test_that("overridden arguments defined and given", {
  expect_warning(CMA0(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      arguments.that.should.not.be.defined=c("overridden.argument1"=NA, "overridden.argument2"=NA), overridden.argument1=32,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA0' overrides argument 'overridden.argument1' with value 'NA'!")
})

test_that("the actual overridden arguments given for CMA1", {
  expect_warning(CMA1(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carryover.within.obs.window=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA1' overrides argument 'carryover.within.obs.window' with value 'FALSE'!")
  expect_warning(CMA1(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carryover.into.obs.window=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA1' overrides argument 'carryover.into.obs.window' with value 'FALSE'!")
  expect_warning(CMA1(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carry.only.for.same.medication=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA1' overrides argument 'carry.only.for.same.medication' with value 'FALSE'!")
  expect_warning(CMA1(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      consider.dosage.change=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA1' overrides argument 'consider.dosage.change' with value 'FALSE'!")
  expect_warning(CMA1(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carry.only.for.same.medication=TRUE, consider.dosage.change=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA1' overrides argument 'carry.only.for.same.medication' with value 'FALSE'!")
})

test_that("the actual overridden arguments given for CMA2", {
  expect_warning(CMA2(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carryover.within.obs.window=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA2' overrides argument 'carryover.within.obs.window' with value 'FALSE'!")
  expect_warning(CMA2(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carryover.into.obs.window=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA2' overrides argument 'carryover.into.obs.window' with value 'FALSE'!")
  expect_warning(CMA2(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carry.only.for.same.medication=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA2' overrides argument 'carry.only.for.same.medication' with value 'FALSE'!")
  expect_warning(CMA2(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      consider.dosage.change=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA2' overrides argument 'consider.dosage.change' with value 'FALSE'!")
  expect_warning(CMA2(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carry.only.for.same.medication=TRUE, consider.dosage.change=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA2' overrides argument 'carry.only.for.same.medication' with value 'FALSE'!")
})

test_that("the actual overridden arguments given for CMA3", {
  expect_warning(CMA3(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carryover.within.obs.window=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA3' overrides argument 'carryover.within.obs.window' with value 'FALSE'!")
  expect_warning(CMA3(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carryover.into.obs.window=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA3' overrides argument 'carryover.into.obs.window' with value 'FALSE'!")
  expect_warning(CMA3(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carry.only.for.same.medication=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA3' overrides argument 'carry.only.for.same.medication' with value 'FALSE'!")
  expect_warning(CMA3(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      consider.dosage.change=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA3' overrides argument 'consider.dosage.change' with value 'FALSE'!")
  expect_warning(CMA3(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carry.only.for.same.medication=TRUE, consider.dosage.change=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA3' overrides argument 'carry.only.for.same.medication' with value 'FALSE'!")
})

test_that("the actual overridden arguments given for CMA4", {
  expect_warning(CMA4(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carryover.within.obs.window=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA4' overrides argument 'carryover.within.obs.window' with value 'FALSE'!")
  expect_warning(CMA4(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carryover.into.obs.window=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA4' overrides argument 'carryover.into.obs.window' with value 'FALSE'!")
  expect_warning(CMA4(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carry.only.for.same.medication=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA4' overrides argument 'carry.only.for.same.medication' with value 'FALSE'!")
  expect_warning(CMA4(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      consider.dosage.change=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA4' overrides argument 'consider.dosage.change' with value 'FALSE'!")
  expect_warning(CMA4(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carry.only.for.same.medication=TRUE, consider.dosage.change=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA4' overrides argument 'carry.only.for.same.medication' with value 'FALSE'!")
})

test_that("the actual overridden arguments given for CMA5", {
  expect_warning(CMA5(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carryover.within.obs.window=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA5' overrides argument 'carryover.within.obs.window' with value 'TRUE'!")
  expect_warning(CMA5(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carryover.into.obs.window=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA5' overrides argument 'carryover.into.obs.window' with value 'FALSE'!")
  expect_silent(CMA5(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carry.only.for.same.medication=TRUE,
                      date.format="%m/%d/%Y"))
})

test_that("the actual overridden arguments given for CMA6", {
  expect_warning(CMA6(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carryover.within.obs.window=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA6' overrides argument 'carryover.within.obs.window' with value 'TRUE'!")
  expect_warning(CMA6(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carryover.into.obs.window=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA6' overrides argument 'carryover.into.obs.window' with value 'FALSE'!")
  expect_silent(CMA6(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carry.only.for.same.medication=TRUE,
                      date.format="%m/%d/%Y"))
})

test_that("the actual overridden arguments given for CMA7", {
  expect_warning(CMA7(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carryover.within.obs.window=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA7' overrides argument 'carryover.within.obs.window' with value 'TRUE'!")
  expect_warning(CMA7(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carryover.into.obs.window=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA7' overrides argument 'carryover.into.obs.window' with value 'TRUE'!")
  expect_silent(CMA7(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carry.only.for.same.medication=TRUE,
                      date.format="%m/%d/%Y"))
})

test_that("the actual overridden arguments given for CMA8", {
  expect_warning(CMA8(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carryover.within.obs.window=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA8' overrides argument 'carryover.within.obs.window' with value 'TRUE'!")
  expect_warning(CMA8(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carryover.into.obs.window=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA8' overrides argument 'carryover.into.obs.window' with value 'TRUE'!")
  expect_silent(CMA8(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carry.only.for.same.medication=TRUE,
                      date.format="%m/%d/%Y"))
})

test_that("the actual overridden arguments given for CMA9", {
  expect_warning(CMA9(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carryover.within.obs.window=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA9' overrides argument 'carryover.within.obs.window' with value 'TRUE'!")
  expect_warning(CMA9(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carryover.into.obs.window=TRUE,
                      date.format="%m/%d/%Y"),
                 "Please note that 'CMA9' overrides argument 'carryover.into.obs.window' with value 'TRUE'!")
  expect_silent(CMA9(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                      carry.only.for.same.medication=TRUE,
                      date.format="%m/%d/%Y"))
})









