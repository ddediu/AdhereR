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
                     arguments.that.should.not.be.defined=c("overridden.argument1", "overridden.argument2"),
                     date.format="%m/%d/%Y"))
})

test_that("overridden arguments defined and given", {
  expect_warning(CMA0(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                     arguments.that.should.not.be.defined=c("overridden.argument1", "overridden.argument2"), overridden.argument1=32,
                     date.format="%m/%d/%Y"),
                 "Please note that 'CMA0' overrides argument 'overridden.argument1'!")
})

test_that("the actual overridden arguments given", {
  expect_warning(CMA1(data=med.events, ID.colname="PATIENT_ID", event.date.colname="DATE", event.duration.colname="DURATION", event.daily.dose.colname="PERDAY", medication.class.colname="CATEGORY",
                     date.format="%m/%d/%Y"),
                 "Please note that 'CMA1' overrides argument 'overridden.argument1'!")
})









