############################################################
#
# Test the functions dealing with date & time manipulation
#
############################################################

context("Date & time")

# rubbish input:
test_that("wrong start.date", {
  expect_warning(as.character(.add.time.interval.to.date("2001/01/01",1,"days"), format="%Y/%m/%d"))
  expect_warning(as.character(.add.time.interval.to.date(NA,1,"days"), format="%Y/%m/%d"))
})

test_that("wrong time.interval", {
  expect_warning(as.character(.add.time.interval.to.date(as.Date("2001/01/01",format="%Y/%m/%d"),"1","days"), format="%Y/%m/%d"))
  expect_warning(as.character(.add.time.interval.to.date(as.Date("2001/01/01",format="%Y/%m/%d"),-1,"days"), format="%Y/%m/%d"))
  expect_warning(as.character(.add.time.interval.to.date(as.Date("2001/01/01",format="%Y/%m/%d"),NA,"days"), format="%Y/%m/%d"))
})

test_that("wrong unit", {
  expect_warning(as.character(.add.time.interval.to.date(as.Date("2001/01/01",format="%Y/%m/%d"),1,"centuries"), format="%Y/%m/%d"))
  expect_warning(as.character(.add.time.interval.to.date(as.Date("2001/01/01",format="%Y/%m/%d"),1,NA), format="%Y/%m/%d"))
})

test_that("suppress.warnings", {
  expect_warning(as.character(.add.time.interval.to.date(as.Date("2001/01/01",format="%Y/%m/%d"),1,"centuries",suppress.warnings=FALSE), format="%Y/%m/%d"))
  expect_equal(is.na(.add.time.interval.to.date(as.Date("2001/01/01",format="%Y/%m/%d"),1,"centuries",suppress.warnings=TRUE)), TRUE)
})



# days:
test_that("add zero days to date", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date("2001/01/01",format="%Y/%m/%d"),0,"days"), format="%Y/%m/%d"), "2001/01/01")
})

test_that("add one day to date", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date("2001/01/01",format="%Y/%m/%d"),1,"days"), format="%Y/%m/%d"), "2001/01/02")
})

test_that("try to add a negative day", {
  expect_warning(is.na(.add.time.interval.to.date(as.Date("2001/01/01",format="%Y/%m/%d"),-1,"days")))
})

test_that("add a day at the end of the year", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date("2001/12/31",format="%Y/%m/%d"),1,"days"), format="%Y/%m/%d"), "2002/01/01")
})

test_that("add a day at the end of February in a normal year", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date("2001/02/28",format="%Y/%m/%d"),1,"days"), format="%Y/%m/%d"), "2001/03/01")
})

test_that("add a day at on 28 February in a leap year", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date("2000/02/28",format="%Y/%m/%d"),1,"days"), format="%Y/%m/%d"), "2000/02/29")
})

test_that("add a day at on 29 February in a leap year", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date("2000/02/29",format="%Y/%m/%d"),1,"days"), format="%Y/%m/%d"), "2000/03/01")
})

test_that("add a day at on 29 February in a leap year", {
  expect_equal(is.na(.add.time.interval.to.date(as.Date("2000/13/01",format="%Y/%m/%d"),1,"days")), TRUE)
})


# vectors (using days)
test_that("add a day to two dates", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date(c("2001/01/01","2001/01/02"),format="%Y/%m/%d"),1,"days"), format="%Y/%m/%d"), c("2001/01/02","2001/01/03"))
})

test_that("add two days to one date", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date("2001/01/01",format="%Y/%m/%d"),c(1,2),"days"), format="%Y/%m/%d"), c("2001/01/02","2001/01/03"))
})

test_that("add two days to two dates", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date(c("2001/01/01","2001/01/02"),format="%Y/%m/%d"),c(1,2),"days"), format="%Y/%m/%d"), c("2001/01/02","2001/01/04"))
})

test_that("add three days to two dates", {
  expect_warning(as.character(.add.time.interval.to.date(as.Date(c("2001/01/01","2001/01/02"),format="%Y/%m/%d"),c(1,2,3),"days"), format="%Y/%m/%d"))
})


# weeks
test_that("add a week", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date("2001/01/01",format="%Y/%m/%d"),1,"weeks"), format="%Y/%m/%d"), "2001/01/08")
})

test_that("add a week across month boundaries", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date("2001/01/28",format="%Y/%m/%d"),1,"weeks"), format="%Y/%m/%d"), "2001/02/04")
})

test_that("add a week across month boundaries in February for a normal year", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date("2001/02/25",format="%Y/%m/%d"),1,"weeks"), format="%Y/%m/%d"), "2001/03/04")
})

test_that("add a week across month boundaries in February for a leap year", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date("2000/02/25",format="%Y/%m/%d"),1,"weeks"), format="%Y/%m/%d"), "2000/03/03")
})

test_that("add a week across year boundaries", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date("2001/12/28",format="%Y/%m/%d"),1,"weeks"), format="%Y/%m/%d"), "2002/01/04")
})


# months
test_that("add a month to a 31-days month", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date("2001/01/10",format="%Y/%m/%d"),1,"months"), format="%Y/%m/%d"), "2001/02/10")
})

test_that("add a month to a 30-days month", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date("2001/04/10",format="%Y/%m/%d"),1,"months"), format="%Y/%m/%d"), "2001/05/10")
})

test_that("add a month to February in a normal year", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date("2001/02/10",format="%Y/%m/%d"),1,"months"), format="%Y/%m/%d"), "2001/03/10")
})

test_that("add a month to February in a leap year", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date("2000/02/10",format="%Y/%m/%d"),1,"months"), format="%Y/%m/%d"), "2000/03/10")
})

test_that("add a month to 29 January in a normal year", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date("2001/01/29",format="%Y/%m/%d"),1,"months"), format="%Y/%m/%d"), "2001/03/01")
})

test_that("add a month to 29 January in a leap year", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date("2000/01/29",format="%Y/%m/%d"),1,"months"), format="%Y/%m/%d"), "2000/02/29")
})

test_that("add a month to 29 January in a leap and a normal year", {
  expect_equal(as.character(.add.time.interval.to.date(c(as.Date("2000/01/29",format="%Y/%m/%d"), as.Date("2001/01/29",format="%Y/%m/%d")),1,"months"), format="%Y/%m/%d"), c("2000/02/29", "2001/03/01"))
})


# years
test_that("add a year to February in a normal year towards a normal year", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date("2001/02/10",format="%Y/%m/%d"),1,"years"), format="%Y/%m/%d"), "2002/02/10")
})

test_that("add a year to February in a leap year towards a normal year", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date("2000/02/10",format="%Y/%m/%d"),1,"years"), format="%Y/%m/%d"), "2001/02/10")
})

test_that("add a year to February in a normal year towards a leap year", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date("2003/02/10",format="%Y/%m/%d"),1,"years"), format="%Y/%m/%d"), "2004/02/10")
})

test_that("add a year to 28 February in a normal year towards a leap year", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date("2003/02/28",format="%Y/%m/%d"),1,"years"), format="%Y/%m/%d"), "2004/02/28")
})

test_that("add a year to 29 February in a leap year towards a normal year", {
  expect_equal(as.character(.add.time.interval.to.date(as.Date("2000/02/29",format="%Y/%m/%d"),1,"years"), format="%Y/%m/%d"), "2001/03/01")
})

test_that("add a year to 29 February in a leap year towards a normal year and a normal year towards a leap year", {
  expect_equal(as.character(.add.time.interval.to.date(c(as.Date("2000/02/29",format="%Y/%m/%d"), as.Date("2003/02/28",format="%Y/%m/%d")),1,"years"), format="%Y/%m/%d"), c("2001/03/01", "2004/02/28"))
})


# fast subtract dates
test_that("subtract wrong input dates", {
  expect_warning(.difftime.Dates.as.days("2001/02/10", as.Date("2001/01/10",format="%Y/%m/%d")))
  expect_warning(.difftime.Dates.as.days(as.Date("2001/02/10",format="%Y/%m/%d"), "2001/01/10"))
  expect_equal(is.na(.difftime.Dates.as.days(as.Date("2002/01/10",format="%Y/%m/%d"), as.Date("2001/02/29",format="%Y/%m/%d"))), TRUE)
})

test_that("subtract ok", {
  expect_equal(.difftime.Dates.as.days(as.Date("2001/02/10",format="%Y/%m/%d"), as.Date("2001/01/10",format="%Y/%m/%d")), 31)
})

test_that("subtract across leap year", {
  expect_equal(.difftime.Dates.as.days(as.Date("2001/01/10",format="%Y/%m/%d"), as.Date("2000/01/10",format="%Y/%m/%d")), 366)
})

test_that("subtract across normal year", {
  expect_equal(.difftime.Dates.as.days(as.Date("2002/01/10",format="%Y/%m/%d"), as.Date("2001/01/10",format="%Y/%m/%d")), 365)
})



