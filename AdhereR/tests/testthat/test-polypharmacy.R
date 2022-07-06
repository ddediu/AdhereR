###############################################################################################
# Test Polypharmacy
###############################################################################################


context("Polypharmacy")

# The custom patient to be used throughout:
p <- read.table(text="
PATIENT_ID       DATE PERDAY CATEGORY GROUP DURATION FUW.DURATION OW.START OW.DURATION      IBDDx
1           2 2008-07-01      4        1     1       30         4216        0        4216 2007-12-11
2           2 2008-07-01      2        2     2       30         4216        0        4216 2007-12-11
3           2 2008-10-01      2        2     2       90         4276        0        4276 2007-12-11
4           2 2008-11-06      8        1     1       30         4216        0        4216 2007-12-11
5           2 2008-12-22      8        1     1       30         4216        0        4216 2007-12-11
6           2 2008-12-23      2        2     2       90         4276        0        4276 2007-12-11
7           2 2009-01-29      2        2     2       90         4276        0        4276 2007-12-11
8           2 2009-01-29      4        1     1       30         4216        0        4216 2007-12-11
9           2 2009-10-02      8        1     1       30         4216        0        4216 2007-12-11
10          2 2009-10-02      5        2     2       45         4231        0        4231 2007-12-11
11          2 2009-03-25      4        1     1       30         4216        0        4216 2007-12-11
12          2 2009-04-15      5        2     2       45         4231        0        4231 2007-12-11
13          2 2009-04-15      4        1     1       30         4216        0        4216 2007-12-11
14          2 2009-06-24      8        1     1       30         4216        0        4216 2007-12-11
15          2 2009-08-07      8        1     1       26         4212        0        4212 2007-12-11
16          2 2009-11-11      8        1     1       30         4216        0        4216 2007-12-11
17          2 2009-12-15      8        1     1       30         4216        0        4216 2007-12-11
18          2 2010-11-01      8        1     1       30         4216        0        4216 2007-12-11
19          2 2010-04-03      8        1     1       30         4216        0        4216 2007-12-11
20          2 2010-03-26      8        1     1       30         4216        0        4216 2007-12-11
21          2 2010-01-05      8        1     1       30         4216        0        4216 2007-12-11
22          2 2010-01-05      5        2     2       45         4231        0        4231 2007-12-11
23          2 2010-06-22      8        1     1       30         4216        0        4216 2007-12-11
24          2 2010-12-08      8        1     1       30         4216        0        4216 2007-12-11
25          2 2010-08-13      8        1     1       30         4216        0        4216 2007-12-11
26          2 2010-06-09      8        1     1       30         4216        0        4216 2007-12-11
27          2 2010-06-09      1        2     2       22         4208        0        4208 2007-12-11
28          2 2010-09-15      2        2     2       30         4216        0        4216 2007-12-11
29          2 2010-09-15      8        1     1       30         4216        0        4216 2007-12-11
30          2 2010-12-17      3        2     2       28         4214        0        4214 2007-12-11
31          2 2010-12-17      8        1     1       30         4216        0        4216 2007-12-11
32          2 2011-02-21      8        1     1       30         4216        0        4216 2007-12-11
33          2 2011-03-15      8        1     1       30         4216        0        4216 2007-12-11
34          2 2011-03-29      8        1     1       30         4216        0        4216 2007-12-11
35          2 2011-03-29      4        2     2       28         4214        0        4214 2007-12-11
36          2 2011-05-18      8        1     1       30         4216        0        4216 2007-12-11
37          2 2011-06-14      8        1     1       30         4216        0        4216 2007-12-11
38          2 2011-06-14      4        2     2        2         4188        0        4188 2007-12-11
39          2 2011-06-30     16        1     1       30         4216        0        4216 2007-12-11
40          2 2011-09-28     16        1     1       30         4216        0        4216 2007-12-11
41          2 2011-10-13      2        2     2       30         4216        0        4216 2007-12-11
42          2 2011-10-13     16        1     1       30         4216        0        4216 2007-12-11
43          2 2012-01-28      4        2     2       90         4276        0        4276 2007-12-11
44          2 2012-04-27      6        1     1       30         4216        0        4216 2007-12-11
45          2 2012-04-27      4        2     2       28         4214        0        4214 2007-12-11
46          2 2012-05-28      6        1     1       30         4216        0        4216 2007-12-11
47          2 2012-06-14      4        2     2       28         4214        0        4214 2007-12-11
48          2 2012-06-14      8        1     1       30         4216        0        4216 2007-12-11
49          2 2012-06-07      6        1     1       30         4216        0        4216 2007-12-11
50          2 2012-10-17      1        2     2       15         4201        0        4201 2007-12-11
51          2 2012-10-17      8        1     1       30         4216        0        4216 2007-12-11
52          2 2012-09-11      8        1     1       30         4216        0        4216 2007-12-11
53          2 2012-11-29      4        1     1       28         4214        0        4214 2007-12-11
54          2 2013-10-03      4        2     2       30         4216        0        4216 2007-12-11
55          2 2013-10-03      8        1     1       30         4216        0        4216 2007-12-11
56          2 2013-06-24      4        2     2       30         4216        0        4216 2007-12-11
57          2 2013-06-24      8        1     1       30         4216        0        4216 2007-12-11
58          2 2013-03-08      8        1     1       90         4276        0        4276 2007-12-11
59          2 2013-03-08      2        2     2       30         4216        0        4216 2007-12-11
60          2 2013-12-09      8        1     1       30         4216        0        4216 2007-12-11
61          2 2013-12-20      8        1     1       30         4216        0        4216 2007-12-11
62          2 2014-01-16      8        1     1       30         4216        0        4216 2007-12-11
63          2 2014-01-16      4        2     2       30         4216        0        4216 2007-12-11
64          2 2014-04-28      4        2     2       30         4216        0        4216 2007-12-11
65          2 2014-04-28      8        1     1       30         4216        0        4216 2007-12-11
66          2 2014-07-29      8        1     1       30         4216        0        4216 2007-12-11
67          2 2014-07-29      4        2     2       30         4216        0        4216 2007-12-11
68          2 2014-08-27      8        1     1       30         4216        0        4216 2007-12-11
69          2 2014-04-11      4        1     1       30         4216        0        4216 2007-12-11
70          2 2015-11-19      8        1     1       30         4216        0        4216 2007-12-11
71          2 2016-04-15      8        1     1       30         4216        0        4216 2007-12-11
72          2 2016-03-10      8        1     1       30         4216        0        4216 2007-12-11
73          2 2016-10-18      4        1     1       30         4216        0        4216 2007-12-11
74          2 2016-10-28      8        1     1       30         4216        0        4216 2007-12-11
75          2 2017-06-01      2        2     2       60         4246        0        4246 2007-12-11
76          2 2017-01-13      8        1     1       30         4216        0        4216 2007-12-11
77          2 2017-07-28      2        2     2       45         4231        0        4231 2007-12-11
78          2 2017-08-22      2        2     2       90         4276        0        4276 2007-12-11
79          2 2017-12-21      3        2     2       67         4253        0        4253 2007-12-11
80          2 2018-03-01      3        2     2       90         4276        0        4276 2007-12-11
81          2 2018-06-13      3        2     2       90         4276        0        4276 2007-12-11
82          2 2018-12-20      3        2     2       90         4276        0        4276 2007-12-11
83          2 2019-05-13      3        2     2       90         4276        0        4276 2007-12-11
84          2 2019-08-09      3        2     2       90         4276        0        4276 2007-12-11
85          2 2019-12-17      3        2     2       90         4276        0        4276 2007-12-11",
                header=TRUE);

treat.epi <- NULL; # the treatment episodes
p2 <- NULL; # pre-processed patient info

# Compute treatment episodes:
test_that("compute treatment episodes", {
  treat.epi <<- compute.treatment.episodes(p,
                                           ID.colname="GROUP",
                                           event.date.colname="DATE",
                                           event.duration.colname="DURATION",
                                           medication.class.colname="CATEGORY",
                                           event.daily.dose.colname="PERDAY",
                                           followup.window.duration = "FUW.DURATION",
                                           observation.window.start = "OW.START",
                                           observation.window.duration = "OW.DURATION",
                                           maximum.permissible.gap = 180,
                                           maximum.permissible.gap.unit = "days",
                                           carry.only.for.same.medication = TRUE,
                                           return.data.table = TRUE,
                                           date.format="%Y-%m-%d")

  expect_is(treat.epi, "data.table") # is a data.table
  expect_equal(names(treat.epi), c("GROUP", "episode.ID", "episode.start", "end.episode.gap.days", "episode.duration", "episode.end") )
  expect_equal(nrow(treat.epi), 4 )

  expect_is(treat.epi$GROUP, "integer")
  expect_is(treat.epi$episode.ID, "numeric")
  expect_is(treat.epi$episode.start, "Date")
  expect_is(treat.epi$end.episode.gap.days, "integer")
  expect_is(treat.epi$episode.duration, "numeric")
  expect_is(treat.epi$episode.end, "Date")

  expect_equal(treat.epi$GROUP, c(1,1,2,2) )
  expect_equal(treat.epi$episode.ID, c(1,2,1,2) )
  expect_equal(treat.epi$end.episode.gap.days, c(418, 1068, 1008, 0) )
  expect_equal(treat.epi$episode.duratio, c(2279, 451, 2249, 959) )
})


# Prepare treatment episodes for polypharmacy:
test_that("Prepare treatment episodes for polypharmacy", {
  # convert to data.table and make sure columns are of the right type:
  p <- data.table(p)
  p$DATE <- as.Date(p$DATE, format="%Y-%m-%d");
  p$IBDDx <- as.Date(p$IBDDx, format="%Y-%m-%d");

  # create new treatment group for each episode
  treat.epi[, new_group := paste0(GROUP, episode.ID)]

  # create new variable for joining
  treat.epi[, join_date := episode.end]
  p[, join_date := DATE]

  # key on variables for joining
  setkey(treat.epi, GROUP, join_date)
  setkey(p, GROUP, join_date)

  p.new <- treat.epi[p, roll = -Inf]

  # adjust OW.START and OW.DURATION
  p.new[, `:=` (OW.START = episode.start,
                OW.DURATION = episode.duration)]

  # drop unnecessary columns
  p.new[, c("episode.ID",
            "episode.start",
            "end.episode.gap.days",
            "episode.duration",
            "episode.end",
            "join_date"):=NULL]

  # save this p.new for later use:
  p2 <<- p.new

  # compute CMA 7 per medication (for testing purposes)
  cma7 <- CMA7(data = p.new,
               ID.colname="new_group",
               event.date.colname="DATE",
               event.duration.colname="DURATION",
               medication.class.colname="CATEGORY",
               event.daily.dose.colname="PERDAY",
               followup.window.duration = "FUW.DURATION",
               observation.window.start = "OW.START",
               observation.window.duration = "OW.DURATION",
               carry.only.for.same.medication = TRUE)

  expect_is(cma7, "CMA7")
  expect_is(getCMA(cma7), "data.frame")
  expect_equal(names(getCMA(cma7)), c("new_group", "CMA") )
  expect_equal(nrow(getCMA(cma7)), 4 )

  expect_is(getCMA(cma7)$new_group, "character")
  expect_is(getCMA(cma7)$CMA, "numeric")

  expect_equal(getCMA(cma7)$new_group, c("11", "12", "21", "22") )
  expect_equal(round(getCMA(cma7)$CMA,2), c(0.63, 0.40, 0.41, 0.77) )
})


# Polypharmacy: summarize before aggregate
test_that("Polypharmacy: summarize before aggregate", {
  PP_mean <- CMA_polypharmacy(data = p2,
                              medication.groups = "new_group",
                              CMA.to.apply = "CMA7",
                              aggregate.first = FALSE, # summarize before aggregation
                              aggregation.method = "mean", # compute mean of CMAs
                              aggregation.method.arguments = list(na.rm = TRUE), # remove NA's during calculation
                              thresholds = NA, # don't apply threshold
                              ID.colname="PATIENT_ID",
                              event.date.colname="DATE",
                              event.duration.colname="DURATION",
                              event.daily.dose.colname="PERDAY",
                              medication.class.colname="CATEGORY",
                              followup.window.start=0,
                              followup.window.duration = "FUW.DURATION",
                              observation.window.start = "OW.START",
                              observation.window.duration = "OW.DURATION",
                              carry.only.for.same.medication = TRUE)


  expect_is(PP_mean, "CMA_polypharmacy")
  expect_is(getCMA(PP_mean), "data.frame")
  expect_equal(names(getCMA(PP_mean)), c("PATIENT_ID", "CMA") )
  expect_equal(nrow(getCMA(PP_mean)), 1 )

  expect_is(getCMA(PP_mean)$PATIENT_ID, "integer")
  expect_is(getCMA(PP_mean)$CMA, "numeric")

  expect_equal(getCMA(PP_mean)$PATIENT_ID, c(2) )
  expect_equal(round(getCMA(PP_mean)$CMA,2), c(0.55) )

  expect_equal(names(PP_mean$CMA.intermediate), c("PATIENT_ID", "group", "CMA") )
  expect_equal(nrow(PP_mean$CMA.intermediate), 4 )

  expect_is(PP_mean$CMA.intermediate$PATIENT_ID, "integer")
  expect_is(PP_mean$CMA.intermediate$group, "character")
  expect_is(PP_mean$CMA.intermediate$CMA, "numeric")

  expect_equal(PP_mean$CMA.intermediate$PATIENT_ID, c(2, 2, 2, 2) )
  expect_equal(PP_mean$CMA.intermediate$group, c("11", "12", "21", "22") )
  expect_equal(round(PP_mean$CMA.intermediate$CMA,2), c(0.63, 0.40, 0.41, 0.77) )
})


# Polypharmacy: aggregate before summarize
test_that("Polypharmacy: aggregate before summarize", {
  PP_DPPR <- CMA_polypharmacy(data = p2,
                              medication.groups = "new_group",
                              CMA.to.apply = "CMA7",
                              aggregate.first = TRUE, # aggregate before summarizing
                              aggregation.method = "mean", # compute mean of CMAs
                              aggregation.method.arguments = list(na.rm = TRUE), # remove NA's during calculation
                              thresholds = NA, # don't apply threshold
                              ID.colname="PATIENT_ID",
                              event.date.colname="DATE",
                              event.duration.colname="DURATION",
                              event.daily.dose.colname="PERDAY",
                              medication.class.colname="CATEGORY",
                              followup.window.start=0,
                              followup.window.duration = "FUW.DURATION",
                              observation.window.start = "OW.START",
                              observation.window.duration = "OW.DURATION",
                              carry.only.for.same.medication = TRUE)


  expect_is(PP_DPPR, "CMA_polypharmacy")
  expect_is(getCMA(PP_DPPR), "data.frame")
  expect_equal(names(getCMA(PP_DPPR)), c("PATIENT_ID", "CMA") )
  expect_equal(nrow(getCMA(PP_DPPR)), 1 )

  expect_is(getCMA(PP_DPPR)$PATIENT_ID, "integer")
  expect_is(getCMA(PP_DPPR)$CMA, "numeric")

  expect_equal(getCMA(PP_DPPR)$PATIENT_ID, c(2) )
  expect_equal(round(getCMA(PP_DPPR)$CMA,2), c(0.50) )

  expect_equal(names(PP_DPPR$CMA.intermediate), c("PATIENT_ID", "intersect.start", "intersect.end", "intersect.ID", "intersect.duration", "prop.trt.groups.available", "11", "12", "21", "22") )
  expect_equal(nrow(PP_DPPR$CMA.intermediate), 106 )

  expect_is(PP_DPPR$CMA.intermediate$PATIENT_ID, "integer")
  expect_is(PP_DPPR$CMA.intermediate$intersect.start, "Date")
  expect_is(PP_DPPR$CMA.intermediate$intersect.end, "Date")
  expect_is(PP_DPPR$CMA.intermediate$intersect.ID, "integer")
  expect_is(PP_DPPR$CMA.intermediate$intersect.duration, "numeric")
  expect_is(PP_DPPR$CMA.intermediate$prop.trt.groups.available, "numeric")
  expect_is(PP_DPPR$CMA.intermediate$`11`, "numeric")
  expect_is(PP_DPPR$CMA.intermediate$`12`, "numeric")
  expect_is(PP_DPPR$CMA.intermediate$`21`, "numeric")
  expect_is(PP_DPPR$CMA.intermediate$`22`, "numeric")

  expect_equal(PP_DPPR$CMA.intermediate$PATIENT_ID[1], c(2) )
  expect_equal(PP_DPPR$CMA.intermediate$intersect.start[1], c(as.Date("2008-07-01")) )
  expect_equal(PP_DPPR$CMA.intermediate$intersect.end[1], c(as.Date("2008-07-31")) )
  expect_equal(PP_DPPR$CMA.intermediate$intersect.ID[1], c(1) )
  expect_equal(PP_DPPR$CMA.intermediate$intersect.duration[1], c(30) )
  expect_equal(PP_DPPR$CMA.intermediate$prop.trt.groups.available[1], c(1) )
  expect_equal(PP_DPPR$CMA.intermediate$`11`[1], c(1) )
  expect_equal(PP_DPPR$CMA.intermediate$`12`[1], c(NA_real_) )
  expect_equal(PP_DPPR$CMA.intermediate$`21`[1], c(1) )
  expect_equal(PP_DPPR$CMA.intermediate$`22`[1], c(NA_real_) )
})
