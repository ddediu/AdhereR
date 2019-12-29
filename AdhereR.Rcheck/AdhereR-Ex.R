pkgname <- "AdhereR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "AdhereR-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('AdhereR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("CMA0")
### * CMA0

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CMA0
### Title: CMA0 constructor.
### Aliases: CMA0

### ** Examples

cma0 <- CMA0(data=med.events,
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             event.daily.dose.colname="PERDAY",
             medication.class.colname="CATEGORY",
             followup.window.start=0,
             followup.window.start.unit="days",
             followup.window.duration=2*365,
             followup.window.duration.unit="days",
             observation.window.start=30,
             observation.window.start.unit="days",
             observation.window.duration=365,
             observation.window.duration.unit="days",
             date.format="%m/%d/%Y",
             summary="Base CMA");



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CMA0", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CMA1")
### * CMA1

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CMA1
### Title: CMA1 and CMA3 constructors.
### Aliases: CMA1 CMA3

### ** Examples

cma1 <- CMA1(data=med.events,
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             followup.window.start=30,
             observation.window.start=30,
             observation.window.duration=365,
             date.format="%m/%d/%Y"
            );
cma3 <- CMA3(data=med.events,
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             followup.window.start=30,
             observation.window.start=30,
             observation.window.duration=365,
             date.format="%m/%d/%Y"
            );



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CMA1", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CMA2")
### * CMA2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CMA2
### Title: CMA2 and CMA4 constructors.
### Aliases: CMA2 CMA4

### ** Examples

## Not run: 
##D cma2 <- CMA2(data=med.events,
##D              ID.colname="PATIENT_ID",
##D              event.date.colname="DATE",
##D              event.duration.colname="DURATION",
##D              followup.window.start=30,
##D              observation.window.start=30,
##D              observation.window.duration=365,
##D              date.format="%m/%d/%Y"
##D             );
##D cma4 <- CMA4(data=med.events,
##D              ID.colname="PATIENT_ID",
##D              event.date.colname="DATE",
##D              event.duration.colname="DURATION",
##D              followup.window.start=30,
##D              observation.window.start=30,
##D              observation.window.duration=365,
##D              date.format="%m/%d/%Y"
##D             );
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CMA2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CMA5")
### * CMA5

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CMA5
### Title: CMA5 constructor.
### Aliases: CMA5

### ** Examples

cma5 <- CMA5(data=med.events,
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             event.daily.dose.colname="PERDAY",
             medication.class.colname="CATEGORY",
             carry.only.for.same.medication=FALSE,
             consider.dosage.change=FALSE,
             followup.window.start=30,
             observation.window.start=30,
             observation.window.duration=365,
             date.format="%m/%d/%Y"
            );



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CMA5", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CMA6")
### * CMA6

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CMA6
### Title: CMA6 constructor.
### Aliases: CMA6

### ** Examples

cma6 <- CMA6(data=med.events,
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             event.daily.dose.colname="PERDAY",
             medication.class.colname="CATEGORY",
             carry.only.for.same.medication=FALSE,
             consider.dosage.change=FALSE,
             followup.window.start=30,
             observation.window.start=30,
             observation.window.duration=365,
             date.format="%m/%d/%Y"
            );



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CMA6", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CMA7")
### * CMA7

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CMA7
### Title: CMA7 constructor.
### Aliases: CMA7

### ** Examples

cma7 <- CMA7(data=med.events,
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             event.daily.dose.colname="PERDAY",
             medication.class.colname="CATEGORY",
             carry.only.for.same.medication=FALSE,
             consider.dosage.change=FALSE,
             followup.window.start=30,
             observation.window.start=30,
             observation.window.duration=365,
             date.format="%m/%d/%Y"
            );



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CMA7", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CMA8")
### * CMA8

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CMA8
### Title: CMA8 constructor.
### Aliases: CMA8

### ** Examples

cma8 <- CMA8(data=med.events,
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             event.daily.dose.colname="PERDAY",
             medication.class.colname="CATEGORY",
             carry.only.for.same.medication=FALSE,
             consider.dosage.change=FALSE,
             followup.window.start=30,
             observation.window.start=30,
             observation.window.duration=365,
             date.format="%m/%d/%Y"
            );



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CMA8", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CMA9")
### * CMA9

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CMA9
### Title: CMA9 constructor.
### Aliases: CMA9

### ** Examples

cma9 <- CMA9(data=med.events,
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             event.daily.dose.colname="PERDAY",
             medication.class.colname="CATEGORY",
             carry.only.for.same.medication=FALSE,
             consider.dosage.change=FALSE,
             followup.window.start=30,
             observation.window.start=30,
             observation.window.duration=365,
             date.format="%m/%d/%Y"
            );



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CMA9", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CMA_per_episode")
### * CMA_per_episode

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CMA_per_episode
### Title: CMA_per_episode constructor.
### Aliases: CMA_per_episode

### ** Examples

## Not run: 
##D cmaE <- CMA_per_episode(CMA="CMA1",
##D                         data=med.events,
##D                         ID.colname="PATIENT_ID",
##D                         event.date.colname="DATE",
##D                         event.duration.colname="DURATION",
##D                         event.daily.dose.colname="PERDAY",
##D                         medication.class.colname="CATEGORY",
##D                         carry.only.for.same.medication=FALSE,
##D                         consider.dosage.change=FALSE,
##D                         followup.window.start=0,
##D                         observation.window.start=0,
##D                         observation.window.duration=365,
##D                         date.format="%m/%d/%Y"
##D                        );
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CMA_per_episode", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CMA_polypharmacy")
### * CMA_polypharmacy

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CMA_polypharmacy
### Title: CMA constructor for polypharmacy.
### Aliases: CMA_polypharmacy

### ** Examples

## Not run: 
##D CMA_PP <- CMA_polypharmacy(data = med.events.pp,
##D medication.groups = med.groups,
##D CMA.to.apply = "CMA7", 
##D aggregate.first = TRUE, # aggregate before summarizing
##D aggregation.method = "mean", # compute mean of CMAs
##D aggregation.method.arguments = list(na.rm = TRUE), # remove NA's during calculation
##D thresholds = NA, # don't apply threshold
##D ID.colname="PATIENT_ID",
##D event.date.colname="DATE",
##D event.duration.colname="DURATION",
##D event.daily.dose.colname="PERDAY",
##D medication.class.colname="CATEGORY",
##D followup.window.start=0,
##D observation.window.start=180,
##D observation.window.duration=365,
##D carry.only.for.same.medication = TRUE);
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CMA_polypharmacy", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CMA_sliding_window")
### * CMA_sliding_window

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CMA_sliding_window
### Title: CMA_sliding_window constructor.
### Aliases: CMA_sliding_window

### ** Examples

## Not run: 
##D cmaW <- CMA_sliding_window(CMA="CMA1",
##D                            data=med.events,
##D                            ID.colname="PATIENT_ID",
##D                            event.date.colname="DATE",
##D                            event.duration.colname="DURATION",
##D                            event.daily.dose.colname="PERDAY",
##D                            medication.class.colname="CATEGORY",
##D                            carry.only.for.same.medication=FALSE,
##D                            consider.dosage.change=FALSE,
##D                            followup.window.start=0,
##D                            observation.window.start=0,
##D                            observation.window.duration=365,
##D                            sliding.window.start=0,
##D                            sliding.window.start.unit="days",
##D                            sliding.window.duration=90,
##D                            sliding.window.duration.unit="days",
##D                            sliding.window.step.duration=7,
##D                            sliding.window.step.unit="days",
##D                            sliding.window.no.steps=NA,
##D                            date.format="%m/%d/%Y"
##D                           );
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CMA_sliding_window", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("compute_event_durations")
### * compute_event_durations

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: compute_event_durations
### Title: Computation of event durations.
### Aliases: compute_event_durations

### ** Examples

## Not run: 
##D event_durations <- compute_event_durations(disp.data = durcomp.dispensing,
##D                                            presc.data = durcomp.prescribing,
##D                                            special.periods.data = durcomp.hospitalisation,
##D                                            ID.colname = "ID",
##D                                            presc.date.colname = "DATE.PRESC",
##D                                            disp.date.colname = "DATE.DISP",
##D                                            medication.class.colnames = c("ATC.CODE",
##D                                            "UNIT", "FORM"),
##D                                            total.dose.colname = "TOTAL.DOSE",
##D                                            presc.daily.dose.colname = "DAILY.DOSE",
##D                                            presc.duration.colname = "PRESC.DURATION",
##D                                            visit.colname = "VISIT",
##D                                            split.on.dosage.change = TRUE,
##D                                            force.init.presc = TRUE,
##D                                            force.presc.renew = TRUE,
##D                                            trt.interruption = "continue",
##D                                            special.periods.method = "continue",
##D                                            date.format = "%Y-%m-%d",
##D                                            suppress.warnings = FALSE,
##D                                            return.data.table = TRUE);
##D        
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("compute_event_durations", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("cover_special_periods")
### * cover_special_periods

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: cover_special_periods
### Title: Cover special periods.
### Aliases: cover_special_periods

### ** Examples

## Not run: 
##D # select medication class of interest and compute event durations
##D disp_data <- durcomp.dispensing[ID == 3 & grepl("J01EE01", ATC.CODE)]
##D presc_data <- durcomp.prescribing[ID == 3 & grepl("J01EE01", ATC.CODE)]
##D 
##D event_durations_list <- compute_event_durations(disp.data = disp_data,
##D                                                 presc.data = presc_data,
##D                                                 special.periods.data = durcomp.hospitalisation,
##D                                                 special.periods.method = "carryover",
##D                                                 ID.colname = "ID",
##D                                                 presc.date.colname = "DATE.PRESC",
##D                                                 disp.date.colname = "DATE.DISP",
##D                                                 date.format = "%Y-%m-%d",
##D                                                 medication.class.colnames = c("ATC.CODE",
##D                                                                               "UNIT",
##D                                                                               "FORM"),
##D                                                 total.dose.colname = "TOTAL.DOSE",
##D                                                 presc.daily.dose.colname = "DAILY.DOSE",
##D                                                 presc.duration.colname = "PRESC.DURATION",
##D                                                 visit.colname = "VISIT",
##D                                                 force.init.presc = TRUE,
##D                                                 force.presc.renew = TRUE,
##D                                                 split.on.dosage.change = TRUE,
##D                                                 trt.interruption = "carryover",
##D                                                 suppress.warnings = FALSE,
##D                                                 return.data.table = TRUE,
##D                                                 progress.bar = FALSE)
##D 
##D event_durations <- prune_event_durations(event_durations_list,
##D                                          include = c("special periods"),
##D                                          medication.class.colnames = "ATC.CODE",
##D                                          days.within.out.date.1 = 7,
##D                                          days.within.out.date.2 = 30,
##D                                          keep.all = TRUE)
##D 
##D # cover special periods
##D special_periods <- event_durations_list$special_periods
##D event_durations_covered <- cover_special_periods(events.data = event_durations,
##D                                                  special.periods.data = special_periods,
##D                                                  ID.colname = "ID",
##D                                                  medication.class.colnames = "ATC.CODE",
##D                                                  disp.start.colname = "DISP.START",
##D                                                  duration.colname = "DURATION",
##D                                                  days.before = 7,
##D                                                  days.after = 7,
##D                                                  date.format = "%Y-%m-%d")
##D        
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("cover_special_periods", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get.plotted.events")
### * get.plotted.events

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get.plotted.events
### Title: Get info about the plotted events.
### Aliases: get.plotted.events

### ** Examples

cma7 <- CMA7(data=med.events[med.events$PATIENT_ID %in% c(1,2),],
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             event.daily.dose.colname="PERDAY",
             medication.class.colname="CATEGORY",
             followup.window.start=0,
             followup.window.start.unit="days",
             followup.window.duration=2*365,
             followup.window.duration.unit="days",
             observation.window.start=30,
             observation.window.start.unit="days",
             observation.window.duration=365,
             observation.window.duration.unit="days",
             date.format="%m/%d/%Y",
             summary="Base CMA");
plot(cma7);
tmp <- get.plotted.events();
head(tmp);
# "Mask" the first event:
rect(tmp$.X.START[1], tmp$.Y.START[1]-0.5, tmp$.X.END[1], tmp$.Y.END[1]+0.5,
     col=adjustcolor("white",alpha.f=0.75), border="black");
# "Mask" the first patient's summary CMA:
rect(tmp$.X.SCMA.START[1], tmp$.Y.SCMA.START[1],
     tmp$.X.SCMA.END[1], tmp$.Y.SCMA.END[1],
     col=adjustcolor("white",alpha.f=0.75), border="black");



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get.plotted.events", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getCMA")
### * getCMA

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getCMA
### Title: Access the actual CMA estimate from a CMA object.
### Aliases: getCMA

### ** Examples

cma1 <- CMA1(data=med.events,
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             followup.window.start=30,
             observation.window.start=30,
             observation.window.duration=365,
             date.format="%m/%d/%Y"
            );
getCMA(cma1);
## Not run: 
##D cmaE <- CMA_per_episode(CMA="CMA1",
##D                         data=med.events,
##D                         ID.colname="PATIENT_ID",
##D                         event.date.colname="DATE",
##D                         event.duration.colname="DURATION",
##D                         event.daily.dose.colname="PERDAY",
##D                         medication.class.colname="CATEGORY",
##D                         carry.only.for.same.medication=FALSE,
##D                         consider.dosage.change=FALSE,
##D                         followup.window.start=0,
##D                         observation.window.start=0,
##D                         observation.window.duration=365,
##D                         date.format="%m/%d/%Y"
##D                        );
##D getCMA(cmaE);
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getCMA", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("last.plot.get.info")
### * last.plot.get.info

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: last.plot.get.info
### Title: Access last adherence plot info.
### Aliases: last.plot.get.info

### ** Examples

cma7 <- CMA7(data=med.events[med.events$PATIENT_ID %in% c(1,2),],
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             event.daily.dose.colname="PERDAY",
             medication.class.colname="CATEGORY",
             followup.window.start=0,
             followup.window.start.unit="days",
             followup.window.duration=2*365,
             followup.window.duration.unit="days",
             observation.window.start=30,
             observation.window.start.unit="days",
             observation.window.duration=365,
             observation.window.duration.unit="days",
             date.format="%m/%d/%Y",
             summary="Base CMA");
plot(cma7);
tmp <- last.plot.get.info();
names(tmp);
tmp$baseR$legend$box; # legend position and size
head(tmp$baseR$cma$data); # events + plotting info
# Add a transparent blue rect between days 270 and 900:
rect(tmp$baseR$.map.event.x(270), tmp$baseR$.map.event.y(1-0.5),
     tmp$baseR$.map.event.x(900), tmp$baseR$.map.event.y(nrow(tmp$baseR$cma$data)+0.5),
     col=adjustcolor("blue",alpha.f=0.5), border="blue");
# Add a transparent rect rect between dates 03/15/2036 and 03/15/2037:
rect(tmp$baseR$.map.event.date(as.Date("03/15/2036", format="%m/%d/%Y")),
     tmp$baseR$.map.event.y(1-0.5),
     tmp$baseR$.map.event.date(as.Date("03/15/2037", format="%m/%d/%Y")),
     tmp$baseR$.map.event.y(nrow(tmp$baseR$cma$data)+0.5),
     col=adjustcolor("red",alpha.f=0.5), border="blue");



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("last.plot.get.info", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("map.event.coords.to.plot")
### * map.event.coords.to.plot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: map.event.coords.to.plot
### Title: Map from event to plot coordinates.
### Aliases: map.event.coords.to.plot

### ** Examples

cma7 <- CMA7(data=med.events[med.events$PATIENT_ID %in% c(1,2),],
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             event.daily.dose.colname="PERDAY",
             medication.class.colname="CATEGORY",
             followup.window.start=0,
             followup.window.start.unit="days",
             followup.window.duration=2*365,
             followup.window.duration.unit="days",
             observation.window.start=30,
             observation.window.start.unit="days",
             observation.window.duration=365,
             observation.window.duration.unit="days",
             date.format="%m/%d/%Y",
             summary="Base CMA");
plot(cma7);
# Add a transparent blue rect:
rect(map.event.coords.to.plot(x=270),
     get.event.plotting.area()["y.min"]-1,
     map.event.coords.to.plot(x="03/15/2037", x.is.Date=TRUE, x.date.format="%m/%d/%Y"),
     get.event.plotting.area()["y.max"]+1,
     col=adjustcolor("blue",alpha.f=0.5), border="blue");



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("map.event.coords.to.plot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.CMA0")
### * plot.CMA0

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.CMA0
### Title: Plot CMA0 objects.
### Aliases: plot.CMA0

### ** Examples

cma0 <- CMA0(data=med.events,
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             event.daily.dose.colname="PERDAY",
             medication.class.colname="CATEGORY",
             followup.window.start=0,
             followup.window.start.unit="days",
             followup.window.duration=2*365,
             followup.window.duration.unit="days",
             observation.window.start=30,
             observation.window.start.unit="days",
             observation.window.duration=365,
             observation.window.duration.unit="days",
             date.format="%m/%d/%Y",
             summary="Base CMA");
plot(cma0, patients.to.plot=c("1","2"));



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.CMA0", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.CMA1")
### * plot.CMA1

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.CMA1
### Title: Plot CMA0-derived objects.
### Aliases: plot.CMA1 plot.CMA2 plot.CMA3 plot.CMA4 plot.CMA5 plot.CMA6
###   plot.CMA7 plot.CMA8 plot.CMA9

### ** Examples

cma1 <- CMA1(data=med.events,
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             followup.window.start=30,
             observation.window.start=30,
             observation.window.duration=365,
             date.format="%m/%d/%Y"
            );
plot(cma1, patients.to.plot=c("1","2"));



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.CMA1", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.CMA_per_episode")
### * plot.CMA_per_episode

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.CMA_per_episode
### Title: Plot CMA_per_episode and CMA_sliding_window objects.
### Aliases: plot.CMA_per_episode plot.CMA_sliding_window

### ** Examples

## Not run: 
##D cmaW <- CMA_sliding_window(CMA=CMA1,
##D                         data=med.events,
##D                         ID.colname="PATIENT_ID",
##D                         event.date.colname="DATE",
##D                         event.duration.colname="DURATION",
##D                         event.daily.dose.colname="PERDAY",
##D                         medication.class.colname="CATEGORY",
##D                         carry.only.for.same.medication=FALSE,
##D                         consider.dosage.change=FALSE,
##D                         followup.window.start=0,
##D                         observation.window.start=0,
##D                         observation.window.duration=365,
##D                         sliding.window.start=0,
##D                         sliding.window.start.unit="days",
##D                         sliding.window.duration=90,
##D                         sliding.window.duration.unit="days",
##D                         sliding.window.step.duration=7,
##D                         sliding.window.step.unit="days",
##D                         sliding.window.no.steps=NA,
##D                         date.format="%m/%d/%Y"
##D                        );
##D plot(cmaW, patients.to.plot=c("1","2"));
##D cmaE <- CMA_per_episode(CMA=CMA1,
##D                         data=med.events,
##D                         ID.colname="PATIENT_ID",
##D                         event.date.colname="DATE",
##D                         event.duration.colname="DURATION",
##D                         event.daily.dose.colname="PERDAY",
##D                         medication.class.colname="CATEGORY",
##D                         carry.only.for.same.medication=FALSE,
##D                         consider.dosage.change=FALSE,
##D                         followup.window.start=0,
##D                         observation.window.start=0,
##D                         observation.window.duration=365,
##D                         date.format="%m/%d/%Y"
##D                        );
##D plot(cmaE, patients.to.plot=c("1","2"));
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.CMA_per_episode", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_interactive_cma")
### * plot_interactive_cma

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_interactive_cma
### Title: Interactive exploration and CMA computation.
### Aliases: plot_interactive_cma

### ** Examples

## Not run: 
##D plot_interactive_cma(med.events,
##D                      ID.colname="PATIENT_ID",
##D                      event.date.colname="DATE",
##D                      event.duration.colname="DURATION",
##D                      event.daily.dose.colname="PERDAY",
##D                      medication.class.colname="CATEGORY");
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_interactive_cma", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("print.CMA0")
### * print.CMA0

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: print.CMA0
### Title: Print CMA0 (and derived) objects.
### Aliases: print.CMA0 print.CMA1 print.CMA2 print.CMA3 print.CMA4
###   print.CMA5 print.CMA6 print.CMA7 print.CMA8 print.CMA9
###   print.CMA_per_episode print.CMA_sliding_window

### ** Examples

cma0 <- CMA0(data=med.events,
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             event.daily.dose.colname="PERDAY",
             medication.class.colname="CATEGORY",
             followup.window.start=0,
             followup.window.start.unit="days",
             followup.window.duration=2*365,
             followup.window.duration.unit="days",
             observation.window.start=30,
             observation.window.start.unit="days",
             observation.window.duration=365,
             observation.window.duration.unit="days",
             date.format="%m/%d/%Y",
             summary="Base CMA");
cma0;
print(cma0, format="markdown");
cma1 <- CMA1(data=med.events,
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             followup.window.start=30,
             observation.window.start=30,
             observation.window.duration=365,
             date.format="%m/%d/%Y"
            );
cma1;



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("print.CMA0", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("prune_event_durations")
### * prune_event_durations

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: prune_event_durations
### Title: Prune event durations.
### Aliases: prune_event_durations

### ** Examples

## Not run: 
##D # select medication class of interest and compute event durations
##D 
##D disp_data <- durcomp.dispensing[ID == 3 & grepl("J01EE01", ATC.CODE)]
##D presc_data <- durcomp.prescribing[ID == 3 & grepl("J01EE01", ATC.CODE)]
##D 
##D # compute event durations
##D event_durations_list <- compute_event_durations(disp.data = disp_data,
##D                                                 presc.data = presc_data,
##D                                                 special.periods.data = durcomp.hospitalisation,
##D                                                 ID.colname = "ID",
##D                                                 presc.date.colname = "DATE.PRESC",
##D                                                 disp.date.colname = "DATE.DISP",
##D                                                 date.format = "%Y-%m-%d",
##D                                                 medication.class.colnames = c("ATC.CODE",
##D                                                                               "UNIT",
##D                                                                               "FORM"),
##D                                                 total.dose.colname = "TOTAL.DOSE",
##D                                                 presc.daily.dose.colname = "DAILY.DOSE",
##D                                                 presc.duration.colname = "PRESC.DURATION",
##D                                                 visit.colname = "VISIT",
##D                                                 force.init.presc = TRUE,
##D                                                 force.presc.renew = TRUE,
##D                                                 split.on.dosage.change = TRUE,
##D                                                 trt.interruption = "carryover",
##D                                                 special.periods.method = "carryover",
##D                                                 suppress.warnings = FALSE,
##D                                                 return.data.table = TRUE,
##D                                                 progress.bar = FALSE)
##D 
##D # prune event durations
##D event_durations <- prune_event_durations(event_durations_list,
##D                                          include = c("special periods"),
##D                                          medication.class.colnames = "ATC.CODE",
##D                                          days.within.out.date.1 = 7,
##D                                          days.within.out.date.2 = 30,
##D                                          keep.all = FALSE)
##D        
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("prune_event_durations", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("time_to_initiation")
### * time_to_initiation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: time_to_initiation
### Title: Computation of initiation times.
### Aliases: time_to_initiation

### ** Examples

time_init <- time_to_initiation(presc.data = durcomp.prescribing,
                                disp.data = durcomp.dispensing,
                                ID.colname = "ID",
                                medication.class.colnames = c("ATC.CODE", "FORM", "UNIT"),
                                presc.start.colname = "DATE.PRESC",
                                disp.date.colname = "DATE.DISP",
                                date.format = "%Y-%m-%d",
                                suppress.warnings = FALSE,
                                return.data.table = TRUE);



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("time_to_initiation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
