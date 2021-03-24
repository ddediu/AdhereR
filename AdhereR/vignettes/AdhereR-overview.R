## ---- echo=FALSE, message=FALSE, warning=FALSE, results='hide'----------------
# Various Rmarkdown output options:
# center figures and reduce their file size:
knitr::opts_chunk$set(fig.align = "center", dpi=100, dev="jpeg"); 

## ---- echo=TRUE, results='asis'-----------------------------------------------
# Load the AdhereR library:
library(AdhereR);

# Select the two patients with IDs 37 and 76 from the built-in dataset "med.events":
ExamplePats <- med.events[med.events$PATIENT_ID %in% c(37, 76), ];
# Display them as pretty markdown table:
knitr::kable(ExamplePats, caption = "<a name=\"Table-1\"></a>**Table 1.** Medication events for two example patients");

## ---- echo=TRUE, fig.show='hold', fig.cap = "<a name=\"Figure-1\"></a>**Figure 1.** Medication histories - two example patients", fig.height=6, fig.width=8, out.width="100%"----
# Create an object "cma0" of the most basic CMA type, "CMA0":
cma0 <- CMA0(data=ExamplePats, # use the two selected patients
             ID.colname="PATIENT_ID", # the name of the column containing the IDs
             event.date.colname="DATE", # the name of the column containing the event date
             event.duration.colname="DURATION", # the name of the column containing the duration
             event.daily.dose.colname="PERDAY", # the name of the column containing the dosage
             medication.class.colname="CATEGORY", # the name of the column containing the category
             followup.window.start=0,  # FUW start in days since earliest event
             observation.window.start=182, # OW start in days since earliest event
             observation.window.duration=365, # OW duration in days
             date.format="%m/%d/%Y"); # date format (mm/dd/yyyy)
# Plot the object (CMA0 shows the actual event data only):
plot(cma0, # the object to plot
     align.all.patients=TRUE); # align all patients for easier comparison

## ---- echo=TRUE, fig.show='hold', fig.cap = "<a name=\"Figure-1a\"></a>**Figure 1 (a).** Same plot as in **Figure 1**, but also showing the daily doses as numbers and as line thickness", fig.height=6, fig.width=8, out.width="100%"----
# Same plot as above but also showing the daily doses:
plot(cma0, # the object to plot
     print.dose=TRUE, plot.dose=TRUE,
     align.all.patients=TRUE); # align all patients for easier comparison

## ---- echo=TRUE, results='asis'-----------------------------------------------
# Compute the treatment episodes for the two patients:
TEs3<- compute.treatment.episodes(ExamplePats,
                                  ID.colname="PATIENT_ID",
                                  event.date.colname="DATE",
                                  event.duration.colname="DURATION",
                                  event.daily.dose.colname="PERDAY",
                                  medication.class.colname="CATEGORY",
                                  carryover.within.obs.window = TRUE, # carry-over into the OW
                                  carry.only.for.same.medication = TRUE, # & only for same type
                                  consider.dosage.change = TRUE, # dosage change starts new episode...
                                  medication.change.means.new.treatment.episode = TRUE, # & type change
                                  maximum.permissible.gap = 180, # & a gap longer than 180 days
                                  maximum.permissible.gap.unit = "days", # unit for the above (days)
                                  followup.window.start = 0, # 2-years FUW starts at earliest event
                                  followup.window.start.unit = "days",
                                  followup.window.duration = 365 * 2,
                                  followup.window.duration.unit = "days",
                                  date.format = "%m/%d/%Y");
knitr::kable(TEs3, 
             caption = "<a name=\"Table-2\"></a>**Table 2.** Example output `compute.treatment.episodes()` function");

## ---- echo=TRUE, results='asis'-----------------------------------------------
# Compute the treatment episodes for the two patients
# but now a change in medication type does not start a new episode:
TEs4<- compute.treatment.episodes(ExamplePats,
                                  ID.colname="PATIENT_ID",
                                  event.date.colname="DATE",
                                  event.duration.colname="DURATION",
                                  event.daily.dose.colname="PERDAY",
                                  medication.class.colname="CATEGORY",
                                  carryover.within.obs.window = TRUE, 
                                  carry.only.for.same.medication = TRUE,
                                  consider.dosage.change = TRUE,
                                  medication.change.means.new.treatment.episode = FALSE, # here
                                  maximum.permissible.gap = 180,
                                  maximum.permissible.gap.unit = "days",
                                  followup.window.start = 0,
                                  followup.window.start.unit = "days",
                                  followup.window.duration = 365 * 2,
                                  followup.window.duration.unit = "days",
                                  date.format = "%m/%d/%Y");
# Pretty print the events:
knitr::kable(TEs4, 
             caption = "<a name=\"Table-3\"></a>**Table 3.** Alternative scenario output `compute.treatment.episodes()` function");

## ---- echo=TRUE, fig.show='hold', fig.cap = "<a name=\"Figure-2\"></a>**Figure 2.** Simple CMA 1", fig.height=5, fig.width=7, out.width="100%"----
# Create the CMA1 object with the given parameters:
cma1 <- CMA1(data=ExamplePats,
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             followup.window.start=0, observation.window.start=182, 
             observation.window.duration=365,
             date.format="%m/%d/%Y");
# Display the summary:
cma1
# Display the estimated CMA table:
cma1$CMA
# and equivalently using an accessor function:
getCMA(cma1);
# Compute the CMA value for patient 76, as percentage rounded at 2 digits:
round(cma1$CMA[cma1$CMA$PATIENT_ID== 76, 2]*100, 2)
# Plot the CMA:
# The legend shows the actual duration, the days covered and gap days, 
# the drug (medication) type, the FUW and OW, and the estimated CMA.
plot(cma1, 
     patients.to.plot=c("76"), # plot only patient 76 
     legend.x=520); # place the legend in a nice way

## ---- echo=TRUE, fig.show='hold', fig.cap = "<a name=\"Figure-3\"></a>**Figure 3.** Simple CMA 2", fig.height=5, fig.width=7, out.width="100%"----
cma2 <- CMA2(data=ExamplePats, # we're estimating CMA2 now!
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             followup.window.start=0, observation.window.start=182, 
             observation.window.duration=365,
             date.format="%m/%d/%Y");
plot(cma2, 
     patients.to.plot=c("76"),  
     show.legend=FALSE); # don't show legend to avoid clutter (see above)

## ---- echo=TRUE, fig.show='hold', fig.cap = "<a name=\"Figure-4\"></a>**Figure 4.** Simple CMA 3", fig.height=5, fig.width=7, out.width="100%"----
cma3 <- CMA3(data=ExamplePats, # we're estimating CMA3 now!
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             followup.window.start=0, observation.window.start=182, 
             observation.window.duration=365,
             date.format="%m/%d/%Y");
plot(cma3, patients.to.plot=c("76"), show.legend=FALSE);

## ---- echo=TRUE, fig.show='hold', fig.cap = "<a name=\"Figure-5\"></a>**Figure 5.** Simple CMA 4", fig.height=5, fig.width=7, out.width="100%"----
cma4 <- CMA4(data=ExamplePats, # we're estimating CMA4 now!
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             followup.window.start=0, observation.window.start=182, 
             observation.window.duration=365,
             date.format="%m/%d/%Y");
plot(cma4,patients.to.plot=c("76"), show.legend=FALSE);

## ---- echo=TRUE, fig.show='hold', fig.cap = "<a name=\"Figure-6\"></a>**Figure 6.** Simple CMA 5", fig.height=5, fig.width=7, out.width="100%"----
cma5 <- CMA5(data=ExamplePats, # we're estimating CMA5 now!
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             event.daily.dose.colname="PERDAY",
             medication.class.colname="CATEGORY",
             carry.only.for.same.medication=FALSE, # carry-over across medication types
             consider.dosage.change=FALSE, # don't consider canges in dosage
             followup.window.start=0, observation.window.start=182, 
             observation.window.duration=365,
             date.format="%m/%d/%Y");
plot(cma5,patients.to.plot=c("76"), show.legend=FALSE);

## ---- echo=TRUE, fig.show='hold', fig.cap = "<a name=\"Figure-7\"></a>**Figure 7.** Simple CMA 6", fig.height=5, fig.width=7, out.width="100%"----
cma6 <- CMA6(data=ExamplePats, # we're estimating CMA6 now!
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             event.daily.dose.colname="PERDAY",
             medication.class.colname="CATEGORY",
             carry.only.for.same.medication=FALSE,
             consider.dosage.change=FALSE,
             followup.window.start=0, observation.window.start=182, 
             observation.window.duration=365,
             date.format="%m/%d/%Y");
plot(cma6,patients.to.plot=c("76"), show.legend=FALSE);

## ---- echo=TRUE, fig.show='hold', fig.cap = "<a name=\"Figure-1a\"></a>**Figure 7 (a).** Same plot as in **Figure 7**, but also showing the daily doses as numbers and as line thickness", fig.height=6, fig.width=8, out.width="100%"----
# Same plot as above but also showing the daily doses:
plot(cma6, # the object to plot
     patients.to.plot=c("76"), # plot only patient 76 
     print.dose=TRUE, plot.dose=TRUE,
     legend.x=520); # place the legend in a nice way

## ---- echo=TRUE, fig.show='hold', fig.cap = "<a name=\"Figure-8\"></a>**Figure 8.** Simple CMA 7", fig.height=5, fig.width=7, out.width="100%"----
cma7 <- CMA7(data=ExamplePats, # we're estimating CMA7 now!
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             event.daily.dose.colname="PERDAY",
             medication.class.colname="CATEGORY",
             carry.only.for.same.medication=FALSE,
             consider.dosage.change=FALSE,
             followup.window.start=0, observation.window.start=182, 
             observation.window.duration=365,
             date.format="%m/%d/%Y");
plot(cma7, patients.to.plot=c("76"), show.legend=FALSE);

## ---- echo=TRUE, fig.show='hold', fig.cap = "<a name=\"Figure-9\"></a>**Figure 9.** Simple CMA 8", fig.height=5, fig.width=7, out.width="100%"----
cma8 <- CMA8(data=ExamplePats, # we're estimating CMA8 now!
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             event.daily.dose.colname="PERDAY",
             medication.class.colname="CATEGORY",
             carry.only.for.same.medication=FALSE,
             consider.dosage.change=FALSE,
             followup.window.start=0, observation.window.start=374, 
             observation.window.duration=294,
             date.format="%m/%d/%Y");
plot(cma8, patients.to.plot=c("76"), show.legend=FALSE);
# The value for patient 76, rounded at 2 digits
round(cma8$CMA[cma8$CMA$PATIENT_ID== 76, 2]*100, 2);

## ---- echo=TRUE, fig.show='hold', fig.cap = "<a name=\"Figure-10\"></a>**Figure 10.** Simple CMA 9", fig.height=5, fig.width=7, out.width="100%"----
cma9 <- CMA9(data=ExamplePats, # we're estimating CMA9 now!
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             event.daily.dose.colname="PERDAY",
             medication.class.colname="CATEGORY",
             carry.only.for.same.medication=FALSE,
             consider.dosage.change=FALSE,
             followup.window.start=0, observation.window.start=182, 
             observation.window.duration=365,
             date.format="%m/%d/%Y");
plot(cma9, patients.to.plot=c("76"), show.legend=FALSE);

## ---- echo=TRUE, fig.show='hold', fig.cap = "<a name=\"Figure-11\"></a>**Figure 11.** CMA 9 per episode", fig.height=5, fig.width=7, out.width="100%"----
cmaE <- CMA_per_episode(CMA="CMA9", # apply the simple CMA9 to each treatment episode
                        data=ExamplePats,
                        ID.colname="PATIENT_ID",
                        event.date.colname="DATE",
                        event.duration.colname="DURATION",
                        event.daily.dose.colname="PERDAY",
                        medication.class.colname="CATEGORY",
                        carryover.within.obs.window = TRUE,
                        carry.only.for.same.medication = FALSE,
                        consider.dosage.change = FALSE, # conditions on treatment episodes
                        medication.change.means.new.treatment.episode = TRUE,
                        maximum.permissible.gap = 180,
                        maximum.permissible.gap.unit = "days",
                        followup.window.start=0,
                        followup.window.start.unit = "days",
                        followup.window.duration = 365 * 2,
                        followup.window.duration.unit = "days",
                        observation.window.start=0,
                        observation.window.start.unit = "days",
                        observation.window.duration=365*2,
                        observation.window.duration.unit = "days",
                        date.format="%m/%d/%Y",
                        parallel.backend="none",
                        parallel.threads=1);
# Summary:
cmaE;
# The CMA estimates table:
cmaE$CMA
getCMA(cmaE); # as above but using accessor function
# The values for patient 76 only, rounded at 2 digits:
round(cmaE$CMA[cmaE$CMA$PATIENT_ID== 76, 7]*100, 2);
# Plot:
plot(cmaE, patients.to.plot=c("76"), show.legend=FALSE);

## ---- echo=TRUE, fig.show='hold', fig.cap = "<a name=\"Figure-12\"></a>**Figure 12.** Sliding window CMA 9", fig.height=5, fig.width=7, out.width="100%"----
cmaW <- CMA_sliding_window(CMA.to.apply="CMA9", # apply the simple CMA9 to each sliding window
                           data=ExamplePats,
                           ID.colname="PATIENT_ID",
                           event.date.colname="DATE",
                           event.duration.colname="DURATION",
                           event.daily.dose.colname="PERDAY",
                           medication.class.colname="CATEGORY",
                           carry.only.for.same.medication=FALSE,
                           consider.dosage.change=FALSE,
                           followup.window.start=0,
                           observation.window.start=0,
                           observation.window.duration=365*2,
                           sliding.window.start=0, # sliding windows definition
                           sliding.window.start.unit="days",
                           sliding.window.duration=120,
                           sliding.window.duration.unit="days",
                           sliding.window.step.duration=120,
                           sliding.window.step.unit="days",
                           date.format="%m/%d/%Y",
                           parallel.backend="none",
                           parallel.threads=1);
# Summary:
cmaW;
# The CMA estimates table:
cmaW$CMA
getCMA(cmaW); # as above but using accessor function
# The values for patient 76 only, rounded at 2 digits
round(cmaW$CMA[cmaW$CMA$PATIENT_ID== 76, 5]*100, 2);
# Plot:
plot(cmaW, patients.to.plot=c("76"), show.legend=FALSE);

## ---- echo=FALSE, fig.show='hold', fig.cap = "<a name=\"Figure-13\"></a>**Figure 13.** Sliding window CMA 9", fig.height=5, fig.width=7, out.width="100%"----
cmaW1 <- CMA_sliding_window(CMA.to.apply="CMA9",
                           data=ExamplePats,
                           ID.colname="PATIENT_ID",
                           event.date.colname="DATE",
                           event.duration.colname="DURATION",
                           event.daily.dose.colname="PERDAY",
                           medication.class.colname="CATEGORY",
                           carry.only.for.same.medication=FALSE,
                           consider.dosage.change=FALSE,
                           followup.window.start=0,
                           observation.window.start=0,
                           observation.window.duration=365*2,
                           sliding.window.start=0, # different sliding windows
                           sliding.window.start.unit="days",
                           sliding.window.duration=120,
                           sliding.window.duration.unit="days",
                           sliding.window.step.duration=30,
                           sliding.window.step.unit="days",
                           date.format="%m/%d/%Y",
                           parallel.backend="none",
                           parallel.threads=1);
# Plot:
plot(cmaW1, patients.to.plot=c("76"), show.legend=FALSE);

## ---- echo=FALSE--------------------------------------------------------------
knitr::kable(head(med.events.ATC, n=5), row.names=FALSE,
             caption="First 5 lines of the `med.events.ATC` dataset.");

## ---- eval=FALSE--------------------------------------------------------------
#  event_durations <- compute_event_durations(disp.data = durcomp.dispensing,
#                                             presc.data = durcomp.prescribing,
#                                             special.periods.data = durcomp.hospitalisation,
#                                             ID.colname = "ID",
#                                             presc.date.colname = "DATE.PRESC",
#                                             disp.date.colname = "DATE.DISP",
#                                             medication.class.colnames = c("ATC.CODE", "UNIT", "FORM"),
#                                             total.dose.colname = "TOTAL.DOSE",
#                                             presc.daily.dose.colname = "DAILY.DOSE",
#                                             presc.duration.colname = "PRESC.DURATION",
#                                             visit.colname = "VISIT",
#                                             split.on.dosage.change = TRUE,
#                                             force.init.presc = TRUE,
#                                             force.presc.renew = TRUE,
#                                             trt.interruption = "continue",
#                                             special.periods.method = "continue",
#                                             date.format = "%Y-%m-%d",
#                                             suppress.warnings = FALSE,
#                                             return.data.table = FALSE);
#  med.events.ATC <- event_durations$event_durations[ !is.na(event_durations$event_durations$DURATION) &
#                                                       event_durations$event_durations$DURATION > 0,
#                                           c("ID", "DISP.START", "DURATION", "DAILY.DOSE", "ATC.CODE")];
#  names(med.events.ATC) <- c("PATIENT_ID", "DATE", "DURATION", "PERDAY", "CATEGORY");
#  # Groups from the ATC codes:
#  sort(unique(med.events.ATC$CATEGORY)); # all the ATC codes in the data
#  # Level 1:
#  med.events.ATC$CATEGORY_L1 <- vapply(substr(med.events.ATC$CATEGORY,1,1), switch, character(1),
#                                       "A"="ALIMENTARY TRACT AND METABOLISM",
#                                       "B"="BLOOD AND BLOOD FORMING ORGANS",
#                                       "J"="ANTIINFECTIVES FOR SYSTEMIC USE",
#                                       "R"="RESPIRATORY SYSTEM",
#                                       "OTHER");
#  # Level 2:
#  med.events.ATC$CATEGORY_L2 <- vapply(substr(med.events.ATC$CATEGORY,1,3), switch, character(1),
#                                       "A02"="DRUGS FOR ACID RELATED DISORDERS",
#                                       "A05"="BILE AND LIVER THERAPY",
#                                       "A09"="DIGESTIVES, INCL. ENZYMES",
#                                       "A10"="DRUGS USED IN DIABETES",
#                                       "A11"="VITAMINS",
#                                       "A12"="MINERAL SUPPLEMENTS",
#                                       "B02"="ANTIHEMORRHAGICS",
#                                       "J01"="ANTIBACTERIALS FOR SYSTEMIC USE",
#                                       "J02"="ANTIMYCOTICS FOR SYSTEMIC USE",
#                                       "R03"="DRUGS FOR OBSTRUCTIVE AIRWAY DISEASES",
#                                       "R05"="COUGH AND COLD PREPARATIONS",
#                                       "OTHER");

## ----eval=FALSE---------------------------------------------------------------
#  med.groups <- c("Vitamins"  = "(CATEGORY_L2 == 'VITAMINS')",
#                  "VitaResp"  = "({Vitamins} | CATEGORY_L1 == 'RESPIRATORY SYSTEM')",
#                  "VitaShort" = "({Vitamins} & DURATION <= 30)",
#                  "VitELow"   = "(CATEGORY == 'A11HA03' & PERDAY <= 500)",
#                  "VitaComb"  = "({VitaShort} | {VitELow})",
#                  "NotVita"   = "(!{Vitamins})");

## ---- echo=FALSE--------------------------------------------------------------
knitr::kable(head(med.events.ATC[med.events.ATC$CATEGORY_L2 == 'VITAMINS',], n=5), row.names=FALSE,
             caption="First 5 events in `med.events.ATC` corresponding to the *Vitamins* medication group.");

## ---- echo=FALSE--------------------------------------------------------------
knitr::kable(head(med.events.ATC[med.events.ATC$CATEGORY_L2 == 'VITAMINS' | med.events.ATC$CATEGORY_L1 == 'RESPIRATORY SYSTEM',], n=5), row.names=FALSE,
             caption="First 5 events in `med.events.ATC` corresponding to the *VitaResp* medication group.");

## -----------------------------------------------------------------------------
cma1_mg <- CMA1(data=med.events.ATC,
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
cma1_mg; # print it

## ----echo=FALSE---------------------------------------------------------------
getMGs(cma1_mg)$def;

## ----echo=FALSE---------------------------------------------------------------
head(getMGs(cma1_mg)$obs, n=10);

## -----------------------------------------------------------------------------
getCMA(cma1_mg);

## -----------------------------------------------------------------------------
getCMA(cma1_mg, flatten.medication.groups=TRUE);

## ----echo=TRUE, message=FALSE, warning=FALSE, fig.show='hold', fig.cap = "<a name=\"Figure-14\"></a>**Figure 14.** CMA 1 with medication groups.", fig.height=9, fig.width=9, out.width="100%"----
plot(cma1_mg,
     #medication.groups.to.plot=c("VitaResp", "VitaShort", "VitaComb"),
     patients.to.plot=1:3,
     show.legend=FALSE);

## ----eval=FALSE---------------------------------------------------------------
#  cma0_types <- CMA0(data=med.events.ATC,
#                     ID.colname="PATIENT_ID",
#                     event.date.colname="DATE",
#                     event.duration.colname="DURATION",
#                     event.daily.dose.colname="PERDAY",
#                     medication.class.colname="CATEGORY",
#                     medication.groups="CATEGORY_L1",
#                     #flatten.medication.groups=TRUE,
#                     #followup.window.start.per.medication.group=TRUE,
#                     followup.window.start=0,
#                     observation.window.start=0,
#                     observation.window.duration=365,
#                     date.format="%m/%d/%Y");

## ----echo=FALSE---------------------------------------------------------------
tmp <- sort(unique(med.events.ATC$CATEGORY_L1));
cat(paste0("c(",
           paste0('"',tmp,'"',' = "(CATEGORY_L1 == \'',tmp,'\')"',collapse=",\n  "),
           ");"));

## ----eval=FALSE---------------------------------------------------------------
#  cmaW3 <- CMA_sliding_window(CMA="CMA1",
#                              data=med.events,
#                              ID.colname="PATIENT_ID",
#                              event.date.colname="DATE",
#                              event.duration.colname="DURATION",
#                              event.daily.dose.colname="PERDAY",
#                              medication.class.colname="CATEGORY",
#                              carry.only.for.same.medication=FALSE,
#                              consider.dosage.change=FALSE,
#                              sliding.window.duration=30,
#                              sliding.window.step.duration=30,
#                              parallel.backend="snow", # make clear we want to use snow
#                              parallel.threads=c(rep(
#                                list(list(host="user@workhorse", # host (and user)
#                                          rscript="/usr/local/bin/Rscript", # Rscript location
#                                          snowlib="/usr/local/lib64/R/library/") # snow package location
#                                     ),
#                                2))) # two remote threads

