## ---- echo=FALSE, message=FALSE, warning=FALSE, results='hide'-----------
# Various Rmarkdown output options:
# center figures and reduce their file size:
knitr::opts_chunk$set(fig.align = "center", dpi=100, dev="jpeg"); 

## ----eval=FALSE----------------------------------------------------------
#  plot_interactive_cma()

## ----eval=FALSE----------------------------------------------------------
#  plot_interactive_cma(data=med.events, # included sample dataset
#                       cma.class="simple", # simple cma, defaults to CMA0
#                       # The important column names:
#                       ID.colname="PATIENT_ID",
#                       event.date.colname="DATE",
#                       event.duration.colname="DURATION",
#                       event.daily.dose.colname="PERDAY",
#                       medication.class.colname="CATEGORY",
#                       # The format of dates in the "DATE" column:
#                       date.format="%m/%d/%Y");

## ----eval=FALSE----------------------------------------------------------
#  # The R code corresponding to the currently displayed Shiny plot:
#  #
#  # Extract the data for the selected 2 patient(s) with ID(s):
#  # "1", "2"
#  #
#  # We denote here by DATA the data you are using in the Shiny plot.
#  # This was manually defined as an object of class data.frame
#  # (or derived from it, such a data.table) that was already in
#  # memory under the name 'med.events'.
#  # Assuming this object still exists with the same name, then:
#  
#  DATA <- med.events;
#  
#  # These data has 5 columns, and contains info for 100 patients.
#  #
#  # To allow using data from other sources than a "data.frame"
#  # and other similar structures (for example, from a remote SQL
#  # database), we use a metchanism to request the data for the
#  # selected patients that uses a function called
#  # "get.data.for.patients.fnc()" which you may have redefined
#  # to better suit your case (chances are, however, that you are
#  # using its default version appropriate to the data source);
#  # in any case, the following is its definition:
#  get.data.for.patients.fnc <- function(patientid, d, idcol, cols=NA, maxrows=NA) d[ d[[idcol]] %in% patientid, ]
#  # Try to extract the data only for the selected patient ID(s):
#  .data.for.selected.patients. <- get.data.for.patients.fnc(
#      c("1", "2"),
#      DATA, ### don't forget to put here your REAL DATA! ###
#      "PATIENT_ID"
#  );
#  # Compute the appropriate CMA:
#  cma <- CMA9(data=.data.for.selected.patients.,
#              # (please note that even if some parameters are
#              # not relevant for a particular CMA type, we
#              # nevertheless pass them as they will be ignored)
#              ID.colname="PATIENT_ID",
#              event.date.colname="DATE",
#              event.duration.colname="DURATION",
#              event.daily.dose.colname="PERDAY",
#              medication.class.colname="CATEGORY",
#              carry.only.for.same.medication=FALSE,
#              consider.dosage.change=FALSE,
#              followup.window.start=0,
#              followup.window.start.unit="days",
#              followup.window.duration=730,
#              followup.window.duration.unit="days",
#              observation.window.start=0,
#              observation.window.start.unit="days",
#              observation.window.duration=730,
#              observation.window.duration.unit="days",
#              date.format="%m/%d/%Y"
#             );
#  
#  if( !is.null(cma) ) # if the CMA was computed ok
#  {
#      # Try to plot it:
#      plot(cma,
#           # (same idea as for CMA: we send arguments even if
#           # they aren't used in a particular case)
#           align.all.patients=FALSE,
#           align.first.event.at.zero=FALSE,
#           show.legend=TRUE,
#           legend.x="right",
#           legend.y="bottom",
#           legend.bkg.opacity=0.5,
#           legend.cex=0.75,
#           legend.cex.title=1,
#           duration=NA,
#           show.period="days",
#           period.in.days=90,
#           bw.plot=FALSE,
#           col.na="#D3D3D3",
#           unspecified.category.label="drug",
#           col.cats=rainbow,
#           lty.event="solid",
#           lwd.event=2,
#           pch.start.event=15,
#           pch.end.event=16,
#           col.continuation="#000000",
#           lty.continuation="dotted",
#           lwd.continuation=1,
#           cex=1,
#           cex.axis=1,
#           cex.lab=1.25,
#           highlight.followup.window=TRUE,
#           followup.window.col="#00FF00",
#           highlight.observation.window=TRUE,
#           observation.window.col="#FFFF00",
#           observation.window.density=35,
#           observation.window.angle=-30,
#           observation.window.opacity=0.3,
#           show.real.obs.window.start=TRUE,
#           real.obs.window.density=35,
#           real.obs.window.angle=30,
#           print.CMA=TRUE,
#           CMA.cex=0.5,
#           plot.CMA=TRUE,
#           CMA.plot.ratio=0.1,
#           CMA.plot.col="#90EE90",
#           CMA.plot.border="#006400",
#           CMA.plot.bkg="#7FFFD4",
#           CMA.plot.text="#006400",
#           plot.CMA.as.histogram=TRUE,
#           show.event.intervals=TRUE,
#           print.dose=TRUE,
#           print.dose.outline.col="#FFFFFF",
#           print.dose.centered=FALSE,
#           plot.dose=FALSE,
#           lwd.event.max.dose=8,
#           plot.dose.lwd.across.medication.classes=FALSE,
#           min.plot.size.in.characters.horiz=10,
#           min.plot.size.in.characters.vert=0.5
#      );
#  }

