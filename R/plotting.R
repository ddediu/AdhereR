################################################################################
#
# Plotting:
#
# Plotting is handled by a single function that can manage CMA0, CMA1+ as well
# as per-episodes and sliding-windows.
# It can handle 1 or more patients.
#
# For a given case, it produces an intermediate format that can be used to
# generate either a static plot (using R base graphics) or various degrees of
# interactivity (using HTML5/CSS/JavaScript).
#
# In principle, this should ensure the "future-proffing" of the plotting system
# as well as make its maintenance and development easier by providing a unified
# codebase.
#
# This is part of AdhereR.
#
#    Copyright (C) 2015-2018  Dan Dediu & Alexandra Dima
#    Copyright (C) 2018-2019  Dan Dediu, Alexandra Dima & Samuel Allemann
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
################################################################################


# For now, unify and refactor the code for all plotting (CMA0, CMA1+, PE and SW) using base R plotting:
.plot.CMAs <- function(cma,                                   # the CMA_per_episode or CMA_sliding_window (or derived) object
                       patients.to.plot=NULL,                 # list of patient IDs to plot or NULL for all
                       duration=NA,                           # duration and end period to plot in days (if missing, determined from the data)
                       align.all.patients=FALSE, align.first.event.at.zero=TRUE, # should all patients be aligned? and, if so, place the first event as the horizontal 0?
                       show.period=c("dates","days")[2],      # draw vertical bars at regular interval as dates or days?
                       period.in.days=90,                     # the interval (in days) at which to draw veritcal lines
                       show.legend=TRUE, legend.x="right", legend.y="bottom", legend.bkg.opacity=0.5, legend.cex=0.75, legend.cex.title=1.0, # legend params and position
                       cex=1.0, cex.axis=0.75, cex.lab=1.0,   # various graphical params
                       show.cma=TRUE,                         # show the CMA type
                       xlab=c("dates"="Date", "days"="Days"), # Vector of x labels to show for the two types of periods, or a single value for both, or NULL for nothing
                       ylab=c("withoutCMA"="patient", "withCMA"="patient (& CMA)"), # Vector of y labels to show without and with CMA estimates, or a single value for both, or NULL ofr nonthing
                       title=c("aligned"="Event patterns (all patients aligned)", "notaligned"="Event patterns"), # Vector of titles to show for and without alignment, or a single value for both, or NULL for nonthing
                       col.cats=rainbow,                      # single color or a function mapping the categories to colors
                       unspecified.category.label="drug",     # the label of the unspecified category of medication
                       medication.groups=NULL,                # optionally, the groups of medications (implictely all are part of the same group)
                       lty.event="solid", lwd.event=2, pch.start.event=15, pch.end.event=16, # event style
                       print.dose=FALSE, cex.dose=0.75, print.dose.outline.col="white", print.dose.centered=FALSE, # print daily dose
                       plot.dose=FALSE, lwd.event.max.dose=8, plot.dose.lwd.across.medication.classes=FALSE, # draw daily dose as line width
                       col.na="lightgray",                    # color for mising data
                       col.continuation="black", lty.continuation="dotted", lwd.continuation=1, # style of the contuniation lines connecting consecutive events
                       print.CMA=TRUE, CMA.cex=0.50, # print CMA next to the participant's ID?
                       plot.CMA=TRUE,                   # plot the CMA next to the participant ID?
                       plot.CMA.as.histogram=TRUE,      # plot CMA as a histogram or as a density plot?
                       plot.partial.CMAs.as=c("stacked", "overlapping", "timeseries")[1], # how to plot the "partial" (i.e., intervals/episodes) CMAs (NULL for none)?
                       plot.partial.CMAs.as.stacked.col.bars="gray90", plot.partial.CMAs.as.stacked.col.border="gray30", plot.partial.CMAs.as.stacked.col.text="black",
                       plot.partial.CMAs.as.timeseries.vspace=7, # how much vertical space to reserve for the timeseries plot (in character lines)
                       plot.partial.CMAs.as.timeseries.start.from.zero=TRUE, #show the vertical axis start at 0 or at the minimum actual value (if positive)?
                       plot.partial.CMAs.as.timeseries.col.dot="darkblue", plot.partial.CMAs.as.timeseries.col.interval="gray70", plot.partial.CMAs.as.timeseries.col.text="firebrick", # setting any of these to NA results in them not being plotted
                       plot.partial.CMAs.as.timeseries.interval.type=c("none", "segments", "arrows", "lines", "rectangles")[2], # how to show the covered intervals
                       plot.partial.CMAs.as.timeseries.lwd.interval=1, # line width for some types of intervals
                       plot.partial.CMAs.as.timeseries.alpha.interval=0.25, # the transparency of the intervales (when drawn as rectangles)
                       plot.partial.CMAs.as.timeseries.show.0perc=TRUE, plot.partial.CMAs.as.timeseries.show.100perc=FALSE, #show the 0% and 100% lines?
                       plot.partial.CMAs.as.overlapping.alternate=TRUE, # should successive intervals be plotted low/high?
                       plot.partial.CMAs.as.overlapping.col.interval="gray70", plot.partial.CMAs.as.overlapping.col.text="firebrick", # setting any of these to NA results in them not being plotted
                       CMA.plot.ratio=0.10,             # the proportion of the total horizontal plot to be taken by the CMA plot
                       CMA.plot.col="lightgreen", CMA.plot.border="darkgreen", CMA.plot.bkg="aquamarine", CMA.plot.text=CMA.plot.border, # attributes of the CMA plot
                       highlight.followup.window=TRUE, followup.window.col="green",
                       highlight.observation.window=TRUE, observation.window.col="yellow", observation.window.density=35, observation.window.angle=-30, observation.window.opacity=0.3,
                       bw.plot=FALSE,                         # if TRUE, override all user-given colors and replace them with a scheme suitable for grayscale plotting
                       min.plot.size.in.characters.horiz=10, min.plot.size.in.characters.vert=0.25, # the minimum plot size (in characters: horizontally, for the whole duration, vertically, per event (and, if shown, per episode/sliding window))
                       max.patients.to.plot=100,        # maximum number of patients to plot
                       ...
)
{
  if( is.null(cma) || !(inherits(cma, "CMA_per_episode") || inherits(cma, "CMA_sliding_window")) || is.null(cma$data) || nrow(cma$data) < 1 ||
      is.na(cma$ID.colname) || !(cma$ID.colname %in% names(cma$data)) ||
      is.na(cma$event.date.colname) || !(cma$event.date.colname %in% names(cma$data)) ||
      !("event.info" %in% names(cma)) || is.null(cma$event.info) ) return (plot.CMA0(cma,...));

  # Convert all data.table to data.frame:
  if( inherits(cma$CMA, "data.table") ) cma$CMA <- as.data.frame(cma$CMA);
  if( inherits(cma$data, "data.table") ) cma$data <- as.data.frame(cma$data);

  # Check compatibility between subtypes of plots:
  if( align.all.patients && show.period != "days" ){ show.period <- "days"; warning("When aligning all patients, cannot show actual dates: showing days instead!\n"); }

  # Depeding on the cma's exact type, the relevant columns might be different: homogenize them for later use
  cmas <- cma$CMA;
  if( inherits(cma, "CMA_per_episode") )
  {
    names(cmas)[2:ncol(cmas)] <- c("WND.ID", "start", "gap.days", "duration", "end", "CMA"); # avoid possible conflict with patients being called "ID"
  } else if( inherits(cma, "CMA_sliding_window") )
  {
    cmas <- cbind(cmas[,1:3], "gap.days"=NA, "duration"=cma$sliding.window.duration, cmas[,4:ncol(cmas)]);
    names(cmas)[2:ncol(cmas)] <- c("WND.ID", "start", "gap.days", "duration", "end", "CMA"); # avoid possible conflict with patients being called "ID"
  }
  # add also the follow-up and observation window infor as well to have everything in one place:
  cmas <- cbind(cmas, do.call(rbind, lapply(1:nrow(cmas), function(i)
  {
    s <- which(cma$event.info[,cma$ID.colname] == cmas[i,cma$ID.colname]);
    if( length(s) != 1 ) return (NULL);
    cma$event.info[s,c(".FU.START.DATE", ".FU.END.DATE", ".OBS.START.DATE", ".OBS.END.DATE")];
  })));

  # Make sure the dates are strings of the right format:
  if( inherits(cma$data[,cma$event.date.colname], "Date") )
  {
    cma$date.format <- "%m/%d/%Y"; # use the default format
    cma$data[,cma$event.date.colname] <- as.character(cma$data[,cma$event.date.colname], format=cma$date.format);
  }

  # The patients:
  patids <- unique(cmas[,cma$ID.colname]); patids <- patids[!is.na(patids)];
  if( !is.null(patients.to.plot) ) patids <- intersect(as.character(patids), as.character(patients.to.plot));
  if( length(patids) == 0 )
  {
    cat("No patients to plot!\n");
    return (invisible(NULL));
  } else if( length(patids) > max.patients.to.plot )
  {
    cat(paste0("Too many patients to plot (",length(patids),
               ")! If you really want that, please change the 'max.patients.to.plot' parameter value (now set at ",
               max.patients.to.plot,"!\n"));
    return (invisible(NULL));
  }
  # Select only the patients to display:
  cma$data <- cma$data[ cma$data[,cma$ID.colname] %in% patids, ];
  cmas <- cmas[ cmas[,cma$ID.colname] %in% patids, ];
  # Make sure the patients are ordered by ID and date:
  cma$data <- cma$data[ order( cma$data[,cma$ID.colname], as.Date(cma$data[,cma$event.date.colname],format=cma$date.format)), ];
  cmas <- cmas[ order( cmas[,cma$ID.colname], cmas$WND.ID, cmas$start), ];

  # Grayscale plotting:
  if( bw.plot )
  {
      if( is.function(col.cats) ) col.cats <- .bw.colors else col.cats <- gray(0.1);
      followup.window.col <- "black";
      observation.window.col <- gray(0.3);
      CMA.plot.col <- gray(0.8);
      CMA.plot.border <- gray(0.2);
      CMA.plot.bkg <- gray(0.5);
      CMA.plot.text <- CMA.plot.border;
  }

  # The colors for the categories:
  if( is.na(cma$medication.class.colname) || !(cma$medication.class.colname %in% names(cma$data)) )
  {
    categories <- unspecified.category.label;
  } else
  {
    categories <- sort(unique(as.character(cma$data[,cma$medication.class.colname])), na.last=FALSE); # all categories making sure NA is first
  }
  if( is.na(categories[1]) )
  {
    if( is.function(col.cats) ) cols <- c(col.na, col.cats(length(categories)-1)) else cols <- c(col.na, rep(col.cats,length(categories)-1));
  } else
  {
    if( is.function(col.cats) ) cols <- col.cats(length(categories)) else cols <- rep(col.cats,length(categories));
  }
  names(cols) <- categories;
  .map.category.to.color <- function( category ) ifelse( is.na(category), cols[1], ifelse( category %in% names(cols), cols[category], "black") );

  # Daily dose:
  if( is.na(cma$event.daily.dose.colname) || !(cma$event.daily.dose.colname %in% names(cma$data)) )
  {
    print.dose <- plot.dose <- FALSE; # can't show daily dose if column is not defined
  }
  if( plot.dose || print.dose ) # consistency checks:
  {
    if( lwd.event.max.dose < lwd.event ) lwd.event.max.dose <- lwd.event;
  }
  if( plot.dose || print.dose )
  {
    if( length(categories) == 1 && categories == unspecified.category.label )
    {
      # Really, no category:
      dose.range <- data.frame("category"=categories, "min"=min(cma$data[,cma$event.daily.dose.colname], na.rm=TRUE), "max"=max(cma$data[,cma$event.daily.dose.colname], na.rm=TRUE));
    } else
    {
      # Range per category:
      tmp <- aggregate(cma$data[,cma$event.daily.dose.colname], by=list("category"=cma$data[,cma$medication.class.colname]), FUN=function(x) range(x,na.rm=TRUE));
      dose.range <- data.frame("category"=tmp$category, "min"=tmp$x[,1], "max"=tmp$x[,2]);
      if( plot.dose.lwd.across.medication.classes )
      {
        dose.range.global <- data.frame("category"="ALL", "min"=min(cma$data[,cma$event.daily.dose.colname], na.rm=TRUE), "max"=max(cma$data[,cma$event.daily.dose.colname], na.rm=TRUE));
      }
    }

    # Function for the linear interpolation of dose between lwd.min and lwd.max:
    adjust.dose.lwd <- function(dose, lwd.min=lwd.event, lwd.max=lwd.event.max.dose, dose.min=dose.range$min[1], dose.max=dose.range$max[1])
    {
      delta <- ifelse(dose.max == dose.min, 1.0, (dose.max - dose.min)); # avoid dividing by zero when there's only one dose
      return (lwd.min + (lwd.max - lwd.min)*(dose - dose.min) / delta);
    }
  }

  # Make sure we are using actual dates:
  if( !inherits(cma$data[,cma$event.date.colname], "Date") )
  {
    cma$data$.DATE.as.Date <- as.Date(cma$data[,cma$event.date.colname],format=cma$date.format);
  } else
  {
    cma$data$.DATE.as.Date <- cma$data[,cma$event.date.colname];
  }
  # Find the earliest date:
  earliest.date <- min(cma$data$.DATE.as.Date, cmas$start, cmas$.OBS.START.DATE, cmas$.FU.START.DATE);

  # If aligning all participants to the same date, simply relocate all dates relative to the earliest date:
  if( align.all.patients )
  {
    for( i in 1:nrow(cma$data) )
    {
      if( i == 1 || cma$data[i,cma$ID.colname] != cma$data[i-1,cma$ID.colname] )
      {
        align.to <- cma$data$.DATE.as.Date[i];
        for( j in which(cmas[,cma$ID.colname] == cma$data[i,cma$ID.colname]) )
        {
          cmas$start[j]           <- earliest.date + (cmas$start[j]           - align.to);
          cmas$end[j]             <- earliest.date + (cmas$end[j]             - align.to);
          cmas$.FU.START.DATE[j]  <- earliest.date + (cmas$.FU.START.DATE[j]  - align.to);
          cmas$.FU.END.DATE[j]    <- earliest.date + (cmas$.FU.END.DATE[j]    - align.to);
          cmas$.OBS.START.DATE[j] <- earliest.date + (cmas$.OBS.START.DATE[j] - align.to);
          cmas$.OBS.END.DATE[j]   <- earliest.date + (cmas$.OBS.END.DATE[j]   - align.to);
        }
      }
      cma$data$.DATE.as.Date[i] <- earliest.date + (cma$data$.DATE.as.Date[i] - align.to);
    }
    correct.earliest.followup.window <- min(cma$data$.DATE.as.Date - min(cmas$.FU.START.DATE,na.rm=TRUE),na.rm=TRUE);
  } else
  {
    correct.earliest.followup.window <- 0;
  }

  # Compute the duration if not given:
  if( is.na(duration) )
  {
    latest.date <- max(c(cmas$end, cmas$.FU.END.DATE, cmas$.OBS.END.DATE),na.rm=TRUE);
    duration <- as.numeric(latest.date - earliest.date) + correct.earliest.followup.window;
  }
  endperiod <- duration;

  # Reserve space for the CMA plotting:
  adh.plot.space <- c(0, ifelse( plot.CMA && !is.null(getCMA(cma)), duration*CMA.plot.ratio, 0) );
  duration.total <- duration + adh.plot.space[2];

  # Save the graphical params and restore them later:
  old.par <- par(no.readonly=TRUE);

  # Make sure there's enough space to actually plot the patient IDs on the y-axis:
  id.labels <- do.call(rbind,lapply(as.character(patids), # for each patient ID, compute the string dimensions in inches
                                    function(p)
                                    {
                                      # The participant axis text:
                                      s <- which(cma$event.info[,cma$ID.colname] == p);
                                      x <- which(getCMA(cma)[cma$ID.colname] == p);
                                      pid <- p;
                                      data.frame("ID"=p, "string"=pid, "width"=strwidth(pid, units="inches", cex=cex.axis), "height"=strheight(pid, units="inches", cex=cex.axis));
                                    }));
  y.label <- data.frame("string"=(tmp <- ifelse(is.null(ylab),"",
                                                ifelse(length(ylab)==1,ylab,
                                                       ifelse((print.CMA || plot.CMA) && !is.null(getCMA(cma)),ylab["withCMA"],ylab["withoutCMA"])))), # space needed for the label (in inches)
                        "width"=strwidth(tmp, units="inches", cex=cex.lab), "height"=strheight(tmp, units="inches", cex=cex.lab));
  left.margin <- (cur.mai <- par("mai"))[2]; # left margin in inches (and cache the current margins too)
  # If there's enough space as it is, don't do anything:
  if( left.margin < (y.label$height + max(id.labels$width,na.rm=TRUE)) ) # remeber that the y.label is vertical
  {
    # Well, there isn't so:
    rotate.id.labels <- 30; # rotate the labels (in degrees)
    new.left.margin <- (y.label$height + (cos(rotate.id.labels*pi/180) * max(id.labels$width,na.rm=TRUE)) + strwidth("0000", units="inches", cex=cex.axis)); # ask for enough space
    par(mai=c(cur.mai[1], new.left.margin, cur.mai[3], cur.mai[4]));
  } else
  {
    # Seems to fit, so don't do anything:
    rotate.id.labels <- 0;
  }

  # Vertical space needed for showing the partial CMAs:
  if( ("timeseries" %in% plot.partial.CMAs.as) && (plot.partial.CMAs.as.timeseries.vspace < 5) )
  {
    warning(paste0("The minimum vertical space for the timeseries plots (plot.partial.CMAs.as.timeseries.vspace) is 5 lines, but it currently is only ",
                   plot.partial.CMAs.as.timeseries.vspace,
                   ": skipping timeseries plots...\n"));
    plot.partial.CMAs.as <- plot.partial.CMAs.as[ plot.partial.CMAs.as != "timeseries" ];
  }
  vert.space.cmas <- 0 +
    ifelse(plot.CMA && !is.null(getCMA(cma)),
           (nrow(cmas)+length(patids)) * as.numeric("stacked" %in% plot.partial.CMAs.as) +
             3 * length(patids) * as.numeric("overlapping" %in% plot.partial.CMAs.as) +
             plot.partial.CMAs.as.timeseries.vspace * length(patids) * as.numeric("timeseries" %in% plot.partial.CMAs.as),
           0);

  # The actual plotting:
  if(inherits(msg <- try(plot( 0, 1,
                               #xlim=c(0-2*duration.total/100,duration.total), xaxs="i",
                               xlim=c(0-5,duration.total+5), xaxs="i", # pad to improve plotting
                               ylim=c(0,nrow(cma$data)+vert.space.cmas+1), yaxs="i", type="n",
                               axes=FALSE,
                               xlab="", ylab="" ),
                         silent=TRUE),
              "try-error"))
  {
    # Some error occured when creatig the plot...
    cat(msg);
    par(old.par); # restore graphical params
    return (invisible(NULL));
  }

  # Character width and height in the current plotting system:
  if( print.dose ) dose.text.height <- strheight("0",cex=cex.dose); # the vertical height of the dose text for plotting adjustment
  char.width <- strwidth("O",cex=cex); char.height <- strheight("O",cex=cex);
  char.height.CMA <- strheight("0",cex=CMA.cex);

  # Minimum plot dimensions:
  if( abs(par("usr")[2] - par("usr")[1]) <= char.width * min.plot.size.in.characters.horiz ||
      abs(par("usr")[4] - par("usr")[3]) <= char.height * min.plot.size.in.characters.vert * (nrow(cma$data)+ifelse(plot.CMA && !is.null(getCMA(cma)), nrow(cmas), 0)))
  {
    cat(paste0("Plotting area is too small (it must be at least ",
               min.plot.size.in.characters.horiz,
               " x ",
               min.plot.size.in.characters.vert,
               " characters per patient, but now it is only ",
               round(abs(par("usr")[2] - par("usr")[1]) / char.width,1),
               " x ",
               round(abs(par("usr")[4] - par("usr")[3]) / (char.height * (nrow(cma$data)+ifelse(plot.CMA && !is.null(getCMA(cma)), nrow(cmas), 0))),1),
               ")!\n"));
    #segments(x0=c(par("usr")[1], par("usr")[1]),
    #         y0=c(par("usr")[3], par("usr")[4]),
    #         x1=c(par("usr")[2], par("usr")[2]),
    #         y1=c(par("usr")[4], par("usr")[3]),
    #         col="red", lwd=3);
    par(old.par); # restore graphical params
    return (invisible(NULL));
  }

  # Continue plotting:
  box();
  title(main=paste0(ifelse(is.null(title),"",
                           ifelse(length(title)==1,title,
                                  ifelse(align.all.patients, title["aligned"], title["notaligned"]))),
                    ifelse(!is.null(title) && show.cma,
                           paste0(" ",
                                  switch(class(cma)[1],
                                         "CMA_sliding_window"="sliding window",
                                         "CMA_per_episode"="per episode"),
                                  " (",cma$computed.CMA,")"),"")),
        xlab=ifelse(is.null(xlab),"",
                    ifelse(length(xlab)==1,xlab, xlab[show.period])),
        #ylab=ifelse((print.CMA || plot.CMA) && !is.null(getCMA(cma)),"patient (& CMA)","patient"),
        cex.lab=cex.lab);
  #text(par("usr")[1] - ((cos(rotate.id.labels*pi/180) * max(vapply(id.labels$string, function(p) strwidth(p, cex=cex.axis), numeric(1)),na.rm=TRUE)) + strwidth("0000", cex=cex.axis)),
  #     (par("usr")[4] + par("usr")[3])/2, y.label$string, cex=cex.lab, srt=90, xpd=TRUE);
  mtext(y.label$string, side=2, line=par("mar")[2]-1, at=(par("usr")[4] + par("usr")[3])/2, cex=cex.lab, las=3);

  # The patient axis and CMA plots:
  if( plot.CMA && !is.null(getCMA(cma)) )
  {
    # Maximum achieved CMA:
    adh.max <- 1.0;
    # Function mapping the CMA values to the appropriate x-coordinates:
    .rescale.xcoord.for.CMA.plot <- function(x,pfree=0.20) return (adh.plot.space[1] + x/adh.max*(adh.plot.space[2]*(1-pfree) - adh.plot.space[1]));
  }
  draw.gray.band <- FALSE;
  y.cur <- 1;
  for( p in as.character(patids) )
  {
    # The participant axis text:
    s <- which(cma$data[,cma$ID.colname] == p);
    x <- which(cmas[cma$ID.colname] == p);
    pid <- p;
    if( rotate.id.labels > 0 )
    {
      # Rotate the labels:
      text(par("usr")[1], y.cur+length(s)/2+ifelse(plot.CMA && !is.null(getCMA(cma)) && adh.plot.space[2] > 0,length(x),0)/2, pid, cex=cex.axis, srt=rotate.id.labels, pos=2, xpd=TRUE );
    } else
    {
      # Don't rotate the labels:
      mtext( pid, 2, line=0.5, at=y.cur+length(s)/2+ifelse(plot.CMA && !is.null(getCMA(cma)) && adh.plot.space[2] > 0,length(x),0)/2, las=2, cex=cex.axis );
    }

    # The alternating gray bands:
    if( draw.gray.band )
    {
      rect( 0-1,
            y.cur-0.5,
            duration.total+1,
            y.cur +
              length(s) +
              ifelse(plot.CMA && !is.null(getCMA(cma)) && adh.plot.space[2] > 0,
                     (length(x)+1) * as.numeric("stacked" %in% plot.partial.CMAs.as) +
                       3 * as.numeric("overlapping" %in% plot.partial.CMAs.as) +
                       plot.partial.CMAs.as.timeseries.vspace * as.numeric("timeseries" %in% plot.partial.CMAs.as),
                     0) -
              0.5,
            col=gray(0.95), border=NA );
    }
    draw.gray.band <- !draw.gray.band;

    # The participant CMA plot:
    if( plot.CMA && !is.null(getCMA(cma)) && adh.plot.space[2] > 0 )
    {
      y.mean <- y.cur+length(s)/2+length(x)/2;
      segments(.rescale.xcoord.for.CMA.plot(0), y.mean-2, .rescale.xcoord.for.CMA.plot(1), y.mean-2, lty="solid", col=CMA.plot.col);
      segments(.rescale.xcoord.for.CMA.plot(0), y.mean+2, .rescale.xcoord.for.CMA.plot(1), y.mean+2, lty="solid", col=CMA.plot.col);
      adh <- na.omit(getCMA(cma)[x,"CMA"]);
      # Scale the CMA (itself or density) in such a way that if within 0..1 stays within 0..1 but scales if it goes outside this interval to accomodate it
      if( plot.CMA.as.histogram )
      {
        # Plot CMA as histogram:
        if( length(adh) > 0 )
        {
          adh.hist <- hist(adh, plot=FALSE);
          adh.x <- adh.hist$breaks[-1]; adh.x.0 <- min(adh.x,0); adh.x.1 <- max(adh.x,1); adh.x <- (adh.x - adh.x.0) / (adh.x.1 - adh.x.0);
          adh.y <- adh.hist$counts; adh.y <- adh.y / max(adh.y);
          adh.x.max <- adh.x[which.max(adh.hist$counts)];
          segments(.rescale.xcoord.for.CMA.plot(adh.x), y.mean-2, .rescale.xcoord.for.CMA.plot(adh.x), y.mean-2 + 4*adh.y, lty="solid", lwd=1, col=CMA.plot.border);
          if( char.height.CMA <= abs(.rescale.xcoord.for.CMA.plot(1) - .rescale.xcoord.for.CMA.plot(0)) )
          {
            # enough space for vertical writing all three of them:
            text(x=.rescale.xcoord.for.CMA.plot(0),         y.mean-2-char.height.CMA/2,
                 sprintf("%.1f%%",100*min(adh.x.0,na.rm=TRUE)), srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
            text(x=.rescale.xcoord.for.CMA.plot(1),         y.mean-2-char.height.CMA/2,
                 sprintf("%.1f%%",100*max(adh.x.1,na.rm=TRUE)), srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
            text(x=.rescale.xcoord.for.CMA.plot(adh.x.max), y.mean+2+char.height.CMA/2,
                 sprintf("%d",max(adh.hist$counts,an.rm=TRUE)), srt=90, pos=3, cex=CMA.cex, col=CMA.plot.text);
          }
        }
      } else
      {
        if( length(adh) > 2 )
        {
          # Plot CMA as density plot:
          #adh.density <- density(adh, from=min(adh,na.rm=TRUE), to=max(adh,na.rm=TRUE));
          adh.density <- density(adh);
          ss <- (adh.density$x >= min(adh,na.rm=TRUE) & adh.density$x <= max(adh,na.rm=TRUE));
          if( sum(ss) == 0 )
          {
            # probably constant numbers?
            # Plot the individual lines:
            #if( length(adh) == 1 || isTRUE(all.equal(min(adh), max(adh))) ) adh.x <- pmax(pmin(adh,1.0),0.0) else adh.x <- (adh - min(adh)) / (max(adh) - min(adh));
            adh.x.0 <- min(adh,0); adh.x.1 <- max(adh,1); adh.x <- (adh - adh.x.0) / (adh.x.1 - adh.x.0);
            segments(.rescale.xcoord.for.CMA.plot(adh.x), y.mean-2, .rescale.xcoord.for.CMA.plot(adh.x), y.mean-2 + 4, lty="solid", lwd=2, col=CMA.plot.border);
            if( char.height.CMA*length(adh) <= abs(.rescale.xcoord.for.CMA.plot(1) - .rescale.xcoord.for.CMA.plot(0)) )
            {
              # enough space for vertical writing all of them:
              for( i in 1:length(adh) )
              {
                text(x=.rescale.xcoord.for.CMA.plot(adh.x[i]), y.mean+ifelse(i %% 2==0,2+char.height.CMA/2,-2-char.height.CMA/2),
                     sprintf("%.1f%%",100*adh[i]), srt=90, pos=ifelse(i %% 2==0,3,1), cex=CMA.cex, col=CMA.plot.text);
              }
            } else if( char.height.CMA <= abs(.rescale.xcoord.for.CMA.plot(1) - .rescale.xcoord.for.CMA.plot(0)) )
            {
              # enough space for vertical writing only the extremes:
              text(x=.rescale.xcoord.for.CMA.plot(adh.x[1]),           y.mean-2-char.height.CMA/2,
                   sprintf("%.1f%%",100*adh[1]),           srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
              text(x=.rescale.xcoord.for.CMA.plot(adh.x[length(adh)]), y.mean-2-char.height.CMA/2,
                   sprintf("%.1f%%",100*adh[length(adh)]), srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
            }
          } else
          {
            adh.density$x <- adh.density$x[ss]; adh.density$y <- adh.density$y[ss];
            #adh.x <- adh.density$x; adh.x <- (adh.x - min(adh.x)) / (max(adh.x) - min(adh.x));
            adh.x <- adh.density$x; adh.x.0 <- min(adh.x,0); adh.x.1 <- max(adh.x,1); adh.x <- (adh.x - adh.x.0) / (adh.x.1 - adh.x.0);
            adh.y <- adh.density$y; adh.y <- (adh.y - min(adh.y)) / (max(adh.y) - min(adh.y));
            points(.rescale.xcoord.for.CMA.plot(adh.x), y.mean-2 + 4*adh.y, type="l", col=CMA.plot.border);
            if( char.height.CMA <= abs(.rescale.xcoord.for.CMA.plot(1) - .rescale.xcoord.for.CMA.plot(0)) )
            {
              # enough space for vertical writing:
              text(x=.rescale.xcoord.for.CMA.plot(0), y.mean-2-char.height.CMA/2, sprintf("%.1f%%",100*adh.x.0), srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
              text(x=.rescale.xcoord.for.CMA.plot(1), y.mean-2-char.height.CMA/2, sprintf("%.1f%%",100*adh.x.1), srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
            }
          }
        } else
        {
          #rect(.rescale.xcoord.for.CMA.plot(0), y.mean-2, .rescale.xcoord.for.CMA.plot(1), y.mean+2, border=NA, col=CMA.plot.col, density=25);
          if( length(adh) == 0 )
          {
            # No points: nothing to plot!
          } else
          {
            # Plot the individual lines:
            #if( length(adh) == 1 || isTRUE(all.equal(min(adh), max(adh))) ) adh.x <- pmax(pmin(adh,1.0),0.0) else adh.x <- (adh - min(adh)) / (max(adh) - min(adh));
            adh.x.0 <- min(adh,0); adh.x.1 <- max(adh,1); adh.x <- (adh - adh.x.0) / (adh.x.1 - adh.x.0);
            segments(.rescale.xcoord.for.CMA.plot(adh.x), y.mean-2, .rescale.xcoord.for.CMA.plot(adh.x), y.mean-2 + 4, lty="solid", lwd=2, col=CMA.plot.border);
            if( char.height.CMA*length(adh) <= abs(.rescale.xcoord.for.CMA.plot(1) - .rescale.xcoord.for.CMA.plot(0)) )
            {
              # enough space for vertical writing all of them:
              for( i in 1:length(adh) )
              {
                text(x=.rescale.xcoord.for.CMA.plot(adh.x[i]), y.mean+ifelse(i %% 2==0,2+char.height.CMA/2,-2-char.height.CMA/2),
                     sprintf("%.1f%%",100*adh[i]), srt=90, pos=ifelse(i %% 2==0,3,1), cex=CMA.cex, col=CMA.plot.text);
              }
            } else if( char.height.CMA <= abs(.rescale.xcoord.for.CMA.plot(1) - .rescale.xcoord.for.CMA.plot(0)) )
            {
              # enough space for vertical writing only the extremes:
              text(x=.rescale.xcoord.for.CMA.plot(adh.x[1]),           y.mean-2-char.height.CMA/2,
                   sprintf("%.1f%%",100*adh[1]),           srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
              text(x=.rescale.xcoord.for.CMA.plot(adh.x[length(adh)]), y.mean-2-char.height.CMA/2,
                   sprintf("%.1f%%",100*adh[length(adh)]), srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
            }
          }
        }
      }
    }

    # The follow-up and observation windows:
    if( highlight.followup.window )
    {
      # rect(adh.plot.space[2] + as.numeric(cmas$.FU.START.DATE[x[1]] - earliest.date) + correct.earliest.followup.window, y.cur-0.25,
      #      adh.plot.space[2] + as.numeric(cmas$.FU.END.DATE[x[1]] - earliest.date) + correct.earliest.followup.window, y.cur+length(s)+0.25,
      #      col=NA, border=followup.window.col, lty="dashed", lwd=2);
      rect(adh.plot.space[2] + as.numeric(cmas$.FU.START.DATE[x[1]] - earliest.date) + correct.earliest.followup.window, y.cur-0.5,
           adh.plot.space[2] + as.numeric(cmas$.FU.END.DATE[x[1]] - earliest.date) + correct.earliest.followup.window, y.cur+length(s)-0.5,
           col=NA, border=followup.window.col, lty="dashed", lwd=2);
    }
    if( highlight.observation.window )
    {
      # The given observation window:
      # rect(adh.plot.space[2] + as.numeric(cmas$.OBS.START.DATE[x[1]] - earliest.date) + correct.earliest.followup.window, y.cur-0.25,
      #      adh.plot.space[2] + as.numeric(cmas$.OBS.END.DATE[x[1]] - earliest.date) + correct.earliest.followup.window, y.cur+length(s)+0.25,
      #      col=adjustcolor(observation.window.col,alpha.f=observation.window.opacity), border=NA, density=observation.window.density, angle=observation.window.angle);
      rect(adh.plot.space[2] + as.numeric(cmas$.OBS.START.DATE[x[1]] - earliest.date) + correct.earliest.followup.window, y.cur-0.5,
           adh.plot.space[2] + as.numeric(cmas$.OBS.END.DATE[x[1]] - earliest.date) + correct.earliest.followup.window, y.cur+length(s)-0.5,
           col=adjustcolor(observation.window.col,alpha.f=observation.window.opacity), border=NA, density=observation.window.density, angle=observation.window.angle);
    }

    y.cur <- y.cur +
      length(s) +
      ifelse(plot.CMA && !is.null(getCMA(cma)) && adh.plot.space[2] > 0,
           (length(x)+1) * as.numeric("stacked" %in% plot.partial.CMAs.as) +
             3 * as.numeric("overlapping" %in% plot.partial.CMAs.as) +
             plot.partial.CMAs.as.timeseries.vspace * as.numeric("timeseries" %in% plot.partial.CMAs.as),
           0);
  }
  if( plot.CMA && !is.null(getCMA(cma)) && adh.plot.space[2] > 0 )
  {
    # Mark the drawing area:
    #rect(.rescale.xcoord.for.CMA.plot(0), par("usr")[3], .rescale.xcoord.for.CMA.plot(1.0), par("usr")[4], col=adjustcolor(CMA.plot.bkg,alpha.f=0.25), border=NA);
    abline(v=c(.rescale.xcoord.for.CMA.plot(0), .rescale.xcoord.for.CMA.plot(1.0)), col=CMA.plot.col, lty="dotted", lwd=1);
    #mtext( c("0%","100%"), 3, line=0.5, at=c(.rescale.xcoord.for.CMA.plot(0), .rescale.xcoord.for.CMA.plot(1.0)), las=2, cex=cex.axis, col=CMA.plot.border );
  }

  # Plot each event:
  curpat <- TRUE;
  y.cur <- 1;
  for( i in 1:nrow(cma$data) )
  {
    start <- as.numeric(cma$data$.DATE.as.Date[i] - earliest.date);
    end <- start + cma$data[i,cma$event.duration.colname];
    if( is.na(cma$medication.class.colname) || !(cma$medication.class.colname %in% names(cma$data)) )
    {
      col <- .map.category.to.color(unspecified.category.label);
    } else
    {
      col <- .map.category.to.color(cma$data[i,cma$medication.class.colname]);
    }
    points( adh.plot.space[2]+start+correct.earliest.followup.window, y.cur, pch=pch.start.event, col=col, cex=cex);
    points(adh.plot.space[2]+end+correct.earliest.followup.window, y.cur, pch=pch.end.event, col=col, cex=cex);
    if( plot.dose )
    {
      if( nrow(dose.range) == 1 )
      {
        segments( adh.plot.space[2]+start+correct.earliest.followup.window, y.cur, adh.plot.space[2]+end+correct.earliest.followup.window, y.cur, col=col, lty=lty.event, lwd=adjust.dose.lwd(cma$data[i,cma$event.daily.dose.colname]));
      } else
      {
        if( plot.dose.lwd.across.medication.classes )
        {
          segments( adh.plot.space[2]+start+correct.earliest.followup.window, y.cur, adh.plot.space[2]+end+correct.earliest.followup.window, y.cur, col=col, lty=lty.event, lwd=adjust.dose.lwd(cma$data[i,cma$event.daily.dose.colname], dose.min=dose.range.global$min, dose.max=dose.range.global$max));
        } else
        {
          dose.for.cat <- (dose.range$category == cma$data[i,cma$medication.class.colname]);
          if( sum(dose.for.cat,na.rm=TRUE) == 1 )
          {
            segments( adh.plot.space[2]+start+correct.earliest.followup.window, y.cur, adh.plot.space[2]+end+correct.earliest.followup.window, y.cur, col=col, lty=lty.event, lwd=adjust.dose.lwd(cma$data[i,cma$event.daily.dose.colname], dose.min=dose.range$min[dose.for.cat], dose.max=dose.range$max[dose.for.cat]));
          } else
          {
            segments( adh.plot.space[2]+start+correct.earliest.followup.window, y.cur, adh.plot.space[2]+end+correct.earliest.followup.window, y.cur, col=col, lty=lty.event, lwd=lwd.event);
          }
        }
      }
    } else
    {
      segments( adh.plot.space[2]+start+correct.earliest.followup.window, y.cur, adh.plot.space[2]+end+correct.earliest.followup.window, y.cur, col=col, lty=lty.event, lwd=lwd.event);
    }
    if( print.dose ) # print daily dose
    {
      dose.text.y <- y.cur - ifelse(print.dose.centered,0 , dose.text.height*2/3); # print it on or below the dose segment?
      if( is.na(print.dose.outline.col) ) # simple or outlined?
      {
        text(adh.plot.space[2]+(start + end)/2+correct.earliest.followup.window, dose.text.y, cma$data[i,cma$event.daily.dose.colname], cex=cex.dose, col=col);
      } else
      {
        .shadow.text(adh.plot.space[2]+(start + end)/2+correct.earliest.followup.window, dose.text.y, cma$data[i,cma$event.daily.dose.colname], cex=cex.dose, col=col, bg=print.dose.outline.col);
      }
    }
    y.cur <- y.cur + 1;

    if( i < nrow(cma$data) && cma$data[i,cma$ID.colname] == cma$data[i+1,cma$ID.colname] )
    {
      # Extend the line
      start.next <- as.numeric(cma$data$.DATE.as.Date[i+1] - earliest.date);
      segments( adh.plot.space[2]+end+correct.earliest.followup.window, y.cur-1, adh.plot.space[2]+start.next+correct.earliest.followup.window, y.cur-1,
                col=col.continuation, lty=lty.continuation, lwd=lwd.continuation);
      segments( adh.plot.space[2]+start.next+correct.earliest.followup.window, y.cur-1, adh.plot.space[2]+start.next+correct.earliest.followup.window, y.cur,
                col=col.continuation, lty=lty.continuation, lwd=lwd.continuation);
    } else
    {
      # Now the patient is changing or is the last patient:
      # Draw its subperiods:
      if( plot.CMA && !is.null(getCMA(cma)) && adh.plot.space[2] > 0 )
      {
        s <- which(cmas[,cma$ID.colname] == cma$data[i,cma$ID.colname]);
        if( length(s) > 0 )
        {
          # There's stuff to plot:
          if( "stacked" %in% plot.partial.CMAs.as )
          {
            # Show subperiods as stacked:
            for( j in 1:length(s) )
            {
              start <- as.numeric(cmas$start[s[j]] - earliest.date);
              end <- as.numeric(cmas$end[s[j]] - earliest.date);
              rect( adh.plot.space[2]+start+correct.earliest.followup.window, y.cur+0.10, adh.plot.space[2]+end+correct.earliest.followup.window, y.cur+0.90, border=gray(0.7), col="white");
              if( !is.na(cmas$CMA[s[j]]) )
              {
                h <- start + (end - start)*max(c(min(c(cmas$CMA[s[j]],1.0)),0.0));
                rect( adh.plot.space[2]+start+correct.earliest.followup.window, y.cur+0.10,
                      adh.plot.space[2]+h+correct.earliest.followup.window, y.cur+0.90,
                      border=plot.partial.CMAs.as.stacked.col.border,
                      col=plot.partial.CMAs.as.stacked.col.bars);
                if( print.CMA && char.height.CMA <= 0.80 )
                {
                  text( adh.plot.space[2]+(start+end)/2+correct.earliest.followup.window, y.cur+0.5,
                        sprintf("%.0f%%",100*cmas$CMA[s[j]]), cex=CMA.cex, col=plot.partial.CMAs.as.stacked.col.text);
                }
              }
              y.cur <- y.cur+1;
            }
            y.cur <- y.cur+1;
          }

          if( "overlapping" %in% plot.partial.CMAs.as )
          {
            # Show subperiods as overlapping segments:
            ppts <- do.call(rbind,lapply(s, function(x)
            {
              start <- as.numeric(cmas$start[x] - earliest.date);
              end <- as.numeric(cmas$end[x] - earliest.date);
              data.frame("x"=(start+end)/2, "y"=cmas$CMA[x], "start"=start, "end"=end, "text"=sprintf("%.0f%%",100*cmas$CMA[x]));
            }));
            if( all(is.na(ppts$y)) )
            {
              # All are missing:
              text(adh.plot.space[2] + correct.earliest.followup.window + (min(ppts$start,na.rm=TRUE) + max(ppts$end,na.rm=TRUE))/2,
                   y.cur + 1,
                   "Missing data only", cex=CMA.cex, col=plot.partial.CMAs.as.overlapping.col.text);
            } else
            {
              # There's at least one non-NA, so plot it:
              ppts$x.plot <- (adh.plot.space[2] + correct.earliest.followup.window + ppts$x);
              min.y <- min(ppts$y,na.rm=TRUE);
              if( !((range.y <- (max(ppts$y,na.rm=TRUE) - min.y)) > 0) )
              {
                range.y <- 1; # avoid division by 0 if there's only one value
              }
              ppts$y.norm <- (ppts$y - min.y)/range.y;

              if( !is.na(plot.partial.CMAs.as.overlapping.col.interval) )
              {
                if( plot.partial.CMAs.as.overlapping.alternate )
                {
                  v <- rep(c(0,1), nrow(ppts))[1:nrow(ppts)]; # alternate between low (0) and high (1) -- not the best way but works fine
                } else
                {
                  v <- rep(0,nrow(ppts)); # all segments are drawn low (0)
                }
                segments(adh.plot.space[2]+ppts$start+correct.earliest.followup.window, y.cur+0.5+v,
                         adh.plot.space[2]+ppts$end+correct.earliest.followup.window, y.cur+0.5+v,
                         col=plot.partial.CMAs.as.overlapping.col.interval);
                segments(adh.plot.space[2]+ppts$start+correct.earliest.followup.window, y.cur+0.5+v,
                         adh.plot.space[2]+ppts$start+correct.earliest.followup.window, y.cur+0.5+v+(ppts$y.norm * -(v*2-1)), # -(v*2-1) maps 0 to 1 and 1 to -1
                         col=plot.partial.CMAs.as.overlapping.col.interval);
                segments(adh.plot.space[2]+ppts$end+correct.earliest.followup.window, y.cur+0.5+v,
                         adh.plot.space[2]+ppts$end+correct.earliest.followup.window, y.cur+0.5+v+(ppts$y.norm * -(v*2-1)),
                         col=plot.partial.CMAs.as.overlapping.col.interval);
              }
              if( print.CMA && char.height.CMA <= 0.80 && !is.na(plot.partial.CMAs.as.overlapping.col.text) )
              {
                text( adh.plot.space[2]+ppts$x+correct.earliest.followup.window, y.cur+1.0,
                      ppts$text, cex=CMA.cex, col=plot.partial.CMAs.as.overlapping.col.text);
              }
            }

            # Advance to next patient:
            y.cur <- y.cur+3;
          }

          if( "timeseries" %in% plot.partial.CMAs.as )
          {
            # Show subperiods as a time series
            ppts <- do.call(rbind,lapply(s, function(x)
            {
              start <- as.numeric(cmas$start[x] - earliest.date);
              end <- as.numeric(cmas$end[x] - earliest.date);
              data.frame("x"=(start+end)/2, "y"=cmas$CMA[x], "start"=start, "end"=end, "text"=sprintf("%.0f%%",100*cmas$CMA[x]));
            }));
            if( all(is.na(ppts$y)) )
            {
              # All are missing:
              text(adh.plot.space[2] + correct.earliest.followup.window + (min(ppts$start,na.rm=TRUE) + max(ppts$end,na.rm=TRUE))/2,
                   y.cur + plot.partial.CMAs.as.timeseries.vspace/2,
                   "Missing data only", cex=CMA.cex, col=plot.partial.CMAs.as.timeseries.col.text);
            } else
            {
              # There's at least one non-NA, so plot it:
              ppts$x.plot <- (adh.plot.space[2] + correct.earliest.followup.window + ppts$x);
              if( plot.partial.CMAs.as.timeseries.start.from.zero )
              {
                min.y <- min(ppts$y,0,na.rm=TRUE);
              } else
              {
                min.y <- min(ppts$y,na.rm=TRUE);
              }
              if( !((range.y <- (max(ppts$y,na.rm=TRUE) - min.y)) > 0) )
              {
                range.y <- 1; # avoid division by 0 if there's only one value
              }
              ppts$y.norm <- (y.cur + 1 + (plot.partial.CMAs.as.timeseries.vspace-3) * (ppts$y - min.y)/range.y);

              # The intervals:
              if( !is.na(plot.partial.CMAs.as.timeseries.col.interval) )
              {
                if( plot.partial.CMAs.as.timeseries.interval.type == "none" )
                {
                  # Nothing to plot
                } else if( plot.partial.CMAs.as.timeseries.interval.type %in% c("segments", "arrows", "lines") )
                {
                  # The lines:
                  segments(adh.plot.space[2] + ppts$start + correct.earliest.followup.window, ppts$y.norm,
                           adh.plot.space[2] + ppts$end + correct.earliest.followup.window, ppts$y.norm,
                           col=plot.partial.CMAs.as.timeseries.col.interval, lwd=plot.partial.CMAs.as.timeseries.lwd.interval);
                  if( plot.partial.CMAs.as.timeseries.interval.type == "segments" )
                  {
                    # The segment endings:
                    segments(adh.plot.space[2] + ppts$start + correct.earliest.followup.window, ppts$y.norm - 0.2,
                             adh.plot.space[2] + ppts$start + correct.earliest.followup.window, ppts$y.norm + 0.2,
                             col=plot.partial.CMAs.as.timeseries.col.interval, lwd=plot.partial.CMAs.as.timeseries.lwd.interval);
                    segments(adh.plot.space[2] + ppts$end + correct.earliest.followup.window, ppts$y.norm - 0.2,
                             adh.plot.space[2] + ppts$end + correct.earliest.followup.window, ppts$y.norm + 0.2,
                             col=plot.partial.CMAs.as.timeseries.col.interval, lwd=plot.partial.CMAs.as.timeseries.lwd.interval);
                  } else if( plot.partial.CMAs.as.timeseries.interval.type == "arrows" )
                  {
                    # The arrow endings:
                    segments(adh.plot.space[2] + ppts$start + correct.earliest.followup.window + char.width/2, ppts$y.norm - char.height/2,
                             adh.plot.space[2] + ppts$start + correct.earliest.followup.window, ppts$y.norm,
                             col=plot.partial.CMAs.as.timeseries.col.interval, lwd=plot.partial.CMAs.as.timeseries.lwd.interval);
                    segments(adh.plot.space[2] + ppts$start + correct.earliest.followup.window + char.width/2, ppts$y.norm + char.height/2,
                             adh.plot.space[2] + ppts$start + correct.earliest.followup.window, ppts$y.norm,
                             col=plot.partial.CMAs.as.timeseries.col.interval, lwd=plot.partial.CMAs.as.timeseries.lwd.interval);
                    segments(adh.plot.space[2] + ppts$end + correct.earliest.followup.window - char.width/2, ppts$y.norm - char.height/2,
                             adh.plot.space[2] + ppts$end + correct.earliest.followup.window, ppts$y.norm,
                             col=plot.partial.CMAs.as.timeseries.col.interval, lwd=plot.partial.CMAs.as.timeseries.lwd.interval);
                    segments(adh.plot.space[2] + ppts$end + correct.earliest.followup.window - char.width/2, ppts$y.norm + char.height/2,
                             adh.plot.space[2] + ppts$end + correct.earliest.followup.window, ppts$y.norm,
                             col=plot.partial.CMAs.as.timeseries.col.interval, lwd=plot.partial.CMAs.as.timeseries.lwd.interval);
                  }
                } else if( plot.partial.CMAs.as.timeseries.interval.type == "rectangles" )
                {
                  # As semi-transparent rectangles:
                  rect(adh.plot.space[2] + ppts$start + correct.earliest.followup.window, y.cur + 0.5,
                       adh.plot.space[2] + ppts$end + correct.earliest.followup.window, y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0,
                       col=scales::alpha(plot.partial.CMAs.as.timeseries.col.interval, alpha=plot.partial.CMAs.as.timeseries.alpha.interval),
                       border=plot.partial.CMAs.as.timeseries.col.interval, lty="dotted");
                }
              }

              # The axes:
              segments(adh.plot.space[2] + correct.earliest.followup.window + min(ppts$start,na.rm=TRUE), y.cur + 0.5,
                       adh.plot.space[2] + correct.earliest.followup.window + max(ppts$end,na.rm=TRUE), y.cur + 0.5,
                       lty="solid", col="black"); # horizontal axis
              segments(adh.plot.space[2] + correct.earliest.followup.window + min(ppts$start,na.rm=TRUE), y.cur + 0.5,
                       adh.plot.space[2] + correct.earliest.followup.window + min(ppts$start,na.rm=TRUE), y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0,
                       lty="solid", col="black"); # vertical axis
              segments(adh.plot.space[2] + correct.earliest.followup.window + min(ppts$start,na.rm=TRUE), min(ppts$y.norm,na.rm=TRUE),
                       adh.plot.space[2] + correct.earliest.followup.window + max(ppts$end,na.rm=TRUE), min(ppts$y.norm,na.rm=TRUE),
                       lty="dashed", col="black"); # the minimum value
              segments(adh.plot.space[2] + correct.earliest.followup.window + min(ppts$start,na.rm=TRUE), max(ppts$y.norm,na.rm=TRUE),
                       adh.plot.space[2] + correct.earliest.followup.window + max(ppts$end,na.rm=TRUE), max(ppts$y.norm,na.rm=TRUE),
                       lty="dashed", col="black"); # the minimum value
              if( plot.partial.CMAs.as.timeseries.show.0perc && (y.for.0perc <- (y.cur + 1 + (plot.partial.CMAs.as.timeseries.vspace-3) * (0 - min.y)/range.y)) >= y.cur + 0.5 )
              {
                segments(adh.plot.space[2] + correct.earliest.followup.window + min(ppts$start,na.rm=TRUE), y.for.0perc,
                         adh.plot.space[2] + correct.earliest.followup.window + max(ppts$end,na.rm=TRUE), y.for.0perc,
                         lty="dotted", col="red"); # 0%
              }
              if( plot.partial.CMAs.as.timeseries.show.100perc && (y.for.100perc <- (y.cur + 1 + (plot.partial.CMAs.as.timeseries.vspace-3) * (1.0 - min.y)/range.y)) <= y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0 )
              {
                segments(adh.plot.space[2] + correct.earliest.followup.window + min(ppts$start,na.rm=TRUE), y.for.100perc,
                         adh.plot.space[2] + correct.earliest.followup.window + max(ppts$end,na.rm=TRUE), y.for.100perc,
                         lty="dotted", col="red"); # 0%
              }
              if( print.CMA && char.height.CMA <= 0.80 )
              {
                text(adh.plot.space[2] + correct.earliest.followup.window + min(ppts$start,na.rm=TRUE), min(ppts$y.norm,na.rm=TRUE),
                     sprintf("%.1f%%",100*min(ppts$y,na.rm=TRUE)), pos=2, cex=CMA.cex, col="black");
                text(adh.plot.space[2] + correct.earliest.followup.window + min(ppts$start,na.rm=TRUE), max(ppts$y.norm,na.rm=TRUE),
                     sprintf("%.1f%%",100*max(ppts$y,na.rm=TRUE)), pos=2, cex=CMA.cex, col="black");
                if( plot.partial.CMAs.as.timeseries.show.0perc && y.for.0perc >= y.cur + 0.5 )
                {
                  text(adh.plot.space[2] + correct.earliest.followup.window + min(ppts$start,na.rm=TRUE), y.for.0perc,
                       "0%", pos=2, cex=CMA.cex, col="red");
                }
                if( plot.partial.CMAs.as.timeseries.show.100perc && y.for.100perc <= y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0 )
                {
                  text(adh.plot.space[2] + correct.earliest.followup.window + min(ppts$start,na.rm=TRUE), y.for.100perc,
                       "100%", pos=2, cex=CMA.cex, col="red");
                }
              }

              # The points and connecting lines:
              if( !is.na(plot.partial.CMAs.as.timeseries.col.dot) )
              {
                points(ppts$x.plot, ppts$y.norm, col=plot.partial.CMAs.as.timeseries.col.dot, cex=CMA.cex, type="o", pch=19, lty="solid");
              }

              # The actual values:
              if( print.CMA && char.height.CMA <= 0.80 && !is.na(plot.partial.CMAs.as.timeseries.col.text) )
              {
                text(ppts$x.plot, ppts$y.norm, ppts$text, adj=c(0.5,-0.5), cex=CMA.cex, col=plot.partial.CMAs.as.timeseries.col.text);
              }
            }

            # Go to the next plot:
            y.cur <- y.cur + plot.partial.CMAs.as.timeseries.vspace;
          }
        }
      }
      curpat <- !curpat;
    }
  }

  # The days/dates axis and the grid at those important days/dates:
  if( period.in.days > 0 )
  {
    if( show.period=="dates" )
    {
        #axis( 1, at=adh.plot.space[2]+seq(0,as.numeric(endperiod),by=period.in.days),
        #      labels=as.character(earliest.date + round(seq(0,as.numeric(endperiod),by=period.in.days),1), format=cma$date.format),
        #      las=3, cex.axis=cex.axis);
        axis( 1, at=adh.plot.space[2]+seq(0,as.numeric(endperiod),by=period.in.days), labels=FALSE);
        axis.labels <- as.character(earliest.date + round(seq(0,as.numeric(endperiod),by=period.in.days),1), format=cma$date.format);
        # text(adh.plot.space[2]+seq(0,as.numeric(endperiod),by=period.in.days) - strwidth(axis.labels, cex=cex.axis)/2,
        #      par("usr")[3] - max(strheight(axis.labels, cex=cex.axis)),
        #      labels=axis.labels,
        #      cex=cex.axis, srt=30, adj=c(1,3), xpd=TRUE);
        text(adh.plot.space[2]+seq(0,as.numeric(endperiod),by=period.in.days),
             par("usr")[3],
             labels=axis.labels,
             cex=cex.axis, srt=30, adj=c(1,3), xpd=TRUE);
        abline( v=adh.plot.space[2]+seq(0,as.numeric(endperiod),by=period.in.days), lty="dotted", col=gray(0.5) );
        abline( v=adh.plot.space[2]+endperiod, lty="solid", col=gray(0.5) );
    } else
    {
        if( align.first.event.at.zero )
        {
            xpos <- c(correct.earliest.followup.window-seq(0,as.numeric(correct.earliest.followup.window),by=period.in.days),
                      seq(0,as.numeric(endperiod),by=period.in.days)+correct.earliest.followup.window);
            xpos <- xpos[ xpos >= 0 & xpos <= endperiod ];
            #axis( 1, at=adh.plot.space[2]+xpos,
            #      labels=as.character(round(xpos-correct.earliest.followup.window,1)),
            #      las=3, cex.axis=cex.axis);
            axis( 1, at=adh.plot.space[2]+xpos, labels=FALSE);
            axis.labels <- as.character(round(xpos-correct.earliest.followup.window,1));
            # text(adh.plot.space[2]+xpos - strwidth(axis.labels, cex=cex.axis)/2,
            #      par("usr")[3] - max(strheight(axis.labels, cex=cex.axis)),
            #      labels=axis.labels,
            #      cex=cex.axis, srt=30, adj=c(1,3), xpd=TRUE);
            text(adh.plot.space[2]+xpos,
                 par("usr")[3],
                 labels=axis.labels,
                 cex=cex.axis, srt=30, adj=c(1,3), xpd=TRUE);
            abline( v=adh.plot.space[2]+xpos, lty="dotted", col=gray(0.5) );
            abline( v=adh.plot.space[2]+endperiod, lty="solid", col=gray(0.5) );
        } else
        {
            #axis( 1, at=adh.plot.space[2]+seq(0,as.numeric(endperiod),by=period.in.days),
            #      labels=as.character(round(seq(0,as.numeric(endperiod),by=period.in.days),1)),
            #      las=3, cex.axis=cex.axis);
            axis( 1, at=adh.plot.space[2]+seq(0,as.numeric(endperiod),by=period.in.days), labels=FALSE);
            axis.labels <- as.character(round(seq(0,as.numeric(endperiod),by=period.in.days),1));
            # text(adh.plot.space[2]+seq(0,as.numeric(endperiod),by=period.in.days) - strwidth(axis.labels, cex=cex.axis)/2,
            #      par("usr")[3] - max(strheight(axis.labels, cex=cex.axis)),
            #      labels=axis.labels,
            #      cex=cex.axis, srt=30, adj=c(1,3), xpd=TRUE);
            text(adh.plot.space[2]+seq(0,as.numeric(endperiod),by=period.in.days),
                 par("usr")[3],
                 labels=axis.labels,
                 cex=cex.axis, srt=30, adj=c(1,3), xpd=TRUE);
            abline( v=adh.plot.space[2]+seq(0,as.numeric(endperiod),by=period.in.days), lty="dotted", col=gray(0.5) );
            abline( v=adh.plot.space[2]+endperiod, lty="solid", col=gray(0.5) );
        }
    }
  }

  # The legend:
  .legend <- function(x=0, y=0, width=1, height=1, do.plot=TRUE)
  {
    # Legend rectangle:
    if( do.plot ) rect(x, y, x + width, y + height, border=gray(0.6), lwd=2, col=rgb(0.99,0.99,0.99,legend.bkg.opacity));

    cur.y <- y + height; # current y
    max.width <- width; # maximum width

    # Legend title:
    if( do.plot ) text(x + width/2, cur.y, "Legend", pos=1, col=gray(0.3), cex=legend.cex.title);
    cur.y <- cur.y - strheight("Legend", cex=legend.cex.title) - 3*legend.char.height; max.width <- max(max.width, strwidth("Legend", cex=legend.cex.title));

    # Event:
    if( do.plot ) segments(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y, lty=lty.event, lwd=lwd.event, col="black");
    if( do.plot ) points(x + 1.0*legend.char.width, cur.y, pch=pch.start.event, cex=legend.cex, col="black");
    if( do.plot ) points(x + 4.0*legend.char.width, cur.y, pch=pch.end.event, cex=legend.cex, col="black");
    if( do.plot ) text(x + 5.0*legend.char.width, cur.y, "duration", col="black", cex=legend.cex, pos=4);
    cur.y <- cur.y - 1.5*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("duration", cex=legend.cex));
    if( do.plot ) segments(x + 1.0*legend.char.width, cur.y - 0.5*legend.char.height, x + 3.0*legend.char.width, cur.y - 0.5*legend.char.height, lty=lty.continuation, lwd=lwd.continuation, col=col.continuation);
    if( do.plot ) segments(x + 3.0*legend.char.width, cur.y - 0.5*legend.char.height, x + 3.0*legend.char.width, cur.y + 0.5*legend.char.height, lty=lty.continuation, lwd=lwd.continuation, col=col.continuation);
    if( do.plot ) text(x + 5.0*legend.char.width, cur.y, "connector", col="black", cex=legend.cex, pos=4);
    cur.y <- cur.y - 1.5*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("connector", cex=legend.cex));

    for( i in 1:length(cols) )
    {
      med.class.name <- names(cols)[i]; med.class.name <- ifelse(is.na(med.class.name),"<missing>",med.class.name);
      if( do.plot ) rect(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y - 1.0*legend.char.height, border="black", col=adjustcolor(cols[i],alpha.f=0.5));
      if( do.plot )
      {
        med.class.name <- names(cols)[i]; med.class.name <- ifelse(is.na(med.class.name),"<missing>",med.class.name);
        if( print.dose || plot.dose )
        {
          dose.for.cat <- (dose.range$category == med.class.name);
          if( sum(dose.for.cat,na.rm=TRUE) == 1 )
          {
            med.class.name <- paste0(med.class.name," (",dose.range$min[dose.for.cat]," - ",dose.range$max[dose.for.cat],")");
          }
        }
        text(x + 5.0*legend.char.width, cur.y - 0.5*legend.char.height, med.class.name, col="black", cex=legend.cex, pos=4);
      }
      cur.y <- cur.y - 1.5*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth(names(cols)[i], cex=legend.cex));
    }
    cur.y <- cur.y - 0.5*legend.char.height;

    # Follow-up window:
    if( highlight.followup.window )
    {
      if( do.plot ) rect(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y - 1.0*legend.char.height, border=followup.window.col, lty="dotted", lwd=2, col=rgb(1,1,1,0.0));
      if( do.plot ) text(x + 5.0*legend.char.width, cur.y - 0.5*legend.char.height, "follow-up wnd.", col="black", cex=legend.cex, pos=4);
      cur.y <- cur.y - 2.0*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("follow-up wnd.", cex=legend.cex));
    }

    # Observation window:
    if( highlight.observation.window )
    {
      if( do.plot ) rect(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y - 1.0*legend.char.height, border=rgb(1,1,1,0.0), col=adjustcolor(observation.window.col,alpha.f=observation.window.opacity), density=observation.window.density, angle=observation.window.angle);
      if( do.plot ) text(x + 5.0*legend.char.width, cur.y - 0.5*legend.char.height, "observation wnd.", col="black", cex=legend.cex, pos=4);
      cur.y <- cur.y - 2.0*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("observation wnd.", cex=legend.cex));
    }

    # Required size:
    return (c("width" =max.width + 5.0*legend.char.width,
              "height"=(y + height - cur.y) + 1.0*legend.char.height));
  }
  if( show.legend )
  {
    # Character size for the legend:
    legend.char.width <- strwidth("O",cex=legend.cex); legend.char.height <- strheight("O",cex=legend.cex);

    legend.size <- .legend(do.plot=FALSE);
    if( is.na(legend.x) || legend.x == "right" )
    {
      legend.x <- par("usr")[2] - legend.size["width"] - legend.char.width;
    } else if( legend.x == "left" )
    {
      legend.x <- par("usr")[1] + legend.char.width;
    } else if( !is.numeric(legend.x) && length(legend.x) != 1 )
    {
      legend.x <- par("usr")[2] - legend.size["width"] - legend.char.width;
    }
    if( is.na(legend.y) || legend.y == "bottom" )
    {
      legend.y <- par("usr")[3] + legend.char.height;
    } else if( legend.y == "top" )
    {
      legend.y <- par("usr")[4] - legend.size["height"] - legend.char.height;
    } else if( !is.numeric(legend.y) && length(legend.y) != 1 )
    {
      legend.y <- par("usr")[3] + legend.char.height;
    }
    ret.val <- .legend(legend.x, legend.y, as.numeric(legend.size["width"]), as.numeric(legend.size["height"]));
  }
  else
  {
    ret.val <- c("width"=NA, "height"=NA);
  }

  par(old.par); # restore graphical params
  return (invisible(ret.val));
}










