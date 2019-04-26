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

## TODO ####
#
# string height & width in SVG
# make sure the image resizes well
# check colors, etc, consistency
# image dimensions (also for export)
#
# HTML + CSS + JavaScript
#
# test
# profile & optimise
#

# Grayscale colors palette:
.bw.colors <- function(n)
{
  gray.colors(n, start=0, end=0.5);
}


# # Draws shadowed/outlined text (taken directly from TeachingDemos to reduced the dependencies on other packages):
# .shadow.text <- function(x, y=NULL, labels, col='white', bg='black', theta= seq(pi/4, 2*pi, length.out=8), r=0.1, ... )
# {
#
#   xy <- xy.coords(x,y);
#   xo <- r*strwidth('A');
#   yo <- r*strheight('A');
#
#   for (i in theta) text( xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col=bg, ... );
#   text(xy$x, xy$y, labels, col=col, ... );
# }

# Draws text on a semi-transparent background:
.shadow.text <- function(x, y=NULL, labels, col='white', bg='black', alpha=0.25, cex=1.0, ... )
{
  #browser()
  xy <- xy.coords(x,y);
  w <- strwidth(labels, cex=cex); h <- strheight(labels, cex=cex); w0 <- strwidth("0", cex=cex); h0 <- strheight("0", cex=cex);
  rect(xy$x-w/2-w0/4, xy$y-h/2-h0/4, xy$x+w/2+w0/4, xy$y+h/2+h0/4, col=scales::alpha(bg, alpha=alpha), border=NA);
  text(xy$x, xy$y, labels, col=col, cex=cex);
}


## SVG special functions and constants ####

.SVG.number <- function(n, prec=3)
{
  if( is.numeric(n) ) as.character(round(n,prec)) else n;
}

# Replace special characters with XML/HTML entities
# Inspired by htmlspecialchars() in package "fun"
# and HTMLdecode()/HTMLencode() in package "textutils"
.SVG.specialchars.2.XMLentities <- function(s)
{
  spec.chars <- c("&amp;"="&",
                  "&quot;"='"',
                  "&#039;"="'",
                  "&lt;"="<",
                  "&gt;"=">");
  for (i in seq_along(spec.chars))
  {
    s <- gsub(spec.chars[i], names(spec.chars)[i], s, fixed = TRUE);
  }
  return (s);
}

.SVG.comment <- function(s,
                         newpara=FALSE, # should there be a newline before the comment?
                         newline=TRUE, # should a newline be added at the end?
                         return_string=FALSE # return a singe string or a vector of strings to be concatenated later?
)
{
  r <- c(if(newpara) '\n',
         '<!-- ',s,' -->',
         if(newline) '\n');
  if( return_string ) return (paste0(r,collapse="")) else return (r);
}

.SVG.color <- function(col,
                       return_string=FALSE)
{
  if( col == "none" )
  {
    return ('none');
  } else
  {
    if( return_string )
    {
      return (paste0("rgb(",paste0(col2rgb(col),collapse=","),")"));
    } else
    {
      return (c('rgb(', {x <- col2rgb(col); c(x[1],',',x[2],',',x[3])}, ')'));
    }
  }
}

# Stroke dash-arrays for line types (lty):
.SVG.lty <- data.frame("lty"=0:6,
                       "names"=c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                       "stroke"=c("none", NA, NA, NA, NA, NA, NA),
                       "stroke-dasharray"=c(NA, NA, "3,3", "1,2", "1,2,3,2", "5,2", "2,2,4,2"),
                       stringsAsFactors=FALSE);

.SVG.rect <- function(x=NA, y=NA, width=NA, height=NA, xend=NA, yend=NA,  # can accomodate both (wdith,height) and (xend,yend)
                      stroke=NA, stroke_width=NA, lty=NA, stroke_dasharray=NA, fill="white", fill_opacity=NA, other_params=NA, # styling attributes
                      id=NA, class=NA, comment=NA,  # ID and comment
                      newline=TRUE, # should a newline be added at the end?
                      return_string=FALSE # return a singe string or a vector of strings to be concatenated later?
)
{
  # Check for missing data:
  if( is.na(x) || is.na(y) || (is.na(width) && is.na(xend)) || (is.na(height) && is.na(yend)) )
  {
    # Nothing to plot!
    if( return_string ) return ("") else return (NULL);
  }

  # Process lty first:
  if( !is.na(lty) )
  {
    if( is.numeric(lty) ) s <- which(.SVG.lty$lty == lty) else s <- which(.SVG.lty$names == as.character(lty));
    if( length(s) == 1 )
    {
      if( !is.na(.SVG.lty$stroke[s]) ) stroke <- .SVG.lty$stroke[s];
      stroke_dasharray <- .SVG.lty$stroke.dasharray[s];
    }
  }

  r <-  c(# The initial comment (if any):
          if(!is.na(comment)) .SVG.comment(comment),

          # The rect element:
          '<rect ',

          # The id and class (if any):
          if(!is.na(id)) c('id="',id,'" '),
          if(!is.na(class)) c('class="',class,'" '),

          # The x and y coordinates of the bottom-left corner:
          if(!is.na(x)) c('x="',.SVG.number(x),'" '),
          if(!is.na(y)) c('y="',.SVG.number(y),'" '),

          # The width and height of the rectangle (either given directly or computed from the top-right corner coordinates):
          if(!is.na(width))  c('width="', .SVG.number(width), '" ') else if(!is.na(xend)) c('width="',.SVG.number(xend-x),'" '),
          if(!is.na(height)) c('height="',.SVG.number(height),'" ') else if(!is.na(yend)) c('height="',.SVG.number(yend-y),'" '),

          # Aesthetics:
          if(!is.na(stroke)) c('stroke="', .SVG.color(stroke), '" '),
          if(!is.na(stroke_width)) c('stroke-width="',stroke_width,'" '),
          if(!is.na(stroke_dasharray)) c('stroke-dasharray="',stroke_dasharray,'" '),
          if(!is.na(fill)) c('fill="', .SVG.color(fill), '" '),
          if(!is.na(fill_opacity)) c('fill-opacity="',fill_opacity,'" '),
          # Other parameters:
          if(!is.na(other_params)) other_params,

          # Close the element:
          '/>',
          # Add ending newline (if so required):
          if(newline) '\n'
        );
  if( return_string ) return (paste0(r,collapse="")) else return (r);
}

.SVG.lines <- function(x, y,  # the coordinates of the points (at least 2)
                       connected=FALSE, # are the lines connected or not?
                       stroke=NA, stroke_width=NA, lty=NA, stroke_dasharray=NA, other_params=NA, # styling attributes (may be one per line for connected==FALSE)
                       id=NA, class=NA, comment=NA,  # ID and comment
                       newline=TRUE, # should a newline be added at the end?
                       return_string=FALSE # return a singe string or a vector of strings to be concatenated later?
)
{
  # Preconditions:
  if( length(x) != length(y) || length(x) < 2 || length(y) < 2 )
  {
    warning("The line point coodinates must be of the same length >= 2.\n");
    if( return_string ) return ("") else return (NULL);
  }

  r <-  c(# The initial comment (if any):
    if(!is.na(comment)) .SVG.comment(comment));

  if(connected)
  {
    # One 'polyline' elememet:

    # Process lty:
    if( length(lty) > 0 && !is.na(lty) )
    {
      lty.cur <- lty[1]; # consider only the first one
      if( is.numeric(lty.cur) ) s <- which(.SVG.lty$lty == lty.cur) else s <- which(.SVG.lty$names == as.character(lty.cur));
      if( length(s) == 1 )
      {
        if( !is.na(.SVG.lty$stroke[s]) ) stroke <- .SVG.lty$stroke[s];
        stroke_dasharray <- .SVG.lty$stroke.dasharray[s];
      }
    }

    # Remove any points with NA coordinates:
    s <- (!is.na(x) & !is.na(y));
    if( !any(s) )
    {
      # Nothing to plot:
      if( return_string ) return ("") else return (NULL);
    }
    x <- x[s]; y <- y[s]; # Keep only the non-missing points

    r <- c(r,
           '<polyline ',

           # The id and class (if any):
           if(!is.na(id)) c('id="',id,'" '),
           if(!is.na(class)) c('class="',class,'" '),

           # The coordinates of the points as pairs separated by ',':
           'points="', unlist(lapply(seq_along(x), function(i) c(.SVG.number(x[i]),",",.SVG.number(y[i])," "))),'" ',

           # Aesthetics:
           'fill="none" ',
           if(!is.na(stroke)) c('stroke="', .SVG.color(stroke), '" '),
           if(!is.na(stroke_width)) c('stroke-width="',stroke_width,'" '),
           if(!is.na(stroke_dasharray)) c('stroke-dasharray="',stroke_dasharray,'" '),
           # Other parameters:
           if(!is.na(other_params)) other_params,

           # Close the element:
           '/>',
           # Add ending newline (if so required):
           if(newline) '\n'
    );
  } else
  {
    # Multiple 'line' elements:
    if( length(x) %% 2 != 0 )
    {
      warning("For unconnected lines there must an even number of point coordinates.\n");
      return (NULL);
    }

    for(i in seq(1,length(x),by=2) )
    {
      # Check for missing coordinates:
      if( is.na(x[i]) || is.na(x[i+1]) || is.na(y[i]) || is.na(y[i+1]) ) next; # cannot draw this line

      # Process lty:
      if( length(lty) > 0 && all(!is.na(lty)) )
      {
        if( length(lty) == length(x)/2 ) lty.cur <- lty[(i+1)/2] else lty.cur <- lty[1]; # consider the corresponding lty or only first one
        if( is.numeric(lty.cur) ) s <- which(.SVG.lty$lty == lty.cur) else s <- which(.SVG.lty$names == as.character(lty.cur));
        if( length(s) == 1 )
        {
          if( !is.na(.SVG.lty$stroke[s]) ) stroke <- .SVG.lty$stroke[s];
          stroke_dasharray <- .SVG.lty$stroke.dasharray[s];
        }
      }

      r <- c(r,
             '<line ',

             # The id (if any):
             if(!is.na(id)) c('id="',id,'" '),

             # The cooridnates of the points:
             'x1="', .SVG.number(x[i]), '" ',
             'y1="', .SVG.number(y[i]), '" ',
             'x2="', .SVG.number(x[i+1]), '" ',
             'y2="', .SVG.number(y[i+1]), '" ',

             # Aesthetics:
             if(!is.na(stroke)) c('stroke="', .SVG.color(stroke), '" '),
             if(!is.na(stroke_width)) c('stroke-width="',stroke_width,'" '),
             if(!is.na(stroke_dasharray)) c('stroke-dasharray="',stroke_dasharray,'" '),
             # Other parameters:
             if(!is.na(other_params)) other_params,

             # Close the element:
             '/>',
             # Add ending newline (if so required):
             if(newline) '\n'
      );
    }
  }

  if( return_string ) return (paste0(r,collapse="")) else return (r);
}

.SVG.points <- function(x, y, pch=0,
                        col="black", cex=1.0, other_params=NA, # styling attributes
                        id=NA, class=NA, comment=NA,  # ID and comment
                        newline=TRUE, # should a newline be added at the end?
                        return_string=FALSE # return a singe string or a vector of strings to be concatenated later?
)
{
  # Preconditions:
  if( length(x) != length(y) || length(x) == 0 )
  {
    warning("There must be at least on point.\n");
    return (NULL);
  }

  # Make sure the point attributes are correctly distributed:
  if( length(pch) != length(x) ) pch <- rep(pch[1], length(x));
  if( length(col) != length(x) ) col <- rep(col[1], length(x));
  if( length(cex) != length(x) ) cex <- rep(cex[1], length(x));

  # Remove any points with NA coordinates:
  s <- (!is.na(x) & !is.na(y) & !is.na(pch));
  if( !any(s) )
  {
    # Nothing to plot:
    if( return_string ) return ("") else return (NULL);
  }
  x <- x[s]; y <- y[s]; pch <- pch[s]; col <- col[s]; cex <- cex[s]; # Keep only the non-missing points

  r <-  c(# The initial comment (if any):
    if(!is.na(comment)) .SVG.comment(comment));

  for(i in seq_along(x))
  {
    r <-  c(r,
            # The element:
            '<g ',

            # The id and class (if any):
            if(!is.na(id)) c('id="',id,'" '),
            if(!is.na(class)) c('class="',class,'" '),
            '>',

            # Reuse the predefined symbol:
            '<use xlink:href="#pch',pch[i],'" ',

            # The coordinates and size:
            'transform="translate(',.SVG.number(x[i]),' ',.SVG.number(y[i]),') scale(',cex[i],')" ',

            # Aesthetics:
            if(!is.na(col[i])) c('stroke="', .SVG.color(col[i]), '" ', 'fill="', .SVG.color(col[i]), '" '),
            # Other parameters:
            if(!is.na(other_params)) other_params,

            # Close the element:
            '/></g>',
            # Add ending newline (if so required):
            if(newline) '\n'
    );
  }

  if( return_string ) return (paste0(r,collapse="")) else return (r);
}

.SVG.text <- function(x, y, text,
                      col="black", font="Arial", font_size=16,
                      h.align=c(NA,"left","center","right")[1], v.align=c(NA,"top","center","bottom")[1], # alignment
                      rotate=NA, # rotation in degrees
                      other_params=NA, # styling attributes
                      id=NA, class=NA, comment=NA,  # ID and comment
                      newline=TRUE, # should a newline be added at the end?
                      return_string=FALSE # return a singe string or a vector of strings to be concatenated later?
)
{
  # Preconditions:
  if( length(x) != length(y) || length(x) != length(text) || length(x) == 0 )
  {
    warning("There must be at least one text and the number of texts should matche the number of coordinates.\n");
    return (NULL);
  }

  # Make sure the attributes are correctly distributed:
  if( length(col) != length(x) ) col <- rep(col[1], length(x));
  if( length(font) != length(x) ) font <- rep(font[1], length(x));
  if( length(font_size) != length(x) ) font_size <- rep(font_size[1], length(x));
  if( length(h.align) != length(x) ) h.align <- rep(h.align[1], length(x));
  if( length(v.align) != length(x) ) v.align <- rep(v.align[1], length(x));
  if( length(rotate) != length(x) ) rotate <- rep(rotate[1], length(x));

  # Remove any points with NA coordinates:
  s <- (!is.na(x) & !is.na(y) & !is.na(text));
  if( !any(s) )
  {
    # Nothing to plot:
    if( return_string ) return ("") else return (NULL);
  }
  x <- x[s]; y <- y[s]; col <- col[s]; font <- font[s]; font_size <- font_size[s]; h.align <- h.align[s]; v.align <- v.align[s]; rotate <- rotate[s]; # Keep only the non-missing points

  r <-  c(# The initial comment (if any):
    if(!is.na(comment)) .SVG.comment(comment));

  for(i in seq_along(x))
  {
    r <-  c(r,
            # The element:
            '<text ',

            # The id and class (if any):
            if(!is.na(id)) c('id="',id,'" '),
            if(!is.na(class)) c('class="',class,'" '),

            # The coordinates:
            'x="',.SVG.number(x[i]),'" y="',.SVG.number(y[i]),'" ',

            # The font:
            'font-family="',font[i],'" font-size="',font_size[i],'" ',

            # The alignment:
            if(!is.na(h.align[i])) c('text-anchor="',switch(h.align[i], "left"="start", "center"="middle", "right"="end"),'" '),
            #if(!is.na(v.align[i])) c('alignment-baseline="',switch(v.align[i], "top"="auto", "center"="central", "bottom"="baseline"),'" '),
            if(!is.na(v.align[i]) && v.align[i]!="top") c('dominant-baseline="',switch(v.align[i], "center"="central", "bottom"="text-before-edge"),'" '),

            # Rotation:
            if(!is.na(rotate[i])) c('transform="rotate(',rotate[i],' ',.SVG.number(x[i]),' ',.SVG.number(y[i]),')" '),

            # Aesthetics:
            if(!is.na(col[i])) c('fill="', .SVG.color(col[i]), '" '),
            # Other parameters:
            if(!is.na(other_params)) other_params,

            # Close the element:
            '>',

            # The text:
            .SVG.specialchars.2.XMLentities(text[i]),

            # Close it:
            '</text>',

            # Add ending newline (if so required):
            if(newline) '\n'
    );
  }

  if( return_string ) return (paste0(r,collapse="")) else return (r);
}

# For a given font, style, font size and cex, compute the string's width and height in pixels
# family cam be "serif", "sans" or "mono"; font can be 1 = plain text, 2 = bold face, 3 = italic and 4 = bold italic; font_size in pixels; cex as for points
.SVG.string.dims <- function(s, family="sans", font=1, font_size=10, cex=1.0)
{
  # Actual font size:
  font_size_cex <- (font_size * cex);

  # The number of lines of text:
  no.lines <- length(grep("\n",s,fixed=TRUE)) + 1;

  ## The stupid way:
  #return (c("width"=(nchar(s) - no.lines + 1) * font_size_cex,
  #          "height"=no.lines * font_size_cex));

  # Slightly better way: use "M" as the reference and compute everything relative to it (use the ):
  M.h <- strheight("M",units="inches"); M.w <- strwidth("M",units="inches");
  s.h <- strheight(s,units="inches");   s.w <- strwidth(s,units="inches");
  return (c("width"=(s.w / M.w) * font_size_cex,
            "height"=(s.h / M.h) * font_size_cex));
}


# Make this function produce SVG
# (and for now display it as well to maintain compatibility with the old function)
.plot.CMAs <- function(cma,                                   # the CMA_per_episode or CMA_sliding_window (or derived) object
                       patients.to.plot=NULL,                 # list of patient IDs to plot or NULL for all
                       duration=NA,                           # duration and end period to plot in days (if missing, determined from the data)
                       align.all.patients=FALSE, align.first.event.at.zero=TRUE, # should all patients be aligned? and, if so, place the first event as the horizontal 0?
                       show.period=c("dates","days")[2],      # draw vertical bars at regular interval as dates or days?
                       period.in.days=90,                     # the interval (in days) at which to draw veritcal lines
                       show.legend=TRUE, legend.x="right", legend.y="bottom", legend.bkg.opacity=0.5, legend.cex=0.75, legend.cex.title=1.0, # legend params and position
                       cex=1.0, cex.axis=0.75, cex.lab=1.0, cex.title=1.5,   # various graphical params
                       show.cma=TRUE,                         # show the CMA type
                       xlab=c("dates"="Date", "days"="Days"), # Vector of x labels to show for the two types of periods, or a single value for both, or NULL for nothing
                       ylab=c("withoutCMA"="patient", "withCMA"="patient (& CMA)"), # Vector of y labels to show without and with CMA estimates, or a single value for both, or NULL ofr nonthing
                       title=c("aligned"="Event patterns (all patients aligned)", "notaligned"="Event patterns"), # Vector of titles to show for and without alignment, or a single value for both, or NULL for nonthing
                       col.cats=rainbow,                      # single color or a function mapping the categories to colors
                       unspecified.category.label="drug",     # the label of the unspecified category of medication
                       medication.groups=NULL,                # optionally, the groups of medications (implictely all are part of the same group)
                       lty.event="solid", lwd.event=2, pch.start.event=15, pch.end.event=16, # event style
                       show.event.intervals=TRUE,             # show the actual prescription intervals
                       print.dose=FALSE, cex.dose=0.75, print.dose.col="black", print.dose.outline.col=NA, print.dose.centered=FALSE, # print daily dose
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
                       show.real.obs.window.start=TRUE, real.obs.window.density=35, real.obs.window.angle=30, # for CMA8, the real observation window starts at a different date
                       alternating.bands.cols=c("white", "gray95"), # the colors of the alternating vertical bands across patients (NULL=don't draw any; can be >= 1 color)
                       rotate.text=-60,                 # some text (e.g., axis labels) may be rotated by this much degrees
                       bw.plot=FALSE,                   # if TRUE, override all user-given colors and replace them with a scheme suitable for grayscale plotting
                       min.plot.size.in.characters.horiz=0, min.plot.size.in.characters.vert=0, # the minimum plot size (in characters: horizontally, for the whole duration, vertically, per event (and, if shown, per episode/sliding window))
                       max.patients.to.plot=100,        # maximum number of patients to plot
                       suppress.warnings=FALSE,         # suppress warnings?
                       export.formats=NULL,             # the formats to export the figure to (by default, none); can be any subset of "svg" (just SVG file), "svg-and-html" (SVG + HTML + CSS + JavaScript as independent files), "svg-in-html" (SVG + HTML + CSS + JavaScript all contained in the HTML document), "png", "webp", "ps" and "pdf"
                       export.formats.fileprefix="AdhereR-plot", # the file name prefix for the exported formats
                       export.formats.directory=NA,     # if exporting, which directory to export to (if not give, creates files in the temporary directory)
                       generate.R.plot=FALSE,           # generate standardR plot for plotting within R?
                       ...
)
{

  ## DEBUG ####
  export.formats <- c("svg", "svg-and-html", "svg-in-html", "png", "webp", "ps", "pdf");
  export.formats.directory <- "~/Temp/tmp";
  generate.R.plot <- TRUE;
  ## END DEBUG ####


  # What sorts of plots to generate (use short names for short if statements):
  .do.R <- generate.R.plot; .do.SVG <- (!is.null(export.formats) && any(c("svg", "svg-and-html", "svg-in-html", "png", "webp", "ps", "pdf") %in% export.formats));
  if( !.do.R && !.do.SVG )
  {
    # Nothing to plot!
    return (invisible(NULL));
  }


  ##
  ## Initialise the SVG file content ####
  ##
  ## Things to remeber about SVGs:
  ##   - coordinates start top-left and go rigth and bottom
  ##   - font size is relative to the viewBox
  ##

  # The SVG header and string (body):
  svg.header <- c('<?xml version="1.0" standalone="no"?>\n',
                  '<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">\n');
  svg.str <- NULL;


  ##
  ## Set-up, checks and local functions ####
  ##

  # Preconditions:
  if( is.null(cma) ||                                                                                            # must be: non-null
      !(inherits(cma, "CMA_per_episode") || inherits(cma, "CMA_sliding_window") || inherits(cma, "CMA0")) ||     # a proper CMA object
      is.null(cma$data) || nrow(cma$data) < 1 || !inherits(cma$data, "data.frame") ||                            # that containins non-null data derived from data.frame
      is.na(cma$ID.colname) || !(cma$ID.colname %in% names(cma$data)) ||                                         # has a valid patient ID column
      is.na(cma$event.date.colname) || !(cma$event.date.colname %in% names(cma$data)) ||                         # has a valid event date column
      is.na(cma$event.duration.colname) || !(cma$event.duration.colname %in% names(cma$data))                    # has a valid event duration column
  )
  {
    if( !suppress.warnings ) warning(paste0("Can only plot a correctly specified CMA object (i.e., with valid data and column names)!\n"));
    return (invisible(NULL));
  }


  # Local functions for the various types of summary CMA plots:
  .plot.summary.CMA.as.histogram <- function(adh, svg.str)
  {
    adh.hist <- hist(adh, plot=FALSE);
    adh.x <- adh.hist$breaks[-1]; adh.x.0 <- min(adh.x,0); adh.x.1 <- max(adh.x,1); adh.x <- (adh.x - adh.x.0) / (adh.x.1 - adh.x.0);
    adh.y <- adh.hist$counts; adh.y <- adh.y / max(adh.y);
    adh.x.max <- adh.x[which.max(adh.hist$counts)];

    if( .do.R ) # Rplot
    {
      segments(.rescale.xcoord.for.CMA.plot(adh.x), y.mean - 2, .rescale.xcoord.for.CMA.plot(adh.x), y.mean - 2 + 4*adh.y, lty="solid", lwd=1, col=CMA.plot.border);
      if( char.height.CMA <= abs(.rescale.xcoord.for.CMA.plot(1.0) - .rescale.xcoord.for.CMA.plot(0.0)) )
      {
        # There's enough space for vertically writing all three of them:
        text(x=.rescale.xcoord.for.CMA.plot(0.0),       y.mean - 2 - char.height.CMA/2,
             sprintf("%.1f%%",100*min(adh.x.0,na.rm=TRUE)), srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
        text(x=.rescale.xcoord.for.CMA.plot(1.0),       y.mean - 2 - char.height.CMA/2,
             sprintf("%.1f%%",100*max(adh.x.1,na.rm=TRUE)), srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
        text(x=.rescale.xcoord.for.CMA.plot(adh.x.max), y.mean + 2 + char.height.CMA/2,
             sprintf("%d",max(adh.hist$counts,an.rm=TRUE)), srt=90, pos=3, cex=CMA.cex, col=CMA.plot.text);
      }
    }

    if( .do.SVG ) # SVG
    {
      svg.str <- c(svg.str,
                   .SVG.comment("The CMA summary as histogram", newpara=TRUE));

      for( j in seq_along(adh.x) )
      {
        svg.str <- c(svg.str,
                     # The CMA as histogram:
                     .SVG.lines(x=rep(.scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(adh.x[j])),2),
                                y=c(.scale.y.to.SVG.plot(y.mean - 2), .scale.y.to.SVG.plot(y.mean - 2 + 4*adh.y[j])),
                                connected=FALSE,
                                stroke=CMA.plot.border, stroke_width=1,
                                class="cma-summary-plot")
        );
      }
      if( 3*dims.chr.cma <= abs(.scale.width.to.SVG.plot(.rescale.xcoord.for.CMA.plot(1.0) - .rescale.xcoord.for.CMA.plot(0.0))) )
      {
        # There's enough space for vertically writing all three of them:
        svg.str <- c(svg.str,
                     # The CMA as histogram:
                     .SVG.text(x=c(.scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(0.0)),
                                   .scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(1.0)),
                                   .scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(adh.x.max))),
                               y=c(.scale.y.to.SVG.plot(y.mean - 2 - 0.25),
                                   .scale.y.to.SVG.plot(y.mean - 2 - 0.25),
                                   .scale.y.to.SVG.plot(y.mean + 2 + 0.25)),
                               text=c(sprintf("%.1f%%",100*min(adh.x.0,na.rm=TRUE)),
                                      sprintf("%.1f%%",100*max(adh.x.1,na.rm=TRUE)),
                                      sprintf("%d",max(adh.hist$counts,an.rm=TRUE))),
                               col=CMA.plot.text, font_size=dims.chr.cma,
                               h.align=c("right","right","left"),
                               v.align="center",
                               rotate=c(-(90+rotate.text),-(90+rotate.text),-90),
                               class="cma-summary-text")
        );
      }
    }

    return (svg.str);
  }

  .plot.summary.CMA.as.density <- function(adh.x, adh.y, svg.str)
  {
    adh.x.0 <- min(adh.x,0); adh.x.1 <- max(adh.x,1); adh.x <- (adh.x - adh.x.0) / (adh.x.1 - adh.x.0);
    adh.y <- (adh.y - min(adh.y)) / (max(adh.y) - min(adh.y));

    if( .do.R ) # Rplot:
    {
      points(.rescale.xcoord.for.CMA.plot(adh.x), y.mean - 2 + 4*adh.y, type="l", col=CMA.plot.border);
      if( char.height.CMA <= abs(.rescale.xcoord.for.CMA.plot(1) - .rescale.xcoord.for.CMA.plot(0)) )
      {
        # There's enough space for vertical writing:
        text(x=.rescale.xcoord.for.CMA.plot(0.0), y.mean - 2 - char.height.CMA/2, sprintf("%.1f%%",100*adh.x.0), srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
        text(x=.rescale.xcoord.for.CMA.plot(1.0), y.mean - 2 - char.height.CMA/2, sprintf("%.1f%%",100*adh.x.1), srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
      }
    }

    if( .do.SVG ) # SVG:
    {
      svg.str <- c(svg.str,
                   .SVG.comment("The CMA summary as density", newpara=TRUE));

      svg.str <- c(svg.str,
                   # The individual lines:
                   .SVG.lines(x=.scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(adh.x)),
                              y=.scale.y.to.SVG.plot(y.mean - 2 + 4*adh.y),
                              connected=TRUE,
                              stroke=CMA.plot.border, stroke_width=1,
                              class="cma-summary-plot")
      );
      if( 2*dims.chr.cma <= abs(.scale.width.to.SVG.plot(.rescale.xcoord.for.CMA.plot(1.0) - .rescale.xcoord.for.CMA.plot(0.0))) )
      {
        # There's enough space for vertical writing:
        svg.str <- c(svg.str,
                     # The actual values as text:
                     .SVG.text(x=c(.scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(0.0)),
                                   .scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(1.0))),
                               y=c(.scale.y.to.SVG.plot(y.mean - 2 - 0.25),
                                   .scale.y.to.SVG.plot(y.mean - 2 - 0.25)),
                               text=c(sprintf("%.1f%%",100*adh.x.0),
                                      sprintf("%.1f%%",100*adh.x.1)),
                               col=CMA.plot.text, font_size=dims.chr.cma,
                               h.align=c("right","right"), v.align="center", rotate=rotate.text,
                               class="cma-summary-text")
        );
      }
    }

    return (svg.str);
  }

  .plot.summary.CMA.as.lines <- function(adh, svg.str)
  {
    adh.x.0 <- min(adh,0); adh.x.1 <- max(adh,1); adh.x <- (adh - adh.x.0) / (adh.x.1 - adh.x.0);

    if( .do.R ) # Rplot:
    {
      segments(.rescale.xcoord.for.CMA.plot(adh.x), y.mean - 2, .rescale.xcoord.for.CMA.plot(adh.x), y.mean - 2 + 4, lty="solid", lwd=2, col=CMA.plot.border);
      if( char.height.CMA*length(adh) <= abs(.rescale.xcoord.for.CMA.plot(1) - .rescale.xcoord.for.CMA.plot(0)) )
      {
        # There's enough space for vertical writing all of them (alternated):
        for( j in 1:length(adh) )
        {
          text(x=.rescale.xcoord.for.CMA.plot(adh.x[j]), y.mean + ifelse(j %% 2==0, 2 + char.height.CMA/2, -2 - char.height.CMA/2),
               sprintf("%.1f%%",100*adh[j]), srt=90, pos=ifelse(j %% 2==0, 3, 1), cex=CMA.cex, col=CMA.plot.text);
        }
      } else if( char.height.CMA <= abs(.rescale.xcoord.for.CMA.plot(1) - .rescale.xcoord.for.CMA.plot(0)) )
      {
        # There's enough space for vertical writing only the extremes:
        text(x=.rescale.xcoord.for.CMA.plot(adh.x[1]),           y.mean - 2 - char.height.CMA/2,
             sprintf("%.1f%%",100*adh[1]),           srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
        text(x=.rescale.xcoord.for.CMA.plot(adh.x[length(adh)]), y.mean - 2 - char.height.CMA/2,
             sprintf("%.1f%%",100*adh[length(adh)]), srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
      }
    }

    if( .do.SVG ) # SVG:
    {
      svg.str <- c(svg.str,
                   .SVG.comment("The CMA summary as barplot", newpara=TRUE));

      for( j in seq_along(adh.x) )
        svg.str <- c(svg.str,
                     # The individual lines:
                     .SVG.lines(x=rep(.scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(adh.x[j])),2),
                                y=c(.scale.y.to.SVG.plot(y.mean - 2), .scale.y.to.SVG.plot(y.mean - 2 + 4)),
                                connected=FALSE,
                                stroke=CMA.plot.border, stroke_width=2,
                                class="cma-summary-plot")
        );
      if( length(adh)*dims.chr.cma <= abs(.scale.width.to.SVG.plot(.rescale.xcoord.for.CMA.plot(1.0) - .rescale.xcoord.for.CMA.plot(0.0))) )
      {
        # There's enough space for vertical writing all of them (alternated):
        svg.str <- c(svg.str,
                     # The actual values as text:
                     .SVG.text(x=.scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(adh.x)),
                               y=.scale.y.to.SVG.plot(y.mean + rep(c(-2 - 0.25, 2 + 0.25),times=length(adh))[1:length(adh)]),
                               text=sprintf("%.1f%%",100*adh),
                               col=CMA.plot.text, font_size=dims.chr.cma,
                               h.align=rep(c("right", "left"),times=length(adh))[1:length(adh)], v.align="center", rotate=rotate.text,
                               class="cma-summary-text")
        );
      } else if( 2*dims.chr.cma <= abs(.scale.width.to.SVG.plot(.rescale.xcoord.for.CMA.plot(1.0) - .rescale.xcoord.for.CMA.plot(0.0))) )
      {
        # There's enough space for vertical writing only the extremes:
        svg.str <- c(svg.str,
                     # The actual values as text:
                     .SVG.text(x=c(.scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(adh.x[1])),
                                   .scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(adh.x[length(adh)]))),
                               y=c(.scale.y.to.SVG.plot(y.mean - 2 - 0.25),
                                   .scale.y.to.SVG.plot(y.mean - 2 - 0.25)),
                               text=c(sprintf("%.1f%%",100*adh[1]),
                                      sprintf("%.1f%%",100*adh[length(adh)])),
                               col=CMA.plot.text, font_size=dims.chr.cma,
                               h.align=c("right","right"), v.align="center",
                               rotate=c(-90,-90),
                               class="cma-summary-text")
        );
      }
    }

    return (svg.str);
  }


  # Is the cma a time series or per episodes?
  is.cma.TS.or.SW <- (inherits(cma, "CMA_per_episode") || inherits(cma, "CMA_sliding_window"));
  # Does the cma contains estimated CMAs?
  has.estimated.CMA <- !is.null(getCMA(cma));

  # Convert data.table to data.frame (basically, to guard against inconsistencies between data.table and data.frame in how they handle d[,i]):
  if( inherits(cma$data, "data.table") ) cma$data <- as.data.frame(cma$data);

  # Check compatibility between subtypes of plots:
  if( align.all.patients && show.period != "days" ){ show.period <- "days"; if( !suppress.warnings ) warning("When aligning all patients, cannot show actual dates: showing days instead!\n"); }


  ##
  ## Select patients ####
  ##

  # The patients:
  patids <- unique(as.character(cma$data[,cma$ID.colname])); patids <- patids[!is.na(patids)];
  if( !is.null(patients.to.plot) ) patids <- intersect(patids, as.character(patients.to.plot));
  if( length(patids) == 0 )
  {
    if( !suppress.warnings ) warning("No patients to plot!\n");
    return (invisible(NULL));
  } else if( length(patids) > max.patients.to.plot )
  {
    if( !suppress.warnings ) warning(paste0("Too many patients to plot (",length(patids),
                                            ")! If this is the desired outcome, please change the 'max.patients.to.plot' parameter value (now set at ",
                                            max.patients.to.plot,") to at least ',length(patids),'!\n"));
    return (invisible(NULL));
  }

  # Select only the patients to display:
  cma <- subsetCMA(cma, patids);


  ##
  ## Cache, consolidate and homogenise the needed info (events, CMAs, FUW an OW) ####
  ##

  # Cache the CMA estimates (if any):
  cmas <- getCMA(cma);
  if( inherits(cmas, "data.table") ) cmas <- as.data.frame(cmas); # same conversion to data.frame as above

  # Depeding on the cma's exact type, the relevant columns might be different or even absent: homogenize them for later use
  if( inherits(cma, "CMA_per_episode") )
  {
    names(cmas)[2:ncol(cmas)] <- c("WND.ID", "start", "gap.days", "duration", "end", "CMA"); # avoid possible conflict with patients being called "ID"
  } else if( inherits(cma, "CMA_sliding_window") )
  {
    cmas <- cbind(cmas[,1:3], "gap.days"=NA, "duration"=cma$sliding.window.duration, cmas[,4:ncol(cmas)]);
    names(cmas)[2:ncol(cmas)] <- c("WND.ID", "start", "gap.days", "duration", "end", "CMA"); # avoid possible conflict with patients being called "ID"
  } else if( inherits(cma, "CMA0") && is.null(cma$event.info) )
  {
    # Try to compute the event.info:
    event.info <- compute.event.int.gaps(data=cma$data,
                                         ID.colname=cma$ID.colname,
                                         event.date.colname=cma$event.date.colname,
                                         event.duration.colname=cma$event.duration.colname,
                                         event.daily.dose.colname=cma$event.daily.dose.colname,
                                         medication.class.colname=cma$medication.class.colname,
                                         event.interval.colname="event.interval",
                                         gap.days.colname="gap.days",
                                         carryover.within.obs.window=FALSE,
                                         carryover.into.obs.window=FALSE,
                                         carry.only.for.same.medication=FALSE,
                                         consider.dosage.change=FALSE,
                                         followup.window.start=cma$followup.window.start,
                                         followup.window.start.unit=cma$followup.window.start.unit,
                                         followup.window.duration=cma$followup.window.duration,
                                         followup.window.duration.unit=cma$followup.window.duration.unit,
                                         observation.window.start=cma$observation.window.start,
                                         observation.window.start.unit=cma$observation.window.start.unit,
                                         observation.window.duration=cma$observation.window.duration,
                                         observation.window.duration.unit=cma$observation.window.duration.unit,
                                         date.format=cma$date.format,
                                         keep.window.start.end.dates=TRUE,
                                         remove.events.outside.followup.window=FALSE,
                                         keep.event.interval.for.all.events=TRUE,
                                         parallel.backend="none", # make sure this runs sequentially!
                                         parallel.threads=1,
                                         suppress.warnings=FALSE,
                                         return.data.table=FALSE);
    if( !is.null(event.info) )
    {
      # Keep only those events that intersect with the observation window (and keep only the part that is within the intersection):

      # Compute end prescription date as well:
      event.info$.DATE.as.Date.end <- .add.time.interval.to.date(event.info$.DATE.as.Date, event.info[,cma$event.duration.colname], "days");

      # Remove all treatments that end before FUW starts and those that start after FUW ends:
      event.info <- event.info[ !(event.info$.DATE.as.Date.end < event.info$.FU.START.DATE | event.info$.DATE.as.Date > event.info$.FU.END.DATE), ];
      if( is.null(event.info) || nrow(event.info) == 0 )
      {
        if( !suppress.warnings ) warning("No events in the follow-up window: nothing to plot!\n");
        return (invisible(NULL));
      }

      # Find all prescriptions that start before the follow-up window and truncate them:
      s <- (event.info$.DATE.as.Date < event.info$.FU.START.DATE);
      if( length(s) > 0 )
      {
        event.info$.DATE.as.Date[s] <- event.info$.FU.START.DATE[s];
      }

      # Find all prescriptions that end after the follow-up window and truncate them:
      s <- (event.info$.DATE.as.Date.end > event.info$.FU.END.DATE);
      if( length(s) > 0 )
      {
        event.info[s,cma$event.duration.colname] <- .difftime.Dates.as.days(event.info$.FU.END.DATE[s], event.info$.DATE.as.Date[s]);
      }

      # Store the event.info data:
      cma$event.info <- event.info;
    } else
    {
      if( !suppress.warnings ) warning("Error(s) concerning the follow-up and observation windows: please see console for details!\n");
      return (invisible(NULL));
    }
  }

  # Add the follow-up and observation window info as well, to have everything in one place:
  if( !is.null(cmas) )
  {
    cmas <- cbind(cmas, do.call(rbind, lapply(1:nrow(cmas), function(i)
    {
      s <- which(cma$event.info[,cma$ID.colname] == cmas[i,cma$ID.colname]);
      if( length(s) == 0 ) return (NULL);
      cma$event.info[s[1],c(".FU.START.DATE", ".FU.END.DATE", ".OBS.START.DATE", ".OBS.END.DATE")];
    })));
  } else
  {
    # Create a fake one, contining but the follow-up and observation window info:
    cmas <- data.frame("..patid.."=unique(cma$data[,cma$ID.colname]), "CMA"=NA); names(cmas)[1] <- cma$ID.colname;
    if( !is.null(cma$event.info) )
    {
      cmas <- merge(cmas, unique(cma$event.info[,c(cma$ID.colname, ".FU.START.DATE", ".FU.END.DATE", ".OBS.START.DATE", ".OBS.END.DATE")]), by=c(cma$ID.colname), all.x=TRUE);
    } else
    {
      cmas <- cbind(cmas, ".FU.START.DATE"=NA, ".FU.END.DATE"=NA, ".OBS.START.DATE"=NA, ".OBS.END.DATE"=NA);
    }
  }

  # Make sure the dates are cached as `Date` objects:
  if( !inherits(cma$data[,cma$event.date.colname], "Date") )
  {
    if( is.na(cma$date.format) || is.null(cma$date.format) || length(cma$date.format) != 1 || !is.character(cma$date.format) )
    {
      if( !suppress.warnings ) warning(paste0("The date format must be a single string: cannot continue plotting!\n"));
      return (invisible(NULL));
    }

    # Convert them to Date:
    cma$data$.DATE.as.Date <- as.Date(cma$data[,cma$event.date.colname], format=cma$date.format);
    if( anyNA(cma$data$.DATE.as.Date) )
    {
      if( !suppress.warnings ) warning(paste0("Not all entries in the event date \"",cma$event.date.colname,"\" column are valid dates or conform to the date format \"",cma$date.format,"\"; first issue occurs on row ",min(which(is.na(cma$data$.DATE.as.Date))),": cannot continue plotting!\n"));
      return (invisible(NULL));
    }
  } else
  {
    # Just make a copy:
    cma$data$.DATE.as.Date <- cma$data[,cma$event.date.colname];
  }

  # Make sure the patients are ordered by ID and date:
  cma$data <- cma$data[ order( cma$data[,cma$ID.colname], cma$data$.DATE.as.Date), ];
  if( all(c("WND.ID","start") %in% names(cmas)) )
  {
    cmas <- cmas[ order( cmas[,cma$ID.colname], cmas$WND.ID, cmas$start), ];
  } else
  {
    cmas <- cmas[ order( cmas[,cma$ID.colname]), ];
  }


  ##
  ## Colors for plotting ####
  ##

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
      col.na <- "lightgray";
      col.continuation <- "black";
      print.dose.outline.col <- "white";
      plot.partial.CMAs.as.stacked.col.bars <- "gray90";
      plot.partial.CMAs.as.stacked.col.border <- "gray30";
      plot.partial.CMAs.as.stacked.col.text <- "black";
      plot.partial.CMAs.as.timeseries.col.dot <- "black";
      plot.partial.CMAs.as.timeseries.col.interval <- "gray70";
      plot.partial.CMAs.as.timeseries.col.text <- "black";
      plot.partial.CMAs.as.overlapping.col.interval <- "gray70";
      plot.partial.CMAs.as.overlapping.col.text <- "black";
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


  ##
  ## Doses ####
  ##

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


  ##
  ## Event dates and durations ####
  ##

  # Find the earliest date:
  earliest.date <- min(cma$data$.DATE.as.Date, if( "start" %in% names(cmas) ) cmas$start, cmas$.OBS.START.DATE, cmas$.FU.START.DATE, na.rm=TRUE);

  # If aligning all participants to the same date, simply relocate all dates relative to the earliest date:
  if( align.all.patients )
  {
    # ASSUMPTIONS: the data is sorted by patient ID and (ascending) by event date
    for( i in 1:nrow(cma$data) )
    {
      # For each event in the dataset:
      if( i == 1 || cma$data[i,cma$ID.colname] != cma$data[i-1,cma$ID.colname] )
      {
        # It's a new patient (or the first one):

        # We will align to the patient's first event:
        align.to <- cma$data$.DATE.as.Date[i];

        # Adjust the dates in the cmas as well:
        for( j in which(cmas[,cma$ID.colname] == cma$data[i,cma$ID.colname]) )
        {
          if( "start" %in% names(cmas) ) cmas$start[j] <- earliest.date + (cmas$start[j] - align.to);
          if( "end" %in% names(cmas) )   cmas$end[j]   <- earliest.date + (cmas$end[j]   - align.to);
          cmas$.FU.START.DATE[j]  <- earliest.date + (cmas$.FU.START.DATE[j]  - align.to);
          cmas$.FU.END.DATE[j]    <- earliest.date + (cmas$.FU.END.DATE[j]    - align.to);
          cmas$.OBS.START.DATE[j] <- earliest.date + (cmas$.OBS.START.DATE[j] - align.to);
          cmas$.OBS.END.DATE[j]   <- earliest.date + (cmas$.OBS.END.DATE[j]   - align.to);
        }
      }

      # Move the event so that it is properly aligned:
      cma$data$.DATE.as.Date[i] <- (earliest.date + (cma$data$.DATE.as.Date[i] - align.to));
    }

    # The corrected earliest follow-up window date:
    correct.earliest.followup.window <- min(cma$data$.DATE.as.Date - min(cmas$.FU.START.DATE,na.rm=TRUE),na.rm=TRUE);
  } else
  {
    # There is no correction to the earliest follow-up window date:
    correct.earliest.followup.window <- 0;
  }

  # Compute the duration if not given:
  if( is.na(duration) )
  {
    latest.date <- max(cmas$.FU.END.DATE, cmas$.OBS.END.DATE, cma$data$.DATE.as.Date + cma$data[,cma$event.duration.colname], na.rm=TRUE);
    if( "end" %in% names(cmas) ) latest.date <- max(cmas$end, latest.date, na.rm=TRUE);
    duration <- as.numeric(latest.date - earliest.date) + correct.earliest.followup.window;
  }
  endperiod <- duration;


  ##
  ## Reserve plotting space for various components ####
  ##

  # Reserve space for the CMA plotting:
  adh.plot.space <- c(0, ifelse( plot.CMA && has.estimated.CMA, duration*CMA.plot.ratio, 0) );
  duration.total <- duration + adh.plot.space[2];

  # Make sure there's enough space to actually plot the patient IDs on the y-axis:
  id.labels <- do.call(rbind,lapply(as.character(patids), # for each patient ID, compute the string dimensions in inches
                                    function(p)
                                    {
                                      # The participant axis text:
                                      s <- which(cma$event.info[,cma$ID.colname] == p);
                                      pid <- ifelse( print.CMA &&
                                                       !is.cma.TS.or.SW &&
                                                       has.estimated.CMA &&
                                                       length(x <- which(getCMA(cma)[cma$ID.colname] == p))==1,
                                                     paste0(p,"\n",sprintf("%.1f%%",getCMA(cma)[x,"CMA"]*100)),
                                                     p);
                                      data.frame("ID"=p,
                                                 "string"=pid,
                                                 "width"=strwidth(pid, units="inches", cex=cex.axis),
                                                 "height"=strheight(pid, units="inches", cex=cex.axis));
                                    }));

  y.label <- data.frame("string"=(tmp <- ifelse(is.null(ylab),"",
                                                ifelse(length(ylab)==1,
                                                       ylab,
                                                       ifelse((print.CMA || plot.CMA) &&
                                                                has.estimated.CMA,
                                                              ylab["withCMA"],
                                                              ylab["withoutCMA"])))), # space needed for the label (in inches)
                        "width"=strwidth(tmp, units="inches", cex=cex.lab),
                        "height"=strheight(tmp, units="inches", cex=cex.lab));

  left.margin <- (cur.mai <- par("mai"))[2]; # left margin in inches (and cache the current margins too)
  if( .do.R ) # Rplot:
  {
    # Save the graphical params and restore them later:
    old.par <- par(no.readonly=TRUE);

    # Rotate the ID labels:
    new.left.margin <- (y.label$height + (cos(-rotate.text*pi/180) * max(id.labels$width,na.rm=TRUE)) + strwidth("0000", units="inches", cex=cex.axis)); # ask for enough space
    par(mai=c(cur.mai[1], new.left.margin, cur.mai[3], cur.mai[4]));
  }

  # Vertical space needed for showing the partial CMAs:
  vert.space.cmas <- 0;
  if( is.cma.TS.or.SW )
  {
    # There actually is a partial CMA to be potentially plotted:
    if( ("timeseries" %in% plot.partial.CMAs.as) && (plot.partial.CMAs.as.timeseries.vspace < 5) )
    {
      warning(paste0("The minimum vertical space for the timeseries plots (plot.partial.CMAs.as.timeseries.vspace) is 5 lines, but it currently is only ",
                     plot.partial.CMAs.as.timeseries.vspace,
                     ": skipping timeseries plots...\n"));
      plot.partial.CMAs.as <- plot.partial.CMAs.as[ plot.partial.CMAs.as != "timeseries" ];
    }

    vert.space.cmas <- vert.space.cmas +
      ifelse(plot.CMA && has.estimated.CMA,
             (nrow(cmas)+length(patids)) * as.numeric("stacked" %in% plot.partial.CMAs.as) +
               3 * length(patids) * as.numeric("overlapping" %in% plot.partial.CMAs.as) +
               plot.partial.CMAs.as.timeseries.vspace * length(patids) * as.numeric("timeseries" %in% plot.partial.CMAs.as),
             0);
  }

  # Vertical space needed for the x axis:
  x.label <- ifelse(is.null(xlab), # x axis label
                    "",
                    ifelse(length(xlab)==1,
                           xlab,
                           xlab[show.period]));
  date.labels <- NULL;
  if( period.in.days > 0 )
  {
    if( show.period=="dates" )
    {
      xpos <- seq(0, as.numeric(endperiod), by=period.in.days); # where to put lables and guidelines
      axis.labels <- as.character(earliest.date + round(xpos, 1), format=cma$date.format);
    } else
    {
      if( align.first.event.at.zero )
      {
        xpos <- c(correct.earliest.followup.window - seq(0, as.numeric(correct.earliest.followup.window), by=period.in.days),
                  seq(0, as.numeric(endperiod), by=period.in.days) + correct.earliest.followup.window);
        xpos <- xpos[ xpos >= 0 & xpos <= endperiod ];
        axis.labels <- as.character(round(xpos - correct.earliest.followup.window, 1));
      } else
      {
        xpos <- seq(0, as.numeric(endperiod), by=period.in.days);
        axis.labels <- as.character(round(xpos, 1));
      }
    }

    date.labels <- data.frame("position"=adh.plot.space[2] + xpos, "string"=axis.labels);
  }

  ##
  ## SVG definitions and setup ####
  ##

  if( .do.SVG ) # SVG:
  {
    # Compute the needed size:
    # the idea is to assume 1 standard character (chr) == 16 user units, and 1 month (x axis) == 1 event (y axis) == 1 chr
    # for the title,  axis ticks and labels: 1 title == 1.5 chr, 1 axis tick == 0.75 chr, 1 axis label = 1.0 chr
    # plus spacing of about 0.5 chr around elements
    dims.chr.std          <- 10; # the "standard" character size (SVG defaults to 16)
    dims.chr.event        <- dims.chr.std / 2;
    dims.chr.title        <- (cex.title * dims.chr.std);
    dims.chr.axis         <- (cex.axis * dims.chr.std);
    dims.chr.lab          <- (cex.lab * dims.chr.std);
    dims.chr.cma          <- (CMA.cex * dims.chr.std);
    dims.chr.legend       <- (legend.cex * dims.chr.std);
    dims.chr.legend.title <- (legend.cex.title * dims.chr.std);
    dims.event.x          <- dims.chr.std*2; # the horizontal size of an event
    dims.event.y          <- (cex * dims.chr.std); # the vertical size of an event
    dims.day              <- ifelse(duration.total <= 90, 1, ifelse(duration.total <= 365, 7, ifelse(duration.total <= 3*365, 30, ifelse(duration.total <= 10*365, 90, 180)))); # how many days correpond to one horizontal user unit (depends on how many days there are in total)
    dims.axis.x           <- dims.chr.std + dims.chr.lab +
      (cos(-rotate.text*pi/180) * max(vapply(as.character(date.labels$string), function(s) .SVG.string.dims(s, font_size=dims.chr.axis)["width"], numeric(1)),na.rm=TRUE));
    dims.axis.y           <- dims.chr.std + dims.chr.lab +
      (sin(-rotate.text*pi/180) * max(vapply(as.character(id.labels$string), function(s) .SVG.string.dims(s, font_size=dims.chr.axis)["width"], numeric(1)),na.rm=TRUE));
    dims.plot.x           <- (dims.axis.y + dims.chr.std);
    dims.plot.y           <- (dims.chr.title + dims.chr.std);
    dims.plot.width       <- (dims.event.x * (duration.total + 10)/dims.day);
    dims.plot.height      <- (dims.event.y * (nrow(cma$data)+vert.space.cmas+1));
    dims.total.width      <- (dims.plot.x + dims.plot.width);
    dims.total.height     <- (dims.plot.y + dims.plot.height + dims.axis.x);

    # Scaling functions for plotting within the SVG:
    .scale.width.to.SVG.plot <- function(w)
    {
      return (dims.event.x * w / dims.day);
    }

    .scale.x.to.SVG.plot <- function(x)
    {
      return (dims.plot.x + .scale.width.to.SVG.plot(x));
    }

    .scale.height.to.SVG.plot <- function(h)
    {
      return (h * dims.event.y);
    }

    .scale.y.to.SVG.plot <- function(y)
    {
      return (dims.plot.y + dims.plot.height - .scale.height.to.SVG.plot(y));
    }

    # Stroke dash-arrays for line types (lty):
    svg.stroke.dasharrays <- data.frame("lty"=0:6,
                                        "names"=c("blank",
                                                  "solid",
                                                  "dashed",
                                                  "dotted",
                                                  "dotdash",
                                                  "longdash",
                                                  "twodash"),
                                        "svg"=c(' fill="none" stroke="none" ',
                                                ' fill="none" ',
                                                ' fill="none" stroke-dasharray="3,3" ',
                                                ' fill="none" stroke-dasharray="1,2" ',
                                                ' fill="none" stroke-dasharray="1,2,3,2" ',
                                                ' fill="none" stroke-dasharray="5,2" ',
                                                ' fill="none" stroke-dasharray="2,2,4,2" '),
                                        stringsAsFactors=FALSE);


    # SVG header:
    svg.str <- c(svg.str,
                 '<svg viewBox="0 0 ',dims.total.width,' ',dims.total.height,'" ',
                 #ifelse(generate.inline.SVG,'width="600" height="600"',''),
                 ' version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">\n'); # the plotting surface

    # Reusable bits:
    dce1 <- .SVG.number(dims.chr.event); dce2 <- .SVG.number(dims.chr.event/2); ndce2 <- .SVG.number(-dims.chr.event/2); dce3 <- .SVG.number(dims.chr.event/3); dce4 <- .SVG.number(dims.chr.event/4); # cache the various relative sizes used to draw the pch symbols
    svg.str <- c(svg.str,
                 # Predefined things to be used in the drawing:
                 '<defs>\n',

                 # The point symbols (pch) used for events etc:
                 # pch 0:
                 '<g id="pch0" fill="none" stroke-width="1"> <rect x="',ndce2,'" y="',ndce2,'" width="',dce1,'" height="',dce1,'"/> </g>\n',
                 # pch 1:
                 '<g id="pch1" fill="none" stroke-width="1"> <circle cx="0" cy="0" r="',dce2,'"/> </g>\n',
                 # pch 2:
                 '<g id="pch2" fill="none" stroke-width="1"> <polyline points="',ndce2,',',dce2,' 0,',ndce2,' ',dce2,',',dce2,' ',ndce2,',',dce2,'"/> </g>\n',
                 # pch 3:
                 '<g id="pch3" fill="none" stroke-width="1"> <line x1="',ndce2,'" y1="0" x2="',dce2,'" y2="0"/> <line x1="0" y1="',ndce2,'" x2="0" y2="',dce2,'"/> </g>\n',
                 # pch 4:
                 '<g id="pch4" fill="none" stroke-width="1"> <line x1="',ndce2,'" y1="',dce2,'" x2="',dce2,'" y2="',ndce2,'"/> <line x1="',ndce2,'" y1="',ndce2,'" x2="',dce2,'" y2="',dce2,'"/> </g>\n',
                 # pch 5:
                 '<g id="pch5" fill="none" stroke-width="1"> <polyline points="',ndce2,',0 0,',ndce2,' ',dce2,',0 0,',dce2,' ',ndce2,',0"/> </g>\n',
                 # pch 6:
                 '<g id="pch6" fill="none" stroke-width="1"> <polyline points="',ndce2,',',ndce2,' 0,',dce2,' ',dce2,',',ndce2,' ',ndce2,',',ndce2,'"/> </g>\n',
                 # pch 7:
                 '<g id="pch7" fill="none" stroke-width="1"> <use xlink:href="#pch0"/> <use xlink:href="#pch4"/> </g>\n',
                 # pch 8:
                 '<g id="pch8" fill="none" stroke-width="1"> <use xlink:href="#pch3"/> <use xlink:href="#pch4"/> </g>\n',
                 # pch 9:
                 '<g id="pch9" fill="none" stroke-width="1"> <use xlink:href="#pch3"/> <use xlink:href="#pch5"/> </g>\n',
                 # pch 10:
                 '<g id="pch10" fill="none" stroke-width="1"> <use xlink:href="#pch3"/> <use xlink:href="#pch1"/> </g>\n',
                 # pch 11:
                 '<g id="pch11" fill="none" stroke-width="1"> <use xlink:href="#pch2"/> <use xlink:href="#pch6"/> </g>\n',
                 # pch 12:
                 '<g id="pch12" fill="none" stroke-width="1"> <use xlink:href="#pch0"/> <use xlink:href="#pch3"/> </g>\n',
                 # pch 13:
                 '<g id="pch13" fill="none" stroke-width="1"> <use xlink:href="#pch1"/> <use xlink:href="#pch4"/> </g>\n',
                 # pch 14:
                 '<g id="pch14" fill="none" stroke-width="1"> <use xlink:href="#pch0"/> <use xlink:href="#pch2"/> </g>\n',
                 # pch 15:
                 '<g id="pch15" stroke-width="1"> <rect x="',ndce2,'" y="',ndce2,'" width="',dce1,'" height="',dce1,'"/> </g>\n',
                 # pch 16:
                 '<g id="pch16" stroke-width="1"> <circle cx="0" cy="0" r="',dce3,'"/> </g>\n',
                 # pch 17:
                 '<g id="pch17" stroke-width="1"> <polyline points="',ndce2,',',dce2,' 0,',ndce2,' ',dce2,',',dce2,' ',ndce2,',',dce2,'"/> </g>\n',
                 # pch 18:
                 '<g id="pch18" stroke-width="1"> <polyline points="',ndce2,',0 0,',ndce2,' ',dce2,',0 0,',dce2,' ',ndce2,',0"/> </g>\n',
                 # pch 19:
                 '<g id="pch19" stroke-width="1"> <circle cx="0" cy="0" r="',dce2,'"/> </g>\n',
                 # pch 20:
                 '<g id="pch20" stroke-width="1"> <circle cx="0" cy="0" r="',dce4,'"/> </g>\n',
                 # pch 26 ( < ):
                 '<g id="pch26" fill="none" stroke-width="1"> <polyline points="0,',dce2,' ',ndce2,',0 0,',ndce2,' "/> </g>\n',
                 # pch 27 ( > ):
                 '<g id="pch27" fill="none" stroke-width="1"> <polyline points="0,',dce2,' ',dce2,',0 0,',ndce2,' "/> </g>\n',
                 # pch 28 ( | ):
                 '<g id="pch28" fill="none" stroke-width="1"> <line x1="0" y1="',dce2,'" x2="0" y2="',ndce2,'"/> </g>\n',

                 '</defs>\n',
                 '\n');
  }


  ##
  ## The actual plotting ####
  ##

  if( .do.R ) # Rplot:
  {
    # The plotting area:
    if(inherits(msg <- try(plot( 0, 1,
                                 xlim=c(0-5,duration.total+5), # pad left and right by 5 days to improve plotting
                                 xaxs="i",
                                 ylim=c(0,nrow(cma$data)+vert.space.cmas+1),
                                 yaxs="i",
                                 type="n",
                                 axes=FALSE,
                                 xlab="",
                                 ylab="" ),
                           silent=TRUE),
                "try-error"))
    {
      # Some error occured when creatig the plot...
      warning(msg);
      par(old.par); # restore graphical params
      return (invisible(NULL));
    }

    # Character width and height in the current plotting system:
    if( print.dose ) dose.text.height <- strheight("0",cex=cex.dose);
    char.width <- strwidth("O",cex=cex); char.height <- strheight("O",cex=cex);
    char.height.CMA <- strheight("0",cex=CMA.cex);

    # Minimum plot dimensions:
    if( abs(par("usr")[2] - par("usr")[1]) <= char.width * min.plot.size.in.characters.horiz ||
        abs(par("usr")[4] - par("usr")[3]) <= char.height * min.plot.size.in.characters.vert * (nrow(cma$data) + ifelse(is.cma.TS.or.SW && plot.CMA && has.estimated.CMA, nrow(cmas), 0)) )
    {
      warning(paste0("Plotting area is too small (it must be at least ",
                     min.plot.size.in.characters.horiz,
                     " x ",
                     min.plot.size.in.characters.vert,
                     " characters per patient, but now it is only ",
                     round(abs(par("usr")[2] - par("usr")[1]) / char.width,1),
                     " x ",
                     round(abs(par("usr")[4] - par("usr")[3]) / (char.height * (nrow(cma$data) + ifelse(is.cma.TS.or.SW && plot.CMA && has.estimated.CMA, nrow(cmas), 0))),1),
                     ")!\n"));
      par(old.par); # restore graphical params
      return (invisible(NULL));
    }
  }

  if( .do.SVG ) # SVG:
  {
    svg.str <- c(svg.str,
                 # Clear the area:
                 .SVG.rect(comment="Clear the whole plotting area",
                           class="plotting-area-background",
                           x=0, y=0, width=dims.total.width, height=dims.total.height,
                           fill="white", stroke="none"),
                 '\n' # one empty line
    );
  }

  # Function mapping the CMA values to the appropriate x-coordinates:
  if( plot.CMA && has.estimated.CMA )
  {
    adh.max <- ifelse(is.cma.TS.or.SW, 1.0, max(c(getCMA(cma)$CMA, 1.0),na.rm=TRUE)); # maximum achieved CMA (used for plotting, forced to 1.0 for PE and SW)
    .rescale.xcoord.for.CMA.plot <- function(x, pfree=0.20)
    {
      return (adh.plot.space[1] + (x / adh.max) * (adh.plot.space[2] * (1-pfree) - adh.plot.space[1]));
    }
  }


  ##
  ## Plot most of the plot components ####
  ##

  # Intialisations
  y.cur <- 1; # the current vertical line at which plotting takes place
  alternating.band.to.draw <- 1; # for this patient, which alternating band to draw?

  # For each individual event in turn:
  for( i in 1:nrow(cma$data) )
  {
    # The current patient ID:
    cur_pat_id <- cma$data[i,cma$ID.colname];

    # For a new patients, draw the alternating bands, show the CMA and print the y-axis label:
    if( i == 1 || (cur_pat_id != cma$data[i-1,cma$ID.colname]) )
    {
      # Save the current vertical position (for drawing the FUW and OW windows):
      y.old <- y.cur;

      # Select the events and partial CMAs belonging to this patient:
      s.events <- which(cma$data[,cma$ID.colname] == cur_pat_id);
      s.cmas   <- which(cmas[,cma$ID.colname]     == cur_pat_id);

      # Total vartical space neede by this patient:
      vspace.needed <- length(s.events) +
        ifelse(plot.CMA && has.estimated.CMA && adh.plot.space[2] > 0,
               (length(s.cmas)+1) * as.numeric("stacked" %in% plot.partial.CMAs.as) +
                 3 * as.numeric("overlapping" %in% plot.partial.CMAs.as) +
                 plot.partial.CMAs.as.timeseries.vspace * as.numeric("timeseries" %in% plot.partial.CMAs.as),
               0);


      ##
      ## The alternating bands ####
      ##

      # Draw the alternating bands
      if( !is.null(alternating.bands.cols) )
      {
        if( .do.R ) # Rplot:
        {
          rect( 0.0 - 1.0, y.cur - 0.5, duration.total + 1.0, y.cur + vspace.needed - 0.5, col=alternating.bands.cols[alternating.band.to.draw], border=NA );
        }

        if( .do.SVG ) # SVG:
        {
          svg.str <- c(svg.str,
                       .SVG.rect(x=.scale.x.to.SVG.plot(0), y=.scale.y.to.SVG.plot(y.cur - 0.5 + vspace.needed),
                                 width=dims.plot.width, height=.scale.height.to.SVG.plot(vspace.needed),
                                 fill=alternating.bands.cols[alternating.band.to.draw],
                                 class=paste0("alternating-bands-",alternating.band.to.draw), comment="The alternating band")
          );
        }

        alternating.band.to.draw <- if(alternating.band.to.draw >= length(alternating.bands.cols)) 1 else (alternating.band.to.draw + 1); # move to the next band
      }


      ##
      ## FUW and OW ####
      ##

      # The follow-up and observation windows (these are drawn only after all the other stuff for this patient has been drawn):
      if( highlight.followup.window )
      {
        if( .do.R ) # Rplot:
        {
          rect(adh.plot.space[2] + as.numeric(cmas$.FU.START.DATE[s.cmas[1]] - earliest.date) + correct.earliest.followup.window, y.cur - 0.5,
               adh.plot.space[2] + as.numeric(cmas$.FU.END.DATE[s.cmas[1]]   - earliest.date) + correct.earliest.followup.window, y.cur + length(s.events) - 0.5,
               col=NA, border=followup.window.col, lty="dashed", lwd=2);
        }

        if( .do.SVG ) # SVG:
        {
          svg.str <- c(svg.str,
                       # FUW:
                       .SVG.rect(x=.scale.x.to.SVG.plot(adh.plot.space[2] + as.numeric(cmas$.FU.START.DATE[s.cmas[1]] - earliest.date) + correct.earliest.followup.window),
                                 y=.scale.y.to.SVG.plot(y.cur + length(s.events) - 0.5),
                                 width=.scale.width.to.SVG.plot(as.numeric(cmas$.FU.END.DATE[s.cmas[1]] - cmas$.FU.START.DATE[s.cmas[1]])),
                                 height=.scale.height.to.SVG.plot(length(s.events)),
                                 stroke=followup.window.col, stroke_width=2, lty="dashed", fill="none",
                                 class="fuw", comment="The Follow-Up Window (FUW)")
          );
        }
      }
      if( highlight.observation.window )
      {
        # The "given" OW:
        if( .do.R ) # Rplot:
        {
          rect(adh.plot.space[2] + as.numeric(cmas$.OBS.START.DATE[s.cmas[1]] - earliest.date) + correct.earliest.followup.window, y.cur - 0.5,
               adh.plot.space[2] + as.numeric(cmas$.OBS.END.DATE[s.cmas[1]]   - earliest.date) + correct.earliest.followup.window, y.cur + length(s.events) - 0.5,
               col=adjustcolor(observation.window.col,alpha.f=observation.window.opacity), border=NA); #, density=observation.window.density, angle=observation.window.angle);
        }

        if( .do.SVG ) # SVG:
        {
          svg.str <- c(svg.str,
                       # OW:
                       .SVG.rect(x=.scale.x.to.SVG.plot(adh.plot.space[2] + as.numeric(cmas$.OBS.START.DATE[s.cmas[1]] - earliest.date) + correct.earliest.followup.window),
                                 y=.scale.y.to.SVG.plot(y.cur + length(s.events) - 0.5),
                                 width=.scale.width.to.SVG.plot(as.numeric(cmas$.OBS.END.DATE[s.cmas[1]] - cmas$.OBS.START.DATE[s.cmas[1]])),
                                 height=.scale.height.to.SVG.plot(length(s.events)),
                                 stroke="none", fill=observation.window.col, fill_opacity=observation.window.opacity,
                                 class="ow", comment="The Observation Window (OW)")
          );
        }

        if( inherits(cma,"CMA8") && !is.null(cma$real.obs.window) && show.real.obs.window.start )
        {
          # For CMA8, the OW might have been changed, so we also have a "real" OW:
          s.realOW <- which(cma$real.obs.window[,cma$ID.colname] == cur_pat_id);

          # Find the begining of the "real" OW:
          if( length(s.realOW) == 1)
          {
            if( !is.null(cma$real.obs.windows$window.start) && !is.na(cma$real.obs.windows$window.start[s.realOW]) )
            {
              real.obs.window.start <- cma$real.obs.windows$window.start[s.realOW];
            } else
            {
              real.obs.window.start <- cma$event.info$.OBS.START.DATE[s.events[1]];
            }
            if( !is.null(cma$real.obs.windows$window.end) && !is.na(cma$real.obs.windows$window.end[s.realOW]) )
            {
              real.obs.window.end <- cma$real.obs.windows$window.end[s.realOW];
            } else
            {
              real.obs.window.end <- cma$event.info$.OBS.END.DATE[s.events[1]];
            }

            # Draw the "real" OW:
            if( .do.R ) # Rplot:
            {
              rect(adh.plot.space[2] + as.numeric(real.obs.window.start - earliest.date) + correct.earliest.followup.window, y.cur - 0.5,
                   adh.plot.space[2] + as.numeric(real.obs.window.end   - earliest.date) + correct.earliest.followup.window, y.cur + length(s.events) - 0.5,
                   col=adjustcolor(observation.window.col,alpha.f=observation.window.opacity), border=NA); #, density=real.obs.window.density, angle=real.obs.window.angle);
            }

            if( .do.SVG ) # SVG:
            {
              svg.str <- c(svg.str,
                           # "real" OW:
                           .SVG.rect(x=.scale.x.to.SVG.plot(adh.plot.space[2] + as.numeric(real.obs.window.start - earliest.date) + correct.earliest.followup.window),
                                     y=.scale.y.to.SVG.plot(y.cur + length(s.events) - 0.5),
                                     width=.scale.width.to.SVG.plot(as.numeric(real.obs.window.end - real.obs.window.start)),
                                     height=.scale.height.to.SVG.plot(length(s.events)),
                                     stroke="none", fill=observation.window.col, fill_opacity=observation.window.opacity,
                                     class="ow-real", comment="The 'real' Observation Window")
              );
            }
          }
        }
      }

      ##
      ## The y-axis labels ####
      ##

      # The y-axis label:
      pid <- cur_pat_id;
      y.mean <- y.cur + vspace.needed/2; # vertical position of the label (centered on patient)
      if( .do.R ) # Rplot:
      {
        text(par("usr")[1], y.mean, pid, cex=cex.axis, srt=-rotate.text, pos=2, xpd=TRUE);
      }
      if( .do.SVG ) # SVG:
      {
        svg.str <- c(svg.str,
                     .SVG.text(x=(dims.plot.x - dims.chr.axis), y=.scale.y.to.SVG.plot(y.cur + vspace.needed/2), text=pid,
                               font_size=dims.chr.axis, h.align="right", v.align="center", rotate=-(90+rotate.text),
                               class="axis-values-y", comment="The y-axis labels")
        );
      }


      ##
      ## The summary CMA plots ####
      ##

      # The patient's CMA plot:
      if( plot.CMA && has.estimated.CMA && adh.plot.space[2] > 0 )
      {
        if( is.cma.TS.or.SW )
        {
          # For per episode and sliding windows we show the distribution of the "partial" CMAs:
          if( .do.R ) # Rplot:
          {
            # The CMA plot background:
            segments(.rescale.xcoord.for.CMA.plot(0.0), y.mean - 2, .rescale.xcoord.for.CMA.plot(1.0), y.mean - 2, lty="solid", col=CMA.plot.col);
            segments(.rescale.xcoord.for.CMA.plot(0.0), y.mean + 2, .rescale.xcoord.for.CMA.plot(1.0), y.mean + 2, lty="solid", col=CMA.plot.col);
          }

          if( .do.SVG ) # SVG:
          {
            svg.str <- c(svg.str,
                         # The CMA plot background:
                         .SVG.lines(x=c(.scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(0.0)),
                                        .scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(1.0)),
                                        .scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(0.0)),
                                        .scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(1.0))),
                                    y=c(.scale.y.to.SVG.plot(y.mean - 2),
                                        .scale.y.to.SVG.plot(y.mean - 2),
                                        .scale.y.to.SVG.plot(y.mean + 2),
                                        .scale.y.to.SVG.plot(y.mean + 2)),
                                    connected=FALSE,
                                    stroke=CMA.plot.col, stroke_width=1,
                                    class="cma-drawing-area-background", comment="The CMA plot background")
            );
          }

          # The non-missing CMA values:
          adh <- na.omit(cmas[s.cmas,"CMA"]);

          # Scale the CMA (itself or density) in such a way that if within 0..1 stays within 0..1 but scales if it goes outside this interval to accomodate it
          if( plot.CMA.as.histogram )
          {
            # Plot CMA as histogram (or nothing, if too little data):
            if( length(adh) > 0 ) svg.str <- .plot.summary.CMA.as.histogram(adh, svg.str);
          } else
          {
            if( length(adh) > 2 )
            {
              # Plot CMA as density plot:
              adh.density <- density(adh);
              ss <- (adh.density$x >= min(adh,na.rm=TRUE) & adh.density$x <= max(adh,na.rm=TRUE));
              if( sum(ss) == 0 )
              {
                # Probably constant numbers? Plot the individual lines:
                svg.str <- .plot.summary.CMA.as.lines(adh, svg.str);
              } else
              {
                # Plot as density:
                svg.str <- .plot.summary.CMA.as.density(adh.density$x[ss], adh.density$y[ss], svg.str);
              }
            } else
            {
              if( length(adh) == 0 )
              {
                # No points at all: nothing to plot!
              } else
              {
                # Plot the individual lines:
                svg.str <- .plot.summary.CMA.as.lines(adh, svg.str);
              }
            }
          }
        } else if( inherits(cma, "CMA1") )
        {
          # For CMA1+ we show the actual point estimate:

          # The adherence estimate:
          adh <- cmas[s.cmas,"CMA"];

          if( !is.na(adh) )
          {
            if( .do.R ) # Rplot:
            {
              # Draw the background rectangle:
              rect(.rescale.xcoord.for.CMA.plot(0.0), mean(s.events) - 1, .rescale.xcoord.for.CMA.plot(min(adh,adh.max)), mean(s.events) + 1, col=CMA.plot.col, border=NA);
              rect(.rescale.xcoord.for.CMA.plot(0.0), mean(s.events) - 1, .rescale.xcoord.for.CMA.plot(max(1.0,adh.max)), mean(s.events) + 1, col=NA, border=CMA.plot.border);
            }

            if( .do.SVG ) # SVG:
            {
              svg.str <- c(svg.str,
                           # Draw the CMA estimate background rectangle:
                           .SVG.rect(x=.scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(0.0)),
                                     y=.scale.y.to.SVG.plot(mean(s.events)+1),
                                     width=.scale.width.to.SVG.plot(.rescale.xcoord.for.CMA.plot(min(adh,adh.max)) - .rescale.xcoord.for.CMA.plot(0.0)),
                                     height=.scale.height.to.SVG.plot(2),
                                     stroke="none", fill=CMA.plot.col,
                                     class="cma-estimate-bkg", comment="The CMA estimate backgound"),
                           .SVG.rect(x=.scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(0.0)),
                                     y=.scale.y.to.SVG.plot(mean(s.events)+1),
                                     width=.scale.width.to.SVG.plot(.rescale.xcoord.for.CMA.plot(max(1.0,adh.max)) - .rescale.xcoord.for.CMA.plot(0.0)),
                                     height=.scale.height.to.SVG.plot(2),
                                     stroke=CMA.plot.border, stroke_width=1, fill="none",
                                     class="cma-estimate-bkg")
              );
            }

            cma.string <- sprintf("%.1f%%",adh*100);
            available.x.space <- abs(.rescale.xcoord.for.CMA.plot(max(1.0,adh.max)) - .rescale.xcoord.for.CMA.plot(0.0));

            if( .do.R ) # Rplot:
            {
              if( strwidth(cma.string, cex=CMA.cex) <= available.x.space )
              { # horizontal writing of the CMA:
                text(x=(.rescale.xcoord.for.CMA.plot(0.0) + .rescale.xcoord.for.CMA.plot(max(1.0,adh.max)))/2, y=mean(s.events),
                     labels=cma.string, col=CMA.plot.text, cex=CMA.cex);
              } else if( strheight(cma.string, cex=CMA.cex) <= available.x.space )
              { # vertical writing of the CMA:
                text(x=(.rescale.xcoord.for.CMA.plot(0.0) + .rescale.xcoord.for.CMA.plot(max(1.0,adh.max)))/2, y=mean(s.events),
                     labels=cma.string, col=CMA.plot.text, cex=CMA.cex, srt=90);
              } # otherwise, theres' no space for showing the CMA here
            }

            if( .do.SVG ) # SVG:
            {
              if( available.x.space * dims.event.x >= dims.chr.cma )
              {
                svg.str <- c(svg.str,
                             # Write the CMA estimate (always vertically):
                             .SVG.text(x=.scale.x.to.SVG.plot((.rescale.xcoord.for.CMA.plot(0.0) + .rescale.xcoord.for.CMA.plot(max(1.0,adh.max)))/2),
                                       y=.scale.y.to.SVG.plot(mean(s.events)),
                                       text=cma.string,
                                       col=CMA.plot.text, font_size=dims.chr.cma, h.align="center", v.align="center", rotate=-90,
                                       class="cma-estimate-text", comment="The CMA estimate (as text)")
                );
              }
            }
          }
        }
      }
    }

    ##
    ## The event ####
    ##

    # Get the event start and end dates:
    start <- as.numeric(cma$data$.DATE.as.Date[i] - earliest.date);
    end   <- start + cma$data[i,cma$event.duration.colname];

    # Map medication classes to colors:
    if( is.na(cma$medication.class.colname) || !(cma$medication.class.colname %in% names(cma$data)) )
    {
      col <- .map.category.to.color(unspecified.category.label);
    } else
    {
      col <- .map.category.to.color(cma$data[i,cma$medication.class.colname]);
    }

    if( .do.R ) # Rplot:
    {
      # Plot the beging and end of the event:
      points(adh.plot.space[2] + start + correct.earliest.followup.window, y.cur, pch=pch.start.event, col=col, cex=cex);
      points(adh.plot.space[2] + end   + correct.earliest.followup.window, y.cur, pch=pch.end.event,   col=col, cex=cex);
    }

    if( .do.SVG ) # SVG:
    {
      svg.str <- c(svg.str,
                   # The begining of the event:
                   .SVG.points(x=.scale.x.to.SVG.plot(adh.plot.space[2] + start + correct.earliest.followup.window), y=.scale.y.to.SVG.plot(y.cur),
                               pch=pch.start.event, col=col, cex=cex,
                               class="event-start"),
                   .SVG.points(x=.scale.x.to.SVG.plot(adh.plot.space[2] + end + correct.earliest.followup.window), y=.scale.y.to.SVG.plot(y.cur),
                               pch=pch.end.event, col=col, cex=cex,
                               class="event-start")
      );
    }


    # Show event intervals as rectangles?
    if( show.event.intervals && !is.null(cma$event.info) && !is.na(cma$event.info$event.interval[i]) )
    {
      # The end of the prescription:
      end.pi <- start + cma$event.info$event.interval[i] - cma$event.info$gap.days[i];

      if( .do.R ) # Rplot:
      {
        rect(adh.plot.space[2] + start  + correct.earliest.followup.window, i - char.height/2,
             adh.plot.space[2] + end.pi + correct.earliest.followup.window, i + char.height/2,
             col=adjustcolor(col,alpha.f=0.2), border=col);
        if( cma$event.info$gap.days[i] > 0 )
          rect(adh.plot.space[2] + end.pi + correct.earliest.followup.window, i - char.height/2,
               adh.plot.space[2] + end.pi + cma$event.info$gap.days[i] + correct.earliest.followup.window, i + char.height/2,
               #density=25, col=adjustcolor(col,alpha.f=0.5),
               col=NA, border=col);
      }

      if( .do.SVG ) # SVG:
      {
        svg.str <- c(svg.str,
                     .SVG.rect(x=.scale.x.to.SVG.plot(adh.plot.space[2] + start + correct.earliest.followup.window),
                               y=.scale.y.to.SVG.plot(y.cur) - dims.event.y/2,
                               xend=.scale.x.to.SVG.plot(adh.plot.space[2] + end.pi + correct.earliest.followup.window),
                               height=dims.event.y,
                               stroke=col, fill=col, fill_opacity=0.2,
                               class="event-interval-covered"),
                     if( cma$event.info$gap.days[i] > 0 )
                       .SVG.rect(x=.scale.x.to.SVG.plot(adh.plot.space[2] + end.pi + correct.earliest.followup.window),
                                 y=.scale.y.to.SVG.plot(y.cur) - dims.event.y/2,
                                 xend=.scale.x.to.SVG.plot(adh.plot.space[2] + end.pi + cma$event.info$gap.days[i] + correct.earliest.followup.window),
                                 height=dims.event.y,
                                 stroke=col, fill="none",
                                 class="event-interval-not-covered")
        );
      }
    }

    # Do we show dose?
    seg.x1 <- adh.plot.space[2] + start + correct.earliest.followup.window;
    seg.x2 <- adh.plot.space[2] + end   + correct.earliest.followup.window;
    seg.lwd <- NA;
    if( plot.dose )
    {
      # Show dose using event line width:
      if( nrow(dose.range) == 1 )
      {
        # Just one dose:
        seg.lwd <- adjust.dose.lwd(cma$data[i,cma$event.daily.dose.colname])
      } else
      {
        # There is a range of doses:
        if( plot.dose.lwd.across.medication.classes )
        {
          # Line width across all medication classes:
          seg.lwd <- adjust.dose.lwd(cma$data[i,cma$event.daily.dose.colname], dose.min=dose.range.global$min, dose.max=dose.range.global$max);
        } else
        {
          # Line width per medication class:
          dose.for.cat <- (dose.range$category == cma$data[i,cma$medication.class.colname]);
          if( sum(dose.for.cat,na.rm=TRUE) == 1 )
          {
            # Found the corresponding medication class:
            seg.lwd <- adjust.dose.lwd(cma$data[i,cma$event.daily.dose.colname], dose.min=dose.range$min[dose.for.cat], dose.max=dose.range$max[dose.for.cat]);
          } else
          {
            # Use a fixed width:
            seg.lwd <- lwd.event;
          }
        }
      }
    } else
    {
      # Use a fixed line width:
      seg.lwd <- lwd.event;
    }
    if( .do.R ) # Rplot:
    {
      segments( seg.x1, y.cur, seg.x2, y.cur, col=col, lty=lty.event, lwd=seg.lwd);
    }
    if( .do.SVG ) # SVG:
    {
      svg.str <- c(svg.str,
                   # The begining of the event:
                   .SVG.lines(x=c(.scale.x.to.SVG.plot(seg.x1), .scale.x.to.SVG.plot(seg.x2)),
                              y=rep(.scale.y.to.SVG.plot(y.cur),2),
                              connected=FALSE,
                              stroke=col, stroke_width=seg.lwd,
                              class="event-segment")
      );
    }

    if( print.dose )
    {
      # Show dose as actual numbers on the plot:
      if( .do.R ) # Rplot:
      {
        dose.text.y <- (y.cur - ifelse(print.dose.centered, 0, dose.text.height*2/3)); # print it on or below the dose segment?

        if( is.na(print.dose.outline.col) ) # simple or outlined?
        {
          # Simple text:
          text(adh.plot.space[2] + (start + end)/2 + correct.earliest.followup.window,
               dose.text.y,
               cma$data[i,cma$event.daily.dose.colname], cex=cex.dose, col=ifelse(is.na(print.dose.col),col,print.dose.col));
        } else
        {
          # Outlined text:
          .shadow.text(adh.plot.space[2] + (start + end)/2 + correct.earliest.followup.window,
                       dose.text.y,
                       cma$data[i,cma$event.daily.dose.colname], cex=cex.dose, col=ifelse(is.na(print.dose.col),col,print.dose.col), bg=print.dose.outline.col);
        }
      }

      if( .do.SVG ) # SVG:
      {
        svg.str <- c(svg.str,
                     # The dose text:
                     .SVG.text(x=.scale.x.to.SVG.plot(adh.plot.space[2] + (start + end)/2 + correct.earliest.followup.window),
                               y=.scale.y.to.SVG.plot(y.cur - ifelse(print.dose.centered, 0, 3/4)),
                               text=cma$data[i,cma$event.daily.dose.colname],
                               font_size=dims.chr.std * cex.dose, h.align="center", v.align="center",
                               col=if(is.na(print.dose.col)) col else print.dose.col,
                               other_params=if(!is.na(print.dose.outline.col)) paste0(' stroke="',.SVG.color(print.dose.outline.col,return_string=TRUE),'" stroke-width="0.5"'),
                               class="event-dose-text")
        );
      }
    }

    # Advance to the next vertical line:
    y.cur <- y.cur + 1;

    # Continuation between successive events:
    if( i < nrow(cma$data) && (cur_pat_id == cma$data[i+1,cma$ID.colname]) )
    {
      # We're still plotting the same patient: show the continuation line:
      start.next <- as.numeric(cma$data$.DATE.as.Date[i+1] - earliest.date);

      if( .do.R ) # Rplot:
      {
        segments( adh.plot.space[2] + end        + correct.earliest.followup.window, y.cur-1,
                  adh.plot.space[2] + start.next + correct.earliest.followup.window, y.cur-1,
                  col=col.continuation, lty=lty.continuation, lwd=lwd.continuation);
        segments( adh.plot.space[2] + start.next + correct.earliest.followup.window, y.cur-1,
                  adh.plot.space[2] + start.next + correct.earliest.followup.window, y.cur,
                  col=col.continuation, lty=lty.continuation, lwd=lwd.continuation);
      }

      if( .do.SVG ) # SVG:
      {
        svg.str <- c(svg.str,
                     # The continuation line:
                     .SVG.lines(x=c(.scale.x.to.SVG.plot(adh.plot.space[2] + end + correct.earliest.followup.window),
                                    .scale.x.to.SVG.plot(adh.plot.space[2] + start.next + correct.earliest.followup.window),
                                    .scale.x.to.SVG.plot(adh.plot.space[2] + start.next + correct.earliest.followup.window),
                                    .scale.x.to.SVG.plot(adh.plot.space[2] + start.next + correct.earliest.followup.window)),
                                y=c(.scale.y.to.SVG.plot(y.cur-1),
                                    .scale.y.to.SVG.plot(y.cur-1),
                                    .scale.y.to.SVG.plot(y.cur-1),
                                    .scale.y.to.SVG.plot(y.cur)),
                                connected=TRUE,
                                stroke=col.continuation, stroke_width=lwd.continuation, lty=lty.continuation,
                                class="continuation-line")
        );
      }
    } else
    { # The patient is changing or is the last one:

      ##
      ## Partial CMAs ####
      ##

      # Draw its subperiods (if so requested, meaningful and possible):
      if( is.cma.TS.or.SW && plot.CMA && has.estimated.CMA && adh.plot.space[2] > 0 )
      {
        if( length(s.cmas) > 0 && !all(is.na(cmas$CMA[s.cmas])) )
        {
          # We do have non-missing partial CMAs to plot:

          # Compute the start, end, location and string to display for these partial estimates:
          ppts <- data.frame("start"=as.numeric(cmas$start[s.cmas] - earliest.date),
                             "end"  =as.numeric(cmas$end[s.cmas]   - earliest.date),
                             "x"    =NA,
                             "y"    =cmas$CMA[s.cmas],
                             "text" =ifelse(!is.na(cmas$CMA[s.cmas]), sprintf("%.0f%%", 100*cmas$CMA[s.cmas]), "?")
                            );
          ppts$x <- (ppts$start + ppts$end)/2;

          # Cache stuff:
          corrected.x <- (adh.plot.space[2] + correct.earliest.followup.window);
          corrected.x.start <- (corrected.x+ppts$start);
          corrected.x.end   <- (corrected.x+ppts$end);
          x.start.min <- min(ppts$start,na.rm=TRUE);
          x.end.max   <- max(ppts$end,  na.rm=TRUE);
          corrected.x.text <- (corrected.x + ppts$x);
          min.y <- min(ppts$y,na.rm=TRUE);
          max.y <- max(ppts$y,na.rm=TRUE);

          # Plotting type:
          if( "stacked" %in% plot.partial.CMAs.as )
          {
            # Show subperiods as stacked:
            ys <- (y.cur + 1:nrow(ppts) - 1); # cache this
            h <- (ppts$end - ppts$start) * pmax(pmin(ppts$y, 1.0), 0.0); # cache the actual CMA estimates scaled for plotting

            if( .do.R ) # Rplot:
            {
              # The intervals as empty rectangles:
              rect(corrected.x.start, ys + 0.10, corrected.x.end,   ys + 0.90, border=gray(0.7), col="white");
              # The CMAs as filled rectangles of length proportional to the CMA:
              rect(corrected.x.start, ys + 0.10, corrected.x.start + h, ys + 0.90, border=plot.partial.CMAs.as.stacked.col.border, col=plot.partial.CMAs.as.stacked.col.bars);
              if( print.CMA && char.height.CMA <= 0.80 )
              {
                text(corrected.x.text, ys + 0.5, ppts$text, cex=CMA.cex, col=plot.partial.CMAs.as.stacked.col.text);
              }
            }

            if( .do.SVG ) # SVG:
            {
              svg.str <- c(svg.str,
                           .SVG.comment("Partial CMAs as stacked bars:", newpara=TRUE));
              for( j in 1:nrow(ppts) )
              {
                svg.str <- c(svg.str,
                             # The background rect:
                             .SVG.rect(x=.scale.x.to.SVG.plot(corrected.x.start[j]), y=.scale.y.to.SVG.plot(ys[j] + 0.90),
                                       xend=.scale.x.to.SVG.plot(corrected.x.end[j]), yend=.scale.y.to.SVG.plot(ys[j] + 0.10),
                                       stroke="gray70", fill="white",
                                       class="partial_cma_stacked_rect_bkg"),
                             # The CMA estimate rect:
                             .SVG.rect(x=.scale.x.to.SVG.plot(corrected.x.start[j]), y=.scale.y.to.SVG.plot(ys[j] + 0.90),
                                       xend=.scale.x.to.SVG.plot(corrected.x.start[j] + h[j]), yend=.scale.y.to.SVG.plot(ys[j] + 0.10),
                                       stroke=plot.partial.CMAs.as.stacked.col.border, fill=plot.partial.CMAs.as.stacked.col.bars,
                                       class="partial_cma_stacked_rect_estimate"),
                             # The numeric estimate:
                             if( print.CMA && dims.chr.cma <= dims.chr.event )
                             {
                               .SVG.text(.scale.x.to.SVG.plot(corrected.x.text[j]), y=.scale.y.to.SVG.plot(ys[j] + 0.50),
                                         text=ppts$text[j], font_size=dims.chr.cma, col=plot.partial.CMAs.as.stacked.col.text,
                                         h.align="center", v.align="center",
                                         class="partial_cma_stacked_text_estimate")
                             }
                );
              }
            }

            # Advance to next patient:
            y.cur <- (y.cur + nrow(ppts) + 1);
          }

          if( "overlapping" %in% plot.partial.CMAs.as )
          {
            # Show subperiods as overlapping segments:
            if( !((range.y <- (max.y - min.y)) > 0) ) range.y <- 1; # avoid division by 0 if there's only one value
            ppts$y.norm <- (ppts$y - min.y)/range.y;

            if( .do.SVG ) # SVG:
            {
              svg.str <- c(svg.str,
                           .SVG.comment("Partial CMAs as overlapping segments:", newpara=TRUE));
            }

            if( !is.na(plot.partial.CMAs.as.overlapping.col.interval) )
            {
              if( plot.partial.CMAs.as.overlapping.alternate )
              {
                v <- rep(c(0,1), nrow(ppts))[1:nrow(ppts)]; # alternate between low (0) and high (1) -- not the best way but works fine
              } else
              {
                v <- rep(0,nrow(ppts)); # all segments are drawn low (0)
              }
              y.norm.v <- (ppts$y.norm * -(v*2-1)); # -(v*2-1) maps 0 to 1 and 1 to -1

              if( .do.R ) # Rplot:
              {
                segments(corrected.x.start, y.cur + 0.5 + v, corrected.x.end,   y.cur + 0.5 + v, col=plot.partial.CMAs.as.overlapping.col.interval);
                segments(corrected.x.start, y.cur + 0.5 + v, corrected.x.start, y.cur + 0.5 + v + y.norm.v, col=plot.partial.CMAs.as.overlapping.col.interval);
                segments(corrected.x.end,   y.cur + 0.5 + v, corrected.x.end,   y.cur + 0.5 + v + y.norm.v, col=plot.partial.CMAs.as.overlapping.col.interval);
              }

              if( .do.SVG ) # SVG:
              {
                for( j in 1:nrow(ppts) )
                {
                  svg.str <- c(svg.str,
                               # The connected segments one by one:
                               .SVG.lines(x=.scale.x.to.SVG.plot(c(corrected.x.start[j], corrected.x.end[j])),
                                          y=.scale.y.to.SVG.plot(c(y.cur + 0.5 + v[j], y.cur + 0.5 + v[j])),
                                          connected=FALSE, stroke=plot.partial.CMAs.as.overlapping.col.interval,
                                          class="partial_cma_overlapping_segments"),
                               if(!is.na(y.norm.v[j])) .SVG.lines(x=.scale.x.to.SVG.plot(c(corrected.x.start[j], corrected.x.start[j],
                                                                                           corrected.x.end[j],   corrected.x.end[j])),
                                                                  y=.scale.y.to.SVG.plot(c(y.cur + 0.5 + v[j], y.cur + 0.5 + v[j] + y.norm.v[j],
                                                                                           y.cur + 0.5 + v[j], y.cur + 0.5 + v[j] + y.norm.v[j])),
                                                                  connected=FALSE, stroke=plot.partial.CMAs.as.overlapping.col.interval,
                                                                  class="partial_cma_overlapping_segments")
                  );
                }
              }
            }

            if( .do.R ) # Rplot:
            {
              if( print.CMA && char.height.CMA <= 0.80 && !is.na(plot.partial.CMAs.as.overlapping.col.text) )
              {
                text(corrected.x.text, y.cur + 1.0, ppts$text, cex=CMA.cex, col=plot.partial.CMAs.as.overlapping.col.text);
              }
            }

            if( .do.SVG ) # SVG:
            {
              if( print.CMA && dims.chr.cma <= dims.chr.event && !is.na(plot.partial.CMAs.as.overlapping.col.text) )
              {
                svg.str <- c(svg.str,
                             # The text estimates:
                             .SVG.text(x=.scale.x.to.SVG.plot(corrected.x.text), y=.scale.y.to.SVG.plot(rep(y.cur + 1.0,length(corrected.x.text))), text=ppts$text,
                                       col=plot.partial.CMAs.as.overlapping.col.text, font_size=dims.chr.cma,
                                       h.align="center", v.align="center",
                                       class="partial_cma_overlapping_text")
                );
              }
            }

            # Advance to next patient:
            y.cur <- y.cur+3;
          }

          if( "timeseries" %in% plot.partial.CMAs.as )
          {
            # Show subperiods as a time series
            if( plot.partial.CMAs.as.timeseries.start.from.zero ) min.y <- min(min.y,0,na.rm=TRUE);

            if( !((range.y <- (max.y - min.y)) > 0) ) range.y <- 1; # avoid division by 0 if there's only one value
            ppts$y.norm <- (y.cur + 1 + (plot.partial.CMAs.as.timeseries.vspace - 3) * (ppts$y - min.y)/range.y);

            if( .do.SVG ) # SVG:
            {
              svg.str <- c(svg.str,
                           .SVG.comment("Partial CMAs as time series:", newpara=TRUE));
            }

            # The axes:
            min.y.norm <- min(ppts$y.norm,na.rm=TRUE);
            max.y.norm <- max(ppts$y.norm,na.rm=TRUE);
            if( .do.R ) # Rplot:
            {
              segments(corrected.x + x.start.min, y.cur + 0.5, corrected.x + x.end.max,   y.cur + 0.5, lty="solid", col="black"); # horizontal axis
              segments(corrected.x + x.start.min, y.cur + 0.5, corrected.x + x.start.min, y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0, lty="solid", col="black"); # vertical axis
              segments(corrected.x + x.start.min, min.y.norm, corrected.x + x.end.max, min.y.norm, lty="dashed", col="black"); # the minimum value
              segments(corrected.x + x.start.min, max.y.norm, corrected.x + x.end.max, max.y.norm, lty="dashed", col="black"); # the minimum value
            }
            if( .do.SVG ) # SVG
            {
              svg.str <- c(svg.str,
                           # The axes:
                           .SVG.lines(x=.scale.x.to.SVG.plot(c(corrected.x + x.start.min, corrected.x + x.end.max,
                                                               corrected.x + x.start.min, corrected.x + x.start.min,
                                                               corrected.x + x.start.min, corrected.x + x.end.max,
                                                               corrected.x + x.start.min, corrected.x + x.end.max)),
                                      y=.scale.y.to.SVG.plot(c(y.cur + 0.5,               y.cur + 0.5,
                                                               y.cur + 0.5,               y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0,
                                                               min.y.norm,                min.y.norm,
                                                               max.y.norm,                max.y.norm)),
                                      connected=FALSE,
                                      stroke="black", lty=c("solid", "solid", "dashed", "dashed"),
                                      class="partial_cma_timeseries_axes")
              );
            }

            # 0%
            if( plot.partial.CMAs.as.timeseries.show.0perc &&
                (y.for.0perc <- (y.cur + 1 + (plot.partial.CMAs.as.timeseries.vspace-3) * (0 - min.y)/range.y)) >= y.cur + 0.5 )
            {
              if( .do.R ) # Rplot:
              {
                segments(corrected.x + x.start.min, y.for.0perc, corrected.x + x.end.max, y.for.0perc, lty="dotted", col="red"); # 0%
              }
              if( .do.SVG ) # SVG:
              {
                svg.str <- c(svg.str,
                             # 0% line
                             .SVG.lines(x=.scale.x.to.SVG.plot(c(corrected.x + x.start.min, corrected.x + x.end.max)),
                                        y=.scale.y.to.SVG.plot(c(y.for.0perc,               y.for.0perc)),
                                        connected=FALSE, stroke="red", lty="dotted",
                                        class="partial_cma_timeseries_0perc-line")
                );
              }
            }

            # 100%
            if( plot.partial.CMAs.as.timeseries.show.100perc &&
                (y.for.100perc <- (y.cur + 1 + (plot.partial.CMAs.as.timeseries.vspace-3) * (1.0 - min.y)/range.y)) <= y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0 )
            {
              if( .do.R ) # Rplot:
              {
                segments(corrected.x + x.start.min, y.for.100perc, corrected.x + x.end.max, y.for.100perc, lty="dotted", col="red"); # 100%
              }
              if( .do.SVG ) # SVG:
              {
                svg.str <- c(svg.str,
                             # 100% line
                             .SVG.lines(x=.scale.x.to.SVG.plot(c(corrected.x + x.start.min, corrected.x + x.end.max)),
                                        y=.scale.y.to.SVG.plot(c(y.for.100perc,             y.for.100perc)),
                                        connected=FALSE, stroke="red", lty="dotted",
                                        class="partial_cma_timeseries_100perc-line")
                );
              }
            }

            # Numeric values:
            if( .do.R ) # Rplot:
            {
              if( print.CMA && char.height.CMA <= 0.80 )
              {
                text(corrected.x + x.start.min, min.y.norm, sprintf("%.1f%%",100*min.y), pos=2, cex=CMA.cex, col="black");
                text(corrected.x + x.start.min, max.y.norm, sprintf("%.1f%%",100*max.y), pos=2, cex=CMA.cex, col="black");
                if( plot.partial.CMAs.as.timeseries.show.0perc && y.for.0perc >= y.cur + 0.5 )
                {
                  text(corrected.x + x.start.min, y.for.0perc, "0%", pos=2, cex=CMA.cex, col="red");
                }
                if( plot.partial.CMAs.as.timeseries.show.100perc && y.for.100perc <= y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0 )
                {
                  text(corrected.x + x.start.min, y.for.100perc, "100%", pos=2, cex=CMA.cex, col="red");
                }
              }
            }
            if( .do.SVG ) # SVG:
            {
              if( print.CMA && dims.chr.cma <= dims.chr.event )
              {
                svg.str <- c(svg.str,
                             # Text
                             .SVG.text(x=.scale.x.to.SVG.plot(c(corrected.x + x.start.min, corrected.x + x.start.min)),
                                       y=.scale.y.to.SVG.plot(c(min.y.norm,                max.y.norm)),
                                       text=c(                sprintf("%.1f%%",100*min.y), sprintf("%.1f%%",100*max.y)),
                                       col="black", font_size=dims.chr.cma, h.align="right", v.align="center", rotate=rotate.text,
                                       class="partial_cma_timeseries_axis_text"),
                             if( plot.partial.CMAs.as.timeseries.show.0perc && y.for.0perc >= y.cur + 0.5 )
                             {
                               .SVG.text(x=.scale.x.to.SVG.plot(corrected.x + x.start.min),
                                         y=.scale.y.to.SVG.plot(y.for.0perc),
                                         text="0%",
                                         col="red", font_size=dims.chr.cma, h.align="right", v.align="center", rotate=rotate.text,
                                         class="partial_cma_timeseries_axis_text")
                             },
                             if( plot.partial.CMAs.as.timeseries.show.100perc && y.for.100perc <= y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0 )
                             {
                               .SVG.text(x=.scale.x.to.SVG.plot(corrected.x + x.start.min),
                                         y=.scale.y.to.SVG.plot(y.for.100perc),
                                         text="100%",
                                         col="red", font_size=dims.chr.cma, h.align="right", v.align="center", rotate=rotate.text,
                                         class="partial_cma_timeseries_axis_text")
                             }
                );
              }
            }

            # The intervals:
            if( !is.na(plot.partial.CMAs.as.timeseries.col.interval) )
            {
              if( plot.partial.CMAs.as.timeseries.interval.type == "none" )
              {
                # Nothing to plot
              } else if( plot.partial.CMAs.as.timeseries.interval.type %in% c("segments", "arrows", "lines") )
              {
                if( .do.R ) # Rplot:
                {
                  # The lines:
                  segments(corrected.x.start, ppts$y.norm, corrected.x.end, ppts$y.norm, col=plot.partial.CMAs.as.timeseries.col.interval, lwd=plot.partial.CMAs.as.timeseries.lwd.interval);
                  if( plot.partial.CMAs.as.timeseries.interval.type == "segments" )
                  {
                    # The segment endings:
                    segments(corrected.x.start, ppts$y.norm - 0.2, corrected.x.start, ppts$y.norm + 0.2,
                             col=plot.partial.CMAs.as.timeseries.col.interval, lwd=plot.partial.CMAs.as.timeseries.lwd.interval);
                    segments(corrected.x.end,   ppts$y.norm - 0.2, corrected.x.end,   ppts$y.norm + 0.2,
                             col=plot.partial.CMAs.as.timeseries.col.interval, lwd=plot.partial.CMAs.as.timeseries.lwd.interval);
                  } else if( plot.partial.CMAs.as.timeseries.interval.type == "arrows" )
                  {
                    # The arrow endings:
                    segments(corrected.x.start + char.width/2, ppts$y.norm - char.height/2, corrected.x.start, ppts$y.norm,
                             col=plot.partial.CMAs.as.timeseries.col.interval, lwd=plot.partial.CMAs.as.timeseries.lwd.interval);
                    segments(corrected.x.start + char.width/2, ppts$y.norm + char.height/2, corrected.x.start, ppts$y.norm,
                             col=plot.partial.CMAs.as.timeseries.col.interval, lwd=plot.partial.CMAs.as.timeseries.lwd.interval);
                    segments(corrected.x.end - char.width/2, ppts$y.norm - char.height/2, corrected.x.end, ppts$y.norm,
                             col=plot.partial.CMAs.as.timeseries.col.interval, lwd=plot.partial.CMAs.as.timeseries.lwd.interval);
                    segments(corrected.x.end - char.width/2, ppts$y.norm + char.height/2, corrected.x.end, ppts$y.norm,
                             col=plot.partial.CMAs.as.timeseries.col.interval, lwd=plot.partial.CMAs.as.timeseries.lwd.interval);
                  }
                }

                if( .do.SVG ) # SVG:
                {
                  for( j in 1:nrow(ppts) )
                  {
                    svg.str <- c(svg.str,
                                 # The lines:
                                 .SVG.lines(x=.scale.x.to.SVG.plot(c(corrected.x.start[j], corrected.x.end[j])),
                                            y=.scale.y.to.SVG.plot(c(ppts$y.norm[j], ppts$y.norm[j])),
                                            stroke=plot.partial.CMAs.as.timeseries.col.interval, stroke_width=plot.partial.CMAs.as.timeseries.lwd.interval,
                                            class="partial_cma_timeseries_lines"),
                                 if( plot.partial.CMAs.as.timeseries.interval.type == "segments" )
                                 {
                                   # The segment endings:
                                   .SVG.lines(x=.scale.x.to.SVG.plot(c(corrected.x.start[j], corrected.x.start[j], corrected.x.end[j],   corrected.x.end[j])),
                                              y=.scale.y.to.SVG.plot(c(ppts$y.norm[j] - 0.2, ppts$y.norm[j] + 0.2, ppts$y.norm[j] - 0.2, ppts$y.norm[j] + 0.2)),
                                              connected=FALSE,
                                              stroke=plot.partial.CMAs.as.timeseries.col.interval, stroke_width=plot.partial.CMAs.as.timeseries.lwd.interval,
                                              class="partial_cma_timeseries_lines")
                                 } else if( plot.partial.CMAs.as.timeseries.interval.type == "arrows" )
                                 {
                                   # The arrow endings:
                                   .SVG.lines(x=.scale.x.to.SVG.plot(c(corrected.x.start[j] + dims.event.x/2, corrected.x.start[j],
                                                                       corrected.x.start[j] + dims.event.x/2, corrected.x.start[j],
                                                                       corrected.x.end[j]   - dims.event.x/2, corrected.x.end[j],
                                                                       corrected.x.end[j]   - dims.event.x/2, corrected.x.end[j])),
                                              y=.scale.y.to.SVG.plot(c(ppts$y.norm[j]       - 0.2, ppts$y.norm[j],
                                                                       ppts$y.norm[j]       + 0.2, ppts$y.norm[j],
                                                                       ppts$y.norm[j]       - 0.2, ppts$y.norm[j],
                                                                       ppts$y.norm[j]       + 0.2, ppts$y.norm[j])),
                                              connected=FALSE,
                                              stroke=plot.partial.CMAs.as.timeseries.col.interval, stroke_width=plot.partial.CMAs.as.timeseries.lwd.interval,
                                              class="partial_cma_timeseries_lines")
                                 }

                    );
                  }
                }
              } else if( plot.partial.CMAs.as.timeseries.interval.type == "rectangles" )
              {
                if( .do.R ) # Rplot:
                {
                  # As semi-transparent rectangles:
                  rect(corrected.x.start, y.cur + 0.5, corrected.x.end, y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0,
                       col=scales::alpha(plot.partial.CMAs.as.timeseries.col.interval, alpha=plot.partial.CMAs.as.timeseries.alpha.interval),
                       border=plot.partial.CMAs.as.timeseries.col.interval, lty="dotted");
                }

                if( .do.SVG ) # SVG:
                {
                  for( j in 1:nrow(ppts) )
                  {
                    svg.str <- c(svg.str,
                                 # As semi-transparent rectangles:
                                 .SVG.rect(x=.scale.x.to.SVG.plot(corrected.x.start[j]),  y=.scale.y.to.SVG.plot(y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0),
                                           xend=.scale.x.to.SVG.plot(corrected.x.end[j]), yend=.scale.y.to.SVG.plot(y.cur + 0.5),
                                           fill=plot.partial.CMAs.as.timeseries.col.interval, fill_opacity=plot.partial.CMAs.as.timeseries.alpha.interval,
                                           stroke=plot.partial.CMAs.as.timeseries.col.interval, lty="dotted",
                                           class="partial_cma_timeseries_rect")
                    );
                  }
                }
              }
            }

            # The points and connecting lines:
            if( !is.na(plot.partial.CMAs.as.timeseries.col.dot) )
            {
              if( .do.R ) # Rplot:
              {
                points(corrected.x.text, ppts$y.norm, col=plot.partial.CMAs.as.timeseries.col.dot, cex=CMA.cex, type="o", pch=19, lty="solid");
              }

              if( .do.SVG ) # SVG:
              {
                svg.str <- c(svg.str,
                             # The connecting lines:
                             .SVG.lines(x=.scale.x.to.SVG.plot(corrected.x.text), y=.scale.y.to.SVG.plot(ppts$y.norm),
                                        connected=TRUE,
                                        stroke=plot.partial.CMAs.as.timeseries.col.dot, lty="solid",
                                        class="partial_cma_timeseries_connecting_lines"),
                             # The points:
                             .SVG.points(x=.scale.x.to.SVG.plot(corrected.x.text), y=.scale.y.to.SVG.plot(ppts$y.norm),
                                         col=plot.partial.CMAs.as.timeseries.col.dot, cex=CMA.cex, pch=19,
                                         class="partial_cma_timeseries_points")
                );
              }
            }

            # The actual values:
            if( .do.R ) # Rplot:
            {
              if( print.CMA && char.height.CMA <= 0.80 && !is.na(plot.partial.CMAs.as.timeseries.col.text) )
              {
                text(corrected.x.text, ppts$y.norm, ppts$text, adj=c(0.5,-0.5), cex=CMA.cex, col=plot.partial.CMAs.as.timeseries.col.text);
              }
            }
            if( .do.SVG ) # SVG:
            {
              if( print.CMA && dims.chr.cma <= dims.chr.event && !is.na(plot.partial.CMAs.as.timeseries.col.text) )
              {
                svg.str <- c(svg.str,
                             # The actual values:
                             .SVG.text(x=.scale.x.to.SVG.plot(corrected.x.text), y=.scale.y.to.SVG.plot(ppts$y.norm) + dims.chr.cma, text=ppts$text,
                                       col=plot.partial.CMAs.as.timeseries.col.text, font_size=dims.chr.cma,
                                       h.align="center", v.align="center",
                                       class="partial_cma_timeseries_values")
                );
              }
            }

            # Advance to next patient:
            y.cur <- y.cur + plot.partial.CMAs.as.timeseries.vspace;
          }
        }
      }
    }
  }


  ##
  ## Separator between CMA and event plotting areas ####
  ##

  # Mark the drawing area for the CMAs:
  if( plot.CMA && has.estimated.CMA && adh.plot.space[2] > 0 )
  {
    if( is.cma.TS.or.SW )
    {
      if( .do.R ) # Rplot:
      {
        abline(v=c(.rescale.xcoord.for.CMA.plot(0.0), .rescale.xcoord.for.CMA.plot(1.0)), col=CMA.plot.col, lty=c("solid","dotted"), lwd=1);
      }

      if( .do.SVG ) # SVG:
      {
        svg.str <- c(svg.str,
                     # Vertical guides:
                     .SVG.comment("The vertical guides for the CMA drawing area"),
                     .SVG.lines(x=rep(c(.scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(0.0)), .scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(1.0))), each=2),
                                y=rep(c(dims.plot.y, dims.plot.y + dims.plot.height), times=2),
                                connected=FALSE,
                                stroke=CMA.plot.border, stroke_width=1, lty=c("solid", "dotted"),
                                class="cma-drawing-area-guides-lines")
        );
      }
    } else
    {
      if( .do.R ) # Rplot:
      {
        if( adh.max > 1.0 )
        {
          rect(.rescale.xcoord.for.CMA.plot(0.0), par("usr")[3], .rescale.xcoord.for.CMA.plot(adh.max), par("usr")[4], col=adjustcolor(CMA.plot.bkg,alpha.f=0.25), border=NA);
          abline(v=c(.rescale.xcoord.for.CMA.plot(0.0), .rescale.xcoord.for.CMA.plot(1.0), .rescale.xcoord.for.CMA.plot(adh.max)), col=CMA.plot.border, lty=c("solid","dotted","solid"), lwd=1);
          mtext( c("0%",sprintf("%.1f%%",adh.max*100)), 3, line=0.5, at=c(.rescale.xcoord.for.CMA.plot(0), .rescale.xcoord.for.CMA.plot(adh.max)), las=2, cex=cex.axis, col=CMA.plot.border );
          if( (.rescale.xcoord.for.CMA.plot(adh.max) - .rescale.xcoord.for.CMA.plot(1.0)) > 1.5*strwidth("0", cex=cex.axis) ) # Don't overcrowd the 100% and maximum CMA by omitting 100%
          {
            mtext( c("100%"), 3, line=0.5, at=c(.rescale.xcoord.for.CMA.plot(1.0)), las=2, cex=cex.axis, col=CMA.plot.border );
          }
        } else
        {
          rect(.rescale.xcoord.for.CMA.plot(0.0), par("usr")[3], .rescale.xcoord.for.CMA.plot(1.0), par("usr")[4], col=adjustcolor(CMA.plot.bkg,alpha.f=0.25), border=NA);
          abline(v=c(.rescale.xcoord.for.CMA.plot(0.0), .rescale.xcoord.for.CMA.plot(1.0)), col=CMA.plot.border, lty="solid", lwd=1);
          mtext( c("0%","100%"), 3, line=0.5, at=c(.rescale.xcoord.for.CMA.plot(0), .rescale.xcoord.for.CMA.plot(1.0)), las=2, cex=cex.axis, col=CMA.plot.border );
        }
      }

      if( .do.SVG ) # SVG:
      {
        svg.str <- c(svg.str,
                     # Background:
                     .SVG.rect(x=.scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(0.0)),
                               y=dims.plot.y,
                               width=.scale.width.to.SVG.plot(.rescale.xcoord.for.CMA.plot(adh.max)),
                               height=dims.plot.height,
                               stroke="none", fill=CMA.plot.bkg, fill_opacity=0.25,
                               class="cma-drawing-area-bkg"),

                     # Vertical guides:
                     .SVG.lines(x=rep(c(.scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(0.0)),
                                        .scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(1.0)),
                                        if(adh.max > 1.0) .scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(adh.max))),
                                      each=2),
                                y=rep(c(dims.plot.y, dims.plot.y + dims.plot.height), times=ifelse(adh.max > 1.0, 3, 2)),
                                connected=FALSE,
                                stroke=CMA.plot.border, stroke_width=1, lty=if(adh.max > 1.0) c("solid", "dotted", "solid") else "solid",
                                class="cma-drawing-area-guides-lines"),

                     # Text guides:
                     .SVG.text(x=.scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(0.0)), y=(dims.plot.y - dims.chr.axis/2),
                               text="0%", col="black", font="Arial", font_size=dims.chr.axis, h.align="left", v.align="center", rotate=-(90+rotate.text),
                               class="cma-drawing-area-guides-text"),
                     if(adh.max > 1.0)
                     {
                       c(
                         .SVG.text(x=.scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(adh.max)), y=(dims.plot.y - dims.chr.axis/2),
                                   text=sprintf("%.1f%%",adh.max*100), col="black", font="Arial", font_size=dims.chr.axis, h.align="left", v.align="center", rotate=-30,
                                   class="cma-drawing-area-guides-text"),
                         if(dims.event.x*(.rescale.xcoord.for.CMA.plot(adh.max) - .rescale.xcoord.for.CMA.plot(1.0))/dims.day > 2.0*dims.chr.axis)
                         {
                           # Don't overcrowd the 100% and maximum CMA
                           .SVG.text(x=.scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(1.0)), y=(dims.plot.y - dims.chr.axis/2),
                                     text="100%", col="black", font="Arial", font_size=dims.chr.axis, h.align="left", v.align="center", rotate=-(90+rotate.text),
                                     class="cma-drawing-area-guides-text")
                         }
                       )
                     } else
                     {
                       .SVG.text(x=.scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(1.0)), y=(dims.plot.y - dims.chr.axis/2),
                                 text="100%", col="black", font="Arial", font_size=dims.chr.axis, h.align="left", v.align="center", rotate=-(90+rotate.text),
                                 class="cma-drawing-area-guides-text")
                     }
        );
      }
    }
  }


  ##
  ## Title, box and axes ####
  ##

  title.string <- paste0(ifelse(is.null(title),"",                                   # the plot title
                                ifelse(length(title)==1,
                                       title,
                                       ifelse(align.all.patients,
                                              title["aligned"],
                                              title["notaligned"]))),
                         ifelse(!is.null(title) && show.cma,
                                paste0(" ",
                                       switch(class(cma)[1],
                                              "CMA_sliding_window"=paste0("sliding window (",cma$computed.CMA,")"),
                                              "CMA_per_episode"=   paste0("per episode (",cma$computed.CMA,")"),
                                              class(cma)[1])
                                ),
                                ""));

  if( .do.R ) # Rplot:
  {
    box(); # bounding box
    title(main=title.string, # title
          xlab=ifelse(is.null(xlab), # x axis label
                      "",
                      ifelse(length(xlab)==1,
                             xlab,
                             xlab[show.period])),
          cex.lab=cex.lab);
    mtext(y.label$string, side=2, line=par("mar")[2]-1, at=(par("usr")[4] + par("usr")[3])/2, cex=cex.lab, las=3); # y-axis label
  }

  if( .do.SVG ) # SVG:
  {
    svg.str <- c(svg.str,
                 # The bounding box:
                 .SVG.rect(x=dims.plot.x,
                           y=dims.plot.y,
                           width=dims.plot.width,
                           height=dims.plot.height,
                           stroke="black", stroke_width=1, fill="none",
                           class="bounding-box", comment="The bounding box"),

                 # The title:
                 .SVG.text(x=(dims.plot.x + dims.total.width)/2, y=dims.chr.title,
                           text=title.string, col="black", font="Arial", font_size=dims.chr.title, h.align="center", v.align="center",
                           class="main-title", comment="The main title"),

                 # The y axis label:
                 .SVG.text(x=dims.chr.axis, y=dims.total.height/2,
                           text=as.character(y.label$string), col="black", font="Arial", font_size=dims.chr.lab, h.align="center", v.align="center", rotate=-90,
                           class="axis-label-y", comment="The y-axis label"),

                 # The x axis label:
                 .SVG.text(x=(dims.plot.x + dims.total.width)/2, y=(dims.total.height - dims.chr.axis),
                           text=as.character(x.label), col="black", font="Arial", font_size=dims.chr.lab, h.align="center", v.align="center",
                           class="axis-label-x", comment="The x-axis label")
    );
  }

  # The x-axis and vertical guides:
  if( period.in.days > 0 )
  {
    if( show.period=="dates" )
    {
      xpos <- seq(0, as.numeric(endperiod), by=period.in.days); # where to put lables and guidelines
      axis.labels <- as.character(earliest.date + round(xpos, 1), format=cma$date.format);
    } else
    {
        if( align.first.event.at.zero )
        {
            xpos <- c(correct.earliest.followup.window - seq(0, as.numeric(correct.earliest.followup.window), by=period.in.days),
                      seq(0, as.numeric(endperiod), by=period.in.days) + correct.earliest.followup.window);
            xpos <- xpos[ xpos >= 0 & xpos <= endperiod ];
            axis.labels <- as.character(round(xpos - correct.earliest.followup.window, 1));
        } else
        {
          xpos <- seq(0, as.numeric(endperiod), by=period.in.days);
          axis.labels <- as.character(round(xpos, 1));
        }
    }

    if( .do.R ) # Rplot:
    {
      axis( 1, at=adh.plot.space[2] + xpos, labels=FALSE);
      text(adh.plot.space[2] + xpos, par("usr")[3], labels=axis.labels, cex=cex.axis, srt=30, adj=c(1,3), xpd=TRUE);
      abline( v=adh.plot.space[2] + xpos,       lty="dotted", col=gray(0.5) );
      abline( v=adh.plot.space[2] + endperiod,  lty="solid",  col=gray(0.5) );
    }
  }

  if( .do.SVG ) # SVG:
  {
    if( !is.null(date.labels) )
    {
      xs <- (dims.plot.x + dims.event.x * date.labels$position/dims.day);
      ys <- (dims.plot.y + dims.plot.height + dims.chr.axis);
      svg.str <- c(svg.str,
                   # Axis labels:
                   .SVG.text(x=xs, y=rep(ys, length(xs)),
                             text=as.character(date.labels$string), col="black", font="Arial", font_size=dims.chr.axis, h.align="right", v.align="center", rotate=-(90+rotate.text),
                             class="axis-values-x"),

                   # Axis ticks:
                   .SVG.lines(x=rep(xs,each=2),
                              y=dims.plot.y + dims.plot.height + rep(c(0, dims.chr.axis/2), times=length(xs)),
                              connected=FALSE,
                              stroke="black", stroke_width=1,
                              class="axis-ticks-x"),

                   # Vertical dotted lines:
                   .SVG.lines(x=rep(xs,each=2),
                              y=dims.plot.y + rep(c(dims.plot.height, 0), times=length(xs)),
                              connected=FALSE,
                              stroke="gray50", stroke_width=1, lty="dotted",
                              class="vertical-date-lines")
      );
    }
  }


  ##
  ## The legend ####
  ##

  if( .do.R ) # Rplot:
  {
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

      if( !plot.dose )
      {
        if( do.plot ) text(x + 5.0*legend.char.width, cur.y, "duration", col="black", cex=legend.cex, pos=4);
        cur.y <- cur.y - 1.5*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("duration", cex=legend.cex));
      } else
      {
        if( do.plot ) text(x + 5.0*legend.char.width, cur.y, "duration (min. dose)", col="black", cex=legend.cex, pos=4);
        cur.y <- cur.y - 1.5*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("duration (min. dose)", cex=legend.cex));
        if( do.plot ) segments(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y, lty=lty.event, lwd=lwd.event.max.dose, col="black");
        if( do.plot ) points(x + 1.0*legend.char.width, cur.y, pch=pch.start.event, cex=legend.cex, col="black");
        if( do.plot ) points(x + 4.0*legend.char.width, cur.y, pch=pch.end.event, cex=legend.cex, col="black");
        if( do.plot ) text(x + 5.0*legend.char.width, cur.y, "duration (max. dose)", col="black", cex=legend.cex, pos=4);
        cur.y <- cur.y - 1.5*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("duration (max. dose)", cex=legend.cex));
      }

      # No event:
      if( do.plot ) segments(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y, lty=lty.continuation, lwd=lwd.continuation, col=col.continuation);
      if( do.plot ) text(x + 5.0*legend.char.width, cur.y, "no event/connector", col="black", cex=legend.cex, pos=4);
      cur.y <- cur.y - 1.5*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("no event/connector", cex=legend.cex));

      # Event intervals:
      if( show.event.intervals )
      {
        if( do.plot ) rect(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y - 1.0*legend.char.height, border="black", col=adjustcolor("black",alpha.f=0.5));
        if( do.plot ) text(x + 5.0*legend.char.width, cur.y - 0.5*legend.char.height, "days covered", col="black", cex=legend.cex, pos=4);
        cur.y <- cur.y - 1.5*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("days covered", cex=legend.cex));
        if( do.plot ) rect(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y - 1.0*legend.char.height, border="black", col=NA); #, col="black", density=25);
        if( do.plot ) text(x + 5.0*legend.char.width, cur.y - 0.5*legend.char.height, "gap days", col="black", cex=legend.cex, pos=4);
        cur.y <- cur.y - 2.0*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("gap days", cex=legend.cex));
      }

      # medication classes:
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
        if( inherits(cma,"CMA8") && !is.null(cma$real.obs.windows) && show.real.obs.window.start )
        {
          # CMA8 also has a "real" OW:
          if( do.plot ) rect(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y - 1.0*legend.char.height,
                             border=rgb(1,1,1,0.0), col=adjustcolor(observation.window.col,alpha.f=observation.window.opacity)); #, density=observation.window.density, angle=observation.window.angle);
          if( do.plot ) text(x + 5.0*legend.char.width, cur.y - 0.5*legend.char.height, "theor. obs. wnd.", col="black", cex=legend.cex, pos=4);
          cur.y <- cur.y - 1.5*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("theor. obs. wnd.", cex=legend.cex));
          if( do.plot ) rect(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y - 1.0*legend.char.height,
                             border=rgb(1,1,1,0.0), col=adjustcolor(observation.window.col,alpha.f=observation.window.opacity)); #, density=real.obs.window.density, angle=real.obs.window.angle);
          if( do.plot ) text(x + 5.0*legend.char.width, cur.y - 0.5*legend.char.height, "real obs.wnd.", col="black", cex=legend.cex, pos=4);
          cur.y <- cur.y - 2.0*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("real obs.wnd.", cex=legend.cex));
        } else
        {
          if( do.plot ) rect(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y - 1.0*legend.char.height,
                             border=rgb(1,1,1,0.0), col=adjustcolor(observation.window.col,alpha.f=observation.window.opacity)) #, density=observation.window.density, angle=observation.window.angle);
          if( do.plot ) text(x + 5.0*legend.char.width, cur.y - 0.5*legend.char.height, "observation wnd.", col="black", cex=legend.cex, pos=4);
          cur.y <- cur.y - 2.0*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("observation wnd.", cex=legend.cex));
        }
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
      x <- legend.x; y <- legend.y;
      if( is.na(x) || x == "right" )
      {
        x <- par("usr")[2] - legend.size["width"] - legend.char.width;
      } else if( x == "left" )
      {
        x <- par("usr")[1] + legend.char.width;
      } else if( !is.numeric(x) && length(x) != 1 )
      {
        x <- par("usr")[2] - legend.size["width"] - legend.char.width;
      }
      if( is.na(y) || y == "bottom" )
      {
        y <- par("usr")[3] + legend.char.height;
      } else if( y == "top" )
      {
        y <- par("usr")[4] - legend.size["height"] - legend.char.height;
      } else if( !is.numeric(y) && length(y) != 1 )
      {
        y <- par("usr")[3] + legend.char.height;
      }
      ret.val <- .legend(x, y, as.numeric(legend.size["width"]), as.numeric(legend.size["height"]));
    }
    else
    {
      ret.val <- c("width"=NA, "height"=NA);
    }
  }

  if( .do.SVG ) # SVG:
  {
    .legend <- function(x=0, y=0)
    {
      # The legend is an object that we can move around, scale, etc:
      l1 <- c(.SVG.comment("The legend", newpara=TRUE, newline=TRUE),
              '<defs>\n', # don't display it yet...
              '<g id="legend">\n');

      # The legend dimensions and other aesthetics:
      lw <- lh <- 0; # width and height
      lmx <- dims.chr.legend; lmy <- 2 # margins
      lnl <- 1.25; lnp <- 0.25; # the vertical size of a newline and newpara (in dims.chr.legend)

      # The actual legend content:
      # The legend title:
      l2 <- c(.SVG.text(x=lmx, y=lmy+lh+dims.chr.legend.title*2/3, text="Legend",
                        font_size=dims.chr.legend.title, font="Arial", h.align="left", v.align="center", col="gray30",
                        class="legend-title"));
      lh <- lh + dims.chr.legend.title + lnl*dims.chr.legend; lw <- max(lw, .SVG.string.dims("Legend", font_size=dims.chr.legend.title)["width"]);
      lh <- lh + lnp*dims.chr.legend.title; # new para

      # The event:
      l2 <- c(l2,
              .SVG.lines(x=c(lmx, lmx + 3*dims.chr.legend), y=c(lmy+lh, lmy+lh),
                         connected=FALSE, stroke="black", stroke_width=lwd.event, lty=lty.event,
                         class="legend-events"),
              .SVG.points(x=c(lmx, lmx + 3*dims.chr.legend), y=c(lmy+lh, lmy+lh),
                          pch=c(pch.start.event, pch.end.event),col="black", cex=legend.cex,
                          class="legend-events"));

      if( !plot.dose )
      {
        l2 <- c(l2,
                .SVG.text(x=lmx + 4*dims.chr.legend, y=lmy+lh, text="duration",
                          col="black", font_size=dims.chr.legend, h.align="left", v.align="center",
                          class="legend-events"));
        lh <- lh + lnl*dims.chr.legend; lw <- max(lw, .SVG.string.dims("duration", font_size=dims.chr.legend)["width"] + 4*dims.chr.legend);
      } else
      {
        # Min dose:
        l2 <- c(l2,
                .SVG.text(x=lmx + 4*dims.chr.legend, y=lmy+lh, text="duration (min. dose)",
                          col="black", font_size=dims.chr.legend, h.align="left", v.align="center",
                          class="legend-events"));
        lh <- lh + lnl*dims.chr.legend; lw <- max(lw, .SVG.string.dims("duration (min. dose)", font_size=dims.chr.legend)["width"] + 4*dims.chr.legend);

        # Max dose:
        l2 <- c(l2,
                .SVG.lines(x=c(lmx, lmx + 3*dims.chr.legend), y=c(lmy+lh, lmy+lh),
                           connected=FALSE, stroke="black", stroke_width=lwd.event.max.dose, lty=lty.event,
                           class="legend-events"),
                .SVG.points(x=c(lmx, lmx + 3*dims.chr.legend), y=c(lmy+lh, lmy+lh),
                            pch=c(pch.start.event, pch.end.event),col="black", cex=legend.cex,
                            class="legend-events"),
                .SVG.text(x=lmx + 4*dims.chr.legend, y=lmy+lh, text="duration (max. dose)",
                          col="black", font_size=dims.chr.legend, h.align="left", v.align="center",
                          class="legend-events"));
        lh <- lh + lnl*dims.chr.legend; lw <- max(lw, .SVG.string.dims("duration (max. dose)", font_size=dims.chr.legend)["width"] + 4*dims.chr.legend);
      }

      # No event:
      l2 <- c(l2,
              .SVG.lines(x=c(lmx, lmx + 3*dims.chr.legend), y=c(lmy+lh, lmy+lh),
                         connected=FALSE, stroke=col.continuation, stroke_width=lwd.continuation, lty=lty.continuation,
                         class="legend-no-event"),
              .SVG.text(x=lmx + 4*dims.chr.legend, y=lmy+lh, text="no event/connector",
                        col="black", font_size=dims.chr.legend, h.align="left", v.align="center",
                        class="legend-no-event"));
      lh <- lh + lnl*dims.chr.legend; lw <- max(lw, .SVG.string.dims("no event/connector", font_size=dims.chr.legend)["width"] + 4*dims.chr.legend);
      lh <- lh + lnp*dims.chr.legend.title; # new para

      # Event intervals:
      if( show.event.intervals )
      {
        l2 <- c(l2,
                .SVG.rect(x=lmx, y=lmy+lh-dims.chr.legend/2, width=3*dims.chr.legend, height=1*dims.chr.legend,
                          stroke="black", fill="black", fill_opacity=0.5,
                          class="legend-interval"),
                .SVG.text(x=lmx + 4*dims.chr.legend, y=lmy+lh, text="days covered",
                          col="black", font_size=dims.chr.legend, h.align="left", v.align="center",
                          class="legend-interval"));
        lh <- lh + lnl*dims.chr.legend; lw <- max(lw, .SVG.string.dims("days covered", font_size=dims.chr.legend)["width"] + 4*dims.chr.legend);
        l2 <- c(l2,
                .SVG.rect(x=lmx, y=lmy+lh-dims.chr.legend/2, width=3*dims.chr.legend, height=1*dims.chr.legend,
                          stroke="black", fill="none",
                          class="legend-interval"),
                .SVG.text(x=lmx + 4*dims.chr.legend, y=lmy+lh, text="gap days",
                          col="black", font_size=dims.chr.legend, h.align="left", v.align="center",
                          class="legend-interval"));
        lh <- lh + lnl*dims.chr.legend; lw <- max(lw, .SVG.string.dims("gap days", font_size=dims.chr.legend)["width"] + 4*dims.chr.legend);
        lh <- lh + lnp*dims.chr.legend.title; # new para
      }

      # Medication classes:
      for( i in 1:length(cols) )
      {
        med.class.name <- names(cols)[i]; med.class.name <- ifelse(is.na(med.class.name),"<missing>",med.class.name);
        l2 <- c(l2,
                .SVG.rect(x=lmx, y=lmy+lh-dims.chr.legend/2, width=3*dims.chr.legend, height=1*dims.chr.legend,
                          stroke="black", fill=cols[i], fill_opacity=0.5,
                          class="legend-medication-class"));
        med.class.name <- names(cols)[i]; med.class.name <- ifelse(is.na(med.class.name),"<missing>",med.class.name);
        if( print.dose || plot.dose )
        {
          dose.for.cat <- (dose.range$category == med.class.name);
          if( sum(dose.for.cat,na.rm=TRUE) == 1 )
          {
            med.class.name <- paste0(med.class.name," (",dose.range$min[dose.for.cat]," - ",dose.range$max[dose.for.cat],")");
          }
        }
        l2 <- c(l2,
                .SVG.text(x=lmx + 4*dims.chr.legend, y=lmy+lh, text=med.class.name,
                          col="black", font_size=dims.chr.legend, h.align="left", v.align="center",
                          class="legend-medication-class"));
        lh <- lh + lnl*dims.chr.legend; lw <- max(lw, .SVG.string.dims(med.class.name, font_size=dims.chr.legend)["width"] + 4*dims.chr.legend);
      }
      lh <- lh + lnp*dims.chr.legend.title; # new para

      # Follow-up window:
      if( highlight.followup.window )
      {
        l2 <- c(l2,
                .SVG.rect(x=lmx, y=lmy+lh-dims.chr.legend/2, width=3*dims.chr.legend, height=1*dims.chr.legend,
                          stroke=followup.window.col, fill="none", stroke_width=2, lty="dashed",
                          class="legend-fuw"),
                .SVG.text(x=lmx + 4*dims.chr.legend, y=lmy+lh, text="follow-up wnd.",
                          col="black", font_size=dims.chr.legend, h.align="left", v.align="center",
                          class="legend-interval"));
        lh <- lh + lnl*dims.chr.legend; lw <- max(lw, .SVG.string.dims("follow-up wnd", font_size=dims.chr.legend)["width"] + 4*dims.chr.legend);
      }

      # Observation window:
      if( highlight.observation.window )
      {
        if( inherits(cma,"CMA8") && !is.null(cma$real.obs.windows) && show.real.obs.window.start )
        {
          # CMA8 also has a "real" OW:
          l2 <- c(l2,
                  .SVG.rect(x=lmx, y=lmy+lh-dims.chr.legend/2, width=3*dims.chr.legend, height=1*dims.chr.legend,
                            stroke="none", fill=observation.window.col, fill_opacity=observation.window.opacity,
                            class="legend-ow-theoretical"),
                  .SVG.text(x=lmx + 4*dims.chr.legend, y=lmy+lh, text="theor. obs. wnd.",
                            col="black", font_size=dims.chr.legend, h.align="left", v.align="center",
                            class="legend-ow-theoretical"));
          lh <- lh + lnl*dims.chr.legend; lw <- max(lw, .SVG.string.dims("theor. obs. wnd", font_size=dims.chr.legend)["width"] + 4*dims.chr.legend);
          l2 <- c(l2,
                  .SVG.rect(x=lmx, y=lmy+lh-dims.chr.legend/2, width=3*dims.chr.legend, height=1*dims.chr.legend,
                            stroke="none", fill=observation.window.col, fill_opacity=observation.window.opacity,
                            class="legend-ow-real"),
                  .SVG.text(x=lmx + 4*dims.chr.legend, y=lmy+lh, text="real obs. wnd.",
                            col="black", font_size=dims.chr.legend, h.align="left", v.align="center",
                            class="legend-ow-real"));
          lh <- lh + lnl*dims.chr.legend; lw <- max(lw, .SVG.string.dims("real obs. wnd.", font_size=dims.chr.legend)["width"] + 4*dims.chr.legend);
        } else
        {
          l2 <- c(l2,
                  .SVG.rect(x=lmx, y=lmy+lh-dims.chr.legend/2, width=3*dims.chr.legend, height=1*dims.chr.legend,
                            stroke="none", fill=observation.window.col, fill_opacity=observation.window.opacity,
                            class="legend-ow"),
                  .SVG.text(x=lmx + 4*dims.chr.legend, y=lmy+lh, text="observation wnd.",
                            col="black", font_size=dims.chr.legend, h.align="left", v.align="center",
                            class="legend-ow"));
          lh <- lh + lnl*dims.chr.legend; lw <- max(lw, .SVG.string.dims("duration", font_size=dims.chr.legend)["width"] + 4*dims.chr.legend);
        }
      }

      # The legend background:
      lbox <- .SVG.rect(x=0, y=0, width=lw+2*lmx, height=lh+2*lmy, stroke="gray60", stroke_width=2, fill="gray99", fill_opacity=legend.bkg.opacity, class="legend-background");

      # The legend position:
      if( is.null(x) || length(x) > 1 || is.na(x) || !(x %in% c("left", "center", "right") || is.numeric(x)) ) x <- "right";
      if( is.na(x) || x == "right" )
      {
        x <- (dims.plot.x + dims.plot.width - lw - 3*lmx);
      } else if( x == "center" )
      {
        x <- (dims.plot.x + lmx + (dims.plot.width - lmx - lw)/2);
      } else if( x == "left" )
      {
        x <- (dims.plot.x + lmx);
      } else
      {
        x <- .scale.x.to.SVG.plot(x);
      }
      if( is.null(y) || length(y) > 1 || is.na(y) || !(y %in% c("top", "center", "bottom") || is.numeric(y)) ) y <- "bottom";
      if( is.na(y) || y == "bottom" )
      {
        y <- (dims.plot.y + dims.plot.height - lh - 3*lmy);
      } else if( y == "center" )
      {
        y <- (dims.plot.y + (dims.plot.height - lh - 2*lmy)/2);
      } else if( y == "top" )
      {
        y <- (dims.plot.y + lmy);
      } else
      {
        y <- .scale.y.to.SVG.plot(y);
      }

      # Close the legend:
      l2 <- c(l2,
              '</g>\n',
              '</defs>\n',
              # Display it as desired:
              '<use xlink:href="#legend" transform="translate(',x,' ',y,')"/>\n');

      # Insert the legend background where it should be:
      return (c(l1, lbox, l2));
    }
    svg.str <- c(svg.str,
                 # The legend:
                 .legend(legend.x, legend.y)
    );
  }


  ##
  ## Finish and possibly export the file(s) ####
  ##

  if( .do.R ) # Rplot:
  {
    par(old.par); # restore graphical params
  }

  if( .do.SVG ) # Close the <sgv> tag:
  {
    svg.str <- c(svg.str, '</svg>\n');

    # Export to various formats (if so requested):
    exported.file.names <- NULL; # the list of exported files (if any)
    if( !is.null(export.formats) )
    {
      file.svg <- NULL;
      if( "svg" %in% export.formats ||
          "svg-and-html" %in% export.formats)
      {
        ## Export as stand-alone SVG file ####
        file.svg <- ifelse( is.na(export.formats.directory),
                            tempfile(export.formats.fileprefix, fileext=".svg"),
                            file.path(export.formats.directory, paste0(export.formats.fileprefix,".svg")) );
        exported.file.names <- c(exported.file.names, file.svg);

        # Export SVG:
        writeLines(c(svg.header, svg.str), file.svg, sep="");
      }

      if( "svg-and-html" %in% export.formats )
      {
        ## Export as HTML and friends as stand-alone files ####
        #  (the SVG was already generated)
        file.html <- ifelse( is.na(export.formats.directory),
                             tempfile(export.formats.fileprefix, fileext=".html"),
                             file.path(export.formats.directory, paste0(export.formats.fileprefix,".html")) );
        file.css  <- ifelse( is.na(export.formats.directory),
                             tempfile(export.formats.fileprefix, fileext=".css"),
                             file.path(export.formats.directory, paste0(export.formats.fileprefix,".css")) );
        file.js   <- ifelse( is.na(export.formats.directory),
                             tempfile(export.formats.fileprefix, fileext=".js"),
                             file.path(export.formats.directory, paste0(export.formats.fileprefix,".js")) );
        exported.file.names <- c(exported.file.names, file.html, file.css, file.js);

        # Load the HTML template and replace generics by their actual values before saving it in the desired location:
        html.template.path <- system.file('html-templates/html-template.html', package='AdhereR');
        if( is.null(html.template.path) || html.template.path=="" )
        {
          warning("Cannot load the HTML template -- please reinstall the AdhereR package!\n");
          return (invisible(NULL));
        }
        html.template <- readLines(html.template.path);
        html.template <- sub("PATH-TO-JS",    basename(file.js),  html.template, fixed=TRUE); # JavaScript
        html.template <- sub("PATH-TO-CSS",   basename(file.css), html.template, fixed=TRUE); # CSS
        html.template <- sub("PATH-TO-IMAGE", basename(file.svg), html.template, fixed=TRUE); # SVG
        #if( TRUE )  html.template <- sub('<img class="adherence_plot" ', '<img class="adherence_plot" height="600" ', html.template, fixed=TRUE); # height (if defined)
        #if( FALSE ) html.template <- sub('<img class="adherence_plot" ', '<img class="adherence_plot" width="600" ', html.template, fixed=TRUE); # width (if defined)
        if( TRUE )  html.template <- sub('<object id="adherence_plot" ', '<object id="adherence_plot" height="600" ', html.template, fixed=TRUE); # height (if defined)
        if( FALSE ) html.template <- sub('<object id="adherence_plot" ', '<object id="adherence_plot" width="600" ', html.template, fixed=TRUE); # width (if defined)
        writeLines(html.template, file.html, sep="\n");

        # Export CSS:
        css.template.path <- system.file('html-templates/css-template.css', package='AdhereR');
        if( is.null(css.template.path) || css.template.path=="" )
        {
          warning("Cannot load the CSS template -- please reinstall the AdhereR package!\n");
          return (invisible(NULL));
        }
        file.copy(from=css.template.path, to=file.css, overwrite=TRUE, recursive=FALSE);

        # Export JS:
        js.template.path <- system.file('html-templates/javascript-template.js', package='AdhereR');
        if( is.null(js.template.path) || js.template.path=="" )
        {
          warning("Cannot load the JavaScript template -- please reinstall the AdhereR package!\n");
          return (invisible(NULL));
        }
        file.copy(from=js.template.path, to=file.js, overwrite=TRUE, recursive=FALSE);
      }

      if( "svg-in-html" %in% export.formats )
      {
        ## Export as self-contained HTML document ####
        html.prefix <- ifelse("svg-and-html" %in% export.formats,
                              paste0(export.formats.fileprefix,"-selfcontained"),
                              export.formats.fileprefix); # avoid conflicts between HTMLs
        file.html <- ifelse( is.na(export.formats.directory),
                             tempfile(html.prefix, fileext=".html"),
                             file.path(export.formats.directory, paste0(html.prefix,".html")) );
        exported.file.names <- c(exported.file.names, file.html);

        # Export HTML:
        writeLines(c('<!DOCTYPE html>\n',
                     '<html>\n',
                     '<head>\n',
                     ' <script>\b',
                     "function display()\n",
                     "{\n",
                     "alert('Hello World!');\n",
                     "}\n",
                     '</script>\n',
                     '<style>\n',
                     '  body {background-color: powderblue;}\n',
                     '  h1   {color: blue;}\n',
                     '  p    {color: red;}\n',
                     '</style>\n',
                     '</head>\n',
                     '<body>\n',
                     svg.str,'\n',
                     '</body>\n',
                     '</html>'),
                   file.html, sep="");
      }

      if( any(c("png", "ps", "pdf", "webp") %in% export.formats) )
      {
        ## Export to flat file formats (PNG, JPG, PS, PDF or WEBP) ####
        # Need to covert the SVG to one of these, so we need to export it (if not already exported):
        if( is.null(file.svg) )
        {
          file.svg <- tempfile(export.formats.fileprefix, fileext=".svg");
          writeLines(c(svg.header, svg.str), file.svg, sep="");
        }

        if( "png" %in% export.formats )
        {
          # PNG file:
          file.png <- ifelse( is.na(export.formats.directory),
                              tempfile(export.formats.fileprefix, fileext=".png"),
                              file.path(export.formats.directory, paste0(export.formats.fileprefix,".png")) );
          exported.file.names <- c(exported.file.names, file.png);
          rsvg::rsvg_png(file.svg, file=file.png);
        }

        if( "webp" %in% export.formats )
        {
          # WEBP file:
          file.webp <- ifelse( is.na(export.formats.directory),
                               tempfile(export.formats.fileprefix, fileext=".webp"),
                               file.path(export.formats.directory, paste0(export.formats.fileprefix,".webp")) );
          exported.file.names <- c(exported.file.names, file.webp);
          rsvg::rsvg_webp(file.svg, file=file.webp);
        }

        if( "ps" %in% export.formats )
        {
          # PS file:
          file.ps <- ifelse( is.na(export.formats.directory),
                             tempfile(export.formats.fileprefix, fileext=".ps"),
                             file.path(export.formats.directory, paste0(export.formats.fileprefix,".ps")) );
          exported.file.names <- c(exported.file.names, file.ps);
          rsvg::rsvg_ps(file.svg, file=file.ps);
        }

        if( "pdf" %in% export.formats )
        {
          # PDF file:
          file.pdf <- ifelse( is.na(export.formats.directory),
                              tempfile(export.formats.fileprefix, fileext=".pdf"),
                              file.path(export.formats.directory, paste0(export.formats.fileprefix,".pdf")) );
          exported.file.names <- c(exported.file.names, file.pdf);
          rsvg::rsvg_pdf(file.svg, file=file.pdf);
        }
      }
    }
  }


  # Return value:
  return (invisible(exported.file.names));
}









# The old unified function plotting using base R graphics (held here for historical purposes)
.plot.CMAs.old <- function(cma,                                   # the CMA_per_episode or CMA_sliding_window (or derived) object
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
                       show.event.intervals=TRUE,             # show the actual prescription intervals
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
                       show.real.obs.window.start=TRUE, real.obs.window.density=35, real.obs.window.angle=30, # for CMA8, the real observation window starts at a different date
                       alternating.bands.cols=c("white", "gray95"), # the colors of the alternating vertical bands across patients (NULL=don't draw any; can be >= 1 color)
                       bw.plot=FALSE,                         # if TRUE, override all user-given colors and replace them with a scheme suitable for grayscale plotting
                       min.plot.size.in.characters.horiz=10, min.plot.size.in.characters.vert=0.25, # the minimum plot size (in characters: horizontally, for the whole duration, vertically, per event (and, if shown, per episode/sliding window))
                       max.patients.to.plot=100,        # maximum number of patients to plot
                       suppress.warnings=FALSE,         # suppress warnings?
                       ...
)
{
  ##
  ## Set-up, checks and local functions
  ##

  # Preconditions:
  if( is.null(cma) ||                                                                                            # must be: non-null
      !(inherits(cma, "CMA_per_episode") || inherits(cma, "CMA_sliding_window") || inherits(cma, "CMA0")) ||     # a proper CMA object
      is.null(cma$data) || nrow(cma$data) < 1 || !inherits(cma$data, "data.frame") ||                            # that containins non-null data derived from data.frame
      is.na(cma$ID.colname) || !(cma$ID.colname %in% names(cma$data)) ||                                         # has a valid patient ID column
      is.na(cma$event.date.colname) || !(cma$event.date.colname %in% names(cma$data)) ||                         # has a valid event date column
      is.na(cma$event.duration.colname) || !(cma$event.duration.colname %in% names(cma$data))                    # has a valid event duration column
  )
  {
    if( !suppress.warnings ) warning(paste0("Can only plot a correctly specified CMA object (i.e., with valid data and column names)!\n"));
    return (invisible(NULL));
  }

  # Is the cma a time series or per episodes?
  is.cma.TS.or.SW <- (inherits(cma, "CMA_per_episode") || inherits(cma, "CMA_sliding_window"));
  # Does the cma contains estimated CMAs?
  has.estimated.CMA <- !is.null(getCMA(cma));

  # Convert data.table to data.frame (basically, to guard against inconsistencies between data.table and data.frame in how they handle d[,i]):
  if( inherits(cma$data, "data.table") ) cma$data <- as.data.frame(cma$data);

  # Check compatibility between subtypes of plots:
  if( align.all.patients && show.period != "days" ){ show.period <- "days"; if( !suppress.warnings ) warning("When aligning all patients, cannot show actual dates: showing days instead!\n"); }


  ##
  ## Select patients:
  ##

  # The patients:
  patids <- unique(as.character(cma$data[,cma$ID.colname])); patids <- patids[!is.na(patids)];
  if( !is.null(patients.to.plot) ) patids <- intersect(patids, as.character(patients.to.plot));
  if( length(patids) == 0 )
  {
    if( !suppress.warnings ) warning("No patients to plot!\n");
    return (invisible(NULL));
  } else if( length(patids) > max.patients.to.plot )
  {
    if( !suppress.warnings ) warning(paste0("Too many patients to plot (",length(patids),
                                            ")! If this is the desired outcome, please change the 'max.patients.to.plot' parameter value (now set at ",
                                            max.patients.to.plot,") to at least ',length(patids),'!\n"));
    return (invisible(NULL));
  }

  # Select only the patients to display:
  cma <- subsetCMA(cma, patids);


  ##
  ## Cache, consolidate and homogenise the needed info (events, CMAs, FUW an OW)
  ##

  # Cache the CMA estimates (if any):
  cmas <- getCMA(cma);
  if( inherits(cmas, "data.table") ) cmas <- as.data.frame(cmas); # same conversion to data.frame as above

  # Depeding on the cma's exact type, the relevant columns might be different or even absent: homogenize them for later use
  if( inherits(cma, "CMA_per_episode") )
  {
    names(cmas)[2:ncol(cmas)] <- c("WND.ID", "start", "gap.days", "duration", "end", "CMA"); # avoid possible conflict with patients being called "ID"
  } else if( inherits(cma, "CMA_sliding_window") )
  {
    cmas <- cbind(cmas[,1:3], "gap.days"=NA, "duration"=cma$sliding.window.duration, cmas[,4:ncol(cmas)]);
    names(cmas)[2:ncol(cmas)] <- c("WND.ID", "start", "gap.days", "duration", "end", "CMA"); # avoid possible conflict with patients being called "ID"
  } else if( inherits(cma, "CMA0") && is.null(cma$event.info) )
  {
    # Try to compute the event.info:
    event.info <- compute.event.int.gaps(data=cma$data,
                                         ID.colname=cma$ID.colname,
                                         event.date.colname=cma$event.date.colname,
                                         event.duration.colname=cma$event.duration.colname,
                                         event.daily.dose.colname=cma$event.daily.dose.colname,
                                         medication.class.colname=cma$medication.class.colname,
                                         event.interval.colname="event.interval",
                                         gap.days.colname="gap.days",
                                         carryover.within.obs.window=FALSE,
                                         carryover.into.obs.window=FALSE,
                                         carry.only.for.same.medication=FALSE,
                                         consider.dosage.change=FALSE,
                                         followup.window.start=cma$followup.window.start,
                                         followup.window.start.unit=cma$followup.window.start.unit,
                                         followup.window.duration=cma$followup.window.duration,
                                         followup.window.duration.unit=cma$followup.window.duration.unit,
                                         observation.window.start=cma$observation.window.start,
                                         observation.window.start.unit=cma$observation.window.start.unit,
                                         observation.window.duration=cma$observation.window.duration,
                                         observation.window.duration.unit=cma$observation.window.duration.unit,
                                         date.format=cma$date.format,
                                         keep.window.start.end.dates=TRUE,
                                         remove.events.outside.followup.window=FALSE,
                                         keep.event.interval.for.all.events=TRUE,
                                         parallel.backend="none", # make sure this runs sequentially!
                                         parallel.threads=1,
                                         suppress.warnings=FALSE,
                                         return.data.table=FALSE);
    if( !is.null(event.info) )
    {
      # Keep only those events that intersect with the observation window (and keep only the part that is within the intersection):

      # Compute end prescription date as well:
      event.info$.DATE.as.Date.end <- .add.time.interval.to.date(event.info$.DATE.as.Date, event.info[,cma$event.duration.colname], "days");

      # Remove all treatments that end before FUW starts and those that start after FUW ends:
      event.info <- event.info[ !(event.info$.DATE.as.Date.end < event.info$.FU.START.DATE | event.info$.DATE.as.Date > event.info$.FU.END.DATE), ];
      if( is.null(event.info) || nrow(event.info) == 0 )
      {
        if( !suppress.warnings ) warning("No events in the follow-up window: nothing to plot!\n");
        return (invisible(NULL));
      }

      # Find all prescriptions that start before the follow-up window and truncate them:
      s <- (event.info$.DATE.as.Date < event.info$.FU.START.DATE);
      if( length(s) > 0 )
      {
        event.info$.DATE.as.Date[s] <- event.info$.FU.START.DATE[s];
      }

      # Find all prescriptions that end after the follow-up window and truncate them:
      s <- (event.info$.DATE.as.Date.end > event.info$.FU.END.DATE);
      if( length(s) > 0 )
      {
        event.info[s,cma$event.duration.colname] <- .difftime.Dates.as.days(event.info$.FU.END.DATE[s], event.info$.DATE.as.Date[s]);
      }

      # Store the event.info data:
      cma$event.info <- event.info;
    } else
    {
      if( !suppress.warnings ) warning("Error(s) concerning the follow-up and observation windows: please see console for details!\n");
      return (invisible(NULL));
    }
  }

  # Add the follow-up and observation window info as well, to have everything in one place:
  if( !is.null(cmas) )
  {
    cmas <- cbind(cmas, do.call(rbind, lapply(1:nrow(cmas), function(i)
    {
      s <- which(cma$event.info[,cma$ID.colname] == cmas[i,cma$ID.colname]);
      if( length(s) == 0 ) return (NULL);
      cma$event.info[s[1],c(".FU.START.DATE", ".FU.END.DATE", ".OBS.START.DATE", ".OBS.END.DATE")];
    })));
  } else
  {
    # Create a fake one, contining but the follow-up and observation window info:
    cmas <- data.frame("..patid.."=unique(cma$data[,cma$ID.colname]), "CMA"=NA); names(cmas)[1] <- cma$ID.colname;
    if( !is.null(cma$event.info) )
    {
      cmas <- merge(cmas, unique(cma$event.info[,c(cma$ID.colname, ".FU.START.DATE", ".FU.END.DATE", ".OBS.START.DATE", ".OBS.END.DATE")]), by=c(cma$ID.colname), all.x=TRUE);
    } else
    {
      cmas <- cbind(cmas, ".FU.START.DATE"=NA, ".FU.END.DATE"=NA, ".OBS.START.DATE"=NA, ".OBS.END.DATE"=NA);
    }
  }

  # Make sure the dates are cached as `Date` objects:
  if( !inherits(cma$data[,cma$event.date.colname], "Date") )
  {
    if( is.na(cma$date.format) || is.null(cma$date.format) || length(cma$date.format) != 1 || !is.character(cma$date.format) )
    {
      if( !suppress.warnings ) warning(paste0("The date format must be a single string: cannot continue plotting!\n"));
      return (invisible(NULL));
    }

    # Convert them to Date:
    cma$data$.DATE.as.Date <- as.Date(cma$data[,cma$event.date.colname], format=cma$date.format);
    if( anyNA(cma$data$.DATE.as.Date) )
    {
      if( !suppress.warnings ) warning(paste0("Not all entries in the event date \"",cma$event.date.colname,"\" column are valid dates or conform to the date format \"",cma$date.format,"\"; first issue occurs on row ",min(which(is.na(cma$data$.DATE.as.Date))),": cannot continue plotting!\n"));
      return (invisible(NULL));
    }
  } else
  {
    # Just make a copy:
    cma$data$.DATE.as.Date <- cma$data[,cma$event.date.colname];
  }

  # Make sure the patients are ordered by ID and date:
  cma$data <- cma$data[ order( cma$data[,cma$ID.colname], cma$data$.DATE.as.Date), ];
  if( all(c("WND.ID","start") %in% names(cmas)) )
  {
    cmas <- cmas[ order( cmas[,cma$ID.colname], cmas$WND.ID, cmas$start), ];
  } else
  {
    cmas <- cmas[ order( cmas[,cma$ID.colname]), ];
  }


  ##
  ## Colors for plotting
  ##

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
      col.na <- "lightgray";
      col.continuation <- "black";
      print.dose.outline.col <- "white";
      plot.partial.CMAs.as.stacked.col.bars <- "gray90";
      plot.partial.CMAs.as.stacked.col.border <- "gray30";
      plot.partial.CMAs.as.stacked.col.text <- "black";
      plot.partial.CMAs.as.timeseries.col.dot <- "black";
      plot.partial.CMAs.as.timeseries.col.interval <- "gray70";
      plot.partial.CMAs.as.timeseries.col.text <- "black";
      plot.partial.CMAs.as.overlapping.col.interval <- "gray70";
      plot.partial.CMAs.as.overlapping.col.text <- "black";
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


  ##
  ## Doses
  ##

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


  ##
  ## Event dates and durations
  ##

  # Find the earliest date:
  earliest.date <- min(cma$data$.DATE.as.Date, if( "start" %in% names(cmas) ) cmas$start, cmas$.OBS.START.DATE, cmas$.FU.START.DATE, na.rm=TRUE);

  # If aligning all participants to the same date, simply relocate all dates relative to the earliest date:
  if( align.all.patients )
  {
    # ASSUMPTIONS: the data is sorted by patient ID and (ascending) by event date
    for( i in 1:nrow(cma$data) )
    {
      # For each event in the dataset:
      if( i == 1 || cma$data[i,cma$ID.colname] != cma$data[i-1,cma$ID.colname] )
      {
        # It's a new patient (or the first one):

        # We will align to the patient's first event:
        align.to <- cma$data$.DATE.as.Date[i];

        # Adjust the dates in the cmas as well:
        for( j in which(cmas[,cma$ID.colname] == cma$data[i,cma$ID.colname]) )
        {
          if( "start" %in% names(cmas) ) cmas$start[j] <- earliest.date + (cmas$start[j] - align.to);
          if( "end" %in% names(cmas) )   cmas$end[j]   <- earliest.date + (cmas$end[j]   - align.to);
          cmas$.FU.START.DATE[j]  <- earliest.date + (cmas$.FU.START.DATE[j]  - align.to);
          cmas$.FU.END.DATE[j]    <- earliest.date + (cmas$.FU.END.DATE[j]    - align.to);
          cmas$.OBS.START.DATE[j] <- earliest.date + (cmas$.OBS.START.DATE[j] - align.to);
          cmas$.OBS.END.DATE[j]   <- earliest.date + (cmas$.OBS.END.DATE[j]   - align.to);
        }
      }

      # Move the event so that it is properly aligned:
      cma$data$.DATE.as.Date[i] <- (earliest.date + (cma$data$.DATE.as.Date[i] - align.to));
    }

    # The corrected earliest follow-up window date:
    correct.earliest.followup.window <- min(cma$data$.DATE.as.Date - min(cmas$.FU.START.DATE,na.rm=TRUE),na.rm=TRUE);
  } else
  {
    # There is no correction to the earliest follow-up window date:
    correct.earliest.followup.window <- 0;
  }

  # Compute the duration if not given:
  if( is.na(duration) )
  {
    latest.date <- max(cmas$.FU.END.DATE, cmas$.OBS.END.DATE, cma$data$.DATE.as.Date + cma$data[,cma$event.duration.colname], na.rm=TRUE);
    if( "end" %in% names(cmas) ) latest.date <- max(cmas$end, latest.date, na.rm=TRUE);
    duration <- as.numeric(latest.date - earliest.date) + correct.earliest.followup.window;
  }
  endperiod <- duration;


  ##
  ## Reserve plotting space for various components
  ##

  # Reserve space for the CMA plotting:
  adh.plot.space <- c(0, ifelse( plot.CMA && has.estimated.CMA, duration*CMA.plot.ratio, 0) );
  duration.total <- duration + adh.plot.space[2];

  # Save the graphical params and restore them later:
  old.par <- par(no.readonly=TRUE);

  # Make sure there's enough space to actually plot the patient IDs on the y-axis:
  id.labels <- do.call(rbind,lapply(as.character(patids), # for each patient ID, compute the string dimensions in inches
                                    function(p)
                                    {
                                      # The participant axis text:
                                      s <- which(cma$event.info[,cma$ID.colname] == p);
                                      pid <- ifelse( print.CMA &&
                                                       !is.cma.TS.or.SW &&
                                                       has.estimated.CMA &&
                                                       length(x <- which(getCMA(cma)[cma$ID.colname] == p))==1,
                                                     paste0(p,"\n",sprintf("%.1f%%",getCMA(cma)[x,"CMA"]*100)),
                                                     p);
                                      data.frame("ID"=p,
                                                 "string"=pid,
                                                 "width"=strwidth(pid, units="inches", cex=cex.axis),
                                                 "height"=strheight(pid, units="inches", cex=cex.axis));
                                    }));

  y.label <- data.frame("string"=(tmp <- ifelse(is.null(ylab),"",
                                                ifelse(length(ylab)==1,
                                                       ylab,
                                                       ifelse((print.CMA || plot.CMA) &&
                                                                has.estimated.CMA,
                                                              ylab["withCMA"],
                                                              ylab["withoutCMA"])))), # space needed for the label (in inches)
                        "width"=strwidth(tmp, units="inches", cex=cex.lab),
                        "height"=strheight(tmp, units="inches", cex=cex.lab));

  left.margin <- (cur.mai <- par("mai"))[2]; # left margin in inches (and cache the current margins too)

  # If there's enough space as it is, don't do anything:
  if( left.margin < (y.label$height + max(id.labels$width,na.rm=TRUE) + strwidth("M", units="inches", cex=cex.lab)) ) # remember: y.label is vertical
  {
    # Well, there isn't enough space, so:
    rotate.id.labels <- 30; # rotate the labels (in degrees)
    new.left.margin <- (y.label$height + (cos(rotate.id.labels*pi/180) * max(id.labels$width,na.rm=TRUE)) + strwidth("0000", units="inches", cex=cex.axis)); # ask for enough space
    par(mai=c(cur.mai[1], new.left.margin, cur.mai[3], cur.mai[4]));
  } else
  {
    # Seems to fit, so don't do anything:
    rotate.id.labels <- 0;
  }

  # Vertical space needed for showing the partial CMAs:
  vert.space.cmas <- 0;
  if( is.cma.TS.or.SW )
  {
    # There actually is a partial CMA to be potentially plotted:
    if( ("timeseries" %in% plot.partial.CMAs.as) && (plot.partial.CMAs.as.timeseries.vspace < 5) )
    {
      warning(paste0("The minimum vertical space for the timeseries plots (plot.partial.CMAs.as.timeseries.vspace) is 5 lines, but it currently is only ",
                     plot.partial.CMAs.as.timeseries.vspace,
                     ": skipping timeseries plots...\n"));
      plot.partial.CMAs.as <- plot.partial.CMAs.as[ plot.partial.CMAs.as != "timeseries" ];
    }

    vert.space.cmas <- vert.space.cmas +
      ifelse(plot.CMA && has.estimated.CMA,
             (nrow(cmas)+length(patids)) * as.numeric("stacked" %in% plot.partial.CMAs.as) +
               3 * length(patids) * as.numeric("overlapping" %in% plot.partial.CMAs.as) +
               plot.partial.CMAs.as.timeseries.vspace * length(patids) * as.numeric("timeseries" %in% plot.partial.CMAs.as),
             0);
  }


  ##
  ## The actual plotting
  ##

  # Create the plotting surface:
  if(inherits(msg <- try(plot( 0, 1,
                               xlim=c(0-5,duration.total+5), # pad left and right by 5 days to improve plotting
                               xaxs="i",
                               ylim=c(0,nrow(cma$data)+vert.space.cmas+1),
                               yaxs="i",
                               type="n",
                               axes=FALSE,
                               xlab="",
                               ylab="" ),
                         silent=TRUE),
              "try-error"))
  {
    # Some error occured when creatig the plot...
    warning(msg);
    par(old.par); # restore graphical params
    return (invisible(NULL));
  }

  # Character width and height in the current plotting system:
  if( print.dose ) dose.text.height <- strheight("0",cex=cex.dose);
  char.width <- strwidth("O",cex=cex); char.height <- strheight("O",cex=cex);
  char.height.CMA <- strheight("0",cex=CMA.cex);

  # Minimum plot dimensions:
  if( abs(par("usr")[2] - par("usr")[1]) <= char.width * min.plot.size.in.characters.horiz ||
      abs(par("usr")[4] - par("usr")[3]) <= char.height * min.plot.size.in.characters.vert * (nrow(cma$data) + ifelse(is.cma.TS.or.SW && plot.CMA && has.estimated.CMA, nrow(cmas), 0)) )
  {
    warning(paste0("Plotting area is too small (it must be at least ",
                   min.plot.size.in.characters.horiz,
                   " x ",
                   min.plot.size.in.characters.vert,
                   " characters per patient, but now it is only ",
                   round(abs(par("usr")[2] - par("usr")[1]) / char.width,1),
                   " x ",
                   round(abs(par("usr")[4] - par("usr")[3]) / (char.height * (nrow(cma$data) + ifelse(is.cma.TS.or.SW && plot.CMA && has.estimated.CMA, nrow(cmas), 0))),1),
                   ")!\n"));
    par(old.par); # restore graphical params
    return (invisible(NULL));
  }

  # Continue plotting:
  box();


  ##
  ## Title & axis labels
  ##

  # Title & axis labels:
  title(main=paste0(ifelse(is.null(title),"",                                   # the plot title
                           ifelse(length(title)==1,
                                  title,
                                  ifelse(align.all.patients,
                                         title["aligned"],
                                         title["notaligned"]))),
                    ifelse(!is.null(title) && show.cma,
                           paste0(" ",
                                  switch(class(cma)[1],
                                         "CMA_sliding_window"=paste0("sliding window (",cma$computed.CMA,")"),
                                         "CMA_per_episode"=   paste0("per episode (",cma$computed.CMA,")"),
                                         class(cma)[1])
                                 ),
                           "")),
        xlab=ifelse(is.null(xlab),                                              # x axis label
                    "",
                    ifelse(length(xlab)==1,
                           xlab,
                           xlab[show.period])),
        cex.lab=cex.lab);

  # y-axis label:
  mtext(y.label$string, side=2, line=par("mar")[2]-1, at=(par("usr")[4] + par("usr")[3])/2, cex=cex.lab, las=3);

  # Function mapping the CMA values to the appropriate x-coordinates:
  if( plot.CMA && has.estimated.CMA )
  {
    adh.max <- ifelse(is.cma.TS.or.SW, 1.0, max(c(getCMA(cma)$CMA, 1.0),na.rm=TRUE)); # maximum achieved CMA (used for plotting, forced to 1.0 for PE and SW)
    .rescale.xcoord.for.CMA.plot <- function(x, pfree=0.20)
    {
      return (adh.plot.space[1] + (x / adh.max) * (adh.plot.space[2] * (1-pfree) - adh.plot.space[1]));
    }
  }


  ##
  ## Plot most of the plot components
  ##

  # Intialisations
  y.cur <- 1; # the current vertical line at which plotting takes place
  alternating.band.to.draw <- 1; # for this patient, which alternating band to draw?

  # For each individual event in turn:
  for( i in 1:nrow(cma$data) )
  {
    # The current patient ID:
    cur_pat_id <- cma$data[i,cma$ID.colname];

    # For a new patients, draw the alternating bands, show the CMA and print the y-axis label:
    if( i == 1 || (cur_pat_id != cma$data[i-1,cma$ID.colname]) )
    {
      # Save the current vertical position (for drawing the FUW and OW windows):
      y.old <- y.cur;

      # Select the events and partial CMAs belonging to this patient:
      s.events <- which(cma$data[,cma$ID.colname] == cur_pat_id);
      s.cmas   <- which(cmas[,cma$ID.colname]     == cur_pat_id);

      # Total vartical space neede by this patient:
      vspace.needed <- length(s.events) +
        ifelse(plot.CMA && has.estimated.CMA && adh.plot.space[2] > 0,
               (length(s.cmas)+1) * as.numeric("stacked" %in% plot.partial.CMAs.as) +
                 3 * as.numeric("overlapping" %in% plot.partial.CMAs.as) +
                 plot.partial.CMAs.as.timeseries.vspace * as.numeric("timeseries" %in% plot.partial.CMAs.as),
               0);


      ##
      ## The alternating bands
      ##

      # Draw the alternating bands
      if( !is.null(alternating.bands.cols) )
      {
        rect( 0.0 - 1.0, y.cur - 0.5, duration.total + 1.0, y.cur + vspace.needed - 0.5, col=alternating.bands.cols[alternating.band.to.draw], border=NA );
        alternating.band.to.draw <- if(alternating.band.to.draw >= length(alternating.bands.cols)) 1 else (alternating.band.to.draw + 1); # move to the next band
      }


      ##
      ## The y-axis labels
      ##

      # The y-axis label:
      pid <- cur_pat_id;
      y.mean <- y.cur + vspace.needed/2; # vertical position of the label (centered on patient)
      if( rotate.id.labels > 0 )
      {
        text(par("usr")[1], y.mean, pid, cex=cex.axis, srt=rotate.id.labels, pos=2, xpd=TRUE); # rotate the labels
      } else
      {
        mtext(pid, 2, line=0.5, at=y.mean, las=2, cex=cex.axis); # # don't rotate the labels
      }


      ##
      ## The summary CMA plots
      ##

      # The patient's CMA plot:
      if( plot.CMA && has.estimated.CMA && adh.plot.space[2] > 0 )
      {
        if( is.cma.TS.or.SW )
        {
          # For per episode and sliding windows we show the distribution of the "partial" CMAs:

          # The CMA plot background:
          segments(.rescale.xcoord.for.CMA.plot(0.0), y.mean - 2, .rescale.xcoord.for.CMA.plot(1.0), y.mean - 2, lty="solid", col=CMA.plot.col);
          segments(.rescale.xcoord.for.CMA.plot(0.0), y.mean + 2, .rescale.xcoord.for.CMA.plot(1.0), y.mean + 2, lty="solid", col=CMA.plot.col);

          # The non-missing CMA values:
          adh <- na.omit(cmas[s.cmas,"CMA"]);

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
              segments(.rescale.xcoord.for.CMA.plot(adh.x), y.mean - 2, .rescale.xcoord.for.CMA.plot(adh.x), y.mean - 2 + 4*adh.y, lty="solid", lwd=1, col=CMA.plot.border);
              if( char.height.CMA <= abs(.rescale.xcoord.for.CMA.plot(1.0) - .rescale.xcoord.for.CMA.plot(0.0)) )
              {
                # There's enough space for vertically writing all three of them:
                text(x=.rescale.xcoord.for.CMA.plot(0.0),       y.mean - 2 - char.height.CMA/2,
                     sprintf("%.1f%%",100*min(adh.x.0,na.rm=TRUE)), srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
                text(x=.rescale.xcoord.for.CMA.plot(1.0),       y.mean - 2 - char.height.CMA/2,
                     sprintf("%.1f%%",100*max(adh.x.1,na.rm=TRUE)), srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
                text(x=.rescale.xcoord.for.CMA.plot(adh.x.max), y.mean + 2 + char.height.CMA/2,
                     sprintf("%d",max(adh.hist$counts,an.rm=TRUE)), srt=90, pos=3, cex=CMA.cex, col=CMA.plot.text);
              }
            }
          } else
          {
            if( length(adh) > 2 )
            {
              # Plot CMA as density plot:
              adh.density <- density(adh);
              ss <- (adh.density$x >= min(adh,na.rm=TRUE) & adh.density$x <= max(adh,na.rm=TRUE));
              if( sum(ss) == 0 )
              {
                # Probably constant numbers?
                # Plot the individual lines:
                adh.x.0 <- min(adh,0); adh.x.1 <- max(adh,1); adh.x <- (adh - adh.x.0) / (adh.x.1 - adh.x.0);
                segments(.rescale.xcoord.for.CMA.plot(adh.x), y.mean - 2, .rescale.xcoord.for.CMA.plot(adh.x), y.mean - 2 + 4, lty="solid", lwd=2, col=CMA.plot.border);
                if( char.height.CMA*length(adh) <= abs(.rescale.xcoord.for.CMA.plot(1) - .rescale.xcoord.for.CMA.plot(0)) )
                {
                  # There's enough space for vertical writing all of them (alternated):
                  for( j in 1:length(adh) )
                  {
                    text(x=.rescale.xcoord.for.CMA.plot(adh.x[j]), y.mean + ifelse(j %% 2==0, 2 + char.height.CMA/2, -2 - char.height.CMA/2),
                         sprintf("%.1f%%",100*adh[j]), srt=90, pos=ifelse(j %% 2==0, 3, 1), cex=CMA.cex, col=CMA.plot.text);
                  }
                } else if( char.height.CMA <= abs(.rescale.xcoord.for.CMA.plot(1) - .rescale.xcoord.for.CMA.plot(0)) )
                {
                  # There's enough space for vertical writing only the extremes:
                  text(x=.rescale.xcoord.for.CMA.plot(adh.x[1]),           y.mean - 2 - char.height.CMA/2,
                       sprintf("%.1f%%",100*adh[1]),           srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
                  text(x=.rescale.xcoord.for.CMA.plot(adh.x[length(adh)]), y.mean - 2 - char.height.CMA/2,
                       sprintf("%.1f%%",100*adh[length(adh)]), srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
                }
              } else
              {
                adh.density$x <- adh.density$x[ss]; adh.density$y <- adh.density$y[ss];
                adh.x <- adh.density$x; adh.x.0 <- min(adh.x,0); adh.x.1 <- max(adh.x,1); adh.x <- (adh.x - adh.x.0) / (adh.x.1 - adh.x.0);
                adh.y <- adh.density$y; adh.y <- (adh.y - min(adh.y)) / (max(adh.y) - min(adh.y));
                points(.rescale.xcoord.for.CMA.plot(adh.x), y.mean - 2 + 4*adh.y, type="l", col=CMA.plot.border);
                if( char.height.CMA <= abs(.rescale.xcoord.for.CMA.plot(1) - .rescale.xcoord.for.CMA.plot(0)) )
                {
                  # There's enough space for vertical writing:
                  text(x=.rescale.xcoord.for.CMA.plot(0.0), y.mean - 2 - char.height.CMA/2, sprintf("%.1f%%",100*adh.x.0), srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
                  text(x=.rescale.xcoord.for.CMA.plot(1.0), y.mean - 2 - char.height.CMA/2, sprintf("%.1f%%",100*adh.x.1), srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
                }
              }
            } else
            {
              if( length(adh) == 0 )
              {
                # No points at all: nothing to plot!
              } else
              {
                # Plot the individual lines:
                adh.x.0 <- min(adh,0); adh.x.1 <- max(adh,1); adh.x <- (adh - adh.x.0) / (adh.x.1 - adh.x.0);
                segments(.rescale.xcoord.for.CMA.plot(adh.x), y.mean - 2, .rescale.xcoord.for.CMA.plot(adh.x), y.mean - 2 + 4, lty="solid", lwd=2, col=CMA.plot.border);
                if( char.height.CMA*length(adh) <= abs(.rescale.xcoord.for.CMA.plot(1) - .rescale.xcoord.for.CMA.plot(0)) )
                {
                  # There's enough space for vertical writing all of them (alternating):
                  for( j in 1:length(adh) )
                  {
                    text(x=.rescale.xcoord.for.CMA.plot(adh.x[j]), y.mean + ifelse(j %% 2==0, 2 + char.height.CMA/2, -2 - char.height.CMA/2),
                         sprintf("%.1f%%",100*adh[j]), srt=90, pos=ifelse(j %% 2==0, 3, 1), cex=CMA.cex, col=CMA.plot.text);
                  }
                } else if( char.height.CMA <= abs(.rescale.xcoord.for.CMA.plot(1) - .rescale.xcoord.for.CMA.plot(0)) )
                {
                  # enough space for vertical writing only the extremes:
                  text(x=.rescale.xcoord.for.CMA.plot(adh.x[1]),           y.mean - 2 - char.height.CMA/2,
                       sprintf("%.1f%%",100*adh[1]),           srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
                  text(x=.rescale.xcoord.for.CMA.plot(adh.x[length(adh)]), y.mean - 2 - char.height.CMA/2,
                       sprintf("%.1f%%",100*adh[length(adh)]), srt=90, pos=1, cex=CMA.cex, col=CMA.plot.text);
                }
              }
            }
          }
        } else if( inherits(cma, "CMA1") )
        {
          # For CMA1+ we show the actual point estimate:

          # The adherence estimate:
          adh <- cmas[s.cmas,"CMA"];

          # Draw the background rectangle:
          rect(.rescale.xcoord.for.CMA.plot(0.0), mean(s.events) - 1, .rescale.xcoord.for.CMA.plot(min(adh,adh.max)), mean(s.events) + 1, col=CMA.plot.col, border=NA);
          rect(.rescale.xcoord.for.CMA.plot(0.0), mean(s.events) - 1, .rescale.xcoord.for.CMA.plot(max(1.0,adh.max)), mean(s.events) + 1, col=NA, border=CMA.plot.border);

          if( !is.na(adh) )
          {
            cma.string <- sprintf("%.1f%%",adh*100);
            available.x.space <- abs(.rescale.xcoord.for.CMA.plot(max(1.0,adh.max)) - .rescale.xcoord.for.CMA.plot(0.0));

            if( strwidth(cma.string, cex=CMA.cex) <= available.x.space )
            { # horizontal writing of the CMA:
              text(x=(.rescale.xcoord.for.CMA.plot(0.0) + .rescale.xcoord.for.CMA.plot(max(1.0,adh.max)))/2, y=mean(s.events),
                   labels=cma.string, col=CMA.plot.text, cex=CMA.cex);
            } else if( strheight(cma.string, cex=CMA.cex) <= available.x.space )
            { # vertical writing of the CMA:
              text(x=(.rescale.xcoord.for.CMA.plot(0.0) + .rescale.xcoord.for.CMA.plot(max(1.0,adh.max)))/2, y=mean(s.events),
                   labels=cma.string, col=CMA.plot.text, cex=CMA.cex, srt=90);
            } # otherwise, theres' no space for showing the CMA here
          }
        }
      }
    }

    ##
    ## The event
    ##

    # Get the event start and end dates:
    start <- as.numeric(cma$data$.DATE.as.Date[i] - earliest.date);
    end   <- start + cma$data[i,cma$event.duration.colname];

    # Map medication classes to colors:
    if( is.na(cma$medication.class.colname) || !(cma$medication.class.colname %in% names(cma$data)) )
    {
      col <- .map.category.to.color(unspecified.category.label);
    } else
    {
      col <- .map.category.to.color(cma$data[i,cma$medication.class.colname]);
    }

    # Plot the bening and end of the event:
    points(adh.plot.space[2] + start + correct.earliest.followup.window, y.cur, pch=pch.start.event, col=col, cex=cex);
    points(adh.plot.space[2] + end   + correct.earliest.followup.window, y.cur, pch=pch.end.event,   col=col, cex=cex);

    # Show event intervals as rectangles?
    if( show.event.intervals && !is.null(cma$event.info) && !is.na(cma$event.info$event.interval[i]) )
    {
      # The end of the prescription:
      end.pi <- start + cma$event.info$event.interval[i] - cma$event.info$gap.days[i];

      # Plot it:
      rect(adh.plot.space[2] + start  + correct.earliest.followup.window, i - char.height/2,
           adh.plot.space[2] + end.pi + correct.earliest.followup.window, i + char.height/2,
           col=adjustcolor(col,alpha.f=0.2), border=col);
      if( cma$event.info$gap.days[i] > 0 )
        rect(adh.plot.space[2] + end.pi + correct.earliest.followup.window, i - char.height/2,
             adh.plot.space[2] + end.pi + cma$event.info$gap.days[i] + correct.earliest.followup.window, i + char.height/2,
             density=25, col=adjustcolor(col,alpha.f=0.5), border=col);
    }

    # Do we show dose?
    if( plot.dose )
    {
      # Show dose using event line width:
      if( nrow(dose.range) == 1 )
      {
        # Just one dose:
        segments( adh.plot.space[2] + start + correct.earliest.followup.window, y.cur,
                  adh.plot.space[2] + end   + correct.earliest.followup.window, y.cur,
                  col=col, lty=lty.event,
                  lwd=adjust.dose.lwd(cma$data[i,cma$event.daily.dose.colname]));
      } else
      {
        # There is a range of doses:
        if( plot.dose.lwd.across.medication.classes )
        {
          # Line width across all medication classes:
          segments( adh.plot.space[2] + start + correct.earliest.followup.window, y.cur,
                    adh.plot.space[2] + end   + correct.earliest.followup.window, y.cur,
                    col=col, lty=lty.event,
                    lwd=adjust.dose.lwd(cma$data[i,cma$event.daily.dose.colname], dose.min=dose.range.global$min, dose.max=dose.range.global$max));
        } else
        {
          # Line width per medication class:
          dose.for.cat <- (dose.range$category == cma$data[i,cma$medication.class.colname]);
          if( sum(dose.for.cat,na.rm=TRUE) == 1 )
          {
            # Found the corresponding medication class:
            segments( adh.plot.space[2] + start + correct.earliest.followup.window, y.cur,
                      adh.plot.space[2] + end   + correct.earliest.followup.window, y.cur,
                      col=col, lty=lty.event,
                      lwd=adjust.dose.lwd(cma$data[i,cma$event.daily.dose.colname], dose.min=dose.range$min[dose.for.cat], dose.max=dose.range$max[dose.for.cat]));
          } else
          {
            # Use a fixed width:
            segments( adh.plot.space[2] + start + correct.earliest.followup.window, y.cur,
                      adh.plot.space[2] + end   + correct.earliest.followup.window, y.cur,
                      col=col, lty=lty.event, lwd=lwd.event);
          }
        }
      }
    } else
    {
      # Use a fixed line width:
      segments( adh.plot.space[2] + start + correct.earliest.followup.window, y.cur,
                adh.plot.space[2] + end   + correct.earliest.followup.window,
                y.cur, col=col, lty=lty.event, lwd=lwd.event);
    }

    if( print.dose )
    {
      # Show dose as actual numbers on the plot:
      dose.text.y <- (y.cur - ifelse(print.dose.centered, 0, dose.text.height*2/3)); # print it on or below the dose segment?

      if( is.na(print.dose.outline.col) ) # simple or outlined?
      {
        # Simple text:
        text(adh.plot.space[2] + (start + end)/2 + correct.earliest.followup.window,
             dose.text.y,
             cma$data[i,cma$event.daily.dose.colname], cex=cex.dose, col=col);
      } else
      {
        # Outlined text:
        .shadow.text(adh.plot.space[2] + (start + end)/2 + correct.earliest.followup.window,
                     dose.text.y,
                     cma$data[i,cma$event.daily.dose.colname], cex=cex.dose, col=col, bg=print.dose.outline.col);
      }
    }

    # Advance to the next vertical line:
    y.cur <- y.cur + 1;


    # Continuation between successive events:
    if( i < nrow(cma$data) && (cur_pat_id == cma$data[i+1,cma$ID.colname]) )
    {
      # We're still plotting the same patient: show the continuation line:
      start.next <- as.numeric(cma$data$.DATE.as.Date[i+1] - earliest.date);
      segments( adh.plot.space[2] + end        + correct.earliest.followup.window, y.cur-1,
                adh.plot.space[2] + start.next + correct.earliest.followup.window, y.cur-1,
                col=col.continuation, lty=lty.continuation, lwd=lwd.continuation);
      segments( adh.plot.space[2] + start.next + correct.earliest.followup.window, y.cur-1,
                adh.plot.space[2] + start.next + correct.earliest.followup.window, y.cur,
                col=col.continuation, lty=lty.continuation, lwd=lwd.continuation);
    } else
    {
      # The patient is changing or is the last one:


      ##
      ## Partial CMAs
      ##

      # Draw its subperiods (if so requested, meaningful and possible):
      if( is.cma.TS.or.SW && plot.CMA && has.estimated.CMA && adh.plot.space[2] > 0 )
      {
        if( length(s.cmas) > 0 && !all(is.na(cmas$CMA[s.cmas])) )
        {
          # We do have non-missing partial CMAs to plot:

          # Compute the start, end, location and string to display for these partial estimates:
          ppts <- data.frame("start"=as.numeric(cmas$start[s.cmas] - earliest.date),
                             "end"  =as.numeric(cmas$end[s.cmas]   - earliest.date),
                             "x"    =NA,
                             "y"    =cmas$CMA[s.cmas],
                             "text" =ifelse(!is.na(cmas$CMA[s.cmas]), sprintf("%.0f%%", 100*cmas$CMA[s.cmas]), "?")
                            );
          ppts$x <- (ppts$start + ppts$end)/2;

          # Cache stuff:
          corrected.x <- (adh.plot.space[2] + correct.earliest.followup.window);
          corrected.x.start <- (corrected.x+ppts$start);
          corrected.x.end   <- (corrected.x+ppts$end);
          x.start.min <- min(ppts$start,na.rm=TRUE);
          x.end.max   <- max(ppts$end,  na.rm=TRUE);
          corrected.x.text <- (corrected.x + ppts$x);
          min.y <- min(ppts$y,na.rm=TRUE);
          max.y <- max(ppts$y,na.rm=TRUE);

          # Plotting type:
          if( "stacked" %in% plot.partial.CMAs.as )
          {
            # Show subperiods as stacked:
            ys <- (y.cur + 1:nrow(ppts) - 1); # cache this

            # The intervals as empty rectangles:
            rect(corrected.x.start, ys + 0.10, corrected.x.end,   ys + 0.90, border=gray(0.7), col="white");

            # The CMAs as filled rectangles of length proportional to the CMA:
            h <- (ppts$end - ppts$start) * pmax(pmin(ppts$y, 1.0), 0.0);
            rect(corrected.x.start, ys + 0.10, corrected.x.start + h, ys + 0.90, border=plot.partial.CMAs.as.stacked.col.border, col=plot.partial.CMAs.as.stacked.col.bars);

            if( print.CMA && char.height.CMA <= 0.80 )
            {
              text(corrected.x.text, ys + 0.5, ppts$text, cex=CMA.cex, col=plot.partial.CMAs.as.stacked.col.text);
            }

            # Advance to next patient:
            y.cur <- (y.cur + nrow(ppts) + 1);
          }

          if( "overlapping" %in% plot.partial.CMAs.as )
          {
            # Show subperiods as overlapping segments:
            if( !((range.y <- (max.y - min.y)) > 0) ) range.y <- 1; # avoid division by 0 if there's only one value
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
              y.norm.v <- (ppts$y.norm * -(v*2-1)); # -(v*2-1) maps 0 to 1 and 1 to -1

              segments(corrected.x.start, y.cur + 0.5 + v, corrected.x.end,   y.cur + 0.5 + v, col=plot.partial.CMAs.as.overlapping.col.interval);
              segments(corrected.x.start, y.cur + 0.5 + v, corrected.x.start, y.cur + 0.5 + v + y.norm.v, col=plot.partial.CMAs.as.overlapping.col.interval);
              segments(corrected.x.end,   y.cur + 0.5 + v, corrected.x.end,   y.cur + 0.5 + v + y.norm.v, col=plot.partial.CMAs.as.overlapping.col.interval);
            }

            if( print.CMA && char.height.CMA <= 0.80 && !is.na(plot.partial.CMAs.as.overlapping.col.text) )
            {
              text(corrected.x.text, y.cur + 1.0, ppts$text, cex=CMA.cex, col=plot.partial.CMAs.as.overlapping.col.text);
            }

            # Advance to next patient:
            y.cur <- y.cur+3;
          }

          if( "timeseries" %in% plot.partial.CMAs.as )
          {
            # Show subperiods as a time series
            if( plot.partial.CMAs.as.timeseries.start.from.zero ) min.y <- min(min.y,0,na.rm=TRUE);

            if( !((range.y <- (max.y - min.y)) > 0) ) range.y <- 1; # avoid division by 0 if there's only one value
            ppts$y.norm <- (y.cur + 1 + (plot.partial.CMAs.as.timeseries.vspace - 3) * (ppts$y - min.y)/range.y);

            # The intervals:
            if( !is.na(plot.partial.CMAs.as.timeseries.col.interval) )
            {
              if( plot.partial.CMAs.as.timeseries.interval.type == "none" )
              {
                # Nothing to plot
              } else if( plot.partial.CMAs.as.timeseries.interval.type %in% c("segments", "arrows", "lines") )
              {
                # The lines:
                segments(corrected.x.start, ppts$y.norm, corrected.x.end, ppts$y.norm, col=plot.partial.CMAs.as.timeseries.col.interval, lwd=plot.partial.CMAs.as.timeseries.lwd.interval);
                if( plot.partial.CMAs.as.timeseries.interval.type == "segments" )
                {
                  # The segment endings:
                  segments(corrected.x.start, ppts$y.norm - 0.2, corrected.x.start, ppts$y.norm + 0.2,
                           col=plot.partial.CMAs.as.timeseries.col.interval, lwd=plot.partial.CMAs.as.timeseries.lwd.interval);
                  segments(corrected.x.end,   ppts$y.norm - 0.2, corrected.x.end,   ppts$y.norm + 0.2,
                           col=plot.partial.CMAs.as.timeseries.col.interval, lwd=plot.partial.CMAs.as.timeseries.lwd.interval);
                } else if( plot.partial.CMAs.as.timeseries.interval.type == "arrows" )
                {
                  # The arrow endings:
                  segments(corrected.x.start + char.width/2, ppts$y.norm - char.height/2, corrected.x.start, ppts$y.norm,
                           col=plot.partial.CMAs.as.timeseries.col.interval, lwd=plot.partial.CMAs.as.timeseries.lwd.interval);
                  segments(corrected.x.start + char.width/2, ppts$y.norm + char.height/2, corrected.x.start, ppts$y.norm,
                           col=plot.partial.CMAs.as.timeseries.col.interval, lwd=plot.partial.CMAs.as.timeseries.lwd.interval);
                  segments(corrected.x.end - char.width/2, ppts$y.norm - char.height/2, corrected.x.end, ppts$y.norm,
                           col=plot.partial.CMAs.as.timeseries.col.interval, lwd=plot.partial.CMAs.as.timeseries.lwd.interval);
                  segments(corrected.x.end - char.width/2, ppts$y.norm + char.height/2, corrected.x.end, ppts$y.norm,
                           col=plot.partial.CMAs.as.timeseries.col.interval, lwd=plot.partial.CMAs.as.timeseries.lwd.interval);
                }
              } else if( plot.partial.CMAs.as.timeseries.interval.type == "rectangles" )
              {
                # As semi-transparent rectangles:
                rect(corrected.x.start, y.cur + 0.5, corrected.x.end, y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0,
                     col=scales::alpha(plot.partial.CMAs.as.timeseries.col.interval, alpha=plot.partial.CMAs.as.timeseries.alpha.interval),
                     border=plot.partial.CMAs.as.timeseries.col.interval, lty="dotted");
              }
            }

            # The axes:
            min.y.norm <- min(ppts$y.norm,na.rm=TRUE);
            max.y.norm <- max(ppts$y.norm,na.rm=TRUE);
            segments(corrected.x + x.start.min, y.cur + 0.5, corrected.x + x.end.max,   y.cur + 0.5, lty="solid", col="black"); # horizontal axis
            segments(corrected.x + x.start.min, y.cur + 0.5, corrected.x + x.start.min, y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0, lty="solid", col="black"); # vertical axis
            segments(corrected.x + x.start.min, min.y.norm, corrected.x + x.end.max, min.y.norm, lty="dashed", col="black"); # the minimum value
            segments(corrected.x + x.start.min, max.y.norm, corrected.x + x.end.max, max.y.norm, lty="dashed", col="black"); # the minimum value
            if( plot.partial.CMAs.as.timeseries.show.0perc &&
                (y.for.0perc <- (y.cur + 1 + (plot.partial.CMAs.as.timeseries.vspace-3) * (0 - min.y)/range.y)) >= y.cur + 0.5 )
            {
              segments(corrected.x + x.start.min, y.for.0perc, corrected.x + x.end.max, y.for.0perc, lty="dotted", col="red"); # 0%
            }
            if( plot.partial.CMAs.as.timeseries.show.100perc &&
                (y.for.100perc <- (y.cur + 1 + (plot.partial.CMAs.as.timeseries.vspace-3) * (1.0 - min.y)/range.y)) <= y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0 )
            {
              segments(corrected.x + x.start.min, y.for.100perc, corrected.x + x.end.max, y.for.100perc, lty="dotted", col="red"); # 0%
            }
            if( print.CMA && char.height.CMA <= 0.80 )
            {
              text(corrected.x + x.start.min, min.y.norm, sprintf("%.1f%%",100*min.y), pos=2, cex=CMA.cex, col="black");
              text(corrected.x + x.start.min, max.y.norm, sprintf("%.1f%%",100*max.y), pos=2, cex=CMA.cex, col="black");
              if( plot.partial.CMAs.as.timeseries.show.0perc && y.for.0perc >= y.cur + 0.5 )
              {
                text(corrected.x + x.start.min, y.for.0perc, "0%", pos=2, cex=CMA.cex, col="red");
              }
              if( plot.partial.CMAs.as.timeseries.show.100perc && y.for.100perc <= y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0 )
              {
                text(corrected.x + x.start.min, y.for.100perc, "100%", pos=2, cex=CMA.cex, col="red");
              }
            }

            # The points and connecting lines:
            if( !is.na(plot.partial.CMAs.as.timeseries.col.dot) )
            {
              points(corrected.x.text, ppts$y.norm, col=plot.partial.CMAs.as.timeseries.col.dot, cex=CMA.cex, type="o", pch=19, lty="solid");
            }

            # The actual values:
            if( print.CMA && char.height.CMA <= 0.80 && !is.na(plot.partial.CMAs.as.timeseries.col.text) )
            {
              text(corrected.x.text, ppts$y.norm, ppts$text, adj=c(0.5,-0.5), cex=CMA.cex, col=plot.partial.CMAs.as.timeseries.col.text);
            }

            # Advance to next patient:
            y.cur <- y.cur + plot.partial.CMAs.as.timeseries.vspace;
          }
        }
      }


      ##
      ## FUW and OW
      ##

      # The follow-up and observation windows (these are drawn only after all the other stuff for this patient has been drawn):
      if( highlight.followup.window )
      {
        rect(adh.plot.space[2] + as.numeric(cmas$.FU.START.DATE[s.cmas[1]] - earliest.date) + correct.earliest.followup.window, y.old - 0.5,
             adh.plot.space[2] + as.numeric(cmas$.FU.END.DATE[s.cmas[1]]   - earliest.date) + correct.earliest.followup.window, y.old + length(s.events) - 0.5,
             col=NA, border=followup.window.col, lty="dashed", lwd=2);
      }
      if( highlight.observation.window )
      {
        # The "given" OW:
        rect(adh.plot.space[2] + as.numeric(cmas$.OBS.START.DATE[s.cmas[1]] - earliest.date) + correct.earliest.followup.window, y.old - 0.5,
             adh.plot.space[2] + as.numeric(cmas$.OBS.END.DATE[s.cmas[1]]   - earliest.date) + correct.earliest.followup.window, y.old + length(s.events) - 0.5,
             col=adjustcolor(observation.window.col,alpha.f=observation.window.opacity), border=NA, density=observation.window.density, angle=observation.window.angle);

        if( inherits(cma,"CMA8") && !is.null(cma$real.obs.window) && show.real.obs.window.start )
        {
          # For CMA8, the OW might have been changed, so we also have a "real" OW:
          s.realOW <- which(cma$real.obs.window[,cma$ID.colname] == cur_pat_id);

          # Find the begining of the "real" OW:
          if( length(s.realOW) == 1)
          {
            if( !is.null(cma$real.obs.windows$window.start) && !is.na(cma$real.obs.windows$window.start[s.realOW]) )
            {
              real.obs.window.start <- cma$real.obs.windows$window.start[s.realOW];
            } else
            {
              real.obs.window.start <- cma$event.info$.OBS.START.DATE[s.events[1]];
            }
            if( !is.null(cma$real.obs.windows$window.end) && !is.na(cma$real.obs.windows$window.end[s.realOW]) )
            {
              real.obs.window.end <- cma$real.obs.windows$window.end[s.realOW];
            } else
            {
              real.obs.window.end <- cma$event.info$.OBS.END.DATE[s.events[1]];
            }

            # Draw the "real" OW:
            rect(adh.plot.space[2] + as.numeric(real.obs.window.start - earliest.date) + correct.earliest.followup.window, y.old - 0.5,
                 adh.plot.space[2] + as.numeric(real.obs.window.end   - earliest.date) + correct.earliest.followup.window, y.old + length(s.events) - 0.5,
                 col=adjustcolor(observation.window.col,alpha.f=observation.window.opacity), border=NA, density=real.obs.window.density, angle=real.obs.window.angle);
          }
        }
      }
    }
  }


  ##
  ## Separator between CMA and event plotting areas
  ##

  # Mark the drawing area for the CMAs:
  if( plot.CMA && has.estimated.CMA && adh.plot.space[2] > 0 )
  {
    if( is.cma.TS.or.SW )
    {
      abline(v=c(.rescale.xcoord.for.CMA.plot(0.0), .rescale.xcoord.for.CMA.plot(1.0)), col=CMA.plot.col, lty=c("solid","dotted"), lwd=1);
    } else
    {
      if( adh.max > 1.0 )
      {
        rect(.rescale.xcoord.for.CMA.plot(0.0), par("usr")[3], .rescale.xcoord.for.CMA.plot(adh.max), par("usr")[4], col=adjustcolor(CMA.plot.bkg,alpha.f=0.25), border=NA);
        abline(v=c(.rescale.xcoord.for.CMA.plot(0.0), .rescale.xcoord.for.CMA.plot(1.0), .rescale.xcoord.for.CMA.plot(adh.max)), col=CMA.plot.border, lty=c("solid","dotted","solid"), lwd=1);
        mtext( c("0%",sprintf("%.1f%%",adh.max*100)), 3, line=0.5, at=c(.rescale.xcoord.for.CMA.plot(0), .rescale.xcoord.for.CMA.plot(adh.max)), las=2, cex=cex.axis, col=CMA.plot.border );
        if( (.rescale.xcoord.for.CMA.plot(adh.max) - .rescale.xcoord.for.CMA.plot(1.0)) > 1.5*strwidth("0", cex=cex.axis) ) # Don't overcrowd the 100% and maximum CMA by omitting 100%
        {
          mtext( c("100%"), 3, line=0.5, at=c(.rescale.xcoord.for.CMA.plot(1.0)), las=2, cex=cex.axis, col=CMA.plot.border );
        }
      } else
      {
        rect(.rescale.xcoord.for.CMA.plot(0), par("usr")[3], .rescale.xcoord.for.CMA.plot(1.0), par("usr")[4], col=adjustcolor(CMA.plot.bkg,alpha.f=0.25), border=NA);
        abline(v=c(.rescale.xcoord.for.CMA.plot(0), .rescale.xcoord.for.CMA.plot(1.0)), col=CMA.plot.border, lty="solid", lwd=1);
        mtext( c("0%","100%"), 3, line=0.5, at=c(.rescale.xcoord.for.CMA.plot(0), .rescale.xcoord.for.CMA.plot(1.0)), las=2, cex=cex.axis, col=CMA.plot.border );
      }
    }
  }


  ##
  ## The x-axis
  ##

  # The x-axis and vertical guides:
  if( period.in.days > 0 )
  {
    if( show.period=="dates" )
    {
      xpos <- seq(0, as.numeric(endperiod), by=period.in.days); # where to put lables and guidelines
      axis.labels <- as.character(earliest.date + round(xpos, 1), format=cma$date.format);
    } else
    {
        if( align.first.event.at.zero )
        {
            xpos <- c(correct.earliest.followup.window - seq(0, as.numeric(correct.earliest.followup.window), by=period.in.days),
                      seq(0, as.numeric(endperiod), by=period.in.days) + correct.earliest.followup.window);
            xpos <- xpos[ xpos >= 0 & xpos <= endperiod ];
            axis.labels <- as.character(round(xpos - correct.earliest.followup.window, 1));
        } else
        {
          xpos <- seq(0, as.numeric(endperiod), by=period.in.days);
          axis.labels <- as.character(round(xpos, 1));
        }
    }

    axis( 1, at=adh.plot.space[2] + xpos, labels=FALSE);
    text(adh.plot.space[2] + xpos, par("usr")[3], labels=axis.labels, cex=cex.axis, srt=30, adj=c(1,3), xpd=TRUE);
    abline( v=adh.plot.space[2] + xpos,       lty="dotted", col=gray(0.5) );
    abline( v=adh.plot.space[2] + endperiod,  lty="solid",  col=gray(0.5) );
  }


  ##
  ## The legend
  ##

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

    if( !plot.dose )
    {
      if( do.plot ) text(x + 5.0*legend.char.width, cur.y, "duration", col="black", cex=legend.cex, pos=4);
      cur.y <- cur.y - 1.5*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("duration", cex=legend.cex));
    } else
    {
      if( do.plot ) text(x + 5.0*legend.char.width, cur.y, "duration (min. dose)", col="black", cex=legend.cex, pos=4);
      cur.y <- cur.y - 1.5*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("duration (min. dose)", cex=legend.cex));
      if( do.plot ) segments(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y, lty=lty.event, lwd=lwd.event.max.dose, col="black");
      if( do.plot ) points(x + 1.0*legend.char.width, cur.y, pch=pch.start.event, cex=legend.cex, col="black");
      if( do.plot ) points(x + 4.0*legend.char.width, cur.y, pch=pch.end.event, cex=legend.cex, col="black");
      if( do.plot ) text(x + 5.0*legend.char.width, cur.y, "duration (max. dose)", col="black", cex=legend.cex, pos=4);
      cur.y <- cur.y - 1.5*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("duration (max. dose)", cex=legend.cex));
    }

    # No event:
    if( do.plot ) segments(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y, lty=lty.continuation, lwd=lwd.continuation, col=col.continuation);
    if( do.plot ) text(x + 5.0*legend.char.width, cur.y, "no event/connector", col="black", cex=legend.cex, pos=4);
    cur.y <- cur.y - 1.5*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("no event/connector", cex=legend.cex));

    # Event intervals:
    if( show.event.intervals )
    {
      if( do.plot ) rect(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y - 1.0*legend.char.height, border="black", col=adjustcolor("black",alpha.f=0.5));
      if( do.plot ) text(x + 5.0*legend.char.width, cur.y - 0.5*legend.char.height, "days covered", col="black", cex=legend.cex, pos=4);
      cur.y <- cur.y - 1.5*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("days covered", cex=legend.cex));
      if( do.plot ) rect(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y - 1.0*legend.char.height, border="black", col="black", density=25);
      if( do.plot ) text(x + 5.0*legend.char.width, cur.y - 0.5*legend.char.height, "gap days", col="black", cex=legend.cex, pos=4);
      cur.y <- cur.y - 2.0*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("gap days", cex=legend.cex));
    }

    # medication classes:
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
      if( inherits(cma,"CMA8") && !is.null(cma$real.obs.windows) && show.real.obs.window.start )
      {
        # CMA8 also has a "real" OW:
        if( do.plot ) rect(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y - 1.0*legend.char.height, border=rgb(1,1,1,0.0), col=adjustcolor(observation.window.col,alpha.f=observation.window.opacity), density=observation.window.density, angle=observation.window.angle);
        if( do.plot ) text(x + 5.0*legend.char.width, cur.y - 0.5*legend.char.height, "theor. obs. wnd.", col="black", cex=legend.cex, pos=4);
        cur.y <- cur.y - 1.5*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("theor. obs. wnd.", cex=legend.cex));
        if( do.plot ) rect(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y - 1.0*legend.char.height, border=rgb(1,1,1,0.0), col=adjustcolor(observation.window.col,alpha.f=observation.window.opacity), density=real.obs.window.density, angle=real.obs.window.angle);
        if( do.plot ) text(x + 5.0*legend.char.width, cur.y - 0.5*legend.char.height, "real obs.wnd.", col="black", cex=legend.cex, pos=4);
        cur.y <- cur.y - 2.0*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("real obs.wnd.", cex=legend.cex));
      } else
      {
        if( do.plot ) rect(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y - 1.0*legend.char.height, border=rgb(1,1,1,0.0), col=adjustcolor(observation.window.col,alpha.f=observation.window.opacity), density=observation.window.density, angle=observation.window.angle);
        if( do.plot ) text(x + 5.0*legend.char.width, cur.y - 0.5*legend.char.height, "observation wnd.", col="black", cex=legend.cex, pos=4);
        cur.y <- cur.y - 2.0*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("observation wnd.", cex=legend.cex));
      }
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












