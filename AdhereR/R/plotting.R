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
                      id=NA, class=NA, comment=NA, tooltip=NA,  # ID, comment and tooltip
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

  if(!is.na(tooltip)) tooltip <- .SVG.specialchars.2.XMLentities(tooltip); # make sure special chars in tooltip are treated correctly

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

          # Close the element (and add optional tooltip):
          if(!is.na(tooltip)) c('>',' <title>', tooltip, '</title>', '</rect>') else '></rect>', # the tooltip title must be first child

          # Add ending newline (if so required):
          if(newline) '\n'
        );
  if( return_string ) return (paste0(r,collapse="")) else return (r);
}

.SVG.lines <- function(x, y,  # the coordinates of the points (at least 2)
                       connected=FALSE, # are the lines connected or not?
                       stroke=NA, stroke_width=NA, lty=NA, stroke_dasharray=NA, other_params=NA, # styling attributes (may be one per line for connected==FALSE)
                       id=NA, class=NA, comment=NA, tooltip=NA,  # ID, comment and tooltip
                       newline=TRUE, # should a newline be added at the end?
                       return_string=FALSE, # return a singe string or a vector of strings to be concatenated later?
                       suppress.warnings=FALSE
)
{
  # Preconditions:
  if( length(x) != length(y) || length(x) < 2 || length(y) < 2 )
  {
    if( !suppress.warnings ) .report.ewms("The line point coodinates must be of the same length >= 2.\n", "error", ".SVG.lines", "AdhereR");
    if( return_string ) return ("") else return (NULL);
  }

  if(!is.na(tooltip)) tooltip <- .SVG.specialchars.2.XMLentities(tooltip); # make sure special chars in tooltip are treated correctly

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

           # Close the element (and add optional tooltip):
           if(!is.na(tooltip)) c('>',' <title>', tooltip, '</title>', '</polyline>') else '></polyline>', # the tooltip title must be first child

           # Add ending newline (if so required):
           if(newline) '\n'
    );
  } else
  {
    # Multiple 'line' elements:
    if( length(x) %% 2 != 0 )
    {
      if( !suppress.warnings ) .report.ewms("For unconnected lines there must an even number of point coordinates.\n", "error", ".SVG.lines", "AdhereR");
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

             # The id and class (if any):
             if(!is.na(id)) c('id="',id,'" '),
             if(!is.na(class)) c('class="',class,'" '),

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

             # Close the element (and add optional tooltip):
             if(!is.na(tooltip)) c('>',' <title>', tooltip, '</title>', '</line>') else '></line>', # the tooltip title must be first child

             # Add ending newline (if so required):
             if(newline) '\n'
      );
    }
  }

  if( return_string ) return (paste0(r,collapse="")) else return (r);
}

.SVG.points <- function(x, y, pch=0,
                        col="black", cex=1.0, other_params=NA, # styling attributes
                        id=NA, class=NA, comment=NA, tooltip=NA,  # ID, comment and tooltip
                        newline=TRUE, # should a newline be added at the end?
                        return_string=FALSE, # return a singe string or a vector of strings to be concatenated later?
                        suppress.warnings=FALSE
)
{
  # Preconditions:
  if( length(x) != length(y) || length(x) == 0 )
  {
    if( !suppress.warnings ) .report.ewms("There must be at least on point.\n", "error", ".SVG.points", "AdhereR");
    return (NULL);
  }

  if(!is.na(tooltip)) tooltip <- .SVG.specialchars.2.XMLentities(tooltip); # make sure special chars in tooltip are treated correctly

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

            # Add optional tooltip:
            if(!is.na(tooltip)) c(' <title>', tooltip, '</title>'), # the tooltip title must be first child

            # Reuse the predefined symbol:
            '<use xlink:href="#pch',pch[i],'" ',

            # The coordinates and size:
            'transform="translate(',.SVG.number(x[i]),' ',.SVG.number(y[i]),') scale(',cex[i],')" ',

            # Aesthetics:
            if(!is.na(col[i])) c('stroke="', .SVG.color(col[i]), '" ', 'fill="', .SVG.color(col[i]), '" '),
            # Other parameters:
            if(!is.na(other_params)) other_params,

            # Close the element:
            '></use></g>',

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
                      id=NA, class=NA, comment=NA, tooltip=NA,  # ID, comment and tooltip
                      newline=TRUE, # should a newline be added at the end?
                      return_string=FALSE, # return a singe string or a vector of strings to be concatenated later?
                      suppress.warnings=FALSE
)
{
  # Preconditions:
  if( length(x) != length(y) || length(x) != length(text) || length(x) == 0 )
  {
    if( !suppress.warnings ) .report.ewms("There must be at least one text and the number of texts should matche the number of coordinates.\n", "error", ".SVG.text", "AdhereR");
    return (NULL);
  }

  if(!is.na(tooltip)) tooltip <- .SVG.specialchars.2.XMLentities(tooltip); # make sure special chars in tooltip are treated correctly

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

            # Close the tag:
            '> ',

            # The text:
            .SVG.specialchars.2.XMLentities(text[i]),

            # Add optional tooltip:
            if(!is.na(tooltip)) c(' <title>', tooltip, '</title>'), # the tooltip title must be first child

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


#' Access last adherence plot info.
#'
#' Returns the full info the last adherence plot, to be used to modify and/or to
#' add new elements to this plot.
#'
#' This is intended for advanced users only.
#' It may return \code{NULL} if no plotting was generated yet, but if one was, a
#' list contaning one named element for each type of plot produced (currently only
#' \emph{baseR} and \emph{SVG} are used).
#' For all types of plots there are a set of \emph{mapping} functions useful for
#' transforming events in plotting coordinates: \code{.map.event.x(x)} takes a
#' number of days \code{x}, \code{.map.event.date(d, adjust.for.earliest.date=TRUE)}
#' takes a \code{Date} \code{d} (and implictely adjusts for the earilerst date
#' plotted), and  \code{.map.event.y(y)} takes a row ("event" number) \code{y}.
#' Besides the shared elements (see the returned value), there are specific ones
#' as well.
#' For \emph{baseR}, the members \emph{old.par} and \emph{used.par} contain the
#' original (pre-plot) \code{par()} environment and the one used within
#' \code{plot()}, respectively, in case these need restoring.
#'
#' @return A \code{list} (possibly empty) contaning one named element for each type
#' of plot produced (currently only \emph{baseR} and \emph{SVG}). Each may contain
#' shared and specific fields concerning:
#' \itemize{
#'  \item the values of the parameters with which \code{plot()} was invoked.
#'  \item actual plot size and other characteristics.
#'  \item actual title, axis names and labels and their position and size.
#'  \item legend size, position and size and position of its components.
#'  \item expanded \code{cma$data} contaning, for each event, info about its
#'  plotting, including the corresponding fullow-uo and observation windows,
#'  event start and end, dose text (if any) and other graphical elements.
#'  \item position, size of the partial CMAs (if any) and of their components.
#'  \item position, size of the plotted CMAs (if any) and of their components.
#'  \item rescaling function(s) useful for mapping events to plotting coordinates.
#' }
#' @examples
#' cma7 <- CMA7(data=med.events[med.events$PATIENT_ID %in% c(1,2),],
#'              ID.colname="PATIENT_ID",
#'              event.date.colname="DATE",
#'              event.duration.colname="DURATION",
#'              event.daily.dose.colname="PERDAY",
#'              medication.class.colname="CATEGORY",
#'              followup.window.start=0,
#'              followup.window.start.unit="days",
#'              followup.window.duration=2*365,
#'              followup.window.duration.unit="days",
#'              observation.window.start=30,
#'              observation.window.start.unit="days",
#'              observation.window.duration=365,
#'              observation.window.duration.unit="days",
#'              date.format="%m/%d/%Y",
#'              summary="Base CMA");
#' plot(cma7);
#' tmp <- last.plot.get.info();
#' names(tmp);
#' tmp$baseR$legend$box; # legend position and size
#' head(tmp$baseR$cma$data); # events + plotting info
#' # Add a transparent blue rect between days 270 and 900:
#' rect(tmp$baseR$.map.event.x(270), tmp$baseR$.map.event.y(1-0.5),
#'      tmp$baseR$.map.event.x(900), tmp$baseR$.map.event.y(nrow(tmp$baseR$cma$data)+0.5),
#'      col=adjustcolor("blue",alpha.f=0.5), border="blue");
#' # Add a transparent rect rect between dates 03/15/2036 and 03/15/2037:
#' rect(tmp$baseR$.map.event.date(as.Date("03/15/2036", format="%m/%d/%Y")),
#'      tmp$baseR$.map.event.y(1-0.5),
#'      tmp$baseR$.map.event.date(as.Date("03/15/2037", format="%m/%d/%Y")),
#'      tmp$baseR$.map.event.y(nrow(tmp$baseR$cma$data)+0.5),
#'      col=adjustcolor("red",alpha.f=0.5), border="blue");
#' @export
last.plot.get.info <- function() { return (get(".last.cma.plot.info", envir=.adherer.env)); }

#' Map from event to plot coordinates.
#'
#' Maps the (x,y) coordinates in the event space to the plotting space.
#'
#' This is intended for advanced users only.
#' In the event space, the \emph{x} coordinate can be either given as the number of
#' days since the first plotted event, or as an actual calendar date (either as a
#' \code{Date} object or a string with a given format; a date may or may not be corrected
#' relative to the first displayed date). On the \emph{y} coordinate, the plotting is
#' divided in equally spaced rows, each row corresponding to a single event or an element
#' of a partial CMA plot (one can specify in between rows using fractions). Any or both of
#' \emph{x} and \emph{y} can be missing.
#'
#' @param x The \emph{x} coordinate in the event space, either a \code{number} giving the
#' number of days since the earliest plotted date, or a \code{Date} or a \code{string} in
#' the format given by the \emph{x.date.format} parameter giving the actual calendar date.
#' @param y The \emph{y} coordinate in the event space, thus a \code{number} giving the
#' plot row.
#' @param x.is.Date A \code{logical}, being \code{TRUE} if \emph{x} is a string giving the
#' date in the \emph{x.date.format} format.
#' @param x.date.format A \code{string} giving the format of the \emph{x} date, if
#' \emph{x.is.Date} id \code{TRUE}.
#' @param adjust.for.earliest.date A \code{logical} which is \code{TRUE} if \emph{x} is a
#' calendar date that must be adjusted for the earliest plotted date (by default
#' \code{TRUE}).
#' @param plot.type Can be either "baseR" or "SVG" and specifies to which type of plotting
#' the mapping applies.
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any
#' warnings.
#'
#' @return A numeric vector with \emph{x} and \emph{y} components giving the plotting
#' coordinates, or \code{NULL} in case of error.
#'
#' @examples
#' cma7 <- CMA7(data=med.events[med.events$PATIENT_ID %in% c(1,2),],
#'              ID.colname="PATIENT_ID",
#'              event.date.colname="DATE",
#'              event.duration.colname="DURATION",
#'              event.daily.dose.colname="PERDAY",
#'              medication.class.colname="CATEGORY",
#'              followup.window.start=0,
#'              followup.window.start.unit="days",
#'              followup.window.duration=2*365,
#'              followup.window.duration.unit="days",
#'              observation.window.start=30,
#'              observation.window.start.unit="days",
#'              observation.window.duration=365,
#'              observation.window.duration.unit="days",
#'              date.format="%m/%d/%Y",
#'              summary="Base CMA");
#' plot(cma7);
#' # Add a transparent blue rect:
#' rect(map.event.coords.to.plot(x=270),
#'      get.event.plotting.area()["y.min"]-1,
#'      map.event.coords.to.plot(x="03/15/2037", x.is.Date=TRUE, x.date.format="%m/%d/%Y"),
#'      get.event.plotting.area()["y.max"]+1,
#'      col=adjustcolor("blue",alpha.f=0.5), border="blue");
#' @export
map.event.coords.to.plot <- function(x=NA, y=NA, x.is.Date=FALSE, x.date.format="%m/%d/%Y", adjust.for.earliest.date=TRUE, plot.type=c("baseR", "SVG")[1], suppress.warnings=FALSE)
{
  lcpi <- last.plot.get.info();

  if( plot.type[1] == "baseR" )
  {
    if( is.null(lcpi) || is.null(lcpi$baseR) )
    {
      if( !suppress.warnings ) .report.ewms("No CMA plot or no base R plot were generated!\n", "error", "map.event.coords.to.plot", "AdhereR");
      return (NULL);
    } else
    {
      # x:
      if( is.na(x) )
      {
        x1 <- NA;
      } else if( inherits(x, "Date") )
      {
        x1 <- lcpi$baseR$.map.event.date(x, adjust.for.earliest.date=adjust.for.earliest.date);
      } else if( x.is.Date )
      {
        x1 <- lcpi$baseR$.map.event.date(as.Date(as.character(x), format=x.date.format), adjust.for.earliest.date=adjust.for.earliest.date);
      } else
      {
        x1 <- lcpi$baseR$.map.event.x(x);
      }
      # y:
      if( is.na(y) )
      {
        y1 <- NA;
      } else
      {
        y1 <- lcpi$baseR$.map.event.y(y);
      }
      # Return value:
      return (c("x"=x1, "y"=y1));
    }
  } else if( plot.type[1] == "SVG" )
  {
    if( is.null(lcpi) || is.null(lcpi$SVG) )
    {
      if( !suppress.warnings ) .report.ewms("No CMA plot or no SVG was generated!\n", "error", "map.event.coords.to.plot", "AdhereR");
      return (NULL);
    } else
    {
      # x:
      if( inherits(x, "Date") || x.is.Date )
      {
        x1 <- lcpi$SVG$.map.event.date(ifelse(inherits(x, "Date"), x, as.Date(x, format=x.date.format)), adjust.for.earliest.date=adjust.for.earliest.date);
      } else
      {
        x1 <- lcpi$SVG$.map.event.x(x);
      }
      # y:
      y1 <- lcpi$SVG$.map.event.y(y);
      # Return value:
      return (c("x"=x1, "y"=y1));
    }
  } else
  {
    if( !suppress.warnings ) .report.ewms("Unknown plot type!\n", "error", "map.event.coords.to.plot", "AdhereR");
    return (NULL);
  }
}

#' Get the actual plotting area.
#'
#' Returns the actual plotting area rectangle in plotting coordinates.
#'
#' This is intended for advanced users only.
#'
#' @param plot.type Can be either "baseR" or "SVG" and specifies to which type of plotting
#' the mapping applies.
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any
#' warnings.
#'
#' @return A numeric vector with components \emph{x.min}, \emph{x.max},
#' \emph{y.min} and \emph{y.max}, or \code{NULL} in case of error.
#' @export
get.event.plotting.area <- function(plot.type=c("baseR", "SVG")[1], suppress.warnings=FALSE)
{
  lcpi <- last.plot.get.info();

  if( plot.type[1] == "baseR" )
  {
    if( is.null(lcpi) || is.null(lcpi$baseR) )
    {
      if( !suppress.warnings ) .report.ewms("No CMA plot or no base R was generated!\n", "error", "get.event.plotting.area", "AdhereR");
      return (NULL);
    } else
    {
      return (c("x.min"=lcpi$baseR$x.min, "x.max"=lcpi$baseR$x.max, "y.min"=lcpi$baseR$y.min, "y.max"=lcpi$baseR$y.max));
    }
  } else if( plot.type[1] == "SVG" )
  {
    if( is.null(lcpi) || is.null(lcpi$SVG) )
    {
      if( !suppress.warnings ) .report.ewms("No CMA plot or no SVG was generated!\n", "error", "get.event.plotting.area", "AdhereR");
      return (NULL);
    } else
    {
      return (c("x.min"=lcpi$SVG$x.min, "x.max"=lcpi$SVG$x.max, "y.min"=lcpi$SVG$y.min, "y.max"=lcpi$SVG$y.max));
    }
  } else
  {
    if( !suppress.warnings ) .report.ewms("Unknown plot type!\n", "error", "get.event.plotting.area", "AdhereR");
    return (NULL);
  }
}

#' Get the legend plotting area.
#'
#' Returns the legend plotting area rectangle in plotting coordinates
#' (if any).
#'
#' This is intended for advanced users only.
#'
#' @param plot.type Can be either "baseR" or "SVG" and specifies to which type of plotting
#' the mapping applies.
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any
#' warnings.
#'
#' @return A numeric vector with components \emph{x.min}, \emph{x.max},
#' \emph{y.min} and \emph{y.max}, or \code{NULL} in case of error or no
#' legend being shown.
#' @export
get.legend.plotting.area <- function(plot.type=c("baseR", "SVG")[1], suppress.warnings=FALSE)
{
  lcpi <- last.plot.get.info();

  if( plot.type[1] == "baseR" )
  {
    if( is.null(lcpi) || is.null(lcpi$baseR) )
    {
      if( !suppress.warnings ) .report.ewms("No CMA plot or no base R was generated!\n", "error", "get.legend.plotting.area", "AdhereR");
      return (NULL);
    } else
    {
      if( is.null(lcpi$baseR$legend) )
      {
        return (NULL); # no legend being shown
      } else
      {
        return (c("x.min"=lcpi$baseR$legend$x.start, "x.max"=lcpi$baseR$legend$x.end, "y.min"=lcpi$baseR$legend$y.start, "y.max"=lcpi$baseR$legend$y.end));
      }
    }
  } else if( plot.type[1] == "SVG" )
  {
    if( is.null(lcpi) || is.null(lcpi$SVG) )
    {
      if( !suppress.warnings ) .report.ewms("No CMA plot or no SVG was generated!\n", "error", "get.legend.plotting.area", "AdhereR");
      return (NULL);
    } else
    {
      if( is.null(lcpi$SVG$legend) )
      {
        return (NULL); # no legend being shown
      } else
      {
        return (c("x.min"=lcpi$SVG$legend$x.start, "x.max"=lcpi$SVG$legend$x.end, "y.min"=lcpi$SVG$legend$y.start, "y.max"=lcpi$SVG$legend$y.end));
      }
    }
  } else
  {
    if( !suppress.warnings ) .report.ewms("Unknown plot type!\n", "error", "get.legend.plotting.area", "AdhereR");
    return (NULL);
  }
}


#' Get info about the plotted events.
#'
#' Returns a \code{data.frame} where each row contains info about one plotted event;
#' the order of the rows reflects the y-axis (first row on bottom).
#'
#' This is intended for advanced users only.
#'
#' @param plot.type Can be either "baseR" or "SVG" and specifies to which type of plotting
#' the mapping applies.
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any
#' warnings.
#'
#' @return A \code{data.frame} that, besides the info about each event, also
#' contains info about:
#' \itemize{
#'  \item the corresponding follow-up and observation windows (and, for
#'  \code{CMA8}, the "real" observation window), given as the corners of the area
#'  \emph{.X...START}, \emph{.X...END}, \emph{.Y...START} and \emph{.Y...END}
#'  (where the mid dot stands for FUW, OW and ROW, respectively).
#'  \item the area occupied by the graphic representation of the event given by
#'  its four corners \emph{.X.START}, \emph{.X.END}, \emph{.Y.START} and
#'  \emph{.Y.END}, as well as the line width \emph{.EV.LWD}.
#'  \item the dose text's (if any) position (\emph{.X.DOSE}, \emph{.Y.DOSE}) and
#'  font size \emph{.FONT.SIZE.DOSE}.
#'  \item if event corvered and not covered are plotted, also give their areas as
#'  \emph{.X.EVC.START}, \emph{.X.EVC.END}, \emph{.Y.EVC.START}, \emph{.Y.EVC.END},
#'  \emph{.X.EVNC.START}, \emph{.X.EVNC.END}, \emph{.Y.EVNC.START} and
#'  \emph{.Y.EVNC.END}.
#'  \item the continuation lines area as \emph{.X.CNT.START}, \emph{.X.CNT.END},
#'  \emph{.Y.CNT.START} and \emph{.Y.CNT.END}.
#'  \item and the corresponding summary CMA (if any) given as the area
#'  \emph{.X.SCMA.START}, \emph{.X.SCMA.END}, \emph{.Y.SCMA.START} and
#'  \emph{.Y.SCMA.END}.
#' }
#' Please note that even if with follow-up and ("real") observation window, and
#' the summary CMA info is repeated for each event, they really make sense at
#' the level of the patient.
#' @examples
#' cma7 <- CMA7(data=med.events[med.events$PATIENT_ID %in% c(1,2),],
#'              ID.colname="PATIENT_ID",
#'              event.date.colname="DATE",
#'              event.duration.colname="DURATION",
#'              event.daily.dose.colname="PERDAY",
#'              medication.class.colname="CATEGORY",
#'              followup.window.start=0,
#'              followup.window.start.unit="days",
#'              followup.window.duration=2*365,
#'              followup.window.duration.unit="days",
#'              observation.window.start=30,
#'              observation.window.start.unit="days",
#'              observation.window.duration=365,
#'              observation.window.duration.unit="days",
#'              date.format="%m/%d/%Y",
#'              summary="Base CMA");
#' plot(cma7);
#' tmp <- get.plotted.events();
#' head(tmp);
#' # "Mask" the first event:
#' rect(tmp$.X.START[1], tmp$.Y.START[1]-0.5, tmp$.X.END[1], tmp$.Y.END[1]+0.5,
#'      col=adjustcolor("white",alpha.f=0.75), border="black");
#' # "Mask" the first patient's summary CMA:
#' rect(tmp$.X.SCMA.START[1], tmp$.Y.SCMA.START[1],
#'      tmp$.X.SCMA.END[1], tmp$.Y.SCMA.END[1],
#'      col=adjustcolor("white",alpha.f=0.75), border="black");
#' @export
get.plotted.events <- function(plot.type=c("baseR", "SVG")[1], suppress.warnings=FALSE)
{
  lcpi <- last.plot.get.info();

  if( plot.type[1] == "baseR" )
  {
    if( is.null(lcpi) || is.null(lcpi$baseR) )
    {
      if( !suppress.warnings ) .report.ewms("No CMA plot or no base R was generated!\n", "error", "get.plotted.events", "AdhereR");
      return (NULL);
    } else
    {
      if( is.null(lcpi$baseR$cma) || is.null(lcpi$baseR$cma$data) )
      {
        if( !suppress.warnings ) .report.ewms("No info about the plotted CMA!\n", "error", "get.plotted.events", "AdhereR");
        return (NULL);
      } else
      {
        return (lcpi$baseR$cma$data);
      }
    }
  } else if( plot.type[1] == "SVG" )
  {
    if( is.null(lcpi) || is.null(lcpi$SVG) )
    {
      if( !suppress.warnings ) .report.ewms("No CMA plot or no SVG was generated!\n", "error", "get.plotted.events", "AdhereR");
      return (NULL);
    } else
    {
      if( is.null(lcpi$SVG$cma) || is.null(lcpi$SVG$cma$data) )
      {
        if( !suppress.warnings ) .report.ewms("No info about the plotted CMA!\n", "error", "get.plotted.events", "AdhereR");
        return (NULL);
      } else
      {
        return (lcpi$SVG$cma$data);
      }
    }
  } else
  {
    if( !suppress.warnings ) .report.ewms("Unknown plot type!\n", "error", "get.plotted.events", "AdhereR");
    return (NULL);
  }
}


#' Get info about the plotted partial CMAs.
#'
#' Returns a \code{data.frame} where each row contains info about one plotted
#' partial CMA (partial CMAs make sense only for "complex" CMAs, i.e., per
#' episode and sliding windows).
#'
#' This is intended for advanced users only.
#'
#' @param plot.type Can be either "baseR" or "SVG" and specifies to which type of plotting
#' the mapping applies.
#' @param suppress.warnings \emph{Logical}, if \code{TRUE} don't show any
#' warnings.
#'
#' @return A \code{data.frame} that contains info about:
#' \itemize{
#'  \item the patient ID (\emph{pid}) to which the partial CMA belongs.
#'  \item the \emph{type} of partial CMA (see the help for plotting "complex"
#'  CMAs).
#'  \item the corners of the whole area covered by the partial CMA plot given as
#'  \emph{x.region.start}, \emph{y.region.start}, \emph{x.region.end} and
#'  \emph{y.region.end}.
#'  \item for each element of the partial CMA plot, its area as
#'  \emph{x.partial.start}, \emph{y.partial.start}, \emph{x.partial.end} and
#'  \emph{y.partial.end}.
#' }
#' Please note that this contains one row per partial CMA element (e.g., if
#' plotting stacked, one row for each rectangle).
#' @export
get.plotted.partial.cmas <- function(plot.type=c("baseR", "SVG")[1], suppress.warnings=FALSE)
{
  lcpi <- last.plot.get.info();

  if( plot.type[1] == "baseR" )
  {
    if( is.null(lcpi) || is.null(lcpi$baseR) )
    {
      if( !suppress.warnings ) .report.ewms("No CMA plot or no base R was generated!\n", "error", "get.plotted.partial.cmas", "AdhereR");
      return (NULL);
    } else
    {
      if( is.null(lcpi$baseR$partialCMAs) )
      {
        if( !suppress.warnings ) .report.ewms("No partial CMAs: are you sur this is the right type of CMA and that the partial CMAs were actually plotted?\n", "error", "get.plotted.partial.cmas", "AdhereR");
        return (NULL);
      } else
      {
        return (lcpi$baseR$partialCMAs);
      }
    }
  } else if( plot.type[1] == "SVG" )
  {
    if( is.null(lcpi) || is.null(lcpi$SVG) )
    {
      if( !suppress.warnings ) .report.ewms("No CMA plot or no SVG was generated!\n", "error", "get.plotted.partial.cmas", "AdhereR");
      return (NULL);
    } else
    {
      if( is.null(lcpi$SVG$partialCMAs) )
      {
        if( !suppress.warnings ) .report.ewms("No partial CMAs: are you sur this is the right type of CMA and that the partial CMAs were actually plotted?\n", "error", "get.plotted.partial.cmas", "AdhereR");
        return (NULL);
      } else
      {
        return (lcpi$SVG$partialCMAs);
      }
    }
  } else
  {
    if( !suppress.warnings ) .report.ewms("Unknown plot type!\n", "error", "get.plotted.partial.cmas", "AdhereR");
    return (NULL);
  }
}


## The plotting function ####
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
                       ylab=c("withoutCMA"="patient", "withCMA"="patient (& CMA)"), # Vector of y labels to show without and with CMA estimates, or a single value for both, or NULL for nothing
                       title=c("aligned"="Event patterns (all patients aligned)", "notaligned"="Event patterns"), # Vector of titles to show for and without alignment, or a single value for both, or NULL for nothing
                       col.cats=rainbow,                      # single color or a function mapping the categories to colors
                       unspecified.category.label="drug",     # the label of the unspecified category of medication
                       medication.groups.to.plot=NULL,        # the names of the medication groups to plot (by default, all)
                       medication.groups.separator.show=TRUE, medication.groups.separator.lty="solid", medication.groups.separator.lwd=2, medication.groups.separator.color="blue", # group medication events by patient?
                       medication.groups.allother.label="*",  # the label to use for the __ALL_OTHERS__ medication class (defaults to *)
                       lty.event="solid", lwd.event=2, pch.start.event=15, pch.end.event=16, # event style
                       show.event.intervals=TRUE,             # show the actual prescription intervals
                       plot.events.vertically.displaced=TRUE, # display the events on different lines (vertical displacement) or not (defaults to TRUE)?
                       print.dose=FALSE, cex.dose=0.75, print.dose.col="black", print.dose.outline.col="white", print.dose.centered=FALSE, # print daily dose
                       plot.dose=FALSE, lwd.event.max.dose=8, plot.dose.lwd.across.medication.classes=FALSE, # draw daily dose as line width
                       col.na="lightgray",                    # color for missing data
                       col.continuation="black", lty.continuation="dotted", lwd.continuation=1, # style of the continuation lines connecting consecutive events
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
                       plot.partial.CMAs.as.timeseries.alpha.interval=0.25, # the transparency of the intervals (when drawn as rectangles)
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
                       force.draw.text=FALSE,           # if true, always draw text even if too big or too small
                       bw.plot=FALSE,                   # if TRUE, override all user-given colors and replace them with a scheme suitable for grayscale plotting
                       min.plot.size.in.characters.horiz=0, min.plot.size.in.characters.vert=0, # the minimum plot size (in characters: horizontally, for the whole duration, vertically, per event (and, if shown, per episode/sliding window))
                       max.patients.to.plot=100,        # maximum number of patients to plot
                       suppress.warnings=FALSE,         # suppress warnings?
                       export.formats=NULL,             # the formats to export the figure to (by default, none); can be any subset of "svg" (just SVG file), "html" (SVG + HTML + CSS + JavaScript all embedded within the HTML document), "jpg", "png", "webp", "ps" and "pdf"
                       export.formats.fileprefix="AdhereR-plot", # the file name prefix for the exported formats
                       export.formats.height=NA, export.formats.width=NA, # desired dimensions (in pixels) for the exported figure (defaults to sane values)
                       export.formats.save.svg.placeholder=TRUE,
                       export.formats.svg.placeholder.type=c("jpg", "png", "webp")[1],
                       export.formats.svg.placeholder.rsvg=TRUE,
                       export.formats.svg.placeholder.embed=FALSE, # save a placeholder for the SVG image?
                       export.formats.html.template=NULL, export.formats.html.javascript=NULL, export.formats.html.css=NULL, # HTML, JavaScript and CSS templates for exporting HTML+SVG
                       export.formats.directory=NA,     # if exporting, which directory to export to (if not give, creates files in the temporary directory)
                       generate.R.plot=TRUE,            # generate standard (base R) plot for plotting within R?
                       ...
)
{
  # What sorts of plots to generate (use short names for short if statements):
  .do.R <- generate.R.plot; .do.SVG <- (!is.null(export.formats) && any(c("svg", "html", "jpg", "png", "webp", "ps", "pdf") %in% export.formats));
  if( !.do.R && !.do.SVG )
  {
    # Nothing to plot!
    return (invisible(NULL));
  }

  # Stuff not yet implemented;
  if( !export.formats.svg.placeholder.rsvg )
  {
    if( !suppress.warnings ) .report.ewms("Using base R for the SVG placeholder is not yet implemented: falling back to using rsvg\n", "warning", ".plot.CMAs", "AdhereR");
    export.formats.svg.placeholder.rsvg <- TRUE;
  }


  #
  # Initialise the SVG file content ####
  #
  # Things to remember about SVGs:
  #   - coordinates start top-left and go right and bottom
  #   - font size is relative to the viewBox
  #

  if( .do.SVG )
  {
    # The SVG header and string (body):
    svg.header <- c('<?xml version="1.0" standalone="no"?>\n',
                    '<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">\n');
  }
  svg.str <- NULL; # some cases need (even an empty) svg.str...

  #
  # Set-up, checks and local functions ####
  #

  # Preconditions:
  if( is.null(cma) ||                                                                                            # must be: non-null
      !(inherits(cma, "CMA_per_episode") || inherits(cma, "CMA_sliding_window") || inherits(cma, "CMA0")) ||     # a proper CMA object
      is.null(cma$data) || nrow(cma$data) < 1 || !inherits(cma$data, "data.frame") ||                            # that containins non-null data derived from data.frame
      is.na(cma$ID.colname) || !(cma$ID.colname %in% names(cma$data)) ||                                         # has a valid patient ID column
      is.na(cma$event.date.colname) || !(cma$event.date.colname %in% names(cma$data)) ||                         # has a valid event date column
      is.na(cma$event.duration.colname) || !(cma$event.duration.colname %in% names(cma$data))                    # has a valid event duration column
  )
  {
    if( !suppress.warnings ) .report.ewms("Can only plot a correctly specified CMA object (i.e., with valid data and column names)!\n", "error", ".plot.CMAs", "AdhereR");
    plot.CMA.error(export.formats=export.formats,
                   export.formats.fileprefix=export.formats.fileprefix,
                   export.formats.directory=export.formats.directory,
                   generate.R.plot=generate.R.plot);
    return (invisible(NULL));
  }

  # Overriding dangerous or aesthetic defaults:
  if( force.draw.text && !suppress.warnings ) .report.ewms("Forcing drawing of text elements even if too big or ugly!\n", "warning", ".plot.CMAs", "AdhereR");

  # SVG placeholder:
  if( export.formats.save.svg.placeholder && (length(export.formats.svg.placeholder.type) != 1 || !export.formats.svg.placeholder.type %in% c("jpg", "png", "webp")) )
  {
    if( !suppress.warnings ) .report.ewms("The SVG place holder can only be a jpg, png or webp!\n", "error", ".plot.CMAs", "AdhereR");
    plot.CMA.error(export.formats=export.formats,
                   export.formats.fileprefix=export.formats.fileprefix,
                   export.formats.directory=export.formats.directory,
                   generate.R.plot=generate.R.plot);
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
      if( force.draw.text || char.height.CMA <= abs(.rescale.xcoord.for.CMA.plot(1.0) - .rescale.xcoord.for.CMA.plot(0.0)) )
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
                                class="cma-summary-plot", suppress.warnings=suppress.warnings)
        );
      }
      if( force.draw.text || 3*dims.chr.cma <= abs(.scale.width.to.SVG.plot(.rescale.xcoord.for.CMA.plot(1.0) - .rescale.xcoord.for.CMA.plot(0.0))) )
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
                               class="cma-summary-text", suppress.warnings=suppress.warnings)
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
      if( force.draw.text || char.height.CMA <= abs(.rescale.xcoord.for.CMA.plot(1) - .rescale.xcoord.for.CMA.plot(0)) )
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
                              class="cma-summary-plot", suppress.warnings=suppress.warnings)
      );
      if( force.draw.text || 2*dims.chr.cma <= abs(.scale.width.to.SVG.plot(.rescale.xcoord.for.CMA.plot(1.0) - .rescale.xcoord.for.CMA.plot(0.0))) )
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
                               class="cma-summary-text", suppress.warnings=suppress.warnings)
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
      } else if( force.draw.text || char.height.CMA <= abs(.rescale.xcoord.for.CMA.plot(1) - .rescale.xcoord.for.CMA.plot(0)) )
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
                                class="cma-summary-plot", suppress.warnings=suppress.warnings)
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
                               class="cma-summary-text", suppress.warnings=suppress.warnings)
        );
      } else if( force.draw.text || 2*dims.chr.cma <= abs(.scale.width.to.SVG.plot(.rescale.xcoord.for.CMA.plot(1.0) - .rescale.xcoord.for.CMA.plot(0.0))) )
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
                               class="cma-summary-text", suppress.warnings=suppress.warnings)
        );
      }
    }

    return (svg.str);
  }


  # Legend plotting auxiliary functions ####
  if( show.legend )
  {
    if( .do.R )
    {
      .legend.R <- function(x=0, y=0, width=1, height=1, do.plot=TRUE)
      {
        # Legend rectangle:
        if( do.plot )
        {
          rect(x, y, x + width, y + height, border=gray(0.6), lwd=2, col=rgb(0.99,0.99,0.99,legend.bkg.opacity));
          # Save the info:
          .last.cma.plot.info$baseR$legend <<- list("box"=data.frame("x.start"=x, "y.start"=y, "x.end"=x+width, "y.end"=y+height));
          .last.cma.plot.info$baseR$legend$components <<- NULL;
        }

        cur.y <- y + height; # current y
        max.width <- width; # maximum width

        # Legend title:
        if( do.plot )
        {
          text(x + width/2, cur.y, "Legend", pos=1, col=gray(0.3), cex=legend.cex.title);
          # Save the info:
          .last.cma.plot.info$baseR$legend$title <<- data.frame("string"="Legend", "x"=x+width/2, "y"=cur.y, "cex"=legend.cex.title);
        }
        cur.y <- cur.y - strheight("Legend", cex=legend.cex.title) - 3*legend.char.height; max.width <- max(max.width, strwidth("Legend", cex=legend.cex.title));

        # Event:
        if( do.plot )
        {
          segments(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y, lty=lty.event, lwd=lwd.event, col="black");
          points(x + 1.0*legend.char.width, cur.y, pch=pch.start.event, cex=legend.cex, col="black");
          points(x + 4.0*legend.char.width, cur.y, pch=pch.end.event, cex=legend.cex, col="black");
        }

        if( !plot.dose )
        {
          if( do.plot )
          {
            text(x + 5.0*legend.char.width, cur.y, "duration", col="black", cex=legend.cex, pos=4);
            # Save the info:
            .last.cma.plot.info$baseR$legend$components <<- rbind(.last.cma.plot.info$baseR$legend$components,
                                                                  data.frame("string"="duration",
                                                                             "x.start"=x + 1.0*legend.char.width, "y.start"=cur.y,
                                                                             "x.end"=x + 4.0*legend.char.width, "y.end"=cur.y,
                                                                             "x.string"=x + 5.0*legend.char.width, "y.string"=cur.y,
                                                                             "cex"=legend.cex));
          }
          cur.y <- cur.y - 1.5*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("duration", cex=legend.cex));
        } else
        {
          if( do.plot )
          {
            text(x + 5.0*legend.char.width, cur.y, "duration (min. dose)", col="black", cex=legend.cex, pos=4);
          }
          cur.y <- cur.y - 1.5*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("duration (min. dose)", cex=legend.cex));
          if( do.plot )
          {
            segments(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y, lty=lty.event, lwd=lwd.event.max.dose, col="black");
            points(x + 1.0*legend.char.width, cur.y, pch=pch.start.event, cex=legend.cex, col="black");
            points(x + 4.0*legend.char.width, cur.y, pch=pch.end.event, cex=legend.cex, col="black");
            text(x + 5.0*legend.char.width, cur.y, "duration (max. dose)", col="black", cex=legend.cex, pos=4);
            # Save the info:
            .last.cma.plot.info$baseR$legend$components <<- rbind(.last.cma.plot.info$baseR$legend$components,
                                                                  data.frame("string"=c("duration (min. dose)", "duration (max. dose)"),
                                                                             "x.start"=rep(x + 1.0*legend.char.width,2), "y.start"=c(cur.y + 1.5*legend.char.height, cur.y),
                                                                             "x.end"=rep(x + 4.0*legend.char.width,2), "y.end"=c(cur.y + 1.5*legend.char.height, cur.y),
                                                                             "x.string"=rep(x + 5.0*legend.char.width,2), "y.string"=c(cur.y + 1.5*legend.char.height, cur.y),
                                                                             "cex"=legend.cex));
          }
          cur.y <- cur.y - 1.5*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("duration (max. dose)", cex=legend.cex));
        }

        # No event:
        if( do.plot )
        {
          segments(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y, lty=lty.continuation, lwd=lwd.continuation, col=col.continuation);
          text(x + 5.0*legend.char.width, cur.y, "no event/connector", col="black", cex=legend.cex, pos=4);
          # Save the info:
          .last.cma.plot.info$baseR$legend$components <<- rbind(.last.cma.plot.info$baseR$legend$components,
                                                                data.frame("string"="no event/connector",
                                                                           "x.start"=x + 1.0*legend.char.width, "y.start"=cur.y,
                                                                           "x.end"=x + 4.0*legend.char.width, "y.end"=cur.y,
                                                                           "x.string"=x + 5.0*legend.char.width, "y.string"=cur.y,
                                                                           "cex"=legend.cex));
        }
        cur.y <- cur.y - 1.5*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("no event/connector", cex=legend.cex));

        # Event intervals:
        if( show.event.intervals )
        {
          if( do.plot )
          {
            rect(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y - 1.0*legend.char.height, border="black", col=adjustcolor("black",alpha.f=0.5));
            text(x + 5.0*legend.char.width, cur.y - 0.5*legend.char.height, "days covered", col="black", cex=legend.cex, pos=4);
            # Save the info:
            .last.cma.plot.info$baseR$legend$components <<- rbind(.last.cma.plot.info$baseR$legend$components,
                                                                  data.frame("string"="days covered",
                                                                             "x.start"=x + 1.0*legend.char.width, "y.start"=cur.y,
                                                                             "x.end"=x + 4.0*legend.char.width, "y.end"=cur.y - 1.0*legend.char.height,
                                                                             "x.string"=x + 5.0*legend.char.width, "y.string"=cur.y - 0.5*legend.char.height,
                                                                             "cex"=legend.cex));
          }
          cur.y <- cur.y - 1.5*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("days covered", cex=legend.cex));
          if( do.plot )
          {
            rect(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y - 1.0*legend.char.height, border="black", col=NA); #, col="black", density=25);
            text(x + 5.0*legend.char.width, cur.y - 0.5*legend.char.height, "gap days", col="black", cex=legend.cex, pos=4);
            # Save the info:
            .last.cma.plot.info$baseR$legend$components <<- rbind(.last.cma.plot.info$baseR$legend$components,
                                                                  data.frame("string"="gap days",
                                                                             "x.start"=x + 1.0*legend.char.width, "y.start"=cur.y,
                                                                             "x.end"=x + 4.0*legend.char.width, "y.end"=cur.y - 1.0*legend.char.height,
                                                                             "x.string"=x + 5.0*legend.char.width, "y.string"=cur.y - 0.5*legend.char.height,
                                                                             "cex"=legend.cex));
          }
          cur.y <- cur.y - 2.0*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("gap days", cex=legend.cex));
        }

        # medication classes:
        for( i in 1:length(cols) )
        {
          med.class.name <- names(cols)[i]; med.class.name <- ifelse(is.na(med.class.name),"<missing>",med.class.name);
          if( do.plot )
          {
            rect(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y - 1.0*legend.char.height, border="black", col=adjustcolor(cols[i],alpha.f=0.5));
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
            # Save the info:
            .last.cma.plot.info$baseR$legend$components <<- rbind(.last.cma.plot.info$baseR$legend$components,
                                                                  data.frame("string"=med.class.name,
                                                                             "x.start"=x + 1.0*legend.char.width, "y.start"=cur.y,
                                                                             "x.end"=x + 4.0*legend.char.width, "y.end"=cur.y - 1.0*legend.char.height,
                                                                             "x.string"=x + 5.0*legend.char.width, "y.string"=cur.y - 0.5*legend.char.height,
                                                                             "cex"=legend.cex));
          }
          cur.y <- cur.y - 1.5*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth(names(cols)[i], cex=legend.cex));
        }
        cur.y <- cur.y - 0.5*legend.char.height;

        # Follow-up window:
        if( highlight.followup.window )
        {
          if( do.plot )
          {
            rect(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y - 1.0*legend.char.height, border=followup.window.col, lty="dotted", lwd=2, col=rgb(1,1,1,0.0));
            text(x + 5.0*legend.char.width, cur.y - 0.5*legend.char.height, "follow-up wnd.", col="black", cex=legend.cex, pos=4);
            # Save the info:
            .last.cma.plot.info$baseR$legend$components <<- rbind(.last.cma.plot.info$baseR$legend$components,
                                                                  data.frame("string"="follow-up wnd.",
                                                                             "x.start"=x + 1.0*legend.char.width, "y.start"=cur.y,
                                                                             "x.end"=x + 4.0*legend.char.width, "y.end"=cur.y - 1.0*legend.char.height,
                                                                             "x.string"=x + 5.0*legend.char.width, "y.string"=cur.y - 0.5*legend.char.height,
                                                                             "cex"=legend.cex));
          }
          cur.y <- cur.y - 2.0*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("follow-up wnd.", cex=legend.cex));
        }

        # Observation window:
        if( highlight.observation.window )
        {
          if( !is.null(cma.realOW) )
          {
            # CMA8 also has a "real" OW:
            if( do.plot )
            {
              rect(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y - 1.0*legend.char.height,
                   border=rgb(1,1,1,0.0), col=adjustcolor(observation.window.col,alpha.f=observation.window.opacity)); #, density=observation.window.density, angle=observation.window.angle);
              text(x + 5.0*legend.char.width, cur.y - 0.5*legend.char.height, "theor. obs. wnd.", col="black", cex=legend.cex, pos=4);
              # Save the info:
              .last.cma.plot.info$baseR$legend$components <<- rbind(.last.cma.plot.info$baseR$legend$components,
                                                                    data.frame("string"="theor. obs. wnd.",
                                                                               "x.start"=x + 1.0*legend.char.width, "y.start"=cur.y,
                                                                               "x.end"=x + 4.0*legend.char.width, "y.end"=cur.y - 1.0*legend.char.height,
                                                                               "x.string"=x + 5.0*legend.char.width, "y.string"=cur.y - 0.5*legend.char.height,
                                                                               "cex"=legend.cex));
            }
            cur.y <- cur.y - 1.5*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("theor. obs. wnd.", cex=legend.cex));
            if( do.plot )
            {
              rect(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y - 1.0*legend.char.height,
                   border=rgb(1,1,1,0.0), col=adjustcolor(observation.window.col,alpha.f=observation.window.opacity)); #, density=real.obs.window.density, angle=real.obs.window.angle);
              text(x + 5.0*legend.char.width, cur.y - 0.5*legend.char.height, "real obs. wnd.", col="black", cex=legend.cex, pos=4);
              # Save the info:
              .last.cma.plot.info$baseR$legend$components <<- rbind(.last.cma.plot.info$baseR$legend$components,
                                                                    data.frame("string"="real obs. wnd.",
                                                                               "x.start"=x + 1.0*legend.char.width, "y.start"=cur.y,
                                                                               "x.end"=x + 4.0*legend.char.width, "y.end"=cur.y - 1.0*legend.char.height,
                                                                               "x.string"=x + 5.0*legend.char.width, "y.string"=cur.y - 0.5*legend.char.height,
                                                                               "cex"=legend.cex));
            }
            cur.y <- cur.y - 2.0*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("real obs.wnd.", cex=legend.cex));
          } else
          {
            if( do.plot )
            {
              rect(x + 1.0*legend.char.width, cur.y, x + 4.0*legend.char.width, cur.y - 1.0*legend.char.height,
                   border=rgb(1,1,1,0.0), col=adjustcolor(observation.window.col,alpha.f=observation.window.opacity)) #, density=observation.window.density, angle=observation.window.angle);
              text(x + 5.0*legend.char.width, cur.y - 0.5*legend.char.height, "observation wnd.", col="black", cex=legend.cex, pos=4);
              # Save the info:
              .last.cma.plot.info$baseR$legend$components <<- rbind(.last.cma.plot.info$baseR$legend$components,
                                                                    data.frame("string"="observation wnd.",
                                                                               "x.start"=x + 1.0*legend.char.width, "y.start"=cur.y,
                                                                               "x.end"=x + 4.0*legend.char.width, "y.end"=cur.y - 1.0*legend.char.height,
                                                                               "x.string"=x + 5.0*legend.char.width, "y.string"=cur.y - 0.5*legend.char.height,
                                                                               "cex"=legend.cex));
            }
            cur.y <- cur.y - 2.0*legend.char.height; max.width <- max(max.width, 5.0*legend.char.width + strwidth("observation wnd.", cex=legend.cex));
          }
        }

        # Required size:
        return (c("width" =max.width + 5.0*legend.char.width,
                  "height"=(y + height - cur.y) + 1.0*legend.char.height));
      }
    }

    if( .do.SVG )
    {
      .legend.SVG <- function(x=0, y=0, do.plot=TRUE)
      {
        if( do.plot )
        {
          # The legend is an object that we can move around, scale, etc:
          l1 <- c(.SVG.comment("The legend", newpara=TRUE, newline=TRUE),
                  '<g id="legend">\n');
        }

        # The legend origins:
        x.origin <- ifelse(!do.plot || is.numeric(x), x, 0.0); y.origin <- ifelse(!do.plot || is.numeric(y), y, 0.0);

        # Save the info:
        .last.cma.plot.info$SVG$legend <<- list();
        .last.cma.plot.info$SVG$legend$components <<- NULL;

        # The legend dimensions and other aesthetics:
        lw <- lh <- 0; # width and height
        lmx <- dims.chr.legend; lmy <- 2 # margins
        lnl <- 1.25; lnp <- 0.25; # the vertical size of a newline and newpara (in dims.chr.legend)

        # The actual legend content:
        # The legend title:
        if( do.plot )
        {
          l2 <- c(.SVG.text(x=x.origin + lmx, y=y.origin + lmy+lh+dims.chr.legend.title*2/3, text="Legend",
                            font_size=dims.chr.legend.title, font="Arial", h.align="left", v.align="center", col="gray30",
                            class="legend-title", suppress.warnings=suppress.warnings));
          # Save the info:
          .last.cma.plot.info$SVG$legend$title <<- data.frame("string"="Legend", "x"=x.origin + lmx, "y"=y.origin + lmy+lh+dims.chr.legend.title*2/3, "font.size"=dims.chr.legend.title);
        }
        lh <- lh + dims.chr.legend.title + lnl*dims.chr.legend; lw <- max(lw, .SVG.string.dims("Legend", font_size=dims.chr.legend.title)["width"]);
        lh <- lh + lnp*dims.chr.legend.title; # new para

        # The event:
        if( do.plot )
        {
          l2 <- c(l2,
                  .SVG.lines(x=x.origin + c(lmx, lmx + 3*dims.chr.legend), y=y.origin + c(lmy+lh, lmy+lh),
                             connected=FALSE, stroke="black", stroke_width=lwd.event, lty=lty.event,
                             class="legend-events", suppress.warnings=suppress.warnings),
                  .SVG.points(x=x.origin + c(lmx, lmx + 3*dims.chr.legend), y=y.origin + c(lmy+lh, lmy+lh),
                              pch=c(pch.start.event, pch.end.event), col="black", cex=legend.cex,
                              class="legend-events", suppress.warnings=suppress.warnings));
        }

        if( !plot.dose )
        {
          if( do.plot )
          {
            l2 <- c(l2,
                    .SVG.text(x=x.origin + lmx + 4*dims.chr.legend, y=y.origin + lmy+lh, text="duration",
                              col="black", font_size=dims.chr.legend, h.align="left", v.align="center",
                              class="legend-events", suppress.warnings=suppress.warnings));
            # Save the info:
            .last.cma.plot.info$SVG$legend$components <<- rbind(.last.cma.plot.info$SVG$legend$components,
                                                                data.frame("string"="duration",
                                                                           "x.start"=x.origin + lmx, "y.start"=y.origin + lmy+lh,
                                                                           "x.end"=x.origin + lmx + 3*dims.chr.legend, "y.end"=y.origin + lmy+lh,
                                                                           "x.string"=lmx + 4*dims.chr.legend, "y.string"=lmy+lh,
                                                                           "font.size"=dims.chr.legend));
          }
          lh <- lh + lnl*dims.chr.legend; lw <- max(lw, .SVG.string.dims("duration", font_size=dims.chr.legend)["width"] + 4*dims.chr.legend);
        } else
        {
          # Min dose:
          if( do.plot )
          {
            l2 <- c(l2,
                    .SVG.text(x=x.origin + lmx + 4*dims.chr.legend, y=y.origin + lmy+lh, text="duration (min. dose)",
                              col="black", font_size=dims.chr.legend, h.align="left", v.align="center",
                              class="legend-events", suppress.warnings=suppress.warnings));
            # Save the info:
            .last.cma.plot.info$SVG$legend$components <<- rbind(.last.cma.plot.info$SVG$legend$components,
                                                                data.frame("string"="duration (min. dose)",
                                                                           "x.start"=x.origin + lmx, "y.start"=y.origin + lmy+lh,
                                                                           "x.end"=x.origin + lmx + 3*dims.chr.legend, "y.end"=y.origin + lmy+lh,
                                                                           "x.string"=lmx + 4*dims.chr.legend, "y.string"=lmy+lh,
                                                                           "font.size"=dims.chr.legend));
          }
          lh <- lh + lnl*dims.chr.legend; lw <- max(lw, .SVG.string.dims("duration (min. dose)", font_size=dims.chr.legend)["width"] + 4*dims.chr.legend);

          # Max dose:
          if( do.plot )
          {
            l2 <- c(l2,
                    .SVG.lines(x=x.origin + c(lmx, lmx + 3*dims.chr.legend), y=y.origin + c(lmy+lh, lmy+lh),
                               connected=FALSE, stroke="black", stroke_width=lwd.event.max.dose, lty=lty.event,
                               class="legend-events", suppress.warnings=suppress.warnings),
                    .SVG.points(x=x.origin + c(lmx, lmx + 3*dims.chr.legend), y=y.origin + c(lmy+lh, lmy+lh),
                                pch=c(pch.start.event, pch.end.event),col="black", cex=legend.cex,
                                class="legend-events", suppress.warnings=suppress.warnings),
                    .SVG.text(x=x.origin + lmx + 4*dims.chr.legend, y=y.origin + lmy+lh, text="duration (max. dose)",
                              col="black", font_size=dims.chr.legend, h.align="left", v.align="center",
                              class="legend-events", suppress.warnings=suppress.warnings));
            # Save the info:
            .last.cma.plot.info$SVG$legend$components <<- rbind(.last.cma.plot.info$SVG$legend$components,
                                                                data.frame("string"="duration (max. dose)",
                                                                           "x.start"=x.origin + lmx, "y.start"=y.origin + lmy+lh,
                                                                           "x.end"=x.origin + lmx + 3*dims.chr.legend, "y.end"=y.origin + lmy+lh,
                                                                           "x.string"=lmx + 4*dims.chr.legend, "y.string"=lmy+lh,
                                                                           "font.size"=dims.chr.legend));
          }
          lh <- lh + lnl*dims.chr.legend; lw <- max(lw, .SVG.string.dims("duration (max. dose)", font_size=dims.chr.legend)["width"] + 4*dims.chr.legend);
        }

        # No event:
        if( do.plot )
        {
          l2 <- c(l2,
                  .SVG.lines(x=x.origin + c(lmx, lmx + 3*dims.chr.legend), y=y.origin + c(lmy+lh, lmy+lh),
                             connected=FALSE, stroke=col.continuation, stroke_width=lwd.continuation, lty=lty.continuation,
                             class="legend-no-event", suppress.warnings=suppress.warnings),
                  .SVG.text(x=x.origin + lmx + 4*dims.chr.legend, y=y.origin + lmy+lh, text="no event/connector",
                            col="black", font_size=dims.chr.legend, h.align="left", v.align="center",
                            class="legend-no-event", suppress.warnings=suppress.warnings));
          # Save the info:
          .last.cma.plot.info$SVG$legend$components <<- rbind(.last.cma.plot.info$SVG$legend$components,
                                                              data.frame("string"="no event/connector",
                                                                         "x.start"=x.origin + lmx, "y.start"=y.origin + lmy+lh,
                                                                         "x.end"=x.origin + lmx + 3*dims.chr.legend, "y.end"=y.origin + lmy+lh,
                                                                         "x.string"=lmx + 4*dims.chr.legend, "y.string"=lmy+lh,
                                                                         "font.size"=dims.chr.legend));
        }
        lh <- lh + lnl*dims.chr.legend; lw <- max(lw, .SVG.string.dims("no event/connector", font_size=dims.chr.legend)["width"] + 4*dims.chr.legend);
        lh <- lh + lnp*dims.chr.legend.title; # new para

        # Event intervals:
        if( show.event.intervals )
        {
          if( do.plot )
          {
            l2 <- c(l2,
                    .SVG.rect(x=x.origin + lmx, y=y.origin + lmy+lh-dims.chr.legend/2, width=3*dims.chr.legend, height=1*dims.chr.legend,
                              stroke="black", fill="black", fill_opacity=0.5,
                              class="legend-interval"),
                    .SVG.text(x=x.origin + lmx + 4*dims.chr.legend, y=y.origin + lmy+lh, text="days covered",
                              col="black", font_size=dims.chr.legend, h.align="left", v.align="center",
                              class="legend-interval", suppress.warnings=suppress.warnings));
            # Save the info:
            .last.cma.plot.info$SVG$legend$components <<- rbind(.last.cma.plot.info$SVG$legend$components,
                                                                data.frame("string"="days covered",
                                                                           "x.start"=x.origin + lmx, "y.start"=y.origin + lmy+lh,
                                                                           "x.end"=x.origin + lmx + 3*dims.chr.legend, "y.end"=y.origin + lmy+lh,
                                                                           "x.string"=lmx + 4*dims.chr.legend, "y.string"=lmy+lh,
                                                                           "font.size"=dims.chr.legend));
          }
          lh <- lh + lnl*dims.chr.legend; lw <- max(lw, .SVG.string.dims("days covered", font_size=dims.chr.legend)["width"] + 4*dims.chr.legend);
          if( do.plot )
          {
            l2 <- c(l2,
                    .SVG.rect(x=x.origin + lmx, y=y.origin + lmy+lh-dims.chr.legend/2, width=3*dims.chr.legend, height=1*dims.chr.legend,
                              stroke="black", fill="none",
                              class="legend-interval"),
                    .SVG.text(x=x.origin + lmx + 4*dims.chr.legend, y=y.origin + lmy+lh, text="gap days",
                              col="black", font_size=dims.chr.legend, h.align="left", v.align="center",
                              class="legend-interval", suppress.warnings=suppress.warnings));
            # Save the info:
            .last.cma.plot.info$SVG$legend$components <<- rbind(.last.cma.plot.info$SVG$legend$components,
                                                                data.frame("string"="gap days",
                                                                           "x.start"=x.origin + lmx, "y.start"=y.origin + lmy+lh,
                                                                           "x.end"=x.origin + lmx + 3*dims.chr.legend, "y.end"=y.origin + lmy+lh,
                                                                           "x.string"=lmx + 4*dims.chr.legend, "y.string"=lmy+lh,
                                                                           "font.size"=dims.chr.legend));
          }
          lh <- lh + lnl*dims.chr.legend; lw <- max(lw, .SVG.string.dims("gap days", font_size=dims.chr.legend)["width"] + 4*dims.chr.legend);
          lh <- lh + lnp*dims.chr.legend.title; # new para
        }

        # Medication classes:
        for( i in 1:length(cols) )
        {
          med.class.name <- names(cols)[i]; med.class.name <- ifelse(is.na(med.class.name),"<missing>",med.class.name);
          if( (is.na(cma$medication.class.colname) || !(cma$medication.class.colname %in% names(cma$data))) && length(cols) == 1 )
          {
            med.class.name.svg <- NA;
          } else
          {
            med.class.name.svg <- .map.category.to.class(med.class.name);
          }
          if( do.plot )
          {
            l2 <- c(l2,
                    .SVG.rect(x=x.origin + lmx, y=y.origin + lmy+lh-dims.chr.legend/2, width=3*dims.chr.legend, height=1*dims.chr.legend,
                              stroke="black", fill=cols[i], fill_opacity=0.5,
                              class=paste0("legend-medication-class-rect", if(med.class.name != "<missing>" && !is.na(med.class.name.svg)) paste0("-",med.class.name.svg) )));
          }
          #med.class.name <- names(cols)[i]; med.class.name <- ifelse(is.na(med.class.name),"<missing>",med.class.name);
          if( print.dose || plot.dose )
          {
            dose.for.cat <- (dose.range$category == med.class.name);
            if( sum(dose.for.cat,na.rm=TRUE) == 1 )
            {
              med.class.name <- paste0(med.class.name," (",dose.range$min[dose.for.cat]," - ",dose.range$max[dose.for.cat],")");
            }
          }
          if( do.plot )
          {
            l2 <- c(l2,
                    .SVG.text(x=x.origin + lmx + 4*dims.chr.legend, y=y.origin + lmy+lh, text=med.class.name,
                              col="black", font_size=dims.chr.legend, h.align="left", v.align="center",
                              class=paste0("legend-medication-class-label", if(med.class.name != "<missing>" && !is.na(med.class.name.svg)) paste0("-",med.class.name.svg) ),
                              suppress.warnings=suppress.warnings));
            # Save the info:
            .last.cma.plot.info$SVG$legend$components <<- rbind(.last.cma.plot.info$SVG$legend$components,
                                                                data.frame("string"=med.class.name,
                                                                           "x.start"=x.origin + lmx, "y.start"=y.origin + lmy+lh-dims.chr.legend/2,
                                                                           "x.end"=x.origin + lmx + 3*dims.chr.legend, "y.end"=y.origin + lmy+lh-dims.chr.legend/2+1*dims.chr.legend,
                                                                           "x.string"=lmx + 4*dims.chr.legend, "y.string"=lmy+lh,
                                                                           "font.size"=dims.chr.legend));
          }
          lh <- lh + lnl*dims.chr.legend; lw <- max(lw, .SVG.string.dims(med.class.name, font_size=dims.chr.legend)["width"] + 4*dims.chr.legend);
        }
        lh <- lh + lnp*dims.chr.legend.title; # new para

        # Follow-up window:
        if( highlight.followup.window )
        {
          if( do.plot )
          {
            l2 <- c(l2,
                    .SVG.rect(x=x.origin + lmx, y=y.origin + lmy+lh-dims.chr.legend/2, width=3*dims.chr.legend, height=1*dims.chr.legend,
                              stroke=followup.window.col, fill="none", stroke_width=2, lty="dashed",
                              class="legend-fuw-rect"),
                    .SVG.text(x=x.origin + lmx + 4*dims.chr.legend, y=y.origin + lmy+lh, text="follow-up wnd.",
                              col="black", font_size=dims.chr.legend, h.align="left", v.align="center",
                              class="legend-fuw-label", suppress.warnings=suppress.warnings));
            # Save the info:
            .last.cma.plot.info$SVG$legend$components <<- rbind(.last.cma.plot.info$SVG$legend$components,
                                                                data.frame("string"="follow-up wnd.",
                                                                           "x.start"=x.origin + lmx, "y.start"=y.origin + lmy+lh-dims.chr.legend/2,
                                                                           "x.end"=x.origin + lmx + 3*dims.chr.legend, "y.end"=y.origin + lmy+lh-dims.chr.legend/2+1*dims.chr.legend,
                                                                           "x.string"=lmx + 4*dims.chr.legend, "y.string"=lmy+lh,
                                                                           "font.size"=dims.chr.legend));
          }
          lh <- lh + lnl*dims.chr.legend; lw <- max(lw, .SVG.string.dims("follow-up wnd", font_size=dims.chr.legend)["width"] + 4*dims.chr.legend);
        }

        # Observation window:
        if( highlight.observation.window )
        {
          if( !is.null(cma.realOW) )
          {
            # CMA8 also has a "real" OW:
            if( do.plot )
            {
              l2 <- c(l2,
                      .SVG.rect(x=x.origin + lmx, y=y.origin + lmy+lh-dims.chr.legend/2, width=3*dims.chr.legend, height=1*dims.chr.legend,
                                stroke="none", fill=observation.window.col, fill_opacity=observation.window.opacity,
                                class="legend-ow-rect"),
                      .SVG.text(x=x.origin + lmx + 4*dims.chr.legend, y=y.origin + lmy+lh, text="theor. obs. wnd.",
                                col="black", font_size=dims.chr.legend, h.align="left", v.align="center",
                                class="legend-ow-label", suppress.warnings=suppress.warnings));
              # Save the info:
              .last.cma.plot.info$SVG$legend$components <<- rbind(.last.cma.plot.info$SVG$legend$components,
                                                                  data.frame("string"="theor. obs. wnd.",
                                                                             "x.start"=x.origin + lmx, "y.start"=y.origin + lmy+lh-dims.chr.legend/2,
                                                                             "x.end"=x.origin + lmx + 3*dims.chr.legend, "y.end"=y.origin + lmy+lh-dims.chr.legend/2+1*dims.chr.legend,
                                                                             "x.string"=lmx + 4*dims.chr.legend, "y.string"=lmy+lh,
                                                                             "font.size"=dims.chr.legend));
            }
            lh <- lh + lnl*dims.chr.legend; lw <- max(lw, .SVG.string.dims("theor. obs. wnd", font_size=dims.chr.legend)["width"] + 4*dims.chr.legend);
            if( do.plot )
            {
              l2 <- c(l2,
                      .SVG.rect(x=x.origin + lmx, y=y.origin + lmy+lh-dims.chr.legend/2, width=3*dims.chr.legend, height=1*dims.chr.legend,
                                stroke="none", fill=observation.window.col, fill_opacity=observation.window.opacity,
                                class="legend-ow-real"),
                      .SVG.text(x=x.origin + lmx + 4*dims.chr.legend, y=y.origin + lmy+lh, text="real obs. wnd.",
                                col="black", font_size=dims.chr.legend, h.align="left", v.align="center",
                                class="legend-ow-real", suppress.warnings=suppress.warnings));
              # Save the info:
              .last.cma.plot.info$SVG$legend$components <<- rbind(.last.cma.plot.info$SVG$legend$components,
                                                                  data.frame("string"="real obs. wnd.",
                                                                             "x.start"=x.origin + lmx, "y.start"=y.origin + lmy+lh-dims.chr.legend/2,
                                                                             "x.end"=x.origin + lmx + 3*dims.chr.legend, "y.end"=y.origin + lmy+lh-dims.chr.legend/2+1*dims.chr.legend,
                                                                             "x.string"=lmx + 4*dims.chr.legend, "y.string"=lmy+lh,
                                                                             "font.size"=dims.chr.legend));
            }
            lh <- lh + lnl*dims.chr.legend; lw <- max(lw, .SVG.string.dims("real obs. wnd.", font_size=dims.chr.legend)["width"] + 4*dims.chr.legend);
          } else
          {
            if( do.plot )
            {
              l2 <- c(l2,
                      .SVG.rect(x=x.origin + lmx, y=y.origin + lmy+lh-dims.chr.legend/2, width=3*dims.chr.legend, height=1*dims.chr.legend,
                                stroke="none", fill=observation.window.col, fill_opacity=observation.window.opacity,
                                class="legend-ow-rect"),
                      .SVG.text(x=x.origin + lmx + 4*dims.chr.legend, y=y.origin + lmy+lh, text="observation wnd.",
                                col="black", font_size=dims.chr.legend, h.align="left", v.align="center",
                                class="legend-ow-label", suppress.warnings=suppress.warnings));
              # Save the info:
              .last.cma.plot.info$SVG$legend$components <<- rbind(.last.cma.plot.info$SVG$legend$components,
                                                                  data.frame("string"="observation wnd.",
                                                                             "x.start"=x.origin + lmx, "y.start"=y.origin + lmy+lh-dims.chr.legend/2,
                                                                             "x.end"=x.origin + lmx + 3*dims.chr.legend, "y.end"=y.origin + lmy+lh-dims.chr.legend/2+1*dims.chr.legend,
                                                                             "x.string"=lmx + 4*dims.chr.legend, "y.string"=lmy+lh,
                                                                             "font.size"=dims.chr.legend));
            }
            lh <- lh + lnl*dims.chr.legend; lw <- max(lw, .SVG.string.dims("duration", font_size=dims.chr.legend)["width"] + 4*dims.chr.legend);
          }
        }

        # The legend background:
        lbox <- .SVG.rect(x=x.origin, y=y.origin, width=lw+2*lmx, height=lh+2*lmy, stroke="gray60", stroke_width=2, fill="gray99", fill_opacity=legend.bkg.opacity, class="legend-background");

        if( !do.plot )
        {
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
        }

        if( do.plot )
        {
          # Close the legend:
          l2 <- c(l2,
                  '</g>\n');
        }

        # Save the info:
        .last.cma.plot.info$SVG$legend$box <<- data.frame("x.start"=x, "y.start"=y, "x.end"=x+lw+2*lmx, "y.end"=y+lh+2*lmy);

        if( do.plot )
        {
          # Insert the legend background where it should be:
          return (c(l1, lbox, l2));
        } else
        {
          return (NULL);
        }
      }
    }
  }

  # Is the cma a time series or per episodes?
  is.cma.TS.or.SW <- (inherits(cma, "CMA_per_episode") || inherits(cma, "CMA_sliding_window"));
  # Does the cma contains estimated CMAs?
  has.estimated.CMA <- !is.null(getCMA(cma));

  # Convert data.table to data.frame (basically, to guard against inconsistencies between data.table and data.frame in how they handle d[,i]):
  if( inherits(cma$data, "data.table") ) cma$data <- as.data.frame(cma$data);

  # Check compatibility between subtypes of plots:
  if( align.all.patients && show.period != "days" ){ show.period <- "days"; if( !suppress.warnings ) .report.ewms("When aligning all patients, cannot show actual dates: showing days instead!\n", "warning", ".plot.CMAs", "AdhereR"); }

  #
  # Cache useful column names ####
  #
  cma.mg <- !is.null(cma$medication.groups); # are there medication groups?
  col.patid <- cma$ID.colname; # patient ID
  if( !cma.mg )
  {
    col.plotid <- col.patid; # when no medication groups, the plotting ID is the same the patient ID
  } else
  {
    col.mg <- cma$medication.groups.colname;
    col.plotid <- paste0("__",col.patid, ":", col.mg,"__"); # when there are medication groups, the plotting ID is patient ID concatenated with the medication group
  }
  cma.data <- cma$data; # the original data

  # Given a patient ID and medication group name, form the display label:
  if( cma.mg )
  {
    .mg.label <- function(patients, medication.groups)
    {
      paste0(patients," [",ifelse(medication.groups=="__ALL_OTHERS__", medication.groups.allother.label, medication.groups),"]");
    }
  }

  #
  # If CMA8, cache the real observation windows if it is to be plotted ####
  #
  if( inherits(cma,"CMA8") && !is.null(cma$real.obs.window) && show.real.obs.window.start )
  {
    cma.realOW <- cma$real.obs.window;
  } else
  {
    cma.realOW <- NULL;
  }

  #
  # Medication groups: expand the data and keep only those patient x group that contain events ####
  #
  if( cma.mg )
  {
    # Expand the data to contain all the patient x groups:
    cma$data <- do.call(rbind, lapply(1:ncol(cma$medication.groups$obs), function(i)
    {
      if( !is.null(medication.groups.to.plot) && length(medication.groups.to.plot) > 0 &&
          !(colnames(cma$medication.groups$obs)[i] %in% medication.groups.to.plot) )
      {
        # Not all medication groups should be plotted and this is one of them!
        return (NULL);
      }
      tmp <- cma$data[cma$medication.groups$obs[,i],];
      if( is.null(tmp) || nrow(tmp) == 0 )
      {
        return (NULL);
      } else
      {
        tmp <- cbind(tmp, colnames(cma$medication.groups$obs)[i]); names(tmp)[ncol(tmp)] <- col.mg;
        return (tmp);
      }
    }));

    # Keep only those that actually have events and that should be plotted:
    patmgids <- unique(cma$data[,c(col.patid, col.mg)]);
    if( is.null(patmgids) || nrow(patmgids) == 0 )
    {
      # Nothing to plot!
      if( !suppress.warnings ) .report.ewms("No patients to plot!\n", "error", ".plot.CMAs", "AdhereR");
      plot.CMA.error(export.formats=export.formats,
                     export.formats.fileprefix=export.formats.fileprefix,
                     export.formats.directory=export.formats.directory,
                     generate.R.plot=generate.R.plot);
      return (invisible(NULL));
    }

    # Add the new column containing the patient ID and the medication group for plotting:
    cma$data <- cbind(cma$data, .mg.label(cma$data[,col.patid], cma$data[,col.mg])); names(cma$data)[ncol(cma$data)] <- col.plotid;
    patmgids <- cbind(patmgids, .mg.label(patmgids[,col.patid], patmgids[,col.mg])); names(patmgids)[ncol(patmgids)] <- col.plotid;

    # The data should be already fine: focus on the CMA estimates:
    if( !is.null(cma$CMA) )
    {
      if( cma$flatten.medication.groups )
      {
        cma$CMA <- cma$CMA[ vapply(1:nrow(cma$CMA), function(i) any(cma$CMA[i,col.patid] == patmgids[,col.patid] & cma$CMA[i,col.mg] == patmgids[,col.mg]), logical(1)), ];
      } else
      {
        tmp <- lapply(1:length(cma$CMA), function(i)
        {
          tmp <- cma$CMA[[i]];
          if( is.null(tmp) ) return (NULL);
          tmp <- tmp[ tmp[,col.patid] %in% patmgids[patmgids[,col.mg] == names(cma$CMA)[i], col.patid], ];
          if( is.null(tmp) || nrow(tmp) == 0 ) return (NULL) else return (tmp);
        });
        names(tmp) <- names(cma$CMA); cma$CMA <- tmp;
      }
    }
    if( !is.null(cma.realOW) )
    {
      if( cma$flatten.medication.groups )
      {
        # Nothing to do, the real OW is already a data.frame!
      } else
      {
        # Flatten the OW:
        tmp <- do.call(rbind, cma.realOW);
        if( is.null(tmp) || nrow(tmp) == 0 )
        {
          cma.realOW <- NULL;
        } else
        {
          tmp <- cbind(tmp, unlist(lapply(1:length(cma.realOW), function(i) if(!is.null(cma.realOW[[i]])){rep(names(cma.realOW)[i], nrow(cma.realOW[[i]]))}else{NULL})));
          names(tmp)[ncol(tmp)] <- cma$medication.groups.colname; rownames(tmp) <- NULL;
          cma.realOW <- tmp;
        }
      }

      # Add the new column containing the patient ID and the medication group for plotting:
      cma.realOW <- cbind(cma.realOW, .mg.label(cma.realOW[,col.patid], cma.realOW[,col.mg])); names(cma.realOW)[ncol(cma.realOW)] <- col.plotid;

    }
  }


  #
  # Select patients ####
  #

  # The patients:
  patids <- unique(as.character(cma$data[,col.patid])); patids <- patids[!is.na(patids)];
  if( !is.null(patients.to.plot) ) patids <- intersect(patids, as.character(patients.to.plot));
  if( length(patids) == 0 )
  {
    if( !suppress.warnings ) .report.ewms("No patients to plot!\n", "error", ".plot.CMAs", "AdhereR");
    plot.CMA.error(export.formats=export.formats,
                   export.formats.fileprefix=export.formats.fileprefix,
                   export.formats.directory=export.formats.directory,
                   generate.R.plot=generate.R.plot);
    return (invisible(NULL));
  } else if( length(patids) > max.patients.to.plot )
  {
    if( !suppress.warnings ) .report.ewms(paste0("Too many patients to plot (",length(patids),
                                            ")! If this is the desired outcome, please change the 'max.patients.to.plot' parameter value (now set at ",
                                            max.patients.to.plot,") to at least ',length(patids),'!\n"), "error", ".plot.CMAs", "AdhereR");
    plot.CMA.error(export.formats=export.formats,
                   export.formats.fileprefix=export.formats.fileprefix,
                   export.formats.directory=export.formats.directory,
                   generate.R.plot=generate.R.plot);
    return (invisible(NULL));
  }

  # Select only the patients to display:
  cma <- subsetCMA(cma, patids);
  if( cma.mg )
  {
    patmgids <- patmgids[ patmgids[,col.patid] %in% patids, ];
  }


  ##
  ## Checks and conversions of various column types
  ##

  # Patient IDs and medical class better be characters:
  cma$data[, col.patid] <- as.character(cma$data[, col.patid]);
  if(!is.na(cma$medication.class.colname) && cma$medication.class.colname %in% names(cma$data))
  {
    cma$data[, cma$medication.class.colname] <- as.character(cma$data[, cma$medication.class.colname]);
  }


  #
  # Cache, consolidate and homogenise the needed info (events, CMAs, FUW an OW) ####
  #

  # Cache the CMA estimates (if any):
  if( !cma.mg )
  {
    # No medication groups:
    cmas <- getCMA(cma);
  } else
  {
    # There are medication groups:
    if( cma$flatten.medication.groups )
    {
      cmas <- getCMA(cma); cma.mg.colname <- col.mg;
    } else
    {
      cmas <- getCMA(cma, flatten.medication.groups=TRUE); cma.mg.colname <- names(cmas)[ncol(cmas)];
    }

    # Add the new column containing the patient ID and the medication group for plotting:
    if( !is.null(cmas) )
    {
      cmas <- cbind(cmas, .mg.label(cmas[,col.patid], cmas[,col.mg])); names(cmas)[ncol(cmas)] <- col.plotid;
    }
  }

  # Keep only those patients with non-missing CMA estimates:
  if( !is.null(cmas) )
  {
    if( inherits(cmas, "data.table") ) cmas <- as.data.frame(cmas); # same conversion to data.frame as above

    non_missing_cmas <- cmas[ !is.na(cmas[,"CMA"]), ]; non_missing_cma_patids <- unique(as.character(non_missing_cmas[,col.patid]));
    if( is.null(non_missing_cma_patids) || length(non_missing_cma_patids) == 0 )
    {
      if( !suppress.warnings ) .report.ewms("No patients with CMA estimates: nothing to plot!\n", "error", ".plot.CMAs", "AdhereR");
      plot.CMA.error(export.formats=export.formats,
                     export.formats.fileprefix=export.formats.fileprefix,
                     export.formats.directory=export.formats.directory,
                     generate.R.plot=generate.R.plot);
      return (invisible(NULL));
    }
  }

  # The patients that have no events to plot:
  patids.no.events.to.plot <- NULL;

  # Depending on the cma's exact type, the relevant columns might be different or even absent: homogenize them for later use
  if( inherits(cma, "CMA_per_episode") )
  {
    names(cmas)[2:7] <- c("WND.ID", "start", "gap.days", "duration", "end", "CMA"); # avoid possible conflict with patients being called "ID"

    # Remove the participants without CMA estimates:
    patids.no.events.to.plot <- setdiff(unique(cma$data[,col.patid]), unique(cmas[,col.patid]));
    if( length(patids.no.events.to.plot) > 0 )
    {
      cma$data <- cma$data[ !(cma$data[,col.patid] %in% patids.no.events.to.plot), ];
      #cma$data[ nrow(cma$data) + 1:length(patids.no.events.to.plot), col.patid ] <- patids.no.events.to.plot; # everything ese is NA except for the patient id
      if( !suppress.warnings ) .report.ewms(paste0("Patient",
                                                   ifelse(length(patids.no.events.to.plot) > 1, "s ", " "),
                                                   paste0("'",patids.no.events.to.plot, "'", collapse=", "),
                                                   ifelse(length(patids.no.events.to.plot) > 1, " have ", " has "), " no events to plot!\n"),
                                            "warning", ".plot.CMAs", "AdhereR");
    }
  } else if( inherits(cma, "CMA_sliding_window") )
  {
    cmas <- cbind(cmas[,1:3], "gap.days"=NA, "duration"=cma$sliding.window.duration, cmas[,4:ncol(cmas)]);
    names(cmas)[2:7] <- c("WND.ID", "start", "gap.days", "duration", "end", "CMA"); # avoid possible conflict with patients being called "ID"

    # Remove the participants without CMA estimates:
    patids.no.events.to.plot <- setdiff(unique(cma$data[,col.patid]), unique(cmas[,col.patid]));
    if( length(patids.no.events.to.plot) > 0 )
    {
      cma$data <- cma$data[ !(cma$data[,col.patid] %in% patids.no.events.to.plot), ];
      #cma$data[ nrow(cma$data) + 1:length(patids.no.events.to.plot), col.patid ] <- patids.no.events.to.plot; # everything ese is NA except for the patient id
      if( !suppress.warnings ) .report.ewms(paste0("Patient",
                                                   ifelse(length(patids.no.events.to.plot) > 1, "s ", " "),
                                                   paste0("'",patids.no.events.to.plot, "'", collapse=", "),
                                                   ifelse(length(patids.no.events.to.plot) > 1, " have ", " has "), " no events to plot!\n"),
                                            "warning", ".plot.CMAs", "AdhereR");
    }
  } else if( inherits(cma, "CMA0") && is.null(cma$event.info) )
  {
    # Try to compute the event.info:
    if( !cma.mg )
    {
      # No medication groups:
      event.info <- compute.event.int.gaps(data=cma$data,
                                           ID.colname=col.patid,
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
        patids.all <- unique(event.info[,col.patid]);
        event.info <- event.info[ !(event.info$.DATE.as.Date.end < event.info$.FU.START.DATE | event.info$.DATE.as.Date > event.info$.FU.END.DATE), ];
        if( is.null(event.info) || nrow(event.info) == 0 )
        {
          if( !suppress.warnings ) .report.ewms("No events in the follow-up window: nothing to plot!\n", "error", ".plot.CMAs", "AdhereR");
          plot.CMA.error(export.formats=export.formats,
                         export.formats.fileprefix=export.formats.fileprefix,
                         export.formats.directory=export.formats.directory,
                         generate.R.plot=generate.R.plot);
          return (invisible(NULL));
        }
        patids.no.events.to.plot <- setdiff(patids.all, unique(event.info[,col.patid]));

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

        # For the patients without stuff to plot, replace their events by a fake single event:
        if( length(patids.no.events.to.plot) > 0 )
        {
          cma$data <- cma$data[ !(cma$data[,col.patid] %in% patids.no.events.to.plot), ];
          #cma$data[ nrow(cma$data) + 1:length(patids.no.events.to.plot), col.patid ] <- patids.no.events.to.plot; # everything ese is NA except for the patient id
          if( !suppress.warnings ) .report.ewms(paste0("Patient",
                                                       ifelse(length(patids.no.events.to.plot) > 1, "s ", " "),
                                                       paste0("'",patids.no.events.to.plot, "'", collapse=", "),
                                                       ifelse(length(patids.no.events.to.plot) > 1, " have ", " has "), " no events to plot!\n"),
                                                "warning", ".plot.CMAs", "AdhereR");
        }
      } else
      {
        if( !suppress.warnings ) .report.ewms("Error(s) concerning the follow-up and observation windows!\n", "error", ".plot.CMAs", "AdhereR");
        plot.CMA.error(export.formats=export.formats,
                       export.formats.fileprefix=export.formats.fileprefix,
                       export.formats.directory=export.formats.directory,
                       generate.R.plot=generate.R.plot);
        return (invisible(NULL));
      }
    } else
    {
      # There are medication groups:

      # Do what the simple CMAs do: compute the event.info!
      # The workhorse auxiliary function: For a given (subset) of data, compute the event intervals and gaps:
      .workhorse.function <- function(data=NULL,
                                      ID.colname=NULL,
                                      event.date.colname=NULL,
                                      event.duration.colname=NULL,
                                      event.daily.dose.colname=NULL,
                                      medication.class.colname=NULL,
                                      event.interval.colname=NULL,
                                      gap.days.colname=NULL,
                                      carryover.within.obs.window=NULL,
                                      carryover.into.obs.window=NULL,
                                      carry.only.for.same.medication=NULL,
                                      consider.dosage.change=NULL,
                                      followup.window.start=NULL,
                                      followup.window.start.unit=NULL,
                                      followup.window.duration=NULL,
                                      followup.window.duration.unit=NULL,
                                      observation.window.start=NULL,
                                      observation.window.start.unit=NULL,
                                      observation.window.duration=NULL,
                                      observation.window.duration.unit=NULL,
                                      date.format=NULL,
                                      suppress.warnings=NULL
      )
      {
        # Call the compute.event.int.gaps() function and use the results:
        event.info <- compute.event.int.gaps(data=as.data.frame(data),
                                             ID.colname=ID.colname,
                                             event.date.colname=event.date.colname,
                                             event.duration.colname=event.duration.colname,
                                             event.daily.dose.colname=event.daily.dose.colname,
                                             medication.class.colname=medication.class.colname,
                                             event.interval.colname=event.interval.colname,
                                             gap.days.colname=gap.days.colname,
                                             carryover.within.obs.window=carryover.within.obs.window,
                                             carryover.into.obs.window=carryover.into.obs.window,
                                             carry.only.for.same.medication=carry.only.for.same.medication,
                                             consider.dosage.change=consider.dosage.change,
                                             followup.window.start=followup.window.start,
                                             followup.window.start.unit=followup.window.start.unit,
                                             followup.window.duration=followup.window.duration,
                                             followup.window.duration.unit=followup.window.duration.unit,
                                             observation.window.start=observation.window.start,
                                             observation.window.start.unit=observation.window.start.unit,
                                             observation.window.duration=observation.window.duration,
                                             observation.window.duration.unit=observation.window.duration.unit,
                                             date.format=date.format,
                                             keep.window.start.end.dates=TRUE,
                                             parallel.backend="none", # make sure this runs sequentially!
                                             parallel.threads=1,
                                             suppress.warnings=suppress.warnings,
                                             return.data.table=TRUE);
        if( is.null(event.info) ) return (list("CMA"=NA, "event.info"=NULL));

        return (list("CMA"=NULL, "event.info"=event.info));
      }

      tmp <- .cma.skeleton(data=cma.data,
                           ret.val=cma,
                           cma.class.name=c("CMA0"),

                           ID.colname=col.patid,
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

                           flatten.medication.groups=cma$flatten.medication.groups,
                           followup.window.start.per.medication.group=cma$followup.window.start.per.medication.group,

                           suppress.warnings=suppress.warnings,
                           force.NA.CMA.for.failed.patients=TRUE, # force the failed patients to have NA CMA estimates
                           parallel.backend="none", # make sure this runs sequentially!
                           parallel.threads=1,
                           .workhorse.function=.workhorse.function);
      cma$event.info <- tmp$event.info;

    }
  } else
  {
    # Remove the participants without CMA estimates:
    patids.no.events.to.plot <- setdiff(unique(cmas[, col.patid ]), unique(cmas[ !is.na(cmas$CMA), col.patid ]));
    if( length(patids.no.events.to.plot) > 0 )
    {
      cma$data <- cma$data[ !(cma$data[,col.patid] %in% patids.no.events.to.plot), ];
      #cma$data[ nrow(cma$data) + 1:length(patids.no.events.to.plot), col.patid ] <- patids.no.events.to.plot; # everything else is NA except for the patient id
      cmas <- cmas[ !(cmas[,col.patid] %in% patids.no.events.to.plot),  ]
      if( !suppress.warnings ) .report.ewms(paste0("Patient",
                                                   ifelse(length(patids.no.events.to.plot) > 1, "s ", " "),
                                                   paste0("'",patids.no.events.to.plot, "'", collapse=", "),
                                                   ifelse(length(patids.no.events.to.plot) > 1, " have ", " has "), " no events to plot!\n"),
                                            "warning", ".plot.CMAs", "AdhereR");
    }
  }

  # Cache the event.info:
  if( !cma.mg )
  {
    # No medication groups:
    evinfo <- getEventInfo(cma);
  } else
  {
    # There are medication groups:
    if( cma$flatten.medication.groups )
    {
      evinfo <- getEventInfo(cma); evinfo.mg.colname <- col.mg;
    } else
    {
      evinfo <- getEventInfo(cma, flatten.medication.groups=TRUE); evinfo.mg.colname <- names(evinfo)[ncol(evinfo)];
    }
  }

  # Add the follow-up and observation window info as well, to have everything in one place:
  if( !is.null(cmas) )
  {
    cmas <- cbind(cmas, do.call(rbind, lapply(1:nrow(cmas), function(i)
    {
      if( !cma.mg )
      {
        s <- which(evinfo[,col.patid] == cmas[i,col.patid]);
      } else
      {
        s <- which(evinfo[,col.patid] == cmas[i,col.patid] & evinfo[,evinfo.mg.colname] == cmas[i,cma.mg.colname]);
      }
      if( length(s) == 0 ) return(data.frame(".FU.START.DATE"=NA, ".FU.END.DATE"=NA, ".OBS.START.DATE"=NA, ".OBS.END.DATE"=NA)); #return (NULL);
      evinfo[s[1],c(".FU.START.DATE", ".FU.END.DATE", ".OBS.START.DATE", ".OBS.END.DATE")];
    })));
  } else
  {
    # Create a fake one, containing but the follow-up and observation window info:
    if( !cma.mg )
    {
      # No medication grops:
      cmas <- data.frame("..patid.."=unique(cma$data[,col.patid]), "CMA"=NA); names(cmas)[1] <- col.patid;
      if( !is.null(evinfo) )
      {
        cmas <- merge(cmas,
                      unique(evinfo[,c(col.patid, ".FU.START.DATE", ".FU.END.DATE", ".OBS.START.DATE", ".OBS.END.DATE")]),
                      by=c(col.patid), all.x=TRUE);
      } else
      {
        cmas <- cbind(cmas, ".FU.START.DATE"=NA, ".FU.END.DATE"=NA, ".OBS.START.DATE"=NA, ".OBS.END.DATE"=NA);
      }
    } else
    {
      # There are medication groups:
      cmas <- cbind(unique(cma$data[,c(col.patid, col.mg)]), "CMA"=NA);
      if( !is.null(evinfo) )
      {
        cmas <- merge(cmas,
                      unique(evinfo[,c(col.patid, col.mg, ".FU.START.DATE", ".FU.END.DATE", ".OBS.START.DATE", ".OBS.END.DATE")]),
                      by=c(col.patid, col.mg), all.x=TRUE);
      } else
      {
        cmas <- cbind(cmas, ".FU.START.DATE"=NA, ".FU.END.DATE"=NA, ".OBS.START.DATE"=NA, ".OBS.END.DATE"=NA);
      }

      # Add the new column containing the patient ID and the medication group for plotting:
      cmas <- cbind(cmas, .mg.label(cmas[,col.patid], cmas[,col.mg])); names(cmas)[ncol(cmas)] <- col.plotid;
    }
  }

  # Make sure the dates are cached as `Date` objects:
  if( !inherits(cma$data[,cma$event.date.colname], "Date") )
  {
    if( is.na(cma$date.format) || is.null(cma$date.format) || length(cma$date.format) != 1 || !is.character(cma$date.format) )
    {
      if( !suppress.warnings ) .report.ewms("The date format must be a single string: cannot continue plotting!\n", "error", ".plot.CMAs", "AdhereR");
      plot.CMA.error(export.formats=export.formats,
                     export.formats.fileprefix=export.formats.fileprefix,
                     export.formats.directory=export.formats.directory,
                     generate.R.plot=generate.R.plot);
      return (invisible(NULL));
    }

    # Convert them to Date:
    cma$data$.DATE.as.Date <- as.Date(cma$data[,cma$event.date.colname], format=cma$date.format);
    if( anyNA(cma$data$.DATE.as.Date) )
    {
      if( !suppress.warnings ) .report.ewms(paste0("Not all entries in the event date \"",cma$event.date.colname,"\" column are valid dates or conform to the date format \"",cma$date.format,"\"; first issue occurs on row ",min(which(is.na(cma$data$.DATE.as.Date))),": cannot continue plotting!\n"), "error", ".plot.CMAs", "AdhereR");
      plot.CMA.error(export.formats=export.formats,
                     export.formats.fileprefix=export.formats.fileprefix,
                     export.formats.directory=export.formats.directory,
                     generate.R.plot=generate.R.plot);
      return (invisible(NULL));
    }
  } else
  {
    # Just make a copy:
    cma$data$.DATE.as.Date <- cma$data[,cma$event.date.colname];
  }

  # Make sure the patients are ordered by ID, medication group (if the case), and date:
  patids <- patids[ order(patids) ];
  if( !cma.mg )
  {
    cma$data <- cma$data[ order( cma$data[,col.patid], cma$data$.DATE.as.Date), ];
  } else
  {
    cma$data <- cma$data[ order( cma$data[,col.patid], cma$data[,col.mg], cma$data$.DATE.as.Date), ];
    patmgids <- patmgids[ order( patmgids[,col.patid], patmgids[,col.mg]), ];
  }
  if( all(c("WND.ID","start") %in% names(cmas)) )
  {
    cmas <- cmas[ order( cmas[,col.patid], cmas$WND.ID, cmas$start), ];
  } else
  {
    cmas <- cmas[ order( cmas[,col.patid]), ];
  }


  #
  # Colors for plotting ####
  #

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
  .map.category.to.color <- function(category, cols.array=cols) ifelse( is.na(category), cols.array[1], ifelse( category %in% names(cols.array), cols.array[category], "black") );

  if( .do.SVG )
  {
    # Map category names to standardized category ids to be stored as class attributes; this mapping will be exported as a JavaScript dictionary in the HTML container(if any):
    categories.to.classes <- paste0("med-class-",1:length(categories)); names(categories.to.classes) <- categories;
    .map.category.to.class <- function(category, cat2class=categories.to.classes) ifelse( is.na(category), cat2class[1],
                                                                                          ifelse( category %in% names(cat2class), cat2class[category],
                                                                                                  cat2class[1]) );
  }


  #
  # Doses ####
  #

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


  #
  # Event dates and durations ####
  #

  # Find the earliest date:
  earliest.date <- min(cma$data$.DATE.as.Date, if( "start" %in% names(cmas) ) cmas$start, cmas$.OBS.START.DATE, cmas$.FU.START.DATE, na.rm=TRUE);

  # If aligning all participants to the same date, simply relocate all dates relative to the earliest date:
  if( align.all.patients )
  {
    # ASSUMPTIONS: the data is sorted by patient ID and (ascending) by event date
    for( i in 1:nrow(cma$data) )
    {
      # For each event in the dataset:
      if( i == 1 || cma$data[i,col.patid] != cma$data[i-1,col.patid] )
      {
        # It's a new patient (or the first one):

        # We will align to the patient's first event:
        align.to <- cma$data$.DATE.as.Date[i];

        # Adjust the dates in the cmas as well:
        for( j in which(cmas[,col.patid] == cma$data[i,col.patid]) )
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
    correct.earliest.followup.window <- as.numeric(min(cma$data$.DATE.as.Date - min(cmas$.FU.START.DATE,na.rm=TRUE),na.rm=TRUE));
  } else
  {
    # There is no correction to the earliest follow-up window date:
    correct.earliest.followup.window <- 0;
  }

  # Compute the duration if not given:
  if( is.na(duration) )
  {
    latest.date <- max(cma$data$.DATE.as.Date + cma$data[,cma$event.duration.colname], cmas$.FU.END.DATE, cmas$.OBS.END.DATE, na.rm=TRUE);
    if( "end" %in% names(cmas) ) latest.date <- max(cmas$end, latest.date, na.rm=TRUE);
    duration <- as.numeric(latest.date - earliest.date) + correct.earliest.followup.window;
  }
  endperiod <- duration;


  #
  # Reserve plotting space for various components ####
  #

  # There may be a difference between patids and plotids, depending on the medication groups being defined or not:
  if( !cma.mg )
  {
    plotids <- patids;
  } else
  {
    plotids <- unique(patmgids[, col.plotid]);
  }

  # Reserve space for the CMA plotting:
  adh.plot.space <- c(0, ifelse( plot.CMA && has.estimated.CMA, duration*CMA.plot.ratio, 0) );
  duration.total <- duration + adh.plot.space[2];

  # Make sure there's enough space to actually plot the plot IDs on the y-axis:
  id.labels <- do.call(rbind,lapply(as.character(plotids), # for each plot ID, compute the string dimensions in inches
                                    function(p)
                                    {
                                      # The participant axis text:
                                      pid <- ifelse( print.CMA &&
                                                       !is.cma.TS.or.SW &&
                                                       has.estimated.CMA &&
                                                       length(x <- which(cmas[col.plotid] == p))==1,
                                                     paste0(p,"\n",sprintf("%.1f%%",cmas[x,"CMA"]*100)),
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

  ## Vertical space needed by the events ####
  vert.space.events <- ifelse(plot.events.vertically.displaced, # are the events for the same patient displayed on different rows?
                              nrow(cma$data), # if yes, we need space for all individual events
                              length(unique(cma$data[,col.plotid]))); # otherwise, we only needs space for each patient

  # Vertical space needed for showing the partial CMAs:
  vert.space.cmas <- 0;
  if( is.cma.TS.or.SW )
  {
    # There actually is a partial CMA to be potentially plotted:
    if( ("timeseries" %in% plot.partial.CMAs.as) && (plot.partial.CMAs.as.timeseries.vspace < 5) )
    {
      if( !suppress.warnings ) .report.ewms(paste0("The minimum vertical space for the timeseries plots (plot.partial.CMAs.as.timeseries.vspace) is 5 lines, but it currently is only ",
                                                   plot.partial.CMAs.as.timeseries.vspace,
                                                   ": skipping timeseries plots...\n"), "warning", ".plot.CMAs", "AdhereR");
      plot.partial.CMAs.as <- plot.partial.CMAs.as[ plot.partial.CMAs.as != "timeseries" ];
    }

    vert.space.cmas <- vert.space.cmas +
      ifelse(has.estimated.CMA,
             (nrow(cmas)+length(plotids)) * as.numeric("stacked" %in% plot.partial.CMAs.as) +
               3 * length(plotids) * as.numeric("overlapping" %in% plot.partial.CMAs.as) +
               plot.partial.CMAs.as.timeseries.vspace * length(plotids) * as.numeric("timeseries" %in% plot.partial.CMAs.as),
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
        # Correctly deal with events starting before the FUW (i.e., correct.earliest.followup.window < 0):
        xpos <- c(correct.earliest.followup.window - seq(0, correct.earliest.followup.window, by=period.in.days * sign(correct.earliest.followup.window)),
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


  #
  # SVG definitions and setup ####
  #

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
    dims.day              <- ifelse(duration.total <= 90, 1, ifelse(duration.total <= 365, 7, ifelse(duration.total <= 3*365, 30, ifelse(duration.total <= 10*365, 90, 180)))); # how many days correspond to one horizontal user unit (depends on how many days there are in total)
    dims.axis.x           <- dims.chr.std + dims.chr.lab +
      (cos(-rotate.text*pi/180) * max(vapply(as.character(date.labels$string), function(s) .SVG.string.dims(s, font_size=dims.chr.axis)["width"], numeric(1)),na.rm=TRUE));
    dims.axis.y           <- dims.chr.std + dims.chr.lab +
      (sin(-rotate.text*pi/180) * max(vapply(as.character(id.labels$string), function(s) .SVG.string.dims(s, font_size=dims.chr.axis)["width"], numeric(1)),na.rm=TRUE));
    dims.plot.x           <- (dims.axis.y + dims.chr.std);
    dims.plot.y           <- (dims.chr.title + dims.chr.std);
    dims.plot.width       <- (dims.event.x * (duration.total + 10)/dims.day);
    dims.plot.height      <- (dims.event.y * (vert.space.events+vert.space.cmas+1));

    # For the legend, we force a call to the .legend.SVG() to get the legend needed size:
    if( !show.legend )
    {
      dims.legend.width     <- 0; # no legend to show
      dims.legend.height    <- 0;
    } else
    {
      .last.cma.plot.info <- list(); # create a fake .last.cma.plot.info because .legend.SVG() stores the results in it (it will be re-created later)
      .legend.SVG(legend.x, legend.y, do.plot=FALSE); # estimate the needed spaces
      dims.legend.width     <- (.last.cma.plot.info$SVG$legend$box$x.end + dims.chr.std); # retrieve the right-most and top-most corner of the legend
      dims.legend.height    <- (.last.cma.plot.info$SVG$legend$box$y.end - .last.cma.plot.info$SVG$legend$box$y.start + dims.chr.std);
    }

    # Total size needed:
    dims.total.width      <- (dims.plot.x + max(dims.plot.width,  dims.legend.width));
    dims.total.height     <- (dims.plot.y + max(dims.plot.height, dims.legend.height) + dims.axis.x);

    # Do we need to adjust for an extra large legend?
    dims.adjust.for.tall.legend <- max(0, dims.legend.height - dims.plot.height);

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
      return (dims.plot.y + dims.plot.height + dims.adjust.for.tall.legend - .scale.height.to.SVG.plot(y));
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
                 '<svg ',
                 'viewBox="0 0 ',dims.total.width,' ',dims.total.height,'" ',
                 ' version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">\n'); # the plotting surface

    # Comments, notes and clarifications:
    svg.str <- c(svg.str,
                 .SVG.comment("This is the self-contained SVG plot.", newpara=TRUE));
    svg.str <- c(svg.str,
                 .SVG.comment("NOTE: due to compatilibity issues with Internet Explorer, we use explicit closing tags."));

    # Reusable bits:
    dce1 <- .SVG.number(dims.chr.event); dce2 <- .SVG.number(dims.chr.event/2); ndce2 <- .SVG.number(-dims.chr.event/2); dce3 <- .SVG.number(dims.chr.event/3); dce4 <- .SVG.number(dims.chr.event/4); # cache the various relative sizes used to draw the pch symbols
    svg.str <- c(svg.str,
                 # Predefined things to be used in the drawing:
                 '<defs>\n',

                 # The point symbols (pch) used for events etc:
                 # (we use explicit tag closing as otherwise Internet Explorer generates warning HTML1500)
                 # pch 0:
                 '<g id="pch0" fill="none" stroke-width="1"> <rect x="',ndce2,'" y="',ndce2,'" width="',dce1,'" height="',dce1,'"></rect> </g>\n',
                 # pch 1:
                 '<g id="pch1" fill="none" stroke-width="1"> <circle cx="0" cy="0" r="',dce2,'"></circle> </g>\n',
                 # pch 2:
                 '<g id="pch2" fill="none" stroke-width="1"> <polyline points="',ndce2,',',dce2,' 0,',ndce2,' ',dce2,',',dce2,' ',ndce2,',',dce2,'"></polyline> </g>\n',
                 # pch 3:
                 '<g id="pch3" fill="none" stroke-width="1"> <line x1="',ndce2,'" y1="0" x2="',dce2,'" y2="0"></line> <line x1="0" y1="',ndce2,'" x2="0" y2="',dce2,'"></line> </g>\n',
                 # pch 4:
                 '<g id="pch4" fill="none" stroke-width="1"> <line x1="',ndce2,'" y1="',dce2,'" x2="',dce2,'" y2="',ndce2,'"></line> <line x1="',ndce2,'" y1="',ndce2,'" x2="',dce2,'" y2="',dce2,'"></line> </g>\n',
                 # pch 5:
                 '<g id="pch5" fill="none" stroke-width="1"> <polyline points="',ndce2,',0 0,',ndce2,' ',dce2,',0 0,',dce2,' ',ndce2,',0"></polyline> </g>\n',
                 # pch 6:
                 '<g id="pch6" fill="none" stroke-width="1"> <polyline points="',ndce2,',',ndce2,' 0,',dce2,' ',dce2,',',ndce2,' ',ndce2,',',ndce2,'"></polyline> </g>\n',
                 # pch 7:
                 '<g id="pch7" fill="none" stroke-width="1"> <use xlink:href="#pch0"></use> <use xlink:href="#pch4"></use> </g>\n',
                 # pch 8:
                 '<g id="pch8" fill="none" stroke-width="1"> <use xlink:href="#pch3"></use> <use xlink:href="#pch4"></use> </g>\n',
                 # pch 9:
                 '<g id="pch9" fill="none" stroke-width="1"> <use xlink:href="#pch3"></use> <use xlink:href="#pch5"></use> </g>\n',
                 # pch 10:
                 '<g id="pch10" fill="none" stroke-width="1"> <use xlink:href="#pch3"></use> <use xlink:href="#pch1"></use> </g>\n',
                 # pch 11:
                 '<g id="pch11" fill="none" stroke-width="1"> <use xlink:href="#pch2"></use> <use xlink:href="#pch6"></use> </g>\n',
                 # pch 12:
                 '<g id="pch12" fill="none" stroke-width="1"> <use xlink:href="#pch0"></use> <use xlink:href="#pch3"></use> </g>\n',
                 # pch 13:
                 '<g id="pch13" fill="none" stroke-width="1"> <use xlink:href="#pch1"></use> <use xlink:href="#pch4"></use> </g>\n',
                 # pch 14:
                 '<g id="pch14" fill="none" stroke-width="1"> <use xlink:href="#pch0"></use> <use xlink:href="#pch2"></use> </g>\n',
                 # pch 15:
                 '<g id="pch15" stroke-width="1"> <rect x="',ndce2,'" y="',ndce2,'" width="',dce1,'" height="',dce1,'"></rect> </g>\n',
                 # pch 16:
                 '<g id="pch16" stroke-width="1"> <circle cx="0" cy="0" r="',dce3,'"></circle> </g>\n',
                 # pch 17:
                 '<g id="pch17" stroke-width="1"> <polyline points="',ndce2,',',dce2,' 0,',ndce2,' ',dce2,',',dce2,' ',ndce2,',',dce2,'"></polyline> </g>\n',
                 # pch 18:
                 '<g id="pch18" stroke-width="1"> <polyline points="',ndce2,',0 0,',ndce2,' ',dce2,',0 0,',dce2,' ',ndce2,',0"></polyline> </g>\n',
                 # pch 19:
                 '<g id="pch19" stroke-width="1"> <circle cx="0" cy="0" r="',dce2,'"></circle> </g>\n',
                 # pch 20:
                 '<g id="pch20" stroke-width="1"> <circle cx="0" cy="0" r="',dce4,'"></circle> </g>\n',
                 # pch 26 ( < ):
                 '<g id="pch26" fill="none" stroke-width="1"> <polyline points="0,',dce2,' ',ndce2,',0 0,',ndce2,' "></polyline> </g>\n',
                 # pch 27 ( > ):
                 '<g id="pch27" fill="none" stroke-width="1"> <polyline points="0,',dce2,' ',dce2,',0 0,',ndce2,' "></polyline> </g>\n',
                 # pch 28 ( | ):
                 '<g id="pch28" fill="none" stroke-width="1"> <line x1="0" y1="',dce2,'" x2="0" y2="',ndce2,'"></line> </g>\n',

                 '</defs>\n',
                 '\n');
  }


  #
  # The actual plotting ####
  #

  # For speed and clarity, we use an internal version of .last.cma.plot.info, which we save into the external environment on exit...
  .last.cma.plot.info <- list("baseR"=NULL, "SVG"=NULL); # delete the previous plot info and replace it with empty info...

  if( .do.R ) # Rplot:
  {
    # The plotting area:
    if(inherits(msg <- try(plot( 0, 1,
                                 xlim=c(0-5,duration.total+5), # pad left and right by 5 days to improve plotting
                                 xaxs="i",
                                 ylim=c(0,vert.space.events+vert.space.cmas+1),
                                 yaxs="i",
                                 type="n",
                                 axes=FALSE,
                                 xlab="",
                                 ylab="" ),
                           silent=TRUE),
                "try-error"))
    {
      # Some error occurred when creating the plot...
      if( !suppress.warnings ) .report.ewms(msg, "error", ".plot.CMAs", "AdhereR");
      try(par(old.par), silent=TRUE); # restore graphical params
      #assign(".last.cma.plot.info", .last.cma.plot.info, envir=.adherer.env); # save the plot infor into the environment
      plot.CMA.error(export.formats=export.formats,
                     export.formats.fileprefix=export.formats.fileprefix,
                     export.formats.directory=export.formats.directory,
                     generate.R.plot=generate.R.plot);
      return (invisible(NULL));
    }

    # Make sure we're initially plotting on white:
    par(bg="white");

    # Character width and height in the current plotting system:
    if( print.dose ) dose.text.height <- strheight("0",cex=cex.dose);
    char.width <- strwidth("O",cex=cex); char.height <- strheight("O",cex=cex);
    char.height.CMA <- strheight("0",cex=CMA.cex);

    # Minimum plot dimensions:
    if( abs(par("usr")[2] - par("usr")[1]) <= char.width * min.plot.size.in.characters.horiz ||
        abs(par("usr")[4] - par("usr")[3]) <= char.height * min.plot.size.in.characters.vert * (vert.space.events + ifelse(is.cma.TS.or.SW && has.estimated.CMA, nrow(cmas), 0)) )
    {
      if( !suppress.warnings ) .report.ewms(paste0("Plotting area is too small (it must be at least ",
                                                   min.plot.size.in.characters.horiz,
                                                   " x ",
                                                   min.plot.size.in.characters.vert,
                                                   " characters per patient, but now it is only ",
                                                   round(abs(par("usr")[2] - par("usr")[1]) / char.width,1),
                                                   " x ",
                                                   round(abs(par("usr")[4] - par("usr")[3]) / (char.height * (vert.space.events +
                                                                                                                ifelse(is.cma.TS.or.SW && has.estimated.CMA, nrow(cmas), 0))),1),
                                                   ")!\n"), "error", ".plot.CMAs", "AdhereR");
      par(old.par); # restore graphical params
      #assign(".last.cma.plot.info", .last.cma.plot.info, envir=.adherer.env); # save the plot infor into the environment
      plot.CMA.error(export.formats=export.formats,
                     export.formats.fileprefix=export.formats.fileprefix,
                     export.formats.directory=export.formats.directory,
                     generate.R.plot=generate.R.plot);
      return (invisible(NULL));
    }

    if( abs(par("usr")[2] - par("usr")[1]) / duration.total < 1.0 && !suppress.warnings ) .report.ewms("The horizontal plotting space might be too small!", "warning", ".plot.CMAs", "AdhereR");
    if( abs(par("usr")[4] - par("usr")[3]) / (vert.space.events + ifelse(is.cma.TS.or.SW && has.estimated.CMA, nrow(cmas), 0)) < 1.0 && !suppress.warnings ) .report.ewms("The vertical plotting space might be too small!", "warning", ".plot.CMAs", "AdhereR");

    # Save plot info:
    .last.cma.plot.info$baseR <- list(
      # Function params:
      "patients.to.plot"=patients.to.plot,
      "align.all.patients"=align.all.patients, "align.first.event.at.zero"=align.first.event.at.zero,
      "show.period"=show.period,
      "period.in.days"=period.in.days,
      "show.legend"=show.legend, "legend.x"=legend.x, "legend.y"=legend.y,
      "legend.bkg.opacity"=legend.bkg.opacity, "legend.cex"=legend.cex, "legend.cex.title"=legend.cex.title,
      "cex"=cex, "cex.axis"=cex.axis, "cex.lab"=cex.lab, "cex.title"=cex.title,
      "show.cma"=show.cma,
      "xlab"=xlab, "ylab"=ylab,
      "title"=title,
      "col.cats"=col.cats, "unspecified.category.label"=unspecified.category.label,
      "medication.groups.to.plot"=medication.groups.to.plot,
      "lty.event"=lty.event, "lwd.event"=lwd.event, "pch.start.event"=pch.start.event, "pch.end.event"=pch.end.event,
      "show.event.intervals"=show.event.intervals,
      "print.dose"=print.dose, "cex.dose"=cex.dose, "print.dose.col"=print.dose.col, "print.dose.centered"=print.dose.centered,
      "plot.dose"=plot.dose, "lwd.event.max.dose"=lwd.event.max.dose, "plot.dose.lwd.across.medication.classes"=plot.dose.lwd.across.medication.classes,
      "col.na"=col.na, "col.continuation"=col.continuation, "lty.continuation"=lty.continuation, "lwd.continuation"=lwd.continuation,
      "print.CMA"=print.CMA, "CMA.cex"=CMA.cex,
      "plot.CMA"=plot.CMA, "plot.CMA.as.histogram"=plot.CMA.as.histogram,
      "plot.partial.CMAs.as"=plot.partial.CMAs.as,
      "plot.partial.CMAs.as.stacked.col.bars"=plot.partial.CMAs.as.stacked.col.bars,
      "plot.partial.CMAs.as.stacked.col.border"=plot.partial.CMAs.as.stacked.col.border,
      "plot.partial.CMAs.as.stacked.col.text"=plot.partial.CMAs.as.stacked.col.text,
      "plot.partial.CMAs.as.timeseries.vspace"=plot.partial.CMAs.as.timeseries.vspace,
      "plot.partial.CMAs.as.timeseries.start.from.zero"=plot.partial.CMAs.as.timeseries.start.from.zero,
      "plot.partial.CMAs.as.timeseries.col.dot"=plot.partial.CMAs.as.timeseries.col.dot,
      "plot.partial.CMAs.as.timeseries.col.interval"=plot.partial.CMAs.as.timeseries.col.interval,
      "plot.partial.CMAs.as.timeseries.col.text"=plot.partial.CMAs.as.timeseries.col.text,
      "plot.partial.CMAs.as.timeseries.interval.type"=plot.partial.CMAs.as.timeseries.interval.type,
      "plot.partial.CMAs.as.timeseries.lwd.interval"=plot.partial.CMAs.as.timeseries.lwd.interval,
      "plot.partial.CMAs.as.timeseries.alpha.interval"=plot.partial.CMAs.as.timeseries.alpha.interval,
      "plot.partial.CMAs.as.timeseries.show.0perc"=plot.partial.CMAs.as.timeseries.show.0perc,
      "plot.partial.CMAs.as.timeseries.show.100perc"=plot.partial.CMAs.as.timeseries.show.100perc,
      "plot.partial.CMAs.as.overlapping.alternate"=plot.partial.CMAs.as.overlapping.alternate,
      "plot.partial.CMAs.as.overlapping.col.interval"=plot.partial.CMAs.as.overlapping.col.interval,
      "plot.partial.CMAs.as.overlapping.col.text"=plot.partial.CMAs.as.overlapping.col.text,
      "CMA.plot.ratio"=CMA.plot.ratio,
      "CMA.plot.col"=CMA.plot.col, "CMA.plot.border"=CMA.plot.border, "CMA.plot.bkg"=CMA.plot.bkg, "CMA.plot.text"=CMA.plot.text,
      "highlight.followup.window"=highlight.followup.window, "followup.window.col"=followup.window.col,
      "highlight.observation.window"=highlight.observation.window,
      "observation.window.col"=observation.window.col,
      "observation.window.opacity"=observation.window.opacity,
      "show.real.obs.window.start"=show.real.obs.window.start,
      "alternating.bands.cols"=alternating.bands.cols,
      "rotate.text"=rotate.text,
      "bw.plot"=bw.plot,
      "min.plot.size.in.characters.horiz"=min.plot.size.in.characters.horiz, "min.plot.size.in.characters.vert"=min.plot.size.in.characters.vert,
      "export.formats"=export.formats, "export.formats.fileprefix"=export.formats.fileprefix, "export.formats.directory"=export.formats.directory,
      "generate.R.plot"=generate.R.plot,

      # Computed things:
      "old.par"=old.par,
      "used.par"=par(no.readonly=TRUE),
      "xlim"=c(0-5,duration.total+5), "ylim"=c(0,vert.space.events+vert.space.cmas+1),
      "x.min"=0, "x.max"=duration.total, "y.min"=1, "y.max"=vert.space.events+vert.space.cmas,
      "dose.text.height"=ifelse(print.dose, dose.text.height, NA),
      "char.width"=char.width, "char.height"=char.height,
      "char.height.CMA"=char.height.CMA,
      "is.cma.TS.or.SW"=is.cma.TS.or.SW, "has.estimated.CMA"=has.estimated.CMA,
      "cma"=cma, "cmas"=cmas,
      "categories"=categories, "cols"=cols, ".map.category.to.color"=.map.category.to.color,
      "earliest.date"=earliest.date, "correct.earliest.followup.window"=correct.earliest.followup.window, "endperiod"=endperiod,
      "adh.plot.space"=adh.plot.space, "duration.total"=duration.total,
      "id.labels"=id.labels, "date.labels"=date.labels, "x.label"=x.label, "y.label"=y.label,
      "vert.space.cmas"=vert.space.cmas
    );
    if(plot.dose || print.dose)
    {
      .last.cma.plot.info$baseR$dose.range <- dose.range;
      if( plot.dose.lwd.across.medication.classes ) .last.cma.plot.info$baseR$dose.range.global <- dose.range.global;
      .last.cma.plot.info$baseR$adjust.dose.lwd <- adjust.dose.lwd;
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

    # Save plot info:
    .last.cma.plot.info$SVG <- list(
      # Function params:
      "patients.to.plot"=patients.to.plot,
      "align.all.patients"=align.all.patients, "align.first.event.at.zero"=align.first.event.at.zero,
      "show.period"=show.period,
      "period.in.days"=period.in.days,
      "show.legend"=show.legend, "legend.x"=legend.x, "legend.y"=legend.y,
      "legend.bkg.opacity"=legend.bkg.opacity, "legend.cex"=legend.cex, "legend.cex.title"=legend.cex.title,
      "cex"=cex, "cex.axis"=cex.axis, "cex.lab"=cex.lab, "cex.title"=cex.title,
      "show.cma"=show.cma,
      "xlab"=xlab, "ylab"=ylab,
      "title"=title,
      "col.cats"=col.cats, "unspecified.category.label"=unspecified.category.label,
      "medication.groups.to.plot"=medication.groups.to.plot,
      "lty.event"=lty.event, "lwd.event"=lwd.event, "pch.start.event"=pch.start.event, "pch.end.event"=pch.end.event,
      "show.event.intervals"=show.event.intervals,
      "print.dose"=print.dose, "cex.dose"=cex.dose, "print.dose.col"=print.dose.col, "print.dose.centered"=print.dose.centered,
      "plot.dose"=plot.dose, "lwd.event.max.dose"=lwd.event.max.dose, "plot.dose.lwd.across.medication.classes"=plot.dose.lwd.across.medication.classes,
      "col.na"=col.na, "col.continuation"=col.continuation, "lty.continuation"=lty.continuation, "lwd.continuation"=lwd.continuation,
      "print.CMA"=print.CMA, "CMA.cex"=CMA.cex,
      "plot.CMA"=plot.CMA, "plot.CMA.as.histogram"=plot.CMA.as.histogram,
      "plot.partial.CMAs.as"=plot.partial.CMAs.as,
      "plot.partial.CMAs.as.stacked.col.bars"=plot.partial.CMAs.as.stacked.col.bars,
      "plot.partial.CMAs.as.stacked.col.border"=plot.partial.CMAs.as.stacked.col.border,
      "plot.partial.CMAs.as.stacked.col.text"=plot.partial.CMAs.as.stacked.col.text,
      "plot.partial.CMAs.as.timeseries.vspace"=plot.partial.CMAs.as.timeseries.vspace,
      "plot.partial.CMAs.as.timeseries.start.from.zero"=plot.partial.CMAs.as.timeseries.start.from.zero,
      "plot.partial.CMAs.as.timeseries.col.dot"=plot.partial.CMAs.as.timeseries.col.dot,
      "plot.partial.CMAs.as.timeseries.col.interval"=plot.partial.CMAs.as.timeseries.col.interval,
      "plot.partial.CMAs.as.timeseries.col.text"=plot.partial.CMAs.as.timeseries.col.text,
      "plot.partial.CMAs.as.timeseries.interval.type"=plot.partial.CMAs.as.timeseries.interval.type,
      "plot.partial.CMAs.as.timeseries.lwd.interval"=plot.partial.CMAs.as.timeseries.lwd.interval,
      "plot.partial.CMAs.as.timeseries.alpha.interval"=plot.partial.CMAs.as.timeseries.alpha.interval,
      "plot.partial.CMAs.as.timeseries.show.0perc"=plot.partial.CMAs.as.timeseries.show.0perc,
      "plot.partial.CMAs.as.timeseries.show.100perc"=plot.partial.CMAs.as.timeseries.show.100perc,
      "plot.partial.CMAs.as.overlapping.alternate"=plot.partial.CMAs.as.overlapping.alternate,
      "plot.partial.CMAs.as.overlapping.col.interval"=plot.partial.CMAs.as.overlapping.col.interval,
      "plot.partial.CMAs.as.overlapping.col.text"=plot.partial.CMAs.as.overlapping.col.text,
      "CMA.plot.ratio"=CMA.plot.ratio,
      "CMA.plot.col"=CMA.plot.col, "CMA.plot.border"=CMA.plot.border, "CMA.plot.bkg"=CMA.plot.bkg, "CMA.plot.text"=CMA.plot.text,
      "highlight.followup.window"=highlight.followup.window, "followup.window.col"=followup.window.col,
      "highlight.observation.window"=highlight.observation.window,
      "observation.window.col"=observation.window.col,
      "observation.window.opacity"=observation.window.opacity,
      "show.real.obs.window.start"=show.real.obs.window.start,
      "alternating.bands.cols"=alternating.bands.cols,
      "rotate.text"=rotate.text,
      "bw.plot"=bw.plot,
      "min.plot.size.in.characters.horiz"=min.plot.size.in.characters.horiz, "min.plot.size.in.characters.vert"=min.plot.size.in.characters.vert,
      "export.formats"=export.formats, "export.formats.fileprefix"=export.formats.fileprefix, "export.formats.directory"=export.formats.directory,
      "generate.R.plot"=generate.R.plot,

      # Computed things:
      "x"=0, "y"=0,
      "width"=dims.total.width, "height"=dims.total.height,
      "x.min"=0, "x.max"=duration.total, "y.min"=1, "y.max"=vert.space.events+vert.space.cmas,
      "dims.chr.std"=dims.chr.std,
      "dims.chr.event"=dims.chr.event,
      "dims.chr.title"=dims.chr.title,
      "dims.chr.axis"=dims.chr.axis,
      "dims.chr.lab"=dims.chr.lab,
      "dims.chr.cma"=dims.chr.cma,
      "dims.chr.legend"=dims.chr.legend,
      "dims.chr.legend.title"=dims.chr.legend.title,
      "dims.event.x"=dims.event.x,
      "dims.event.y"=dims.event.y,
      "dims.day"=dims.day,
      "dims.axis.x"=dims.axis.x,
      "dims.axis.y"=dims.axis.y,
      "dims.plot.x"=dims.plot.x,
      "dims.plot.y"=dims.plot.y,
      "dims.plot.width"=dims.plot.width,
      "dims.plot.height"=dims.plot.height,
      "dims.legend.width"=dims.legend.width,
      "dims.legend.height"=dims.legend.height,
      "dims.total.width"=dims.total.width,
      "dims.total.height"=dims.total.height,
      ".scale.width.to.SVG.plot"=.scale.width.to.SVG.plot,
      ".scale.x.to.SVG.plot"=.scale.x.to.SVG.plot,
      ".scale.height.to.SVG.plot"=.scale.height.to.SVG.plot,
      ".scale.y.to.SVG.plot"=.scale.y.to.SVG.plot,
      "svg.stroke.dasharrays"=svg.stroke.dasharrays,
      "is.cma.TS.or.SW"=is.cma.TS.or.SW, "has.estimated.CMA"=has.estimated.CMA,
      "cma"=cma, "cmas"=cmas,
      "categories"=categories, "cols"=cols, ".map.category.to.color"=.map.category.to.color,
      "categories.to.classes"=categories.to.classes, ".map.category.to.class"=.map.category.to.class,
      "earliest.date"=earliest.date, "correct.earliest.followup.window"=correct.earliest.followup.window, "endperiod"=endperiod,
      "adh.plot.space"=adh.plot.space, "duration.total"=duration.total,
      "id.labels"=id.labels, "date.labels"=date.labels, "x.label"=x.label, "y.label"=y.label,
      "vert.space.cmas"=vert.space.cmas
    );
    if(plot.dose || print.dose)
    {
      .last.cma.plot.info$SVG$dose.range <- dose.range;
      if( plot.dose.lwd.across.medication.classes ) .last.cma.plot.info$SVG$dose.range.global <- dose.range.global;
      .last.cma.plot.info$SVG$adjust.dose.lwd <- adjust.dose.lwd;
    }
  }

  # Functions mapping an event given as (days, row) to the plotting coordinates:
  .map.event.x <- function(x) { return (adh.plot.space[2] + x); }
  .map.event.date <- function(d, adjust.for.earliest.date=TRUE)
  {
    if( adjust.for.earliest.date )
    {
      return (as.numeric(d - earliest.date + adh.plot.space[2] + correct.earliest.followup.window));
    } else
    {
      return (as.numeric(d + adh.plot.space[2] + correct.earliest.followup.window));
    }
  }
  .map.event.y <- function(y) { return (y); }
  # Save plot info:
  if( .do.R )
  {
    .last.cma.plot.info$baseR$.map.event.x <- .map.event.x;
    .last.cma.plot.info$baseR$.map.event.date <- .map.event.date;
    .last.cma.plot.info$baseR$.map.event.y <- .map.event.y;
  }
  if( .do.SVG )
  {
    .last.cma.plot.info$SVG$.map.event.x <- .map.event.x;
    .last.cma.plot.info$SVG$.map.event.date <- .map.event.date;
    .last.cma.plot.info$SVG$.map.event.y <- .map.event.y;
  }

  # Function mapping the CMA values to the appropriate x-coordinates:
  if( plot.CMA && has.estimated.CMA )
  {
    adh.max <- ifelse(is.cma.TS.or.SW, 1.0, max(c(getCMA(cma)$CMA, 1.0),na.rm=TRUE)); # maximum achieved CMA (used for plotting, forced to 1.0 for PE and SW)
    .rescale.xcoord.for.CMA.plot <- function(x, pfree=0.20, plot.space=adh.plot.space, max.x=adh.max)
    {
      return (plot.space[1] + (x / max.x) * (plot.space[2] * (1-pfree) - plot.space[1]));
    }

    # Save plot info:
    if( .do.R )
    {
      .last.cma.plot.info$baseR$adh.max <- adh.max;
      .last.cma.plot.info$baseR$.rescale.xcoord.for.CMA.plot <- .rescale.xcoord.for.CMA.plot;
    }
    if( .do.SVG )
    {
      .last.cma.plot.info$SVG$adh.max <- adh.max;
      .last.cma.plot.info$SVG$.rescale.xcoord.for.CMA.plot <- .rescale.xcoord.for.CMA.plot;
    }
  }


  ##
  ## Plot most of the plot components ####
  ##

  # Initialisations
  y.cur <- 1; # the current vertical line at which plotting takes place
  alternating.band.to.draw <- 1; # for this patient, which alternating band to draw?

  # For each event in cma$data, as well as for each of the partial CMAs (if the case), record important plotting info
  if( .do.R )
  {
    .last.cma.plot.info$baseR$cma$data <- cbind(.last.cma.plot.info$baseR$cma$data,
                                                ".X.OW.START"=NA,   ".X.OW.END"=NA,   ".Y.OW.START"=NA,   ".Y.OW.END"=NA,   # observation window extension on the plot
                                                ".X.ROW.START"=NA,  ".X.ROW.END"=NA,  ".Y.ROW.START"=NA,  ".Y.ROW.END"=NA,  # "real" observation window extension on the plot
                                                ".X.FUW.START"=NA,  ".X.FUW.END"=NA,  ".Y.FUW.START"=NA,  ".Y.FUW.END"=NA,  # follow-up window extension on the plot
                                                ".X.START"=NA,      ".X.END"=NA,      ".Y.START"=NA,      ".Y.END"=NA,      # event extension on the plot
                                                ".EV.LWD"=NA, # the event's line width
                                                ".X.DOSE"=NA, ".Y.DOSE"=NA, ".FONT.SIZE.DOSE"=NA, # dose text position and size
                                                ".X.EVC.START"=NA,  ".X.EVC.END"=NA,  ".Y.EVC.START"=NA,  ".Y.EVC.END"=NA,  # event covered extension on the plot
                                                ".X.EVNC.START"=NA, ".X.EVNC.END"=NA, ".Y.EVNC.START"=NA, ".Y.EVNC.END"=NA, # event not covered extension on the plot
                                                ".X.CNT.START"=NA,  ".X.CNT.END"=NA,  ".Y.CNT.START"=NA,  ".Y.CNT.END"=NA,  # continuation lines extension on the plot
                                                ".X.SCMA.START"=NA, ".X.SCMA.END"=NA, ".Y.SCMA.START"=NA, ".Y.SCMA.END"=NA  # summary CMA extension on the plot
    );
    .last.cma.plot.info$baseR$partialCMAs <- NULL;
  }
  if( .do.SVG )
  {
    .last.cma.plot.info$SVG$cma$data <- cbind(.last.cma.plot.info$SVG$cma$data,
                                              ".X.OW.START"=NA,   ".X.OW.END"=NA,   ".Y.OW.START"=NA,   ".Y.OW.END"=NA,   # observation window extension on the plot
                                              ".X.ROW.START"=NA,  ".X.ROW.END"=NA,  ".Y.ROW.START"=NA,  ".Y.ROW.END"=NA,  # "real" observation window extension on the plot
                                              ".X.FUW.START"=NA,  ".X.FUW.END"=NA,  ".Y.FUW.START"=NA,  ".Y.FUW.END"=NA,  # follow-up window extension on the plot
                                              ".X.START"=NA,      ".X.END"=NA,      ".Y.START"=NA,      ".Y.END"=NA,      # event extension on the plot
                                              ".EV.LWD"=NA, # the event's line width
                                              ".X.DOSE"=NA, ".Y.DOSE"=NA, ".FONT.SIZE.DOSE"=NA, # dose text position and size
                                              ".X.EVC.START"=NA,  ".X.EVC.END"=NA,  ".Y.EVC.START"=NA,  ".Y.EVC.END"=NA,  # event covered extension on the plot
                                              ".X.EVNC.START"=NA, ".X.EVNC.END"=NA, ".Y.EVNC.START"=NA, ".Y.EVNC.END"=NA, # event not covered extension on the plot
                                              ".X.CNT.START"=NA,  ".X.CNT.END"=NA,  ".Y.CNT.START"=NA,  ".Y.CNT.END"=NA,  # continuation lines extension on the plot
                                              ".X.SCMA.START"=NA, ".X.SCMA.END"=NA, ".Y.SCMA.START"=NA, ".Y.SCMA.END"=NA  # summary CMA extension on the plot
    );
    .last.cma.plot.info$SVG$partialCMAs <- NULL;
  }

  # For each individual event in turn:
  alternating.band.mg.to.draw <- FALSE;
  y.old.mg <- y.cur;
  for( i in 1:nrow(cma$data) )
  {
    # The current plot ID:
    cur_plot_id <- cma$data[i,col.plotid];

    # For a new patient, draw the alternating bands, show the CMA and print the y-axis label:
    if( i == 1 || (cur_plot_id != cma$data[i-1,col.plotid]) )
    {
      # Save the current vertical position (for drawing the FUW and OW windows):
      y.old <- y.cur;

      # Select the events and partial CMAs belonging to this patient:
      s.events <- which(cma$data[,col.plotid] == cur_plot_id);
      s.cmas   <- which(cmas[,col.plotid]     == cur_plot_id);

      # Vertical space needed by this patient for the events and overall:
      vspace.needed.events <- ifelse(plot.events.vertically.displaced, length(s.events), 1);
      vspace.needed.partial.cmas <- ifelse(has.estimated.CMA,
                                           (length(s.cmas)+1) * as.numeric("stacked" %in% plot.partial.CMAs.as) +
                                             3 * as.numeric("overlapping" %in% plot.partial.CMAs.as) +
                                             plot.partial.CMAs.as.timeseries.vspace * as.numeric("timeseries" %in% plot.partial.CMAs.as),
                                           0);
      vspace.needed.total  <- vspace.needed.events + vspace.needed.partial.cmas;


      ##
      ## The alternating bands ####
      ##

      # Draw the alternating bands
      if( !is.null(alternating.bands.cols) )
      {
        if( .do.R ) # Rplot:
        {
          rect( 0.0 - 1.0, y.cur - 0.5, duration.total + 1.0, y.cur + vspace.needed.total - 0.5, col=alternating.bands.cols[alternating.band.to.draw], border=NA );
        }

        if( .do.SVG ) # SVG:
        {
          svg.str <- c(svg.str,
                       .SVG.rect(x=.scale.x.to.SVG.plot(0), y=.scale.y.to.SVG.plot(y.cur - 0.5 + vspace.needed.total),
                                 width=dims.plot.width, height=.scale.height.to.SVG.plot(vspace.needed.total),
                                 fill=alternating.bands.cols[alternating.band.to.draw],
                                 class=paste0("alternating-bands-",alternating.band.to.draw), comment="The alternating band")
          );
        }

        alternating.band.to.draw <- if(alternating.band.to.draw >= length(alternating.bands.cols)) 1 else (alternating.band.to.draw + 1); # move to the next band
      }
    }

    ##
    ## Medication groups within patients ####
    ##

    # Draw the separators over the alternating bands but bellow all other graphical elements:
    if( cma.mg &&
        medication.groups.separator.show &&
        (i == 1 || (i > 1 && cma$data[i,col.patid] != cma$data[i-1,col.patid]) || i == nrow(cma$data)) )
    {
      # The y coordinates:
      y.mg.start <- ifelse(i == nrow(cma$data), y.cur + vspace.needed.partial.cmas + 0.5, y.cur - 0.5);
      y.mg.end   <- (y.old.mg - 0.5);

      if( .do.R ) # Rplot:
      {
        # The separating line:
        segments(par("usr")[1], y.mg.start, par("usr")[2], y.mg.start,
                 col=medication.groups.separator.color, lty=medication.groups.separator.lty, lwd=medication.groups.separator.lwd);
        if( i > 1 && alternating.band.mg.to.draw )
        {
          rect( par("usr")[1],        y.mg.start, 0.0,           y.mg.end, col=medication.groups.separator.color, border=NA );
          rect( duration.total + 1.0, y.mg.start, par("usr")[2], y.mg.end, col=medication.groups.separator.color, border=NA );
        }
      }

      if( .do.SVG ) # SVG:
      {
        # Draw:
        svg.str <- c(svg.str,
                     # The separating line:
                     .SVG.lines(x=c(dims.plot.x, dims.plot.x+dims.plot.width),
                               y=rep(.scale.y.to.SVG.plot(y.mg.start),2),
                               connected=FALSE,
                               stroke=medication.groups.separator.color, lty=medication.groups.separator.lty, stroke_width=medication.groups.separator.lwd,
                               class="medication-groups-separator-hline", comment="Medication groups separator: horizontal line", suppress.warnings=suppress.warnings));
        if( i > 1 && alternating.band.mg.to.draw )
        {
          svg.str <- c(svg.str,
                       # The left and right lines:
                       .SVG.lines(x=c(dims.plot.x, dims.plot.x),
                                  y=c(.scale.y.to.SVG.plot(y.mg.start), .scale.y.to.SVG.plot(y.mg.end)),
                                  connected=FALSE,
                                  stroke=medication.groups.separator.color, lty=medication.groups.separator.lty, stroke_width=medication.groups.separator.lwd,
                                  class="medication-groups-separator-vline", comment="Medication groups separator: vertical lines", suppress.warnings=suppress.warnings),
                       .SVG.lines(x=c(dims.plot.x, dims.plot.x)+dims.plot.width,
                                  y=c(.scale.y.to.SVG.plot(y.mg.start), .scale.y.to.SVG.plot(y.mg.end)),
                                  connected=FALSE,
                                  stroke=medication.groups.separator.color, lty=medication.groups.separator.lty, stroke_width=medication.groups.separator.lwd,
                                  class="medication-groups-separator-vline", comment="Medication groups separator: vertical lines", suppress.warnings=suppress.warnings)
          );
        }
      }

      alternating.band.mg.to.draw <- !alternating.band.mg.to.draw;
      y.old.mg <- y.cur;
    }


    # Continue doing things for a new patient...
    if( i == 1 || (cur_plot_id != cma$data[i-1,col.plotid]) )
    {

      ##
      ## FUW and OW ####
      ##

      # The follow-up and observation windows (these are drawn only after all the other stuff for this patient has been drawn):
      if( highlight.followup.window )
      {
        if( .do.R ) # Rplot:
        {
          # Save the info:
          .last.cma.plot.info$baseR$cma$data[s.events,".X.FUW.START"] <- (adh.plot.space[2] + as.numeric(cmas$.FU.START.DATE[s.cmas[1]] - earliest.date) + correct.earliest.followup.window);
          .last.cma.plot.info$baseR$cma$data[s.events,".Y.FUW.START"] <- (y.cur - 0.5);
          .last.cma.plot.info$baseR$cma$data[s.events,".X.FUW.END"]   <- (adh.plot.space[2] + as.numeric(cmas$.FU.END.DATE[s.cmas[1]]   - earliest.date) + correct.earliest.followup.window);
          .last.cma.plot.info$baseR$cma$data[s.events,".Y.FUW.END"]   <- (y.cur + vspace.needed.events - 0.5);

          # Draw:
          rect(.last.cma.plot.info$baseR$cma$data[s.events[1],".X.FUW.START"], .last.cma.plot.info$baseR$cma$data[s.events[1],".Y.FUW.START"],
               .last.cma.plot.info$baseR$cma$data[s.events[1],".X.FUW.END"],   .last.cma.plot.info$baseR$cma$data[s.events[1],".Y.FUW.END"],
               col=NA, border=followup.window.col, lty="dashed", lwd=2);
        }

        if( .do.SVG ) # SVG:
        {
          # Save the info:
          .last.cma.plot.info$SVG$cma$data[s.events,".X.FUW.START"] <- .scale.x.to.SVG.plot(adh.plot.space[2] + as.numeric(cmas$.FU.START.DATE[s.cmas[1]] - earliest.date) + correct.earliest.followup.window);
          .last.cma.plot.info$SVG$cma$data[s.events,".Y.FUW.START"] <- .scale.y.to.SVG.plot(y.cur + vspace.needed.events - 0.5);
          .last.cma.plot.info$SVG$cma$data[s.events,".X.FUW.END"]   <- .scale.x.to.SVG.plot(adh.plot.space[2] + as.numeric(cmas$.FU.END.DATE[s.cmas[1]]   - earliest.date) + correct.earliest.followup.window);
          .last.cma.plot.info$SVG$cma$data[s.events,".Y.FUW.END"]   <- .scale.y.to.SVG.plot(y.cur + 0.5);

          # Draw:
          svg.str <- c(svg.str,
                       # FUW:
                       .SVG.rect(x=.last.cma.plot.info$SVG$cma$data[s.events[1],".X.FUW.START"], y=.last.cma.plot.info$SVG$cma$data[s.events[1],".Y.FUW.START"],
                                 width=.scale.width.to.SVG.plot(as.numeric(cmas$.FU.END.DATE[s.cmas[1]] - cmas$.FU.START.DATE[s.cmas[1]])),
                                 height=.scale.height.to.SVG.plot(vspace.needed.events),
                                 stroke=followup.window.col, stroke_width=2, lty="dashed", fill="white", fill_opacity=0.0, # fully transparent but tooltips also work
                                 class="fuw", comment="The Follow-Up Window (FUW)", tooltip="Follow-Up Window (FUW)")
          );
        }
      }
      if( highlight.observation.window )
      {
        # The "given" OW:
        if( .do.R ) # Rplot:
        {
          # Save the info:
          .last.cma.plot.info$baseR$cma$data[s.events,".X.OW.START"] <- (adh.plot.space[2] + as.numeric(cmas$.OBS.START.DATE[s.cmas[1]] - earliest.date) + correct.earliest.followup.window);
          .last.cma.plot.info$baseR$cma$data[s.events,".Y.OW.START"] <- (y.cur - 0.5);
          .last.cma.plot.info$baseR$cma$data[s.events,".X.OW.END"]   <- (adh.plot.space[2] + as.numeric(cmas$.OBS.END.DATE[s.cmas[1]]   - earliest.date) + correct.earliest.followup.window);
          .last.cma.plot.info$baseR$cma$data[s.events,".Y.OW.END"]   <- (y.cur + vspace.needed.events - 0.5);

          # Draw:
          rect(.last.cma.plot.info$baseR$cma$data[s.events[1],".X.OW.START"], .last.cma.plot.info$baseR$cma$data[s.events[1],".Y.OW.START"],
               .last.cma.plot.info$baseR$cma$data[s.events[1],".X.OW.END"],   .last.cma.plot.info$baseR$cma$data[s.events[1],".Y.OW.END"],
               col=adjustcolor(observation.window.col,alpha.f=observation.window.opacity), border=NA); #, density=observation.window.density, angle=observation.window.angle);
        }

        if( .do.SVG ) # SVG:
        {
          # Save the info:
          .last.cma.plot.info$SVG$cma$data[s.events,".X.OW.START"] <- .scale.x.to.SVG.plot(adh.plot.space[2] + as.numeric(cmas$.OBS.START.DATE[s.cmas[1]] - earliest.date) + correct.earliest.followup.window);
          .last.cma.plot.info$SVG$cma$data[s.events,".Y.OW.START"] <- .scale.y.to.SVG.plot(y.cur + vspace.needed.events - 0.5);
          .last.cma.plot.info$SVG$cma$data[s.events,".X.OW.END"]   <- .scale.x.to.SVG.plot(adh.plot.space[2] + as.numeric(cmas$.OBS.END.DATE[s.cmas[1]]   - earliest.date) + correct.earliest.followup.window);
          .last.cma.plot.info$SVG$cma$data[s.events,".Y.OW.END"]   <- .scale.y.to.SVG.plot(y.cur + 0.5);

          # Draw:
          svg.str <- c(svg.str,
                       # OW:
                       .SVG.rect(x=.last.cma.plot.info$SVG$cma$data[s.events[1],".X.OW.START"], y=.last.cma.plot.info$SVG$cma$data[s.events[1],".Y.OW.START"],
                                 width=.scale.width.to.SVG.plot(as.numeric(cmas$.OBS.END.DATE[s.cmas[1]] - cmas$.OBS.START.DATE[s.cmas[1]])),
                                 height=.scale.height.to.SVG.plot(vspace.needed.events),
                                 stroke="none", fill=observation.window.col, fill_opacity=observation.window.opacity,
                                 class="ow", comment="The Observation Window (OW)", tooltip="Observation Window (OW)")
          );
        }

        if( !is.null(cma.realOW) )
        {
          # For CMA8, the OW might have been changed, so we also have a "real" OW:
          s.realOW <- which(cma.realOW[,col.plotid] == cur_plot_id);

          # Find the beginning of the "real" OW:
          if( length(s.realOW) == 1)
          {
            if( !is.null(cma.realOW$window.start) && !is.na(cma.realOW$window.start[s.realOW]) )
            {
              real.obs.window.start <- cma.realOW$window.start[s.realOW];
            } else
            {
              real.obs.window.start <- evinfo$.OBS.START.DATE[s.events[1]];
            }
            if( !is.null(cma.realOW$window.end) && !is.na(cma.realOW$window.end[s.realOW]) )
            {
              real.obs.window.end <- cma.realOW$window.end[s.realOW];
            } else
            {
              real.obs.window.end <- evinfo$.OBS.END.DATE[s.events[1]];
            }

            # Draw the "real" OW:
            if( .do.R ) # Rplot:
            {
              # Save the info:
              .last.cma.plot.info$baseR$cma$data[s.events,".X.ROW.START"] <- (adh.plot.space[2] + as.numeric(real.obs.window.start - earliest.date) + correct.earliest.followup.window);
              .last.cma.plot.info$baseR$cma$data[s.events,".Y.ROW.START"] <- (y.cur - 0.5);
              .last.cma.plot.info$baseR$cma$data[s.events,".X.ROW.END"]   <- (adh.plot.space[2] + as.numeric(real.obs.window.end   - earliest.date) + correct.earliest.followup.window);
              .last.cma.plot.info$baseR$cma$data[s.events,".Y.ROW.END"]   <- (y.cur + vspace.needed.events - 0.5);

              # Draw:
              rect(.last.cma.plot.info$baseR$cma$data[s.events[1],".X.ROW.START"], .last.cma.plot.info$baseR$cma$data[s.events[1],".Y.ROW.START"],
                   .last.cma.plot.info$baseR$cma$data[s.events[1],".X.ROW.END"],   .last.cma.plot.info$baseR$cma$data[s.events[1],".Y.ROW.END"],
                   col=adjustcolor(observation.window.col,alpha.f=observation.window.opacity), border=NA); #, density=real.obs.window.density, angle=real.obs.window.angle);
            }

            if( .do.SVG ) # SVG:
            {
              # Save the info:
              .last.cma.plot.info$SVG$cma$data[s.events,".X.ROW.START"] <- .scale.x.to.SVG.plot(adh.plot.space[2] + as.numeric(real.obs.window.start - earliest.date) + correct.earliest.followup.window);
              .last.cma.plot.info$SVG$cma$data[s.events,".Y.ROW.START"] <- .scale.y.to.SVG.plot(y.cur + vspace.needed.events - 0.5);
              .last.cma.plot.info$SVG$cma$data[s.events,".X.ROW.END"]   <- .scale.x.to.SVG.plot(adh.plot.space[2] + as.numeric(real.obs.window.start - earliest.date) + correct.earliest.followup.window);
              .last.cma.plot.info$SVG$cma$data[s.events,".Y.ROW.END"]   <- .scale.y.to.SVG.plot(y.cur + 0.5);

              # Draw:
              svg.str <- c(svg.str,
                           # "real" OW:
                           .SVG.rect(x=.last.cma.plot.info$SVG$cma$data[s.events[1],".X.ROW.START"], y=.last.cma.plot.info$SVG$cma$data[s.events[1],".Y.ROW.START"],
                                     width=.scale.width.to.SVG.plot(as.numeric(real.obs.window.end - real.obs.window.start)),
                                     height=.scale.height.to.SVG.plot(vspace.needed.events),
                                     stroke="none", fill=observation.window.col, fill_opacity=observation.window.opacity,
                                     class="ow-real", comment="The 'real' Observation Window", tooltip="'Real' Observation Window")
              );
            }
          }
        }
      }

      ##
      ## The y-axis labels ####
      ##

      # The y-axis label:
      pid <- cur_plot_id;
      y.mean <- y.cur + vspace.needed.total/2 - ifelse(plot.events.vertically.displaced, 0.0, 0.5); # vertical position of the label (centered on patient)
      if( .do.R ) # Rplot:
      {
        text(par("usr")[1], y.mean, pid, cex=cex.axis, srt=-rotate.text, pos=2, xpd=TRUE);

        # Save the info:
        .last.cma.plot.info$baseR$y.labels <- rbind(.last.cma.plot.info$baseR$y.labels,
                                                    data.frame("string"=pid,
                                                               "x"=par("usr")[1],
                                                               "y"=y.mean,
                                                               "cex"=cex.axis));
      }
      if( .do.SVG ) # SVG:
      {
        svg.str <- c(svg.str,
                     .SVG.text(x=(dims.plot.x - dims.chr.axis), y=.scale.y.to.SVG.plot(y.cur + vspace.needed.total/2), text=pid,
                               font_size=dims.chr.axis, h.align="right", v.align="center", rotate=-(90+rotate.text),
                               class="axis-labels-y", comment="The y-axis labels", suppress.warnings=suppress.warnings)
        );

        # Save the info:
        .last.cma.plot.info$SVG$y.labels <- rbind(.last.cma.plot.info$SVG$y.labels,
                                                  data.frame("string"=pid,
                                                             "x"=(dims.plot.x - dims.chr.axis),
                                                             "y"=.scale.y.to.SVG.plot(y.cur + vspace.needed.total/2),
                                                             "font.size"=dims.chr.axis));
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
            # Save the info:
            .last.cma.plot.info$baseR$cma$data[s.events,".X.SCMA.START"] <- .rescale.xcoord.for.CMA.plot(0.0);
            .last.cma.plot.info$baseR$cma$data[s.events,".Y.SCMA.START"] <- (y.mean - 2);
            .last.cma.plot.info$baseR$cma$data[s.events,".X.SCMA.END"]   <- .rescale.xcoord.for.CMA.plot(1.0);
            .last.cma.plot.info$baseR$cma$data[s.events,".Y.SCMA.END"]   <- (y.mean + 2);

            # Draw:
            segments(.last.cma.plot.info$baseR$cma$data[s.events[1],".X.SCMA.START"], .last.cma.plot.info$baseR$cma$data[s.events[1],".Y.SCMA.START"],
                     .last.cma.plot.info$baseR$cma$data[s.events[1],".X.SCMA.END"],   .last.cma.plot.info$baseR$cma$data[s.events[1],".Y.SCMA.START"],
                     lty="solid", col=CMA.plot.col);
            segments(.last.cma.plot.info$baseR$cma$data[s.events[1],".X.SCMA.START"], .last.cma.plot.info$baseR$cma$data[s.events[1],".Y.SCMA.END"],
                     .last.cma.plot.info$baseR$cma$data[s.events[1],".X.SCMA.END"],   .last.cma.plot.info$baseR$cma$data[s.events[1],".Y.SCMA.END"],
                     lty="solid", col=CMA.plot.col);
          }

          if( .do.SVG ) # SVG:
          {
            # Save the info:
            .last.cma.plot.info$SVG$cma$data[s.events,".X.SCMA.START"] <- .scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(0.0));
            .last.cma.plot.info$SVG$cma$data[s.events,".Y.SCMA.START"] <- .scale.y.to.SVG.plot(y.mean - 2);
            .last.cma.plot.info$SVG$cma$data[s.events,".X.SCMA.END"]   <- .scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(1.0));
            .last.cma.plot.info$SVG$cma$data[s.events,".Y.SCMA.END"]   <- .scale.y.to.SVG.plot(y.mean + 2);

            # Draw:
            svg.str <- c(svg.str,
                         # The CMA plot background:
                         .SVG.lines(x=c(.last.cma.plot.info$SVG$cma$data[s.events[1],".X.SCMA.START"],
                                        .last.cma.plot.info$SVG$cma$data[s.events[1],".X.SCMA.END"],
                                        .last.cma.plot.info$SVG$cma$data[s.events[1],".X.SCMA.START"],
                                        .last.cma.plot.info$SVG$cma$data[s.events[1],".X.SCMA.END"]),
                                    y=c(.last.cma.plot.info$SVG$cma$data[s.events[1],".Y.SCMA.START"],
                                        .last.cma.plot.info$SVG$cma$data[s.events[1],".Y.SCMA.START"],
                                        .last.cma.plot.info$SVG$cma$data[s.events[1],".Y.SCMA.END"],
                                        .last.cma.plot.info$SVG$cma$data[s.events[1],".Y.SCMA.END"]),
                                    connected=FALSE,
                                    stroke=CMA.plot.col, stroke_width=1,
                                    class="cma-drawing-area-background", comment="The CMA plot background", suppress.warnings=suppress.warnings)
            );
          }

          # The non-missing CMA values:
          adh <- na.omit(cmas[s.cmas,"CMA"]);

          # Scale the CMA (itself or density) in such a way that if within 0..1 stays within 0..1 but scales if it goes outside this interval to accommodate it
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
            # The vertical position where it will be drawn and its vertical extent:
            if( plot.events.vertically.displaced )
            {
              # Events are vertically displaced:
              adh.y <- mean(s.events);
              adh.h <- ifelse(length(s.events) < 2, 0.5, ifelse(length(s.events) == 2, 0.75, 1.0));
            } else
            {
              # Events are all on a single line:
              adh.y <- y.cur;
              adh.h <- 0.25;
            }

            if( .do.R ) # Rplot:
            {
              # Draw the background rectangle:
              # Save the info:
              .last.cma.plot.info$baseR$cma$data[s.events,".X.SCMA.START"] <- .rescale.xcoord.for.CMA.plot(0.0);
              .last.cma.plot.info$baseR$cma$data[s.events,".Y.SCMA.START"] <- (adh.y - adh.h);
              .last.cma.plot.info$baseR$cma$data[s.events,".X.SCMA.END"]   <- .rescale.xcoord.for.CMA.plot(max(1.0,adh.max));
              .last.cma.plot.info$baseR$cma$data[s.events,".Y.SCMA.END"]   <- (adh.y + adh.h);

              # Draw:
              rect(.last.cma.plot.info$baseR$cma$data[s.events[1],".X.SCMA.START"], .last.cma.plot.info$baseR$cma$data[s.events[1],".Y.SCMA.START"],
                   .rescale.xcoord.for.CMA.plot(min(adh,adh.max)),                  .last.cma.plot.info$baseR$cma$data[s.events[1],".Y.SCMA.END"],
                   col=CMA.plot.col, border=NA);
              rect(.last.cma.plot.info$baseR$cma$data[s.events[1],".X.SCMA.START"], .last.cma.plot.info$baseR$cma$data[s.events[1],".Y.SCMA.START"],
                   .last.cma.plot.info$baseR$cma$data[s.events[1],".X.SCMA.END"],   .last.cma.plot.info$baseR$cma$data[s.events[1],".Y.SCMA.END"],
                   col=NA, border=CMA.plot.border);
            }

            if( .do.SVG ) # SVG:
            {
              # Save the info:
              .last.cma.plot.info$SVG$cma$data[s.events,".X.SCMA.START"] <- .scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(0.0));
              .last.cma.plot.info$SVG$cma$data[s.events,".Y.SCMA.START"] <- .scale.y.to.SVG.plot(adh.y + adh.h);
              .last.cma.plot.info$SVG$cma$data[s.events,".X.SCMA.END"]   <- .scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(max(1.0,adh.max)));
              .last.cma.plot.info$SVG$cma$data[s.events,".Y.SCMA.END"]   <- .scale.y.to.SVG.plot(adh.y - adh.h);

              # Draw:
              svg.str <- c(svg.str,
                           # Draw the CMA estimate background rectangle:
                           .SVG.rect(x=.last.cma.plot.info$SVG$cma$data[s.events[1],".X.SCMA.START"],
                                     y=.last.cma.plot.info$SVG$cma$data[s.events[1],".Y.SCMA.START"],
                                     width=.scale.width.to.SVG.plot(.rescale.xcoord.for.CMA.plot(min(adh,adh.max)) - .rescale.xcoord.for.CMA.plot(0.0)),
                                     height=.scale.height.to.SVG.plot(2*adh.h),
                                     stroke="none", fill=CMA.plot.col,
                                     class="cma-estimate-bkg", comment="The CMA estimate backgound"),
                           .SVG.rect(x=.last.cma.plot.info$SVG$cma$data[s.events[1],".X.SCMA.START"],
                                     y=.last.cma.plot.info$SVG$cma$data[s.events[1],".Y.SCMA.START"],
                                     width=.scale.width.to.SVG.plot(.rescale.xcoord.for.CMA.plot(max(1.0,adh.max)) - .rescale.xcoord.for.CMA.plot(0.0)),
                                     height=.scale.height.to.SVG.plot(2*adh.h),
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
                text(x=(.rescale.xcoord.for.CMA.plot(0.0) + .rescale.xcoord.for.CMA.plot(max(1.0,adh.max)))/2, y=adh.y,
                     labels=cma.string, col=CMA.plot.text, cex=CMA.cex);
              } else if( strheight(cma.string, cex=CMA.cex) <= available.x.space )
              { # vertical writing of the CMA:
                text(x=(.rescale.xcoord.for.CMA.plot(0.0) + .rescale.xcoord.for.CMA.plot(max(1.0,adh.max)))/2, y=adh.y,
                     labels=cma.string, col=CMA.plot.text, cex=CMA.cex, srt=90);
              } else if( force.draw.text )
              { # force horizontal writing of the CMA:
                text(x=(.rescale.xcoord.for.CMA.plot(0.0) + .rescale.xcoord.for.CMA.plot(max(1.0,adh.max)))/2, y=adh.y,
                     labels=cma.string, col=CMA.plot.text, cex=CMA.cex);
              } # otherwise, there's no space for showing the CMA here
            }

            if( .do.SVG ) # SVG:
            {
              if( available.x.space * dims.event.x >= dims.chr.cma )
              {
                svg.str <- c(svg.str,
                             # Write the CMA estimate (always vertically):
                             .SVG.text(x=.scale.x.to.SVG.plot((.rescale.xcoord.for.CMA.plot(0.0) + .rescale.xcoord.for.CMA.plot(max(1.0,adh.max)))/2),
                                       y=.scale.y.to.SVG.plot(adh.y),
                                       text=cma.string,
                                       col=CMA.plot.text, font_size=dims.chr.cma, h.align="center", v.align="center", rotate=-90,
                                       class="cma-estimate-text", comment="The CMA estimate (as text)", suppress.warnings=suppress.warnings)
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
      if( .do.SVG ) med.class.svg <- NA;
    } else
    {
      col <- .map.category.to.color(cma$data[i,cma$medication.class.colname]);
      if( .do.SVG ) med.class.svg <- .map.category.to.class(cma$data[i,cma$medication.class.colname]);
    }

    if( .do.R ) # Rplot:
    {
      # Save the info:
      .last.cma.plot.info$baseR$cma$data[i,".X.START"] <- (adh.plot.space[2] + start + correct.earliest.followup.window);
      .last.cma.plot.info$baseR$cma$data[i,".Y.START"] <- (y.cur);
      .last.cma.plot.info$baseR$cma$data[i,".X.END"]   <- (adh.plot.space[2] + end   + correct.earliest.followup.window);
      .last.cma.plot.info$baseR$cma$data[i,".Y.END"]   <- (y.cur);

      # Plot the beging and end of the event:
      points(.last.cma.plot.info$baseR$cma$data[i,".X.START"], .last.cma.plot.info$baseR$cma$data[i,".Y.START"], pch=pch.start.event, col=col, cex=cex);
      points(.last.cma.plot.info$baseR$cma$data[i,".X.END"],   .last.cma.plot.info$baseR$cma$data[i,".Y.END"],   pch=pch.end.event,   col=col, cex=cex);
    }

    if( .do.SVG ) # SVG:
    {
      # Save the info:
      .last.cma.plot.info$SVG$cma$data[i,".X.START"] <- .scale.x.to.SVG.plot(adh.plot.space[2] + start + correct.earliest.followup.window);
      .last.cma.plot.info$SVG$cma$data[i,".Y.START"] <- .scale.y.to.SVG.plot(y.cur);
      .last.cma.plot.info$SVG$cma$data[i,".X.END"]   <- .scale.x.to.SVG.plot(adh.plot.space[2] + end + correct.earliest.followup.window);
      .last.cma.plot.info$SVG$cma$data[i,".Y.END"]   <- .scale.y.to.SVG.plot(y.cur);

      # Draw:
      svg.str <- c(svg.str,
                   # The begining of the event:
                   .SVG.points(x=.last.cma.plot.info$SVG$cma$data[i,".X.START"], y=.last.cma.plot.info$SVG$cma$data[i,".Y.START"],
                               pch=pch.start.event, col=col, cex=cex,
                               class=paste0("event-start",if(!is.na(med.class.svg)) paste0("-",med.class.svg)),
                               tooltip=med.class.svg, suppress.warnings=suppress.warnings),
                   # The end of the event:
                   .SVG.points(x=.last.cma.plot.info$SVG$cma$data[i,".X.END"], y=.last.cma.plot.info$SVG$cma$data[i,".Y.END"],
                               pch=pch.end.event, col=col, cex=cex,
                               class=paste0("event-end",if(!is.na(med.class.svg)) paste0("-",med.class.svg)),
                               tooltip=med.class.svg, suppress.warnings=suppress.warnings)
      );
    }


    # Show event intervals as rectangles?
    if( show.event.intervals && !is.null(evinfo) && !is.na(evinfo$event.interval[i]) )
    {
      # The end of the prescription:
      end.pi <- start + evinfo$event.interval[i] - evinfo$gap.days[i];

      if( .do.R ) # Rplot:
      {
        # Save the info:
        .last.cma.plot.info$baseR$cma$data[i,".X.EVC.START"] <- (adh.plot.space[2] + start  + correct.earliest.followup.window);
        .last.cma.plot.info$baseR$cma$data[i,".Y.EVC.START"] <- (y.cur - char.height/2);
        .last.cma.plot.info$baseR$cma$data[i,".X.EVC.END"]   <- (adh.plot.space[2] + end.pi + correct.earliest.followup.window);
        .last.cma.plot.info$baseR$cma$data[i,".Y.EVC.END"]   <- (y.cur + char.height/2);

        # Draw:
        rect(.last.cma.plot.info$baseR$cma$data[i,".X.EVC.START"], .last.cma.plot.info$baseR$cma$data[i,".Y.EVC.START"],
             .last.cma.plot.info$baseR$cma$data[i,".X.EVC.END"],   .last.cma.plot.info$baseR$cma$data[i,".Y.EVC.END"],
             col=adjustcolor(col,alpha.f=0.2), border=col);
        if( evinfo$gap.days[i] > 0 )
        {
          # Save the info:
          .last.cma.plot.info$baseR$cma$data[i,".X.EVNC.START"] <- (adh.plot.space[2] + end.pi + correct.earliest.followup.window);
          .last.cma.plot.info$baseR$cma$data[i,".Y.EVNC.START"] <- (y.cur - char.height/2);
          .last.cma.plot.info$baseR$cma$data[i,".X.EVNC.END"]   <- (adh.plot.space[2] + end.pi + evinfo$gap.days[i] + correct.earliest.followup.window);
          .last.cma.plot.info$baseR$cma$data[i,".Y.EVNC.END"]   <- (y.cur + char.height/2);

          # Draw:
          rect(.last.cma.plot.info$baseR$cma$data[i,".X.EVNC.START"], .last.cma.plot.info$baseR$cma$data[i,".Y.EVNC.START"],
               .last.cma.plot.info$baseR$cma$data[i,".X.EVNC.END"],   .last.cma.plot.info$baseR$cma$data[i,".Y.EVNC.END"],
               #density=25, col=adjustcolor(col,alpha.f=0.5),
               col=NA, border=col);
        }
      }

      if( .do.SVG ) # SVG:
      {
        # Save the info:
        .last.cma.plot.info$SVG$cma$data[i,".X.EVC.START"] <- .scale.x.to.SVG.plot(adh.plot.space[2] + start + correct.earliest.followup.window);
        .last.cma.plot.info$SVG$cma$data[i,".Y.EVC.START"] <- .scale.y.to.SVG.plot(y.cur) - dims.event.y/2;
        .last.cma.plot.info$SVG$cma$data[i,".X.EVC.END"]   <- .scale.x.to.SVG.plot(adh.plot.space[2] + end.pi + correct.earliest.followup.window);
        .last.cma.plot.info$SVG$cma$data[i,".Y.EVC.END"]   <- .last.cma.plot.info$SVG$cma$data[i,".Y.EVC.START"] + dims.event.y;

        # Draw:
        svg.str <- c(svg.str,
                     .SVG.rect(x=.last.cma.plot.info$SVG$cma$data[i,".X.EVC.START"],
                               y=.last.cma.plot.info$SVG$cma$data[i,".Y.EVC.START"],
                               xend=.last.cma.plot.info$SVG$cma$data[i,".X.EVC.END"],
                               height=dims.event.y,
                               stroke=col, fill=col, fill_opacity=0.2,
                               class=paste0("event-interval-covered",if(!is.na(med.class.svg)) paste0("-",med.class.svg)),
                               tooltip=med.class.svg));
        if( evinfo$gap.days[i] > 0 )
        {
          # Save the info:
          .last.cma.plot.info$SVG$cma$data[i,".X.EVNC.START"] <- .scale.x.to.SVG.plot(adh.plot.space[2] + end.pi + correct.earliest.followup.window);
          .last.cma.plot.info$SVG$cma$data[i,".Y.EVNC.START"] <- .scale.y.to.SVG.plot(y.cur) - dims.event.y/2;
          .last.cma.plot.info$SVG$cma$data[i,".X.EVNC.END"]   <- .scale.x.to.SVG.plot(adh.plot.space[2] + end.pi + evinfo$gap.days[i] + correct.earliest.followup.window);
          .last.cma.plot.info$SVG$cma$data[i,".Y.EVNC.END"]   <- .last.cma.plot.info$SVG$cma$data[i,".Y.EVNC.START"] + dims.event.y;

          # Draw:
          svg.str <- c(svg.str,
                       .SVG.rect(x=.last.cma.plot.info$SVG$cma$data[i,".X.EVNC.START"],
                                 y=.last.cma.plot.info$SVG$cma$data[i,".Y.EVNC.START"],
                                 xend=.last.cma.plot.info$SVG$cma$data[i,".X.EVNC.END"],
                                 height=dims.event.y,
                                 stroke=col, fill="none",
                                 class=paste0("event-interval-not-covered",if(!is.na(med.class.svg)) paste0("-",med.class.svg)),
                                 tooltip=med.class.svg));
        }
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
      # Save the info:
      .last.cma.plot.info$baseR$cma$data[s.events,".EV.LWD"] <- seg.lwd;

      # Draw:
      segments( seg.x1, y.cur, seg.x2, y.cur, col=col, lty=lty.event, lwd=seg.lwd);
    }
    if( .do.SVG ) # SVG:
    {
      # Save the info:
      .last.cma.plot.info$SVG$cma$data[s.events,".EV.LWD"] <- seg.lwd;

      # Draw:
      svg.str <- c(svg.str,
                   # The beginning of the event:
                   .SVG.lines(x=c(.scale.x.to.SVG.plot(seg.x1), .scale.x.to.SVG.plot(seg.x2)),
                              y=rep(.scale.y.to.SVG.plot(y.cur),2),
                              connected=FALSE,
                              stroke=col, stroke_width=seg.lwd,
                              class=paste0("event-segment",if(!is.na(med.class.svg)) paste0("-",med.class.svg)),
                              tooltip=med.class.svg, suppress.warnings=suppress.warnings)
      );
    }

    if( print.dose )
    {
      # Show dose as actual numbers on the plot:
      if( .do.R ) # Rplot:
      {
        # Save the info:
        .last.cma.plot.info$baseR$cma$data[i,".X.DOSE"] <- (adh.plot.space[2] + (start + end)/2 + correct.earliest.followup.window);
        .last.cma.plot.info$baseR$cma$data[i,".Y.DOSE"] <- (y.cur - ifelse(print.dose.centered, 0, dose.text.height*2/3)); # print it on or below the dose segment?
        .last.cma.plot.info$baseR$cma$data[i,".FONT.SIZE.DOSE"] <- cex.dose;

        # Draw:
        text(.last.cma.plot.info$baseR$cma$data[i,".X.DOSE"], .last.cma.plot.info$baseR$cma$data[i,".Y.DOSE"],
             cma$data[i,cma$event.daily.dose.colname], cex=cex.dose, col=ifelse(is.na(print.dose.col),col,print.dose.col), font=2);
      }

      if( .do.SVG ) # SVG:
      {
        # Save the info:
        .last.cma.plot.info$SVG$cma$data[i,".X.DOSE"] <- .scale.x.to.SVG.plot(adh.plot.space[2] + (start + end)/2 + correct.earliest.followup.window);
        .last.cma.plot.info$SVG$cma$data[i,".Y.DOSE"] <- .scale.y.to.SVG.plot(y.cur - ifelse(print.dose.centered, 0, 3/4));
        .last.cma.plot.info$SVG$cma$data[i,".FONT.SIZE.DOSE"] <- (dims.chr.std * cex.dose);

        # Draw:
        svg.str <- c(svg.str,
                     # The dose text:
                     .SVG.text(x=.last.cma.plot.info$SVG$cma$data[i,".X.DOSE"], y=.last.cma.plot.info$SVG$cma$data[i,".Y.DOSE"],
                               text=cma$data[i,cma$event.daily.dose.colname],
                               font_size=.last.cma.plot.info$SVG$cma$data[i,".FONT.SIZE.DOSE"], h.align="center", v.align="center",
                               col=if(is.na(print.dose.col)) col else print.dose.col,
                               other_params=if(!is.na(print.dose.outline.col)) paste0(' stroke="',.SVG.color(print.dose.outline.col,return_string=TRUE),'" stroke-width="0.5"'),
                               class=paste0("event-dose-text",if(!is.na(med.class.svg)) paste0("-",med.class.svg)),
                               tooltip=med.class.svg, suppress.warnings=suppress.warnings)
        );
      }
    }

    # Advance to the next vertical line:
    if( plot.events.vertically.displaced )
    {
      y.cur <- y.cur + 1;
    }

    # Continuation between successive events:
    if( i < nrow(cma$data) && (cur_plot_id == cma$data[i+1,col.plotid]) )
    {
      # We're still plotting the same patient: show the continuation line:
      start.next <- as.numeric(cma$data$.DATE.as.Date[i+1] - earliest.date);

      # How many lines to jump?
      cont.v.jump <- ifelse(plot.events.vertically.displaced, 1, 0);

      if( .do.R ) # Rplot:
      {
        # Save the info:
        .last.cma.plot.info$baseR$cma$data[i,".X.CNT.START"] <- (adh.plot.space[2] + end        + correct.earliest.followup.window);
        .last.cma.plot.info$baseR$cma$data[i,".Y.CNT.START"] <- (y.cur - cont.v.jump);
        .last.cma.plot.info$baseR$cma$data[i,".X.CNT.END"]   <- (adh.plot.space[2] + start.next + correct.earliest.followup.window);
        .last.cma.plot.info$baseR$cma$data[i,".Y.CNT.END"]   <- (y.cur);

        # Draw:
        segments( .last.cma.plot.info$baseR$cma$data[i,".X.CNT.START"], .last.cma.plot.info$baseR$cma$data[i,".Y.CNT.START"],
                  .last.cma.plot.info$baseR$cma$data[i,".X.CNT.END"],   .last.cma.plot.info$baseR$cma$data[i,".Y.CNT.START"],
                  col=col.continuation, lty=lty.continuation, lwd=lwd.continuation);
        segments( .last.cma.plot.info$baseR$cma$data[i,".X.CNT.END"], .last.cma.plot.info$baseR$cma$data[i,".Y.CNT.START"],
                  .last.cma.plot.info$baseR$cma$data[i,".X.CNT.END"], .last.cma.plot.info$baseR$cma$data[i,".Y.CNT.END"],
                  col=col.continuation, lty=lty.continuation, lwd=lwd.continuation);
      }

      if( .do.SVG ) # SVG:
      {
        # Save the info:
        .last.cma.plot.info$SVG$cma$data[i,".X.CNT.START"] <- .scale.x.to.SVG.plot(adh.plot.space[2] + end + correct.earliest.followup.window);
        .last.cma.plot.info$SVG$cma$data[i,".Y.CNT.START"] <- .scale.y.to.SVG.plot(y.cur - cont.v.jump);
        .last.cma.plot.info$SVG$cma$data[i,".X.CNT.END"]   <- .scale.x.to.SVG.plot(adh.plot.space[2] + start.next + correct.earliest.followup.window);
        .last.cma.plot.info$SVG$cma$data[i,".Y.CNT.END"]   <- .scale.y.to.SVG.plot(y.cur);

        # Draw:
        svg.str <- c(svg.str,
                     # The continuation line:
                     .SVG.lines(x=c(.last.cma.plot.info$SVG$cma$data[i,".X.CNT.START"],
                                    .last.cma.plot.info$SVG$cma$data[i,".X.CNT.END"],
                                    .last.cma.plot.info$SVG$cma$data[i,".X.CNT.END"],
                                    .last.cma.plot.info$SVG$cma$data[i,".X.CNT.END"]),
                                y=c(.last.cma.plot.info$SVG$cma$data[i,".Y.CNT.START"],
                                    .last.cma.plot.info$SVG$cma$data[i,".Y.CNT.START"],
                                    .last.cma.plot.info$SVG$cma$data[i,".Y.CNT.START"],
                                    .last.cma.plot.info$SVG$cma$data[i,".Y.CNT.END"]),
                                connected=TRUE,
                                stroke=col.continuation, stroke_width=lwd.continuation, lty=lty.continuation,
                                class=paste0("continuation-line",if(!is.na(med.class.svg)) paste0("-",med.class.svg)),
                                tooltip=med.class.svg, suppress.warnings=suppress.warnings)
        );
      }
    } else
    { # The patient is changing or is the last one:

      # Advance to next line of need be:
      if( !plot.events.vertically.displaced )
      {
        y.cur <- y.cur + 1;
      }


      ##
      ## Partial CMAs ####
      ##

      # Draw its sub-periods (if so requested, meaningful and possible):
      if( is.cma.TS.or.SW && has.estimated.CMA )
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
              # Save the info:
              .last.cma.plot.info$baseR$partialCMAs <- rbind(.last.cma.plot.info$baseR$partialCMAs,
                                                             data.frame("pid"=cur_plot_id, "type"="stacked",
                                                                        "x.region.start"=min(corrected.x.start, na.rm=TRUE),
                                                                        "y.region.start"=min(ys, na.rm=TRUE),
                                                                        "x.region.end"=max(corrected.x.end, na.rm=TRUE),
                                                                        "y.region.end"=max(ys, na.rm=TRUE)+1,
                                                                        "x.partial.start"=corrected.x.start,
                                                                        "y.partial.start"=ys + 0.10,
                                                                        "x.partial.end"=corrected.x.end,
                                                                        "y.partial.end"=ys + 0.90));
              # The intervals as empty rectangles:
              rect(corrected.x.start, ys + 0.10, corrected.x.end,   ys + 0.90, border=gray(0.7), col="white");
              # The CMAs as filled rectangles of length proportional to the CMA:
              rect(corrected.x.start, ys + 0.10, corrected.x.start + h, ys + 0.90, border=plot.partial.CMAs.as.stacked.col.border, col=plot.partial.CMAs.as.stacked.col.bars);
              if( force.draw.text || print.CMA && char.height.CMA <= 0.80 )
              {
                text(corrected.x.text, ys + 0.5, ppts$text, cex=CMA.cex, col=plot.partial.CMAs.as.stacked.col.text);
              }
            }

            if( .do.SVG ) # SVG:
            {
              # Save the info:
              .last.cma.plot.info$SVG$partialCMAs <- rbind(.last.cma.plot.info$SVG$partialCMAs,
                                                           data.frame("pid"=cur_plot_id, "type"="stacked",
                                                                      "x.region.start"=.scale.x.to.SVG.plot(min(corrected.x.start, na.rm=TRUE)),
                                                                      "y.region.start"=.scale.y.to.SVG.plot(max(ys, na.rm=TRUE)+1),
                                                                      "x.region.end"=.scale.x.to.SVG.plot(max(corrected.x.end, na.rm=TRUE)),
                                                                      "y.region.end"=.scale.y.to.SVG.plot(min(ys, na.rm=TRUE)),
                                                                      "x.partial.start"=.scale.x.to.SVG.plot(corrected.x.start),
                                                                      "y.partial.start"=.scale.y.to.SVG.plot(ys + 0.90),
                                                                      "x.partial.end"=.scale.x.to.SVG.plot(corrected.x.end),
                                                                      "y.partial.end"=.scale.y.to.SVG.plot(ys + 0.10)));
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
                             if( force.draw.text || print.CMA && dims.chr.cma <= dims.chr.event )
                             {
                               .SVG.text(.scale.x.to.SVG.plot(corrected.x.text[j]), y=.scale.y.to.SVG.plot(ys[j] + 0.50),
                                         text=ppts$text[j], font_size=dims.chr.cma, col=plot.partial.CMAs.as.stacked.col.text,
                                         h.align="center", v.align="center",
                                         class="partial_cma_stacked_text_estimate", suppress.warnings=suppress.warnings)
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
                # Save the info:
                .last.cma.plot.info$baseR$partialCMAs <- rbind(.last.cma.plot.info$baseR$partialCMAs,
                                                               data.frame("pid"=cur_plot_id, "type"="overlapping",
                                                                          "x.region.start"=min(corrected.x.start, na.rm=TRUE),
                                                                          "y.region.start"=min(y.cur + 0.5 + v, na.rm=TRUE),
                                                                          "x.region.end"=max(corrected.x.end, na.rm=TRUE),
                                                                          "y.region.end"=max(y.cur + 0.5 + v + ifelse(!is.na(y.norm.v),y.norm.v,0), na.rm=TRUE),
                                                                          "x.partial.start"=corrected.x.start,
                                                                          "y.partial.start"=y.cur + 0.5 + v,
                                                                          "x.partial.end"=corrected.x.end,
                                                                          "y.partial.end"=y.cur + 0.5 + v + ifelse(!is.na(y.norm.v),y.norm.v,0)));
                segments(corrected.x.start, y.cur + 0.5 + v, corrected.x.end,   y.cur + 0.5 + v, col=plot.partial.CMAs.as.overlapping.col.interval);
                segments(corrected.x.start, y.cur + 0.5 + v, corrected.x.start, y.cur + 0.5 + v + y.norm.v, col=plot.partial.CMAs.as.overlapping.col.interval);
                segments(corrected.x.end,   y.cur + 0.5 + v, corrected.x.end,   y.cur + 0.5 + v + y.norm.v, col=plot.partial.CMAs.as.overlapping.col.interval);
              }

              if( .do.SVG ) # SVG:
              {
                # Save the info:
                .last.cma.plot.info$SVG$partialCMAs <- rbind(.last.cma.plot.info$SVG$partialCMAs,
                                                             data.frame("pid"=cur_plot_id, "type"="stacked",
                                                                        "x.region.start"=.scale.x.to.SVG.plot(min(corrected.x.start, na.rm=TRUE)),
                                                                        "y.region.start"=.scale.y.to.SVG.plot(max(y.cur + 0.5 + v + ifelse(!is.na(y.norm.v),y.norm.v,0), na.rm=TRUE)),
                                                                        "x.region.end"=.scale.x.to.SVG.plot(max(corrected.x.end, na.rm=TRUE)),
                                                                        "y.region.end"=.scale.y.to.SVG.plot(min(y.cur + 0.5 + v, na.rm=TRUE)),
                                                                        "x.partial.start"=.scale.x.to.SVG.plot(corrected.x.start),
                                                                        "y.partial.start"=.scale.y.to.SVG.plot(y.cur + 0.5 + v + ifelse(!is.na(y.norm.v),y.norm.v,0)),
                                                                        "x.partial.end"=.scale.x.to.SVG.plot(corrected.x.end),
                                                                        "y.partial.end"=.scale.y.to.SVG.plot(y.cur + 0.5 + v)));
                for( j in 1:nrow(ppts) )
                {
                  svg.str <- c(svg.str,
                               # The connected segments one by one:
                               .SVG.lines(x=.scale.x.to.SVG.plot(c(corrected.x.start[j], corrected.x.end[j])),
                                          y=.scale.y.to.SVG.plot(c(y.cur + 0.5 + v[j], y.cur + 0.5 + v[j])),
                                          connected=FALSE, stroke=plot.partial.CMAs.as.overlapping.col.interval,
                                          class="partial_cma_overlapping_segments", suppress.warnings=suppress.warnings),
                               if(!is.na(y.norm.v[j])) .SVG.lines(x=.scale.x.to.SVG.plot(c(corrected.x.start[j], corrected.x.start[j],
                                                                                           corrected.x.end[j],   corrected.x.end[j])),
                                                                  y=.scale.y.to.SVG.plot(c(y.cur + 0.5 + v[j], y.cur + 0.5 + v[j] + y.norm.v[j],
                                                                                           y.cur + 0.5 + v[j], y.cur + 0.5 + v[j] + y.norm.v[j])),
                                                                  connected=FALSE, stroke=plot.partial.CMAs.as.overlapping.col.interval,
                                                                  class="partial_cma_overlapping_segments", suppress.warnings=suppress.warnings)
                  );
                }
              }
            }

            if( .do.R ) # Rplot:
            {
              if( print.CMA && (force.draw.text || char.height.CMA <= 0.80) && !is.na(plot.partial.CMAs.as.overlapping.col.text) )
              {
                text(corrected.x.text, y.cur + 1.0, ppts$text, cex=CMA.cex, col=plot.partial.CMAs.as.overlapping.col.text);
              }
            }

            if( .do.SVG ) # SVG:
            {
              if( print.CMA && (force.draw.text || dims.chr.cma <= dims.chr.event) && !is.na(plot.partial.CMAs.as.overlapping.col.text) )
              {
                svg.str <- c(svg.str,
                             # The text estimates:
                             .SVG.text(x=.scale.x.to.SVG.plot(corrected.x.text), y=.scale.y.to.SVG.plot(rep(y.cur + 1.0,length(corrected.x.text))), text=ppts$text,
                                       col=plot.partial.CMAs.as.overlapping.col.text, font_size=dims.chr.cma,
                                       h.align="center", v.align="center",
                                       class="partial_cma_overlapping_text", suppress.warnings=suppress.warnings)
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
                                      class="partial_cma_timeseries_axes", suppress.warnings=suppress.warnings)
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
                                        class="partial_cma_timeseries_0perc-line", suppress.warnings=suppress.warnings)
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
                                        class="partial_cma_timeseries_100perc-line", suppress.warnings=suppress.warnings)
                );
              }
            }

            # Numeric values:
            if( .do.R ) # Rplot:
            {
              if( print.CMA && (force.draw.text || char.height.CMA <= 0.80) )
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
              if( print.CMA && (force.draw.text || dims.chr.cma <= dims.chr.event) )
              {
                svg.str <- c(svg.str,
                             # Text
                             .SVG.text(x=.scale.x.to.SVG.plot(c(corrected.x + x.start.min, corrected.x + x.start.min)),
                                       y=.scale.y.to.SVG.plot(c(min.y.norm,                max.y.norm)),
                                       text=c(                sprintf("%.1f%%",100*min.y), sprintf("%.1f%%",100*max.y)),
                                       col="black", font_size=dims.chr.cma, h.align="right", v.align="center", rotate=rotate.text,
                                       class="partial_cma_timeseries_axis_text", suppress.warnings=suppress.warnings),
                             if( plot.partial.CMAs.as.timeseries.show.0perc && y.for.0perc >= y.cur + 0.5 )
                             {
                               .SVG.text(x=.scale.x.to.SVG.plot(corrected.x + x.start.min),
                                         y=.scale.y.to.SVG.plot(y.for.0perc),
                                         text="0%",
                                         col="red", font_size=dims.chr.cma, h.align="right", v.align="center", rotate=rotate.text,
                                         class="partial_cma_timeseries_axis_text", suppress.warnings=suppress.warnings)
                             },
                             if( plot.partial.CMAs.as.timeseries.show.100perc && y.for.100perc <= y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0 )
                             {
                               .SVG.text(x=.scale.x.to.SVG.plot(corrected.x + x.start.min),
                                         y=.scale.y.to.SVG.plot(y.for.100perc),
                                         text="100%",
                                         col="red", font_size=dims.chr.cma, h.align="right", v.align="center", rotate=rotate.text,
                                         class="partial_cma_timeseries_axis_text", suppress.warnings=suppress.warnings)
                             }
                );
              }
            }

            # The intervals:
            if( !is.na(plot.partial.CMAs.as.timeseries.col.interval) )
            {
              if( plot.partial.CMAs.as.timeseries.interval.type == "none" )
              {
                # Nothing to plot, but save the actual points:
                if( .do.R )
                {
                  .last.cma.plot.info$baseR$partialCMAs <- rbind(.last.cma.plot.info$baseR$partialCMAs,
                                                                 data.frame("pid"=cur_plot_id, type="timeseries",
                                                                            "x.region.start"=corrected.x + x.start.min,
                                                                            "y.region.start"=y.cur + 0.5,
                                                                            "x.region.end"=corrected.x + x.end.max,
                                                                            "y.region.end"=y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0,
                                                                            "x.partial.start"=corrected.x.text[!is.na(ppts$y.norm)],
                                                                            "y.partial.start"=ppts$y.norm[!is.na(ppts$y.norm)],
                                                                            "x.partial.end"=corrected.x.text[!is.na(ppts$y.norm)],
                                                                            "y.partial.end"=ppts$y.norm[!is.na(ppts$y.norm)]));
                }
                if( .do.SVG )
                {
                  .last.cma.plot.info$SVG$partialCMAs <- rbind(.last.cma.plot.info$SVG$partialCMAs,
                                                               data.frame("pid"=cur_plot_id, type="timeseries",
                                                                          "x.region.start"=.scale.x.to.SVG.plot(corrected.x + x.start.min),
                                                                          "y.region.start"=.scale.y.to.SVG.plot(y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0),
                                                                          "x.region.end"=.scale.x.to.SVG.plot(corrected.x + x.end.max),
                                                                          "y.region.start"=.scale.y.to.SVG.plot(y.cur + 0.5),
                                                                          "x.partial.start"=.scale.x.to.SVG.plot(corrected.x.text[!is.na(ppts$y.norm)]),
                                                                          "y.partial.start"=.scale.y.to.SVG.plot(ppts$y.norm[!is.na(ppts$y.norm)]),
                                                                          "x.partial.end"=.scale.x.to.SVG.plot(corrected.x.text[!is.na(ppts$y.norm)]),
                                                                          "y.partial.end"=.scale.y.to.SVG.plot(ppts$y.norm[!is.na(ppts$y.norm)])));
                }
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
                    # Save the info:
                    .last.cma.plot.info$baseR$partialCMAs <- rbind(.last.cma.plot.info$baseR$partialCMAs,
                                                                   data.frame("pid"=cur_plot_id, type="timeseries",
                                                                              "x.region.start"=corrected.x + x.start.min,
                                                                              "y.region.start"=y.cur + 0.5,
                                                                              "x.region.end"=corrected.x + x.end.max,
                                                                              "y.region.end"=y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0,
                                                                              "x.partial.start"=corrected.x.start[!is.na(ppts$y.norm)],
                                                                              "y.partial.start"=ppts$y.norm[!is.na(ppts$y.norm)] - 0.2,
                                                                              "x.partial.end"=corrected.x.end[!is.na(ppts$y.norm)],
                                                                              "y.partial.end"=ppts$y.norm[!is.na(ppts$y.norm)] + 0.2));
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
                    # Save the info:
                    .last.cma.plot.info$baseR$partialCMAs <- rbind(.last.cma.plot.info$baseR$partialCMAs,
                                                                   data.frame("pid"=cur_plot_id, type="timeseries",
                                                                              "x.region.start"=corrected.x + x.start.min,
                                                                              "y.region.start"=y.cur + 0.5,
                                                                              "x.region.end"=corrected.x + x.end.max,
                                                                              "y.region.end"=y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0,
                                                                              "x.partial.start"=corrected.x.start[!is.na(ppts$y.norm)],
                                                                              "y.partial.start"=ppts$y.norm[!is.na(ppts$y.norm)] - char.height/2,
                                                                              "x.partial.end"=corrected.x.end[!is.na(ppts$y.norm)],
                                                                              "y.partial.end"=ppts$y.norm[!is.na(ppts$y.norm)] + char.height/2));
                  } else
                  {
                    # Just the lines:
                    # Save the info:
                    .last.cma.plot.info$baseR$partialCMAs <- rbind(.last.cma.plot.info$baseR$partialCMAs,
                                                                   data.frame("pid"=cur_plot_id, type="timeseries",
                                                                              "x.region.start"=corrected.x + x.start.min,
                                                                              "y.region.start"=y.cur + 0.5,
                                                                              "x.region.end"=corrected.x + x.end.max,
                                                                              "y.region.end"=y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0,
                                                                              "x.partial.start"=corrected.x.start[!is.na(ppts$y.norm)],
                                                                              "y.partial.start"=ppts$y.norm[!is.na(ppts$y.norm)],
                                                                              "x.partial.end"=corrected.x.end[!is.na(ppts$y.norm)],
                                                                              "y.partial.end"=ppts$y.norm[!is.na(ppts$y.norm)]));
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
                                            class="partial_cma_timeseries_lines", suppress.warnings=suppress.warnings),
                                 if( plot.partial.CMAs.as.timeseries.interval.type == "segments" )
                                 {
                                   # The segment endings:
                                   .SVG.lines(x=.scale.x.to.SVG.plot(c(corrected.x.start[j], corrected.x.start[j], corrected.x.end[j],   corrected.x.end[j])),
                                              y=.scale.y.to.SVG.plot(c(ppts$y.norm[j] - 0.2, ppts$y.norm[j] + 0.2, ppts$y.norm[j] - 0.2, ppts$y.norm[j] + 0.2)),
                                              connected=FALSE,
                                              stroke=plot.partial.CMAs.as.timeseries.col.interval, stroke_width=plot.partial.CMAs.as.timeseries.lwd.interval,
                                              class="partial_cma_timeseries_lines", suppress.warnings=suppress.warnings)
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
                                              class="partial_cma_timeseries_lines", suppress.warnings=suppress.warnings)
                                 }
                    );
                  }
                  # Save the info:
                  if( plot.partial.CMAs.as.timeseries.interval.type == "segments" )
                  {
                    .last.cma.plot.info$SVG$partialCMAs <- rbind(.last.cma.plot.info$SVG$partialCMAs,
                                                                 data.frame("pid"=cur_plot_id, "type"="timeseries",
                                                                            "x.region.start"=.scale.x.to.SVG.plot(corrected.x + x.start.min),
                                                                            "y.region.start"=.scale.y.to.SVG.plot(y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0),
                                                                            "x.region.end"=.scale.x.to.SVG.plot(corrected.x + x.end.max),
                                                                            "y.region.end"=.scale.y.to.SVG.plot(y.cur + 0.5),
                                                                            "x.partial.start"=.scale.x.to.SVG.plot(corrected.x.start[!is.na(ppts$y.norm)]),
                                                                            "y.partial.start"=.scale.y.to.SVG.plot(ppts$y.norm[!is.na(ppts$y.norm)] - 0.2),
                                                                            "x.partial.end"=.scale.x.to.SVG.plot(corrected.x.end[!is.na(ppts$y.norm)]),
                                                                            "y.partial.end"=.scale.y.to.SVG.plot(ppts$y.norm[!is.na(ppts$y.norm)] + 0.2)));
                  } else if( plot.partial.CMAs.as.timeseries.interval.type == "arrows" )
                  {
                    .last.cma.plot.info$SVG$partialCMAs <- rbind(.last.cma.plot.info$SVG$partialCMAs,
                                                                 data.frame("pid"=cur_plot_id, "type"="timeseries",
                                                                            "x.region.start"=.scale.x.to.SVG.plot(corrected.x + x.start.min),
                                                                            "y.region.start"=.scale.y.to.SVG.plot(y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0),
                                                                            "x.region.end"=.scale.x.to.SVG.plot(corrected.x + x.end.max),
                                                                            "y.region.end"=.scale.y.to.SVG.plot(y.cur + 0.5),
                                                                            "x.partial.start"=.scale.x.to.SVG.plot(corrected.x.start[!is.na(ppts$y.norm)]),
                                                                            "y.partial.start"=.scale.y.to.SVG.plot(ppts$y.norm[!is.na(ppts$y.norm)] - 0.2),
                                                                            "x.partial.end"=.scale.x.to.SVG.plot(corrected.x.end[!is.na(ppts$y.norm)]),
                                                                            "y.partial.end"=.scale.y.to.SVG.plot(ppts$y.norm[!is.na(ppts$y.norm)] + 0.2)));
                  } else # just lines
                  {
                    .last.cma.plot.info$SVG$partialCMAs <- rbind(.last.cma.plot.info$SVG$partialCMAs,
                                                                 data.frame("pid"=cur_plot_id, "type"="timeseries",
                                                                            "x.region.start"=.scale.x.to.SVG.plot(corrected.x + x.start.min),
                                                                            "y.region.start"=.scale.y.to.SVG.plot(y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0),
                                                                            "x.region.end"=.scale.x.to.SVG.plot(corrected.x + x.end.max),
                                                                            "y.region.end"=.scale.y.to.SVG.plot(y.cur + 0.5),
                                                                            "x.partial.start"=.scale.x.to.SVG.plot(corrected.x.start[!is.na(ppts$y.norm)]),
                                                                            "y.partial.start"=.scale.y.to.SVG.plot(ppts$y.norm[!is.na(ppts$y.norm)]),
                                                                            "x.partial.end"=.scale.x.to.SVG.plot(corrected.x.end[!is.na(ppts$y.norm)]),
                                                                            "y.partial.end"=.scale.y.to.SVG.plot(ppts$y.norm[!is.na(ppts$y.norm)])));
                  }
                }
              } else if( plot.partial.CMAs.as.timeseries.interval.type == "rectangles" )
              {
                if( .do.R ) # Rplot:
                {
                  # As semi-transparent rectangles:
                  rect(corrected.x.start, y.cur + 0.5, corrected.x.end, y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0,
                       #col=scales::alpha(plot.partial.CMAs.as.timeseries.col.interval, alpha=plot.partial.CMAs.as.timeseries.alpha.interval),
                       col=adjustcolor(plot.partial.CMAs.as.timeseries.col.interval, alpha.f=plot.partial.CMAs.as.timeseries.alpha.interval),
                       border=plot.partial.CMAs.as.timeseries.col.interval, lty="dotted");
                  # Save the info:
                  .last.cma.plot.info$baseR$partialCMAs <- rbind(.last.cma.plot.info$baseR$partialCMAs,
                                                                 data.frame("pid"=cur_plot_id, "type"="timeseries",
                                                                            "x.region.start"=corrected.x + x.start.min,
                                                                            "y.region.start"=y.cur + 0.5,
                                                                            "x.region.end"=corrected.x + x.end.max,
                                                                            "y.region.end"=y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0,
                                                                            "x.partial.start"=corrected.x.start[!is.na(ppts$y.norm)],
                                                                            "y.partial.start"=y.cur + 0.5,
                                                                            "x.partial.end"=corrected.x.end[!is.na(ppts$y.norm)],
                                                                            "y.partial.end"=y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0));
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
                  # Save the info:
                  .last.cma.plot.info$SVG$partialCMAs <- rbind(.last.cma.plot.info$SVG$partialCMAs,
                                                               data.frame("pid"=cur_plot_id, "type"="timeseries",
                                                                          "x.region.start"=.scale.x.to.SVG.plot(corrected.x + x.start.min),
                                                                          "y.region.start"=.scale.y.to.SVG.plot(y.cur + 0.5),
                                                                          "x.region.end"=.scale.x.to.SVG.plot(corrected.x + x.end.max),
                                                                          "y.region.end"=.scale.y.to.SVG.plot(y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0),
                                                                          "x.partial.start"=.scale.x.to.SVG.plot(corrected.x.start[!is.na(ppts$y.norm)]),
                                                                          "y.partial.start"=.scale.y.to.SVG.plot(y.cur + 0.5),
                                                                          "x.partial.end"=.scale.x.to.SVG.plot(corrected.x.end[!is.na(ppts$y.norm)]),
                                                                          "y.partial.end"=.scale.y.to.SVG.plot(y.cur + plot.partial.CMAs.as.timeseries.vspace - 1.0)));
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
                                        class="partial_cma_timeseries_connecting_lines", suppress.warnings=suppress.warnings),
                             # The points:
                             .SVG.points(x=.scale.x.to.SVG.plot(corrected.x.text), y=.scale.y.to.SVG.plot(ppts$y.norm),
                                         col=plot.partial.CMAs.as.timeseries.col.dot, cex=CMA.cex, pch=19,
                                         class="partial_cma_timeseries_points", suppress.warnings=suppress.warnings)
                );
              }
            }

            # The actual values:
            if( .do.R ) # Rplot:
            {
              if( print.CMA && (force.draw.text || char.height.CMA <= 0.80) && !is.na(plot.partial.CMAs.as.timeseries.col.text) )
              {
                text(corrected.x.text, ppts$y.norm, ppts$text, adj=c(0.5,-0.5), cex=CMA.cex, col=plot.partial.CMAs.as.timeseries.col.text);
              }
            }
            if( .do.SVG ) # SVG:
            {
              if( print.CMA && (force.draw.text || dims.chr.cma <= dims.chr.event) && !is.na(plot.partial.CMAs.as.timeseries.col.text) )
              {
                svg.str <- c(svg.str,
                             # The actual values:
                             .SVG.text(x=.scale.x.to.SVG.plot(corrected.x.text), y=.scale.y.to.SVG.plot(ppts$y.norm) + dims.chr.cma, text=ppts$text,
                                       col=plot.partial.CMAs.as.timeseries.col.text, font_size=dims.chr.cma,
                                       h.align="center", v.align="center",
                                       class="partial_cma_timeseries_values", suppress.warnings=suppress.warnings)
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
  if( has.estimated.CMA && adh.plot.space[2] > 0 )
  {
    if( is.cma.TS.or.SW )
    {
      if( .do.R ) # Rplot:
      {
        # Background:
        rect(.rescale.xcoord.for.CMA.plot(0.0), par("usr")[3], .rescale.xcoord.for.CMA.plot(max(adh.max,1.0)), par("usr")[4], col=adjustcolor(CMA.plot.bkg,alpha.f=0.25), border=NA);
        # Vertical guides:
        abline(v=c(.rescale.xcoord.for.CMA.plot(0.0), .rescale.xcoord.for.CMA.plot(1.0)), col=CMA.plot.col, lty=c("solid","dotted"), lwd=1);
      }

      if( .do.SVG ) # SVG:
      {
        svg.str <- c(svg.str,
                     # Background:
                     .SVG.rect(x=.scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(0.0)),
                               y=dims.plot.y + dims.adjust.for.tall.legend,
                               width=.scale.width.to.SVG.plot(.rescale.xcoord.for.CMA.plot(adh.max)),
                               height=dims.plot.height,
                               stroke="none", fill=CMA.plot.bkg, fill_opacity=0.25,
                               class="cma-drawing-area-bkg", tooltip="CMA estimate"),

                     # Vertical guides:
                     .SVG.comment("The vertical guides for the CMA drawing area"),
                     .SVG.lines(x=rep(c(.scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(0.0)), .scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(1.0))), each=2),
                                y=rep(c(dims.plot.y, dims.plot.y + dims.plot.height), times=2) + dims.adjust.for.tall.legend,
                                connected=FALSE,
                                stroke=CMA.plot.border, stroke_width=1, lty=c("solid", "dotted"),
                                class="cma-drawing-area-guides-lines", suppress.warnings=suppress.warnings)
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
                               y=dims.plot.y + dims.adjust.for.tall.legend,
                               width=.scale.width.to.SVG.plot(.rescale.xcoord.for.CMA.plot(adh.max)),
                               height=dims.plot.height,
                               stroke="none", fill=CMA.plot.bkg, fill_opacity=0.25,
                               class="cma-drawing-area-bkg", tooltip="CMA estimate"),

                     # Vertical guides:
                     .SVG.lines(x=rep(c(.scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(0.0)),
                                        .scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(1.0)),
                                        if(adh.max > 1.0) .scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(adh.max))),
                                      each=2),
                                y=rep(c(dims.plot.y, dims.plot.y + dims.plot.height), times=ifelse(adh.max > 1.0, 3, 2)) + dims.adjust.for.tall.legend,
                                connected=FALSE,
                                stroke=CMA.plot.border, stroke_width=1, lty=if(adh.max > 1.0) c("solid", "dotted", "solid") else "solid",
                                class="cma-drawing-area-guides-lines", suppress.warnings=suppress.warnings),

                     # Text guides:
                     .SVG.text(x=.scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(0.0)), y=(dims.plot.y + dims.adjust.for.tall.legend - dims.chr.axis/2),
                               text="0%", col="black", font="Arial", font_size=dims.chr.axis, h.align="left", v.align="center", rotate=-(90+rotate.text),
                               class="cma-drawing-area-guides-text", suppress.warnings=suppress.warnings),
                     if(adh.max > 1.0)
                     {
                       c(
                         .SVG.text(x=.scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(adh.max)), y=(dims.plot.y + dims.adjust.for.tall.legend - dims.chr.axis/2),
                                   text=sprintf("%.1f%%",adh.max*100), col="black", font="Arial", font_size=dims.chr.axis, h.align="left", v.align="center", rotate=-30,
                                   class="cma-drawing-area-guides-text", suppress.warnings=suppress.warnings),
                         if(dims.event.x*(.rescale.xcoord.for.CMA.plot(adh.max) - .rescale.xcoord.for.CMA.plot(1.0))/dims.day > 2.0*dims.chr.axis)
                         {
                           # Don't overcrowd the 100% and maximum CMA
                           .SVG.text(x=.scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(1.0)), y=(dims.plot.y + dims.adjust.for.tall.legend - dims.chr.axis/2),
                                     text="100%", col="black", font="Arial", font_size=dims.chr.axis, h.align="left", v.align="center", rotate=-(90+rotate.text),
                                     class="cma-drawing-area-guides-text", suppress.warnings=suppress.warnings)
                         }
                       )
                     } else
                     {
                       .SVG.text(x=.scale.x.to.SVG.plot(.rescale.xcoord.for.CMA.plot(1.0)), y=(dims.plot.y + dims.adjust.for.tall.legend - dims.chr.axis/2),
                                 text="100%", col="black", font="Arial", font_size=dims.chr.axis, h.align="left", v.align="center", rotate=-(90+rotate.text),
                                 class="cma-drawing-area-guides-text", suppress.warnings=suppress.warnings)
                     }
        );
      }
    }
  }


  ##
  ## Title, box and axes ####
  ##

  title.string <- paste0(ifelse(is.null(title),"", # the plot title
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
    xlab.string <- ifelse(is.null(xlab), # x axis label
                      "",
                      ifelse(length(xlab)==1,
                             xlab,
                             xlab[show.period]));
    title(main=title.string, # title
          xlab=xlab.string, cex.lab=cex.lab);
    mtext(y.label$string, side=2, line=par("mar")[2]-1, at=(par("usr")[4] + par("usr")[3])/2, cex=cex.lab, las=3); # y-axis label

    # Save the info:
    .last.cma.plot.info$baseR$title   <- data.frame("string"=title.string,   "x"=NA, "y"=NA, "cex"=NA);
    .last.cma.plot.info$baseR$x.label <- data.frame("string"=xlab.string,    "x"=NA, "y"=NA, "cex"=cex.lab);
    .last.cma.plot.info$baseR$y.label <- data.frame("string"=y.label$string, "x"=par("mar")[2]-1, "y"=(par("usr")[4] + par("usr")[3])/2, "cex"=cex.lab);
  }

  if( .do.SVG ) # SVG:
  {
    svg.str <- c(svg.str,
                 # The bounding box:
                 .SVG.rect(x=dims.plot.x,
                           y=dims.plot.y,
                           width=dims.plot.width,
                           height=dims.plot.height + dims.adjust.for.tall.legend,
                           stroke="black", stroke_width=1, fill="none",
                           class="bounding-box", comment="The bounding box"),

                 # The title:
                 .SVG.text(x=(dims.plot.x + dims.total.width)/2, y=dims.chr.title,
                           text=title.string, col="black", font="Arial", font_size=dims.chr.title, h.align="center", v.align="center",
                           class="main-title", comment="The main title", suppress.warnings=suppress.warnings),

                 # The y axis label:
                 .SVG.text(x=dims.chr.axis, y=dims.total.height/2,
                           text=as.character(y.label$string), col="black", font="Arial", font_size=dims.chr.lab, h.align="center", v.align="center", rotate=-90,
                           class="axis-name-y", comment="The y-axis label", tooltip="Y axis: patients, events and (possibly) CMA estimates", suppress.warnings=suppress.warnings),

                 # The x axis label:
                 .SVG.text(x=(dims.plot.x + dims.total.width)/2, y=(dims.total.height - dims.chr.axis),
                           text=as.character(x.label), col="black", font="Arial", font_size=dims.chr.lab, h.align="center", v.align="center",
                           class="axis-name-x", comment="The x-axis label", tooltip="X axis: the events ordered in time (from left to right)", suppress.warnings=suppress.warnings)
    );

    # Save the info:
    .last.cma.plot.info$SVG$title   <- data.frame("string"=title.string, "x"=(dims.plot.x + dims.total.width)/2, "y"=dims.chr.title, "font.size"=dims.chr.title);
    .last.cma.plot.info$SVG$x.label <- data.frame("string"=as.character(x.label), "x"=(dims.plot.x + dims.total.width)/2, "y"=(dims.total.height - dims.chr.axis), "font.size"=dims.chr.lab);
    .last.cma.plot.info$SVG$y.label <- data.frame("string"=as.character(y.label$string), "x"=dims.chr.axis, "y"=dims.total.height/2, "font.size"=dims.chr.lab);
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
          # Correctly deal with events starting before the FUW (i.e., correct.earliest.followup.window < 0):
          xpos <- c(correct.earliest.followup.window - seq(0, correct.earliest.followup.window, by=period.in.days * sign(correct.earliest.followup.window)),
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

      # Save the info:
      .last.cma.plot.info$baseR$x.labels <- data.frame("string"=axis.labels, "x"=adh.plot.space[2] + xpos, "y"=par("usr")[3], "cex"=cex.axis);
    }
  }

  if( .do.SVG ) # SVG:
  {
    if( !is.null(date.labels) )
    {
      xs <- (dims.plot.x + dims.event.x * date.labels$position/dims.day);
      ys <- (dims.plot.y + dims.plot.height + dims.chr.axis + dims.adjust.for.tall.legend);
      svg.str <- c(svg.str,
                   # Axis labels:
                   .SVG.text(x=xs, y=rep(ys, length(xs)),
                             text=as.character(date.labels$string), col="black", font="Arial", font_size=dims.chr.axis, h.align="right", v.align="center", rotate=-(90+rotate.text),
                             class="axis-labels-x", suppress.warnings=suppress.warnings),

                   # Axis ticks:
                   .SVG.lines(x=rep(xs,each=2),
                              y=dims.plot.y + dims.plot.height + dims.adjust.for.tall.legend + rep(c(0, dims.chr.axis/2), times=length(xs)),
                              connected=FALSE,
                              stroke="black", stroke_width=1,
                              class="axis-ticks-x", suppress.warnings=suppress.warnings),

                   # Vertical dotted lines:
                   .SVG.lines(x=rep(xs,each=2),
                              y=dims.plot.y + rep(c(dims.plot.height + dims.adjust.for.tall.legend, 0), times=length(xs)),
                              connected=FALSE,
                              stroke="gray50", stroke_width=1, lty="dotted",
                              class="vertical-date-lines", suppress.warnings=suppress.warnings)
      );

      # Save the info:
      .last.cma.plot.info$SVG$x.labels <- data.frame("string"=as.character(date.labels$string), "x"=xs, "y"=rep(ys, length(xs)), "font.size"=dims.chr.axis);
    }
  }


  ##
  ## The legend ####
  ##

  if( show.legend )
  {
    if( .do.R ) # Rplot:
    {
      # Character size for the legend:
      legend.char.width <- strwidth("O",cex=legend.cex); legend.char.height <- strheight("O",cex=legend.cex);

      legend.size <- .legend.R(do.plot=FALSE);
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
      ret.val <- .legend.R(x, y, as.numeric(legend.size["width"]), as.numeric(legend.size["height"]));
      # Remove superfluous rownames from the saved info:
      if( !is.null(.last.cma.plot.info$baseR$legend$box) ) rownames(.last.cma.plot.info$baseR$legend$box) <- NULL;
      if( !is.null(.last.cma.plot.info$baseR$legend$title) ) rownames(.last.cma.plot.info$baseR$legend$title) <- NULL;
      if( !is.null(.last.cma.plot.info$baseR$legend$components) ) rownames(.last.cma.plot.info$baseR$legend$components) <- NULL;
    }

    if( .do.SVG ) # SVG:
    {
      # Compute the bounding box of the legend without showing it yet:
      .legend.SVG(legend.x, legend.y, do.plot=FALSE);

      # Display the legend where it should be displayed:
      svg.str <- c(svg.str,
                   # The legend:
                   .legend.SVG(.last.cma.plot.info$SVG$legend$box$x.start, .last.cma.plot.info$SVG$legend$box$y.start + dims.adjust.for.tall.legend, do.plot=TRUE)
      );

      # Remove superfluous rownames from the saved info:
      if( !is.null(.last.cma.plot.info$SVG$legend$box) ) rownames(.last.cma.plot.info$SVG$legend$box) <- NULL;
      if( !is.null(.last.cma.plot.info$SVG$legend$title) ) rownames(.last.cma.plot.info$SVG$legend$title) <- NULL;
      if( !is.null(.last.cma.plot.info$SVG$legend$components) ) rownames(.last.cma.plot.info$SVG$legend$components) <- NULL;
    }
  }


  #
  # Finish and possibly export the file(s) ####
  #

  if( .do.R ) # Rplot:
  {
    #par(old.par); # restore graphical params
  }

  exported.file.names <- NULL; # the list of exported files (if any)
  if( .do.SVG ) # Close the <sgv> tag:
  {
    svg.str <- c(svg.str, '</svg>\n');

    # Export to various formats (if so requested):
    if( !is.null(export.formats) )
    {
      file.svg <- NULL;
      svg.placeholder.filename <- NULL;
      if( "svg" %in% export.formats )
      {
        ## Export as stand-alone SVG file ####
        file.svg <- ifelse( is.na(export.formats.directory),
                            tempfile(export.formats.fileprefix, fileext=".svg"),
                            file.path(export.formats.directory, paste0(export.formats.fileprefix,".svg")) );
        exported.file.names <- c(exported.file.names, file.svg);

        # Export SVG:
        writeLines(c(svg.header, svg.str), file.svg, sep="");
      }

      if( "html" %in% export.formats )
      {
        ## Export as self-contained HTML document ####
        file.html <- ifelse( is.na(export.formats.directory),
                             tempfile(export.formats.fileprefix, fileext=".html"),
                             file.path(export.formats.directory, paste0(export.formats.fileprefix,".html")) );
        exported.file.names <- c(exported.file.names, file.html);

        # Load the CSS template:
        if( !is.null(export.formats.html.css) )
        {
          # Try to load the given template:
          if( length(export.formats.html.css) != 1 || !file.exists(as.character(export.formats.html.css)) )
          {
            if( !suppress.warnings ) .report.ewms("The given CSS template '",as.character(export.formats.html.css),"' does not seem to exist: falling back to the default one!\n", "warning", ".plot.CMAs", "AdhereR");
            export.formats.html.css <- NULL;
          } else
          {
            css.template.path <- as.character(export.formats.html.css);
          }
        }
        if( is.null(export.formats.html.css) )
        {
          # Load the default CSS template:
          css.template.path <- system.file('html-templates/css-template.css', package='AdhereR');
          if( is.null(css.template.path) || css.template.path=="" )
          {
            if( !suppress.warnings ) .report.ewms("Cannot load the CSS template -- please reinstall the AdhereR package!\n", "error", ".plot.CMAs", "AdhereR");
            .last.cma.plot.info$SVG <- NULL;
            assign(".last.cma.plot.info", .last.cma.plot.info, envir=.adherer.env); # save the plot infor into the environment
            plot.CMA.error(export.formats=export.formats,
                           export.formats.fileprefix=export.formats.fileprefix,
                           export.formats.directory=export.formats.directory,
                           generate.R.plot=FALSE);
            return (invisible(NULL));
          }
        }

        # Load the JavaScript template:
        if( !is.null(export.formats.html.javascript) )
        {
          # Try to load the given template:
          if( length(export.formats.html.javascript) != 1 || !file.exists(as.character(export.formats.html.javascript)) )
          {
            if( !suppress.warnings ) .report.ewms("The given JavaScript template '",as.character(export.formats.html.javascript),"' does not seem to exist: falling back to the default one!\n", "warning", ".plot.CMAs", "AdhereR");
            export.formats.html.javascript <- NULL;
          } else
          {
            js.template.path <- as.character(export.formats.html.javascript);
          }
        }
        if( is.null(export.formats.html.javascript) )
        {
          # Load the default JavaScript template:
          js.template.path <- system.file('html-templates/javascript-template.js', package='AdhereR');
          if( is.null(js.template.path) || js.template.path=="" )
          {
            if( !suppress.warnings ) .report.ewms("Cannot load the JavaScript template -- please reinstall the AdhereR package!\n", "error", ".plot.CMAs", "AdhereR");
            .last.cma.plot.info$SVG <- NULL;
            assign(".last.cma.plot.info", .last.cma.plot.info, envir=.adherer.env); # save the plot infor into the environment
            plot.CMA.error(export.formats=export.formats,
                           export.formats.fileprefix=export.formats.fileprefix,
                           export.formats.directory=export.formats.directory,
                           generate.R.plot=FALSE);
            return (invisible(NULL));
          }
        }

        # Read the templates:
        css.template <- readLines(css.template.path);
        js.template  <- readLines(js.template.path);

        # Add the medication categories to class names mapping as a dictionary:
        js.template <- c(js.template,
                         '// Mapping between medication categories and -med-class-X class names',
                         'adh_svg["medication_classes"] = {\n',
                         paste0('  "',names(categories.to.classes),'" : "',categories.to.classes,'"',collapse=",\n"),
                         '\n};\n');

        if( export.formats.save.svg.placeholder )
        {
          # Check that base64 exists:
          if( !requireNamespace("base64", quietly=TRUE) )
          {
            # Does not seem to:
            if( !suppress.warnings ) .report.ewms("Package 'base64' required for emedding images in HTML documents does not seem to exist: we'll store the images outside the HTML document!\n", "warning", ".plot.CMAs", "AdhereR");
            export.formats.svg.placeholder.embed <- FALSE;
          }

          if( !export.formats.svg.placeholder.embed )
          {
            # The SVG placeholder is an external file: add its filename:
            svg.placeholder.filename <- ifelse( is.na(export.formats.directory),
                                                tempfile(paste0(export.formats.fileprefix,"-svg-placeholder"), fileext=paste0(".",export.formats.svg.placeholder.type)),
                                                file.path(export.formats.directory, paste0(paste0(export.formats.fileprefix,"-svg-placeholder"),paste0(".",export.formats.svg.placeholder.type))) );
            js.template <- c(js.template,
                             "// The SVG placeholder's filename:",
                             paste0('adh_svg["svg_placeholder_file_name"] = "',basename(svg.placeholder.filename),'";\n'));
          } else
          {
            # The SVG placeholder must be embedded in base_64 encoded-form into en <img> tag:
            if( !(export.formats.svg.placeholder.type %in% c("jpg", "png")) )
            {
              if( !suppress.warnings ) .report.ewms("Can only embed a JPEG or PNG image as a placeholder for the SVG image: defaulting to JPEG!\n", "warning", ".plot.CMAs", "AdhereR");
              export.formats.svg.placeholder.type <- "jpg";
            }

            # Base encode the placeholder:
            # Need to covert the SVG to one of these, so we need to export it (if not already exported):
            if( is.null(file.svg) )
            {
              file.svg <- tempfile(export.formats.fileprefix, fileext=".svg");
              writeLines(c(svg.header, svg.str), file.svg, sep="");
            }

            # Convert the SVG:
            bitmap <- rsvg::rsvg(file.svg,
                                 height=if(!is.na(export.formats.height)) export.formats.height else dims.total.height * 2, # prepare for high DPI/quality
                                 width =if(!is.na(export.formats.width))  export.formats.width  else NULL);
            svg.placeholder.tmpfile <- tempfile(paste0(export.formats.fileprefix,"-svg-placeholder"), fileext=paste0(".",export.formats.svg.placeholder.type));
            svg.placeholder.end64.tmpfile <- paste0(svg.placeholder.tmpfile,"-enc64.txt");
            if( export.formats.svg.placeholder.type == "jpg" )
            {
              jpeg::writeJPEG(bitmap, svg.placeholder.tmpfile, quality=0.90);
            } else if( export.formats.svg.placeholder.type == "png" )
            {
              png::writePNG(bitmap, svg.placeholder.tmpfile, dpi=150);
            }

            # Encode it to base64:
            base64::encode(svg.placeholder.tmpfile, svg.placeholder.end64.tmpfile, linebreaks=FALSE);

            # Load it and embed it:
            svg.placeholder.end64 <- try(readLines(svg.placeholder.end64.tmpfile), silent=TRUE);
            if( inherits(svg.placeholder.end64, "try-error") )
            {
              if( !suppress.warnings ) .report.ewms("Failed embedding an image in the HTML document: reverting to having it as an external file!\n", "warning", ".plot.CMAs", "AdhereR");
              export.formats.svg.placeholder.embed <- FALSE;
              try(unlink(c(svg.placeholder.tmpfile, svg.placeholder.end64.tmpfile)), silent=TRUE); # clean up the temp files

              # The SVG placeholder is an external file: add its filename:
              svg.placeholder.filename <- ifelse( is.na(export.formats.directory),
                                                  tempfile(paste0(export.formats.fileprefix,"-svg-placeholder"), fileext=paste0(".",export.formats.svg.placeholder.type)),
                                                  file.path(export.formats.directory, paste0(paste0(export.formats.fileprefix,"-svg-placeholder"),paste0(".",export.formats.svg.placeholder.type))) );
              js.template <- c(js.template,
                               "// The SVG placeholder's filename:",
                               paste0('adh_svg["svg_placeholder_file_name"] = "',basename(svg.placeholder.filename),'";\n'));
            } else
            {
              js.template <- c(js.template,
                               "// The SVG placeholder's content:",
                               paste0('adh_svg["svg_placeholder_file_name"] = "data:image/',
                                      ifelse(export.formats.svg.placeholder.type == "png", "png", "jpeg"),
                                      ';base64,',svg.placeholder.end64,'";\n'));

              # Clean up the temp files
              try(unlink(c(svg.placeholder.tmpfile, svg.placeholder.end64.tmpfile)), silent=TRUE);
            }
          }
        }

        # Load the HTML template and replace generics by their actual values before saving it in the desired location:
        if( !is.null(export.formats.html.template) )
        {
          # Try to load the given template:
          if( length(export.formats.html.template) != 1 || !file.exists(as.character(export.formats.html.template)) )
          {
            if( !suppress.warnings ) .report.ewms("The given HTML template '",as.character(export.formats.html.template),"' does not seem to exist: falling back to the default one!\n", "warning", ".plot.CMAs", "AdhereR");
            export.formats.html.template <- NULL;
          } else
          {
            html.template.path <- as.character(export.formats.html.template);
          }
        }
        if( is.null(export.formats.html.template) )
        {
          # Load the default HTML template:
          html.template.path <- system.file('html-templates/html-template.html', package='AdhereR');
          if( is.null(html.template.path) || html.template.path=="" )
          {
            if( !suppress.warnings ) .report.ewms("Cannot load the HTML template -- please reinstall the AdhereR package!\n", "error", ".plot.CMAs", "AdhereR");
            .last.cma.plot.info$SVG <- NULL;
            assign(".last.cma.plot.info", .last.cma.plot.info, envir=.adherer.env); # save the plot infor into the environment
            plot.CMA.error(export.formats=export.formats,
                           export.formats.fileprefix=export.formats.fileprefix,
                           export.formats.directory=export.formats.directory,
                           generate.R.plot=FALSE);
            return (invisible(NULL));
          }
        }
        # Load it:
        html.template <- readLines(html.template.path);
        # Check place-holders and replace them with the actual values:
        if( length(grep('<script type="text/javascript" src="PATH-TO-JS"></script>', html.template, fixed=TRUE)) != 1 )
        {
          if( !suppress.warnings ) .report.ewms("The HTML template seems corrupted: there should 1 and only 1 '<script type=\"text/javascript\" src=\"PATH-TO-JS\"></script>'!\n", "error", ".plot.CMAs", "AdhereR");
          .last.cma.plot.info$SVG <- NULL;
          assign(".last.cma.plot.info", .last.cma.plot.info, envir=.adherer.env); # save the plot infor into the environment
          plot.CMA.error(export.formats=export.formats,
                         export.formats.fileprefix=export.formats.fileprefix,
                         export.formats.directory=export.formats.directory,
                         generate.R.plot=FALSE);
          return (invisible(NULL));
        }
        if( length(grep('<link rel="stylesheet" href="PATH-TO-CSS">', html.template, fixed=TRUE)) != 1 )
        {
          if( !suppress.warnings ) .report.ewms("The HTML template seems corrupted: there should 1 and only 1 '<link rel=\"stylesheet\" href=\"PATH-TO-CSS\">'!\n", "error", ".plot.CMAs", "AdhereR");
          .last.cma.plot.info$SVG <- NULL;
          assign(".last.cma.plot.info", .last.cma.plot.info, envir=.adherer.env); # save the plot infor into the environment
          plot.CMA.error(export.formats=export.formats,
                         export.formats.fileprefix=export.formats.fileprefix,
                         export.formats.directory=export.formats.directory,
                         generate.R.plot=FALSE);
          return (invisible(NULL));
        }
        html.template <- sub('<script type="text/javascript" src="PATH-TO-JS"></script>',
                             paste0('<script type="text/javascript">\n', paste0(js.template, collapse="\n"), '\n</script>'),
                             html.template, fixed=TRUE); # JavaScript
        html.template <- sub('<link rel="stylesheet" href="PATH-TO-CSS">',
                             paste0('<style>\n', paste0(css.template, collapse="\n"), '\n</style>'),
                             html.template, fixed=TRUE); # CSS

        # SVG:
        if( length(grep('<object id="adherence_plot" data="PATH-TO-IMAGE" type="image/svg+xml">Please use a modern browser!</object>', html.template, fixed=TRUE)) != 1 )
        {
          if( !suppress.warnings ) .report.ewms("The HTML template seems corrupted: there should 1 and only 1 '<object id=\"adherence_plot\" data=\"PATH-TO-IMAGE\" type=\"image/svg+xml\">Please use a modern browser!</object>'!\n", "error", ".plot.CMAs", "AdhereR");
          .last.cma.plot.info$SVG <- NULL;
          assign(".last.cma.plot.info", .last.cma.plot.info, envir=.adherer.env); # save the plot infor into the environment
          plot.CMA.error(export.formats=export.formats,
                         export.formats.fileprefix=export.formats.fileprefix,
                         export.formats.directory=export.formats.directory,
                         generate.R.plot=FALSE);
          return (invisible(NULL));
        }
        svg.str.embedded <- c('<svg id="adherence_plot" ', # add id and (possibly) the dimensions to the <svg> tag
                              if( TRUE ) 'height="600" ', # height (if defined)
                              if( FALSE ) 'width="600" ', # width (if defined)
                              svg.str[-1]);
        html.template <- sub('<object id="adherence_plot" data="PATH-TO-IMAGE" type="image/svg+xml">Please use a modern browser!</object>',
                             paste0(paste0(svg.str.embedded, collapse=""), "\n"),
                             html.template, fixed=TRUE); # SVG

        # Export the self-contained HTML document:
        writeLines(html.template, file.html, sep="\n");
      }

      if( export.formats.save.svg.placeholder ||
          any(c("jpg", "png", "ps", "pdf", "webp") %in% export.formats) )
      {
        ## Export to flat file formats (PNG, JPG, PS, PDF or WEBP) ####
        # Need to covert the SVG to one of these, so we need to export it (if not already exported):
        if( is.null(file.svg) )
        {
          file.svg <- tempfile(export.formats.fileprefix, fileext=".svg");
          writeLines(c(svg.header, svg.str), file.svg, sep="");
        }

        if( export.formats.save.svg.placeholder ||
            any(c("jpg", "png","webp") %in% export.formats) )
        {
          # For the bitmapped formats, render it once:
          bitmap <- rsvg::rsvg(file.svg,
                               height=if(!is.na(export.formats.height)) export.formats.height else dims.total.height * 2, # prepare for high DPI/quality
                               width =if(!is.na(export.formats.width))  export.formats.width  else NULL);

          if( export.formats.save.svg.placeholder && !is.null(svg.placeholder.filename) )
          {
            # The SVG placeholder:
            exported.file.names <- c(exported.file.names, svg.placeholder.filename);
            if( export.formats.svg.placeholder.type == "jpg" )
            {
              jpeg::writeJPEG(bitmap, svg.placeholder.filename, quality=0.90);
            } else if( export.formats.svg.placeholder.type == "png" )
            {
              png::writePNG(bitmap, svg.placeholder.filename, dpi=150);
            } else if( export.formats.svg.placeholder.type == "webp" )
            {
              webp::write_webp(bitmap, svg.placeholder.filename, quality=90);
            }
          }

          if( "jpg" %in% export.formats )
          {
            # JPG file:
            file.jpg <- ifelse( is.na(export.formats.directory),
                                tempfile(export.formats.fileprefix, fileext=".jpg"),
                                file.path(export.formats.directory, paste0(export.formats.fileprefix,".jpg")) );
            exported.file.names <- c(exported.file.names, file.jpg);
            jpeg::writeJPEG(bitmap, file.jpg, quality=0.90);
          }

          if( "png" %in% export.formats )
          {
            # PNG file:
            file.png <- ifelse( is.na(export.formats.directory),
                                tempfile(export.formats.fileprefix, fileext=".png"),
                                file.path(export.formats.directory, paste0(export.formats.fileprefix,".png")) );
            exported.file.names <- c(exported.file.names, file.png);
            #rsvg::rsvg_png(file.svg, file=file.png);
            png::writePNG(bitmap, file.png, dpi=150);
          }

          if( "webp" %in% export.formats )
          {
            # WEBP file:
            file.webp <- ifelse( is.na(export.formats.directory),
                                 tempfile(export.formats.fileprefix, fileext=".webp"),
                                 file.path(export.formats.directory, paste0(export.formats.fileprefix,".webp")) );
            exported.file.names <- c(exported.file.names, file.webp);
            #rsvg::rsvg_webp(file.svg, file=file.webp);
            webp::write_webp(bitmap, file.webp, quality=90);
          }
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


  ## Save plot info into the external environment ####
  assign(".last.cma.plot.info", .last.cma.plot.info, envir=.adherer.env);


  # Return value:
  return (invisible(exported.file.names));
}

## The error plotting function ####
plot.CMA.error <- function(cma=NA, patients.to.plot=NULL,
                           export.formats=NULL, export.formats.fileprefix="AdhereR-plot", export.formats.directory=NA,
                           generate.R.plot=TRUE
)
{
  # What sorts of plots to generate (use short names for short if statements):
  .do.R <- generate.R.plot; .do.SVG <- (!is.null(export.formats) && any(c("svg", "html", "jpg", "png", "webp", "ps", "pdf") %in% export.formats));
  if( !.do.R && !.do.SVG )
  {
    # Nothing to plot!
    return (invisible(NULL));
  }

  if( .do.R )
  {
    #dev.new(); # clear any previous plots
    old.par <- par(no.readonly=TRUE); # save the origial par
    par(mar=c(0,0,0,0), bg="gray60");
    plot.new();
    segments(0, 0, 1, 1, col="gray40", lwd=10);
    segments(0, 1, 1, 0, col="gray40", lwd=10);
    par(old.par); # restore the original par at the end
  }

  exported.file.names <- NULL; # the list of exported files (if any)
  if( .do.SVG )
  {
    # Build the SVG plot:
    svg.header <- c('<?xml version="1.0" standalone="no"?>\n',
                    '<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">\n');
    svg.str <- c( '<svg ',
                  'viewBox="0 0 100 100" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">\n', # the plotting surface

                  # Comments, notes and clarifications:
                  .SVG.comment("This is the self-contained SVG error plot.", newpara=TRUE),
                  # Clear the area:
                  .SVG.rect(comment="Clear the whole plotting area",
                            class="plotting-area-background",
                            x=0, y=0, width=100, height=100,
                            fill="gray60", stroke="none"),
                  '\n', # one empty line
                  .SVG.lines(x=c(0,100, 0,100), y=c(0,100, 100,0), stroke="gray40", stroke_width=5),
                  '</svg>\n');

    # Export to various formats (if so requested):
    if( !is.null(export.formats) )
    {
      file.svg <- NULL;
      if( "svg" %in% export.formats )
      {
        ## Export as stand-alone SVG file ####
        file.svg <- ifelse( is.na(export.formats.directory),
                            tempfile(export.formats.fileprefix, fileext=".svg"),
                            file.path(export.formats.directory, paste0(export.formats.fileprefix,".svg")) );
        exported.file.names <- c(exported.file.names, file.svg);

        # Export SVG:
        writeLines(c(svg.header, svg.str), file.svg, sep="");
      }

      if( "html" %in% export.formats )
      {
        ## Export as self-contained HTML document ####
        file.html <- ifelse( is.na(export.formats.directory),
                             tempfile(export.formats.fileprefix, fileext=".html"),
                             file.path(export.formats.directory, paste0(export.formats.fileprefix,".html")) );
        exported.file.names <- c(exported.file.names, file.html);

        # Load the CSS and JavaScript templates:
        css.template.path <- system.file('html-templates/css-template.css', package='AdhereR');
        if( is.null(css.template.path) || css.template.path=="" )
        {
          if( !suppress.warnings ) .report.ewms("Cannot load the CSS template -- please reinstall the AdhereR package!\n", "error", ".plot.CMAs", "AdhereR");
          .last.cma.plot.info$SVG <- NULL;
          assign(".last.cma.plot.info", .last.cma.plot.info, envir=.adherer.env); # save the plot infor into the environment
          plot.CMA.error(export.formats=export.formats,
                         export.formats.fileprefix=export.formats.fileprefix,
                         export.formats.directory=export.formats.directory,
                         generate.R.plot=FALSE);
          return (invisible(NULL));
        }
        js.template.path <- system.file('html-templates/javascript-template.js', package='AdhereR');
        if( is.null(js.template.path) || js.template.path=="" )
        {
          if( !suppress.warnings ) .report.ewms("Cannot load the JavaScript template -- please reinstall the AdhereR package!\n", "error", ".plot.CMAs", "AdhereR");
          .last.cma.plot.info$SVG <- NULL;
          assign(".last.cma.plot.info", .last.cma.plot.info, envir=.adherer.env); # save the plot infor into the environment
          plot.CMA.error(export.formats=export.formats,
                         export.formats.fileprefix=export.formats.fileprefix,
                         export.formats.directory=export.formats.directory,
                         generate.R.plot=FALSE);
          return (invisible(NULL));
        }
        css.template <- readLines(css.template.path);
        js.template  <- readLines(js.template.path);

        # Load the HTML template and replace generics by their actual values before saving it in the desired location:
        html.template.path <- system.file('html-templates/html-template.html', package='AdhereR');
        if( is.null(html.template.path) || html.template.path=="" )
        {
          if( !suppress.warnings ) .report.ewms("Cannot load the HTML template -- please reinstall the AdhereR package!\n", "error", ".plot.CMAs", "AdhereR");
          .last.cma.plot.info$SVG <- NULL;
          assign(".last.cma.plot.info", .last.cma.plot.info, envir=.adherer.env); # save the plot infor into the environment
          plot.CMA.error(export.formats=export.formats,
                         export.formats.fileprefix=export.formats.fileprefix,
                         export.formats.directory=export.formats.directory,
                         generate.R.plot=FALSE);
          return (invisible(NULL));
        }
        html.template <- readLines(html.template.path);
        html.template <- sub('<script type="text/javascript" src="PATH-TO-JS"></script>',
                             paste0('<script type="text/javascript">\n', paste0(js.template, collapse="\n"), '\n</script>'),
                             html.template, fixed=TRUE); # JavaScript
        html.template <- sub('<link rel="stylesheet" href="PATH-TO-CSS">',
                             paste0('<style>\n', paste0(css.template, collapse="\n"), '\n</style>'),
                             html.template, fixed=TRUE); # CSS

        # SVG:
        svg.str.embedded <- c('<svg id="adherence_plot" ', # add id and (possibly) the dimensions to the <svg> tag
                              if( TRUE ) 'height="600" ', # height (if defined)
                              if( FALSE ) 'width="600" ', # width (if defined)
                              svg.str[-1]);
        html.template <- sub('<object id="adherence_plot" data="PATH-TO-IMAGE" type="image/svg+xml">Please use a modern browser!</object>',
                             paste0(paste0(svg.str.embedded, collapse=""), "\n"),
                             html.template, fixed=TRUE); # SVG

        # Explort the self-contained HTML document:
        writeLines(html.template, file.html, sep="\n");
      }

      if( any(c("jpg", "png", "ps", "pdf", "webp") %in% export.formats) )
      {
        ## Export to flat file formats (PNG, JPG, PS, PDF or WEBP) ####
        # Need to covert the SVG to one of these, so we need to export it (if not already exported):
        if( is.null(file.svg) )
        {
          file.svg <- tempfile(export.formats.fileprefix, fileext=".svg");
          writeLines(c(svg.header, svg.str), file.svg, sep="");
        }

        if( any(c("jpg", "png","webp") %in% export.formats) )
        {
          # For the bitmapped formats, render it once:
          bitmap <- rsvg::rsvg(file.svg); # , height = 1440

          if( "jpg" %in% export.formats )
          {
            # JPG file:
            file.jpg <- ifelse( is.na(export.formats.directory),
                                tempfile(export.formats.fileprefix, fileext=".jpg"),
                                file.path(export.formats.directory, paste0(export.formats.fileprefix,".jpg")) );
            exported.file.names <- c(exported.file.names, file.jpg);
            jpeg::writeJPEG(bitmap, file.jpg, quality=0.90);
          }

          if( "png" %in% export.formats )
          {
            # PNG file:
            file.png <- ifelse( is.na(export.formats.directory),
                                tempfile(export.formats.fileprefix, fileext=".png"),
                                file.path(export.formats.directory, paste0(export.formats.fileprefix,".png")) );
            exported.file.names <- c(exported.file.names, file.png);
            #rsvg::rsvg_png(file.svg, file=file.png);
            png::writePNG(bitmap, file.png, dpi=150);
          }

          if( "webp" %in% export.formats )
          {
            # WEBP file:
            file.webp <- ifelse( is.na(export.formats.directory),
                                 tempfile(export.formats.fileprefix, fileext=".webp"),
                                 file.path(export.formats.directory, paste0(export.formats.fileprefix,".webp")) );
            exported.file.names <- c(exported.file.names, file.webp);
            #rsvg::rsvg_webp(file.svg, file=file.webp);
            webp::write_webp(bitmap, file.webp, quality=90);
          }
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

  # No last plot (really)...
  assign(".last.cma.plot.info", NULL, envir=.adherer.env);

  # Return value:
  return (invisible(exported.file.names));
}


