% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adherer.R
\name{plot.CMA1}
\alias{plot.CMA1}
\alias{plot.CMA2}
\alias{plot.CMA3}
\alias{plot.CMA4}
\alias{plot.CMA5}
\alias{plot.CMA6}
\alias{plot.CMA7}
\alias{plot.CMA8}
\alias{plot.CMA9}
\title{Plot CMA0-derived objects.}
\usage{
\method{plot}{CMA1}(
  x,
  ...,
  patients.to.plot = NULL,
  duration = NA,
  align.all.patients = FALSE,
  align.first.event.at.zero = FALSE,
  show.period = c("dates", "days")[2],
  period.in.days = 90,
  show.legend = TRUE,
  legend.x = "right",
  legend.y = "bottom",
  legend.bkg.opacity = 0.5,
  legend.cex = 0.75,
  legend.cex.title = 1,
  cex = 1,
  cex.axis = 0.75,
  cex.lab = 1,
  show.cma = TRUE,
  col.cats = rainbow,
  unspecified.category.label = "drug",
  medication.groups.to.plot = NULL,
  medication.groups.separator.show = TRUE,
  medication.groups.separator.lty = "solid",
  medication.groups.separator.lwd = 2,
  medication.groups.separator.color = "blue",
  medication.groups.allother.label = "*",
  lty.event = "solid",
  lwd.event = 2,
  pch.start.event = 15,
  pch.end.event = 16,
  show.event.intervals = TRUE,
  col.na = "lightgray",
  print.CMA = TRUE,
  CMA.cex = 0.5,
  plot.CMA = TRUE,
  CMA.plot.ratio = 0.1,
  CMA.plot.col = "lightgreen",
  CMA.plot.border = "darkgreen",
  CMA.plot.bkg = "aquamarine",
  CMA.plot.text = CMA.plot.border,
  highlight.followup.window = TRUE,
  followup.window.col = "green",
  highlight.observation.window = TRUE,
  observation.window.col = "yellow",
  observation.window.density = 35,
  observation.window.angle = -30,
  observation.window.opacity = 0.3,
  show.real.obs.window.start = TRUE,
  real.obs.window.density = 35,
  real.obs.window.angle = 30,
  print.dose = FALSE,
  cex.dose = 0.75,
  print.dose.outline.col = "white",
  print.dose.centered = FALSE,
  plot.dose = FALSE,
  lwd.event.max.dose = 8,
  plot.dose.lwd.across.medication.classes = FALSE,
  alternating.bands.cols = c("white", "gray95"),
  bw.plot = FALSE,
  rotate.text = -60,
  force.draw.text = FALSE,
  min.plot.size.in.characters.horiz = 0,
  min.plot.size.in.characters.vert = 0,
  max.patients.to.plot = 100,
  export.formats = NULL,
  export.formats.fileprefix = "AdhereR-plot",
  export.formats.height = NA,
  export.formats.width = NA,
  export.formats.save.svg.placeholder = TRUE,
  export.formats.svg.placeholder.type = c("jpg", "png", "webp")[2],
  export.formats.svg.placeholder.embed = FALSE,
  export.formats.directory = NA,
  export.formats.html.template = NULL,
  export.formats.html.javascript = NULL,
  export.formats.html.css = NULL,
  generate.R.plot = TRUE,
  do.not.draw.plot = FALSE
)

\method{plot}{CMA2}(...)

\method{plot}{CMA3}(...)

\method{plot}{CMA4}(...)

\method{plot}{CMA5}(...)

\method{plot}{CMA6}(...)

\method{plot}{CMA7}(...)

\method{plot}{CMA8}(...)

\method{plot}{CMA9}(...)
}
\arguments{
\item{x}{A \emph{\code{CMA0}} or derived object, representing the CMA to
plot}

\item{...}{other possible parameters}

\item{patients.to.plot}{A vector of \emph{strings} containing the list of
patient IDs to plot (a subset of those in the \code{cma} object), or
\code{NULL} for all}

\item{duration}{A \emph{number}, the total duration (in days) of the whole
period to plot; in \code{NA} it is automatically determined from the event
data such that the whole dataset fits.}

\item{align.all.patients}{\emph{Logical}, should all patients be aligned
(i.e., the actual dates are discarded and all plots are relative to the
earliest date)?}

\item{align.first.event.at.zero}{\emph{Logical}, should the first event be
placed at the origin of the time axis (at 0)?}

\item{show.period}{A \emph{string}, if "dates" show the actual dates at the
regular grid intervals, while for "days" (the default) shows the days since
the beginning; if \code{align.all.patients == TRUE}, \code{show.period} is
taken as "days".}

\item{period.in.days}{The \emph{number} of days at which the regular grid is
drawn (or 0 for no grid).}

\item{show.legend}{\emph{Logical}, should the legend be drawn?}

\item{legend.x}{The position of the legend on the x axis; can be "left",
"right" (default), or a \emph{numeric} value.}

\item{legend.y}{The position of the legend on the y axis; can be "bottom"
(default), "top", or a \emph{numeric} value.}

\item{legend.bkg.opacity}{A \emph{number} between 0.0 and 1.0 specifying the
opacity of the legend background.}

\item{cex, cex.axis, cex.lab, legend.cex, legend.cex.title, CMA.cex}{\emph{numeric}
values specifying the \code{cex} of the various types of text.}

\item{show.cma}{\emph{Logical}, should the CMA type be shown in the title?}

\item{col.cats}{A \emph{color} or a \emph{function} that specifies the single
colour or the colour palette used to plot the different medication; by
default \code{rainbow}, but we recommend, whenever possible, a
colorblind-friendly palette such as \code{viridis} or \code{colorblind_pal}.}

\item{unspecified.category.label}{A \emph{string} giving the name of the
unspecified (generic) medication category.}

\item{medication.groups.to.plot}{the names of the medication groups to plot or
\code{NULL} (the default) for all.}

\item{medication.groups.separator.show}{a \emph{boolean}, if \code{TRUE} (the
default) visually mark the medication groups the belong to the same patient,
using horizontal lines and alternating vertical lines.}

\item{medication.groups.separator.lty, medication.groups.separator.lwd, medication.groups.separator.color}{graphical parameters (line type, line width and colour describing the visual
marking og medication groups as beloning to the same patient.}

\item{medication.groups.allother.label}{a \emph{string} giving the label to
use for the implicit \code{__ALL_OTHERS__} medication group (defaults to "*").}

\item{lty.event, lwd.event, pch.start.event, pch.end.event}{The style of the
event (line style, width, and start and end symbols).}

\item{show.event.intervals}{\emph{Logical}, should the actual event intervals
be shown?}

\item{col.na}{The colour used for missing event data.}

\item{print.CMA}{\emph{Logical}, should the CMA values be printed?}

\item{plot.CMA}{\emph{Logical}, should the CMA values be represented
graphically?}

\item{CMA.plot.ratio}{A \emph{number}, the proportion of the total horizontal
plot space to be allocated to the CMA plot.}

\item{CMA.plot.col, CMA.plot.border, CMA.plot.bkg, CMA.plot.text}{\emph{Strings}
giving the colours of the various components of the CMA plot.}

\item{highlight.followup.window}{\emph{Logical}, should the follow-up window
be plotted?}

\item{followup.window.col}{The follow-up window's colour.}

\item{highlight.observation.window}{\emph{Logical}, should the observation
window be plotted?}

\item{observation.window.col, observation.window.density, observation.window.angle, observation.window.opacity}{Attributes of the observation window (colour, shading density, angle and
opacity).}

\item{show.real.obs.window.start, real.obs.window.density, real.obs.window.angle}{For some CMAs, the observation window might
be adjusted, in which case should it be plotted and with that attributes?}

\item{print.dose}{\emph{Logical}, should the daily dose be printed as text?}

\item{cex.dose}{\emph{Numeric}, if daily dose is printed, what text size
to use?}

\item{print.dose.outline.col}{If \emph{\code{NA}}, don't print dose text with
outline, otherwise a color name/code for the outline.}

\item{print.dose.centered}{\emph{Logical}, print the daily dose centered on
the segment or slightly below it?}

\item{plot.dose}{\emph{Logical}, should the daily dose be indicated through
segment width?}

\item{lwd.event.max.dose}{\emph{Numeric}, the segment width corresponding to
the maximum daily dose (must be >= lwd.event but not too big either).}

\item{plot.dose.lwd.across.medication.classes}{\emph{Logical}, if \code{TRUE},
the line width of the even is scaled relative to all medication classes (i.e.,
relative to the global minimum and maximum doses), otherwise it is scale
relative only to its medication class.}

\item{alternating.bands.cols}{The colors of the alternating vertical bands
distinguishing the patients; can be \code{NULL} = don't draw the bandes;
or a vector of colors.}

\item{bw.plot}{\emph{Logical}, should the plot use grayscale only (i.e., the
\code{\link[grDevices]{gray.colors}} function)?}

\item{rotate.text}{\emph{Numeric}, the angle by which certain text elements
(e.g., axis labels) should be rotated.}

\item{force.draw.text}{\emph{Logical}, if \code{TRUE}, always draw text even
if too big or too small}

\item{min.plot.size.in.characters.horiz, min.plot.size.in.characters.vert}{\emph{Numeric}, the minimum size of the plotting surface in characters;
horizontally (min.plot.size.in.characters.horiz) refers to the the whole
duration of the events to plot; vertically (min.plot.size.in.characters.vert)
refers to a single event. If the plotting is too small, possible solutions
might be: if within \code{RStudio}, try to enlarge the "Plots" panel, or
(also valid outside \code{RStudio} but not if using \code{RStudio server}
start a new plotting device (e.g., using \code{X11()}, \code{quartz()}
or \code{windows()}, depending on OS) or (works always) save to an image
(e.g., \code{jpeg(...); ...; dev.off()}) and display it in a viewer.}

\item{max.patients.to.plot}{\emph{Numeric}, the maximum patients to attempt
to plot.}

\item{export.formats}{a \emph{string} giving the formats to export the figure
to (by default \code{NULL}, meaning no exporting); can be any combination of
"svg" (just an \code{SVG} file), "html" (\code{SVG} + \code{HTML} + \code{CSS}
+ \code{JavaScript}, all embedded within one \code{HTML} document), "jpg",
"png", "webp", "ps" or "pdf".}

\item{export.formats.fileprefix}{a \emph{string} giving the file name prefix
for the exported formats (defaults to "AdhereR-plot").}

\item{export.formats.height, export.formats.width}{\emph{numbers} giving the
desired dimensions (in pixels) for the exported figure (defaults to sane
values if \code{NA}).}

\item{export.formats.save.svg.placeholder}{a \emph{logical}, if TRUE, save an
image placeholder of type given by \code{export.formats.svg.placeholder.type}
for the \code{SVG} image.}

\item{export.formats.svg.placeholder.type}{a \emph{string}, giving the type of
placeholder for the \code{SVG} image to save; can be "jpg",
"png" (the default) or "webp".}

\item{export.formats.svg.placeholder.embed}{a \emph{logical}, if \code{TRUE},
embed the placeholder image in the HTML document (if any) using \code{base64}
encoding, otherwise (the default) leave it as an external image file (works
only when an \code{HTML} document is exported and only for \code{JPEG} or
\code{PNG} images.}

\item{export.formats.directory}{a \emph{string}; if exporting, which directory
to export to; if \code{NA} (the default), creates the files in a temporary
directory.}

\item{export.formats.html.template, export.formats.html.javascript, export.formats.html.css}{\emph{character strings} or \code{NULL} (the default) giving the path to the
\code{HTML}, \code{JavaScript} and \code{CSS} templates, respectively, to be
used when generating the HTML+CSS semi-interactive plots; when \code{NULL},
the default ones included with the package will be used. If you decide to define
new templates please use the default ones for inspiration and note that future
version are not guaranteed to be backwards compatible!}

\item{generate.R.plot}{a \emph{logical}, if \code{TRUE} (the default),
generate the standard (base \code{R}) plot for plotting within \code{R}.}

\item{do.not.draw.plot}{a \emph{logical}, if \code{TRUE} (\emph{not} the default),
does not draw the plot itself, but only the legend (if \code{show.legend} is
\code{TRUE}) at coordinates (0,0) irrespective of the given legend coordinates.
This is intended to allow (together with the \code{get.legend.plotting.area()}
function) the separate plotting of the legend.}
}
\description{
Plots the event data and estimated CMA encapsulated in objects derived from
\code{CMA0}.
}
\details{
Please note that this function plots objects inheriting from \code{CMA0} but
not objects of type \code{CMA0} itself (these are plotted by
\code{\link{plot.CMA0}}).

The x-axis represents time (either in days since the earliest date or as
actual dates), with consecutive events represented as ascending on the y-axis.

Each event is represented as a segment with style \code{lty.event} and line
width \code{lwd.event} starting with a \code{pch.start.event} and ending with
a \code{pch.end.event} character, coloured with a unique color as given by
\code{col.cats}, extending from its start date until its end date.
Superimposed on these are shown the event intervals and gap days as estimated
by the particular CMA method, more precisely plotting the start and end of
the available events as solid filled-in rectangles, and the event gaps as
shaded rectangles.

The follow-up and the observation windows are plotted as an empty rectangle
and as shaded rectangle, respectively (for some CMAs the observation window
might be adjusted in which case the adjustment may also be plotted using a
 different shading).

The CMA estimates can be visually represented as well in the left side of the
figure using bars (sometimes the estimates can go above 100\%, in which case
the maximum possible bar filling is adjusted to reflect this).

When several patients are displayed on the same plot, they are organized
vertically, and alternating bands (white and gray) help distinguish
consecutive patients.
Implicitely, all patients contained in the \code{cma} object will be plotted,
but the \code{patients.to.plot} parameter allows the selection of a subset of
patients.

Finally, the y-axis shows the patient ID and possibly the CMA estimate as
well.
}
\examples{
cma1 <- CMA1(data=med.events,
             ID.colname="PATIENT_ID",
             event.date.colname="DATE",
             event.duration.colname="DURATION",
             followup.window.start=30,
             observation.window.start=30,
             observation.window.duration=365,
             date.format="\%m/\%d/\%Y"
            );
plot(cma1, patients.to.plot=c("1","2"));
}
