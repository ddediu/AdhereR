# AdhereR 0.7.1
  
## New features

  - now `plot()` respects the order of patients given through the `patients.to.plot` argument
  
  - plotting sliding windows and per episodes can now show the events and gaps (as for simple plots, but things are a bit more complicated as different windows/events might result in different estimates)

## Bug fixes

### `CMA_polypharmacy`

  - remove option to supply medication class as list after `.check.medication.class` was suppressed from AdhereR.
  
  - specify columns to merge by.

  
# AdhereR 0.7
  
## New features

  - this introduces the concept of "medication groups" which represent subsets of observations for which CMAs can be computed and which are grouped together in the plots; this prompted massive changes in the output of the various `CMA` functions and in the plotting when medication groups are defined, but is full backward-compatible when no medication groups are defined (the default).
  
  - `compute.treatment.episodes()` and, in consequence, `CMA_per_episode()`, have a new argument, `maximum.permissible.gap.append.to.episode`, which specifies if the maximum permissible gap is to be appended to the episodes (FALSE by default).
  
  - the Shiny (>= 0.2) interactive interface is aware of medication groups and `maximum.permissible.gap.append.to.episode`.
  
  - better and faster `SVG` plotting, better semi-interactive `HTML`+`SVG`+`JavaScript`+`CSS` output, which can be used for display in any standard modern web browser without online R processing (i.e., overnight batch-mode generation of the plots and CMA estimates for later display and further processing).
  
  - various enhancements and bugfixes.


# AdhereR 0.6
  
## New features

  - this release splits the old `AdhereR` package into two packages: `AdhereR`, which continues to implement all the computations and plotting, and `AdhereRViz`, which implements all the interactive plotting and GUI (using either RStudio's manipulate and Shiny); while `AdhereRViz` requires `AdhereR`, `AdhereR` only suggests `AdhereRViz`.
  
  - the plotting can be done either using R plotting and/or by generating SVG images (which can be further exported to different formats, such as JPG and PNG and can be embedded in HTML/CSS/JavaScript pages with limited interactivity).
  
  - new subtype of plot where the events are shown on the same row (i.e., without vertical separation): useful for cases where not many events overlap in time and vertical space is at a premium.
  
  - other enhancements and bugfixes.


# AdhereR 0.5

## New features

  - major improvements to `compute_event_durations`, with a complete makeover of the handling for special periods like hospitalization and restructuring of the output.

  - New function `prune_event_durations` to remove leftover supply from previous dispensing events in case of new dispensing events within a user-specified time after dosage changes, special periods, or treatment interruptions.

  - New function `cover_special_periods` to identify special periods that are in proximity to already covered durations and add an additional event for these durations.

  - New option `treat.epi` in function `CMA_per_episode` to specify precomputed treatment episodes for the computation of CMA per episode.
  
  - New vignette [*Data preparation for computing adherence to medication in AdhereR*](https://ddediu.github.io/AdhereR/compute_event_durations/compute_event_durations.html) added to explain the functions `compute_event_durations`, `prune_event_durations`, `cover_special_periods`, and `time_to_initiation`.

## Bug fixes

### `compute_event_durations`

  - fixed a few instances where colnames were hardcoded to colnames in example dataset
  
  - throw errors when disp.date, presc.date, total.dose, or presc.daily.dose contain NAs
  
  - fix error when visit.colname is not in presc.data
  
  - fix progress bar when ID's are not sequentially starting from 1

  
# AdhereR 0.4.1

## Bug fixes

  - Shiny App: don't show print and plot dose UI for CMA1-CMA4 (these don't care about dose)
  
  - Shiny App: refactor it so it can be started independently from outside the package (allowing it, for example, to be hosted on https://www.shinyapps.io/)
  
  - plotting CMA1+ and complex: fix crash when no treatment column was defined but there was a dose column

# AdhereR 0.4
  
## New features

  - major improvements to the interactive Shiny plotting, now a fully self-contained point-and-click user interface/App for plotting and estimating CMA from a variety of data sources
  
  - new vignette *AdhereR: Interactive plotting (and more) with Shiny* added for explaining the new Shiny App

  - now can plot dose as text and/or line width
  
  - show a warning if the user calls CMA functions with arguments that are ignored by that particular CMA (useful to make the user aware that, for example, carry-over will not be considered by `CMA1` and then wonder why asking for it didn't change anything)
  
  - various plotting improvements (axis lables, vertical lines, legend customisation, font sizes, customisable OW transparency, better computation of minimum size requirements)

## Bug fixes

  - `plot.CMA0` ignored grascale plotting for OW and FUW
  
  - legend now is resized depending on the various legend font sizes and not on the global font sized
  
  - better handling of font sizes of 0 (now forced transparently to 0.01)
  
  - better handling of 0% for the CMA estimate plot for CMA1+, per episode and sliding windows
  
  - small bugs fixed in `compute_event_durations()`
  
  - better handling of messages, warnings and error during plotting
  
  - uniformisation of plotting between all types of CMA

# AdhereR 0.3.1

## Bug fixes

  - error when passing a `data.table` to interactive plotting;
  
  - in `plot.CMA0`, `align.all.patients=TRUE` didn't work anymore (regression);
  
  - `time_to_initiation()` was not exported;
  
  - the examples for `time_to_initiation()` and `compute_event_durations()` were wrongly formatted.

# AdhereR 0.3

## Small features and bug fixes

  - removed margins when plotting CMA0 (they could become too large when plotting many patients);
  
  - now allows the custom placement of legend when plotting CMA0; 
  
  - the plots for CMA0-9 can now also show the daily dose as text and/or line thickness.
  
## Allow `AdhereR` to use databases

`AdhereR` can access data (read and write) stored in various types of databases, ranging from "classic" relational databases (such as `MySQL` and `SQLite`) to new approaches (such as Apache's `Hadoop`).
This allows the seamless processing of very large datasets across many types of architectures, ranging from a consumer-grade laptop to large heterogeneous computer clusers, without loading the dataset in memory.
Even interactive plotting is now capable of accessing data stored using various engines in real-time by allowing the user to define accessor functions that implement the details of this access (e.g., use `SQL` queries for accessing data stored in a classic relational database).

A new vignette (*Using AdhereR with various database technologies for processing very large datasets*) gives all the needed details, including actual code, for running `AdhereR` on *relational databases* (using either explicit `SQL` or implicitely through `dbplyr`) and on *Apache `Hadoop`* (using `RHadoop` for access to HDFS and MapReduce).
(Please note that this vignette is pre-compiled due to its requirements in terms of thrid-party software such as `MySQL` and Apache `Hadoop`.)

## New function to compute event durations from prescription, dispensing, and hospitalization data

Computation of CMAs requires a supply duration for medications dispensed to patients. 
If medications are not supplied for fixed durations but as a quantity that may last for various durations based on the prescribed dose, the supply duration has to be calculated based on dispensed and prescribed doses.
Treatments may be interrupted and resumed at later times, for which existing supplies may or may not be taken into account. 
Patients may be hospitalized or incarcerated, and may not use their own supplies during these periods. 
The new function `compute_event_durations` calculates the supply durations, taking into account the aforementioned situations and offering parameters for flexible adjustments.

## New function to compute time to initiation

The period between the first prescription event and the first dose administration may impact health outcomes differently than omitting doses once on treatment or interrupting medication for longer periods of time. 
Primary non-adherence (not acquiring the first prescription) or delayed initiation may have a negative impact on health outcomes. 
The new function `time_to_initiation` calculates the time between the first prescription and the first dispensing event, taking into account multiple variables to differentiate between treatments.


# AdhereR 0.2.1

## Optimisations, small features, and bug fixes

  - overall optimisation of the workhorse function`compute.event.int.gaps()`, resulting in sizeable speed-ups in some scenarios;
  
  - replacing the direct use of `snow` by `parallel`, dropping thus this explicit dependency;
  
  - initial introduction of unit testing (`testthat`; currently not on CRAN but only during development);
  
  - distributing computations across multiple computers over a network;
  
  - small bug fixes in plotting and cma computation.
  
## Shiny interactive plotting

While the original interactive plotting using `RStudio`'s `manipulate()` function was useful, it had several limitations (dependence on `RStudio`, interface design, etc.), prompting a massive rewritting of the interactive plotting system using `Shiny`.
The old `manipulate()` method is still available, but the new one based on `Shiny` offers multiple advantages, such as running in any modern web browser and even on a remote machine.

## Using `AdhereR` from outside `R` (e.g., from `Python 3`)

`AdhereR` is now tansparently callable from other programs/platforms than `R` using a very simple, transparent and portable mechanism.
To illustrate the full process, we implemented a fully functional `reference implementation` allowing `AdhereR` to be transparently used from `Python 3`, implementation consisting of a `Python` module `adherer` included with the `AhdereR` package (thus it does not need separate installation through `pip` or similar mechanisms) that automatically detects where `R` is installed and exposes a class hierarchy that can be used in `Python` scripts or programs.
Full details are available in the new `calling-AdhereR-from-python3` vignette.


# AdhereR 0.1.0

This is the initial release. Please see ReadMe.md and the accompanying paper for details:

> [Dima AL, Dediu D (2017). Computation of adherence to medication and visualization of medication histories in R with AdhereR: Towards transparent and reproducible use of electronic healthcare data. *PLoS ONE*, **12**(4): e0174426. doi:10.1371/journal.pone.0174426](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0174426)
