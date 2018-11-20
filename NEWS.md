# AdhereR 0.2.2

## Small features and bug fixes

  - removed margins when plotting CMA0 (they could become too large when plotting many patients);
  
  - now allow custom placement of legend when plotting CMA0; 
  
## Allow `AdhereR` to use databases

`AdhereR` can access data (read and write) stored in various types of databases, ranging from "classic" relational databases (such as `MySQL` and `SQLite`) to new approaches (such as Apache's `Hadoop`).
This allows the seamless processing of very large datasets across many types of architectures, ranging from a consumer-grade laptop to large heterogeneous computer clusers, without loading the dataset in memory.
Even interactive plotting is now capable of accessing data stored using various engines in real-time by allowing the user to define accessor functions that implement the details of this access (e.g., use `SQL` queries for accessing data stored in a classic relational database).

A new vignette (*Using AdhereR with various database technologies for processing very large datasets*) gives all the needed details, including actual code, for running `AdhereR` on *relational databases* (using either explicit `SQL` or implicitely through `dbplyr`) and on *Apache `Hadoop`* (using `RHadoop` for access to HDFS and MapReduce).
(Please note that this vignette is pre-compiled due to its requirements in terms of thrid-party software such as `MySQL` and Apache `Hadoop`.)

## New function to compute event durations from prescription, dispensing, and hospitalization data

Computation of CMAs requires a supply duration for medications dispensed to patients. If medications are not supplied for fixed durations but as a quantity that may last for various durations based on the prescribed dose, the supply duration has to be calculated based on dispensed and prescribed doses. Treatments may be interrupted and resumed at later times, for which existing supplies may or may not be taken into account. Patients may be hospitalized or incarcerated, and may not use their own supplies during these periods. The new function `compute_event_durations` allows to calculate supply durations, taking into account the aforementioned situations and offering parameters for flexible adjustments.

## New function to compute time to initiation

The period between the first prescription event and the first dose administration may impact health outcomes differently than omitting doses once on treatment or interrupting medication for longer periods of time. Primary non-adherence (not acquiring the first prescription) or delayed initiation may have a negative impact on health outcomes. The new function `time_to_initiation` allows to calculate the time between the first prescription and the first dispensing event, taking into account multiple variables to differentiate between treatments.

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
