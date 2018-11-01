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
