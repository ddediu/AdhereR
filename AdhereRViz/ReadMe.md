# `AdhereRViz`: an `R` package for the interactive computation and visualisation of medication histories

This is a package for the [`R`](https://www.r-project.org) open-source statistical environment implementing implements an interactive graphical user interface (a **GUI**) that allows the user to explore various datasets, computations and visualisations of medication histories and of adherence to medication.

This package is based on the accompanying package **`AdhereR`** (considered as one of its dependencies) that specifically implements various ways of computing adherence to medications and multiple high quality (publication-ready) visualisations of medication histories. The present package (**`AdhereRViz`**) does not work without `AdhereR` being installed and working on the system, and represents an extension of `AdhereR`'s functionalities to a user-driven interactive scenario.

More information is given in the package's various vignettes (see also the `AdhereR`).

While the package can be created from the source or installed using the source package (`.tar.gz`) provided in this repository, we recommend that users install it directly from [The Comprehensive R Archive Network (CRAN)](https://cran.r-project.org/) using the normal procedure<sup>1</sup> (i.e., `install.packages("AdhereRViz", dep=TRUE)`).

The package is implemented in pure `R` using [`Shiny`](https://shiny.rstudio.com/) but with `JavaScript`/`HTML`/`CSS` components, as needed.
The code is released under [GPL v3](https://www.gnu.org/licenses/gpl-3.0-standalone.html).

Feedback is welcome, preferably by using GitHub's *Issues* system for bug reporting and suggestions.

For what is new in each release, please see the `NEWS.md` file.

Copyright (by period, in alphabetical order by family name; please see individual files for details) (C):

  - 2015-2018: Alexandra Dima, Dan Dediu
  - 2018-2019: Samuel Allemann, Alexandra Dima, Dan Dediu

-----
