# `AdhereR`: an `R` package for computing adherence to medications and for visualising medication histories

This is a package for the [`R`](https://www.r-project.org) open-source statistical environment implementing various ways of computing adherence to medications and visualisations high quality (publication-ready) of medication histories.

There is an accompanying package **`AdhereRViz`** that specifically implements an interactive graphical user interface (a **GUI**) that allows the user to explore various datasets, computations and visualisations. The present package (**`AdhereR`**) can be installed without `AdhereRViz` if there is no need for an interactive GUI and/or if the setup does not allow all of `AdhereRViz`'s dependencies to be installed or run (e.g., in a server/batch environment or on some old Operating Systems).

More information is given in the package's various vignettes and in the accompanying paper:

> [Dima AL, Dediu D (2017). Computation of adherence to medication and visualization of medication histories in R with AdhereR: Towards transparent and reproducible use of electronic healthcare data. *PLoS ONE*, **12**(4): e0174426. doi:10.1371/journal.pone.0174426](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0174426)

While the package can be created from the source or installed using the source package (`.tar.gz`) provided in this repository, we recommend that users install it directly from [The Comprehensive R Archive Network (CRAN)](https://cran.r-project.org/) using the normal procedure<sup>1</sup> (i.e., `install.packages("AdhereR", dep=TRUE)`).

The package is implemented in pure `R`, but optimised to work on mid-range hardware even for large databases (stored, for example, in an `SQL` `RDBMS`), single-threaded or parallelized in large heterogeneous clusters (using, for example, `Apache Hadoop`), and runs on any platform that supports `R`. 
A standardized interface allows `AdhereR` to be used transparently from other programming languages or platforms, and a fully functional bridge to `Psython 3` is provided as an example.
The code is released under [GPL v3](https://www.gnu.org/licenses/gpl-3.0-standalone.html).

Feedback is welcome, preferably by using GitHub's *Issues* system for bug reporting and suggestions.

For what is new in each release, please see the `NEWS.md` file.

Copyright (by period, in alphabetical order by family name; please see individual files for details) (C):

  - 2015-2018: Alexandra Dima, Dan Dediu
  - 2018-2019: Samuel Allemann, Alexandra Dima, Dan Dediu

-----

<sup>1</sup> There might be a delay between the release of new versions here and their availability on CRAN; in this case, we would recommend, if possible, to wait for the version to appear on CRAN.
