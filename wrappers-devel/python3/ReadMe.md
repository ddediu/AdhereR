# Calling `AdhereR` from `Python`

## Introduction

This is a reference implementation of a very general mechanism that allows `AdhereR` to be called transparently from other programming languages and platforms than `R`.
This reference implementation is for `Python 3`[^1] and does not aim to be efficient or very elegant, but to illustrate how `AdhereR` can be used in a transparent manner and to provide a basis for further implementations in other programming languages and for other statistical platforms (and even for more efficient approaches).

## General description of the mechanism

The mechanism is very general and is based on a *wrapper* available on the *client platform* (here, `Python 3`) that performs the following general tasks (the details for this reference implementation follow):

  1. exposes to the users on the client platform a set of functions/methods/objects (or other mechanisms specific to that platform) that encapsulate in a platform-specific way the main functionalities in `AdhereR` of interest to the platform's users;
  2. when the user calls such an exposed function with a given set of argument values, the wrapper transparently translates these argument values in a format understandable by `AdhereR`; in particular, it saves any datasets to be processed as TAB-separated files and writes the argument values to another file with a standardised format;
  3. the wrapper uses a `shell` mechanism to create an instance of `R` into which it loads and executes a dedicated `R script`;
  4. the `R script` parses and loads the data and arguments saved to file, performs basic consistency checks, loads the `AdhereR` package, and calls the appropriate `AdhereR` method with the appropriate arguments;
  5. after the execution of the `AdhereR` method, the `R script` checks the results and, as appropriate, writes back to a predefined file any error messages, warnings or any other messages generated, and, if the case, saves the results to file as TAB-separated files or plots;
  6. the wrapper is notified when the `R script` has finished executing, loads the file containing the errors, warnings and messages and possibly the results, packs them into objects appropriate to the platform, and returns them to the user.

The currently used protocol can be found in [**Appendix I**](#appendix-1) below. 

## Calling `AdhereR` using the shell

The fundamental idea is that the `Python 3` `wrapper` creates the input files (essentially, `parameters.log` and `dataset.csv`) in a directory (let us denote it here as `PATH_TO_DATA_AND_RESULTS`, by default the current working directory).
We will use here a `macOS` setup for illustration purposes. 
Let's assume that `PATH_TO_DATA_AND_RESULTS ` is set to `~/Temp` on a Linux or macOS machine (on a Windows machine it could be `C:\\Temp`); then, before calling `AdhereR` this folder should contain the files:

    ~
     |-parameters.log
     \-dataset.csv

Please note that `R` must be properly installed on the system such that `Rscript` (or `Rscript.exe` on Windows) does exist; the `wrapper` tries to locate `Rscript` but if it fails or if the user wants to use a non-standard `R` installation, the `wrapper` provides an argument `path_to_rscript` that can be used to these ends.
Let's say for now that `Rscript` is located in `/usr/local/bin/Rscript` and is successfully automatically located; this results in the variable `PATH_TO_RSCRIPT` being set to this location.

The next bit that is needed is the location of the `R` script that encapsulates the communication between the `wrapper` and `AhdereR`. 
This script is named `callAdhereR.R` and is included in the `AdhereR` package, which means that is should be placed in the same location as the package itself; the `wrapper` will try to automatically locate the script, but, as above, the user can override this using the `path_to_adherer` argument.
Let's say for now that `callAdhereR.R` is located in the standard packages location `/Library/Frameworks/R.framework/Versions/Current/Resources/library/AdhereR/` and the automatic process is successful, resulting in the variable `PATH_TO_ADHERER` being set to this location.

With these path variables automatically or manually set, the `Python 3` `wrapper` calls `AdhereR`:

``` python
import subprocess # allow shell calls

[...]

rscript_cmd = PATH_TO_RSCRIPT + ' --vanilla ' + PATH_TO_ADHERER + \
			  '/foreign/callAdhereR.R' + ' ' + PATH_TO_DATA_AND_RESULTS
			  # build the full command line string
return_code = subprocess.call(rscript_cmd, shell=True) 
			  # use the shell call mechanism
			  # and wait for the process to return
			  # return_code should be 0 for success
```

When the `Rscript` process returns, `return_code` should be `0` for success (in the sense of calling `AdhereR` not in the sense that `AdhereR` also succeeded in the task it was assigned to do) or something else for errors.

If `return_code != 0`, the process stops with an exception.
Otherwise, an attempt is made to read the messages produced by `AdhereR`, normally saved in the `Adherer-results.txt` file in the `PATH_TO_DATA_AND_RESULTS` directory and checking if the last line begins with `OK:`.
If it does not, an exception occurs and the messages are displayed to the user.

If it does, the appropriate output files are read, parsed and loaded (depending on the invoked function, these files might differ).
For example, after successfully invoking `CMA1`, the `PATH_TO_DATA_AND_RESULTS` might look like:

    ~
     |-parameters.log
     |-dataset.csv
     |-Adherer-results.txt
     \-CMA.csv

In this example, the `wrapper` would parse and load `CMA.csv` as a `pandas` table:

``` python
import pandas # load pandas

[...]

cma = pandas.read_csv(PATH_TO_DATA_AND_RESULTS + '/CMA.csv',
					   sep='\t', header=0)
```

If plotting was requested, the resulting plot is also loaded using the `PIL` library:

``` python
from PIL import Image # load images

[...]

plot = Image.open(PATH_TO_PLOTS + '/adherer-plot' + '.' + PLOT_TYPE)
```

where `PATH_TO_PLOTS` and `PLOT_TYPE` are arguments to the plotting function.


## Python 3-specific stuff

This reference `Python 3` implementation is structured as a single Python file (`module`) named `adherer.py`.
It currently requires the modules `warnings`, `subprocess`, `os`, `numbers`, `datetime`, `pandas` and `PIL`.

It implements its own exception `CallAdhereRError` and is structured as a hierarchy of classes ultimately derived from `object`:

    object
     \-CMA0
        |-CMA1
        |  |-CMA2
        |  |-CMA3
        |  \-CMA4
        |-CMA5
        |  |-CMA6
        |  |-CMA7
        |  |-CMA8
        |  \-CMA9
        |-CMAPerEpisode
        \-CMASlidingWindow

`CMA0` is the foundation of the hierarchy and implements the most basic type of CMA that encapsulates all the possible arguments that a CMA can have, implements the basic accessors to the results, its plotting, interactive plotting and the two advanced methods for computing the treatment episodes and the event intervals and gaps.
It also houses the static method `_call_adherer` that encapsulates all the `wrapper` logic discussed above, and which is internally used by all the other methods and classes.

All the other classes derived from `CMA0` implement a particular type of CMA (see the `AdhereR` documentation and vignette, as well as [Dima & Dediu, 2017](#dimadediu2017)) and encapsulate the appropriate results.
For example, plotting one of these classes produces an image that can be directly shown or processed in `Python 3`.


## Parallel processing (locally and remotely)






## Appendix I: the communication protocol <a id="appendix-1"></a>

### Context

All arguments are written to the text file `parameters.log`; the input data are in the TAB-separated no quotes file `dataset.csv`.
The call returns any errors, warning and messages in the text file `Adherer-results.txt` file, and the actual results as TAB-separated no quotes files (not all necessarily produced, depending on the specific methods called) `CMA.csv`, `EVENTINFO.csv` and `TREATMENTEPISODES.csv`, and various image file(s).
All these files are exchanged in a user-defined directory `PATH_TO_WHERE_DATA_AND_RESULTS_ARE` (by default, the current working directory of the caller).
*N.B.* argument values in the `parameters.log` are contained between single (`' '`) or double (`" "`) quotes.

### Protocol

#### PARAMETERS[^2]
Some are *required* and must be explicitly defined, but for most we can use implicit values (i.e., if the user doesn't set them explicitly, we may simply not specify them to the `parameters.log` file and the default values in `AdhereR` will be used).


#### COMMENTS
Everything on a line following `///` or `#` is considered a comment and ignored (except when included within quotes `" "` or `' '`).


#### SPECIAL PARAMETERS[^3]

| PARAMETER  | MEANING  | DEFAULT VALUE IF MISSING | PYHTON 3 | STATA |
|------------|----------|--------------------------|----------|-------|
| `NA.SYMBOL.NUMERIC` | the numeric missing data symbol | `NA` | `NA` | `.` |
| `NA.SYMBOL.STRING` | the string missing data symbol | `NA` | `NA` | `""` |
| `LOGICAL.SYMBOL.TRUE` | the logical `TRUE` symbol | `TRUE` | `TRUE` | `1` |
| `LOGICAL.SYMBOL.FALSE` | the logical `FALSE` symbol | `FALSE` | `FALSE` | `0` |
| `COLNAMES.DOT.SYMBOL` | can we use `.` in column names, and if not, what to replace it with? | `.` | `.` | `_` |
| `COLNAMES.START.DOT` | can begin column names with `.` (or equivalent symbol), and if not, what to replace it with? | `.` | `.` | `internal_` |


#### FUNCTIONS
Possible values are:

  - `CMA0`,
  - `CMA1` ...`CMA9`,
  - `CMA_per_episode`,
  - `CMA_sliding_window`,
  - `compute.event.int.gaps`,
  - `compute.treatment.episodes` and 
  - `plot_interactive_cma`.


#### PLOTTING 
For all the `CMA` functions (i.e., `CMA0`, `CMA1` ...`CMA9`, `CMA_per_episode`, `CMA_sliding_window`) one can ask for a plot of (a subset) of the patients, in which case the parameter `plot.show` must be `TRUE`, and there are several plotting-specific parameters that can be set:

| PARAMETER  | REQUIRED  | DEFAULT_VALUE | POSSIBLE_VALUES |
|------------|-----------|---------------|-----------------|
| `function`   | YES       | `"CMA0"`        | can also be `"CMA0"` for plotting! |
| `plot.show`  | NO        | `"FALSE"`       | [do the plotting? If `TRUE`, save the resulting dataset with a `"-plotted"` suffix to avoid overwriting previous results] |
| `plot.save.to` | NO | `""` | [the folder where to save the plots (by default, same folder as the results)] | 
| `plot.save.as` | NO | `"jpg"` | `"jpg"`, `"png"`, `"tiff"`, `"eps"`, `"pdf"` [the type of image to save] | 
| `plot.width` | NO | `"7"` | [plot width in inches] | 
| `plot.height` | NO | `"7"` | [plot height in inches] | 
| `plot.quality` | NO | `"90"` | [plot quality (applies only to some types of plots] | 
| `plot.dpi` | NO | `"150"` | [plot DPI (applies only to some types of plots] | 
| `plot.patients.to.plot` | NO | `""` | [the patient IDs to plot (if missing, all patients) given as `"id1;id2; .. ;idn"`] | 
| `plot.duration`	 | NO | `""` | [duration to plot in days (if missing, determined from the data)] | 
| `plot.align.all.patients` | NO | `"FALSE"` | [should all patients be aligned? and, if so, place the first event as the horizontal 0?] | 
| `plot.align.first.event.at.zero` | NO | `"TRUE"` |  | 
| `plot.show.period` | NO	 | `"days"` | `"dates"`, `"days"`  [draw vertical bars at regular interval as dates or days?] | 
| `plot.period.in.days` | NO | `"90"` | [the interval (in days) at which to draw vertical lines] | 
| `plot.show.legend` | NO | `"TRUE"` | [legend params and position] | 
| `plot.legend.x` | NO | `"bottom right"` |  | 
| `plot.legend.y` | NO | `""` |  | 
| `plot.legend.bkg.opacity` | NO | `"0.5"` | [background opacity] | 
| `plot.cex` | NO | `"1.0"`	 | [various plotting font sizes] | 
| `plot.cex.axis` | NO | `"0.75"` |  | 
| `plot.cex.lab` | NO	 | `"1.0"` |  | 
| `plot.show.cma`	 | NO | `"TRUE"` | [show the CMA type] | 
| `plot.unspecified.category.label` | NO | `"drug"` | [the label of the unspecified category of medication] | 
| `plot.lty.event` | NO | `"solid"`	 | [style parameters controlling the plotting of events] | 
| `plot.lwd.event` | NO | `"2"` |  | 
| `plot.pch.start.event` | NO | `"15"` |  | 
| `plot.pch.end.event` | NO | `"16"` |  | 
| `plot.show.event.intervals`	 | NO | `"TRUE"` | [show the actual prescription intervals] | 
| `plot.col.na` | NO | `"lightgray"` | [colour for missing data] | 
| `plot.col.continuation` | NO | `"black"` | [colour, style and width of the continuation lines connecting consecutive events] | 
| `plot.lty.continuation` | NO | `"dotted"` |  | 
| `plot.lwd.continuation` | NO | `"1"` |  | 
| `plot.print.CMA` | NO | `"TRUE"` | [print CMA next to the participant's ID?] | 
| `plot.plot.CMA`	 | NO | `"TRUE"` | [plot the CMA next to the participant ID?] | 
| `plot.plot.CMA.as.histogram` | NO | `"TRUE"` | [lot CMA as a histogram or as a density plot?] | 
| `plot.CMA.plot.ratio` | NO | `"0.10"` | [the proportion of the total horizontal plot to be taken by the CMA plot] | 
| `plot.CMA.plot.col`	 | NO | `"lightgreen"` | [attributes of the CMA plot] | 
| `plot.CMA.plot.border` | NO	 | `"darkgreen"` |  | 
| `plot.CMA.plot.bkg`	 | NO | `"aquamarine"` |  | 
| `plot.CMA.plot.text` | NO | `""` | [by default, the same as `plot.CMA.plot.border`]  | 
| `plot.highlight.followup.window` | NO | `"TRUE"` |  | 
| `plot.followup.window.col` | NO	 | `"green"` |  | 
| `plot.highlight.observation.window` | NO | `"TRUE"` |  | 
| `plot.observation.window.col` | NO | `"yellow"` |  | 
| `plot.observation.window.density` | NO | `"35"` |  | 
| `plot.observation.window.angle`	 | NO | `"-30"` |  | 
| `plot.show.real.obs.window.start` | NO | `"TRUE"` | [for some CMAs, the real observation window starts at a different date] | 
| `plot.real.obs.window.density` | NO | `"35"` |  | 
| `plot.real.obs.window.angle` | NO | `"30"` |  |  
| `plot.bw.plot` | NO | `"FALSE"` | [if `TRUE`, override all user-given colours and replace them with a scheme suitable for grayscale plotting] | 


#### `CMA1`, `CMA2`, `CMA3`, `CMA4`
The parameters for these functions are (*N.B.*: the plotting parameters can also appear if  plotting is required):

| PARAMETER  | REQUIRED  | DEFAULT_VALUE | POSSIBLE_VALUES |
|------------|-----------|---------------|-----------------|
| `ID.colname` | YES  |  | 
| `event.date.colname` | YES |  | 
| `event.duration.colname` | YES |  | 
| `followup.window.start.type` | NO | `"numeric"` | `"numeric"`, `"character"`, `"date"` | 
| `followup.window.start` | NO | `0` |  | 
| `followup.window.start.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `followup.window.duration.type` | NO | `"numeric"`  | `"numeric"`, `"character"`, `"date"` | 
| `followup.window.duration` | NO | `"365 * 2"` |  | 
| `followup.window.duration.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `observation.window.start.type` | NO | `"numeric"` | `"numeric"`, `"character"`, `"date"` | 
| `observation.window.start	` | NO | `0` |  | 
| `observation.window.start.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `observation.window.duration.type` | NO | `"numeric"` | `"numeric"`, `"character"`, `"date"` | 
| `observation.window.duration` | NO | `"365 * 2"` |  | 
| `observation.window.duration.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `date.format` | NO | `"%m/%d/%Y"` |  | 
| `event.interval.colname` | NO	 | `"event.interval"` |  | 
| `gap.days.colname` | NO | `"gap.days"` |  | 
| `force.NA.CMA.for.failed.patients` | NO | `"TRUE"` |  | 
| `parallel.backend	` | NO | `"none"` | `"none"`, `"multicore"`, `"snow"`, `"snow(SOCK)"`, `"snow(MPI)"`, `"snow(NWS)"` | 
| `parallel.threads` | NO | `"auto"` | | 
| `suppress.warnings` | NO | `"FALSE"` |  | 
| `save.event.info` | NO | `"FALSE"` |  | 

| RETURN VALUE(S) | FILE | OBSERVATIONS |
|-----------------|------|--------------|
| Errors, warnings and other messages | `Adherer-results.txt` | Possibly more than one line; if the processing was successful, the last line must begin with `OK:` |
| The computed CMAs, as a TAB-separated no quotes CSV file | `CMA.csv` | Always generated in case of successful processing |
| The gap days and event info data, as a TAB-separated no quotes CSV file | `EVENTINFO.csv` | Only by explicit request (i.e., `save.event.info = "TRUE"`) |



#### `CMA5`, `CMA6`, `CMA7`, `CMA8`, `CMA9`
The parameters for these functions are (*N.B.*: the plotting parameters can also appear if  plotting is required):

| PARAMETER  | REQUIRED  | DEFAULT_VALUE | POSSIBLE_VALUES |
|------------|-----------|---------------|-----------------|
| `ID.colname` | YES  |  | 
| `event.date.colname` | YES |  | 
| `event.duration.colname` | YES |  | 
| `event.daily.dose.colname	` | YES |  | 
| `medication.class.colname	` | YES |  | 
| `carry.only.for.same.medication` | NO | `"FALSE"` |  | 
| `consider.dosage.change` | NO	 | `"FALSE"` |  | 
| `followup.window.start.type` | NO | `"numeric"` | `"numeric"`, `"character"`, `"date"` | 
| `followup.window.start` | NO | `0` |  | 
| `followup.window.start.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `followup.window.duration.type` | NO | `"numeric"`  | `"numeric"`, `"character"`, `"date"` | 
| `followup.window.duration` | NO | `"365 * 2"` |  | 
| `followup.window.duration.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `observation.window.start.type` | NO | `"numeric"` | `"numeric"`, `"character"`, `"date"` | 
| `observation.window.start	` | NO | `0` |  | 
| `observation.window.start.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `observation.window.duration.type` | NO | `"numeric"` | `"numeric"`, `"character"`, `"date"` | 
| `observation.window.duration` | NO | `"365 * 2"` |  | 
| `observation.window.duration.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `date.format` | NO | `"%m/%d/%Y"` |  | 
| `event.interval.colname` | NO	 | `"event.interval"` |  | 
| `gap.days.colname` | NO | `"gap.days"` |  | 
| `force.NA.CMA.for.failed.patients` | NO | `"TRUE"` |  | 
| `parallel.backend	` | NO | `"none"` | `"none"`, `"multicore"`, `"snow"`, `"snow(SOCK)"`, `"snow(MPI)"`, `"snow(NWS)"` | 
| `parallel.threads` | NO | `"auto"` | | 
| `suppress.warnings` | NO | `"FALSE"` |  | 
| `save.event.info` | NO | `"FALSE"` |  | 

| RETURN VALUE(S) | FILE | OBSERVATIONS |
|-----------------|------|--------------|
| Errors, warnings and other messages | `Adherer-results.txt` | Possibly more than one line; if the processing was successful, the last line must begin with `OK:` |
| The computed CMAs, as a TAB-separated no quotes CSV file | `CMA.csv` | Always generated in case of successful processing |
| The gap days and event info data, as a TAB-separated no quotes CSV file | `EVENTINFO.csv` | Only by explicit request (i.e., `save.event.info = "TRUE"`) |



#### `CMA_per_episode`
The parameters for this function are (*N.B.*: the plotting parameters can also appear if  plotting is required):

| PARAMETER  | REQUIRED  | DEFAULT_VALUE | POSSIBLE_VALUES |
|------------|-----------|---------------|-----------------|
| `CMA.to.apply	` | YES | | `CMA1`, `CMA2`, `CMA3`, `CMA4`, `CMA5`, `CMA6`, `CMA7`, `CMA8`, `CMA9` |
| `ID.colname` | YES  |  | 
| `event.date.colname` | YES |  | 
| `event.duration.colname` | YES |  | 
| `event.daily.dose.colname	` | YES |  | 
| `medication.class.colname	` | YES |  | 
| `carry.only.for.same.medication` | NO | `"FALSE"` |  | 
| `consider.dosage.change` | NO	 | `"FALSE"` |  | 
| `medication.change.means.new.treatment.episode` | NO | `"TRUE"` |  | 
| `maximum.permissible.gap` | NO | `"90"` |  | 
| `maximum.permissible.gap.unit	` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"`, `"percent"` | 
| `followup.window.start.type` | NO | `"numeric"` | `"numeric"`, `"character"`, `"date"` | 
| `followup.window.start` | NO | `0` |  | 
| `followup.window.start.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `followup.window.duration.type` | NO | `"numeric"`  | `"numeric"`, `"character"`, `"date"` | 
| `followup.window.duration` | NO | `"365 * 2"` |  | 
| `followup.window.duration.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `observation.window.start.type` | NO | `"numeric"` | `"numeric"`, `"character"`, `"date"` | 
| `observation.window.start	` | NO | `0` |  | 
| `observation.window.start.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `observation.window.duration.type` | NO | `"numeric"` | `"numeric"`, `"character"`, `"date"` | 
| `observation.window.duration` | NO | `"365 * 2"` |  | 
| `observation.window.duration.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `date.format` | NO | `"%m/%d/%Y"` |  | 
| `event.interval.colname` | NO	 | `"event.interval"` |  | 
| `gap.days.colname` | NO | `"gap.days"` |  | 
| `force.NA.CMA.for.failed.patients` | NO | `"TRUE"` |  | 
| `parallel.backend	` | NO | `"none"` | `"none"`, `"multicore"`, `"snow"`, `"snow(SOCK)"`, `"snow(MPI)"`, `"snow(NWS)"` | 
| `parallel.threads` | NO | `"auto"` | | 
| `suppress.warnings` | NO | `"FALSE"` |  | 
| `save.event.info` | NO | `"FALSE"` |  | 

| RETURN VALUE(S) | FILE | OBSERVATIONS |
|-----------------|------|--------------|
| Errors, warnings and other messages | `Adherer-results.txt` | Possibly more than one line; if the processing was successful, the last line must begin with `OK:` |
| The computed CMAs, as a TAB-separated no quotes CSV file | `CMA.csv` | Always generated in case of successful processing |
| The gap days and event info data, as a TAB-separated no quotes CSV file | `EVENTINFO.csv` | Only by explicit request (i.e., `save.event.info = "TRUE"`) |



#### `CMA_sliding_window`
The parameters for this function are (*N.B.*: the plotting parameters can also appear if  plotting is required):

| PARAMETER  | REQUIRED  | DEFAULT_VALUE | POSSIBLE_VALUES |
|------------|-----------|---------------|-----------------|
| `CMA.to.apply	` | YES | | `CMA1`, `CMA2`, `CMA3`, `CMA4`, `CMA5`, `CMA6`, `CMA7`, `CMA8`, `CMA9` |
| `ID.colname` | YES  |  | 
| `event.date.colname` | YES |  | 
| `event.duration.colname` | YES |  | 
| `event.daily.dose.colname	` | YES |  | 
| `medication.class.colname	` | YES |  | 
| `carry.only.for.same.medication` | NO | `"FALSE"` |  | 
| `consider.dosage.change` | NO	 | `"FALSE"` |  | 
| `followup.window.start.type` | NO | `"numeric"` | `"numeric"`, `"character"`, `"date"` | 
| `followup.window.start` | NO | `0` |  | 
| `followup.window.start.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `followup.window.duration.type` | NO | `"numeric"`  | `"numeric"`, `"character"`, `"date"` | 
| `followup.window.duration` | NO | `"365 * 2"` |  | 
| `followup.window.duration.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `observation.window.start.type` | NO | `"numeric"` | `"numeric"`, `"character"`, `"date"` | 
| `observation.window.start	` | NO | `0` |  | 
| `observation.window.start.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `observation.window.duration.type` | NO | `"numeric"` | `"numeric"`, `"character"`, `"date"` | 
| `observation.window.duration` | NO | `"365 * 2"` |  | 
| `observation.window.duration.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `sliding.window.start.type` | NO | `"numeric"` | `"numeric"`, `"character'`, `"date'` | 
| `sliding.window.start` | NO | `0` |  | 
| `sliding.window.start.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `sliding.window.duration.type	` | NO | `"numeric"` | `"numeric"`, `"character"`, `"date"` | 
| `sliding.window.duration` | NO | `"90"` |  | 
| `sliding.window.duration.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `sliding.window.step.duration.type` | NO | `"numeric"` | `"numeric"`, `"character"` | 
| `sliding.window.step.duration	` | NO | `"30"` |  | 
| `sliding.window.step.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `sliding.window.no.steps` | NO | `"-1"` |  | 
| `date.format` | NO | `"%m/%d/%Y"` |  | 
| `event.interval.colname` | NO	 | `"event.interval"` |  | 
| `gap.days.colname` | NO | `"gap.days"` |  | 
| `force.NA.CMA.for.failed.patients` | NO | `"TRUE"` |  | 
| `parallel.backend	` | NO | `"none"` | `"none"`, `"multicore"`, `"snow"`, `"snow(SOCK)"`, `"snow(MPI)"`, `"snow(NWS)"` | 
| `parallel.threads` | NO | `"auto"` | | 
| `suppress.warnings` | NO | `"FALSE"` |  | 
| `save.event.info` | NO | `"FALSE"` |  | 

| RETURN VALUE(S) | FILE | OBSERVATIONS |
|-----------------|------|--------------|
| Errors, warnings and other messages | `Adherer-results.txt` | Possibly more than one line; if the processing was successful, the last line must begin with `OK:` |
| The computed CMAs, as a TAB-separated no quotes CSV file | `CMA.csv` | Always generated in case of successful processing |
| The gap days and event info data, as a TAB-separated no quotes CSV file | `EVENTINFO.csv` | Only by explicit request (i.e., `save.event.info = "TRUE"`) |


#### `compute_event_int_gaps`
This function is intended for advanced users only; the parameters for this function are:

| PARAMETER  | REQUIRED  | DEFAULT_VALUE | POSSIBLE_VALUES |
|------------|-----------|---------------|-----------------|
| `ID.colname` | YES  |  |  | 
| `event.date.colname` | YES |  |  | 
| `event.duration.colname` | YES |  |  | 
| `event.daily.dose.colname	` | NO |  |  | 
| `medication.class.colname` | NO |  |  | 
| `carryover.within.obs.window` | NO | `"FALSE"` |  | 
| `carryover.into.obs.window` | NO | `"FALSE"` |  | 
| `carry.only.for.same.medication` | NO | `"FALSE"` |  | 
| `consider.dosage.change` | NO	 | `"FALSE"` |  | 
| `followup.window.start.type` | NO | `"numeric"` | `"numeric"`, `"character"`, `"date"` | 
| `followup.window.start` | NO | `"0"` |  | 
| `followup.window.start.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `followup.window.duration.type` | NO | `"numeric"` | `"numeric"`, `"character"`, `"date"` | 
| `followup.window.duration` | NO | `"365 * 2`" |  | 
| `followup.window.duration.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `observation.window.start.type` | NO | `"numeric"` | `"numeric"`, `"character"`, `"date"` | 
| `observation.window.start	` | NO | `"0"` |  | 
| `observation.window.start.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `observation.window.duration.type` | NO | `"numeric"` | `"numeric"`, `"character"`, `"date"` | 
| `observation.window.duration` | NO | `"365 * 2"` |  | 
| `observation.window.duration.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `date.format` | NO | `"%m/%d/%Y"` |  | 
| `keep.window.start.end.dates` | NO | `"FALSE"` |  | 
| `remove.events.outside.followup.window` | NO	 | `"TRUE"` |  | 
| `keep.event.interval.for.all.events` | NO | `"FALSE"` |  | 
| `event.interval.colname` | NO	 | `"event.interval`" |  | 
| `gap.days.colname	` | NO | `"gap.days"` |  | 
| `force.NA.CMA.for.failed.patients` | NO | `"TRUE"` |  | 
| `parallel.backend	` | NO | `"none	"` | `"none"`, `"multicore"`, `"snow"`, `"snow(SOCK)"`, `"snow(MPI)"`, `"snow(NWS)"` | 
| `parallel.threads` | NO | `"auto"` |  | 
| `suppress.warnings` | NO | `"FALSE"` |  | 

| RETURN VALUE(S) | FILE | OBSERVATIONS |
|-----------------|------|--------------|
| Errors, warnings and other messages | `Adherer-results.txt` | Possibly more than one line; if the processing was successful, the last line must begin with `OK:` |
| The gap days and event info data, as a TAB-separated no quotes CSV file | `EVENTINFO.csv` | In this case, always returned is successful |


#### `compute_treatment_episodes`
This function is intended for advanced users only; the parameters for this function are: 

| PARAMETER  | REQUIRED  | DEFAULT_VALUE | POSSIBLE_VALUES |
|------------|-----------|---------------|-----------------|
| `ID.colname` | YES  |  | 
| `event.date.colname` | YES |  | 
| `event.duration.colname` | YES |  | 
| `event.daily.dose.colname	` | NO |  | 
| `medication.class.colname	` | NO |  | 
| `carryover.within.obs.window` | NO | `"FALSE"` |  | 
| `carryover.into.obs.window` | NO | `"FALSE"` |  | 
| `carry.only.for.same.medication` | NO | `"FALSE"` |  | 
| `consider.dosage.change` | NO	 | `"FALSE"` |  | 
| `medication.change.means.new.treatment.episode` | NO | `"TRUE"` |  | 
| `maximum.permissible.gap` | NO | `"90"` |  | 
| `maximum.permissible.gap.unit	` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"`, `"percent"` | 
| `followup.window.start.type` | NO | `"numeric"` | `"numeric"`, `"character"`, `"date"` | 
| `followup.window.start` | NO | `0` |  | 
| `followup.window.start.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `followup.window.duration.type` | NO | `"numeric"`  | `"numeric"`, `"character"`, `"date"` | 
| `followup.window.duration` | NO | `"365 * 2"` |  | 
| `followup.window.duration.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `observation.window.start.type` | NO | `"numeric"` | `"numeric"`, `"character"`, `"date"` | 
| `observation.window.start	` | NO | `0` |  | 
| `observation.window.start.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `observation.window.duration.type` | NO | `"numeric"` | `"numeric"`, `"character"`, `"date"` | 
| `observation.window.duration` | NO | `"365 * 2"` |  | 
| `observation.window.duration.unit` | NO | `"days"` | `"days"`, `"weeks"`, `"months"`, `"years"` | 
| `date.format` | NO | `"%m/%d/%Y"` |  | 
| `keep.window.start.end.dates` | NO | `"FALSE"` |  | 
| `remove.events.outside.followup.window` | NO | `"TRUE"` |  | 
| `keep.event.interval.for.all.events` | NO | `"FALSE"` |  | 
| `event.interval.colname` | NO	 | `"event.interval"` |  | 
| `gap.days.colname` | NO | `"gap.days"` |  | 
| `force.NA.CMA.for.failed.patients` | NO | `"TRUE"` |  | 
| `parallel.backend	` | NO | `"none"` | `"none"`, `"multicore"`, `"snow"`, `"snow(SOCK)"`, `"snow(MPI)"`, `"snow(NWS)"` | 
| `parallel.threads` | NO | `"auto"` | | 
| `suppress.warnings` | NO | `"FALSE"` |  | 

| RETURN VALUE(S) | FILE | OBSERVATIONS |
|-----------------|------|--------------|
| Errors, warnings and other messages | `Adherer-results.txt` | Possibly more than one line; if the processing was successful, the last line must begin with `OK:` |
| The treatment episodes data, as a TAB-separated no quotes CSV file | `TREATMENTEPISODES.csv ` | Always if successful |


#### `plot_interactive_cma`
This function initiates the interactive plotting in `AdhereR` using `Shiny`: all the plotting will be done in the current internet browser and there are no results expected (except for errors, warnings and other messages).
This function ignores the argument `plot.show = "TRUE"` and takes very few arguments of its own, as most of the relevant parameters can be set interactively through the `Shiny` interface.

| PARAMETER  | REQUIRED  | DEFAULT_VALUE | POSSIBLE_VALUES |
|------------|-----------|---------------|-----------------|
| `patient_to_plot`	 | NO | | defaults to the first patient in the dataset | 
| `ID.colname` | YES  |  | 
| `event.date.colname` | YES |  | 
| `event.duration.colname` | YES |  | 
| `event.daily.dose.colname	` | NO |  | 
| `medication.class.colname	` | NO |  | 
| `date.format` | NO | `"%m/%d/%Y"` |  | 
| `followup.window.start.max` | NO | | integer >0 
| `followup.window.duration.max` | NO | | integer >0 
| `observation.window.start.max` | NO | | integer >0 
| `observation.window.duration.max	` | NO | | integer >0 
| `maximum.permissible.gap.max	` | NO | | integer >0 
| `sliding.window.start.max` | NO | | integer >0 
| `sliding.window.duration.max	` | NO | | integer >0 
| `sliding.window.step.duration.max` | NO | | integer >0 



## References

Dima AL, Dediu D (2017) Computation of adherence to medication and visualization of medication histories in R with AdhereR: Towards transparent and reproducible use of electronic healthcare data. *PLoS ONE* **12(4)**: e0174426. [https://doi.org/10.1371/journal.pone.0174426](https://doi.org/10.1371/journal.pone.0174426) <a id="dimadediu2017"></a>

## Notes


[^1]: It was developed and tested with `Python 3.6.5` on macOS 10.13 (High Sierra), but should be quite general.

[^2]: For more info on the parameters and their values for all these functions please see the `AdhereR` documentation and vignette.

[^3]: While this document concerns mainly `Python 3`, I also give the default values for other platforms, in particular `STATA`.
