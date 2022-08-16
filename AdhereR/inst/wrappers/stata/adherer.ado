* 	adherer: use R's AdhereR package from STATA
*   Copyright (C) 2022  Dan Dediu (ddediu@gmail.com)
*             based on initial work by María Rubio-Valera (c) 2018
*             based on ideas and code from Julian Reif's and David Molitor's 
*              `RSCRIPT` package (https://github.com/reifjulian/rscript) (v1.1)
*               code from them is marked by [from rscript]...[end from rscript]
*
*   This program is free software: you can redistribute it and/or modify
*   it under the terms of the GNU General Public License as published by
*   the Free Software Foundation, either version 3 of the License, or
*   (at your option) any later version.
*
*   This program is distributed in the hope that it will be useful,
*   but WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*   GNU General Public License for more details.
*
*   You should have received a copy of the GNU General Public License
*   along with this program.  If not, see <https://www.gnu.org/licenses/>.


* Note: this has been developed and tested for Windows 10 with STATA MP 13.1
*       while it may run on other systems as well, this is not guaranteed.


* FOR DEVELOPMENT ONLY: make STATA find this .ado file!
* NORMALLY this .ado file should reside in the user's PERSONAL STATA folder...
sysdir set PERSONAL "C:\Users\ddedi\Work\Misc\AdhereR\GitHub\AdhereR\AdhereR\inst\wrappers\stata"

* FOR DEBUGGING, run with the "verbose" parameter: adherer ,verbose
* FOR DEBUGGING: load (or pass as param) the CSV file "C:\Users\ddedi\Work\Misc\AdhereR\tests\test-dataset.csv":
* import delimited "C:\Users\ddedi\Work\Misc\AdhereR\GitHub\AdhereR\AdhereR\inst\wrappers\stata\test-dataset.csv", varnames(1) delimiter(tab) clear
* adherer , verbose cma("CMA1") id_col("id") ev_date_col("date") ev_dur_col("duration")
* adherer , verbose cma("CMA1") plot id_col("id") ev_date_col("date") ev_dur_col("duration") folder_to_use("C:\Temp\for_adherer")
* adherer , verbose cma("CMA1") plot patients_to_plot("1;2;3") plot_width(12) align_all_patients id_col("id") ev_date_col("date") ev_dur_col("duration") folder_to_use("C:\Temp\for_adherer") dont_load_results
* adherer , verbose cma("CMA_per_episode") cma_to_apply("CMA1") plot patients_to_plot("1;2;3") plot_width(12) align_all_patients id_col("id") ev_date_col("date") ev_dur_col("duration") ev_dose_col("perday") med_class_col("class") folder_to_use("C:\Temp\for_adherer") dont_load_results save_event_info
* adherer , verbose cma("CMA_sliding_window") cma_to_apply("CMA1") sw_dur(60) sw_step_dur(90) plot patients_to_plot("1;2;3") plot_width(12) align_all_patients id_col("id") ev_date_col("date") ev_dur_col("duration") ev_dose_col("perday") med_class_col("class") folder_to_use("C:\Temp\for_adherer") dont_load_results save_event_info
* adherer , verbose cma("compute_event_int_gaps") id_col("id") ev_date_col("date") ev_dur_col("duration")
* adherer , verbose cma("compute_treatment_episodes") id_col("id") ev_date_col("date") ev_dur_col("duration") ev_dose_col("perday") med_class_col("class") 
* adherer , verbose cma("plot_interactive_cma") id_col("id") ev_date_col("date") ev_dur_col("duration") ev_dose_col("perday") med_class_col("class") 

* TODO:
* - try to import the plot --> doesn't seem to be any obvious way :(
* - hide command window on shell invokation --> doesn't seem to be any obvious way :(

* Make sure this program can de reloaded:
capture program drop adherer
capture program drop rscript_adh
capture program drop write_r_script_adh
capture program drop parse_stderr_version_control_adh
capture program drop parse_stderr_adh
capture program drop parse_stdout_adh

* The adherer program:
*  returns the path to the folder contaning the results in r(adherer_resdir)
program adherer, rclass

	version 13.0

	* the parameters:
	syntax [using/], /* use the dataset in memory (default) or a CSV file? */ ///
	       cma(string) /* which CMA to compute; must be one of (case insensistive) CMA0 (can only be plotted), CMA1, CMA2, CMA3, CMA4, CMA5, CMA6, CMA7, CMA8, CMA9, CMA_per_episode, CMA_sliding_window, compute_event_int_gaps, compute_treatment_episodes and  plot_interactive_cma */ ///
		   id_col(string) ev_date_col(string) ev_dur_col(string) /* required column names for all CMAs (these must exist in exactly the same form in the dataset/input CSV file); patient ID, event date, and event duration */ ///
		   [ ///
		    ev_dose_col(string) med_class_col(string) /* column names only needed for CMAs 5 to 9; dose, and medication cass */ ///
			fuw_start_type(string) fuw_start(string) fuw_start_unit(string) fuw_dur_type(string) fuw_dur(string) fuw_dur_unit(string) /* follow-up window specification: start type (can be "numeric" (default), "character" or "date"), the start value conforming to the previous specification, and the unit (if "numeric"; can be "days" (default), "weeks", "months" or "years"), duration is similar to start */ ///
			ow_start_type(string) ow_start(string) ow_start_unit(string) ow_dur_type(string) ow_dur(string) ow_dur_unit(string) /* observation windows spec: similar to the follow-up-window spec (see above) */ ///
			date_format(string) /* date format (defaults to "%m/%d/%Y") */ ///
			carry_for_same_med consider_dose_change /* CMA5..CMA9: carry only for the same medication, and should a change in dosage be considered? */ ///
			cma_to_apply(string) /* which simple CMA to apply (only for per episodes and sliding window); must be > CMA0 */ ///
			dont_med_chg_is_new_episode max_permissible_gap(string) max_permissible_gap_unit(string) /* params defining the episodes (the unit can be one of "days", "weeks", "months", "years" or "percent") */ ///
			sw_start_type(string) sw_start(string) sw_start_unit(string) sw_dur_type(string) sw_dur(string) sw_dur_unit(string) /* sliding windows spec: similar to the follow-up-window spec (see above) */  ///
			sw_step_dur_type(string) sw_step_dur(string) sw_step_unit(string) sw_number_steps(string) /* specification of sliding window steps either as duration between consecutive steps or as the number of steps */ ///
			save_event_info /* also save the event info in the EVENTINFO.csv TAB-separated file? */ ///
			carryover_within_ow carryover_into_ow keep_window_start_end_dates keep_events_outside_fuw keep_ev_int_for_all_events ev_int_col(string) gap_days_col(string) /* advanced params: see AdhereR's R help */ ///
			plot /* if present, a plot is (also) generated */ ///
			patients_to_plot(string) /* the patient IDs to plot (if missing, all patients) given as "id1;id2; .. ;idn" */ ///
			plot_type(string) plot_width(string) plot_height(string) plot_quality(string) plot_dpi(string) /* plot specification: type (can be "jpg", "png", "tiff", "eps" or "pdf"), width and height (in inches), quality and dpi */ ///
			plot_duration(string) /* duration to plot in days (if missing, determined from the data) */ ///
			align_all_patients dont_align_first_ev_zero show_period_as_dates /* shuld the patients be aligned? should the first event not be aligned to 0? show period as dtes (instead of days)? */ ///
			dont_show_legend legend_position(string) /* show legend and, if yes, where; can be "bottom right" (default), "bottom left", "top right", "top left" */ ///
			dont_show_ev_ints dont_plot_evs_displaced /* how to plot the event intervals */ ///
			print_dose plot_dose /* should we print and/or plot (as line width) the dose? */ ///
			dont_plot_cma plot_partial_cmas_as(string) /* how to plot the CMA and partial CMAs (per events and sliding window; can be “stacked” (default), “overlapping” or “timeseries”) */ ///
			bw_plot /* gray-scale plotting? */ ///
			max_patients_to_plot(string) /* how many patients to plot (maximum value); if missing, 100 */ ///
			folder_to_use(string) /* if missing, we'll use STATA's temporary folder, but if given must be an existing folder (directoy) where we can read and write files */ ///
			dont_load_results /* by default, the estimated CMAs (if any) are loaded in STATA, otherwise no changes are made to the dataset and the results are left on disk */ ///
		    rpath(string) rversion(string) /* force a particular version of location for R? otherwise we use the default one */ ///
			verbose /* if requested we display all sorts of information, potentially useful for debugging purposes */ ///
		   ]
	
	* preserve the already existsing dataset:
	preserve

	if mi("`folder_to_use'") {
		* use STATA's temp files:
		tempfile auto adherer_script
		tempfile auto adherer_input_csv
		tempfile auto adherer_input_args
		tempfile auto adherer_output
	}
	else {
		* use the user-supplied folder:
		capture cd "`folder_to_use'"
		if _rc!=0 {
			di as error `"The given folder "`folder_to_use'" for data exchange does not exist!"'
			exit 1
		}
		local adherer_script     "`folder_to_use'/script.R"
		local adherer_input_csv  "`folder_to_use'/dataset.csv"
		local adherer_input_args "`folder_to_use'/parameters.log"
		local adherer_output     "`folder_to_use'/script_output.txt"
	}

	* important names: the AdhereR R package name, minimum required version, and external call function:
	local R_PACKAGE_NAME = "AdhereR"              // package name
	local R_PACKAGE_MIN_VERSION = "0.1.1"         // minimum version
	local R_PACKAGE_CALL_FUNCTION = "callAdhereR" // function interfacing with non-R callers


	* 0. sanity checks:
	* only runs on Windows:
	if "`c(os)'" != "Windows" {
		display as error "This program currently runs only in STATA for Windows!"
		exit 1
	}
  
	* [from rscript] does not work in batch mode on Stata for Windows because Stata ignores shell requests (as of Stata 17.0):
	if "`c(os)'" == "Windows" & "`c(mode)'" == "batch" {
		di as error "This program does not work in batch mode on Stata for Windows because Stata ignores shell requests in this setting!"
		exit 1
	}
	* [end from rscript]

	* 1. locate and check R and AdhereR
	global RSCRIPT_PATH_ADHERER ""
	
	* 1.1. check R:
	tempname filebf
	qui file open `filebf' using "`adherer_script'", write text replace
	file write `filebf' `"args <- commandArgs(trailingOnly = "TRUE");"' _n
	file write `filebf' `"if(length(args)) { outfile <- args[1] } else { stop("Output file path missing") }"' _n
	file write `filebf' `"res <- data.frame('result'=(1+1));"' _n
	file write `filebf' `"write.csv(res, file=outfile, row.names=FALSE);"' _n
	qui file close `filebf'
	if "`verbose'"!="" { 
		rscript_adh using "`adherer_script'", args("`adherer_output'") verbose
	} 
	else {
		rscript_adh using "`adherer_script'", args("`adherer_output'") 
	}
	quiet import delimited using "`adherer_output'", varnames(1) delimiter(tab) clear
	quiet ds
	if r(varlist) != "result" {
		di as error "There's something wrong with R: please check if it is correctly installed and that STATA is using the crrect version!"
		exit 1
	}
	if result[1] != 2  {
		di as error "There's something wrong with R: please check if it is correctly installed and that STATA is using the crrect version!"
		exit 1
	}
	
	* 1.1. check AdhereR and AdhereRViz:
	qui file open `filebf' using "`adherer_script'", write text replace
	file write `filebf' `"args <- commandArgs(trailingOnly = "TRUE");"' _n
	file write `filebf' `"if(length(args)) { outfile <- args[1] } else { stop("Output file path missing") }"' _n
	file write `filebf' `"if(!require("`R_PACKAGE_NAME'")){ res <- 10 } else{ if(compareVersion("`R_PACKAGE_MIN_VERSION'", as.character(packageVersion("`R_PACKAGE_NAME'"))) > 0 ){ res <- 11 } else { res <- 0 }}"' _n
	file write `filebf' `"write.csv(data.frame('result'=res), file=outfile, row.names=FALSE);"' _n
	qui file close `filebf'
	if "`verbose'"!="" { 
		rscript_adh using "`adherer_script'", args("`adherer_output'") verbose
	} 
	else {
		rscript_adh using "`adherer_script'", args("`adherer_output'") 
	}
	quiet import delimited using "`adherer_output'", varnames(1) delimiter(tab) clear
	quiet ds
	if r(varlist) != "result" {
		di as error "There's something wrong with R: please check if it is correctly installed and that STATA is using the crrect version!"
		exit 1
	}
	if result[1] == 10  {
		di as error "The `R_PACKAGE_NAME' package is not installed for the version of R that STATA is using; please install at least version `R_PACKAGE_MIN_VERSION'!"
		exit 1
	}
	if result[1] == 11  {
		di as error "The `R_PACKAGE_NAME' package installed for the version of R that STATA is using must be at least version `R_PACKAGE_MIN_VERSION'!"
		exit 1
	}
	
	* All basic checks passed...
	* 2. Check params:
	if lower("`cma'") != "cma0"  & lower("`cma'") != "cma1"  & lower("`cma'") != "cma2"  & lower("`cma'") != "cma3"  & lower("`cma'") != "cma4"  & lower("`cma'") != "cma5"  & ///
		 lower("`cma'") != "cma6"  & lower("`cma'") != "cma7"  & lower("`cma'") != "cma8"  & lower("`cma'") != "cma9"  & ///
		  lower("`cma'") != "cma_per_episode"  & lower("`cma'") != "cma_sliding_window" & ///
		  lower("`cma'") != "compute_event_int_gaps" & lower("`cma'") != "compute_treatment_episodes" & ///
		  lower("`cma'") != "plot_interactive_cma" {
		di as error "Unknown CMA function `cma' requested!"
		exit 1
	}
	if lower("`cma'") == "cma0"  & mi("`plot'") {
		di as error "CMA0 can only be used for plots: make sure the 'plot' argument is given!"
		exit 1
	}
	if !mi("`plot'") & ///
	    (lower("`cma'") != "cma0"  & lower("`cma'") != "cma1"  & lower("`cma'") != "cma2"  & lower("`cma'") != "cma3"  & lower("`cma'") != "cma4"  & lower("`cma'") != "cma5"  & ///
		 lower("`cma'") != "cma6"  & lower("`cma'") != "cma7"  & lower("`cma'") != "cma8"  & lower("`cma'") != "cma9"  & ///
		  lower("`cma'") != "cma_per_episode"  & lower("`cma'") != "cma_sliding_window") {
		di as error "Only CMAs can be plotted!"
		exit 1
	}
	if lower("`cma'") == "cma5" | lower("`cma'") == "cma6" | lower("`cma'") == "cma7" | lower("`cma'") == "cma8" | lower("`cma'") == "cma9" {
		if mi("`ev_dose_col'") | mi("`med_class_col'") {
			di as error "For CMA5 to CMA9, also 'ev_dose_col' and 'med_class_col' are required parameters!"
			exit 1
		}
	}
	if !mi("`cma_to_apply'") & (lower("`cma'") != "cma_per_episode"  & lower("`cma'") != "cma_sliding_window") {
		di as error "Argument 'CMA_to_apply' makes sense only for 'cma_per_episode' and 'cma_sliding_window'!"
		exit 1
	}
	if !mi("`cma_to_apply'") & lower("`CMA_to_apply'") == "cma0" {
		di as error "Argument 'CMA_to_apply' cannot be CMA0!"
		exit 1
	}
	if !mi("`fuw_start_type'") {
		if lower("`fuw_start_type'") != "numeric" & lower("`fuw_start_type'") != "character" & lower("`fuw_start_type'") != "date" {
			di as error "If given, 'fuw_start_type' must be one of 'numeric', 'character' or 'date'!"
			exit 1
		}
	}
	if !mi("`fuw_start_unit'") {
		if lower("`fuw_start_unit'") != "days" & lower("`fuw_start_unit'") != "weeks" & lower("`fuw_start_unit'") != "months" & lower("`fuw_start_unit'") != "years" {
			di as error "If given, 'fuw_start_unit' must be one of 'days', 'weeks', 'months' or 'years'!"
			exit 1
		}
	}
	if !mi("`fuw_dur_type'") {
		if lower("`fuw_dur_type'") != "numeric" & lower("`fuw_dur_type'") != "character" & lower("`fuw_dur_type'") != "date" {
			di as error "If given, 'fuw_dur_type' must be one of 'numeric', 'character' or 'date'!"
			exit 1
		}
	}
	if !mi("`fuw_dur_unit'") {
		if lower("`fuw_dur_unit'") != "days" & lower("`fuw_dur_unit'") != "weeks" & lower("`fuw_dur_unit'") != "months" & lower("`fuw_dur_unit'") != "years" {
			di as error "If given, 'fuw_dur_unit' must be one of 'days', 'weeks', 'months' or 'years'!"
			exit 1
		}
	}
	if !mi("`ow_start_type'") {
		if lower("`ow_start_type'") != "numeric" & lower("`ow_start_type'") != "character" & lower("`ow_start_type'") != "date" {
			di as error "If given, 'ow_start_type' must be one of 'numeric', 'character' or 'date'!"
			exit 1
		}
	}
	if !mi("`ow_start_unit'") {
		if lower("`ow_start_unit'") != "days" & lower("`ow_start_unit'") != "weeks" & lower("`ow_start_unit'") != "months" & lower("`ow_start_unit'") != "years" {
			di as error "If given, 'ow_start_unit' must be one of 'days', 'weeks', 'months' or 'years'!"
			exit 1
		}
	}
	if !mi("`ow_dur_type'") {
		if lower("`ow_dur_type'") != "numeric" & lower("`ow_dur_type'") != "character" & lower("`ow_dur_type'") != "date" {
			di as error "If given, 'ow_dur_type' must be one of 'numeric', 'character' or 'date'!"
			exit 1
		}
	}
	if !mi("`ow_dur_unit'") {
		if lower("`ow_dur_unit'") != "days" & lower("`ow_dur_unit'") != "weeks" & lower("`ow_dur_unit'") != "months" & lower("`ow_dur_unit'") != "years" {
			di as error "If given, 'ow_dur_unit' must be one of 'days', 'weeks', 'months' or 'years'!"
			exit 1
		}
	}
	
	* 3. Build the input files to pass to AdhereR...
	quiet file open `filebf' using "`adherer_input_args'", write text replace
	
	* write the parameters header:
	file write `filebf' `"Parameters"' _n
	
	* the function to call:
	file write `filebf' `"function = "`cma'""' _n
	
	* required column names:
	file write `filebf' `"ID.colname = "`id_col'""' _n
	file write `filebf' `"event.date.colname = "`ev_date_col'""' _n
	file write `filebf' `"event.duration.colname = "`ev_dur_col'""' _n
	if !mi("`ev_dose_col'") file write `filebf' `"event.daily.dose.colname = "`ev_dose_col'""' _n
	if !mi("`med_class_col'") file write `filebf' `"medication.class.colname = "`med_class_col'""' _n
	
	* follow-up and observation windows:
	if !mi("`fuw_start_type'")    file write `filebf' `"followup.window.start.type = "`fuw_start_type'""' _n
	if !mi("`fuw_start'")         file write `filebf' `"followup.window.start = "`fuw_start'""' _n
	if !mi("`fuw_start_unit'")    file write `filebf' `"followup.window.start.unit = "`fuw_start_unit'""' _n
	if !mi("`fuw_dur_type'") 	  file write `filebf' `"followup.window.duration.type = "`fuw_dur_type'""' _n
	if !mi("`fuw_dur'")      	  file write `filebf' `"followup.window.duration = "`fuw_dur'""' _n
	if !mi("`fuw_dur_unit'") 	  file write `filebf' `"followup.window.duration.unit = "`fuw_dur_unit'""' _n
	if !mi("`ow_start_type'")     file write `filebf' `"observation.window.start.type = "`ow_start_type'""' _n
	if !mi("`ow_start'")          file write `filebf' `"observation.window.start = "`ow_start'""' _n
	if !mi("`ow_start_unit'")     file write `filebf' `"observation.window.start.unit = "`ow_start_unit'""' _n
	if !mi("`ow_dur_type'")  	  file write `filebf' `"observation.window.duration.type = "`ow_dur_type'""' _n
	if !mi("`ow_dur'")       	  file write `filebf' `"observation.window.duration = "`ow_dur'""' _n
	if !mi("`ow_dur_unit'")  	  file write `filebf' `"observation.window.duration.unit = "`ow_dur_unit'""' _n
	
	* params for some CMAs:
	if !mi("`carry_for_same_med'")   file write `filebf' `"carry.only.for.same.medication = "TRUE""' _n
	if !mi("`consider_dose_change'") file write `filebf' `"consider.dosage.change = "TRUE""' _n
	if !mi("`cma_to_apply'")         file write `filebf' `"CMA.to.apply = "`cma_to_apply'""' _n
	*   per episodes:
	if !mi("`dont_med_chg_is_new_episode'") file write `filebf' `"medication.change.means.new.treatment.episode = "FALSE""' _n
	if !mi("`max_permissible_gap'")         file write `filebf' `"maximum.permissible.gap = "`max_permissible_gap'""' _n
	if !mi("`max_permissible_gap_unit'")    file write `filebf' `"maximum.permissible.gap.unit = "`max_permissible_gap_unit'""' _n
	*   sliding window:
	if !mi("`sw_start_type'")    file write `filebf' `"sliding.window.start.type = "`sw_start_type'""' _n
	if !mi("`sw_start'")         file write `filebf' `"sliding.window.start = "`sw_start'""' _n
	if !mi("`sw_start_unit'")    file write `filebf' `"sliding.window.start.unit = "`sw_start_unit'""' _n
	if !mi("`sw_dur_type'")      file write `filebf' `"sliding.window.duration.type = "`sw_dur_type'""' _n
	if !mi("`sw_dur'")           file write `filebf' `"sliding.window.duration = "`sw_dur'""' _n
	if !mi("`sw_dur_unit'")      file write `filebf' `"sliding.window.duration.unit = "`sw_dur_unit'""' _n
	if !mi("`sw_step_dur_type'") file write `filebf' `"sliding.window.step.duration.type = "`sw_step_dur_type'""' _n
	if !mi("`sw_step_dur'")      file write `filebf' `"sliding.window.step.duration = "`sw_step_dur'""' _n
	if !mi("`sw_step_unit'")     file write `filebf' `"sliding.window.step.unit = "`sw_step_unit'""' _n
	if !mi("`sw_number_steps'")  file write `filebf' `"sliding.window.no.steps = "`sw_number_steps'""' _n
	
	* date format:
	if !mi("`date_format'") file write `filebf' `"date.format = "`date_format'""' _n
	
	* plotting:
	if !mi("`plot'") {
		* plotting requested!
		file write `filebf' `"plot.show = "TRUE""' _n
		
		* specific parameters:
		if !mi("`patients_to_plot'")         file write `filebf' `"plot.patients.to.plot = "`patients_to_plot'""' _n
		if !mi("`plot_type'")                file write `filebf' `"plot.save.as = "`plot_type'""' _n
		if !mi("`plot_width'")               file write `filebf' `"plot.width = "`plot_width'""' _n
		if !mi("`plot_height'")              file write `filebf' `"plot.height = "`plot_height'""' _n
		if !mi("`plot_quality'")             file write `filebf' `"plot.quality = "`plot_quality'""' _n
		if !mi("`plot_dpi'")                 file write `filebf' `"plot.dpi = "`plot_dpi'""' _n
		if !mi("`plot_duration'")            file write `filebf' `"plot.duration = "`plot_duration'""' _n
		if !mi("`align_all_patients'")       file write `filebf' `"plot.align.all.patients = "TRUE""' _n
		if !mi("`dont_align_first_ev_zero'") file write `filebf' `"plot.align.first.event.at.zero = "FALSE""' _n
		if !mi("`show_period_as_dates'")     file write `filebf' `"plot.show.period = "dates""' _n
		if !mi("`dont_show_legend'")         file write `filebf' `"plot.show.legend = "FALSE""' _n
		if !mi("`legend_position'")          file write `filebf' `"plot.legend.x = "`legend_position'""' _n
		if !mi("`dont_show_ev_ints'")        file write `filebf' `"plot.show.event.intervals = "FALSE""' _n
		if !mi("`dont_plot_evs_displaced'")  file write `filebf' `"plot.plot.events.vertically.displaced = "FALSE""' _n
		if !mi("`print_dose'")               file write `filebf' `"plot.print.dose = "TRUE""' _n
		if !mi("`plot_dose'")                file write `filebf' `"plot.plot.dose = "TRUE""' _n
		if !mi("`dont_plot_cma'")            file write `filebf' `"plot.plot.CMA = "FALSE""' _n
		if !mi("`plot_partial_cmas_as'")     file write `filebf' `"plot.plot.partial.CMAs.as = "`plot_partial_cmas_as'""' _n
		if !mi("`bw_plot'")                  file write `filebf' `"plot.bw.plot = "TRUE""' _n
		if !mi("`max_patients_to_plot'")     file write `filebf' `"plot.max.patients.to.plot = "`max_patients_to_plot'""' _n
	}
	
	* advanced params:
	if !mi("`save_event_info'")             file write `filebf' `"save.event.info = "TRUE""' _n
	if !mi("`carryover_within_ow'")         file write `filebf' `"carryover.within.obs.window = "TRUE""' _n
	if !mi("`carryover_into_ow'")           file write `filebf' `"carryover.into.obs.window = "TRUE""' _n
	if !mi("`keep_window_start_end_dates'") file write `filebf' `"keep.window.start.end.dates = "TRUE""' _n
	if !mi("`keep_events_outside_fuw'")     file write `filebf' `"remove.events.outside.followup.window = "FALSE""' _n
	if !mi("`keep_ev_int_for_all_events'")  file write `filebf' `"keep.event.interval.for.all.events = "TRUE""' _n
	if !mi("`ev_int_col'")                  file write `filebf' `"event.interval.colname = "`ev_int_col'""' _n
	if !mi("`gap_days_col'")                file write `filebf' `"gap.days.colname = "`gap_days_col'""' _n
	
	* special parameters:
	file write `filebf' `"NA.SYMBOL.NUMERIC = ".""' _n
	file write `filebf' `"NA.SYMBOL.STRING = """' _n
	file write `filebf' `"LOGICAL.SYMBOL.TRUE = "1""' _n
	file write `filebf' `"LOGICAL.SYMBOL.FALSE = "0""' _n
	file write `filebf' `"COLNAMES.DOT.SYMBOL = "_""' _n
	file write `filebf' `"COLNAMES.START.DOT = "internal_""' _n
	
	* make sure everything is done serially:
	file write `filebf' `"parallel.backend = "none""' _n
	file write `filebf' `"parallel.threads = "1""' _n
	
	* end the params file:
	file write `filebf' `"end_parameters"' _n
	
	* close the file:
	qui file close `filebf'
	
	* make a copy of the data to send to AdhereR (either the given file or the current dataset):
	if mi("`using'") {
		* save the original dataset as CSV in the given location:
		quiet restore, preserve /* make sure we restore the original dataset first (and preserve it again) */
		quiet export delimited "`adherer_input_csv'", delimiter(tab) replace
	}
	else {
		* make a copy of the input file to the given location:
		quiet confirm file "`using'"
		quiet copy "`using'" "`adherer_input_csv'", replace
	}
	if "`verbose'"!="" display "CSV data for AdhereR is in file `adherer_input_csv'"
	
	* 4. Build the skeleton script to call AdhereR:
	qui file open `filebf' using "`adherer_script'", write text replace
	if mi("`folder_to_use'") {
		* we're using STATA's temporary directory
		file write `filebf' `"args <- commandArgs(trailingOnly = "TRUE");"' _n
		file write `filebf' `"if(length(args) != 3) stop("There must be exactly 3 arguments");"' _n
		file write `filebf' `"data_dir <- dirname(args[1]); # the STATA temporary folder"' _n
		file write `filebf' `"if(file.exists(paste0(data_dir,"/parameters.log"))) invisible(file.remove(paste0(data_dir,"/parameters.log")));"' _n
		file write `filebf' `"invisible(file.rename(args[1], paste0(data_dir,"/parameters.log"))); # make sure the params file is called parameters.log"' _n
		file write `filebf' `"if(file.exists(paste0(data_dir,"/dataset.csv"))) invisible(file.remove(paste0(data_dir,"/dataset.csv")));"' _n
		file write `filebf' `"invisible(file.rename(args[2], paste0(data_dir,"/dataset.csv"))); # make sure the data file is called dataset.csv"' _n
		file write `filebf' `"if(file.exists(paste0(data_dir,"/Adherer-results.txt"))) invisible(file.remove(paste0(data_dir,"/Adherer-results.txt"))); # remove Adherer-results.txt"' _n
		file write `filebf' `"write.csv(data.frame('results_dir'=data_dir), file=args[3], row.names=FALSE); # store the directory with the results"' _n
	}
	else {
		* we're using the user-supplied directory
		file write `filebf' `"args <- commandArgs(trailingOnly = "TRUE");"' _n
		file write `filebf' `"if(length(args) != 1) stop("There must be exactly 1 argument");"' _n
		file write `filebf' `"data_dir <- args[1]; # the user-supplied data exchange folder"' _n
		file write `filebf' `"if( !dir.exists(data_dir) ) stop(paste0("The data exchange folder ",data_dir," does not seem to exist!"));"' _n
		file write `filebf' `"if(file.exists(paste0(data_dir,"/Adherer-results.txt"))) invisible(file.remove(paste0(data_dir,"/Adherer-results.txt"))); # remove Adherer-results.txt"' _n
	}
	file write `filebf' `"# Call AdhereR..."' _n
	file write `filebf' `"library("`R_PACKAGE_NAME'");"' _n
	file write `filebf' `"`R_PACKAGE_CALL_FUNCTION'(data_dir);"' _n
	qui file close `filebf'
	
	* 5. AdhereR:
	if "`verbose'"!="" { 
		if mi("`folder_to_use'") {
			rscript_adh using "`adherer_script'", args("`adherer_input_args'" "`adherer_input_csv'" "`adherer_output'") verbose
		}
		else {
			rscript_adh using "`adherer_script'", args("`folder_to_use'") verbose
		}
	} 
	else {
		if mi("`folder_to_use'") {
			rscript_adh using "`adherer_script'", args("`adherer_input_args'" "`adherer_input_csv'" "`adherer_output'") 
		}
		else {
			rscript_adh using "`adherer_script'", args("`folder_to_use'")
		}
	}
	if mi("`folder_to_use'") {
		import delimited using "`adherer_output'", varnames(1) delimiter(tab) clear
		quiet ds
		if r(varlist) != "results_dir" {
			di as error "There's something wrong with R and AdhereR: please check if it is correctly installed and that STATA is using the crrect version!"
			exit 1
		}
		local resdir = results_dir[1]
	}
	else {
		local resdir = "`folder_to_use'"
	}
	
	* 6. check AdhereR's output:
	quiet import delimited using "`resdir'/Adherer-results.txt", delimiter(tab) clear
	quiet ds
	if r(varlist) != "v1" {
		di as error "There's something wrong with R and AdhereR: please check if it is correctly installed and that STATA is using the crrect version!"
		exit 1
	}
	if _N != 2 | v1[2] != "OK: the results were exported successfully (but there might be warnings and messages above worth paying attention to)!"  {
		di as error "There seems to have been an error calling AdhereR: please check the output above to identify it..."
		exit 1
	}
	
	
	* 7. if so desired, import the results:
	if mi("`dont_load_results'") & lower("`cma'") != "plot_interactive_cma" {
		* discard the previous dataset:
		restore , not 
		* load adherer's results:
		if mi("`plot'") {
			if lower("`cma'") == "compute_event_int_gaps" {
				quiet import delimited using "`resdir'/EVENTINFO.csv", varnames(1) delimiter(tab) clear
			}
			else if lower("`cma'") == "compute_treatment_episodes" {
				quiet import delimited using "`resdir'/TREATMENTEPISODES.csv", varnames(1) delimiter(tab) clear
			}
			else {
				quiet import delimited using "`resdir'/CMA.csv", varnames(1) delimiter(tab) clear
			}
		}
		else {
			quiet import delimited using "`resdir'/CMA-plotted.csv", varnames(1) delimiter(tab) clear
		}
		quiet ds
		* let the user know:
		if lower("`cma'") == "compute_event_int_gaps" {
			display "Apprently, all went well: the gap days and event info data should have been loaded in STATA as a dataset with " _N " observations and columns: " r(varlist) " :"
		}
		else if lower("`cma'") == "compute_treatment_episodes" {
			display "Apprently, all went well: the treatment episodes data should have been loaded in STATA as a dataset with " _N " observations and columns: " r(varlist) " :"
		}
		else {
			display "Apprently, all went well: the estimated CMAs should have been loaded in STATA as a dataset with " _N " observations and columns: " r(varlist) " :"
		}
		describe
	}
	else {
		* restore the previous dataset (should be done automatically...):
		quiet restore
		* let the user know that the data is on the disk:
		if lower("`cma'") == "plot_interactive_cma" {
			display `"The interactive plotting should have finished and there are no data to load (unless you explicitely saved some from the interactive interface)..."'
		}
		else if mi("`plot'") {
			if lower("`cma'") == "compute_event_int_gaps" {
				display `"Apprently, all went well: however, the estimated gap days and event info data were not loaded in STATA but can be found on disk in the file "`resdir'/EVENTINFO.csv" (in fact, the folder "`resdir'" should contain the input data, the parameters and script used to invoke AdhereR in R and possibly other types of output (including any plots!), depending on what you aksed for)."'
			}
			else if lower("`cma'") == "compute_treatment_episodes" {
				display `"Apprently, all went well: however, the treatment episodes data were not loaded in STATA but can be found on disk in the file "`resdir'/TREATMENTEPISODES.csv" (in fact, the folder "`resdir'" should contain the input data, the parameters and script used to invoke AdhereR in R and possibly other types of output (including any plots!), depending on what you aksed for)."'
			}
			else {
				display `"Apprently, all went well: however, the estimated CMAs were not loaded in STATA but can be found on disk in the file "`resdir'/CMA.csv" (in fact, the folder "`resdir'" should contain the input data, the parameters and script used to invoke AdhereR in R and possibly other types of output (including any plots!), depending on what you aksed for)."'
			}
		}
		else {
			display `"Apprently, all went well: however, the estimated CMAs were not loaded in STATA but can be found on disk in the file "`resdir'/CMA-plotted.csv" (in fact, the folder "`resdir'" should contain the input data, the parameters and script used to invoke AdhereR in R and possibly other types of output (including any plots!), depending on what you aksed for)."'
		}
	}
	
	* 9. return the path to the folder with the results and end the progam:
	return local adherer_resdir "`resdir'"
end




* This is basiclly the original code of rscript slightly changed to avoid conflicts 
* if the user has also installed it....

* rscript 1.1 3aug2021 by David Molitor and Julian Reif
* 1.1:   added rversion() and require() options. fixed text output when using RSCRIPT_PATH
* 1.0.4: added default pathname
* 1.0.3: added support for "~" pathnames
* 1.0.2: stderr is now parsed by Mata instead of Stata
* 1.0.1: updated error handling

* DD: add a "verbose" parameter for showing R version and R output or not

program define rscript_adh, rclass

	version 13.0

	tempfile shell out err tmpfile_require rversion_control_script
	tempname shellfile tmpname_require

	syntax [using/], [rpath(string) args(string asis) rversion(string) verbose require(string asis) force]
	
	************************************************
	* Error checking
	************************************************
  
	* rscript does not work in batch mode on Stata for Windows because Stata ignores shell requests (as of Stata 17.0)
	if "`c(os)'" == "Windows" & "`c(mode)'" == "batch" {
		di as error "rscript does not work in batch mode on Stata for Windows because Stata ignores shell requests in this setting"
		exit 1
	}
  
	* User must specify a filename, unless rversion() was specified
	if mi(`"`rversion'`require'"') & mi("`using'") {
		di as error "using required"
		exit 100
	}
	if !mi("`using'") confirm file "`using'"
	
	* If user does not specify the location of the R executable, set the default to what is stored in RSCRIPT_PATH
	* If both are blank, then try using an os-specific default
	if mi(`"`rpath'"') {
		local rpath `"$RSCRIPT_PATH_ADHERER"'
		
		if mi(`"`rpath'"') {
			
			local os = lower("`c(os)'")
			
			* Unix/mac default paths: (1) /usr/local/bin/Rscript (2) /usr/bin/Rscript
			if inlist("`os'","macosx","unix") {
				local rpath "/usr/local/bin/Rscript"
				cap confirm file "`rpath'"
				if _rc local rpath "/usr/bin/Rscript"
				cap confirm file "`rpath'"
				if _rc local rpath 
			}
			
			* Windows default path: "C:/Program Files/R/R-X.Y.Z/bin/Rscript.exe" (newest version)
			else if "`os'" == "windows" {
				local subdirs : dir "C:/Program Files/R/" dirs "R-?.?.?", respectcase
				local subdirs : list clean subdirs
				local subdirs : list sort subdirs
				local ndirs   : list sizeof subdirs
				if `ndirs' > 0 {
					local newest  : word `ndirs' of `subdirs'
					local rpath "C:/Program Files/R/`newest'/bin/Rscript.exe"
				}
			}
			
			if mi(`"`rpath'"') {
				di as error "No default R executable found. Specify R executable using option rpath() or using the global RSCRIPT_PATH"
				exit 198	
			}
			if "`verbose'"!="" { 
				di as result `"Using R at location: `rpath'"'
			}
		}
	}
	
	cap confirm file "`rpath'"
	if _rc {
		di as error "R executable not found. Specify R executable using option rpath() or using the global RSCRIPT_PATH"
		exit 601		
	}
	
	* Calling a script using "~" notation causes a fatal error with shell (Unix/Mac). Avoid by converting to absolute path.
	qui if strpos("`using'","~") {
		mata: pathsplit(st_local("using"), path = "", fname = "")
		mata: st_local("path", path)
		mata: st_local("fname", fname)
		
		local workdir_orig "`c(pwd)'"
		cd `"`path'"'
		local using "`c(pwd)'/`fname'"
		cd "`workdir_orig'"
		confirm file "`using'"
	}
	
	* Do basic QC to help ensure valid version numbers were specified in rversion():
	*  (1) Check that no more than 2 version numbers were passed
	*  (2) ".." is not allowable syntax in R
	*  (3) R does not allow version numbers to end in "."
	*  (4) some additional checks by regex
	if !mi(`"`rversion'"') {
		if wordcount(`"`rversion'"')>2 {
			di as error "rversion() invalid -- too many elements"
			exit 198
		}
		
		tokenize `"`rversion'"'
		while !mi(`"`1'"') {
			if strpos(`"`1'"',"..") {
				di as error `"rversion() invalid: `1' is invalid version number"'
				exit 198
			}
			
			if substr(trim(`"`1'"'),-1,1)=="." {
				di as error `"rversion() invalid: `1' is invalid version number (cannot end in '.')"'
				exit 198
			}			
			
			* The following regex:
			*   (1) allows trailing and leading spaces
			*   (2) requires first char to be a digit (requirement by R when checking string version numbers)
			*   (3) allows remainder to be an arbitrary number of digits and decimals
			if !regexm(`"`1'"',"^[ ]*[0-9]+[.0-9]* *$") {
				di as error `"rversion() invalid: `1' is invalid version number"'
				exit 198					
			}			
			macro shift
		}
		
		* Second argument to rversion is optional; set to -1 if not specified
		if wordcount(`"`rversion'"')==1 local rversion `"`rversion' -1"'
	}
	
	* Ensure no quotation marks passed to require()
	if !mi(`"`require'"') {
		
		tokenize `"`require'"'
		while !mi(`"`1'"') {
			if strpos(`"`1'"',`"""') {
				di as error "require() invalid: embedded quotation marks not allowed"
				exit 198
			}			
			macro shift
		}
	}
	
	
	************************************************
	* Detect shell version
	************************************************	
	* Syntax for the -shell- call depends on which version of the shell is running:
	*	Unix csh:  /bin/csh
	*	Unix tcsh: /usr/local/bin/tcsh (default on NBER server)
	*	Unix bash: /bin/bash
	*	Windows
	qui shell echo "$0" > `shell'
	file open `shellfile' using `"`shell'"', read
	file read `shellfile' shellline
	file close `shellfile'

	************************************************
	* Version control: rversion() and/or require() options. Redirect stdout to `out' and stderr to `err'
	************************************************
	if !mi(`"`rversion'`require'"') {
		
		* If rversion() not specified, set to default values of -1
		if mi(`"`rversion'"') local rversion "-1 -1"
		
		* If require() specified, write out list of packages to file
		if !mi(`"`require'"') {
			
			qui file open `tmpname_require' using `"`tmpfile_require'"', write text replace
			
			tokenize `"`require'"'
			while !mi(`"`1'"') {
				file write `tmpname_require' `"`1'"' _n	
				macro shift
			}
			qui file close `tmpname_require'
			
			local arg_require "`tmpfile_require'"
		}
		
		* Create an R script that will be used to check R version and/or installed packages
		qui write_r_script_adh `rversion_control_script'
		
		* Call that R script. Note: shell call differs for csh/bash/other (windows is "other")
		if strpos("`shellline'", "csh") {	
			qui shell ("`rpath'" "`rversion_control_script'" `rversion' `arg_require' > `out') >& `err'
		}

		else if strpos("`shellline'", "bash") {
			qui shell "`rpath'" "`rversion_control_script'" `rversion' `arg_require' > `out' 2>`err'
		}

		else {
			qui shell "`rpath'" "`rversion_control_script'" `rversion' `arg_require' > `out' 2>`err'
		}
		
		* Report output from version control script call
		di as result "Version information:"
		type `"`out'"'
		type `"`err'"'
		
		cap mata: parse_stderr_version_control_adh("`err'")
		if _rc==1 {
			di as error "This R installation does not meet the version requirements specified in rversion()"
			di as error _skip(2) `"You can download the version you need by visiting {browse "https://www.r-project.org"}"'
			error 9
		}
		else if _rc==2 {
			di as error "This R installation is missing packages specified in require()"
			di as error _skip(2) `"Packages can usually be installed by typing install.packages("X") at the R prompt, where X is the name of the package"'
			error 9
		}
		else if _rc==198 {
			display as error _n "Version and package requirement checks ended with an error in R"
			display as error "See stderr output above for details"
			if "`force'"=="" error 198
		}			
		else if _rc {
			di as error "Encountered a problem while parsing stderr"
			di as error "Mata error code: " _rc
		}
		if !mi("`using'") di ""
	}
	
	************************************************
	* Run the script specified by user. Redirect stdout to `out' and stderr to `err'
	************************************************
	if !mi("`using'") {
		
		if "`verbose'"!="" { 
			di as result `"Running R script: `using'"'
			if !mi(`"`args'"') di as result `"Args: `args'"'	
		}
		
		* shell call differs for csh/bash/other (windows is "other")
		if strpos("`shellline'", "csh") {	
			shell ("`rpath'" "`using'" `args' > `out') >& `err'
		}
		
		else if strpos("`shellline'", "bash") {
			shell "`rpath'" "`using'" `args' > `out' 2>`err'
		}
		
		else {
			shell "`rpath'" "`using'" `args' > `out' 2>`err'
		}
		
		return local rpath `rpath'
		
		************************************************
		* Display stdout and stderr output
		************************************************
		if "`verbose'"!="" { 
			di as result "Begin R output:"
			di as result "`="_"*80'"
			
			di as result "{ul:stdout}:"
			type `"`out'"'
			di as result _n
			di as result "{ul:stderr}:"
			type `"`err'"'
			
			di as result "`="_"*80'"
			di as result "...end R output"
		}
		
		
		************************************************
		* If there was an "error" in the execution of the R script, notify the user (and break, unless -force- option is specified)
		************************************************
		cap mata: parse_stderr_adh("`err'")
		if _rc==198 {
			display as error _n "`using' ended with an error"
			display as error "See stderr output above for details"
			if "`force'"=="" error 198
		}
		else if _rc {
			display as error _n "Encountered a problem while parsing stderr"
			display as error "Mata error code: " _rc
		}
		
		* In a few (rare) cases, a "fatal error" message will be written to stdout rather than stderr
		cap mata: parse_stdout_adh("`out'")
		if _rc==198 {
			display as error _n "`using' ended with a fatal error"
			display as error "See stdout output above for details"
			if "`force'"=="" error 198
		}
		else if _rc {
			display as error _n "Encountered a problem while parsing stdout"
			display as error "Mata error code: " _rc
		}
	}
end

********************************
* AUXILIARY FUNCTIONS
********************************

***
* write_script writes out an R script that checks the version of base R and confirms package installations 
***

* The program write_script expects one argument: the name of the file being written

* The R script that is written accepts three arguments:
*  (1) rmin (default, '-1', causes script to ignore enforcmement of minimum version)
*  (2) rmax (default, '-1', causes script to ignore enforcmement of maximum version)
*  (3) filename containing list of package names (optional)

program define write_r_script_adh

	tempname filebf

	***
	* Write file to the first argument passed to write_script
	***
	qui file open `filebf' using "`1'", write text replace
	
	***
	* R script contents
	***
			
	* Parse arguments. Third argument (list of package names) is optional and referenced later on
	file write `filebf' `"args = commandArgs(trailingOnly = "TRUE")"' _n
	file write `filebf' `"rmin <- args[1]"' _n
	file write `filebf' `"rmax <- args[2]"' _n _n
	
	* Report curent version
	file write `filebf' `"rcurrent <- packageVersion("base")"' _n
	file write `filebf' `"print(paste("R installation is version", rcurrent))"' _n _n
	
	* Enforce minimum version requirements (if specified)
	file write `filebf' `"if (rmin != '-1') {"' _n
	file write `filebf' `"  if (rcurrent < rmin) {"' _n
	file write `filebf' `"    vers_ex_msg = paste0("This R installation is older than version ", rmin)"' _n
	file write `filebf' `"    stop(vers_ex_msg)"' _n
	file write `filebf' `"  }"' _n
	file write `filebf' `"}"' _n _n
	
	* Enforce maximum version requirements (if specified)
	file write `filebf' `"if (rmax != '-1') {"' _n
	file write `filebf' `"  if (rcurrent > rmax ) {"' _n
	file write `filebf' `"    vers_ex_msg = paste0("This R installation is newer than version ", rmax)"' _n
	file write `filebf' `"    stop(vers_ex_msg)"' _n
	file write `filebf' `"  }"' _n
	file write `filebf' `"}"' _n _n

	* If arg[3] (filename) was specified, read the file and check whether those packages were installed
	file write `filebf' `"if(length(args)==3) {"' _n
	file write `filebf' `"  packages <-  as.character(read.csv(args[3],header = FALSE)\$V1)"' _n
	file write `filebf' `"  installed <- packages %in% installed.packages()[, "Package"]"' _n
	file write `filebf' `"  if(any(!installed)) {"' _n
	file write `filebf' `"    vers_ex_msg = paste0("The following packages are not installed:\n  ", paste(packages[!installed],collapse="\n  "))"' _n
	file write `filebf' `"    stop(vers_ex_msg)"' _n
	file write `filebf' `"  }"' _n
	file write `filebf' `"}"' _n
	
	***
	* File close
	***	
	qui file close `filebf'

end

*********
* Mata functions used to parse the stderr and stdout output files to check for errors
*********
mata: mata clear

// Parser for the stderr file created by rversion() and require() options
mata:
void parse_stderr_version_control_adh(string scalar filename)
{
	real scalar input_fh
	string scalar line

	input_fh = fopen(filename, "r")
	
	while ((line=fget(input_fh)) != J(0,0,"")) {
		if (strpos(strlower(line), "error: this r installation is")!=0) exit(error(1))
		if (strpos(strlower(line), "error: the following packages are not installed")!=0) exit(error(2))
		if (strpos(strlower(line), "error")!=0) exit(error(198))
	}
	
	fclose(input_fh)
}

// Parsers for the stderr and stdout files created when running the R script specified by the user
void parse_stderr_adh(string scalar filename)
{
	real scalar input_fh
	string scalar line

	input_fh = fopen(filename, "r")
	
	while ((line=fget(input_fh)) != J(0,0,"")) {
		if (strpos(strlower(line), "error")!=0) exit(error(198))
	}
	
	fclose(input_fh)
}

void parse_stdout_adh(string scalar filename)
{
	real scalar input_fh
	string scalar line

	input_fh = fopen(filename, "r")
	
	while ((line=fget(input_fh)) != J(0,0,"")) {
		if (strpos(strlower(line), "fatal error")!=0) exit(error(198))
	}
	
	fclose(input_fh)
}

end
** EOF
