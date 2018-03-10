#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar  4 19:23:21 2018

@author: ddediu
"""

import pandas, warnings, subprocess, os, numbers, datetime, PIL

# Load the test dataset
df = pandas.read_csv('./test-dataset.csv', sep='\t', header=0)

# Change the column names:
df.rename(columns={'ID': 'patientID', 
                   'DATE': 'prescriptionDate', 
                   'PERDAY': 'quantityPerDay', 
                   'CLASS': 'medicationType', 
                   'DURATION': 'prescriptionDuration'}, 
inplace=True)




def call_adhereR(dataset,
                function,
                ID_colname,
                event_date_colname,
                event_duration_colname,
                followup_window_start_type = 'numeric',
                followup_window_start = 0,
                followup_window_start_unit = "days",
                followup_window_duration_type = 'numeric',
                followup_window_duration = 365*2,
                followup_window_duration_unit = "days",
                observation_window_start_type = 'numeric',
                observation_window_start = 0,
                observation_window_start_unit = "days",
                observation_window_duration_type = 'numeric',
                observation_window_duration = 365*2,
                observation_window_duration_unit = "days",
                date_format = "%m/%d/%Y",
                event_interval_colname = 'event.interval',
                gap_days_colname = 'gap.days',
                force_NA_CMA_for_failed_patients = True,
                parallel_backend = 'none',
                parallel_threads = 'auto',
                suppress_warnings = False,
                save_event_info = False,
                NA_symbol_numeric = 'NA',
                NA_symbol_string = 'NA',
                logical_symbol_TRUE = 'TRUE',
                logical_symbol_FALSE = 'FALSE',
                colnames_dot_symbol = '.',
                colnames_start_dot = '.',
                plot_show = False,
                plot_save_to = None,
                plot_save_as = 'jpg',
                plot_width = 7, 
                plot_height = 7,
                plot_quality = 90,
                plot_dpi = 150,
                plot_patients_to_plot = None,
                plot_duration = None,
                plot_align_all_patients = False,
                plot_align_first_event_at_zero = True,
                plot_show_period = 'days',
                plot_period_in_days = 90,
                plot_show_legend = True,
                plot_legend_x = 'right',
                plot_legend_y = 'bottom',
                plot_legend_bkg_opacity = 0.5,
                plot_cex = 1.0,
                plot_cex_axis = 0.75,
                plot_cex_lab = 1.0,
                plot_show_cma = True,
                plot_unspecified_category_label = 'drug',
                plot_lty_event = 'solid',
                plot_lwd_event = 2,
                plot_pch_start_event = 15,
                plot_pch_end_event = 16,
                plot_show_event_intervals = True,
                plot_col_na = 'lightgray',
                plot_col_continuation = 'black',
                plot_lty_continuation = 'dotted',
                plot_lwd_continuation = 1,
                plot_print_CMA = True,
                plot_plot_CMA = True,
                plot_plot_CMA_as_histogram = True,
                plot_CMA_plot_ratio = 0.10,
                plot_CMA_plot_col = 'lightgreen',
                plot_CMA_plot_border = 'darkgreen',
                plot_CMA_plot_bkg = 'aquamarine',
                plot_CMA_plot_text = None,
                plot_highlight_followup_window = True,
                plot_followup_window_col = 'green',
                plot_highlight_observation_window = True,
                plot_observation_window_col = 'yellow',
                plot_observation_window_density = 35,
                plot_observation_window_angle = -30,
                plot_show_real_obs_window_start = True,
                plot_real_obs_window_density = 35,
                plot_real_obs_window_angle = 30,
                plot_bw_plot = False,
                path_to_Rscript = '/usr/local/bin/Rscript',
                path_to_adherer = os.getcwd(),
                path_to_data_directory = os.getcwd(),
                print_adherer_messages = True
                ):
    """
    Call AdhereR.

    Call various functions exported by the R package AdhereR.
    Uses the 'shell' mechanism for portability and generality.

    Parameters
    ----------
    dataset : pandas.DataFrame
        The dataset on which AdhereR will work
    function : str
        The name of the AdhereR function to call
    ID_colname : str
        The name of the column in dataset containing the patient IDs
    event_date_colname : str
        The name of the column in dataset containing the event dates
    event_duration_colname : str
        The name of the column in dataset containing the event duration
    followup_window_start_type : str
        The follow-up window start unit; can be 'numeric' (default), 'character' or 'date'
    followup_window_start : numeric, str, or date
        The follow-up window start; can be a number, a string or a date
    followup_window_start_unit : str
        The follow-up window start unit; can be 'days' (default), 'weeks', 'months' or 'years'
    followup_window_duration_type : str
        The follow-up window duration unit; can be 'numeric' (default), 'character' or 'date'
    followup_window_duration : numeric, str, or date
        The follow-up window duration; can be a number, a string or a date
    followup_window_duration_unit : str
        The follow-up window duration unit; can be 'days' (default), 'weeks', 'months' or 'years'
    observation_window_start_type : str
        The observation window start unit; can be 'numeric' (default), 'character' or 'date'
    observation_window_start : numeric, str, or date
        The observation window start; can be a number, a string or a date
    observation_window_start_unit : str
        The observation window start unit; can be 'days' (default), 'weeks', 'months' or 'years'
    observation_window_duration_type : str
        The observation window duration unit; can be 'numeric' (default), 'character' or 'date'
    observation_window_duration : numeric, str, or date
        The observation window duration; can be a number, a string or a date
    observation_window_duration_unit : str
        The observation window duration unit; can be 'days' (default), 'weeks', 'months' or 'years'
    date_format : str
        The date format to be used throughout the call (in the standard strftime() format)
    event_interval_colname : str
        What name to use for the internal column saving the event intervals (defaults to 'event.interval')
    gap_days_colname : str
        What name to use for the internal column saving the gap days (defaults to 'gap.days')
    force_NA_CMA_for_failed_patients : bool
        Force the patients that failed to missing CMA? (default to 'True')
    parallel_backend : str
        The parallel backend to use; can be 'none', 'multicore', 'snow', 'snow(SOCK)', 'snow(MPI)', 'snow(NWS)' (defaults to 'none')
    parallel_threads : numeric or str
        Specification of the number of parallel threads; can be an actual number, 'auto' or a more complex list of nodes (defaults to 'auto')
    suppress_warnings : bool
        Suppress the warnings produced by AdhereR? (default to False)
    save_event_info : bool
        Should the EVENTINFO be also saved?
    NA_symbol_numeric : str
        The symbol used for missing data in numeric columns (defaults to 'NA')
    NA_symbol_string : str
        The symbol used for missing data in string columns (defaults to 'NA')
    logical_symbol_TRUE : str 
        The symbol used for logical true (defaults to 'TRUE')
    logical_symbol_FALSE : str
        The symbol used for logical true (defaults to 'FALSE')
    colnames_dot_symbol : str
        What symbol to replace the '.' in column names with (defaults to '.', i.e., no replacement)
    colnames_start_dot : str
        What symbol to replace the '.' begining a column names with (defaults to '.', i.e., no replacement)
    plot_show : bool
        Do the plotting? If true, also save the resulting dataset with a "-plotted" suffix to avoid overwriting previous results (defaults to False)
    plot_save_to : str
        The folder where to save the plots (defaults to None, i.e. same folder as the other results)
    plot_save_as : str
        The format of the saved plot; can be 'jpg', 'png', 'tiff', 'eps' or 'pdf' (defaults to 'jpg')
    plot_width : numeric
        Plot width in inches (defaults to 7)
    plot_height : numeric
        Plot heght in inches (defaults to 7)
    plot_quality : numeric
        Plot quality (applies only to certain formts; defaults to 90)
    plot_dpi : numeric
        Plot resultion (applies only to certain formts; defaults to 150)
    plot_patients_to_plot : strings
        The patient IDs to plot as a vector (defaults to None, i.e., all)
    plot_duration : numeric
        Duration to plot in days (defaults to None, i.e., is determined from the data)
    plot_align_all_patients : bool
        Alling all patients? (defaults to False)
    plot_align_first_event_at_zero : bool
        If plot_align_all_patients == True, also place the event at the origin? (defaults to True)
    plot_show_period : str
        Draw vertical bars at regular interval as dates or days; can be 'days' or 'dates' (defaults to 'days')
    plot_period_in_days : numeric
        The interval (in days) at which to draw vertical guides (defaults to 90)
    plot_show_legend : bool
        Show the legend? (defaults to True)
    plot_legend_x : str or numeric
        Together with plot_legend_y specifies the position of the legend; can be 'left' or 'right' or a number; (defaults to 'right')
    plot_legend_y : str or numeric
        Together with plot_legend_x specifies the position of the legend; can be 'bottom' or 'top' or a number; (defaults to 'bottom')
    plot_legend_bkg_opacity : numeric
        The legend background opacity (between 0 and 1, defaults to 0.5)
    plot_cex : numeric
        The relative text size (defaults to 1.0)
    plot_cex_axis : numeric
        The relative axis text size (defaults to 0.75)
    plot_cex_lab : numeric
        The relative labels text size (defaults to 1.0)
    plot_show_cma : bool
        Show the CMA type? (defaults to True)
    plot_unspecified_category_label : str
        The label of the unspecified category of medication (defaults to 'drug')
    plot_lty_event : str
        Line style for plotting events; can be 'solid', 'dotted' or 'dashed' (defaults to 'solid')
    plot_lwd_event : numeric
        Line width for plitting events (defaults to 2)
    plot_pch_start_event : numeric
        Symbol for the event start; can be any of the R plotting symbols given at, for example, http://www.endmemo.com/program/R/pchsymbols.php (defaults to 15)
    plot_pch_end_event : numeric
        Symbol for event end (see plot_pch_start_event for details; defaults to 16)
    plot_show_event_intervals : bool
        Show the prescription intervals? (defaults to True)
    plot_col_na : str
        The color of the missing data; can be any R color specification as, for example, given at http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf (defaults to 'lightgray')
    plot_col_continuation : str
        The color of the lines connections consecutive events (see plot_col_na for details; defaults to 'black')
    plot_lty_continuation : str
        Style of the lines connections consecutive events (see plot_lty_event for details; defaults to 'dotted')
    plot_lwd_continuation : numeric
        Line width for plitting events (defaults to 1)
    plot_print_CMA : bool
        Print CMA value next to the participant's ID? (defaults to True)
    plot_plot_CMA : bool
        Plot the CMA next to the participant ID? (defaults to True)
    plot_plot_CMA_as_histogram : bool
        Plot CMA as a histogram or as a density plot? (defaults to True)
    plot_CMA_plot_ratio : numeric
        The proportion of the total horizontal plot to be taken by the CMA plot (defaults to 0.10)
    plot_CMA_plot_col : str
        The color of the CMA plot (see plot_col_na for details; defaults to 'lightgreen')
    plot_CMA_plot_border : str
        The color of the CMA border (see plot_col_na for details; defaults to 'darkgreen')
    plot_CMA_plot_bkg : str
        The color of the CMA background (see plot_col_na for details; defaults to 'darkgreen')
    plot_CMA_plot_text : str
        The color of the CMA text (see plot_col_na for details; defaults to None, i.e., the same as plot_CMA_plot_border)
    plot_highlight_followup_window : bool
        Highlight the follow-up window? (defaults to True)
    plot_followup_window_col : str
        The color of the CMA follow-up window (see plot_col_na for details; defaults to 'green')
    plot_highlight_observation_window : bool
        Highlight the observaion window? (defaults to True)
    plot_observation_window_col : str
        The color of the CMA observation window (see plot_col_na for details; defaults to 'yellow')
    plot_observation_window_density : numeric
        The density (per inch) of the hash lines marking the obervation window (defaults to 35)
    plot_observation_window_angle : numeric
        The angle (in degrees) of the hash lines marking the obervation window (defaults to -30)
    plot_show_real_obs_window_start : bool
        For some CMAs, the real observation window starts at a different date: should we show it? (defaults to True)
    plot_real_obs_window_density : numeric
        Same as plot_observation_window_density (defaults to 35)
    plot_real_obs_window_angle : numeric
        Same as plot_observation_window_angle (defaults to 30)
    plot_bw_plot : bool
        If True, override all user-given colors and replace them with a scheme suitable for grayscale plotting (fedaults to False)
    path_to_Rscript : str
        The path to where Rscript is installed
    path_to_adherer : str
        The path to where the callAdhereR.R script is (defaults to the current folder)
    path_to_data_directory : str
        The path to the directory where the various data should be saved (defaults to the current folder)
    print_adherer_messages : bool
        Print the AdhereR message (on top of returning them to the caller)?

    Returns
    -------
    Dictionary
        If a serious error has occured before being able to call AdhereR, returns None. 
        Otherwise returns a dictionary containing various keys appropriate to the called function, as follows:
        - all: 
            - return_code: numeric code returned by the shell call to AdhereR (0 = OK)
            - message: the string message returned by AdhereR (if any)
        - CMA1 .. CMA9, CMA_per_episode, CMA_sliding_window also return:
            - CMA: a pandas.Dataframe containing the computed CMAs
            - EVENTINFO: if explicitely requested (save_event_info == True), a pandas.Dataframe containing the event intervals and gaps

    """    
    # Check that dataset is of the right type and contains the required columns:
    if not isinstance(dataset, pandas.DataFrame):
        warnings.warn('adhereR: argument "dataset" must be a pandas DataFrame (or compatible).')
        return None;
    if not function in ('CMA1', 'CMA2', 'CMA3', 'CMA4'):
        warnings.warn('adhereR: argument "function" (' + function + ') is not a known adhereR function".')
        return None;
    if not ID_colname in df.columns.values.tolist():
        warnings.warn('adhereR: argument "ID_colname" (' + ID_colname + ') must a column in "dataset".')
        return None;
    if not event_date_colname in df.columns.values.tolist():
        warnings.warn('adhereR: argument "event_date_colname" (' + event_date_colname + ') must a column in "dataset".')
        return None;
    if not event_duration_colname in df.columns.values.tolist():
        warnings.warn('adhereR: argument "event_duration_colname" (' + event_duration_colname + ') must a column in "dataset".')
        return None;
    
    # Export the dataset:
    df.to_csv('./dataset.csv', sep='\t', na_rep='NA', header=True, index=False)
    
    # Create the parameters.log file:
    parameters_file = open ('./parameters.log', 'w+')
    # Write the parameters header:
    parameters_file.write('Parameters\n')
    
    # Write the parameters:
    
    # The function to call:
    parameters_file.write('function = "' + function + '"\n')
    
    
    # Required column names:
    parameters_file.write('ID.colname = "' + ID_colname + '"\n')
    parameters_file.write('event.date.colname = "' + event_date_colname + '"\n')
    parameters_file.write('event.duration.colname = "' + event_duration_colname + '"\n')


    # Follow-up window:
    if not followup_window_start_type in ('numeric', 'character', 'date'):
        warnings.warn('adhereR: argument "followup_window_start_type" (' + followup_window_start_type + ') is not recognized.')
        parameters_file.close()
        return None;
    parameters_file.write('followup.window.start.type = "' + followup_window_start_type + '"\n')
    
    if isinstance(followup_window_start, numbers.Number):
        parameters_file.write('followup.window.start = "' + str(followup_window_start) + '"\n')
    elif isinstance(followup_window_start, datetime.date) or isinstance(followup_window_start, datetime.datetime):
        parameters_file.write('followup.window.start = "' + followup_window_start.strftime(date_format) + '"\n')
    else:
        parameters_file.write('followup.window.start = "' + followup_window_start + '"\n')
    
    if not followup_window_start_unit in ('days', 'weeks', 'months', 'years'):
        warnings.warn('adhereR: argument "followup_window_start_unit" (' + followup_window_start_unit + ') is not recognized.')
        parameters_file.close()
        return None;
    parameters_file.write('followup.window.start.unit = "' + followup_window_start_unit + '"\n')


    if not followup_window_duration_type in ('numeric', 'character', 'date'):
        warnings.warn('adhereR: argument "followup_window_duration_type" (' + followup_window_duration_type + ') is not recognized.')
        parameters_file.close()
        return None;
    parameters_file.write('followup.window.duration.type = "' + followup_window_duration_type + '"\n')
    
    if isinstance(followup_window_duration, numbers.Number):
        parameters_file.write('followup.window.duration = "' + str(followup_window_duration) + '"\n')
    elif isinstance(followup_window_duration, datetime.date) or isinstance(followup_window_duration, datetime.datetime):
        parameters_file.write('followup.window.duration = "' + followup_window_duration.strftime(date_format) + '"\n')
    else:
        parameters_file.write('followup.window.duration = "' + followup_window_duration + '"\n')
    
    if not followup_window_duration_unit in ('days', 'weeks', 'months', 'years'):
        warnings.warn('adhereR: argument "followup_window_duration_unit" (' + followup_window_duration_unit + ') is not recognized.')
        parameters_file.close()
        return None;
    parameters_file.write('followup.window.duration.unit = "' + followup_window_duration_unit + '"\n')


    # Observation window:
    if not observation_window_start_type in ('numeric', 'character', 'date'):
        warnings.warn('adhereR: argument "observation_window_start_type" (' + observation_window_start_type + ') is not recognized.')
        parameters_file.close()
        return None;
    parameters_file.write('observation.window.start.type = "' + observation_window_start_type + '"\n')
    
    if isinstance(observation_window_start, numbers.Number):
        parameters_file.write('observation.window.start = "' + str(observation_window_start) + '"\n')
    elif isinstance(observation_window_start, datetime.date) or isinstance(observation_window_start, datetime.datetime):
        parameters_file.write('observation.window.start = "' + observation_window_start.strftime(date_format) + '"\n')
    else:
        parameters_file.write('observation.window.start = "' + observation_window_start + '"\n')
    
    if not observation_window_start_unit in ('days', 'weeks', 'months', 'years'):
        warnings.warn('adhereR: argument "observation_window_start_unit" (' + observation_window_start_unit + ') is not recognized.')
        parameters_file.close()
        return None;
    parameters_file.write('observation.window.start.unit = "' + observation_window_start_unit + '"\n')


    if not observation_window_duration_type in ('numeric', 'character', 'date'):
        warnings.warn('adhereR: argument "observation_window_duration_type" (' + observation_window_duration_type + ') is not recognized.')
        parameters_file.close()
        return None;
    parameters_file.write('observation.window.duration.type = "' + observation_window_duration_type + '"\n')
    
    if isinstance(observation_window_duration, numbers.Number):
        parameters_file.write('observation.window.duration = "' + str(observation_window_duration) + '"\n')
    elif isinstance(observation_window_duration, datetime.date) or isinstance(observation_window_duration, datetime.datetime):
        parameters_file.write('observation.window.duration = "' + observation_window_duration.strftime(date_format) + '"\n')
    else:
        parameters_file.write('observation.window.duration = "' + observation_window_duration + '"\n')
    
    if not observation_window_duration_unit in ('days', 'weeks', 'months', 'years'):
        warnings.warn('adhereR: argument "observation_window_duration_unit" (' + observation_window_duration_unit + ') is not recognized.')
        parameters_file.close()
        return None;
    parameters_file.write('observation.window.duration.unit = "' + observation_window_duration_unit + '"\n')


    # Date format:    
    if not isinstance(date_format, str):
        warnings.warn('adhereR: argument "date_format" must be a string specifying a valid strftime() date.')
        parameters_file.close()
        return None;
    parameters_file.write('date.format = "' + date_format + '"\n')   
    
    
    # Auxiliary columns for event intervals computation:
    if not isinstance(event_interval_colname, str):
        warnings.warn('adhereR: argument "event_interval_colname" must be a string specifying a valid column name.')
        parameters_file.close()
        return None;
    parameters_file.write('event.interval.colname = "' + event_interval_colname + '"\n')   

    if not isinstance(gap_days_colname, str):
        warnings.warn('adhereR: argument "gap_days_colname" must be a string specifying a valid column name.')
        parameters_file.close()
        return None;
    parameters_file.write('gap.days.colname = "' + gap_days_colname + '"\n')   
    
    
    # Parallel processing:
    if not parallel_backend in ('none', 'multicore', 'snow', 'snow(SOCK)', 'snow(MPI)', 'snow(NWS)'):
        warnings.warn('adhereR: argument "parallel_backend" (' + parallel_backend + ') is not recognized.')
        parameters_file.close()
        return None;
    parameters_file.write('parallel.backend = "' + parallel_backend + '"\n')   

    if isinstance(parallel_threads, numbers.Number):
        parameters_file.write('parallel.threads = "' + str(parallel_threads) + '"\n')
    else:
        parameters_file.write('parallel.threads = "' + parallel_threads + '"\n')
    
    
    # Other arguments:
    if not isinstance(force_NA_CMA_for_failed_patients, bool):
        warnings.warn('adhereR: argument "force_NA_CMA_for_failed_patients" must be a bool.')
        parameters_file.close()
        return None;
    parameters_file.write('force.NA.CMA.for.failed.patients = "' + ('TRUE' if force_NA_CMA_for_failed_patients else 'FALSE') + '"\n')   

    if not isinstance(suppress_warnings, bool):
        warnings.warn('adhereR: argument "suppress_warnings" must be a bool.')
        parameters_file.close()
        return None;
    parameters_file.write('suppress.warnings = "' + ('TRUE' if suppress_warnings else 'FALSE') + '"\n')   

    if not isinstance(save_event_info, bool):
        warnings.warn('adhereR: argument "save_event_info" must be a bool.')
        parameters_file.close()
        return None;
    parameters_file.write('save.event.info = "' + ('TRUE' if save_event_info else 'FALSE') + '"\n')   

    
    # Caller-specific conventions:
    if not isinstance(NA_symbol_numeric, str):
        warnings.warn('adhereR: argument "save_event_info" must be a bool.')
        parameters_file.close()
        return None;
    parameters_file.write('NA.SYMBOL.NUMERIC = "' + NA_symbol_numeric + '"\n')   
    
    if not isinstance(NA_symbol_string, str):
        warnings.warn('adhereR: argument "save_event_info" must be a bool.')
        parameters_file.close()
        return None;
    parameters_file.write('NA.SYMBOL.STRING = "' + NA_symbol_string + '"\n')   
    
    if not isinstance(logical_symbol_TRUE, str):
        warnings.warn('adhereR: argument "save_event_info" must be a bool.')
        parameters_file.close()
        return None;
    parameters_file.write('LOGICAL.SYMBOL.TRUE = "' + logical_symbol_TRUE + '"\n')   
    
    if not isinstance(logical_symbol_FALSE, str):
        warnings.warn('adhereR: argument "save_event_info" must be a bool.')
        parameters_file.close()
        return None;
    parameters_file.write('LOGICAL.SYMBOL.FALSE = "' + logical_symbol_FALSE + '"\n')   
    
    if not isinstance(colnames_dot_symbol, str):
        warnings.warn('adhereR: argument "save_event_info" must be a bool.')
        parameters_file.close()
        return None;
    parameters_file.write('COLNAMES.DOT.SYMBOL = "' + colnames_dot_symbol + '"\n')   
    
    if not isinstance(colnames_start_dot, str):
        warnings.warn('adhereR: argument "save_event_info" must be a bool.')
        parameters_file.close()
        return None;
    parameters_file.write('COLNAMES.START.DOT = "' + colnames_start_dot + '"\n')  
    
    
    # Plotting:
    if not isinstance(plot_show, bool):
        warnings.warn('adhereR: argument "plot_show" must be a bool.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.show = "' + ('TRUE' if plot_show else 'FALSE') + '"\n')  
    
    if plot_save_to is None:
        parameters_file.write('plot.save.to = ""\n') 
    elif isinstance(plot_save_to, str):
        parameters_file.write('plot.save.to = "' + plot_save_to + '"\n')  
    else:
        warnings.warn('adhereR: argument "plot_save_to" must be a string or "None".')
        parameters_file.close()
        return None;

    if not plot_save_as in ('jpg', 'png', 'tiff', 'eps', 'pdf'):
        warnings.warn('adhereR: argument "plot_save_as" (' + plot_save_as + ') is not recognized.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.save.as = "' + plot_save_as + '"\n')  
    
    if not isinstance(plot_width, numbers.Number) or plot_width <= 0:
        warnings.warn('adhereR: argument "plot_width" must be a strictly positive number.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.width = "' + str(plot_width) + '"\n')  
    
    if not isinstance(plot_height, numbers.Number) or plot_height <= 0:
        warnings.warn('adhereR: argument "plot_height" must be a strictly positive number.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.height = "' + str(plot_height) + '"\n')      
    
    if not isinstance(plot_quality, numbers.Number) or plot_quality <= 0:
        warnings.warn('adhereR: argument "plot_quality" must be a strictly positive number.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.quality = "' + str(plot_quality) + '"\n')      
    
    if not isinstance(plot_dpi, numbers.Number) or plot_dpi <= 0:
        warnings.warn('adhereR: argument "plot_dpi" must be a strictly positive number.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.dpi = "' + str(plot_dpi) + '"\n') 

    if plot_patients_to_plot is None:
        parameters_file.write('plot.patients.to.plot = ""\n')
    elif isinstance(plot_patients_to_plot, list):
        parameters_file.write('plot.patients.to.plot = "' + (';'.join(str(x) for x in plot_patients_to_plot)) + '"\n')
    else:
        parameters_file.write('plot.patients.to.plot = "' + str(plot_patients_to_plot) + '"\n')
        
    if plot_duration is None:
        parameters_file.write('plot.duration = ""\n')
    elif isinstance(plot_duration, numbers.Number) and plot_duration > 0:
        parameters_file.write('plot.duration = "' + str(plot_duration) + '"\n')
    else:
        warnings.warn('adhereR: argument "plot_duration" must be a strictly positive number.')
        parameters_file.close()
        return None;

    if not isinstance(plot_align_all_patients, bool):
        warnings.warn('adhereR: argument "plot_align_all_patients" must be a bool.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.align.all.patients = "' + ('TRUE' if plot_align_all_patients else 'FALSE') + '"\n')   

    if not isinstance(plot_align_first_event_at_zero, bool):
        warnings.warn('adhereR: argument "plot_align_first_event_at_zero" must be a bool.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.align.first.event.at.zero = "' + ('TRUE' if plot_align_first_event_at_zero else 'FALSE') + '"\n')   

    if not plot_show_period in ('days', 'dates'):
        warnings.warn('adhereR: argument "plot_show_period" (' + plot_show_period + ') is not recognized.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.show.period = "' + plot_show_period + '"\n')  
    
    if not isinstance(plot_period_in_days, numbers.Number) or plot_period_in_days <= 0:
        warnings.warn('adhereR: argument "plot_period_in_days" must be a strictly positive number.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.period.in.days = "' + str(plot_period_in_days) + '"\n')  
    
    if not isinstance(plot_show_legend, bool):
        warnings.warn('adhereR: argument "plot_show_legend" must be a bool.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.show.legend = "' + ('TRUE' if plot_show_legend else 'FALSE') + '"\n') 
    
    if plot_legend_x in ('left', 'right') and plot_legend_y in ('bottom', 'top'):
        parameters_file.write('plot.legend.x = "' + plot_legend_x + '"\n')
        parameters_file.write('plot.legend.y = "' + plot_legend_y + '"\n')
    elif isinstance(plot_legend_x, numbers.Number) and isinstance(plot_legend_y, numbers.Number) and plot_legend_x >= 0 and plot_legend_y >= 0:
        parameters_file.write('plot.legend.x = "' + str(plot_legend_x) + '"\n')
        parameters_file.write('plot.legend.y = "' + str(plot_legend_y) + '"\n')
    else:
        warnings.warn('adhereR: argument "plot_legend_x" and "plot_legend_y" are not recognized.')
        parameters_file.close()
        return None;
        
    if not isinstance(plot_legend_bkg_opacity, numbers.Number) or plot_legend_bkg_opacity < 0 or plot_legend_bkg_opacity > 1:
        warnings.warn('adhereR: argument "plot_legend_bkg_opacity" must be a number between 0 and 1.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.legend.bkg.opacity = "' + str(plot_legend_bkg_opacity) + '"\n')  
        
    if not isinstance(plot_cex, numbers.Number) or plot_cex < 0:
        warnings.warn('adhereR: argument "plot_cex" must be a strictly positive number.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.cex = "' + str(plot_cex) + '"\n')  
        
    if not isinstance(plot_cex_axis, numbers.Number) or plot_cex_axis < 0:
        warnings.warn('adhereR: argument "plot_cex_axis" must be a strictly positive number.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.cex.axis = "' + str(plot_cex_axis) + '"\n')  
        
    if not isinstance(plot_cex_lab, numbers.Number) or plot_cex_lab < 0:
        warnings.warn('adhereR: argument "plot_cex_lab" must be a strictly positive number.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.cex.lab = "' + str(plot_cex_lab) + '"\n')  
        
    if not isinstance(plot_show_cma, bool):
        warnings.warn('adhereR: argument "plot_show_cma" must be a bool.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.show.cma = "' + ('TRUE' if plot_show_cma else 'FALSE') + '"\n') 
    
    if not isinstance(plot_unspecified_category_label, str):
        warnings.warn('adhereR: argument "plot_unspecified_category_label" must be a string.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.unspecified.category.label = "' + plot_unspecified_category_label + '"\n') 
        
    if not plot_lty_event in ('solid', 'dotted', 'dashed'):
        warnings.warn('adhereR: argument "plot_lty_event" (' + plot_lty_event + ') is not recognized.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.lty.event = "' + plot_lty_event + '"\n')  
    
    if not isinstance(plot_lwd_event, numbers.Number) or plot_lwd_event < 0:
        warnings.warn('adhereR: argument "plot_lwd_event" must be a strictly positive number.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.lwd.event = "' + str(plot_lwd_event) + '"\n')
    
    if isinstance(plot_pch_start_event, numbers.Number):
        parameters_file.write('plot.pch.start.event = "' + str(plot_pch_start_event) + '"\n')
    elif isinstance(plot_pch_start_event, str):
        parameters_file.write('plot.pch.start.event = "' + plot_pch_start_event + '"\n')
    else:
        warnings.warn('adhereR: argument "plot_pch_start_event" is not recognized.')
        parameters_file.close()
        return None;
        
    if isinstance(plot_pch_end_event, numbers.Number):
        parameters_file.write('plot.pch.end.event = "' + str(plot_pch_end_event) + '"\n')
    elif isinstance(plot_pch_end_event, str):
        parameters_file.write('plot.pch.end.event = "' + plot_pch_end_event + '"\n')
    else:
        warnings.warn('adhereR: argument "plot_pch_end_event" is not recognized.')
        parameters_file.close()
        return None;
        
    if not isinstance(plot_show_event_intervals, bool):
        warnings.warn('adhereR: argument "plot_show_event_intervals" must be a bool.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.show.event.intervals = "' + ('TRUE' if plot_show_event_intervals else 'FALSE') + '"\n') 
    
    if not isinstance(plot_col_na, str):
        warnings.warn('adhereR: argument "plot_col_na" must be a string.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.col.na = "' + plot_col_na + '"\n') 
        
    if not isinstance(plot_col_continuation, str):
        warnings.warn('adhereR: argument "plot_col_continuation" must be a string.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.col.continuation = "' + plot_col_continuation + '"\n') 
        
    if not plot_lty_continuation in ('solid', 'dotted', 'dashed'):
        warnings.warn('adhereR: argument "plot_lty_continuation" (' + plot_lty_continuation + ') is not recognized.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.lty.continuation = "' + plot_lty_continuation + '"\n')  
    
    if not isinstance(plot_lwd_continuation, numbers.Number) or plot_lwd_continuation < 0:
        warnings.warn('adhereR: argument "plot_lwd_continuation" must be a strictly positive number.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.lwd.continuation = "' + str(plot_lwd_continuation) + '"\n')
    
    if not isinstance(plot_print_CMA, bool):
        warnings.warn('adhereR: argument "plot_print_CMA" must be a bool.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.print.CMA = "' + ('TRUE' if plot_print_CMA else 'FALSE') + '"\n') 
    
    if not isinstance(plot_plot_CMA, bool):
        warnings.warn('adhereR: argument "plot_plot_CMA" must be a bool.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.plot.CMA = "' + ('TRUE' if plot_plot_CMA else 'FALSE') + '"\n') 
    
    if not isinstance(plot_plot_CMA_as_histogram, bool):
        warnings.warn('adhereR: argument "plot_plot_CMA_as_histogram" must be a bool.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.plot.CMA.as.histogram = "' + ('TRUE' if plot_plot_CMA_as_histogram else 'FALSE') + '"\n') 
    
    if not isinstance(plot_CMA_plot_ratio, numbers.Number) or plot_CMA_plot_ratio < 0 or plot_CMA_plot_ratio > 1:
        warnings.warn('adhereR: argument "plot_CMA_plot_ratio" must be a number between 0 and 1.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.CMA.plot.ratio = "' + str(plot_CMA_plot_ratio) + '"\n')
    
    if not isinstance(plot_CMA_plot_col, str):
        warnings.warn('adhereR: argument "plot_CMA_plot_col" must be a string.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.CMA.plot.col = "' + plot_CMA_plot_col + '"\n') 
        
    if not isinstance(plot_CMA_plot_border, str):
        warnings.warn('adhereR: argument "plot_CMA_plot_border" must be a string.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.CMA.plot.border = "' + plot_CMA_plot_border + '"\n') 
        
    if not isinstance(plot_CMA_plot_bkg, str):
        warnings.warn('adhereR: argument "plot_CMA_plot_bkg" must be a string.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.CMA.plot.bkg = "' + plot_CMA_plot_bkg + '"\n') 
    
    if plot_CMA_plot_text is None:
        parameters_file.write('plot.CMA.plot.text = ""\n')
    elif not isinstance(plot_CMA_plot_text, str):
        warnings.warn('adhereR: argument "plot_CMA_plot_text" must be a string.')
        parameters_file.close()
        return None;
    else:
        parameters_file.write('plot.CMA.plot.text = "' + plot_CMA_plot_text + '"\n')
        
    if not isinstance(plot_highlight_followup_window, bool):
        warnings.warn('adhereR: argument "plot_highlight_followup_window" must be a bool.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.highlight.followup.window = "' + ('TRUE' if plot_highlight_followup_window else 'FALSE') + '"\n') 
    
    if not isinstance(plot_followup_window_col, str):
        warnings.warn('adhereR: argument "plot_followup_window_col" must be a string.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.followup.window.col = "' + plot_followup_window_col + '"\n') 
    
    if not isinstance(plot_highlight_observation_window, bool):
        warnings.warn('adhereR: argument "plot_highlight_observation_window" must be a bool.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.highlight.observation.window = "' + ('TRUE' if plot_highlight_observation_window else 'FALSE') + '"\n') 
    
    if not isinstance(plot_observation_window_col, str):
        warnings.warn('adhereR: argument "plot_observation_window_col" must be a string.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.observation.window.col = "' + plot_observation_window_col + '"\n') 
    
    if not isinstance(plot_observation_window_density, numbers.Number) or plot_observation_window_density < 0:
        warnings.warn('adhereR: argument "plot_observation_window_density" must be a positive number.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.observation.window.density = "' + str(plot_observation_window_density) + '"\n')
    
    if not isinstance(plot_observation_window_angle, numbers.Number):
        warnings.warn('adhereR: argument "plot_observation_window_angle" must be a positive number.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.observation.window.angle = "' + str(plot_observation_window_angle) + '"\n')
    
    if not isinstance(plot_show_real_obs_window_start, bool):
        warnings.warn('adhereR: argument "plot_show_real_obs_window_start" must be a bool.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.show.real.obs.window.start = "' + ('TRUE' if plot_show_real_obs_window_start else 'FALSE') + '"\n') 
    
    if not isinstance(plot_real_obs_window_density, numbers.Number) or plot_real_obs_window_density < 0:
        warnings.warn('adhereR: argument "plot_real_obs_window_density" must be a positive number.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.real.obs.window.density = "' + str(plot_real_obs_window_density) + '"\n')
    
    if not isinstance(plot_real_obs_window_angle, numbers.Number):
        warnings.warn('adhereR: argument "plot_real_obs_window_angle" must be a positive number.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.real.obs.window.angle = "' + str(plot_real_obs_window_angle) + '"\n')
    
    if not isinstance(plot_bw_plot, bool):
        warnings.warn('adhereR: argument "plot_bw_plot" must be a bool.')
        parameters_file.close()
        return None;
    parameters_file.write('plot.bw.plot = "' + ('TRUE' if plot_bw_plot else 'FALSE') + '"\n') 
    

    
    # Write the parameters ending:
    parameters_file.write('end_parameters\n')
    # Close the parameters file:
    parameters_file.close()

    # Remove any pre-existing results file:
    os.remove(path_to_data_directory + "/Adherer-results.txt")

    # Call adhereR:
    Rscript_cmd = path_to_Rscript + ' --vanilla ' + path_to_adherer + 'callAdhereR.R' + ' ' + path_to_data_directory
    #print('DEBUG: call = ' + Rscript_cmd)
    return_code = subprocess.call(Rscript_cmd, shell=True) 
    #print('DEBUG: return code = ' + str(return_code))

    
    # Check and load the results:
    with open(path_to_data_directory + "/Adherer-results.txt", 'r') as adherer_messages_file:
        adherer_messages = adherer_messages_file.read()
        adherer_messages_file.close()
    if print_adherer_messages:
        print('Adherer returned code ' + str(return_code) + ' and said:\n' + adherer_messages)
    if return_code != 0 or adherer_messages[0:3] != 'OK:':
        warnings.warn('adhereR: some error has occured when calling AdhereR (code ' + str(return_code) + '): "' + adherer_messages + '".')
        return None;

    # The return value (as a dictionary 'name':'value')
    ret_val = {'return_code':return_code,
               'message':adherer_messages}
    
    if function in ('CMA1', 'CMA2', 'CMA3', 'CMA4', 'CMA5', 'CMA6', 'CMA7', 'CMA8', 'CMA9', 'CMA_per_episode', 'CMA_sliding_window'):
        # Expecting CMA.csv and possibly EVENTINFO.csv
        ret_val['CMA'] = pandas.read_csv(path_to_data_directory + '/CMA' + ('-plotted' if plot_show else '') + '.csv', sep='\t', header=0)
        if save_event_info:
            ret_val['EVENTINFO'] = pandas.read_csv(path_to_data_directory + '/EVENTINFO' + ('-plotted' if plot_show else '') + '.csv', sep='\t', header=0)
            
    if plot_show is True:
        # Load the produced image (if any):
        ret_val['plot'] = PIL.Image.open((plot_save_to if not (plot_save_to is None) else path_to_data_directory) + '/adherer-plot' + '.' + plot_save_as)
    
    # Everything seems fine....
    return ret_val;


x = call_adhereR(df, 'CMA1', 'patientID', 'prescriptionDate', 'prescriptionDuration', 
                followup_window_start_type = 'numeric', followup_window_start = 0, followup_window_start_unit = "days",
                followup_window_duration_type = 'numeric', followup_window_duration = 365*2, followup_window_duration_unit = "days",
                observation_window_start_type = 'numeric', observation_window_start = 30, observation_window_start_unit = "days",
                observation_window_duration_type = 'numeric', observation_window_duration = 365, observation_window_duration_unit = "days",
                plot_show = True, plot_patients_to_plot = [2,3],
                save_event_info = True, path_to_adherer = '../')



