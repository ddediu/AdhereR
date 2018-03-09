#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar  4 19:23:21 2018

@author: ddediu
"""

import pandas, warnings, subprocess, os, numbers, datetime

# Load the test dataset
df = pandas.read_csv('./test-dataset.csv', sep='\t', header=0)

# Change the column names:
df.rename(columns={'ID': 'patientID', 
                   'DATE': 'prescriptionDate', 
                   'PERDAY': 'quantityPerDay', 
                   'CLASS': 'medicationType', 
                   'DURATION': 'prescriptionDuration'}, 
inplace=True)




def adhereR(dataset,
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
    
    # Write the shared parameters:
    parameters_file.write('function = "' + function + '"\n')
    parameters_file.write('ID.colname = "' + ID_colname + '"\n')
    parameters_file.write('event.date.colname = "' + event_date_colname + '"\n')
    parameters_file.write('event.duration.colname = "' + event_duration_colname + '"\n')


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
    
    #if function in ('CMA1', 'CMA2', 'CMA3', 'CMA4'):
    #    maka;

    
    if not isinstance(date_format, str):
        warnings.warn('adhereR: argument "date_format" must be a string specifying a valid strftime() date.')
        parameters_file.close()
        return None;
    parameters_file.write('date.format = "' + date_format + '"\n')   
    
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
    
    if not isinstance(force_NA_CMA_for_failed_patients, bool):
        warnings.warn('adhereR: argument "force_NA_CMA_for_failed_patients" must be a bool.')
        parameters_file.close()
        return None;
    parameters_file.write('force.NA.CMA.for.failed.patients = "' + ('TRUE' if force_NA_CMA_for_failed_patients else 'FALSE') + '"\n')   


    if not parallel_backend in ('none', 'multicore', 'snow', 'snow(SOCK)', 'snow(MPI)', 'snow(NWS)'):
        warnings.warn('adhereR: argument "parallel_backend" (' + parallel_backend + ') is not recognized.')
        parameters_file.close()
        return None;
    parameters_file.write('parallel.backend = "' + parallel_backend + '"\n')   

    if isinstance(parallel_threads, numbers.Number):
        parameters_file.write('parallel.threads = "' + str(parallel_threads) + '"\n')
    else:
        parameters_file.write('parallel.threads = "' + parallel_threads + '"\n')
    
    
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

    
    # Check and load the results
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
        ret_val['CMA'] = pandas.read_csv(path_to_data_directory + '/CMA.csv', sep='\t', header=0)
        if save_event_info:
            ret_val['EVENTINFO'] = pandas.read_csv(path_to_data_directory + '/EVENTINFO.csv', sep='\t', header=0)
    
    # Everything seems fine....
    return ret_val;


x = adhereR(df, 'CMA1', 'patientID', 'prescriptionDate', 'prescriptionDuration', 
            followup_window_start_type = 'numeric', followup_window_start = 0, followup_window_start_unit = "days",
            followup_window_duration_type = 'numeric', followup_window_duration = 365*2, followup_window_duration_unit = "days",
            observation_window_start_type = 'numeric', observation_window_start = 30, observation_window_start_unit = "days",
            observation_window_duration_type = 'numeric', observation_window_duration = 365, observation_window_duration_unit = "days", 
            save_event_info = True, path_to_adherer = '../')



