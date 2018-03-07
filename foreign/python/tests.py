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
            date_format = "%m/%d/%Y",
            save_event_info = False,
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
    date_format : str
        The date format to be used throughout the call (in the standard strftime() format)
    save_event_info : bool
        Should the EVENTINFO be also saved?
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
    
    #if function in ('CMA1', 'CMA2', 'CMA3', 'CMA4'):
    #    maka;
    
    if not isinstance(date_format, str):
        warnings.warn('adhereR: argument "date_format" must be a string specifying a valid strftime() date.')
        parameters_file.close()
        return None;
    parameters_file.write('date.format = "' + date_format + '"\n')   
    
    parameters_file.write('save.event.info = "' + ('TRUE' if save_event_info else 'FALSE') + '"\n')   
    
    # Write the parameters ending:
    parameters_file.write('end_parameters\n')
    # Close the parameters file:
    parameters_file.close()

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
            save_event_info = True, path_to_adherer = '../')



