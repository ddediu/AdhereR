#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar  4 19:23:21 2018

@author: ddediu
"""

import pandas, warnings, subprocess, os

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
            path_to_Rscript = '/usr/local/bin/Rscript',
            path_to_adherer = os.getcwd(),
            path_to_data_directory = os.getcwd()
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
        The follow-up window start unit; can be 'numeric', 'character' or 'date' (defaults to 'numeric')
    followup_window_start
        The follow-up window start; can be a number, a string or a date
    path_to_Rscript : str
        The path to where Rscript is installed
    path_to_adherer : str
        The path to where the callAdhereR.R script is (defaults to the current folder)
    path_to_data_directory : str
        The path to the directory where the various data should be saved (defaults to the current folder)

    Returns
    -------
    MAKA
        Description of return value

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
    parameters_file.write('followup.window.start = "' + str(followup_window_start) + '"\n')
    
    #if function in ('CMA1', 'CMA2', 'CMA3', 'CMA4'):
    #    maka;
    
    # Write the parameters ending:
    parameters_file.write('end_parameters\n')
    # Close the parameters file:
    parameters_file.close()

    # Call adhereR:
    Rscript_cmd = path_to_Rscript + ' --vanilla ' + path_to_adherer + 'callAdhereR.R' + ' ' + path_to_data_directory
    print(Rscript_cmd)
    return_code = subprocess.call(Rscript_cmd, shell=True) 
    print(return_code)
    
    # Check and load the results
    
    # Everything seems fine....
    return True;


adhereR(df, 'CMA1', 'patientID', 'prescriptionDate', 'prescriptionDate', 
        path_to_adherer = '../')



