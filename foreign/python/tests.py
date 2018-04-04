#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar  4 19:23:21 2018

@author: ddediu
"""

import pandas
import adherer

# Load the test dataset
df = pandas.read_csv('./test-dataset.csv', sep='\t', header=0)

# Change the column names:
df.rename(columns={'ID': 'patientID',
                   'DATE': 'prescriptionDate',
                   'PERDAY': 'quantityPerDay',
                   'CLASS': 'medicationType',
                   'DURATION': 'prescriptionDuration'},
          inplace=True)


# Tests:
#x = adherer.__call_adhereR(df, 'CMA1', 'patientID', 'prescriptionDate', 'prescriptionDuration',
#                followup_window_start_type = 'numeric', followup_window_start = 0, followup_window_start_unit = "days",
#                followup_window_duration_type = 'numeric', followup_window_duration = 365*2, followup_window_duration_unit = "days",
#                observation_window_start_type = 'numeric', observation_window_start = 30, observation_window_start_unit = "days",
#                observation_window_duration_type = 'numeric', observation_window_duration = 365, observation_window_duration_unit = "days",
#                plot_show = True, plot_patients_to_plot = [2,3],
#                save_event_info = True, path_to_adherer = '../')

#y = call_adhereR(df, 'plot_interactive_cma',
#                 ID_colname='patientID', event_date_colname='prescriptionDate', event_duration_colname='prescriptionDuration',
#                 path_to_adherer = '../')

testcma1 = adherer.CMA1(df,
                        ID_colname='patientID',
                        event_date_colname='prescriptionDate',
                        event_duration_colname='prescriptionDuration',
                        #event_daily_dose_colname='quantityPerDay',
                        #medication_class_colname='medicationType',
                        #save_event_info = True,
                        path_to_adherer='../')

testcma8 = adherer.CMA8(df,
                        ID_colname='patientID',
                        event_date_colname='prescriptionDate',
                        event_duration_colname='prescriptionDuration',
                        event_daily_dose_colname='quantityPerDay',
                        medication_class_colname='medicationType',
                        #save_event_info = True,
                        path_to_adherer='../')
#testcma.plotInteractive(patient_to_plot=3)
x = testcma8.plot(patients_to_plot=['1', '2', '3'],
                  save_as="tiff",
                  width=7, height=7,
                  quality=90, dpi=92, align_all_patients=True,
                  period_in_days=30,
                  show_legend=True, legend_x='left', legend_y='top',
                  cex=0.5, col_continuation='blue', bw_plot=True)


testcmaE = adherer.CMAPerEpisode(df,
                                   CMA_to_apply='CMA1',
                                   ID_colname='patientID',
                                   event_date_colname='prescriptionDate',
                                   event_duration_colname='prescriptionDuration',
                                   event_daily_dose_colname='quantityPerDay',
                                   medication_class_colname='medicationType',
                                   #save_event_info = True,
                                   path_to_adherer='../')
#testcma.plotInteractive(patient_to_plot=3)
y = testcmaE.plot(patients_to_plot=['1', '2', '3'],
                  save_as="tiff",
                  width=7, height=7,
                  quality=90, dpi=92, align_all_patients=True,
                  period_in_days=30,
                  show_legend=True, legend_x='left', legend_y='top',
                  cex=0.5, col_continuation='blue', bw_plot=True)


testcmaW = adherer.CMASlidingWindow(df,
                                      CMA_to_apply='CMA1',
                                      ID_colname='patientID',
                                      event_date_colname='prescriptionDate',
                                      event_duration_colname='prescriptionDuration',
                                      event_daily_dose_colname='quantityPerDay',
                                      medication_class_colname='medicationType',
                                      #save_event_info = True,
                                      path_to_adherer='../')
#testcma.plotInteractive(patient_to_plot=3)
z = testcmaW.plot(patients_to_plot=['1', '2', '3'],
                  save_as="tiff",
                  width=7, height=7,
                  quality=90, dpi=92, align_all_patients=True,
                  period_in_days=30,
                  show_legend=True, legend_x='left', legend_y='top',
                  cex=0.5, col_continuation='blue', bw_plot=True)


