###############################################################################################
#
#    AdhereRViz: interactive visualisations for AdhereR.
#    This implements interactive plotting using shiny.
#    Copyright (C) 2018-2019  Dan Dediu, Alexandra Dima & Samuel Allemann
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
###############################################################################################


# Imports ----
#library(shiny)
#' @import AdhereR
#' @import AdhereRViz
#' @import shiny
#' @import colourpicker
#' @import viridisLite
#' @import highlight
#' @import clipr
#' @import shinyjs
#' @import shinyWidgets
#' @import knitr
#' @import readODS
#' @import readxl
#' @import haven
#' @import DBI
#' @import RMariaDB
#' @import RSQLite
NULL


# Symbols for lines and dots as images ----
line.types <- c("blank"   ="symbols/lty-blank.png",
                "solid"   ="symbols/lty-solid.png",
                "dashed"  ="symbols/lty-dashed.png",
                "dotted"  ="symbols/lty-dotted.png",
                "dotdash" ="symbols/lty-dotdash.png",
                "longdash"="symbols/lty-longdash.png",
                "twodash" ="symbols/lty-twodash.png");
point.types <- c("plus"=3,
                 "times"=4,
                 "star"=8,
                 "square"=0,
                 "circle"=1,
                 "triangle"=2,
                 "down triangle"=6,
                 "diamond"=5,
                 "fill square"=15,
                 "fill small circle"=20,
                 "fill med circle"=16,
                 "fill big circle"=19,
                 "fill triangle"=17,
                 "fill diamond"=18,
                 "square plus"=12,
                 "square times"=7,
                 "circle plus"=10,
                 "circle times"=13,
                 "two triangles"=11);


# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # JavaScript ----
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text="shinyjs.scroll_cma_compute_log = function() {var x = document.getElementById('cma_computation_progress_log_container'); x.scrollTop = x.scrollHeight;}",
                         #functions=c("shinyjs.scroll_cma_compute_log")),
                         functions=c("scroll_cma_compute_log")),
  #shinyjs::extendShinyjs(text="shinyjs.show_hide_sections = function() {$('#follow_up_folding_bits').toggle();"),

  # APP TITLE ----
  #titlePanel(windowTitle="AdhereR: Interactive plotting using Shiny..."),
  list(tags$head(HTML('<link rel="icon", href="adherer-logo.png", type="image/png" />'))),
  div(style="padding: 1px 0px; width: '100%'", titlePanel(title="", windowTitle="AdhereR: Shiny plot...")),

  fluidRow(

    # LOGO & ABOUT ----
    column(12,
           div(
             img(src='adherer-logo.png', align = "left", style="font-size: x-large; font-weight: bold; height: 2em; vertical-align: baseline;"),
             div(style="width: 3em; display: inline-block; "),
             #h1("interactive plots with Shiny...", style="color:DarkBlue; font-size: x-large; font-weight: bold; margin: 0; display: inline-block;"),
             div(title="About AdhereR and AdhereRViz, and links to more info online and offline...",
                 actionButton(inputId="about_button", label=strong("About"), icon=icon("question-sign", lib="glyphicon"), style="color: #3498db; border: none; background: none;"),
                 style="float: right;")
           ),
           hr()
    ),

    #shinythemes::themeSelector(),

    # SIDEBAR PANEL ----
    column(3,

           # PARAMS TAB ----
           shinyjs::hidden(div(id="sidebar_tabpanel_container", # start with these hidden...
             tabsetPanel(id="sidebar-tabpanel",
                         selected=ifelse(!is.null(.GlobalEnv$.plotting.params$data), "sidebar-params-tab", "sidebar-params-data"),

                       tabPanel(span("Params", title="See the plot and adjust various parameters (type of CMA, patients, etc.)"),
                                value="sidebar-params-tab", icon=icon("wrench", lib="glyphicon"), fluid=TRUE,
                                conditionalPanel(
                                  condition="!(output.is_dataset_defined)",

                                  wellPanel(id = "tPanelnodataset", style = "overflow:scroll; max-height: 90vh;",
                                            div(h4("No datasource!"), style="color:DarkRed"),
                                            br(),
                                            div(span(span("There is no valid source of data defined (probably because the interactive Shiny plotting was invoked without passing a dataset).")), style="color: red;"),
                                            hr(),
                                            div(span("Please use the "),
                                                span(icon("hdd",lib="glyphicon"),strong("Data"), style="color: darkblue"),
                                                span(" tab to select a valid datesource!"))
                                  )
                                ),

                                conditionalPanel(
                                  condition="(output.is_dataset_defined)",

                                  wellPanel(id = "tPanel", style = "overflow:scroll; max-height: 90vh;",

                                            # Dataset info ----
                                            div(title="Info about the currently used dataset...",
                                                actionButton(inputId="about_dataset_button",
                                                             label=strong("Dataset info..."),
                                                             icon=icon("hdd", lib="glyphicon"),
                                                             style="color: #3498db; border: none; background: none;")),

                                            # General settings ----
                                            div(id='general_settings_section', style="cursor: pointer;",
                                                span(title='General setting that apply to all kinds of plots', # trick for adding tooltips: create a container div with the title the desired tootltip text...
                                                     id = "general_settings", h4("General settings"), style="color:DarkBlue"),
                                                shinyjs::hidden(div(title='Click to unfold...', id="general_settings_unfold_icon", icon("option-horizontal", lib="glyphicon")))),

                                            div(id="general_settings_contents",
                                                # Select the CMA class ----
                                                div(title='Select the type of CMA to plot: "simple", "per eipsode" or "sliding window"',
                                                    selectInput(inputId="cma_class",
                                                                label="CMA type",
                                                                choices=c("simple", "per episode", "sliding window"),
                                                                selected=.GlobalEnv$.plotting.params$cma.class)),

                                                # Select the simple CMA to compute
                                                conditionalPanel(
                                                  condition = "(input.cma_class == 'simple')",
                                                  div(title='The "simple" CMA to compute by itself',
                                                      selectInput(inputId="cma_to_compute",
                                                                  label="CMA to compute",
                                                                  choices=paste0("CMA",0:9),
                                                                  selected="CMA0"))
                                                ),
                                                conditionalPanel(
                                                  condition = "(input.cma_class != 'simple')",
                                                  div(title='The "simple" CMA to compute for each episode/sliding window',
                                                      selectInput(inputId="cma_to_compute_within_complex",
                                                                  label="CMA to compute",
                                                                  choices=paste0("CMA",1:9),
                                                                  selected="CMA1"))
                                                ),

                                                # Select the patients to plot ----
                                                div(title='Select one (or more, by repeatedly selecting) patient(s) to plot',
                                                    selectInput(inputId="patient",
                                                                label="Patient(s) to plot",
                                                                choices=.GlobalEnv$.plotting.params$all.IDs,
                                                                selected=.GlobalEnv$.plotting.params$ID,
                                                                multiple=TRUE)),

                                                hr()
                                            ),

                                            # Medication groups to plot ----
                                            conditionalPanel(
                                              condition="(output.is_mg_defined && input.mg_use_medication_groups)",

                                              div(id='mg_section', style="cursor: pointer;",
                                                  div(title='Chose which medication groups to plot', h4(id="medication_groups", "Medication groups"), style="color:DarkBlue;"),
                                                  div(title='Click to unfold...', id="mg_unfold_icon", icon("option-horizontal", lib="glyphicon"))),

                                              shinyjs::hidden(div(id="mg_contents",
                                                                  # View the current medication groups definitions:
                                                                  conditionalPanel(
                                                                    condition="(input.mg_definitions_source == 'named vector')",
                                                                    div(title="Click here to view the medication groups...",
                                                                        actionButton("mg_view_button", label="View definitions!", icon=icon("eye-open", lib="glyphicon")))
                                                                  ),

                                                                  # List the medication groups to show:
                                                                  div(title='Please select all the medication groups that should be plotted!',
                                                                      shinyWidgets::pickerInput(inputId="mg_to_plot_list",
                                                                                                label="Groups to plot:",
                                                                                                choices=c(names(.GlobalEnv$.plotting.params$medication.groups), "* (all others)"),
                                                                                                #choices="<none>",
                                                                                                options=list(`actions-box`=TRUE,
                                                                                                             `select-all-text`  ="<b>ALL</b>",
                                                                                                             `deselect-all-text`="<b>NONE</b>"),
                                                                                                multiple = TRUE,
                                                                                                selected=c(names(.GlobalEnv$.plotting.params$medication.groups), "* (all others)"))),
                                                                                                #selected="<none>")),

                                                                  conditionalPanel(
                                                                    condition="(input.mg_to_plot_list.length > 0)",

                                                                    div(title='Apply the selected medication groups!',
                                                                      actionButton(inputId="mg_plot_apply_button",
                                                                                   label=strong("Show them!"),
                                                                                   icon=icon("sunglasses", lib="glyphicon"),
                                                                                   style="color:DarkBlue; border-color:DarkBlue;"),
                                                                      style="float: center;")
                                                                    ),

                                                                  hr(),

                                                                  div(title='Visually group the medication groups by patient?',
                                                                      shinyWidgets::materialSwitch(inputId="mg_plot_by_patient",
                                                                                                   label="Visually group by patient?",
                                                                                                   value=TRUE, status="primary", right=TRUE)),

                                                                  hr()

                                              ))
                                            ),

                                            # Follow-up window ----
                                            div(id='follow_up_section', style="cursor: pointer;",
                                                div(title='Define the follow-up window (shortened to FUW)', h4(id="followup_window", "Follow-up window (FUW)"), style="color:DarkBlue;"),
                                                div(title='Click to unfold...', id="follow_up_unfold_icon", icon("option-horizontal", lib="glyphicon"))),

                                            shinyjs::hidden(div(id="follow_up_contents",
                                                                # Follow-up window start
                                                                div(title='The unit of the start of the follow-up window (can be "days", "weeks", "months", "years" or an actual "calendar date")',
                                                                    selectInput(inputId="followup_window_start_unit",
                                                                                label="FUW start unit",
                                                                                choices=c("days", "weeks", "months", "years", "calendar date"), # "column in dataset"),
                                                                                selected="days")),

                                                                # If follow-up window unit is "calendar date"
                                                                conditionalPanel(
                                                                  condition = "(input.followup_window_start_unit == 'calendar date')",
                                                                  # Select an actual date
                                                                  div(title='Select the actual start date of the follow-up window (possibly using a calendar widget in the format year-month-day)',
                                                                      dateInput(inputId="followup_window_start_date",
                                                                                label="FUW start",
                                                                                value=NULL, format="yyyy-mm-dd", startview="month", weekstart=1))
                                                                ),

                                                                ## If follow-up window unit is "column in dataset"
                                                                #conditionalPanel(
                                                                #  condition = "(input.followup_window_start_unit == 'column in dataset')",
                                                                #              selectInput(inputId="followup_window_start_column",
                                                                #                        label="Follow-up wnd. start",
                                                                #                        choices=names(.GlobalEnv$.plotting.params$data),
                                                                #                        selected="")
                                                                #),

                                                                # If follow-up window unit is regular unit
                                                                conditionalPanel(
                                                                  condition = "(input.followup_window_start_unit != 'calendar date')",  # && input.followup_window_start_unit != 'column in dataset')",
                                                                  # Select the number of units
                                                                  div(title='Select the number of units defining the start of the follow-up window',
                                                                      numericInput(inputId="followup_window_start_no_units",
                                                                                   label="FUW start",
                                                                                   value=0, min=0, max=NA, step=30))
                                                                ),

                                                                # Follow-up window duration
                                                                div(title='The unit of the duration of the follow-up window (can be "days", "weeks", "months" or "years")',
                                                                    selectInput(inputId="followup_window_duration_unit",
                                                                                label="FUW duration unit",
                                                                                choices=c("days", "weeks", "months", "years"),
                                                                                selected="days")),

                                                                # Select the number of units
                                                                div(title='Select the number of units defining the duration of the follow-up window',
                                                                    numericInput(inputId="followup_window_duration",
                                                                                 label="FUW duration",
                                                                                 value=2*365, min=0, max=NA, step=30)),

                                                                hr()
                                            )),

                                            # Observation window ----
                                            div(id='observation_section', style="cursor: pointer;",
                                                span(title='Define the observation window (shortened to OW)', id="observation_window", h4("Observation window (OW)"), style="color:DarkBlue"),
                                                div(title='Click to unfold...', id="observation_unfold_icon", icon("option-horizontal", lib="glyphicon"))),

                                            shinyjs::hidden(div(id="observation_contents",
                                                                # Observation window start
                                                                div(title='The unit of the start of the observation window (can be "days", "weeks", "months", "years" or an actual "calendar date")',
                                                                    selectInput(inputId="observation_window_start_unit",
                                                                                label="OW start unit",
                                                                                choices=c("days", "weeks", "months", "years", "calendar date"),
                                                                                selected="days")),

                                                                # If observation window unit is "calendar date"
                                                                conditionalPanel(
                                                                  condition = "(input.observation_window_start_unit == 'calendar date')",
                                                                  # Select an actual date
                                                                  div(title='Select the actual start date of the observation window (possibly using a calendar widget in the format year-month-day)',
                                                                      dateInput(inputId="observation_window_start_date",
                                                                                label="OW start",
                                                                                value=NULL, format="yyyy-mm-dd", startview="month", weekstart=1))
                                                                ),

                                                                # If observation window unit is not "calendar date"
                                                                conditionalPanel(
                                                                  condition = "(input.observation_window_start_unit != 'calendar date')",
                                                                  # Select the number of units
                                                                  div(title='Select the number of units defining the start of the observation window',
                                                                      numericInput(inputId="observation_window_start_no_units",
                                                                                   label="OW start",
                                                                                   value=0, min=0, max=NA, step=30))
                                                                ),


                                                                # Observation window duration
                                                                div(title='The unit of the duration of the observation window (can be "days", "weeks", "months" or "years")',
                                                                    selectInput(inputId="observation_window_duration_unit",
                                                                                label="OW duration unit",
                                                                                choices=c("days", "weeks", "months", "years"),
                                                                                selected="days")),

                                                                # Select the number of units
                                                                div(title='Select the number of units defining the duration of the observation window',
                                                                    numericInput(inputId="observation_window_duration",
                                                                                 label="OW duration",
                                                                                 value=2*365, min=0, max=NA, step=30)),

                                                                hr()
                                            )),


                                            # CMA5+ only ----
                                            # carry_only_for_same_medication, consider_dosage_change
                                            conditionalPanel(
                                              condition = "((input.cma_class == 'simple' &&
                                                             (input.cma_to_compute == 'CMA5' ||
                                                              input.cma_to_compute == 'CMA6' ||
                                                              input.cma_to_compute == 'CMA7' ||
                                                              input.cma_to_compute == 'CMA8' ||
                                                              input.cma_to_compute == 'CMA9')) ||
                                                            (input.cma_class != 'simple' &&
                                                             (input.cma_to_compute_within_complex == 'CMA5' ||
                                                              input.cma_to_compute_within_complex == 'CMA6' ||
                                                              input.cma_to_compute_within_complex == 'CMA7' ||
                                                              input.cma_to_compute_within_complex == 'CMA8' ||
                                                              input.cma_to_compute_within_complex == 'CMA9')))",

                                              div(id='cma_plus_section', style="cursor: pointer;",
                                                  span(title='What type of carry over to consider?', h4("Carry over"), style="color:DarkBlue"),
                                                  div(title='Click to unfold...', id="cma_plus_unfold_icon", icon("option-horizontal", lib="glyphicon"))),

                                              shinyjs::hidden(div(id="cma_plus_contents",
                                                                  # Carry-over for same treat only?
                                                                  div(title='Carry over only across treatments of the same type?',
                                                                      shinyWidgets::materialSwitch(inputId="carry_only_for_same_medication",
                                                                                                   label="For same treat. only?",
                                                                                                   value=FALSE, status="primary", right=TRUE)),

                                                                  # Consider dosage changes?
                                                                  div(title='Consider dosage change when computing the carry over?',
                                                                      shinyWidgets::materialSwitch(inputId="consider_dosage_change",
                                                                                                   label="Consider dose changes?",
                                                                                                   value=FALSE, status="primary", right=TRUE)),

                                                                  hr()
                                              ))
                                            ),


                                            # Per episode only ----
                                            conditionalPanel(
                                              condition = "(input.cma_class == 'per episode')",

                                              div(id='episodes_section', style="cursor: pointer;",
                                                  span(title='Parameters defining treatment episodes', h4("Define episodes"), style="color:DarkBlue"),
                                                  div(title='Click to unfold...', id="episodes_unfold_icon", icon("option-horizontal", lib="glyphicon"))),

                                              shinyjs::hidden(div(id="episodes_contents",
                                                                  # Does treat. change start new episode?
                                                                  conditionalPanel(
                                                                    condition="output.is_treat_class_defined",
                                                                    div(title='Does changing the treatment type trigger a new episode?',
                                                                        shinyWidgets::materialSwitch(inputId="medication_change_means_new_treatment_episode",
                                                                                                     label="Treat. change starts new episode?",
                                                                                                     value=FALSE, status="primary", right=TRUE))
                                                                  ),

                                                                  # Does dosage change start new episode?
                                                                  conditionalPanel(
                                                                    condition="output.is_dose_defined",
                                                                    div(title='Does changing the dose trigger a new episode?',
                                                                        shinyWidgets::materialSwitch(inputId="dosage_change_means_new_treatment_episode",
                                                                                                     label="Dose change starts new episode?",
                                                                                                     value=FALSE, status="primary", right=TRUE))
                                                                  ),

                                                                  # Max. permis. gap duration unit
                                                                  div(title='The unit of the maximum permissible gap after which a new episode is triggered: either absolute ("days", "weeks", "months" or "years") or relative ("percent")',
                                                                      selectInput(inputId="maximum_permissible_gap_unit",
                                                                                  label="Max. gap duration unit",
                                                                                  choices=c("days", "weeks", "months", "years", "percent"),
                                                                                  selected="days")),

                                                                  # Max. permissible gap
                                                                  div(title='The maximum permissible gap after which a new episode is triggered (in the above-selected units)',
                                                                      numericInput(inputId="maximum_permissible_gap",
                                                                                   label="Max. gap duration",
                                                                                   value=0, min=0, max=NA, step=1)),

                                                                  # Append max gap duration?
                                                                  div(title='Append the maximum permissible gap to the episodes?',
                                                                      shinyWidgets::materialSwitch(inputId="maximum_permissible_gap_append",
                                                                                                   label="Append gap?",
                                                                                                   value=FALSE, status="primary", right=TRUE)),

                                                                  # Plot CMA as histogram
                                                                  div(title='Show the distribution of estimated CMAs across episodes as a histogram or barplot?',
                                                                      shinyWidgets::materialSwitch(inputId="plot_CMA_as_histogram_episodes",
                                                                                                   label="Plot CMA as histogram?",
                                                                                                   value=FALSE, status="primary", right=TRUE)),

                                                                  hr()
                                              ))
                                            ),


                                            # Sliding window only ----
                                            conditionalPanel(
                                              condition = "(input.cma_class == 'sliding window')",

                                              div(id='sliding_windows_section', style="cursor: pointer;",
                                                  span(title='Parameters defining the sliding windows (shortened to SW))', h4("Define sliding windows (SW)"), style="color:DarkBlue"),
                                                  div(title='Click to unfold...', id="sliding_windows_unfold_icon", icon("option-horizontal", lib="glyphicon"))),

                                              shinyjs::hidden(div(id="sliding_windows_contents",
                                                                  # Sliding window start
                                                                  div(title='The unit of the start of the sliding windows ("days", "weeks", "months" or "years")',
                                                                      selectInput(inputId="sliding_window_start_unit",
                                                                                  label="SW start unit",
                                                                                  choices=c("days", "weeks", "months", "years"),
                                                                                  selected="days")),

                                                                  # Select the number of units
                                                                  div(title='Select the number of units defining the start of the sliding windows',
                                                                      numericInput(inputId="sliding_window_start",
                                                                                   label="SW start",
                                                                                   value=0, min=0, max=NA, step=30)),

                                                                  # Sliding window duration
                                                                  div(title='The unit of the duration of the sliding windows ("days", "weeks", "months" or "years")',
                                                                      selectInput(inputId="sliding_window_duration_unit",
                                                                                  label="SW duration unit",
                                                                                  choices=c("days", "weeks", "months", "years"),
                                                                                  selected="days")),

                                                                  # Select the number of units
                                                                  div(title='Select the number of units defining the duration of the sliding windows',
                                                                      numericInput(inputId="sliding_window_duration",
                                                                                   label="SW duration",
                                                                                   value=90, min=0, max=NA, step=30)),

                                                                  # Steps choice
                                                                  div(title='How is the step of the sliding windows defined: by giving their number or their duration?',
                                                                      selectInput(inputId="sliding_window_step_choice",
                                                                                  label="Define SW steps by",
                                                                                  choices=c("number of steps", "duration of a step"),
                                                                                  selected="the duration of a step")),

                                                                  # Sliding window steps
                                                                  conditionalPanel(
                                                                    condition = "(input.sliding_window_step_choice == 'duration of a step')",
                                                                    div(title='The unit of the sliding windows step duration ("days", "weeks", "months" or "years")',
                                                                        selectInput(inputId="sliding_window_step_unit",
                                                                                    label="SW step unit",
                                                                                    choices=c("days", "weeks", "months", "years"),
                                                                                    selected="days")),
                                                                    div(title='The sliding windows duration (in the units selected above)',
                                                                        numericInput(inputId="sliding_window_step_duration",
                                                                                     label="SW step duration",
                                                                                     value=60, min=0, max=NA, step=7))
                                                                  ),
                                                                  conditionalPanel(
                                                                    condition = "(input.sliding_window_step_choice == 'number of steps')",
                                                                    div(title='The number of sliding windows steps',
                                                                        numericInput(inputId="sliding_window_no_steps",
                                                                                     label="SW number of steps",
                                                                                     value=10, min=0, max=NA, step=1))
                                                                  ),

                                                                  # Plot CMA as histogram
                                                                  div(title='Show the distribution of estimated CMAs across sliding windows as a histogram or barplot?',
                                                                      shinyWidgets::materialSwitch(inputId="plot_CMA_as_histogram_sliding_window",
                                                                                                   label="Plot CMA as histogram?",
                                                                                                   value=TRUE, status="primary", right=TRUE)),

                                                                  hr()
                                              ))

                                            ),


                                            # Align all patients ----
                                            conditionalPanel(
                                              condition="(input.patient.length > 1)",

                                              div(id='align_section', style="cursor: pointer;",
                                                  span(title='Align patients for clearer plots?', h4("Align patients"), style="color:DarkBlue"),
                                                  div(title='Click to unfold...', id="align_unfold_icon", icon("option-horizontal", lib="glyphicon"))),

                                              shinyjs::hidden(div(id="align_contents",
                                                                  div(title='Should all the patients be vertically aligned relative to their first event?',
                                                                      shinyWidgets::materialSwitch(inputId="plot_align_all_patients",
                                                                                                   label="Align patients?",
                                                                                                   value=FALSE, status="primary", right=TRUE)),

                                                                  # Align al patients
                                                                  conditionalPanel(
                                                                    condition="input.plot_align_all_patients",
                                                                    div(title='Should the first event (across patients) be considered as the origin of time?',
                                                                        shinyWidgets::materialSwitch(inputId="plot_align_first_event_at_zero",
                                                                                                     label="Align 1st event at 0?",
                                                                                                     value=FALSE, status="primary", right=TRUE))
                                                                  ),

                                                                  hr()
                                              ))
                                            ),


                                            # Duration and period ----
                                            div(id='duration_period_section', style="cursor: pointer;",
                                                span(title='Duration and period', h4("Duration & period"), style="color:DarkBlue"),
                                                div(title='Click to unfold...', id="duration_period_unfold_icon", icon("option-horizontal", lib="glyphicon"))),

                                            shinyjs::hidden(div(id="duration_period_contents",
                                                                # Duration:
                                                                div(title='The duration to plot (in days), or 0 to determine it from the data',
                                                                    numericInput(inputId="duration",
                                                                                 label="Duration (in days)",
                                                                                 value=0, min=0, max=NA, step=90)),

                                                                # Period:
                                                                conditionalPanel(
                                                                  condition="(input.patient.length > 1) && input.plot_align_all_patients",

                                                                  div(title='Draw vertical grid at regular interval as days since the earliest date',
                                                                      selectInput(inputId="show_period_align_patients",
                                                                                  label="Show period as",
                                                                                  choices=c("days"), # only days are possible when aligning the patients
                                                                                  selected="days"))
                                                                ),
                                                                conditionalPanel(
                                                                  condition="!((input.patient.length > 1) && input.plot_align_all_patients)", # ELSE

                                                                  div(title='Draw vertical grid at regular interval as days since the earliest date or as actual dates?',
                                                                      selectInput(inputId="show_period",
                                                                                  label="Show period as",
                                                                                  choices=c("days", "dates"),
                                                                                  selected="days"))
                                                                ),

                                                                div(title='The interval (in days) at which to draw the vertical grid (or 0 for no grid)',
                                                                    numericInput(inputId="period_in_days",
                                                                                 label="Period (in days)",
                                                                                 value=90, min=0, max=NA, step=30)),

                                                                hr()
                                            )),


                                            # CMA estimate ----
                                            conditionalPanel(
                                              condition="(input.cma_class == 'per episode') || (input.cma_class == 'sliding window') || (input.cma_class == 'simple' && input.cma_to_compute != 'CMA0')",

                                              div(id='cma_estimate_section', style="cursor: pointer;",
                                                  span(title='How to show the CMA estimates', h4("CMA estimates"), style="color:DarkBlue"),
                                                  div(title='Click to unfold...', id="cma_estimate_unfold_icon", icon("option-horizontal", lib="glyphicon"))),

                                              shinyjs::hidden(div(id="cma_estimate_contents",
                                                                  conditionalPanel(
                                                                    condition="input.cma_class == 'simple'",

                                                                    div(title='Print the CMA estimate next to the participant\'s ID?',
                                                                        shinyWidgets::materialSwitch(inputId="print_cma",
                                                                                                     label="Print CMA?",
                                                                                                     value=TRUE, status="primary", right=TRUE))
                                                                  ),

                                                                  div(title='Plot the CMA estimate next to the participant\'s ID?',
                                                                      shinyWidgets::materialSwitch(inputId="plot_cma",
                                                                                                   label="Plot CMA?",
                                                                                                   value=TRUE, status="primary", right=TRUE)),

                                                                  conditionalPanel(
                                                                    condition="input.cma_class != 'simple'",

                                                                    div(title='Show the "partial" CMA estimates as stacked bars?',
                                                                        shinyWidgets::materialSwitch(inputId="plot_cma_stacked",
                                                                                                     label="... as stacked bars?",
                                                                                                     value=TRUE, status="primary", right=TRUE)),

                                                                    div(title='Show the "partial" CMA estimates as overlapping segments?',
                                                                        shinyWidgets::materialSwitch(inputId="plot_cma_overlapping",
                                                                                                     label="... as overlapping lines?",
                                                                                                     value=FALSE, status="primary", right=TRUE)),

                                                                    div(title='Show the "partial" CMA estimates as time series?',
                                                                        shinyWidgets::materialSwitch(inputId="plot_cma_timeseries",
                                                                                                     label="... as time series?",
                                                                                                     value=FALSE, status="primary", right=TRUE))

                                                                  ),

                                                                  hr()
                                              ))
                                            ),


                                            # Dose ----
                                            conditionalPanel(
                                              condition="output.is_dose_defined && (input.cma_class == 'per episode' || input.cma_class == 'sliding window' || (input.cma_class == 'simple' && (input.cma_to_compute == 'CMA0' || input.cma_to_compute == 'CMA5' || input.cma_to_compute == 'CMA6' || input.cma_to_compute == 'CMA7' || input.cma_to_compute == 'CMA8' || input.cma_to_compute == 'CMA9')))",

                                              div(id='dose_section', style="cursor: pointer;",
                                                  span(title='Show dose', h4("Show dose"), style="color:DarkBlue"),
                                                  div(title='Click to unfold...', id="dose_unfold_icon", icon("option-horizontal", lib="glyphicon"))),

                                              shinyjs::hidden(div(id="dose_contents",
                                                                  # Print dose?
                                                                  div(title='Print the dosage (i.e., the actual numeric values)?',
                                                                      shinyWidgets::materialSwitch(inputId="print_dose",
                                                                                                   label="Print it?",
                                                                                                   value=FALSE, status="primary", right=TRUE)),

                                                                  # Print dose attributes
                                                                  conditionalPanel(
                                                                    condition="input.print_dose",

                                                                    div(title='Relative font size (please note that a size of 0 is autmatically forced to 0.01)',
                                                                        numericInput(inputId="cex_dose",
                                                                                     label="Font size",
                                                                                     value=0.75, min=0.0, max=NA, step=0.25)),

                                                                    div(title='Dose text outline color',
                                                                        colourpicker::colourInput(inputId="print_dose_outline_col",
                                                                                                  label="Outline color",
                                                                                                  value="white")),

                                                                    div(title='Print the dose centered on the event?',
                                                                        shinyWidgets::materialSwitch(inputId="print_dose_centered",
                                                                                                     label="Centered?",
                                                                                                     value=FALSE, status="primary", right=TRUE)),

                                                                    hr()
                                                                  ),

                                                                  # Plot dose?
                                                                  div(title='Represent the dose as event line width?',
                                                                      shinyWidgets::materialSwitch(inputId="plot_dose",
                                                                                                   label="As line width?",
                                                                                                   value=FALSE, status="primary", right=TRUE)),

                                                                  # Plot dose attributes
                                                                  conditionalPanel(
                                                                    condition="input.plot_dose",

                                                                    div(title='What line width corresponds to the maximum dose?',
                                                                        numericInput(inputId="lwd_event_max_dose",
                                                                                     label="Max dose width",
                                                                                     value=8, min=1, max=NA, step=1)),

                                                                    div(title='Consider maximum dose globally or per each medication class separately?',
                                                                        shinyWidgets::materialSwitch(inputId="plot_dose_lwd_across_medication_classes",
                                                                                                     label="Global max?",
                                                                                                     value=FALSE, status="primary", right=TRUE))
                                                                  ),

                                                                  hr()
                                              ))
                                            ),


                                            # Legend ----
                                            div(id='legend_section', style="cursor: pointer;",
                                                span(title='The legend', h4("Legend"), style="color:DarkBlue"),
                                                div(title='Click to unfold...', id="legend_unfold_icon", icon("option-horizontal", lib="glyphicon"))),

                                            shinyjs::hidden(div(id="legend_contents",
                                                                # Show legend?
                                                                div(title='Display the plot legend?',
                                                                    shinyWidgets::materialSwitch(inputId="show_legend",
                                                                                                 label="Show legend?",
                                                                                                 value=TRUE, status="primary", right=TRUE)),

                                                                # Legend attributes
                                                                conditionalPanel(
                                                                  condition="input.show_legend",

                                                                  div(title='The legend\'s x position',
                                                                      selectInput(inputId="legend_x",
                                                                                  label="Legend x",
                                                                                  choices=c("left", "right"),
                                                                                  selected="right")),

                                                                  div(title='The legend\'s y position',
                                                                      selectInput(inputId="legend_y",
                                                                                  label="Legend y",
                                                                                  choices=c("bottom", "top"),
                                                                                  selected="bottom")),

                                                                  div(title='Relative font size of legend title (please note that a size of 0 is autmatically forced to 0.01)',
                                                                      numericInput(inputId="legend_cex_title",
                                                                                   label="Title font size",
                                                                                   value=1.0, min=0.0, max=NA, step=0.25)),

                                                                  div(title='Relative font size of legend text and symbols (please note that a size of 0 is autmatically forced to 0.01)',
                                                                      numericInput(inputId="legend_cex",
                                                                                   label="Text font size",
                                                                                   value=0.75, min=0.0, max=NA, step=0.25)),

                                                                  div(title='The legend\'s background opacity (between 0.0=fully transparent and 1.0=fully opaque)',
                                                                      sliderInput(inputId="legend_bkg_opacity",
                                                                                  label="Legend bkg. opacity",
                                                                                  min=0.0, max=1.0, value=0.5, step=0.1, round=TRUE))
                                                                ),

                                                                hr()
                                            )),


                                            # Aesthetics ----
                                            div(id='aesthetics_section', style="cursor: pointer;",
                                                span(title='Colors, fonts, line style...', h4("Aesthetics"), style="color:DarkBlue"),
                                                div(title='Click to unfold...', id="aesthetics_unfold_icon", icon("option-horizontal", lib="glyphicon"))),

                                            shinyjs::hidden(div(id="aesthetics_contents",
                                                                ## Show CMA type in the title?
                                                                #div(title='Show CMA type in the plot title?',
                                                                #         checkboxInput(inputId="show_cma",
                                                                #            label="Show CMA in title?",
                                                                #            value=TRUE)),

                                                                div(title='Colors or grayscale?',
                                                                    span(p("Color or grayscale"), style="color:RoyalBlue; font-weight: bold;")),

                                                                # Draw grayscale?
                                                                div(title='Draw using only grayscales? (overrides everything else)',
                                                                    shinyWidgets::materialSwitch(inputId="bw_plot",
                                                                                                 label="Grayscale?",
                                                                                                 value=FALSE, status="primary", right=TRUE)),

                                                                hr(),

                                                                conditionalPanel(
                                                                  condition="!(input.bw_plot)",

                                                                  div(title='Colors for catgories of treatment',
                                                                      span(p("Treatment colors"), style="color:RoyalBlue; font-weight: bold;")),

                                                                  # Colors for categories:
                                                                  div(title='The color for missing data',
                                                                      colourpicker::colourInput(inputId="col_na",
                                                                                                label="Missing data color",
                                                                                                value="lightgray")),

                                                                  # Unspecified category name:
                                                                  div(title='The label of the unspecified (generic) treatment category',
                                                                      textInput(inputId="unspecified_category_label",
                                                                                label="Unspec. cat. label",
                                                                                value="drug")),

                                                                  # The colour palette for treatment types:
                                                                  conditionalPanel(
                                                                    condition="output.is_treat_class_defined",
                                                                    div(title='Color palette for mapping treatment categories to colors (the last two are colour-blind-friendly and provided by ).\nPlease see R\'s help for more info about each palette (first 5 are provided by the standard library, and the last 5 are in package "viridisLight").\nThe mapping is done automatically based on the alphabetic ordering of the category names.',
                                                                        selectInput(inputId="col_cats",
                                                                                    label="Treatment palette",
                                                                                    choices=c("rainbow", "heat.colors", "terrain.colors", "topo.colors", "cm.colors", "magma", "inferno", "plasma", "viridis", "cividis"),
                                                                                    selected="rainbow"))
                                                                  ),

                                                                  hr()
                                                                ),

                                                                div(title='Event visual attributes',
                                                                    span(p("Events"), style="color:RoyalBlue; font-weight: bold;")),

                                                                # Event style:
                                                                div(title='Event line style',
                                                                    #selectInput(inputId="lty_event", # using UNICODE character
                                                                    #            label="Event line style",
                                                                    #            choices=c("\U00A0\U00A0\U00A0\U00A0\U00A0 blank"="blank",
                                                                    #                      "\U2E3B solid"="solid",
                                                                    #                      "\U2012\U2009\U2012\U2009\U2012\U2009\U2012 dashed"="dashed",
                                                                    #                      "\U00B7\U00B7\U00B7\U00B7\U00B7\U00B7\U00B7 dotted"="dotted",
                                                                    #                      "\U00B7\U2012\U00B7\U2012\U00B7\U2012 dotdash"="dotdash",
                                                                    #                      "\U2014\U2014\U2009\U2014\U2014 longdash"="longdash",
                                                                    #                      "\U2012\U2009\U2014\U2009\U2012\U2009\U2014 twodash"="twodash"),
                                                                    #            selected="solid")),
                                                                    shinyWidgets::pickerInput(inputId="lty_event", # using actual images
                                                                                              label="Event line style",
                                                                                              multiple=FALSE,
                                                                                              choices=names(line.types),
                                                                                              choicesOpt=list(content=lapply(1:length(line.types),
                                                                                                                             function(i)
                                                                                                                               HTML(paste0("<img src='",
                                                                                                                                           line.types[i],
                                                                                                                                           "' width=50 height=10/>",
                                                                                                                                           names(line.types[i]))))),
                                                                                              selected="solid")),

                                                                div(title='Event line width',
                                                                    numericInput(inputId="lwd_event",
                                                                                 label="Event line width",
                                                                                 value=2, min=0, max=NA, step=1)),

                                                                div(title='Event start symbol (most commonly used)...',
                                                                    #selectInput(inputId="pch_start_event",
                                                                    #            label="Event start",
                                                                    #            choices=c("none"=NA, # using UNICODE characters
                                                                    #                      "\UFF0B plus"=3,
                                                                    #                      "\U00D7times"=4,
                                                                    #                      "\U2733\UFE0E star"=8,
                                                                    #                      "\U25FB\UFE0E square"=0,
                                                                    #                      "\U25CB circle"=1,
                                                                    #                      "\U25B3 up triangle"=2,
                                                                    #                      "\U25BD down triangle"=6,
                                                                    #                      "\U25C7 diamond"=5,
                                                                    #                      "\U25A0 fill square"=15,
                                                                    #                      "\U25CF fill small circle"=20,
                                                                    #                      "\U26AB\UFE0E fill med circle"=16,
                                                                    #                      "\U2B24 fill big circle"=19,
                                                                    #                      "\U25B2 fill triangle"=17,
                                                                    #                      "\U25C6 fill diamond"=18,
                                                                    #                      "\U229E square plus"=12,
                                                                    #                      "\U22A0 square times"=7,
                                                                    #                      "\U2A01 circle plus"=10,
                                                                    #                      "\U2A02 circle times"=13,
                                                                    #                      "\U2721 David star"=11),
                                                                    #            selected=15)),
                                                                    shinyWidgets::pickerInput(inputId="pch_start_event", # using actual images
                                                                                              label="Event start",
                                                                                              multiple=FALSE,
                                                                                              choices=point.types,
                                                                                              choicesOpt=list(content=lapply(1:length(point.types),
                                                                                                                             function(i)
                                                                                                                               HTML(paste0("<img src='symbols/pch-",
                                                                                                                                           point.types[i],
                                                                                                                                           ".png' width=15 height=15/>",
                                                                                                                                           names(point.types[i]))))),
                                                                                              selected=15)),
                                                                div(title='Event end symbol (most commonly used)...',
                                                                  # selectInput(inputId="pch_end_event",
                                                                  #           label="Event end",
                                                                  #            choices=c("none"=NA,
                                                                  #                       "\UFF0B plus"=3,
                                                                  #                        "\U00D7times"=4,
                                                                  #                        "\U2733\UFE0E star"=8,
                                                                  #                        "\U25FB\UFE0E square"=0,
                                                                  #                        "\U25CB circle"=1,
                                                                  #                        "\U25B3 up triangle"=2,
                                                                  #                        "\U25BD down triangle"=6,
                                                                  #                        "\U25C7 diamond"=5,
                                                                  #                        "\U25A0 fill square"=15,
                                                                  #                        "\U25CF fill small circle"=20,
                                                                  #                        "\U26AB\UFE0E fill med circle"=16,
                                                                  #                        "\U2B24 fill big circle"=19,
                                                                  #                        "\U25B2 fill triangle"=17,
                                                                  #                        "\U25C6 fill diamond"=18,
                                                                  #                       "\U229E square plus"=12,
                                                                  #                       "\U22A0 square times"=7,
                                                                  #                        "\U2A01 circle plus"=10,
                                                                  #                       "\U2A02 circle times"=13,
                                                                  #                        "\U2721 David star"=11),
                                                                  #             selected=16)),
                                                                    shinyWidgets::pickerInput(inputId="pch_end_event", # using actual images
                                                                                              label="Event end",
                                                                                              multiple=FALSE,
                                                                                              choices=point.types,
                                                                                              choicesOpt=list(content=lapply(1:length(point.types),
                                                                                                                             function(i)
                                                                                                                               HTML(paste0("<img src='symbols/pch-",
                                                                                                                                           point.types[i],
                                                                                                                                           ".png' width=15 height=15/>",
                                                                                                                                           names(point.types[i]))))),
                                                                                              selected=16)),

                                                                hr(),

                                                                # Continuation (CMA0 and complex only):
                                                                conditionalPanel(
                                                                  condition="(input.cma_class == 'per eipsode') || (input.cma_class == 'sliding window') || (input.cma_class == 'simple' && input.cma_to_compute == 'CMA0')",

                                                                  div(title='Continuation visual attributes',
                                                                      span(p("Continuation"), style="color:RoyalBlue; font-weight: bold;")),

                                                                  conditionalPanel(
                                                                    condition="!(input.bw_plot)",

                                                                    div(title='The color of continuation lines connecting consecutive events',
                                                                        colourpicker::colourInput(inputId="col_continuation",
                                                                                                  label="Cont. line color",
                                                                                                  value="black"))
                                                                  ),
                                                                  div(title='The line style of continuation lines connecting consecutive events',
                                                                      #selectInput(inputId="lty_continuation",
                                                                      #            label="Cont. line style",
                                                                      #            choices=c("\U00A0\U00A0\U00A0\U00A0\U00A0 blank"="blank",
                                                                      #                      "\U2E3B solid"="solid",
                                                                      #                      "\U2012\U2009\U2012\U2009\U2012\U2009\U2012 dashed"="dashed",
                                                                      #                      "\U00B7\U00B7\U00B7\U00B7\U00B7\U00B7\U00B7 dotted"="dotted",
                                                                      #                      "\U00B7\U2012\U00B7\U2012\U00B7\U2012 dotdash"="dotdash",
                                                                      #                      "\U2014\U2014\U2009\U2014\U2014 longdash"="longdash",
                                                                      #                      "\U2012\U2009\U2014\U2009\U2012\U2009\U2014 twodash"="twodash"),
                                                                      #            selected="dotted")),
                                                                      shinyWidgets::pickerInput(inputId="lty_continuation", # using actual images
                                                                                                label="Cont. line style",
                                                                                                multiple=FALSE,
                                                                                                choices=names(line.types),
                                                                                                choicesOpt=list(content=lapply(1:length(line.types),
                                                                                                                               function(i)
                                                                                                                                 HTML(paste0("<img src='",
                                                                                                                                             line.types[i],
                                                                                                                                             "' width=50 height=10/>",
                                                                                                                                             names(line.types[i]))))),
                                                                                                selected="dotted")),

                                                                  div(title='The line width of continuation lines connecting consecutive events',
                                                                      numericInput(inputId="lwd_continuation",
                                                                                   label="Cont. line width",
                                                                                   value=1, min=0, max=NA, step=1)),

                                                                  hr()
                                                                ),

                                                                # Show event intervals:
                                                                conditionalPanel(
                                                                  condition="(input.cma_class == 'simple' && input.cma_to_compute != 'CMA0') || (input.cma_class == 'per episode' || input.cma_class == 'sliding window')",

                                                                  div(title='Event intervals',
                                                                      span(p("Event intervals"), style="color:RoyalBlue; font-weight: bold;")),

                                                                  div(title='Show the event intervals?',
                                                                      shinyWidgets::materialSwitch(inputId="show_event_intervals",
                                                                                                   label="Show event interv.?",
                                                                                                   value=TRUE, status="primary", right=TRUE)),


                                                                  # What to do with overlapping event interval estimates (for sliding windows and per episode only)?
                                                                  conditionalPanel(
                                                                    condition="input.show_event_intervals && (input.cma_class == 'per episode' || input.cma_class == 'sliding window')",

                                                                    div(title='For sliding windows and per episode, for the overlapping event interval estimates, which one to show?',
                                                                        selectInput(inputId="overlapping_evint",
                                                                                    label="The estimate for which windows/episode?",
                                                                                    choices=c("first"="first",
                                                                                              "last"="last",
                                                                                              "minimizes gap"="min gap",
                                                                                              "maximizes gap"="max gap",
                                                                                              "average"="average"),
                                                                                    selected="first"))
                                                                  ),

                                                                  hr()
                                                                ),

                                                                # Font sizes:
                                                                div(title='Font sizes',
                                                                    span(p("Font sizes"), style="color:RoyalBlue; font-weight: bold;")),
                                                                div(title='Relative font size of general plotting text (please note that a size of 0 is autmatically forced to 0.01)',
                                                                    numericInput(inputId="cex",
                                                                                 label="General font size",
                                                                                 value=1.0, min=0.0, max=NA, step=0.25)),
                                                                div(title='Relative font size of axis text (please note that a size of 0 is autmatically forced to 0.01)',
                                                                    numericInput(inputId="cex_axis",
                                                                                 label="Axis font size",
                                                                                 value=0.75, min=0.0, max=NA, step=0.25)),
                                                                div(title='Relative font size of axis labels text (please note that a size of 0 is autmatically forced to 0.01)',
                                                                    numericInput(inputId="cex_lab",
                                                                                 label="Axis labels font size",
                                                                                 value=1.0, min=0.0, max=NA, step=0.25)),
                                                                div(title='Force showing text elements, even if they might be too small or ulgy?',
                                                                    shinyWidgets::materialSwitch(inputId="force_draw_text",
                                                                                                 label="Force drawing text?",
                                                                                                 value=FALSE, status="primary", right=TRUE)),

                                                                hr(),

                                                                # Follow-up window:
                                                                div(title='Follow-up window visual attributes',
                                                                    span(p("FUW visuals"), style="color:RoyalBlue; font-weight: bold;")),
                                                                div(title='Show the follow-up window?',
                                                                    shinyWidgets::materialSwitch(inputId="highlight_followup_window",
                                                                                                 label="Show FUW?",
                                                                                                 value=TRUE, status="primary", right=TRUE)),
                                                                conditionalPanel(
                                                                  condition="input.highlight_followup_window &&  !(input.bw_plot)",

                                                                  div(title='The color of the follow-up window',
                                                                      colourpicker::colourInput(inputId="followup_window_col",
                                                                                                label="FUW color",
                                                                                                value="green"))
                                                                ),

                                                                hr(),

                                                                # Observation window:
                                                                div(title='Observation window visual attributes',
                                                                    span(p("OW visuals"), style="color:RoyalBlue; font-weight: bold;")),
                                                                div(title='Show the observation window?',
                                                                    shinyWidgets::materialSwitch(inputId="highlight_observation_window",
                                                                                                 label="Show OW?",
                                                                                                 value=TRUE, status="primary", right=TRUE)),
                                                                conditionalPanel(
                                                                  condition="input.highlight_observation_window",

                                                                  conditionalPanel(
                                                                    condition="!(input.bw_plot)",

                                                                    div(title='The color of the observation window',
                                                                        colourpicker::colourInput(inputId="observation_window_col",
                                                                                                  label="OW color",
                                                                                                  value="yellow"))
                                                                  ),
                                                                  #div(title='The density of the hashing lines (number of lines per inch) used to draw the observation window',
                                                                  #    numericInput(inputId="observation_window_density",
                                                                  #                 label="OW hash dens.",
                                                                  #                 value=35, min=0, max=NA, step=5)),
                                                                  #div(title='The orientation of the hashing lines (in degrees) used to draw the observation window',
                                                                  #    sliderInput(inputId="observation_window_angle",
                                                                  #                label="OW hash angle",
                                                                  #                min=-90.0, max=90.0, value=-30, step=15, round=TRUE)),
                                                                  div(title='The observation window\'s background opacity (between 0.0=fully transparent and 1.0=fully opaque)',
                                                                      sliderInput(inputId="observation_window_opacity",
                                                                                  label="OW opacity",
                                                                                  min=0.0, max=1.0, value=0.3, step=0.1, round=TRUE))
                                                                ),

                                                                hr(),

                                                                # Real observation window:
                                                                conditionalPanel(
                                                                  condition="(input.cma_class == 'simple' && input.cma_to_compute == 'CMA8')",

                                                                  div(title='Real observation window visual attributes',
                                                                      span(p("Real OW visuals"), style="color:RoyalBlue; font-weight: bold;")),
                                                                  div(title='Show the real observation window (the color and transparency are the same as for the theoretial observation window but the hasing pattern can be different)?',
                                                                      shinyWidgets::materialSwitch(inputId="show_real_obs_window_start",
                                                                                                   label="Show real OW?",
                                                                                                   value=TRUE, status="primary", right=TRUE)),
                                                                  #conditionalPanel(
                                                                  #  condition="input.show_real_obs_window_start",
                                                                  #
                                                                  #  div(title='The density of the hashing lines (number of lines per inch) used to draw the real observation window',
                                                                  #      numericInput(inputId="real_obs_window_density",
                                                                  #                   label="Real OW hash dens.",
                                                                  #                   value=35, min=0, max=NA, step=5)),
                                                                  #  div(title='The orientation of the hashing lines (in degrees) used to draw the real observation window',
                                                                  #      sliderInput(inputId="real_obs_window_angle",
                                                                  #                  label="Real OW hash angle",
                                                                  #                  min=-90.0, max=90.0, value=30, step=15, round=TRUE))
                                                                  #),

                                                                  hr()
                                                                ),

                                                                # Axis labels & title
                                                                div(title='Axis labels and title',
                                                                    span(p("Axis labels and title"), style="color:RoyalBlue; font-weight: bold;")),

                                                                div(title='Show to main title?',
                                                                    shinyWidgets::materialSwitch(inputId="show_plot_title",
                                                                                                 label="Show title?",
                                                                                                 value=TRUE, status="primary", right=TRUE)),

                                                                div(title='Show to x axis label?',
                                                                    shinyWidgets::materialSwitch(inputId="show_xlab",
                                                                                                 label="Show x label?",
                                                                                                 value=TRUE, status="primary", right=TRUE)),

                                                                div(title='Show to y axis label?',
                                                                    shinyWidgets::materialSwitch(inputId="show_ylab",
                                                                                                 label="Show y label?",
                                                                                                 value=TRUE, status="primary", right=TRUE)),
                                                                hr(),


                                                                # CMA estimate aesthetics:
                                                                conditionalPanel(
                                                                  condition="(input.cma_class == 'per episode') || (input.cma_class == 'sliding window') || (input.cma_class == 'simple' && input.cma_to_compute != 'CMA0')",

                                                                  div(title='CMA estimate visual attributes',
                                                                      span(p("CMA estimate"), style="color:RoyalBlue; font-weight: bold;")),

                                                                  conditionalPanel(
                                                                    condition="input.plot_cma",

                                                                    div(title='Relative font size of CMA estimate for per episode and sliding windows (please note that a size of 0 is autmatically forced to 0.01)',
                                                                        numericInput(inputId="cma_cex",
                                                                                     label="CMA font size",
                                                                                     value=0.5, min=0.0, max=NA, step=0.25)),

                                                                    hr()
                                                                  ),

                                                                  conditionalPanel(
                                                                    condition="input.plot_cma",

                                                                    div(title='The horizontal percent of the total plotting area to be taken by the CMA plot',
                                                                        sliderInput(inputId="cma_plot_ratio",
                                                                                    label="CMA plot area %",
                                                                                    min=0, max=100, value=10, step=5, round=TRUE)),

                                                                    conditionalPanel(
                                                                      condition="!(input.bw_plot)",

                                                                      div(title='The color of the CMA plot',
                                                                          colourpicker::colourInput(inputId="cma_plot_col",
                                                                                                    label="CMA plot color",
                                                                                                    value="lightgreen")),

                                                                      div(title='The color of the CMA plot border',
                                                                          colourpicker::colourInput(inputId="cma_plot_border",
                                                                                                    label="CMA border color",
                                                                                                    value="darkgreen")),

                                                                      div(title='The color of the CMA plot background',
                                                                          colourpicker::colourInput(inputId="cma_plot_bkg",
                                                                                                    label="CMA bkg. color",
                                                                                                    value="aquamarine")),

                                                                      div(title='The color of the CMA plot text',
                                                                          colourpicker::colourInput(inputId="cma_plot_text",
                                                                                                    label="CMA text color",
                                                                                                    value="darkgreen")),

                                                                      conditionalPanel(
                                                                        condition="input.cma_class != 'simple'",

                                                                        conditionalPanel(
                                                                          condition="input.plot_cma_timeseries",

                                                                          hr(),

                                                                          div(title='Plotting the "partial" CMAs as time series...',
                                                                              span(p("Showing CMAs as time series"), style="color:RoyalBlue; font-weight: italic;")),

                                                                          div(title='The vertical space (in text lines) taken by the time series plot of the "partial" CMAs',
                                                                              numericInput(inputId="cma_as_timeseries_vspace",
                                                                                           label="Time series vertical space",
                                                                                           value=10, min=5, max=NA, step=1)),

                                                                          div(title='Show the 0% mark?',
                                                                              shinyWidgets::materialSwitch(inputId="cma_as_timeseries_show_0perc",
                                                                                                           label="Show 0% mark?",
                                                                                                           value=TRUE, status="primary", right=TRUE)),

                                                                          div(title='Show the 100% mark?',
                                                                              shinyWidgets::materialSwitch(inputId="cma_as_timeseries_show_100perc",
                                                                                                           label="Show 100% mark?",
                                                                                                           value=FALSE, status="primary", right=TRUE)),

                                                                          div(title='Should the vertical axis of the time series plot start at 0 or at the minimum actually observed value?',
                                                                              shinyWidgets::materialSwitch(inputId="cma_as_timeseries_start_from_zero",
                                                                                                           label="Start plot from 0?",
                                                                                                           value=TRUE, status="primary", right=TRUE)),

                                                                          div(title='Show time series values as points and lines?',
                                                                              shinyWidgets::materialSwitch(inputId="cma_as_timeseries_show_dots",
                                                                                                           label="Show dots and lines?",
                                                                                                           value=TRUE, status="primary", right=TRUE)),
                                                                          conditionalPanel(
                                                                            condition="input.cma_as_timeseries_show_dots",
                                                                            div(title='The color of the time series dots and lines',
                                                                                colourpicker::colourInput(inputId="cma_as_timeseries_color_dots",
                                                                                                          label="Dots and lines color",
                                                                                                          value="#422CD1"))), # dark blue

                                                                          div(title='Show time series interval?',
                                                                              shinyWidgets::materialSwitch(inputId="cma_as_timeseries_show_interval",
                                                                                                           label="Show intervals?",
                                                                                                           value=TRUE, status="primary", right=TRUE)),

                                                                          conditionalPanel(
                                                                            condition="input.cma_as_timeseries_show_interval",

                                                                            div(title='Which way to show the intervals?',
                                                                                selectInput(inputId="cma_as_timeseries_show_interval_type",
                                                                                            label="Show intervals as",
                                                                                            choices=c("none", "segments", "arrows", "lines", "rectangles"),
                                                                                            selected="segments")),

                                                                            div(title='The color of the time series intervals',
                                                                                colourpicker::colourInput(inputId="cma_as_timeseries_color_intervals",
                                                                                                          label="Intervals color",
                                                                                                          value="blue")),

                                                                            conditionalPanel(
                                                                              condition="input.cma_as_timeseries_show_interval_type == 'segments' || input.cma_as_timeseries_show_interval_type == 'arrows' || input.cma_as_timeseries_show_interval_type == 'lines'",
                                                                              div(title='Line width',
                                                                                  numericInput(inputId="cma_as_timeseries_lwd_intervals",
                                                                                               label="Intervals line width",
                                                                                               value=1.0, min=0.01, max=NA, step=0.25))),

                                                                            conditionalPanel(
                                                                              condition="input.cma_as_timeseries_show_interval_type == 'rectangles'",
                                                                              div(title='Rectangle transparency (0=fully transparent to 1=fully opaque)',
                                                                                  sliderInput(inputId="cma_as_timeseries_alpha_intervals",
                                                                                              label="Intervals transparency",
                                                                                              value=0.25, min=0.00, max=1.00, step=0.05)))
                                                                          ),

                                                                          div(title='Show time series text?',
                                                                              shinyWidgets::materialSwitch(inputId="cma_as_timeseries_show_text",
                                                                                                           label="Show text values?",
                                                                                                           value=TRUE, status="primary", right=TRUE)),

                                                                          conditionalPanel(
                                                                            condition="input.cma_as_timeseries_show_text",
                                                                            div(title='The color of the time series text values',
                                                                                colourpicker::colourInput(inputId="cma_as_timeseries_color_text",
                                                                                                          label="Text values color",
                                                                                                          value="firebrick")))

                                                                        ),

                                                                        conditionalPanel(
                                                                          condition="input.plot_cma_overlapping",

                                                                          hr(),

                                                                          div(title='Plotting the "partial" CMAs as overlapping segments...',
                                                                              span(p("Showing CMAs as overlapping segments"), style="color:RoyalBlue; font-weight: italic;")),

                                                                          div(title='Show the overlapping intervals?',
                                                                              shinyWidgets::materialSwitch(inputId="cma_as_overlapping_show_interval",
                                                                                                           label="Show intervals?",
                                                                                                           value=TRUE, status="primary", right=TRUE)),
                                                                          conditionalPanel(
                                                                            condition="input.cma_as_overlapping_show_interval",
                                                                            div(title='The color of the overlapping intervals',
                                                                                colourpicker::colourInput(inputId="cma_as_overlapping_color_intervals",
                                                                                                          label="Intervals color",
                                                                                                          value="gray70"))),

                                                                          div(title='Show overlapping text?',
                                                                              shinyWidgets::materialSwitch(inputId="cma_as_overlapping_show_text",
                                                                                                           label="Show text values?",
                                                                                                           value=TRUE, status="primary", right=TRUE)),
                                                                          conditionalPanel(
                                                                            condition="input.cma_as_overlapping_show_text",
                                                                            div(title='The color of the overlapping text values',
                                                                                colourpicker::colourInput(inputId="cma_as_overlapping_color_text",
                                                                                                          label="Text values color",
                                                                                                          value="firebrick")))

                                                                        ),

                                                                        conditionalPanel(
                                                                          condition="input.plot_cma_stacked",

                                                                          hr(),

                                                                          div(title='Plotting the "partial" CMAs as stacked bars...',
                                                                              span(p("Showing CMAs as stacked bars"), style="color:RoyalBlue; font-weight: italic;")),

                                                                          div(title='The color of the bar',
                                                                              colourpicker::colourInput(inputId="plot_partial_cmas_as_stacked_col_bars",
                                                                                                        label="Bar color",
                                                                                                        value="gray90")),

                                                                          div(title='The color of the border',
                                                                              colourpicker::colourInput(inputId="plot_partial_cmas_as_stacked_col_border",
                                                                                                        label="Border color",
                                                                                                        value="gray30")),
                                                                          div(title='The color of the text',
                                                                              colourpicker::colourInput(inputId="plot_partial_cmas_as_stacked_col_text",
                                                                                                        label="text color",
                                                                                                        value="black"))

                                                                        )

                                                                      )
                                                                    ),

                                                                    hr()
                                                                  )
                                                                ),

                                                                conditionalPanel(
                                                                  condition="(output.is_mg_defined && input.mg_use_medication_groups)",

                                                                  div(title='Visually grouping medication groups within patients',
                                                                      span(p("Medication groups"), style="color:RoyalBlue; font-weight: bold;")),

                                                                  div(title='The separator color',
                                                                      colourpicker::colourInput(inputId="plot_mg_separator_color",
                                                                                                label="Separator color",
                                                                                                value="blue")),

                                                                  div(title='The line style of the separator',
                                                                      shinyWidgets::pickerInput(inputId="plot_mg_separator_lty", # using actual images
                                                                                                label="Separator line style",
                                                                                                multiple=FALSE,
                                                                                                choices=names(line.types),
                                                                                                choicesOpt=list(content=lapply(1:length(line.types),
                                                                                                                               function(i)
                                                                                                                                 HTML(paste0("<img src='",
                                                                                                                                             line.types[i],
                                                                                                                                             "' width=50 height=10/>",
                                                                                                                                             names(line.types[i]))))),
                                                                                                selected="solid")),

                                                                  div(title='The line width of the separator',
                                                                      numericInput(inputId="plot_mg_separator_lwd",
                                                                                   label="Separator line width",
                                                                                   value=2, min=0, max=NA, step=1)),

                                                                  div(title='The label of the __ALL_OTHERS__ implicit medication group',
                                                                      textInput(inputId="plot_mg_allothers_label",
                                                                                   label="__ALL_OTHERS__ label",
                                                                                   value="*"))

                                            )

                                            )),


                                            # Advanced ----
                                            div(id='advanced_section', style="cursor: pointer;",
                                                span(title='Advanced stuff...', h4("Advanced"), style="color:DarkBlue"),
                                                div(title='Click to unfold...', id="advanced_unfold_icon", icon("option-horizontal", lib="glyphicon"))),

                                            shinyjs::hidden(div(id="advanced_contents",
                                                                div(title='The minimum horizontal plot size (in characters, for the whole duration to plot)',
                                                                    numericInput(inputId="min_plot_size_in_characters_horiz",
                                                                                 label="Min plot size (horiz.)",
                                                                                 value=0, min=0, max=NA, step=1.0)), # should be 10

                                                                div(title='The minimum vertical plot size (in characters, per event)',
                                                                    numericInput(inputId="min_plot_size_in_characters_vert",
                                                                                 label="Min plot size (vert.)",
                                                                                 value=0.0, min=0.0, max=NA, step=0.25)) # should be 0.5
                                            )),


                                            # Allow last comma:
                                            NULL
                                  )
                                )
                       ),

                       # DATA TAB ----
                       tabPanel(span("Data", title="Select the data source (in-memory data frame, file, SQL database)..."),
                                value="sidebar-params-data", icon=icon("hdd", lib="glyphicon"), fluid=TRUE,
                                wellPanel(id = "tPanel2", style = "overflow:scroll; max-height: 90vh; min-height: 50vh",

                                          # Datasource ----
                                          span(title='The data source to use...',
                                               h4("Data source"), style="color:DarkBlue"),

                                          div(title='Select the type of data source (currently supported: in-memory data.frame, flat files or SQL connection)',
                                              selectInput(inputId="datasource_type",
                                                          label="Datasource type",
                                                          choices=c("already in memory",
                                                                    "load from file",
                                                                    "SQL database"),
                                                          selected="already in memory")),

                                          # Use in-memory dataset ----
                                          conditionalPanel(
                                            condition = "(input.datasource_type == 'already in memory')",

                                            # Obligatory stuff:
                                            div(title='Required: select the name of the dataset to use from those available in memory',
                                                selectInput(inputId="dataset_from_memory",
                                                            label="In-memory dataset",
                                                            choices=c("[none]"),
                                                            selected="[none]")),
                                            div(title="Click here to peek at the selected dataset...",
                                                actionButton("dataset_from_memory_peek_button", label="Peek at dataset...", icon=icon("eye-open", lib="glyphicon"))),

                                            hr(),

                                            div(title='Required: select the name of the column containing the patient IDs',
                                                shinyWidgets::pickerInput(inputId="dataset_from_memory_patient_id",
                                                                          label="Patient ID column",
                                                                          choices=c("[none]"),
                                                                          selected="[none]")),

                                            div(title='Required: give the date format.\nBasic codes are:\n  "%d" (day of the month as decimal number),\n  "%m" (month as decimal number),\n  "%b" (Month in abbreviated form),\n  "%B" (month full name),\n  "%y" (year in 2 digit format) and\n  "%Y" (year in 4 digit format).\nSome examples are %m/%d/%Y or %Y%m%d.\nPlease see help entry for "strptime()".',
                                                textInput(inputId="dataset_from_memory_event_format",
                                                          label="Date format",
                                                          value="%m/%d/%Y",
                                                          placeholder="%m/%d/%Y")),

                                            div(title='Required: select the name of the column containing the event dates (in the format defined above)',
                                                shinyWidgets::pickerInput(inputId="dataset_from_memory_event_date",
                                                                          label="Event date column",
                                                                          choices=c("[none]"),
                                                                          selected="[none]")),

                                            div(title='Required: select the name of the column containing the event duration (in days)',
                                                shinyWidgets::pickerInput(inputId="dataset_from_memory_event_duration",
                                                                          label="Event duration column",
                                                                          choices=c("[none]"),
                                                                          selected="[none]")),

                                            div(title='Optional (potentially used by CMA5+): select the name of the column containing the daily dose',
                                                shinyWidgets::pickerInput(inputId="dataset_from_memory_daily_dose",
                                                                          label="Daily dose column",
                                                                          choices=c("[not defined]"),
                                                                          selected="[not defined]")),

                                            div(title='Optional (potentially used by CMA5+): select the name of the column containing the treatment class',
                                                shinyWidgets::pickerInput(inputId="dataset_from_memory_medication_class",
                                                                          label="Treatment class column",
                                                                          choices=c("[not defined]"),
                                                                          selected="[not defined]")),

                                            hr(),

                                            div(title='Validate choices and use the dataset!',
                                                actionButton(inputId="dataset_from_memory_button_use",
                                                             label=strong("Validate & use!"),
                                                             icon=icon("sunglasses", lib="glyphicon"),
                                                             style="color:DarkBlue; border-color:DarkBlue;"),
                                                style="float: center;")
                                          ),

                                          # Use dataset from file ----
                                          conditionalPanel(
                                            condition = "(input.datasource_type == 'load from file')",

                                            # Obligatory stuff:
                                            div(title='Required: which type of file to load?\n.csv and .tsv are prefered, .RData and .rds should pose no problems, but for the others there might be limitations (please see the help for packages "readODS", "SASxport", "readxl" and "heaven").\nPlease note that readling very big files might be very slow and need quite a bit of RAM.',
                                                selectInput(inputId="dataset_from_file_filetype",
                                                            label="Type of file",
                                                            choices=c("Comma/TAB-separated (.csv; .tsv; .txt)",
                                                                      #"R objects from save() (.RData)",
                                                                      "Serialized R object (.rds)",
                                                                      "Open Document Spreadsheet (.ods)",
                                                                      "Microsoft Excel (.xls; .xlsx)",
                                                                      "SPSS (.sav; .por)",
                                                                      "SAS Transport data file (.xpt)",
                                                                      "SAS sas7bdat data file (.sas7bdat)",
                                                                      "Stata (.dta)"),
                                                            selected="Comma/TAB-separated (.csv; .tsv; .txt)")),

                                            conditionalPanel(
                                              condition = "(input.dataset_from_file_filetype == 'Comma/TAB-separated (.csv; .tsv; .txt)')",

                                              div(title='The filed separator (or delimiter); usually, a comma (,) for .csv files and a [TAB] for .tsv files...',
                                                  selectInput(inputId="dataset_from_file_csv_separator",
                                                              label="Filed separator",
                                                              choices=c("[TAB] (\\t)",
                                                                        "comma (,)",
                                                                        "white spaces (1+)",
                                                                        "semicolon (;)",
                                                                        "colon (:)"),
                                                              selected="[TAB] (\\t)")),

                                              div(title='The quote character (if any); usually, single (\' \') or double quotes (" ")...',
                                                  selectInput(inputId="dataset_from_file_csv_quotes",
                                                              label="Quote character",
                                                              choices=c("[none] ()",
                                                                        "singe quotes (' ')",
                                                                        "double quotes (\" \")"),
                                                              selected="[none] ()")),

                                              div(title='The decimal point symbol; usually, the dot (.) or the comma (,)...',
                                                  selectInput(inputId="dataset_from_file_csv_decimal",
                                                              label="Decimal point",
                                                              choices=c("dot (.)",
                                                                        "comma (,)"),
                                                              selected="dot (.)")),

                                              div(title='Should the first row to be considered as the header?',
                                                  shinyWidgets::materialSwitch(inputId="dataset_from_file_csv_header",
                                                                               label=HTML("Header 1<sup>st</sup> row"),
                                                                               value=TRUE, status="primary", right=TRUE)),

                                              conditionalPanel(
                                                condition = "(input.dataset_from_file_csv_quotes != '[none] ()')",
                                                div(title='Should the leading and trailing white spaces from unquoted fields be deleted?',
                                                    shinyWidgets::materialSwitch(inputId="dataset_from_file_csv_strip_white",
                                                                                 label="Strip white spaces",
                                                                                 value=FALSE, status="primary", right=TRUE))),

                                              div(title='Which strings in the file should be considered as missing data?\nPlease include the value(s) within double quotes and, if multiple values, separate them with commas (e.g. "NA", " ", "9999", "?").',
                                                  textInput(inputId="dataset_from_file_csv_na_strings",
                                                            label="Missing data symbols",
                                                            value='"NA"'))
                                            ),

                                            conditionalPanel(
                                              condition = "(input.dataset_from_file_filetype == 'Open Document Spreadsheet (.ods)') || (input.dataset_from_file_filetype == 'Microsoft Excel (.xls; .xlsx)')",

                                              div(title='Which sheet to load (if there are several sheets)?',
                                                  numericInput(inputId="dataset_from_file_sheet",
                                                               label="Which sheet to load?",
                                                               value=1, min=1, max=NA, step=1))
                                            ),

                                            #conditionalPanel(
                                            #  condition = "(input.dataset_from_file_filetype == 'SAS Transport data file (.xpt)')",
                                            #
                                            #  div(title='Which dataset to load (if there are several of them)?',
                                            #      numericInput(inputId="dataset_from_file_sheet_sas",
                                            #                   label="Which dataset to load?",
                                            #                   value=1, min=1, max=NA, step=1))
                                            #),

                                            div(title='Required: select and load a file...',
                                                fileInput(inputId="dataset_from_file_filename",
                                                          label="Load from file",
                                                          multiple=FALSE,
                                                          buttonLabel="Select")),

                                            conditionalPanel(
                                              #condition="(typeof(input.dataset_from_file_filename) != 'undefined') && (input.dataset_from_file_filename != null)",
                                              condition="(output.is_file_loaded == true)",

                                              div(title="Click here to peek at the selected dataset...",
                                                  actionButton("dataset_from_file_peek_button", label="Peek at file...", icon=icon("eye-open", lib="glyphicon"))),

                                              hr(),

                                              div(title='Required: select the name of the column containing the patient IDs',
                                                  shinyWidgets::pickerInput(inputId="dataset_from_file_patient_id",
                                                                            label="Patient ID column",
                                                                            choices=c("[none]"),
                                                                            selected="[none]")),

                                              div(title='Required: give the date format.\nBasic codes are:\n  "%d" (day of the month as decimal number),\n  "%m" (month as decimal number),\n  "%b" (Month in abbreviated form),\n  "%B" (month full name),\n  "%y" (year in 2 digit format) and\n  "%Y" (year in 4 digit format).\nSome examples are %m/%d/%Y or %Y%m%d.\nPlease see help entry for "strptime()".',
                                                  textInput(inputId="dataset_from_file_event_format",
                                                            label="Date format",
                                                            value="%m/%d/%Y",
                                                            placeholder="%m/%d/%Y")),

                                              div(title='Required: select the name of the column containing the event dates (in the format defined above)',
                                                  shinyWidgets::pickerInput(inputId="dataset_from_file_event_date",
                                                                            label="Event date column",
                                                                            choices=c("[none]"),
                                                                            selected="[none]")),

                                              div(title='Required: select the name of the column containing the event duration (in days)',
                                                  shinyWidgets::pickerInput(inputId="dataset_from_file_event_duration",
                                                                            label="Event duration column",
                                                                            choices=c("[none]"),
                                                                            selected="[none]")),

                                              div(title='Optional (potentially used by CMA5+): select the name of the column containing the daily dose',
                                                  shinyWidgets::pickerInput(inputId="dataset_from_file_daily_dose",
                                                                            label="Daily dose column",
                                                                            choices=c("[not defined]"),
                                                                            selected="[not defined]")),

                                              div(title='Optional (potentially used by CMA5+): select the name of the column containing the treatment class',
                                                  shinyWidgets::pickerInput(inputId="dataset_from_file_medication_class",
                                                                            label="Treatment class column",
                                                                            choices=c("[not defined]"),
                                                                            selected="[not defined]")),

                                              hr(),

                                              div(title='Validate choices and use the dataset!',
                                                  actionButton(inputId="dataset_from_file_button_use",
                                                               label=strong("Validate & use!"),
                                                               icon=icon("sunglasses", lib="glyphicon"),
                                                               style="color:DarkBlue; border-color:DarkBlue;"),
                                                  style="float: center;")
                                            )
                                          ),


                                          # Use dataset from SQL database ----
                                          conditionalPanel(
                                            condition = "(input.datasource_type == 'SQL database')",

                                            # Obligatory stuff:
                                            div(title=HTML('Required: connection to SQL database (please note that the credentials are <b>not</b> stored! What RDBMS/SQL server type/vendor/solution to connect to?'),
                                                selectInput(inputId="dataset_from_sql_server_type",
                                                            label="What RDBMS solution?",
                                                            choices=c("SQLite",
                                                                      "MySQL/MariaDB"),
                                                            selected="SQLite")),

                                            conditionalPanel(
                                              condition="(input.dataset_from_sql_server_type == 'SQLite')",

                                              div(title=HTML('This is intended as an example of using SQL with SQLite, and uses an in-memory "med_events" database that contains the example dataset included in the package.\nFor security reasons, we do not allow at this time the use of a user-given SQLite database, but this is easy to do.\nDespite its limitations, SQLite could be a fully working solution in specific scenarios involving, for example, local applications or a pre-defined databasestored in a file on the server.'),
                                                  shinyjs::disabled(textInput(inputId="dataset_from_sqlite_database_name",
                                                                              label=HTML("Database name <span style='color: red'>(fixed example)</span>"),
                                                                              value=c("med_events"))))
                                            ),

                                            conditionalPanel(
                                              condition="(input.dataset_from_sql_server_type == 'MySQL/MariaDB')",

                                              div(title=HTML('Required: the address (name or IP) of the host database (if on the local machine, use "localhost" or "[none]").'),
                                                  textInput(inputId="dataset_from_sql_server_host",
                                                            label="Host name/address",
                                                            value=c("[none]"))),

                                              div(title=HTML('Required: the database server TCP/IP port number.'),
                                                  numericInput(inputId="dataset_from_sql_server_port",
                                                               label="TCP/IP port number",
                                                               value=c(0), min=0, max=NA, step=1)),

                                              div(title=HTML('Required: the name of the database.'),
                                                  textInput(inputId="dataset_from_sql_database_name",
                                                            label="Database name",
                                                            value=c("[none]"))),

                                              div(title=HTML('Required: the username.'),
                                                  textInput(inputId="dataset_from_sql_username",
                                                            label="Username",
                                                            value=c(""),
                                                            placeholder="user")),

                                              div(title=HTML('Required: the password'),
                                                  passwordInput(inputId="dataset_from_sql_password",
                                                                label="Password",
                                                                value=c(""),
                                                                placeholder="password"))
                                            ),

                                            div(title='Connect to datadase and fetch tables!',
                                                actionButton(inputId="dataset_from_sql_button_connect",
                                                             label=strong("Connect!"),
                                                             icon=icon("transfer", lib="glyphicon"),
                                                             style="color:DarkBlue; border-color:DarkBlue;"),
                                                style="padding-bottom: 10px;"),

                                            conditionalPanel(
                                              condition="(output.is_database_connected == true)",

                                              div(title='Disconnect from datadase (not really necessary, as the disconnection is automatic when closing the application, but nice to do))',
                                                  actionButton(inputId="dataset_from_sql_button_disconnect",
                                                               label=strong("Disconnect!"),
                                                               icon=icon("ban-circle", lib="glyphicon"),
                                                               style="color:red; border-color:red;"),
                                                  style="padding-bottom: 20px;"),

                                              div(title='Peek at database!',
                                                  actionButton(inputId="dataset_from_sql_button_peek",
                                                               label=strong("Peek at database..."),
                                                               icon=icon("eye-open", lib="glyphicon"))),

                                              hr(),

                                              div(title=HTML('Required: the table or view to use...'),
                                                  shinyWidgets::pickerInput(inputId="dataset_from_sql_table",
                                                                            label="Which table/view",
                                                                            choices=c("[none]"),
                                                                            selected="[none]")),

                                              div(title='Required: select the name of the column containing the patient IDs',
                                                  shinyWidgets::pickerInput(inputId="dataset_from_sql_patient_id",
                                                                            label="Patient ID column",
                                                                            choices=c("[none]"),
                                                                            selected="[none]")),

                                              div(title='Required: give the date format.\nBasic codes are:\n  "%d" (day of the month as decimal number),\n  "%m" (month as decimal number),\n  "%b" (Month in abbreviated form),\n  "%B" (month full name),\n  "%y" (year in 2 digit format) and\n  "%Y" (year in 4 digit format).\nSome examples are %m/%d/%Y or %Y%m%d.\nPlease see help entry for "strptime()".\nFor SQL, the standard format is %Y-%m-%d (i.e., YYYY-MM-DD).',
                                                  textInput(inputId="dataset_from_sql_event_format",
                                                            label="Date format",
                                                            value="%Y-%m-%d",
                                                            placeholder="%Y-%m-%d")),

                                              div(title='Required: select the name of the column containing the event dates (please note the the format is the standard SQL YYYY-MM-DD)',
                                                  shinyWidgets::pickerInput(inputId="dataset_from_sql_event_date",
                                                                            label="Event date column",
                                                                            choices=c("[none]"),
                                                                            selected="[none]")),

                                              div(title='Required: select the name of the column containing the event duration (in days)',
                                                  shinyWidgets::pickerInput(inputId="dataset_from_sql_event_duration",
                                                                            label="Event duration column",
                                                                            choices=c("[none]"),
                                                                            selected="[none]")),

                                              div(title='Optional (potentially used by CMA5+): select the name of the column containing the daily dose',
                                                  shinyWidgets::pickerInput(inputId="dataset_from_sql_daily_dose",
                                                                            label="Daily dose column",
                                                                            choices=c("[not defined]"),
                                                                            selected="[not defined]")),

                                              div(title='Optional (potentially used by CMA5+): select the name of the column containing the treatment class',
                                                  shinyWidgets::pickerInput(inputId="dataset_from_sql_medication_class",
                                                                            label="Treatment class column",
                                                                            choices=c("[not defined]"),
                                                                            selected="[not defined]")),

                                              hr(),

                                              div(title='Validate choices and use the dataset!',
                                                  actionButton(inputId="dataset_from_sql_button_use",
                                                               label=strong("Validate & use!"),
                                                               icon=icon("sunglasses", lib="glyphicon"),
                                                               style="color:DarkBlue; border-color:DarkBlue;"),
                                                  style="float: center;")
                                            )

                                          ),


                                          # Allow last comma:
                                          NULL
                                )
                       ),

                       # MEDICATION GROUPS TAB ----
                       tabPanel(span("Groups", title="Define medication groups..."),
                                value="sidebar-params-data", icon=icon("list", lib="glyphicon"), fluid=TRUE,
                                conditionalPanel(
                                  condition="!(output.is_dataset_defined)",


                                  wellPanel(id = "tPanelnomg", style = "overflow:scroll; max-height: 90vh;",
                                            div(h4("No datasource!"), style="color:DarkRed"),
                                            br(),
                                            div(span(span("There is no valid source of data defined (probably because the interactive Shiny plotting was invoked without passing a dataset).")), style="color: red;"),
                                            hr(),
                                            div(span("Please use the "),
                                                span(icon("hdd",lib="glyphicon"),strong("Data"), style="color: darkblue"),
                                                span(" tab to select a valid datesource!"))
                                  )
                                ),

                                conditionalPanel(
                                  condition="(output.is_dataset_defined)",

                                  wellPanel(id = "tPanel3", style = "overflow:scroll; max-height: 90vh; min-height: 50vh",

                                            # Source of medication groups ----
                                            span(title='Define medication groups...',
                                                 h4("Medication groups"), style="color:DarkBlue"),

                                            div(title='Are there medication groups or not?',
                                                shinyWidgets::materialSwitch(inputId="mg_use_medication_groups",
                                                                             label=HTML("Use medication groups?"),
                                                                             value=!is.null(.GlobalEnv$.plotting.params$medication.groups), status="primary", right=TRUE)),

                                            conditionalPanel(
                                              condition="(input.mg_use_medication_groups)",

                                              div(title='How to define the medication groups?',
                                                  selectInput(inputId="mg_definitions_source",
                                                              label="Medication groups from:",
                                                              choices=c("named vector", "column in data"),
                                                              selected="named vector")),

                                              conditionalPanel(
                                                condition="(input.mg_definitions_source == 'named vector')",

                                                div(title='Load the medication groups from a named vector in memory',
                                                    # From in-memory vector ----
                                                    selectInput(inputId="mg_from_memory",
                                                                label="Load named vector",
                                                                choices=c("[none]"),
                                                                selected="[none]")),

                                                div(style="height: 0.50em;"),

                                                div(title="Click here to check the selected vector...",
                                                    actionButton("mg_from_memory_peek_button", label="Check it!", icon=icon("eye-open", lib="glyphicon")))
                                              ),

                                              conditionalPanel(
                                                condition="(input.mg_definitions_source == 'column in data')",

                                                div(title='The medication groups are defined by a column in the data',
                                                    # From column in the data ----
                                                    shinyWidgets::pickerInput(inputId="mg_from_column",
                                                                              label="Medication groups column",
                                                                              choices=c("[none]"),
                                                                              selected="[none]")),

                                                div(style="height: 0.50em;")
                                              ),

                                              hr(),

                                              div(title='Validate and use the medication groups!',
                                                  actionButton(inputId="mg_from_memory_button_use",
                                                               label=strong("Use it!"),
                                                               icon=icon("sunglasses", lib="glyphicon"),
                                                               style="color:DarkBlue; border-color:DarkBlue;"),
                                                  style="float: center;"),

                                              hr()
                                            )
                                  )
                                )
                       )

             )))),


    # OUTPUT PANEL ----
    #mainPanel(
    column(9,

           #shinyjs::hidden(checkboxInput(inputId="output_panel_container_show", label="", value=FALSE)),
           shinyjs::hidden(div(id="output_panel_container", # start with these hidden...
           #conditionalPanel(
           #  condition="(input.output_panel_container_show == true)",

           conditionalPanel(
             condition="(output.is_dataset_defined == true)",

             # Plot dimensions ----
             column(3,
                    div(title='The width of the plotting area (in pixles)',
                        sliderInput(inputId="plot_width",
                                    label="Plot width",
                                    min=100, max=5000, value=500, step=20, round=TRUE))
             ),

             conditionalPanel(
               condition = "(!input.plot_keep_ratio)",
               column(3,
                      div(title='The height of the plotting area (in pixels)',
                          sliderInput(inputId="plot_height",
                                      label="height",
                                      min=60, max=5000, value=300, step=20, round=TRUE))
               )
             ),
             conditionalPanel(
               condition = "(input.plot_keep_ratio)",
               column(3,
                      p("")
               )
             ),

             column(3,
                    div(title='Freeze the width/height ratio of the plotting area (or make the width and height independent of each other)?',
                        shinyWidgets::materialSwitch(inputId="plot_keep_ratio",
                                                     label="Keep ratio",
                                                     value=TRUE, status="primary", right=TRUE)),

                    #checkboxInput(inputId="plot_auto_size",
                    #            label="auto size",
                    #            value=TRUE)
                    #),

                    #column(2,
                    # Save image to file:
                    div(title='Export this plot to an image file?',
                        shinyWidgets::materialSwitch(inputId="save_to_file_info",
                                                     label="Save plot!",
                                                     value=FALSE, status="primary", right=TRUE))
                    #shinyWidgets::checkboxGroupButtons(inputId="save_to_file_info",
                    #                     label=NULL,
                    #                     choices=c(`<span><i class='fa fa-bar-chart'></i>&nbsp; Save plot!</span>` = "Save it!")))
                    #                     #choices=c(`<i class='fa fa-bar-chart'></i>` = "Save it!")))
             )
           ),

           conditionalPanel(
             condition="!(output.is_dataset_defined)",
             column(9,
                    div(p(" "))
             )
           ),

           column(3,
                  # Close shop:
                  div(title='Exit this Shiny plotting app? (The plot will NOT be automatically saved!)',
                      actionButton(inputId="close_shop", label=strong("Exit..."), icon=icon("remove-circle", lib="glyphicon"), style="color: #C70039 ; border-color: #C70039"))
           ),

           # Export to file ----
           column(12,
                  conditionalPanel(
                    condition="(input.save_to_file_info)",

                    div(title='Save the plot using the same size as currently displayed or pick a new size?',
                        checkboxInput(inputId="save_plot_displayed_size", label="Save plot using current size?", value=TRUE)),

                    conditionalPanel(
                      condition="(!input.save_plot_displayed_size)",
                      column(2,
                             div(title='The width of the exported plot (in the selected units)',
                                 numericInput(inputId="save_plot_width", label="width", value=5))),
                      column(2,
                             div(title='The height of the exported plot (in the selected units)',
                                 numericInput(inputId="save_plot_height", label="height", value=5))),

                      conditionalPanel( # EPS + PDF
                        condition="(input.save_plot_type == 'eps' || save_plot_type == 'pdf')",
                        column(2,
                               div(title='For EPS and PDF, only inches are available',
                                   selectInput(inputId="save_plot_dim_unit", label="unit", choices=c("in"), selected="in"))) # only inches
                      ),
                      conditionalPanel( # JPEG + PNG + TIFF
                        condition="(input.save_plot_type != 'eps' && save_plot_type != 'pdf')",
                        column(2,
                               div(title='The unit of exported plot',
                                   selectInput(inputId="save_plot_dim_unit", label="unit", choices=c("in","cm","px"), selected="in")))
                      )
                    ),

                    conditionalPanel(
                      condition="(input.save_plot_displayed_size)",
                      column(6,
                             div())
                    ),

                    column(2,
                           div(title='The type of the exported image',
                               selectInput(inputId="save_plot_type", label="type", choices=c("jpg","png","tiff","eps","pdf"), selected="jpg"))),

                    #column(2,numericInput(inputId="save_plot_quality", label="quality", value=75, min=0, max=100, step=1)),
                    conditionalPanel(
                      condition="(input.save_plot_type != 'eps' && input.save_plot_type != 'pdf')",
                      column(2,
                             div(title='The resolution of the exported image (not useful for EPS and PDF)',
                                 numericInput(inputId="save_plot_resolution", label="resolution", value=72, min=0)))
                    ),

                    conditionalPanel(
                      condition="(input.save_plot_type == 'eps' || input.save_plot_type == 'pdf')",
                      column(2,
                             div())
                    ),

                    column(2, style="margin-top: 25px;",
                           div(title='Export the plot now!',
                               downloadButton(outputId="save_to_file", label="Save plot")))
                  )
           ),

           conditionalPanel(
             condition="(output.is_dataset_defined)",

             # Messages ----
             column(12,
                    tags$head(tags$style("#container * { display: inline; }")),
                    div(id="container", title="Various messages (in blue), warnings (in green) and errors (in red) generated during plotting...",
                        span(" Messages:", style="color:DarkBlue; font-weight: bold;"),
                        span(htmlOutput(outputId = "messages")),
                        style="height: 4em; resize: vertical; overflow: auto")
             ),

             # The actual plot ----
             column(12, wellPanel(id = "tPlot",
                                  style="resize: none; overflow:scroll; max-height: 75vh; max-width: 80vw",
                                  plotOutput(outputId = "distPlot", inline=TRUE))),

             # Export R code for plot ----
             column(12,
                    # Show the R code:
                    div(title='Show the R code that would generate the current plot',
                        actionButton(inputId="show_r_code", label=strong("Show R code..."), icon=icon("eye-open", lib="glyphicon")))
             ),

             column(12,
                    div(p(" "))
             ),

             hr(),

             # Compute CMA for a (larger) set of patients using the same parameters as for the current plot:
             column(12,
                    conditionalPanel(
                      condition="!(input.cma_class == 'simple' && input.cma_to_compute == 'CMA0')",
                      div(title='Compute the same CMA with the same parameters as those used to generate the current plot but for (possible) more patients and export the results',
                          # actionButton(inputId="compute_cma_for_larger_sample", label=strong("Compute CMA for (more) patients..."), icon=icon("play", lib="glyphicon")))
                          shinyWidgets::materialSwitch(inputId="compute_cma_for_larger_sample",
                                                       label=strong("Compute CMA for several  patients..."),
                                                       value=FALSE, status="primary", right=TRUE))
                    )
             ),

             # Compute CMA and export results and R code ----
             column(12,
                    conditionalPanel(
                      condition="!(input.cma_class == 'simple' && input.cma_to_compute == 'CMA0') && (input.compute_cma_for_larger_sample)",

                      column(12,
                             div(HTML(paste0('Please select the patients for which to compute the <code>CMA</code>.<br/>',
                                             'Please note that, due to the potentially high computational costs associated, we limit the <b>maximum number of patients</b> to ',
                                             .GlobalEnv$.plotting.params$max.number.patients.to.compute,
                                             ', the <b>maximum number of events</b> across all patients to ',
                                             .GlobalEnv$.plotting.params$max.number.events.to.compute,
                                             ', and the <b>running time</b> to a maximum of ',
                                             .GlobalEnv$.plotting.params$max.running.time.in.minutes.to.compute,
                                             ' minutes.<br/>',
                                             'For larger computations, we provide the <code>R</code> code, ',
                                             'which we recommend running from a <b>dedicated <code>R</code> session</b> on appropriately powerful hardware...'
                             ))),

                             #div(title="TEST!",
                             #    radioButtons(inputId="compute_cma_patient_selection_method",
                             #                 label="You can select the patients either:",
                             #                 choices=c("individually by their ID"="by_id",
                             #                           "or as a range based on their position in a list"="by_position"),
                             #                 inline=TRUE)),

                             hr(),
                             div(title="Compue the CMA for the selected patients...",
                                 actionButton(inputId="compute_cma_for_larger_sample_button",
                                              label=strong("Compute CMA..."),
                                              icon=icon("play", lib="glyphicon"),
                                              style="color: darkblue ; border-color: darkblue")),
                             hr(),

                             div(HTML('<b>You can select the patients:</b>')),
                             #hr(),

                             tabsetPanel(id="compute_cma_patient_selection_method",

                                         #conditionalPanel(
                                         #  condition="(input.compute_cma_patient_selection_method == 'by_id'",
                                         tabPanel(title=HTML("<b>individually</b>, using their IDs"), value="by_id",
                                                  column(12,div(title="Please select one or more patient IDs to process...",
                                                                selectInput(inputId="compute_cma_patient_by_id",
                                                                            label="Select the IDs of the patients to include",
                                                                            choices=.GlobalEnv$.plotting.params$all.IDs,
                                                                            selected=.GlobalEnv$.plotting.params$all.IDs[1],
                                                                            multiple=TRUE)))
                                         ),

                                         #conditionalPanel(
                                         #  condition="(input.compute_cma_patient_selection_method == 'by_position'",
                                         tabPanel(title=HTML("as a <b>range</b>, based on their position in a list"), value="by_position",
                                                  column(12,
                                                         div(title="Define the range of positions (#) corresponding to the selected IDs...",
                                                             sliderInput(inputId="compute_cma_patient_by_group_range",
                                                                         label="Select range of ID positions (#)",
                                                                         min=1, max=100, step=1, value=c(1,1),
                                                                         round=TRUE, width="100%"))
                                                  ),

                                                  column(3,
                                                         div(title="Order the patients by..."),
                                                         selectInput(inputId="compute_cma_patient_by_group_sorting",
                                                                     label="Sort patients by",
                                                                     choices=c("original order",
                                                                               "by ID (↑)",
                                                                               "by ID (↓)"))
                                                  ),

                                                  column(9,
                                                         div(div("The patient IDs with their position (#):"),
                                                             dataTableOutput(outputId="show_patients_as_list"))
                                                  )
                                         )
                             )
                      )
                    )
             )
           ),

           conditionalPanel(
             condition="!(output.is_dataset_defined)",

             column(12, align="center",
             div(br(),br(),br(),
                 span("Please use the "),
                 span(icon("hdd",lib="glyphicon"),strong("Data"), style="color: darkblue"),
                 span(" tab to select a valid datesource!")))
           )

    )))#)
  )
)


# THE SERVER LOGIC ----
server <- function(input, output, session)
{
  # Initialisation of the Shiny app ----
  isolate({showModal(modalDialog("Adherer", title=div(icon("hourglass", lib="glyphicon"), "Please wait while initializing the App..."), easyClose=FALSE, footer=NULL))})

  # Reactive value to allow UI updating on dataset changes:
  rv <- reactiveValues(toggle.me = FALSE);

  isolate(
    {
      options(shiny.sanitize.errors=FALSE);

      # Initialisation for a directly-launched Shiny App or for a new session:
      if( is.null(.GlobalEnv$.plotting.params) ||
          (is.logical(.GlobalEnv$.plotting.params$.dataset.comes.from.function.arguments) &&
           !is.na(.GlobalEnv$.plotting.params$.dataset.comes.from.function.arguments) &&
           !.GlobalEnv$.plotting.params$.dataset.comes.from.function.arguments) )
      {
        # Ok, seem we've been launched directly as a "normal" Shiny app:
        # make sure things are set to their default in the .plotting.params global list:
        .GlobalEnv$.plotting.params <- list("data"=NULL,
                                            "cma.class"="simple",
                                            "ID.colname"=NA,
                                            "event.date.colname"=NA,
                                            "event.duration.colname"=NA,
                                            "event.daily.dose.colname"=NA,
                                            "medication.class.colname"=NA,
                                            "medication.groups"=NULL,
                                            "date.format"=NA,
                                            "align.all.patients"=FALSE,
                                            "align.first.event.at.zero"=FALSE,
                                            "ID"="[not defined]", "all.IDs"=c("[not defined]"),
                                            "max.number.patients.to.plot"=10, "max.number.events.to.plot"=500,
                                            "max.number.patients.to.compute"=100, "max.number.events.to.compute"=5000, "max.running.time.in.minutes.to.compute"=5,
                                            ".patients.to.compute"=NULL,
                                            "print.full.params"=FALSE,
                                            "get.colnames.fnc"=NULL,
                                            "get.patients.fnc"=NULL,
                                            "get.data.for.patients.fnc"=NULL,
                                            ".plotting.fnc"=AdhereRViz:::.plotting.fnc.shiny,
                                            ".dataset.type"=NA,
                                            ".dataset.comes.from.function.arguments"=FALSE,
                                            ".dataset.name"=NA,
                                            ".inmemory.dataset"=NULL,
                                            ".fromfile.dataset"=NULL,
                                            ".fromfile.dataset.filetype"=NULL,
                                            ".fromfile.dataset.header"=NULL,
                                            ".fromfile.dataset.sep"=NULL,
                                            ".fromfile.dataset.quote"=NULL,
                                            ".fromfile.dataset.dec"=NULL,
                                            ".fromfile.dataset.strip.white"=NULL,
                                            ".fromfile.dataset.na.strings"=NULL,
                                            ".fromfile.dataset.sheet"=NULL,
                                            ".db.connection.tables"=NULL,
                                            ".db.connection.selected.table"=NULL,
                                            ".db.connection"=NULL
        );

        # Make sure the med.events data is loaded as well:
        data("medevents", package="AdhereR");
      }
    })

  # Show/hide UI elements based on various conditions:
  output$is_dataset_defined <- reactive({!is.null(.GlobalEnv$.plotting.params$data)});
  outputOptions(output, "is_dataset_defined", suspendWhenHidden = FALSE);

  output$is_file_loaded <- reactive({!is.null(.GlobalEnv$.plotting.params$.fromfile.dataset)});
  outputOptions(output, "is_file_loaded", suspendWhenHidden = FALSE);

  output$is_database_connected <- reactive({!is.null(.GlobalEnv$.plotting.params$.db.connection)});
  outputOptions(output, "is_database_connected", suspendWhenHidden = FALSE);

  output$is_dose_defined <- reactive({!is.null(.GlobalEnv$.plotting.params$event.daily.dose.colname) && !is.na(.GlobalEnv$.plotting.params$event.daily.dose.colname)});
  outputOptions(output, "is_dose_defined", suspendWhenHidden = FALSE);

  output$is_treat_class_defined <- reactive({!is.null(.GlobalEnv$.plotting.params$medication.class.colname) && !is.na(.GlobalEnv$.plotting.params$medication.class.colname)});
  outputOptions(output, "is_treat_class_defined", suspendWhenHidden = FALSE);

  output$is_mg_defined <- reactive({!is.null(.GlobalEnv$.plotting.params$medication.groups)});
  outputOptions(output, "is_mg_defined", suspendWhenHidden = FALSE);

  #outputOptions(output, 'save_to_file', suspendWhenHidden=FALSE);

  # Clean up at session end ----
  session$onSessionEnded(function()
  {
    # Disconnect any open database connections...
    if( !is.null(.GlobalEnv$.plotting.params$.db.connection) )
    {
      try(DBI::dbDisconnect(.GlobalEnv$.plotting.params$.db.connection), silent=TRUE);
    }

    # Clean up stuff from the previous session:
    .GlobalEnv$.plotting.params <- NULL;
    collected.results <<- NULL;
    cma.computation.progress.log.text <<- NULL;
  })


  # The plotting function ----
  .renderPlot <- function()
  {
    patients.to.plot <- input$patient;
    # Checks concerning the maximum number of patients and events to plot:
    if( length(patients.to.plot) > .GlobalEnv$.plotting.params$max.number.patients.to.plot )
    {
      patients.to.plot <- patients.to.plot[ 1:.GlobalEnv$.plotting.params$max.number.patients.to.plot ];
      #updateSelectInput(session, inputId="patient", selected=patients.to.plot);
      cat(paste0("Warning: a maximum of ",.GlobalEnv$.plotting.params$max.number.patients.to.plot,
                     " patients can be shown in an interactive plot: we kept only the first ",.GlobalEnv$.plotting.params$max.number.patients.to.plot,
                     " from those you selected!\n"));
    }
    ## This check can be too costly during plotting (especially for database connections), so we don't do it for now assuming there's not too many events per patient anyway:
    #if( !is.null(n.events <- .GlobalEnv$.plotting.params$get.data.for.patients.fnc(patients.to.plot, .GlobalEnv$.plotting.params$data, .GlobalEnv$.plotting.params$ID.colname)) &&
    #    nrow(n.events) > .GlobalEnv$.plotting.params$max.number.events.to.plot )
    #{
    #  n.events.per.patient <- cumsum(table(n.events[,.GlobalEnv$.plotting.params$ID.colname]));
    #  n <- min(which(n.events.per.patient > .GlobalEnv$.plotting.params$max.number.events.to.plot));
    #  if( n > 1 ) n <- n-1;
    #  patients.to.plot <- patients.to.plot[ 1:n ];
    #  cat(paste0("Warning: a maximum of ",.GlobalEnv$.plotting.params$max.number.events.to.plot,
    #                 " events across all patients can be shown in an interactive plot: we kept only the first ",length(patients.to.plot),
    #                 " patients from those you selected (totalling ",n.events.per.patient[n]," events)!\n"));
    #}

    res <- NULL;
    try(res <- .GlobalEnv$.plotting.params$.plotting.fnc(data=.GlobalEnv$.plotting.params$data,
                                                         ID.colname=.GlobalEnv$.plotting.params$ID.colname,
                                                         event.date.colname=.GlobalEnv$.plotting.params$event.date.colname,
                                                         event.duration.colname=.GlobalEnv$.plotting.params$event.duration.colname,
                                                         event.daily.dose.colname=.GlobalEnv$.plotting.params$event.daily.dose.colname,
                                                         medication.class.colname=.GlobalEnv$.plotting.params$medication.class.colname,
                                                         date.format=.GlobalEnv$.plotting.params$date.format,

                                                         ID=patients.to.plot,

                                                         medication.groups=if( input$mg_use_medication_groups ){ .GlobalEnv$.plotting.params$medication.groups }else{ NULL },
                                                         medication.groups.separator.show=input$mg_plot_by_patient,
                                                         medication.groups.to.plot=.GlobalEnv$.plotting.params$medication.groups.to.plot,
                                                         medication.groups.separator.lty=input$plot_mg_separator_lty,
                                                         medication.groups.separator.lwd=input$plot_mg_separator_lwd,
                                                         medication.groups.separator.color=input$plot_mg_separator_color,
                                                         medication.groups.allother.label=input$plot_mg_allothers_label,

                                                         cma=ifelse(input$cma_class == "simple",
                                                                    input$cma_to_compute,
                                                                    input$cma_class),
                                                         cma.to.apply=ifelse(input$cma_class == "simple",
                                                                             "none",
                                                                             ifelse(input$cma_to_compute_within_complex == "CMA0", "CMA1", input$cma_to_compute_within_complex)), # don't use CMA0 for complex CMAs
                                                         #carryover.within.obs.window=FALSE,
                                                         #carryover.into.obs.window=FALSE,
                                                         carry.only.for.same.medication=input$carry_only_for_same_medication,
                                                         consider.dosage.change=input$consider_dosage_change,
                                                         followup.window.start=if(input$followup_window_start_unit == "calendar date"){as.Date(input$followup_window_start_date, format="%Y-%m-%d")} else {as.numeric(input$followup_window_start_no_units)},
                                                         followup.window.start.unit=ifelse(input$followup_window_start_unit == "calendar date",
                                                                                           "days",
                                                                                           input$followup_window_start_unit),
                                                         followup.window.duration=as.numeric(input$followup_window_duration),
                                                         followup.window.duration.unit=input$followup_window_duration_unit,
                                                         observation.window.start=if(input$observation_window_start_unit == "calendar date"){as.Date(input$observation_window_start_date, format="%Y-%m-%d")} else {as.numeric(input$observation_window_start_no_units)},
                                                         observation.window.start.unit=ifelse(input$observation_window_start_unit == "calendar date",
                                                                                              "days",
                                                                                              input$observation_window_start_unit),
                                                         observation.window.duration=as.numeric(input$observation_window_duration),
                                                         observation.window.duration.unit=input$observation_window_duration_unit,
                                                         medication.change.means.new.treatment.episode=input$medication_change_means_new_treatment_episode,
                                                         dosage.change.means.new.treatment.episode=input$dosage_change_means_new_treatment_episode,
                                                         maximum.permissible.gap=as.numeric(input$maximum_permissible_gap),
                                                         maximum.permissible.gap.unit=input$maximum_permissible_gap_unit,
                                                         maximum.permissible.gap.append.to.episode=input$maximum_permissible_gap_append,
                                                         sliding.window.start=as.numeric(input$sliding_window_start),
                                                         sliding.window.start.unit=input$sliding_window_start_unit,
                                                         sliding.window.duration=as.numeric(input$sliding_window_duration),
                                                         sliding.window.duration.unit=input$sliding_window_duration_unit,
                                                         sliding.window.step.duration=as.numeric(input$sliding_window_step_duration),
                                                         sliding.window.step.unit=input$sliding_window_step_unit,
                                                         sliding.window.no.steps=ifelse(input$sliding_window_step_choice == "number of steps" ,as.numeric(input$sliding_window_no_steps), NA),
                                                         plot.CMA.as.histogram=ifelse(input$cma_class == "sliding window",
                                                                                      !input$plot_CMA_as_histogram_sliding_window,
                                                                                      !input$plot_CMA_as_histogram_episodes),
                                                         align.all.patients=input$plot_align_all_patients,
                                                         align.first.event.at.zero=input$plot_align_first_event_at_zero,
                                                         show.legend=input$show_legend, legend.x=input$legend_x, legend.y=input$legend_y, legend.bkg.opacity=input$legend_bkg_opacity,
                                                         legend.cex=max(0.01,input$legend_cex), legend.cex.title=max(0.01,input$legend_cex_title),
                                                         duration=ifelse(input$duration==0, NA, input$duration), # duration to plot
                                                         show.period=ifelse(length(input$patient) > 1 && input$plot_align_all_patients, "days", input$show_period), period.in.days=input$period_in_days, # period
                                                         bw.plot=input$bw_plot, # grayscale plotting
                                                         #show.cma=input$show_cma,
                                                         col.na=input$col_na,
                                                         unspecified.category.label=input$unspecified_category_label,
                                                         col.cats=list("rainbow"       =grDevices::rainbow,
                                                                       "heat.colors"   =grDevices::heat.colors,
                                                                       "terrain.colors"=grDevices::terrain.colors,
                                                                       "topo.colors"   =grDevices::topo.colors,
                                                                       "cm.colors"     =grDevices::cm.colors,
                                                                       "magma"         =viridisLite::magma,
                                                                       "inferno"       =viridisLite::inferno,
                                                                       "plasma"        =viridisLite::plasma,
                                                                       "viridis"       =viridisLite::viridis,
                                                                       "cividis"       =viridisLite::cividis)[[input$col_cats]],
                                                         lty.event=input$lty_event, lwd.event=input$lwd_event, pch.start.event=as.numeric(input$pch_start_event), pch.end.event=as.numeric(input$pch_end_event),
                                                         col.continuation=input$col_continuation, lty.continuation=input$lty_continuation, lwd.continuation=input$lwd_continuation,
                                                         cex=max(0.01,input$cex), cex.axis=max(0.01,input$cex_axis), cex.lab=max(0.01,input$cex_lab),
                                                         force.draw.text=input$force_draw_text,
                                                         highlight.followup.window=input$highlight_followup_window, followup.window.col=input$followup_window_col,
                                                         highlight.observation.window=input$highlight_observation_window, observation.window.col=input$observation_window_col,
                                                         #observation.window.density=input$observation_window_density, observation.window.angle=input$observation_window_angle,
                                                         observation.window.opacity=input$observation_window_opacity,
                                                         show.real.obs.window.start=input$show_real_obs_window_start,
                                                         #real.obs.window.density=input$real_obs_window_density, real.obs.window.angle=input$real_obs_window_angle,
                                                         print.CMA=input$print_cma, CMA.cex=max(0.01,input$cma_cex),
                                                         plot.CMA=input$plot_cma, CMA.plot.ratio=input$cma_plot_ratio / 100.0,
                                                         CMA.plot.col=input$cma_plot_col, CMA.plot.border=input$cma_plot_border, CMA.plot.bkg=input$cma_plot_bkg, CMA.plot.text=input$cma_plot_text,
                                                         plot.partial.CMAs.as=c(if(input$plot_cma_stacked){"stacked"}else{NULL},
                                                                                if(input$plot_cma_overlapping){"overlapping"}else{NULL},
                                                                                if(input$plot_cma_timeseries){"timeseries"}else{NULL}),
                                                         plot.partial.CMAs.as.stacked.col.bars=input$plot_partial_cmas_as_stacked_col_bars,
                                                         plot.partial.CMAs.as.stacked.col.border=input$plot_partial_cmas_as_stacked_col_border,
                                                         plot.partial.CMAs.as.stacked.col.text=input$plot_partial_cmas_as_stacked_col_text,
                                                         plot.partial.CMAs.as.timeseries.vspace=input$cma_as_timeseries_vspace,
                                                         plot.partial.CMAs.as.timeseries.start.from.zero=input$cma_as_timeseries_start_from_zero,
                                                         plot.partial.CMAs.as.timeseries.col.dot=if(!input$cma_as_timeseries_show_dots){NA}else{input$cma_as_timeseries_color_dots},
                                                         plot.partial.CMAs.as.timeseries.interval.type=input$cma_as_timeseries_show_interval_type,
                                                         plot.partial.CMAs.as.timeseries.lwd.interval=input$cma_as_timeseries_lwd_intervals,
                                                         plot.partial.CMAs.as.timeseries.alpha.interval=input$cma_as_timeseries_alpha_intervals,
                                                         plot.partial.CMAs.as.timeseries.col.interval=if(!input$cma_as_timeseries_show_interval){NA}else{input$cma_as_timeseries_color_intervals},
                                                         plot.partial.CMAs.as.timeseries.col.text=if(!input$cma_as_timeseries_show_text){NA}else{input$cma_as_timeseries_color_text},
                                                         plot.partial.CMAs.as.timeseries.show.0perc=input$cma_as_timeseries_show_0perc,
                                                         plot.partial.CMAs.as.timeseries.show.100perc=input$cma_as_timeseries_show_100perc,
                                                         plot.partial.CMAs.as.overlapping.col.interval=if(!input$cma_as_overlapping_show_interval){NA}else{input$cma_as_overlapping_color_intervals},
                                                         plot.partial.CMAs.as.overlapping.col.text=if(!input$cma_as_overlapping_show_text){NA}else{input$cma_as_overlapping_color_text},
                                                         show.event.intervals=input$show_event_intervals,
                                                         print.dose=input$print_dose,
                                                         cex.dose=max(0.01,input$cex_dose),
                                                         print.dose.outline.col=input$print_dose_outline_col,
                                                         print.dose.centered=input$print_dose_centered,
                                                         plot.dose=input$plot_dose,
                                                         lwd.event.max.dose=input$lwd_event_max_dose,
                                                         plot.dose.lwd.across.medication.classes=input$plot_dose_lwd_across_medication_classes,
                                                         show.overlapping.event.intervals=input$overlapping_evint,
                                                         xlab=if(input$show_xlab) {c("dates"="Date", "days"="Days")} else {NULL},
                                                         ylab=if(input$show_ylab) {c("withoutCMA"="patient", "withCMA"="patient (& CMA)")} else {NULL},
                                                         title=if(input$show_plot_title) {c("aligned"="Event patterns (all patients aligned)", "notaligned"="Event patterns")} else {NULL},
                                                         min.plot.size.in.characters.horiz=input$min_plot_size_in_characters_horiz,
                                                         min.plot.size.in.characters.vert=input$min_plot_size_in_characters_vert,

                                                         get.colnames.fnc=.GlobalEnv$.plotting.params$get.colnames.fnc,
                                                         get.patients.fnc=.GlobalEnv$.plotting.params$get.patients.fnc,
                                                         get.data.for.patients.fnc=.GlobalEnv$.plotting.params$get.data.for.patients.fnc
    ), silent=FALSE);

    return (res);
  }

  # renderPlot() ----
  output$distPlot <- renderPlot({

      rv$toggle.me; # make the plot aware of forced updates to the UI (for example, when changing the dataset)

      msgs <- ""; # the output messages
      res <- NULL; # the result of plotting

      if( is.null(.GlobalEnv$.plotting.params$data) )
      {
        # Switch to the Data tab:
        updateTabsetPanel(session=session, inputId="sidebar-tabpanel", selected="sidebar-params-data");

        ## Display a warning urging the user to select a data source:
        #showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR..."),
        #                        div(span(code("plot_interactive_cma()"),
        #                                 span("was called without a dataset!\nPlease use the ")),
        #                          span(icon("hdd",lib="glyphicon")),
        #                          span("Data"),
        #                          span(" tab to select a datesource!"), style="color: red;"),
        #                      footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      } else
      {
        # Depeding on the CMA class we might do things differently:
        if( input$cma_class %in% c("simple", "per episode", "sliding window") )
        {
          # Call the workhorse plotting function with the appropriate argumens:
          #msgs <- capture.output(res <- .renderPlot());
          res <- .renderPlot();
        } else
        {
          # Quitting....
          showModal(modalDialog(title="AdhereR interactive plotting...", paste0("Unknwon CMA class '",input$cma_class,"'."), easyClose=TRUE));
        }

        # Show the messages (if any):
        ewms <- AdhereR:::.get.ewms();
        if( !is.null(ewms) && nrow(ewms) > 0 )
        {
          msgs <- vapply(1:nrow(ewms), function(i)
          {
            switch(as.character(ewms$type[i]),
                   "error"=  paste0("<b>&gt;</b> <font color=\"red\"><b>",as.character(ewms$text[i]),"</b></font>"),
                   "warning"=paste0("<b>&gt;</b> <font color=\"green\"><i>",as.character(ewms$text[i]),"</i></font>"),
                   "message"=paste0("<b>&gt;</b> <font color=\"blue\">",as.character(ewms$text[i]),"</font>"),
                   paste0("<b>&gt;</b> ",as.character(ewms$text[i])));
          }, character(1));
          output$messages <- renderText(paste0(paste0("<font color=\"red\"><b>",  sum(ewms$type=="error",na.rm=TRUE),  " error(s)</b></font>, ",
                                                      "<font color=\"green\"><i>",sum(ewms$type=="warning",na.rm=TRUE)," warning(s)</i></font> & ",
                                                      "<font color=\"blue\">",    sum(ewms$type=="message",na.rm=TRUE)," message(s)</font>:<br>"),
                                               paste0(msgs,collapse="<br>")));
        }

        #if( is.null(res) || length(grep("error", msgs, ignore.case=TRUE)) > 0 )
        #{
        #  # Errors:
        #  output$messages <- renderText({ paste0("<font color=\"red\"><b>",msgs,"</b></font>"); })
        #} else if( length(grep("warning", msgs, ignore.case=TRUE)) > 0 )
        #{
        #  # Warnings:
        #  output$messages <- renderText({ paste0("<font color=\"green\"><i>",msgs,"</i></font>"); })
        #} else
        #{
        #  # Normal output:
        #  output$messages <- renderText({ paste0("<font color=\"blue\">",msgs,"</font>"); })
        #}
      }
    },
    width=function() # plot width
      {
        return (input$plot_width);
      },
    height=function() # plot height
      {
        if( !is.numeric(.GlobalEnv$.plotting.params$plot.ratio) ) .GlobalEnv$.plotting.params$plot.ratio <- (input$plot_width / input$plot_height); # define the ratio
        if( input$plot_keep_ratio )
        {
          return (input$plot_width / .GlobalEnv$.plotting.params$plot.ratio);
        } else
        {
          return (input$plot_height);
        }
      },
    execOnResize=TRUE # force redrawing on resize
  )

  # The text messages ----
  output$messages <- renderText({
    ""
  })

  # Keep ratio toggle event ----
  observeEvent(input$plot_keep_ratio,
  {
    if( input$plot_keep_ratio )
    {
      .GlobalEnv$.plotting.params$plot.ratio <- (input$plot_width / input$plot_height); # save the ratio
    } else
    {
      updateSliderInput(session, "plot_height", value = round(input$plot_width / .GlobalEnv$.plotting.params$plot.ratio));
    }
  })


  # Export plot to file ----
  output$save_to_file <- downloadHandler(
    filename = function()
      {
        paste0("adherer-plot-",
               input$cma_class,"-",
               ifelse(input$cma_class=="simple", input$cma_to_compute, input$cma_to_compute_within_complex),
               "-",
               "ID-",input$patient,
               ".",
               input$save_plot_type)
      },
    content = function(file)
    {
      # Plot dimensions:
      if( input$save_plot_displayed_size )
      {
        if( input$save_plot_type %in% c("eps", "pdf") )
        {
          # Cairo PS and PDF only understand inches, so make sure we convert right:
          plot.dims.width  <- input$plot_width / 72; # by default 72 DPI
          if( !is.numeric(.GlobalEnv$.plotting.params$plot.ratio) ) .GlobalEnv$.plotting.params$plot.ratio <- (input$plot_width / input$plot_height); # define the ratio
          plot.dims.height <- ifelse(input$plot_keep_ratio, input$plot_width / .GlobalEnv$.plotting.params$plot.ratio, input$plot_height) / 72; # by default 72 DPI
          plot.dims.unit   <- "in";
        } else
        {
          # The others work in pixels:
          plot.dims.width  <- input$plot_width;
          if( !is.numeric(.GlobalEnv$.plotting.params$plot.ratio) ) .GlobalEnv$.plotting.params$plot.ratio <- (input$plot_width / input$plot_height); # define the ratio
          plot.dims.height <- ifelse(input$plot_keep_ratio, input$plot_width / .GlobalEnv$.plotting.params$plot.ratio, input$plot_height);
          plot.dims.unit   <- "px";
       }
      } else
      {
        plot.dims.width  <- input$save_plot_width;
        plot.dims.height <- input$save_plot_height;
        plot.dims.unit   <- input$save_plot_dim_unit;
      }

      # The type of plot to save:
      if( input$save_plot_type == "png" )
      {
        png(file, width=plot.dims.width, height=plot.dims.height, units=plot.dims.unit, res=input$save_plot_resolution, type="cairo");
      } else if( input$save_plot_type == "tiff" )
      {
        tiff(file, width=plot.dims.width, height=plot.dims.height, units=plot.dims.unit, res=input$save_plot_resolution, compression="zip", type="cairo");
      } else if( input$save_plot_type == "eps" )
      {
        cairo_ps(file, width=plot.dims.width, height=plot.dims.height, onefile=FALSE);
      } else if( input$save_plot_type == "pdf" )
      {
        cairo_pdf(file, width=plot.dims.width, height=plot.dims.height, onefile=FALSE);
      } else # default to JPEG
      {
        jpeg(file, width=plot.dims.width, height=plot.dims.height, units=plot.dims.unit, res=input$save_plot_resolution);
      }

      # Plot it:
      .renderPlot();

      # Close the device:
      dev.off();
    }
  )

  # Make sure by default the event intervals are not shown for complex CMAs ----
  observeEvent(input$cma_class,
               {
                 if( input$cma_class %in% c("sliding windows", "per episode") )
                 {
                   shinyWidgets::updateMaterialSwitch(session, inputId="show_event_intervals", value=FALSE); # set it to FALSE
                 }
               })


  # About and help box ----
  observeEvent(input$about_button,
  {
    # Get most of the relevant info from the DESCRIPTION file:
    descr.adherer    <- utils::packageDescription("AdhereR");
    descr.adhererviz <- utils::packageDescription("AdhereRViz");
    msg <- paste0(# Logo:
                  "<img src='adherer-logo.png', align = 'left', style='font-size: x-large; font-weight: bold; height: 2em; vertical-align: baseline;'/>",
                  #"<div style='width: 1em; display: inline-block;'/>",
                  "<div style='display: inline-block;'/>",
                  "<hr/>",
                  # AdhereR:
                  "<div style='max-height: 50vh; overflow: auto;'>",
                  "<h2>AdhereR</h1>",
                  "<p><b>Version</b> ",descr.adherer$Version,"</p>",
                  "<p><b>Authors:</b> ",descr.adherer$Author,"</p>",
                  "<p><b>Maintainer:</b> ",descr.adherer$Maintainer,"</p>",
                  "<p align='justify'>",descr.adherer$Description,"</p>",
                  "<p><b>Website:</b> <a href='",descr.adherer$URL,"' target='_blank'>",descr.adherer$URL,"</a></p>",
                  "<p><b>Released under:</b> ",descr.adherer$License,"</p>",
                  "<p><b>Citation:</b></p>",paste0(format(citation(package="AdhereR"),style="html"),collapse=" "),
                  "<hr/>",
                  # AdhereRViz:
                  "<h2>AdhereRViz</h1>",
                  "<p><b>Version</b> ",descr.adhererviz$Version,"</p>",
                  "<p><b>Authors:</b> ",descr.adhererviz$Author,"</p>",
                  "<p><b>Maintainer:</b> ",descr.adhererviz$Maintainer,"</p>",
                  "<p align='justify'>",descr.adhererviz$Description,"</p>",
                  "<p><b>Website:</b> <a href='",descr.adhererviz$URL,"' target='_blank'>",descr.adhererviz$URL,"</a></p>",
                  "<p><b>Released under:</b> ",descr.adhererviz$License,"</p>",
                  "<p><b>Citation:</b></p>",paste0(format(citation(package="AdhereRViz"),style="html"),collapse=" "),
                  "<hr/>",
                  # More info:
                  "<h2>More info</h1>",
                  "<p>For more info <b>online</b> please visit the project's <a href='http://www.adherer.eu' target='_blank'>homepage</a> (<a href='http://www.adherer.eu' target='_blank'>www.adherer.eu</a>) and its source code repository on <a href='https://github.com/ddediu/AdhereR' target='_blank'>GitHub</a> (<a href='https://github.com/ddediu/AdhereR' target='_blank'>github.com/ddediu/AdhereR</a>). ",
                  "The official releases are hosted on <a href='https://cran.r-project.org/package=AdhereR' target='_blank'>CRAN</a> (<a href='https://cran.r-project.org/package=AdhereR' target='_blank'>https://cran.r-project.org/package=AdhereR</a>).",
                  "<p><b>Offline</b> help is available within R (and RStudio):</p>",
                  "<ul>",
                  "<li>running <code>help(package='AdhereR')</code> in the R cosole displayes the <i>main documentation</i> for the package with links to detailed help for particular topics;</li>",
                  "<li>running <code>help('CMA0')</code> (or the equivalent <code>?CMA0</code>) in the R cosole displayes the <i>detailed documentation</i> the particular topic (here, CMA0); in RStudio, selecting the keyword ('CMA0') in the script editor and pressing <code>F1</code> has the same effect. Please note that to obtain help for <i>overloaded</i> functions (such as <code>plot</code>) for, say, sliding windows, one must use the fully qualified function name (here, <code>?plot.CMA_sliding_window</code>);</li>",
                  "<li>the various <i>vignettes</i> contain a lot of information about selected topics. To list all available vignettes for AdhereR, run <code>browseVignettes(package='AdhereR')</code> in the R console. Currently, the main vignettes concern:</li>",
                  "<ul>",
                  "<li><i><a href='https://CRAN.R-project.org/package=AdhereR/vignettes/AdhereR-overview.html' target='_blank'>AdhereR: Adherence to Medications</a></i> gives an overview of what AdhereR can do;</li>",
                  "<li><i><a href='https://CRAN.R-project.org/package=AdhereR/vignettes/calling-AdhereR-from-python3.html' target='_blank'>Calling AdhereR from Python 3</a></i> describes a mechanism that allows AdhereR to be used from programming languages and platforms other than R (in particular, from Python 3);</li>",
                  "<li><i><a href='https://CRAN.R-project.org/package=AdhereR/vignettes/adherer_with_databases.pdf' target='_blank'>Using AdhereR with various database technologies for processing very large datasets</a></i> described how to use AdhereR to process data stored in 'classic' SQL Relational Databases Management Systems (RDBMSs) or in Apache's Hadoop;</li>",
                  "<li><i><b><a href='https://CRAN.R-project.org/package=AdhereRViz/vignettes/adherer_interctive_plots.html' target='_blank'>AdhereR: Interactive plotting (and more) with Shiny</a></b></i> is probably the most relevant here.</li>",
                  "</ul>",
                  "</ul>",
                  "</div>");

    tryCatch(showModal(modalDialog(HTML(msg),
                                   title=NULL,
                                   footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon"))))),
             error = function(e) showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                                                       div("Cannot display the About message!", style="color: red;"),
                                                       footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))))
    );
  })


  # Show the R code box ----
  r_code <- ""; # must be global because we need to access it form other functions as well (and it's not a big object anyway)
  observeEvent(input$show_r_code,
  {
    if( is.na(.GlobalEnv$.plotting.params$.dataset.type) )
    {
      showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                            HTML("No dataset (or a NULL one) was given through the <code>data</code> argument to the <code>plot_interactive_cma()</code> function call, and no datasource was manually selected either: please select one using the <b>Data</b> tab!"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return (invisible(NULL));
    }

    if( is.null(input$patient) || length(input$patient) < 1 )
    {
      showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                            div("No patients selected, so nothing to do!", style="color: red;"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return (invisible(NULL));
    }

    # Create the R code:
    r_code <<- "";

    # Initial comments:
    r_code <<- paste0(r_code, "# The R code corresponding to the currently displayed Shiny plot:\n");
    r_code <<- paste0(r_code, "# \n");

    # The selected patients:
    r_code <<- paste0(r_code, "# Extract the data for the selected ", length(input$patient), " patient(s) with ID(s):\n");
    r_code <<- paste0(r_code, "# ",paste0('"',input$patient,'"',collapse=", "),"\n");
    r_code <<- paste0(r_code, "# \n");

    # The datasource:
    r_code <<- paste0(r_code, "# We denote here by DATA the data you are using in the Shiny plot.\n");
    if( .GlobalEnv$.plotting.params$.dataset.comes.from.function.arguments )
    {
      # The dataset came as the `data` argument to `plot_interactive_cam()`, so we don't know it's "name":
      r_code <<- paste0(r_code, "# For reasons to do with how R works, we cannot display the name\n");
      r_code <<- paste0(r_code, "# you used for it (if any), but we can tell you that it is of type\n");
      r_code <<- paste0(r_code, "# \"", paste0(class(.GlobalEnv$.plotting.params$data),collapse=","), "\", and it has the structure:\n");
      r_code <<- paste0(r_code, paste0("#   ",capture.output(str(.GlobalEnv$.plotting.params$data, vec.len=3, width=60)),collapse="\n"),"\n");
    } else
    {
      # The dataset was manually selected, so we know quite a bit about it:
      r_code <<- paste0(r_code, "# This was manually defined as ");
      r_code <<- paste0(r_code, switch(.GlobalEnv$.plotting.params$.dataset.type,
                                       "in memory"=   paste0("an object of class data.frame\n",
                                                             "# (or derived from it, such a data.table) that was already in\n",
                                                             "# memory under the name '",.GlobalEnv$.plotting.params$.dataset.name,"'.\n",
                                                             "# Assuming this object still exists with the same name, then:\n\n",
                                                             "DATA <- ",.GlobalEnv$.plotting.params$.dataset.name,";\n\n"),
                                       "from file"=   paste0("a data.frame object loaded from the\n",
                                                             "# file '",.GlobalEnv$.plotting.params$.dataset.name,"'\n",
                                                             "# of type '",.GlobalEnv$.plotting.params$.fromfile.dataset.filetype,"'\n",
                                                             "# (unfortunately, the full path to the original file cannot be recovered)\n",
                                                             "# Assuming this file still exists with the same name, then:\n\n",
                                                             switch(.GlobalEnv$.plotting.params$.fromfile.dataset.filetype,
                                                                    "Comma/TAB-separated (.csv; .tsv; .txt)"=
                                                                      paste0("DATA <- read.table(\"",.GlobalEnv$.plotting.params$.dataset.name,"\",\n",
                                                                             "    header=",.GlobalEnv$.plotting.params$.fromfile.dataset.header,",\n",
                                                                             "    sep=\"",if(.GlobalEnv$.plotting.params$.fromfile.dataset.sep=="\t") "\\t" else .GlobalEnv$.plotting.params$.fromfile.dataset.sep,"\",\n",
                                                                             "    quote=",if(.GlobalEnv$.plotting.params$.fromfile.dataset.quote=='"') "'\"'" else paste0('"',.GlobalEnv$.plotting.params$.fromfile.dataset.quote,'"'),",\n",
                                                                             "    dec=\"",.GlobalEnv$.plotting.params$.fromfile.dataset.dec,"\",\n",
                                                                             "    strip.white=",.GlobalEnv$.plotting.params$.fromfile.dataset.strip.white,",\n",
                                                                             "    na.strings=c(",paste0('"',.GlobalEnv$.plotting.params$.fromfile.dataset.na.strings,'"',collapse=","),")\n",
                                                                             ");\n"),
                                                                    "R objects from save() (.RData)"=
                                                                      paste0("# load the .RData file that contains the object using load() and\n",
                                                                             "DATA <- ",.GlobalEnv$.plotting.params$.dataset.name,";\n"),
                                                                    "Serialized R object (.rds)"=
                                                                      paste0("DATA <- readRDS(\"",.GlobalEnv$.plotting.params$.dataset.name,"\");\n"),
                                                                    "Open Document Spreadsheet (.ods)"=
                                                                      paste0("DATA <- readODS::read_ods(\"",.GlobalEnv$.plotting.params$.dataset.name,"\",\n",
                                                                             "    sheet=",.GlobalEnv$.plotting.params$.fromfile.dataset.sheet,");\n"),
                                                                    "Microsoft Excel (.xls; .xlsx)"=
                                                                      paste0("DATA <- readxl::read_excel(\"",.GlobalEnv$.plotting.params$.dataset.name,"\",\n",
                                                                             "    sheet=",.GlobalEnv$.plotting.params$.fromfile.dataset.sheet,");\n"),
                                                                    "SPSS (.sav; .por)"=
                                                                      paste0("DATA <- haven::read_spss(\"",.GlobalEnv$.plotting.params$.dataset.name,"\");\n"),
                                                                    "SAS Transport data file (.xpt)"=
                                                                      paste0("DATA <- haven::read_xpt(\"",.GlobalEnv$.plotting.params$.dataset.name,"\");\n"),
                                                                    "SAS sas7bdat data file (.sas7bdat)"=
                                                                      paste0("DATA <- haven::read_sas(\"",.GlobalEnv$.plotting.params$.dataset.name,"\");\n"),
                                                                    "Stata (.dta)"=
                                                                      paste0("DATA <- haven::read_stata(\"",.GlobalEnv$.plotting.params$.dataset.name,"\");\n"),
                                                                    "NULL; # please make sure you load this file manually!!!\n"),
                                                             "\n"),
                                       "SQL database"=paste0("a connection to the SQL database\n# '",.GlobalEnv$.plotting.params$.dataset.name,"'\n")),
                        "");
      r_code <<- paste0(r_code, "# These data has ", length(.GlobalEnv$.plotting.params$get.colnames.fnc(.GlobalEnv$.plotting.params$data)), " columns, ",
                                "and contains info for ", length(unique(.GlobalEnv$.plotting.params$get.patients.fnc(.GlobalEnv$.plotting.params$data, .GlobalEnv$.plotting.params$ID.colname))), " patients.\n");
    }

    # The medication group:
    if( !is.null(.GlobalEnv$.plotting.params$medication.groups) && input$mg_use_medication_groups )
    {
      # Defined:
      r_code <<- paste0(r_code, "# \n",
                        "# There are medication groups defined: we denote them here as MGs.\n");
      if( input$mg_definitions_source == 'named vector' &&
          (length(.GlobalEnv$.plotting.params$medication.groups) > 1 ||
           (length(.GlobalEnv$.plotting.params$medication.groups) == 1 &&
            !(.GlobalEnv$.plotting.params$medication.groups %in% .GlobalEnv$.plotting.params$get.colnames.fnc(.GlobalEnv$.plotting.params$data)))) )
      {
        # Vector defining the medication groups:
        if( .GlobalEnv$.plotting.params$.mg.comes.from.function.arguments )
        {
          # The medication groups came as the `medication.groups` argument to `plot_interactive_cam()`, so we don't know it's "name":
          r_code <<- paste0(r_code, "# For reasons to do with how R works, we cannot display the name\n");
          r_code <<- paste0(r_code, "# you used for it (if any), but we can tell you that it is of type\n");
          r_code <<- paste0(r_code, "# \"", paste0(class(.GlobalEnv$.plotting.params$medication.groups),collapse=","), "\", and has ",length(.GlobalEnv$.plotting.params$medication.groups)," elements:\n");
        } else
        {
          # The medication groups were manually selected, so we know quite a bit about them:
          r_code <<- paste0(r_code, "# The medication groups are defined using an object already\n",
                            "# in memory under the name '",.GlobalEnv$.plotting.params$.mg.name,"'.\n",
                            "# Assuming this object still exists with the same name, then:\n");
          r_code <<- paste0(r_code, "# it is of type \"", paste0(class(.GlobalEnv$.plotting.params$medication.groups),collapse=","), "\"\n",
                            "# and has ",length(.GlobalEnv$.plotting.params$medication.groups)," elements:\n");
        }
        # The elements are the same:
        r_code <<- paste0(r_code, "# c(\n");
        r_code <<- paste0(r_code,
                          paste0("#   '",names(.GlobalEnv$.plotting.params$medication.groups),"' = '",.GlobalEnv$.plotting.params$medication.groups,"'",collapse=",\n"),
                          "\n");
        r_code <<- paste0(r_code, "#  );\n",
                          "# \n");
      } else
      {
        # Column name:
        r_code <<- paste0(r_code, "# The medication groups are defined by column\n");
        r_code <<- paste0(r_code, "# '",.GlobalEnv$.plotting.params$medication.groups,"'\n");
        r_code <<- paste0(r_code, "# in the data.\n");
        r_code <<- paste0(r_code, "# \n");
      }
    }

    # The accessor functions:
    r_code <<- paste0(r_code, "# \n");
    r_code <<- paste0(r_code, "# To allow using data from other sources than a \"data.frame\"\n");
    r_code <<- paste0(r_code, "# and other similar structures (for example, from a remote SQL\n");
    r_code <<- paste0(r_code, "# database), we use a metchanism to request the data for the\n");
    r_code <<- paste0(r_code, "# selected patients that uses a function called\n");
    r_code <<- paste0(r_code, "# \"get.data.for.patients.fnc()\" which you may have redefined\n");
    r_code <<- paste0(r_code, "# to better suit your case (chances are, however, that you are\n");
    r_code <<- paste0(r_code, "# using its default version appropriate to the data source);\n");
    r_code <<- paste0(r_code, "# in any case, the following is its definition:\n");
    fnc.code <- capture.output(print(.GlobalEnv$.plotting.params$get.data.for.patients.fnc));
    if( is.null(fnc.code) || length(fnc.code) == 0 )
    {
      showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                            div("Cannot display the R code for plot!", style="color: red;"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return (invisible(NULL));
    }
    if( length(grep("<environment", fnc.code[length(fnc.code)], fixed=TRUE)) == 1 ){ fnc.code <- fnc.code[-length(fnc.code)]; }
    if( length(grep("<bytecode", fnc.code[length(fnc.code)], fixed=TRUE)) == 1 ){ fnc.code <- fnc.code[-length(fnc.code)]; }
    if( is.null(fnc.code) || length(fnc.code) == 0 )
    {
      showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                            div("Cannot display the R code for plot!", style="color: red;"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return (invisible(NULL));
    }
    if( length(fnc.code) == 1 )
    {
      r_code <<- paste0(r_code, "get.data.for.patients.fnc <- ",fnc.code,"\n");
    } else
    {
      r_code <<- paste0(r_code, "get.data.for.patients.fnc <- ",fnc.code[1],"\n");
      r_code <<- paste0(r_code, paste0(fnc.code[-1],collapse="\n"), "\n\n");
    }

    r_code <<- paste0(r_code, "# Try to extract the data only for the selected patient ID(s):\n");
    r_code <<- paste0(r_code, ".data.for.selected.patients. <- get.data.for.patients.fnc(\n");
    r_code <<- paste0(r_code, "    c(", paste0('"',input$patient,'"',collapse=", "),"),\n");
    r_code <<- paste0(r_code, "    DATA, ### don't forget to put here your REAL DATA! ###\n");
    r_code <<- paste0(r_code, "    \"",.GlobalEnv$.plotting.params$ID.colname,"\"\n");
    r_code <<- paste0(r_code, ");\n");

    r_code <<- paste0(r_code, "# Compute the appropriate CMA:\n");
    # The CMA function name:
    cma_fnc_name <- switch(input$cma_class,
                           "simple"=input$cma_to_compute,
                           "per episode"="CMA_per_episode",
                           "sliding window"="CMA_sliding_window");
    r_code <<- paste0(r_code, "cma <- ",cma_fnc_name,"("); # the CMA function call
    cma_fnc_body_indent <- paste0(rep(" ",nchar(cma_fnc_name) + nchar("cma <- ")),collapse=""); # the CMA function body indent

    # The parameters:
    r_code <<- paste0(r_code, "data=.data.for.selected.patients.,\n");
    if( input$cma_class != "simple" ) r_code <<- paste0(r_code, cma_fnc_body_indent, " ", 'CMA="',input$cma_to_compute_within_complex,'",\n');
    if( !is.null(.GlobalEnv$.plotting.params$medication.groups) && input$mg_use_medication_groups )
    {
      # Medication groups defined:
      r_code <<- paste0(r_code, cma_fnc_body_indent, " medication.groups=MGs, ### don't forget to put here your REAL medication groups! ###\n");
    }
    r_code <<- paste0(r_code, cma_fnc_body_indent, " # (please note that even if some parameters are\n");
    r_code <<- paste0(r_code, cma_fnc_body_indent, " # not relevant for a particular CMA type, we\n");
    r_code <<- paste0(r_code, cma_fnc_body_indent, " # nevertheless pass them as they will be ignored)\n");
    params.cma <- list("all"=c("ID.colname"=if(!is.na(.GlobalEnv$.plotting.params$ID.colname)) paste0('"',.GlobalEnv$.plotting.params$ID.colname,'"') else "NA",
                               "event.date.colname"=if(!is.na(.GlobalEnv$.plotting.params$event.date.colname)) paste0('"',.GlobalEnv$.plotting.params$event.date.colname,'"') else "NA",
                               "event.duration.colname"=if(!is.na(.GlobalEnv$.plotting.params$event.duration.colname)) paste0('"',.GlobalEnv$.plotting.params$event.duration.colname,'"') else "NA",
                               "event.daily.dose.colname"=if(!is.na(.GlobalEnv$.plotting.params$event.daily.dose.colname)) paste0('"',.GlobalEnv$.plotting.params$event.daily.dose.colname,'"') else "NA",
                               "medication.class.colname"=if(!is.na(.GlobalEnv$.plotting.params$medication.class.colname)) paste0('"',.GlobalEnv$.plotting.params$medication.class.colname,'"') else "NA",
                               #"carryover.within.obs.window"=NA,
                               #"carryover.into.obs.window"=NA,
                               "carry.only.for.same.medication"=input$carry_only_for_same_medication,
                               "consider.dosage.change"=input$consider_dosage_change,
                               "followup.window.start"=ifelse(input$followup_window_start_unit=="calendar date",
                                                              paste0('"',as.character(as.Date(input$followup_window_start_date, format="%Y-%m-%d"), format=.GlobalEnv$.plotting.params$date.format),'"'),
                                                              input$followup_window_start_no_units),
                               "followup.window.start.unit"=ifelse(input$followup_window_start_unit=="calendar date", '"days"', paste0('"',input$followup_window_start_unit,'"')),
                               "followup.window.duration"=input$followup_window_duration,
                               "followup.window.duration.unit"=paste0('"',input$followup_window_duration_unit,'"'),
                               "observation.window.start"=ifelse(input$observation_window_start_unit=="calendar date",
                                                                 paste0('"',as.character(as.Date(input$observation_window_start_date, format="%Y-%m-%d"), format=.GlobalEnv$.plotting.params$date.format),'"'),
                                                                 input$observation_window_start_no_units),
                               "observation.window.start.unit"=ifelse(input$observation_window_start_unit=="calendar date", '"days"', paste0('"',input$observation_window_start_unit,'"')),
                               "observation.window.duration"=input$observation_window_duration,
                               "observation.window.duration.unit"=paste0('"',input$observation_window_duration_unit,'"')),
                       "per.episode"=c("medication.change.means.new.treatment.episode"=input$medication_change_means_new_treatment_episode,
                                       "dosage.change.means.new.treatment.episode"=input$dosage_change_means_new_treatment_episode,
                                       "maximum.permissible.gap"=input$maximum_permissible_gap,
                                       "maximum.permissible.gap.unit"=paste0('"',input$maximum_permissible_gap_unit,'"'),
                                       "maximum.permissible.gap.append.to.episode"=input$maximum_permissible_gap_append),
                       "sliding.window"=c("sliding.window.start"=as.numeric(input$sliding_window_start),
                                          "sliding.window.start.unit"=paste0('"',input$sliding_window_start_unit,'"'),
                                          "sliding.window.duration"=input$sliding_window_duration,
                                          "sliding.window.duration.unit"=paste0('"',input$sliding_window_duration_unit,'"'),
                                          "sliding.window.step.duration"=input$sliding_window_step_duration,
                                          "sliding.window.step.unit"=paste0('"',input$sliding_window_step_unit,'"'),
                                          "sliding.window.no.steps"=ifelse(input$sliding_window_step_choice=="number of steps", input$sliding_window_no_steps, NA)),
                       "date.format"=paste0('"',.GlobalEnv$.plotting.params$date.format,'"') # keep date.format as last to avoid issues with dangling commas
    );
    r_code <<- paste0(r_code, paste0(cma_fnc_body_indent, " ", names(params.cma$all), "=", params.cma$all, collapse=",\n"), ",\n");
    if( input$cma_class == "per episode" ) r_code <<- paste0(r_code, paste0(cma_fnc_body_indent, " ", names(params.cma$per.episode), "=", params.cma$per.episode, collapse=",\n"), ",\n");
    if( input$cma_class == "sliding window" ) r_code <<- paste0(r_code, paste0(cma_fnc_body_indent, " ", names(params.cma$sliding.window), "=", params.cma$sliding.window, collapse=",\n"), ",\n");
    r_code <<- paste0(r_code, cma_fnc_body_indent, " date.format=", params.cma$date.format, "\n");

    # End end of CMA function call:
    r_code <<- paste0(r_code, cma_fnc_body_indent,");\n\n");

    # The plotting:
    r_code <<- paste0(r_code, "if( !is.null(cma) ) # if the CMA was computed ok\n");
    r_code <<- paste0(r_code, "{\n");
    r_code <<- paste0(r_code, "    # Try to plot it:\n");
    r_code <<- paste0(r_code, "    plot(cma,\n");
    r_code <<- paste0(r_code, "         # (same idea as for CMA: we send arguments even if\n");
    r_code <<- paste0(r_code, "         # they aren't used in a particular case)\n");

    params.plot <- c("align.all.patients"=input$plot_align_all_patients,
                     "align.first.event.at.zero"=input$plot_align_first_event_at_zero,
                     "show.legend"=input$show_legend,
                     "legend.x"=ifelse( is.numeric(input$legend_x), input$legend_x, paste0('"',input$legend_x,'"')),
                     "legend.y"=ifelse( is.numeric(input$legend_y), input$legend_y, paste0('"',input$legend_y,'"')),
                     "legend.bkg.opacity"=input$legend_bkg_opacity,
                     "legend.cex"=max(0.01,input$legend_cex),
                     "legend.cex.title"=max(0.01,input$legend_cex_title),
                     "duration"=ifelse(input$duration==0, NA, input$duration),
                     "show.period"=ifelse(length(input$patient) > 1 && input$plot_align_all_patients, '"days"', paste0('"',input$show_period,'"')),
                     "period.in.days"=input$period_in_days,
                     "bw.plot"=input$bw_plot,
                     #show.cma=input$show_cma,
                     "col.na"=paste0('"',input$col_na,'"'),
                     "unspecified.category.label"=paste0('"',input$unspecified_category_label,'"'),
                     "col.cats"=input$col_cats,
                     "lty.event"=paste0('"',input$lty_event,'"'),
                     "lwd.event"=input$lwd_event,
                     "pch.start.event"=input$pch_start_event,
                     "pch.end.event"=input$pch_end_event,
                     "col.continuation"=paste0('"',input$col_continuation,'"'),
                     "lty.continuation"=paste0('"',input$lty_continuation,'"'),
                     "lwd.continuation"=input$lwd_continuation,
                     "cex"=max(0.01,input$cex),
                     "cex.axis"=max(0.01,input$cex_axis),
                     "cex.lab"=max(0.01,input$cex_lab),
                     "force.draw.text"=input$force.draw.text,
                     "highlight.followup.window"=input$highlight_followup_window,
                     "followup.window.col"=paste0('"',input$followup_window_col,'"'),
                     "highlight.observation.window"=input$highlight_observation_window,
                     "observation.window.col"=paste0('"',input$observation_window_col,'"'),
                     #"observation.window.density"=input$observation_window_density,
                     #"observation.window.angle"=input$observation_window_angle,
                     "observation.window.opacity"=input$observation_window_opacity,
                     "show.real.obs.window.start"=input$show_real_obs_window_start,
                     #"real.obs.window.density"=input$real_obs_window_density,
                     #"real.obs.window.angle"=input$real_obs_window_angle,
                     "print.CMA"=input$print_cma,
                     "CMA.cex"=max(0.01,input$cma_cex),
                     "plot.CMA"=input$plot_cma,
                     "CMA.plot.ratio"=input$cma_plot_ratio / 100.0,
                     "CMA.plot.col"=paste0('"',input$cma_plot_col,'"'),
                     "CMA.plot.border"=paste0('"',input$cma_plot_border,'"'),
                     "CMA.plot.bkg"=paste0('"',input$cma_plot_bkg,'"'),
                     "CMA.plot.text"=paste0('"',input$cma_plot_text,'"'),
                     "plot.CMA.as.histogram"=ifelse(input$cma_class=="sliding window", !input$plot_CMA_as_histogram_sliding_window, !input$plot_CMA_as_histogram_episodes),
                     "show.event.intervals"=input$show_event_intervals,
                     "print.dose"=input$print_dose,
                     "cex.dose"=max(0.01,input$cex.dose),
                     "print.dose.outline.col"=paste0('"',input$print_dose_outline_col,'"'),
                     "print.dose.centered"=input$print_dose_centered,
                     "plot.dose"=input$plot_dose,
                     "lwd.event.max.dose"=input$lwd_event_max_dose,
                     "plot.dose.lwd.across.medication.classes"=input$plot_dose_lwd_across_medication_classes,
                     "show.overlapping.event.intervals"=input$overlapping_evint,
                     "min.plot.size.in.characters.horiz"=input$min_plot_size_in_characters_horiz,
                     "min.plot.size.in.characters.vert"=input$min_plot_size_in_characters_vert,
                     "medication.groups.to.plot"=ifelse(!is.null(.GlobalEnv$.plotting.params$medication.groups) && input$mg_use_medication_groups,
                                                        paste0("c(",
                                                               paste0('"',
                                                                      ifelse(input$mg_to_plot_list=="* (all others)","__ALL_OTHERS__",input$mg_to_plot_list),
                                                                      '"',collapse=","),
                                                               ")"),
                                                        "NULL"),
                     "medication.groups.separator.show"=input$mg_plot_by_patient,
                     "medication.groups.separator.lty"=paste0('"',input$plot_mg_separator_lty,'"'),
                     "medication.groups.separator.lwd"=input$plot_mg_separator_lwd,
                     "medication.groups.separator.color"=paste0('"',input$plot_mg_separator_color,'"'),
                     "medication.groups.allother.label"=paste0('"',input$plot_mg_allothers_label,'"')
    );
    r_code <<- paste0(r_code, paste0("         ", names(params.plot), "=", params.plot, collapse=",\n"), "\n");
    r_code <<- paste0(r_code, "    );\n");
    r_code <<- paste0(r_code, "}\n");

    ## DEBUG:
    #cat(r_code);

    tryCatch(showModal(modalDialog(div(div(HTML("<p>This is the <code>R</code> that would generate the plot currently seen. You can copy it to the clipboard using the <i>Copy to clipboard</i> button.</p>
                                                <p>Please note that the parameter values <b><code>DATA</code></b> and <b><code>MGs</code></b> <i>must be replaced</i> by the actual data and medication groups (if the case) you passed to the Shiny interactive plot function!</p>")),
                                       div(HTML(gsub('<span class="symbol">MGs</span>','<span class="symbol_data">MGs</span>',
                                                     gsub('<span class="symbol">DATA</span>','<span class="symbol_data">DATA</span>',
                                                          highlight::highlight(parse.output=parse(text=r_code),
                                                                               renderer=highlight::renderer_html(document=TRUE,
                                                                                                                 stylesheet=file.path(system.file('interactivePlotShiny',
                                                                                                                                                  package='AdhereRViz'),
                                                                                                                                      "r-code-highlight.css")),
                                                                               show_line_numbers=FALSE,
                                                                               output=NULL),
                                                          fixed=TRUE),
                                                     fixed=TRUE)),
                                           #div(HTML(highlight::external_highlight(code=r_code, theme="acid", lang="r", type="HTML", doc=TRUE, file=NULL, outfile=NULL)),
                                           style="max-height: 50vh; overflow-x: scroll; overflow-y: scroll; white-space: nowrap;")), # overflow: auto;
                                   title=HTML("The <code>R</code> code for the current plot..."),
                                   footer = tagList(actionButton("copy_code", "Copy to clipboard", icon=icon("copy", lib="glyphicon")),
                                                    modalButton("Close", icon=icon("ok", lib="glyphicon"))))),
             error = function(e) showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                                                       div("Cannot display the R code for plot!", style="color: red;"),
                                                       footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))))
    );
  })
  observeEvent(input$copy_code,
  {
    if( clipr::clipr_available() ) clipr::write_clip(r_code, object_type="character");
  })


  # Close shop nicely ----
  observeEvent(input$close_shop,
  {
    showModal(modalDialog(title="AdhereR interactive plotting...", "Are you sure you want to close the interactive plotting?",
                          footer = tagList(modalButton("No", icon=icon("remove-circle", lib="glyphicon")),
                                           actionButton("ok", "Yes", icon=icon("ok-circle", lib="glyphicon")))))
  })
  observeEvent(input$ok,
  {
    removeModal();
    stopApp();
  })

  # Show/hide panel sections ----
  .toggle.all.sections <- function(id=c("follow_up"), anim=TRUE, animType=c("slide","fade")[1])
  {
    shinyjs::toggle(id=paste0(id,"_unfold_icon"), anim=anim, animType=animType); # the unfolding icon
    shinyjs::toggle(id=paste0(id,"_contents"),    anim=anim, animType=animType); # the section content
  }
  shinyjs::onclick("mg_section",               function(e){.toggle.all.sections("mg");})
  shinyjs::onclick("follow_up_section",        function(e){.toggle.all.sections("follow_up");})
  shinyjs::onclick("general_settings_section", function(e){.toggle.all.sections("general_settings");})
  shinyjs::onclick("observation_section",      function(e){.toggle.all.sections("observation");})
  shinyjs::onclick("cma_plus_section",         function(e){.toggle.all.sections("cma_plus");})
  shinyjs::onclick("episodes_section",         function(e){.toggle.all.sections("episodes");})
  shinyjs::onclick("sliding_windows_section",  function(e){.toggle.all.sections("sliding_windows");})
  shinyjs::onclick("align_section",            function(e){.toggle.all.sections("align");})
  shinyjs::onclick("duration_period_section",  function(e){.toggle.all.sections("duration_period");})
  shinyjs::onclick("cma_estimate_section",     function(e){.toggle.all.sections("cma_estimate");})
  shinyjs::onclick("dose_section",             function(e){.toggle.all.sections("dose");})
  shinyjs::onclick("legend_section",           function(e){.toggle.all.sections("legend");})
  shinyjs::onclick("aesthetics_section",       function(e){.toggle.all.sections("aesthetics");})
  shinyjs::onclick("advanced_section",         function(e){.toggle.all.sections("advanced");})


  # Recursively list objects in memory ----
  .recursively.list.objects.in.memory <- function(..., # inspired from http://adv-r.had.co.nz/Environments.html#env-recursion
                                                  env = parent.frame(),
                                                  of.class="data.frame", # if NULL, no type testing (all go)
                                                  min.nrow=1, min.ncol=3, # NA if not relevant
                                                  return.dimensions=TRUE,
                                                  consider.derived.classes=TRUE)
  {
    if( identical(env, emptyenv()) )
    {
      # No objects in the empty environment
      return (NULL);
    } else
    {
      # List all objects in this environment:
      all.objects <- objects(envir=env, all.names=TRUE);
      # Check each objects' class:
      if( !is.null(of.class) )
      {
        # Do type testing:
        objects.to.keep <- vapply(all.objects, function(s)
        {
          x <- get(s, envir=env); # get the actual object
          if( (!consider.derived.classes && (of.class %in% class(x))) || (consider.derived.classes && inherits(x, of.class)) )
          {
            if( !is.na(min.nrow) && !is.na(min.ncol) )
            {
              return (nrow(x) >= min.nrow && ncol(x) >= min.ncol);
            } else
            {
              return (TRUE);
            }
          } else
          {
            return (FALSE);
          }
        }, logical(1));
        all.objects <- all.objects[ objects.to.keep ];
      }

      # Recursive processing:
      all.objects <- c(all.objects,
                       .recursively.list.objects.in.memory(...,
                                                           env = parent.env(env),
                                                           of.class=of.class,
                                                           min.nrow=min.nrow, min.ncol=min.ncol,
                                                           return.dimensions=return.dimensions,
                                                           consider.derived.classes=consider.derived.classes));
      # Return the list of objects:
      return (all.objects);
    }
  }

  # In-memory dataset: update the list of data.frame-derived objects all the way to the base environment ----
  observeEvent(input$datasource_type,
  {
    if( input$datasource_type == "already in memory" )
    {
      # List all the data.frame-derived objects currently in memory:
      x <- sort(.recursively.list.objects.in.memory());

      # If defined, add the data.frame sent to the Shiny plotting function:
      if( !is.null(.GlobalEnv$.plotting.params) &&
          !is.null(.GlobalEnv$.plotting.params$data) &&
          inherits(.GlobalEnv$.plotting.params$data, "data.frame"))
      {
        if( .GlobalEnv$.plotting.params$.dataset.comes.from.function.arguments )
        {
          # Add the function argument as well:
          x <- c("<<'data' argument to plot_interactive_cma() call>>", x);
        }
      }
      updateSelectInput(session, "dataset_from_memory", choices=x, selected=head(x,1));
    }
  })

  # In-memory dataset: list the columns and update the selections ----
  observeEvent(input$dataset_from_memory,
  {
    # Disconnect any pre-existing database connections:
    if( !is.null(.GlobalEnv$.plotting.params$.db.connection) )
    {
      try(DBI::dbDisconnect(.GlobalEnv$.plotting.params$.db.connection), silent=TRUE);
      .GlobalEnv$.plotting.params$.db.connection <- NULL;
    }

    # Set the data.frame:
    .GlobalEnv$.plotting.params$.inmemory.dataset <- NULL;
    if( input$dataset_from_memory == "[none]" || input$dataset_from_memory == "" )
    {
      # Initialisation:
      return (invisible(NULL));
    } else if( input$dataset_from_memory == "<<'data' argument to plot_interactive_cma() call>>" )
    {
      # The special value pointing to the argument to plot_interactive_cma():
      .GlobalEnv$.plotting.params$.inmemory.dataset <- .GlobalEnv$.plotting.params$data;
    } else
    {
      # Try to find it memory:
      try(.GlobalEnv$.plotting.params$.inmemory.dataset <- get(input$dataset_from_memory), silent=TRUE);
    }

    # Sanity checks:
    if( (input$dataset_from_memory != "[none]") &&
        (is.null(.GlobalEnv$.plotting.params$.inmemory.dataset) ||
         !inherits(.GlobalEnv$.plotting.params$.inmemory.dataset, "data.frame") ||
         ncol(.GlobalEnv$.plotting.params$.inmemory.dataset) < 1 ||
         nrow(.GlobalEnv$.plotting.params$.inmemory.dataset) < 1 ))
    {
      showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                            div(paste0("Cannot load the selected dataset '",input$dataset_from_memory, "' from memory!"), style="color: red;"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return (invisible(NULL));
    }

    if( (input$dataset_from_memory != "[none]") && nrow(.GlobalEnv$.plotting.params$.inmemory.dataset) < 3 )
    {
      showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                            div(paste0("Dataset '",input$dataset_from_memory, "' must have at least three distinct columns (patient ID, event date and duration)!"), style="color: red;"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return (invisible(NULL));
    }

    n.vals.to.show <-3;
    d <- as.data.frame(.GlobalEnv$.plotting.params$.inmemory.dataset);
    x <- names(d);
    x.info <- vapply(1:ncol(d),
                     function(i) paste0("(",
                                        paste0(class(d[,i]),collapse=","),
                                        ": ",
                                        paste0(d[1:min(n.vals.to.show,nrow(d)),i],collapse=", "),
                                        if(nrow(d)>n.vals.to.show) "...",
                                        ")"),
                     character(1));

    # Required columns:
    shinyWidgets::updatePickerInput(session, "dataset_from_memory_patient_id",     choices=x, selected=x[1], choicesOpt=list(subtext=x.info));
    shinyWidgets::updatePickerInput(session, "dataset_from_memory_event_date",     choices=x, selected=x[2], choicesOpt=list(subtext=x.info));
    shinyWidgets::updatePickerInput(session, "dataset_from_memory_event_duration", choices=x, selected=x[3], choicesOpt=list(subtext=x.info));

    # Optional columns (possibly used by CMA5+):
    shinyWidgets::updatePickerInput(session, "dataset_from_memory_medication_class", choices=c("[not defined]", x), selected="[not defined]", choicesOpt=list(subtext=c("", x.info)));
    shinyWidgets::updatePickerInput(session, "dataset_from_memory_daily_dose",       choices=c("[not defined]", x), selected="[not defined]", choicesOpt=list(subtext=c("", x.info)));

    # Medication groups:
    shinyWidgets::updatePickerInput(session, "mg_from_column", choices=x, selected=x[1], choicesOpt=list(subtext=x.info));
  })

  # Display a data.frame as a nice HTML table ----
  .show.data.frame.as.HTML <- function(d, # the data.frame-derived object to show
                                       max.rows=50, # if NA, show all
                                       escape=TRUE)
  {
    if( is.null(d) || !inherits(d, "data.frame") || nrow(d) < 1 || ncol(d) < 3 )
    {
      return ("<b>The given dataset is empty, of the wrong type, or too small!</b>");
    }

    # This is a pretty basic thing thet tweaks the output of knitr::kable...
    d.as.html <- knitr::kable(d[1:min(max.rows,nrow(d),na.rm=TRUE),], format="html",
                              align="c",
                              col.names=names(d), #col.names=paste0("\U2007\U2007",names(d),"\U2007\U2007"),
                              row.names=FALSE);

    # The data.frame info in a nice HTML format:
    d.info <- paste0("An object of type", ifelse(length(class(d))>1,"s "," "),
                     paste0("<i>",class(d),"</i>", collapse=", "),
                     ". It has ", nrow(d), " rows × ", ncol(d), " columns [with type(s)]: <br/>",
                     paste0(vapply(1:ncol(d),function(i) paste0("<b>",names(d)[i], "</b> [", paste0("<i>",class(d[,i]),"</i>",collapse=", "), "]"), character(1)), collapse=", "),
                     ".");

    if( length(s <- strsplit(d.as.html, "<table", fixed=TRUE)[[1]]) > 1 )
    {
      # Found the <table> tag: add its class and caption:
      d.as.html <- paste0(s[1],
                          paste0("<table class='peekAtTable'",
                                 "<caption style='caption-side: top;'>", d.info,"</caption", # > is already in s[2] because of <table
                                 s[-1]));

      # Add the CSS:
      d.as.html <- paste0(d.as.html, "\n\n
                          <style>
                            table.peekAtTable {
                              border: 1px solid #1C6EA4;
                              background-color: #EEEEEE;
                              #width: 100%;
                              text-align: left;
                              border-collapse: collapse;
                              white-space: nowrap;
                            }
                            table.peekAtTable td, table.peekAtTable th {
                              border: 1px solid #AAAAAA;
                              padding: 0.2em 0.5em;
                            }
                            table.peekAtTable tbody td {
                              font-size: 13px;
                            }
                            table.peekAtTable tr:nth-child(even) {
                              background: #D0E4F5;
                            }
                            table.peekAtTable thead {
                              background: #1C6EA4;
                              background: -moz-linear-gradient(top, #5592bb 0%, #327cad 66%, #1C6EA4 100%);
                              background: -webkit-linear-gradient(top, #5592bb 0%, #327cad 66%, #1C6EA4 100%);
                              background: linear-gradient(to bottom, #5592bb 0%, #327cad 66%, #1C6EA4 100%);
                              border-bottom: 2px solid #444444;
                            }
                            table.peekAtTable thead th {
                              font-size: 15px;
                              font-weight: bold;
                              color: #FFFFFF;
                              border-left: 2px solid #D0E4F5;
                            }
                            table.peekAtTable thead th:first-child {
                              border-left: none;
                            }
                          </style>\n");
    }

    return (d.as.html);
  }

  # In-memory dataset: peek ----
  observeEvent(input$dataset_from_memory_peek_button,
  {
    # Sanity checks:
    if( is.null(.GlobalEnv$.plotting.params$.inmemory.dataset) ||
        !inherits(.GlobalEnv$.plotting.params$.inmemory.dataset, "data.frame") ||
        ncol(.GlobalEnv$.plotting.params$.inmemory.dataset) < 1 ||
        nrow(.GlobalEnv$.plotting.params$.inmemory.dataset) < 3 )
    {
      showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                            div(paste0("Cannot load the selected dataset '",input$dataset_from_memory, "' from memory!\nPlease make sure you selected a valid data.frame (or derived object) with at least 3 columns and 1 row..."), style="color: red;"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return (invisible(NULL));
    }

    showModal(modalDialog(title="AdhereR: peeking at the selected in-memory dataset ...",
                          div(HTML(.show.data.frame.as.HTML(.GlobalEnv$.plotting.params$.inmemory.dataset)),
                                   style="max-height: 50vh; max-width: 90vw; overflow: auto; overflow-x:auto;"),
                          footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));

  })

  # Validate a given dataset and possibly load it ----
  .validate.and.load.dataset <- function(d, # the dataset
                                         get.colnames.fnc, get.patients.fnc, get.data.for.patients.fnc, # getter functions appropriate for the dataset
                                         min.npats=1, min.ncol=3, # minimum number of patients and columns
                                         ID.colname, event.date.colname, event.duration.colname, event.daily.dose.colname, medication.class.colname, # column names
                                         date.format # date format
  )
  {
    # Check dataset dimensions:
    all.IDs <- get.patients.fnc(d, ID.colname);
    if( length(all.IDs) < min.npats || length(get.colnames.fnc(d)) < min.ncol )
    {
      showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                            div(paste0("Cannot load the selected dataset!\nPlease make sure your selection is valid and has at least ",min.ncol," column(s) and data for at least ",min.npats," patient(s)..."), style="color: red;"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return (invisible(NULL));
    }

    # Check if the column names refer to existing columns in the dataset:
    if( is.na(ID.colname) || length(ID.colname) != 1 || ID.colname=="" || !(ID.colname %in% get.colnames.fnc(d)) )
    {
      showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                            div(paste0("Patient ID column '",ID.colname, "' must be a string and a valid column name in the selected dataset..."), style="color: red;"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return (invisible(NULL));
    }

    if( is.na(event.date.colname) || length(event.date.colname) != 1 || event.date.colname=="" || !(event.date.colname %in% get.colnames.fnc(d)) )
    {
      showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                            div(paste0("Event date column '",event.date.colname, "' must be a string and a valid column name in the selected dataset..."), style="color: red;"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return (invisible(NULL));
    }

    if( is.na(event.duration.colname) || length(event.duration.colname) != 1 || event.duration.colname=="" || !(event.duration.colname %in% get.colnames.fnc(d)) )
    {
      showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                            div(paste0("Event duration column '",event.duration.colname, "' must be a string and a valid column name in the selected dataset..."), style="color: red;"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return (invisible(NULL));
    }

    if( is.na(event.daily.dose.colname) || length(event.daily.dose.colname) != 1 || event.daily.dose.colname=="" )
    {
      showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                            div(paste0("Event duration column '",event.daily.dose.colname, "' must be a non-empty string..."), style="color: red;"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return (invisible(NULL));
    } else if( event.daily.dose.colname == "[not defined]" )
    {
      event.daily.dose.colname <- NA; # not defined
    } else if( !(event.daily.dose.colname %in% get.colnames.fnc(d)) )
    {
      showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                            div(paste0("Event duration column '",event.daily.dose.colname, "' if given, must be either '[not defined]' or a valid column name in the selected dataset..."), style="color: red;"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return (invisible(NULL));
    }

    if( is.na(medication.class.colname) || length(medication.class.colname) != 1 || medication.class.colname=="" )
    {
      showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                            div(paste0("Treatment class column '",medication.class.colname, "' must be a non-empty string..."), style="color: red;"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return (invisible(NULL));
    } else if( medication.class.colname == "[not defined]" )
    {
      medication.class.colname <- NA; # not defined
    } else if( !(medication.class.colname %in% get.colnames.fnc(d)) )
    {
      showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                            div(paste0("Treatment class column '",medication.class.colname, "' if given, must be either '[not defined]' or a valid column name in the selected dataset..."), style="color: red;"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return (invisible(NULL));
    }

    # Check if the column names are unique (i.e., do not repeat):
    if( anyDuplicated(na.omit(c(ID.colname, event.date.colname, event.duration.colname, event.daily.dose.colname, medication.class.colname))) )
    {
      showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                            div("The selected column names must be unique!", style="color: red;"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return (invisible(NULL));
    }

    # More advanced checks of the column types:
    if( inherits(d, "data.frame") ) # for data.frame's
    {
      d <- as.data.frame(d); # force it to a data.frame to avoid unexpected behaviours from derived classes
      if( inherits(d[,event.date.colname], "Date") )
      {
        # It's a column of Dates: perfect!
      } else if( is.factor(d[,event.date.colname]) || is.character(d[,event.date.colname]) )
      {
        # It's a factor or string: check if it conforms to the given date.format:
        s <- na.omit(as.character(d[,event.date.colname]));
        if( length(s) == 0 )
        {
          showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                                div(paste0("There are no non-missing dates in the '",event.date.colname,"' column!"), style="color: red;"),
                                footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
          return (invisible(NULL));
        }
        tmp <- as.Date(s, format=input$dataset_from_memory_event_format);
        if( all(is.na(tmp)) )
        {
          showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                                div(paste0("Please check if the date format is correct and fits the actual dates in the '",event.date.colname,"' column: all conversions failed!"), style="color: red;"),
                                footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
          return (invisible(NULL));
        } else if( any(is.na(tmp)) )
        {
          showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                                div(paste0("Please check if the date format is correct and fits the actual dates in the '",event.date.colname,"' column: ", length(is.na(tmp))," conversions failed!"), style="color: red;"),
                                footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
          return (invisible(NULL));
        }
      } else
      {
        showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                              div(paste0("The event date column '",event.date.colname,"' must contain either objects of class 'Date' or correctly-formatted strings (or factor levels)!"), style="color: red;"),
                              footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        return (invisible(NULL));
      }

      if( !is.na(event.duration.colname) && (!is.numeric(d[,event.duration.colname]) || any(d[,event.duration.colname] < 0, na.rm=TRUE)) )
      {
        showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                              div(paste0("If given, the event duration column '",event.duration.colname,"' must contain non-negative numbers!"), style="color: red;"),
                              footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        return (invisible(NULL));
      }

      if( !is.na(event.daily.dose.colname) && (!is.numeric(d[,event.daily.dose.colname]) || any(d[,event.daily.dose.colname] < 0, na.rm=TRUE)) )
      {
        showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                              div(paste0("If given, the daily dose column '",event.daily.dose.colname,"' must contain non-negative numbers!"), style="color: red;"),
                              footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        return (invisible(NULL));
      }
    }

    # Even more complex check: try to compute CMA0 on the first patient:
    test.cma <- NULL;
    test.res <- tryCatch(test.cma <- AdhereR::CMA0(data=get.data.for.patients.fnc(all.IDs[1], d, ID.colname),
                                                   ID.colname=ID.colname,
                                                   event.date.colname=event.date.colname,
                                                   event.duration.colname=event.duration.colname,
                                                   event.daily.dose.colname=event.daily.dose.colname,
                                                   medication.class.colname=medication.class.colname,
                                                   date.format=date.format),
                         error=function(e) e, warning=function(w) w);
    if( is.null(test.cma) || inherits(test.res, "error") )
    {
      # Some error occured!
      showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                            div("There's something wrong with these data!\nI tried to create a CMA0 object and this is what I got back:\n"), div(as.character(test.res), style="color: red;"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return (invisible(NULL));
    } else
    {
      if( inherits(test.res, "warning") )
      {
        showModal(modalDialog(title=div(icon("warning-sign", lib="glyphicon"), "AdhereR warning!"),
                              div("These data seem ok, but when I tried to create a CMA0 object I got some warnings:\n"), div(as.character(test.res), style="color: blue;"),
                              footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      }
    }

    ### Now, really load the data! ###
    # Place the data in the .GlobalEnv$.plotting.params list:
    .GlobalEnv$.plotting.params$data <- d;
    .GlobalEnv$.plotting.params$ID.colname <- ID.colname;
    .GlobalEnv$.plotting.params$event.date.colname <- event.date.colname;
    .GlobalEnv$.plotting.params$event.duration.colname <- event.duration.colname;
    .GlobalEnv$.plotting.params$event.daily.dose.colname <- event.daily.dose.colname;
    .GlobalEnv$.plotting.params$medication.class.colname <- medication.class.colname;
    .GlobalEnv$.plotting.params$date.format <- date.format;
    # This is a data.frame, so use the appropriate getters:
    .GlobalEnv$.plotting.params$get.colnames.fnc <- get.colnames.fnc;
    .GlobalEnv$.plotting.params$get.patients.fnc <- get.patients.fnc;
    .GlobalEnv$.plotting.params$get.data.for.patients.fnc <- get.data.for.patients.fnc;
    # CMA class:
    .GlobalEnv$.plotting.params$cma.class <- "simple";
    # Patient IDs and selected ID:
    .GlobalEnv$.plotting.params$all.IDs <- all.IDs;
    .GlobalEnv$.plotting.params$ID <- all.IDs[1];

    # Force UI updating...
    .force.update.UI();
  }

  # In-memory dataset: validate and use ----
  observeEvent(input$dataset_from_memory_button_use,
  {
    # Sanity checks:
    if( is.null(.GlobalEnv$.plotting.params$.inmemory.dataset) ||
        !inherits(.GlobalEnv$.plotting.params$.inmemory.dataset, "data.frame") ||
        ncol(.GlobalEnv$.plotting.params$.inmemory.dataset) < 1 ||
        nrow(.GlobalEnv$.plotting.params$.inmemory.dataset) < 3 )
    {
      showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                            div(paste0("Cannot load the selected dataset '",input$dataset_from_memory, "' from memory!\nPlease make sure you selected a valid data.frame (or derived object) with at least 3 columns and 1 row..."), style="color: red;"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return (invisible(NULL));
    }

    # Checks:
    .validate.and.load.dataset(.GlobalEnv$.plotting.params$.inmemory.dataset,
                               get.colnames.fnc=function(d) names(d),
                               get.patients.fnc=function(d, idcol) unique(d[[idcol]]),
                               get.data.for.patients.fnc=function(patientid, d, idcol, cols=NA, maxrows=NA) d[ d[[idcol]] %in% patientid, ],
                               ID.colname=input$dataset_from_memory_patient_id,
                               event.date.colname=input$dataset_from_memory_event_date,
                               event.duration.colname=input$dataset_from_memory_event_duration,
                               event.daily.dose.colname=input$dataset_from_memory_daily_dose,
                               medication.class.colname=input$dataset_from_memory_medication_class,
                               date.format=input$dataset_from_memory_event_format);

     # Let the world know this:
    .GlobalEnv$.plotting.params$.dataset.type <- "in memory";
    .GlobalEnv$.plotting.params$.dataset.comes.from.function.arguments <- FALSE;
    if( input$dataset_from_memory == "[none]" )
    {
      # How did we get here???
      return (invisible(NULL));
    } else if( input$dataset_from_memory == "<<'data' argument to plot_interactive_cma() call>>" )
    {
      # The special value pointing to the argument to plot_interactive_cma():
      .GlobalEnv$.plotting.params$.dataset.name <- NA;
    } else
    {
      .GlobalEnv$.plotting.params$.dataset.name <- input$dataset_from_memory;
    }
  })

  # Force updating the Shiny UI using the new data ----
  .force.update.UI <- function()
  {
    updateSelectInput(session, "cma_class", selected=.GlobalEnv$.plotting.params$cma.class);
    #updateSelectInput(session, "cma_to_compute", selected=.GlobalEnv$.plotting.params$cma.class);
    updateSelectInput(session, "patient", choices=.GlobalEnv$.plotting.params$all.IDs, selected=.GlobalEnv$.plotting.params$ID);

    updateSelectInput(session, "compute_cma_patient_by_id", choices=.GlobalEnv$.plotting.params$all.IDs, selected=.GlobalEnv$.plotting.params$all.IDs[1]);

    if( input$mg_definitions_source == 'named vector' )
    {
      shinyWidgets::updatePickerInput(session, "mg_to_plot_list",
                                      choices=c(names(.GlobalEnv$.plotting.params$medication.groups), "* (all others)"),
                                      selected=c(names(.GlobalEnv$.plotting.params$medication.groups), "* (all others)"));
    } else if( input$mg_definitions_source == 'column in data' )
    {
      if( !is.null(.GlobalEnv$.plotting.params$data) )
      {
        mg_vals <- .GlobalEnv$.plotting.params$get.data.for.patients.fnc(.GlobalEnv$.plotting.params$all.IDs,
                                                                         .GlobalEnv$.plotting.params$data,
                                                                         idcol=.GlobalEnv$.plotting.params$ID.colname);
        if( !is.null(mg_vals) && length(mg_vals) > 0 )
        {
          mg_vals <- unique(mg_vals[, input$mg_from_column]);
          mg_vals <- mg_vals[ !is.na(mg_vals) ];
          if( !is.null(mg_vals) && length(mg_vals) > 0 )
          {
            mg_vals <- sort(mg_vals);
          }
        }
      } else
      {
        mg_vals <- NULL;
      }
      shinyWidgets::updatePickerInput(session, "mg_to_plot_list",
                                      choices=c(mg_vals, "* (all others)"),
                                      selected=c(mg_vals, "* (all others)"));
    }

    #if( is.na(.GlobalEnv$.plotting.params$event.daily.dose.colname) ) shinyjs::hide(id="dose_is_defined") else shinyjs::show(id="dose_is_defined");
    output$is_dose_defined <- reactive({!is.null(.GlobalEnv$.plotting.params$event.daily.dose.colname) && !is.na(.GlobalEnv$.plotting.params$event.daily.dose.colname)});
    output$is_treat_class_defined <- reactive({!is.null(.GlobalEnv$.plotting.params$medication.class.colname) && !is.na(.GlobalEnv$.plotting.params$medication.class.colname)});

    rv$toggle.me <- !rv$toggle.me; # make the plotting aware of a change (even if we did not change any UI elements)

    output$is_dataset_defined <- reactive({!is.null(.GlobalEnv$.plotting.params$data)}); # now a dataset is defined!
    output$is_mg_defined <- reactive({!is.null(.GlobalEnv$.plotting.params$medication.groups)}); # and medication groups!
  }


  # Dataset from file: load it, list the columns and upate the selections ----
  observeEvent(input$dataset_from_file_filename,
  {
    # Disconnect any pre-existing database connections:
    if( !is.null(.GlobalEnv$.plotting.params$.db.connection) )
    {
      try(DBI::dbDisconnect(.GlobalEnv$.plotting.params$.db.connection), silent=TRUE);
      .GlobalEnv$.plotting.params$.db.connection <- NULL;
    }

    if( is.null(input$dataset_from_file_filename) || nrow(input$dataset_from_file_filename) < 1 )
    {
      # No file loaded, nothing to do:
      return (invisible(NULL));
    }

    # Try to parse and load it:
    d <- NULL;
    #.GlobalEnv$.plotting.params$.fromfile.dataset <- NULL;
    #output$is_file_loaded <- reactive({FALSE}); # Update UI

    if( input$dataset_from_file_filetype == "Comma/TAB-separated (.csv; .tsv; .txt)" )
    {
      # Load CSV/TSV:
      showModal(modalDialog("Loading and processing data...", title=div(icon("hourglass", lib="glyphicon"), "Please wait..."), easyClose=FALSE, footer=NULL));
      res <- tryCatch(d <- read.table(input$dataset_from_file_filename$datapath[1],
                                      header=input$dataset_from_file_csv_header,
                                      sep=switch(input$dataset_from_file_csv_separator,
                                                 "[TAB] (\\t)"="\t",
                                                 "comma (,)"=",",
                                                 "white spaces (1+)"="",
                                                 "semicolon (;)"=";",
                                                 "colon (:)"=":"),
                                      quote=switch(input$dataset_from_file_csv_quotes,
                                                   "[none] ()"="",
                                                   "singe quotes (' ')"="'",
                                                   "double quotes (\" \")"='"'),
                                      dec=switch(input$dataset_from_file_csv_decimal,
                                                 "dot (.)"=".",
                                                 "comma (,)"=","),
                                      strip.white=input$dataset_from_file_csv_strip_white,
                                      na.strings=gsub('["\']','',trimws(strsplit(input$dataset_from_file_csv_na_strings,",",fixed=TRUE)[[1]], "both"))),
                      error=function(e) e, warning=function(w) w);
      removeModal();
      if( is.null(d) || inherits(res, "error") )
      {
        # Some error occured!
        showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                              div("There's something wrong with the given CSV/TSV file: I tried reading it and this is what I got back:\n"), div(as.character(res), style="color: red;"),
                              footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        return (invisible(NULL));
      } else
      {
        if( inherits(res, "warning") )
        {
          showModal(modalDialog(title=div(icon("warning-sign", lib="glyphicon"), "AdhereR warning!"),
                                div("This CSV/TSV file seems ok, but when reading it I got some warnings:\n"), div(as.character(res), style="color: blue;"),
                                footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        }
      }
      d <- as.data.frame(d);
    } else if( input$dataset_from_file_filetype == "R objects from save() (.RData)" )
    {
      # Use load to recover them but then ask the user to use "load from memory" UI to continue...
      showModal(modalDialog("Loading and processing data...", title=div(icon("hourglass", lib="glyphicon"), "Please wait..."), easyClose=FALSE, footer=NULL));
      res <- tryCatch(read.objs <- load(input$dataset_from_file_filename$datapath[1]),
                      error=function(e) e, warning=function(w) w);
      removeModal();
      if( is.null(read.objs) || length(read.objs) == 0 || inherits(res, "error") )
      {
        # Some error occured!
        showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                              div("There's something wrong with the given R datasets file: I tried reading it and this is what I got back:\n"), div(as.character(res), style="color: red;"),
                              footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        return (invisible(NULL));
      } else
      {
        if( inherits(res, "warning") )
        {
          showModal(modalDialog(title=div(icon("warning-sign", lib="glyphicon"), "AdhereR warning!"),
                                div("This R datasets file seems ok, but when reading it I got some warnings:\n"), div(as.character(res), style="color: blue;"),
                                footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        }
      }

      # Show the user the objects that were read and redirect them to load them from memory:
      showModal(modalDialog(title="AdhereR R datasets file loaded!",
                            HTML(paste0("The R datasets file was successfully loaded and the following objects are now in memory: ",paste0("'",read.objs,"'",collapse=", "),".<br/>Please use the <b>load from memory</b> option to load the desired object from memory...")),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return (invisible(NULL)); # don't continue...
    } else if( input$dataset_from_file_filetype == "Serialized R object (.rds)" )
    {
      # Load them with readRDS:
      showModal(modalDialog("Loading and processing data...", title=div(icon("hourglass", lib="glyphicon"), "Please wait..."), easyClose=FALSE, footer=NULL));
      res <- tryCatch(d <- readRDS(input$dataset_from_file_filename$datapath[1]),
                      error=function(e) e, warning=function(w) w);
      removeModal();
      if( is.null(d) || inherits(res, "error") )
      {
        # Some error occured!
        showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                              div("There's something wrong with the given serialized single R object file: I tried reading it and this is what I got back:\n"), div(as.character(res), style="color: red;"),
                              footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        return (invisible(NULL));
      } else
      {
        if( inherits(res, "warning") )
        {
          showModal(modalDialog(title=div(icon("warning-sign", lib="glyphicon"), "AdhereR warning!"),
                                div("This serialized single R object file seems ok, but when reading it I got some warnings:\n"), div(as.character(res), style="color: blue;"),
                                footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        }
      }
    } else if( input$dataset_from_file_filetype == "Open Document Spreadsheet (.ods)" )
    {
      # Use readODS::read.ods to read the first sheet:
      showModal(modalDialog("Loading and processing data...", title=div(icon("hourglass", lib="glyphicon"), "Please wait..."), easyClose=FALSE, footer=NULL));
      res <- tryCatch(d <- readODS::read_ods(input$dataset_from_file_filename$datapath[1], sheet=input$dataset_from_file_sheet),
                      error=function(e) e, warning=function(w) w);
      removeModal();
      if( is.null(d) || inherits(res, "error") )
      {
        # Some error occured!
        showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                              div("There's something wrong with the given ODS file: I tried reading it and this is what I got back:\n"), div(as.character(res), style="color: red;"),
                              footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        return (invisible(NULL));
      } else
      {
        if( inherits(res, "warning") )
        {
          showModal(modalDialog(title=div(icon("warning-sign", lib="glyphicon"), "AdhereR warning!"),
                                div("This ODS file seems ok, but when reading it I got some warnings:\n"), div(as.character(res), style="color: blue;"),
                                footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        }
      }
      d <- as.data.frame(d);
    } else if( input$dataset_from_file_filetype == "Microsoft Excel (.xls; .xlsx)" )
    {
      # Use readxl::read_excel to read the first sheet:
      showModal(modalDialog("Loading and processing data...", title=div(icon("hourglass", lib="glyphicon"), "Please wait..."), easyClose=FALSE, footer=NULL));
      res <- tryCatch(d <- readxl::read_excel(input$dataset_from_file_filename$datapath[1], sheet=input$dataset_from_file_sheet),
                      #d <- openxlsx::read.xlsx(input$dataset_from_file_filename$datapath[1], sheet=input$dataset_from_file_sheet),
                      error=function(e) e, warning=function(w) w);
      removeModal();
      if( is.null(d) || inherits(res, "error") )
      {
        # Some error occured!
        showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                              div("There's something wrong with the given XLS/XLSX file: I tried reading it and this is what I got back:\n"), div(as.character(res), style="color: red;"),
                              footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        return (invisible(NULL));
      } else
      {
        if( inherits(res, "warning") )
        {
          showModal(modalDialog(title=div(icon("warning-sign", lib="glyphicon"), "AdhereR warning!"),
                                div("This XLS/XLSX file seems ok, but when reading it I got some warnings:\n"), div(as.character(res), style="color: blue;"),
                                footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        }
      }
      d <- as.data.frame(d);
    } else if( input$dataset_from_file_filetype == "SPSS (.sav; .por)" )
    {
      # Use haven::read_spss to read the first sheet:
      showModal(modalDialog("Loading and processing data...", title=div(icon("hourglass", lib="glyphicon"), "Please wait..."), easyClose=FALSE, footer=NULL));
      res <- tryCatch(d <- haven::read_spss(input$dataset_from_file_filename$datapath[1]),
                      error=function(e) e, warning=function(w) w);
      removeModal();
      if( is.null(d) || inherits(res, "error") )
      {
        # Some error occured!
        showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                              div("There's something wrong with the given SPSS file: I tried reading it and this is what I got back:\n"), div(as.character(res), style="color: red;"),
                              footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        return (invisible(NULL));
      } else
      {
        if( inherits(res, "warning") )
        {
          showModal(modalDialog(title=div(icon("warning-sign", lib="glyphicon"), "AdhereR warning!"),
                                div("This SPSS file seems ok, but when reading it I got some warnings:\n"), div(as.character(res), style="color: blue;"),
                                footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        }
      }
      d <- as.data.frame(d);
    } else if( input$dataset_from_file_filetype == "SAS Transport data file (.xpt)" )
    {
      # Use haven::read_xpt to read the first sheet:
      showModal(modalDialog("Loading and processing data...", title=div(icon("hourglass", lib="glyphicon"), "Please wait..."), easyClose=FALSE, footer=NULL));
      res <- tryCatch(d <- haven::read_xpt(input$dataset_from_file_filename$datapath[1]),
                      #d <- SASxport::read.xport(input$dataset_from_file_filename$datapath[1], as.list=TRUE),
                      error=function(e) e, warning=function(w) w);
      removeModal();
      if( is.null(d) || inherits(res, "error") )
      {
        # Some error occured!
        showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                              div("There's something wrong with the given SAS Transport file: I tried reading it and this is what I got back:\n"), div(as.character(res), style="color: red;"),
                              footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        return (invisible(NULL));
      } else
      {
        if( inherits(res, "warning") )
        {
          showModal(modalDialog(title=div(icon("warning-sign", lib="glyphicon"), "AdhereR warning!"),
                                div("This SAS Transport file seems ok, but when reading it I got some warnings:\n"), div(as.character(res), style="color: blue;"),
                                footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        }
      }
      d <- as.data.frame(d);

      #if( length(d) < input$dataset_from_file_sheet_sas )
      #{
      #  showModal(modalDialog(title="AdhereR warning!",
      #                        paste0("This SAS Transport file contains only ",length(d)," datasets, so I can't load the ",input$dataset_from_file_sheet_sas,"th: loading the first instead!"),
      #                        footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      #  d <- d[[1]];
      #} else
      #{
      #  d <- d[[input$dataset_from_file_sheet_sas]];
      #}
    } else if( input$dataset_from_file_filetype == "SAS sas7bdat data file (.sas7bdat)" )
    {
      # Use haven::read_sas to read the first sheet:
      showModal(modalDialog("Loading and processing data...", title=div(icon("hourglass", lib="glyphicon"), "Please wait..."), easyClose=FALSE, footer=NULL));
      res <- tryCatch(d <- haven::read_sas(input$dataset_from_file_filename$datapath[1]),
                      error=function(e) e, warning=function(w) w);
      removeModal();
      if( is.null(d) || inherits(res, "error") )
      {
        # Some error occured!
        showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                              div("There's something wrong with the given SAS sas7bdat file: I tried reading it and this is what I got back:\n"), div(as.character(res), style="color: red;"),
                              footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        return (invisible(NULL));
      } else
      {
        if( inherits(res, "warning") )
        {
          showModal(modalDialog(title=div(icon("warning-sign", lib="glyphicon"), "AdhereR warning!"),
                                div("This SAS sas7bdat file seems ok, but when reading it I got some warnings:\n"), div(as.character(res), style="color: blue;"),
                                footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        }
      }
      d <- as.data.frame(d);
    } else if( input$dataset_from_file_filetype == "Stata (.dta)" )
    {
      # Use haven::read_stata to read the first sheet:
      showModal(modalDialog("Loading and processing data...", title=div(icon("hourglass", lib="glyphicon"), "Please wait..."), easyClose=FALSE, footer=NULL));
      res <- tryCatch(d <- haven::read_stata(input$dataset_from_file_filename$datapath[1]),
                      error=function(e) e, warning=function(w) w);
      removeModal();
      if( is.null(d) || inherits(res, "error") )
      {
        # Some error occured!
        showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                              div("There's something wrong with the given Stata file: I tried reading it and this is what I got back:\n"), div(as.character(res), style="color: red;"),
                              footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        return (invisible(NULL));
      } else
      {
        if( inherits(res, "warning") )
        {
          showModal(modalDialog(title=div(icon("warning-sign", lib="glyphicon"), "AdhereR warning!"),
                                div("This Stata file seems ok, but when reading it I got some warnings:\n"), div(as.character(res), style="color: blue;"),
                                footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        }
      }
      d <- as.data.frame(d);
    }

    # Set it as the from-file dataset and update columns:
    if( !is.null(d) && inherits(d, "data.frame") )
    {
      if( nrow(d) < 3 )
      {
        showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                              div("The selected file must have at least three distinct columns (patient ID, event date and duration)!", style="color: red;"),
                              footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        return (invisible(NULL));
      }

      n.vals.to.show <-3;
      x <- names(d);
      x.info <- vapply(1:ncol(d),
                       function(i) paste0("(",
                                          paste0(class(d[,i]),collapse=","),
                                          ": ",
                                          paste0(d[1:min(n.vals.to.show,nrow(d)),i],collapse=", "),
                                          if(nrow(d)>n.vals.to.show) "...",
                                          ")"),
                       character(1));

      # Required columns:
      shinyWidgets::updatePickerInput(session, "dataset_from_file_patient_id",     choices=x, selected=x[1], choicesOpt=list(subtext=x.info));
      shinyWidgets::updatePickerInput(session, "dataset_from_file_event_date",     choices=x, selected=x[2], choicesOpt=list(subtext=x.info));
      shinyWidgets::updatePickerInput(session, "dataset_from_file_event_duration", choices=x, selected=x[3], choicesOpt=list(subtext=x.info));

      # Optional columns (possibly used by CMA5+):
      shinyWidgets::updatePickerInput(session, "dataset_from_file_medication_class", choices=c("[not defined]", x), selected="[not defined]", choicesOpt=list(subtext=c("",x.info)));
      shinyWidgets::updatePickerInput(session, "dataset_from_file_daily_dose",       choices=c("[not defined]", x), selected="[not defined]", choicesOpt=list(subtext=c("",x.info)));

      # set the dataset and various parameters used to read it:
      .GlobalEnv$.plotting.params$.fromfile.dataset <- d;
      .GlobalEnv$.plotting.params$.fromfile.dataset.filetype <- input$dataset_from_file_filetype;
      .GlobalEnv$.plotting.params$.fromfile.dataset.header <- input$dataset_from_file_csv_header;
      .GlobalEnv$.plotting.params$.fromfile.dataset.sep <- switch(input$dataset_from_file_csv_separator,
                                                                  "[TAB] (\\t)"="\t",
                                                                  "comma (,)"=",",
                                                                  "white spaces (1+)"="",
                                                                  "semicolon (;)"=";",
                                                                  "colon (:)"=":");
      .GlobalEnv$.plotting.params$.fromfile.dataset.quote <- switch(input$dataset_from_file_csv_quotes,
                                                                    "[none] ()"="",
                                                                    "singe quotes (' ')"="'",
                                                                    "double quotes (\" \")"='"');
      .GlobalEnv$.plotting.params$.fromfile.dataset.dec <- switch(input$dataset_from_file_csv_decimal,
                                                                  "dot (.)"=".",
                                                                  "comma (,)"=",");
      .GlobalEnv$.plotting.params$.fromfile.dataset.strip.white <- input$dataset_from_file_csv_strip_white;
      .GlobalEnv$.plotting.params$.fromfile.dataset.na.strings <- gsub('["\']','',trimws(strsplit(input$dataset_from_file_csv_na_strings,",",fixed=TRUE)[[1]], "both"));
      .GlobalEnv$.plotting.params$.fromfile.dataset.sheet <- input$dataset_from_file_sheet;

      output$is_file_loaded <- reactive({TRUE}); # Update UI to reflect this change
    }
  })

  # Dataset from file: peek ----
  observeEvent(input$dataset_from_file_peek_button,
  {
    # Sanity checks:
    if( is.null(.GlobalEnv$.plotting.params$.fromfile.dataset) ||
        !inherits(.GlobalEnv$.plotting.params$.fromfile.dataset, "data.frame") ||
        ncol(.GlobalEnv$.plotting.params$.fromfile.dataset) < 1 ||
        nrow(.GlobalEnv$.plotting.params$.fromfile.dataset) < 3 )
    {
      showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                            div("Could not load the selected file '",input$dataset_from_file_filename$name[1], "' in memory!\nPlease make sure you selected a valid file contaning at least 3 columns and 1 row...", style="color: red;"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return (invisible(NULL));
    }

    showModal(modalDialog(title="AdhereR: peeking at the selected file ...",
                          div(HTML(.show.data.frame.as.HTML(.GlobalEnv$.plotting.params$.fromfile.dataset)),
                                   style="max-height: 50vh; max-width: 90vw; overflow: auto; overflow-x:auto;"),
                          footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
  })

  # Dataset from file: validate and use ----
  observeEvent(input$dataset_from_file_button_use,
  {
    # Sanity checks:
    if( is.null(.GlobalEnv$.plotting.params$.fromfile.dataset) ||
        !inherits(.GlobalEnv$.plotting.params$.fromfile.dataset, "data.frame") ||
        ncol(.GlobalEnv$.plotting.params$.fromfile.dataset) < 1 ||
        nrow(.GlobalEnv$.plotting.params$.fromfile.dataset) < 3 )
    {
      showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                            div(paste0("Cannot load the selected file '",input$dataset_from_file_filename$name[1], "'!\nPlease make sure you selected a valid file with at least 3 columns and 1 row..."), style="color: red;"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return (invisible(NULL));
    }

    # Checks:
    .validate.and.load.dataset(.GlobalEnv$.plotting.params$.fromfile.dataset,
                               get.colnames.fnc=function(d) names(d),
                               get.patients.fnc=function(d, idcol) unique(d[[idcol]]),
                               get.data.for.patients.fnc=function(patientid, d, idcol, cols=NA, maxrows=NA) d[ d[[idcol]] %in% patientid, ],
                               ID.colname=input$dataset_from_file_patient_id,
                               event.date.colname=input$dataset_from_file_event_date,
                               event.duration.colname=input$dataset_from_file_event_duration,
                               event.daily.dose.colname=input$dataset_from_file_daily_dose,
                               medication.class.colname=input$dataset_from_file_medication_class,
                               date.format=input$dataset_from_file_event_format);

     # Let the world know this:
    .GlobalEnv$.plotting.params$.dataset.type <- "from file";
    .GlobalEnv$.plotting.params$.dataset.comes.from.function.arguments <- FALSE;
    .GlobalEnv$.plotting.params$.dataset.name <- input$dataset_from_file_filename$name[1];
  })

  # SQL database: connect and fetch tables ----
  observeEvent(input$dataset_from_sql_button_connect,
  {
    # Disconnect any pre-existing database connections:
    if( !is.null(.GlobalEnv$.plotting.params$.db.connection) )
    {
      try(DBI::dbDisconnect(.GlobalEnv$.plotting.params$.db.connection), silent=TRUE);
    }
    updateSelectInput(session, inputId="dataset_from_sql_table", choices="[none]", selected="[none]");
    .GlobalEnv$.plotting.params$.db.connection <- NULL;
    output$is_database_connected <- reactive({FALSE}); # update UI

    if( input$dataset_from_sql_server_type == "SQLite" )
    {
      d <- NULL; res <- NULL;
      if( input$dataset_from_sqlite_database_name == "med_events" )
      {
        # Create this one on-the-fly in memory:
        res <- tryCatch(d <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", bigint="numeric"),
                        error=function(e) e, warning=function(w) w);
        if( is.null(d) || inherits(res, "error") )
        {
          # Some error occured!
          showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                                div("Can't create the example in-memory SQLite database: this is what I got back:\n"), div(as.character(res), style="color: red;"),
                                footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
          return (invisible(NULL));
        } else
        {
          if( inherits(res, "warning") )
          {
            showModal(modalDialog(title=div(icon("warning-sign", lib="glyphicon"), "AdhereR warning!"),
                                  div("Creating the example in-memory SQLite database seems ok, but when connecting I got some warnings:\n"), div(as.character(res, style="color: blue;")),
                                  footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
          }
        }

        # Put the data in:
        tmp <- AdhereR::med.events; tmp$DATE <- as.character(as.Date(tmp$DATE,format="%m/%d/%Y"),format="%Y-%m-%d"); # make sure the dates are in the YYYY-MM-DD SQL format
        res <- tryCatch(DBI::dbWriteTable(d, "med_events", tmp, overwrite=TRUE),
                        error=function(e) e, warning=function(w) w);
        if( is.null(d) || inherits(res, "error") )
        {
          # Some error occured!
          showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                                div("Can't put med.events in the example in-memory SQLite database: this is what I got back:\n"), div(as.character(res), style="color: red;"),
                                footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
          return (invisible(NULL));
        } else
        {
          if( inherits(res, "warning") )
          {
            showModal(modalDialog(title=div(icon("warning-sign", lib="glyphicon"), "AdhereR warning!"),
                                  div("Putting med.events in the example in-memory SQLite database seems ok, but when connecting I got some warnings:\n"), div(as.character(res, style="color: blue;")),
                                  footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
          }
        }
      } else
      {
        # Simply connect to the table:
        res <- tryCatch(d <- DBI::dbConnect(RSQLite::SQLite(), input$dataset_from_sqlite_database_name),
                        error=function(e) e, warning=function(w) w);
        if( is.null(d) || inherits(res, "error") )
        {
          # Some error occured!
          showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                                div("Can't access the SQLite database: this is what I got back:\n"), div(as.character(res), style="color: red;"),
                                footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
          return (invisible(NULL));
        } else
        {
          if( inherits(res, "warning") )
          {
            showModal(modalDialog(title=div(icon("warning-sign", lib="glyphicon"), "AdhereR warning!"),
                                  div("Accessing the SQLite database seems ok, but when connecting I got some warnings:\n"), div(as.character(res, style="color: blue;")),
                                  footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
          }
        }
      }
      # Fetch the tables:
      db_tables <- NULL;
      res <- tryCatch(db_tables <- DBI::dbListTables(d),
                      error=function(e) e, warning=function(w) w);
      if( is.null(db_tables) || inherits(res, "error") )
      {
        # Some error occured!
        showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                              div("Can't read tables from the SQL server: this is what I got back:\n"), div(as.character(res), style="color: red;"),
                              footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        removeModal();
        return (invisible(NULL));
      } else
      {
        if( inherits(res, "warning") )
        {
          showModal(modalDialog(title=div(icon("warning-sign", lib="glyphicon"), "AdhereR warning!"),
                                div("Could read tables from SQL server, but I got some warnings:\n"), div(as.character(res), style="color: blue;"),
                                footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        }
      }

      # Build a list of columns for each table:
      d.tables.columns <- do.call(rbind, lapply(db_tables, function(s)
      {
        x <- NULL;
        try(x <- DBI::dbGetQuery(d, paste0("PRAGMA table_info(",s,");")), silent=TRUE);
        if( !is.null(x) && inherits(x, "data.frame") )
        {
          n <- NULL;
          try(n <- DBI::dbGetQuery(d, paste0("SELECT COUNT(*) FROM ",s,";")), silent=TRUE);
          if( is.null(n) || !inherits(n, "data.frame") || nrow(n) != 1 || ncol(n) != 1 )
          {
            # Error retreiving the number of rows:
            n <- NA;
          } else
          {
            n <- as.numeric(n[1,1]);
          }
          # Get first X values for each column:
          fr <- NULL;
          try(fr <- DBI::dbGetQuery(d, paste0("SELECT * FROM ",s," LIMIT 3;")), silent=TRUE);
          if( is.null(fr) || !inherits(fr, "data.frame") || nrow(fr) < 1 || ncol(fr) != nrow(x) )
          {
            fr <- rep(nrow(x),"");
          } else
          {
            fr <- vapply(1:ncol(fr), function(i) paste0(as.character(fr[,i]),collapse=", "), character(1));
          }
          return (data.frame("table"=s,
                             "nrow"=n,
                             "column"=x$name,
                             "type"=x$type,
                             "null"=(x$notnull == 0),
                             "key"=(x$pk != 0),
                             "firstvals"=fr));
        } else
        {
          return (NULL);
        }
      }));

    } else if( input$dataset_from_sql_server_type == "MySQL/MariaDB" )
    {
      d <- NULL;
      showModal(modalDialog("Connecting to SQL database...", title=div(icon("hourglass", lib="glyphicon"), "Please wait..."), easyClose=FALSE, footer=NULL))
      res <- tryCatch(d <- DBI::dbConnect(RMariaDB::MariaDB(), # works also for MySQL
                                          user=input$dataset_from_sql_username, # the username
                                          password=input$dataset_from_sql_password, # and password
                                          dbname=if( input$dataset_from_sql_database_name == "[none]" ) NULL else input$dataset_from_sql_database_name, # which database
                                          host=if( input$dataset_from_sql_server_host == "[none]" ) NULL else input$dataset_from_sql_server_host, # on which host
                                          port=input$dataset_from_sql_server_port, # the TCP/IP port
                                          bigint="numeric" # force bigint to numeric to avoid weird problems down the line
                                         ),
                      error=function(e) e, warning=function(w) w);
      removeModal();
      if( is.null(d) || inherits(res, "error") )
      {
        # Some error occured!
        showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                              div("Can't connect to the SQL server: this is what I got back:\n"), div(as.character(res), style="color: red;"),
                              footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        return (invisible(NULL));
      } else
      {
        if( inherits(res, "warning") )
        {
          showModal(modalDialog(title=div(icon("warning-sign", lib="glyphicon"), "AdhereR warning!"),
                                div("The SQL server seems ok, but when connecting I got some warnings:\n"), div(as.character(res, style="color: blue;")),
                                footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        }
      }

      # Fetch the tables:
      db_tables <- NULL;
      showModal(modalDialog("Reading tables from the SQL database...", title=div(icon("hourglass", lib="glyphicon"), "Please wait..."), easyClose=FALSE, footer=NULL))
      res <- tryCatch(db_tables <- DBI::dbListTables(d),
                      error=function(e) e, warning=function(w) w);
      if( is.null(db_tables) || inherits(res, "error") )
      {
        # Some error occured!
        showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                              div("Can't read tables from the SQL server: this is what I got back:\n"), div(as.character(res), style="color: red;"),
                              footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        removeModal();
        return (invisible(NULL));
      } else
      {
        if( inherits(res, "warning") )
        {
          showModal(modalDialog(title=div(icon("warning-sign", lib="glyphicon"), "AdhereR warning!"),
                                div("Could read tables from SQL server, but I got some warnings:\n"), div(as.character(res), style="color: blue;"),
                                footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        }
      }

      # Build a list of columns for each table:
      d.tables.columns <- do.call(rbind, lapply(db_tables, function(s)
      {
        x <- NULL;
        # Get columns:
        try(x <- DBI::dbGetQuery(d, paste0("SHOW COLUMNS FROM ",s,";")), silent=TRUE);
        if( !is.null(x) && inherits(x, "data.frame") )
        {
          n <- NULL;
          # Get number of rows:
          try(n <- DBI::dbGetQuery(d, paste0("SELECT COUNT(*) FROM ",s,";")), silent=TRUE);
          if( is.null(n) || !inherits(n, "data.frame") || nrow(n) != 1 || ncol(n) != 1 )
          {
            # Error retreiving the number of rows:
            n <- NA;
          } else
          {
            n <- as.numeric(n[1,1]);
          }
          # Get first X values for each column:
          fr <- NULL;
          try(fr <- DBI::dbGetQuery(d, paste0("SELECT * FROM ",s," LIMIT 3;")), silent=TRUE);
          if( is.null(fr) || !inherits(fr, "data.frame") || nrow(fr) < 1 || ncol(fr) != nrow(x) )
          {
            fr <- rep(nrow(x),"");
          } else
          {
            fr <- vapply(1:ncol(fr), function(i) paste0(as.character(fr[,i]),collapse=", "), character(1));
          }

          return (data.frame("table"=s,
                             "nrow"=n,
                             "column"=x$Field,
                             "type"=x$Type,
                             "null"=x$Null,
                             "key"=x$Key,
                             "firstvals"=fr));
        } else
        {
          return (NULL);
        }
      }));

      removeModal();
    }

    if( is.null(d.tables.columns) )
    {
      # Some error occured!
      showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                            div("Could not fetch any info from the database!", style="color: red;"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return (invisible(NULL));
    }

    # Set it as the current SQL databse:
    .GlobalEnv$.plotting.params$.db.connection.tables <- d.tables.columns;
    .GlobalEnv$.plotting.params$.db.connection <- d;

    # Update the list of tables/views:
    x <- aggregate(column ~ nrow + table, d.tables.columns, length);
    x.eligible <- which(x$column >= 3); # which are the eligible tables/views
    x.to.pick <- 1;
    if( length(x.eligible) == 0 )
    {
      # Warning:
      showModal(modalDialog(title=div(icon("warning-sign", lib="glyphicon"), "AdhereR warning!"),
                            div("There doesn't seem to be any tables/views with at least 3 columns in this database: picking the first (but this will generate an error)!\n"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      x.to.pick <- 1;
    } else
    {
      x.to.pick <- x.eligible[1];
    }
    shinyWidgets::updatePickerInput(session, "dataset_from_sql_table",
                                    choices=as.character(x$table),
                                    selected=as.character(x$table)[x.to.pick],
                                    choicesOpt=list(subtext=paste0(x$nrow," x ",x$column)));

    # Update UI:
    output$is_database_connected <- reactive({TRUE});

    # Show the info:
    .show.db.info();
  })

  # SQL database: disconnect ----
  observeEvent(input$dataset_from_sql_button_disconnect,
  {
    if( !is.null(.GlobalEnv$.plotting.params$.db.connection) )
    {
      try(DBI::dbDisconnect(.GlobalEnv$.plotting.params$.db.connection), silent=TRUE);
      .GlobalEnv$.plotting.params$.db.connection <- NULL;
    }
    # Update UI:
    updateSelectInput(session, inputId="dataset_from_sql_table", choices="[none]", selected="[none]");
    output$is_database_connected <- reactive({FALSE});
  })

  # SQL database: peek ----
  observeEvent(input$dataset_from_sql_button_peek,
  {
     .show.db.info();
  })

  # SQL database: show info ----
  .show.db.info <- function()
  {
    if( is.null(.GlobalEnv$.plotting.params$.db.connection.tables) ||
        !inherits(.GlobalEnv$.plotting.params$.db.connection.tables, "data.frame") ||
        nrow(.GlobalEnv$.plotting.params$.db.connection.tables) < 1 ||
        is.null(.GlobalEnv$.plotting.params$.db.connection) ||
        !DBI::dbIsValid(.GlobalEnv$.plotting.params$.db.connection) )
    {
      # Some error occured!
      showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                            div("Error accessing the database!", style="color: red;"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return (invisible(NULL));
    }

    # Display the info:
    showModal(modalDialog(title="AdhereR SQL database connection...",
                          div(style="max-height: 50vh; max-width: 90vw; overflow: auto; overflow-x:auto;",
                              HTML(paste0("Successfully connected to SQL server <i>",
                                          if( input$dataset_from_sql_server_host == "[none]" ) "localhost" else input$dataset_from_sql_server_host, "</i>",
                                          if( input$dataset_from_sql_server_port > 0 ) paste0(":",input$dataset_from_sql_server_port)," and fetched data from ",
                                          length(unique(.GlobalEnv$.plotting.params$.db.connection.tables$table))," tables.<br/><hl/><br/>",
                                          "Please note that, currently, this interactive Shiny User Interface <b style='color: red'>requires that all the data</b> (namely, patient ID, event date and duration,(and possibly dosage and type)) are in <b style='color: red'>ONE TABLE or VIEW</b>! ",
                                          "If this is not the case, please create a <i>temporary table</i> or a <i>view</i> and reconnect to the database. ",
                                          "(Please see the vignette for more details.) ",
                                          #                                      "For example, using the <code>MySQL</code> database described in the vignette <i>Using AdhereR with various database technologies for processing very large datasets</i>, named <code>med_events</code> and consisting of 4 tables containing information about the patients (<code>patients</code>), the events (<code>event_info</code> and <code>event_date</code>), and the connections between the two (<code>event_patients</code>), we can create a <i>view</i> named <code>all_info_view</code> with the <code>SQL</code> commands:<br/>",
                                          # "<pre>
                                          # CREATE VIEW `all_info_view` AS
                                          # SELECT event_date.id,
                                          #        event_date.date,
                                          #        event_info.category,
                                          #        event_info.duration,
                                          #        event_info.perday,
                                          #        event_patients.patient_id,
                                          #        patients.sex
                                          # FROM event_date
                                          #   JOIN event_info
                                          #   ON event_info.id = event_date.id
                                          #     JOIN event_patients
                                          #     ON event_patients.id = event_info.id
                                          #       JOIN patients
                                          #       ON patients.id = event_patients.patient_id;</pre>",
                                          "<br/><hl/><br/>",
                                          "We list below, for each <b style='color: DarkBlue'>table</b>, the <b style='color: blue'>columns</b> [with their <span style='color: green'>types</span> and other <i>relevant info</i>], possibly followed by the first few values:<br/>",
                                          "<ul>",
                                          paste0(vapply(unique(.GlobalEnv$.plotting.params$.db.connection.tables$table), function(table_name)
                                          {
                                            s <- which(.GlobalEnv$.plotting.params$.db.connection.tables$table == table_name);
                                            if( length(s) > 0 )
                                            {
                                              paste0("<li><b style='color: DarkBlue'>", table_name, "</b>",
                                                     ": ", if( is.na(.GlobalEnv$.plotting.params$.db.connection.tables$nrow[s[1]]) ) "<span style='color: red'>ERROR RETREIVING NUMBER OF ROWS</span>" else paste0("with ", .GlobalEnv$.plotting.params$.db.connection.tables$nrow[s[1]]," row(s)"),
                                                     "<ul>",
                                                     paste0(vapply(s, function(i) paste0("<li><b style='color: blue'>", .GlobalEnv$.plotting.params$.db.connection.tables$column[i], "</b>",
                                                                                         " [<span style='color: green'>", .GlobalEnv$.plotting.params$.db.connection.tables$type[i], "</span>",
                                                                                         if( .GlobalEnv$.plotting.params$.db.connection.tables$key[i] == "PRI" ) ", <i>primary key</i>",
                                                                                         "]",
                                                                                         " ",.GlobalEnv$.plotting.params$.db.connection.tables$firstvals[i],
                                                                                         "</li>"),
                                                                   character(1)), collapse="\n"),
                                                     "</ul>",
                                                     "</li>")
                                            }
                                          }, character(1)), collapse="\n"),
                                          "</ul>"
                              ))),
                          footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
  }


  # SQL database: update columns depending on the selected table ----
  observeEvent(input$dataset_from_sql_table,
  {
    if( input$dataset_from_sql_table != "[none]" &&
        !is.null(.GlobalEnv$.plotting.params$.db.connection) &&
        !is.null(.GlobalEnv$.plotting.params$.db.connection.tables) &&
        sum((s <- (.GlobalEnv$.plotting.params$.db.connection.tables$table == input$dataset_from_sql_table)), na.rm=TRUE) > 0 )
    {
      if( sum(s) < 3 )
      {
        showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                              div("The table/view must have at least three columns!", style="color: red;"),
                              footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        return (invisible(NULL));
      }

      # Save this as the selected table:
      .GlobalEnv$.plotting.params$.db.connection.selected.table <- input$dataset_from_sql_table;

      # Update them:
      column.names <- as.character(.GlobalEnv$.plotting.params$.db.connection.tables$column[s]);
      column.info <- paste0("(",.GlobalEnv$.plotting.params$.db.connection.tables$type[s],": ",.GlobalEnv$.plotting.params$.db.connection.tables$firstvals[s],")");
      shinyWidgets::updatePickerInput(session, "dataset_from_sql_patient_id", choices=column.names, selected=column.names[1], choicesOpt=list(subtext=column.info));
      shinyWidgets::updatePickerInput(session, "dataset_from_sql_event_date", choices=column.names, selected=column.names[2], choicesOpt=list(subtext=column.info));
      shinyWidgets::updatePickerInput(session, "dataset_from_sql_event_duration", choices=column.names, selected=column.names[3], choicesOpt=list(subtext=column.info));

      shinyWidgets::updatePickerInput(session, "dataset_from_sql_daily_dose", choices=c("[not defined]", column.names), selected="[not defined]", choicesOpt=list(subtext=c("",column.info)));
      shinyWidgets::updatePickerInput(session, "dataset_from_sql_medication_class", choices=c("[not defined]", column.names), selected="[not defined]", choicesOpt=list(subtext=c("",column.info)));
    }
  })

  # SQL database: validate and use ----
  observeEvent(input$dataset_from_sql_button_use,
  {
    # Sanity checks:
    if( is.null(.GlobalEnv$.plotting.params$.db.connection) ||
        !DBI::dbIsValid(.GlobalEnv$.plotting.params$.db.connection) )
    {
        showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                              div(paste0("Cannot use the selected database and table/view!"), style="color: red;"),
                              footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        return (invisible(NULL));
    }

    # Check and load:
    .validate.and.load.dataset(.GlobalEnv$.plotting.params$.db.connection,
                               get.colnames.fnc=
                                 if(input$dataset_from_sql_server_type == "SQLite")
                                 {
                                   function(d)
                                   {
                                     if( is.null(d) || !DBI::dbIsValid(d) ){ warning("Connection to database lost!"); return (NULL); }
                                     x <- NULL;
                                     try(x <- DBI::dbGetQuery(d, paste0("PRAGMA table_info(",.GlobalEnv$.plotting.params$.db.connection.selected.table,");")), silent=TRUE);
                                     if( !is.null(x) && inherits(x, "data.frame") && nrow(x) > 0 ) return (as.character(x$name)) else { warning("Cannot fetch DB column names!"); return (NULL); }
                                   }
                                 } else if(input$dataset_from_sql_server_type == "MySQL/MariaDB")
                                 {
                                   function(d)
                                   {
                                     if( is.null(d) || !DBI::dbIsValid(d) ){ warning("Connection to database lost!"); return (NULL); }
                                     x <- NULL;
                                     try(x <- DBI::dbGetQuery(d, paste0("SHOW COLUMNS FROM ",.GlobalEnv$.plotting.params$.db.connection.selected.table,";")), silent=TRUE);
                                     if( !is.null(x) && inherits(x, "data.frame") && nrow(x) > 0 ) return (as.character(x$Field)) else { warning("Cannot fetch DB column names!"); return (NULL); }
                                   }
                                 },
                               get.patients.fnc=
                                 function(d, idcol)
                                 {
                                   if( is.null(d) || !DBI::dbIsValid(d) ){ warning("Connection to database lost!"); return (NULL); }
                                   x <- NULL;
                                   try(x <- DBI::dbGetQuery(d, paste0("SELECT DISTINCT ",idcol," FROM ",.GlobalEnv$.plotting.params$.db.connection.selected.table,";")), silent=TRUE);
                                   if( !is.null(x) && inherits(x, "data.frame") && nrow(x) > 0 ) return (x[,1]) else { warning("Cannot fetch patients from DB!"); return (NULL); }
                                 },
                               get.data.for.patients.fnc=
                                 function(patientid, d, idcol, cols=NA, maxrows=NA)
                                 {
                                   if( is.null(d) || !DBI::dbIsValid(d) ){ warning("Connection to database lost!"); return (NULL); }
                                   x <- NULL;
                                   try(x <- DBI::dbGetQuery(d, paste0("SELECT ",
                                                                      if(is.na(cols)) "*" else paste0(cols,collapse=","),
                                                                      " FROM ",.GlobalEnv$.plotting.params$.db.connection.selected.table,
                                                                      " WHERE ",idcol,
                                                                      " IN (",paste0(patientid,collapse=","),")",
                                                                      if(!is.na(maxrows)) paste0("LIMIT ",maxrows),
                                                                      ";")),
                                       silent=TRUE);
                                   if( !is.null(x) && inherits(x, "data.frame") && nrow(x) > 0 ) return (x) else { warning("Cannot fetch data for patient(s) from DB!"); return (NULL); }
                                 },
                               ID.colname=input$dataset_from_sql_patient_id,
                               event.date.colname=input$dataset_from_sql_event_date,
                               event.duration.colname=input$dataset_from_sql_event_duration,
                               event.daily.dose.colname=input$dataset_from_sql_daily_dose,
                               medication.class.colname=input$dataset_from_sql_medication_class,
                               date.format=input$dataset_from_sql_event_format);

     # Let the world know this:
    .GlobalEnv$.plotting.params$.dataset.type <- "SQL database";
    .GlobalEnv$.plotting.params$.dataset.comes.from.function.arguments <- FALSE;
    .GlobalEnv$.plotting.params$.dataset.name <- paste0({if( input$dataset_from_sql_username != "" ) paste0(input$dataset_from_sql_username," @ ")},
                                                        {if( input$dataset_from_sql_server_host == "[none]" ) "localhost" else input$dataset_from_sql_server_host},
                                                        {if( input$dataset_from_sql_server_port > 0 ) paste0(":",input$dataset_from_sql_server_port)},
                                                        " :: ",.GlobalEnv$.plotting.params$.db.connection.selected.table);
  })


  # Show info about the curent dataset ----
  observeEvent(input$about_dataset_button,
  {
    showModal(modalDialog(title=div(icon("hdd", lib="glyphicon"), "AdhereR: info over the current dataset..."),
                          div(style="max-height: 50vh; max-width: 90vw; overflow: auto; overflow-x:auto;",
                              HTML(if( .GlobalEnv$.plotting.params$.dataset.type %in% c("in memory", "from file", "SQL database") )
                                   {
                                    paste0("The dataset currently used ",
                                     {if(.GlobalEnv$.plotting.params$.dataset.comes.from.function.arguments)
                                      {
                                        paste0("was given as the <code>data</code> argument to the <code>plot_interactive_cma()</code> function called by the user.<br/>",
                                               "Therefore, we cannot know its name outside the function call (and there might not be such a \"name\" as the data might have been created on-the-fly in the function call), and instead we identify it as the <b style='color:darkblue'><<'data' argument to plot_interactive_cma() call>></b> of class <i>", paste0(class(.GlobalEnv$.plotting.params$data),collapse=","), "</i>."
                                               )
                                      } else
                                      {
                                        paste0("was manualy defined as ",
                                               switch(.GlobalEnv$.plotting.params$.dataset.type,
                                                      "in memory"=paste0("an object of class <i>data.frame</i> (or derived from it, such a <i>data.table</i>) that was already <b>in-memory</b> under the name <b style='color:darkblue'>",.GlobalEnv$.plotting.params$.dataset.name,"</b>"),
                                                      "from file"=paste0("a <i>data.frame</i> object loaded <b>from the file</b> <b style='color:darkblue'>",.GlobalEnv$.plotting.params$.dataset.name,"</b>"),
                                                      "SQL database"=paste0("a connection to the <b>SQL database</b> <b style='color:darkblue'>",.GlobalEnv$.plotting.params$.dataset.name,"</b>")
                                                     ),
                                               ".")
                                      }
                                     },
                                     "<br/><hr/>",
                                     "These data has <b>", length(.GlobalEnv$.plotting.params$get.colnames.fnc(.GlobalEnv$.plotting.params$data)), " columns</b>, ",
                                     "and contains info for <b>", length(unique(.GlobalEnv$.plotting.params$get.patients.fnc(.GlobalEnv$.plotting.params$data, .GlobalEnv$.plotting.params$ID.colname))), " patients</b>.<br/>",
                                     "The relevant information is distributed as:</br>",
                                     "<ul>",
                                     "<li>the <i>patient IDs</i> are in column <b>", .GlobalEnv$.plotting.params$ID.colname, "</b></li>",
                                     "<li>the event <i>dates</i> are in column <b>", .GlobalEnv$.plotting.params$event.date.colname, "</b></li>",
                                     "<li>the event <i>durations</i> are in column <b>", .GlobalEnv$.plotting.params$event.duration.colname, "</b></li>",
                                     {if(!is.na(.GlobalEnv$.plotting.params$event.daily.dose.colname)) paste0("<li>the <i>doses</i> are in column <b>", .GlobalEnv$.plotting.params$event.daily.dose.colname, "</b></li>")},
                                     {if(!is.na(.GlobalEnv$.plotting.params$medication.class.colname)) paste0("<li>the <i>treatment types</i> are in column <b>", .GlobalEnv$.plotting.params$medication.class.colname, "</b></li>")},
                                     "</ul>"
                                     )
                                    } else
                                    {
                                      "There was no argument (or a NULL) passed as the <code>data</code> argument to the <code>plot_interactive_cma()</code>, which means that there's no dataset defined: please define one using the <b>Data</b> tab!"
                                    }
                                )),
                          footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
  })


  # Update the patient IDs table ----
  .update.patients.IDs.table <- function(reset.slider=TRUE)
  {
    if( is.null(.GlobalEnv$.plotting.params$all.IDs) || length(.GlobalEnv$.plotting.params$all.IDs) < 1 )
    {
      .GlobalEnv$.plotting.params$all.IDs <- c("[not defined]");
    }
    tmp <- data.frame("#"=  1:length(.GlobalEnv$.plotting.params$all.IDs),
                      "ID"= if( input$compute_cma_patient_by_group_sorting == "by ID (↑)" )
                      {
                        sort(.GlobalEnv$.plotting.params$all.IDs, decreasing=FALSE)
                      } else if( input$compute_cma_patient_by_group_sorting == "by ID (↓)" )
                      {
                        sort(.GlobalEnv$.plotting.params$all.IDs, decreasing=TRUE)
                      } else
                      {
                        .GlobalEnv$.plotting.params$all.IDs
                      },
                      check.names=FALSE);

    .GlobalEnv$.plotting.params$.patients.to.compute <- tmp;
    output$show_patients_as_list <- renderDataTable(.GlobalEnv$.plotting.params$.patients.to.compute, options=list(pageLength=10));
    if( reset.slider ) updateSliderInput(session, inputId="compute_cma_patient_by_group_range", max=nrow(tmp), value=c(1,1));
  }
  observeEvent(input$compute_cma_patient_selection_method,
  {
    if( input$compute_cma_patient_selection_method == "by_position" )
    {
      .update.patients.IDs.table();
    }
  })
  observeEvent(input$compute_cma_patient_by_group_sorting,
  {
    .update.patients.IDs.table();
  })


  # Basic checks for a putative vector containing medication group definitions:
  .check.basic.mg.definition <- function(v)
  {
    return (!( is.null(v) ||                            # must be non-NULL
                 (!is.character(v) && !is.factor(v)) || # must be a vector of characters or factors
                 length(v) == 0 ||                      # of at least length 1
                 length(v <- v[!is.na(v)]) == 0 ||      # and with at least 1 non-NA element
                 is.null(names(v)) ||                   # must have names
                 "" %in% names(v) ||                    # that are non-empty
                 any(duplicated(names(v))) ));          # and unique
  }


  # In-memory medication groups: get the list of appropriate vectors objects all the way to the base environment ----
  .list.mg.from.memory <- function()
  {
    # List all the character or factor vectors currently in memory:
    x <- sort(c(.recursively.list.objects.in.memory(of.class="character", min.nrow=NA, min.ncol=NA, return.dimensions=FALSE),
                .recursively.list.objects.in.memory(of.class="factor", min.nrow=NA, min.ncol=NA, return.dimensions=FALSE)));

    if( is.null(x) || length(x) == 0 )
    {
      showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                            div("There are no fitting medication group definitions in memory!", style="color: red;"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
    } else
    {
      # Check that they meet the basic requirements for medication group definitions:
      s <- vapply(x, function(v) .check.basic.mg.definition(get(v)), logical(1));
      if( !any(s) )
      {
        showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                              div("There are no fitting medication group definitions in memory!", style="color: red;"),
                              footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      } else
      {
        # Keep only these:
        x <- x[s];

        # If defined, add the medication groups sent to the Shiny plotting function:
        if( !is.null(.GlobalEnv$.plotting.params) &&
            !is.null(.GlobalEnv$.plotting.params$medication.groups) &&
            (is.character(.GlobalEnv$.plotting.params$medication.groups) || is.factor(.GlobalEnv$.plotting.params$medication.groups)))
        {
          if( .GlobalEnv$.plotting.params$.mg.comes.from.function.arguments )
          {
            # Add the function argument as well:
            x <- c("<<'medication.groups' argument to plot_interactive_cma() call>>", x);
          }
        }
        updateSelectInput(session, "mg_from_memory", choices=x, selected=x[1]);
        .update.mg.inmemory();
      }
    }
  }

  # In-memory vector: update the selections ----
  .update.mg.inmemory <- function()
  {
    # Set the vector:
    .GlobalEnv$.plotting.params$.inmemory.mg <- NULL;
    if( input$mg_from_memory == "[none]" || input$mg_from_memory == "" )
    {
      # Initialisation:
      .GlobalEnv$.plotting.params$.inmemory.mg <- NULL;
    } else if( input$mg_from_memory == "<<'medication.groups' argument to plot_interactive_cma() call>>" )
    {
      # The special value pointing to the argument to plot_interactive_cma():
      .GlobalEnv$.plotting.params$.inmemory.mg <- .GlobalEnv$.plotting.params$medication.groups;
    } else
    {
      try(.GlobalEnv$.plotting.params$.inmemory.mg <- get(input$mg_from_memory), silent=TRUE);
      if( inherits(.GlobalEnv$.plotting.params$.inmemory.mg, "try-error") ) .GlobalEnv$.plotting.params$.inmemory.mg <- NULL;
    }

    # Sanity checks:
    if( (input$mg_from_memory != "[none]") &&
        (is.null(.GlobalEnv$.plotting.params$.inmemory.mg) ||
         (!is.character(.GlobalEnv$.plotting.params$.inmemory.mg) && !is.factor(.GlobalEnv$.plotting.params$.inmemory.mg)) ||
         length(.GlobalEnv$.plotting.params$.inmemory.mg) < 1 ))
    {
      showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                            div(paste0("Cannot load the selected medication group definitions '",input$mg_from_memory, "' from memory!"), style="color: red;"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return (invisible(NULL));
    }

    # Update the relevant UI elements:
    updateRadioButtons(session, "mg_list_of_groups",
                       choices=if( is.null(.GlobalEnv$.plotting.params$.inmemory.mg) ) {"<EMPTY>"} else {names(.GlobalEnv$.plotting.params$.inmemory.mg)},
                       selected=NULL);
  }

  observeEvent(input$mg_from_memory,
               {
                 .update.mg.inmemory();
               })

  # Display a vector of medication groups nicely as HTML ----
  .show.medication.groups.as.HTML <- function(mg, # the vector to show
                                              max.entries=200, # if NA, show all
                                              escape=TRUE)
  {
    if( !.check.basic.mg.definition(mg) )
    {
      return ("<b>The given medication group definitions are empty of the wrong type!</b>");
    }

    # Highlight things in the definitions using HTML tags:
    # The calls:
    for( s in names(mg) )
    {
      mg <- gsub(paste0("{",s,"}"), paste0("{<b><i>",s,"</i></b>}"), mg, fixed=TRUE);
    }
    # The names:
    names(mg) <- paste0("<b><i>",names(mg),"</i></b>");

    # This is a pretty basic thing that tweaks the output of knitr::kable...
    d <- data.frame("Name"=names(mg), "Definition"=mg);
    d.as.html <- knitr::kable(d[1:min(max.entries,nrow(d),na.rm=TRUE),], format="html",
                              align="l",
                              col.names=names(d), #col.names=paste0("\U2007\U2007",names(d),"\U2007\U2007"),
                              row.names=FALSE);

    # Put back the HTML tags:
    d.as.html <- gsub("&lt;/i&gt;", "</i>",
                      gsub("&lt;i&gt;", "<i>",
                           gsub("&lt;/b&gt;", "</b>",
                                gsub("&lt;b&gt;", "<b>",
                                     d.as.html,
                                     fixed=TRUE),
                                fixed=TRUE),
                           fixed=TRUE),
                      fixed=TRUE);

    # The data.frame info in a nice HTML format:
    d.info <- paste0("There are ", nrow(d), " medication groups defined.");

    if( length(s <- strsplit(d.as.html, "<table", fixed=TRUE)[[1]]) > 1 )
    {
      # Found the <table> tag: add its class and caption:
      d.as.html <- paste0(s[1],
                          paste0("<table class='peekAtMGTable'",
                                 "<caption style='caption-side: top;'>", d.info,"</caption", # > is already in s[2] because of <table
                                 s[-1]));

      # Add the CSS:
      d.as.html <- paste0(d.as.html, "\n\n
                          <style>
                            table.peekAtMGTable {
                              border: 1px solid #1C6EA4;
                              background-color: #fbfcfc;
                              #width: 100%;
                              text-align: left;
                              border-collapse: collapse;
                              #white-space: nowrap;
                            }
                            table.peekAtMGTable td, table.peekAtMGTable th {
                              border: 1px solid #AAAAAA;
                              padding: 0.2em 0.5em;
                            }
                            table.peekAtMGTable tbody td {
                              font-size: 13px;
                            }
                            table.peekAtMGTable tr:nth-child(even) {
                              background: #f0f3f4;
                            }
                            table.peekAtMGTable thead {
                              background: #1C6EA4;
                              background: -moz-linear-gradient(top, #5592bb 0%, #327cad 66%, #1C6EA4 100%);
                              background: -webkit-linear-gradient(top, #5592bb 0%, #327cad 66%, #1C6EA4 100%);
                              background: linear-gradient(to bottom, #5592bb 0%, #327cad 66%, #1C6EA4 100%);
                              border-bottom: 2px solid #444444;
                            }
                            table.peekAtMGTable thead th {
                              font-size: 15px;
                              font-weight: bold;
                              color: #FFFFFF;
                              border-left: 2px solid #D0E4F5;
                            }
                            table.peekAtMGTable thead th:first-child {
                              border-left: none;
                            }
                          </style>\n");
    }

    return (d.as.html);
  }

  # In-memory medication groups: peek ----
  observeEvent(input$mg_from_memory_peek_button,
               {
                 # Sanity checks:
                 if( is.null(.GlobalEnv$.plotting.params$.inmemory.mg) ||
                     (!is.character(.GlobalEnv$.plotting.params$.inmemory.mg) && !is.factor(.GlobalEnv$.plotting.params$.inmemory.mg)) ||
                     length(.GlobalEnv$.plotting.params$.inmemory.mg) < 1 )
                 {
                   showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                                         div(paste0("Cannot load the selected medication group definitions '",input$mg_from_memory, "' from memory!\nPlease make sure you selected a valid vector of definitions..."), style="color: red;"),
                                         footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
                   return (invisible(NULL));
                 }

                 showModal(modalDialog(title="AdhereR: the selected in-memory medication group definitions ...",
                                       div(HTML(.show.medication.groups.as.HTML(.GlobalEnv$.plotting.params$.inmemory.mg)),
                                           style="max-height: 50vh; max-width: 90vw; overflow: auto; overflow-x:auto;"),
                                       footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));

               })

  # View the currently selected medication group definitions ----
  observeEvent(input$mg_view_button,
               {
                 # Sanity checks:
                 if( is.null(.GlobalEnv$.plotting.params$medication.groups) ||
                     (!is.character(.GlobalEnv$.plotting.params$medication.groups) && !is.factor(.GlobalEnv$.plotting.params$medication.groups)) ||
                     length(.GlobalEnv$.plotting.params$medication.groups) < 1 )
                 {
                   showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                                         div(paste0("The medication group definitions seems to be of the wrong type or empty!\n"), style="color: red;"),
                                         footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
                   return (invisible(NULL));
                 }

                 showModal(modalDialog(title="AdhereR: the medication group definitions ...",
                                       div(HTML(.show.medication.groups.as.HTML(.GlobalEnv$.plotting.params$medication.groups)),
                                           style="max-height: 50vh; max-width: 90vw; overflow: auto; overflow-x:auto;"),
                                       footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));

               })

  # Validate a given medication groups definition and possibly load it ----
  .validate.and.load.medication.groups <- function(mg, # the vector of definitions or column name
                                                   d=NULL # the data.frame to which these definitions refer to (or NULL for no such checks)
  )
  {
     # Place the data in the .GlobalEnv$.plotting.params list:
    .GlobalEnv$.plotting.params$medication.groups <- mg;
    .GlobalEnv$.plotting.params$medication.groups.to.plot <- NULL;

    # Force UI updating...
    .force.update.UI();
  }

  # In-memory medication group definitions: validate and use ----
  observeEvent(input$mg_from_memory_button_use,
               {
                 if( input$mg_definitions_source == 'named vector' )
                 {
                   # From a named vector in memory:

                   # Sanity checks:
                   if( is.null(.GlobalEnv$.plotting.params$.inmemory.mg) ||
                       (!is.character(.GlobalEnv$.plotting.params$.inmemory.mg) && !is.factor(.GlobalEnv$.plotting.params$.inmemory.mg)) ||
                       length(.GlobalEnv$.plotting.params$.inmemory.mg) < 1 )
                   {
                     showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                                           div(paste0("Cannot load the selected medication group definitions '",input$mg_from_memory, "' from memory!\nPlease make sure you selected a valid vector of definitions..."), style="color: red;"),
                                           footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
                     return (invisible(NULL));
                   }

                   # Checks:
                   .validate.and.load.medication.groups(.GlobalEnv$.plotting.params$.inmemory.mg,
                                                        .GlobalEnv$.plotting.params$data);

                   # Let the world know this:
                   .GlobalEnv$.plotting.params$.mg.type <- "in memory";
                   .GlobalEnv$.plotting.params$.mg.comes.from.function.arguments <- FALSE;
                   if( input$mg_from_memory == "[none]" )
                   {
                     # How did we get here???
                     return (invisible(NULL));
                   } else if( input$mg_from_memory == "<<'medication.groups' argument to plot_interactive_cma() call>>" )
                   {
                     # The special value pointing to the argument to plot_interactive_cma():
                     .GlobalEnv$.plotting.params$.mg.name <- NA;
                   } else
                   {
                     .GlobalEnv$.plotting.params$.mg.name <- input$mg_from_memory;
                   }
                 } else if( input$mg_definitions_source == 'column in data' )
                 {
                   # From column in the data:

                   # Check if the column name refers to an existing column in the dataset:
                   if( is.na(input$mg_from_column) || length(input$mg_from_column) != 1 || input$mg_from_column=="" ||
                       !(input$mg_from_column %in% .GlobalEnv$.plotting.params$get.colnames.fnc(.GlobalEnv$.plotting.params$data)) )
                   {
                     showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                                           div(paste0("The medication groups column '",ID.colname, "' must be a string and a valid column name in the selected dataset..."), style="color: red;"),
                                           footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
                     return (invisible(NULL));
                   }

                   # Validate and load the medication groups
                   .validate.and.load.medication.groups(input$mg_from_column,
                                                        .GlobalEnv$.plotting.params$data);

                   # Let the world know this:
                   .GlobalEnv$.plotting.params$.mg.type <- "column in data";
                   .GlobalEnv$.plotting.params$.mg.comes.from.function.arguments <- FALSE;
                   if( input$mg_from_column == "[none]" )
                   {
                     # How did we get here???
                     return (invisible(NULL));
                   } else if( input$mg_from_column == "<<'medication.groups' argument to plot_interactive_cma() call>>" )
                   {
                     # The special value pointing to the argument to plot_interactive_cma():
                     .GlobalEnv$.plotting.params$.mg.name <- NA;
                   } else
                   {
                     .GlobalEnv$.plotting.params$.mg.name <- input$mg_from_column;
                   }
                 }
               })

  # Show selected medication groups ----
  observeEvent(input$mg_plot_apply_button,
               {
                 # Sanity checks:

                 # Let the world know this:
                 .GlobalEnv$.plotting.params$medication.groups.to.plot <- input$mg_to_plot_list;

                 # Re-plot things:
                 rv$toggle.me <- !rv$toggle.me; # make the plotting aware of the changes
               })


  # CMA computation for multiple patients ----
  # Allow the user to break it and show progress (inspired by https://gist.github.com/jcheng5/1659aff15904a0c4ffcd4d0c7788f789 )
  # The CMA computation main UI ----
  observeEvent(input$compute_cma_for_larger_sample_button,
  {
    # Get the selected patient IDs:
    if( input$compute_cma_patient_selection_method == "by_id" )
    {
      patients.to.compute <- input$compute_cma_patient_by_id;
    } else if( input$compute_cma_patient_selection_method == "by_position" )
    {
      if( is.null(.GlobalEnv$.plotting.params$.patients.to.compute) ||
          !inherits(.GlobalEnv$.plotting.params$.patients.to.compute, "data.frame") )
      {
        .update.patients.IDs.table(reset.slider=FALSE); # for re-udating this table (there were left-over from previous computations that result in an ugly crash (but keep range)...
      }
      patients.to.compute <- .GlobalEnv$.plotting.params$.patients.to.compute$ID[
        .GlobalEnv$.plotting.params$.patients.to.compute$`#` %in%
          input$compute_cma_patient_by_group_range[1]:input$compute_cma_patient_by_group_range[2] ];
    }

    # Checks concerning the maximum number of patients and events to plot:
    msgs <- NULL;
    if( length(patients.to.compute) > .GlobalEnv$.plotting.params$max.number.patients.to.compute )
    {
      patients.to.compute <- patients.to.compute[ 1:.GlobalEnv$.plotting.params$max.number.patients.to.compute ];
      msgs <- c(msgs, paste0("Warning: a maximum of ",.GlobalEnv$.plotting.params$max.number.patients.to.compute,
                             " patients can be shown in an interactive plot: we kept only the first ",.GlobalEnv$.plotting.params$max.number.patients.to.compute,
                             " from those you selected!\n"));
    }

    data.for.patients <- .GlobalEnv$.plotting.params$get.data.for.patients.fnc(patients.to.compute, .GlobalEnv$.plotting.params$data, .GlobalEnv$.plotting.params$ID.colname)
    n.events.per.patient <- cumsum(table(data.for.patients[,.GlobalEnv$.plotting.params$ID.colname]));
    if( !is.null(data.for.patients) && nrow(data.for.patients) > .GlobalEnv$.plotting.params$max.number.events.to.compute )
    {
      n <- min(which(n.events.per.patient > .GlobalEnv$.plotting.params$max.number.events.to.compute));
      if( n > 1 ) n <- n-1;
      patients.to.compute <- patients.to.compute[ 1:n ];
      msgs <- c(msgs, paste0("Warning: a maximum of ",.GlobalEnv$.plotting.params$max.number.events.to.compute,
                             " events across all patients can be shown in an interactive plot: we kept only the first ",length(patients.to.compute),
                             " patients from those you selected (totalling ",n.events.per.patient[n]," events)!\n"));
    }
    .GlobalEnv$.plotting.params$.patients.to.compute <- patients.to.compute;

    # Where there any messages or anyhting else wrong? Ask the user if they are sure they want to start this...
    r.ver.info <- sessionInfo();
    cma.computation.progress.log.text <<- "";
    try(output$cma_computation_progress_log <- renderText(cma.computation.progress.log.text), silent=TRUE);
    showModal(modalDialog(title=div(icon("play", lib="glyphicon"), "AdhereR..."),
                          div(div(if(!is.null(msgs)) paste0("There ",ifelse(length(msgs)==1,"was a warning",paste0("were ",length(msgs)," warnings")),":\n") else ""),
                              div(HTML(paste0(msgs,collapse="<br/>")), style="color: red;"),
                              div(HTML(paste0("We will compute the selected CMA for the <b>",length(patients.to.compute)," patients</b>:"))),
                              div(paste0(patients.to.compute,collapse=", "), style="overflow: auto; max-height: 10em; color: blue;"),
                              div(HTML(paste0("totalling <b>",n.events.per.patient[length(patients.to.compute)]," events</b>."))),
                              div(HTML(paste0("The running time is limited to <b>",.GlobalEnv$.plotting.params$max.running.time.in.minutes.to.compute," minutes</b>."))),
                              div(HTML("The actual <code>R</code> code needed to compute the selected CMA for any set of patients and data source can be accessed through the "),
                                  span(icon("eye-open", lib="glyphicon"), strong("Show R code..."), style="border: 1px solid darkblue; border-radius: 5px; color: darkblue; background-color: lightblue"),
                                  HTML(" button in the main window (please ignore the plotting code).<br/>")),
                              hr(),
                              div(HTML(paste0("We are using ",R.version.string,
                                              " and AdhereR version ",descr <- utils::packageDescription("AdhereR")$Version,
                                              " on ",r.ver.info$running,"."))),
                              hr(),
                              shinyWidgets::progressBar(id="cma_computation_progress",
                                                        value=0, display_pct=TRUE, status="info",
                                                        title="Progress:"),
                              div(title="Progress long...",
                                  strong("Progress log:"), br(),
                                  div(htmlOutput(outputId="cma_computation_progress_log", inline=FALSE),
                                      id="cma_computation_progress_log_container",
                                      style="height: 5em; overflow: auto; border-radius: 5px; border-style: solid; border-width: 2px; background-color: #f0f0f0;"))

                            ),
                          footer = tagList(span(title="Close this dialog box",
                                               actionButton(inputId="close_compute_cma_dialog", label="Close", icon=icon("remove", lib="glyphicon"))),
                                           span(title="Start computation...",
                                                actionButton(inputId="start_computation_now", label="Start!", icon=icon("play", lib="glyphicon"))),
                                           span(title="Stop computation...",
                                                shinyjs::disabled(actionButton(inputId="cancel_cma_computation", label="Stop!", icon=icon("stop", lib="glyphicon")))),
                                           span(title="Save the results to a TAB-separated (no quotes) CSV file...",
                                                shinyjs::disabled(downloadButton(outputId="save_cma_computation_results", label="Save results (as TSV)", icon=icon("floppy-save", lib="glyphicon"))))
                                          )));

  })

  # Close the CMA computation main UI ----
  observeEvent(input$close_compute_cma_dialog,
  {
    removeModal();
  })


  # observeEvent(input$start_computation_now,
  # {
  #   session=shiny::getDefaultReactiveDomain();
  #   # Show up the progress bar and stopping button:
  #   #showModal(modalDialog(title=div(icon("hourglass", lib="glyphicon"), paste0("Computing CMA for ",length(.GlobalEnv$.plotting.params$.patients.to.compute)," patients: please wait...")),
  #   #                      #div(checkboxInput(inputId="stop_cma_computation", label="STOP!", value=FALSE)),
  #   #                      actionButton("stop","Stop",class="btn-danger", onclick="Shiny.onInputChange('stopThis',true)"),
  #   #                      footer=NULL));
  #
  #   cat(session$input$cma_class);
  #   cat('Computing CMA for patient: ');
  #   withProgress(message="", detail="",
  #                min=0, max=length(.GlobalEnv$.plotting.params$.patients.to.compute), value=0,
  #                {
  #                  start.time <- Sys.time();
  #                  for(i in seq_along(.GlobalEnv$.plotting.params$.patients.to.compute) )
  #                  {
  #                    # Show the patient currently processed:
  #                    cur.time <- Sys.time();
  #                    time.left <- difftime(cur.time, start.time, units="sec");
  #                    incProgress(0,
  #                                message=paste0("Computing patient ",.GlobalEnv$.plotting.params$.patients.to.compute[i]),
  #                                detail=paste0(" (",round(difftime(cur.time, start.time, units="sec"),1),"s)"));
  #                    cat(paste0(.GlobalEnv$.plotting.params$.patients.to.compute[i]," (",round(difftime(cur.time, start.time, units="sec"),1),"s)",", "));
  #
  #                    # The computation:
  #                    Sys.sleep(1);
  #
  #                    # Increment the progress:
  #                    incProgress(1);
  #
  #                    # Stop?
  #                    httpuv:::service();
  #                    invalidateLater(1);
  #                    cat(input$cma_class);
  #                    if( !is.null(session$input$stopThis) && session$input$stopThis )
  #                    {
  #                      cat("\nCancelled....\n")
  #                      break;
  #                    }
  #                  }
  #                  incProgress(0,
  #                              message=paste0("Completed in ",round(difftime(cur.time, start.time, units="sec"),1)," seconds!"), detail="");
  #                  cat("DONE in ",round(difftime(cur.time, start.time, units="sec"),1)," seconds\n");
  #                  Sys.sleep(2); # wait a bit for the message to be (possibly) seen...
  #                });
  #
  #   removeModal();
  # })

  # Compute CMA for one patient ----
  .compute.cma.for.patient <- function(i, start.time)
  {
    # Show the patient currently processed:
    cur.time <- Sys.time();
    #cat(paste0(.GlobalEnv$.plotting.params$.patients.to.compute[i]," (",round(difftime(cur.time, start.time, units="sec"),1),"s)",", "));
    shinyWidgets::updateProgressBar(session, id="cma_computation_progress", value=(i/length(.GlobalEnv$.plotting.params$.patients.to.compute))*100,
                                    title=paste0("Progress: computing CMA for patient '",
                                                      .GlobalEnv$.plotting.params$.patients.to.compute[i],"' (",
                                                      i," of ",length(.GlobalEnv$.plotting.params$.patients.to.compute),"); time: ",
                                                      round(difftime(cur.time, start.time, units="sec"),1)," seconds."));

    # The computation:
    res <- NULL;
    compute.start.time <- Sys.time();
    msgs <- capture.output(res <-
                             .GlobalEnv$.plotting.params$.plotting.fnc(data=.GlobalEnv$.plotting.params$data,
                                                                       ID.colname=.GlobalEnv$.plotting.params$ID.colname,
                                                                       event.date.colname=.GlobalEnv$.plotting.params$event.date.colname,
                                                                       event.duration.colname=.GlobalEnv$.plotting.params$event.duration.colname,
                                                                       event.daily.dose.colname=.GlobalEnv$.plotting.params$event.daily.dose.colname,
                                                                       medication.class.colname=.GlobalEnv$.plotting.params$medication.class.colname,
                                                                       date.format=.GlobalEnv$.plotting.params$date.format,

                                                                       ID=.GlobalEnv$.plotting.params$.patients.to.compute[i],

                                                                       medication.groups=if( input$mg_use_medication_groups ){ .GlobalEnv$.plotting.params$medication.groups }else{ NULL },
                                                                       medication.groups.separator.show=input$mg_plot_by_patient,
                                                                       medication.groups.to.plot=.GlobalEnv$.plotting.params$medication.groups.to.plot,
                                                                       medication.groups.separator.lty=input$plot_mg_separator_lty,
                                                                       medication.groups.separator.lwd=input$plot_mg_separator_lwd,
                                                                       medication.groups.separator.color=input$plot_mg_separator_color,
                                                                       medication.groups.allother.label=input$plot_mg_allothers_label,

                                                                       cma=ifelse(input$cma_class == "simple",
                                                                                  input$cma_to_compute,
                                                                                  input$cma_class),
                                                                       cma.to.apply=ifelse(input$cma_class == "simple",
                                                                                           "none",
                                                                                           ifelse(input$cma_to_compute_within_complex == "CMA0",
                                                                                                  "CMA1",
                                                                                                  input$cma_to_compute_within_complex)), # don't use CMA0 for complex CMAs
                                                                       #carryover.within.obs.window=FALSE,
                                                                       #carryover.into.obs.window=FALSE,
                                                                       carry.only.for.same.medication=input$carry_only_for_same_medication,
                                                                       consider.dosage.change=input$consider_dosage_change,
                                                                       followup.window.start=ifelse(input$followup_window_start_unit== "calendar date",
                                                                                                    as.Date(input$followup_window_start_date, format="%Y-%m-%d"),
                                                                                                    as.numeric(input$followup_window_start_no_units)),
                                                                       followup.window.start.unit=ifelse(input$followup_window_start_unit== "calendar date",
                                                                                                         "days",
                                                                                                         input$followup_window_start_unit),
                                                                       followup.window.duration=as.numeric(input$followup_window_duration),
                                                                       followup.window.duration.unit=input$followup_window_duration_unit,
                                                                       observation.window.start=ifelse(input$observation_window_start_unit== "calendar date",
                                                                                                       as.Date(input$observation_window_start_date, format="%Y-%m-%d"),
                                                                                                       as.numeric(input$observation_window_start_no_units)),
                                                                       observation.window.start.unit=ifelse(input$observation_window_start_unit== "calendar date",
                                                                                                            "days",
                                                                                                            input$observation_window_start_unit),
                                                                       observation.window.duration=as.numeric(input$observation_window_duration),
                                                                       observation.window.duration.unit=input$observation_window_duration_unit,
                                                                       medication.change.means.new.treatment.episode=input$medication_change_means_new_treatment_episode,
                                                                       dosage.change.means.new.treatment.episode=input$dosage_change_means_new_treatment_episode,
                                                                       maximum.permissible.gap=as.numeric(input$maximum_permissible_gap),
                                                                       maximum.permissible.gap.unit=input$maximum_permissible_gap_unit,
                                                                       sliding.window.start=as.numeric(input$sliding_window_start),
                                                                       sliding.window.start.unit=input$sliding_window_start_unit,
                                                                       sliding.window.duration=as.numeric(input$sliding_window_duration),
                                                                       sliding.window.duration.unit=input$sliding_window_duration_unit,
                                                                       sliding.window.step.duration=as.numeric(input$sliding_window_step_duration),
                                                                       sliding.window.step.unit=input$sliding_window_step_unit,
                                                                       sliding.window.no.steps=ifelse(input$sliding_window_step_choice == "number of steps" ,
                                                                                                      as.numeric(input$sliding_window_no_steps),
                                                                                                      NA),
                                                                       get.colnames.fnc=.GlobalEnv$.plotting.params$get.colnames.fnc,
                                                                       get.patients.fnc=.GlobalEnv$.plotting.params$get.patients.fnc,
                                                                       get.data.for.patients.fnc=.GlobalEnv$.plotting.params$get.data.for.patients.fnc,
                                                                       compute.cma.only=TRUE # just compute the CMA, no plotting please!
                             ));

    # Display result summary and return full info (including any messages/warnings/errors):
    if( is.null(res) || length(grep("error", msgs, ignore.case=TRUE)) > 0 )
    {
      # Errors:
      cma.computation.progress.log.text <<- paste0(cma.computation.progress.log.text,
                                                   "<span style='color: red;'>Patient <b>",
                                                   .GlobalEnv$.plotting.params$.patients.to.compute[i],
                                                   "</b> <b>failed</b> after ",
                                                   round(difftime(Sys.time(),compute.start.time,units="sec"),2)," seconds",
                                                   if(length(msgs)>1) paste0(" with error(s): ", paste0("'",msgs,"'",collapse="; ")),
                                                   ".</span><br/>");
    } else if( length(grep("warning", msgs, ignore.case=TRUE)) > 0 )
    {
      # Warnings:
      cma.computation.progress.log.text <<- paste0(cma.computation.progress.log.text,
                                                   "<span style='color: blue;'>Patient <b>",
                                                   .GlobalEnv$.plotting.params$.patients.to.compute[i],
                                                   "</b> finished <b>successfully</b> after ",
                                                   round(difftime(Sys.time(),compute.start.time,units="sec"),2)," seconds",
                                                   if(length(msgs)>1) paste0(", but with warning(s): ", paste0("'",msgs,"'",collapse="; ")),
                                                   ".</span><br/>");
    } else
    {
      # Normal output:
      cma.computation.progress.log.text <<- paste0(cma.computation.progress.log.text,
                                                   "<span style='color: black;'>Patient <b>",
                                                   .GlobalEnv$.plotting.params$.patients.to.compute[i],
                                                   "</b> finished <b>successfully</b> after ",
                                                   round(difftime(Sys.time(),compute.start.time,units="sec"),2)," seconds",
                                                   if(length(msgs)>1) paste0(" with message(s): ", paste0("'",msgs,"'",collapse="; ")),
                                                   ".</span><br/>");
    }

    # Print progress so far...
    output$cma_computation_progress_log <- renderText(cma.computation.progress.log.text);
    shinyjs::js$scroll_cma_compute_log(); # make sure the last message is in view

    # Return the results:
    if( !input$mg_use_medication_groups )
    {
      # No medication groups:
      return (AdhereR::getCMA(res));
    } else
    {
      # Medication groups:
      return (AdhereR::getCMA(res, flatten.medication.groups=TRUE));
    }
  }

  # Collect computed CMA for several patients ----
  collected.results <<- list();
  cma.computation.progress.log.text <<- "";
  workQueue <- function(start.time = Sys.time(),
                        max.time = Inf, # max run time (in seconds, Inf == forever)
                        cancel = cancelFunc,
                        onSuccess = NULL,
                        onError = stop)
  {
    i <- 1;

    if (is.null(onSuccess)) { onSuccess <- function(...) NULL }

    result <- list();
    makeReactiveBinding("result");

    observe(
    {
      if( i > length(.GlobalEnv$.plotting.params$.patients.to.compute) ) # natural finishing
      {
        #message("Finished naturally");
        result <<- i;
        #removeModal();
        shinyjs::enable('start_computation_now');
        shinyjs::disable('cancel_cma_computation');
        shinyjs::enable('close_compute_cma_dialog');
        if( !is.null(collected.results) && length(collected.results) > 0 ) shinyjs::enable('save_cma_computation_results');
        shinyWidgets::updateProgressBar(session, id="cma_computation_progress", value=100,
                                        title=paste0("Finished for all ",length(.GlobalEnv$.plotting.params$.patients.to.compute)," patients, took ",round(difftime(Sys.time(), start.time, units="sec"),1)," seconds."));
        cma.computation.progress.log.text <<- paste0(cma.computation.progress.log.text,
                                                     "<br/>Finished for all ",length(.GlobalEnv$.plotting.params$.patients.to.compute)," patients, took ",round(difftime(Sys.time(), start.time, units="sec"),1)," seconds.");
        output$cma_computation_progress_log <- renderText(cma.computation.progress.log.text);
        shinyjs::js$scroll_cma_compute_log(); # make sure the last message is in view
        return();
      }

      time.spent <- difftime(Sys.time(), start.time, units="sec");
      if( time.spent > max.time ) # finished because the time ran out
      {
        #message("Ran out of time");
        result <<- Inf; # ran out of time
        #removeModal();
        shinyjs::enable('start_computation_now');
        shinyjs::disable('cancel_cma_computation');
        shinyjs::enable('close_compute_cma_dialog');
        if( !is.null(collected.results) && length(collected.results) > 0 ) shinyjs::enable('save_cma_computation_results');
        shinyWidgets::updateProgressBar(session, id="cma_computation_progress", value=(i/length(.GlobalEnv$.plotting.params$.patients.to.compute))*100,
                                        title=paste0("Stopped after ",round(difftime(Sys.time(), start.time, units="sec"),1)," seconds, succesfully computed for ",i," patients."));
        cma.computation.progress.log.text <<- paste0(cma.computation.progress.log.text,
                                                     "<br/>Stopped after ",round(difftime(Sys.time(), start.time, units="sec"),1)," seconds, succesfully computed for ",i," patients.");
        output$cma_computation_progress_log <- renderText(cma.computation.progress.log.text);
        shinyjs::js$scroll_cma_compute_log(); # make sure the last message is in view
        return();
      }

      if( isolate(cancel()) )
      {
        #message("Cancelled by user");
        #collected.results <<- list(); # erase the results collected so far...
        result <- NA; # cancelled by user
        #removeModal();
        shinyjs::enable('start_computation_now');
        shinyjs::disable('cancel_cma_computation');
        shinyjs::enable('close_compute_cma_dialog');
        if( !is.null(collected.results) && length(collected.results) > 0 ) shinyjs::enable('save_cma_computation_results');
        shinyWidgets::updateProgressBar(session, id="cma_computation_progress", value=(i/length(.GlobalEnv$.plotting.params$.patients.to.compute))*100,
                                        title=paste0("Manually cancelled after ",round(difftime(Sys.time(), start.time, units="sec"),1)," seconds, succesfully computed for ",i," patients."));
        cma.computation.progress.log.text <<- paste0(cma.computation.progress.log.text,
                                                     "<br/>Manually cancelled after ",round(difftime(Sys.time(), start.time, units="sec"),1)," seconds, succesfully computed for ",i," patients.");
        output$cma_computation_progress_log <- renderText(cma.computation.progress.log.text);
        shinyjs::js$scroll_cma_compute_log(); # make sure the last message is in view
        return();
      }

      tryCatch(
        {
          collected.results[[length(collected.results) + 1]] <<- isolate(.compute.cma.for.patient(i, start.time));
          names(collected.results)[length(collected.results)] <- .GlobalEnv$.plotting.params$.patients.to.compute[i];
          result <<- i;
        }, error = onError)
      i <<- i + 1;
      invalidateLater(1);
    });

    reactive(req(result));
  }

  # observeEvent(input$go,
  # {
  #   isCancelled <- local(
  #   {
  #     origCancel <- isolate(input$cancel_cma_computation);
  #     function() { !identical(origCancel, input$cancel_cma_computation) }
  #   });
  #
  #   cat('Computing CMA for patient: ');
  #   collected.results <<- list();
  #   result <- workQueue(patients=.GlobalEnv$.plotting.params$.patients.to.compute,
  #                       cancel=isCancelled);
  #
  #   #observe(
  #   #{
  #   #  val <- result();
  #   #  message("The result was ", val);
  #   #});
  # })

  # Start CMA computation ----
  observeEvent(input$start_computation_now,
  {
    # Show up the progress bar and stopping button:
    #showModal(modalDialog(title=div(icon("hourglass", lib="glyphicon"), paste0("Computing CMA for ",length(.GlobalEnv$.plotting.params$.patients.to.compute)," patients: please wait...")),
    #                      #div(checkboxInput(inputId="stop_cma_computation", label="STOP!", value=FALSE)),
    #                      #actionButton("go", "Go"),
    #                      actionButton("cancel_cma_computation", "Stop"),
    #                      footer=NULL));

    shinyjs::disable('start_computation_now');
    shinyjs::enable('cancel_cma_computation');
    shinyjs::disable('close_compute_cma_dialog');
    shinyjs::disable('save_cma_computation_results');
    shinyWidgets::updateProgressBar(session, id="cma_computation_progress", value=0);

    isCancelled <- local(
    {
      origCancel <- isolate(input$cancel_cma_computation);
      function() { !identical(origCancel, input$cancel_cma_computation) }
    });

    #cat('Computing CMA for patient: ');
    collected.results <<- list();
    cma.computation.progress.log.text <<- "";
    result <- workQueue(cancel=isCancelled);

    #observe(
    #{
    #  val <- result();
    #  message("The result was ", val);
    #});
  })

  # observeEvent(input$save_cma_computation_results,
  # {
  #   if( is.null(collected.results) || length(collected.results) < 1 )
  #   {
  #     showModal(modalDialog(title="Adherer warning...", "No results to export..."));
  #     return (invisible(NULL));
  #   }
  #
  #   # Assemble the results as a single data.frame:
  #   d <- NULL;
  #   try(d <- do.call(rbind, collected.results), silent=TRUE);
  #   if( !is.null(d) || !inherits(d, "data.frame") || nrow(d) < 1 || ncol(d) < 1 )
  #   {
  #     showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
  #                           div("Error collecting the results of the CMA computation: nothing to save to file!", style="color: red;"),
  #                           footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
  #     return (invisible(NULL));
  #   }
  #
  #   # Ask the user for a file to save them to:
  #
  # })

  # Export results to file ----
  output$save_cma_computation_results <- downloadHandler(
    filename = function() paste0("adherer-compute-",input$cma_class,"-",ifelse(input$cma_class=="simple", input$cma_to_compute, input$cma_to_compute_within_complex),"-results.tsv"),
    content = function(file)
    {
      if( is.null(collected.results) || length(collected.results) < 1 )
      {
        showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "Adherer warning..."), "No results to export..."));
      } else
      {
        # Assemble the results as a single data.frame:
        d <- NULL;
        try(d <- do.call(rbind, collected.results), silent=FALSE);
        if( is.null(d) || !inherits(d, "data.frame") || nrow(d) < 1 || ncol(d) < 1 )
        {
          showModal(modalDialog(title=div(icon("exclamation-sign", lib="glyphicon"), "AdhereR error!"),
                                div("Error collecting the results of the CMA computation: nothing to save to file!", style="color: red;"),
                                footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        } else
        {
          # Write them to file:
          write.table(d, file=file, col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t");
        }
      }
    }
  )

  # Make sure the UI is properly updated for each new session ----
  isolate(
  {
    .list.mg.from.memory();
    .force.update.UI();
    removeModal();
    shinyjs::show(id="sidebar_tabpanel_container");
    #updateCheckboxInput(session, inputId="output_panel_container_show", value=TRUE);
    shinyjs::show(id="output_panel_container");
  })

}


# Call shiny ----
shinyApp(ui=ui, server=server);

