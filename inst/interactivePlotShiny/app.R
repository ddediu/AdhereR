###############################################################################################
#
#    AdhereR: an R package for computing various estimates of medication adherence.
#    This implements interactive plotting using shiny.
#    Copyright (C) 2015-2018  Dan Dediu & Alexandra Dima
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


#library(shiny)
#' @import shiny
#' @import colourpicker
#' @import viridisLite
#' @import highlight
#' @import clipr
#' @import shinyjs
NULL


# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # JavaScript ----
  shinyjs::useShinyjs(),
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
             div(title="About AdhereR and links to more info online and offline...",
                 actionButton(inputId="about_button", label=strong("About"), icon=icon("question-sign", lib="glyphicon"), style="color: #3498db; border: none; background: none;"),
                 style="float: right;")
           ),
           hr()
    ),

    #shinythemes::themeSelector(),

    # SIDEBAR PANEL ----
    column(3,

           # PARAMS TAB ----
           tabsetPanel(id="sidebar-tabpanel",
                       tabPanel("Params", value="sidebar-params-tab", icon=icon("wrench", lib="glyphicon"), fluid=TRUE,
                                wellPanel(id = "tPanel", style = "overflow:scroll; max-height: 90vh;",


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
                                                                div(title='Select the actual start date of the follow-up window (possibly using a calendar widget)',
                                                                    dateInput(inputId="followup_window_start_date",
                                                                              label="FUW start",
                                                                              value=NULL, format="dd/mm/yyyy", startview="month", weekstart=1))
                                                              ),

                                                              ## If follow-up window unit is "column in dataset"
                                                              #conditionalPanel(
                                                              #  condition = "(input.followup_window_start_unit == 'column in dataset')",
                                                              #              # Select an actual date ----
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
                                                                div(title='Select the actual start date of the observation window (possibly using a calendar widget)',
                                                                    dateInput(inputId="observation_window_start_date",
                                                                              label="OW start",
                                                                              value=NULL, format="dd/mm/yyyy", startview="month", weekstart=1))
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
                                                                    checkboxInput(inputId="carry_only_for_same_medication",
                                                                                  label="For same treat. only?",
                                                                                  value=FALSE)),

                                                                # Consider dosage changes?
                                                                div(title='Consider dosage change when computing the carry over?',
                                                                    checkboxInput(inputId="consider_dosage_change",
                                                                                  label="Consider dose changes?",
                                                                                  value=FALSE)),

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
                                                                div(title='Does changing the treatment type trigger a new episode?',
                                                                    checkboxInput(inputId="medication_change_means_new_treatment_episode",
                                                                                  label="Treat. change starts new episode?",
                                                                                  value=FALSE)),

                                                                # Does dosage change start new episode?
                                                                div(title='Does changing the dose trigger a new episode?',
                                                                    checkboxInput(inputId="dosage_change_means_new_treatment_episode",
                                                                                  label="Dose change starts new episode?",
                                                                                  value=FALSE)),

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

                                                                # Plot CMA as histogram
                                                                div(title='Show the distribution of estimated CMAs across episodes as a histogram or barplot?',
                                                                    checkboxInput(inputId="plot_CMA_as_histogram_episodes",
                                                                                  label="Plot CMA as histogram?",
                                                                                  value=FALSE)),

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
                                                                    checkboxInput(inputId="plot_CMA_as_histogram_sliding_window",
                                                                                  label="Plot CMA as histogram?",
                                                                                  value=TRUE)),

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
                                                                    checkboxInput(inputId="plot_align_all_patients",
                                                                                  label="Align patients?",
                                                                                  value=FALSE)),

                                                                # Align al patients
                                                                conditionalPanel(
                                                                  condition="input.plot_align_all_patients",
                                                                  div(title='Should the first event (across patients) be considered as the origin of time?',
                                                                      checkboxInput(inputId="plot_align_first_event_at_zero",
                                                                                    label="Align 1st event at 0?",
                                                                                    value=FALSE))
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
                                                                               label="Duration",
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
                                                                      checkboxInput(inputId="print_cma",
                                                                                    label="Print CMA?",
                                                                                    value=TRUE))
                                                                ),

                                                                div(title='Plot the CMA estimate next to the participant\'s ID?',
                                                                    checkboxInput(inputId="plot_cma",
                                                                                  label="Plot CMA?",
                                                                                  value=TRUE)),

                                                                hr()
                                            ))
                                          ),


                                          # Dose ----
                                          div(id='dose_section', style="cursor: pointer;",
                                              span(title='Show dose', h4("Show dose"), style="color:DarkBlue"),
                                              div(title='Click to unfold...', id="dose_unfold_icon", icon("option-horizontal", lib="glyphicon"))),

                                          shinyjs::hidden(div(id="dose_contents",
                                                              # Print dose?
                                                              div(title='Print the dosage (i.e., the actual numeric values)?',
                                                                  checkboxInput(inputId="print_dose",
                                                                                label="Print dose?",
                                                                                value=FALSE)),

                                                              # Print dose attributes
                                                              conditionalPanel(
                                                                condition="input.print_dose",

                                                                div(title='Relative font size',
                                                                    numericInput(inputId="cex_dose",
                                                                                 label="Font size",
                                                                                 value=0.75, min=0.0, max=NA, step=0.25)),

                                                                div(title='Dose text outline color',
                                                                    colourpicker::colourInput(inputId="print_dose_outline_col",
                                                                                              label="Outline color",
                                                                                              value="white")),

                                                                div(title='Print the dose centered on the event?',
                                                                    checkboxInput(inputId="print_dose_centered",
                                                                                  label="Print centered?",
                                                                                  value=FALSE)),

                                                                hr()
                                                              ),

                                                              # Plot dose?
                                                              div(title='Represent the dose as event line width?',
                                                                  checkboxInput(inputId="plot_dose",
                                                                                label="Dose as line width?",
                                                                                value=FALSE)),

                                                              # Plot dose attributes
                                                              conditionalPanel(
                                                                condition="input.plot_dose",

                                                                div(title='What line width corresponds to the maximum dose?',
                                                                    numericInput(inputId="lwd_event_max_dose",
                                                                                 label="Max dose width",
                                                                                 value=8, min=1, max=NA, step=1)),

                                                                div(title='Consider maximum dose globally or per each medication class separately?',
                                                                    checkboxInput(inputId="plot_dose_lwd_across_medication_classes",
                                                                                  label="Global max dose?",
                                                                                  value=FALSE))
                                                              ),

                                                              hr()
                                          )),


                                          # Legend ----
                                          div(id='legend_section', style="cursor: pointer;",
                                              span(title='The legend', h4("Legend"), style="color:DarkBlue"),
                                              div(title='Click to unfold...', id="legend_unfold_icon", icon("option-horizontal", lib="glyphicon"))),

                                          shinyjs::hidden(div(id="legend_contents",
                                                              # Show legend?
                                                              div(title='Display the plot legend?',
                                                                  checkboxInput(inputId="show_legend",
                                                                                label="Show the legend?",
                                                                                value=TRUE)),

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

                                                                div(title='Relative font size of legend title',
                                                                    numericInput(inputId="legend_cex_title",
                                                                                 label="Title font size",
                                                                                 value=1.0, min=0.0, max=NA, step=0.25)),

                                                                div(title='Relative font size of legend text and symbols',
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
                                                              div(title='Draw grayscale (overrides everything lese)?',
                                                                  checkboxInput(inputId="bw_plot",
                                                                                label="Draw grayscale?",
                                                                                value=FALSE)),

                                                              hr(),

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
                                                              div(title='Color palette for mapping treatment categories to colors (the last two are colour-blind-friendly and provided by ).\nPlease see R\'s help for more info about each palette (first 5 are provided by the standard library, and the last 5 are in package "viridisLight").\nThe mapping is done automatically based on category order.',
                                                                  selectInput(inputId="col_cats",
                                                                              label="Treatment palette",
                                                                              choices=c("rainbow", "heat.colors", "terrain.colors", "topo.colors", "cm.colors", "magma", "inferno", "plasma", "viridis", "cividis"),
                                                                              selected="rainbow")),

                                                              hr(),

                                                              div(title='Event visual attributes',
                                                                  span(p("Events"), style="color:RoyalBlue; font-weight: bold;")),

                                                              # Event style:
                                                              div(title='Event line style',
                                                                  selectInput(inputId="lty_event",
                                                                              label="Event line style",
                                                                              choices=c("\U00A0\U00A0\U00A0\U00A0\U00A0 blank"="blank",
                                                                                        "\U2E3B solid"="solid",
                                                                                        "\U2012\U2009\U2012\U2009\U2012\U2009\U2012 dashed"="dashed",
                                                                                        "\U00B7\U00B7\U00B7\U00B7\U00B7\U00B7\U00B7 dotted"="dotted",
                                                                                        "\U00B7\U2012\U00B7\U2012\U00B7\U2012 dotdash"="dotdash",
                                                                                        "\U2014\U2014\U2009\U2014\U2014 longdash"="longdash",
                                                                                        "\U2012\U2009\U2014\U2009\U2012\U2009\U2014 twodash"="twodash"),
                                                                              selected="solid")),
                                                              div(title='Event line width',
                                                                  numericInput(inputId="lwd_event",
                                                                               label="Event line width",
                                                                               value=2, min=0, max=NA, step=1)),
                                                              div(title='Event start symbol (most commonly used)...',
                                                                  selectInput(inputId="pch_start_event",
                                                                              label="Event start",
                                                                              choices=c("none"=NA,
                                                                                        "\UFF0B plus"=3,
                                                                                        "\U00D7times"=4,
                                                                                        "\U2733\UFE0E star"=8,
                                                                                        "\U25FB\UFE0E square"=0,
                                                                                        "\U25CB circle"=1,
                                                                                        "\U25B3 up triangle"=2,
                                                                                        "\U25BD down triangle"=6,
                                                                                        "\U25C7 diamond"=5,
                                                                                        "\U25A0 fill square"=15,
                                                                                        "\U25CF fill small circle"=20,
                                                                                        "\U26AB\UFE0E fill med circle"=16,
                                                                                        "\U2B24 fill big circle"=19,
                                                                                        "\U25B2 fill triangle"=17,
                                                                                        "\U25C6 fill diamond"=18,
                                                                                        "\U229E square plus"=12,
                                                                                        "\U22A0 square times"=7,
                                                                                        "\U2A01 circle plus"=10,
                                                                                        "\U2A02 circle times"=13,
                                                                                        "\U2721 David star"=11),
                                                                              selected=15)),
                                                              div(title='Event end symbol (most commonly used)...',
                                                                  selectInput(inputId="pch_end_event",
                                                                              label="Event end",
                                                                              choices=c("none"=NA,
                                                                                        "\UFF0B plus"=3,
                                                                                        "\U00D7times"=4,
                                                                                        "\U2733\UFE0E star"=8,
                                                                                        "\U25FB\UFE0E square"=0,
                                                                                        "\U25CB circle"=1,
                                                                                        "\U25B3 up triangle"=2,
                                                                                        "\U25BD down triangle"=6,
                                                                                        "\U25C7 diamond"=5,
                                                                                        "\U25A0 fill square"=15,
                                                                                        "\U25CF fill small circle"=20,
                                                                                        "\U26AB\UFE0E fill med circle"=16,
                                                                                        "\U2B24 fill big circle"=19,
                                                                                        "\U25B2 fill triangle"=17,
                                                                                        "\U25C6 fill diamond"=18,
                                                                                        "\U229E square plus"=12,
                                                                                        "\U22A0 square times"=7,
                                                                                        "\U2A01 circle plus"=10,
                                                                                        "\U2A02 circle times"=13,
                                                                                        "\U2721 David star"=11),
                                                                              selected=16)),

                                                              hr(),

                                                              # Continuation (CMA0 and complex only):
                                                              conditionalPanel(
                                                                condition="(input.cma_class == 'per eipsode') || (input.cma_class == 'sliding window') || (input.cma_class == 'simple' && input.cma_to_compute == 'CMA0')",

                                                                div(title='Continuation visual attributes',
                                                                    span(p("Continuation"), style="color:RoyalBlue; font-weight: bold;")),

                                                                div(title='The color of continuation lines connecting consecutive events',
                                                                    colourpicker::colourInput(inputId="col_continuation",
                                                                                              label="Cont. line color",
                                                                                              value="black")),
                                                                div(title='The line style of continuation lines connecting consecutive events',
                                                                    selectInput(inputId="lty_continuation",
                                                                                label="Cont. line style",
                                                                                choices=c("\U00A0\U00A0\U00A0\U00A0\U00A0 blank"="blank",
                                                                                          "\U2E3B solid"="solid",
                                                                                          "\U2012\U2009\U2012\U2009\U2012\U2009\U2012 dashed"="dashed",
                                                                                          "\U00B7\U00B7\U00B7\U00B7\U00B7\U00B7\U00B7 dotted"="dotted",
                                                                                          "\U00B7\U2012\U00B7\U2012\U00B7\U2012 dotdash"="dotdash",
                                                                                          "\U2014\U2014\U2009\U2014\U2014 longdash"="longdash",
                                                                                          "\U2012\U2009\U2014\U2009\U2012\U2009\U2014 twodash"="twodash"),
                                                                                selected="dotted")),
                                                                div(title='The line width of continuation lines connecting consecutive events',
                                                                    numericInput(inputId="lwd_continuation",
                                                                                 label="Cont. line width",
                                                                                 value=1, min=0, max=NA, step=1)),

                                                                hr()
                                                              ),

                                                              # Show event intervals:
                                                              conditionalPanel(
                                                                condition="(input.cma_class == 'simple' && input.cma_to_compute != 'CMA0')",

                                                                div(title='Event intervals',
                                                                    span(p("Event intervals"), style="color:RoyalBlue; font-weight: bold;")),

                                                                div(title='Show the event intervals?',
                                                                    checkboxInput(inputId="show_event_intervals",
                                                                                  label="Show event interv.?",
                                                                                  value=TRUE)),

                                                                hr()
                                                              ),

                                                              # Font sizes:
                                                              div(title='Font sizes',
                                                                  span(p("Font sizes"), style="color:RoyalBlue; font-weight: bold;")),
                                                              div(title='Relative font size of general plotting text',
                                                                  numericInput(inputId="cex",
                                                                               label="General font size",
                                                                               value=1.0, min=0.0, max=NA, step=0.25)),
                                                              div(title='Relative font size of axis text',
                                                                  numericInput(inputId="cex_axis",
                                                                               label="Axis font size",
                                                                               value=0.75, min=0.0, max=NA, step=0.25)),
                                                              div(title='Relative font size of axis labels text',
                                                                  numericInput(inputId="cex_lab",
                                                                               label="Axis labels font size",
                                                                               value=1.0, min=0.0, max=NA, step=0.25)),

                                                              hr(),

                                                              # Follow-up window:
                                                              div(title='Follow-up window visual attributes',
                                                                  span(p("FUW visuals"), style="color:RoyalBlue; font-weight: bold;")),
                                                              div(title='Show the follow-up window?',
                                                                  checkboxInput(inputId="highlight_followup_window",
                                                                                label="Show FUW?",
                                                                                value=TRUE)),
                                                              conditionalPanel(
                                                                condition="input.highlight_followup_window",

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
                                                                  checkboxInput(inputId="highlight_observation_window",
                                                                                label="Show OW?",
                                                                                value=TRUE)),
                                                              conditionalPanel(
                                                                condition="input.highlight_observation_window",

                                                                div(title='The color of the observation window',
                                                                    colourpicker::colourInput(inputId="observation_window_col",
                                                                                              label="OW color",
                                                                                              value="yellow")),
                                                                div(title='The density of the hashing lines (number of lines per inch) used to draw the observation window',
                                                                    numericInput(inputId="observation_window_density",
                                                                                 label="OW hash dens.",
                                                                                 value=35, min=0, max=NA, step=5)),
                                                                div(title='The orientation of the hashing lines (in degrees) used to draw the observation window',
                                                                    sliderInput(inputId="observation_window_angle",
                                                                                label="OW hash angle",
                                                                                min=-90.0, max=90.0, value=-30, step=15, round=TRUE)),
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
                                                                    checkboxInput(inputId="show_real_obs_window_start",
                                                                                  label="Show real OW?",
                                                                                  value=TRUE)),
                                                                conditionalPanel(
                                                                  condition="input.show_real_obs_window_start",

                                                                  div(title='The density of the hashing lines (number of lines per inch) used to draw the real observation window',
                                                                      numericInput(inputId="real_obs_window_density",
                                                                                   label="Real OW hash dens.",
                                                                                   value=35, min=0, max=NA, step=5)),
                                                                  div(title='The orientation of the hashing lines (in degrees) used to draw the real observation window',
                                                                      sliderInput(inputId="real_obs_window_angle",
                                                                                  label="Real OW hash angle",
                                                                                  min=-90.0, max=90.0, value=30, step=15, round=TRUE))
                                                                ),

                                                                hr()
                                                              ),

                                                              # CMA estimate aesthetics:
                                                              conditionalPanel(
                                                                condition="(input.cma_class == 'per episode') || (input.cma_class == 'sliding window') || (input.cma_class == 'simple' && input.cma_to_compute != 'CMA0')",

                                                                div(title='CMA estimate visual attributes',
                                                                    span(p("CMA estimate"), style="color:RoyalBlue; font-weight: bold;")),

                                                                conditionalPanel(
                                                                  condition="input.plot_cma",

                                                                  div(title='Relative font size of CMA estimate for per episode and sliding windows',
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

                                                                  hr()
                                                                )

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
                                                                               value=10, min=0, max=NA, step=1.0)),

                                                              div(title='The minimum vertical plot size (in characters, per event)',
                                                                  numericInput(inputId="min_plot_size_in_characters_vert",
                                                                               label="Min plot size (vert.)",
                                                                               value=0.5, min=0.0, max=NA, step=0.25))
                                          )),


                                          # Allow last comma:
                                          NULL
                                )),

                       # DATA TAB ----
                       tabPanel("Data", value="sidebar-params-data", icon=icon("hdd", lib="glyphicon"), fluid=TRUE,
                                wellPanel(id = "tPanel2", style = "overflow:scroll; max-height: 90vh; min-height: 50vh",

                                          # Datasource ----
                                          span(title='The data source to use...',
                                               h4("Data source"), style="color:DarkBlue"),

                                          div(title='Select the type of data source (currently supported: in-memory data.frame, flat files or SQL connection)',
                                              selectInput(inputId="datasource_type",
                                                          label="Datasource type",
                                                          choices=c("already in memory", "load from file", "SQL database"),
                                                          selected="already in memory")),

                                          # Use in-memory dataset ----
                                          conditionalPanel(
                                            condition = "(input.datasource_type == 'already in memory')",

                                            # Obligatory stuff:
                                            div(title='Required: select the name of the dataset to use from those available in memory',
                                                selectInput(inputId="dataset_from_memory",
                                                            label="In-memory dataset",
                                                            choices=c("none"),
                                                            selected="none")),
                                            div(title="Click here to peek at the selected dataset...",
                                                actionButton("dataset_from_memory_peek_button", label="Peek at dataset...", icon=icon("eye-open", lib="glyphicon"))),

                                            hr(),

                                            div(title='Required: select the name of the column containing the patient IDs',
                                                selectInput(inputId="dataset_from_memory_patient_id",
                                                            label="Patient ID column",
                                                            choices=c("none"),
                                                            selected="none")),

                                            div(title='Required: give the date format.\nBasic codes are:\n  "%d" (day of the month as decimal number),\n  "%m" (month as decimal number),\n  "%b" (Month in abbreviated form),\n  "%B" (month full name),\n  "%y" (year in 2 digit format) and\n  "%Y" (year in 4 digit format).\nSome examples are %m/%d/%Y or %Y%m%d.\nPlease see help entry for "strptime()".',
                                                textInput(inputId="dataset_from_memory_event_format",
                                                          label="Date format",
                                                          value="%m/%d/%Y",
                                                          placeholder="%m/%d/%Y")),

                                            div(title='Required: select the name of the column containing the event dates (in the format defined above)',
                                                selectInput(inputId="dataset_from_memory_event_date",
                                                            label="Event date column",
                                                            choices=c("none"),
                                                            selected="none")),

                                            div(title='Required: select the name of the column containing the event duration (in days)',
                                                selectInput(inputId="dataset_from_memory_event_duration",
                                                            label="Event duration column",
                                                            choices=c("none"),
                                                            selected="none")),

                                            div(title='Optional (potentially used by CMA5+): select the name of the column containing the daily dose',
                                                selectInput(inputId="dataset_from_memory_daily_dose",
                                                            label="Daily dose column",
                                                            choices=c("[not defined]"),
                                                            selected="[not defined]")),

                                            div(title='Optional (potentially used by CMA5+): select the name of the column containing the treatment class',
                                                selectInput(inputId="dataset_from_memory_medication_class",
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


                                          # Allow last comma:
                                          NULL
                                )
                       )

    )),


    # OUTPUT PANEL ----
    #mainPanel(
    column(9,

      # Plot dimensions ----
      column(3,
        div(title='The width of the plotting area (in pixles)',
                 sliderInput(inputId="plot_width",
                    label="Plot width",
                    min=0, max=5000, value=500, step=20, round=TRUE))
      ),

      conditionalPanel(
        condition = "(!input.plot_keep_ratio)",
        column(3,
          div(title='The height of the plotting area (in pixels)',
                   sliderInput(inputId="plot_height",
                      label="height",
                      min=0, max=5000, value=300, step=20, round=TRUE))
        )
      ),
      conditionalPanel(
        condition = "(input.plot_keep_ratio)",
        column(3,
          p("")
        )
      ),

      column(2,
        div(title='Freeze the width/height ratio of the plotting area (or make the width and height independent of each other)?',
                 checkboxInput(inputId="plot_keep_ratio",
                    label="Keep ratio",
                    value=TRUE))#,

        #checkboxInput(inputId="plot_auto_size",
        #            label="auto size",
        #            value=TRUE)
      ),

      column(2,
        # Save image to file:
        div(title='Export this plot to an image file?',
                 checkboxInput(inputId="save_to_file_info",
                    label="Save plot!",
                    value=FALSE))
      ),

      column(2,
        # Close shop:
        div(title='Exit this Shiny plotting app? (The plot will NOT be automatically saved!)',
                 actionButton(inputId="close_shop", label=strong("Exit..."), icon=icon("remove-circle", lib="glyphicon"), style="color: #C70039 ; border-color: #C70039"))
      ),

      # Export to file ----
      column(12,
        conditionalPanel(
          condition="(input.save_to_file_info)",

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
                            selectInput(inputId="save_plot_dim_unit", label="unit", choices=c("in","cm","mm","px"), selected="in")))
          ),

          column(2,
                 div(title='The type of the exported image',
                          selectInput(inputId="save_plot_type", label="type", choices=c("jpg","png","tiff","eps","pdf"), selected="jpeg"))),

          #column(2,numericInput(inputId="save_plot_quality", label="quality", value=75, min=0, max=100, step=1)),
          column(2,
                 div(title='The resolution of the exported image (not useful for EPS and PDF)',
                          numericInput(inputId="save_plot_resolution", label="resolution", value=72, min=0))),

          column(2, style="margin-top: 25px;",
                 div(title='Export the plot now!',
                          downloadButton(outputId="save_to_file", label="Save plot")))
        )
      ),

      # Messages ----
      column(12,
        tags$head(tags$style("#container * { display: inline; }")),
        div(id="container", title="Various messages (in blue), warnings (in green) and errors (in red) generated during plotting...",
            span(" Messages:", style="color:DarkBlue; font-weight: bold;"),
            span(htmlOutput(outputId = "messages")),
            style="height: 2em; resize: none; overflow: auto")
      ),

      # The actual plot ----
      column(12, wellPanel(id = "tPlot",
                           style="resize: none; overflow:scroll; max-height: 75vh; max-width: 80vw",
                           plotOutput(outputId = "distPlot", inline=TRUE))),

      # The R code for the plot ----
      column(12,
        # Show the R code:
        div(title='Show the R code that would generate the current plot',
                 actionButton(inputId="show_r_code", label=strong("Show R code..."), icon=icon("eye-open", lib="glyphicon")))
      )

    )
  )
)


# The server logic ----
server <- function(input, output, session) {

  # Reactive value to allow UI updating on dataset changes:
  rv <- reactiveValues(toggle.me = FALSE);

  # The plotting function:
  .renderPlot <- function()
  {
    .GlobalEnv$.plotting.params$.plotting.fnc(data=.GlobalEnv$.plotting.params$data,
                                              ID.colname=.GlobalEnv$.plotting.params$ID.colname,
                                              event.date.colname=.GlobalEnv$.plotting.params$event.date.colname,
                                              event.duration.colname=.GlobalEnv$.plotting.params$event.duration.colname,
                                              event.daily.dose.colname=.GlobalEnv$.plotting.params$event.daily.dose.colname,
                                              medication.class.colname=.GlobalEnv$.plotting.params$medication.class.colname,
                                              date.format=.GlobalEnv$.plotting.params$date.format,

                                              ID=input$patient,
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
                                              sliding.window.no.steps=ifelse(input$sliding_window_step_choice == "number of steps" ,as.numeric(input$sliding_window_no_steps), NA),
                                              plot.CMA.as.histogram=ifelse(input$cma_class == "sliding window",
                                                                           !input$plot_CMA_as_histogram_sliding_window,
                                                                           !input$plot_CMA_as_histogram_episodes),
                                              align.all.patients=input$plot_align_all_patients,
                                              align.first.event.at.zero=input$plot_align_first_event_at_zero,
                                              show.legend=input$show_legend, legend.x=input$legend_x, legend.y=input$legend_y, legend.bkg.opacity=input$legend_bkg_opacity,
                                              legend.cex=input$legend_cex, legend.cex.title=input$legend_cex_title,
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
                                              cex=input$cex, cex.axis=input$cex_axis, cex.lab=input$cex_lab,
                                              highlight.followup.window=input$highlight_followup_window, followup.window.col=input$followup_window_col,
                                              highlight.observation.window=input$highlight_observation_window, observation.window.col=input$observation_window_col,
                                              observation.window.density=input$observation_window_density, observation.window.angle=input$observation_window_angle,
                                              observation.window.opacity=input$observation_window_opacity,
                                              show.real.obs.window.start=input$show_real_obs_window_start,
                                              real.obs.window.density=input$real_obs_window_density, real.obs.window.angle=input$real_obs_window_angle,
                                              print.CMA=input$print_cma, CMA.cex=input$cma_cex,
                                              plot.CMA=input$plot_cma, CMA.plot.ratio=input$cma_plot_ratio / 100.0,
                                              CMA.plot.col=input$cma_plot_col, CMA.plot.border=input$cma_plot_border, CMA.plot.bkg=input$cma_plot_bkg, CMA.plot.text=input$cma_plot_text,
                                              show.event.intervals=input$show_event_intervals,
                                              print.dose=input$print_dose,
                                              cex.dose=input$cex_dose,
                                              print.dose.outline.col=input$print_dose_outline_col,
                                              print.dose.centered=input$print_dose_centered,
                                              plot.dose=input$plot_dose,
                                              lwd.event.max.dose=input$lwd_event_max_dose,
                                              plot.dose.lwd.across.medication.classes=input$plot_dose_lwd_across_medication_classes,
                                              min.plot.size.in.characters.horiz=input$min_plot_size_in_characters_horiz,
                                              min.plot.size.in.characters.vert=input$min_plot_size_in_characters_vert,
                                              get.colnames.fnc=.GlobalEnv$.plotting.params$get.colnames.fnc,
                                              get.patients.fnc=.GlobalEnv$.plotting.params$get.patients.fnc,
                                              get.data.for.patients.fnc=.GlobalEnv$.plotting.params$get.data.for.patients.fnc
    )
  }

  # Do the ploting:
  output$distPlot <- renderPlot({

      rv$toggle.me; # make the plot care about forced updated to the UI (for example, when chainging the dataset)

      # Depeding on the CMA class we might do things differently:
      msgs <- ""; # the output messages
      res <- NULL; # the result of plotting
      if( input$cma_class %in% c("simple", "per episode", "sliding window") )
      {
        # Call the workhorse plotting function with the appropriate argumens:
        msgs <- capture.output(res <- .renderPlot());
      } else
      {
        # Quitting....
        showModal(modalDialog(title="AdhereR interactive plotting...", paste0("Unknwon CMA class '",input$cma_class,"'."), easyClose=TRUE));
      }

      if( is.null(res) || length(grep("error", msgs, ignore.case=TRUE)) > 0 )
      {
        # Errors:
        output$messages <- renderText({ paste0("<font color=\"red\"><b>",msgs,"</b></font>"); })
      } else if( length(grep("warning", msgs, ignore.case=TRUE)) > 0 )
      {
        # Warnings:
        output$messages <- renderText({ paste0("<font color=\"green\"><i>",msgs,"</i></font>"); })
      } else
      {
        # Normal output:
        output$messages <- renderText({ paste0("<font color=\"blue\">",msgs,"</font>"); })
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

  # Text messages:
  output$messages <- renderText({
    ""
  })

  observeEvent(input$plot_keep_ratio, # plot keep ratio toggle
  {
    if( input$plot_keep_ratio )
    {
      .GlobalEnv$.plotting.params$plot.ratio <- (input$plot_width / input$plot_height); # save the ratio
    } else
    {
      updateSliderInput(session, "plot_height", value = round(input$plot_width / .GlobalEnv$.plotting.params$plot.ratio));
    }
  })


  # Export plot to file:
  output$save_to_file <- downloadHandler(
    filename = function()
      {
        paste0("adherer-plot-",
               input$cma_class,"-",
               ifelse(input$cma_class=="simple", input$cma_to_compute, cma_to_compute_within_complex),
               "-",
               "ID-",input$patient,
               ".",
               input$save_plot_type)
      },
    content = function(file)
    {
      # The type of plot to save:
      if( input$save_plot_type == "png" )
      {
        png(file, height=input$save_plot_width, width=input$save_plot_height, units=input$save_plot_dim_unit, res=input$save_plot_resolution, type="cairo");
      } else if( input$save_plot_type == "tiff" )
      {
        tiff(file, height=input$save_plot_width, width=input$save_plot_height, units=input$save_plot_dim_unit, res=input$save_plot_resolution, compression="zip", type="cairo");
      } else if( input$save_plot_type == "eps" )
      {
        cairo_ps(file, height=input$save_plot_width, width=input$save_plot_height, onefile=FALSE);
      } else if( input$save_plot_type == "pdf" )
      {
        cairo_pdf(file, height=input$save_plot_width, width=input$save_plot_height, onefile=FALSE);
      } else # default to JPEG
      {
        jpeg(file, height=input$save_plot_width, width=input$save_plot_height, units=input$save_plot_dim_unit, res=input$save_plot_resolution);
      }

      # Plot it:
      .renderPlot();

      # Close the device:
      dev.off();
    }
  )


  # About and Help:
  observeEvent(input$about_button,
  {
    # Get most of the relevant info from the DESCRIPTION file:
    descr <- utils::packageDescription("AdhereR.devel");
    msg <- paste0("<img src='adherer-logo.png', align = 'left', style='font-size: x-large; font-weight: bold; height: 2em; vertical-align: baseline;'/>",
                  "<div style='width: 1em; display: inline-block;'/>",
                  "<hr/>",
                  "<div style='max-height: 50vh; overflow: auto;'>",
                  "<p><b>Version</b> ",descr$Version,"</p>",
                  "<p><b>Authors:</b> ",descr$Author,"</p>",
                  "<p><b>Maintainer:</b> ",descr$Maintainer,"</p>",
                  "<p align='justify'>",descr$Description,"</p>",
                  "<p><b>Website:</b> <a href='",descr$URL,"'>",descr$URL,"</a></p>",
                  "<p><b>Released under:</b> ",descr$License,"</p>",
                  "<p><b>Citation:</b></p>",format(citation(package="AdhereR.devel"),style="html"),
                  "<hr/>",
                  "<p>For more info <b>online</b> please visit the project's <a href='http://www.adherer.eu'>homepage</a> (<a href='http://www.adherer.eu'>www.adherer.eu</a>) and its source code repository on <a href='https://github.com/ddediu/AdhereR'>GitHub</a> (<a href='https://github.com/ddediu/AdhereR'>github.com/ddediu/AdhereR</a>). ",
                  "The official releases are hosted on <a href='https://cran.r-project.org/package=AdhereR'>CRAN</a> (<a href='https://cran.r-project.org/package=AdhereR'>https://cran.r-project.org/package=AdhereR</a>).",
                  "<p><b>Offline</b> help is available within R (and RStudio):</p>",
                  "<ul>",
                  "<li>running <code>help(package='AdhereR')</code> in the R cosole displayes the <i>main documentation</i> for the package with links to detailed help for particular topics;</li>",
                  "<li>running <code>help('CMA0')</code> (or the equivalent <code>?CMA0</code>) in the R cosole displayes the <i>detailed documentation</i> the particular topic (here, CMA0); in RStudio, selecting the keyword ('CMA0') in the script editor and pressing <code>F1</code> has the same effect. Please note that to obtain help for <i>overloaded</i> functions (such as <code>plot</code>) for, say, sliding windows, one must use the fully qualified function name (here, <code>?plot.CMA_sliding_window</code>);</li>",
                  "<li>the various <i>vignettes</i> contain a lot of information about selected topics. To list all available vignettes for AdhereR, run <code>browseVignettes(package='AdhereR')</code> in the R console. Currently, the main vignettes concern:</li>",
                  "<ul>",
                  "<li><i><a href='https://cran.r-project.org/web/packages/AdhereR/vignettes/AdhereR-overview.html'>AdhereR: Adherence to Medications</a></i> gives an overview of what AdhereR can do;</li>",
                  "<li><i><a href='https://cran.r-project.org/web/packages/AdhereR/vignettes/calling-AdhereR-from-python3.html'>Calling AdhereR from Python 3</a></i> described a mechanism that allows AdhereR to be used from other programming languages and platofrms than R (in particular, from Python 3);</li>",
                  "<li><i><a href='https://cran.r-project.org/web/packages/AdhereR/vignettes/adherer_with_databases.pdf'>Using AdhereR with various database technologies for processing very large datasets</a></i> described how to use AdhereR to process data stored in 'classic' SQL Relational Databases Management Systems (RDBMSs) or in Apache's Hadoop;</li>",
                  "<li><i><a href='https://cran.r-project.org/web/packages/AdhereR/vignettes/adherer_interctive_plots.html'>Interactive plotting with Shiny</a></i> is probably the most relevant here.</li>",
                  "</ul>",
                  "</ul>",
                  "</div>");

    tryCatch(showModal(modalDialog(HTML(msg),
                                   title=NULL,
                                   footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon"))))),
             error = function(e) showModal(modalDialog(title="AdhereR error!",
                                                       "Cannot display the About message!",
                                                       footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))))
    );
  })


  # Show r code:
  observeEvent(input$show_r_code,
  {
    if( is.null(input$patient) || length(input$patient) < 1 )
    {
      showModal(modalDialog(title="AdhereR error!",
                            "No patients selected, so nothing to do!",
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return;
    }

    # Create the R code:
    r_code <<- "";

    # Initial comments:
    r_code <<- paste0(r_code, "# The R code corresponding to the currently displayed Shiny plot:\n\n");

    # The data and selected patients:
    r_code <<- paste0(r_code, "# Extract the data for the selected ", length(input$patient), " patient(s) with ID(s):\n");
    r_code <<- paste0(r_code, "# ",paste0('"',input$patient,'"',collapse=", "),"\n\n");

    r_code <<- paste0(r_code, "# We denote here by DATA the data you are using in the Shiny plot.\n");
    r_code <<- paste0(r_code, "# For reasons to do with how R works, we cannot display the name\n");
    r_code <<- paste0(r_code, "# you used for it (if any), but we can tell you that it is of type\n");
    r_code <<- paste0(r_code, "# \"", class(.GlobalEnv$.plotting.params$data), "\", and it has the structure:\n");
    r_code <<- paste0(r_code, paste0("#   ",capture.output(str(.GlobalEnv$.plotting.params$data, vec.len=3, width=60)),collapse="\n"),"\n\n");

    r_code <<- paste0(r_code, "# To allow using data from other sources than a \"data.frame\"\n");
    r_code <<- paste0(r_code, "# and other similar structures (for example, from a remote SQL\n");
    r_code <<- paste0(r_code, "# database), we use a metchanism to request the data for the\n");
    r_code <<- paste0(r_code, "# selected patients that uses a function called\n");
    r_code <<- paste0(r_code, "# \"get.data.for.patients.fnc()\" which you may have redefined\n");
    r_code <<- paste0(r_code, "# to better suit your case (chances are, however, that you are\n");
    r_code <<- paste0(r_code, "# using its default version); in any case, the following is its\n");
    r_code <<- paste0(r_code, "# definition:\n");
    fnc.code <- capture.output(print(.GlobalEnv$.plotting.params$get.data.for.patients.fnc));
    if( is.null(fnc.code) || length(fnc.code) == 0 )
    {
      showModal(modalDialog(title="AdhereR error!",
                            "Cannot display the R code for plot!",
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return;
    }
    if( length(grep("<environment", fnc.code[length(fnc.code)], fixed=TRUE)) == 1 ){ fnc.code <- fnc.code[-length(fnc.code)]; }
    if( length(grep("<bytecode", fnc.code[length(fnc.code)], fixed=TRUE)) == 1 ){ fnc.code <- fnc.code[-length(fnc.code)]; }
    if( is.null(fnc.code) || length(fnc.code) == 0 )
    {
      showModal(modalDialog(title="AdhereR error!",
                            "Cannot display the R code for plot!",
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return;
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
    if( input$cma_class != "simple" ) r_code <<- paste0(r_code, cma_fnc_body_indent, " ", "CMA=",input$cma_to_compute_within_complex,",\n");
    r_code <<- paste0(r_code, cma_fnc_body_indent, " # (please note that even if some parameters are\n");
    r_code <<- paste0(r_code, cma_fnc_body_indent, " # not relevant for a particular CMA type, we\n");
    r_code <<- paste0(r_code, cma_fnc_body_indent, " # nevertheless pass them as they will be ignored)\n");
    params.cma <- list("all"=c("ID.colname"=paste0('"',.GlobalEnv$.plotting.params$ID.colname,'"'),
                               "event.date.colname"=paste0('"',.GlobalEnv$.plotting.params$event.date.colname,'"'),
                               "event.duration.colname"=paste0('"',.GlobalEnv$.plotting.params$event.duration.colname,'"'),
                               "event.daily.dose.colname"=paste0('"',.GlobalEnv$.plotting.params$event.daily.dose.colname,'"'),
                               "medication.class.colname"=paste0('"',.GlobalEnv$.plotting.params$medication.class.colname,'"'),
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
                                       "maximum.permissible.gap.unit"=paste0('"',input$maximum_permissible_gap_unit,'"')),
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
                     "legend.cex"=input$legend_cex,
                     "legend.cex.title"=input$legend_cex_title,
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
                     "cex"=input$cex,
                     "cex.axis"=input$cex_axis,
                     "cex.lab"=input$cex_lab,
                     "highlight.followup.window"=input$highlight_followup_window,
                     "followup.window.col"=paste0('"',input$followup_window_col,'"'),
                     "highlight.observation.window"=input$highlight_observation_window,
                     "observation.window.col"=paste0('"',input$observation_window_col,'"'),
                     "observation.window.density"=input$observation_window_density,
                     "observation.window.angle"=input$observation_window_angle,
                     "observation.window.opacity"=input$observation_window_opacity,
                     "show.real.obs.window.start"=input$show_real_obs_window_start,
                     "real.obs.window.density"=input$real_obs_window_density,
                     "real.obs.window.angle"=input$real_obs_window_angle,
                     "print.CMA"=input$print_cma,
                     "CMA.cex"=input$cma_cex,
                     "plot.CMA"=input$plot_cma,
                     "CMA.plot.ratio"=input$cma_plot_ratio / 100.0,
                     "CMA.plot.col"=paste0('"',input$cma_plot_col,'"'),
                     "CMA.plot.border"=paste0('"',input$cma_plot_border,'"'),
                     "CMA.plot.bkg"=paste0('"',input$cma_plot_bkg,'"'),
                     "CMA.plot.text"=paste0('"',input$cma_plot_text,'"'),
                     "plot.CMA.as.histogram"=ifelse(input$cma_class=="sliding window", !input$plot_CMA_as_histogram_sliding_window, !input$plot_CMA_as_histogram_episodes),
                     "show.event.intervals"=input$show_event_intervals,
                     "print.dose"=input$print_dose,
                     "cex.dose"=input$cex.dose,
                     "print.dose.outline.col"=paste0('"',input$print_dose_outline_col,'"'),
                     "print.dose.centered"=input$print_dose_centered,
                     "plot.dose"=input$plot_dose,
                     "lwd.event.max.dose"=input$lwd_event_max_dose,
                     "plot.dose.lwd.across.medication.classes"=input$plot_dose_lwd_across_medication_classes,
                     "min.plot.size.in.characters.horiz"=input$min_plot_size_in_characters_horiz,
                     "min.plot.size.in.characters.vert"=input$min_plot_size_in_characters_vert);
    r_code <<- paste0(r_code, paste0("         ", names(params.plot), "=", params.plot, collapse=",\n"), "\n");
    r_code <<- paste0(r_code, "    );\n");
    r_code <<- paste0(r_code, "}\n");

    ## DEBUG:
    #cat(r_code);

    tryCatch(showModal(modalDialog(div(div(HTML("<p>This is the <code>R</code> that would generate the plot currently seen. You can copy it to the clipboard using the <i>Copy to clipboard</i> button.</p>
                                                <p>Please note that the parameter value <b><code>DATA</code></b> <i>must be replaced</i> by the actual data you passed to the Shiny interactive plot function!</p>")),
                                       div(HTML(gsub('<span class="symbol">DATA</span>','<span class="symbol_data">DATA</span>',
                                                     highlight::highlight(parse.output=parse(text=r_code),
                                                                          renderer=highlight::renderer_html(document=TRUE,
                                                                                                            stylesheet=file.path(system.file('interactivePlotShiny',
                                                                                                                                             package='AdhereR.devel'),
                                                                                                                                 "r-code-highlight.css")),
                                                                          show_line_numbers=FALSE,
                                                                          output=NULL),
                                                     fixed=TRUE)),
                                       #div(HTML(highlight::external_highlight(code=r_code, theme="acid", lang="r", type="HTML", doc=TRUE, file=NULL, outfile=NULL)),
                                           style="max-height: 50vh; overflow: auto;")),
                                   title=HTML("<code>R</code> code for the current plot"),
                                   footer = tagList(actionButton("copy_code", "Copy to clipboard", icon=icon("copy", lib="glyphicon")),
                                                    modalButton("Close", icon=icon("ok", lib="glyphicon"))))),
             error = function(e) showModal(modalDialog(title="AdhereR error!",
                                                       "Cannot display the R code for plot!",
                                                       footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))))
    );
  })
  observeEvent(input$copy_code,
  {
    if( clipr::clipr_available() ) clipr::write_clip(r_code, object_type="character");
  })


  # Close shop nicely:
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

  # Show/hide panel sections:
  .toggle.all.sections <- function(id=c("follow_up"), anim=TRUE, animType=c("slide","fade")[1])
  {
    shinyjs::toggle(id=paste0(id,"_unfold_icon"), anim=anim, animType=animType); # the unfolding icon
    shinyjs::toggle(id=paste0(id,"_contents"),    anim=anim, animType=animType); # the section content
  }
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


  # Dataset stuff:

  .recursively.list.objects.in.memory <- function(..., # inspired from http://adv-r.had.co.nz/Environments.html#env-recursion
                                                  env = parent.frame(),
                                                  of.class="data.frame", # if NULL, no type testing (all go)
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
          return( (!consider.derived.classes && (of.class %in% class(x))) || (consider.derived.classes && inherits(x, of.class)) );
        }, logical(1));
        all.objects <- all.objects[ objects.to.keep ];
      }

      # Recursive processing:
      all.objects <- c(all.objects,
                       .recursively.list.objects.in.memory(..., env = parent.env(env), of.class=of.class, consider.derived.classes=consider.derived.classes));
      # Return the list of objects:
      return (all.objects);
    }
  }

  # If selecting an in-memory dataset, update the list of data.frame-derived objects all the way to the base environment:
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
        x <- c("<<'data' argument to plot_interactive_cma() call>>", x);
      }
      updateSelectInput(session, "dataset_from_memory", choices=x, selected=head(x,1));
    }
  })

  # For a given dataset from memory, list the columns and upate the selections:
  observeEvent(input$dataset_from_memory,
  {
    # Set the data.frame:
    .GlobalEnv$.plotting.params$.inmemory.dataset <- NULL;
    if( input$dataset_from_memory == "none" )
    {
      # Initialisation:
      return;
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
    if( (input$dataset_from_memory != "none") &&
        (is.null(.GlobalEnv$.plotting.params$.inmemory.dataset) ||
         !inherits(.GlobalEnv$.plotting.params$.inmemory.dataset, "data.frame") ||
         ncol(.GlobalEnv$.plotting.params$.inmemory.dataset) < 1 ||
         nrow(.GlobalEnv$.plotting.params$.inmemory.dataset) < 1 ))
    {
      showModal(modalDialog(title="AdhereR error!",
                            paste0("Cannot load the selected dataset '",input$dataset_from_memory, "' from memory!"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return;
    }

    if( (input$dataset_from_memory != "none") && nrow(.GlobalEnv$.plotting.params$.inmemory.dataset) < 3 )
    {
      showModal(modalDialog(title="AdhereR error!",
                            paste0("Dataset '",input$dataset_from_memory, "' must have at least three distinct columns (patient ID, event date and duration)!"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return;
    }

    x <- names(.GlobalEnv$.plotting.params$.inmemory.dataset);

    # Required columns:
    updateSelectInput(session, "dataset_from_memory_patient_id",     choices=x, selected=head(x,1));
    updateSelectInput(session, "dataset_from_memory_event_date",     choices=x, selected=head(x,2));
    updateSelectInput(session, "dataset_from_memory_event_duration", choices=x, selected=head(x,3));

    # Optional columns (possibly used by CMA5+):
    updateSelectInput(session, "dataset_from_memory_medication_class", choices=c("[not defined]", x), selected="[not defined]");
    updateSelectInput(session, "dataset_from_memory_daily_dose",       choices=c("[not defined]", x), selected="[not defined]");
  })

  # Display a data.frame as a nice HTML table:
  .show.data.frame.as.HTML <- function(d, # the data.frame-derived object to show
                                       max.rows=50, # if NA, show all
                                       escape=TRUE)
  {
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

  # Peek at in-memory dataset:
  observeEvent(input$dataset_from_memory_peek_button,
  {
    # Sanity checks:
    if( is.null(.GlobalEnv$.plotting.params$.inmemory.dataset) ||
        !inherits(.GlobalEnv$.plotting.params$.inmemory.dataset, "data.frame") ||
        ncol(.GlobalEnv$.plotting.params$.inmemory.dataset) < 1 ||
        nrow(.GlobalEnv$.plotting.params$.inmemory.dataset) < 3 )
    {
      showModal(modalDialog(title="AdhereR error!",
                            paste0("Cannot load the selected dataset '",input$dataset_from_memory, "' from memory!\nPlease make sure you selected a valid data.frame (or derived object) with at least 3 columns and 1 row..."),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return;
    }
    # Use shorter name:
    d <- .GlobalEnv$.plotting.params$.inmemory.dataset;
    showModal(modalDialog(title="AdhereR: peeking at the selected in-memory dataset ...",
                          div(HTML(.show.data.frame.as.HTML(d)),
                                   style="max-height: 50vh; max-width: 90vw; overflow: auto; overflow-x:auto;"),
                          footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));

  })

  # Validate and use in-memory dataset:
  observeEvent(input$dataset_from_memory_button_use,
  {
    # The appropriate getters:
    .get.colnames.fnc <- function(d) names(d);
    .patients.fnc <- function(d, idcol) unique(d[[idcol]]);
    .get.data.for.patients.fnc=function(patientid, d, idcol) d[ d[[idcol]] %in% patientid, ];

    # Sanity checks:
    if( is.null(.GlobalEnv$.plotting.params$.inmemory.dataset) ||
        !inherits(.GlobalEnv$.plotting.params$.inmemory.dataset, "data.frame") ||
        ncol(.GlobalEnv$.plotting.params$.inmemory.dataset) < 1 ||
        nrow(.GlobalEnv$.plotting.params$.inmemory.dataset) < 3 )
    {
      showModal(modalDialog(title="AdhereR error!",
                            paste0("Cannot load the selected dataset '",input$dataset_from_memory, "' from memory!\nPlease make sure you selected a valid data.frame (or derived object) with at least 3 columns and 1 row..."),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return;
    }
    # Use shorter name:
    d <- .GlobalEnv$.plotting.params$.inmemory.dataset;

    # Check if the column names refer to existing columns in the dataset:
    ID.colname <- input$dataset_from_memory_patient_id;
    if( is.na(ID.colname) || length(ID.colname) != 1 || ID.colname=="" || !(ID.colname %in% .get.colnames.fnc(d)) )
    {
      showModal(modalDialog(title="AdhereR error!",
                            paste0("Patient ID column '",ID.colname, "' must be a string and a valid column name in the selected dataset..."),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return;
    }

    event.date.colname <- input$dataset_from_memory_event_date;
    if( is.na(event.date.colname) || length(event.date.colname) != 1 || event.date.colname=="" || !(event.date.colname %in% .get.colnames.fnc(d)) )
    {
      showModal(modalDialog(title="AdhereR error!",
                            paste0("Event date column '",event.date.colname, "' must be a string and a valid column name in the selected dataset..."),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return;
    }

    date.format <- input$dataset_from_memory_event_format;

    event.duration.colname <- input$dataset_from_memory_event_duration;
    if( is.na(event.duration.colname) || length(event.duration.colname) != 1 || event.duration.colname=="" || !(event.duration.colname %in% .get.colnames.fnc(d)) )
    {
      showModal(modalDialog(title="AdhereR error!",
                            paste0("Event duration column '",event.duration.colname, "' must be a string and a valid column name in the selected dataset..."),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return;
    }

    event.daily.dose.colname <- input$dataset_from_memory_daily_dose;
    if( is.na(event.daily.dose.colname) || length(event.daily.dose.colname) != 1 || event.daily.dose.colname=="" )
    {
      showModal(modalDialog(title="AdhereR error!",
                            paste0("Event duration column '",event.daily.dose.colname, "' must be a non-empty string..."),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return;
    } else if( event.daily.dose.colname == "[not defined]" )
    {
      event.daily.dose.colname <- NA; # not defined
    } else if( !(event.daily.dose.colname %in% .get.colnames.fnc(d)) )
    {
      showModal(modalDialog(title="AdhereR error!",
                            paste0("Event duration column '",event.daily.dose.colname, "' if given, must be either '[not defined]' or a valid column name in the selected dataset..."),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return;
    }

    medication.class.colname <- input$dataset_from_memory_medication_class;
    if( is.na(medication.class.colname) || length(medication.class.colname) != 1 || medication.class.colname=="" )
    {
      showModal(modalDialog(title="AdhereR error!",
                            paste0("Treatment class column '",medication.class.colname, "' must be a non-empty string..."),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return;
    } else if( medication.class.colname == "[not defined]" )
    {
      medication.class.colname <- NA; # not defined
    } else if( !(medication.class.colname %in% .get.colnames.fnc(d)) )
    {
      showModal(modalDialog(title="AdhereR error!",
                            paste0("Treatment class column '",medication.class.colname, "' if given, must be either '[not defined]' or a valid column name in the selected dataset..."),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return;
    }

    # Check if the column names are unique (i.e., do not repeat):
    if( anyDuplicated(na.omit(c(ID.colname, event.date.colname, event.duration.colname, event.daily.dose.colname, medication.class.colname))) )
    {
      showModal(modalDialog(title="AdhereR error!",
                            paste0("The selected column names must be unique!"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return;
    }

    # More advanced checks of the column types:
    if( inherits(d[,event.date.colname], "Date") )
    {
      # It's a column of Dates: perfect!
    } else if( is.factor(d[,event.date.colname]) || is.character(d[,event.date.colname]) )
    {
      # It's a factor or string: check if it conforms to the given date.format:
      s <- na.omit(as.character(d[,event.date.colname]));
      if( length(s) == 0 )
      {
        showModal(modalDialog(title="AdhereR error!",
                              paste0("There are no non-missing dates in the '",event.date.colname,"' column!"),
                              footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        return;
      }
      tmp <- as.Date(s, format=input$dataset_from_memory_event_format);
      if( all(is.na(tmp)) )
      {
        showModal(modalDialog(title="AdhereR error!",
                              paste0("Please check if the date format is correct and fits the actual dates in the '",event.date.colname,"' column: all conversions failed!"),
                              footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        return;
      } else if( any(is.na(tmp)) )
      {
        showModal(modalDialog(title="AdhereR error!",
                              paste0("Please check if the date format is correct and fits the actual dates in the '",event.date.colname,"' column: ", length(is.na(tmp))," conversions failed!"),
                              footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
        return;
      }
    } else
    {
      showModal(modalDialog(title="AdhereR error!",
                            paste0("The event date column '",event.date.colname,"' must contain either objects of class 'Date' or correctly-formatted strings (or factor levels)!"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return;
    }

    if( !is.na(event.duration.colname) && (!is.numeric(d[,event.duration.colname]) || any(d[,event.duration.colname] < 0, na.rm=TRUE)) )
    {
      showModal(modalDialog(title="AdhereR error!",
                            paste0("If given, the event duration column '",event.duration.colname,"' must contain non-negative numbers!"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return;
    }

    if( !is.na(event.daily.dose.colname) && (!is.numeric(d[,event.daily.dose.colname]) || any(d[,event.daily.dose.colname] < 0, na.rm=TRUE)) )
    {
      showModal(modalDialog(title="AdhereR error!",
                            paste0("If given, the daily dose column '",event.daily.dose.colname,"' must contain non-negative numbers!"),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return;
    }

    # Even more complex check: try to compute CMA0 on the first patient:
    test.cma <- NULL;
    test.res <- tryCatch(test.cma <- CMA0(data=.get.data.for.patients.fnc(.patients.fnc(d, ID.colname)[1], d, ID.colname),
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
      showModal(modalDialog(title="AdhereR error!",
                            paste0("There's something wrong with these data!\nI tried to create a CMA0 object and this is what I got back:\n", as.character(test.res)),
                            footer = tagList(modalButton("Close", icon=icon("ok", lib="glyphicon")))));
      return;
    } else
    {
      if( inherits(test.res, "warning") )
      {
        showModal(modalDialog(title="AdhereR error!",
                              paste0("These data seem ok, but when I tried to create a CMA0 object I got some warnings:\n", as.character(test.res)),
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
    .GlobalEnv$.plotting.params$get.colnames.fnc <- function(d) names(d);
    .GlobalEnv$.plotting.params$get.patients.fnc <- function(d, idcol) unique(d[[idcol]]);
    .GlobalEnv$.plotting.params$get.data.for.patients.fnc=function(patientid, d, idcol) d[ d[[idcol]] %in% patientid, ];
    # CMA class:
    .GlobalEnv$.plotting.params$cma.class <- "CMA0";
    # Patient IDs and selected ID:
    all.IDs <- .GlobalEnv$.plotting.params$get.patients.fnc(d, ID.colname);
    .GlobalEnv$.plotting.params$all.IDs <- all.IDs;
    .GlobalEnv$.plotting.params$ID <- all.IDs[1];

    # Force UI updating...
    .force.update.UI();
  })

  # Force updating the Shiny UI using the new data:
  .force.update.UI <- function()
  {
    updateSelectInput(session, "cma_class", selected="simple");
    updateSelectInput(session, "cma_to_compute", selected=.GlobalEnv$.plotting.params$cma.class);
    updateSelectInput(session, "patient", choices=.GlobalEnv$.plotting.params$all.IDs, selected=.GlobalEnv$.plotting.params$ID);

    rv$toggle.me <- !rv$toggle.me; # make the plotting aware of a change (even if we did not send any UI elements)
  }
}


# call shiny
shinyApp(ui = ui, server = server)

