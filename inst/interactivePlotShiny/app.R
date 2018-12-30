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
NULL

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  #titlePanel(windowTitle="AdhereR: Interactive plotting using Shiny..."),
  list(tags$head(HTML('<link rel="icon", href="adherer-logo.png", type="image/png" />'))),
  div(style="padding: 1px 0px; width: '100%'", titlePanel(title="", windowTitle="AdhereR: Shiny plot...")),

  # Sidebar layout with input and output definitions ----
  #sidebarLayout(
  fluidRow(

    # Title & help
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

    # Sidebar panel for inputs ----
    #sidebarPanel(
    column(3, wellPanel(id = "tPanel", style = "overflow:scroll; max-height: 90vh;",


      div(title='General setting that apply to all kinds of plots', # trick for adding tooltips: create a container div with the title the desired tootltip text...
               span(id = "general_settings", h4("General settings..."), style="color:DarkBlue")),

      # Select the CMA class ----
      div(title='Select the type of CMA to plot: "simple", "per eipsode" or "sliding window"',
               selectInput(inputId="cma_class",
                  label="Select CMA type",
                  choices=c("simple", "per episode", "sliding window"),
                  selected=.plotting.params$cma.class)),

      hr(),

      # Select the simple CMA to compute ----
      conditionalPanel(
        condition = "(input.cma_class == 'simple')",
        div(title='The "simple" CMA to compute by itself',
                 selectInput(inputId="cma_to_compute",
                    label="Select CMA to compute",
                    choices=paste0("CMA",0:9),
                    selected="CMA0"))
      ),
      conditionalPanel(
        condition = "(input.cma_class != 'simple')",
        div(title='The "simple" CMA to compute for each episode/sliding window',
                 selectInput(inputId="cma_to_compute_within_complex",
                    label="Select CMA to compute",
                    choices=paste0("CMA",1:9),
                    selected="CMA1"))
      ),

      # Select the patient to plot ----
      div(title='Select one (or more, by repeatedly selecting) patient(s) to plot',
               selectInput(inputId="patient",
                  label="Select patient(s) to plot",
                  choices=.plotting.params$all.IDs,
                  selected=.plotting.params$ID,
                  multiple=TRUE)),

      hr(),
      div(title='Define the follow-up window',
               span(id="followup_window", h4("Follow-up window..."), style="color:DarkBlue")),

      # Follow-up window start ----
      div(title='The unit of the start of the follow-up window (can be "days", "weeks", "months", "years" or an actual "calendar date")',
               selectInput(inputId="followup_window_start_unit",
                  label="Follow-up wnd. start unit",
                  choices=c("days", "weeks", "months", "years", "calendar date"), # "column in dataset"),
                  selected="days")),

      # If follow-up window unit is "calendar date" ----
      conditionalPanel(
        condition = "(input.followup_window_start_unit == 'calendar date')",
                    # Select an actual date ----
                    div(title='Select the actual start date of the follow-up window (possibly using a calendar widget)',
                             dateInput(inputId="followup_window_start_date",
                              label="Follow-up wnd. start",
                              value=NULL, format="dd/mm/yyyy", startview="month", weekstart=1))
      ),

      ## If follow-up window unit is "column in dataset" ----
      #conditionalPanel(
      #  condition = "(input.followup_window_start_unit == 'column in dataset')",
      #              # Select an actual date ----
      #              selectInput(inputId="followup_window_start_column",
      #                        label="Follow-up wnd. start",
      #                        choices=names(.plotting.params$data),
      #                        selected="")
      #),

      # If follow-up window unit is regular unit ----
      conditionalPanel(
        condition = "(input.followup_window_start_unit != 'calendar date')",  # && input.followup_window_start_unit != 'column in dataset')",
                    # Select the number of units ----
                    div(title='Select the number of units defining the start of the follow-up window',
                             sliderInput(inputId="followup_window_start_no_units",
                                label="Follow-up wnd. start",
                                min=0, max=.plotting.params$followup.window.start.max, value=0, step=1, round=TRUE))
      ),


      # Follow-up window duration ----
      div(title='The unit of the duration of the follow-up window (can be "days", "weeks", "months" or "years")',
               selectInput(inputId="followup_window_duration_unit",
                  label="Follow-up wnd. duration unit",
                  choices=c("days", "weeks", "months", "years"),
                  selected="days")),

      # Select the number of units ----
      div(title='Select the number of units defining the duration of the follow-up window',
               sliderInput(inputId="followup_window_duration",
                  label="Follow-up wnd. duration",
                  min=0, max=.plotting.params$followup.window.duration.max, value=2*365, step=1, round=TRUE)),

      hr(),
      div(title='Define the observation window',
               span(id="observation_window", h4("Observation window..."), style="color:DarkBlue")),

      # Observation window start ----
      div(title='The unit of the start of the observation window (can be "days", "weeks", "months", "years" or an actual "calendar date")',
               selectInput(inputId="observation_window_start_unit",
                  label="Observation wnd. start unit",
                  choices=c("days", "weeks", "months", "years", "calendar date"),
                  selected="days")),

      # If observation window unit is "calendar date" ----
      conditionalPanel(
        condition = "(input.observation_window_start_unit == 'calendar date')",
                    # Select an actual date ----
                    div(title='Select the actual start date of the observation window (possibly using a calendar widget)',
                             dateInput(inputId="observation_window_start_date",
                              label="Observation wnd. start",
                              value=NULL, format="dd/mm/yyyy", startview="month", weekstart=1))
      ),

      # If observation window unit is not "calendar date" ----
      conditionalPanel(
        condition = "(input.observation_window_start_unit != 'calendar date')",
                    # Select the number of units ----
                    div(title='Select the number of units defining the start of the observation window',
                             sliderInput(inputId="observation_window_start_no_units",
                                label="Observation wnd. start",
                                min=0, max=.plotting.params$observation.window.start.max, value=0, step=1, round=TRUE))
      ),


      # Observation window duration ----
      div(title='The unit of the duration of the observation window (can be "days", "weeks", "months" or "years")',
               selectInput(inputId="observation_window_duration_unit",
                  label="Observation wnd. duration unit",
                  choices=c("days", "weeks", "months", "years"),
                  selected="days")),

      # Select the number of units ----
      div(title='Select the number of units defining the duration of the observation window',
               sliderInput(inputId="observation_window_duration",
                  label="Observation wnd. duration",
                  min=0, max=.plotting.params$observation.window.duration.max, value=2*365, step=1, round=TRUE)),

      hr(),


      # For CMA5 to CMA9: carry_only_for_same_medication, consider_dosage_change ----
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

                    div(title='What type of carry over to consider?',
                             span(h4("Carry over..."), style="color:DarkBlue")),

                    # Carry-over for same treat only? ----
                    div(title='Carry over only across treatments of the same type?',
                             checkboxInput(inputId="carry_only_for_same_medication",
                                  label="Carry over for same treat only?",
                                  value=FALSE)),

                    # Consider dosage changes? ----
                    div(title='Consider dosage change when computing the carry over?',
                             checkboxInput(inputId="consider_dosage_change",
                                  label="Consider dosage changes?",
                                  value=FALSE)),

                    hr()
      ),


      # For per episode:
      conditionalPanel(
        condition = "(input.cma_class == 'per episode')",

                    div(title='Parameters defining treatment episodes',
                             span(h4("Define the episodes..."), style="color:DarkBlue")),

                    # Does treat. change start new episode? ----
                    div(title='Does changing the treatment type trigger a new episode?',
                             checkboxInput(inputId="medication_change_means_new_treatment_episode",
                                  label="Treat. change start new episode?",
                                  value=FALSE)),

                    # Does dosage change start new episode? ----
                    div(title='Does changing the dose trigger a new episode?',
                             checkboxInput(inputId="dosage_change_means_new_treatment_episode",
                                  label="Dosage change start new episode?",
                                  value=FALSE)),

                    # Max. permis. gap duration unit ----
                    div(title='The unit of the maximum permissible gap after which a new episode is triggered: either absolute ("days", "weeks", "months" or "years") or relative ("percent")',
                             selectInput(inputId="maximum_permissible_gap_unit",
                                label="Max. permis. gap duration unit",
                                choices=c("days", "weeks", "months", "years", "percent"),
                                selected="days")),

                    # Max. permissible gap ----
                    div(title='The maximum permissible gap after which a new episode is triggered (in the above-selected units)',
                             sliderInput(inputId="maximum_permissible_gap",
                                label="Max. permissible gap",
                                min=0, max=.plotting.params$followup.window.duration.max, value=0, step=1, round=TRUE)),

                    # Plot CMA as histogram ----
                    div(title='Show the distribution of estimated CMAs across episodes as a histogram or barplot?',
                             checkboxInput(inputId="plot_CMA_as_histogram_episodes",
                                  label="Plot CMA as histogram?",
                                  value=FALSE)),

                    hr()
      ),


      # For sliding window:
      conditionalPanel(
        condition = "(input.cma_class == 'sliding window')",

                    div(title='Parameters defining the sliding windows',
                             span(h4("Define the sliding windows..."), style="color:DarkBlue")),

                    # Sliding window start ----
                    div(title='The unit of the start of the sliding windows ("days", "weeks", "months" or "years")',
                             selectInput(inputId="sliding_window_start_unit",
                                label="Sliding wnd. start unit",
                                choices=c("days", "weeks", "months", "years"),
                                selected="days")),

                    # Select the number of units ----
                    div(title='Select the number of units defining the start of the sliding windows',
                             sliderInput(inputId="sliding_window_start",
                                label="Sliding wnd. start",
                                min=0, max=.plotting.params$sliding.window.start.max, value=0, step=1, round=TRUE)),

                    # Sliding window duration ----
                    div(title='The unit of the duration of the sliding windows ("days", "weeks", "months" or "years")',
                             selectInput(inputId="sliding_window_duration_unit",
                                label="Sliding wnd. duration unit",
                                choices=c("days", "weeks", "months", "years"),
                                selected="days")),

                    # Select the number of units ----
                    div(title='Select the number of units defining the duration of the sliding windows',
                             sliderInput(inputId="sliding_window_duration",
                                label="Sliding wnd. duration",
                                min=0, max=.plotting.params$sliding.window.duration.max, value=90, step=1, round=TRUE)),

                    # Steps choice ----
                    div(title='How is the step of the sliding windows defined: by giving their number or their duration?',
                             selectInput(inputId="sliding_window_step_choice",
                                label="Define the sliding wnd. steps by",
                                choices=c("the number of steps", "the duration of a step"),
                                selected="the duration of a step")),

                    # Sliding window steps
                    conditionalPanel(
                      condition = "(input.sliding_window_step_choice == 'the duration of a step')",
                                  div(title='The unit of the sliding windows step duration ("days", "weeks", "months" or "years")',
                                           selectInput(inputId="sliding_window_step_unit",
                                              label="Sliding wnd. step unit",
                                              choices=c("days", "weeks", "months", "years"),
                                              selected="days")),
                                  div(title='The sliding windows duration (in the units selected above)',
                                           sliderInput(inputId="sliding_window_step_duration",
                                              label="Sliding wnd. step duration",
                                              min=0, max=.plotting.params$sliding.window.duration.max, value=7, step=1, round=TRUE))
                    ),
                    conditionalPanel(
                      condition = "(input.sliding_window_step_choice == 'the number of steps')",
                                  div(title='The number of sliding windows steps',
                                           sliderInput(inputId="sliding_window_no_steps",
                                              label="Sliding wnd. number of steps",
                                              min=0, max=1000, value=10, step=1, round=TRUE))
                    ),

                    # Plot CMA as histogram ----
                    div(title='Show the distribution of estimated CMAs across sliding windows as a histogram or barplot?',
                             checkboxInput(inputId="plot_CMA_as_histogram_sliding_window",
                                  label="Plot CMA as histogram?",
                                  value=TRUE)),

                    hr()

      ),


      div(title='Misc. parameters',
               span(h4("Other..."), style="color:DarkBlue")),

      # Align al patients ----
      conditionalPanel(
        condition="(input.patient.length > 1)",

        div(title='Should all the patients be vertically aligned relative to their first event?',
                 checkboxInput(inputId="plot_align_all_patients",
                      label="Align patients?",
                      value=FALSE)),

        # Align al patients ----
        conditionalPanel(
          condition="input.plot_align_all_patients",
          div(title='Should the first event (across patients) be considered as the origin of time?',
                   checkboxInput(inputId="plot_align_first_event_at_zero",
                        label="Align 1st event at 0?",
                        value=FALSE))
        )
      ),

      # Show legend? ----
      div(title='Display the plot legend?',
               checkboxInput(inputId="show_legend",
                  label="Show the legend?",
                  value=TRUE)),

      # Legend attributes ----
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

        div(title='The legend\'s bacground opacity (between 0.0=fully transparent and 1.0=fully opaque)',
            sliderInput(inputId="legend_bkg_opacity",
                        label="Legend bkg. opacity",
                        min=0.0, max=1.0, value=0.5, step=0.1, round=TRUE))
      )

    )),


    # Main panel for displaying outputs ----
    #mainPanel(
    column(9,

      # Control the plot dimensions ----
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
        div(title='Explort this plot to an image file?',
                 checkboxInput(inputId="save_to_file_info",
                    label="Save plot!",
                    value=FALSE))
      ),

      column(2,
        # Close shop:
        div(title='Exit this Shiny plotting app? (The plot will NOT be automatically saved!)',
                 actionButton(inputId="close_shop", label=strong("Exit..."), icon=icon("remove-circle", lib="glyphicon"), style="color: #C70039 ; border-color: #C70039"))
      ),

      # Save to file info:
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

      # Messages:
      column(12,
        tags$head(tags$style("#container * { display: inline; }")),
        div(id="container", title="Various messages (in blue), warnings (in green) and errors (in red) generated during plotting...",
            span(" Messages:", style="color:DarkBlue; font-weight: bold;"),
            span(htmlOutput(outputId = "messages")),
            style="height: 2em; resize: none; overflow: auto")
      ),

      # Output: the actual plot ----
      column(12, wellPanel(id = "tPlot",
                           style="resize: none; overflow:scroll; max-height: 75vh; max-width: 80vw",
                           plotOutput(outputId = "distPlot", inline=TRUE)))

    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  # The plotting function:
  .renderPlot <- function()
  {
    .plotting.params$.plotting.fnc(data=.plotting.params$data,
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
                                   sliding.window.no.steps=ifelse(input$sliding_window_step_choice == "the number of steps" ,as.numeric(input$sliding_window_no_steps), NA),
                                   plot.CMA.as.histogram=ifelse(input$cma_class == "sliding window",
                                                                !input$plot_CMA_as_histogram_sliding_window,
                                                                !input$plot_CMA_as_histogram_episodes),
                                   align.all.patients=input$plot_align_all_patients,
                                   align.first.event.at.zero=input$plot_align_first_event_at_zero,
                                   show.legend=input$show_legend, legend.x=input$legend_x, legend.y=input$legend_y, legend.bkg.opacity=input$legend_bkg_opacity, # legend
                                   get.colnames.fnc=.plotting.params$get.colnames.fnc,
                                   get.patients.fnc=.plotting.params$get.patients.fnc,
                                   get.data.for.patients.fnc=.plotting.params$get.data.for.patients.fnc
    )
  }

  # Do the ploting:
  output$distPlot <- renderPlot({

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
        if( !is.numeric(.plotting.params$plot.ratio) ) .plotting.params$plot.ratio <<- (input$plot_width / input$plot_height); # define the ratio
        if( input$plot_keep_ratio )
        {
          return (input$plot_width / .plotting.params$plot.ratio);
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
      .plotting.params$plot.ratio <<- (input$plot_width / input$plot_height); # save the ratio
    } else
    {
      updateSliderInput(session, "plot_height", value = round(input$plot_width / .plotting.params$plot.ratio));
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

}


# call shiny
shinyApp(ui = ui, server = server)

