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
  titlePanel("AdhereR: Interactive plotting using Shiny..."),

  # Sidebar layout with input and output definitions ----
  #sidebarLayout(
  fluidRow(

    # Sidebar panel for inputs ----
    #sidebarPanel(
    column(3, wellPanel(id = "tPanel", style = "overflow-y:scroll; max-height: 600px;",


      span(h4("General settings..."), style="color:DarkBlue"),

      # Select the CMA class ----
      selectInput(inputId="cma_class",
                  label="Select CMA type",
                  choices=c("simple", "per episode", "sliding window"),
                  selected=.plotting.params$cma.class),

      hr(),

      # Select the simple CMA to compute ----
      selectInput(inputId="cma_to_compute",
                  label="Select CMA to compute",
                  choices=paste0("CMA",0:9),
                  selected="CMA1"),

      # Select the patient to plot ----
      selectInput(inputId="patient",
                  label="Select patient to plot",
                  choices=.plotting.params$all.IDs,
                  selected=.plotting.params$ID),

      hr(),
      span(h4("Follow-up window..."), style="color:DarkBlue"),

      # Follow-up window start ----
      selectInput(inputId="followup_window_start_unit",
                  label="Follow-up wnd. start unit",
                  choices=c("days", "weeks", "months", "years", "calendar date"), # "column in dataset"),
                  selected="days"),

      # If follow-up window unit is "calendar date" ----
      conditionalPanel(
        condition = "(input.followup_window_start_unit == 'calendar date')",
                    # Select an actual date ----
                    dateInput(inputId="followup_window_start_date",
                              label="Follow-up wnd. start",
                              value=NULL, format="dd/mm/yyyy", startview="month", weekstart=1)
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
                    sliderInput(inputId="followup_window_start_no_units",
                                label="Follow-up wnd. start",
                                min=0, max=.plotting.params$followup.window.start.max, value=0, step=1, round=TRUE)
      ),


      # Follow-up window duration ----
      selectInput(inputId="followup_window_duration_unit",
                  label="Follow-up wnd. duration unit",
                  choices=c("days", "weeks", "months", "years"),
                  selected="days"),

      # Select the number of units ----
      sliderInput(inputId="followup_window_duration",
                  label="Follow-up wnd. duration",
                  min=0, max=.plotting.params$followup.window.duration.max, value=2*365, step=1, round=TRUE),

      hr(),
      span(h4("Observation window..."), style="color:DarkBlue"),

      # Observation window start ----
      selectInput(inputId="observation_window_start_unit",
                  label="Observation wnd. start unit",
                  choices=c("days", "weeks", "months", "years", "calendar date"),
                  selected="days"),

      # If observation window unit is "calendar date" ----
      conditionalPanel(
        condition = "(input.observation_window_start_unit == 'calendar date')",
                    # Select an actual date ----
                    dateInput(inputId="observation_window_start_date",
                              label="Observation wnd. start",
                              value=NULL, format="dd/mm/yyyy", startview="month", weekstart=1)
      ),

      # If observation window unit is not "calendar date" ----
      conditionalPanel(
        condition = "(input.observation_window_start_unit != 'calendar date')",
                    # Select the number of units ----
                    sliderInput(inputId="observation_window_start_no_units",
                                label="Observation wnd. start",
                                min=0, max=.plotting.params$observation.window.start.max, value=0, step=1, round=TRUE)
      ),


      # Observation window duration ----
      selectInput(inputId="observation_window_duration_unit",
                  label="Observation wnd. duration unit",
                  choices=c("days", "weeks", "months", "years"),
                  selected="days"),

      # Select the number of units ----
      sliderInput(inputId="observation_window_duration",
                  label="Observation wnd. duration",
                  min=0, max=.plotting.params$observation.window.duration.max, value=2*365, step=1, round=TRUE),

      hr(),


      # For CMA5 to CMA9: carry_only_for_same_medication, consider_dosage_change ----
      conditionalPanel(
        condition = "(input.cma_to_compute == 'CMA5' || input.cma_to_compute == 'CMA6' || input.cma_to_compute == 'CMA7' || input.cma_to_compute == 'CMA8' || input.cma_to_compute == 'CMA9')",

                    span(h4("Carry over..."), style="color:DarkBlue"),

                    # Carry-over for same treat only? ----
                    checkboxInput(inputId="carry_only_for_same_medication",
                                  label="Carry-over for same treat only?",
                                  value=FALSE),

                    # Consider dosage changes? ----
                    checkboxInput(inputId="consider_dosage_change",
                                  label="Consider dosage changes?",
                                  value=FALSE),

                    hr()
      ),


      # For per episode:
      conditionalPanel(
        condition = "(input.cma_class == 'per episode')",

                    span(h4("Define the episodes..."), style="color:DarkBlue"),

                    # Does treat. change start new episode? ----
                    checkboxInput(inputId="medication_change_means_new_treatment_episode",
                                  label="Treat. change start new episode?",
                                  value=FALSE),

                    # Max. permis. gap duration unit ----
                    selectInput(inputId="maximum_permissible_gap_unit",
                                label="Max. permis. gap duration unit",
                                choices=c("days", "weeks", "months", "years"),
                                selected="days"),

                    # Max. permissible gap ----
                    sliderInput(inputId="maximum_permissible_gap",
                                label="Max. permissible gap",
                                min=0, max=.plotting.params$followup.window.duration.max, value=0, step=1, round=TRUE),

                    # Plot CMA as histogram ----
                    checkboxInput(inputId="plot_CMA_as_histogram_episodes",
                                  label="Plot CMA as histogram?",
                                  value=FALSE),

                    hr()
      ),


      # For sliding window:
      conditionalPanel(
        condition = "(input.cma_class == 'sliding window')",

                    span(h4("Define the sliding windows..."), style="color:DarkBlue"),

                    # Sliding window start ----
                    selectInput(inputId="sliding_window_start_unit",
                                label="Sliding wnd. start unit",
                                choices=c("days", "weeks", "months", "years"),
                                selected="days"),

                    # Select the number of units ----
                    sliderInput(inputId="sliding_window_start",
                                label="Sliding wnd. start",
                                min=0, max=.plotting.params$sliding.window.start.max, value=0, step=1, round=TRUE),

                    # Sliding window duration ----
                    selectInput(inputId="sliding_window_duration_unit",
                                label="Sliding wnd. duration unit",
                                choices=c("days", "weeks", "months", "years"),
                                selected="days"),

                    # Select the number of units ----
                    sliderInput(inputId="sliding_window_duration",
                                label="Sliding wnd. duration",
                                min=0, max=.plotting.params$sliding.window.duration.max, value=90, step=1, round=TRUE),

                    # Steps choice ----
                    selectInput(inputId="sliding_window_step_choice",
                                label="Define the sliding wnd. steps by",
                                choices=c("the number of steps", "the duration of a step"),
                                selected="the duration of a step"),

                    # Sliding window steps
                    conditionalPanel(
                      condition = "(input.sliding_window_step_choice == 'the duration of a step')",
                                  selectInput(inputId="sliding_window_step_unit",
                                              label="Sliding wnd. step unit",
                                              choices=c("days", "weeks", "months", "years"),
                                              selected="days"),
                                  sliderInput(inputId="sliding_window_step_duration",
                                              label="Sliding wnd. step duration",
                                              min=0, max=.plotting.params$sliding.window.duration.max, value=7, step=1, round=TRUE)
                    ),
                    conditionalPanel(
                      condition = "(input.sliding_window_step_choice == 'the number of steps')",
                                  sliderInput(inputId="sliding_window_no_steps",
                                              label="Sliding wnd. number of steps",
                                              min=0, max=1000, value=10, step=1, round=TRUE)
                    ),

                    # Plot CMA as histogram ----
                    checkboxInput(inputId="plot_CMA_as_histogram_sliding_window",
                                  label="Plot CMA as histogram?",
                                  value=TRUE),

                    hr()

      ),


      span(h4("Other..."), style="color:DarkBlue"),

      # Show legend? ----
      checkboxInput(inputId="show_legend",
                  label="Show the legend?",
                  value=TRUE)

    )),


    # Main panel for displaying outputs ----
    #mainPanel(
    column(9,

      # Control the plot dimensions ----
      column(3,
        sliderInput(inputId="plot_width",
                    label="Plot width",
                    min=0, max=5000, value=500, step=20, round=TRUE, )
      ),

      column(3,
        sliderInput(inputId="plot_height",
                    label="height",
                    min=0, max=5000, value=300, step=20, round=TRUE)
      ),

      column(2,
        checkboxInput(inputId="plot_keep_ratio",
                    label="keep ratio",
                    value=TRUE),

        checkboxInput(inputId="plot_auto_size",
                    label="auto size",
                    value=TRUE)
      ),

      column(2,
        # Save image to file:
        downloadButton(outputId="save_to_file", label="Save plot")
      ),

      column(2,
        # Close shop:
        actionButton(inputId="close_shop", label=strong("Exit..."), icon=icon("remove-circle", lib="glyphicon"), style="color: #C70039 ; border-color: #C70039")
      ),

      # Messages:
      column(12,
        tags$head(tags$style("#container * { display: inline; }")),
        div(id="container", span(" Messages:", style="color:DarkBlue"), span(textOutput(outputId = "messages"), style="color:Blue"))
      ),

      # Output: the actual plot ----
      column(12, wellPanel(id = "tPlot",
                           style="resize: both; overflow-y:scroll; overflow-x:scroll; max-height: 800px;",
                           plotOutput(outputId = "distPlot", inline=TRUE)))

    )
  )#,
  #
  #
  #fluidRow(
  #
  #  # Messages:
  #  column(12,
  #    span(h4(" Messages:"), style="color:DarkBlue")
  #    span(textOutput(outputId = "messages"), style="color:Blue")
  #  )
  #
  #)

)


# Define server logic required to draw a histogram ----
server <- function(input, output) {

  # The ploting:
  output$distPlot <- renderPlot({

      # Depeding on the CMA class we might do things differently:
      msgs <- ""; # the output messages
      if( input$cma_class %in% c("simple", "per episode", "sliding window") )
      {
        # Call the workhorse plotting function with the appropriate argumens:
        msgs <- capture.output(
          .plotting.params$.plotting.fnc(data=.plotting.params$data,
                                         ID=input$patient,
                                         cma=ifelse(input$cma_class == "simple",
                                                    input$cma_to_compute,
                                                    input$cma_class),
                                         cma.to.apply=ifelse(input$cma_class == "simple",
                                                             "none",
                                                             input$cma_to_compute),
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
                                         show.legend=input$show_legend # show the legend?
          )
        );
      } else
      {
        # Quitting....
        showModal(modalDialog(title="AdhereR interactive plotting...", paste0("Unknwon CMA class '",input$cma_class,"'."), easyClose=TRUE));
      }

      output$messages <- renderText({
        msgs;
      })

    },
    width=function(){ input$plot_width }, # plot dimensions
    height=function(){ input$plot_height }
    )

  # Text messages:
  output$messages <- renderText({
    ""
  })

  # Export plot to file:
  output$save_to_file <- downloadHandler(
    filename = function(){ paste0("adherer-plot-",input$cma_class,"-",input$cma_to_compute,"-ID-",input$patient,".jpg") },
    content = function(file) {
      write.csv(data, file) # TODO!!!
    }
  )



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

