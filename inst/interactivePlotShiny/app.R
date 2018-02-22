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


library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("AdhereR: Interactive plotting using Shiny..."),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      span(h4("General settings..."), style="color:DarkBlue"),

      # Select the CMA class ----
      selectInput(inputId="cma_class",
                  label="Select the class of CMAs",
                  choices=c("simple", "per episode", "sliding window"),
                  selected=.plotting.params$cma.class),

      hr(),

      # Select the simple CMA to compute ----
      selectInput(inputId="cma_to_compute",
                  label="Select the simple CMA to compute",
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
                  choices=c("days", "weeks", "months", "years", "Actual Calendar Date"),
                  selected="days"),

      # If follow-up window unit is "Actual Calendar Date" ----
      conditionalPanel(
        condition = "(input.followup_window_start_unit == 'Actual Calendar Date')",
                    # Select an actual date ----
                    dateInput(inputId="followup_window_start_date",
                              label="Follow-up wnd. start",
                              value=NULL, format="dd/mm/yyyy", startview="month", weekstart=1)
      ),

      # If follow-up window unit is not "Actual Calendar Date" ----
      conditionalPanel(
        condition = "(input.followup_window_start_unit != 'Actual Calendar Date')",
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
                  choices=c("days", "weeks", "months", "years", "Actual Calendar Date"),
                  selected="days"),

      # If observation window unit is "Actual Calendar Date" ----
      conditionalPanel(
        condition = "(input.observation_window_start_unit == 'Actual Calendar Date')",
                    # Select an actual date ----
                    dateInput(inputId="observation_window_start_date",
                              label="Observation wnd. start",
                              value=NULL, format="dd/mm/yyyy", startview="month", weekstart=1)
      ),

      # If observation window unit is not "Actual Calendar Date" ----
      conditionalPanel(
        condition = "(input.observation_window_start_unit != 'Actual Calendar Date')",
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


                    hr()

      ),


      span(h4("Other..."), style="color:DarkBlue"),

      # Show legend? ----
      checkboxInput(inputId="show_legend",
                  label="Show the legend?",
                  value=TRUE),

      # Save image to file:
      downloadButton(outputId="save_to_file", label="Export plot to file..."),

      hr(),

      # Close shop:
      span(h4("Close shop..."), style="color:Red"),
      actionButton(inputId="close_shop", label="  and return to caller...", icon=icon("remove-circle", lib="glyphicon"))

    ),


    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      plotOutput(outputId = "distPlot")

    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {

  # The ploting:
  output$distPlot <- renderPlot({

      # Depeding on the CMA class we might do things differently:
      if( input$cma_class == "simple" )
      {
        # Call the workhorse plotting function with the appropriate argumens:
        .plotting.params$.plotting.fnc(data=.plotting.params$data,
                                       ID=input$patient,
                                       cma=input$cma_to_compute,
                                       carryover.within.obs.window=NA, # carryover.within.obs.window,
                                       carryover.into.obs.window=NA, # carryover.into.obs.window,
                                       carry.only.for.same.medication=FALSE,
                                       consider.dosage.change=FALSE,
                                       followup.window.start=0,
                                       followup.window.start.unit="days", # followup.window.start.unit,
                                       followup.window.duration=365,
                                       followup.window.duration.unit="days", # followup.window.duration.unit,
                                       observation.window.start=0,
                                       observation.window.start.unit="days", # observation.window.start.unit,
                                       observation.window.duration=365,
                                       observation.window.duration.unit="days", # observation.window.duration.unit,
                                       medication.change.means.new.treatment.episode=FALSE, #medication.change.means.new.treatment.episode,
                                       maximum.permissible.gap=0, #maximum.permissible.gap,
                                       maximum.permissible.gap.unit="days", # maximum.permissible.gap.unit,
                                       sliding.window.start=0, #sliding.window.start,
                                       sliding.window.start.unit="days", # sliding.window.start.unit,
                                       sliding.window.duration=10, #sliding.window.duration,
                                       sliding.window.duration.unit="days", # sliding.window.duration.unit,
                                       sliding.window.step.duration=5, #sliding.window.step.duration,
                                       sliding.window.step.unit="days", # sliding.window.step.unit,
                                       sliding.window.no.steps=NA, # sliding.window.no.steps
                                       plot.CMA.as.histogram=NA, # plot CMA as historgram?
                                       show.legend=TRUE # show the legend?
        );
      } else if( input$cma_class == "per episode" )
      {
        plot(0:10,0:10,col="red");
      } else if( input$cma_class == "sliding window" )
      {
        plot(0:10,0:10,col="green");
      } else
      {
        showModal(modalDialog(title="AdhereR interactive plotting...", paste0("Unknwon CMA class '",input$cma_class,"'."), easyClose=TRUE));
      }

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

