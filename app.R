# Expanding and testing example from: https://stackoverflow.com/questions/40547786/shiny-can-dynamically-generated-buttons-act-as-trigger-for-an-event


# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Dynamic selectInput"),
  dashboardSidebar(
    sidebarMenu(
      menuItemOutput("menuitem")
    )
  ),
  dashboardBody(
    numericInput("go_btns_quant","Number of GO buttons",value = 1,min = 1,max = 10),
    uiOutput("go_buttons"),
    plotOutput("plot")
  )
)

server <- function(input, output, session) {
  output$menuitem <- renderMenu({
    menuItem("Menu item", icon = icon("calendar"))
  })
  
  # to store observers and make sure only once is created per button
  obsList <- list()
  
  output$go_buttons <- renderUI({
    buttons <- as.list(1:input$go_btns_quant)
    buttons <- lapply(buttons, function(i)
    {
      btName <- paste0("go_btn",i)
      # creates an observer only if it doesn't already exists
      if (is.null(obsList[[btName]])) {
        # make sure to use <<- to update global variable obsList
        obsList[[btName]] <<- observeEvent(input[[btName]], {
          cat("Button ", i, "\n")
          output$plot <-renderPlot({hist(rnorm(100, 4, 1),breaks = 50*i)})
        })
      }
      fluidRow(
        actionButton(btName,paste("Go",i))
      )
    }
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

