library(shiny)
library(rCharts)
options(
  rcharts.cdn = FALSE,
  RCHART_HEIGHT = 600
)

shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "mycss.css")
  ),
  titlePanel("Data Science In Practice HW3"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("dataset", "Choose a Dataset:", selected = "set1",
                choices = c("set1", "set2", "set3", "set4", "set5")),
      selectInput("x", "Choose a X:", selected = "specificity",
                choices = c("F1", "AUC", "sensitivity","specificity")),
      selectInput("y", "Choose a Y:", selected = "sensitivity",
                choices = c("F1", "AUC", "sensitivity","specificity"))
    ),
    mainPanel(
      width = 9,
      showOutput("plot","nvd3")
    )
  ),
  mainPanel(
    width = 12,
    tabPanel('mytable1', DT::dataTableOutput('mytable1'))
  )
))
