library(shiny)
library(rCharts)

shinyServer(function(input, output) {
  observe({
    # Update data
    dataset <- input$dataset
    data <- read.csv(paste("results/", dataset, ".csv", sep=""), header = TRUE)

    # Update datatable
    output$mytable1 <- myRenderDataTable(data)

    # Update chart
    xAxis <- input$x
    yAxis <- input$y
    output$plot <- myRenderScatterChart(data,xAxis,yAxis)
  })
})

myRenderDataTable <- function(data){
  DTOptions = list(
    searching = FALSE,
    paging = FALSE,
    pageLength = 11
  )
  return(DT::renderDataTable({
    DT::datatable(data, options = DTOptions)
  }))
}

myRenderScatterChart <- function(data, xAxis, yAxis, output){
  renderChart({
    #scatter chart
    data <- data[-c(length(data[[1]])), ]

    p1 <- nPlot(as.formula(paste(yAxis,"~",xAxis,sep="")), group = "method", data = data, type = 'scatterChart', size = list(const = 5000))
    p1$xAxis(axisLabel = xAxis)
    p1$xAxis(tickFormat="#!function(d) {return d3.format(',.2f')(d);}!#")
    p1$yAxis(axisLabel = yAxis)
    p1$yAxis(tickFormat="#!function(d) {return d3.format(',.2f')(d);}!#")
    p1$chart(sizeRange = c(300,300))
    p1$chart(tooltipContent = "#! function(key, x, y, e){
      html = '<h3>' + key + '</h3>' +
      '<div>AUC: ' + e.point.AUC + '</div>' +
      '<div>F1: ' + e.point.F1 + '</div>' +
      '<div>Sensitivity: ' + e.point.sensitivity + '</div>' +
      '<div>Specificity: ' + e.point.specificity + '</div>' +
      '<div>Significant: ' + e.point.significant + '</div>';
     return html;
    } !#")
    p1$chart(showControls = FALSE)
    p1$set(dom = "plot")
    return(p1)
  })
}
