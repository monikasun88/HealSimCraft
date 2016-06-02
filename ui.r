# 2048 in R



shinyUI(fluidPage(

  fluidRow(actionButton("reset", label = "Reset")),
  
  hr(),
  fluidRow(plotOutput(outputId = "plot_2048", height = "500px", width = "500px"), align = "center"),
  
  # Copy the line below to make an action button
  fluidRow(actionButton("up", label = "Up"), align = "center"),
  fluidRow(
    actionButton("left", label = "Left"),
    actionButton("down", label = "Down"),
    actionButton("right", label = "Right"),
    align = "center"),
  hr()
  
))

# runApp("2048", display.mode = "showcase")