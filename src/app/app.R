#rm(list=ls()) #clean all stuff NATWIP!!!!
#setwd("C:/GIT2/envi-3d-interpolation/src/app")
library(shiny)
library(tidyverse)
library(DT)
library(shinydashboard)
library(leaflet)
library(plotly)

library(shinycssloaders)
library(modules)

library(Metrics)
library(purrr)
library(dplyr)

interpolate = use("modules/interpolate.R")

#######################
#### Builiding UI######
#######################

#graphics for header
dbHeader <- dashboardHeader(title = div(tags$a(href='http://www.ngi.no',
                                               tags$img(src='MiljoData.png',height='40',width='100'),
                                               style="height: 70px;"),
                                        style="height: 70px;"),
                            tags$li(a(href = 'http://www.ngi.no',
                                      tags$img(src='ngi_logo2.jpg',height='40',width='72.6'),
                                      style = "padding-top:10px; padding-bottom:9px;padding-right:0px; padding-left:0px;"),
                                    class = "dropdown",
                                    style="height: 40px; width:72.6px;margin-bottom:0px;padding: 0 0px;"))

ui = dashboardPage(
  title="3D interpolation",
  dbHeader,
  dashboardSidebar(
    width="280",
    sidebarMenu(
      id="tabs",
      br(),
      menuItem("Import Data", tabName = "data", icon = icon("map")),
      menuItem("Interpolate", tabName = "inter", icon = icon("map")),
      menuItem("Results", tabName = "res", icon = icon("th")))
  ),

  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "app.css")
    ),
    tabItems(
      tabItem("data",
              "Import Data",
              title = "Import Data",
              # Input: Select a file ----
              fileInput("file1", "Choose CSV File",
                        multiple = FALSE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")),

              # Horizontal line ----
              tags$hr(),

              # Input: Checkbox if file has header ----
              checkboxInput("header", "Header", TRUE),

              # Input: Select separator ----
              radioButtons("sep", "Separator",
                           choices = c(Comma = ",",
                                       Semicolon = ";",
                                       Tab = "\t"),
                           selected = ","),

              # Input: Select quotes ----
              radioButtons("quote", "Quote",
                           choices = c(None = "",
                                       "Double Quote" = '"',
                                       "Single Quote" = "'"),
                           selected = '"'),

              # Horizontal line ----
              tags$hr(),

              # Input: Select number of rows to display ----
              radioButtons("disp", "Display",
                           choices = c(Head = "head",
                                       All = "all"),
                           selected = "head"),
              hr(),

              # Output: Data file ----
              tableOutput("contents"),

              div(helpText(em("This webpage neither collects nor stores personal information")))
              ,align="center"),

      tabItem("inter",
              "Interpolate!",
              title = "Interpolate!",

              sliderInput("neighbours", "Number of Neighbours",1,100,c(1,10)),
              sliderInput("elevFactor", "Elevation Factor",1,100,c(1,10)),
              sliderInput("idwPower", "IDW p-faktor",1,10,c(2)),

              div(helpText(em("This webpage neither collects nor stores personal information")))
              ,align="center"),

      tabItem("res",
              "Results!",
              title = "Results!",

              div(helpText(em("This webpage neither collects nor stores personal information")))
              ,align="center")
    )
  )
)

server = function(input, output, session) {

  ###### IMport Data #####

  inputDataset = reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)

    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })

  output$contents <- renderTable({

    inputDataset()

  })


  ###### ANALYZE Data #####

  interpolatedDataset = reactive({

  })

  output$contents <- renderTable({

    inputDataset()

  })



}

shinyApp(ui, server)
