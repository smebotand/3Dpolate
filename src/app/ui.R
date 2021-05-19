
ui = dashboardPage(
  title = "3DPolate",

  dashboardHeader(
    title = span(img(src = "MiljoData.png", height = 40)),
    titleWidth = 300,
    tags$li(
              img(src = "ngiLogo.jpg", height = "50vh"),
      class = "dropdown"
    )
  ),

  dashboardSidebar(
    width = 300,
             div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5"),
             br(),
             h5(em("Navigation"), style = "margin:10px;"),
             bsButton("introBut",
                      label = "WELCOME",
                      icon = icon("tasks"),
                      style="success"),
             bsButton("loadDataBut",
                      label = " LOAD DATA",
                      icon = icon("flask", class = "flask-box"),
                      style="success"),
             bsButton("runInterBut",
                      label = " RUN INTERPOLATION",
                      icon = icon("chart-line"),
                      style="success"),
             bsButton("viewResBut",
                      label = " VIEW RESULTS",
                      icon = icon("chart-line"),
                      style="success"),
             bsButton("aboutUsBut",
                      label = " ABOUT US",
                      icon = icon("chart-line"),
                      style="success")
  ),

  dashboardBody(

    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "app.css")
    ),
    tags$style(HTML(".dataTables_paginate .paginate_button .paginate_button.disabled .paginate_button.active .paginate_button.current {
            color: white;
            background-color: #727477;
            border-radius: 4px;
        }")),
    tags$head(
      tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32x32.png")
    ),
    introjsUI(),
    shinyjs::useShinyjs(),

    div(
      fluidRow(div(id = "introPanel",
                   tabBox(
                     width = 12,
                     tabPanel(
                       title = h3(strong("Welcome")),

                       div(h2(strong("Let's 3DPolate!")),
                           align = "center"),

                       div(em(h5(" - Your New Favourite 3D Interpolation App! -")),
                           align = "center"),

                       fluidRow(
                         column(12, plotlyOutput("logoPlot")),
                       ),
                       fluidRow(
                         div(column(12,h5(strong(HTML("Please use the navigation Buttons in the Sidepanel to Navigate<br/>Go to \"Load Data\" to Get Started!"))),
                                    align = "center"))
                       )
                     )
                   )
      )),
      fluidRow(div(id = "loadDataPanel",
                   tabBox(
                     width = 12,
                     tabPanel(
                       title = h3(strong("Load Some Data to Interpolate In-Between")),
                       br(),

                       materialSwitch("testData","Use Test Dataset",value = TRUE, status = "success"),
                       hr(),
                       conditionalPanel(condition = "input.testData",
                                        div(h5(em("The Test Dataset is the Meuse pollution dataset (using zonc values) from the gstat package")))),
                       conditionalPanel(condition = "!input.testData",
                                        fileInput("file1", "Choose CSV File",
                                                  multiple = FALSE,
                                                  accept = c("text/csv",
                                                             "text/comma-separated-values,text/plain",
                                                             ".csv"))),
                       conditionalPanel(condition = "!input.testData",
                                        radioButtons("sep", "Separator",
                                                     choices = c(Comma = ",",
                                                                 Semicolon = ";",
                                                                 Tab = "\t"))),
                       conditionalPanel(condition = "!input.testData",
                                        materialSwitch("importHeader","Header",value = TRUE, status = "success")),
                       hr(),
                       withSpinner(
                         plotlyOutput("plotRawdata"),
                         type = 4,
                         color = "#d33724",
                         size = 0.7
                       ),

                       hr(),
                       materialSwitch("showRawDataPlot","Show Rawdata Table",value = FALSE, status = "success"),
                       br(),
                       conditionalPanel(condition = "input.showRawDataPlot",
                                        dataTableOutput("tableRawdata"))
                       #TODO: show histogram of value
                     )
                   )
      )),
      fluidRow(div(id = "runInterPanel",
                   tabBox(
                     width = 12,
                     tabPanel(
                       title = h3(strong("Run Different Interpolation Methods!")),


                       div(h4(strong("Deterministic Interpolation"))),
                       div(h5(em("Deterministic Methods include Baseline (i.e. average concentration), Nearest Neighbour (NN) and Inverse Distance Weighting (IDW))"))),
                       fixedRow(column(3, sliderInput("idwNoN","Number of neighbours (IDW)", min = 2, max = 100, value = c(2,10),step=1)),
                                column(3, sliderInput("idwEF","Elevation Factor (anisotropy; IDW and NN)", min = -100, max = 100, value = c(2,10),step=2)),
                                column(3, numericInput("idwPower","Power Parameter (IDW)",value =2))),
                       fixedRow(column(3, div(actionButton("showDeterHelp","Parameter Tips!"),style="margin-left: 15px;"))),
                       hr(),
                       div(h4(strong("Ordinary Kriging"))),
                       fixedRow(column(3, numericInput("krigPsill","Partial Sill", 158000)),
                                column(3, numericInput("krigRange","Range", 500)),
                                column(3, numericInput("krigNug","Nugget",10000)),
                                column(3, selectInput("krigModel","Choose Model",list("Spherical" = "Sph","Gausian" = "Gau","Exponential" = "Exp")))),
                       fixedRow(column(3, div(actionButton("showVar","Show Variogram"),style="margin-left: 15px;")),
                                column(3, div(actionButton("showKrigHelp","Parameter Tips!"),style="margin-left: 15px;"))),
                       hr(),
                       div(h4(strong("Overall Parameters"))),
                       fluidRow(column(4, "Logtransform Values"),column(8, materialSwitch("logTrans",NULL, FALSE,status = "success"))),
                       br(),
                       fixedRow(column(3, div(actionButton("interRun","Run Interpolations!"),style="margin-left: 15px;"))),
                                hr(),

                       withSpinner(
                         plotlyOutput("rmsePlot"),
                         type = 4,
                         color = "#d33724",
                         size = 0.7
                       ),
                       hr(),
                       fixedRow(column(6, div(em("Show Table of Model Errors"),materialSwitch("showTableCvPredictions",NULL, FALSE,status = "success")))),
                       dataTableOutput("tableCvPredictions")

                     )))),
      fluidRow(div(id = "viewResPanel",
                   tabBox(
                     width = 12,
                     tabPanel(

                       title = h3(strong("View Results")),
                       br(),
                       selectInput("resSelData","Dataset to Plot",list("Baseline" = "baseline","Nearest Neighbour" = "nn",
                                                                       "Inverse Distance Weigthing" = "idw", "Kriging" = "krig")),
                       div(h5(em("To visualize the 3D Interpolation Values have been Assigned to a 3D Grid Covering the Area of Interest (range of input data)"))),
                       hr(),
                       withSpinner(
                         plotlyOutput("resPlot3d"),
                         type = 4,
                         color = "#d33724",
                         size = 0.7
                       ),
                       div(h5(em("Please select Method to Visualize Above"))),
                        hr(),
                       materialSwitch("resShowTable","Show Table of Estimates",value = FALSE, status = "success"),
                       dataTableOutput("resTable"),
                       hr(),
                       materialSwitch("resShowHist","Show Histograms of Estimates",value = FALSE, status = "success"),

                       plotlyOutput("resPlotHist"),
                       div(h5(em("Please Double Click Legend to Select One Method at a time")))

                     )
                   )
      )),
      fluidRow(div(id = "aboutUsPanel",
                   tabBox(
                     width = 12,
                     tabPanel(
                       title = h3(strong("About us")),
                       br(),
                       h5("NGI is an independent international centre for research and consulting in engineering geosciences. We are Norway's leading geotechnical specialist community and work in the areas of Building, construction, and transportation, Environmental engineering, Offshore energy and Natural hazards. Our knowledge on sustainable solutions and technology is important to ensure that we build our society on safe ground."),
                       br(),
                       h5("About the author: Andreas Botnen Smebye is a senior environmental advisor and scientific programmer at the Norwegian Geotechnical Institute (ngi.no). He is part of the team behind the innovation center earthresQue (https://www.nmbu.no/en/services/centers/earthresque/about) to find new innovative mapping and treatment methods for polluted sites (https://www.ngi.no/Prosjekter/Nye-verktoey-og-metoder-for-risikovurdering-og-tiltak-ved-skytebaner-med-Forsvarsbygg) to enhance sustainable solutions in a circular economy. He is particular interested in how to use digitalization as a tool to implement findings and methodology from R&D into new practice."),
                       br(),
                       h5("Want to learn more(?), join our up-coming meetup presentation: https://www.meetup.com/Oslo-useR-Group/events/277702734/")
                       )
                     )
                   )
      ),
      div(column(12,em("NGI is not liable for any damages arising in contract, tort or otherwise from the use of or inability to use this site or any material contained in it, or from any action or decision taken as a result of using the site. This Web Page neither Stores nor Collects Personal Information"),
                 align = "center")),
      br(),br()
    )
  )
)
