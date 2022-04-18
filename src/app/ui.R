
ui = dashboardPage(
  title = "3DPolate",
  
  dashboardHeader(
    title = span(img(src = "EnviData.PNG", height = 40)),
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
    introBox( data.step = 1, data.intro = intro$text[1],
              h5(em("Navigation"), style = "margin:10px;"),
              bsButton("introBut",
                       label = "WELCOME",
                       icon = icon("tasks"),
                       style="success"),
              introBox( data.step = 2, data.intro = HTML(intro$text[2]),
                        bsButton("loadDataBut",
                                 label = " LOAD DATA",
                                 icon = icon("flask", class = "flask-box"),
                                 style="success")
              ),
              introBox( data.step = 6, data.intro = intro$text[6],
                        bsButton("runInterBut",
                                 label = " RUN INTERPOLATION",
                                 icon = icon("arrows"),
                                 style="success")),
              introBox(data.step = 12, data.intro = intro$text[12],
                       bsButton("viewResBut",
                                label = " VIEW RESULTS",
                                icon = icon("chart-line"),
                                style="success")
              ),
              bsButton("aboutUsBut",
                       label = " ABOUT US",
                       icon = icon("info"),
                       style="success")
    )),
  
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
                       
                       div(em(h5(" - Your New Favourite 3D Interpolation App -")),
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
      introBox( data.step = 3, data.intro = intro$text[3],
                fluidRow(div(id = "loadDataPanel",
                             tabBox(
                               width = 12,
                               tabPanel(
                                 title = h3(strong("Load Some Data to Interpolate In-Between")),
                                 br(),
                                 introBox( data.step = 5, data.intro = intro$text[5],
                                           materialSwitch("testData","Use Test Dataset",value = TRUE, status = "success")),
                                 hr(),
                                 conditionalPanel(condition = "input.testData",
                                                  div(h5(em("The Test Dataset is the Meuse pollution dataset (using Zinc Values) from the {gstat} package")))
                                 ),
                                 conditionalPanel(condition = "!input.testData",
                                                  tagList(fileInput("file1", "Choose CSV File",
                                                                    multiple = FALSE,
                                                                    accept = c("text/csv",
                                                                               "text/comma-separated-values,text/plain",
                                                                               ".csv")),
                                                          radioButtons("sep", "Separator",
                                                                       choices = c(Comma = ",",
                                                                                   Semicolon = ";",
                                                                                   Tab = "\t")),
                                                          materialSwitch("importHeader","Header",value = TRUE, status = "success"))
                                 ),
                                 hr(),
                                 introBox( data.step = 4, data.intro = intro$text[4],
                                           withSpinner(
                                             plotlyOutput("plotRawdata"),
                                             type = 4,
                                             color = "#d33724",
                                             size = 0.7
                                           )),
                                 
                                 hr(),
                                 materialSwitch("showRawDataPlot","Show Rawdata Table",value = FALSE, status = "success"),
                                 br(),
                                 conditionalPanel(condition = "input.showRawDataPlot",
                                                  dataTableOutput("tableRawdata"))
                               )
                             )
                ))),
      fluidRow(div(id = "runInterPanel",
                   tabBox(
                     width = 12,
                     tabPanel(
                       title = h3(strong("Run Different Interpolation Methods!")),
                       
                       introBox(data.step = 7, data.intro = intro$text[7],
                                div(h4(strong("Deterministic Interpolation"))),
                                div(h5(em("Deterministic Methods include Baseline (i.e. average concentration), Nearest Neighbour (NN) and Inverse Distance Weighting (IDW))"))),
                                fixedRow(column(3, sliderInput("idwNoN","Number of neighbours (IDW)", min = 2, max = 100, value = c(2,10),step=1)),
                                         column(3, sliderInput("idwEF","Elevation Factor (anisotropy; IDW and NN)", min = -100, max = 100, value = c(2,10),step=2)),
                                         column(3, numericInput("idwPower","Power Parameter (IDW)",value =2))),
                                introBox( data.step = 8, data.intro = intro$text[8],
                                          fixedRow(column(3, div(actionButton("showDeterHelp","Parameter Tips!"),style="margin-left: 15px;")))
                                )
                       ),
                       hr(),
                       introBox( data.step = 9, data.intro = intro$text[9],
                                 div(h4(strong("Ordinary Kriging"))),
                                 fixedRow(column(3, numericInput("krigPsill","Partial Sill", 158000)),
                                          column(3, numericInput("krigRange","Range", 500)),
                                          column(3, numericInput("krigNug","Nugget",10000)),
                                          column(3, selectInput("krigModel","Choose Model",list("Spherical" = "Sph","Gausian" = "Gau","Exponential" = "Exp")))),
                                 fixedRow(column(3, div(actionButton("showVar","Show Variogram"),style="margin-left: 15px;")),
                                          column(3, div(actionButton("showKrigHelp","Parameter Tips!"),style="margin-left: 15px;")))
                       ),
                       hr(),
                       div(h4(strong("Overall Parameters"))),
                       fluidRow(column(4, "Logtransform Values"),column(8, materialSwitch("logTrans",NULL, FALSE,status = "success"))),
                       br(),
                       introBox( data.step = 10, data.intro = intro$text[10],
                                 fixedRow(column(3, div(actionButton("interRun","Run Interpolations!"),style="margin-left: 15px;")))
                       ),
                       hr(),
                       introBox(data.step = 11, data.intro = intro$text[11],
                                withSpinner(
                                  plotlyOutput("rmsePlot"),
                                  type = 4,
                                  color = "#d33724",
                                  size = 0.7
                                )
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
                       introBox(data.step = 13, data.intro = intro$text[13],
                                selectInput("resSelData","Dataset to Plot",list("Baseline" = "baseline","Nearest Neighbour" = "nn",
                                                                                "Inverse Distance Weigthing" = "idw", "Kriging" = "krig"),
                                            selected = "krig")
                       ),
                       div(h5(em("To visualize the 3D Interpolation Values have been Assigned to a 3D Grid Covering the Area of Interest (range of input data)"))),
                       hr(),
                       introBox(data.step = 14, data.intro = intro$text[14],
                                withSpinner(
                                  plotlyOutput("resPlot3d"),
                                  type = 4,
                                  color = "#d33724",
                                  size = 0.7
                                )
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
                       div(
                         img(src = "ngiBigLogo.png", height = 90),
                         
                         h5(tags$span(
                           tags$strong("3DPolate was Financed by ", tags$a(href = "https://www.ngi.no/", "NGI")),
                           tags$br(), tags$br(),
                           tags$em(tags$strong(" an Independent International Centre for Research and Consulting within the Engineering Geosciences.")), 
                           tags$br(), tags$br(),
                           "We are Norway's leading Geotechnical Specialist Community and work in the areas of Building, Construction, and Transportation, Environmental Engineering, Offshore energy and Natural hazards.",
                           tags$br(),
                           "Our knowledge on Sustainable Solutions and Technology is important to ensure that we", 
                           tags$strong("Build our Society on Safe Ground."),
                           tags$br(), tags$br(), tags$br(),
                           
                           tags$strong(tags$em("How We Use {Shiny} Apps")), 
                           tags$br(), tags$br(),
                           "We use Shiny Apps both for R&D portals to interact with our External Partners, but also to support Internal Everyday Needs.",
                           tags$br(),
                           "We take pride in Keeping our Apps Up-and-Running at all times, properly tested before deployment, supplied with only Quality Assured functionality and Fast and Snappy!",
                           tags$br(),
                           "To achieve this we run our Apps in Docker containers in Kubernetes hosted by Azure, making it easy to scale-up and integrate with ArcGIS Services and Our Python Back-End.",
                           tags$br(),
                           "We Monitor all Services with Grafana Dashboards and Handle our Errors with Sentry",
                           tags$br(),
                           "We Mostly use e-mail to Receive Our Raw Data, just because that's what Our Partners curently can Offer.",
                           tags$br(), tags$br(),
                           img(src = "services.png", height = 300),
                           tags$br(), tags$br(), tags$br(),
                           
                           tags$strong(tags$em("Packages we Love and Have Used in this App:")),
                           tags$br(), tags$br(),
                           tags$a(href = "https://github.com/rstudio/shiny", "{shiny}, No Explanation Needed, Keep Up the Good Work Rstudio!"),
                           tags$br(),
                           tags$a(href = "https://github.com/r-spatial/gstat", "{gstat}, a brilliant geostatistical package!"),
                           tags$br(),
                           tags$a(href = "https://github.com/carlganz/rintrojs", "{rintrojs}, Allowing you to Create Those Nice and Intuitive Tutorials!"),
                           tags$br(),
                           tags$a(href = "https://github.com/plotly/plotly.R", "{plotly}, Either You Just Need a Single bar or Some 3D plots, Plotly does the Job!"),
                           tags$br(),
                           tags$a(href = "https://github.com/ceefluz/radar", 
                                  tags$span("We had a Looked and Borrowed from the \"RadaR\" app, Developed by",
                                            tags$a(href = "https://github.com/ceefluz", "Christian Luz, Thanks for Sharing!"))),
                           tags$br(), tags$br(),
                           
                           tags$strong(tags$em("Want to Join our Team(!?)")),
                           tags$br(), tags$br(),
                           tags$a(href = "https://candidate.hr-manager.net/ApplicationInit.aspx?cid=388&ProjectId=175660&uiculture=en&MediaId=4181",
                                  img(src = "hiring.png", height = 200)),
                           tags$br(),
                           tags$a(href = "https://candidate.hr-manager.net/ApplicationInit.aspx?cid=388&ProjectId=175660&uiculture=en&MediaId=4181",
                                  "Reach Out to Us Through Our Recruitment Portal"),
                           tags$br(), tags$br(), tags$br(),
                           
                           
                           tags$strong(tags$em("Want to Learn More(!?)")),
                           tags$br(), tags$br(),
                           tags$a(href = "https://www.youtube.com/watch?v=Fo4Wq0tmkB0",
                                  img(src = "meetup.png", height = 200)),
                           tags$br(),
                           tags$a(href = "https://www.youtube.com/watch?v=Fo4Wq0tmkB0",
                                  "Watch our Meet-up \"Build Interactive {shiny} Apps to Share Your Work With Anyone!"),
                           tags$br(), tags$br(), tags$br(),
                           
                           
                           tags$strong(tags$em("About the App Authors:")),
                           tags$br(), tags$br(),
                           "Main Author", tags$strong("Andreas Botnen Smebye"), 
                           "is a Scientific Programmer and Digital Product Owner working with Environmental Chemistry for more than ten years at NGI.",
                           tags$br(), 
                           "He is part of the team behind the innovation center", 
                           tags$a(href = "https://www.nmbu.no/en/services/centers/earthresque/about", "earthresQue"),
                           "to find new Innovative Mapping and Treatment Methods for Polluted Sites in a circular economy.",
                           tags$br(),
                           "He is particular interested in how to use digitalization as a tool to implement findings and methodology from R&D into",
                           tags$a(href = "https://www.ngi.no/Prosjekter/Nye-verktoey-og-metoder-for-risikovurdering-og-tiltak-ved-skytebaner-med-Forsvarsbygg)", "New Practice."),
                           
                           tags$br(), tags$br(),
                           "Co-Author", tags$strong("Erlend Briseid Storrøsten"), 
                           "is a Mathematician who knows his Gausian distributions and never mistakes Correlation for Causation.",
                           tags$br(), tags$br()
                         )),
                         
                         align = "center"
                       )
                     )
                   )
      )
      ),
      div(column(12,
                 h6(em(tags$span("NGI is not liable for any damages arising in contract, tort or otherwise from the use of or inability to use this site or any material contained in it, or from any action or decision taken as a result of using the site.",
                                 tags$br(),
                                 "This Web Page neither Stores nor Collects Personal Information")
                 ))),
          align = "center"),
      br(),br()
    )
  )
)



