
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
  title="3Dpolate",
  dbHeader,
  dashboardSidebar(disable =T),

  dashboardBody(
    # tags$head(
    #   tags$link(
    #     rel = "stylesheet",
    #     type = "text/css",
    #     href = "app.css")
    # ),

    glide(
      height = "90%",
      div(screen(
        h1("Welcome Page"),
        p("This is a very simple shinyglide application."),
        p("Please click on Next to go to the next screen.")

      ),style = "padding: 30px;"),

      div(screen(
        h1("The concept"),
        p("This is a very simple shinyglide application."),
        p("Please click on Next to go to the next screen.")
      ),style = "padding: 30px;"),

      div(screen(
        h1("Upload Your Dataset"),
        fluidRow(
          column(3,strong("Download All Pictures: ")),
          column(3,fileInput("file1", "Choose CSV File",
                             multiple = TRUE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")),offset =3)
        ),
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ";"),
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"'),
        dataTableOutput("tableRawdata")

      ),style = "padding: 30px;"),

      div(screen(
        h1("Explore Your Dataset"),

        p("This is a very simple shinyglide application."),
        p("Please click on Next to go to the next screen."),
        plotlyOutput("plotRawdata"),
        selectInput("plotRawdataType","Visualization",c("values","Dev from mean","Dev from median"))
      ),style = "padding: 30px;"),


      div(screen(
        h1("Hunting Your Spatial Correlation"),
        h4("Inputs for Deterministic Methods (Baseline, Nearest Neighbour (NN), Inverse Distance Weighting (IDW))"),
        div("Logtransform Value",checkboxInput("logTrans",NULL, FALSE)),
        div("Number of neighbours (IDW)",sliderInput("idwNoN",NULL,min = 2, max = 10, value = c(1,10),step=1)),
        div("Elevation Factor (anisotropy; IDW and NN)",sliderInput("idwEF",NULL,min = 1, max = 10, value = c(1,10),step=1)),
        div("Power Parameter (IDW)",numericInput("idwPower",NULL, 2)),
        actionButton("deterRun","Run Interpolation!"),

        plotlyOutput("output$rmsePlot"),
        dataTableOutput("tablePredictions")


      ),style = "padding: 30px;"),


      div(screen(
        h1("Runing Ordinary Kriging Interpolation"),
        checkboxInput("krigLogTrans","Logtransform Value", FALSE),
        numericInput("krigPsill","Partial Sill", 0.6),
        numericInput("krigRange","Range",500),
        numericInput("krigNug","Nugget", 0.01),
        selectInput("krigModel","Choose Interpolation Method",
                    list("Spherical" = "Sph",
                         "Gausian" = "Gau",
                         "Exponential" = "Exp",
                         "Nugget" = "Nug")),
        plotOutput("varPlot"),
        dataTableOutput("tableInterpolants")
      ),style = "padding: 30px; align: center;"),



      screen(
        p("Please choose a value."),
        numericInput("n", "n", value = 10, min = 10)
      ),

      screen(
        p("And here is the result."),
        plotOutput("plot")
      )
    )
  )
)

server = function(input, output, session) {

  ###### IMport Data #####

  vgm()

  inputDatasetRaw = reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    #
    #     if(is.null(input$file1)) return()
    #
    #     # when reading semicolon separated files,
    #     # having a comma separator causes `read.csv` to error
    #     tryCatch(
    #       {
    #         df <- read.csv(input$file1$datapath,
    #                        header = input$header,
    #                        sep = input$sep,
    #                        quote = input$quote)
    #         names(df)=c("x","y","z","value")
    #       },
    #       error = function(e) {
    #         # return a safeError if a parsing error occurs
    #         stop(safeError(e))
    #       }
    #     )
    #
    #     browser()
    #     saveRDS(df,"df.rds")
    data(meuse)
    df = meuse %>%
      dplyr::rename(z = elev,
                    value = zinc) %>%
      select(x,y,z,value)

    #df = readRDS("df.rds")
    return(df)

  })

  inputDataset = reactive({

    inputDataset = inputDatasetRaw()

    if(input$logTrans) mutate(inputDataset, value = log(value))

    return(inputDataset)

  })


  coordDatasetKrig = reactive({

    if(is.null(inputDataset())) return()

    coordDataset = inputDataset()
    coordinates(coordDataset) = ~x+y+z

    return(coordDataset)

  })

  output$tableRawdata <- renderDataTable({

    inputDataset()

  })

  output$plotRawdata = renderPlotly({

    data3Draw = inputDatasetRaw()

    plot_ly() %>%
      add_markers(x = data3Draw$x, y = data3Draw$y, z = data3Draw$z,
                  marker = list(size=10,color = data3Draw$value, colorscale = c('blue', '#683531'),
                                showscale = TRUE))  %>%
      layout(scene = list(xaxis = list(title = 'x'),
                          yaxis = list(title = 'y'),
                          zaxis = list(title = 'z')))

  })


  ###### ANALYZE Data #####

  interpolants = reactive({

    inp = inputDatasetRaw()

    grid3D <- expand.grid(x = seq(from = min(inp$x), to = max(inp$x), length = 10),
                          y = seq(from = min(inp$y), to = max(inp$y), length = 10),
                          z = seq(from = min(inp$z), to = max(inp$z), length = 10))
    gridded(grid3D) = ~x+y+z

    return(grid3D)

  })


  varKrig = reactive({

    input = coordDatasetKrig()
    varKrig = variogram(value~1, input)

    return(varKrig)

  })

  vgmKrig = reactive({

    if(input$krigNug ==0) nugget = NA else nugget = input$krigNug

    vgm(psill = input$krigPsill,
        model = input$krigModel,
        range = input$krigRange,
        nugget = nugget)

  })

  output$varPlot = renderPlot({

    plot(varKrig(), pl=T, model=vgmKrig())

  })

  #cvKrig = eventReactive(input$deterRun,{
  cvKrig = reactive({

    predicted = NULL
    var = NULL
    coordDatasetRaw = coordDatasetKrig()

    for(i in 1:nrow(coordDatasetRaw)){#i = 1 i=2

      print(i)
      obs = as.data.frame(coordDatasetRaw)[i,]
      predictedCoord = data.frame(x = obs$x, y = obs$y, z = obs$z)
      coordinates(predictedCoord) = ~x+y+z

      coordDataset = coordDatasetRaw[-i,]

      predictTemp = krige0(formula = value ~ 1, coordDataset, predictedCoord,
                           model = vgmKrig(),nsim=1, computeVar = T)
      predicted = c(predicted,unlist(predictTemp$pred))
      var = c(var, unlist(predictTemp$var))
    }

    rmse = rmse(coordDatasetRaw$value,predicted)

    return(list("prediction" = predicted,"rmse" = rmse))

  })


  #cvDetermin = eventReactive(input$deterRun,{
  cvDetermin = reactive({

    browser()

    #TODO: write test to see if maxNoN is greater than total number of neighbours
    results = interpolate$interpolateCV(inputDataset = inputDataset(),
                                        #must have following columns: x, y, z, value
                                        inputNeighboursAll = input$idwNoN,
                                        inputElevationFactorAll = input$idwEF,
                                        idwPowerInput = input$idwPower)

  })

  rmseAll = reactive({

    rmseDeter = cvDetermin()
    rmseKrig = data.frame("EF" = NA,
                          "N" = NA,
                          "method" = "Ordinary kriging",
                          "RMSE" = cvKrig()$rmse,
                          "R2" = NA)

    rmseAll = rmseDeter$baselineRmse %>%
      dplyr::bind_rows(rmseDeter$nnRmse) %>%
      dplyr::bind_rows(rmseDeter$idwRmse) %>%
      dplyr::bind_rows(rmseKrig) %>%
      mutate(descrip = paste0(method," (EF: ",EF,", N: ",N,")"),
             method = factor(descrip,levels = c("Baseline","Nearest Neighbour","Inverse Distane Weighting","Ordinary kriging"))) %>%
      arrange(method)

    rmseAll$descrip = factor(rmseAll$descrip,levels = unique(rmseAll$descrip))

    return(rmseAll)

  })

  output$rmsePlot = renderPlotly({

    #interpolate = use("modules/interpolate.R")
    browser()

    rmseAll = rmseAll()


    m <- list(
      l = 100,
      r = 50,
      b = 50,
      t = 50,
      pad = 4
    )

    f <- list(
      size = 18,
      color = "#7f7f7f"
    )

    plot_ly(rmseAll, x = ~RMSE, y = ~descrip, type = 'bar', orientation = 'h',
            color = ~method,showlegend = F,
            marker = list(#color = ~as.factor(rmseAll$method),#'rgba(246, 78, 139, 0.6)',
              line = list(color = 'black',
                          width = 1)
            ))%>%
      layout(margin = m,
             yaxis = list(title = "",tickfont = list(size = 8)),
             xaxis = list(title = "RMSE", titlefont = list(size = 12), tickfont = list(size = 8)),
             legend = list(orientation = 'h'))



  })

  cvPredictions = reactive({

    rmseAll = rmseAll() %>%
      group_by(method) %>%
      arrange(RMSE) %>%
      top_n(1)
    predDeter = cvDetermin()
    inputDatasetRaw = inputDatasetRaw()

    predKrig = cvKrig()$pred

    predictions = tibble("Observed Value" = inputDatasetRaw$value,
                         "Ordinary Kriging" = predKrig,
                         "Inverse Distance Weighting" = predDeter$idwResults$predicted,
                         "Nearest neighbour" = predDeter$nnResults$predicted,
                         "Baseline (mean)" = predDeter$baselineResults$predicted,
                         "x" = inputDatasetRaw$x,
                         "y" = inputDatasetRaw$y,
                         "z" = inputDatasetRaw$z)

  })

  output$tablePredictions = renderDataTable({



    rmseKrig = data.frame("EF" = NA,
                          "N" = NA,
                          "method" = "Ordinary kriging",
                          "RMSE" = cvKrig()$pred,
                          "R2" = NA)

  })


  krigEstimate = reactive({

    estKrig <- krige0(formula = value ~ 1, coordDatasetKrig(), interpolants(),
                      model = vgmKrig(),nsim=1, computeVar = T)

    return(unlist(estKrig$pred))

  })

  deterEstimate = reactive({

    inputDataset = inputDataset()
    interpolants = interpolants()
    rmseAll = rmseAll()

    baselinePred = mean(inputDataset$value)
    nnPred = interpolateDataset(referenceDataset = inputDataset,
                                interpolantCoordinates = interpolants,
                                inputNeighbours = unlist(rmseAll[rmseAll$method=="Nearest Neighbour","N"]),
                                inputElevationFactor =  unlist(rmseAll[rmseAll$method=="Nearest Neighbour","EF"]),
                                idwPowerInput = input$idwPower)

    idwPred = interpolateDataset(referenceDataset = inputDataset,
                                 interpolantCoordinates = interpolants,
                                 inputNeighbours = unlist(rmseAll[rmseAll$method=="Inverse Distance Weighting","N"]),
                                 inputElevationFactor =  unlist(rmseAll[rmseAll$method=="Inverse Distance Weighting","EF"]),
                                 idwPowerInput = input$idwPower)

    browser()

  })
}

shinyApp(ui, server)
