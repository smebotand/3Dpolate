
server = function(input, output, session) {


  ###### Constructing Buttons and Intro #####

  source("server/serverRenderButtonsIntro.R", encoding = "UTF-8", local = TRUE)


  ###### Load Data #####

  inputDatasetRaw = reactive({

    if(is.null(input$testData)) return()

    if(input$testData) return(testData)

        if(is.null(input$file1)) return()

        tryCatch(
          {
            df <- read.csv(input$file1$datapath,
                           header = input$importHeader,
                           sep = input$importSep,
                           quote = '"')
            names(df)=c("x","y","z","value")
          },
          error = function(e) {
            stop(safeError(e))
          }
        )

        return(df)

  })

  output$tableRawdata <- renderDataTable({

    if(is.null(inputDatasetRaw())) return()

    inputDatasetRaw()

  })

  output$plotRawdata = renderPlotly({

    if(is.null(inputDatasetRaw())) return()

    data3Draw = inputDatasetRaw()

    plot_ly() %>%
      add_markers(x = data3Draw$x, y = data3Draw$y, z = data3Draw$z,
                  marker = list(size=5,color = data3Draw$value, colorscale = "RdBlu",
                                showscale = TRUE))  %>%
      layout(scene = list(camera = list(eye = list(x = 0.5, y = -2, z = 0.5),
                                        up = list(x = 0, y = 1, z = 0.5)),
                          xaxis = list(title = 'x'),
                          yaxis = list(title = 'y'),
                          zaxis = list(title = 'z')))

  })


  ###### Run INterpolation #####

  inputDataset = reactive({

    if(is.null(inputDatasetRaw())) return()

    inputDataset = inputDatasetRaw()

    if(input$logTrans) mutate(inputDataset, value = log(value))

    return(inputDataset)

  })


  coordDatasetKrig = reactive({

    if(is.null(inputDatasetRaw())) return()

    coordDataset = inputDataset()
    coordinates(coordDataset) = ~x+y+z

    return(coordDataset)

  })


  interpolants = reactive({

    if(is.null(inputDataset())) return()

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

    if(is.null(input$krigPsill)) return()

    if(input$krigNug ==0) nugget = NA else nugget = input$krigNug

    vgm(psill = input$krigPsill,
        model = input$krigModel,
        range = input$krigRange,
        nugget = nugget)

  })

  output$variogramPlot = renderPlot({

    plot(varKrig(), pl=T, model=vgmKrig())

  })

  cvKrig = eventReactive(input$interRun,{

    if(is.null(inputDataset())) return()

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
      sd = c(var, sqrt(unlist(predictTemp$var)))
    }

    rmse = rmse(coordDatasetRaw$value,predicted)

    return(list("prediction" = predicted,"rmse" = rmse))

  })


  cvDetermin = eventReactive(input$interRun,{

    if(is.null(inputDataset())) return()

    #TODO: write test to see if maxNoN is greater than total number of neighbours
    results = interpolate$interpolateCV(inputDataset = inputDataset(),
                                        #must have following columns: x, y, z, value
                                        inputNeighboursAll = input$idwNoN,
                                        inputElevationFactorAll = input$idwEF,
                                        idwPowerInput = input$idwPower)

    # saveRDS(cvDetermin(),"results.rds")
    #results = readRDS("results.rds")

    return(results)

  })

  rmseAll = reactive({

    if(is.null(inputDataset())) return()

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
             method = factor(method,levels = c("Baseline","Nearest Neighbour","Inverse Distance Weighting","Ordinary kriging"))) %>%
      arrange(method)

    rmseAll$descrip = factor(rmseAll$descrip,levels = unique(rmseAll$descrip))

    return(rmseAll)

  })

  output$rmsePlot = renderPlotly({

     if(!input$showRmsePlot) return()

    if(is.null(rmseAll())) return()

    rmseAll = rmseAll()

    m <- list(
      l = 150,
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
             yaxis = list(title = "",tickfont = list(size = 12)),
             xaxis = list(title = "RMSE", titlefont = list(size = 12), tickfont = list(size = 8)),
             legend = list(orientation = 'h'))



  })

  observeEvent(input$showVar,{

    showModal(modalDialog(
      plotOutput("variogramPlot"),
      title = "Semivariogram Plot and Fit of Variogram Model",
      easyClose = TRUE
    ))
  })

  observeEvent(input$showDeterHelp,{

    showModal(modalDialog(
      div(img(src = "deterHelp.png", height = "500vh"),align = "center"),
      title = "Deterministic Interpolation Tips",
      easyClose = TRUE,
      size = "l"
    ))
  })

  observeEvent(input$showKrigHelp,{

    showModal(modalDialog(
      img(src = "krigHelp.png", height = "600vh"),
      title = "Kriging Tips",
      easyClose = TRUE,
      size = "l"
    ))
  })

  cvPredictions = reactive({

    if(is.null(rmseAll())) return()

    rmseAll = rmseAll() %>%
      group_by(method) %>%
      arrange(RMSE) %>%
      top_n(1)
    predDeter = cvDetermin()
    inputDataset = inputDataset()

    predKrig = cvKrig()$pred

    predictions = tibble("Observed Value" = inputDataset$value,
                         "Ordinary Kriging" = predKrig,
                         "Inverse Distance Weighting" = predDeter$idwResults$predicted,
                         "Nearest neighbour" = predDeter$nnResults$predicted,
                         "Baseline (mean)" = predDeter$baselineResults$predicted,
                         "x" = inputDataset$x,
                         "y" = inputDataset$y,
                         "z" = inputDataset$z)

    return(predictions)

  })

  output$tableCvPredictions = renderDataTable({

    if(is.null(rmseAll())) return()

    cvPredictions()

  })


  ###### View Results #####

  krigEstimate = reactive({

    if(is.null(rmseAll())) return()

    estKrig <- krige0(formula = value ~ 1, coordDatasetKrig(), interpolants(),
                      model = vgmKrig(),nsim=1, computeVar = T)

    df = data.frame("krig" = unlist(estKrig$pred))

    return(df)

  })

  deterEstimate = reactive({

    if(is.null(rmseAll())) return()

    inputDataset = inputDataset()
    interpolants = interpolants()
    rmseAll = rmseAll()

    baselinePred = mean(inputDataset$value)

    nnPred = interpolate$interpolateDataset(referenceDataset = inputDataset,
                                interpolantCoordinates = interpolants,
                                inputNeighbours = unlist(rmseAll[rmseAll$method=="Nearest Neighbour","N"])[1],
                                inputElevationFactor =  unlist(rmseAll[rmseAll$method=="Nearest Neighbour","EF"])[1],
                                idwPowerInput = input$idwPower)

    idwPred = interpolate$interpolateDataset(referenceDataset = inputDataset,
                                 interpolantCoordinates = interpolants,
                                 inputNeighbours = unlist(rmseAll[rmseAll$method=="Inverse Distance Weighting","N"])[1],
                                 inputElevationFactor =  unlist(rmseAll[rmseAll$method=="Inverse Distance Weighting","EF"])[1],
                                 idwPowerInput = input$idwPower)

    df = data.frame("baseline" = rep(baselinePred,nrow(as.data.frame(interpolants))),
                    "nn" = nnPred$predicted,
                    "idw" = idwPred$predicted)

    return(df)

  })

  estimates = reactive({

    if(is.null(rmseAll())) return()

    interpolants = as.data.frame(interpolants())
    deterEstimate = deterEstimate()
    krigEstimate = krigEstimate()

    estimates = cbind(interpolants,deterEstimate,krigEstimate)

    return(estimates)

  })

  output$resTable = renderDataTable({

    if(is.null(rmseAll())) return()
    if(!input$resShowTable) return()

    estimates()

  })

  output$resPlot3d = renderPlotly({

    if(is.null(rmseAll())) return()

    estimates = estimates()

    values = estimates[,"idw"]

    plot_ly() %>%
      add_markers(x = estimates$x, y = estimates$y, z = estimates$z,
                  marker = list(size=3,color = values, colorscale = "RdBlu",
                                showscale = TRUE))  %>%
      layout(scene = list(camera = list(eye = list(x = 0.5, y = -2, z = 0.5),
                                        up = list(x = 0, y = 1, z = 0.5)),
                          xaxis = list(title = 'x'),
                          yaxis = list(title = 'y'),
                          zaxis = list(title = 'z')))

  })

  output$resPlotHist = renderPlotly({

    if(is.null(rmseAll())) return()
    if(!input$resShowHist) return()

    estimates = estimates()

    values = c(estimates$baseline,estimates$nn,estimates$idw,estimates$krig)
    method = c(rep("Baseline",nrow(estimates)),
               rep("Nearest neighbour",nrow(estimates)),
               rep("Inverse Distance Weighting",nrow(estimates)),
               rep("Ordinary kriging",nrow(estimates)))



     plot_ly(estimates,x = values, type = "histogram", color = method)

  })



}

