import(RColorBrewer)
import(Metrics)
import(plotly)
import(dplyr)
import(purrr)
import(stats)


##### Defining functions #####


##### Baseline functions #####

baseline = function(referenceDataset, interpolantCoordinates){
  #calculates dif from mean

  interpolantCoordinates %>%
    mutate(predicted = mean(referenceDataset$value))

}


##### calculating distande between interpolant and neighbour points

euclidDist=function(inputData,interpolant,elevationFactor=1){

  #adjusting z coordinate accord to elevationFactor/anisotropy
  inputElevated=as.numeric(inputData["z"])*elevationFactor
  interpolantElevated=as.numeric(interpolant["z"])*elevationFactor

  #euclidean distance of 3 dim matrix
  dist(matrix(c(inputData[c("x","y")],inputElevated,
                interpolant[c("x","y")],interpolantElevated),
              nrow=2,byrow=T))

}

##### running nearest neighbour to estimate value at interpolant
estimateInterpolant=function(euclidDistInterpolants,reference,neighbours,idwPower=idwPowerInput){

  distancesSorted=sort(euclidDistInterpolants)
  inverseDistance=1/(distancesSorted[1:neighbours])^idwPower
  samplesWeight=inverseDistance/sum(inverseDistance)#normalization

  #picks values to interpolate from
  samplesRows=names(distancesSorted[1:neighbours])
  samplesValues=reference[as.numeric(samplesRows),"value"]

  #interpolate
  samplesContribution=samplesValues*samplesWeight
  interpolantValue=sum(samplesContribution)

  #returns estimate
  return(interpolantValue)
}

##### Predict value for one point at a time - cross-validation

interpolateOnePoint=function(referenceDataset, interpolantCoordinates,inputNeighboursAll,inputElevationFactorAll,idwPowerInput){

  results=NULL

  for(inputNeighbours in inputNeighboursAll){#inputNeighbours = inputNeighboursAll[1]#debugging
    for(inputElevationFactor in inputElevationFactorAll){#inputElevationFactor = inputElevationFactorAll[1]#debugging

      euclidDistInterpolants=apply(referenceDataset,1,euclidDist,interpolant=interpolantCoordinates,elevationFactor=inputElevationFactor)
      names(euclidDistInterpolants)=1:nrow(referenceDataset)

      predicted=estimateInterpolant(euclidDistInterpolants,
                                    reference=referenceDataset,
                                    neighbours=inputNeighbours,
                                    idwPower=idwPowerInput)
      tempRes=c(interpolantCoordinates$x,
                   interpolantCoordinates$y,
                   interpolantCoordinates$z,
                   interpolantCoordinates$value,
                   inputElevationFactor,
                   inputNeighbours,
                   predicted)
      names(tempRes)=c("x","y","z","value","EF","N","predicted")
      results=rbind(results,tempRes)

      print(paste("EF",inputElevationFactor,"N",inputNeighbours,"pred",predicted,"\n"))
    }
  }
  return(as.data.frame(results))
}


##### Loope through a dataset one point at a time to cross-validation

interpolateCV = function(inputDataset,inputNeighboursAll,inputElevationFactorAll,idwPowerInput){

  baselineResults = data.frame()
  nnResults = data.frame()
  idwResults = data.frame()

  for(i in 1:nrow(inputDataset)){#i=1

    referenceDataset = inputDataset[-i,]
    interpolantCoordinates = inputDataset[i,]

    baselineResults=rbind(baselineResults,
                      baseline(referenceDataset, interpolantCoordinates))

    nnResults=rbind(nnResults,
                     interpolateOnePoint(referenceDataset,
                                         interpolantCoordinates,
                                         1,#forceed due to NN
                                         inputElevationFactorAll,
                                         idwPowerInput))

    idwResults=rbind(idwResults,
                     interpolateOnePoint(referenceDataset,
                                         interpolantCoordinates,
                                         inputNeighboursAll,
                                         inputElevationFactorAll,
                                         idwPowerInput))

  }

  #Baseline RMSE
  baselineRmse = baselineResults %>%
    summarise(
      method="Baseline",
      EF=NA,
      N=NA,
      RMSE = rmse(predicted, value)
      ,R2 = cor(predicted, value)^2
    )

  #Nearest Negihbour RMSE
  nnRmse = nnResults %>%
    group_by(EF,N) %>%
    summarise(
      method="Nearest Neighbour",
      RMSE = rmse(predicted, value)
      ,R2 = cor(predicted, value)^2
    )

  nnMinRmse = min(nnRmse$RMSE,na.rm=T)

  nnBestFit = nnRmse %>%
    dplyr::filter(RMSE == nnMinRmse) %>%
    ungroup() %>%
    dplyr::slice(1) #picks first treatment with the lowest RMSE

  nnPred = nnResults %>%
    dplyr::filter(EF == nnBestFit$EF)


  #IDW RMSE
  idwRmse = idwResults %>%
    group_by(EF,N) %>%
    summarise(
      method="Inverse Distance Weighting",
      RMSE = rmse(predicted, value)
      ,R2 = cor(predicted, value)^2
    )

  idwMinRmse = min(idwRmse$RMSE,na.rm=T)

  idwBestFit = idwRmse %>%
    dplyr::filter(RMSE == idwMinRmse) %>%
    ungroup() %>%
    dplyr::slice(1)  #picks first treatment with the lowest RMSE

  idwPred = idwResults %>%
    dplyr::filter(EF == idwBestFit$EF,
                  N == idwBestFit$N)

  return(list("baselineResults" = baselineResults,
              "baselineRmse" = baselineRmse,
              "baselineBestFit" = baselineRmse,
              "nnResults" = nnPred,
              "nnRmse" = nnRmse,
              "nnBestFit" = nnBestFit,
              "idwResults" = idwPred,
              "idwRmse" = idwRmse,
              "idwBestFit" = idwBestFit))
}



interpolateDataset = function(referenceDataset,
                              interpolantCoordinates,
                              inputNeighbours,
                              inputElevationFactor,
                              idwPowerInput){

  idwResults = NULL
  interpolantCoordinatesAll = as.data.frame(interpolantCoordinates)
  interpolantCoordinatesAll$value = 0

  for(i in 1:nrow(interpolantCoordinatesAll)){#i =1

    interpolantCoordinates = interpolantCoordinatesAll[i,]

    idwResults = rbind(idwResults,
                     interpolateOnePoint(referenceDataset,
                                         interpolantCoordinates,
                                         inputNeighbours,
                                         inputElevationFactor,
                                         idwPowerInput))
  }

  return(idwResults)
}

