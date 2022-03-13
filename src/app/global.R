#rm(list=ls()) #clean all stuff NATWIP!!!!
#setwd("C:/GIT2/envi-3d-interpolation/src/app")

#Web
library(shiny)
library(DT)
library(shinydashboard)
library(shinyBS)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(shinyhelper)
library(modules)
library(rintrojs)
library(modules)
#library(shinyglide)

#saptial
library(plotly)
library(sf)
library(sp)
library(gstat)

#data
library(Metrics)
library(lattice)
library(purrr)
library(dplyr)
library(magrittr)
library(xlsx)
library(tidyverse)

#options(shiny.error = browser)#type e to see failed code
#options(shiny.error = NULL)#type e to see failed code

options(encoding="UTF-8")

interpolate = use("modules/interpolate.R")

intro <- read.csv2("intro.csv")

data(meuse)
testData = meuse %>%
  rename(value = zinc,
         z = elev) %>%
  select(x,y,z,value)


logoDummyPts = data.frame(x=c(1,1,80,80,1,1,80,80),
                      y=c(9,11,9,11,9,11,9,11),
                      z=c(1,1,1,1,36,36,36,36))

logoPts = readRDS("data/logoPts.rds")


cam.zoom = 2
ver.angle = 0

sceneLogo = list(camera = list(eye = list(x = 0.2, y = -1.2, z = -0.5),
                           up = list(x = 0, y = 1, z = 0.5)),
             xaxis = list(title = '',showticklabels = F),
             yaxis = list(title = '',showticklabels = F),
             zaxis = list(title = '',showticklabels = F))

logoPlot = plot_ly() %>%
  add_markers(x = as.numeric(logoPts$x), y = 9.5 , z = logoPts$z+10,
              showlegend = FALSE,

              marker = list(size=5,
                            colorscale='Portland',
                            color = (1:nrow(logoPts)),
                            showlegend = FALSE))  %>%

  add_markers(x = logoDummyPts$x,
              y = logoDummyPts$y,
              z = logoDummyPts$z,
              opacity = 0,
              marker = list(NULL))  %>%
  layout(legend=list(NULL),
         scene = sceneLogo)



cat(file = stderr(), "Done running global\n")
