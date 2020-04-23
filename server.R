library(shiny)
library(data.table)
library(RCurl)
library(jsonlite)
library(tidyverse)
library(RJSONIO)
library(ggplot2)
library(maptools)

options(shiny.maxRequestSize=1000*1024^2) 

shinyServer(function(input, output, session) {
  
  # Objects in this file are defined in each session
  print("server.R")
  source('globals.R', local = TRUE)

#   getCovid19EusMetaData <- function(){
#     jsondata <- GetCovid19Metadata(covid19jsonurl)
#     return(jsondata)
#   }
#   
#   output$lastGeneralUpdate <- renderText({
#     return(getCovid19EusMetaData()[["lastupdate"]])
#   })
  
  #######################################################
  #######################################################
  # ANALISIS TAB
  
  getAnalisisData <- function(){
    return(analisislist)
  }
  
  output$getAnalisisLastUpdate <- renderText({
    return(as.character(as.Date(getAnalisisData()[["lastupdate"]])))
  })

  output$positivosAcumuladosPorTTHH <- renderPlot({
    return(PlotCasosPositivosAcumuladosPorTTHH(getAnalisisData(), input[["selectTTHH"]]))
  })
  
  output$positivosNuevosPorTTHH <- renderPlot({
    return(PlotCasosPositivosNuevosPorTTHH(getAnalisisData(), input[["selectTTHH"]]))
  })
  
  output$positivosNuevospordiadelasemana <- renderPlot({
    return(PlotCasosPositivosNuevosPorTTHHyPorDiaDeSemana(getAnalisisData(), input[["selectTTHH"]]))
  })
  
  output$porcentajePositivosPorTTHH <- renderPlot({
    return(PlotPorcentajePositivosPorTTHH(getAnalisisData(), input[["selectTTHH"]]))
  })
  
  # #######################################################
  # #######################################################
  # HOSPITALIZACIONES TAB

  getHospitalizacionesData <- function(){
    return(hospitaleslist)
  }

  output$getHospitalizacionesLastUpdate <- renderText({
    return(as.character(as.Date(getHospitalizacionesData()[["lastupdate"]])))
  })

  output$plotHospitalizaciones <- renderPlot({
    return(PlotHospitalizacionesByDate(getHospitalizacionesData(), hospitales=input[["selectHospital"]]))
  })

  output$plotHospitalizacionesTotales <- renderPlot({
    return(PlotHospitalizacionesByDate(getHospitalizacionesData(), hospitales=hospitales))
  })

  output$plotComparacionHospitales <- renderPlot({
    return(PlotHospitalizacionesTotales(getHospitalizacionesData()))
  })
  
  #######################################################
  #######################################################
  # LETALIDAD TAB
  
  getLetalidadData <- function(){
    return(letalidadlist)
  }
  
  output$getLetalidadLastUpdate <- renderText({
    return(as.character(as.Date(getLetalidadData()[["lastupdate"]])))
  })
  
  output$plotLetalidadPorEdadYGenero <- renderPlot({
    return(PlotLetalidadPorEdadYGenero(getLetalidadData()))
  })
  
  #######################################################
  #######################################################
  # POR MUNICIPIO
  
  getPorMunicipioData <- function(){
    return(municipiolist)
  }
  
  output$getMunicipioLastUpdate <- renderText({
    return(as.character(as.Date(getPorMunicipioData()[["lastupdate"]])))
  })
  
  output$plotPositivosPorMunicipio <- renderPlot({
    return(PlotPositivosPorMunicipio(getPorMunicipioData()))
  })
  
  #######################################################
  #######################################################
  # POR ZONAS DE SALUD
  
  getPorZonasDeSaludData <- function(){
    return(zonasdesaludlist)
  }
  
  output$getZonasDeSaludLastUpdate <- renderText({
    return(as.character(as.Date(getPorZonasDeSaludData()[["lastupdate"]])))
  })
  
  output$plotPositivosPorZonasDeSalud <- renderPlot({
    return(PlotPositivosPorZonasDeSalud(getPorZonasDeSaludData()))
  })  
})
