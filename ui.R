library(shiny)

# Objects in this file are shared across all sessions in the same R process
source("IrekiaCOVID19.R")
covid19jsonurl <- "https://opendata.euskadi.eus/contenidos/ds_informes_estudios/covid_19_2020/opendata/covid19.json"
hospitalizacionesdataname <- "Hospital"
hospitaleslist <- GetIrekiaCovid19HospitalizacionesData(covid19jsonurl, spanishdataname=hospitalizacionesdataname)
hospitales <- hospitaleslist$hospitals


shinyUI(fluidPage(
  
  # Application title
  titlePanel("COVID-19 EUSKADI"),

  fluidRow(style = "padding-left: 0px; padding-top: 5px; padding-bottom: 5px;",
           column(12,h4("Datos: Open Data Euskadi - Evolución del coronavirus (COVID-19) en Euskadi"))),
  
  tabsetPanel(
    
    ###################################################################################
    ### ANALISIS
    tabPanel("Análisis",
             fluidRow(style = "padding-left: 10px; padding-top: 10px; padding-bottom: 2px;",
                      column(2,"Última actualización: "),
                      column(4,textOutput("getAnalisisLastUpdate")),
                      column(6,selectizeInput(
                        'selectTTHH', 'Elige el/los TTHH a mostrar: ', choices = c("Araba", "Bizkaia", "Gipuzkoa", "Euskadi"), selected=c("Euskadi"), multiple = TRUE
                      ))),
             fluidRow(style = "padding-left: 20px; padding-top: 10px;",
                      column(4,h3("Positivos acumulados:")),
                      column(4,h3("Positivos nuevos:")),
                      column(4,h3("Positivos nuevos por día de la semana:"))#,
                      # column(3,h3("Porcentaje de tests positivos:"))
                      ),
             fluidRow(style = "padding-left: 20px; padding-top: 10px;",
                      column(4,plotOutput("positivosAcumuladosPorTTHH")),
                      column(4,plotOutput("positivosNuevosPorTTHH")),
                      column(4,plotOutput("positivosNuevospordiadelasemana"))#,
                      # column(3,plotOutput("porcentajePositivosPorTTHH"))
                      )
    ),
    
    # ###################################################################################
    # ### HOSPITALIZACIONES
    tabPanel("Hospitalizaciones",
             fluidRow(style = "padding-left: 10px; padding-top: 10px; padding-bottom: 2px;",
                      column(4,"Última actualización: "),
                      column(8,textOutput("getHospitalizacionesLastUpdate"))),
             fluidRow(style = "padding-left: 10px; padding-top: 10px; padding-bottom: 2px;",
                      column(12,selectInput(
                        'selectHospital', label='Elige el hospital a mostrar: ', choices = hospitales, selected=hospitales, multiple = FALSE
                      ))),
             fluidRow(style = "padding-left: 20px; padding-top: 10px;",
                      column(12,plotOutput("plotHospitalizaciones"))),
             fluidRow(style = "padding-left: 20px; padding-top: 10px;",
                      column(6,h3("Hospitalizaciones totales por fecha")),
                      column(6,h3("Comparación por hospital"))),
             fluidRow(style = "padding-left: 20px; padding-top: 10px;",
                      column(6,plotOutput("plotHospitalizacionesTotales")),
                      column(6,plotOutput("plotComparacionHospitales")))
    ),
    
    ###################################################################################
    ### LETALIDAD
    tabPanel("Letalidad",
             fluidRow(style = "padding-left: 10px; padding-top: 10px; padding-bottom: 2px;",
                      column(4,"Última actualización: "),
                      column(8,textOutput("getLetalidadLastUpdate"))),
             fluidRow(style = "padding-left: 20px; padding-top: 10px;",
                      column(12,plotOutput("plotLetalidadPorEdadYGenero")))
    ),
    
    ###################################################################################
    ### MUNICIPIOS
    tabPanel("Municipios",
             fluidRow(style = "padding-left: 10px; padding-top: 10px; padding-bottom: 2px;",
                      column(3,"Última actualización: "),
                      column(9,textOutput("getMunicipioLastUpdate"))),
             fluidRow(style = "padding-left: 20px; padding-top: 10px;",
                      column(3,""),
                      column(6,plotOutput("plotPositivosPorMunicipio")),
                      column(3,""))
    ),
    
    ###################################################################################
    ### ZONAS DE SALUD
    tabPanel("Zonas de salud",
             fluidRow(style = "padding-left: 10px; padding-top: 10px; padding-bottom: 2px;",
                      column(3,"Última actualización: "),
                      column(9,textOutput("getZonasDeSaludLastUpdate"))),
             fluidRow(style = "padding-left: 20px; padding-top: 10px;",
                      column(3,""),
                      column(6,plotOutput("plotPositivosPorZonasDeSalud")),
                      column(3,""))
    )
    
)))