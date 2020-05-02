###################################################################################
###################################################################################
library(XML)
library(data.table)
library(RCurl)
library(jsonlite)
library(tidyverse)
library(maptools)
library(RJSONIO)
library(ggplot2)
library(rgeos)
library(rgdal)
library(maptools)
###################################################################################
###################################################################################
covid19jsonurl <- "https://opendata.euskadi.eus/contenidos/ds_informes_estudios/covid_19_2020/opendata/covid19.json"
colTTHH <- c("Araba"="darkgreen", "Bizkaia"="red", "Gipuzkoa"="blue", "Euskadi"="darkgrey")
# Imanol, Open Data Euskadi:
# x.json eta x_bydate.json datu berdinak dituzte baina modu desberdinean antolatuta
###################################################################################
###################################################################################
GetCovid19Metadata <- function(covid19irekiajsonurl){
  print("GetCovid19Metadata")
  covid19json <- fromJSON(getURL(covid19irekiajsonurl, encoding = "UTF-8"), flatten=TRUE)
  items <- NULL
  for(n in 1:length(covid19json$items)){
    l <- covid19json$items[[n]]
    dt <- data.table(url=l$url, name.SPANISH=l$name[["SPANISH"]], name.BASQUE=l$name[["BASQUE"]])
    items <- rbind(items, dt)
  }
  # por fecha
  itemsbydate <- items[ grep("fecha",name.SPANISH) ]
  # totales
  otheritems <- items[ grep("fecha",name.SPANISH, invert=TRUE) ]
  jsondata <- list(lastupdate=covid19json$lastUpdateDate,
                   byDateJsonUrlsAndNames=itemsbydate,
                   jsonUrlsAndNames=otheritems)
  return(jsondata)
}
###################################################################################
###################################################################################
# ANALISIS
###################################################################################
###################################################################################
GetIrekiaCovid19AnalisisData <- function(covid19jsonurl, basquedataname="Egindako analisiak"){
  print("GetIrekiaCovid19AnalisisData")
  jsonlist <- GetCovid19Metadata(covid19jsonurl)
  analisisjson <- jsonlist$jsonUrlsAndNames[grep(basquedataname,name.BASQUE), url]
  analisislist <- fromJSON(getURL(analisisjson, encoding = "latin1"), flatten=TRUE)
  analisisname <- analisislist$name[["SPANISH"]]
  analisislastupdate <- analisislist$lastUpdateDate
  # convert data to data table
  # total
  analisistotaldt <- data.table(Variable=names(analisislist$total), Count=analisislist$total)
  analisistotaldt[, TTHH:=strsplit(Variable, "Count")[[1]][2], by=.(Variable)]
  analisistotaldt[, Variable:=strsplit(Variable, "Count")[[1]][1], by=.(Variable)]
  analisistotaldt[Variable=="positive", Variable:="Positivo"]
  analisistotaldt[Variable=="negative", Variable:="Negativo"]
  analisistotaldt[Variable=="total", Variable:="Total"]
  # byDate
  bydatelistnames <- names(analisislist$byDate)
  somenames <- c("date", "positiveCountEuskadi", "negativeCountEuskadi", "totalCountEuskadi", "positiveCountBizkaia",
                 "negativeCountBizkaia", "totalCountBizkaia", "positiveCountGipuzkoa", "negativeCountGipuzkoa",
                 "totalCountGipuzkoa", "positiveCountAraba", "negativeCountAraba", "totalCountAraba")
  res <- table(sort(bydatelistnames)==sort(somenames))
  if( (length(res)==1) && (names(res)=="TRUE") ){
    analisisbydatedt <- as.data.table(analisislist$byDate)
  }else{
    analisisbydatedt <- NULL
    for(n in 1:length(analisislist$byDate)){
      # l <- analisislist$byDate[[n]]
      # dt <- data.table(Variable=names(l), Count=l)
      # dt[, date:=l["date"]]
      # dt <- dt[Variable!="date"]
      # analisisbydatedt <- rbind(analisisbydatedt, dt)
      analisisbydatedt <- rbind(analisisbydatedt, as.data.table(analisislist$byDate[[n]]))  
    }
  }
  analisisbydatedt <- melt(analisisbydatedt, id.vars="date", variable.name="Variable", value.name="Count")
  analisisbydatedt[, Variable:=as.character(Variable)]
  analisisbydatedt[, TTHH:=strsplit(Variable, "Count")[[1]][2], by=.(Variable)]
  analisisbydatedt[, Variable:=strsplit(Variable, "Count")[[1]][1], by=.(Variable)]
  analisisbydatedt[Variable=="positive", Variable:="Positivo"]
  analisisbydatedt[Variable=="negative", Variable:="Negativo"]
  analisisbydatedt[Variable=="total", Variable:="Total"]
  analisisbydatedt[, weekday:=weekdays(as.Date(date))]
  if(Sys.getlocale(category="LC_TIME")!="es_ES.UTF-8"){
    analisisbydatedt[, weekdaynum:=strftime(as.Date(date), '%u')]
    wdays <- analisisbydatedt[, .(weekday, weekdaynum)]
    wdays <- wdays[!duplicated(wdays)]
    wdays <- wdays[order(weekdaynum)]
    analisisbydatedt[, weekday:=factor(weekday, levels=wdays[,weekday], ordered = TRUE)]
    analisisbydatedt[, weekdaynum:=NULL]
  }else{
    analisisbydatedt[, weekday:=factor(weekday, levels=c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"), ordered = TRUE)]
  }
  return(list(dataname=analisisname,
              lastupdate=analisislastupdate,
              analisistotaldt=analisistotaldt,
              analisisbydatedt=analisisbydatedt))
}
###################################################################################
###################################################################################
PlotCasosPositivosAcumuladosPorTTHH <- function(analisislist, plotTTHH=c("Araba", "Bizkaia", "Gipuzkoa", "Euskadi")){
  # data
  analisisbydatedt <- analisislist$analisisbydatedt
  analisisbydatedt <- analisisbydatedt[TTHH %in% plotTTHH]
  # plot
  dt <- analisisbydatedt[(Variable=="Positivo")]
  dt <- dt[order(date), .(date=as.character(as.Date(date)), PositivosAcumulados=cumsum(Count)), by=.(Variable, TTHH)]
  gg <- ggplot(dt, aes(x=date, y=PositivosAcumulados))
  gg <- gg + geom_bar(stat="identity", aes(fill=TTHH))
  # gg <- gg + geom_line(aes(col=TTHH))
  gg <- gg + theme(legend.position="bottom", axis.text.x = element_text(angle = 90))
  gg <- gg + scale_fill_manual(values=colTTHH)
  gg <- gg + facet_grid(.~TTHH)
  gg <- gg + xlab("Fecha") + ylab("Positivos acumulados")
  return(gg)
}
###################################################################################
###################################################################################
PlotCasosPositivosNuevosPorTTHH <- function(analisislist, plotTTHH=c("Araba", "Bizkaia", "Gipuzkoa", "Euskadi")){
  # data
  analisisbydatedt <- analisislist$analisisbydatedt
  analisisbydatedt <- analisisbydatedt[TTHH %in% plotTTHH]
  # plot
  dt <- analisisbydatedt[(Variable=="Positivo")]
  dt <- dt[, Positivos:=Count]
  gg <- ggplot(dt, aes(x=as.character(as.Date(date)), y=Positivos))
  gg <- gg + geom_bar(stat="identity", aes(fill=TTHH), position="dodge")
  gg <- gg + theme(legend.position="bottom", axis.text.x = element_text(angle = 90))
  gg <- gg + scale_fill_manual(values=colTTHH)
  gg <- gg + xlab("Fecha") + ylab("Positivos nuevos")
  gg <- gg + facet_grid(.~TTHH)
  return(gg)
}
###################################################################################
###################################################################################
PlotCasosPositivosNuevosPorTTHHyPorDiaDeSemana <- function(analisislist, plotTTHH=c("Araba", "Bizkaia", "Gipuzkoa", "Euskadi")){
  # data
  analisisbydatedt <- analisislist$analisisbydatedt
  analisisbydatedt <- analisisbydatedt[TTHH %in% plotTTHH]
  # plot
  dt <- analisisbydatedt[(Variable=="Positivo")]
  dt <- dt[, Positivos:=Count]
  gg <- ggplot(dt, aes(x=weekday, y=Positivos))
  gg <- gg + geom_boxplot(aes(fill=TTHH))
  gg <- gg + theme(legend.position="bottom", axis.text.x = element_text(angle = 90))
  gg <- gg + scale_fill_manual(values=colTTHH)
  gg <- gg + xlab("Día de la semana") + ylab("Positivos nuevos")
  gg <- gg + facet_grid(.~TTHH)
  return(gg)
}
###################################################################################
###################################################################################
PlotPorcentajePositivosPorTTHH <- function(analisislist, plotTTHH=c("Araba", "Bizkaia", "Gipuzkoa", "Euskadi")){
  # data
  analisisbydatedt <- analisislist$analisisbydatedt
  analisisbydatedt <- analisisbydatedt[TTHH %in% plotTTHH]
  dt <- as.data.table(dcast(analisisbydatedt, date+TTHH~Variable, value.var = "Count"))
  dt[, Total:=Positivo+Negativo]
  dt[, PorcentajePositivos:=100*Positivo/Total]
  # plot
  gg <- ggplot(dt, aes(x=as.character(as.Date(date)), y=PorcentajePositivos))
  gg <- gg + geom_point(aes(col=TTHH))
  gg <- gg + geom_line(aes(col=TTHH, group=TTHH))
  gg <- gg + theme(legend.position="bottom", axis.text.x = element_text(angle = 90))
  gg <- gg + scale_color_manual(values=colTTHH)
  gg <- gg + xlab("Fecha") + ylab("Porcentaje de tests positivos")
  gg <- gg + facet_grid(.~TTHH)
  return(gg)
}
###################################################################################
###################################################################################
# HOSPITALIZACIONES
###################################################################################
###################################################################################
GetIrekiaCovid19HospitalizacionesData <- function(covid19jsonurl, spanishdataname="Hospital"){
  print("GetIrekiaCovid19HospitalizacionesData")
  jsonlist <- GetCovid19Metadata(covid19jsonurl)
  hospitaljson <- jsonlist$jsonUrlsAndNames[grep(spanishdataname,name.SPANISH), url]
  hospitallist <- fromJSON(getURL(hospitaljson, encoding = "UTF-8"), flatten=TRUE, encoding = "UTF-8")
  hospitalname <- hospitallist$name[["SPANISH"]]
  hospitallastupdate <- hospitallist$lastUpdateDate
  ###############################
  # convert data to data table
  # byDate
  bydatelistnames <- names(hospitallist$byDate)
  somenames <- c("date", "items", "total.floorPeopleCount", "total.icuPeopleCount", "total.totalPeopleCount",
                 "total.icuReleasedPeopleCount", "total.releasedPeopleCount")
  res <- table(sort(bydatelistnames)==sort(somenames))
  if( (length(res)==1) && (names(res)=="TRUE") ){
    hospitalbydatedt <- NULL
    for(n in 1:length(hospitallist$byDate$date)){
      dt <- as.data.table(hospitallist$byDate$items[[n]])
      dt[, date:=hospitallist$byDate$date[[n]]]
      hospitalbydatedt <- rbind(hospitalbydatedt, melt(dt, id.vars=c("date","hospital")))
    }
    hospitalbydatedt <- hospitalbydatedt[value>-1]
  }else{
    hospitalbydatedt <- NULL
    for(n in 1:length(hospitallist$byDate)){
      l <- hospitallist$byDate[[n]]
      # items: by date
      for(i in 1:length(l$items)){
        dt <- as.data.table(l$items[[i]])
        dt[, date:=l$date]
        hospitalbydatedt <- rbind(hospitalbydatedt, melt(dt, id.vars=c("date","hospital")))
      }
    }
    hospitalbydatedt <- hospitalbydatedt[value>-1]
  }
  hospitalbydatedt[grep("Ã<81>LAVA", hospital), hospital:=gsub("Ã<81>LAVA","ÁLAVA", hospital)]
  hospitalbydatedt[grep("ASUNCIÃ“N", hospital), hospital:=gsub("ASUNCIÃ“N","ASUNCIÓN", hospital)]
  ###############################
  # metadata
  md <- hospitallist$metaData
  mdnames <- names(md)
  somenames <- c("id.id", "name.SPANISH", "name.BASQUE" )
  res <- table(sort(mdnames)==sort(somenames))
  if( (length(res)==1) && (names(res)=="TRUE") ){
    hospitalvars <- as.data.table(hospitallist$metaData)
    hospitalvars[, resumen:=name.SPANISH]
    hospitalvars[id.id=="floorPeopleCount", resumen:="Ingresos planta"]
    hospitalvars[id.id=="icuPeopleCount", resumen:="Ingresos UCI"]
    hospitalvars[, id:=id.id]
    hospitalvars <- hospitalvars[, .(id, name.SPANISH, resumen)]
  }else{
    hospitalvars <- NULL
    for(i in 1:length(md)){
      hospitalvars <- rbind(hospitalvars,
                            data.table(id=md[[i]]$id, data.table(lang=names(md[[i]]$name), value=md[[i]]$name)))
    }
    hospitalvars <- hospitalvars[lang=="SPANISH"]
    hospitalvars[, resumen:=value]
    hospitalvars[id=="floorPeopleCount", resumen:="Ingresos planta"]
    hospitalvars[id=="icuPeopleCount", resumen:="Ingresos UCI"]
  }
  return(list(dataname=hospitalname,
              lastupdate=hospitallastupdate,
              hospitalbydatedt=hospitalbydatedt,
              hospitalvars=hospitalvars,
              hospitals=hospitalbydatedt[, unique(hospital)]))
}
###################################################################################
###################################################################################
PlotHospitalizacionesByDate <- function(hospitallist, hospitales="ALL"){
  hospitalbydatedt <- hospitallist$hospitalbydatedt
  dt <- hospitalbydatedt[value>-1]
  dt <- dt[!grep("OSPITALERATUTAKO", hospital)]
  if( hospitales!="ALL" ){
    dt <- dt[hospital %in% hospitales]
  }
  dt <- dt[variable!="totalPeopleCount"]
  for( var in dt[,unique(variable)] ){
    dt[variable==var, variable:=hospitallist$hospitalvars[id==var, resumen]]
  }
  fechasdt <- data.table(fecha=dt[, unique(as.Date(date))])
  fechasdt[order(fecha), N:=1:.N]
  fechasdt[, fechares:=""]
  fechasdt[N%%5==0, fechares:=as.character(as.Date(fecha))]
  fechasdt[N==1, fechares:=as.character(as.Date(fecha))]
  fechasdt[N==.N, fechares:=as.character(as.Date(fecha))]
  dt[, fecha:=as.Date(date)]
  dt <- merge(dt, fechasdt[,.(fecha, fechares)], by="fecha")
  dt[, fecha:=as.character(fecha)]
  fechasdt <- fechasdt[order(fecha)]
  # plot
  gg <- ggplot(dt, aes(x=fecha, y=value))
  gg <- gg + geom_bar(stat="identity", aes(fill=hospital))
  gg <- gg + theme(legend.position="bottom",
                   # axis.text.x = element_text(angle = 90, size=4),
                   axis.text.x = element_text(angle = 90),
                   legend.text=element_text(size=5),
                   legend.title=element_text(size=6))
  gg <- gg + xlab("Fecha") + ylab("Número")
  gg <- gg + facet_grid(.~variable)
  return(gg)
}
###################################################################################
###################################################################################
PlotHospitalizacionesTotales <- function(hospitallist){
  varcolors = c("Ingresos planta"="orange","Ingresos UCI"="red","Altas UCI"="yellow","Altas Hospitalarias"="green")
  hospitalbydatedt <- hospitallist$hospitalbydatedt
  dt <- hospitalbydatedt[value>-1]
  dt <- dt[!grep("OSPITALERATUTAKO", hospital)]
  dt <- dt[variable!="totalPeopleCount"]
  dt <- dt[, .(Count=sum(value)), by=.(hospital, variable)]
  for( var in dt[,unique(variable)] ){
    dt[variable==var, variable:=hospitallist$hospitalvars[id==var, resumen]]
  }
  # plot
  gg <- ggplot(dt, aes(x=hospital, y=Count))
  gg <- gg + geom_bar(stat="identity", aes(fill=variable), position="dodge")
  gg <- gg + theme(legend.position="bottom", axis.text.x = element_text(angle = 90))
  gg <- gg + xlab("Fecha") + ylab("Número")
  gg <- gg + scale_fill_manual(values=varcolors)
  return(gg)
}
###################################################################################
###################################################################################
# LETALIDAD
###################################################################################
###################################################################################
GetIrekiaCovid19LetalidadData <- function(covid19jsonurl, spanishdataname="letalidad"){
  print("GetIrekiaCovid19LetalidadData")
  jsonlist <- GetCovid19Metadata(covid19jsonurl)
  letalidadjson <- jsonlist$jsonUrlsAndNames[grep(spanishdataname,name.SPANISH), url]
  letalidadlist <- fromJSON(getURL(letalidadjson, encoding = "UTF-8"), flatten=TRUE, encoding = "UTF-8")
  letalidadname <- letalidadlist$name[["SPANISH"]]
  letalidadlastupdate <- letalidadlist$lastUpdateDate
  letalidadnotes <- letalidadlist$notes[["SPANISH"]]
  # convert bydate data to data table
  bydatelistnames <- names(letalidadlist$byDate)
  somenames <- c("date", "items", "total.positiveMenCount", "total.positiveWomenCount", "total.positiveTotalCount",
                 "total.deathMenCount", "total.deathWomenCount", "total.deathCount", "total.menLethalityRate",
                 "total.womenLethalityRate", "total.totalLethalityRate")
  res <- table(sort(bydatelistnames)==sort(somenames))
  if( (length(res)==1) && (names(res)=="TRUE") ){
    # totales
    letalidadtotaldt <- NULL
    # totals <- somenames[somenames!="items"]
    
    # items
    letalidadbyagerangedt <- NULL
    for(n in 1:length(letalidadlist$byDate$date)){
      dt <- as.data.table(letalidadlist$byDate$items[[n]])
      dt[, date:=letalidadlist$byDate$date[[n]]]
      letalidadbyagerangedt <- rbind(letalidadbyagerangedt, melt(dt, id.vars=c("date","ageRange")))
    }
  }else{
    letalidadtotaldt <- NULL
    letalidadbyagerangedt <- NULL
    for(n in 1:length(letalidadlist$byDate)){
      l <- letalidadlist$byDate[[n]]
      # total
      dt <- data.table(variable=names(l$total),value=l$total)
      dt[, date:=l$date]
      letalidadtotaldt <- rbind(letalidadtotaldt, dt)
      # items: by date
      for(i in 1:length(l$items)){
        dt <- as.data.table(l$items[[i]])
        dt[, date:=l$date]
        letalidadbyagerangedt <- rbind(letalidadbyagerangedt, melt(dt, id.vars=c("date","ageRange")))
      }
    }
  }
  # letalidadtotaldt <- letalidadtotaldt[value>-1]
  letalidadbyagerangedt <- letalidadbyagerangedt[value>-1]
  letalidadbyagerangedt[, genero:=variable]
  letalidadbyagerangedt[, genero:=gsub("Count","",genero)]
  letalidadbyagerangedt[, genero:=gsub("positive","",genero)]
  letalidadbyagerangedt[, genero:=gsub("death","",genero)]
  letalidadbyagerangedt[, genero:=gsub("LethalityRate","",genero)]
  letalidadbyagerangedt[, genero:=gsub("women","Mujeres",genero)]
  letalidadbyagerangedt[, genero:=gsub("Men","Hombres",genero)]
  letalidadbyagerangedt[, genero:=gsub("Women","Mujeres",genero)]
  letalidadbyagerangedt[, genero:=gsub("men","Hombres",genero)]
  letalidadbyagerangedt[, genero:=gsub("total","Total",genero)]
  letalidadbyagerangedt[, medida:=variable]
  letalidadbyagerangedt[, medida:=gsub("Count","",medida)]
  letalidadbyagerangedt[, medida:=gsub("Women","",medida)]
  letalidadbyagerangedt[, medida:=gsub("women","",medida)]
  letalidadbyagerangedt[, medida:=gsub("Men","",medida)]
  letalidadbyagerangedt[, medida:=gsub("men","",medida)]
  letalidadbyagerangedt[, medida:=gsub("Total","",medida)]
  letalidadbyagerangedt[, medida:=gsub("total","",medida)]
  letalidadbyagerangedt[, medida:=gsub("positive","Positivos",medida)]
  letalidadbyagerangedt[, medida:=gsub("death","Fallecimientos",medida)]
  letalidadbyagerangedt[, medida:=gsub("LethalityRate","Letalidad",medida)]
  letalidadbyagerangedt[grep("aÃ±os", ageRange), ageRange:=gsub("aÃ±os", "años", ageRange)]
  letalidadbyagerangedt[grep("mÃ¡s", ageRange), ageRange:=gsub("mÃ¡s", "más", ageRange)]
  # metadata
  md <- letalidadlist$metaData
  mdnames <- names(md)
  somenames <- c("id.id", "name.SPANISH", "name.BASQUE" )
  res <- table(sort(mdnames)==sort(somenames))
  if( (length(res)==1) && (names(res)=="TRUE") ){
    letalidadvars <- as.data.table(letalidadlist$metaData)
    letalidadvars[, id:=id.id]
    letalidadvars <- melt(letalidadvars[, .(id, name.SPANISH, name.BASQUE)], id.vars = "id", variable.name = "lang", value.name = "value")
    letalidadvars[, lang:=gsub("name.", "", lang)]
  }else{
    letalidadvars <- NULL
    for(i in 1:length(md)){
      letalidadvars <- rbind(letalidadvars,
                             data.table(id=md[[i]]$id, data.table(lang=names(md[[i]]$name), value=md[[i]]$name)))
    }
  }
  letalidadvars <- letalidadvars[lang=="SPANISH"]
  letalidadvars[, genero:=""]
  letalidadvars[grep("Hombres", value), genero:="Hombres"]
  letalidadvars[grep("Mujeres", value), genero:="Mujeres"]
  letalidadvars[grep("Total", value), genero:="Total"]
  return(list(dataname=letalidadname,
              lastupdate=letalidadlastupdate,
              letalidadtotaldt=letalidadtotaldt,
              letalidadbyagerangedt=letalidadbyagerangedt,
              letalidadvars=letalidadvars,
              letalidadnotes=letalidadnotes))
}
###################################################################################
###################################################################################
PlotLetalidadPorEdadYGenero <- function(fallecidoslist){
  varcolors <- c("Positivos"="red", "Fallecimientos"="black", "Letalidad"="blue")
  letalidadbyagedt <- fallecidoslist$letalidadbyagerangedt
  letalidadbyagedt[, Medida:=factor(medida, levels=c("Positivos", "Fallecimientos", "Letalidad"), ordered=TRUE)]
  letalidadbyagedt[, facet:="Positivos y fallecimientos (número)"]
  letalidadbyagedt[medida=="Letalidad", facet:="Letalidad (porcentaje)"]
  # plot
  gg <- ggplot(letalidadbyagedt, aes(x=ageRange, y=value))
  gg <- gg + geom_bar(stat="identity", aes(fill=Medida), position="dodge")
  gg <- gg + theme(legend.position="bottom", axis.text.x = element_text(angle = 90))
  gg <- gg + xlab("Edad") + ylab("")
  gg <- gg + scale_fill_manual(values=varcolors)
  gg <- gg + facet_grid(facet~genero, scales = "free")
  return(gg)
}
###################################################################################
###################################################################################
# POR MUNICIPIO
# drawing maps with ggplot and sf
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
###################################################################################
###################################################################################
GetIrekiaCovid19MunicipioData <- function(covid19jsonurl, spanishdataname="municipio"){
  print("GetIrekiaCovid19MunicipioData")
  jsonlist <- GetCovid19Metadata(covid19jsonurl)
  Municipiojson <- jsonlist$jsonUrlsAndNames[grep(spanishdataname,name.SPANISH), url]
  Municipiolist <- fromJSON(getURL(Municipiojson, encoding = "latin1"), flatten=TRUE)
  Municipioname <- Municipiolist$name[["SPANISH"]]
  Municipioname <- gsub("euskadi", "Euskadi", Municipioname)
  Municipiolastupdate <- Municipiolist$lastUpdateDate
  Municipionotes <- Municipiolist$notes[["SPANISH"]]
  # convert data to data table
  # byDate
  mainnames <- c("date", "items")
  bydatelistnames <- names(Municipiolist$byDate)
  if( !is.null(bydatelistnames) ){
    res <- table(mainnames %in% bydatelistnames)
  }else{
    res <- list("FALSE"=1)
  }
  if( (length(res)==1) && (names(res)=="TRUE") ){
    # items
    Municipiobydatedt <- NULL
    for(n in 1:length(Municipiolist$byDate$date)){
      dt <- as.data.table(Municipiolist$byDate$items[[n]])
      dt[, date:=Municipiolist$byDate$date[[n]]]
      oldcols <- colnames(dt)
      newcols <- gsub("geoMunicipality.oid.id","id",oldcols)
      newcols <- gsub("geoMunicipality.nameByLang.SPANISH","name",newcols)
      newcols <- gsub("geoMunicipality.nameByLang.BASQUE","BASQUE",newcols)
      setnames(dt, oldcols, newcols)
      dt[, BASQUE:=NULL]
      Municipiobydatedt <- rbind(Municipiobydatedt, dt)
    }
  }else{
    Municipiobydatedt <- NULL
    for(n in 1:length(Municipiolist$byDate)){
      l <- Municipiolist$byDate[[n]]
      # items: by date
      for(i in 1:length(l$items)){
        ll <- l$items[[i]]
        dt <- data.table(id=ll$geoMunicipality$oid,
                         name=ll$geoMunicipality$nameByLang[["SPANISH"]], 
                         positiveCount=ll$positiveCount)
        dt[, date:=l$date]
        Municipiobydatedt <- rbind(Municipiobydatedt, dt)
      }
    }
  }
  # metadata
  md <- Municipiolist$metaData
  mdnames <- names(md)
  somenames <- c("id.id", "name.SPANISH", "name.BASQUE" )
  res <- table(sort(mdnames)==sort(somenames))
  if( (length(res)==1) && (names(res)=="TRUE") ){
    municipiovars <- as.data.table(Municipiolist$metaData)
    municipiovars[, id:=id.id]
    municipiovars <- melt(municipiovars[, .(id, name.SPANISH, name.BASQUE)], id.vars = "id", variable.name = "lang", value.name = "value")
    municipiovars[, lang:=gsub("name.", "", lang)]
  }else{
    municipiovars <- NULL
    for(i in 1:length(md)){
      municipiovars <- rbind(municipiovars,
                             data.table(id=md[[i]]$id, value=md[[i]]$name[["SPANISH"]]))
    }
  }
  return(list(dataname=Municipioname,
              lastupdate=Municipiolastupdate,
              notes=Municipionotes,
              municipiobydatedt=Municipiobydatedt,
              municipiovars=municipiovars))
}
###################################################################################
###################################################################################
GetCodigosPostalesEuskadi <- function(){
  if (!file.exists("./data/codigospostales/Euskadi.xls")){ 
    euscpurl <- "http://eustat.eus/documentos/datos/codigos/Euskadi.xls"
    if (!dir.exists("./data/codigospostales"))
      dir.create("./data/codigospostales",recursive=T)
    download.file(euscpurl, "./data/codigospostales/Euskadi.xls")
  }
  test <- as.data.table(readxl::read_excel("./data/codigospostales/Euskadi.xls", sheet=1, range='A14:C265'))
  # temp = tempfile(fileext = ".xls")
  # download.file(euscpurl, destfile=temp, mode='wb')
  # test <- as.data.table(readxl::read_excel(temp, sheet=1, range='A14:C265'))
  test[, id:=paste0(`Codigo del territorio histórico`,`Código del municipio`)]
  return(test)
}
###################################################################################
###################################################################################
GetEuskalgeoMunicipiosShapeFile <- function(onlyEuskadi=TRUE){
  ## Download shape file and unzip from Euskalgeo
  if (!file.exists("./data/shp/udalerriak/udalerriak.shp")){ 
    udalerriak.url <- "http://www.euskalgeo.net/sites/euskalgeo.net/files/fitxategi-eranskin/udalerriak.zip"
    if (!dir.exists("./data/shp"))
      dir.create("./data/shp",recursive=T)
    download.file(udalerriak.url, "./data/shp/udalerriak.zip")
    unzip("./data/shp/udalerriak.zip", exdir="./data/shp/udalerriak")
  }
  udalerriak.shp <- readOGR("./data/shp/udalerriak/udalerriak.shp")
  ## Change encoding from Windows-1252 to UTF-8
  for ( i in 1:ncol(udalerriak.shp@data))
    udalerriak.shp@data[,i] <- iconv(udalerriak.shp@data[,i], "Windows-1252", "UTF-8")
  # Fix codes in araba (add starting with 0)
  # for (i in 1:nrow(udalerriak.dat))
  #   if (nchar(udalerriak.dat[i,1])==4)
  #     udalerriak.dat[i,1] <- paste0("0",udalerriak.dat[i,1])
  #
  # udalerriak_eae.shp <- udalerriak.shp[udalerriak.shp$ud_kodea %in% udalerriak.dat$Udalerri.kodea,]
  # rownames(udalerriak_eae.shp@data) <- udalerriak_eae.shp$iz_ofizial
  # udalerriak_eae.shp$id <- udalerriak_eae.shp$ud_kodea
  # udalerriak.shp$id <- udalerriak.shp$ud_kodea
  #
  # udalerriak_eae.map <- fortify(udalerriak_eae.shp, region="id")
  udalerriak_eae.map <- as.data.table(fortify(udalerriak.shp, region="ud_kodea"))
  if( onlyEuskadi ){
    # euscpurl <- "http://eustat.eus/documentos/datos/codigos/Euskadi.xls"
    # temp = tempfile(fileext = ".xls")
    # download.file(euscpurl, destfile=temp, mode='wb')
    # test <- as.data.table(readxl::read_excel(temp, sheet=1, range='A14:C265'))
    # test[, id:=paste0(`Codigo del territorio histórico`,`Código del municipio`)]
    # head(test)
    test <- GetCodigosPostalesEuskadi()
    udalerriak_eae.map <- udalerriak_eae.map[ id %in% test[,id]]
    udalerriak_eae.map <- merge(udalerriak_eae.map, test[,.(id,Municipio=`Nombre del municipio`)], by="id")
  }
  return(udalerriak_eae.map)
}
###################################################################################
###################################################################################
PlotPositivosPorMunicipio <- function(municipiolist){
  # drawing maps with ggplot and sf
  # https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
  # http://www.euskalgeo.net/
  municipiobydatedt <- municipiolist$municipiobydatedt
  latestmunicipiobydatedt <- municipiobydatedt[date==max(date)]
  fillscale <- c('0', '1-10', '11-25', '26-50', '51-100', '101-200', '201-1000', '>1000')
  latestmunicipiobydatedt[, positiverange:='0']
  latestmunicipiobydatedt[positiveCount>0, positiverange:='1-10']
  latestmunicipiobydatedt[positiveCount>10, positiverange:='11-25']
  latestmunicipiobydatedt[positiveCount>25, positiverange:='26-50']
  latestmunicipiobydatedt[positiveCount>51, positiverange:='51-100']
  latestmunicipiobydatedt[positiveCount>101, positiverange:='101-200']
  latestmunicipiobydatedt[positiveCount>200, positiverange:='201-1000']
  latestmunicipiobydatedt[positiveCount>1000, positiverange:='>1000']
  latestmunicipiobydatedt[, positiverange:=factor(positiverange, levels=fillscale, ordered=TRUE)]
  municipiossf <- GetEuskalgeoMunicipiosShapeFile(onlyEuskadi=TRUE)
  gg <- ggplot()
  # map
  gg <- gg + geom_map(data=municipiossf, map=municipiossf,
                      aes(x=long, y=lat, group=group, map_id=id),
                      color="#7f7f7f", fill="white", size=0.15)
  # data
  gg <- gg + geom_map(data=latestmunicipiobydatedt, map=municipiossf,
                      aes(map_id=id, fill=positiverange),
                      color="#7f7f7f", size=0.15)
  gg <- gg + scale_fill_brewer(palette="OrRd") + labs(fill = "Número de positivos")
  gg <- gg + theme(legend.position="bottom", axis.text.x = element_text(angle = 90))
  # gg <- gg + geom_map(aes(map_id = id), map = municipiossf, data = municipiossf) 
  # gg <- gg + expand_limits(x = municipiossf$long, y = municipiossf$lat)
  return(gg)
}
###################################################################################
###################################################################################
# POR ZONA DE SALUD
###################################################################################
###################################################################################
GetIrekiaCovid19ZonaDeSaludData <- function(covid19jsonurl, spanishdataname="zonas de salud"){
  print("GetIrekiaCovid19ZonaDeSaludData")
  jsonlist <- GetCovid19Metadata(covid19jsonurl)
  zonadesaludjson <- jsonlist$jsonUrlsAndNames[grep(spanishdataname,name.SPANISH), url]
  zonadesaludlist <- fromJSON(getURL(zonadesaludjson, encoding = "latin1"), flatten=TRUE)
  zonadesaludname <- zonadesaludlist$name[["SPANISH"]]
  zonadesaludlastupdate <- zonadesaludlist$lastUpdateDate
  zonadesaludnotes <- zonadesaludlist$notes[["SPANISH"]]
  # convert data to data table
  # byDate
  mainnames <- c("date", "items")
  bydatelistnames <- names(zonadesaludlist$byDate)
  if( !is.null(bydatelistnames) ){
    res <- table(mainnames %in% bydatelistnames)
  }else{
    res <- list("FALSE"=1)
  }
  if( (length(res)==1) && (names(res)=="TRUE") ){
    # items
    zonadesaludbydatedt <- NULL
    for(n in 1:length(zonadesaludlist$byDate$date)){
      dt <- as.data.table(zonadesaludlist$byDate$items[[n]])
      dt[, date:=zonadesaludlist$byDate$date[[n]]]
      oldcols <- colnames(dt)
      newcols <- gsub("geoRegion.oid.id","id",oldcols)
      newcols <- gsub("geoRegion.nameByLang.SPANISH","name",newcols)
      newcols <- gsub("geoRegion.nameByLang.BASQUE","BASQUE",newcols)
      setnames(dt, oldcols, newcols)
      dt[, BASQUE:=NULL]
      zonadesaludbydatedt <- rbind(zonadesaludbydatedt, dt)
    }
  }else{
    zonadesaludbydatedt <- NULL
    for(n in 1:length(zonadesaludlist$byDate)){
      l <- zonadesaludlist$byDate[[n]]
      # items: by date
      for(i in 1:length(l$items)){
        ll <- l$items[[i]]
        dt <- data.table(id=ll$geoRegion$oid,
                         name=ll$geoRegion$nameByLang[["SPANISH"]], 
                         population=ll$population,
                         positiveCount=ll$positiveCount,
                         positiveRate=ll$positiveRate)
        dt[, date:=l$date]
        zonadesaludbydatedt <- rbind(zonadesaludbydatedt, dt)
      }
    }
  }
  # metadata
  md <- zonadesaludlist$metaData
  mdnames <- names(md)
  somenames <- c("id.id", "name.SPANISH", "name.BASQUE" )
  res <- table(sort(mdnames)==sort(somenames))
  if( (length(res)==1) && (names(res)=="TRUE") ){
    zonadesaludvars <- as.data.table(zonadesaludlist$metaData)
    zonadesaludvars[, id:=id.id]
    zonadesaludvars <- melt(zonadesaludvars[, .(id, name.SPANISH, name.BASQUE)], id.vars = "id", variable.name = "lang", value.name = "value")
    zonadesaludvars[, lang:=gsub("name.", "", lang)]
  }else{
    zonadesaludvars <- NULL
    for(i in 1:length(md)){
      zonadesaludvars <- rbind(zonadesaludvars,
                               data.table(id=md[[i]]$id, value=md[[i]]$name[["SPANISH"]]))
    }
  }
  return(list(dataname=zonadesaludname,
              lastupdate=zonadesaludlastupdate,
              notes=zonadesaludnotes,
              zonadesaludbydatedt=zonadesaludbydatedt,
              zonadesaludvars=zonadesaludvars))
}
###################################################################################
###################################################################################
GetEuskalgeoZonasDeSaludShapeFile <- function(){
  ## Download shape file and unzip from Euskalgeo
  if (!file.exists("./data/shp/zonasdesalud/OsasunEremuak_ZonasSalud_A_2018.shp")){ 
    zonasdesalud.url <- "ftp://ftp.geo.euskadi.eus/cartografia/Salud/Zonas_Salud/OsasunEremuak_ZonasSalud_A_2018.zip"
    if (!dir.exists("./data/shp"))
      dir.create("./data/shp",recursive=T)
    download.file(zonasdesalud.url, "./data/shp/zonasdesalud.zip")
    unzip("./data/shp/zonasdesalud.zip", exdir="./data/shp/zonasdesalud")
  }
  zonasdesalud.shp <- readOGR("./data/shp/zonasdesalud/OsasunEremuak_ZonasSalud_A_2018.shp")
  zonasdesalud.map <- as.data.table(fortify(zonasdesalud.shp, region="ZON_Cod"))
  return(zonasdesalud.map)
}
###################################################################################
###################################################################################
PlotPositivosPorZonasDeSalud <- function(zonadesaludlist){
  # drawing maps with ggplot and sf
  # https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
  # http://www.euskalgeo.net/
  # shapefile
  zonadesaludssf <- GetEuskalgeoZonasDeSaludShapeFile()
  # data
  zonadesaludbydatedt <- zonadesaludlist$zonadesaludbydatedt
  latestzonadesaludbydatedt <- zonadesaludbydatedt[date==max(date)]
  fillscale <- c('0', '1-20', '21-50', '51-100', '101-150', '151-200', '201-250', '>250')
  latestzonadesaludbydatedt[, positiverange:='0']
  latestzonadesaludbydatedt[positiveCount>0, positiverange:='1-20']
  latestzonadesaludbydatedt[positiveCount>20, positiverange:='21-50']
  latestzonadesaludbydatedt[positiveCount>50, positiverange:='51-100']
  latestzonadesaludbydatedt[positiveCount>100, positiverange:='101-150']
  latestzonadesaludbydatedt[positiveCount>150, positiverange:='151-200']
  latestzonadesaludbydatedt[positiveCount>200, positiverange:='201-250']
  latestzonadesaludbydatedt[positiveCount>250, positiverange:='>250']
  latestzonadesaludbydatedt[, positiverange:=factor(positiverange, levels=fillscale, ordered=TRUE)]
  gg <- ggplot()
  # map
  gg <- gg + geom_map(data=zonadesaludssf, map=zonadesaludssf,
                      aes(x=long, y=lat, group=group, map_id=id),
                      color="#7f7f7f", fill="white", size=0.15)
  # data
  gg <- gg + geom_map(data=latestzonadesaludbydatedt, map=zonadesaludssf,
                      aes(map_id=id, fill=positiverange),
                      color="#7f7f7f", size=0.15)
  gg <- gg + scale_fill_brewer(palette="OrRd") + labs(fill = "Número de positivos")
  gg <- gg + theme(legend.position="bottom", axis.text.x = element_text(angle = 90))
  # gg <- gg + geom_map(aes(map_id = id), map = zonadesaludssf, data = zonadesaludssf) 
  # gg <- gg + expand_limits(x = zonadesaludssf$long, y = zonadesaludssf$lat)
  return(gg)
}
###################################################################################
###################################################################################