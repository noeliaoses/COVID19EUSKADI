print("=============================== GLOBALS =========================================================")
print("Source IrekiaCOVID19.R")
source("IrekiaCOVID19.R")

print("COVID19 Open Data EUS url:")
covid19jsonurl <- "https://opendata.euskadi.eus/contenidos/ds_informes_estudios/covid_19_2020/opendata/covid19.json"
print(covid19jsonurl)

print("Datos análisis")
analisisdataname <- "Análisis"
analisislist <- GetIrekiaCovid19AnalisisData(covid19jsonurl, spanishdataname=analisisdataname)

print("Datos hospitales")
hospitalizacionesdataname <- "Hospital"
hospitaleslist <- GetIrekiaCovid19HospitalizacionesData(covid19jsonurl, spanishdataname=hospitalizacionesdataname)
hospitales <- hospitaleslist$hospitals

print("Datos letalidad")
letalidaddataname <- "letalidad"
letalidadlist <- GetIrekiaCovid19LetalidadData(covid19jsonurl, spanishdataname=letalidaddataname)

print("Datos municipios")
pormunicipiodataname <- "municipio"
municipiolist <- GetIrekiaCovid19MunicipioData(covid19jsonurl, spanishdataname=pormunicipiodataname)

print("Datos zonas de salud")
porzonasdesaluddataname <- "zonas de salud"
zonasdesaludlist <- GetIrekiaCovid19ZonaDeSaludData(covid19jsonurl, spanishdataname=porzonasdesaluddataname)
print("=================================================================================================")