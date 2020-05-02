print("=============================== GLOBALS =========================================================")
print("Source IrekiaCOVID19.R")
source("IrekiaCOVID19.R")

print("COVID19 Open Data EUS url:")
covid19jsonurl <- "https://opendata.euskadi.eus/contenidos/ds_informes_estudios/covid_19_2020/opendata/covid19.json"
print(covid19jsonurl)

print("Datos análisis")
analisislist <- GetIrekiaCovid19AnalisisData(covid19jsonurl)

print("Datos hospitales")
hospitaleslist <- GetIrekiaCovid19HospitalizacionesData(covid19jsonurl)
hospitales <- hospitaleslist$hospitals

print("Datos letalidad")
letalidadlist <- GetIrekiaCovid19LetalidadData(covid19jsonurl)

print("Datos municipios")
municipiolist <- GetIrekiaCovid19MunicipioData(covid19jsonurl)

print("Datos zonas de salud")
zonasdesaludlist <- GetIrekiaCovid19ZonaDeSaludData(covid19jsonurl)
print("=================================================================================================")