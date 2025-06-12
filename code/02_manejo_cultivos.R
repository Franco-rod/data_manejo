##Autores: Franco Rodriguez; Gonzalo Rizzo
#procesamiento de los datos de manejo de los cultivos.

#cargando paquetes
library(data.table)

#cargando manejos
manejo <- fread("data/manejo_cultivo.csv", sep = ",", encoding = "UTF-8", fill = TRUE)
head(manejo)
str(manejo)

#data frame a data.table 
setDT(manejo)

#eliminando las columnas N y Rotacion de cultivos anuales
manejo <- manejo[, !c("Rotaciones"), with = FALSE]

head(manejo)

#columna Ventana de siembra y Ventana de cosecha/terminacion en dos fechas de tipo IDate
manejo[, c("Fecha_inicio_siembra", "Fecha_fin_siembra") := 
         tstrsplit(Ventana_siembra, "-", fixed = TRUE)]

manejo[, c("Fecha_inicio_cosecha", "Fecha_fin_cosecha") := 
         tstrsplit(Ventana_cosecha, "-", fixed = TRUE)]

#transformando a formato IDate las fechas de ventanas de siembra y cosecha 
manejo[, `:=`(
  Fecha_inicio_siembra = as.IDate(Fecha_inicio_siembra, format = "%d/%m"),
  Fecha_fin_siembra = as.IDate(Fecha_fin_siembra, format = "%d/%m"),
  Fecha_inicio_cosecha = as.IDate(Fecha_inicio_cosecha, format = "%d/%m"),
  Fecha_fin_cosecha = as.IDate(Fecha_fin_cosecha, format = "%d/%m")
)]

head(manejo)

#eliminando las columnas Ventana_siembra y Ventana_cosecha
manejo <- manejo[, !c("Ventana_siembra", "Ventana_cosecha"), with = FALSE]

head(manejo)

# Crear columnas con el promedio de fechas de siembra y cosecha
manejo[, Fecha_Siembra := as.IDate((as.numeric(Fecha_inicio_siembra) + as.numeric(Fecha_fin_siembra)) / 2, origin = "1970-01-01")]
manejo[, Fecha_Cosecha := as.IDate((as.numeric(Fecha_inicio_cosecha) + as.numeric(Fecha_fin_cosecha)) / 2, origin = "1970-01-01")]

#eliminando las columnas Ventana_siembra y Ventana_cosecha
manejo <- manejo[, !c("Fecha_inicio_siembra", "Fecha_fin_siembra", "Fecha_inicio_cosecha", "Fecha_fin_cosecha", "Cultivar_dominante"), with = FALSE]

setnames(manejo, "densidad", "PPOP")
setnames(manejo, "Fecha_Siembra", "PDAT")
setnames(manejo, "Fecha_Cosecha", "HDAT")


#definir la ruta de exportación 
ruta_exportacion <- "outputs/manejo.csv" 

#exportar la tabla como archivo CSV
write.csv(manejo, file = ruta_exportacion, row.names = FALSE)



