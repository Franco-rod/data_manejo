# Autores: Franco Rodriguez; Gonzalo Rizzo
# Aim: procesamiento de los datos de manejo de los cultivos.

# cargando paquetes
library(data.table)

# cargando manejos
mng <- fread("data/manejo_cultivo.csv", encoding = "Latin-1")

# eliminando las columnas N y Rotacion de cultivos anuales
mng <- mng[, !c("Rotaciones"), with = FALSE]

# columna Ventana de siembra y Ventana de cosecha/terminacion en dos fechas de
# tipo IDate
mng[, c("Fecha_inicio_siembra", "Fecha_fin_siembra") := 
         tstrsplit(Ventana_siembra, "-", fixed = TRUE)]

mng[, c("Fecha_inicio_cosecha", "Fecha_fin_cosecha") := 
         tstrsplit(Ventana_cosecha, "-", fixed = TRUE)]

# transformando a formato IDate las fechas de ventanas de siembra y cosecha 
mng[, `:=`(
  Fecha_inicio_siembra = as.IDate(Fecha_inicio_siembra, format = "%d/%m"),
  Fecha_fin_siembra = as.IDate(Fecha_fin_siembra, format = "%d/%m"),
  Fecha_inicio_cosecha = as.IDate(Fecha_inicio_cosecha, format = "%d/%m"),
  Fecha_fin_cosecha = as.IDate(Fecha_fin_cosecha, format = "%d/%m")
  )]

# eliminando las columnas Ventana_siembra y Ventana_cosecha
mng <- mng[, !c("Ventana_siembra", "Ventana_cosecha"), with = FALSE]

# Crear columnas con el promedio de fechas de siembra y cosecha
mng[, Fecha_Siembra := as.IDate((as.numeric(Fecha_inicio_siembra) + as.numeric(Fecha_fin_siembra)) / 2, origin = "1970-01-01")]
mng[, Fecha_Cosecha := as.IDate((as.numeric(Fecha_inicio_cosecha) + as.numeric(Fecha_fin_cosecha)) / 2, origin = "1970-01-01")]

# eliminando las columnas Ventana_siembra y Ventana_cosecha
mng <- mng[, !c("Fecha_inicio_siembra", "Fecha_fin_siembra", "Fecha_inicio_cosecha", "Fecha_fin_cosecha", "Cultivar_dominante"), with = FALSE]

# update names
setnames(mng,
         c("CODE", "densidad", "Fecha_Siembra", "Fecha_Cosecha"),
         c("name_2", "PPOP", "PDAT", "HDAT"))

# definir la ruta de exportación 
ruta_exportacion <- "outputs/manejo.csv" 

# make output directory
dir.create("outputs", showWarnings = FALSE)

# exportar la tabla como archivo CSV
fwrite(mng, file = ruta_exportacion)
