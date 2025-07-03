#created inputs DSSAT

library(data.table)

PEDON <- fread("data/initial_condition_soils_info.csv")

# Seleccionar PEDON únicos por estacion
SELECT_PEDON <- PEDON[, .SD[1], by = .(name, PEDON)][, .(name, PEDON, PERFIL)]

# creat dssat_code for each station
SELECT_PEDON[, dssat_code := fcase(
  name == "Treinta y Tres", "UYTT",
  name == "Trinidad G4", "UYTR",
  name == "Palmitas (TP)", "UYPA",
  name == "Florida G3", "UYFL",
  name == "La Estanzuela", "UYLE",
  name == "Young G3", "UYYO",
  name == "Mercerdes G3", "UYME",  
  default = name  # Para mantener cualquier otro valor sin cambio
  )]

# read management data
MANEJO <- fread("outputs/manejo.csv", encoding = "Latin-1")


ROTACIONES <- read.csv("outputs/ROTACIONES_ESTACION.csv")
setDT(ROTACIONES)
head(ROTACIONES)
str(ROTACIONES)

# Crear tabla base
BASE <- data.table(
  CODE = c("UYLE", "UYTT", "UYME", "UYFL", "UYYO", "UYTR", "UYPA")
)

BASE <- merge(BASE, SELECT_PEDON, by = "CODE", all.x = TRUE)
head(BASE)

# Hacer el join 
BASE_ROT <- ROTACIONES[BASE, on = "CODE", allow.cartesian = TRUE]

# Hacer el join
BASE_ROT_MANEJO <- MANEJO[BASE_ROT, on = "CODE", allow.cartesian = TRUE]

# Agregar una columna nueva sin datos
BASE_ROT_MANEJO[, INGENIO := NA]
setnames(BASE_ROT_MANEJO, "PEDON", "ID_SOIL")

imputs_dssat <- BASE_ROT_MANEJO

#definir la ruta de exportación 
ruta_exportacion <- "outputs/inputs_dssat.csv" 

#exportar la tabla como archivo CSV
write.csv(imputs_dssat, file = ruta_exportacion, row.names = FALSE)
