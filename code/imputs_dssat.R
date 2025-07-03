#created inputs DSSAT

library(data.table)

PEDON <- read.csv("outputs/soils/dssat/initial_condition_soils_info.csv")

head(PEDON)

setDT(PEDON)

# Seleccionar PEDON únicos por estacion
SELECT_PEDON <- PEDON[, .SD[1], by = .(name, PEDON)][, .(name, PEDON, PERFIL)]

unique(SELECT_PEDON$name)

SELECT_PEDON[, name := fcase(
  name == "Treinta y Tres", "UYTT",
  name == "Trinidad G4", "UYTR",
  name == "Palmitas (TP)", "UYPA",
  name == "Florida G3", "UYFL",
  name == "La Estanzuela", "UYLE",
  name == "Young G3", "UYYO",
  name == "Mercerdes G3", "UYME",  
  default = name  # Para mantener cualquier otro valor sin cambio
)]

setnames(SELECT_PEDON, "name", "CODE")

MANEJO <- fread("outputs/manejo_cultivo.csv", encoding = "Latin-1")

setDT(MANEJO)
head(MANEJO)
str(MANEJO)
unique(MANEJO$CODE)

MANEJO[, CODE := fcase(
  CODE == "Treinta y Tres", "UYTT",
  CODE == "Trinidad",        "UYTR",
  CODE == "Palmitas",        "UYPA",
  CODE == "Florida",         "UYFL",
  CODE == "La Estanzuela",   "UYLE",
  CODE == "Young",           "UYYO",
  CODE == "Mercedes",        "UYME",
  default = CODE  # mantiene cualquier otro valor
)]

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
