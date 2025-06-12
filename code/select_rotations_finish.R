#Autores: Franco Rodriguez; Gonzalo Rizzo
#procesamiento: datos de rotaciones de los cultivos.

#cargando paquetes necesarios
library(data.table)
library(tidyverse)
library(stringr)

#cargando base de datos de rotaciones
rota <- read.csv("data/Rotaciones.csv", header = TRUE, sep = ";")

# ETAPA 1, REORGANIZACION DE LA BASE DE DATOS-----------------------

#converciones de tipo de datos para cada variable
rota <- rota %>% mutate(Superficie = as.numeric(Superficie),
                        RotacionId = as.numeric(RotacionId),
                        orden.componente = as.numeric(orden.componente),
                        PoligonoEstacion = as.character(PoligonoEstacion),  
                        componenterotacion.unificado = as.character(componenterotacion.unificado))

#sistematizando la disposicion de la tabla, de manera que queden horizontales las rotaciones
rota_horiz <- rota %>% arrange(PoligonoEstacion, RotacionId, orden.componente) %>%  #orden de las columnas
              pivot_wider(names_from = orden.componente,  #orden como nombres de columna
              values_from = componenterotacion.unificado,  #poner los cultivos en columnas
              names_prefix = "Cultivo_")  #prefijo para columnas nuevas

#misma sistematizacion pero dejando en una sola columna las rotaciones separando los cultivos con -
rota_horiz_concatenada <- rota_horiz %>% mutate(rotaciones = apply(select(., starts_with("Cultivo_")), 1, function(x) {
                          paste(na.omit(x), collapse = "-")}))

rota_horiz_concatenada <- rota_horiz_concatenada %>%
                          select(-starts_with("Cultivo_")) #eliminando las columnas individuales de los cultivos

# ETAPA 2: FILTRANDO ROTACIONES POR AREA Y PREPARANDO LA BASE DE DATOS PARA LA SELECCION-------------

#comprobando proporcion de area de rotaciones para cada estacion (tiene que dar 100%)
setDT(rota_horiz_concatenada) #rota_horiz_concatenada a data.table
rota_horiz_concatenada[, .(area = sum(Superficie)),
                      .(PoligonoEstacion, rotaciones)][, area_prop := area/sum(area),
                      PoligonoEstacion][][, .(prop = sum(area_prop), .N), PoligonoEstacion]

#cambiando nombres de las variables de rotaciones (cultivos)
dt <- setDT(rota_horiz_concatenada)
dt3 <- dt %>% mutate(rot_simp = str_replace_all(rotaciones,
                                                c("Cereal Invierno" = "Cereal_Inv",
                                                  "Cobertura" = "Cobertura",
                                                  "Sorgo/Maiz" = "Cereal_Ver",
                                                  "Colza" = "Colza",
                                                  "Soja" = "Soja",
                                                  "Girasol" = "Girasol",
                                                  "Pastura" = "Pastura")))
dt3 <- dt3[, .(area = sum(Superficie, na.rm = TRUE)), by = .(PoligonoEstacion, rot_simp)][order(PoligonoEstacion, -area)]
dt3 <- dt3[, area_prop := area/sum(area, na.rm = TRUE) * 100, by = .(PoligonoEstacion)]
dt3[area_prop > 1]

#comprobando area en proporcion
dt3[, .(prop = sum(area_prop), .N), PoligonoEstacion]

#extrayendo los cultivos unicos de cada rotacion
dt3[, cultivos_unicos := sapply(strsplit(rot_simp, "-"), function(x) paste(unique(x), collapse = "-"))]

#ordenando cultivos unicos para que sean comparables
dt3[, clave_cultivos := sapply(strsplit(cultivos_unicos, "-"), function(x) paste(sort(x), collapse = "-"))]
head(dt3)

# remove unneeded objects
rm(rota, rota_horiz, rota_horiz_concatenada, dt)

# ETAPA 3: METODOS DE SELECCION---------------

## Metodo 1 - Cultivos unicos ----------------

#agrupando por estacion y clave cultivo (y sumando area_prop para obtener la proporcion por cada clave cultivo).
A <- dt3[, .(prop = sum(area_prop), .N), .(PoligonoEstacion, clave_cultivos)]
head(A)

#seleccionando para cada estacion la clave_cultivo de mayor proporcion
B <- A[, .SD[order(-prop)][1:3], by = PoligonoEstacion]
head(B)

#para cada clave_cultivo seleccionada en B, se elige la rot_simp con mayor area_prop
C <- dt3[B, on = .(clave_cultivos, PoligonoEstacion)]
C <- C[, .SD[which.max(area_prop)], by = .(clave_cultivos, PoligonoEstacion)]

#reordenar columnas 
setcolorder(C, c("PoligonoEstacion", "clave_cultivos", "rot_simp", 
                 "prop", "area_prop", "area", "N"))

C[, .(porcentaje = sum(prop), .N), PoligonoEstacion] #comprobando area en proporcion

#exportar archivo
ruta_exportacion <- "outputs/C.csv" #definir la ruta de exportación
write.csv(C, file = ruta_exportacion, row.names = FALSE) #exportar la tabla como archivo CSV

## Metodo 2 - Diferenciacion de cultivos unicos de primera y de segunda ----------------

#copia de dt3
dt3_corregido <- dt3

dt3_corregido[, Soja1 := grepl("(Cobertura-Soja|Cereal_Ver-Soja|Soja-Soja|^Soja.*(Cereal_Ver|Cobertura)|^Soja)", 
                rot_simp)] #aplicar condiciones para detectar Soja1

dt3_corregido[, Soja1_1 := grepl("^Soja", 
                rot_simp)] #aplicar condiciones para detectar Soja1 (al principio del string)

dt3_corregido[, Soja2 := grepl("(Cereal_Inv-Soja|Colza-Soja)", 
                rot_simp)] #aplicar condiciones para detectar Soja2

dt3_corregido[, Maiz1 := grepl("(Cobertura-Cereal_Ver|Soja-Cereal_Ver|Cereal_Ver-Cereal_Ver|^Cereal_Ver.*(Soja|Cobertura)|^Cereal_Ver)",
                               rot_simp)] #aplicar condiciones para detectar Maiz1

dt3_corregido[, Maiz1_1 := grepl("^Cereal_Ver", 
                rot_simp)] #aplicar condiciones para detectar Maiz1 (al principio del string)

dt3_corregido[, Maiz2 := grepl("(Cereal_Inv-Cereal_Ver|Colza-Cereal_Ver)", 
                rot_simp)] #aplicar condiciones para detectar Maiz2

#sustituyendo y corrigiendo Maiz y Soja de primera (TRUE) en las columnas Maiz y Soja1 si en las columnas MaizSoja1_1 es TRUE
dt3_corregido[, Soja1 := fifelse(Soja1_1 == TRUE & Soja1 == FALSE, TRUE, Soja1)]
dt3_corregido[, Maiz1 := fifelse(Maiz1_1 == TRUE & Maiz1 == FALSE, TRUE, Maiz1)]
head(dt3_corregido)

#generando  clave_cultivos2 
dt3_corregido[, clave_cultivos2 := 
                #reemplazando Soja por Soja1 o Soja2 
                paste(
                  ifelse(Soja1 == TRUE, "Soja1", ifelse(grepl("Soja", clave_cultivos), "Soja", "")),
                  ifelse(Soja2 == TRUE, "Soja2", ""),
                  
                  #reemplazando Cereal_Ver por Cereal_Ver1 o Cereal_Ver2 
                  ifelse(Maiz1 == TRUE, "Cereal_Ver1", ifelse(grepl("Cereal_Ver", clave_cultivos), "Cereal_Ver", "")),
                  ifelse(Maiz2 == TRUE, "Cereal_Ver2", ""),
                  
                  #agregando el resto de los cultivos de clave_cultivos, respetando su secuencia
                  gsub("Soja|Cereal_Ver", "", clave_cultivos),
                  sep = "-"
                )]

#limpiando posibles guiones innecesarios
dt3_corregido[, clave_cultivos2 := gsub("^[-]+|[-]+$", "", clave_cultivos2)]  #eliminar guiones al inicio o al final
dt3_corregido[, clave_cultivos2 := gsub("[-]+", "-", clave_cultivos2)]  #reemplazar guiones multiples por uno solo
head(dt3_corregido)

#agrupando por estacion y clave cultivo2 (y sumando area_prop para obtener la proporcion por cada clave cultivo).
D <- dt3_corregido[, .(prop = sum(area_prop), .N), .(PoligonoEstacion, clave_cultivos2)]
head(D)

#seleccionando para cada estacion la clave_cultivo2 de mayor proporcion
E <- D[, .SD[order(-prop)][1:3], by = PoligonoEstacion]
head(E)

#para cada clave_cultivo2 seleccionada en E, elegir la rot_simp con mayor area_prop
G <- dt3_corregido[E, on = .(clave_cultivos2, PoligonoEstacion)]
G <- G[, .SD[which.max(area_prop)], by = .(clave_cultivos2, PoligonoEstacion)]

#reordenar columnas 
setcolorder(G, c("PoligonoEstacion", "clave_cultivos2", "rot_simp", 
                 "prop", "area_prop", "area", "N", "Soja1", "Soja2", "Maiz1", "Maiz2", "Soja1_1", "Maiz1_1"))

#comprobando area en proporcion
G[, .(porcentaje = sum(prop), .N), PoligonoEstacion]

#exportar archivo 
ruta_exportacion <- "outputs/G.csv" #definir la ruta de exportación
write.csv(G, file = ruta_exportacion, row.names = FALSE) #exportar la tabla como archivo CSV

## Metodo 3 - Crear un identificador para detectar el número de especies de cultivos en la rotación ----------------

# Define regex patterns for Soja1
patterns_soja1 <- c(
  "^Soja.*(Soja|Cereal_Ver|Cobertura)$", # Pattern 1
  "Cobertura-Soja",                      # Pattern 2
  "Cereal_Ver-Soja",                     # Pattern 3
  "Soja-Soja",                           # Pattern 4
  "^Soja.$"                               # Pattern 5
)

# Define regex patterns for Soja2
patterns_soja2 <- c(
  "Cereal_Inv-Soja", # Pattern 1
  "Colsa-Soja"       # Pattern 2
)

# Define regex patterns for Maiz1
patterns_maiz1 <- c(
  "^Cereal_Ver.*(Soja|Cereal_Ver|Cobertura)$", # Pattern 1
  "Cobertura-Cereal_Ver",                      # Pattern 2
  "Cereal_Ver-Cereal_Ver",                     # Pattern 3
  "Cereal_Ver-Cereal_Ver",                     # Pattern 4
  "^Cereal_Ver.$"                              # Pattern 5
)

# Define regex patterns for Maiz2
patterns_maiz2 <- c(
  "Cereal_Inv-Cereal_Ver", # Pattern 1
  "Colsa-Cereal_Ver"       # Pattern 2
)

# Define regex patterns for Cobertura
patterns_cobertura <- "Cobertura"

# Define regex patterns for Cer_inv
patterns_cerinv <- "Cereal_Inv"

# Define regex patterns for Maiz1
patterns_veranos <- "Soja|Cereal_Ver|Girasol"
patterns_inviernos <- "Cobertura|Cereal_Inv|Colza"

# Count occurrences and store them in new columns
dt3[, Soja1 := rowSums(sapply(patterns_soja1, function(p) str_count(rot_simp, p)))]
dt3[, Soja2 := rowSums(sapply(patterns_soja2, function(p) str_count(rot_simp, p)))]
dt3[, Maiz1 := rowSums(sapply(patterns_maiz1, function(p) str_count(rot_simp, p)))]
dt3[, Maiz2 := rowSums(sapply(patterns_maiz2, function(p) str_count(rot_simp, p)))]
dt3[, Cobertura := rowSums(sapply(patterns_cobertura, function(p) str_count(rot_simp, p)))]
dt3[, Cer_Inv := rowSums(sapply(patterns_cerinv, function(p) str_count(rot_simp, p)))]
dt3[, Veranos := rowSums(sapply(patterns_veranos, function(p) str_count(rot_simp, p)))]
dt3[, Inviernos := rowSums(sapply(patterns_inviernos, function(p) str_count(rot_simp, p)))]
dt3[, rot_length := ifelse(Veranos == 0, Inviernos, Veranos)]

# cultivos por año
dt3[, Soja1     := round(Soja1 / rot_length, 1)]
dt3[, Soja2     := round(Soja2 / rot_length, 1)]
dt3[, Maiz1     := round(Maiz1 / rot_length, 1)]
dt3[, Maiz2     := round(Maiz2 / rot_length, 1)]
dt3[, Cobertura := round(Cobertura / rot_length, 1)]
dt3[, Cer_Inv   := round(Cer_Inv / rot_length, 1)]

# create ID unico
dt3[, ID_rot := paste(Soja1, Soja2, Maiz1, Maiz2, Cobertura, Cer_Inv, sep = "-")]

out <- dt3[, .(area_prop_total = sum(area_prop)), .(PoligonoEstacion, ID_rot)]

#seleccionando para cada estacion la clave_cultivo2 de mayor proporcion
out1 <- out[, .SD[order(-area_prop_total)][1:3], by = PoligonoEstacion]
head(out1)

#para cada clave_cultivo2 seleccionada en E, elegir la rot_simp con mayor area_prop
H <- out[out1, on = .(ID_rot , PoligonoEstacion)]
H <- H[, .SD[which.max(area_prop_total)], by = .(ID_rot, PoligonoEstacion)]

# area de cultivo acumulada por la rotación
H[, .(acc_proportion = sum(area_prop_total)), PoligonoEstacion]

# Print results
print(H)

# make the selection of the reference cropping sequence
H <- 
  dt3[H, on = .(PoligonoEstacion, ID_rot)
      ][, .SD[order(-area_prop)][1], by = .(PoligonoEstacion, ID_rot)
        ][, .(PoligonoEstacion, ID_rot, rot_simp, area_prop_total)]

#exportar archivo 
ruta_exportacion <- "outputs/H.csv" #definir la ruta de exportación
write.csv(H, file = ruta_exportacion, row.names = FALSE) #exportar la tabla como archivo CSV
 
unique(H$rot_simp)
