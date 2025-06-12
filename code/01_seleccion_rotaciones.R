#Autores: Franco Rodriguez; Gonzalo Rizzo
#procesamiento de los datos de rotaciones de los cultivos.

#cargando paquetes
library(data.table)
library(tidyverse)

#cargando rotaciones
rota <- read.csv("data/Rotaciones.csv", header = TRUE, sep = ";")
head(rota)
str(rota)
unique(rota$componenterotacion.unificado)

#converciones
rota <- rota %>%
  mutate(
    Superficie = as.numeric(Superficie),
    RotacionId = as.numeric(RotacionId),
    orden.componente = as.numeric(orden.componente),
    PoligonoEstacion = as.character(PoligonoEstacion),  
    componenterotacion.unificado = as.character(componenterotacion.unificado)
  )

#obvservando el numero de valores NA después de convertir
sum(is.na(rota$Superficie))  

#sistematizando la disposicion de la tabla, de manera que queden horizontales las rotaciones
rota_horiz <- rota %>%
  arrange(PoligonoEstacion, RotacionId, orden.componente) %>%  #orden de las columnas
  pivot_wider(
    names_from = orden.componente,  #orden como nombres de columna
    values_from = componenterotacion.unificado,  #poner los cultivos en columnas
    names_prefix = "Cultivo_"  #prefijo para columnas nuevas
  )

head(rota_horiz)

# Delete Pasture
rota_horiz[rota_horiz == "Pastura"] <- NA

#misma sistematizacion pero dejando en una sola columna las rotaciones separando los cultivos con -
rota_horiz_concatenada <- rota_horiz %>%
  mutate(rotaciones = apply(select(., starts_with("Cultivo_")), 1, function(x) {
    paste(na.omit(x), collapse = "-")
  }))

rota_horiz_concatenada <- rota_horiz_concatenada %>%
  select(-starts_with("Cultivo_"))

head(rota_horiz_concatenada)

#comprobando proporcion de area de rotaciones para cada estacion (tiene que dar 100%)
rota_horiz_concatenada[, .(area = sum(Superficie)), .(PoligonoEstacion, rotaciones)][, area_prop := area/sum(area), PoligonoEstacion][][, .(prop = sum(area_prop), .N), PoligonoEstacion]

# estimate aggregated area and proportion
dt <- setDT(rota_horiz_concatenada)
dt2 <- dt[, .(area = sum(Superficie, na.rm = TRUE)), by = .(PoligonoEstacion, rotaciones)][order(PoligonoEstacion, -area)]
dt2 <- dt2[, area_prop := round(area/sum(area, na.rm = TRUE) * 100, 0), by = .(PoligonoEstacion)]
dt2[area_prop > 1]

head(dt2)

#cambiando nombres de las variables de rotaciones (cultivos)
dt3 <- dt %>% mutate(rot_simp = str_replace_all(rotaciones,
                                                c("Cereal Invierno" = "Cereal_Inv",
                                                  "Cobertura" = "Cobertura",
                                                  "Sorgo/Maiz" = "Cereal_Ver",
                                                  "Colza" = "Colza",
                                                  "Soja" = "Soja",
                                                  "Girasol" = "Girasol")))
dt3 <- dt3[, .(area = sum(Superficie, na.rm = TRUE)), by = .(PoligonoEstacion, rot_simp)][order(PoligonoEstacion, -area)]
dt3 <- dt3[, area_prop := area/sum(area, na.rm = TRUE) * 100, by = .(PoligonoEstacion)]

#comprobando area en proporcion
dt3[, .(prop = sum(area_prop), .N), PoligonoEstacion]

#extrayendo cultivos unicos en cada rotacion
dt3[, cultivos_unicos := sapply(strsplit(rot_simp, "-"), function(x) paste(unique(x), collapse = "-"))]

#ordenando cultivos unicos para que sean comparables
dt3[, clave_cultivos := sapply(strsplit(cultivos_unicos, "-"), function(x) paste(sort(x), collapse = "-"))]
head(dt3)
#agrupando por estacion y clave cultivo (y sumando area_prop para obtener la proporcion por cada clave cultivo).
A <- dt3[, .(prop = sum(area_prop), .N), .(PoligonoEstacion, clave_cultivos)]
head(A)

#seleccionando para cada estacion la clave_cultivo de mayor proporcion
B <- A[, .SD[order(-prop)][1:3], by = PoligonoEstacion]
head(B)

#para cada clave_cultivo seleccionada en B, se elige la rot_simp con mayor area_prop
C <- dt3[B, on = .(clave_cultivos, PoligonoEstacion)]
C <- C[, .SD[which.max(area_prop)], by = .(clave_cultivos, PoligonoEstacion)]

head(C)
unique(B$clave_cultivos)
unique(C$clave_cultivos)
unique(C$rot_simp)

#reordenar columnas 
setcolorder(C, c("PoligonoEstacion", "clave_cultivos", "rot_simp", 
                 "prop", "area_prop", "area", "N"))

#definir la ruta de exportación 
ruta_exportacion <- "outputs/C.csv" 

#exportar la tabla como archivo CSV
write.csv(C, file = ruta_exportacion, row.names = FALSE)





#diferencioando rotaciones segun cultivo de primera y cultivo de segunda

#copia de dt3
dt3_corregido <- copy(dt3)

#aplicar condiciones para detectar Soja1
dt3_corregido[, Soja1 := grepl(
  "(Cobertura-Soja|Cereal_Ver-Soja|Soja-Soja)|(^Soja.*(Cereal_Ver|Cobertura))", 
  rot_simp
)]

#aplicar condiciones para detectar Maiz1
dt3_corregido[, Maiz1 := grepl(
  "(Cobertura-Cereal_Ver|Soja-Cereal_Ver|Cereal_Ver-Cereal_Ver)|(^Cereal_Ver.*(Soja|Cobertura))", 
  rot_simp
)]

#aplicar condiciones para detectar Soja2
dt3_corregido[, Soja2 := grepl("(Cereal_Inv-Soja|Colza-Soja)", rot_simp)]

#aplicar condiciones para detectar Maiz2
dt3_corregido[, Maiz2 := grepl("(Cereal_Inv-Cereal_Ver|Colza-Cereal_Ver)", rot_simp)]

#aplicar condiciones para detectar Maiz1 (al principio del string)
dt3_corregido[, Maiz1_1 := grepl("^Cereal_Ver", rot_simp)]

#aplicar condiciones para detectar Soja1 (al principio del string)
dt3_corregido[, Soja1_1 := grepl("^Soja", rot_simp)]
head(dt3_corregido)

#sustituyendo y corrigiendo Maiz y Soja de primera (TRUE) en las columnas Maiz y Soja1 si en las columnas MaizSoja1_1 es TRUE
dt3_corregido[, Soja1 := fifelse(Soja1_1 == TRUE & Soja1 == FALSE, TRUE, Soja1)]
dt3_corregido[, Maiz1 := fifelse(Maiz1_1 == TRUE & Maiz1 == FALSE, TRUE, Maiz1)]

# Ver el resultado corregido
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

#definir la ruta de exportación 
ruta_exportacion <- "outputs/G.csv" 

#exportar la tabla como archivo CSV
write.csv(G, file = ruta_exportacion, row.names = FALSE)





# Filtrar las filas donde 'rotaciones' contiene "Cereal Invierno-Cereal Invierno" y sumar la superficie
prueba <- rota_horiz_concatenada[grepl("Cereal Invierno-Cereal Invierno", rotaciones)]

print(prueba)
unique(prueba$RotacionId)
unique(rotaciones_filtradas$RotacionId)
# Comparar ignorando el orden
identical(sort(unique(prueba$RotacionId)), sort(unique(rotaciones_filtradas$RotacionId)))

# Convertir a data.table si no lo es
setDT(rota)

# Ordenar por RotacionId y orden.componente para asegurar secuencia correcta
setorder(rota, RotacionId, orden.componente)

# Filtrar las rotaciones que tienen "Cereal Invierno" en dos filas consecutivas dentro del mismo RotacionId
rotaciones_filtradas <- rota[, .SD[componenterotacion.unificado == "Cereal Invierno" & 
                                     shift(componenterotacion.unificado, type = "lead") == "Cereal Invierno"], 
                             by = RotacionId]

