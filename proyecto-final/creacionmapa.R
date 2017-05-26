# Aquí creamos el mapa con el archivo
# indice_marginacion.xls

setwd("C:/Users/Daniela Baltazar/Downloads/federicogarza/Aplicada3")

# Carga las librerías necesarias
x = c("maptools","foreign","RColorBrewer","classInt","pROC",
      "plyr","colorRamps","leaflet","rgdal","rgeos","sp",
      "ggmap", "rgdal", "rgeos", "maptools", "dplyr", 
      "tidyr", "tmap", "tmaptools","leaflet","car","foreign","readxl")
#install.packages(x)
lapply(x, library, character.only = TRUE)

# Leemos la información geográfica a nivel municipal y a nivel estatal
mxgeo = read_shape(file = "info_geo/conjunto_de_datos/areas_geoestadisticas_municipales.shp")
mxgeo.est = read_shape(file = "info_geo/conjunto_de_datos/areas_geoestadisticas_estatales.shp")

# Establecemos los IDS de los municipios
mxgeo$IDMUN = paste0(mxgeo$CVE_ENT,mxgeo$CVE_MUN)

# Cargamos los índices de marginación
X = read_excel("indice_marginacion.xls", col_names = TRUE)

#Quitamos la primer columna (innecesaria)
X = X[,-1]

#Verificamos que los IDS de los datos sean iguales a los IDS de la información
#geográfica
# Vemos en qué son distintos
identical(sort(mxgeo$IDMUN),X$ID_MUN)

# Viendo qué falta pues son distintos
setdiff(sort(mxgeo$IDMUN), X$ID_MUN)

# OBSERVAMOS QUE FALTAN DOS MUNICIPIOS DE Q. ROO
# REVISANDO LOS DATOS ORIGINALES NOS PERCATAMOS QUE 
# LOS RESULTADOS POR LOCALIDAD DEL INEGI NO LOS CONTIENEN
# LOS AGREGAMOS AL ÍNDICE SIMPLEMENTE COMO NA

missing = setdiff(sort(mxgeo$IDMUN), X$ID_MUN)

missing = cbind(missing,c(NA,NA))
colnames(missing) = colnames(X)
X = rbind(X,missing)

# Ahora sí, verificamos que sean iguales los IDS de ambas
# datas

identical(sort(mxgeo$IDMUN),sort(X$ID_MUN))

# Con esto podemos agregar el índice a los datos
# geográficos para crear el mapa

# Antes volvemos númerico los índices
X$IND_MARG = sapply(X$IND_MARG, as.numeric)

# Combinamos los datos
# No tenemos que ordenarlos, append_data encuentra el match
munmap = append_data(mxgeo, X,
                      key.shp = "IDMUN", key.data="ID_MUN")

# Con munmap podemos elbarorar el mapa de marginación por municipios

# Utilizaremos tmap de R


# Creamos el mapa
# Mapa normal
mun.map.p = tm_shape(munmap)+
  tm_fill("IND_MARG",title = "",
          style="cont",
          palette = "PuRd", 
          contrast = c(0.2, 0.83)) + 
  tm_borders(col="gray50",
             alpha=.5)+
  tm_credits("Elaboración propia con datos de INEGI", align = "left",
             position = c("left","bottom"))+
  tm_layout(title="Índice de marginación \n por municipio",
            title.position = c("right", "top"), 
            legend.position = c(.73,.55), frame=FALSE)+
  tm_shape(mxgeo.est)+ 
  tm_borders(col = "black",
             lwd = 1,
             alpha=.5)
save_tmap(mun.map.p,"marginacion_municipios_p.jpg",width=1920, height=1080)
# Cambiamos a modo interactivo
ttm()

mun.map = tm_shape(munmap)+
  tm_fill("IND_MARG", id = "NOM_MUN",
          title="<b> Índice de marginación <\b> 
          <br>
          por Estado.",
          style="cont",
          palette = "PuRd", 
          contrast = c(0.2, 0.83))+
  tm_borders(col="gray50",
             alpha=.5) +
  tm_shape(mxgeo.est)+ 
  tm_borders(col = "black",
             lwd = 1,
             alpha=.5)

save_tmap(mun.map, "marginacion_municipios.html")




