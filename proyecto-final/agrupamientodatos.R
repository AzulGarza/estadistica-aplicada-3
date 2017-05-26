
# Aquí juntamos los datos de TODOS los estados (32)

setwd("C:/Users/Daniela Baltazar/Downloads/federicogarza/Aplicada3")

library(readxl)
library(xlsx)

# Creando los nombres de todos los archivos
files.names = c()
for(i in 1:9){
  files.names = c(files.names,paste0("ITER_0",as.character(i),"XLS10.xls"))
}
for(i in 10:32){
  files.names = c(files.names,paste0("ITER_",as.character(i),"XLS10.xls"))
}

# Obteniendo lo necesario de las bases
writefile.names = c()
counter = 1
for(file in files.names){
X = read_excel(file, col_names = TRUE)

# Antes de pegarla limpiamos la base de datos
X[X == "*"] = NA
X[X == "N/D"] = NA
loc = which(X$LOC=="0000")
X=X[loc,]
mun = which(X$MUN == "000")
X = X[-mun,]
X$ID_MUN = paste0(X$ENTIDAD,X$MUN)
X = X[,-c(1:9)]

writefile.name = paste0("datos_municipios_",as.character(counter),".xls")
write.xlsx(X, writefile.name)
writefile.names = c(writefile.names,writefile.name)
counter = counter + 1
}

# Función para agrupar los datos

finalX = read_excel(writefile.names[1], col_names = TRUE)
X = finalX

for(file in writefile.names[2:32]){
  otherX = read_excel(file, col_names = TRUE)
  X = rbind(X, otherX)
}

write.xlsx(X,"datos_agrupados.xls")
