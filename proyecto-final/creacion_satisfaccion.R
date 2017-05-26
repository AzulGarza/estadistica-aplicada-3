# Aquí creamos el file de datos que usaremos para el modelo loglineal
setwd("C:/Users/Daniela Baltazar/Downloads/federicogarza/Aplicada3")
# Cargamos las librerías necesarias
x = c("readxl","xlsx")
lapply(x, library, character.only=TRUE)

D = read_excel("pob_sat_3.XLSX", col_names = TRUE,
               sheet = "Nal", na = "NA", skip = 5)

# Nos quedamos solo con las columnas que nos interesan
D = D[,c("X__1","Muy insatisfecho","Insatisfecho","Satisfecho","Muy satisfecho")]
D = D[-c(1:10),]

# Guardamos el archivo

write.xlsx(D,"satisfaccion_mexico.xls")
