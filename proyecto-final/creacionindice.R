# Aquí se hace el índice de marginación
setwd("C:/Users/Daniela Baltazar/Downloads/federicogarza/Aplicada3")
x = c("readxl","xlsx","FactoMineR","Factoshiny")
lapply(x, library, character.only = TRUE)

X = read_excel("datos_agrupados.xls", col_names = TRUE)

# Quitamos dos primeras columnas inncesarias

X = X[,-c(1,2)]
# Convirtiendo a variables numéricas
X[,c(1:191)] = sapply(X[,c(1:191)],as.numeric)


# Seleccionando las variables para el índice

vars = c("POBTOT","P_5YMAS","P_12YMAS","P_15YMAS",
         "PROM_HNV",
         "P3YM_HLI",
         "P6A11_NOA","P12A14NOA","P8A14AN","P15YM_AN","P15YM_SE","P15PRI_IN","P15SEC_IN","P15SEC_CO",
         "PE_INAC","PDESOCUP",
         "PSINDER", "PDER_SEGP",
         "PRO_OCUP_C",
         "VIVPAR_HAB","VPH_PISOTI","VPH_S_ELEC",
         "VPH_AGUAFV", "VPH_EXCSA","VPH_NODREN",
         "VPH_REFRI","VPH_LAVAD", "VPH_TELEF",
         "VPH_INTER")
# OBS: Hay que sumar las primeras dos "P6A14_NOA"
# OJO: Secundaria completa es población con educación básica
# Las últimas tres hay que restarles el total de viviendas

Xpca = X[,vars]
Xpca[,"P6A14_NOA"] = Xpca$P6A11_NOA+Xpca$P12A14NOA
Xpca[,c("VPH_NREFRI","VPH_NLAVAD","VPH_NTELEF","VPH_NEXCSA","VPH_NINTER")] = Xpca$VIVPAR_HAB - 
  Xpca[,c("VPH_REFRI","VPH_LAVAD","VPH_TELEF","VPH_EXCSA","VPH_INTER")] 

#Cálculo de porcentajes
Xpca[,"P3YM_HLI"] = Xpca[,"P3YM_HLI"]/Xpca$POBTOT
Xpca[,"P6A14_NOA"] = Xpca[,"P6A14_NOA"]/(Xpca$P_5YMAS-Xpca$P_15YMAS)
Xpca[,"P8A14AN"] = Xpca[,"P8A14AN"]/(Xpca$P_12YMAS-Xpca$P_15YMAS)
Xpca[,c("P15YM_AN","P15YM_SE","P15PRI_IN","P15SEC_IN","P15SEC_CO")] = 
  Xpca[,c("P15YM_AN","P15YM_SE","P15PRI_IN","P15SEC_IN","P15SEC_CO")]/Xpca$P_15YMAS 
Xpca[,c("PE_INAC","PDESOCUP","PSINDER", "PDER_SEGP")] = 
  Xpca[,c("PE_INAC","PDESOCUP","PSINDER", "PDER_SEGP")]/X$POBTOT
Xpca[,c("VPH_NREFRI","VPH_NLAVAD","VPH_NTELEF","VPH_NEXCSA",
           "VPH_PISOTI","VPH_S_ELEC",
           "VPH_AGUAFV","VPH_NODREN","VPH_INTER")] = 
  Xpca[,c("VPH_NREFRI","VPH_NLAVAD","VPH_NTELEF","VPH_NEXCSA",
             "VPH_PISOTI","VPH_S_ELEC",
             "VPH_AGUAFV","VPH_NODREN","VPH_NINTER")]/Xpca$VIVPAR_HAB

# Quitamos las variables que ya no hacen falta
Xpca = Xpca[, -which(names(Xpca) %in% c("POBTOT",
                                        "P_5YMAS","P_15YMAS", "P_12YMAS",
                                        "P6A11_NOA","P12A14NOA",
                                        "VIVPAR_HAB",
                                        "VPH_REFRI","VPH_LAVAD",
                                        "VPH_TELEF",
                                        "VPH_EXCSA", "VPH_INTER"))]

# Todas las variables
X.PCA = PCA(Xpca)
PCAshiny(Xpca)


# Quitamos variables de vivienda

Xpca.1 = Xpca[,-which(names(Xpca) %in% c("VPH_NREFRI","VPH_NLAVAD","VPH_NTELEF","VPH_NEXCSA",
                                       "VPH_PISOTI","VPH_S_ELEC",
                                       "VPH_AGUAFV","VPH_NODREN","VPH_NINTER"))]
PCAshiny(PCA(Xpca.1))


# Dejamos solo vivienda


Xpca.2 = Xpca[,which(names(Xpca) %in% c("VPH_NREFRI","VPH_NLAVAD","VPH_NTELEF","VPH_NEXCSA",
                                        "VPH_PISOTI","VPH_S_ELEC",
                                        "VPH_AGUAFV","VPH_NODREN","VPH_NINTER"))]

PCAshiny(PCA(Xpca.2))

# Quitamos variables raras

Xpca.3 = Xpca[,-which(names(Xpca) %in% c("PE_INAC","PDESOCUP",
                                         "PSINDER", "PDER_SEGP",
                                         "P3YM_HLI"))]

PCAshiny(PCA(Xpca.3))

Xpca = Xpca[,c("VPH_NREFRI","VPH_NLAVAD","VPH_NTELEF","VPH_NEXCSA",
            "VPH_PISOTI","VPH_S_ELEC",
            "VPH_AGUAFV","VPH_NODREN")]


# Luego de escoger las variables relevantes modificamos 
# el índice de marginación para que sean todos positivos


marginacion = data.frame(X$ID_MUN,X.PCA$ind$coord[,1])
colnames(marginacion) = c("ID_MUN","IND_MARG")

  # Guardamos el índice

write.xlsx(marginacion, "indice_marginacion.xls")
