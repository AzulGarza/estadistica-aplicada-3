# Aquí elaboramos todo el análisis del modelo 
# loglineal

libraries = c("readxl","MASS")
lapply(libraries, library, character.only = TRUE)

# Cargamos los datos elegidos

D = read_excel("satisfaccion_mexico.xls", col_names = TRUE)
D = D[-c(3,5,12,14),]

# Nombramos las variables
n_satisfaccion = c("M_INS","INS","SAT","M_SAT")
sexo = c("M","H")
n_instruccion = c("N","PRI_CO","SEC_CO",
                  "PREP","LIC","POSG")

# Creamos el vector de TODOS los datos
counters = c()

for(j in 0:(length(n_instruccion)-1)){
  counters = c(counters,as.vector(data.matrix(D[9+j,3:6])))
  counters = c(counters,as.vector(data.matrix(D[2+j,3:6])))
}

# Creamos el data frame con los datos
muestra = data.frame (expand.grid(
  satisfaccion = factor(n_satisfaccion,levels = n_satisfaccion),
  sexo = factor(sexo,levels = sexo),
  instruccion = factor(n_instruccion,levels = n_instruccion)) ,
  count = counters)


# Con esto hacemos los AJUSTES
# NS_S_I (JUNTOS)
ajuste.NS_S_I = loglm(count~ satisfaccion * sexo * instruccion, 
                      data = muestra)
# NS_S , NS_I , S_I
ajuste.NS_S.NS_I.S_I = update(ajuste.NS_S_I, 
                                .~. -satisfaccion:sexo:instruccion)
# NS_I, S_I
ajuste.NS_I.S_I = update(ajuste.NS_S.NS_I.S_I,
                           .~. -satisfaccion:sexo)
# NS_S, S_I
ajuste.NS_S.S_I = update(ajuste.NS_S.NS_I.S_I,
                         .~. -satisfaccion:instruccion)
# Ns_S, NS_I
ajuste.NS_S.NS_I = update(ajuste.NS_S.NS_I.S_I,
                         .~. -sexo:instruccion)

# NS_S, I
ajuste.NS_S.I = update(ajuste.NS_S.NS_I.S_I,
                         .~. -satisfaccion:instruccion - sexo:instruccion)
#NS_I, S
ajuste.NS_I.S = update(ajuste.NS_S.NS_I.S_I,
                       .~. -satisfaccion:sexo - sexo:instruccion )

#S_I, NS
ajuste.S_I.NS = update(ajuste.NS_S.NS_I.S_I,
                       .~. -satisfaccion:sexo - satisfaccion:instruccion )

# NS , S , I
ajuste.NS.S.I = update(ajuste.NS_S.I,
                        .~. -satisfaccion:sexo)

# Organizamos los resultados

ajustes = data.frame (muestra[64:1,-4], NS_S_I = c(aperm(fitted(ajuste.NS_S_I))),
                      NS_S.NS_I.S_I = c(aperm(fitted(ajuste.NS_S.NS_I.S_I))) ,
                      NS_I.S_I = c(aperm(fitted(ajuste.NS_I.S_I))),
                      NS_S.S_I = c(aperm(fitted(ajuste.NS_S.S_I))),
                      NS_S.NS_I = c(aperm(fitted(ajuste.NS_S.NS_I))),
                      NS_S.I = c(aperm(fitted(ajuste.NS_S.I))),
                      NS_I.S = c(aperm(fitted(ajuste.NS_I.S))),
                      S_I.NS = c(aperm(fitted(ajuste.S_I.NS))),
                      NS.S.I = c(aperm(fitted(ajuste.NS.S.I))))
anova(ajuste.NS_S_I,ajuste.S_I.NS,ajuste.NS.S.I)


stepAIC(ajuste.NS_S_I, direction = "backward", test="Chisq",
        scope = list(upper = ~satisfaccion*sexo*instruccion, 
                     lower = ~satisfaccion+sexo+instruccion),
        trace = TRUE)

ajuste.NS.S.I$deviance

tabla <- data.frame ( expand.grid (
  marihuana = factor ( c ( " Yes " ," No " ) ,levels = c ( " No " ," Yes " ) ) ,
  tabaco = factor ( c ( " Yes " ," No " ) ,levels = c ( " No " ," Yes " ) ) ,
  alcohol = factor ( c ( " Yes " ," No " ) ,levels = c ( " No " ," Yes " ) ) ) ,
  count = c (911 ,538 ,44 ,456 ,3 ,43 ,2 ,279) )

fitACM <-
  loglm ( count∼alcohol * tabaco * marihuana , data = tabla , param =T , fit = T )

h = step(fitACM, direction = "backward", test="Chisq")

data(Titanic)  # loading the built-in data set

Titanic3 <- margin.table(Titanic, c(4,2,1))[2:1,,]

print(Titanic3)  # 2-by-2-by-4 table

library(MASS) # load the "MASS" package

# The 'step' function picks the "best" model according to the Akaike Information Criterion (AIC):

# Start with the "saturated model" containing ALL possible interactions and work backward:

saturated.model <- loglm(~ Class * Sex * Survived, data=Titanic3)

step(saturated.model, direction="backward")

# Is it possible to leave off the second-order interaction?

loglm(~ Class + Sex + Survived + Class:Sex + Class:Survived + Sex:Survived, 
      data = Titanic3)

