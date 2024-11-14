# Trabajo Aplicado III

#Instalar líbrerías
library(dplyr)
library(haven)
library(ggplot2)
library(corrplot)

#Cargar base de datos

DatosPais <- read_excel("Downloads/TA III.xlsx")
View(DatosPais) 

#Cambiar países por números de 1 al 20, para que sean variables númericas

DatosPais2 <- DatosPais
DatosPais2$Países <- 1:20 

# Función para limpiar y convertir a numérico
convertir_a_numero <- function(x) {
  # Reemplaza las comas por puntos y convierte a numérico
  as.numeric(gsub(",", ".", x))
}

# Aplicar la función a las columnas que deben ser numéricas
DatosPais2$GastoEd <- convertir_a_numero(DatosPais2$GastoEd)
DatosPais2$CoeGini <- convertir_a_numero(DatosPais2$CoeGini)
DatosPais2$PorDesempleo <- convertir_a_numero(DatosPais2$PorDesempleo)
DatosPais2$PorGradSecundaria <- convertir_a_numero(DatosPais2$PorGradSecundaria)
DatosPais2$IDH <- convertir_a_numero(DatosPais2$IDH)
DatosPais2$IPG <- convertir_a_numero(DatosPais2$IPG)

str(DatosPais2)


#Crear matriz de correlaciones 

cor_matrix <- cor(DatosPais2[, -1], use = "complete.obs")

#Crear correlograma
corrplot(cor_matrix, method = "color", type = "upper", 
         title = "Correlograma de Variables en Latinoamerica y paises OCDE", 
         mar = c(0, 0, 2, 0))

#Coeficientes de Pearson para las variables que se vean correlacionables 

cor_pearson <- cor(DatosPais2$CoeGini, DatosPais2$IDH, use = "complete.obs")
print(cor_pearson)


cor_pearson2 <- cor(DatosPais2$IPG, DatosPais2$IDH, use = "complete.obs")
print(cor_pearson2)


#Parte para hacer que los números de país ahora tengan nombres

nombres_paises <- c("Perú", "Chile", "Bolivia", "Ecuador", "Colombia", "Argentina", 
                    "Costa Rica", "Uruguay", "Brasil", "Republica Dominicana", 
                    "Mexico", "Honduras", "El Salvador", "Paraguay", "Finlandia", 
                    "España", "Canada", "Noruega", "Portugal", "Italia")

DatosPais3 <- DatosPais2
DatosPais3$Países <-nombres_paises


#Gráficos de dispersion en ggplot de las variables 

ggplot(DatosPais3, aes(x = CoeGini, y = IDH, label = Países)) +
  geom_point(shape = 21, fill = "skyblue", color = "black", size = 3, stroke = 0.8) + # Círculos con borde negro
  geom_smooth(method = "lm", color = "blue") +
  geom_text_repel(size = 3, box.padding = 0.35, point.padding = 0.5, max.overlaps = 10) +
  labs(title = "Coeficiente de Gini vs Índice de Desarrollo Humano (IDH)",
       subtitle = paste("Coeficiente de correlación de Pearson:", round(cor_pearson, 2)),
       x = "Coeficiente de Gini",
       y = "Índice de Desarrollo Humano (IDH)") +
  theme_minimal()

ggplot(DatosPais3, aes(x = IPG, y = IDH, label = Países)) +
  geom_point(color = "skyblue", size = 3) +
  geom_smooth(method = "lm", color = "blue") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +  # Ajustar etiquetas cerca del punto
  labs(title = "Indice de Paz Global vs Índice de Desarrollo Humano (IDH) en América Latina",
       subtitle = paste("Coeficiente de correlación de Pearson:", round(cor_pearson2, 2)),
       x = "Indíce de Paz Global",
       y = "Índice de Desarrollo Humano (IDH)") +
  theme_minimal()

########ALI CODIGOS#######


####RELACION ENTRE GASTO EN EDUCACION Y COEGINI


# Crear matriz de correlaciones entre Gasto en Educación y Coeficiente de Gini
cor_matrix <- cor(DatosPais2[, c("GastoEd", "CoeGini")], use = "complete.obs")

# Crear correlograma
corrplot(cor_matrix, method = "color", type = "upper", 
         title = "Correlograma de Gasto en Educación y Coeficiente de Gini", 
         mar = c(0, 0, 2, 0))

# Coeficiente de Pearson entre Gasto en Educación y Coeficiente de Gini
cor_pearson <- cor(DatosPais2$GastoEd, DatosPais2$CoeGini, use = "complete.obs")
print(cor_pearson)

# Parte para hacer que los números de país ahora tengan nombres
nombres_paises <- c("Perú", "Chile", "Bolivia", "Ecuador", "Colombia", "Argentina", 
                    "Costa Rica", "Uruguay", "Brasil", "Republica Dominicana", 
                    "Mexico", "Honduras", "El Salvador", "Paraguay", "Finlandia", 
                    "España", "Canada", "Noruega", "Portugal", "Italia")

DatosPais3 <- DatosPais2
DatosPais3$Países <- nombres_paises

# Gráfico de dispersión en ggplot de Gasto en Educación vs Coeficiente de Gini
ggplot(DatosPais3, aes(x = GastoEd, y = CoeGini, label = Países)) +
  geom_point(color = "skyblue", size = 3) +
  geom_smooth(method = "lm", color = "blue") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +  # Ajustar etiquetas cerca del punto
  labs(title = "Gasto en Educación vs Coeficiente de Gini en América Latina y países OCDE",
       subtitle = paste("Coeficiente de correlación de Pearson:", round(cor_pearson, 2)),
       x = "Gasto en Educación (%)",
       y = "Coeficiente de Gini") +
  theme_minimal()


###RELACION ENTRE GRADUACIONES DE SECUNDARIA E IDH

# Crear matriz de correlaciones entre Graduaciones de Secundaria y IDH
cor_matrix <- cor(DatosPais2[, c("PorGradSecundaria", "IDH")], use = "complete.obs")

# Crear correlograma
corrplot(cor_matrix, method = "color", type = "upper", 
         title = "Correlograma de Graduaciones de Secundaria y IDH", 
         mar = c(0, 0, 2, 0))

# Coeficiente de Pearson entre Graduaciones de Secundaria y IDH
cor_pearson <- cor(DatosPais2$PorGradSecundaria, DatosPais2$IDH, use = "complete.obs")
print(cor_pearson)

# Parte para hacer que los números de país ahora tengan nombres
nombres_paises <- c("Perú", "Chile", "Bolivia", "Ecuador", "Colombia", "Argentina", 
                    "Costa Rica", "Uruguay", "Brasil", "Republica Dominicana", 
                    "Mexico", "Honduras", "El Salvador", "Paraguay", "Finlandia", 
                    "España", "Canada", "Noruega", "Portugal", "Italia")

DatosPais3 <- DatosPais2
DatosPais3$Países <- nombres_paises

# Gráfico de dispersión en ggplot de Graduaciones de Secundaria vs IDH
ggplot(DatosPais3, aes(x = PorGradSecundaria, y = IDH, label = Países)) +
  geom_point(color = "skyblue", size = 3) +
  geom_smooth(method = "lm", color = "blue") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +  # Ajustar etiquetas cerca del punto
  labs(title = "Graduaciones de Secundaria vs Índice de Desarrollo Humano (IDH)",
       subtitle = paste("Coeficiente de correlación de Pearson:", round(cor_pearson, 2)),
       x = "Porcentaje de Graduaciones de Secundaria",
       y = "Índice de Desarrollo Humano (IDH)") +
  theme_minimal()


#### RELACION COEGINI E IPG

# Crear matriz de correlaciones entre Coeficiente de Gini y IPG
cor_matrix <- cor(DatosPais2[, c("CoeGini", "IPG")], use = "complete.obs")

# Crear correlograma
corrplot(cor_matrix, method = "color", type = "upper", 
         title = "Correlograma de Coeficiente de Gini y IPG", 
         mar = c(0, 0, 2, 0))

# Coeficiente de Pearson entre Coeficiente de Gini y IPG
cor_pearson <- cor(DatosPais2$CoeGini, DatosPais2$IPG, use = "complete.obs")
print(cor_pearson)

# Parte para hacer que los números de país ahora tengan nombres
nombres_paises <- c("Perú", "Chile", "Bolivia", "Ecuador", "Colombia", "Argentina", 
                    "Costa Rica", "Uruguay", "Brasil", "Republica Dominicana", 
                    "Mexico", "Honduras", "El Salvador", "Paraguay", "Finlandia", 
                    "España", "Canada", "Noruega", "Portugal", "Italia")

DatosPais3 <- DatosPais2
DatosPais3$Países <- nombres_paises

# Gráfico de dispersión en ggplot de Coeficiente de Gini vs IPG
ggplot(DatosPais3, aes(x = CoeGini, y = IPG, label = Países)) +
  geom_point(color = "skyblue", size = 3) +
  geom_smooth(method = "lm", color = "blue") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +  # Ajustar etiquetas cerca del punto
  labs(title = "Coeficiente de Gini vs Índice de Paz Global (IPG)",
       subtitle = paste("Coeficiente de correlación de Pearson:", round(cor_pearson, 2)),
       x = "Coeficiente de Gini",
       y = "Índice de Paz Global (IPG)") +
  theme_minimal()


#Interpretación 

Un coeficiente de correlación de pearson de -0,79 apunta a la existencia de una correlación negativa y fuerte entre el coeficiente de Gini y el Índice de Paz Global.
Lo que indica que mientras exista un Coeficiente de Gini más alto , existirá también un menor nivel de paz.
Se puede observar una Regresión lineal que muestra la tendencia central de la relación ,
mientras más aumenta el Coeficiente de Gini también aumenta el Índice de paz Global, lo que deja como resultado un menor nivel de paz.

En referencia a los distintos países en estudio, Noruega y Finlandia cuentan con un bajo coeficiente de Gini,
y por consiguiente un bajo índice de Paz. Por otro lado, Colombia y Brasil muestran un alto coeficiente de Gini y un alto Nivel de Paz Índice de paz, es decir,
un menor nivel de paz. Seguidamente, México y Honduras cuentan con valores similares en desigualdad, sin embargo el Índice de Paz es más alta para Honduras, 
lo que apunta a un entorno menos pacifico.Por último, Costa Rica muestra una situación característica,  
se encuentra desfasado en relación a los países que cuentan con un coeficiente de Gini similar suyo,
lo que muestra un nivel de paz mucho más elevado al esperado según su nivel de desigualdad.

Resulta evidente que existe una relación negativa y significativa entre la desigualdad y el nivel de paz.
Los países con menor desigualdad (menor Coeficiente de Gini) cuentan con un mejor Índice de Paz Global,
mientras que los países con mayor desigualdad suelen presentar mayores niveles de conflictividad o menor paz.
Esta tendencia refleja que la desigualdad influye en la estabilidad social y en las condiciones de paz de un país.

