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
  geom_point(color = "skyblue", size = 3) +
  geom_smooth(method = "lm", color = "blue") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +  # Ajustar etiquetas cerca del punto
  labs(title = "Coeficiente de Gini vs Índice de Desarrollo Humano (IDH) en América Latina",
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


#Interpretación 


