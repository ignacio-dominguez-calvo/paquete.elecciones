
library(mapSpain)
library(ggplot2)
library(sf)
library(dplyr)
library(data.table)
municipios <- esp_get_munic()
ggplot(data = municipios) +
  geom_sf() +
  theme_minimal() +
  ggtitle("Mapa de Municipios de España")


valladolid <- municipios[municipios$ine.prov.name == "Valladolid", ]
set.seed(123)  # Para asegurar reproducibilidad
valladolid$poblacion_ficticia <- sample(1000:50000, nrow(valladolid), replace = TRUE

provincia <- "Burgse"
df<-df
variable<-df$votos_en_blanco

##############


mapa_provincia_secciones<-function(df,variable, provincia){
  ine_codes <- data.frame(
    INE_CODE = as.numeric(c(2, 3, 4, 1, 33, 5, 6, 7, 8, 48, 9, 10, 11, 39, 12, 13, 14, 15,
                            16, 20, 17, 18, 19, 21, 22, 23, 24, 25, 27, 28, 29, 30, 31, 32, 34, 35,
                            36, 26, 37, 38, 40, 41, 42, 43, 44, 45, 46, 47, 49, 50, 51, 52)),
    PROVINCE = c("Albacete", "Alicante/Alacant", "Almería", "Araba/Álava", "Asturias", "Ávila", "Badajoz", "Balears, Illes",
                 "Barcelona", "Bizkaia", "Burgos", "Cáceres", "Cádiz", "Cantabria", "Castellón/Castelló", "Ciudad Real",
                 "Córdoba", "A Coruña", "Cuenca", "Gipuzkoa", "Girona", "Granada", "Guadalajara", "Huelva", "Huesca",
                 "Jaén", "León", "Lleida", "Lugo", "Madrid", "Málaga", "Murcia", "Navarra", "Ourense", "Palencia",
                 "Las Palmas", "Pontevedra", "La Rioja", "Salamanca", "Santa Cruz de Tenerife", "Segovia", "Sevilla",
                 "Soria", "Tarragona", "Teruel", "Toledo", "Valencia/València", "Valladolid", "Zamora", "Zaragoza",
                 "Ceuta", "Melilla")
  )
  variable <- deparse(substitute(variable))
  variable <- sub(".*\\$", "", variable)
  municipios <- esp_get_munic()
  municipios$cpro<-as.numeric(municipios$cpro)
  municipios$cmun<-as.numeric(municipios$cmun)
  df_reducido <- df[, c("codigo_ine_provincia","codigo_ine_municipio",variable)]
  provincia <- get_min_distancia_Levenshtein(provincia,municipios$ine.prov.name)
  df_reducido<-df_reducido[df_reducido$codigo_ine_provincia == ine_codes$INE_CODE[ine_codes$PROVINCE == get_min_distancia_Levenshtein(provincia,ine_codes$PROVINCE)],]
  # Realizar el join usando data.table
  municipios<-municipios[municipios$ine.prov.name==provincia,]
  data <- full_join(municipios,df_reducido, by = c("cmun" = "codigo_ine_municipio"))
  # Crear el mapa usando ggplot2
  ggplot(data = data) +
    geom_sf(aes(fill = .data[[variable]]), color = NA) +  # Ensure the variable is correctly referenced
    scale_fill_viridis_c(
      name = variable,
    ) +
    labs(
      title = paste("Mapa por municipios de la provincia de", provincia),
      subtitle = paste("Variable:", variable)
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

mapa_provincia_secciones(df,df$prop_votos_blanco,"sevilla")


df$prop_votos_blanco<-df$votos_en_blanco/(df$votos_candidaturas+df$votos_en_blanco+df$votos_nulos)*100
get_min_distancia_Levenshtein <- function(input_string, municipios) {
  # Calcula la distancia de Levenshtein entre el string de entrada y cada municipio
  distances <- stringdist::stringdist(input_string, municipios)

  # Encuentra el índice del valor mínimo en distances
  min_index <- which.min(distances)

  # Devuelve el nombre del municipio con la distancia mínima
  closest_municipio <- municipios[min_index]
  return(closest_municipio)
}



ine_codes <- data.frame(
  INE_CODE = as.numeric(c(2, 3, 4, 1, 33, 5, 6, 7, 8, 48, 9, 10, 11, 39, 12, 13, 14, 15,
                          16, 20, 17, 18, 19, 21, 22, 23, 24, 25, 27, 28, 29, 30, 31, 32, 34, 35,
                          36, 26, 37, 38, 40, 41, 42, 43, 44, 45, 46, 47, 49, 50, 51, 52)),
  PROVINCE = c("Albacete", "Alicante/Alacant", "Almería", "Araba/Álava", "Asturias", "Ávila", "Badajoz", "Balears, Illes",
               "Barcelona", "Bizkaia", "Burgos", "Cáceres", "Cádiz", "Cantabria", "Castellón/Castelló", "Ciudad Real",
               "Córdoba", "A Coruña", "Cuenca", "Gipuzkoa", "Girona", "Granada", "Guadalajara", "Huelva", "Huesca",
               "Jaén", "León", "Lleida", "Lugo", "Madrid", "Málaga", "Murcia", "Navarra", "Ourense", "Palencia",
               "Las Palmas", "Pontevedra", "La Rioja", "Salamanca", "Santa Cruz de Tenerife", "Segovia", "Sevilla",
               "Soria", "Tarragona", "Teruel", "Toledo", "Valencia/València", "Valladolid", "Zamora", "Zaragoza",
               "Ceuta", "Melilla")
)
variable <- deparse(substitute(variable))
variable <- 'prop_votos_blanco'
municipios <- esp_get_munic()
df_reducido <- df[, c("nombre_municipio","codigo_ine_provincia",variable)]
provincia <- get_min_distancia_Levenshtein(provincia,municipios$ine.prov.name)
df_reducido<-df_reducido[df_reducido$codigo_ine_provincia == ine_codes$INE_CODE[ine_codes$PROVINCE == get_min_distancia_Levenshtein(provincia,ine_codes$PROVINCE)],]
# Realizar el join usando data.table
municipios<-municipios[municipios$ine.prov.name==provincia,]
df_reducido$nombre_mapa <- sapply(df_reducido$nombre_municipio, function(x) {
  get_min_distancia_Levenshtein(x, municipios$name)
})
data <- full_join(municipios,df_reducido, by = c("name" = "nombre_mapa"))

# Crear el mapa usando ggplot2
ggplot(data = data) +
  geom_sf(aes(fill = .data[[variable]]), color = NA) +  # Ensure the variable is correctly referenced
  scale_fill_viridis_c(
    name = variable,

  ) +
  labs(
    title = paste("Mapa por municipios de la provincia de", provincia),
    subtitle = paste("Variable:", variable)
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )




















