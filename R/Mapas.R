
library(sf)
library(ggplot2)
library(giscoR)
library(dplyr)
library(viridis)

#' Crear un mapa de las provincias de España coloreado por una variable específica
#'
#' Esta función toma un dataframe y una variable de interés, y crea un mapa de las provincias de España
#' coloreado según los valores agregados de la variable especificada.
#'
#' @param df Un dataframe que contiene datos de población por provincias.
#' @param variable La variable del dataframe cuyo total se desea representar en el mapa.
#' @return Un mapa de las provincias de España coloreado por la variable especificada.
#' @examples
#' \dontrun{crear_mapa_provincias(df,df$votos_en_blanco)
#' }
#' @import dplyr
#' @import ggplot2
#' @import sf
#' @import giscoR
#' @importFrom stats sym
#' @export
crear_mapa_provincias <- function(df, variable) {
  variable <- deparse(substitute(variable))
  variable <- sub(".*\\$", "", variable)
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

  spain_provinces <- gisco_get_nuts(country = "ES", nuts_level = 3, resolution = "03", year = "2021")


  spain_provinces_selected <- spain_provinces %>%
    select(NUTS_ID, NAME_LATN) %>%
    rename(PROVINCE = NAME_LATN)

  df_poblacion_sum <- df %>%
    group_by(codigo_ine_provincia) %>%
    summarise(poblacion_total = sum(as.numeric(!!sym(variable)), na.rm = TRUE))

  combined_df <- left_join(spain_provinces_selected, ine_codes, by = "PROVINCE") %>%
    left_join(df_poblacion_sum, by = c("INE_CODE" = "codigo_ine_provincia"))

  canarias <- combined_df %>%
    filter(PROVINCE %in% c("El Hierro", "Fuerteventura", "La Gomera", "La Palma", "Gran Canaria", "Lanzarote", "Tenerife"))

  canarias_poblacion <- data.frame(
    PROVINCE = c("El Hierro", "Fuerteventura", "Gran Canaria", "La Gomera", "La Palma", "Lanzarote", "Tenerife"),
    codigo_ine_provincia = c(38,35,35,38, 38, 35, 38)
  )
  canarias_poblacion_sum <- canarias_poblacion %>%
    left_join(df_poblacion_sum, by = "codigo_ine_provincia") %>%
    group_by(codigo_ine_provincia, PROVINCE) %>%
    summarise(poblacion_total = sum(poblacion_total, na.rm = TRUE))

  canarias <- canarias %>%
    left_join(canarias_poblacion_sum, by = "PROVINCE") %>%
    mutate(poblacion_total = coalesce(poblacion_total.y, poblacion_total.x)) %>%
    select(-poblacion_total.x, -poblacion_total.y)

  canarias_shifted <- canarias %>%
    mutate(geometry = st_geometry(geometry) + c(18, 7))

  st_crs(canarias_shifted) <- st_crs(combined_df)

  baleares <- combined_df %>%
    filter(PROVINCE %in% c("Eivissa y Formentera", "Mallorca", "Menorca"))

  baleares_poblacion <- data.frame(
    PROVINCE = c("Eivissa y Formentera", "Mallorca", "Menorca"),
    codigo_ine_provincia = c(07, 07, 07)
  )
  baleares_poblacion_sum <- baleares_poblacion %>%
    left_join(df_poblacion_sum, by = "codigo_ine_provincia") %>%
    group_by(codigo_ine_provincia, PROVINCE) %>%
    summarise(poblacion_total = sum(poblacion_total, na.rm = TRUE))

  baleares <- baleares %>%
    left_join(baleares_poblacion_sum, by = "PROVINCE") %>%
    mutate(poblacion_total = coalesce(poblacion_total.y, poblacion_total.x)) %>%
    select(-poblacion_total.x, -poblacion_total.y)

  # Combinar los datos
  spain_provinces_shifted <- combined_df %>%
    filter(!PROVINCE %in% c("El Hierro", "Fuerteventura", "La Gomera", "La Palma", "Gran Canaria", "Lanzarote", "Tenerife", "Eivissa y Formentera", "Mallorca", "Menorca")) %>%
    bind_rows(canarias_shifted) %>%
    bind_rows(baleares)

  # Crear el mapa de las provincias de España coloreado por la variable total
  ggplot(data = spain_provinces_shifted) +
    geom_sf(aes(fill = poblacion_total)) +
    scale_fill_viridis_c(
      option = "D",
      labels = label_number(big.mark = ",", accuracy = 1)  # Formatea los números con comas y una cifra decimal
    ) +
    theme_minimal() +
    ggtitle(paste("Mapa de las Provincias de España Coloreado por", variable))
}
crear_mapa_CCAA <- function(df, variable) {
  library(dplyr)
  library(ggplot2)
  library(sf)
  library(giscoR)
  library(rlang)

  variable <- deparse(substitute(variable))
  variable <- sub(".*\\$", "", variable)
  # Mapa de provincias a comunidades autónomas
  provincia_CCAA <- data.frame(
    INE_CODE = as.numeric(c(2, 3, 4, 1, 33, 5, 6, 7, 8, 48, 9, 10, 11, 39, 12, 13, 14, 15,
                            16, 20, 17, 18, 19, 21, 22, 23, 24, 25, 27, 28, 29, 30, 31, 32, 34, 35,
                            36, 26, 37, 38, 40, 41, 42, 43, 44, 45, 46, 47, 49, 50, 51, 52)),
    PROVINCE = c("Albacete", "Alicante/Alacant", "Almería", "Araba/Álava", "Asturias", "Ávila", "Badajoz", "Balears, Illes",
                 "Barcelona", "Bizkaia", "Burgos", "Cáceres", "Cádiz", "Cantabria", "Castellón/Castelló", "Ciudad Real",
                 "Córdoba", "A Coruña", "Cuenca", "Gipuzkoa", "Girona", "Granada", "Guadalajara", "Huelva", "Huesca",
                 "Jaén", "León", "Lleida", "Lugo", "Madrid", "Málaga", "Murcia", "Navarra", "Ourense", "Palencia",
                 "Las Palmas", "Pontevedra", "La Rioja", "Salamanca", "Santa Cruz de Tenerife", "Segovia", "Sevilla",
                 "Soria", "Tarragona", "Teruel", "Toledo", "Valencia/València", "Valladolid", "Zamora", "Zaragoza",
                 "Ceuta", "Melilla"),
    CCAA = c("Castilla-La Mancha", "Comunitat Valenciana", "Andalucía", "País Vasco", "Principado de Asturias", "Castilla y León", "Extremadura", "Illes Balears",
             "Cataluña", "País Vasco", "Castilla y León", "Extremadura", "Andalucía", "Cantabria", "Comunitat Valenciana", "Castilla-La Mancha",
             "Andalucía", "Galicia", "Castilla-La Mancha", "País Vasco", "Cataluña", "Andalucía", "Castilla-La Mancha", "Andalucía", "Aragón",
             "Andalucía", "Castilla y León", "Cataluña", "Galicia", "Comunidad de Madrid", "Andalucía", "Región de Murcia", "Comunidad Foral de Navarra", "Galicia", "Castilla y León",
             "Canarias", "Galicia", "La Rioja", "Castilla y León", "Canarias", "Castilla y León", "Andalucía", "Castilla y León", "Cataluña",
             "Aragón", "Castilla-La Mancha", "Comunitat Valenciana", "Castilla y León", "Castilla y León", "Aragón",
             "Ciudad de Ceuta", "Ciudad de Melilla")
  )
  df <- df %>%
    left_join(provincia_CCAA, by = c("codigo_ine_provincia" = "INE_CODE"))

  # Sumarizar los datos por CCAA

  df_CCAA_sum <- df %>%
    group_by(CCAA) %>%
    summarise(poblacion_total = sum(as.numeric(.data[[variable]]), na.rm = TRUE))


  df_CCAA_sum# Obtener datos geográficos de las CCAA de España
  spain_CCAA <- gisco_get_nuts(country = "ES", nuts_level = 2, resolution = "03", year = "2021")

  # Unir datos geográficos con datos sumarizados
  combined_df <- left_join(spain_CCAA, df_CCAA_sum, by = c("NAME_LATN"="CCAA"))
  df_poblacion_sum <- df %>%
    group_by(codigo_ine_provincia) %>%
    summarise(poblacion_total = sum(as.numeric(!!sym(variable)), na.rm = TRUE))

  canarias <- combined_df %>%
    filter(NAME_LATN %in% c("Canarias"))


  canarias_shifted <- canarias %>%
    mutate(geometry = st_geometry(geometry) + c(18, 7))

  st_crs(canarias_shifted) <- st_crs(combined_df)

  baleares <- combined_df %>%
    filter(NAME_LATN %in% c("Illes Balears"))

  baleares_poblacion <- data.frame(
    CCAA = c("Illes Balears","Illes Balears","Illes Balears"),
    codigo_ine_provincia = c(07, 07, 07)
  )
  baleares_poblacion_sum <- baleares_poblacion %>%
    left_join(df_CCAA_sum, by = "CCAA") %>%
    group_by(CCAA) %>%
    summarise(poblacion_total = sum(poblacion_total, na.rm = TRUE))


  # Combinar los datos
  spain_provinces_shifted <- combined_df %>%
    filter(!NAME_LATN %in% c("Illes Balears","Canarias")) %>%
    bind_rows(canarias_shifted) %>% bind_rows(baleares)

  # Crear el mapa de las provincias de España coloreado por la variable total
  ggplot(data = spain_provinces_shifted) +
    geom_sf(aes(fill = poblacion_total)) +
    scale_fill_viridis_c(
      option = "D",
      labels = label_number(big.mark = ",", accuracy = 1)  # Formatea los números con comas y una cifra decimal
    ) +
    theme_minimal() +
    ggtitle(paste("Mapa de las Provincias de España Coloreado por", variable))

}
#' Mapa de Secciones Provinciales
#'
#' Esta función crea un mapa visualizando una variable específica por secciones provinciales,
#' utilizando los nombres de municipios y sus correspondientes códigos de provincia INE.
#' La función calcula la distancia de Levenshtein para encontrar el nombre de provincia más cercano
#' y realiza un join entre los municipios y un dataframe reducido basado en esta proximidad.
#'
#' @param df DataFrame que contiene los datos a mapear.
#' @param variable Nombre de la columna en `df` que contiene los valores a visualizar en el mapa.
#' @param provincia Nombre de la provincia para la cual se generará el mapa.
#'
#' @return Un objeto ggplot que representa el mapa de la provincia con las secciones coloreadas
#'         según los valores de la columna especificada. Los valores ausentes se representan en gris
#'
#' @importFrom dplyr filter full_join mutate
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_viridis_c labs theme_minimal theme
#' @importFrom stringdist stringdist  # if you use stringdist inside get_min_distancia_Levenshtein
#' @importFrom purrr map_chr  # if using purrr for the map_chr instead of sapply
#' @importFrom sf geom_sf  # if sf functions are used directly
#'
#' @examples
#' mapa_provincia_secciones(df, df$votos_en_blanco, "Valladolid")
#'
#' @export
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
        option = "D",
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
#' Calcular el Nombre del Municipio Más Cercano por Distancia de Levenshtein
#'
#' Esta función toma una cadena de texto y un vector con nombres de municipios, calcula la distancia de Levenshtein
#' para cada uno, y devuelve el nombre del municipio más cercano a la cadena de entrada.
#'
#' @param input_string Una cadena de entrada única para la cual se busca la coincidencia más cercana.
#' @param municipios Un vector de cadenas que contiene nombres de municipios.
#'
#' @return Devuelve el nombre del municipio que tiene la menor distancia de Levenshtein respecto a la cadena de entrada.
#'
#' @importFrom stringdist stringdist
#' @examples
#' municipios <- c("Barcelona", "Valencia", "Madrid", "Sevilla")
#' get_min_distancia_Levenshtein("Barcelon", municipios)
#'
#' @export
get_min_distancia_Levenshtein <- function(cadena, conjunto) {
  # Calcula la distancia de Levenshtein entre el string de entrada y cada municipio
  distances <- stringdist::stringdist(cadena, conjunto)

  # Encuentra el índice del valor mínimo en distances
  min_index <- which.min(distances)

  # Devuelve el nombre del municipio con la distancia mínima
  closest_municipio <- conjunto[min_index]
  return(closest_municipio)
}
# Ejemplo
mapa_provincia_secciones(df,df$prop_votos_blanco,"valladolid")
crear_mapa_provincias(df, df$votos_en_blanco)
df$prop_primera_vuelta<-df$votantes_primer_avance/df$censo_INE
crear_mapa_provincias(df, df$prop_primera_vuelta)
crear_mapa_CCAA(df,df$prop_primera_vuelta)
df$total_votantes_CERE
str(df$codigo_ine_provincia)

