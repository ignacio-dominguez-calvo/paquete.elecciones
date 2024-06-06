
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
    INE_CODE = c("2", "3", "4", "1", "33", "5", "6", "7", "8", "48", "9", "10", "11", "39", "12", "13", "14", "15",
                 "16", "20", "17", "18", "19", "21", "22", "23", "24", "25", "27", "28", "29", "30", "31", "32", "34", "35",
                 "36", "26", "37", "38", "40", "41", "42", "43", "44", "45", "46", "47", "49", "50", "51", "52"),
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
    codigo_ine_provincia = c("38", "35", "35", "38", "38", "35", "38")
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
    codigo_ine_provincia = c("07", "07", "07")
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
    scale_fill_viridis_c(option = "C") +
    theme_minimal() +
    ggtitle(paste("Mapa de las Provincias de España Coloreado por", variable))
}

# Ejemplo
crear_mapa_provincias(df, df$numero_de_mesas)
