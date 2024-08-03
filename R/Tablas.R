


library(dplyr)
library(readxl)
source("./R/LecturaDatos.R")
#' Calcula y muestra tablas de participación basadas en el tipo de elección y provincia
#'
#' Esta función carga datos electorales para un tipo y fecha especificados,
#' opcionalmente filtrando por provincia. Luego calcula resúmenes de participación
#' y muestra los resultados en forma tabular. Si se especifica una provincia,
#' filtra los datos para esa provincia usando los códigos INE.
#'
#' @param tipo_eleccion Tipo de elección como texto.
#' @param año Año de la elección.
#' @param mes Mes de la elección.
#' @param provincia Nombre de la provincia (opcional).
#' @return Un dataframe invisible con los resultados de participación y una
#'   impresión en consola de los resultados detallados
#' @import dplyr
#' @export
tabla_participacion <- function(tipo_eleccion, año, mes, provincia=NULL) {
  ambito='municipio'
  if(!is.null(provincia)){
    provincia<-tolower(provincia)
  }

  if(!is.null(provincia)) {
    ine_codes <- data.frame(
      INE_CODE = as.numeric(c(2, 3, 4, 1, 33, 5, 6, 7, 8, 48, 9, 10, 11, 39, 12, 13, 14, 15,
                              16, 20, 17, 18, 19, 21, 22, 23, 24, 25, 27, 28, 29, 30, 31, 32, 34, 35,
                              36, 26, 37, 38, 40, 41, 42, 43, 44, 45, 46, 47, 49, 50, 51, 52)),
      PROVINCE = tolower(c("Albacete", "Alicante", "Almería", "Araba/Álava", "Asturias", "Ávila", "Badajoz", "Balears, Illes",
                   "Barcelona", "Bizkaia", "Burgos", "Cáceres", "Cádiz", "Cantabria", "Castellón", "Ciudad Real",
                   "Córdoba", "A Coruña", "Cuenca", "Gipuzkoa", "Girona", "Granada", "Guadalajara", "Huelva", "Huesca",
                   "Jaén", "León", "Lleida", "Lugo", "Madrid", "Málaga", "Murcia", "Navarra", "Ourense", "Palencia",
                   "Las Palmas", "Pontevedra", "La Rioja", "Salamanca", "Santa Cruz de Tenerife", "Segovia", "Sevilla",
                   "Soria", "Tarragona", "Teruel", "Toledo", "Valencia", "Valladolid", "Zamora", "Zaragoza",
                   "Ceuta", "Melilla"))
    )
    provincia_code <- ine_codes$INE_CODE[ine_codes$PROVINCE == provincia]
    if (length(provincia_code) > 0) {
      df <- leer_tabla(tipo_eleccion, año, mes, ambito, directorio = "./descargas/", tabla = '05')
      df <- df[df$codigo_ine_provincia == provincia_code, ]
      df_final <- resultados_tabla_participacion(df,tipo_eleccion)
    } else {
      stop(paste("Provincia no válida. Lista de provincias válidas:", paste(ine_codes$PROVINCE, collapse=", ")))

    }
  }
  else{

    df_final <- resultados_tabla_participacion(df,tipo_eleccion)}

  return(invisible(df_final))
}



tabla_participacion('referendum',2005,2,'valladolid')
