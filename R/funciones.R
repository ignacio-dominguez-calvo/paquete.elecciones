
#' Descargar Archivo
#'
#' Descarga un archivo desde la fuente de datos electorales del gobierno
#'
#' @param tipo_eleccion Tipo de elección. Puede ser 'referendum', 'congreso', 'senado', 'municipales', 'cabildos' o 'europeas'.
#' @param año Año de la elección.
#' @param mes Mes de la elección.
#' @param ambito Ámbito de la elección. Puede ser 'mesa', 'municipio' o 'superior'.
#' @param directorio Directorio donde se guardará el archivo descargado. Por defecto es "./descargas/".
#'
#' @return La ruta del directorio donde se ha descargado el archivo.
#'
#' @details Esta función descarga un archivo en base a unos parametros que define cada elección, de los que se genera una URL de la que obtener los datos.
#'
#'
#' @examples
#' # Descargar archivo de elecciones municipales del año 2023, mes 05, en el ámbito mesa
#' descargar_Archivo(tipo_eleccion = "municipales", año = 2023, mes = '05', ambito = "mesa")
#'
#' @importFrom utils download.file unzip
#' @export
#' @seealso https://infoelectoral.interior.gob.es/es/elecciones-celebradas/area-de-descargas/ URL base de descarga de archivos
#' @author Ignacio Domínguez Calvo
#' @keywords archivo descarga elección elecciones
#'
descargar_Archivo <- function(tipo_eleccion, año, mes, ambito, directorio = "./descargas/") {
  # Diccionarios con los parámetros admitidos para tipo eleccion y ambito y sus correspondencias


  tipo_eleccion<-comprueba_tipo_eleccion(tipo_eleccion)
  ambito<-comprueba_ambito(ambito)
  mes<-comprueba_mes(mes)

  #Si la carpeta especificada no existe, se crea
  if (!file.exists(directorio)) {
    dir.create(directorio)
  }
  url_base <- "https://infoelectoral.interior.gob.es/estaticos/docxl/apliextr/"
  nombre_archivo <- paste(tipo_eleccion, año, mes, '_', ambito, ".zip", sep = "")
  nombre_archivo <- trimws(nombre_archivo)
  url_completo <- paste(url_base, nombre_archivo, sep = "")
  directorio <- trimws(directorio)  # Eliminar espacio en blanco al final de directorio
  ruta <- file.path(directorio, nombre_archivo)  # Usar file.path para construir la ruta
  download.file(url_completo, destfile = ruta, mode = "wb")
  unzip(ruta, exdir = directorio, overwrite = FALSE, junkpaths = FALSE)  # Usar solo el directorio como exdir

  return(directorio)
}

#descargar_Archivo('02', '2019', '04', 'tota')


#' Comprueba el tipo de elección y devuelve su código correspondiente.
#'
#' Esta función toma un tipo de elección y devuelve su código correspondiente
#' según un diccionario predefinido.
#'
#' @param tipo_eleccion El tipo de elección a comprobar.
#' @return El código correspondiente al tipo de elección.
#' @examples
#' comprueba_tipo_eleccion("congreso")
comprueba_tipo_eleccion <- function(tipo_eleccion){
  tipo_eleccion<-tolower(tipo_eleccion)
  dict_elec <- list(
    'referendum' = '01',
    'referéndum' = '01',
    'congreso' = '02',
    'senado' = '03',
    'municipales' = '04',
    'cabildos' = '06',
    'europeas' = '07'
  )
  if (!tipo_eleccion %in% names(dict_elec)) {
    stop("El tipo de elección proporcionado no es válido. Tipos válidos: referendum, congreso,senado,municipales,cabildos,europeas")
  }

  return(dict_elec[[tipo_eleccion]])
}

#' Comprueba el ámbito y devuelve su código correspondiente.
#'
#' Esta función toma un ámbito y devuelve su código correspondiente según un diccionario predefinido.
#'
#' @param ambito El ámbito a comprobar.
#' @return El código correspondiente al ámbito.
#' @examples
#' comprueba_ambito("municipio")
comprueba_ambito <- function(ambito){
  ambito <- tolower(ambito)
  dict_ambito <-list(
    'mesa' = 'MESA',
    'municipio' = 'MUNI',
    'superior' = 'TOTA'
  )

  if (!ambito %in% names(dict_ambito)) {
    stop("El ámbito proporcionado no es válido. Ámbitos válidos: mesa, municipio o superior")
  }
  return(dict_ambito[[ambito]])
}

#' Comprueba el mes y lo formatea si es un número.
#'
#' Esta función comprueba si el mes es un número y lo formatea a dos dígitos si es necesario.
#'
#' @param mes El mes a comprobar y formatear.
#' @return El mes formateado.
#' @examples
#' comprueba_mes(5)
comprueba_mes<-function(mes){
  if (is.numeric(mes)) {
    # Si el mes es un número, lo formateamos a dos dígitos
    mes <- sprintf('%02d', mes)
  }
  return(mes)
}



leer_datos <- function(tipo_eleccion, año, mes, ambito, directorio = "./descargas/", tabla = '05') {

  directorio <- descargar_Archivo(tipo_eleccion, año, mes, ambito, directorio)
  tablas_path <- "C:\\Users\\nacho\\OneDrive\\Escritorio\\TFG ESTADISTICA\\paquete.elecciones\\inst\\extdata\\tablas_finales_2.xlsx"

  if (tabla == '11' || tabla == '12') {
    sheet_name = paste(tabla, "04aamm.DAT", sep = "")
  } else {
    sheet_name = paste(tabla, "xxaamm.DAT", sep = "")
  }

  tabla_variables <- read_excel(tablas_path, sheet = sheet_name)
  tabla_variables <- tabla_variables[-c(1, 2), , drop = FALSE]
  ultimos_dos_digitos <- substr(año, nchar(año) - 1, nchar(año))


  mes<-comprueba_mes(mes)
  tipo_eleccion<-comprueba_tipo_eleccion(tipo_eleccion)
  nombres_columnas <- unique(tabla_variables$Variable)
  nuevo_df <- data.frame(matrix(ncol = length(nombres_columnas)))
  names(nuevo_df) <- nombres_columnas

  ruta<- paste(directorio,tabla,tipo_eleccion,ultimos_dos_digitos,mes,".DAT",sep="")
  file_content <- readLines(ruta, encoding = 'UTF-8')
  file_content <- iconv(file_content, "latin1", "UTF-8")


  for (municipio in file_content) {
    # Inicializar un vector para almacenar los valores extraídos de cada línea
    valores <- c()
    # Iterar sobre las columnas "Inicio" y "Fin"
    for (i in 1:nrow(tabla_variables)) {
      # Extraer valores de cada línea
      valor <- substr(municipio, as.integer(tabla_variables[i, "Inicio"]), as.integer(tabla_variables[i, "Fin"]))
      # Convertir a entero si corresponde
      if (grepl("Num", tabla_variables[i, "Tipo"])) {
        valor <- as.integer(valor)
      }
      # Agregar el valor al vector
      valores <- c(valores, valor)
    }
    # Agregar los valores extraídos al DataFrame
    nuevo_df <- rbind(nuevo_df, valores)
  }

  # Eliminar la primera fila vacía generada por la inicialización del DataFrame vacío
  nuevo_df <- nuevo_df[-1, ]

  # Mostrar el nuevo DataFrame
  return(nuevo_df)
}

df <- leer_datos(tipo_eleccion = 'congreso', año = 2019, mes = '04', ambito = 'municipio')
df

if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}



devtools::install("C:/Users/nacho/OneDrive/Escritorio/TFG ESTADISTICA/paquete.elecciones")
library(paquete.elecciones)
leer_datos('congreso', '2019', '04', 'mesa')

system.file("extdata", "tablas_finales_2.xlsx", package = "paquete.elecciones")


library(usethis)
create_package("C:/Users/nacho/OneDrive/Escritorio/TFG ESTADISTICA/paquete.elecciones")

unlink(tempdir(), recursive = TRUE)

