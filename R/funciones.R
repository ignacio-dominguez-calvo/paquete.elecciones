
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
  dict_elec <- list(
    'referendum' = '01',
    'referéndum' = '01',
    'congreso' = '02',
    'senado' = '03',
    'municipales' = '04',
    'cabildos' = '06',
    'europeas' = '07'


  )
  dict_ambito <-list(
    'mesa' = 'MESA',
    'municipio' = 'MUNI',
    'superior' = 'TOTA'
  )

  tipo_eleccion<-tolower(tipo_eleccion)
  ambito <- tolower(ambito)

  # Comprueba que la clave proporcionada esté entre las claves permitidas
  if (!tipo_eleccion %in% names(dict_elec)) {
    stop("El tipo de elección proporcionado no es válido. Tipos válidos: referendum, congreso,senado,municipales,cabildos,europeas")
  }

  if (!ambito %in% names(dict_ambito)) {
    stop("El ámbito proporcionado no es válido. Ámbitos válidos: mesa, municipio o superior")
  }
  if (is.numeric(mes)) {
    # Si el mes es un número, lo formateamos a dos dígitos
    mes <- sprintf('%02d', mes)
  }
  print(mes)

  #Si la carpeta especificada no existe, se crea
  if (!file.exists(directorio)) {
    dir.create(directorio)
  }
  url_base <- "https://infoelectoral.interior.gob.es/estaticos/docxl/apliextr/"
  nombre_archivo <- paste(dict_elec[[tipo_eleccion]], año, mes, '_', dict_ambito[[ambito]], ".zip", sep = "")
  nombre_archivo <- trimws(nombre_archivo)
  url_completo <- paste(url_base, nombre_archivo, sep = "")
  directorio <- trimws(directorio)  # Eliminar espacio en blanco al final de directorio
  ruta <- file.path(directorio, nombre_archivo)  # Usar file.path para construir la ruta
  download.file(url_completo, destfile = ruta, mode = "wb")
  unzip(ruta, exdir = directorio, overwrite = FALSE, junkpaths = FALSE)  # Usar solo el directorio como exdir

  return(directorio)
}

#descargar_Archivo('02', '2019', '04', 'tota')


leer_datos <- function(tipo_eleccion, año, mes, ambito,directorio = "./descargas/",tabla='05'){
  directorio <- descargar_Archivo(tipo_eleccion,año,mes,ambito,directorio)
  tablas_path <- system.file("extdata", "tablas_finales_2.xlsx", package = "paquete.elecciones")
  print(tablas_path)
  if( tabla == '11'|| tabla == '12'){
    sheet_name = paste(tabla, "04aamm.DAT", sep = "")
  }else{
    sheet_name = paste(tabla, "xxaamm.DAT", sep = "")
  }
  tabla <- read_excel(tablas_path, sheet = sheet_name)
  print(tabla)
  return(FALSE)
  ultimos_dos_digitos <- substr(año, nchar(año) - 1, nchar(año))
  ruta<- paste(directorio,"05",tipo_eleccion,ultimos_dos_digitos,mes,".DAT",sep="")
  print(ruta)
  file_content <- readLines(ruta, encoding = 'UTF-8')
  file_content <- iconv(file_content, "latin1", "UTF-8")

  # Inicializar el dataframe vacío
  df <- data.frame(tipo_eleccion = integer(),
                   año = integer(),
                   mes = integer(),
                   numero_vuelta = integer(),
                   codigo_CCAA = integer(),
                   codigo_provincia = integer(),
                   codigo_municipio = integer(),
                   num_distrito_municipal = integer(),
                   nombre = character(),
                   codigo_distrito_electoral = integer(),
                   codigo_partido_judicial = integer(),
                   codigo_diputacion_provincial = integer(),
                   codigo_comarca = integer(),
                   pablacion_derecho = integer(),
                   numero_mesas = integer(),
                   censo_INE = integer(),
                   censo_escrutinio = integer(),
                   censo_CERE = character(),
                   total_votantes_CERE = integer(),
                   votantes_primer_avance = integer(),
                   votantes_segundo_avance = integer(),
                   voto_blanco = integer(),
                   voto_nulo = integer(),
                   voto_a_candidatura = character(),
                   numero_escaños = integer(),
                   votos_afirmativos_referendum = integer(),
                   votos_negativos_referendum = integer(),
                   stringsAsFactors = FALSE)

  # Recorrer cada línea en file_content
  for(municipio in file_content) {
    # Extraer valores de cada línea y asignar a las columnas correspondientes
    tipo_eleccion <- as.integer(substr(municipio, 1, 2))
    año <- as.integer(substr(municipio, 3, 6))
    mes <- as.integer(substr(municipio, 7, 8))
    numero_vuelta <- as.integer(substr(municipio, 9, 9))
    codigo_CCAA <- as.integer(substr(municipio, 10, 11))
    codigo_provincia <- as.integer(substr(municipio, 12, 13))
    codigo_municipio <- as.integer(substr(municipio, 14, 16))
    num_distrito_municipal <- as.integer(substr(municipio, 17, 18))
    nombre <- substr(municipio, 19, 118)
    codigo_distrito_electoral <- as.integer(substr(municipio, 119, 119))
    codigo_partido_judicial <- as.integer(substr(municipio, 120, 122))
    codigo_diputacion_provincial <- as.integer(substr(municipio, 123, 125))
    codigo_comarca <- as.integer(substr(municipio, 126, 128))
    pablacion_derecho <- as.integer(substr(municipio, 129, 136))
    numero_mesas <- as.integer(substr(municipio, 137, 141))
    censo_INE <- as.integer(substr(municipio, 142, 149))
    censo_escrutinio <- as.integer(substr(municipio, 150, 157))
    censo_CERE <- substr(municipio, 158, 165)
    total_votantes_CERE <- as.integer(substr(municipio, 166, 173))
    votantes_primer_avance <- as.integer(substr(municipio, 174, 181))
    votantes_segundo_avance <- as.integer(substr(municipio, 182, 189))
    voto_blanco <- as.integer(substr(municipio, 190, 197))
    voto_nulo <- as.integer(substr(municipio, 198, 205))
    voto_a_candidatura <- substr(municipio, 206, 213)
    numero_escaños <- as.integer(substr(municipio, 214, 216))
    votos_afirmativos_referendum <- as.integer(substr(municipio, 217, 224))
    votos_negativos_referendum <- as.integer(substr(municipio, 225, 232))

    # Agregar los valores extraídos al dataframe
    df <- rbind(df, data.frame(tipo_eleccion, año, mes, numero_vuelta, codigo_CCAA,
                               codigo_provincia, codigo_municipio, num_distrito_municipal,
                               nombre, codigo_distrito_electoral, codigo_partido_judicial,
                               codigo_diputacion_provincial, codigo_comarca, pablacion_derecho,
                               numero_mesas, censo_INE, censo_escrutinio, censo_CERE,
                               total_votantes_CERE, votantes_primer_avance,
                               votantes_segundo_avance, voto_blanco, voto_a_candidatura,
                               numero_escaños, votos_afirmativos_referendum,
                               votos_negativos_referendum))

  }
  return(df)
}

library(readxl)

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

