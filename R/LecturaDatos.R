
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
  if (!file.exists(ruta)) {
    download.file(url_completo, destfile = ruta, mode = "wb")
    unzip(ruta, exdir = directorio, overwrite = FALSE, junkpaths = FALSE)  # Usar solo el directorio como exdir
  } else {
    message("El archivo ya existe en el directorio especificado.")
  }
  return(directorio)
}



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



#' Leer Datos de Elecciones
#'
#' Esta función descarga y procesa archivos de datos electorales según los parámetros especificados.
#'
#' @param tipo_eleccion Un carácter que indica el tipo de elección de entre referendum,congreso,senado,municipales,cabildos o europeas.
#' @param año Un número que indica el año de la elección.
#' @param mes Un número que indica el mes de la elección.
#' @param ambito Un carácter que indica el ámbito de la elección que puede ser mesa,municipio o superios.
#' @param directorio Un carácter que indica el directorio donde se guardarán los archivos descargados. Por defecto es "./descargas/".
#' @param tabla Un carácter que indica el código de la tabla de datos a procesar. Por defecto es '05'.
#' @return Un data.frame con los datos procesados de las elecciones.
#' @importFrom readxl read_excel
#' @export
leer_tabla <- function(tipo_eleccion, año, mes, ambito, directorio = "./descargas/", tabla = '05') {

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

  lista_valores <- vector("list", length = length(file_content))

  for (index in seq_along(file_content)) {
    municipio <- file_content[index]
    # Extraer los valores para la línea actual
    valores <- mapply(function(inicio, fin, tipo) {
      valor <- substr(municipio, as.integer(inicio), as.integer(fin))
      if (grepl("Num.", tipo)) {
        return(as.integer(valor))
      }
      return(valor)
    }, tabla_variables$Inicio, tabla_variables$Fin, tabla_variables$Tipo)

    # Almacenar los valores en la lista
    lista_valores[[index]] <- valores
  }

  # Convertir la lista en un DataFrame
  nuevo_df <- do.call(rbind, lista_valores)
  if ("nombre_municipio" %in% names(nuevo_df)) {
    nuevo_df[, nombre_municipio <- trimws(nombre_municipio, which = "right")]
  }

  # Transformar columnas que se puedan convertir a número
  nuevo_df <- as.data.frame(nuevo_df)  # Asegúrate de que es un dataframe
  nuevo_df[] <- lapply(nuevo_df, function(x) {
    if(is.character(x)) {
      # Intenta convertir a numérico
      y <- as.numeric(x)
      # Si la conversión falla completamente (solo NA's), devuelve x original, de lo contrario y
      if(all(is.na(y))) x else y
    } else {
      x  # Devuelve la columna sin cambios si no es de tipo caracter
    }
  })
  names(nuevo_df) <- nombres_columnas

  # Mostrar el nuevo DataFrame
  return(nuevo_df)
}

#' Obtener Datos de Elecciones Filtrados
#'
#' Esta función lee un archivo CSV que contiene el histórico de elecciones y a partir de ella devuelve
#' todas las elecciones que se han celebrado hasta el momento.
#'
#' @param tipo El tipo de elección a filtrar, como 'referendum', 'congreso', etc.
#'        Si se especifica, la función filtrará los datos para incluir solo las entradas
#'        que correspondan a este tipo. Debe ser uno de los siguientes:
#'        'referendum', 'congreso', 'senado', 'municipales', 'cabildos', 'europeas'.
#'        Si es `NULL`, no se aplica ningún filtro por tipo.
#' @param año El año de las elecciones a filtrar.
#'        Si se especifica, la función filtrará los datos para incluir solo las entradas
#'        de ese año específico. Si es `NULL`, no se aplica ningún filtro por año.
#'
#' @return Un data.frame que contiene las columnas del archivo CSV original,
#'         pero filtrado según los criterios especificados y con los códigos de tipo
#'         de elección traducidos a texto.
#' @examples
#' get_elecciones(tipo = "congreso", año = 2021)
#' get_elecciones(año = 2020)
#' get_elecciones(tipo = "municipales")
#' @import dplyr
#' @export
#'
#' @importFrom readr read_csv
get_elecciones <- function(tipo = NULL, año = NULL) {
  # Diccionario de tipo a código
  dict_elec_text_to_num <- list(
    'referendum' = '01',
    'congreso' = '02',
    'senado' = '03',
    'municipales' = '04',
    'cabildos' = '06',
    'europeas' = '07'
  )

  # Diccionario de código a tipo
  dict_elec_num_to_text <- list(
    '1' = 'referendum',
    '2' = 'congreso',
    '3' = 'senado',
    '4' = 'municipales',
    '6' = 'cabildos',
    '7' = 'europeas'
  )
  datos <- read.csv("C:/Users/nacho/OneDrive/Escritorio/TFG ESTADISTICA/paquete.elecciones/inst/extdata/entradas_validas.csv", stringsAsFactors = FALSE)

  if (!is.null(tipo) && tipo %in% names(dict_elec_text_to_num)) {
    codigo_tipo <- dict_elec_text_to_num[[tipo]]
    datos <- datos %>% filter(Tipo == codigo_tipo)
  }

  if (!is.null(año)) {
    datos <- datos %>% filter(Año == año)
  }

  # Traducir los códigos de tipo a texto
  datos$Tipo <- sapply(datos$Tipo, function(x) dict_elec_num_to_text[[as.character(x)]])

  return(datos)
}
#' Obtener los prefijos de tablas disponibles para un tipo de elección específico
#'
#' Esta función devuelve los prefijos de los ficheros asociados con un tipo específico
#' de elección, como referéndum, congreso, senado, etc.
#'
#' @param tipo_eleccion Una palabra que indica el tipo de eleccón.
#' @return Un vector de caracteres con los prefijos de ficheros para el tipo de elección especificado.
#' @examples
#' get_tablas_disponibles('02')
#' @export
get_tablas_disponibles<-function(tipo_eleccion){
  tipo_eleccion<-comprueba_tipo_eleccion(tipo_eleccion)
  prefijos_por_tipo <- list(
    '01' = c("05", "07", "09"),   # Referéndum
    '02' = c("03", "04", "05", "06", "07", "08", "09", "10"),   # Congreso
    '03' = c("03", "04", "05", "06", "07", "08", "09", "10"),   # Senado
    '04' = c("03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),  # Municipales
    '06' = c("03", "04", "05", "06", "07", "08", "09", "10"),   # Cabildos
    '07' = c("03", "04", "05", "06", "07", "08", "09", "10")    # Europeas
  )

  return(prefijos_por_tipo[[tipo_eleccion]])
}#' Descripción de la tabla basada en el prefijo numérico
#'
#' Esta función devuelve la descripción asociada a un prefijo de fichero específico,
#' como los ficheros de control, identificación del proceso electoral, etc.
#'
#' @param tabla Un carácter que indica el prefijo de la tabla.
#'        Debe ser un número de dos dígitos como '01', '02', etc.
#' @return Una cadena con la descripción del fichero asociado al prefijo dado.
#' @examples
#' descripcion_tabla('09')
#' @export
descripcion_tabla <- function(tabla) {
  # Asegurarse de que el número esté en formato de dos dígitos
  numero_formateado <- sprintf("%02d", as.integer(tabla))

  # Diccionario que mapea números de fichero a descripciones
  descripciones <- list(
    '01' = 'Fichero de CONTROL de los ficheros que componen el proceso electoral.',
    '02' = 'Fichero de IDENTIFICACION del proceso electoral.',
    '03' = 'Fichero de CANDIDATURAS.',
    '04' = 'Fichero de RELACION DE CANDIDATOS.',
    '05' = 'Fichero de DATOS COMUNES DE MUNICIPIOS.',
    '06' = 'Fichero de DATOS DE CANDIDATURAS DE MUNICIPIOS.',
    '07' = 'Fichero de DATOS COMUNES DE AMBITO SUPERIOR AL MUNICIPIO.',
    '08' = 'Fichero de DATOS DE CANDIDATURAS DE AMBITO SUPERIOR AL MUNICIPIO.',
    '09' = 'Fichero de DATOS COMUNES DE MESAS y del C.E.R.A.',
    '10' = 'Fichero de DATOS DE CANDIDATURAS DE MESAS y del C.E.R.A.',
    '11' = 'Fichero de DATOS COMUNES DE MUNICIPIOS menores de 250 habitantes. (Solo en Elecciones Municipales)',
    '12' = 'Fichero de DATOS DE CANDIDATURAS DE MUNICIPIOS menores de 250 hab. (Solo en Elecciones Municipales)'
  )

  # Devolver la descripción correspondiente al número de fichero formateado
  if (numero_formateado %in% names(descripciones)) {
    return(descripciones[[numero_formateado]])
  } else {
    return("Número de fichero no reconocido.")
  }
}
#' Leer Varias Tablas de Datos Electorales
#'
#' Esta función descarga y procesa múltiples archivos de datos electorales basados en los parámetros especificados.
#' Cada archivo representa un conjunto de datos asociado a un tipo de elección, año y mes específicos,
#' y es procesado de acuerdo con su ámbito particular.
#'
#' @param tipo_eleccion Un carácter que indica el tipo de elección de entre: referendum, congreso, senado, municipales, cabildos o europeas.
#' @param año Un número que indica el año de la elección.
#' @param mes Un número que indica el mes de la elección.
#' @param ambito Un carácter que indica el ámbito de la elección que puede ser mesa, municipio o superior.
#' @param directorio Un carácter que indica el directorio donde se guardarán los archivos descargados.
#'        Por defecto es "./descargas/".
#' @param tablas Un vector de caracteres que contiene los códigos de las tablas de datos a procesar.
#' @return Una lista de data.frames, cada uno conteniendo los datos procesados de una tabla específica.
#' @examples
#' tablas <- c("05", "07", "09")
#' datos_electorales <- leer_varias_tablas("congreso", 2021, 6, "nacional", "./descargas/", tablas)
#' @export
#' @importFrom readr read_csv
#' @importFrom dplyr bind_rows
leer_varias_tablas <- function(tipo_eleccion, año, mes, ambito, directorio = "./descargas/", tablas) {
  # Usar lapply para aplicar la función leer_datos a cada elemento del vector tablas
  dfs <- lapply(tablas, function(tabla) {
    leer_tablas(tipo_eleccion, año, mes, ambito, directorio, tabla)
  })
  return(dfs)
}
descripcion_tabla(4)
get_tablas_disponibles("congreso")
get_elecciones(año=2019)



system.time({
  df <- leer_datos2(tipo_eleccion = 'congreso', año = 2019, mes = '04', ambito = 'municipio')
})





