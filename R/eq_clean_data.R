#' eq_clean_data
#'
#' This function cleans and readies the U.S. National Oceanographic
#' and Atmospheric Administration (NOAA) dataset on significant earthquakes
#' around the world
#'
#' https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1
#'
#' #' You can use this function in two ways. Assigning the file_name (e.g. ../path/my_file.txt) with path or piping it
#' as sequence of a read_delim.
#'
#' @param x You can insert the file_name and path to reach the tsv file or pipe it with a read_delim.
#'
#' @return A DataFrame version of file_name.
#'
#' @importFrom magrittr %>%
#'
#' @importFrom dplyr mutate
#'
#' @importFrom lubridate ymd
#'
#' @importFrom readr read_delim
#'
#' @importFrom rlang .data
#'
#' @examples
#'
#' \dontrun{
#' # Loading using read_delim function and then cleaning the DataFrame.
#' readr::read_delim("inst/extdata/signif.txt",
#'                   delim = "\t") %>%
#'                           eq_clean_data()
#'
#' # Piping a DataFrame.
#' any_dataframe %>% eq_clean_data()
#'
#' # Assigning a file_name
#' eq_clean_data(file_name = "my_folder/signif.txt")}
#'
#' @export
#'
#'
#'
#'


eq_clean_data <- function(x) {
                DATE <- NULL
                LATITUDE <- NULL
                LONGITUDE <- NULL
                data <- readr::read_tsv(x, col_names = TRUE, col_types = NULL) %>%
                        tidyr::unite(DATE, 'YEAR', 'MONTH', 'DAY') %>%
                        dplyr::mutate(DATE = lubridate::ymd(DATE)) %>%
                        dplyr::mutate(LATITUDE = as.numeric('LATITUDE')) %>%
                        dplyr::mutate(LONGITUDE = as.numeric('LONGITUDE'))
                return(data)
        }



#' Function to clean the LOCATION_NAME field in the NOAA
#' earthquake dataset
#'
#' @param data NOAA signification earthquake dataset
#'
#' @return data.frame with cleaned LOCATION_NAME field
#'
#' @examples \dontrun{eq_location_clean(data)}
#' @export

eq_location_clean <- function(data) {
        data <- data %>%
                dplyr::mutate_(
                        LOCATION_NAME =
                                ~purrr::map2_chr(COUNTRY, LOCATION_NAME,
                                                 function(COUNTRY, LOCATION_NAME) {
                                                         gsub(paste0(COUNTRY, ":"), '', LOCATION_NAME)
                                                 }),
                        LOCATION_NAME = ~stringr::str_trim(LOCATION_NAME),
                        LOCATION_NAME = ~stringr::str_to_title(LOCATION_NAME)
                )

        data
}




