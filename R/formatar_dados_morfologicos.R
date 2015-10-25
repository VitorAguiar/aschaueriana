library(magrittr)

format_morpho <- function(input_file, sheet) {
  
  x <- 
    readxl::read_excel(input_file, sheet = sheet, na = "NA") %>%
    tidyr::gather(date, value, -c(mae, localidade, individuo)) %>%
    dplyr::mutate(date = as.numeric(levels(date))[date] %>% 
                    as.Date(origin = "1904-01-01"))
  
  days_diff <- 
    dplyr::data_frame(date = unique(x$date),
                      days = c(0, unique(x$date) %>% diff() %>% as.numeric()),
                      rate = 0:(dplyr::n_distinct(x$date) - 1))
  
  x %>% 
    dplyr::inner_join(days_diff, by = "date") %>% 
    dplyr::group_by(mae, localidade, individuo) %>%
    dplyr::mutate(growth = c(0, diff(value))/days) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(variable = rep(gsub(" ", "_", tolower(sheet)), nrow(.))) %>%
    dplyr::select(variable, mae:growth) %>%
    dplyr::rename(mother = mae, location = localidade, individual = individuo)
}

c("ALTURA", "DIAMETRO", "N FOLHAS") %>%
  lapply(. %>% format_morpho(input_file = "../data/dados_morfologicos.xlsx", 
                         sheet = .)) %>%
  dplyr::bind_rows() %>%
  readr::write_csv("../data/dados_morfologicos.csv")