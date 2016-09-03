library(magrittr)

format_morpho <- function(input_file, sheet) {
  
  variavel <- gsub(" ", "_", tolower(sheet))
  
  x <- 
    readxl::read_excel(input_file, sheet = sheet, na = "NA") %>%
    .[, sapply(., function(x) !all(is.na(x)))] %>%
    .[apply(., 1, function(x) !all(is.na(x))), ] %>%
    tidyr::gather(data, valor, -c(mae, localidade, individuo)) %>%
    dplyr::mutate(data = data %>% as.character() %>% as.numeric() %>% 
                    as.Date(origin = "1904-01-01")) %>%
    dplyr::mutate(variavel = rep(variavel, nrow(.)))
  
  dates <- unique(x$data)
  prev_dates <- c(dates[1], dates[-length(dates)])
  prev <- data.frame(data = dates, data_previa = prev_dates)
  
  x %>%
    dplyr::inner_join(prev, by = "data") %>%
    dplyr::mutate(dias_passados = as.numeric(data - data_previa), 
                  periodo = paste(data_previa, data, sep = "\nto\n")) %>%
    dplyr::group_by(mae, localidade, individuo) %>%
    dplyr::mutate(crescimento = c(0, diff(valor))/dias_passados) %>%
    dplyr::ungroup() %>%
    dplyr::filter(dias_passados > 0) %>%
    dplyr::select(variavel, localidade, mae, individuo, periodo, crescimento) %>%
    dplyr::arrange(localidade, mae, individuo, periodo)
}

c("ALTURA", "DIAMETRO", "N FOLHAS") %>%
  lapply(. %>% format_morpho(input_file = "../data/dados_morfologicos.xlsx", 
                             sheet = .)) %>%
  dplyr::bind_rows() %>%
  readr::write_csv("../data/dados_morfologicos.csv")
