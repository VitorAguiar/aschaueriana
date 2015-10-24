library(magrittr)

format_weather <- function(input_file) {
  
  location <- gsub("^.*?([^/]+)\\.xlsx?$", "\\1", input_file) %>% tolower()
  
  lc <- 
    readxl::read_excel(input_file, col_names = FALSE) %>% 
    dplyr::slice(-c(1:8)) %>%
    dplyr::filter(!is.na(.[[1]]))
  
  names_lc <- lc[[1]] %>% .[grep("Ano", .) - 1]
  
  lc %>% 
    split(cumsum(.[[1]] %in% names_lc)) %>% 
    lapply(. %>%
            `names<-`(.[2, ]) %>%
            dplyr::mutate(variable = rep(Ano[1], nrow(.))) %>%
            dplyr::slice(-c(1:2)) %>% 
            dplyr::select(variable, Ano, Jan:Dez)
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(local = rep(location, nrow(.))) %>%
    dplyr::select(local, variable, Ano, Jan:Dez) %>%
    tidyr::gather(mes, valor, Jan:Dez) %>% 
    tidyr::spread(variable, valor) %>%
    dplyr::mutate_each(dplyr::funs(. %>% as.numeric()), -c(local, mes)) %>%
    `names<-`(sub("-", " ", names(.)) %>% gsub("[ ]+", "_", .) %>% tolower())
}

c("FLORIANOPOLIS_SC.xlsx", "SANTA_MARTA_SC.xlsx", "SALINOPOLIS_PA.xls") %>%
  paste0("../data/", .) %>%
  lapply(. %>% format_weather) %>%
  dplyr::bind_rows() %>%
  readr::write_csv("../data/dados_climaticos.csv")
