library(readxl)
library(magrittr)
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)

file_names <- 
  c("FLORIANOPOLIS_SC.xlsx", "SANTA_MARTA_SC.xls", "SALINOPOLIS_PA.xls")

format_data <- function(f) {

  lc <- read_excel(f, col_names = FALSE)

  lc[lc == "Null"] <- NA
  
  names_start <- which(lc[[1]] == "Ano")[1] - 1
  
  suppressWarnings(names_lc <- 
    which(is.na(as.numeric(lc[[1]])) & lc[[1]] != "Ano") %>% 
  .[. >= names_start] %>%
  lc[[1]][.])

  wrong_name <- grep("RADIACAO.*TOTAL", names_lc)
  if (length(wrong_name)) 
    names_lc[wrong_name] <- gsub("TOTAL", "MÉDIA", names_lc[6])

  lc %<>% 
  filter(!is.na(.[[1]])) %>%
  .[grep("(Ano)|\\d+", .[[1]]), ] 

  seps <- grep("(Ano)", lc[[1]])
  year_range <- lc[[1]] %>% table %>% .[names(.) != "Ano"] %>% as.numeric()
  
  list_lc <- vector("list", length(seps))

  counter <- 1
  for (i in seps) {
    
    if (i != last(seps)) 
      i_years <- 1:nrow(lc) %>% .[. > nth(seps, i) & . < nth(seps, i+1)]
    if(i == last(seps)) 
      i_years <- 1:nrow(lc) %>% .[. > nth(seps, i) & . <= nrow(lc)]

    list_lc[[counter]] <- lc[c(i:(i+7)), ]
    counter %<>% `+`(1)
  }

  list_lc %<>% 
    lapply(function(x) {
      names(x) <- as.character(x[1,])
      x %<>% .[-1,]
      x})

  names(list_lc) <- names_lc

  for (i in seq_along(list_lc)) 
    list_lc[[i]] %<>% mutate(var = names_lc[i])

  lc_df <- 
    rbind_all(list_lc) %>%
    filter(!is.na(Ano)) %>%
    select(var, Ano:Dez) %>% 
    gather(month, value, Jan:Dez) %>%
    spread(var, value) %>%
    mutate(local = strsplit(f[1], "\\.")[[1]][1]) %>%
    select(local, which(!names(.) %in% "local"))
}

all_data <- 
  lapply(file_names, format_data) %>%
  rbind_all()

output_file <- "dados_climaticos.csv" 
write.csv(all_data, output_file , row.names = FALSE, quote = FALSE)
message(paste(output_file, "was saved to your working directory"))
