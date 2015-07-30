# Install (if needed) and attach packages -------------------------------------#
packages <- c("readxl", "magrittr", "dplyr", "tidyr", "ggplot2", "Hmisc")
pkgs_to_install <- packages[which(!packages %in% installed.packages())]

if (length(pkgs_to_install)) install.packages(pkgs_to_install)

library(magrittr)
library(ggplot2)

# Functions -------------------------------------------------------------------#
## read data
read_data <- function(datafile = "../data/dados_morfologicos.xlsx", sheet) {
    
  x <- 
    readxl::read_excel(datafile, sheet = toupper(sheet), na = "NA") %>%
    .[sapply(., function(column) !all(is.na(column)))] %>%
    .[apply(., 1, function(row) !all(is.na(row))), ]
  
  names(x)[grep("^\\d{5}", names(x))] %<>%
    as.numeric() %>% 
    as.Date.numeric(origin = "1904-01-01") %>%
    as.character()
  
  x
}

## calculate growth rate
calc_growth_rate <- function(x) {
  
  date_cols <- grep("^\\d{4}-\\d{2}-\\d{2}", names(x))
  
  days_diff <- 
    diff(as.Date(names(x)[date_cols])) %>% 
    tidyr::extract_numeric()

  x[date_cols] %>% 
    apply(1, diff) %>% 
    sweep(1, days_diff, "/") %>% 
    t() %>%
    as.data.frame() %>%
    `names<-`(paste0("rate", 1:ncol(.))) %>%
    cbind(x, .)
}

## make plot
plot_var <- function(x, unit) {
  
  dates <- names(x)[grep("^\\d{4}-\\d{2}-\\d{2}$", names(x))]
  x_labels <- character(length(dates) - 1)
  for (i in seq_along(x_labels))
    x_labels[i] <- paste(dates[i], dates[i+1], sep = "\nto\n")
  
  x %<>% 
    dplyr::select(localidade, starts_with("rate")) %>%
    tidyr::gather(rate, value, -localidade) 
  
  p_pos <-
    x %>%
    dplyr::group_by(localidade, rate) %>%
    dplyr::summarise(p = mean(value, na.rm=TRUE) + sd(value, na.rm=TRUE)) %>%
    dplyr::group_by(rate) %>%
    dplyr::summarise(p = max(p) + min(x$value[x$value > 0], na.rm=TRUE)) %$%
    p

  p_vals <- 
    x %>% 
    dplyr::group_by(rate) %>% 
    dplyr::summarise_each(dplyr::funs(
      t.test(.[localidade == "PA"], .[localidade == "SC"])$p.value), value) %>%
    dplyr::mutate(value = format(value, scientific = TRUE, digits = 2)) %$%
    paste("p =", value)

  ggplot(x, aes(x = rate, y = value, group = localidade, color = localidade)) + 
    stat_summary(fun.y = "mean", geom = "point", size = 5) +
    stat_summary(fun.y = "mean", geom = "line", size = 1.2) +
    stat_summary(fun.data = "mean_sdl", geom = "errorbar", mult=1, width=.2) +
    annotate("text", x = seq_along(p_pos), y = p_pos, label = p_vals, size=6) +
    scale_x_discrete(labels = x_labels) +
    xlab("") +
    ylab(sprintf("Taxa de crescimento (%s / dia)", unit)) +
    scale_color_manual(name = "Location",
                       values = c("PA" = "tomato2", "SC" = "deepskyblue4")) +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18))
}

# read data, calculate growth rate and make plot ------------------------------#
run_pipeline <- function(sheet, unit) 
  read_data(sheet = sheet) %>% calc_growth_rate() %>% plot_var(unit = unit)

png("../plots/altura.png", width = 16, height = 8, res = 300, units = "in")
run_pipeline(sheet = "altura", unit = "cm")
dev.off()

png("../plots/diametro.png", width = 16, height = 8, res = 300, units = "in")
run_pipeline(sheet = "diametro", unit = "mm")
dev.off()

png("../plots/nfolhas.png", width = 16, height = 8, res = 300, units = "in")
run_pipeline(sheet = "n folhas", unit = "N folhas")
dev.off()