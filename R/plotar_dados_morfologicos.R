# Install (if needed) and attach packages -------------------------------------#

packages <- c("readxl", "magrittr", "dplyr", "tidyr", "ggplot2")
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
  
  calc_fun <- function(x, first, last) {
  
    days_diff <- 
      as.Date(names(x)[last], format = "%Y-%m-%d") - 
      as.Date(names(x)[first], format = "%Y-%m-%d")
      
    days_diff %<>% tidyr::extract_numeric()

    x[c(first, last)] %>% apply(1, diff) / days_diff
  }

  date_cols <- grep("^\\d{4}-\\d{2}-\\d{2}", names(x))
  for (i in date_cols[-length(date_cols)]) {
    x[ncol(x) + 1] <- calc_fun(x, i, i+1)
    names(x)[ncol(x)] <- paste0("rate", i-(date_cols[1]-1))
  }
  
  x
}

## make plot
plot_var <- function(x, unit) {
  
  # plot data 
  summary_stats <-
    x %>% 
    dplyr::select(localidade, starts_with("rate")) %>%
    dplyr::group_by(localidade) %>%
    dplyr::summarise_each(dplyr::funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE)), 
                          starts_with("rate")) %>%
    tidyr::gather(rate, value, -localidade) %>%
    tidyr::extract(rate, c("rate", "stat"), "(rate.)_(.+)") %>%
    tidyr::spread(stat, value)
  
  # Plot parameters:
  
  ## parameter 1: pvalues of the difference in growth rates between diff locals
  p_values <- sapply(dplyr::select(x, starts_with("rate")),
                     function(rate) t.test(rate ~ x$localidade)$p.value)
  
  ## parameter 2: location of the pvalues on the plot
  p_plot_location <-  
    summary_stats %>% 
    dplyr::group_by(rate) %>% 
    dplyr::summarise(m_plus_sd = max(mean + sd)) %$% 
    {m_plus_sd + min(summary_stats$mean)} 
  
  ## parameter 3: labels of the x axis
  dates <- names(x)[grep("^\\d{4}-\\d{2}-\\d{2}$", names(x))]
  x_labels <- character()
  for (i in 1:(length(dates)-1)) {
    x_labels <- c(x_labels, paste(dates[i], dates[i+1], sep = "\nto\n"))
  }
  
  # Make plot
  ggplot(summary_stats, 
         aes(x = rate, y = mean, group = localidade, color = localidade)) + 
    geom_point(size = 5) +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), 
                  stat = "identity", width = 0.2) +
    geom_line(size = 1.2) +
    scale_x_discrete(labels = x_labels) +
    xlab("") +
    ylab(sprintf("Taxa de crescimento (%s / dia)", unit)) +
    scale_color_manual(name = "Localidade",
                       values = c("PA" = "tomato2", "SC" = "deepskyblue4")) +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18)) +
    annotate("text", x = 1:length(p_plot_location), y = p_plot_location,
             label = paste("p =", format(p_values, digits = 2)), size = 6)
}

# read data, calculate growth rate and make plot ------------------------------#
png("../plots/altura.png", width = 16, height = 8, res = 300, units = "in")
read_data(sheet = "altura") %>% 
  calc_growth_rate() %>% 
  plot_var(unit = "cm")
dev.off()

png("../plots/diametro.png", width = 16, height = 8, res = 300, units = "in")
read_data(sheet = "diametro") %>% 
  calc_growth_rate() %>%
  plot_var(unit = "mm")
dev.off()

png("../plots/nfolhas.png", width = 16, height = 8, res = 300, units = "in")
read_data(sheet = "n folhas") %>% 
  calc_growth_rate() %>%
  plot_var(unit = "N folhas")
dev.off()