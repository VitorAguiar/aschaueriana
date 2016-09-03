library(magrittr)
library(ggplot2)
library(ggthemes)

#function to make plots
plot_weather <- function(plot_df) {
  
  ylabel <- plot_df %$% paste0(variavel[1], " (", unidade[1], ")")
  
  ggplot(data = plot_df, aes(x = mes, y = valor, group = local, color = local)) +
    stat_summary(fun.data = mean_cl_boot, geom = "point", size = 4) +
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar") +
    stat_summary(fun.data = mean_cl_boot, geom = "line", size = 1.1) +
    scale_color_fivethirtyeight() +
    ylab(ylabel) +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 10),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14))
}

#data
weather_data <- 
  readr::read_csv("../data/dados_climaticos.csv") %>%
  tidyr::gather(variavel, valor, -(local:mes)) %>%
  tidyr::separate(variavel, c("variavel", "unidade"), 
                  sep = "_\\(|\\)_", extra = "drop") %>%
  dplyr::mutate(mes = factor(mes, levels = unique(mes)))

variables <- unique(weather_data$variavel)

#save plots
for (v in variables) {
  png(paste0("../plots/", v, ".png"), 
      width = 12, height = 6, res = 300, units = "in")
  print(plot_weather(dplyr::filter(weather_data, variavel == v)))
  dev.off()
}