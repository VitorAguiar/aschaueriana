library(ggplot2)

#function to make plots
plot_vars <- function(plot_df) {
  
  ylabel <- plot_df %$% paste0(variable[1], " (", unit[1], ")")
  
  ggplot(data = plot_df, 
         aes(x = month, y = average, group = local, color = local)) +
    geom_errorbar(aes(ymin = average - sd, ymax = average + sd), 
                  stat = "identity", width = 0.2) +
    geom_line(size = 1.1) +
    geom_point(size = 5) +
    scale_color_manual(name = "Location",
                       values = c("salinopolis_pa" = "tomato2", 
                                  "florianopolis_sc" = "deepskyblue4", 
                                  "santa_marta_sc" = "mediumturquoise"),
                       labels = c("salinopolis_pa" = "Salinópolis, PA",
                                  "florianopolis_sc" = "Florianópolis, SC",
                                  "santa_marta_sc" = "Santa Marta, SC")) +
    ylab(ylabel) +
    xlab("month") +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18))
}

#data
weather_data <- 
  readr::read_csv("../data/dados_climaticos.csv") %>%
  tidyr::gather(variable, value, -c(local:mes)) %>%
  tidyr::separate(variable, c("variable", "unit"), 
                  sep = "_\\(|\\)_", extra = "drop") %>%
  dplyr::mutate(month = factor(mes, levels = c("Jan", "Fev", "Mar", "Abr", "Mai",
                                             "Jun", "Jul", "Ago", "Set", "Out",
                                             "Nov", "Dez"))) %>%
  dplyr::group_by(variable, unit, local, month) %>%
  dplyr::summarise(average = mean(value, na.rm = TRUE), 
                   sd = sd(value, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  split(.$variable)

#save plots
for (i in names(weather_data)) {
  png(paste0("../plots/", i, ".png"), 
      width = 12, height = 6, res = 300, units = "in")
  print(plot_vars(weather_data[[i]]))
  dev.off()
}