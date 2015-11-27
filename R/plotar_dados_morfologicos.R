library(magrittr)
library(ggplot2)

#function to make plots
plot_morpho <- function(df, unit) {
  
  p <- 
    ggplot(data = df, aes(x = periodo, y = crescimento, group = localidade, color = localidade)) + 
    stat_summary(fun.data = mean_cl_boot, geom = "point", size = 4) +
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar") +
    stat_summary(fun.data = mean_cl_boot, geom = "line", size = 1.1) +
    xlab("") +
    ylab(sprintf("Taxa de crescimento (%s / dia)", unit)) +
    scale_color_fivethirtyeight() +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14)) 
  
  p_pos <-
    ggplot_build(p)$data[[3]] %>%
    dplyr::group_by(x) %>%
    dplyr::summarise(y = min(y), ymax = max(ymax)) %$%
    {ymax + min(y)}
  
  p_vals <- 
    df %>%
    split(.$periodo) %>%
    purrr::map(~ t.test(crescimento ~ localidade, data = .)) %>%
    purrr::map_dbl("p.value") %>%
    format(scientific = TRUE, digits = 2) %>%
    paste("p =", .)
  
  p + 
    annotate("text", x = seq_along(p_pos), y = p_pos, label = p_vals, size = 4)
}

#data
morpho_data <- readr::read_csv("../data/dados_morfologicos.csv")

png(paste0("../plots/altura.png"), width = 16, height = 8, res = 300, units = "in")
print(plot_morpho(dplyr::filter(morpho_data, variavel == "altura"), unit = "cm"))
dev.off()

png(paste0("../plots/diametro.png"), width = 16, height = 8, res = 300, units = "in")
print(plot_morpho(dplyr::filter(morpho_data, variavel == "altura"), unit = "mm"))
dev.off()

png(paste0("../plots/n_folhas.png"), width = 16, height = 8, res = 300, units = "in")
print(plot_morpho(dplyr::filter(morpho_data, variavel == "altura"), unit = "N folhas"))
dev.off()