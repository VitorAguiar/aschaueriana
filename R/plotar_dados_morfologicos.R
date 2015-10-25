library(magrittr)
library(ggplot2)

#function to make plots
plot_morpho <- function(x, unit) {
  
  dates <- unique(x$date)
  xlabels <- 
    sapply(2:length(dates), function(i)
      paste(dates[i-1], dates[i], sep = "\nto\n"))
  
  plot_data <- 
    dplyr::filter(x, rate > 0) %>%
    dplyr::group_by(location, rate) %>%
    dplyr::summarise(ave_growth = mean(growth, na.rm = TRUE),
                     sd = sd(growth, na.rm = TRUE))
  
  p <- 
    ggplot(plot_data, 
           aes(x = rate, y = ave_growth, 
               group = location, color = location)) + 
    geom_line(size = 1.1) +
    geom_point(size = 5) +
    geom_errorbar(aes(ymin = ave_growth - sd, ymax = ave_growth + sd),
                  stat = "identity", width = 0.2) +
    scale_x_discrete(limits = unique(plot_data$rate), labels = xlabels) +
    xlab("") +
    ylab(sprintf("Taxa de crescimento (%s / dia)", unit)) +
    scale_color_manual(name = "Location",
                       values = c("PA" = "tomato2", "SC" = "deepskyblue4")) +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18))
    
  p_pos <-
    ggplot_build(p)$data[[3]] %>%
    dplyr::group_by(x) %>%
    dplyr::summarise(y = min(y), ymax = max(ymax)) %$%
    {ymax + min(y)}

  p_vals <- 
    x %>%
    dplyr::filter(rate > 0) %>% 
    dplyr::group_by(rate) %>% 
    dplyr::summarise_each(dplyr::funs(
      t.test(.[location == "PA"], .[location == "SC"])$p.value), growth) %>%
    dplyr::mutate(p_value = format(growth, scientific = TRUE, digits = 2)) %$%
    paste("p =", p_value)

  p + 
    annotate("text", x = seq_along(p_pos), y = p_pos, label = p_vals, size = 6)
}

#data
morpho_data <- 
  readr::read_csv("../data/dados_morfologicos.csv") %>%
  split(.$variable)

units <- c("cm", "mm", "N folhas") %>% `names<-`(names(morpho_data))

for (i in names(morpho_data)) {
  png(paste0("../plots/", i, ".png"), 
      width = 16, height = 8, res = 300, units = "in")
  print(plot_morpho(x = morpho_data[[i]], unit = units[[i]]))
  dev.off()
}
