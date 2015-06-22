# Install (if needed) and attach packages -------------------------------------#

packages <- c("dplyr", "lazyeval", "ggplot2")
pkgs_to_install <- packages[which(!packages %in% installed.packages())]

if (length(pkgs_to_install)) install.packages(pkgs_to_install)

library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(lazyeval)

# Function to make plots ------------------------------------------------------#
plot_vars <- function(var) {
  
  dados_climaticos_subset <-
    dados_climaticos %>%
    select_("local", "ano", "mes", var) %>%
    group_by(local, mes) %>% 
    summarise_(media = interp(~mean(var, na.rm = TRUE), var = as.name(var)),
               sd = interp(~sd(var, na.rm = TRUE), var = as.name(var))) %>%
    group_by(local) %>% 
    arrange(dados_climaticos$mes %>% unique %>% order %>% rep(3))
  
  if (var == "precipitacao") 
    y_axis_label <- "Precipitação (mm)"
  if (var == "radiacao") 
    y_axis_label <- bquote('Radiação (' ~ KJ/m^2 ~ ')')
  if (var == "temp_media") 
    y_axis_label <- expression(paste("Temperatura Média (", degree ~ C, ")"))
  if (var == "temp_max") 
    y_axis_label <- expression(paste("Temperatura Máxima (", degree ~ C, ")"))
  if (var == "temp_min") 
    y_axis_label <- expression(paste("Temperatura Mínima (", degree ~ C, ")"))
  if (var == "umidade") 
    y_axis_label <- "Umidade (%)"
  
  ggplot(data = dados_climaticos_subset, 
         aes(x = factor(mes, levels = unique(mes)), y = media, 
             group = local, color = local)) +
    geom_errorbar(aes(ymin = media - sd, ymax = media + sd), 
                  stat = "identity", width = 0.2) +
    geom_line(size = 1.1) +
    geom_point(size = 5) +
    scale_color_manual(name = "Local",
                       values = c("SALINOPOLIS_PA" = "tomato2", 
                                  "FLORIANOPOLIS_SC" = "deepskyblue4", 
                                  "SANTA_MARTA_SC" = "mediumturquoise"),
                       labels = c("SALINOPOLIS_PA" = "Salinópolis, PA",
                                  "FLORIANOPOLIS_SC" = "Florianópolis, SC",
                                  "SANTA_MARTA_SC" = "Santa Marta, SC")) +
    ylab(y_axis_label) +
    xlab("mês") +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18))
}

# Read data -------------------------------------------------------------------#

dados_climaticos <- read.csv("dados_climaticos.csv")

vars <- c("precipitacao", "radiacao", "temp_media",
          "temp_max", "temp_min",  "umidade")

names(dados_climaticos) <- c("local", "ano", "mes", vars)

# Make plot and save to pdf ---------------------------------------------------#
plot_list <- vector("list", length(vars))
names(plot_list) <- vars

for(variable in vars)
  plot_list[[variable]] <- plot_vars(variable)

pdf("graficos_dados_climaticos.pdf", width = 12)
plot_list
dev.off()

# Make plot and save to png ---------------------------------------------------#
for (variavel in vars) {
  png(paste0(variavel, ".png"), width = 12, height = 6, res = 300, units = "in")
  print(plot_vars(variavel))
  dev.off()
}