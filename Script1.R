#libary
library(ggplot2)
library(tidyverse)
library(dplyr)

#Salvar Figuras
#Diretorio de Saída - Mudar caso queira salvar em outro lugar
output_folder <- "C:\\Users\\Rian Lucas\\Documents\\R\\project\\output\\figures"

#Dataframe
area <- runif(100, min = 1, max = 10000)
prox <- c(rep(1,25),rep(2,25),rep(3,25),rep(4,25))
riq <- log10(area)*7 + prox*5 + rnorm(100)
par <- (1:100)
prox <- factor(prox)

#Fontes
windowsFonts(fonte1 = windowsFont("TT Courier New"))

#Carregar a figura para vizualização e colocando o log10
d <- tibble (par,area, prox, riq)
d$log_area <- log10(d$area)
colnames <- colnames(d)
print(colnames)


#Criando os Grafico


#Um histograma da riqueza com as parcelas
gg <- ggplot(d, aes(x = riq)) +
  geom_histogram(aes(y = ..density..), 
                 binwidth = 2, color = "black", fill = "#777AEC", alpha = 0.8) +
  geom_density(aes(x = riq, y = ..density..), fill = "#F8F8FF", alpha = 0.4) +
  labs(title = "Histograma de Riqueza", x = "Riqueza", y = "Densidade") +
  theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "gray"),
           legend.position = "bottom",
           plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 2, family = "sans"),
           plot.margin = unit(c(1,1,1,1), "cm"),
                axis.title = element_text(size = 12, face = "bold.italic"),
                axis.title.y = element_text(angle = 90, vjust = 6),
                axis.title.x = element_text(margin = margin(t = 8)),
          )
gg
ggsave(file.path(output_folder, "Hist_Riq.png"), width = 9, height = 6)


#Um box plot da riqueza vs proximidade
ggplot(d, aes(prox, riq)) +
    geom_boxplot(aes(fill = prox), color = "#000000", size = 0.3) +
  labs(title = "Box Plot de Riqueza x Proximidade", x = "Proximidade", y = "Riqueza") +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(color = "gray"),
    legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, vjust = 2, family = "sans"),
        plot.margin = unit(c(1,1,1,1), "cm"),
          axis.title = element_text(size = 12, face = "bold.italic"),
          axis.title.y = element_text(angle = 90, vjust = 6),
          axis.title.x = element_text(margin = margin(t = 9))
  ) +
      scale_fill_discrete(name = "Proximidade")

ggsave(file.path(output_folder, "BoxPlot_Riq_Prox.png"), width = 9, height = 6)


#Um violinplot da riqueza vs proximidade
ggplot(d, aes(prox, riq)) +
  geom_violin(aes(fill = prox), color = "#330f02", size = 0.7, alpha = 0.15) +
    geom_boxplot(width=0.1, color = "#330f02", size = 0.6) +
    geom_jitter(aes(color = prox), shape = 16, size = 2, alpha = 0.6) +
      scale_fill_discrete(name = "Proximidade") +
      scale_color_discrete(name = "Proximidade") +
  labs(title = "Violin Plot de Riqueza x Proximidade", x = "Proximidade", y = "Riqueza") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, vjust = 2),
        axis.title = element_text(size = 12, face = "bold",vjust = 5),
        axis.title.y = element_text(angle = 90, vjust = 4.5),
        axis.text = element_text(size = 14),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
            panel.grid.major = element_line(color = "gray", linetype = "dashed"),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(color = "black", fill = NA, size = 1.2),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

ggsave(file.path(output_folder, "Violin_Riq_Prox.png"), width = 9, height = 6)


#Um gráfico de barra com a média da riqueza (±ep) vs proximidade
gg1 <- ggplot(d, aes(prox,riq)) +
    geom_bar(stat="summary", fun = "mean",
            width=0.6, fill = "#819EFF") +
    geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.3, color = "black") +
      coord_cartesian(ylim = c(1, 50), xlim = c(1, 4)) +
  labs(title = "Box Plot de Riqueza x Proximidade", x = "Proximidade", y = "Riqueza")
gg1

ggsave(file.path(output_folder, "Barra_Media_Riq_Prox.png"), width = 9, height = 6)


#Um scattergram de riqueza vs area, com um reta de tendência
ggplot(d, aes(area,riq)) +
  geom_point(aes())+
  geom_smooth(method = "lm") +
  labs(title = "Box Plot de Riqueza x Proximidade", x = "Area", y = "Riqueza")

ggsave(file.path(output_folder, "Scart_riq_area_tend.png"), width = 9, height = 6)


#Um scattergram de riqueza vs log10(área), mas com um reta para cada proximidade
ggplot(d, aes(x = log_area, y = riq)) +
  geom_point() +
  	geom_smooth(method = "lm", se = FALSE) +
  	facet_wrap(~prox, nrow = 1) +
	scale_color_manual(values = c("blue")) +
  labs(title = "Gráfico de Dispersão de Riqueza x Log10 da Área para cada Proximidade",
       x = "Log10 da Área", y = "Riqueza") +

ggsave(file.path(output_folder, "Scart_riq_log_area_reta.png"), width = 9, height = 6)

