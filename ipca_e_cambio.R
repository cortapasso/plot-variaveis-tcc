rm(list = ls())

library(readxl)
library(tidyverse)
library(jsonlite)
library(ggrepel)
library(tsibble)
library(feasts)
library(ggplot2)
library(lubridate)

#Puxa os dados do BACEN
ipca <- fromJSON("http://api.bcb.gov.br/dados/serie/bcdata.sgs.433/dados?formato=json&dataInicial=01/01/2000&dataFinal=01/01/2018")
cambio <- fromJSON("http://api.bcb.gov.br/dados/serie/bcdata.sgs.3697/dados?formato=json&dataInicial=01/01/2000&dataFinal=01/01/2018")

#Converte os valores para numerico
ipca$valor <- as.numeric(ipca$valor)
cambio$valor <- as.numeric(cambio$valor)

#Converte os valores para log
#ipca$valor <- log(ipca$valor)
#cambio$valor <- log(cambio$valor)

#Converte as datas
ipca$data <- as.Date(ipca$data, format = "%d/%m/%Y")
cambio$data <- as.Date(cambio$data, format = "%d/%m/%Y")

ano <- c(2000:2018)
comeco <- mdy("01-01-2000")
fim <- mdy("01-01-2018")
por <- "1 month"

x <- seq(comeco, fim, by = por)
ano <- x
length(ano)
ano


df1 <- data.frame(ano, ipca$valor, cambio$valor)
df1 <- df1 %>% gather(key = 'type', value = 'taxa', -c(ano))


my_labels <- c(
  'ipca.valor' = 'IPCA',
  'cambio.valor' = 'Câmbio'
)

df1 %>% 
  # plot mapping and geoms
  ggplot(aes(ano, taxa)) +
  geom_line(aes(linetype = type, color = type), size = 0.5) +
  #geom_text(aes(x = ano, y = taxa, label = taxa), data = df1 %>% filter(type == 'inflacao'), nudge_y = 1, size = 3) +
  #geom_label(aes(x = ano, y = taxa, label = taxa), data = df1 %>% filter(type == 'inflacao'), nudge_y = 1, size = 2,
  #           label.padding = unit(0.3, "lines"),
  #           label.r = unit(0.15, "lines"),
  #           label.size = 0.5)+
  # scale functions
  scale_color_manual(values = c(
    "ipca.valor" = "black", "cambio.valor" = "black"),
    labels = my_labels) +
  scale_linetype_manual(values = c(
    "ipca.valor" = "solid", "cambio.valor" = "dotted"),
    labels = my_labels) +
  # labeling and theme elements
  labs(
    x = "Anos",
    y = "Valor"
  ) +
  
  theme(
    text = element_text(family = "serif"),
    axis.text = element_text(size = 10),
    legend.title = element_blank(),
    axis.text.x = element_text(vjust = 0),
    axis.title.x = element_text(vjust = -1),
    legend.position = "top"
  )

ggsave("ipca_e_cambio.svg")

