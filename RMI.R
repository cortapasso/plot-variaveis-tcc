library(lubridate)
library(ggplot2)
library(tidyverse)
ano <- c(1999:2019)
comeco <- mdy("01-01-1999")
fim <- mdy("01-01-2019")
por <- "1 year"

x <- seq(comeco, fim, by = por)
ano <- x
inferior <- c(6, 4, 2, 1.5, 1.5, 3, 2, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 3, 3, 2.75)
superior <- c(10, 8, 6, 5.5, 6.5, 8, 7, 6.5, 6.5, 6.5, 6.5, 6.5, 6.5, 6.5, 6.5, 6.5, 6.5, 6.5, 6, 6, 5.75)
meta <- c(8, 6, 4, 3.5, 4, 5.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.25)
inflacao <- c(8.94, 5.97, 7.67, 12.53, 9.3, 7.6, 5.69, 3.14, 4.46, 5.9, 4.31, 5.91, 6.5, 5.84, 5.91, 6.41, 10.67, 6.29, 2.95, 3.75, 4.31)
length(inferior)
class(ano)
length(superior)
length(ano)
length(meta)
length(inflacao)

df1 <- data.frame(ano, inferior, superior, inflacao, meta)
df1 <- df1 %>% gather(key = 'type', value = 'taxa', -c(ano))
df1

my_labels <- c(
  'superior' = 'Limite Superior',
  'inferior' = 'Limite Inferior',
  'meta' = 'Meta de Inflação',
  'inflacao' = 'Inflação Efetiva'
)

df1 %>% 
  # plot mapping and geoms
  ggplot(aes(ano, taxa)) +
  geom_line(aes(linetype = type, color = type), size = 0.5) +
  geom_point(aes(color = type, shape = type), size = 2) +
  #geom_text(aes(x = ano, y = taxa, label = taxa), data = df1 %>% filter(type == 'inflacao'), nudge_y = 1, size = 3) +
  #geom_label(aes(x = ano, y = taxa, label = taxa), data = df1 %>% filter(type == 'inflacao'), nudge_y = 1, size = 2,
  #           label.padding = unit(0.3, "lines"),
  #           label.r = unit(0.15, "lines"),
  #           label.size = 0.5)+
  # scale functions
  scale_x_date(date_labels = "%Y", breaks = df1$ano) +
  scale_color_manual(values = c(
    "meta" = "black", "inflacao" = "black", "superior" = "black", "inferior" = "black"),
    labels = my_labels) +
  scale_linetype_manual(values = c(
    "meta" = "solid", "inflacao" = "dashed", "superior" = "solid", "inferior" = "solid"),
    labels = my_labels) +
  scale_shape_manual(values = c(
    "meta" = NA, "inflacao" = NA, "superior" = 17, "inferior" = 15),
    labels = my_labels) +
  
  # labeling and theme elements
  labs(
    x = "Anos",
    y = "Taxa (%)"
  ) +
  
  theme(
    text = element_text(family = "serif"),
    axis.text = element_text(size = 8),
    legend.title = element_blank(),
    axis.text.x = element_text(vjust = 0),
    axis.title.x = element_text(vjust = -1),
    legend.position = "top"
  )
  
ggsave("RMI.svg")
  


