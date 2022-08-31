library(readxl)
library(tidyverse)
library(jsonlite)
library(ggrepel)

dados2 <- fromJSON("http://api.bcb.gov.br/dados/serie/bcdata.sgs.433/dados?formato=json&dataInicial=01/01/1994&dataFinal=01/01/1995")
summary(dados2)
head(dados2)

dados2$data <- as.Date(dados2$data, format = "%d/%m/%Y")
dados2$valor <- as.numeric(dados2$valor)
plot.ts(dados2)
taxa <- ts(dados2, start = c(1994, 1), end = c(1995, 1), frequency = 12)
taxa
plot(taxa)

dados2 %>%
  ggplot(aes(x = data, y = valor)) +
  geom_line(size = 1) +
  geom_text(aes(label=valor), hjust = 0.8, vjust = 2, size=3.5, parse = TRUE, 
            check_overlap = TRUE, nudge_x = 11, nudge_y = 5, family = "serif") +
  #geom_text_repel(aes(label = valor), size = 4,
  #                family = "serif") +
  #theme(axis.text.x = element_text(size=12),
  #      axis.text.y = element_text(size=12))+
  scale_x_date(date_breaks = "month", date_labels = "%m/%y") +
  annotate(geom="text", x=as.Date("1994-06-01"), y=7, label="Plano Real", family="serif") +
  annotate(geom="point", x=as.Date("1994-07-01"), y=7, size=5, shape=1, fill="transparent") +
  theme(text = element_text(family = "serif")) +
  #theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  labs(
    #title = "Taxa de inflação (IPCA) mensal no Brasil 1985-1995",
    x = "Mês",
    y = "Taxa (%)"
  )
ggsave("real2.svg")

