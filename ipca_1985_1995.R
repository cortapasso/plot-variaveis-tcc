library(readxl)
library(tidyverse)
library(jsonlite)
library(svglite)

dados <- fromJSON("http://api.bcb.gov.br/dados/serie/bcdata.sgs.433/dados?formato=json&dataInicial=01/01/1985&dataFinal=01/01/1995")
summary(dados)
head(dados)

dados$data <- as.Date(dados$data, format = "%d/%m/%Y")
dados$valor <- as.numeric(dados$valor)
plot.ts(dados)
taxa <- ts(dados, start = c(1985, 1), end = c(1995, 1), frequency = 12)
taxa
plot(taxa)

dados %>%
  ggplot(aes(x = data, y = valor)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "year", date_labels = "%m/%Y") +
  theme(text = element_text(family = "serif")) +
  annotate(geom="text", x=as.Date("1989-01-01"), y=42, label="Plano Verão", family="serif") +
  annotate(geom="point", x=as.Date("1989-01-01"), y=37, size=5, shape=1, fill="transparent") +
  annotate(geom="text", x=as.Date("1991-03-01"), y=82, label="Plano Collor I", family="serif") +
  annotate(geom="point", x=as.Date("1990-03-01"), y=82, size=5, shape=1, fill="transparent") +
  #theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  labs(
    #title = "Taxa de inflação (IPCA) mensal no Brasil 1985-1995",
    x = "Mês/Ano",
    y = "Taxa (%)"
  )

ggsave("taxa_nova.svg")
