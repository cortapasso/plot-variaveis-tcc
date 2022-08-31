rm(list = ls())

library(readxl)
library(tidyverse)
library(jsonlite)
library(ggrepel)
library(tsibble)
library(feasts)
library(ggplot2)
library(lubridate)
library(urca)

dados <- read_xlsx("teste1.xlsx") #puxa os dados
ano <- c(2015:2021) #cria escala de tempo
comeco <- mdy("01-01-2015") #define começo
fim <- mdy("06-01-2021") #define o fim
por <- "1 month" #define o passo
x <- seq(comeco, fim, by = por) #cria a sequencia
ano <- x
length(ano)
ano

df1 <- data.frame(ano, dados) #usa os dados e o ano
df1

plot(df1$ano, df1$IGPDI)

#tambem fiz pra variaveis em log
df1_log <- data.frame(ano, log(df1$IGPDI), log(df1$IPCA), log(df1$PIB), log(df1$CAMBIONOMINAL), log(df1$SELIC))
df1_log <- df1_log %>% 
  rename(IGPDI = log.df1.IGPDI.,
         IPCA = log.df1.IPCA.,
         PIB = log.df1.PIB.,
         CAMBIONOMINAL = log.df1.CAMBIONOMINAL.,
         SELIC = log.df1.SELIC.)

df1_log <- df1_log %>% 
  pivot_longer(cols = 2:6,
               names_to = "type",
               values_to = "taxa")
head(df1_log)

#IPCA

df1_log %>%
  filter(type == "IPCA") %>% 
  ggplot(aes(x = ano, y = taxa)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "year", date_labels = "%m/%Y") +
  theme(text = element_text(family = "serif")) +
  #theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  labs(
    #title = "Taxa de inflação (IPCA) mensal no Brasil 1985-1995",
    x = "Mês/Ano",
    y = "Taxa (ago. 1994 = 100)"
  )

ggsave("IPCA_APENDICE_LOG.svg")

df1 %>%
  ggplot(aes(x = ano, y = IPCA )) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "year", date_labels = "%m/%Y") +
  theme(text = element_text(family = "serif")) +
  #theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  labs(
    #title = "Taxa de inflação (IPCA) mensal no Brasil 1985-1995",
    x = "Mês/Ano",
    y = "Taxa (ago. 1994 = 100)"
  )

ggsave("IPCA_APENDICE.svg")

#IGPDI

df1_log %>%
  filter(type == "IGPDI") %>% 
  ggplot(aes(x = ano, y = taxa)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "year", date_labels = "%m/%Y") +
  theme(text = element_text(family = "serif")) +
  #theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  labs(
    #title = "Taxa de inflação (IPCA) mensal no Brasil 1985-1995",
    x = "Mês/Ano",
    y = "Taxa (ago. 1994 = 100)"
  )

ggsave("IGPDI_APENDICE_LOG.svg")

df1 %>%
  ggplot(aes(x = ano, y = IGPDI)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "year", date_labels = "%m/%Y") +
  theme(text = element_text(family = "serif")) +
  #theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  labs(
    #title = "Taxa de inflação (IPCA) mensal no Brasil 1985-1995",
    x = "Mês/Ano",
    y = "Taxa (ago. 1994 = 100)"
  )

ggsave("IGPDI_APENDICE.svg")

#CAMBIO

df1_log %>%
  filter(type == "CAMBIONOMINAL") %>% 
  ggplot(aes(x = ano, y = taxa)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "year", date_labels = "%m/%Y") +
  theme(text = element_text(family = "serif")) +
  #theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  labs(
    #title = "Taxa de inflação (IPCA) mensal no Brasil 1985-1995",
    x = "Mês/Ano",
    y = "Valor (R$)"
  )

ggsave("CAMBIO_APENDICE_LOG.svg")

df1 %>%
  ggplot(aes(x = ano, y = CAMBIONOMINAL)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "year", date_labels = "%m/%Y") +
  theme(text = element_text(family = "serif")) +
  #theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  labs(
    #title = "Taxa de inflação (IPCA) mensal no Brasil 1985-1995",
    x = "Mês/Ano",
    y = "Valor (R$)"
  )

ggsave("CAMBIO_APENDICE.svg")

#PIB

df1_log %>%
  filter(type == "PIB") %>% 
  ggplot(aes(x = ano, y = taxa)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "year", date_labels = "%m/%Y") +
  theme(text = element_text(family = "serif")) +
  #theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  labs(
    #title = "Taxa de inflação (IPCA) mensal no Brasil 1985-1995",
    x = "Mês/Ano",
    y = "Valor em milhões de R$ correntes de 06/2021"
  )

ggsave("PIBREAL_APENDICE_LOG.svg")

df1 %>%
  mutate(PIB = PIB/1000) %>%
  ggplot(aes(x = ano, y = PIB)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "year", date_labels = "%m/%Y") +
  theme(text = element_text(family = "serif")) +
  #theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  labs(
    #title = "Taxa de inflação (IPCA) mensal no Brasil 1985-1995",
    x = "Mês/Ano",
    y = "Valor em milhões de R$ correntes de 06/2021"
  )

ggsave("PIBREAL_APENDICE.svg")

#SELIC

df1_log %>%
  filter(type == "SELIC") %>% 
  ggplot(aes(x = ano, y = taxa)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "year", date_labels = "%m/%Y") +
  theme(text = element_text(family = "serif")) +
  #theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  labs(
    #title = "Taxa de inflação (IPCA) mensal no Brasil 1985-1995",
    x = "Mês/Ano",
    y = "Taxa (%)"
  )

ggsave("SELIC_APENDICE_LOG.svg")

df1 %>%
  ggplot(aes(x = ano, y = SELIC)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "year", date_labels = "%m/%Y") +
  theme(text = element_text(family = "serif")) +
  #theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
  labs(
    #title = "Taxa de inflação (IPCA) mensal no Brasil 1985-1995",
    x = "Mês/Ano",
    y = "Taxa (%)"
  )

ggsave("SELIC_APENDICE.svg")
