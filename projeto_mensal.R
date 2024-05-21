library(read.dbc)
library(dplyr)
library(ggplot2)
library(stats)
library(gapminder)

municipios <- read.csv("data/municipios.csv")
municipios <- municipios %>% mutate(MUNICIPIO = as.character(substring(COD, 1, 6)))

df <- read.dbc("data/RDPR2401.dbc")

summary(df$RACA_COR)

df <- df %>% mutate(SEXO = case_when(SEXO == "0" ~ "Ignorado",
                                        SEXO == "1" ~ "Masculino",
                                        SEXO == "3" ~ "Feminino"))

df <- df %>% mutate(MORTE = case_when(MORTE == "0" ~ "Não",
                                      MORTE == "1" ~ "Sim"))

df <- left_join(df, municipios, by = c("MUNIC_RES" = "MUNICIPIO"))

df$NUM_FILHOS
