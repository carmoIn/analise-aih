# Instalar pacotes necessários
install.packages("here")

# Carregar pacotes
library(read.dbc)
library(here)
library(dplyr)
library(ggplot2)
library(stats)
library(gapminder)
library(tidyr)
library(gdata)

# Carregar e preparar os dados
municipios <- read.csv("data/municipios.csv")
municipios <- municipios %>% mutate(MUNICIPIO = as.character(substring(COD, 1, 6)))

procedimentos <- read.csv("data/sigtap/procedimentos.csv", colClasses = 'character')

diretorio <- here("data/RD/")
files <- list.files(diretorio, pattern = "\\.dbc$", full.names = TRUE)

read_dbc_file <- function(file) {
  read.dbc(file)
}

all_data <- do.call(rbind, lapply(files, read_dbc_file))
df <- data.frame(all_data)

# Resumo da variável RACA_COR
summary(df$RACA_COR)

# Ajustar variáveis de sexo e morte
df <- df %>% mutate(
  SEXO = case_when(
    SEXO == "0" ~ "Ignorado",
    SEXO == "1" ~ "Masculino",
    SEXO == "3" ~ "Feminino"
  ),
  MORTE = case_when(
    MORTE == "0" ~ "Não",
    MORTE == "1" ~ "Sim"
  )
)

# Mesclar com dados de municípios
df <- inner_join(df, municipios, by = c("MUNIC_RES" = "MUNICIPIO"))

df <- inner_join(df, procedimentos, by = c("PROC_REA" = "co_procedimento"))

# Certificar-se de que VAL_TOT é numérico
df$VAL_TOT <- as.numeric(df$VAL_TOT)

# Cálculos de média e mediana
mean_val_tot_geral <- mean(df$VAL_TOT, na.rm = TRUE)
median_val_tot_geral <- median(df$VAL_TOT, na.rm = TRUE)

media_por_sexo <- df %>% group_by(SEXO) %>% summarise(media_val_tot = mean(VAL_TOT, na.rm = TRUE))
 <- df %>% group_by(SEXO) %>% summarise(median_val_tot = median(VAL_TOT, na.rm = TRUE))

# Transformar SEXO em numérico
df <- df %>%
  mutate(SEXO_NUM = case_when(
    SEXO == "Feminino" ~ 0,
    SEXO == "Masculino" ~ 1,
    TRUE ~ NA_real_
  ))

# Remover valores NA para a correlação
df_filtered <- df %>%
  filter(!is.na(VAL_TOT) & !is.na(SEXO_NUM))

# Calcular a correlação de Pearson
correlation <- cor.test(df_filtered$SEXO_NUM, df_filtered$VAL_TOT, method = "pearson")
print(correlation)

# Interpretação dos resultados do teste de correlação de Pearson
if (correlation$p.value < 0.05) {
  cat("A correlação entre o custo e o sexo é estatisticamente significativa (p < 0.05).\n")
} else {
  cat("A correlação entre o custo e o sexo não é estatisticamente significativa (p >= 0.05).\n")
}

# Exibir resultados de média e mediana
print(paste("Média geral do valor total:", mean_val_tot_geral))
print(paste("Mediana geral do valor total:", median_val_tot_geral))
print(media_por_sexo)
print(median_por_sexo)

# Calculando o aumento percentual do custo
custo_feminino_val <- 1687
custo_masculino_val <- 2300
aumento_percentual <- ((custo_masculino_val - custo_feminino_val) / custo_feminino_val) * 100
print(paste("O aumento percentual do custo em comparação ao feminino é de", round(aumento_percentual, 2), "%"))

# Gráfico de barras para média do custo total por sexo
ggplot(media_por_sexo, aes(x = factor(SEXO, levels = c("Feminino", "Masculino", "Ignorado")), y = media_val_tot, fill = SEXO)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(media_val_tot, 2)), vjust = -0.3, color = "black", size = 3.5) +
  scale_fill_manual(values = c("Feminino" = "red", "Masculino" = "blue", "Ignorado" = "lightgrey")) +
  labs(x = "Sexo", y = "Média do Custo Total") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  )

# Gráfico de barras para mediana do valor total por sexo
ggplot(median_por_sexo, aes(x = factor(SEXO, levels = c("Feminino", "Masculino", "Ignorado")), y = median_val_tot, fill = SEXO)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(median_val_tot, 2)), vjust = -0.3, color = "black", size = 3.5) +
  scale_fill_manual(values = c("Feminino" = "red", "Masculino" = "blue", "Ignorado" = "grey")) +
  labs(title = "Mediana do Valor Total por Sexo", x = "Sexo", y = "Mediana do Valor Total") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  )

# Cálculo da taxa de mortalidade
mortalidade_por_sexo <- df %>%
  group_by(SEXO) %>%
  summarise(
    total_pacientes = n(),
    total_mortes = sum(MORTE == "Sim", na.rm = TRUE),
    taxa_mortalidade = (total_mortes / total_pacientes) * 100
  )

# Gráfico de barras para a taxa de mortalidade por sexo
ggplot(mortalidade_por_sexo, aes(x = as.factor(SEXO), y = taxa_mortalidade, fill = SEXO)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0(round(taxa_mortalidade, 2), "% de ", total_pacientes)), vjust = -0.5, size = 4, color = "black") +
  scale_fill_manual(values = c("Feminino" = "red", "Masculino" = "blue", "Ignorado" = "grey")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) +
  labs(x = "Sexo", y = "Taxa de Mortalidade (%)") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  )

# Contagem de valores nulos em cada coluna
na_count <- sapply(df, function(x) sum(is.na(x)))
na_count_df <- data.frame(variable = names(na_count), na_count = na_count) %>%
  arrange(desc(na_count))

# Selecionando as variáveis com mais valores nulos e criando o gráfico
top_na_count_df <- na_count_df %>% head(10)
ggplot(top_na_count_df, aes(x = reorder(variable, na_count), y = na_count, fill = variable)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = na_count), hjust = -0.1, color = "black", size = 3.5) +
  labs(
    title = "Variáveis com Mais Valores Nulos",
    x = "Variável",
    y = "Número de Valores Nulos",
    fill = "Variável"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Contagem de colunas com valores nulos
num_colunas_com_nulos <- sum(na_count > 0)
print(paste("Número de colunas com valores nulos:", num_colunas_com_nulos))

# Taxa de mortalidade por município
mortalidade_por_municipio <- df %>%
  group_by(MUNIC_RES) %>%
  summarise(
    total_pacientes = n(),
    total_mortes = sum(MORTE == "Sim", na.rm = TRUE),
    taxa_mortalidade = (total_mortes / total_pacientes) * 100
  )

# Selecionando os top 10 municípios com mais pacientes e criando o gráfico
top10_mortalidade <- mortalidade_por_municipio %>%
  arrange(desc(total_pacientes)) %>%
  head(10)

# Gráfico de barras para os top 10 municípios com mais pacientes e suas taxas de mortalidade
ggplot(top10_mortalidade, aes(x = reorder(MUNIC_RES, -total_pacientes), y = taxa_mortalidade, fill = as.factor(total_pacientes))) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = total_pacientes), vjust = -0.5, color = "white", size = 3.5) +
  labs(
    x = "Código dos Municípios",
    y = "Taxa de Mortalidade (%)",
    fill = "Número de Pacientes"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 15),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 5)) +
  scale_fill_brewer(palette = "Set3")

# Análise de mortalidade por deslocamento
# Criar variável indicadora para pacientes deslocados
df <- df %>%
  mutate(DESLOCADO = ifelse(MUNIC_RES != MUNIC_MOV, "Sim", "Não"))

# Verificar se as colunas estão corretamente formatadas
df <- df %>%
  mutate(MORTE = as.factor(MORTE),
         DESLOCADO = as.factor(DESLOCADO))

# Contar o número de mortos e não mortos para pacientes deslocados e não deslocados
mortalidade_contagem <- df %>%
  group_by(DESLOCADO, MORTE) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = MORTE, values_from = count, values_fill = list(count = 0)) %>%
  rename(Nao_Morte = `Não`, Morte = `Sim`)

# Calcular a porcentagem de mortos e o total de pacientes
mortalidade_contagem <- mortalidade_contagem %>%
  mutate(total = Morte + Nao_Morte,
         perc_morte = (Morte / total) * 100)

print(mortalidade_contagem)

# Gráfico de barras para o número de mortos por deslocamento
ggplot(mortalidade_contagem, aes(x = DESLOCADO, y = Morte, fill = DESLOCADO)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0(Morte, " mortos (", round(perc_morte, 2), "% de ", total, ")")), vjust = -0.5, color = "black", size = 3.5) +
  labs(
    x = "Deslocamento",
    y = "Número de Mortos",
    fill = "Deslocamento"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set3")

obitos_pacientes <- df %>% filter(MORTE == 'Sim')
top_10_procedimentos <- obitos_pacientes %>% group_by(no_procedimento) %>%  summarise(count = n()) %>% arrange(desc(count))

top_10_procedimentos <- top_10_procedimentos %>% head(10)


ggplot(top_10_procedimentos, aes(x = reorder(trimws(no_procedimento), count), y = count, fill = no_procedimento)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = count), hjust = -0.1, color = "black", size = 3.5) +
  labs(
    title = "10 Procedimentos com Maior Número de Óbitos",
    x = "Procedimentos Realizados",
    y = "Número de Valores Nulos",
    fill = "Variável"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
