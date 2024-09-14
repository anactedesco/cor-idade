#Vamos chamar os pacotes
#Certifique-se de ter todos instalados na sua máquina
library(censobr)
library(arrow)
library(dplyr)
library(ggplot2)
library(readxl)
library(purrr) 
library(sidrar)
library(showtext) 
library(tidyverse)
library(janitor) 
library(grid)
library(patchwork)

#Vamos agilizar já importando a fonte que queremos para o gráfico
font_add_google("Montserrat", "montserrat")
showtext_auto()

options(scipen = 99999)


#Definindo o caminho para os arquivos
caminho <- "C:/Users/caminho/para/o/seu/diretório/"

######
#Para 2022 - atente-se que já realizei o download dos dados via sidrar e salvei o que preciso
cor_sc <- read_excel(paste0(caminho, "cor_sc.xlsx"))
colnames(cor_sc)

#Garantindo que nossos dados não numéricos serão NA
cor_sc$Amarela <- as.numeric(gsub("-", NA, cor_sc$Amarela))
cor_sc$Indígena <- as.numeric(gsub("-", NA, cor_sc$Indígena))
cor_sc$`Pessoas indígenas declaradas pelos quesitos de cor ou raça ou se considera indígena` <- as.numeric(gsub("-", NA, cor_sc$`Pessoas indígenas declaradas pelos quesitos de cor ou raça ou se considera indígena`))

#Agora vamos filtrar os municipios desejados
abelardo <- cor_sc %>%
  filter(Municípios == "Abelardo Luz (SC)")

fronteira <- cor_sc %>%
  filter(Municípios %in% c("São Domingos (SC)", "Ipuaçu (SC)", "Bom Jesus (SC)", "Ouro Verde (SC)", "Faxinal dos Guedes (SC)", "Vargeão (SC)", "Passos Maia (SC)"))

amai <- cor_sc %>%
  filter(Municípios %in% c("Abelardo Luz (SC)", "Bom Jesus (SC)", "Entre Rios (SC)", "Faxinal dos Guedes (SC)", "Ipuaçu (SC)", "Lajeado Grande (SC)", "Marema (SC)", "Ouro Verde (SC)", "Passos Maia (SC)", "Ponte Serrada (SC)", "São Domingos (SC)", "Vargeão (SC)", "Xanxerê (SC)", "Xaxim (SC)"))

oeste_sc <- cor_sc %>%
  filter(Municípios %in% c("Abelardo Luz (SC)", "Água Doce (SC)", "Águas de Chapecó (SC)", "Águas Frias (SC)", "Alto Bela Vista (SC)", "Anchieta (SC)", "Arabutã (SC)", "Arroio Trinta (SC)", "Arvoredo (SC)", "Bandeirante (SC)", "Barra Bonita (SC)", "Belmonte (SC)", "Bom Jesus (SC)", "Bom Jesus do Oeste (SC)", "Caçador (SC)", "Caibi (SC)", "Calmon (SC)", "Campo Erê (SC)", "Capinzal (SC)", "Catanduvas (SC)", "Caxambu do Sul (SC)", "Chapecó (SC)", "Concórdia (SC)", "Cordilheira Alta (SC)", "Coronel Freitas (SC)", "Coronel Martins (SC)", "Cunha Porã (SC)", "Cunhataí (SC)", "Descanso (SC)", "Dionísio Cerqueira (SC)", "Entre Rios (SC)", "Erval Velho (SC)", "Faxinal dos Guedes (SC)", "Flor do Sertão (SC)", "Formosa do Sul (SC)", "Fraiburgo (SC)", "Galvão (SC)", "Guaraciaba (SC)", "Guarujá do Sul (SC)", "Guatambu (SC)", "Herval d'Oeste (SC)", "Ibiam (SC)", "Ibicaré (SC)", "Iomerê (SC)", "Ipira (SC)", "Iporã do Oeste (SC)", "Ipuaçu (SC)", "Ipumirim (SC)", "Iraceminha (SC)", "Irani (SC)", "Irati (SC)", "Itá (SC)", "Itapiranga (SC)", "Jaborá (SC)", "Jardinópolis (SC)", "Joaçaba (SC)", "Jupiá (SC)", "Lacerdópolis (SC)", "Lajeado Grande (SC)", "Lebon Régis (SC)", "Lindóia do Sul (SC)", "Luzerna (SC)", "Macieira (SC)", "Maravilha (SC)", "Marema (SC)", "Matos Costa (SC)", "Modelo (SC)", "Mondaí (SC)", "Nova Erechim (SC)", "Nova Itaberaba (SC)", "Novo Horizonte (SC)", "Ouro (SC)", "Ouro Verde (SC)", "Paial (SC)", "Palma Sola (SC)", "Palmitos (SC)", "Paraíso (SC)", "Passos Maia (SC)", "Peritiba (SC)", "Pinhalzinho (SC)", "Pinheiro Preto (SC)", "Piratuba (SC)", "Planalto Alegre (SC)", "Ponte Serrada (SC)", "Presidente Castello Branco (SC)", "Princesa (SC)", "Quilombo (SC)", "Rio das Antas (SC)", "Riqueza (SC)", "Romelândia (SC)", "Saltinho (SC)", "Salto Veloso (SC)", "Santa Helena (SC)", "Santa Terezinha do Progresso (SC)", "Santiago do Sul (SC)", "São Bernardino (SC)", "São Carlos (SC)", "São Domingos (SC)", "São João do Oeste (SC)", "São José do Cedro (SC)", "São Lourenço do Oeste (SC)", "São Miguel da Boa Vista (SC)", "São Miguel do Oeste (SC)", "Saudades (SC)", "Seara (SC)", "Serra Alta (SC)", "Sul Brasil (SC)", "Tangará (SC)", "Tigrinhos (SC)", "Treze Tílias (SC)", "Tunápolis (SC)", "União do Oeste (SC)", "Vargeão (SC)", "Vargem Bonita (SC)", "Videira (SC)", "Xanxerê (SC)", "Xavantina (SC)", "Xaxim (SC)"))

#Funçãozinha para calcular as médias por grupo
calc_media_rounded <- function(df) {
  summarise(df,
            Total = round(mean(Total, na.rm = TRUE)),
            Branca = round(mean(Branca, na.rm = TRUE)),
            Preta = round(mean(Preta, na.rm = TRUE)),
            Amarela = round(mean(Amarela, na.rm = TRUE)),
            Parda = round(mean(Parda, na.rm = TRUE)),
            Indígena = round(mean(Indígena, na.rm = TRUE)))}

#Calculando nossas estatísticas para cada df
abelardo_summary <- calc_media_rounded(abelardo)
fronteira_summary <- calc_media_rounded(fronteira)
amai_summary <- calc_media_rounded(amai)
oeste_sc_summary <- calc_media_rounded(oeste_sc)

#Criando uma tabela consolidada
tabela_resumida_22 <- bind_rows(
  cbind(Grupo = "Abelardo Luz", abelardo_summary),
  cbind(Grupo = "Média Fronteira2", fronteira_summary),
  cbind(Grupo = "Média AMAI", amai_summary),
  cbind(Grupo = "Média Oeste SC", oeste_sc_summary))
print(tabela_resumida_22)

######
#Para 2010 - mesma coisa
cor_sc_10 <- read_excel(paste0(caminho, "cor_sc_10.xlsx"))
colnames(cor_sc_10)

#Garantindo que nossos dados não numéricos serão NA
cor_sc_10$Total <- as.numeric(gsub("-", NA, cor_sc_10$Total))
cor_sc_10$Branca <- as.numeric(gsub("-", NA, cor_sc_10$Branca))
cor_sc_10$`Preta` <- as.numeric(gsub("-", NA, cor_sc_10$`Preta`))
cor_sc_10$`Parda` <- as.numeric(gsub("-", NA, cor_sc_10$`Parda`))
cor_sc_10$Amarela <- as.numeric(gsub("-", NA, cor_sc_10$Amarela))
cor_sc_10$Indígena <- as.numeric(gsub("-", NA, cor_sc_10$Indígena))
cor_sc_10$`Pessoas indígenas declaradas pelos quesitos de cor ou raça ou se considera indígena` <- as.numeric(gsub("-", NA, cor_sc_10$`Pessoas indígenas declaradas pelos quesitos de cor ou raça ou se considera indígena`))

#Novamente filtrando os municipios desejados
abelardo <- cor_sc_10 %>%
  filter(Municípios == "Abelardo Luz (SC)")

fronteira <- cor_sc_10 %>%
  filter(Municípios %in% c("São Domingos (SC)", "Ipuaçu (SC)", "Bom Jesus (SC)", "Ouro Verde (SC)", "Faxinal dos Guedes (SC)", "Vargeão (SC)", "Passos Maia (SC)"))

amai <- cor_sc_10 %>%
  filter(Municípios %in% c("Abelardo Luz (SC)", "Bom Jesus (SC)", "Entre Rios (SC)", "Faxinal dos Guedes (SC)", "Ipuaçu (SC)", "Lajeado Grande (SC)", "Marema (SC)", "Ouro Verde (SC)", "Passos Maia (SC)", "Ponte Serrada (SC)", "São Domingos (SC)", "Vargeão (SC)", "Xanxerê (SC)", "Xaxim (SC)"))

oeste_sc <- cor_sc_10 %>%
  filter(Municípios %in% c("Abelardo Luz (SC)", "Água Doce (SC)", "Águas de Chapecó (SC)", "Águas Frias (SC)", "Alto Bela Vista (SC)", "Anchieta (SC)", "Arabutã (SC)", "Arroio Trinta (SC)", "Arvoredo (SC)", "Bandeirante (SC)", "Barra Bonita (SC)", "Belmonte (SC)", "Bom Jesus (SC)", "Bom Jesus do Oeste (SC)", "Caçador (SC)", "Caibi (SC)", "Calmon (SC)", "Campo Erê (SC)", "Capinzal (SC)", "Catanduvas (SC)", "Caxambu do Sul (SC)", "Chapecó (SC)", "Concórdia (SC)", "Cordilheira Alta (SC)", "Coronel Freitas (SC)", "Coronel Martins (SC)", "Cunha Porã (SC)", "Cunhataí (SC)", "Descanso (SC)", "Dionísio Cerqueira (SC)", "Entre Rios (SC)", "Erval Velho (SC)", "Faxinal dos Guedes (SC)", "Flor do Sertão (SC)", "Formosa do Sul (SC)", "Fraiburgo (SC)", "Galvão (SC)", "Guaraciaba (SC)", "Guarujá do Sul (SC)", "Guatambu (SC)", "Herval d'Oeste (SC)", "Ibiam (SC)", "Ibicaré (SC)", "Iomerê (SC)", "Ipira (SC)", "Iporã do Oeste (SC)", "Ipuaçu (SC)", "Ipumirim (SC)", "Iraceminha (SC)", "Irani (SC)", "Irati (SC)", "Itá (SC)", "Itapiranga (SC)", "Jaborá (SC)", "Jardinópolis (SC)", "Joaçaba (SC)", "Jupiá (SC)", "Lacerdópolis (SC)", "Lajeado Grande (SC)", "Lebon Régis (SC)", "Lindóia do Sul (SC)", "Luzerna (SC)", "Macieira (SC)", "Maravilha (SC)", "Marema (SC)", "Matos Costa (SC)", "Modelo (SC)", "Mondaí (SC)", "Nova Erechim (SC)", "Nova Itaberaba (SC)", "Novo Horizonte (SC)", "Ouro (SC)", "Ouro Verde (SC)", "Paial (SC)", "Palma Sola (SC)", "Palmitos (SC)", "Paraíso (SC)", "Passos Maia (SC)", "Peritiba (SC)", "Pinhalzinho (SC)", "Pinheiro Preto (SC)", "Piratuba (SC)", "Planalto Alegre (SC)", "Ponte Serrada (SC)", "Presidente Castello Branco (SC)", "Princesa (SC)", "Quilombo (SC)", "Rio das Antas (SC)", "Riqueza (SC)", "Romelândia (SC)", "Saltinho (SC)", "Salto Veloso (SC)", "Santa Helena (SC)", "Santa Terezinha do Progresso (SC)", "Santiago do Sul (SC)", "São Bernardino (SC)", "São Carlos (SC)", "São Domingos (SC)", "São João do Oeste (SC)", "São José do Cedro (SC)", "São Lourenço do Oeste (SC)", "São Miguel da Boa Vista (SC)", "São Miguel do Oeste (SC)", "Saudades (SC)", "Seara (SC)", "Serra Alta (SC)", "Sul Brasil (SC)", "Tangará (SC)", "Tigrinhos (SC)", "Treze Tílias (SC)", "Tunápolis (SC)", "União do Oeste (SC)", "Vargeão (SC)", "Vargem Bonita (SC)", "Videira (SC)", "Xanxerê (SC)", "Xavantina (SC)", "Xaxim (SC)"))


#Calculando nossas estatísticas para cada df
abelardo_summary_10 <- calc_media_rounded(abelardo)
fronteira_summary_10 <- calc_media_rounded(fronteira)
amai_summary_10 <- calc_media_rounded(amai)
oeste_sc_summary_10 <- calc_media_rounded(oeste_sc)

#Criando uma tabela consolidada
tabela_resumida_10 <- bind_rows(
  cbind(Grupo = "Abelardo Luz", abelardo_summary_10),
  cbind(Grupo = "Média Fronteira", fronteira_summary_10),
  cbind(Grupo = "Média AMAI", amai_summary_10),
  cbind(Grupo = "Média Oeste SC", oeste_sc_summary_10))
print(tabela_resumida_10)


#Criando os data frames
tabela_resumida_10 <- data.frame(
  Grupo = c("Abelardo Luz", "Fronteira", "AMAI", "Oeste de SC"),
  Total = c(27, 29, 30, 32),
  Branca = c(29, 31, 32, 34),
  Preta = c(28, 32, 30, 30),
  Amarela = c(30, 36, 33, 32),
  Parda = c(25, 25, 25, 26),
  Indígena = c(19, 31, 28, 34))

tabela_resumida_22 <- data.frame(
  Grupo = c("Abelardo Luz", "Fronteira", "AMAI", "Oeste de SC"),
  Total = c(33, 35, 35, 38),
  Branca = c(34, 38, 38, 40),
  Preta = c(34, 42, 39, 38),
  Amarela = c(35, 40, 40, 38),
  Parda = c(31, 32, 31, 31),
  Indígena = c(25, 29, 27, 34))

#Adicionando a coluna "Ano" e combinando os dois dataframes
tabela_resumida_10$Ano <- 2010
tabela_resumida_22$Ano <- 2022
tabela_combinada <- bind_rows(tabela_resumida_10, tabela_resumida_22)

#Transformando os dados para o formato longo
tabela_long <- tabela_combinada %>%
  pivot_longer(cols = Total:Indígena, names_to = "Categoria", values_to = "Média_Idade")

#Filtrando os dados para incluir apenas a categoria "Total"
tabela_total <- tabela_long %>%
  filter(Categoria == "Total")
print(tabela_total)

#Criando o gráfico para a categoria "Total"
ggplot(tabela_total, aes(x = Ano, y = Média_Idade, color = Grupo, group = Grupo)) +
  geom_line(size = 1, linetype = 1, lineend = "round") +  
  geom_point(size = 3, shape = 21) +
  theme_minimal() +
  labs(title = "Gráfico 1 - Evolução da mediana de idade ao longo do tempo (2010 vs 2022)",
       x = "",
       y = "Média de Idade",
       color = "",
       caption = "Fonte: Censo Demográfico/IBGE (2022). Elaborado por: @abelardoluzemdados") +
  scale_x_continuous(breaks = c(2010, 2022)) + 
  scale_y_continuous(breaks = seq(25, 40, 5), limits = c(25, 40)) + 
  scale_color_manual(values = c(
    "Abelardo Luz" = "#65b7dd",
    "Fronteira" = "#027c31",
    "AMAI" = "#fded02",
    "Oeste de SC" = "#9b1627")) +
  theme(legend.position = "top",  
        legend.direction = "horizontal",  
        legend.text = element_text(size = 10),
        legend.spacing.x = unit(0.5, 'cm'),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),  
        text = element_text(family = "Montserrat"),  
        strip.text.x = element_text(face = "bold"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.ticks = element_blank(),  
        axis.line = element_line(size = 0.5),  
        panel.spacing = unit(1, "lines"),
        plot.caption = element_text(hjust = 0.5, size = 8))

#Para análises por raça/cor
cores_personalizadas <- c(
  "Amarela" = "#d5bc92",
  "Indígena" = "#027c31",
  "Preta" = "#65b7dd",
  "Parda" = "#9b1627",
  "Branca" = "#fded02",
  "Total" = "#010c12")

print(tabela_combinada)

#Criando o gráfico para todas as raças/cores
ggplot(tabela_long, aes(x = Ano, y = Média_Idade, color = Categoria, group = Categoria)) +
  geom_line(size = 1, linetype = 1, lineend = "round") +  
  geom_point(size = 3, shape = 21) +
  facet_wrap(~ Grupo) +
  theme_minimal() +
  labs(title = "Gráfico 2 - Comparando medianas de idade por raça/cor (2010 vs 2022)",
       x = "",
       y = "Média de Idade",
       color = "",
       caption = "Fonte: Censo Demográfico/IBGE (2022). Elaborado por: @abelardoluzemdados") +
  scale_x_continuous(breaks = c(2010, 2022)) + 
  scale_color_manual(values = cores_personalizadas) +
  theme(legend.position = "top",  
        legend.direction = "horizontal",  
        legend.text = element_text(size = 10),
        legend.spacing.x = unit(0.5, 'cm'),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),      
        text = element_text(family = "Montserrat"),  
        strip.text.x = element_text(face = "bold"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.ticks = element_blank(),  
        axis.line = element_line(size = 0.5),  
        panel.spacing = unit(1, "lines"),
        plot.caption = element_text(hjust = 0.5, size = 8)) 