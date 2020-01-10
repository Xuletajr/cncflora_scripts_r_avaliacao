##############################################################################################################
   ###                                Baixar e organizar dados do REFLORA                               ###        
##############################################################################################################

# Ler pacotes
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(biogeo)
library(readr)
library(flora)

# Ler a planilha com as espécies de plantas do recorte
treespp <- readr::read_csv("./results/names_flora.csv")

# Colocar as famílias e binômio das espécies em vetores.  
familias <- treespp$final_family
especies <- treespp$nome_especie

# Ler as planilhas com dados reflora - Os dados foram baixados com script do Vicente no NODE (disponível no Bitbucket dele)
# Próximo passa seria automatizar o script para baixar diretamente as ocorrências do REFLORA
reflora_1 <- read_excel("./data/RelatorioConsultaTestemunho_grupo1.xlsx", sheet = 1)
reflora_2 <- read_excel("./data/RelatorioConsultaTestemunho_grupo2.xlsx", sheet = 1)
reflora_3 <- read_excel("./data/RelatorioConsultaTestemunho_grupo3.xlsx", sheet = 1)
reflora_4 <- read_excel("./data/RelatorioConsultaTestemunho_grupo4.xlsx", sheet = 1)
reflora_5 <- read_excel("./data/RelatorioConsultaTestemunho_grupo5.xlsx", sheet = 1)
reflora_6 <- read_excel("./data/RelatorioConsultaTestemunho_grupo6.xlsx", sheet = 1)
reflora_7 <- read_excel("./data/RelatorioConsultaTestemunho_grupo7.xlsx", sheet = 1)
reflora_8 <- read_excel("./data/RelatorioConsultaTestemunho_grupo8.xlsx", sheet = 1)
reflora_9 <- read_excel("./data/RelatorioConsultaTestemunho_grupo9.xlsx", sheet = 1)
