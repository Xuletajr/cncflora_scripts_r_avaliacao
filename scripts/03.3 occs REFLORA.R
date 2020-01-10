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
reflora_1 <- readxl::read_excel("./data/RelatorioConsultaTestemunho_grupo1.xlsx", sheet = 1)
reflora_2 <- readxl::read_excel("./data/RelatorioConsultaTestemunho_grupo2.xlsx", sheet = 1)
reflora_3 <- readxl::read_excel("./data/RelatorioConsultaTestemunho_grupo3.xlsx", sheet = 1)
reflora_4 <- readxl::read_excel("./data/RelatorioConsultaTestemunho_grupo4.xlsx", sheet = 1)
reflora_5 <- readxl::read_excel("./data/RelatorioConsultaTestemunho_grupo5.xlsx", sheet = 1)
reflora_6 <- readxl::read_excel("./data/RelatorioConsultaTestemunho_grupo6.xlsx", sheet = 1)
reflora_7 <- readxl::read_excel("./data/RelatorioConsultaTestemunho_grupo7.xlsx", sheet = 1)
reflora_8 <- readxl::read_excel("./data/RelatorioConsultaTestemunho_grupo8.xlsx", sheet = 1)
reflora_9 <- readxl::read_excel("./data/RelatorioConsultaTestemunho_grupo9.xlsx", sheet = 1)

# Juntando as planilhas
reflora_all <- dplyr::bind_rows(reflora_1, reflora_2, reflora_3, reflora_4,
                         reflora_5, reflora_6, reflora_7, reflora_8, reflora_9)

# Exportar uma planilha csv com todos os dados do reflora juntos
write.csv(reflora_all, "./results/reflora_all.csv", fileEncoding = "UTF-8")

# Juntando o gênero e epíteto específico em uma coluna
reflora_all <- reflora_all %>% 
   dplyr::mutate(Gênero = str_to_title(Gênero)) %>%
   dplyr::mutate(Espécie = str_to_lower(Espécie)) %>%
   tidyr::unite("nome_especie", Gênero, Espécie, remove = FALSE, sep = " ") 

# Juntando a planilha de ocorrências REFLORA com a planilha com informações do FLORA
reflora_all2 <- left_join(reflora_all, treespp)

# Quantos nomes vieram sem autor (i.e., NA's)
reflora_all2 %>% count(is.na(scientificName))
