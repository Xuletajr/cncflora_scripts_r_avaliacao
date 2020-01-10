##############################################################################################################
###                               Baixar e organizar dados do speciesLink                            ###        
##############################################################################################################

# Ler pacotes
library(readxl)
library(dplyr)
library(tidyr)
library(biogeo)
library(readr)
library(stringr)

# Os dados de ocorrência foram baixados diratamente do site do speciesLink buscando 40 spp por vez utilizando
# o binômio das espécies  ---- fonte: www.splink.org.br
# Próximo passa seria automatizar o script para baixar diretamente as ocorrências do speciesLink, uma possibilidade
# é seria a função 'rspeciesLink' da Sara Mortara (https://github.com/saramortara/rspeciesLink) --- Precisa testar. 

# Ler a planilha com as espécies de plantas do recorte
treespp <- readr::read_csv("./results/names_flora.csv")

# Colocar as famílias e binômio das espécies em vetores.  
familias <- treespp$final_family
especies <- treespp$nome_especie

# Ler a tabela com todas as ocorrências provenientes do speciesLink
# Estas ocorrências foram baixadas em planilhas separadas e juntadas em uma planilha só previamente
speciesLink <- read_csv("./results/speciesLink_geral.csv",
                        locale = locale(encoding = "UTF-8"))

# Filtra somente os nomes das espécies iguais aos da nossa lista (alguns nomes vêm desconfigurados)
speciesLink2 <- speciesLink %>% filter(scientificname %in% especies) %>%
   mutate(nome_especie = scientificname) %>% rename(notas  = notes) %>% 
   left_join(treespp) %>%
   dplyr::mutate(acceptedNameUsage = scientificName) %>%
   dplyr::mutate(scientificNameAuthorship = word(acceptedNameUsage, 3, -1, sep = " "))

# Conferindo algumas informações da planilha
names(speciesLink2) # variáveis disponíveis na planilha
speciesLink2$scientificNameAuthorship # Se os nomes vieram escritos corretamente, dependendo do "enconding" pode desconfigurar
speciesLink2 %>% count(is.na(scientificNameAuthorship)) # Quantos nomes vieram sem autor (i.e., NA's)
