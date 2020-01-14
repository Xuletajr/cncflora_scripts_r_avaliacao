##############################################################################################################
   ###                                      Juntando todos os dados                                     ###        
##############################################################################################################

# Ler pacotes
library(tidyverse)
library(readr)

# Ler as diferentes planilhas
# Tava dando errado usando read.csv
#aoo <- read.csv("./results/aoo_veg_cites.csv", row.names = 1, fileEncoding = "UTF-8")

aoo <- readr::read_csv("./results/aoo_veg_cites.csv")
use <- read.csv("./results/Use_results_general.csv", row.names = 1,  fileEncoding = "UTF-8") %>%
   rename(nome_especie = especies)
ucs <- read.csv("./results/tabela_UCs.csv", row.names = 1, fileEncoding = "UTF-8") %>% 
   dplyr::rename(nusado_ucs = nusado)
coleta <- read.csv("./results/spp_coleta.csv", row.names = 1, fileEncoding = "UTF-8")

names(aoo)
names(use)
names(ucs)
names(coleta)

# Juntar tudo
final <- left_join(aoo, use) %>% left_join(ucs) %>%
   dplyr::left_join(coleta) %>%
   dplyr::arrange(final_family, nome_especie) %>% 
   dplyr::mutate(scientificNameAuthorship = word(scientificName, 3, -1, sep = " ")) %>%
   dplyr::select(-1) # Tirando a primeira coluna com números

View(final)

# Exportar planilha com as informações juntas
write.csv(final, "./results/final_results.csv", fileEncoding = "UTF-8")

######   end----