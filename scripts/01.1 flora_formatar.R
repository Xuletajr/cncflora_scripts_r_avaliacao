######################################################################################################
   ###         Baixar, formatar e exportar dados do Projeto Flora do Brasil 2020 (IPT)          ###        
######################################################################################################

# Ler pacotes
library(dplyr)
library(flora)
library(readr)
library(purrr)
library(tidyr)
library(stringr)
library(jsonlite)
library(downloader)

# Função change_NA_to_df.R disponível no GitHub de Andrea S. Tapia:
# https://github.com/AndreaSanchezTapia/CNCFlora_IUCN_LC/blob/master/scripts/change_NA_to_df.R
source("./functions/change_NA_to_df.R") 

# Baixar dados do Flora do Projeto Flora do Brasil 2020 (IPT)---
pag <- "http://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil"
# Para sistema operacional windows só funcionou com mode = "wb". Possivelmente precisará de ajustes para outrs sistemas operacionais. 
download(url = pag, destfile = "iptflora", mode = "wb") 
unzip("iptflora", exdir = "./ipt") # descompactar e salvar dentro subpasta "ipt" na pasta principal

# Formatar distribuição das espécies flora---
distribution  <- read_delim("./ipt/distribution.txt", delim = "\t", quote = "") %>%
   dplyr::group_by(id) %>%
   dplyr::mutate(location = paste(locationID, collapse = "-")) %>%
   dplyr::select(-locationID) %>% distinct() %>% ungroup()

names(distribution)

ocurrence_remarks <- distribution %>%
   dplyr::select(occurrenceRemarks) %>%
   data.frame() %>%
   dplyr::mutate(occurrenceRemarks = as.character(occurrenceRemarks))

head(ocurrence_remarks)

occurrenceRemarks_df <- purrr::map(ocurrence_remarks$occurrenceRemarks, 
                                   ~data.frame(jsonlite::fromJSON(.))) %>%
   purrr::map(., ~ mutate(., om = paste(.$endemism, .$phytogeographicDomain,
                                        sep = "/"))) %>%
   purrr::map( ~ mutate(., om_all = paste(om, collapse = "-"))) %>%
   purrr::map( ~ dplyr::select(., om_all)) %>%
   purrr::map( ~ distinct(.)) %>%
   purrr::map(., .f = ~change_others_to_dataframe(.))

omdf <- bind_rows(occurrenceRemarks_df,.id = "sp")

head(distribution)

# Distribuição com as observações de ocorrência (occurrenceRemarks) modificadas
# A coluna "location" tem sigla BR antes dos Estados, talvez fosse melhor excluir esta informação
distribution_mod <- distribution %>% mutate(occurrenceRemarks = omdf$om_all)

head(distribution_mod)

# Exportar planilha csv com a distribuição modificada
write.csv(distribution_mod, "./ipt/distribution_modified.csv", 
          fileEncoding = "UTF-8")

# Ler informações taxon---
taxon <- read_tsv("./ipt/taxon.txt", quote = "", trim_ws = T)

head(taxon)

# Ler informações sobre a taxonomia---
relationship <- read_delim("./ipt/resourcerelationship.txt", delim = "\t", 
                           quote = "") %>% distinct()

head(relationship)

relacion <- unique(relationship$relationshipOfResource)
relacion

# Ler informações das referências bibliográficas das espécies---
ref <- read_delim("./ipt/reference.txt",delim = "\t", quote = "") 

head(ref)

# Ler a forma de vida formatada (gerada pelo script "01.1 download_ipt")---
lf_mod <- read_csv("./ipt/lf_hab_modified.csv") %>% 
   select(-1)

head(lf_mod)

#  Ler informações sobre espécimes---
types <- read_delim("./ipt/typesandspecimen.txt", delim = "\t", quote = "") %>%
   group_by(id) %>%
   mutate_all(.funs = function(x) paste(x, collapse = "-"))

head(types)

#  Ler nomes populares---
vernacular <- read_delim("./ipt/vernacularname.txt", delim = "\t") %>%
   mutate(vernacular = paste(vernacularName, language, locality, sep = "-")) %>%
   dplyr::select(id, vernacular) %>%
   group_by(id) %>% mutate(vernacular_names = paste(vernacular, collapse = "/")) %>%
   dplyr::select(-vernacular) %>% distinct()

names(vernacular)

# Ler planilha do Global Tree Search
gtsearch <- read.csv("./data/global_tree_search_trees_1_3.csv") %>%
   mutate(GTSearch = "sim") %>% 
   rename(nome_especie = Taxon.name) %>% 
   select(nome_especie, GTSearch)

# Juntar todas as informações do projeto Flora do Brasil 2020 em uma única planilha com Global Tree Search 
# Está levando ~ 4 minutos para juntar os dados.
all  <- left_join(taxon, distribution_mod) %>%
   left_join(ref) %>% left_join(lf_mod) %>%
   left_join(types) %>% left_join(vernacular) %>% 
   left_join(gtsearch) %>% distinct() %>% 
   mutate(nome_especie = purrr::map(scientificName, ~remove.authors(.)) %>%
                            simplify2array())

# Exportar planilha csv com todos os dados da flora do brasil + GTS juntos
write.csv(all, "./ipt/all_flora.csv", fileEncoding = "UTF-8")

######   end----