####################################################################################################
   ###                Baixar dados do Projeto Flora do Brasil 2020 (IPT)                      ###
   ###              e formatar a distribuição e forma de vida das plantas                     ###               
####################################################################################################

# Ler pacotes
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(jsonlite)
library(downloader)
source("./functions/change_NA_to_df.R") 
# Função disponível no GitHub de Andrea S. Tapia:
# https://github.com/AndreaSanchezTapia/CNCFlora_IUCN_LC/blob/master/scripts/change_NA_to_df.R

# Baixar dados do Flora do Projeto Flora do Brasil 2020 (IPT)---
pag <- "http://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil"
download(url = pag, destfile = "iptflora", mode = "wb") # Para sistema operacional windows só funciona com mode = "wb"
unzip("iptflora", exdir = "./ipt") # salvar em pasta "ipt" dentro da pasta de trabalho

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
distribution_mod <- distribution %>% mutate(occurrenceRemarks = omdf$om_all)
head(distribution_mod)

# Exportar planilha csv com a distribuição modificada
write.csv(distribution_mod, "./ipt/distribution_modified.csv", 
          fileEncoding = "UTF-8")
