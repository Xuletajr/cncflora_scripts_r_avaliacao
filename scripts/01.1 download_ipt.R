#######################################################################################################################################
     ###       Baixar dados do Projeto Flora do Brasil 2020 (IPT)  e formatar a distribuição e forma de vida das plantas       ###        
#######################################################################################################################################

# Ler pacotes
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(jsonlite)
library(downloader)
# Função change_NA_to_df.R disponível no GitHub de Andrea S. Tapia:
# https://github.com/AndreaSanchezTapia/CNCFlora_IUCN_LC/blob/master/scripts/change_NA_to_df.R
source("./functions/change_NA_to_df.R") 

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

# Formatar a forma de vida das espécie (lifeForm)---
lf_habitat <- read_delim("./ipt/speciesprofile.txt", delim = "\t", quote = "")

names(lf_habitat)
head(lf_habitat) # Está cheio de NAs

lf <- list(); hab <- list(); veg <- list()

for (i in seq_along(lf_habitat$lifeForm)) {
   lf[[i]] <- data.frame(id = lf_habitat$id[i], lifeForm = NA)
   hab[[i]] <- data.frame(id = lf_habitat$id[i], habitat = NA)
   veg[[i]] <- data.frame(id = lf_habitat$id[i], vegetationType = NA)
   if (!is.na(lf_habitat$lifeForm[i])) {
      jason <- jsonlite::fromJSON(as.character(lf_habitat$lifeForm[i]))
      if ("lifeForm" %in% names(jason)) lf[[i]] <- data.frame(id = lf_habitat$id[i], lifeForm = jason["lifeForm"])
      if ("habitat" %in% names(jason)) hab[[i]] <- data.frame(id = lf_habitat$id[i], habitat = jason["habitat"])
      if ("vegetationType" %in% names(jason)) veg[[i]] <- data.frame(id = lf_habitat$id[i], vegetationType = jason["vegetationType"])
   }
}

head(lf)
head(veg)
head(hab)

lf2 <- lf %>%
   purrr::map(~ mutate(., lifeForm = paste(lifeForm, collapse = "-"))) %>%
   purrr::map(~distinct(.))

lf3 <- bind_rows(lf2)

veg2 <- veg %>%
   purrr::map(~ mutate(., vegetationType = paste(vegetationType, collapse = "-"))) %>%
   purrr::map(~distinct(.))

veg3 <- bind_rows(veg2)

hab2 <- hab %>%
   purrr::map(~ mutate(., habitat = paste(habitat, collapse = "-"))) %>%
   purrr::map(~distinct(.))

hab3 <- bind_rows(hab2)

lf_hab <- lf3 %>% left_join(veg3) %>% left_join(hab3)
lf_habitat %>%
    head(.)
lf_hab %>%
    head(.)

# Exportar planilha csv com a forma de vida das árvores modificada
write.csv(lf_hab, "./ipt/lf_hab_modified.csv", fileEncoding = "UTF-8")
