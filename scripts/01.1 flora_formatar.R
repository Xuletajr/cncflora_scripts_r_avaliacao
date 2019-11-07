#####################################################################################################
          ### Baixar, formatar e exportar do projeto Flora do Brasil 2020 (FB - IPT) ###        
#####################################################################################################

# Ler pacotes
library(dplyr)
library(flora)
library(readr)
library(purrr)

# Ler a distribução formatada (gerada pelo script "01.1 download_ipt")---
distribution <- read_csv("./ipt/distribution_modified.csv") %>%
   dplyr::select(-1) # Já pode ser gerado com "row.names = FALSE"

head(distribution)

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
