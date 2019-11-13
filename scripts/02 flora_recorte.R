##############################################################################################################
      ###          Cruzar informações do recorte com o Projeto Flora do Brasil 2020 (IPT)          ###        
##############################################################################################################

# Ler pacotes
library(readr)
library(readxl)
library(dplyr)
library(flora)

# Ler dados FB2020 gerado pelos script 01----
flora <- read_csv("./ipt/all_flora.csv", locale = locale(encoding = "UTF-8")) %>% 
   dplyr::select(-1)

head(flora)

# Ler os dados com as espécies que será trabalhadas no recorte----
# A planilha precisa ter ao menos uma coluna denominada "scientificName" que contenha o nome científico + autoria 
# e uma coluna "Familia" com as famílias das espécies. Colocar a planilha com spp recorte dentro da pasta "data".
treespp <- read_csv("./data/avaliacoes2019_Bloco3.csv") %>%
   dplyr::select(-1) %>%
   # Função do pacote "flora" que retira autoria do nome científico das espécies. Para manter padronização seria importante
   # sempre que entrar uma planilha a coluna com o binômio das espécies ser gerada com essa função. 
   dplyr::mutate(nome_especie = purrr::map(scientificName, ~remove.authors(.)) %>%
             simplify2array())

# Juntar spp do recorte com informações do FB2020 e selecionar colunas que serão mantidas.
treespp2 <- treespp %>%
   dplyr::select(Familia, scientificName, nome_especie) %>%
   dplyr::rename(original_name = scientificName) %>%
   dplyr::left_join(flora) %>%
   dplyr::select(
      Familia,
      original_name,
      scientificName,
      nome_especie,
      taxonID,
      acceptedNameUsageID,
      acceptedNameUsage,
      parentNameUsage,
      family,
      taxonomicStatus,
      nomenclaturalStatus,
      taxonRank,
      vernacular_names,
      nome_especie)

# Excluir os "nomes mal aplicados" e "não validamente publicados". Pode ser útil filtrar outra categorias do "nomenclaturalStatus"
# Há ainda as seguintes possibilidades vindas do FB2020: "NOME_CORRETO";  "NA"; "NOME_LEGITIMO_MAS_INCORRETO";
# "NOME_CORRETO_VIA_CONSERVACAO"; "VARIANTE_ORTOGRAFICA"; "NOME_ILEGITIMO"; "NOME_NAO_EFETIVAMENTE_PUBLICADO";
# "NOME_REJEITADO"; "NOME_APLICACAO_INCERTA". Precisa ver o que fazer nos casos que são NA's, 
# pois essa condição passará pelos ifelse abaixo como se o nome estivesese correto.
treespp2_nodupl <- treespp2 %>%
   dplyr::filter(!nomenclaturalStatus %in% c("NOME_MAL_APLICADO", "NOME_NAO_VALIDAMENTE_PUBLICADO"))

# Sumário do status taxonômico das espécies que ficaram no recorte. Algumas spp podem ser NA's.
treespp2_nodupl %>% dplyr::count(is.na(acceptedNameUsage), taxonomicStatus, nomenclaturalStatus)

# Conferir qual spp têm os nomes mal aplicados
spp_nome_mal_aplicado <- treespp2 %>%
   dplyr::filter(nomenclaturalStatus %in% c("NOME_MAL_APLICADO", "NOME_NAO_VALIDAMENTE_PUBLICADO"))

# Se o nome da sp está correto: "scientificName" - se não aceito deve incluir sinônimos.
treespp3 <- treespp2_nodupl %>%
   dplyr::mutate(final_name = ifelse(is.na(acceptedNameUsage), scientificName, acceptedNameUsage)) %>%
   # Mesma coisa para ID das spp.
   dplyr::mutate(final_ID = ifelse(is.na(acceptedNameUsage), taxonID, acceptedNameUsageID)) %>%
   # Se o nome final for NA colocar o nome que entrou no recorte. Precisa rever essa condição, talvez fosse melhor descartar ID com NA. 
   dplyr::mutate(final_name = ifelse(is.na(taxonID), original_name, final_name)) %>%
   # Mesma coisa para família.
   dplyr::mutate(final_family = if_else(!is.na(family), family, Familia)) %>%
   #new column notes
   dplyr::mutate(notes = if_else(is.na(taxonID), "not found", "LFB")) %>%
   # Nova coluna mostrando se o nome está ok ou é sinônimo. 
   dplyr::mutate(tax_notes = if_else(taxonomicStatus == "NOME_ACEITO" & nomenclaturalStatus == "NOME_CORRETO", "name ok", "")) %>%
   dplyr::mutate(tax_notes = if_else(taxonomicStatus == "SINONIMO", "synonym", tax_notes)) %>%
   dplyr::mutate(tax_notes = if_else(is.na(taxonomicStatus), nomenclaturalStatus, tax_notes)) %>%
   dplyr::mutate(notes_fam = if_else(final_family != Familia, "family changed", "")) %>%
   # 
   dplyr::rename(original_family = Familia,
      name_in_Flora = scientificName,
      original_ID = taxonID,
      scientificName = final_name,
      taxonID = final_ID) %>%
   dplyr::select(original_family, original_name, name_in_Flora,original_ID,
                 final_family, scientificName, taxonID,
                 vernacular_names, notes, tax_notes, notes_fam)
