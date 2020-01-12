##############################################################################################################
   ###     Limpezas espaciais e adicionar coordenadas dos centroides dos municípios nas ocorrências     ###        
##############################################################################################################

# Ler pacotes
library(dplyr)
library(stringr)
library(rgbif)
library(readxl)
library(textclean)
library(flora)
library(lubridate)
library(rgdal)
library(readr)
# Função "filt_andrea.R"
source("./functions/filt_andrea.R")

# Ler a planilha com as espécies de plantas do recorte
treespp <- readr::read_csv("./results/names_flora.csv")
familias <- treespp$final_family
especies <- treespp$nome_especie

# Ler a planilha com as coordenadas dos centroides dos municípios
tabela_centroides <- read.delim(file = "./data/centroide_municipio.csv",
                                header = TRUE, sep = ";",
                                stringsAsFactors = FALSE,
                                fileEncoding = "ISO-8859-9")

tabela_centroides$NOME
tabela_centroides$NOMEUF

# Tirar todos os caracteres e botar minúscula
tabela_centroides <- tabela_centroides %>%
   mutate(municipality = replace_non_ascii(tolower(NOME)),
          stateProvince = replace_non_ascii(tolower(NOMEUF)))

tabela_centroides$municipality
tabela_centroides$stateProvince

# Ler a planilha com as coordenadas dos centroides das UCs
tabela_centroides_ucs <- read.delim(file = "./data/centroide_uc.csv",
                                    header = TRUE, sep = ";",
                                    stringsAsFactors = FALSE,
                                    fileEncoding = "ISO-8859-9")

# Deixar todos os nomes das UCs em minúsculo
tabela_centroides_ucs <- tabela_centroides_ucs %>%
   dplyr::mutate(uc = replace_non_ascii(tolower(NOME_UC1)))

tabela_centroides_ucs$uc

# Ler shape dos estados
estados <- rgdal::readOGR(dsn = "./data/shape/Limites_v2017", layer = "lim_unidade_federacao_a",
                          encoding = "UTF-8", use_iconv = TRUE)

estados$nome

# Tabela com a sigla pois uma limpeza é substituir a sigla ("rj") pelo nome completo
sigla_estados <- estados@data[,c("nome", "sigla")] %>%
   data.frame() %>%
   mutate(stateProvince = replace_non_ascii(tolower(nome))) %>%
   mutate(sigla = replace_non_ascii(tolower(sigla)))


# Centroides estados---- já deixa em minusculo sem acento tudo
centroides_estados <- rgeos::gCentroid(estados, byid = T, id = estados$nome) %>%
   data.frame %>%
   tibble::rownames_to_column(var = "estados_shp") %>%
   mutate(stateProvince = replace_non_ascii(tolower(estados_shp))) %>%
   rename(x_estado = x, y_estado = y) %>%
   left_join(sigla_estados)

centroides_estados

# Comparar o nome dos estados e dos municípios porque há municípios com o mesmo nome de alguns estados, importante para a limpeza----
sort(setdiff(centroides_estados$stateProvince, tabela_centroides$municipality))

# Mesmo nome estado e município
tabela_centroides[which(tabela_centroides$municipality %in%
                           centroides_estados$stateProvince),]

dupl_names_state_city <-
   tabela_centroides$municipality[which(tabela_centroides$municipality %in%
                                           centroides_estados$stateProvince)]

# Nomes de estado seguros
non_dupl_names <- setdiff(centroides_estados$stateProvince, dupl_names_state_city)

# Nomes únicos de município
# hay 5570 municipios, 282 son duplicados
unique_mpo <- tabela_centroides %>%
   distinct(municipality) %>%
   pull()
dupl_mpo <- tabela_centroides$municipality[duplicated(tabela_centroides$municipality)]
mpo_estado_unico <- setdiff(unique_mpo, dupl_mpo)

# Loop para adicionar centroides----
# Precisa trabalhar neste loop para que não pare... Loop tem parado em algumas circustâncias, por exemplo,
# após as limpezas algumas espécies ficaram com zero ocorrências na planilha Clean que é utilzada aqui. 
for (i in 488:length(especies)) {
   print(paste("Processando", especies[i], i, "de", length(especies), sep = " "))
   
   nome_clean <- paste0("./output_final3/",familias[i],"/",familias[i], "_", especies[i],"_",
                        "clean.csv")
   
   nome_centroides <- paste0("./output_final3/",familias[i],"/",familias[i], "_", especies[i],"_",
                             "centroides.csv")
   
   tabela_especie <- read.csv(nome_clean, stringsAsFactors = F, encoding = "UTF-8") %>% #row.names = 1, 
      mutate(catalogNumber = factor(catalogNumber))
   
   # Corrige nombres de estados
   # Corrige siglas
   substituir_siglas <- function(x) {
      x1 <- textclean::replace_non_ascii(tolower(x))
      if (any(sigla_estados$sigla %in% x1)) {
         return(as.character(sigla_estados$nome[which(sigla_estados$sigla == x1)]))
         
      } else {
         as.character(x)
      }
   }
   
   tabela_especie$stateProvince <-
      purrr::map(tabela_especie$stateProvince, .f = substituir_siglas) %>%
      simplify2array()
   
   # Igual com o municipio
   tabela_especie_edit <- tabela_especie %>%
      rename(municipality.original = municipality) %>%
      rename(stateProvince.original = stateProvince) %>%
      rename(country.original = country) %>%
      mutate(municipality = replace_non_ascii(tolower(municipality.original)),
             stateProvince = replace_non_ascii(tolower(stateProvince.original)),
             country = replace_non_ascii(tolower(country.original))) %>%
      
      #corrige espirito santo # acho que isso não está sendo mais necessário, o nome dos estados no shp file está sem erros...
      #mutate(stateProvince = ifelse(stateProvince == "espa-rito santo",
      #                              "espirito santo",
      #                              stateProvince)) %>%
      #corrige estados na casa de municipios
      mutate(municipality = ifelse(municipality %in% c("brasil", "brazil", non_dupl_names), NA, municipality))
   
   # Junta con centroides
   tabela_especie_edit <- tabela_especie_edit %>%
      left_join(tabela_centroides) %>%
      left_join(centroides_estados)
   
   # Assignar centroides
   tabela_corrigida <- tabela_especie_edit %>%
      # Cria as colunas
      mutate(new_Lat = NA, new_Lon = NA, notes = NA) %>%
      # Quando é numérico e não é zero
      mutate(new_Lat = ifelse(decimalLatitude != 0 & decimalLongitude != 0,
                              decimalLatitude, new_Lat),
             new_Lon = ifelse(decimalLatitude != 0 & decimalLongitude != 0,
                              decimalLongitude, new_Lon),
             notes = ifelse(decimalLatitude != 0 & decimalLongitude != 0,
                            "original coordinates", notes)) %>%
      # Cuando existen y valen cero
      mutate(new_Lat = ifelse(
         decimalLatitude == 0 & decimalLongitude == 0 & !is.na(municipality) &
            !is.na(stateProvince),
         POINT_Y,
         new_Lat),
         new_Lon = ifelse(
            decimalLatitude == 0 &
               decimalLongitude == 0 &
               !is.na(municipality) &
               !is.na(stateProvince),
            POINT_X,
            new_Lon),
         notes = ifelse(
            decimalLatitude == 0 & decimalLongitude == 0 &  !is.na(municipality) &
               !is.na(stateProvince),
            "centroide mpo (0)",
            notes)) %>%
      
      # Quando não existe mas tem municipio, bota o municipio
      mutate(new_Lat = ifelse(
         is.na(decimalLatitude) & is.na(decimalLongitude) &  !is.na(municipality) &
            !is.na(stateProvince), POINT_Y, new_Lat),
         new_Lon = ifelse(
            is.na(decimalLatitude) & is.na(decimalLongitude) &  !is.na(municipality) &
               !is.na(stateProvince), POINT_X, new_Lon),
         notes = ifelse(
            is.na(decimalLatitude) & is.na(decimalLongitude) &  !is.na(municipality) &
               !is.na(stateProvince), "centroide mpo", notes)) %>%
      mutate(notes = ifelse(
         decimalLatitude == 0 & decimalLongitude == 0 & is.na(municipality) &
            is.na(stateProvince),
         "no coordinates, municipality or state provided (0)", notes)) %>%
      mutate(notes = ifelse(
         is.na(decimalLatitude) & is.na(decimalLongitude) &  is.na(municipality) &
            is.na(stateProvince),
         "no coordinates, municipality or state provided", notes)) %>%
      mutate(new_Lat = ifelse(is.na(notes) & municipality %in% unique_mpo, POINT_Y, new_Lat),
             new_Lon = ifelse(is.na(notes) & municipality %in% unique_mpo, POINT_X, new_Lon),
             notes = ifelse(is.na(notes) & municipality %in% unique_mpo, "centroide mpo (no state)", notes)) %>%
      mutate(new_Lat = ifelse(is.na(notes) & municipality %in% dupl_mpo, POINT_Y, new_Lat),
             new_Lon = ifelse(is.na(notes) & municipality %in% dupl_mpo, POINT_X, new_Lon),
             notes = ifelse(is.na(notes) & municipality %in% dupl_mpo, "check coordinate: municipality name is duplicated", notes))
   
   print(count(tabela_corrigida, notes))
   
   # Los cambios que pidió mary
   tabela_corrigida2 <- tabela_corrigida %>%
      mutate(
         municipality = municipality.original,
         stateProvince = stateProvince.original,
         decimalLongitude = new_Lon,
         decimalLatitude = new_Lat,
         comments = notes
      )
   
   tabela_corrigida2[is.na(tabela_corrigida2)] <- ""
   
   # Para checar o resultado
   write.csv(tabela_corrigida2, file = nome_centroides, fileEncoding = "UTF-8")
}

###
# Fazendo filtros geoespaciais para passar pela função filt do Diogo. 
# Precisa pensar melhor para colocar isso dentro de outro loop, principalmente alguns poucos 
# pontos que podem cair fora do limite do Brasil... 

for (i in 1:length(especies)) {
   print(paste("Processando", especies[i], i, "de", length(especies), sep = " "))
   nome_centroides <- paste0("./output_final3/",familias[i],"/",familias[i], "_", especies[i],"_",
                             "centroides.csv")
   nome_excluded <- paste0("./output_final3/",familias[i],"/",familias[i], "_", especies[i],"_",
                           "excluded_geo.csv")
   nome_geofilt <- paste0("./output_final3/",familias[i],"/",familias[i], "_", especies[i],"_",
                          "geofilt.csv")
   
   tabela_especie <- read_csv(nome_centroides, locale = locale(encoding = "UTF-8"),
                              na = c("", "NA"))
   
   tabela_exclude1 <- tabela_especie %>% dplyr::filter(is.na(decimalLatitude) | is.na(decimalLongitude))
   
   tabela_especie1 <- dplyr::anti_join(tabela_especie, tabela_exclude1)
  
   # Excluindo dados que caiam fora do limite do Brasil. 
   tabela_exclude2 <- tabela_especie1 %>% dplyr::filter(decimalLatitude < -33.753 | decimalLatitude > 5.272 | 
                                                           decimalLongitude < -73.991 | decimalLongitude > -28.836 )
   
   tabela_especie2 <- dplyr::anti_join(tabela_especie1, tabela_exclude2) 
   
   tabela_exclude_final <- plyr::rbind.fill(tabela_exclude1, tabela_exclude2) 
   write.csv(tabela_exclude_final, file = nome_excluded,  fileEncoding = "UTF-8", na = "") 
   
   write.csv(tabela_especie2, file = nome_geofilt, fileEncoding = "UTF-8", na = "") 
   
}

######   end----