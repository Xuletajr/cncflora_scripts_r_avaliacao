##############################################################################################################
   ###        Limpezas espaciais e adicionar centróides dos municípios nas coordenadas                  ###        
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
# Função "filt_andrea.R"
source("./functions/filt_andrea.R")

# Ler a planilha com as espécies de plantas do recorte
treespp <- readr::read_csv("./results/names_flora.csv")
familias <- treespp$final_family
especies <- treespp$nome_especie

# Ler a planilha com as coordenadas dos centróides dos municípios
tabela_centroides <- read.delim(file = "./data/centroide_municipio.csv",
                                header = TRUE, sep = ";",
                                stringsAsFactors = FALSE,
                                fileEncoding = "ISO-8859-9")

tabela_centroides$municipality
tabela_centroides$stateProvince

# Ler a planilha com as coordenadas dos centróides das UCs
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

# Comparar o nome dos estados e dos municipios porque há municipios com o mesmo nome de alguns estados, importante para a limpeza----
sort(setdiff(centroides_estados$stateProvince, tabela_centroides$municipality))

# Mesmo nome estado e municipio
tabela_centroides[which(tabela_centroides$municipality %in%
                           centroides_estados$stateProvince),]

dupl_names_state_city <-
   tabela_centroides$municipality[which(tabela_centroides$municipality %in%
                                           centroides_estados$stateProvince)]

#nomes de estado seguros
non_dupl_names <- setdiff(centroides_estados$stateProvince, dupl_names_state_city)

#nomes unicos de municipio
#hay 5570municipios, 282 son duplicados
unique_mpo <- tabela_centroides %>%
   distinct(municipality) %>%
   pull()
dupl_mpo <- tabela_centroides$municipality[duplicated(tabela_centroides$municipality)]
mpo_estado_unico <- setdiff(unique_mpo, dupl_mpo)

#assignação de centroides----
especies

#cria um vetor vazio para ficar de olho em algumas espécies que ainda tem NA nas notas.
# esp?cie 345 n?o gerou! N?o tem informa??es na planilha clean para esta esp?cie!
especies[65]

for (i in 487:length(especies)) { # problemas com 486
   print(paste("Processando", especies[i], i, "de", length(especies), sep = " "))
   
   nome_clean <- paste0("./output_final3/",familias[i],"/",familias[i], "_", especies[i],"_",
                        "clean.csv")
   nome_centroides <- paste0("./output_final3/",familias[i],"/",familias[i], "_", especies[i],"_",
                             "centroides.csv")
   
   tabela_especie <- read.csv(nome_clean, stringsAsFactors = F, encoding = "UTF-8") %>% #row.names = 1, 
      mutate(catalogNumber = factor(catalogNumber))
   
   #corrige nombres de estados
   #corrige siglas
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
   
   #igual com o municipio
   tabela_especie_edit <- tabela_especie %>%
      rename(municipality.original = municipality) %>%
      rename(stateProvince.original = stateProvince) %>%
      rename(country.original = country) %>%
      mutate(municipality = replace_non_ascii(tolower(municipality.original)),
             stateProvince = replace_non_ascii(tolower(stateProvince.original)),
             country = replace_non_ascii(tolower(country.original))) %>%
      #corrige espirito santo
      #mutate(stateProvince = ifelse(stateProvince == "espa-rito santo",
      #                              "espirito santo",
      #                              stateProvince)) %>%
      #corrige estados na casa de municipios
      mutate(municipality = ifelse(municipality %in% c("brasil", "brazil", non_dupl_names), NA, municipality))
   
   #junta con centroides
   tabela_especie_edit <- tabela_especie_edit %>%
      left_join(tabela_centroides) %>%
      left_join(centroides_estados)
   
   #assignar centroides
   tabela_corrigida <- tabela_especie_edit %>%
      # cria as colunas
      mutate(new_Lat = NA, new_Lon = NA, notes = NA) %>%
      # quando é numérico e não é zero
      mutate(new_Lat = ifelse(decimalLatitude != 0 & decimalLongitude != 0,
                              decimalLatitude, new_Lat),
             new_Lon = ifelse(decimalLatitude != 0 & decimalLongitude != 0,
                              decimalLongitude, new_Lon),
             notes = ifelse(decimalLatitude != 0 & decimalLongitude != 0,
                            "original coordinates", notes)) %>%
      #cuando existen y valen cero
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
      
      #quando não existe mas tem municipio , bota o municipio
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
   
   #los cambios que pidió mary
   tabela_corrigida2 <- tabela_corrigida %>%
      mutate(
         municipality = municipality.original,
         stateProvince = stateProvince.original,
         decimalLongitude = new_Lon,
         decimalLatitude = new_Lat,
         comments = notes
      )
   
   tabela_corrigida2[is.na(tabela_corrigida2)] <- ""
   
   #para checar o resultado
   write.csv(tabela_corrigida2, file = nome_centroides, fileEncoding = "UTF-8")
}

####
# Fazendo filtros geoespaciais para passar pelo pacote filt do Diogo. 
###

library(readr)
library(dplyr)
for (i in 487:length(especies)) { # problemas com a espécie 486
   print(paste("Processando", especies[i], i, "de", length(especies), sep = " "))
   nome_centroides <- paste0("./output_final3/",familias[i],"/",familias[i], "_", especies[i],"_",
                             "centroides.csv")
   nome_excluded <- paste0("./output_final3/",familias[i],"/",familias[i], "_", especies[i],"_",
                           "excluded_geo.csv")
   nome_geofilt <- paste0("./output_final3/",familias[i],"/",familias[i], "_", especies[i],"_",
                          "geofilt.csv")
   
   tabela_especie <- read_csv(nome_centroides, locale = locale(encoding = "UTF-8"),
                              na = c("", "NA"))
   #tabela_especie <- read.csv(nome_centroides, row.names = 1, stringsAsFactors = F, 
   #  fileEncoding = "UTF-8") # enconding="UTF-8"
   
   tabela_exclude1 <- tabela_especie %>% dplyr::filter(is.na(decimalLatitude) | is.na(decimalLongitude))
   
   tabela_especie1 <- dplyr::anti_join(tabela_especie, tabela_exclude1)
   
   tabela_exclude2 <- tabela_especie1 %>% dplyr::filter(decimalLatitude < -33.753 | decimalLatitude > 5.272 | # excluindo dados que caiam fora do limite do Brasil. 
                                                           decimalLongitude < -73.991 | decimalLongitude > -28.836 )
   
   tabela_especie2 <- dplyr::anti_join(tabela_especie1, tabela_exclude2) 
   
   tabela_exclude_final <- plyr::rbind.fill(tabela_exclude1, tabela_exclude2) #dplyr::bind_rows(tabela_exclude1, tabela_exclude2, tabela_exclude3)
   write.csv(tabela_exclude_final, file = nome_excluded,  fileEncoding = "UTF-8", na = "") #, fileEncoding = "UTF-8", na = "", row.names = FALSE
   
   write.csv(tabela_especie2, file = nome_geofilt, fileEncoding = "UTF-8", na = "") # fileEncoding = "UTF-8",, na = "", row.names = FALSE
   
}

####sp_filt de Diogo
mpos <- rgdal::readOGR(dsn = "./data/shape/Limites_v2017", layer = "lim_municipio_a",
                       encoding = "UTF-8", use_iconv = TRUE)
tabela_centroides_2 <- tabela_centroides %>%
   rename(geocodigo = GEOCODIGO, nome = NOME) %>%
   mutate(geocodigo = as.factor(geocodigo))
mpos@data <- left_join(mpos@data, tabela_centroides_2)
proj4string(mpos)
mpos2 <- sp::spTransform(mpos, CRSobj = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# names(mpos2)
# dim(mpos@data)

shape.municipios <- mpos2

source("./functions/filt_andrea.R")
#source("./functions/filt_andrea.txt")
#devtools::install_github("AndreaSanchezTapia/spfilt")

## Problemas com spp 50, 333, 339, 340, 341, 353, 359, 370, 375, 380, 424, 436, 452, 460, 
# 475, 478, 479, 482, 486, 494, 497, 498, 504, 507
dir.create("output_final4")

# Loop n?o funciona inteiro, ele para
for (i in 508:length(especies)) { ## Problemas com spp 50, 333, 339, 340, 341, 353, 359, 370, 375, 380, 436, 452, 
   # 460, 475, 478, 479, 482, 486, 494, 494, 497, 498, 504, 507
   print(paste("Processando", especies[i], i, "de", length(especies), sep = " "))
   dir.create(paste0("./output_final4/",familias[i]), showWarnings = F)
   nome_geofilt <- paste0("./output_final3/",familias[i],"/",familias[i], "_", especies[i],"_",
                          "geofilt.csv")
   nome_spfilt <- paste0("./output_final4/",familias[i],"/",familias[i], "_", especies[i],"_",
                         "sp_filt.csv")
   
   tabela_especie <- read.csv(nome_geofilt, row.names = 1, fileEncoding = "UTF-8",
                              stringsAsFactors = F) #fileEncoding = "UTF-8", row.names = 1, stringsAsFactors = F
   
   tabela_especie2 <- tabela_especie %>% dplyr::mutate(ID = row_number())
   
   tabela_sppfilt <- tabela_especie2 %>%
      dplyr::select(ID, scientificName, decimalLongitude, 
                    decimalLatitude, municipality, stateProvince) %>%
      dplyr::rename(species = scientificName,
                    lon = decimalLongitude,
                    lat = decimalLatitude,
                    adm1 = stateProvince)
   
   tabela_sppfilt <- tabela_sppfilt[complete.cases(cbind(tabela_sppfilt$lon, tabela_sppfilt$lat)),]
   
   sp_filt_res <- filt_andrea(pts = tabela_sppfilt,
                              inverted = T,
                              shape.municipios = mpos2)
   print(nrow(tabela_especie))
   print(nrow(tabela_especie2))
   print(nrow(tabela_sppfilt))
   print(nrow(sp_filt_res))
   sp_filt_res <- sp_filt_res %>% 
      dplyr::rename(scientificName = species,
                    decimalLongitude = lon,
                    decimalLatitude = lat,
                    lowercase_municipality = county.original)
   tabela_especie2 <- tabela_especie2 %>%
      dplyr::mutate(lowercase_municipality = textclean::replace_non_ascii(tolower(municipality)))
   
   resultado_final <- left_join(tabela_especie2, sp_filt_res) %>%
      dplyr::mutate(comments = paste(comments, filt)) %>%
      dplyr::mutate(comments = ifelse(filt == "outside municipality",
                                      paste(comments, "original in", lowercase_municipality, "falls in", county.shape),
                                      comments)) %>%
      dplyr::select(one_of(names(tabela_especie)))
   
   if (nrow(resultado_final) != nrow(tabela_especie)) stop()
   write.csv(resultado_final, nome_spfilt, fileEncoding = "UTF-8", row.names = FALSE)
   
}

# Verificando os pontos de ocorr?ncia que ca?ram fora do Brasil. 
library(dplyr)
library(magrittr)
sp_filt_final <- list.files("output_final4", pattern = "sp_filt.csv$", recursive = T, full.names = T)
arq <- sp_filt_final %>% purrr::map(~read.csv(., row.names = 1, fileEncoding = "UTF-8"))

# **** daqui para baixo as coisas n?o est?o dando certas!!!!
arq2 <- arq %>% purrr::map( ~ select(.,
                                     family, scientificName, decimalLatitude, #X1,
                                     decimalLongitude, country.original, stateProvince.original,
                                     municipality.original, locality, nome, notes, comments,))
arq3 <- arq2 %>% bind_rows()
warnings()
cuenta <- count(arq3, comments)

no <- grep(pattern = "original in  falls", x = cuenta$comments)
mpo <- grep(pattern = "outside municipality", x = cuenta$comments)
nrow(cuenta)
length(no)
length(mpo)
cuenta %>% slice(-mpo)

names(arq3)

outside_brazil <- arq3 %>% filter(comments == "original coordinates outside Brazil")

unique(outside_brazil$scientificName)

write.csv(outside_brazil, "./results/outside_brazil.csv", fileEncoding = "UTF-8")

#dif entre inpa y spfilt

# Filtrar as coordenadas que ca?ram fora do Brasil - 357 registros... 
dir.create("output_final5")
library(plyr)

arq4 <- arq %>% rbind.fill() %>% filter(!comments == "original coordinates outside Brazil")

arq4$scientificName

# Exportar as ocorrencias por esp?cie
for (i in 1:length(especies)){
   dir.create(paste0("./output_final5/",familias[i]), showWarnings = F)
   nome_arquivo <- paste0("./output_final5/", familias[i],"/",familias[i],"_", especies[i],"_", "final.csv")
   print(paste("Processando", especies[i], i, "de", length(especies), sep = " "))
   
   if (!file.exists(nome_arquivo)){
      occs <- list()
      occs[[i]] <- arq4 %>% filter(scientificName %in% especies[i])
      
      print(lapply(occs,dim))
      if (any(!is.null(lapply(occs,dim)))){
         dim.null <- lapply(occs, function(x) {!is.null(dim(x))})
         occs.f <- subset(occs, dim.null == T)
         occs.f <- dplyr::bind_rows(occs.f)
         print(dim(occs.f))
         write.csv(occs.f, nome_arquivo, na = "", fileEncoding = "UTF-8") # row.names = F,
      }
      
   } else {
      warning(paste("No data", especies[i], "\n"))
   }
}

######   end----