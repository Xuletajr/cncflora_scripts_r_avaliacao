##############################################################################################################
   ###                            Verificar ocorrências em Unidades de Conservação                      ###        
##############################################################################################################

# Ler pacotes
library(rgdal)
library(dplyr)
library(sf)
library(readr)
library(purrr)

# Ler shape com as Unidades de Conservação
uc <- rgdal::readOGR(dsn = "./data/shape/UCs_WGS84", layer = "UCs_wgs84", 
              use_iconv = TRUE, encoding = "UTF-8")

st_crs(uc)
proj4string(uc)
uc$NOME_UC1

# Projetando as UCs
proj4string(uc) <-  CRS("+proj=longlat +datum=WGS84")
uc <- spTransform(uc, CRS=CRS("+proj=longlat +datum=WGS84"))
st_crs(uc)
class(uc)

# Ler a planilha com as espécies de plantas do recorte
treespp <- readr::read_csv("./results/aoo_veg_cites.csv")

especies <- treespp$nome_especie
familias <- treespp$final_family

# Plotando mapa do Brasil com Unidades de Conservação
estados <- rgdal::readOGR(dsn="./data/shape/Limites_v2017", 
                          layer="lim_unidade_federacao_a")

st_crs(estados)
plot(estados)

plot(uc, add = T, col = "cyan") 

# Criar uma pasta para colocar os dados de ocorrência em UCs
dir.create("./UC")

# Loop para checar quais ocorrências são em UCs
for (i in 1:length(especies)) { 
   print(paste(especies[i], familias[i], "- analyzing presence in UCs", i, "of", length(especies)))
   
   nome_final <- paste0("./output_final5/",familias[i],"/",familias[i], "_",
                        especies[i],"_", "final.csv")
   
   dir.create(paste0("./UC/",familias[i]), showWarnings = F)
   uc_output <- paste0("./UC/", familias[i], "/", familias[i], "_", especies[i],"_", "UC.csv")
   
   tabela.spfilt <- read.csv(nome_final, row.names = 1, fileEncoding = "UTF-8") %>%
      # Só checar ocorrência apenas para coordenadas orginais, centróides não são usados
      dplyr::filter(comments %in% "original coordinates OK") 
   
   tabela <- tabela.spfilt[complete.cases(tabela.spfilt[,c("decimalLongitude", "decimalLatitude")]),]
   pts <- SpatialPoints(tabela[,c("decimalLongitude", "decimalLatitude")])
   coordinates(tabela) <- ~ decimalLongitude + decimalLatitude
   proj4string(pts) <- CRS("+proj=longlat +datum=WGS84")
   # Andrea estava utilizando sad69. Não consigui chegar a uma conclusão pq SAD69 pudesse ser melhor
   pts.wgs84 <- spTransform(pts, CRS("+proj=longlat +datum=WGS84"))
   
   over_results <- sp::over(pts.wgs84, uc)
   
   n_in_uc <- over_results %>% 
      dplyr::filter(!is.na(ID_UC0)) %>% 
      dplyr::count() %>% 
      pull
   
   n_uc <- over_results %>% 
      dplyr::filter(!is.na(ID_UC0)) %>% 
      dplyr::distinct(NOME_UC1) %>% 
      dplyr::count() %>% 
      pull
   
   which_uc <- ifelse(n_in_uc == 0, NA, over_results %>%
                         dplyr::filter(!is.na(ID_UC0)) %>%
                         dplyr::distinct(NOME_UC1) %>%
                         purrr::map(.f = stringr::str_to_title)) 
   
   final_res <- data.frame(nome_especie = especies[i],
                           final_family = familias[i],
                           nusado = nrow(tabela),
                           occ_in_UC = n_in_uc,
                           n_UC = n_uc,
                           UC = which_uc[[1]])
   
   write.csv(final_res, file = uc_output, fileEncoding = "UTF-8")
   
}

# Juntando as informações de ocorrências nas UCs
tabela_UCs <- list.files("./UC", full.names = T, pattern = "UC.csv$", recursive = T) %>%
   purrr::map(.f = readr::read_csv) %>%
   dplyr::bind_rows() %>%
   dplyr::select(-1)  %>%  group_by(nome_especie) %>% 
   dplyr::mutate(UCs = paste(UC, collapse = ", ")) %>%
   dplyr::select(-UC) %>% distinct() %>% ungroup()

tabela_UCs

write.csv(tabela_UCs, "./results/tabela_UCs.csv", na = "", fileEncoding = "UTF-8")

