##############################################################################################################
   ###                                        Estimando EOO e AOO                                       ###        
##############################################################################################################

# Ler pacotes
library(readxl)
library(textclean)
library(dplyr)
library(stringr)
library(purrr)
library(flora)
library(red)
library(ConR)
library(rgdal)
library(redlistr)

# Plotando mapa do Brasil e Américas.
a <- maps::map(,c("Brazil","Mexico"))
a$range
b <- maps::map(add = T)
b$range

# Ler a planilha com as espécies de plantas do recorte
treespp <- readr::read_csv("./results/names_flora.csv")

especies <- treespp$nome_especie
familias <- treespp$final_family

# Criar uma pasta para colocar as planilhas com estimativas de EOO e AOO
dir.create("./aaoeoo/")

# Loop para estimar EOO e AOO para cada espécie
# Para o loop funcionar o mapa do Brasil precisa estar plotada e aberto no painel de gráfico
# do Rstudio, pois há um comando para plotar os pontos de ocorrência no mapa. Se não tiver com mapa aberto dará um erro:
# "Error in plot.xy(xy.coords(x, y), type = type, ...) : invalid graphics state"
# Conforme tem acontecido com os loops anteriores, este também tem para em algumas circustâncias
for (i in 1:length(especies)) { 
   print(paste(especies[i], familias[i], "- calculating AOO and EOO", i, "of", length(especies)))
   nome_final <- paste0("./output_final5/",familias[i],"/",familias[i], "_",
                        especies[i],"_", "final.csv")
   
   data_csv1 <- paste0("./aaoeoo/", familias[i], "_", especies[i], "_", "aoo_eoo.csv")
   
   tabela.final <- read.csv(nome_final) 
   
   tabela <- tabela.final[complete.cases(tabela.final[,c("decimalLongitude", "decimalLatitude")]),]
   coord <- tabela[,c("decimalLongitude", "decimalLatitude")]
   pts <- sp::SpatialPoints(coord)
   proj4string(pts) <- sp::CRS("+proj=longlat +datum=WGS84")
   pts.utm <- sp::spTransform(pts, CRS("+init=epsg:29193"))
   
   aoo2 <- redlistr::getAOO(pts.utm, 2000)
   red_aoo2 <- round(red::aoo(pts.utm@coords), digits = 2)
   
   # Só estimar EOO se a planilha de ocorrências tiver mais de 3 ocorrências
   # Foi uma tentativa para que o loop não parasse com planilhas com menos de 4 ocorrências, mas não foi bem sucedido. 
   if (aoo2 < 4) {
      data <- data.frame(nome_especie = especies[i],
                         final_family = familias[i],
                         nusado = aoo2,
                         eoo = NA,
                         eoo_conr = NA,
                         aoo2 = aoo2,
                         red_aoo2 = red_aoo2,
                         aoo10 = NA)
      warning(paste(especies[i], "has less than 4 coordinates", "\n"))
      
   }  else { 
      
      eoo <- redlistr::makeEOO(pts.utm)
      eoo_area <- redlistr::getAreaEOO(eoo)
      eoo_conr <- ConR::EOO.computing(coord, Name_Sp = especies[i], write_shp = T)
      points(coord)
      aoo10 <- redlistr::getAOO(pts.utm, 10000)
      
      data <- data.frame(nome_especie = especies[i],
                         final_family = familias[i],
                         nusado = nrow(tabela),
                         eoo = round(eoo_area, digits = 2),
                         eoo_conr = round(eoo_conr$EOO, digits = 2),
                         aoo2 = aoo2,
                         red_aoo2 = red_aoo2,
                         aoo10 = aoo10)
   }
   
   # Exportar as planilhas com estimativas de EOO e AOO
   write.csv(data, file = data_csv1, fileEncoding = "UTF-8", na = "")
   
}

# Juntar os resultados de todas as espécies numa mesma planilha
library(purrr)
tabela_aoo_eoo <- list.files("./aoo", full.names = T) %>%
   purrr::map(.f = read.csv) %>%
   bind_rows() %>% dplyr::select(-1)
names(tabela_aoo_eoo)
dim(tabela_aoo_eoo)
names(treespp)
tabela_final <- left_join(treespp, tabela_aoo_eoo)
tabela_final$eoo
head(tabela_final)
write.csv(tabela_final, file = "./results/tree_final_with_aooeoo.csv")

names(tabela_final)
eeo_alto <- tabela_final %>% filter(eoo > 50000) 
eeo_baixo <- tabela_final %>% filter(eoo <= 50000) 
eeo_na <- tabela_final %>% filter(is.na(eoo)) 
