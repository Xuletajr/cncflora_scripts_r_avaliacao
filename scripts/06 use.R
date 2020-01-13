##############################################################################################################
   ###                                     Buscando uso das espécies                                    ###        
##############################################################################################################

library(rvest)
library(stringr)
library(magrittr)
library(dplyr)
library(purrr)

treespp <- read.csv("./results/names_flora.csv", fileEncoding = "UTF-8") #, row.names = 1
names(treespp)
familias <- treespp$final_family
especies <- unique(sort(treespp$nome_especie))

#Specifying the url for desired website to be scraped
url_id <- 'http://tropical.theferns.info/viewtropical.php?id='
url_full <- 'http://tropical.theferns.info/viewtropical.php?full='

names <- especies %>% 
   stringr::str_split(" ", simplify = T) %>%
   data.frame() %>% 
   rename(Genero = X1, epiteto = X2)

especie <- especies[1]

# Fun??o para ler os dados na web
read.webpage.text = function(url_vectors_id){
   read_html(url_vectors_id) %>% html_nodes('.PageBox') %>% html_text()
}

dir.create("./output_final5/_use")

buscar_the_ferns <- function(especie) {
   names <- especie %>% stringr::str_split(" ", simplify = T) %>%
      data.frame() %>% rename(Genero = X1, epiteto = X2)
   url_vectors_id <-  paste0(url_id, names$Genero,"+", names$epiteto)
   
   #Reading the HTML code from the website
   webpage_i <- map(url_vectors_id, 
                    possibly(read.webpage.text, 
                             otherwise = paste0("The webpage of ", especies[i], " could not be acessed")))
   
   write.table(webpage_i,paste0("./output_final5/_use/",especies[i],".txt"))
   Sys.sleep(0.5)
}

# Rodando a fun??o para todas as esp?cies
for (i in 1:length(especies)) {
   print(paste(especies[i], i, "de", length(especies), sep = " "))
   buscar_the_ferns(especies[i])
   
}

# 211, 286 paila
files <- list.files("./output_final5/_use", full.names = T)
done <- files %>% str_split("/", simplify = T) %>% 
   data.frame() %>% dplyr::select(4) %>% pull %>% 
   str_split(".txt", simplify = T) %>% data.frame() %>% 
   dplyr::select(1)

setdiff(done$X1, especies)
setdiff(especies, done$X1)
which(especies %in% setdiff(especies, done$X1))
head(files)
head(done)

Res <- list()
for (i in seq_along(files)) {
   #names <- done[i,] %>% stringr::str_split(" ", simplify = T) %>%
   #    data.frame() %>% rename(Genero = X1, epiteto = X2)
   url_vectors_id <-  paste0(url_id, names$Genero[i],"+", names$epiteto[i])
   rl <- readLines(files[i],skipNul = T)
   gr <- paste0("^", done[i,], "$")
   gr2 <- rl[grep(gr, rl)]
   Res[[i]] <- data.frame(especies = done[i,]) %>%
      mutate(use = ifelse(length(gr2) == 0, "", "use")) %>%
      mutate(url = ifelse(use == "use", url_vectors_id, ""))
}

#Res <- list()
#for (i in seq_along(files)) {

#names <- done[i,] %>% stringr::str_split(" ", simplify = T) %>%
#    data.frame() %>% rename(Genero = X1, epiteto = X2)
#    url_vectors_id <-  paste0(url_id, names$Genero,"+", names$epiteto)
#    rl <- readLines(files[i],skipNul = T)
#    gr <- paste0("^", done[i,], "$")
#    gr2 <- rl[grep(gr, rl)]
#    Res[[i]] <- data.frame(especies = done[i,]) %>%
#        mutate(use = ifelse(length(gr2) == 0, "", "use")) %>%
#        mutate(url = ifelse(use == "use",url_vectors_id,""))
#}

Results_use <- Res %>% bind_rows()
write.csv(Results_use, "./results/Use_results_general2.csv")

done[2,] %>% stringr::str_split(" ", simplify = T) %>%
   data.frame() %>% rename(Genero = X1, epiteto = X2)

names <- especies %>% stringr::str_split(" ", simplify = T) %>% 
   data.frame() %>% rename(Genero = X1, epiteto = X2)
#TESTANDO AS LINHAS DO LOOP 
readLines(files[2], skipNul = T)
