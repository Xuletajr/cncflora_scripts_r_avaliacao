#' @title Mark occurrences with the municipality informed different from the coordinate
#' @name filt
#'
#' @description A function to mark occurrences with the municipality informed different from the coordinate.
#'
#' @param pts data.frame. Table with points of occurrence, including the municipalities informed on the label. the data frame must contain the following columns in this order: "species", "lon", "lat", "municipality", "adm1"
#' @param inverted logical. If TRUE (default), it will check if longitude and latitude are changed. For now this option may be slow when there are many records of occurrence.
#' @param shape.municipios It can be a shape of municipalities of Brazil in format "SpatialPolygonsDataFrame". If it is NULL, the Brazilian shape will be used available on the IBGE website.
#'
#' @details
#'
#' @return a data frame
#'
#' @author Diogo S. B. Rocha
#'
#'
#' @examples
#'
#' filt(euterpe)
#'
#' @import raster
#' @import dismo
#' @import maptools
#' @import rgdal
#' @import sp
#' @import textclean
#'
#' @export

filt_andrea <- function(pts,
                        inverted = TRUE,
                        shape.municipios = NULL,
                        name.col = c("nome", "stateProvince")) { #poderia ser name.col = c("NOME", "NOMEUF")
   if (class(pts) != "data.frame" & class(pts) != "matrix") {
      stop("Invalid format. Please enter 'data.frame' or 'matrix'.")
   }
   
   if (ncol(pts) != 6) {
      stop(
         "The 'pts' argument must have six columns: 'id', 'species', 'lon', 'lat', 'municipality', 'UF'"
      )
   }
   if (any(pts[, "lat"] > 90) |
       any(pts[, "lat"] < (-90))) {
      stop(
         "There is no latitude greater than 90° or less than -90°"
      )
   }
   if (any(pts[, "lon"] > 90) |
       any(pts[, "lon"] < (-90))) {
      stop(
         "There is no latitude greater than 90° or less than -90°"
      )
   }
   
   
   #pts <- na.exclude(pts)
   
   coordinates(pts) <- ~ lon + lat
   
   if (is.null(shape.municipios)) {
      br_mun
   }
   if (is.null(shape.municipios) == FALSE &
       class(shape.municipios) == "SpatialPolygonsDataFrame") {
      br_mun <- shape.municipios
   }
   if (is.null(proj4string(br_mun))) {
      proj4string(br_mun) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0, 0, 0")
   }
   
   if (is.na(proj4string(pts))) {
      proj4string(pts) <- proj4string(br_mun)
   }
   pts1 <- as.data.frame(pts)
   muni_shape  <- over(pts, br_mun)[, name.col]
   pts1  <- cbind(pts1, muni_shape)
   
   pts1 <- pts1 %>% mutate_at(5:ncol(pts1), .funs = function(x) ifelse(!is.na(x), textclean::replace_non_ascii(tolower(x)), x))
   
   
   pts1$filt <- "NA"
   
   pts1 <- pts1 %>%
      mutate(filt = ifelse(is.na(municipality) | municipality=="", "original municipality not informed", filt)) %>%
      mutate(filt = ifelse(!is.na(municipality) & !is.na(nome) & municipality != nome, "outside municipality", filt)) %>%
      
      mutate(filt = ifelse(is.na(municipality == nome), "outside Brazil", filt)) %>%
      mutate(filt = ifelse(!is.na(municipality) &
                              municipality != "" &
                              !is.na(nome) & nome != "" & municipality == nome, "OK", filt))
   
   #guardar esto#pts2 %>% mutate(municipality = ifelse(municipality == "" &
   #                                         !is.na(nome),
   #                                    nome,
   #                                   municipality))
   pts2 <- pts1 %>% dplyr::select(-adm1, -stateProvince)
   
   pts2 <- pts2 %>% rename("county.original" = "municipality",
                           "county.shape" = "nome")
   
   if (inverted == T) {
      #pts3 <- pts2 %>% filter(filt != "OK")
      valor1 <- pts2
      coordinates(valor1) <- ~ lat + lon
      proj4string(valor1) <- proj4string(br_mun)
      muni <- over(valor1, br_mun)[, name.col]
      if (any(!is.na(muni))) {
         muni2 <- muni %>% mutate_all( .funs = function(x) ifelse(!is.na(x), textclean::replace_non_ascii(tolower(x)), x))
         pts2 <- pts2 %>% mutate(filt = ifelse(county.original == muni2[,1], "inverted coordinates", filt))
      }
   }
   print(table(pts2$filt))
   cat("\n\n")
   return(pts2)
   
}