### baixa dados gbif  ###

# Pablo Hendrigo Alves de Melo - pablopains@yahoo.com.br
# Adaptação da função José W. Ribeiro Jr.

###  datagbif	###

###---------------------------------------------------------------------###

# utiliza pacote rgbif, função occ_seach,  busca em scientificName
# concatena resultados de buscas nomes válidos e sinônimos de uma mesma espécie 
# permite ajustar pra utilizar difrentes fontes de sinônimos
# força retorno de todas as culunas gbif, com ou sem informação

###---------------------------------------------------------------------###

datagbif <- function(sp_search = NULL, remove.badissues = TRUE, limite = 0, dgbif = data.frame(
  # Reestruturei o data frame para o formato de trabalho do CNCFlora, este é o formato de entrada no sistema de avaliação do Centro.
  # Poderia ter deixado a estrutura original e filtrado posteriormente, mas como tive que fazer outras alterações na função já fiz esta também.
  modified = NA, 
  institutionCode = NA, 
  collectionCode = NA, 
  catalogNumber= NA, 
  scientificName = NA, 
  identificationQualifier = NA, 
  family = NA, 
  genus = NA, 
  specificEpithet = NA,
  infraspecificEpithet = NA, 
  scientificNameAuthorship = NA,
  identifiedBy = NA, 
  dateIdentified = NA, 
  typeStatus = NA,
  recordNumber = NA, 
  fieldNumber = NA, 
  recordedBy = NA,
  year = NA, 
  month = NA,
  day = NA, 
  country = NA, 
  stateProvince = NA, 
  municipality = NA,
  locality = NA,
  decimalLatitude = NA, 
  decimalLongitude = NA, 
  occurrenceRemarks = NA,
  acceptedNameUsage = NA, 
  occurrenceID = NA, 
  comments = NA, 
  bibliographicCitation = NA
  
)){
  for (s in 1:NROW(sp_search)){
    
    dat.full <- dat.full.in <- datain <- {}
    
    #sp.name <- as.character(sp_search[s,1])
    sp.name <- as.character(sp_search[s])
    # Alterei de 'speciesKey' por 'usageKey'. Não foi encontrado metadados para o significado de cada chave do GBIF Backbone Taxonomy,
    # mas tivemos problemas com as ocorrências utilizando 'speciesKey', pois muitas vezes baixava ocorrências de sinônimos e não da 
    # espécie de interesse. Fizemos alguns testes com 'usageKey' e nos pareceu mais consistente com as espécies alvos. 
    speciesKey <- name_backbone(name=sp.name)$usageKey
    
    if (limite==0){
      if (length(speciesKey)>0){n.occ <- occ_count(speciesKey, basisOfRecord = 'PRESERVED_SPECIMEN')}
      else{n.occ <- 10}}
    else{n.occ <-limite}
    
    cat(' -> ',sp.name,' [key gbif ',speciesKey,'(',n.occ,') ]')
    
    # busca registros 
    # Alterei para procurar a ocorrência utilizando 'speciesKey' ao invés do nome da espécie. Pareceu ser mais consistente a busca de 
    # de ocorrências com a 'speciesKey'. 
    dat.full <- occ_search(taxonKey = speciesKey, limit = n.occ, basisOfRecord = 'PRESERVED_SPECIMEN')
    
    
    # dat.spocc <- occ(query = sp.name, from = source.data,limit = n.occ)
    #    if(NROW(dat.spocc$gbif$data[[1]])==0){cat(' (Sem Registros)')}
    #    else{
    #      dat.full <- as.data.frame(dat.spocc$gbif$data[[1]])
    
    #dat.full <- dat.full[!duplicated(dat.full),] 
    cat(' - (',NROW(dat.full$data),') encontrados ')
    
    # retira issues restritivos
    if (remove.badissues & NROW(dat.full$data) > 0){
      dat.full.in <- trata_issues(dat.full) 
      cat(' - (',NROW(dat.full.in$data),') sem issues ')}
    else{dat.full.in <-dat.full}
    
    if(NROW(dat.full.in$data)==0){cat(' (Sem Registros)')}
    else{
      datain <- as.data.frame(dat.full.in$data)
      name_fields <- colnames(datain)
      for (nlinha in 1:NROW(datain)){
        nlinhagbif=nrow(dgbif)+1
        for (ncoluna in 1:length(dgbif)){
          coluna = colnames(dgbif)[ncoluna]
          if (length(name_fields[name_fields %in% coluna])>0)
          {dgbif[nlinhagbif,ncoluna] = datain[nlinha,c(coluna)]}
        }
      }
    }
    cat(' - (',NROW(datain),') baixadas ')
  }
  return(dgbif[2:nrow(dgbif),])
}

###---------------------------------------------------------------------###

# Remove registros com bad issues

trata_issues <- function(dat.full){
  dat.full <- dat.full %>% occ_issues(mutate = "expand")
  
  # ver detalhes em http://gbif.github.io/gbif-api/apidocs/org/gbif/api/vocabulary/OccurrenceIssue.html
  dat.full.in <- dat.full %>% occ_issues(-ZERO_COORDINATE,
                                         -COORDINATE_OUT_OF_RANGE,
                                         -COORDINATE_INVALID,
                                         -GEODETIC_DATUM_INVALID,
                                         -COORDINATE_REPROJECTION_FAILED,
                                         -COORDINATE_ACCURACY_INVALID,
                                         -COORDINATE_PRECISION_INVALID,
                                         -COORDINATE_UNCERTAINTY_METERS_INVALID,
                                         -COUNTRY_INVALID,
                                         -CONTINENT_INVALID,
                                         -PRESUMED_SWAPPED_COORDINATE
                                         -RECORDED_DATE_INVALID,
                                         -PRESUMED_NEGATED_LONGITUDE,
                                         -PRESUMED_NEGATED_LATITUDE,
                                         -BASIS_OF_RECORD_INVALID)
  return(dat.full.in)}