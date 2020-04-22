###############################################################################
################# Leitura dos arquivos do excel e verificacao #################
# Mantem os nomes dos arquivos de origem na 1a coluna                         #
# (permite rastrear o dado)                                                   #
# Salva em um csv com todos os dados                                          #
###############################################################################
# versao do script e conjunto de dados. Evitar sobrescrever.
vrsN <- "v04-1"

# define diretorio de trabalho
# setwd('<your working diretory path>')


# Biblitecas a serem utilizadas
# install.packages('openxlsx')
library(openxlsx)

# nomes dos arquivos
# caminhoArqs <- "../DATASETs/"
caminhoArqs <- "../datasets_2019_03_23/"
arqs <- dir(caminhoArqs, pattern = "\\.xlsx")
arqsErr <- dir(caminhoArqs, pattern = "\\.xls")
arqsErr <- setdiff(arqsErr, arqs)

# arquivos que estao no formato errado
if(length(arqsErr)>0){
  warning(paste0(c("O(s) arquivo(s)\n", paste(arqsErr, sep = "\n"),
                 "\nesta(ao) no formato .xls e nao .xlsx"), collapse = "\n"))
}





###############################################################################
# leitura de todos os dados presentes nos arquivos e verificacao das colunas

# lista que vai armazenar os arquivos
listArqs <- vector("list", length = length(arqs))

# data frame para armazenar todos os arquivos
allFiles <- as.data.frame(matrix(nrow = 0, ncol = 56))
colnames(allFiles) <- c("file", "ordemdb", "Record_id", "Municipality", 
                        "State", "Country", "Veg_Landuse_type_point", 
                        "Veg_landuse_type_buffer5km", "Latitude_y", 
                        "Longitude_x", "Precision", 
                        "Regional_name_of_sampled_area", "site_name_id", 
                        "What_time", "Temperature", "Relative_humidity", 
                        "Plant_Order", "Plant_Family", "Plant_Genera", 
                        "Plant species_complete_name", "Plant_Type", 
                        "Plant_Habit", "Plant_flower_colour", 
                        "Plant_flower_shape", "Insect_Order", 
                        "Insect_Family", "Insect_Genera", 
                        "Insect_Species_complete_name", "Insect_Origins", 
                        "Insect_behavior", "Insect_collector_structure", 
                        "Campain_code", "Campain_Year_start", 
                        "Campain_Month_start", "Campain_Year_finish", 
                        "Campain_Month_finish", "Data_Interaction_Type", 
                        "Number_interactions", 
                        "Number_of_this_plant_interactions_onsite", 
                        "Number_of_this_invertebrate_interactions_onsite", 
                        "Sampling_method", 
                        "Sampling_method_of_data_collection", 
                        "Sampling_Method_detail", "Sampling_effort", 
                        "Sampling_effort_unit", "Sampling_effort_detail", 
                        "Plant_collected_material", "Plant_destination", 
                        "Plant_voucher", "Plant_identification_responsability",
                        "Insect_collected", "Insect_destination", 
                        "Insect_voucher", 
                        "Insect_identification_responsability", 
                        "Reference_citation", "OBS")
colnames(allFiles) <- tolower(colnames(allFiles))

# preenche a lista com os arquivos e cria um data.frame com todos os dados
for(i in 1:length(arqs)){
  # qual arquivo esta importando
  cat(paste0(sprintf("%02d\t", i), arqs[i]),"\n")
  ## importa cada artigo
  listArqs[[i]] <- read.xlsx(paste0(caminhoArqs,arqs[i]),
                             sheet = "Sheet_to_fill",
                             startRow = 1,  
                             detectDates = TRUE,
                             colNames = FALSE)
  # Ha variacoes em qual linha comeca a ter os dados
  aux <- which(tolower(listArqs[[i]][,1]) == "record_id" & 
                  tolower(listArqs[[i]][,2]) == "municipality")
  ## Primeira coluna deve ser 'record_id'
  # Se nao tem nenhuma linha com os nomes das colunas "Record_id" e "Municipality"
  # interrompe e indica qual arquivo nao segue a padronizacao
  if (length(aux) < 1) stop(paste0("Ha um problema no arquivo ", arqs[i],
                                    ":\nnomes das colunas fora do padrao"))
  colnames(listArqs[[i]]) <- listArqs[[i]][aux[1], ]
  listArqs[[i]] <- listArqs[[i]][(aux[1]+1):dim(listArqs[[i]])[1], ]
  ## VERIFICA se o arquivo tem 54 colunas
  # Se nao tem 54 colunas, interrompe e indica qual arquivo tem num col != 54
  if (dim(listArqs[[i]])[2] != 54) stop(paste0("Ha um problema no arquivo ", 
                                               arqs[i],
                                               ":\ntem ", 
                                               dim(listArqs[[i]])[2],
                                               " colunas"))
  # escreve os arquivos no formato que Miltinho usou 
#   write.csv(listArqs[[i]], paste0("___CSVs_complete/",
# 		gsub("\\.xls[a-z]*", ".txt", arqs[i]) ))
  ## concatena os arquivos em um data frame
  file <- arqs[i]
  aux <- tolower(colnames(listArqs[[i]]))
  colnames(listArqs[[i]]) <- aux
  ## VERIFICA se o nome das colunas nos arquivos estao correspondendo ao padrao
  if (all(aux != colnames(allFiles)[-c(1,2)] )) stop(paste0("Ha um problema no arquivo ", 
                                               arqs[i],
                                               ":\ncoluna ", 
                                               aux[aux == 
                                               colnames(allFiles)[-c(1,2)]],
                                               " nao corresponde ao padrao\n"))
  ## tira possiveis linhas exemplos que foram esquecidas de serem apagadas
  example <- data.frame(record_id = c("E0001","E0002","E0002"),
                        municipality = c("Iporanga","Alfenas","Curitiba"),
                        site_name_id = c("3p51","5p69","001ca"))
  # deve coincidir a definicao das 3 colunas 
  # (evitar remover dados q por coincidencia sao parecidos com o exemplo)
  # record_id
  aux <- union(grep(example$record_id[1], listArqs[[i]]$record_id),
               grep(example$record_id[2], listArqs[[i]]$record_id))
  aux <- union(aux, grep(example$record_id[3], listArqs[[i]]$record_id))
  # municipality
  aux2 <- union(grep(example$municipality[1], listArqs[[i]]$municipality),
                grep(example$municipality[2], listArqs[[i]]$municipality))
  aux2 <- union(aux2, grep(example$municipality[3], listArqs[[i]]$municipality))
  aux <- intersect(aux, aux2)
  # site_name_id
  aux2 <- union(grep(example$site_name_id[1], listArqs[[i]]$site_name_id),
                    grep(example$site_name_id[2], listArqs[[i]]$site_name_id))
  aux2 <- union(aux2, grep(example$site_name_id[3], listArqs[[i]]$site_name_id))
  aux <- intersect(aux, aux2)
  # remocao das linhas exemplo (qd ha)
  if(length(aux) > 0){
    listArqs[[i]] <- listArqs[[i]][-aux,]
  }
  ## cria um id unico para cada linha, com base no nome do arquivo
  ordemdb <- sub("ATFlowInvInt_V[0-9]_", "", file)
  ordemdb <- sub("_[0-9].*", "", ordemdb)
  ordemdb <- gsub(" ", "", ordemdb)
  ordemdb <- gsub(",", "_", ordemdb)
  ordemdb <- gsub("-", "", ordemdb)
  ordemdb <- gsub("\\.", "", ordemdb)
  ordemdb <- gsub("[a-z]", "", ordemdb)
  ordemdb <- gsub("__", "", ordemdb)
  ordemdb <- gsub("_$", "", ordemdb)
  ordemdb <- paste0(ordemdb, sprintf("_%04d", 1:nrow(listArqs[[i]])))
  listArqs[[i]] <- cbind(ordemdb, listArqs[[i]])
  ## adiciona coluna com o nome do arquivo
  allFiles <- rbind(allFiles, cbind(file, listArqs[[i]]))
}

# salva o dataframe com todos os dados dos arquivos em um csv
write.csv(allFiles, paste0("output/", vrsN, "_allFiles.csv"))




###############################################################################
################# Remocao de caracteres especiais dos arquivos ################
# Adaptacao do arquivo de Milton "script_chegagem_01_special_v07.txt          #
###############################################################################
# limpa as variaveis armazenadas na area de trabalho (workspace) (menos vrsN)
rm(list = setdiff(ls(), "vrsN"))
# arquivo gerado na etapa anterior
allFiles <- read.csv(paste0("output/", vrsN, "_allFiles.csv"), row.names = 1)

# string de caracteres especiais a serem substituidos
AccChars <- "ÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïðñòóôõöùúûüýÿ"
# string de caracteres para subtituir os especiais (string de mesmo compri/o)
RegChars <- "SZszYAAAAAACEEEEIIIIDNOOOOOUUUUYaaaaaaceeeeiiiidnooooouuuuyy"

# realiza a subtituicao
for(i in 2:56){ # menos na 1a coluna (nome dos arquivos)
  # retira acentuacao (utilizando as strings AccChars e RegChars)
  allFiles[,i] <- chartr(AccChars, RegChars, allFiles[,i])
  # demais substituicoes
  allFiles[,i] <- gsub("'", '', allFiles[,i]) # tira '
  allFiles[,i] <- gsub("Â´", '', allFiles[,i]) 
  allFiles[,i] <- gsub('"', '', allFiles[,i])
  allFiles[,i] <- gsub('`', '', allFiles[,i])
  allFiles[,i] <- gsub('Â°', '', allFiles[,i])
  allFiles[,i] <- gsub('\\#', '-', allFiles[,i]) # substitui # por -
  #allFiles[,i] <- gsub('\\(', '-', allFiles[,i]) # substituir parenteses? 
  #allFiles[,i] <- gsub('\\)', '-', allFiles[,i])
  allFiles[,i] <- gsub("\r", '', allFiles[,i]) # tira pulo de linha do windows
  allFiles[,i] <- gsub("\n", '', allFiles[,i]) # tira pulo de linha
  allFiles[,i] <- gsub("  ", '', allFiles[,i]) # tira espacos duplos  
  allFiles[,i] <- gsub(" $", '', allFiles[,i]) # tira espaco do final
  # se restar algum caractere especial, converte de codificacao retirando
  # caracteres sem correspondencia
  # [formato ASCII tem um num. limitado de bytes, por tanto, nao tem caracteres
  # especiais, tais como ideogramas, letra acentuadas...]
  allFiles[,i] <- iconv(allFiles[,i], "latin1", "ASCII", sub="")
}
# OBS: ao final dessas substituicoes, todas as colunas foram convertidas de 
# factor para character por coersao do chartr (o gsub tambem faz isso).

# salva os dados normalizados
write.csv(allFiles, paste0("output/", vrsN,
                           "_allFiles_without_special_char.csv"))




###############################################################################
########### Verificacao das colunas de coordenadas qt ao padrao ###############
# aponta e retira os NAs                                                      #
# encontra coordenadas sem pontuacao (registra quais arquivos ocorrem)        #
# coloca pontuacao na setima posicao antes do ultimo digito                   #
# Converte os dados de coordenadas para numerico                              #
# verifica erros nas coordenadas por arquivo                                  #
###############################################################################
# limpa as variaveis armazenadas na area de trabalho (workspace)
rm(list = setdiff(ls(), "vrsN"))
# le arquivo gerado na etapa anterior
# uso do colClasses = "character" p/ nao ler como factor as colunas com dados
allFiles <- read.csv(paste0("output/", vrsN,
                            "_allFiles_without_special_char.csv"),
                     row.names = 1,
                     colClasses = "character")
# converte o nome dos arquivos para factor
allFiles$file <- as.factor(allFiles$file)


# substitui , por .
allFiles$latitude_y <- gsub(",", ".", allFiles$latitude_y)
allFiles$longitude_x <- gsub(",", ".", allFiles$longitude_x)




###############################################################################
# encontra NA's nas coordenadas ###############################################
temp <- table(allFiles$file[is.na(allFiles$latitude_y) | 
                              is.na(allFiles$longitude_x)])
# tabela com quais arquivos: nome arq | num de coord com NA | ttl de entradas
temp <- as.data.frame(temp) # arquivos e qtdade de NAs
temp <- cbind(temp, as.data.frame(table(allFiles$file))[,2]) # ttl de entradas
colnames(temp) <- c("file","num_NA","length")
temp <- temp[temp$num_NA != 0,]
# salva um arquivo com dados dos arquivos com NA nas coordenadas
write.table(temp, paste0("output/", vrsN, "_arq_with_NA_coordinate.txt"))
warning(paste0(c("O(s) arquivo(s):", paste(temp$file, sep = "\n"),
               "apreseta(m) NAs nas coordenadas."), collapse = "\n"))
# arquivo com as linhas com NAs
write.csv(allFiles[(is.na(allFiles$latitude_y) | 
                       is.na(allFiles$longitude_x)), ], 
          paste0("output/", vrsN, "_allFiles_coord_NA.csv"))
### retiro as linhas que estao com NA nas coordenadas, armazeno na var rowNAs
rowNAs <- allFiles[(is.na(allFiles$latitude_y) | 
                       is.na(allFiles$longitude_x)), ]
allFiles <- allFiles[!(is.na(allFiles$latitude_y) | 
                         is.na(allFiles$longitude_x)), ]




###############################################################################
# encontra as coordenadas sem pontuacao decimal ###############################
temp <- allFiles$file[!(grepl("\\.", allFiles$latitude_y) |
                          grepl("\\.", allFiles$latitude_y))]
# tabela com quais arquivos: nome arq | num de coord nao dec | ttl de entradas
temp <- as.data.frame(table(temp)) # arquivos e qtdade de coord nao decimais
temp <- cbind(temp, as.data.frame(table(allFiles$file))[,2]) # ttl de entradas
colnames(temp) <- c("file","wt_decimal","length")
temp <- temp[temp$wt_decimal != 0,]
# salva um arquivo com dados dos arquivos com coordenadas nao decimais
write.table(temp, paste0("output/", vrsN, 
                         "_arq_without_decimal_coordinate.txt"))
warning(paste(c("O(s) arquivo(s):", paste(temp$file, sep = "\n"),
               "nao apreseta(m) coordenadas decimais."), collapse = "\n"))
# colocar "." deixando dd.dddddd ou -dd.dddddd
# para dados -dddddddd para -dd.dddddd
#### latitude
# seleciona as linha que nao tem "." na latitude
temp <- c(1:length(allFiles$latitude_y))[!grepl("\\.", allFiles$latitude_y)]
# coloca ponto depois do 2o digito numerico
for(i in temp){
  # posicao do ultimo digito
  aux <- nchar(allFiles$latitude_y[i]) 
  # posicao 2o digito numerico (ve se tem "-" e mais q 2 digitos)
  aux1 <- grep("-[0-9][0-9][0-9]", allFiles$latitude_y[i]) + 2 # 3a posicao
  # posicao 2o digito numerico (para coordenadas sem "-" e com mais de 2 dig)
  if(length(aux1) == 0 & !grepl("-", allFiles$latitude_y[i]) & aux > 2){
    aux1 <- 2 # 2a posicao da string
  }
  if(length(aux1) > 0){ # para dados q tem onde add o pto 
  allFiles$latitude_y[i] <- paste(substr(allFiles$latitude_y[i], 0, aux1), 
                                  substr(allFiles$latitude_y[i], aux1+1, aux), 
                                  sep = ".")
  }
}
### longitude
# seleciona as linha que nao tem "." na latitude
temp <- c(1:length(allFiles$longitude_x))[!grepl("\\.", allFiles$longitude_x)]
# coloca ponto 6 posicoes antes do ultimo digito
for(i in temp){
  # posicao do ultimo digito
  aux <- nchar(allFiles$longitude_x[i]) 
  # posicao 2o digito numerico (ve se tem "-" e mais q 2 digitos)
  aux1 <- grep("-[0-9][0-9][0-9]", allFiles$longitude_x[i]) + 2 # 3a posicao
  # posicao 2o digito numerico (para coordenadas sem "-" e com mais de 2 dig)
  if(length(aux1) == 0 & !grepl("-", allFiles$longitude_x[i]) & aux > 2){
    aux1 <- 2 # 2a posicao da string
  }
  if(length(aux1) > 0){ # pode ser q os dados so nao sejam precisos (ex.: -13)
  allFiles$longitude_x[i] <- paste(substr(allFiles$longitude_x[i], 0, aux1), 
                                  substr(allFiles$longitude_x[i], aux1+1, aux), 
                                  sep = ".")
  }
}




###############################################################################
# transforma em numerico ######################################################
allFiles$latitude_y <- as.numeric(allFiles$latitude_y)
allFiles$longitude_x <- as.numeric(allFiles$longitude_x)

# salva os dados com as coordenadas corrigidas
write.csv(rbind(allFiles, rowNAs), # add as linhas com NAs nas coordenadas 
          paste0("output/", vrsN, "_allFiles_coord_ok.csv"))




###############################################################################
##### Tabela de termos por coluna: um arquivo para cada coluna de termos ######
# - glossario para padronizar as descricoes colocadas na tabela               #
#   utilizar para "traduzir" os termos para um padrao comum, deixando o banco #
#   dados mais conciso                                                        #
# - colunas que nao precisam passar por essa padronizacao:                    #
#     file (1), ordembd (2), record_id (3), site_name_id (13),                #
#     campain_code (32), latitude (9), longitude (10) e                       #
#     taxonomias (17 a 20 e 25 a 28)                                          #
# - Tabela para conferir tera 3 colunas:                                      #
#   <nome da coluna> | qtidade de vezes que o termo aparece | padronizacao    #
###############################################################################

# **** OBSERVACAO: ja foram gerados os arquivos (por isso o trecho td coment)

# # limpa as variaveis armazenadas na area de trabalho (workspace)
# rm(list = setdiff(ls(), "vrsN"))
# # le arquivo gerado na etapa anterior
# # uso do stringsAsFactors = FALSE p/ nao ler como factor, mas num serem numeric
# allFiles <- read.csv(paste0("output/", vrsN, "_allFiles_coord_ok.csv"),
#                      row.names = 1,
#                      stringsAsFactors = FALSE)
# # converte o nome dos arquivos para factor
# allFiles$file <- as.factor(allFiles$file)
# # biblioteca para lidar com xlsx
# library(openxlsx)
# 
# ### altera para a versao anterior dos termos
# # selecionar apenas os termos que ainda não foram padronizados
# caminhosb <- "../padronizacao_termos/xlxs_NAO MEXER/"
# arqsb <- dir(caminhosb, pattern = "col[0-9].*.xlsx")
# # lista para armazenar cada arquivo de padronizacao dos termos anterior
# lstTermOld <- vector("list", length = length(arqsb))
# for (i in 1:length(arqsb)){
#   temp <- read.xlsx(paste0(caminhosb,arqsb[i]))
#   lstTermOld[[i]] <- temp[, 1]
# }
# 
# # numero identificador das colunas de interesse
# temp <- c(1:3,9,10,13,17:20,25:28,32) # <colunas nao padronizaveis>
# temp <- c(temp, 44:46) # 3 colunas "Sampling_effort" terao processamento diferente
# temp <- setdiff(1:56, temp) # <tds as colunas> - <colunas nao padronizaveis>
# # um arquivo para cada termo
# j <- 1
# for(i in temp){
#   aux <- as.data.frame(table(allFiles[,i]), stringsAsFactors = F)
#   aux <- aux[!is.element(aux[,1],lstTermOld[[j]]) ,]
#   if (nrow(aux) > 0){
#     aux <- cbind(aux, "")
#     colnames(aux) <- c(colnames(allFiles)[i], "frequence", "new")
#     write.xlsx(aux, paste0("../__afericao_termos/v02_col",
#                            sprintf("%02d_", i), colnames(allFiles)[i],
#                            sprintf("_%02dterms", nrow(aux)),".xlsx"))
#   }
#   j <- j + 1
# }
# ### 3 colunas "Sampling_effort"
# caminhosb <- "../padronizacao_termos/_col44-45-46/"
# arqsb <- dir(caminhosb, pattern = "_col[0-9].*orig.xlsx")
# # data frame para armazenar o que ja tem dessas 3 colunas
# lstTermOld <- read.xlsx(paste0(caminhosb,arqsb))
# lstTermOld <- unique(lstTermOld[-1,1:3])
# a <- vector("character", length = nrow(lstTermOld))
# for(i in 1:nrow(lstTermOld)){
#   a[i] <- paste(lstTermOld[i,], collapse = " | ")
# }
# # essas tres colunas na versao atual
# aux <- unique(allFiles[,44:46])
# b <- vector("character", length = nrow(aux))
# for(i in 1:nrow(aux)){
#   b[i] <- paste(aux[i,], collapse = " | ")
# }
# temp <- aux[!is.element(b,a), ]
# write.xlsx(temp, paste0("../__afericao_termos/v02_col44-45-46",
#                        sprintf("_%02dterms", nrow(temp)),".xlsx"))

###############################################################################
########### Verificacao das coordenadas #######################################
# Adaptacao do arquivo de Milton "script_chegagem_02_maps_Latlong_v05.txt     #
# plota as coordenadas nos mapas do brasil e mata atlantica                   #
###############################################################################
# limpa as variaveis armazenadas na area de trabalho (workspace)
rm(list = setdiff(ls(), "vrsN"))
# le arquivo gerado na etapa anterior
# uso do stringsAsFactors = FALSE p/ nao ler como factor, mas num serem numeric
allFiles <- read.csv(paste0("output/", vrsN, "_allFiles_coord_ok.csv"),
                     row.names = 1,
                     stringsAsFactors = FALSE)
# converte o nome dos arquivos para factor
allFiles$file <- as.factor(allFiles$file)

# biblioteca necessaria
# install.packages("maptools")
library(maptools)
library(graphics)

### retiro as linhas que estao com NA nas coordenadas
allFiles <- allFiles[!(is.na(allFiles$latitude_y) | 
                         is.na(allFiles$longitude_x)), ]


###############################################################################
# analisa se as coordenadas correspondem a area de distribuicao da 
# Mata Atlantica. Busca detectar problemas de ponto q caem em alto mar ou 
# com inversao entre latitude e longitude

# carrega o mata da mata atlantica, estados brasileiros
MataAtlantica <- readShapeSpatial("../Brazil_estados/Atlantic_MMA_miltinho_LEI_WWF_INTERNATIONAL_albers_sad69_LATLONG_WGS84.shp")
BrState <- readShapeSpatial("../Brazil_estados/Brazilian_States_Limits.shp")
amSul <- readShapeSpatial("../Brazil_estados/Lowenberg_Neto_2014.shp")

par(mar=c(3,3,2,0), lend = 0)
png(paste0("../__afericao_geograficas/_", vrsN, "todos_os_dados.png"), 1000,900)
plot(amSul, asp=1, 
     main=paste("ATLANTIC : Plant-Invertebrate Interactions", 
                nrow(allFiles),"Records"), 
     usePolypath = FALSE, cex.main = 2.5,
     col = "gray75", bor = "white", bg = "lightskyblue",
     xlim = c(-84,-30), ylim = c(-56,12))
plot(MataAtlantica, asp=1, usePolypath = FALSE, add = T, 
     col = "olivedrab2", bor = NA)
plot(BrState, asp=1, usePolypath = FALSE, add = T, 
     bor = "gray48")
axis(1); axis(2)
points(latitude_y~longitude_x, data=allFiles, pch=19, 
       col="darkorchid3", cex=2)
points(longitude_x~latitude_y, data=allFiles, pch=16, 
       col="blue", cex=1.5)
# Localizar entradas na base de dados que fogem da distribuicao
# Y: a latitude na mata atlantica varia de -40 a 0
# X: o longitude na mata atlantica varia de -60 a -30
dataOut <- allFiles[allFiles$longitude_x < -70 | allFiles$longitude_x > -30 |
                      allFiles$latitude_y < -40 | allFiles$latitude_y > 0, ]
validCoord <- allFiles[allFiles$longitude_x > -70 & allFiles$longitude_x < -30 &
                      allFiles$latitude_y > -40 & allFiles$latitude_y < 0, ]
points(latitude_y~longitude_x, data=dataOut, pch=4, 
       col="red", cex=3)
par( lend = 1)
legend("bottomright", cex = 1.5, pt.cex = c(2, 1.5, 3), 
       pch= c(19,16,4, NA, NA, NA), 
       col= c("darkorchid3","blue","red", "olivedrab2", "gray75", "gray48"), 
       lwd  = c(NA, NA, NA, 15, 15, 2),
       lty  = c(NA, NA, NA, 1, 1, 1),
       seg.len = c(1, 1, 1, 2, 2, 2),
       legend = c("coordinates\n(lat~lon coord)\n",
                 "lon~lat coord\n", 
                  paste0("out of Atlantic Forest domain\n(", 
                         nrow(dataOut), ")\n"),
                  "Atlantic Forest biome\n",
                  "biogeographical regions\n",
                  "brazilian states") )
dev.off()

# salva as coordenadas validas
write.csv(validCoord, paste0("output/", vrsN, 
                             "_allFiles_coordmap_validCoord.csv"))
# salva as coordenadas invalidas
write.csv(dataOut, paste0("output/", vrsN,
                          "_allFiles_coordmap_invalidCoord.csv"))




###############################################################################
# plota o mapa de distribuicao dos dados para cada conjunto de dados

# qtdade de coordenadas unicas
tam <- nrow(unique(allFiles[,9:10]))
# mapa de distribuicao dos dados para cada conjunto de dados
for (planilha in sort(unique(allFiles$file))) {
  # cria uma area de plotagem com 2 graficos
  png(paste0("../__afericao_geograficas/", vrsN, "_", 
             gsub(".xlsx",".png", planilha)),
      1000,600)	
  par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
  aux <- subset(allFiles, file == planilha)
  # plota o grafico da Mata Atlantica inteira, destacando em laranja os ptos
  # do dataset especifico
  plot(amSul, asp=1, main="", #xlim = c(-70,-30), ylim = c(-35,0),
       usePolypath = FALSE, col = "gray75", bor = "white", bg = "lightskyblue")
  plot(MataAtlantica, asp=1, main=planilha, 
       usePolypath = FALSE, col = "olivedrab2", bor = NA, add = T)
  plot(BrState, asp=1, main=planilha, 
       usePolypath = FALSE, bor = "gray48", add = T)
  axis(1); axis(2)
  points(latitude_y~longitude_x, data=allFiles, pch=19, 
         col="darkorchid3", cex=1.5)
  points(latitude_y~longitude_x, data=aux, pch=16, col="orange", cex=2.5)
  legend("bottomleft", cex = 1, pt.cex = c(1.5, 2.5), 
         pch= c(19,16), 
         col= c("darkorchid3","orange"), 
         legend = c(paste0("all coordinates: ",
                           nrow(allFiles[,9:10]),"\n"),
                    paste0("this file coordinates: ",
                           nrow(aux[,9:10]),"\n")) )
  title(main = paste0("num. of all coordinates: ", tam, 
                      " (without repetition)"))
  # plota o grafico da Mata Atlantica referente aos ptos do dataset especifico
  plot(amSul, asp=1, main="", 
       xlim=range(aux$longitude_x, na.rm=T), 
       ylim=range(aux$latitude_y, na.rm=T),
       usePolypath = FALSE, col = "gray75", bor = "white", bg = "lightskyblue")
  plot(MataAtlantica, asp=1, main=planilha, 
       usePolypath = FALSE, col = "olivedrab2", bor = NA, add = T)
  plot(BrState, asp=1, main=planilha, 
       usePolypath = FALSE, bor = "gray48", add = T)
  axis(1); axis(2)
  points(latitude_y~longitude_x, data=aux, pch=19, col="orange", cex=2.5)
  title(main = paste0("num. of coordinates in this file: ", 
                      nrow(unique(aux[,9:10])), " (without repetition)"))
  mtext(planilha, outer = TRUE, cex = 2)
  dev.off()
}



# ******* PRECISA VERIFICAR A TAXONOMIA ANTES DAS PROXIMAS ETAPAS




###############################################################################
# plota o mapa de distribuicao dos dados para cada taxon (sp e ge)
# plota graficos de distribuicao de sp e ge para ver inconsistencias          #




###############################################################################  
######## graficos apresentando a diversidade taxonomica dos arquivos ##########
## grafico de barras de diversidade                                           #
# tem mais de um mesmo nome de especie pq as classificacoes em familia ou     #
# superior divergem (ex. mesma especie, mas alocada em familias diferentes)   #
###############################################################################
# Planta: diversidade taxons
planTx <- cbind(validCoord$plant_order, validCoord$plant_family,
                validCoord$plant_genera, 
                validCoord$plant.species_complete_name)
planTx <- unique(tolower(planTx)) # tira repeticoes
planTx[is.na(planTx)] <- "" # passa NA para "" p permitir calculos
planTx <- as.data.frame(planTx)
#planTx <- planTx[planTx[,4] != "", ] # tiro especies ""
colnames(planTx) <- c("Order", "family", "genus", "species")

# Animais: diversidade taxons
animTx <- cbind(validCoord$insect_order, validCoord$insect_family,
                validCoord$insect_genera, 
                validCoord$insect_species_complete_name)
animTx <- unique(tolower(animTx))
animTx[is.na(animTx)] <- "" # passa NA para "" p permitir calculos
animTx[animTx == "(vazio)"] <- "" # teve alguns taxons definidos c/o "(vazio)"
animTx <- as.data.frame(animTx)
#animTx <- animTx[animTx[,4] != "", ] # tiro especies ""
colnames(animTx) <- c("Order", "family", "genus", "species")

tiff(paste0("output/", vrsN, "_diversityTaxon.tif"),
     width = 174,height = 233,units = "mm",res = 300)
par(mfcol= c(4,2), mar= c(2,8,1.8,1)+0.1, mgp= c(1,1,0))
# plantas
for (i in 1:4){
  temp <- unique(planTx[ ,i])
  soma <- vector(mode = "numeric", length = length(temp))
  for (j in 1:length(temp)){
    soma[j] <- sum(planTx[ ,i] == temp[j])
  }
  soma <- data.frame(name = temp[order(soma, decreasing =T)], 
                     sum = soma[order(soma, decreasing =T)])
  soma$name <- as.character(soma$name)
  aux <- which(nchar(soma$name) == 0)
  if(i == 1){
    barplot(soma[setdiff(25:1,aux),2], names.arg = soma[setdiff(25:1,aux),1],
            las = 1, horiz = T,
            beside = T, col = "#008033", border = NA,
            main = paste0("Diversity of animal species by\n ", colnames(animTx)[i],
                          " - total of ",length(soma[,1]), " (" , soma[aux,2],
                          " com NA)"))
  }else{
    barplot(soma[setdiff(25:1,aux),2], names.arg = soma[setdiff(25:1,aux),1],
            las = 1, horiz = T,
            beside = T, col = "#008033", border = NA,
            main = paste0(colnames(planTx)[i],
                          " - total of ",length(soma[,1]), " (" , soma[1,2],
                          " com NA)"))
  }
}
# animais
for (i in 1:4){
  temp <- unique(animTx[ ,i])
  soma <- vector(mode = "numeric", length = length(temp))
  for (j in 1:length(temp)){
    soma[j] <- sum(animTx[ ,i] == temp[j])
  }
  soma <- data.frame(name = temp[order(soma, decreasing =T)], 
                     sum = soma[order(soma, decreasing =T)])
  soma$name <- as.character(soma$name)
  aux <- which(nchar(soma$name) == 0)
  if(i == 1){
    barplot(soma[setdiff(25:1,aux),2], names.arg = soma[setdiff(25:1,aux),1], 
            las = 1, horiz = T,
            beside = T, col = "#ff2a2a", border = NA,
            main = paste0("Diversity of animal species by\n ", colnames(animTx)[i],
                          " - total of ",length(soma[,1]), " (" , soma[aux,2],
                          " com NA)"))
  }else{
    barplot(soma[setdiff(25:1,aux),2], names.arg = soma[setdiff(25:1,aux),1], 
            las = 1, horiz = T,
            beside = T, col = "#ff2a2a", border = NA,
            main = paste0(colnames(animTx)[i],
                          " - total of ",length(soma[,1]), " (" , soma[aux,2],
                          " com NA)"))
  }
}
par(mfcol= c(1,1), mar= c(5,4,4,2)+0.1, mgp= c(3,1,0))
dev.off()







