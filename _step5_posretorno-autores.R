###############################################################################
################# Leitura dos arquivos revisados pelos autores ################
# Mantem os nomes dos arquivos de origem na 1a coluna                         #
# (permite rastrear o dado)                                                   #
# Salva em um csv e xlsx com todos os dados                                   #
###############################################################################
# versao do script e conjunto de dados. Evitar sobrescrever.
vrsN <- "v03"

# define diretorio de trabalho
setwd("<your working diretory path>")

# Biblitecas a serem utilizadas
# install.packages('openxlsx')
library(openxlsx)

# nomes dos arquivos
# caminhoArqs <- "../DATASETs/"
# caminhoArqs <- "F:/GoogleDrive/Atlantic Plant-Invertebrate Interactions/DATASETs/"
caminhoArqs <- "I:/coisasPessoais/Doutorado/DataPaper/1o_release_2019_03/datasets_2019_12_05/"
arqs <- dir(caminhoArqs, pattern = "\\.xlsx")
# remove arquivos temp do excel (ex. "~$ATFlowInvInt_V1...")
temp <- grep("\\~\\$",arqs)
if(length(temp) > 0) arqs <- arqs[-temp]
# arquivos no formato xls (que nao podem ser lidos com openxlsx)
arqsErr <- dir(caminhoArqs, pattern = "\\.xls$")

# arquivos que estao no formato errado
if(length(arqsErr)>0){
  warning(paste0(c("O(s) arquivo(s)\n", paste(arqsErr, sep = "\n"),
                   "\nesta(ao) no formato .xls e nao .xlsx"), collapse = "\n"))
}





###############################################################################
# leitura de todos os dados presentes nos arquivos para unir em um arquivo

# lista que vai armazenar os arquivos
listArqs <- vector("list", length = length(arqs))

# data frame para armazenar todos os arquivos
allFiles <- as.data.frame(matrix(nrow = 0, ncol = 61))
colnames(allFiles) <- c("file",
                        "ordemdb",
                        "record_id",
                        "municipality",
                        "state",
                        "country",
                        "veg_landuse_type_point",
                        "veg_landuse_type_buffer5km",
                        "latitude_y",
                        "longitude_x",
                        "precision",
                        "regional_name_of_sampled_area",
                        "site_name_id",
                        "what_time",
                        "temperature",
                        "relative_humidity",
                        "plant_order",
                        "plant_family",
                        "plant_genera",
                        "plant_species",
                        "plant_species_complete_name",  # col add
                        "plant_AuthorSp",
                        "plant_type",
                        "plant_habit",
                        "plant_flower_colour",
                        "plant_flower_shape",
                        "insect_order",
                        "insect_family",
                        "insect_genera",
                        "insect_species",
                        "insect_species_complete_name",  # col add
                        "insect_AuthorSp",
                        "insect_origins",
                        "insect_behavior",
                        "insect_collector_structure",
                        "campain_code",
                        "campain_year_start",
                        "campain_month_start",
                        "campain_year_finish",
                        "campain_month_finish",
                        "data_interaction_type",
                        "number_interactions",
                        "number_of_this_plant_interactions_onsite",
                        "number_of_this_invertebrate_interactions_onsite",
                        "sampling_method",
                        "sampling_method_of_data_collection",
                        "sampling_method_detail",
                        "sampling_effort",
                        "sampling_effort_unit",
                        "sampling_effort_detail",
                        "sampling_effort_hours", # col add
                        "plant_collected_material",
                        "plant_destination",
                        "plant_voucher",
                        "plant_identification_responsability",
                        "insect_collected",
                        "insect_destination",
                        "insect_voucher",
                        "insect_identification_responsability",
                        "reference_citation",
                        "obs")
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
  aux <- which(tolower(listArqs[[i]][,2]) == "record_id" & 
                 tolower(listArqs[[i]][,3]) == "municipality")
  ## Primeira coluna deve ser 'record_id'
  # Se nao tem nenhuma linha com os nomes das colunas "Record_id" e "Municipality"
  # interrompe e indica qual arquivo nao segue a padronizacao
  if (length(aux) < 1) stop(paste0("Ha um problema no arquivo ", arqs[i],
                                   ":\nnomes das colunas fora do padrao"))
  colnames(listArqs[[i]]) <- listArqs[[i]][aux[1], ]
  listArqs[[i]] <- listArqs[[i]][(aux[1]+1):dim(listArqs[[i]])[1], ]
  ## VERIFICA se o arquivo tem 60 colunas
  # Se nao tem 54 colunas, interrompe e indica qual arquivo tem num col != 54
  if (dim(listArqs[[i]])[2] != 60) stop(paste0("Ha um problema no arquivo ", 
                                               arqs[i],
                                               ":\ntem ", 
                                               dim(listArqs[[i]])[2],
                                               " colunas"))
  ## concatena os arquivos em um data frame
  file <- arqs[i]
  aux <- tolower(colnames(listArqs[[i]]))
  colnames(listArqs[[i]]) <- aux
  ## VERIFICA se o nome das colunas nos arquivos estao correspondendo ao padrao
  temp <- apply(cbind(aux,colnames(allFiles)[-1]), 1, function(x) x[1] == x[2])
  if (!all(temp)) stop(paste0("Ha um problema no arquivo ",
                              arqs[i],
                              ":\ncoluna ", (1:60)[!temp], " = ",
                              aux[!temp], " (", colnames(allFiles)[-1][!temp], ")",
                              " nao corresponde ao padrao\n"))

  ## adiciona coluna com o nome do arquivo
  allFiles <- rbind.data.frame(allFiles, cbind(file,listArqs[[i]]))
}






###############################################################################
# Remocao de caracteres especiais dos arquivos ################

# Adaptacao do arquivo de Milton "script_chegagem_01_special_v07.txt          #
# string de caracteres especiais a serem substituidos
AccChars <- "ÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïðñòóôõöùúûüýÿ"
# string de caracteres para subtituir os especiais (string de mesmo compri/o)
RegChars <- "AAAAAACEEEEIIIIDNOOOOOUUUUYaaaaaaceeeeiiiidnooooouuuuyy"

for(i in 3:61){ # menos na 1a coluna (nome dos arquivos)
  # retira acentuacao (utilizando as strings AccChars e RegChars)
  allFiles[,i] <- chartr(AccChars, RegChars, allFiles[,i])
  # se restar algum caractere especial, converte de codificacao retirando
  # caracteres sem correspondencia
  # [formato ASCII tem um num. limitado de bytes, por tanto, nao tem caracteres
  # especiais, tais como ideogramas, letra acentuadas...]
  allFiles[,i] <- iconv(allFiles[,i], "latin1", "ASCII", sub="")
}
# fim da parte adaptada do Milton
  
# realiza a subtituicao
for(i in 3:61){ # menos na 1a coluna (nome dos arquivos)
  # retira acentuacao (utilizando as strings AccChars e RegChars)
  allFiles[,i] <- chartr(AccChars, RegChars, allFiles[,i])
  # demais substituicoes
  allFiles[,i] <- gsub("'", '', allFiles[,i]) # tira '
  allFiles[,i] <- gsub('\\#', '-', allFiles[,i]) # substitui # por -
  #allFiles[,i] <- gsub('\\(', '-', allFiles[,i]) # substituir parenteses? 
  #allFiles[,i] <- gsub('\\)', '-', allFiles[,i])
  allFiles[,i] <- gsub("\r", '', allFiles[,i]) # tira pulo de linha do windows
  allFiles[,i] <- gsub("\n", '', allFiles[,i]) # tira pulo de linha
  allFiles[,i] <- gsub("  ", '', allFiles[,i]) # tira espacos duplos  
  allFiles[,i] <- gsub(" $", '', allFiles[,i]) # tira espaco do final
  allFiles[,i] <- gsub("^ ", '', allFiles[,i]) # tira espaco do comeco
}




###############################################################################
# salva o dataframe com todos os dados dos arquivos em um csv
write.csv(allFiles, paste0("output/_rev_", vrsN, "_allFiles.csv"))
write.xlsx(allFiles, paste0("output/_rev_", vrsN, "_allFiles.xlsx"))








###############################################################################
# leitura de todos os dados de contato para unir em um arquivo

# lista que vai armazenar os arquivos
listCoaut <- vector("list", length = length(arqs))
# data frame para armazenar todos os arquivos
allCoauthors <- as.data.frame(matrix(nrow = 0, ncol = 10))

# preenche a lista com os arquivos e cria um data.frame com todos os dados
for(i in 1:length(arqs)){
  # qual arquivo esta importando
  cat(paste0(sprintf("%02d\t", i), arqs[i]),"\n")
  ## importa cada artigo
  listCoaut[[i]] <- read.xlsx(paste0(caminhoArqs,arqs[i]),
                              sheet = "Co-authorship",
                              startRow = 1,
                              detectDates = FALSE,
                              colNames = FALSE)[,-1]
  # Ha variacoes em qual linha comeca a ter os dados
  aux <- which(tolower(listCoaut[[i]][,2]) == "author_name" & 
                 tolower(listCoaut[[i]][,3]) == "contact_person_for_this_dataset?")
  ## Primeira coluna deve ser 'author_name (include on author per line)'
  # Se nao tem nenhuma linha com os nomes das colunas
  # interrompe e indica qual arquivo nao segue a padronizacao
  if (length(aux) < 1) stop(paste0("Ha um problema no arquivo ", arqs[i],
                                   ":\nnomes das colunas fora do padrao"))
  listCoaut[[i]] <- listCoaut[[i]][(aux[1]+1):nrow(listCoaut[[i]]), ]
  ## VERIFICA se o arquivo tem 10 colunas
  # Se nao tem 10 colunas, interrompe e indica qual arquivo tem num col != 10
  if (ncol(listCoaut[[i]]) != 11) stop(paste0("Ha um problema no arquivo ", 
                                                arqs[i],
                                                ":\ntem ", 
                                                dim(listCoaut[[i]])[2],
                                                " colunas"))
  ## concatena os arquivos em um data frame
  file <- arqs[i]
  ## adiciona coluna com o nome do arquivo
  allCoauthors <- rbind(allCoauthors, cbind(file, listCoaut[[i]]))
}

colnames(allCoauthors)[2:12] <- c("ordemdb","author_name",
                                  "contact_person_for_this_dataset?","e-mail",
                                  "orcid","cpf only for brazilian",
                                  "filiation_1","filiation_2","filiation_3",
                                  "funding","acknowledgements")
# padronizar o orcid
allCoauthors$orcid <- gsub("http\\:\\/\\/","",allCoauthors$orcid)
allCoauthors$orcid <- gsub("https\\:\\/\\/","",allCoauthors$orcid)
allCoauthors$orcid <- gsub("^orcid.org/", "", allCoauthors$orcid)
allCoauthors$orcid <- gsub("\\.", "", allCoauthors$orcid)
allCoauthors$orcid <- gsub("^","orcid.org/",allCoauthors$orcid)
allCoauthors$orcid <- gsub("orcidorg\\/","",allCoauthors$orcid)

# # padronizar o cpf
# aux <- allCoauthors$`cpf only for brazilian`
# aux <- gsub("\\.","", aux)
# aux <- gsub("-","", aux)
# # coloca "." e "-" no cpf
# for(i in 1:length(aux)){
#   aux[i] <- paste(substr(allFiles$longitude_x[i], 0, aux1), 
#                                    substr(allFiles$longitude_x[i], aux1+1, aux), 
#                                    sep = ".")
# }
# allCoauthors$`cpf only for brazilian` <- aux 

#### adiciona uma coluna com para dizer se já temos o e-mail
# tira eventuais espacos no final ou comeco do nome do e-mail
allCoauthors$`e-mail` <- gsub(" $", "", allCoauthors$`e-mail`)
allCoauthors$`e-mail` <- gsub("^ ", "", allCoauthors$`e-mail`)

# salva o dataframe com todos os dados dos arquivos em um csv
write.csv(allCoauthors, paste0("output/_rev_", vrsN, "_allCoauthorship.csv"))

# # salva o dataframe com todos os coauthorship dos arquivos em um xlsx
# write.xlsx(list(allCoauthors, unique(allCoauthors[,3:5])),
#            paste0("output/_rev_", vrsN, "_allCoauthorship.xlsx"), 
#            sheetName = c("all","NameEmail"))





























###############################################################################
# IMPORTA OS DADOS PARA FAZER AS ANALISES ABAIXO sem precisar rodar o inicio 
# do script

# allFiles <- read.xlsx("F:/GoogleDrive/Atlantic Plant-Invertebrate Interactions/posRetornoAutores/_rev_v02_allFiles.xlsx")
###############################################################################

















###############################################################################
# verificacao das coordenadas
###############################################################################
# encontra as coordenadas sem pontuacao decimal ###############################
temp <- allFiles$file[!(grepl("\\.", allFiles$latitude_y) |
                          grepl("\\.", allFiles$latitude_y))]
# tabela com quais arquivos: nome arq | num de coord nao dec | ttl de entradas
temp <- as.data.frame(table(temp)) # arquivos e qtdade de coord nao decimais
temp <- cbind(temp, as.data.frame(table(allFiles$file))[,2]) # ttl de entradas
colnames(temp) <- c("file","wt_decimal","length")
temp <- temp[temp$wt_decimal != 0,]

warning(paste(c("O(s) arquivo(s):", paste(temp$file, sep = "\n"),
                "nao apreseta(m) coordenadas decimais."), collapse = "\n"))
# salva um arquivo com dados dos arquivos com coordenadas nao decimais
if (nrow(temp) > 0){
  write.table(temp, paste0("output/_rev_", vrsN, 
                           "_arq_without_decimal_coordinate_",
                           format(Sys.time(), "_%Y-%m-d%d"),"-",
                           gsub(":", "", format(Sys.time(), "%X") ),".txt"))
}
# verificacao manual para detectar quais sao os problemas
# i <- 6 # 1 ate o num de linhas de temp (arquivos c/ problemas
# a <- allFiles[allFiles$file == temp$file[i],]
# a <- a[, c("latitude_y","longitude_x")]
# unique(a)
# a[!(grepl("\\.", a$latitude_y) |
#       grepl("\\.", a$latitude_y)),]
# temp$file[i]


# transforma em numerico ######################################################
allFiles$latitude_y <- as.numeric(allFiles$latitude_y)
allFiles$longitude_x <- as.numeric(allFiles$longitude_x)


# biblioteca necessarias
# install.packages("rgeos")
library(rgeos)
# install.packages("rgdal")
library(rgdal)
# plota as coordenadas nos mapas do brasil e mata atlantica ###################

###############################################################################
# analisa se as coordenadas correspondem a area de distribuicao da 
# Mata Atlantica. Busca detectar problemas de ponto q caem em alto mar

# carrega o mata da mata atlantica, estados brasileiros
MataAtlantica <- readOGR(dsn="../Brazil_estados/Atlantic_MMA_miltinho_LEI_WWF_INTERNATIONAL_albers_sad69_LATLONG_WGS84.shp")
BrState <- readOGR(dsn="../Brazil_estados/Brazilian_States_Limits.shp")
amSul <- readOGR(dsn="../Brazil_estados/Lowenberg_Neto_2014.shp")



png(paste0("output/_rev_", vrsN, "todos_os_dados.png"), 1000,900)
# tiff(paste0("output/_rev_", vrsN, "todos_os_dados.tif"),
#      width = 1000, height = 900,units = "px") # ,res = 300
par(mar=c(3,3,2,0), lend = 0)

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
axis(1, at = seq(-100,-20,10), labels = seq(-100,-20,10)); axis(2)
points(latitude_y~longitude_x, data=allFiles, pch=19, 
       col="darkorchid3", cex=2)
# Localizar entradas na base de dados que fogem da distribuicao
# Y: a latitude na mata atlantica varia de -40 a 0
# X: o longitude na mata atlantica varia de -60 a -30
validCoord <- allFiles[((allFiles$longitude_x > -70 & allFiles$longitude_x < -30) &
                         (allFiles$latitude_y > -40 & allFiles$latitude_y < 0)), ]
dataOut <- allFiles[!((allFiles$longitude_x > -70 & allFiles$longitude_x < -30) &
                      (allFiles$latitude_y > -40 & allFiles$latitude_y < 0)), ]
points(latitude_y~longitude_x, data=dataOut, pch=4, 
       col="red", cex=3)
par( lend = 1)
legend("topleft", cex = 1.5, pt.cex = c(2, 3), 
       pch= c(19, 4, NA, NA, NA), 
       col= c("darkorchid3","red", "olivedrab2", "gray75", "gray48"), 
       lwd  = c(NA, NA, 15, 15, 2),
       lty  = c(NA, NA, 1, 1, 1),
       seg.len = c(1, 1, 2, 2, 2),
       legend = c("coordinates\n(lat~lon coord)\n",
                  paste0("out of Atlantic Forest domain\n(", 
                         nrow(dataOut), ")\n"),
                  "Atlantic Forest biome\n",
                  "biogeographical regions\n",
                  "brazilian states") )
dev.off()
# salva os nomes dos arquivos com coordenadas fora da area foco
temp <- as.data.frame(table(dataOut$file))
temp <- temp[temp[,2] > 0, ]
temp2 <- as.data.frame(table(allFiles$file))
temp <- cbind.data.frame(temp, ttl = temp2[is.element(temp2[,1],temp[,1]),2])
write.table(temp, paste0("output/_rev_", vrsN, 
                         "_arq_out-coordinate_",
                         format(Sys.time(), "_%Y-%m-d%d"),"-",
                         gsub(":", "", format(Sys.time(), "%X") ),".txt"))
###############################################################################
# salva o dataframe com todas as coordenadas fora do dominio
write.csv(dataOut, paste0("output/_rev_", vrsN, "_dataOut.csv"))
write.xlsx(dataOut, paste0("output/_rev_", vrsN, "_dataOut.xlsx"))



###############################################################################
# plota o mapa de distribuicao dos dados para cada conjunto de dados

# qtdade de coordenadas unicas
tam <- nrow(unique(allFiles[,9:10]))
# mapa de distribuicao dos dados para cada conjunto de dados
for (planilha in sort(unique(allFiles$file))) {
  # cria uma area de plotagem com 2 graficos
  # # teste
  # png(paste0("output/_rev_", vrsN, "teste.png"), 500,300)
  png(paste0("output/_afericao_coord_por_arquivo/_rev_", vrsN, "_",
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
  legend("topleft", cex = 1, pt.cex = c(1.5, 2.5), 
         pch= c(19,16), 
         col= c("darkorchid3","orange"), 
         legend = c(paste0("all coordinates: ",
                           nrow(allFiles[,9:10]),"\n"),
                    paste0("this file coordinates: ",
                           nrow(aux[,9:10]),"\n")) )
  title(main = paste0("num. of all coordinates:\n", tam, 
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
  title(main = paste0("num. of coordinates in this file:\n", 
                      nrow(unique(aux[,9:10])), " (without repetition)"))
  mtext(planilha, outer = TRUE, cex = 1.5)
  dev.off()
}








###############################################################################
# mapa de distribuicao por taxonomia
###############################################################################
# mapa de distribuicao dos dados para cada tx

###############################################################################
## PLANTA
# por ordem
temp2 <- sort(unique(allFiles$plant_order))
# w <- 1
for (w in 1:length(temp2)) { 
  # separa um sub set por ordem
  sep_or_Files <- allFiles[allFiles$plant_order == temp2[w], ]
  # por familia, genero e (especie)
  temp <- c("plant_family","plant_genera")
  j <- 1
  for (planilha in sort(unique( sep_or_Files[,temp[j]] )) ) { # planilha <- sort(unique( sep_or_Files[,temp[j]] ))[1]
    # qtdade de fam unicas
    tam <- length(unique(sep_or_Files[,temp[j]]))
    # dessa familia:
    aux <- subset(sep_or_Files, plant_family == planilha)
    # cria uma area de plotagem com 2 graficos
    # # teste
    # png(paste0("output/_rev_", vrsN, "teste_tx.png"), 500,300)
    # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    png(paste0("output/_afericao_coord_por_tx/_rev_plt", vrsN, "_",
               temp2[w], "_", # qual ordem
               temp[j], # "familia"
               sprintf("_%05d",nrow(aux)), "_", # qt de entradas dessa fam
               planilha, ".png"), # qual familia
        600,360)
    par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
    
    # plota o grafico da Mata Atlantica inteira, destacando em laranja os ptos
    # do dataset especifico
    plot(amSul, asp=1, main="", #xlim = c(-70,-30), ylim = c(-35,0),
         usePolypath = FALSE, col = "gray75", bor = "white", bg = "lightskyblue")
    plot(MataAtlantica, asp=1, main=planilha, 
         usePolypath = FALSE, col = "olivedrab2", bor = NA, add = T)
    plot(BrState, asp=1, main=planilha, 
         usePolypath = FALSE, bor = "gray48", add = T)
    axis(1); axis(2)
    points(latitude_y~longitude_x, data=sep_or_Files, pch=19, 
           col="darkorchid3", cex=1.5)
    points(latitude_y~longitude_x, data=aux, pch=16, col="orange", cex=2.5)
    legend("topleft", cex = 1, pt.cex = c(1.5, 2.5), 
           pch= c(19,16), 
           col= c("darkorchid3","orange"), 
           legend = c(paste0("all coordinates: ",
                             nrow(sep_or_Files[,9:10]),"\n"),
                      paste0("this file taxon: ",
                             nrow(aux[,9:10]),"\n")) )
    title(main = paste0("num. of all ", sub("_"," ", temp[j]), ":\n", tam, 
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
    title(main = paste0("num. of coordinates in this taxon:\n", 
                        nrow(unique(aux[,9:10]))))
    mtext(planilha, outer = TRUE, cex = 1.5)
    dev.off()
    # # generos dentro dessa familia
    # for (ge_nm in sort(unique( aux[,temp[2]] ))) { # ge_nm <- sort(unique( aux[,temp[2]] ))[1]
    #   # qtdade de gen unicos
    #   tam <- length(unique(aux[,temp[2]]))
    #   # dessa familia:
    #   subtb_ge <- subset(aux, plant_genera == ge_nm)
    #   # cria uma area de plotagem com 2 graficos
    #   # # teste
    #   # png(paste0("output/_rev_", vrsN, "teste_tx.png"), 500,300)
    #   # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #   png(paste0("output/_afericao_coord_por_tx/_rev_ptl", vrsN, "_",
    #              "genera-from-", 
    #              temp[2], "_", temp2[w], # qual ordem
    #              sprintf("_%05d",nrow(subtb_ge)), "_", # qt de entradas desse ge
    #              ge_nm, ".png"), # nome do genero
    #       600,360)
    #   par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
    #   
    #   # plota o grafico da Mata Atlantica inteira, destacando em laranja os ptos
    #   # do dataset especifico
    #   plot(amSul, asp=1, main="", #xlim = c(-70,-30), ylim = c(-35,0),
    #        usePolypath = FALSE, col = "gray75", bor = "white", bg = "lightskyblue")
    #   plot(MataAtlantica, asp=1, main=ge_nm, 
    #        usePolypath = FALSE, col = "olivedrab2", bor = NA, add = T)
    #   plot(BrState, asp=1, main=ge_nm, 
    #        usePolypath = FALSE, bor = "gray48", add = T)
    #   axis(1); axis(2)
    #   points(latitude_y~longitude_x, data=aux, pch=19, 
    #          col="darkorchid3", cex=1.5)
    #   points(latitude_y~longitude_x, data=subtb_ge, pch=16, col="orange", cex=2.5)
    #   legend("topleft", cex = 1, pt.cex = c(1.5, 2.5), 
    #          pch= c(19,16), 
    #          col= c("darkorchid3","orange"), 
    #          legend = c(paste0("all coordinates: ",
    #                            nrow(aux[,9:10]),"\n"),
    #                     paste0("this file taxon: ",
    #                            nrow(subtb_ge[,9:10]),"\n")) )
    #   title(main = paste0("num. of all ", sub("_"," ", temp[2]), ":\n", tam, 
    #                       " (without repetition)"))
    #   # plota o grafico da Mata Atlantica referente aos ptos do dataset especifico
    #   plot(amSul, asp=1, main="", 
    #        xlim=range(subtb_ge$longitude_x, na.rm=T), 
    #        ylim=range(subtb_ge$latitude_y, na.rm=T),
    #        usePolypath = FALSE, col = "gray75", bor = "white", bg = "lightskyblue")
    #   plot(MataAtlantica, asp=1, main=ge_nm, 
    #        usePolypath = FALSE, col = "olivedrab2", bor = NA, add = T)
    #   plot(BrState, asp=1, main=ge_nm, 
    #        usePolypath = FALSE, bor = "gray48", add = T)
    #   axis(1); axis(2)
    #   points(latitude_y~longitude_x, data=subtb_ge, pch=19, col="orange", cex=2.5)
    #   title(main = paste0("num. of coordinates in this taxon:\n", 
    #                       nrow(unique(subtb_ge[,9:10]))))
    #   mtext(paste0(planilha," - ",ge_nm), outer = TRUE, cex = 1.5)
    #   dev.off()
    # } # fim por genero
  } # fim por familia
} # fim por ordem

###############################################################################
## INSETO
# por ordem
temp2 <- sort(unique(allFiles$insect_order))
# w <- 1
for (w in 1:length(temp2)) { 
  # separa um sub set por ordem
  sep_or_Files <- allFiles[allFiles$insect_order == temp2[w], ]
  # por familia, genero e (especie)
  temp <- c("insect_family","insect_genera")
  j <- 1
  for (planilha in sort(unique( sep_or_Files[,temp[j]] )) ) { # planilha <- sort(unique( sep_or_Files[,temp[j]] ))[1]
    # qtdade de fam unicas
    tam <- length(unique(sep_or_Files[,temp[j]]))
    # dessa familia:
    aux <- subset(sep_or_Files, insect_family == planilha)
    # cria uma area de plotagem com 2 graficos
    # # teste
    # png(paste0("output/_rev_", vrsN, "teste_tx.png"), 500,300)
    # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    png(paste0("output/_afericao_coord_por_tx/_rev_ins", vrsN, "_",
               temp2[w], "_", # qual ordem
               temp[j], # "familia"
               sprintf("_%05d",nrow(aux)), "_", # qt de entradas dessa fam
               planilha, ".png"), # qual familia
        600,360)
    par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
    
    # plota o grafico da Mata Atlantica inteira, destacando em laranja os ptos
    # do dataset especifico
    plot(amSul, asp=1, main="", #xlim = c(-70,-30), ylim = c(-35,0),
         usePolypath = FALSE, col = "gray75", bor = "white", bg = "lightskyblue")
    plot(MataAtlantica, asp=1, main=planilha, 
         usePolypath = FALSE, col = "olivedrab2", bor = NA, add = T)
    plot(BrState, asp=1, main=planilha, 
         usePolypath = FALSE, bor = "gray48", add = T)
    axis(1); axis(2)
    points(latitude_y~longitude_x, data=sep_or_Files, pch=19, 
           col="darkorchid3", cex=1.5)
    points(latitude_y~longitude_x, data=aux, pch=16, col="orange", cex=2.5)
    legend("topleft", cex = 1, pt.cex = c(1.5, 2.5), 
           pch= c(19,16), 
           col= c("darkorchid3","orange"), 
           legend = c(paste0("all coordinates: ",
                             nrow(sep_or_Files[,9:10]),"\n"),
                      paste0("this file taxon: ",
                             nrow(aux[,9:10]),"\n")) )
    title(main = paste0("num. of all ", sub("_"," ", temp[j]), ":\n", tam, 
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
    title(main = paste0("num. of coordinates in this taxon:\n", 
                        nrow(unique(aux[,9:10]))))
    mtext(planilha, outer = TRUE, cex = 1.5)
    dev.off()
    # generos dentro dessa familia
    # for (ge_nm in sort(unique( aux[,temp[2]] ))) { # ge_nm <- sort(unique( aux[,temp[2]] ))[1]
    #   # qtdade de gen unicos
    #   tam <- length(unique(aux[,temp[2]]))
    #   # dessa familia:
    #   subtb_ge <- subset(aux, insect_genera == ge_nm)
    #   # cria uma area de plotagem com 2 graficos
    #   # # teste
    #   # png(paste0("output/_rev_", vrsN, "teste_tx.png"), 500,300)
    #   # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #   png(paste0("output/_afericao_coord_por_tx/_rev_ins", vrsN, "_",
    #              "_genera-from-",
    #              temp[2], "_",temp2[w],  # qual ordem
    #              sprintf("_%05d",nrow(subtb_ge)), "_", # qt de entradas desse ge
    #              ge_nm, ".png"), # nome do genero
    #       600,360)
    #   par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
    #   
    #   # plota o grafico da Mata Atlantica inteira, destacando em laranja os ptos
    #   # do dataset especifico
    #   plot(amSul, asp=1, main="", #xlim = c(-70,-30), ylim = c(-35,0),
    #        usePolypath = FALSE, col = "gray75", bor = "white", bg = "lightskyblue")
    #   plot(MataAtlantica, asp=1, main=ge_nm, 
    #        usePolypath = FALSE, col = "olivedrab2", bor = NA, add = T)
    #   plot(BrState, asp=1, main=ge_nm, 
    #        usePolypath = FALSE, bor = "gray48", add = T)
    #   axis(1); axis(2)
    #   points(latitude_y~longitude_x, data=aux, pch=19, 
    #          col="darkorchid3", cex=1.5)
    #   points(latitude_y~longitude_x, data=subtb_ge, pch=16, col="orange", cex=2.5)
    #   legend("topleft", cex = 1, pt.cex = c(1.5, 2.5), 
    #          pch= c(19,16), 
    #          col= c("darkorchid3","orange"), 
    #          legend = c(paste0("all coordinates: ",
    #                            nrow(aux[,9:10]),"\n"),
    #                     paste0("this file taxon: ",
    #                            nrow(subtb_ge[,9:10]),"\n")) )
    #   title(main = paste0("num. of all ", sub("_"," ", temp[2]), ":\n", tam, 
    #                       " (without repetition)"))
    #   # plota o grafico da Mata Atlantica referente aos ptos do dataset especifico
    #   plot(amSul, asp=1, main="", 
    #        xlim=range(subtb_ge$longitude_x, na.rm=T), 
    #        ylim=range(subtb_ge$latitude_y, na.rm=T),
    #        usePolypath = FALSE, col = "gray75", bor = "white", bg = "lightskyblue")
    #   plot(MataAtlantica, asp=1, main=ge_nm, 
    #        usePolypath = FALSE, col = "olivedrab2", bor = NA, add = T)
    #   plot(BrState, asp=1, main=ge_nm, 
    #        usePolypath = FALSE, bor = "gray48", add = T)
    #   axis(1); axis(2)
    #   points(latitude_y~longitude_x, data=subtb_ge, pch=19, col="orange", cex=2.5)
    #   title(main = paste0("num. of coordinates in this taxon:\n", 
    #                       nrow(unique(subtb_ge[,9:10]))))
    #   mtext(paste0(planilha," - ",ge_nm), outer = TRUE, cex = 1.5)
    #   dev.off()
    # } # fim por genero
  } # fim por familia
} # fim por ordem






# save.image("ws_datapaper_2019_12_.RData")
# load("ws_datapaper_2019_12_09.RData")

###############################################################################
# Cria arvores para cada ordem
###############################################################################
# install.packages("phytools")
library(phytools)

## funcao que cria uma sting no formato do arquivo tre 
# a partir de data frame
#  ## com for inception
df2newick <- function(dframe){
  nwck <- ""
  # ordena o df
  for(i in ncol(dframe):1){
    dframe <- dframe[order(dframe[,i]),]
  }
  # por ordem
  temp1 <- unique(dframe[,1])
  for (w in 1:length(temp1)) { 
    # separa um sub set por ordem
    sep_or <- dframe[dframe[,1] == temp1[w], ]
    temp2 <- unique(sep_or[,2])
    vtfa <- ""
    for (j in 1:length(temp2)) {
      # separa um sub set por familia
      sep_fa <- sep_or[sep_or[,2] == temp2[j], ]
      temp3 <- unique(sep_fa[,3]) # generos
      vtge <- paste0(paste(temp3, collapse = ", "))
      vtfa <- paste0(vtfa,"(", vtge, ")",temp2[j],",")
    } # fim por familia
    nwck <- paste0(nwck,"(", vtfa, ")",temp1[w],",")
    nwck <- sub(",\\)","\\)", nwck)
  } # fim por ordem
  nwck <- sub(",\\)","\\)", nwck)
  nwck <- paste0("(", nwck, ")",";")
  return(nwck)
}

## PLANTA
df <- unique(allFiles[, c("plant_order","plant_family","plant_genera")])
# retira familias nao informadas 
df <- df[!is.na(df[,2]),]
geNA <- df[is.na(df[,3]),]
# df <- cbind.data.frame(Filo = "Plantae", df)
orders <- sort(unique(df$plant_order))
# para cada ordem, uma arvore
for(i in 1:length(orders)){
  subdf <- cbind.data.frame(df[df$plant_order == orders[i], ])
  # funcao que cria uma sting no formato do arquivo tre
  myNewick <- df2newick(subdf)
  # salva e le o arquivo (para ficar no formato adequado)
  write(myNewick, file = "testeTree2.tre")
  tstree <- read.tree("testeTree2.tre")
  # mar : 5.1 4.1 4.1 2.1
  png(paste0("output/_afericao_trees/_rev_", vrsN, "_treePlant_",
             orders[i], "_", # qual ordem
             sprintf("_%05d",nrow(subdf)), "generas", # qt de entradas dessa ordem
             ".png"),
      600,900)
  
  par(mfrow=c(1,1), mar=c(1,1,1,1))
  # plot(tstree)
  plot.phylo(tstree, type = "phy", label.offset = 0.05*nrow(tstree$edge), 
             x.lim = c(-0.2*nrow(tstree$edge), 1.4*nrow(tstree$edge) ))
  nodelabels(text=tstree$node.label[-c(1:2)], node=unique(tstree$edge[,1])[-c(1:2)],
             frame = "n", # tipo de moldura no rotulo
             srt= 0, # rotacao
             cex=1, # tamanho
             adj = c(1, -0.1)) # posicao 
  # nodelabels(text=tstree$node.label[1:2], node = unique(tstree$edge[,1])[1:2],
  #            frame = "n", # tipo de moldura no rotulo
  #            srt= 90, # rotacao
  #            cex=0.8, # tamanho
  #            adj = c(0.5, -1.5)) # posicao 
  title(main = orders[i])
  dev.off()
}
# arquivo com os generos que aparecem em mais de uma familia
df <- df[!is.na(df[,3]),]
temp <- df[0,]
for(i in 1:nrow(df)){
  temp2 <- df[df[,3] == df[i,3],]
  if(nrow(temp2) > 1){
    temp <- rbind(temp, temp2)
  }
}
temp <- unique(temp)

# inconsistencia taxonomica
wb <- createWorkbook()
# cria abas
addWorksheet(wb, "problemPlants")
addWorksheet(wb, "problemAnimals")
### salva info nas abas
writeData(wb, "problemPlants", temp)


## INSETOS
df <- unique(allFiles[, c("insect_order","insect_family","insect_genera")])
# retira familias nao informadas 
df <- df[!is.na(df[,2]),]
# df <- cbind.data.frame(Filo = "Plantae", df)
orders <- sort(unique(df$insect_order))
# para cada ordem, uma arvore
for(i in 1:length(orders)){
  subdf <- cbind.data.frame(df[df$insect_order == orders[i], ])
  # funcao que cria uma sting no formato do arquivo tre
  myNewick <- df2newick(subdf)
  # salva e le o arquivo (para ficar no formato adequado)
  write(myNewick, file = "testeTree2.tre")
  tstree <- read.tree("testeTree2.tre")
  # mar : 5.1 4.1 4.1 2.1
  png(paste0("output/_afericao_trees/_rev_", vrsN, "_treeInsec_",
             orders[i], "_", # qual ordem
             sprintf("_%05d",nrow(subdf)), "generas", # qt de entradas dessa ordem
             ".png"),
      600,900)
  
  par(mfrow=c(1,1), mar=c(1,1,1,1))
  # plot(tstree)
  plot.phylo(tstree, type = "phy", label.offset = 0.05*nrow(tstree$edge), 
             x.lim = c(-0.2*nrow(tstree$edge), 1.4*nrow(tstree$edge) ))
  nodelabels(text=tstree$node.label[-c(1:2)], node=unique(tstree$edge[,1])[-c(1:2)],
             frame = "n", # tipo de moldura no rotulo
             srt= 0, # rotacao
             cex=1, # tamanho
             adj = c(1, -0.1)) # posicao 
  # nodelabels(text=tstree$node.label[1:2], node = unique(tstree$edge[,1])[1:2],
  #            frame = "n", # tipo de moldura no rotulo
  #            srt= 90, # rotacao
  #            cex=0.8, # tamanho
  #            adj = c(0.5, -1.5)) # posicao 
  title(main = orders[i])
  dev.off()
}
# arquivo com os generos que aparecem em mais de uma familia
df <- df[!is.na(df[,3]),]
temp <- df[0,]
for(i in 1:nrow(df)){
  temp2 <- df[df[,3] == df[i,3],]
  if(nrow(temp2) > 1){
    temp <- rbind(temp, temp2)
  }
}
temp <- unique(temp)
### salva info nas abas
writeData(wb, "problemAnimals", temp)
# salva o arquivo no formato xlsx
saveWorkbook(wb, file = paste0("output/insconsistenciaTaxonomica", vrsN,
                               format(Sys.time(), "_%Y-%m-d%d"),".xlsx"))








###############################################################################  
# graficos apresentando a diversidade taxonomica das redes
## grafico de barras de diversidade
###############################################################################
plotdiversity <- function(gNvplanTx,gNvanimTx){
  # tiff(paste0("../output/_rev_", vrsN, "_diversityColor.tif"),
  #      width = 174,height = 233,units = "mm",res = 300)
  png(paste0("output/_rev_", vrsN, "_nSpColor.png"),
      width = 174,height = 233,units = "mm",res = 300)
  par(mfcol= c(3,2), mar= c(2,8,1.8,1)+0.1, mgp= c(1,1,0))
  # plantas
  for (i in 1:3){
    tempgNvplanTx <- gNvplanTx[!is.na(gNvplanTx[,i]), ]
    temp <- unique(tempgNvplanTx[,i])
    soma <- vector(mode = "numeric", length = length(temp))
    for (j in 1:length(temp)){
      soma[j] <- sum(tempgNvplanTx[ ,i] == temp[j])
    }
    soma <- data.frame(name = temp[order(soma, decreasing =T)], 
                       sum = soma[order(soma, decreasing =T)])
    if(i == 1){
      barplot(soma[25:1,2], names.arg = soma[25:1,1], las = 1, horiz = T,
              beside = T, col = "#008033", border = NA,
              main = paste0("Number of plant species by\n ", colnames(gNvplanTx)[i],
                            " - total of ",length(soma[,1]) ))
    }else{
      barplot(soma[25:1,2], names.arg = soma[25:1,1], las = 1, horiz = T,
              beside = T, col = "#008033", border = NA,
              main = paste0(colnames(gNvplanTx)[i],
                            " - total of ",length(soma[,1]) ))
    }
  }
  # animais
  for (i in 1:3){
    tempgNvanimTx <- gNvanimTx[!is.na(gNvanimTx[,i]), ]
    temp <- unique(tempgNvanimTx[ ,i])
    soma <- vector(mode = "numeric", length = length(temp))
    for (j in 1:length(temp)){
      soma[j] <- sum(tempgNvanimTx[ ,i] == temp[j])
    }
    soma <- data.frame(name = temp[order(soma, decreasing =T)], 
                       sum = soma[order(soma, decreasing =T)])
    if(i == 1){
      barplot(soma[25:1,2], names.arg = soma[25:1,1], las = 1, horiz = T,
              beside = T, col = "#ff2a2a", border = NA,
              main = paste0("Number of animal species by\n ", colnames(gNvanimTx)[i],
                            " - total of ",length(soma[,1]) ))
    }else{
      barplot(soma[25:1,2], names.arg = soma[25:1,1], las = 1, horiz = T,
              beside = T, col = "#ff2a2a", border = NA,
              main = paste0(colnames(gNvanimTx)[i],
                            " - total of ",length(soma[,1]) ))
    }
  }
  par(mfcol= c(1,1), mar= c(5,4,4,2)+0.1, mgp= c(3,1,0))
  dev.off()
}


Plt <- unique(allFiles[, c("plant_order","plant_family","plant_genera","plant_species")])
colnames(Plt) <- c("order","family","genera","species")
Anm <- unique(allFiles[, c("insect_order","insect_family","insect_genera","insect_species")])
colnames(Anm) <- c("order","family","genera","species")

write(paste0("Number of Animals\nSpecies:\t", length(Anm$species[!is.na(Anm$species)]),
             "\nGenus:\t", length(unique(Anm$genera[!is.na(Anm$genera)])),
             "\nFamily:\t", length(unique(Anm$family[!is.na(Anm$family)])),
             "\nOrder:\t", length(unique(Anm$order)),
             "\nNumber of Plants\nSpecies:\t", length(Plt$species[!is.na(Plt$species)]),
             "\nGenus:\t", length(unique(Plt$genera[!is.na(Plt$genera)])),
             "\nFamily:\t", length(unique(Plt$family[!is.na(Plt$family)])),
             "\nOrder:\t", length(unique(Plt$order)) ),
      file = "output/_rev_NumberOfTaxon.txt")

plotdiversity(Plt,Anm)










###############################################################################  
# graficos apresentando a abundacia das redes
## grafico de barras de abundancia
###############################################################################
plotAbundancia <- function(gNvplanTx,gNvanimTx){
  # tiff(paste0("../output/_rev_", vrsN, "_diversityColor.tif"),
  #      width = 174,height = 233,units = "mm",res = 300)
  png(paste0("output/_rev_", vrsN, "_abundanceColor.png"),
      width = 174,height = 233,units = "mm",res = 300)
  par(mfcol= c(3,2), mar= c(2,8,1.8,1)+0.1, mgp= c(1,1,0))
  # plantas
  for (i in 1:3){
    soma <- as.data.frame(table(gNvplanTx[!is.na(gNvplanTx[,i]),i]))
    colnames(soma) <- c("name","sum")
    soma <- soma[order(soma$sum, decreasing =T),]
    if(i == 1){
      barplot(soma[25:1,2], names.arg = soma[25:1,1], las = 1, horiz = T,
              beside = T, col = "#008033", border = NA,
              main = paste0("Number of plant entries\n ", colnames(gNvplanTx)[i],
                            " - total of ",length(soma[,1]) ))
    }else{
      barplot(soma[25:1,2], names.arg = soma[25:1,1], las = 1, horiz = T,
              beside = T, col = "#008033", border = NA,
              main = paste0(colnames(gNvplanTx)[i],
                            " - total of ",length(soma[,1]) ))
    }
  }
  # animais
  for (i in 1:3){
    soma <- as.data.frame(table(gNvanimTx[!is.na(gNvanimTx[,i]),i]))
    colnames(soma) <- c("name","sum")
    soma <- soma[order(soma$sum, decreasing =T),]
    if(i == 1){
      barplot(soma[25:1,2], names.arg = soma[25:1,1], las = 1, horiz = T,
              beside = T, col = "#ff2a2a", border = NA,
              main = paste0("Number of animal entries\n ", colnames(gNvanimTx)[i],
                            " - total of ",length(soma[,1]) ))
    }else{
      barplot(soma[25:1,2], names.arg = soma[25:1,1], las = 1, horiz = T,
              beside = T, col = "#ff2a2a", border = NA,
              main = paste0(colnames(gNvanimTx)[i],
                            " - total of ",length(soma[,1]) ))
    }
  }
  par(mfcol= c(1,1), mar= c(5,4,4,2)+0.1, mgp= c(3,1,0))
  dev.off()
}


Plt <- allFiles[, c("plant_order","plant_family","plant_genera","plant_species")]
colnames(Plt) <- c("order","family","genera","species")
Anm <- allFiles[, c("insect_order","insect_family","insect_genera","insect_species")]
colnames(Anm) <- c("order","family","genera","species")

write(paste0("Number of Animals\nSpecies:\t", length(Anm$species[!is.na(Anm$species)]),
             "\nGenus:\t", length(Anm$genera[!is.na(Anm$genera)]),
             "\nFamily:\t", length(Anm$family[!is.na(Anm$family)]),
             "\nOrder:\t", length(Anm$order),
             "\nNumber of Plants\nSpecies:\t", length(Plt$species[!is.na(Plt$species)]),
             "\nGenus:\t", length(Plt$genera[!is.na(Plt$genera)]),
             "\nFamily:\t", length(Plt$family[!is.na(Plt$family)]),
             "\nOrder:\t", length(Plt$order) ),
      file = "output/_rev_AbundancePerTaxon.txt")

plotAbundancia(Plt,Anm)










###############################################################################  
# rede de interacao por taxons superiores
# biblioteca a ser utilizada
library(bipartite)

# tabela geral de interações
# plantFaXInsectOrder
temp <- allFiles[,c("plant_family", "insect_order")]
gf <- as.matrix(table(temp))

png(paste0("output/_rev_", vrsN, "_redeInteracao_plantFaXInsectOrder.png"),
    width = 600,height = 174,units = "mm",res = 300)
# plotweb(gf)
plotweb(gf, col.high= "#ed7d31", col.low= "#5b9bd5", # cores dos nos
        bor.col.high= F, bor.col.low= F, # bordas dos nos
        arrow="both", bor.col.interaction= "grey18", # linhas repre as int
        # col.interaction="grey18", bor.col.interaction= "grey80", # representacao das int
        ybig = 3.5, low.y=2.5, high.y=4.3, # posicoes (entre as camadas, camada de baixo e a de cima)
        high.lab.dis= 0.05, low.lab.dis= 0.05, # proximidade das label dos nos
        method = "normal", # ordem dos nos 
        text.rot= 90,labsize = 0.8) # direcao do texto)
dev.off()

# plantOrXInsectOrder
temp <- allFiles[,c("plant_order", "insect_order")]
gf <- as.matrix(table(temp))

png(paste0("output/_rev_", vrsN, "_redeInteracao_plantOrderXInsectOrder.png"),
    width = 300,height = 174,units = "mm",res = 300)
# plotweb(gf)
plotweb(gf, col.high= "#ed7d31", col.low= "#5b9bd5", # cores dos nos
        bor.col.high= F, bor.col.low= F, # bordas dos nos
        arrow="both", bor.col.interaction= "grey18", # linhas repre as int
        # col.interaction="grey18", bor.col.interaction= "grey80", # representacao das int
        ybig = 3.5, low.y=2.5, high.y=4.3, # posicoes (entre as camadas, camada de baixo e a de cima)
        high.lab.dis= 0.05, low.lab.dis= 0.05, # proximidade das label dos nos
        method = "normal", # ordem dos nos 
        text.rot= 90,labsize = 1) # direcao do texto)
dev.off()
