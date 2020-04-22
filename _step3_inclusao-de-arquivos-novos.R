##########################################################################
# Etapa de atualizacao entradas novas (arquivos novos)                   #
#                                                                        #
# Separa se os arquivos novos trazem novos termos e taxonomias           #
# Comparando com os termos e taxonomias que ja tem para ver se se        #
# tratam de termos/taxonomias novas                                      #
##########################################################################

# versao do script e conjunto de dados. Evitar sobrescrever.
vrsN <- "v04-1"

##########################################################################

setwd("<your working diretory path>")
##########################################################################
# Junta os termos novos e os antigos para salvar em um arquivo base sem
# alteracoes

# limpa as variaveis armazenadas na area de trabalho (workspace)
rm(list = setdiff(ls(), "vrsN"))
# le arquivo gerado na etapa anterior
# uso do stringsAsFactors = FALSE p/ nao ler como factor, mas num serem numeric
allFiles <- read.csv(paste0("output/", vrsN, "_allFiles_coord_ok.csv"),
                     row.names = 1,
                     stringsAsFactors = FALSE)
# converte o nome dos arquivos para factor
allFiles$file <- as.factor(allFiles$file)
# biblioteca para lidar com xlsx
library(openxlsx)

### altera para a versao anterior dos termos
# selecionar apenas os termos que ainda não foram padronizados
caminhosb <- "../padronizacao_termos/xlxs_NAO MEXER/"
arqsb <- dir(caminhosb, pattern = "col[0-9].*.xlsx")
# lista para armazenar cada arquivo de padronizacao dos termos anterior
lstTermOld <- vector("list", length = length(arqsb))
for (i in 1:length(arqsb)){
  temp <- read.xlsx(paste0(caminhosb,arqsb[i]))
  lstTermOld[[i]] <- temp[, 1]
}

# numero identificador das colunas de interesse
temp <- c(1:3,9,10,13,17:20,25:28,32) # <colunas nao padronizaveis>
temp <- c(temp, 44:46) # 3 colunas "Sampling_effort" terao processamento diferente
temp <- setdiff(1:56, temp) # <tds as colunas> - <colunas nao padronizaveis>
# um arquivo para cada termo
j <- 1
for(i in temp){
  aux <- as.data.frame(table(allFiles[,i]), stringsAsFactors = F)
  aux2 <-  aux[is.element(aux[,1],lstTermOld[[j]]) ,]
  aux <- aux[!is.element(aux[,1],lstTermOld[[j]]) ,]
  aux <- rbind(aux2,aux)
  if (nrow(aux) > 0){
    aux <- cbind(aux, "")
    colnames(aux) <- c(colnames(allFiles)[i], "frequence", "new")
    write.xlsx(aux, paste0("../__afericao_termos/v01+02_col",
                           sprintf("%02d_", i), colnames(allFiles)[i],
                           sprintf("_%02dterms", nrow(aux)),".xlsx"))
  }
  j <- j + 1
}
### 3 colunas "Sampling_effort"
caminhosb <- "../padronizacao_termos/_col44-45-46/"
arqsb <- dir(caminhosb, pattern = "_col[0-9].*orig.xlsx")
# data frame para armazenar o que ja tem dessas 3 colunas
lstTermOld <- read.xlsx(paste0(caminhosb,arqsb))
lstTermOld <- unique(lstTermOld[-1,1:3])
a <- vector("character", length = nrow(lstTermOld))
for(i in 1:nrow(lstTermOld)){
  a[i] <- paste(lstTermOld[i,], collapse = " | ")
}
# essas tres colunas na versao atual
aux <- unique(allFiles[,44:46])
b <- vector("character", length = nrow(aux))
for(i in 1:nrow(aux)){
  b[i] <- paste(aux[i,], collapse = " | ")
}
# dos novos, quais novos não são elementos dos velhos
temp <- aux[!is.element(b,a), ]
# dos novos, quais novos são elementos dos velhos
aux2 <- aux[is.element(b,a), ]
# temp <- rbind(temp,aux2) # coloca os termos novos no final e salva
# write.xlsx(temp, paste0("../__afericao_termos/_v01+02_col44-45-46",
write.xlsx(temp, paste0("../__afericao_termos/_v02_col44-45-46",
                        sprintf("_%02dterms", nrow(temp)),".xlsx"))



#####################################################################
# Compara as taxonomias ja trabalhadas com as dos arquivos novos
# arquivos:
# - output/v04-1_orTx.xlsx
# - ../../scriptsR/orTx_v03.xlsx

# biblioteca para leitura de xlsx
library(openxlsx)
# leitura dos arquivos
oldPlant <- read.xlsx("../../scriptsR/orTx_v03.xlsx", sheet = "orTxPlant")
oldAnima <- read.xlsx("../../scriptsR/orTx_v03.xlsx", sheet = "orTxAnima")
newPlant <- read.xlsx("output/v04-1_orTx.xlsx", sheet = "orTxPlant")
newAnima <- read.xlsx("output/v04-1_orTx.xlsx", sheet = "orTxAnima")

### funcao para separas os novos dos velhos
# retorna os termos novos
separaVelhoNovo <- function(new,old){
  # taxonomias da versao antiga
  a <- vector("character", length = nrow(old))
  for(i in 1:nrow(old)){
    a[i] <- paste(old[i,], collapse = " | ")
  }
  # taxonomias da versao atual
  b <- vector("character", length = nrow(new))
  for(i in 1:nrow(new)){
    b[i] <- paste(new[i,], collapse = " | ")
  }
  # dos novos, quais novos não são elementos dos velhos (novos)
  temp <- new[!is.element(b,a), ]
  
  return(temp)
}
### fim da funcao

### separa os novos de cada
sepPlant <- separaVelhoNovo(newPlant,oldPlant)
sepAnima <- separaVelhoNovo(newAnima,oldAnima)


### procura as taxonomias dos nomes novos
library(taxize)
###############################################################################
# funcoes a serem utilizadas (iguais da parte do taxise) ######################

# normalizacao simples dos nomes das especies 
simplNorm <- function(nomesOriginais){
  # tira palavras que nao contribuem para a busca por ge fa e ordem
  simpl <- gsub(" sp\\.[0-9]", "", nomesOriginais) # tira " sp.<num>"
  simpl <- gsub(" sp\\.", "", simpl) # tira " sp."
  simpl <- gsub(" spp\\.", "", simpl) # tira " spp."
  simpl <- gsub("sp\\.[0-9]$", "", simpl) # tira "sp.<num>"
  simpl <- gsub("sp\\.$", "", simpl) # tira "sp."
  simpl <- gsub(" aff\\.", "", simpl) # tira aff.
  simpl <- gsub(" cf\\.", "", simpl) # tira aff.
  # tira coisas entre parenteses 
  simpl <- gsub("\\(.*\\)", "",  simpl)
  # tira simbolos 
  simpl <- chartr("#$%&'*+,-./:;,<=>?@[]^_`{|}~", 
                  "                            ", 
                  simpl)
  # tira espacos duplos
  simpl <- gsub("  ", " ",  simpl)
  # tira digitos
  simpl <- gsub("\\d", "",  simpl)
  
  # relaciona o nome corrigido com o original
  simpl <- cbind(nomesOriginais,  simpl)
  # ordena o vetor (pela coluna com os nomes originais)
  simpl <- simpl[order(simpl[, 1]), ]
  return(simpl)
}

# grafia correta dos nomes segundo a funcao gnr_resolve 
#(qt mais nomes, mais demora)
# quando o vetor de nomes esta muito grande a coneccao eh interrompida
# eh necessario separar em conjuntos menores e depois juntar
## (de 250 em 250)
## de 500 em 500 excede o tempo limite do servidor e nao conclui a busca
nomeCorreto <- function(nomeOriginal){
  # bases de dados que retornam a classificacao taxonomica tbm
  #databases <- gnr_datasources()[c(1,4,11),] # cat of life, NCBI, GBIF
  databases <- gnr_datasources()[c(1,11),] # cat of life, GBIF
  # dependendo da base de dados e do nome, as vezes pode nao ter o mesmo num
  #de colunas, nem na mesma ordem 
  tam <- length(nomeOriginal)
  qtpg <- 100
  i <- 1
  temp <- data.frame()
  while ((i+qtpg) < tam){
    gnrTtl <- gnr_resolve(names = nomeOriginal[i:(i+qtpg-1)], 
                          best_match_only = T, 
                          fields = "all", # busca todas as info possiveis
                          data_source_ids = databases[, 1])
    temp <- rbind(temp,
                  data.frame(user_supplied_name = gnrTtl$user_supplied_name,
                             matched_name = gnrTtl$matched_name,
                             classification_path = gnrTtl$classification_path,
                             classification_path_ranks = gnrTtl$classification_path_ranks,
                             data_source_title = gnrTtl$data_source_title,
                             taxon_id = gnrTtl$taxon_id,
                             score = gnrTtl$score)
    )
    cat(paste(" ",i," |"))
    i <- i+qtpg
  }
  cat("done")
  gnrTtl <- gnr_resolve(names = nomeOriginal[i:tam], 
                        best_match_only = T, 
                        fields = "all", # busca todas as info possiveis
                        data_source_ids = databases[, 1])
  temp <- rbind(temp,
                data.frame(user_supplied_name = gnrTtl$user_supplied_name,
                           matched_name = gnrTtl$matched_name,
                           classification_path = gnrTtl$classification_path,
                           classification_path_ranks = gnrTtl$classification_path_ranks,
                           data_source_title = gnrTtl$data_source_title,
                           taxon_id = gnrTtl$taxon_id,
                           score = gnrTtl$score)
  )
  return(temp)
}

# constroi uma matriz com as classificacoes taxonomicas disponiveis
# utiliza a saida da funcao gnr_resolve com o parametro fields= "all"
classifTx <- function(gnrOutput_alterado){
  tam <- length(gnrOutput_alterado[,1])
  nomeCol <- c("original_name", "matched_name", "species", "subgenus", 
               "genus", "subfamily", "family", "suborder", "order",
               "class", "phylum", "kingdom")
  numCol <- length(nomeCol)
  # precisa da funcao transNome
  aTx <- matrix(c(as.vector(gnrOutput_alterado$user_supplied_name),# era factor 
                  gnrOutput_alterado$matched_name,
                  #transNome(gnrOutput_alterado$matched_name), 
                  rep("",tam*(numCol-2))), # preenche com as info que ja tem 
                nrow = tam, ncol = numCol)
  colnames(aTx) <- nomeCol
  for(i in 1:tam){
    vtClass <- strsplit(as.vector(gnrOutput_alterado$classification_path[i]),
                        "\\|")[[1]]
    vtRanks <- strsplit(as.vector(gnrOutput_alterado$classification_path_ranks[i]),
                        "\\|")[[1]]
    cat(paste0(i, " | "))
    for(j in 1:numCol){
      if(length(vtClass[vtRanks == colnames(aTx)[j]]) > 0){
        aTx[i,j] <- vtClass[vtRanks == colnames(aTx)[j]]
      }
    }
  }
  return(aTx)
}

# junta os nomes presentes coluna com a id mais precisa
#(nome original|nome normalizado) com a matriz com as classificacoes 
#taxonomicas
juntaNm <- function(mtArtigo, mtTx){
  numcol <- length(mtTx[1, ])
  numrow <- length(mtArtigo[, 1])
  nvMt <- matrix(rep("", numcol*numrow), nrow = numrow, ncol = numcol)
  colnames(nvMt) <- colnames(mtTx)
  rownames(nvMt) <- rownames(mtArtigo)
  nvMt[, 1:2] <- mtArtigo[, 1:2] # teoricamente td a matriz de nome ori
  for(i in 1:numrow){
    linha <- mtTx[mtTx[, 1] == mtArtigo[i, 2], 3:numcol]
    if(length(linha) > 0) { # se tem taxonomia
      nvMt[i, 3:numcol] <- mtTx[mtTx[, 1] == mtArtigo[i, 2], 3:numcol]
    }
  }
  return(nvMt)
}
# fim das funcoes #############################################################
###############################################################################

# normalisa a coluna com as id mais precisas e mantem a col com nomes orig
mtOrTxPlant <- simplNorm(sepPlant[,5])
mtOrTxAnima <- simplNorm(sepAnima[,5])


###############################################################################
# busca automatica de taxonomia
# manda para o taxise a coluna com as id mais precisas
gnrTxPlant <- nomeCorreto(unique(mtOrTxPlant[,2]))
gnrTxAnima <- nomeCorreto(unique(mtOrTxAnima[,2]))
# save.image("output/part_taxise_v04_sep.RData")
#load("part_taxise_v04.RData")
# retorna a classificacao
tplanTx <- classifTx(gnrTxPlant)
tanimTx <- classifTx(gnrTxAnima)

### Separa quais não retornou classificacao e salva no arquivo
# (para procurar manualmente)
tanimTx <- unique(tanimTx) # torna unica cada linha
tplanTx <- unique(tplanTx) # torna unica cada linha

colnames(tanimTx)[1] <- "simpl"
tanimTx <- merge(mtOrTxAnima, tanimTx)
semClassA <- setdiff(mtOrTxAnima[,1],tanimTx[,1]) # classif sem Tx
semClassA <- sepAnima[is.element(sepAnima$X5, semClassA), ]

colnames(tplanTx)[1] <- "simpl"
tplanTx <- merge(mtOrTxPlant, tplanTx)
semClassP <- setdiff(mtOrTxPlant[,1],tplanTx[,1]) # classif sem Tx
semClassP <- sepPlant[is.element(sepPlant$X5, semClassP), ]

library(openxlsx)
# salva arquivo para ajuste manual (nao retornou nada automaticamente)
write.xlsx(list(`0errorPlant` = semClassP, `4errorAnimal` = semClassA), 
           file = paste0("output/",vrsN,"_taxonomia_semCorrespondencia.xlsx"))

# gera tabela com classif original e classif recuperada
# seleciona so os que o taxise retornou alguma coisa (atraves do merge)
md_orTxAnima <- sepAnima
colnames(md_orTxAnima)[5] <- "nomesOriginais"
md_orTxAnima <- merge(md_orTxAnima, tanimTx)

md_orTxPlant <- sepPlant
colnames(md_orTxPlant)[5] <- "nomesOriginais"
md_orTxPlant <- merge(md_orTxPlant, tplanTx)

write.xlsx(list(txAnim = md_orTxAnima, txPlant = md_orTxPlant), 
           paste0("output/",vrsN,"_conferencia_tx.xlsx"))