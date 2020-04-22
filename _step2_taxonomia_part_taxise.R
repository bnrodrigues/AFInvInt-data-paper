# versao do script e conjunto de dados. Evitar sobrescrever.
vrsN <- "v04-1"

# define diretorio de trabalho
setwd("<your working diretory path>")

###############################################################################  
######## verifica a taxonomia dos especimens presentes nos arquivos ###########
# uso do pacote taxise para recuperar a taxonomia                             #
# gera 2 tabelas a partir dos arquivos:                                       #
# - nomes sem correspondencia na base de dados taxise (nao tem como conferir) #
# - nomes que estao com classificacao diferente da taxise (seriam erros)      #
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

# install.packages("taxize")
library(taxize)

library(openxlsx)


###############################################################################
# funcoes a serem utilizadas ##################################################

# Padronizar os aff. cf. sp. spp. e sp.<numero> spp. e etc
# sem alterar o conteudo do nome do especime
padronizaTx <- function(txNameSp){
  # sub "Anthaxia_sp" por "Anthaxia sp"
  txNameSp <- gsub("_", " ", txNameSp)
  # sub "Anthaxia spp" por "Anthaxia spp."
  txNameSp <- gsub("spp$", "spp.", txNameSp)
  # separa "Apterostigmasp.1" para "Apterostigma sp.1"
  txNameSp <- gsub("(?=[a-z])sp\\.(?=[0-9])", " sp.", txNameSp, perl= T)
  # sub "Apterostigma sp1" por "Apterostigma sp.1"
  txNameSp <- gsub(" sp(?=[0-9])", " sp.", txNameSp, perl= T)
  # sub "Heilipus sp. 2" por "Heilipus sp.2"
  txNameSp <- gsub(" sp\\. (?=[0-9])", " sp.", txNameSp, perl= T)
  # sub "Augochlora sp 2" por "Augochlora sp. 2"
  txNameSp <- gsub(" sp (?=[0-9])", " sp.", txNameSp, perl= T)
  # coloca ponto em "Augochlora aff tantilla" para ficar "Augochlora aff. tantilla"
  txNameSp <- gsub(" aff ", " aff. ", txNameSp)
  # sub "Tegosa ca. claudina" por "Tegosa cf. claudina"
  txNameSp <- gsub(" ca\\. ", " cf. ", txNameSp)
  # sub "Augochlora cf amphitrite" por "Augochlora cf. amphitrite"
  txNameSp <- gsub(" cf ", " cf. ", txNameSp)
  # sub "Augochlora cfr. Foxiana" por "Augochlora cf. Foxiana"
  txNameSp <- gsub(" cfr\\. ", " cf. ", txNameSp)
  # sub "Augochlora cfr Foxiana" por "Augochlora cf. Foxiana"
  txNameSp <- gsub(" cfr ", " cf. ", txNameSp)
  # sub "Tegosa ca. claudina" por "Tegosa cf. claudina"
  txNameSp <- gsub(" \\.", "", txNameSp)
  # sub espacos duplos
  txNameSp <- gsub("  ", " ", txNameSp)
  
  return(txNameSp)
}

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




###############################################################################
# espacos extras
for (i in c(17:20,25:28)){
  # tira espacos do final
  allFiles[, i] <- sub(" $", "",  allFiles[, i])
  # tira espacos do comeco
  allFiles[, i] <- sub("^ ", "",  allFiles[, i])
  # tira espacos duplos
  allFiles[, i] <- gsub("  ", " ",  allFiles[, i])
}
# separa as colunas que tem info taxonomica de planta e de animal
orTxPlant <- allFiles[, 17:20]
orTxAnima <- allFiles[, 25:28]
# tira repetições
orTxPlant <- unique(orTxPlant)
orTxAnima <- unique(orTxAnima)
# gera uma coluna com a identificacao mais precisa disponivel por entrada
orTxPlant <- cbind(orTxPlant, NA)
orTxPlant[!is.na(orTxPlant[, 1]), 5] <- orTxPlant[!is.na(orTxPlant[, 1]), 1]
orTxPlant[!is.na(orTxPlant[, 2]), 5] <- orTxPlant[!is.na(orTxPlant[, 2]), 2]
orTxPlant[!is.na(orTxPlant[, 3]), 5] <- orTxPlant[!is.na(orTxPlant[, 3]), 3]
orTxPlant[!is.na(orTxPlant[, 4]), 5] <- orTxPlant[!is.na(orTxPlant[, 4]), 4]
orTxAnima <- cbind(orTxAnima, NA)
orTxAnima[!is.na(orTxAnima[, 1]), 5] <- orTxAnima[!is.na(orTxAnima[, 1]), 1]
orTxAnima[!is.na(orTxAnima[, 2]), 5] <- orTxAnima[!is.na(orTxAnima[, 2]), 2]
orTxAnima[!is.na(orTxAnima[, 3]), 5] <- orTxAnima[!is.na(orTxAnima[, 3]), 3]
orTxAnima[!is.na(orTxAnima[, 4]), 5] <- orTxAnima[!is.na(orTxAnima[, 4]), 4]
# ordena a matriz de classificacoes originais
orTxPlant <- orTxPlant[order(orTxPlant[, 5]), ]
orTxAnima <- orTxAnima[order(orTxAnima[, 5]), ]

# Padronizar os aff. cf. sp. spp. e sp.<numero> spp. e etc
orTxAnima[,5] <- padronizaTx(orTxAnima[,5])
orTxPlant[,5] <- padronizaTx(orTxPlant[,5])
# salva
write.xlsx(list("orTxPlant" = orTxPlant, "orTxAnima" = orTxAnima),
           paste0("output/",vrsN,"_orTx.xlsx"))
# normalisa a coluna com as id mais precisas e mantem a col com nomes orig
mtOrTxPlant <- simplNorm(orTxPlant[,5])
mtOrTxAnima <- simplNorm(orTxAnima[,5])




###############################################################################
# busca automatica de taxonomia
# manda para o taxise a coluna com as id mais precisas
gnrTxPlant <- nomeCorreto(unique(mtOrTxPlant[,2]))
gnrTxAnima <- nomeCorreto(unique(mtOrTxAnima[,2]))
save.image("part_taxise_v03.RData")
#load("part_taxise_v03.RData")
# retorna a classificacao
tplanTx <- classifTx(gnrTxPlant)
tanimTx <- classifTx(gnrTxAnima)
# para especies que não retornou as classificacoes mais abrangentes
#(ex. Henrietta succosa):
# a <- tanimTx[tanimTx[, 7] == "" & tanimTx[, 5] != "", ]
# 
# # busca em cada uma das linhas q a familia esta faltando
# for(i in 1:length(a[, 1])){
#   temp <- classification(a[i,5], db = "gbif", 
#                          rows = 1) # considera apenas a primeira opcao 
#   #(qd tem mais de um match)
#   if (length(temp[[1]]$name[temp[[1]]$rank == "family"]) > 0){
#     a[i, 7] <- temp[[1]]$name[temp[[1]]$rank == "family"]
#   }else{
#     a[i, 7] <- paste0(a[i, 9], " (awaiting allocation)")
#   }
# }
# gNvanimTx[gNvanimTx[, 7] == "", ] <- a




###############################################################################
### Separa quais não retornou classificacao e salva no arquivo
# (para procurar manualmente)
tanimTx <- unique(tanimTx) # torna unica cada linha
tplanTx <- unique(tplanTx) # torna unica cada linha

colnames(tanimTx)[1] <- "simpl"
tanimTx <- merge(mtOrTxAnima, tanimTx)
semClassA <- setdiff(mtOrTxAnima[,1],tanimTx[,2]) # classif sem Tx
semClassA <- orTxAnima[is.element(orTxAnima$`NA`, semClassA), ]

colnames(tplanTx)[1] <- "simpl"
tplanTx <- merge(mtOrTxPlant, tplanTx)
semClassP <- setdiff(mtOrTxPlant[,1],tplanTx[,2]) # classif sem Tx
semClassP <- orTxPlant[is.element(orTxPlant$`NA`, semClassP), ]

library(openxlsx)
# salva arquivo para ajuste manual (nao retornou nada automaticamente)
write.xlsx(list(`59errorPlant` = semClassP, `312errorAnimal` = semClassA), 
      file = paste0("output/",vrsN,"_colunas_taxonomia.xlsx"))

# save.image("part_taxise_v03-a.RData")
# load("part_taxise_v03-a.RData")



# quando tem o nome do descritor da especie... (tem , antes de uma data)

# nomes com problemas (deixa com subespecie?):
# "Mechanitis l. lysimnia", "Manduca h. hannibal"
# "Mechanitis lysimnia lysimnia", "Manduca hannibal hannibal"


###############################################################################
### dos que encontrou classificacao ###########################################
# ver a id de especie bate, 
#  - se tem sp, desconsiderar da comparacao
#  - se tem erro de grafia: subtitui
#  - se nao: marca com [sem corresp] (ver se bate c o q ja foi separado)
# se o id da especie bate,
#  - ver se as classificacoes mais abrangentes batem
#  - se for erro de grafia: substitui
#  - se não: marca com [rever] nome_q_esta / sugestao
###############################################################################

###############################################################################
### 1 - gera tabela com classif original e classif recuperada
# seleciona so os que o taxise retornou alguma coisa (atraves do merge)
md_orTxAnima <- orTxAnima
colnames(md_orTxAnima)[5] <- "nomesOriginais"
md_orTxAnima <- merge(md_orTxAnima, tanimTx)

md_orTxPlant <- orTxPlant
colnames(md_orTxPlant)[5] <- "nomesOriginais"
md_orTxPlant <- merge(md_orTxPlant, tplanTx)

write.xlsx(list(txAnim = md_orTxAnima, txPlant = md_orTxPlant), 
           paste0("output/",vrsN,"_conferencia_tx.xlsx"))






###############################################################################
### 2 - ETAPA DE VERIFICACAO MANUAL!!!! 
# taxon que nao retornaram classificacao ou com inconsitencias







###############################################################################
### 3 - leitura da planilha com revisoes
# limpa as variaveis armazenadas na area de trabalho (workspace)
rm(list = setdiff(ls(), "vrsN"))
# le arquivo gerado na etapa anterior
# uso do stringsAsFactors = FALSE p/ nao ler como factor, mas num serem numeric
allFiles <- read.csv(paste0("output/", vrsN, "_allFiles_coord_ok.csv"),
                     row.names = 1,
                     stringsAsFactors = FALSE)
# converte o nome dos arquivos para factor
allFiles$file <- as.factor(allFiles$file)

### Funcoes utilizadas nessa etapa #############################
### Funcao para simplificar nomes
# funcao q cria um id unico para cada taxonomia fornecida pelo
# autor, para permitir relacionar a taxonomia fornecida com a
# padronizada
hiperSimpl <- function(vectorOfNames){
  # sub "(", ")", "." e " "
  vectorOfNames <- gsub("\\(", "", vectorOfNames)
  vectorOfNames <- gsub("\\)", "", vectorOfNames)
  vectorOfNames <- gsub("\\.", "", vectorOfNames)
  vectorOfNames <- gsub(" ", "", vectorOfNames)
  vectorOfNames <- tolower(vectorOfNames)
}
# fim da funcao hiperSimpl #####################################

## funcao para verifica se a alteracao foi erro de grafia ######
#(1 letra trocada ou faltando) com o uso da func "agrep"
# retorna o nome para preencher a coluna
# - se o nome tem uma peq diferenca: so sub
# - se o nome eh mto diferente: deixa registrada as 2 versoes e
#   e marca para o autor rever
subNomesSeme <- function(nomeOri, nomePadr){
  aux <- agrep(nomeOri,nomePadr,
               max.distance= 0.2, ignore.case= T)
  if (length(aux) > 0) {
    nome <- nomePadr # se o nome padronizado eh mto prox do ori
  } else {
    nome <- paste0("[to review] ", nomeOri, " / ",
                   nomePadr) # nome mto diferentes
  }
  return(nome)
}
# fim da funcao subNomesSeme ###################################
### funcao q sub as taxonomias na tabela total #################
subTaxonomia <- function(caminhoTxXlsx, allFiles, colNum, SeAnimOrPlant) {
  ### Taxonomia
  
  # especialistas
  rw_orTx <- read.xlsx(caminhoTxXlsx, sheet = paste0("exp", SeAnimOrPlant))
  
  # revisao manual
  temp1 <- read.xlsx(caminhoTxXlsx, sheet = paste0("man", SeAnimOrPlant))
  temp1 <- unique(temp1)
  # linhas da revisao manual que nao foram para especialistas
  temp2 <- temp1[!is.element(temp1[,1], rw_orTx[,1]),]
  rw_orTx <- rbind.data.frame(rw_orTx, temp2)
  
  # revisao automatica
  temp1 <- read.xlsx(caminhoTxXlsx, sheet = paste0("tx", SeAnimOrPlant))
  temp1 <- unique(temp1)
  # linhas da revisao automatica q nao foram para especialistas nem rev manual
  temp2 <- temp1[!is.element(temp1[,1], rw_orTx[,1]),]
  rw_orTx <- rbind.data.frame(rw_orTx, temp2)
  
  # vetor p armazenar o nome mais preciso
  # coluna onde comeca as tx
  x <- colNum
  fullName <- vector("character", length= nrow(allFiles))
  for (i in x:(x+3)){
    fullName[!is.na(allFiles[, i])] <- allFiles[!is.na(allFiles[, i]), i]
  }
  
  ### vefica os nomes de animais
  # vetores com os nomes simplificados (só nomes e numeros) q serao a uniao
  # do original com o padronizado
  ligaOri <- hiperSimpl(fullName)
  ligaPad <- hiperSimpl(rw_orTx[,1]) # é para ser de unicos
  if(length(unique(ligaPad))!=length(rw_orTx[,1])){
    warning(paste0("Tem uma diferenca entre a quantidade de id unicos"))
  }
  setdiff(ligaPad, unique(ligaOri)) # eh para ligaPad estar contido em ligaOri
  setdiff(unique(ligaOri), ligaPad) # e vice-versa
  
  
  # cria um dataframe para receber as classificacoes padronizadas
  # id linha | or | fa | ge | sp | full name | simpl
  allTx <- as.data.frame(matrix(nrow= nrow(allFiles), ncol= 7))
  allTx[, 1] <- allFiles$ordemdb
  
  # armazena os indices das linhas q foram alteradas
  praConferir <- vector("numeric", 0)
  
  # preenche alltx, taxonomia padronizada equivalente a encontrada nos dados 
  # enviados
  tam <- seq(1:nrow(allTx))
  for (i in 1:length(ligaPad)) {
    alltx[ligaOri==ligaPad[i], 2:7] <- c(rw_orTx[i,],ligaPad[i])
    praConferir <- c(praConferir, tam[ ligaOri==ligaPad[i] ])
  }
  # para os com "" (nao contabilizar como nao alterado)
  praConferir <- c(praConferir, tam[ ligaOri=="" ])
  
  # preenche os dados dos autores, marcando onde a nomenclatura sofreu 
  # "grandes alteracoes"
  # verifica se a alteracao foi erro de grafia (2 letras trocadas ou faltando)
  # para or | fa | ge | sp | full name
  # se estava sem nome preenchimento: nao pedir verificacao
  allFilesTx <- allFiles
  for (i in 1:praConferir) {
    for (j in 0:2){
      if (allFilesTx[i, x+j]!=""){
        allFilesTx[i, x+j] <- subNomesSeme(allFilesTx[i, x+j], 
                                           rw_orTx[i,1+j])
      } else{
        allFilesTx[i, x+j] <- rw_orTx[i,1+j]
      }
    }
  }
  return(allFilesTx)
}
# fim da funcao subTaxonomia ###################################
### Fim das funcoes utilizadas nessa etapa #####################




allFilesAlt <- subTaxonomia("../Revisao_taxonomia/_taxonomia_revisada_alt.xlsx",
                            allFiles, colNum= 27, SeAnimOrPlant= "Anim")

allFilesAlt <- subTaxonomia("../Revisao_taxonomia/_taxonomia_revisada_alt.xlsx",
                            allFiles, colNum= 19, SeAnimOrPlant= "Anim")