# substituicao dos termos da tabela pelos termos padronizados
#install.packages("openxlsx")
setwd("<your working diretory path>")
vrsN <- "v01_trm"

library(openxlsx)
# leitura do arquivo com todos os dados
# allsem <- read.csv("../v03-3_allFiles_coord_ok.csv")
allsem <- read.csv("output/v04-1_allFiles_coord_ok.csv", as.is= F)
# coluna "X", com o numero das linhas, colocar novos valores (para poder reordenar)
# poderia ser a coluna "ordemdb", mas a coluna "X" tem menos informaçges e poder 
# ser numerica (agiliza a ordenar)
allsem$X <- 1:length(allsem$X)
# converte o nome dos arquivos para factor
allsem$file <- as.factor(allsem$file)

### leitura dos arquivos com as colunas padronizadas
caminhos <- "../padronizacao_termos/"
arqs <- dir(caminhos, pattern = "^col[0-9].*.xlsx")
### altera para a versao anterior dos termos 
# para evitar erros decorrentes de edicoes nao intencionais do termo
# original. Ex.: "Ribeirao Preto,Campus USP" para "Ribeirao Preto, Campus USP"
caminhosb <- "../__afericao_termos/"
arqsb <- dir(caminhosb, pattern = "v01\\+02_col[0-9].*.xlsx")
# lista para armazenar cada arquivo de padronizacao dos termos
lstTermOK <- vector("list", length = length(arqs))
for (i in 1:length(arqs)){
  lstTermOK[[i]] <- read.xlsx(paste0(caminhos,arqs[i]))
}
names(lstTermOK) <- arqs
# substitui a 1a coluna do padranizado pela 1a coluna do sem padronizar
for (i in 1:length(arqs)){
  temp <- read.xlsx(paste0(caminhosb,arqsb[i]))
  cat(paste0("\n", names(lstTermOK)[i], ": ", 
             nrow(temp)," x ", nrow(lstTermOK[[i]])))
  lstTermOK[[i]][, 1] <- temp[, 1]
}

### vetor com os numeros das colunas com os termos que foram padronizados
selCols <- gsub("col", "", arqs)
selCols <- gsub("_[a-z].*", "", selCols)
# coluna X foi add nessa etapa (ordem original das linhas)
selCols <- as.numeric(selCols)+1 

### transforma as colunas que foram padronizadas no tipo character
for(i in selCols){
  allsem[,i] <- as.character(allsem[,i])
}


### termos que nao foram padronizados estao com NA (vazio)
# para nao confundir com NA (por ' NA == <a qlqr coisa>' eh NA...)
# trocar por string vazia ""
for(i in 1:length(lstTermOK)){
  lstTermOK[[i]][is.na(lstTermOK[[i]][,3]),3]  <- ""
}


### adiciona NA nos termos padronizados qd a coluna tem NA
# NA não foi padronizado, pois era NA (ficou vazio no excel)
j <- 1
for(i in selCols){
  if ( any(is.na(allsem[,i])) ){ # tem algum NA ?
    lstTermOK[[j]]  <- rbind(lstTermOK[[j]], NA)
  }
  j <- j + 1
}


### vetor para armezenar o conjunto de termos padronizados 
# termos possiveis para o preenchimento das colunas para sugerir p os
# autores que tiveram problemas de preenchimento
vtTermos <- vector()

### substitui os termos antigos, pelos termos padronizados
# - quando ha um termo novo: substitui
# - quando nao foi possivel substituir: 
#     copiar o termo antigo e add [to review]

# loop (para cada coluna padronizada, aplicar o mesmo processo)
padrao <- allsem
for (i in 1:length(selCols)){
  temp <- lstTermOK[[i]]
  temp1 <- colnames(temp)[1]
  cat(paste0("\n", temp1))
  # adiciona o nome padronizado sem alterar a ordem das linhas
  padrao <- merge(temp[,c(1,3)],padrao, sort = F)
  # col <nome da coluna> recebem o prefixo "old_": old_<nome da coluna>
  colnames(padrao)[1] <- paste0("old_", temp1)
  # col new recebem o nome original: <nome da coluna>
  colnames(padrao)[2] <- temp1
  # data frame para armezenar termos
  temp2 <- paste0("\"",paste(unique(temp[,3]), collapse = "\"; \""),"\"")
  temp2 <- paste(temp1, temp2, sep = ": ")
  temp2 <- gsub('; \\\"\\\";', ';', temp2) # qd o termo foi deixado ""
  vtTermos <- paste0(vtTermos, temp2, "\n")
  cat(paste0(" ", i,":",nrow(padrao),"| "))
}



### gera dataframe com as colunas padronizadas
# data frame final recebe colunas padronizadas a partir do data frame com tudo
# (colunas padronizadas e sem padronizar)
tdpadrao <- padrao[, colnames(allsem)]
# re-ordena as linhas
tdpadrao <- tdpadrao[order(tdpadrao$X), ]



### adiciona [to review] para os termos que não foi possivel padronizar
# os termos que não foi possivel padronizar foram deixados em branco
# na versão para voltar para o autor os termos que não foram padronizados
# precisam de uma atenção do autor e serão descados da seguinte forma:
# - [to review] <termo proposto pelo autor (não padronizado)>
for(i in selCols){
  # as entradas deixadas propositamente em branco pelos autores (com 'NA')
  temp <- allsem$X[!is.na(allsem[,i])] 
  for(j in temp){
    if (tdpadrao[j,i] == ""){
      tdpadrao[j,i] <- paste0("[to review] ", allsem[j,i])
    }
  }
  cat( paste0("\n", colnames(allsem)[i]) )
}


# salva os arquivos (csv para a tabela com termos padronizados)
write.csv(vtTermos, paste0("output/", vrsN,"_lista_termos_aceitos.csv"))
write.csv(tdpadrao, paste0("output/", vrsN,"_allFiles_crd-ok_trm-ok.csv"))
write.xlsx(tdpadrao, paste0("output/", vrsN,"_allFiles_crd-ok_trm-ok.xlsx"))



### colunas 44-45-46: 
# - col44_sampling_effort
# - col45_sampling_effort_unit
# - col46_sampling_effort_detail
# - new colunm: sampling_effort_hours

# precisa transformar tudo em character
#tdpadrao <- read.csv("output/v03-3_allFiles_crd-ok_trm-ok.csv", colClasses = "character")[,-1]

# arquivo com as 3 colunas + sampling_effort_hours
caminhos <- "../padronizacao_termos/"
temp <- read.xlsx(paste0(caminhos, "_col44-45-46/",
                              "_col44-45-46.xlsx"),startRow = 2)
# termos originais das 3 colunas
# trmMethod <- read.xlsx(paste0(caminhos, "_col44-45-46/",
#                          "_col44-45-46_orig.xlsx"),startRow = 1)
# trmMethod <- unique(tdpadrao[,45:47])[-4,]
trmMethod <- read.xlsx(paste0(caminhosb,"_col44-45-46/",
                              "_v01+02_col44-45-46_570terms.xlsx"),
                       startRow = 1)[1:3]
temp[,1:3] <- trmMethod[,1:3] # coloca os termos originais
# td para char
for (i in 1:ncol(temp)) {
  temp[,i] <- as.character(temp[,i])
}
# add uma linha com NA, NA, NA
temp <- rbind(temp, c(NA,NA,NA,NA))
# adiciona a coluna sampling_effort_hours
colMethodHours <- tdpadrao
temp1 <- colnames(temp)[1:3]
cat(paste0("\n", temp1))
# adiciona o nome padronizado sem alterar a ordem das linhas
colMethodHours <- merge(temp[,1:4],colMethodHours, sort = F)
cat(paste0(" ", i,":",nrow(colMethodHours),"| "))


# Salva versao com os termos de metodologia padronizados
write.csv(colMethodHours, paste0("output/", vrsN,"_allFiles_crd-ok_trm-ok_meth-ok.csv"))
write.xlsx(colMethodHours, paste0("output/", vrsN,"_allFiles_crd-ok_trm-ok_meth-ok.xlsx"))





# tdpadrao <- read.csv("output/v01_trm_allFiles_crd-ok_trm-ok_meth-ok.csv", colClasses = "character")[,-1]



















###############################################################################
### Padronizacao das taxonomias

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
  vectorOfNames <- gsub("\\n", "", vectorOfNames)
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
  rw_orTx <- read.xlsx(caminhoTxXlsx, sheet = paste0("exp", SeAnimOrPlant))[,1:10]
  # rw_orTx
  # revisao manual
  temp1 <- read.xlsx(caminhoTxXlsx, sheet = paste0("man", SeAnimOrPlant))[,1:10]
  temp1 <- unique(temp1)
  # id unico de cada taxoniomia fornecida pelo autor (man 'contra' o q ja tem)
  temp <- temp1[,1]
  for (i in 1:length(temp)) {
    temp[i] <- paste0(temp1[i,1:4], collapse = "") # name, or, fa, ge
  }
  temp <- hiperSimpl(temp)
  ligaPad <- rw_orTx[,1]
  for (i in 1:length(ligaPad)) {
    ligaPad[i] <- paste0(rw_orTx[i,1:4], collapse = "") # name, or, fa, ge
  }
  ligaPad <- hiperSimpl(ligaPad)
  # linhas da revisao manual que nao foram para especialistas
  temp2 <- temp1[!is.element(temp, ligaPad),]
  rw_orTx <- rbind.data.frame(rw_orTx, temp2)
  
  # revisao automatica
  temp1 <- read.xlsx(caminhoTxXlsx, sheet = paste0("tx", SeAnimOrPlant))[,1:10]
  temp1 <- unique(temp1)
  # id unico de cada taxoniomia fornecida pelo autor (aut 'contra' o q ja tem)
  temp <- temp1[,1]
  for (i in 1:length(temp)) {
    temp[i] <- paste0(temp1[i,1:4], collapse = "") # name, or, fa, ge
  }
  temp <- hiperSimpl(temp)
  ligaPad <- rw_orTx[,1]
  for (i in 1:length(ligaPad)) {
    ligaPad[i] <- paste0(rw_orTx[i,1:4], collapse = "") # name, or, fa, ge
  }
  ligaPad <- hiperSimpl(ligaPad)
  # linhas da revisao automatica q nao foram para especialistas nem rev manual
  temp2 <- temp1[!is.element(temp, ligaPad),]
  rw_orTx <- rbind.data.frame(rw_orTx, temp2)
  # linhas com NA
  rw_orTx <- rbind.data.frame(rw_orTx,  rep(NA, 10))
  
  # vetor p armazenar o nome mais preciso
  # coluna onde comeca as tx
  x <- colNum
  fullName <- vector("character", length= nrow(allFiles))
  # se nao for NA, ele "assimila" o nome que esta na coluna de [or] ate [sp]
  for (i in x:(x+3)){
    fullName[!is.na(allFiles[, i])] <- allFiles[!is.na(allFiles[, i]), i]
  }
  fullName[fullName == ""] <- NA

  ### vefica os nomes de animais
  # vetores com os nomes simplificados (só nomes e numeros) q serao a uniao
  # do original com o padronizado
  # eh um id unico da entrada, portanto o nome mais preciso + or + fa + ge
  ligaOri <- fullName
  for (i in 1:length(ligaOri)) {
    ligaOri[i] <- paste0(c(fullName[i], allFiles[i, x:(x+2)]), collapse = "") # or, fa, ge
    #ligaOri[i] <- paste0(c(allFiles[i, x+3], allFiles[i, x:(x+2)]), collapse = "") # 'sp', or, fa, ge
  }
  ligaOri <- hiperSimpl(ligaOri)
  
  ligaPad <- rw_orTx[,1]
  for (i in 1:length(ligaPad)) {
    ligaPad[i] <- paste0(rw_orTx[i,1:4], collapse = "")
  }
  ligaPad <- hiperSimpl(ligaPad) # é para ser de unicos
  
  # if(length(unique(ligaPad))!=length(rw_orTx[,1])){
  #   warning(paste0("Tem uma diferenca entre a quantidade de id unicos"))
  # }
  length(setdiff(ligaPad, unique(ligaOri))) # eh para ligaPad estar contido em ligaOri
  length(setdiff(unique(ligaOri), ligaPad)) # e vice-versa
  if(length(setdiff(unique(ligaOri), ligaPad)) > 0){
    warning(paste0("Ha taxons sem versao padronizada!!"))
  }
  
  
  # cria um dataframe para receber as classificacoes padronizadas
  # id linha | or | fa | ge | sp | full name | author | simpl
  allTx <- as.data.frame(matrix(nrow= nrow(allFiles), ncol= 8))
  # allTx[, 1] <- allFiles$ordemdb
  # colnames(allTx) <- c("ordemdb", "or", "fa", "ge", "sp", "spmName", "author", "simpl")
  allTx[, 1] <- allFiles$X
  colnames(allTx) <- c("X", "or", "fa", "ge", "sp", "spmName", "author", "simpl")

  # armazena os indices das linhas q foram alteradas
  praConferir <- vector("numeric", 0)
  
  # preenche alltx, taxonomia padronizada equivalente a encontrada nos dados 
  # enviados
  tam <- seq(1:nrow(allTx))
  for (i in 1:length(ligaPad)) {
    allTx[ligaOri==ligaPad[i], 2:8] <- c(rw_orTx[i,5:10],ligaPad[i])
    praConferir <- c(praConferir, tam[ ligaOri==ligaPad[i] ])
  }
  # para os com "" (nao contabilizar como nao alterado)
  # praConferir <- c(praConferir, tam[ ligaOri=="" ])
  praConferir <- unique(praConferir)
  
  if(length(praConferir) > nrow(allFiles)){
    warning(paste0("Ha taxons sem versao padronizada!! - linhas nao modificadas"))
  }
  
  # preenche os dados dos autores, marcando onde a nomenclatura sofreu 
  # "grandes alteracoes"
  # verifica se a alteracao foi erro de grafia (2 letras trocadas ou faltando)
  # para or | fa | ge | sp | full name
  # se estava sem nome preenchimento: nao pedir verificacao
  allFilesTx <- allFiles
  for (i in praConferir) {
    numCol <- c(2,3,4,6) # or, fa, ge e nomesConferidos
    for (j in 0:3){
      if (allFiles[i, x+j]!="" & !is.na(allFiles[i, x+j])){
        allFilesTx[i, x+j] <- subNomesSeme(allFiles[i, x+j], 
                                           allTx[ i, numCol[j+1] ])
      } else{
        allFilesTx[i, x+j] <- allTx[ i, numCol[j+1] ]
      }
    }
  }
  
  # adiciona uma coluna com o nome da especie
  temp <- allTx$sp
  allFilesTx <- cbind(allFilesTx, temp)
  if (SeAnimOrPlant == "Anim") {
    colnames(allFilesTx)[ncol(allFilesTx)] <- "insect_species"
  } else {
    colnames(allFilesTx)[ncol(allFilesTx)] <- "plant_species"
  }
  
  # adiciona uma coluna com o nome do descritor da especie
  temp <- allTx$author
  allFilesTx <- cbind(allFilesTx, temp)
  if (SeAnimOrPlant == "Anim") {
    colnames(allFilesTx)[ncol(allFilesTx)] <- "insect_AuthorSp"
  } else {
    colnames(allFilesTx)[ncol(allFilesTx)] <- "plant_AuthorSp"
  }
  
  write.csv(allTx, paste0("output/","allTx_", SeAnimOrPlant, "_", 
                          format(Sys.time(), "%Y%b%d_%H%M"), ".csv"))
  # allFilesTx <- cbind.data.frame(allFilesTx, allTx[,7]) # coloca nome autor sp
  # colnames(allFilesTx)[ncol(allFilesTx)] <- paste0(SeAnimOrPlant,"_desc_author")
  return(allFilesTx)
}
# fim da funcao subTaxonomia ###################################
### Fim das funcoes utilizadas nessa etapa #####################

tdpadraoAlt <- subTaxonomia("../Revisao_taxonomia/_taxonomia_revisada_alt.xlsx",
                            allFiles= tdpadrao, 
                            colNum= grep("insect_order", colnames(tdpadrao)), 
                            SeAnimOrPlant= "Anim")

tdpadraoAlt <- subTaxonomia("../Revisao_taxonomia/_taxonomia_revisada_alt.xlsx",
                            allFiles= tdpadraoAlt, 
                            colNum= grep("plant_order", colnames(tdpadrao)), 
                            SeAnimOrPlant= "Plant")

# Salva versao com as taxonomias ok
write.csv(tdpadraoAlt, paste0("output/", vrsN,"_allFiles_crd-ok_trm-ok_meth-ok_tx-ok.csv"))
write.xlsx(tdpadraoAlt, paste0("output/", vrsN,"_allFiles_crd-ok_trm-ok_meth-ok_tx-ok.xlsx"))





















###############################################################################
# Etapa de ultimo retorno aos autores
# salva um xlsx para cada arquivo recebido
tdpadraoAlt <- read.csv(paste0("output/", vrsN,"_allFiles_crd-ok_trm-ok_meth-ok_tx-ok.csv"),
                        row.names = 1, na.strings = "")


###############################################################################
# Reordena as colunas com a coluna a mais de metodos
# precisa de uma xlsx com os numeros das colunas a serem ordenadas
nomesColunas <- read.xlsx("nomeColunasOrdem.xlsx")
tdpadraoOrd <- tdpadraoAlt[, nomesColunas$num]
colnames(tdpadraoOrd) <- nomesColunas$Nome.novo



###############################################################################
# le os dados da aba "Co-authorship"
coAutor <- read.xlsx("../allCoauthorship_20190821.xlsx", sheet = "all")[,-1]


###############################################################################
# NA como string (para poder salvar no excel)
coAutor[is.na(coAutor)] <- "NA"


# vetor com o nome unico de cada arquivo
temp <- unique(tdpadraoOrd[,2])
for ( i in 1:length(temp)){
  temp1 <- tdpadraoOrd[tdpadraoOrd[,2] == temp[i], -c(1,2)]
  temp1 <- temp1[ order(temp1[,1]),]
  tempAutor <- coAutor[coAutor[,1] == temp[i], ]
  temp2 <- sub("..........\\.xlsx", "2019_08_22.xlsx", temp[i])
  
  # cria um objeto no formato do documento xlsx
  wb <- createWorkbook()
  # cria uma aba
  addWorksheet(wb, "Sheet_to_fill")
  addWorksheet(wb, "Co-authorship")
  # preenche a aba
  writeData(wb, "Sheet_to_fill", temp1)
  writeData(wb, "Co-authorship", tempAutor)
  # deixa tudo como text, para ele não alterar a info
  addStyle(wb, "Sheet_to_fill", rows = 1:(nrow(temp1)+1), cols=1:ncol(temp1), 
           gridExpand= T,
           style=  createStyle(numFmt = "TEXT")) # para as celulas serem todas 'text'
  addStyle(wb, "Co-authorship", rows = 1:(nrow(tempAutor)+1), cols=1:ncol(tempAutor), 
           gridExpand= T,
           style=  createStyle(numFmt = "TEXT"))
  addStyle(wb, "Sheet_to_fill", cols=1, rows = 1:(nrow(temp1)+1),
           style = createStyle(fontColour = "#c0c0c0",textDecoration = "italic") )
  # formata as celulas que atendem a uma condicao
  # Sheet_to_fill
  conditionalFormatting(wb, "Sheet_to_fill", 
                        cols=1:ncol(temp1), rows = 1:(nrow(temp1)+1),
                        rule = '="NA"', # celulas q tem "NA" [PRECISA SER '="NA"' PARA FUNC!!]
                        style = createStyle(bgFill = "yellow") ) # marca em amarelo
  conditionalFormatting(wb, "Sheet_to_fill", 
                        cols=1:ncol(temp1), rows = 1:(nrow(temp1)+1),
                        type = "contains", rule = "[to", # celulas q tem "[to"
                        style = createStyle(bgFill = "red") ) # marca em verm
  conditionalFormatting(wb, "Sheet_to_fill", 
                        cols=1:ncol(temp1), rows = 1:(nrow(temp1)+1),
                        type = "contains", rule = "[not", # celulas q tem "[not"
                        style = createStyle(bgFill = "pink") ) # marca em rosa
  conditionalFormatting(wb, "Sheet_to_fill", 
                        cols=2:58, rows = 1:(nrow(temp1)+1),
                        type = "contains", rule = "~?", # celulas q tem "?"
                        style = createStyle(bgFill = "blue") ) # marca em azul
  # Co-authorship
  conditionalFormatting(wb, "Co-authorship", 
                        cols=1:7, rows = 1:(nrow(tempAutor)+1),
                        rule = '="NA"',
                        style = createStyle(bgFill = "red") ) # marca em amarelo
  # salva o arquivo no formato xlsx
  saveWorkbook(wb, file = paste0("../__para_enviar_autores/", temp2), 
               overwrite = TRUE)
  write.csv(temp1, paste0("../__para_enviar_autores/", sub("\\.xlsx","", temp2),".csv"))
}
