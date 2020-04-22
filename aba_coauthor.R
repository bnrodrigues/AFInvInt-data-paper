###############################################################################
################# Leitura dos arquivos do excel e verificacao #################
# Dados dos autores e coautores                                               #
# compara com a lista de e-mails que contactamos                              #
# cria um id unico e curto para cada arquivo (demanda uma edicao manual)      #
###############################################################################

# versao do script e conjunto de dados. Evitar sobrescrever.
vrsN <- "v01"

# define diretorio de trabalho
# setwd('<your working diretory path>')

# Biblitecas a serem utilizadas
# install.packages('openxlsx') # biblioteca para lidar com xlsx
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

##############################################################################
# Leitura de todos os autores presentes nos arquivos

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
  aux <- which(tolower(listCoaut[[i]][,1]) == "author_name (include on author per line)" & 
                 tolower(listCoaut[[i]][,2]) == "contact_person_for_this_dataset?")
  ## Primeira coluna deve ser 'author_name (include on author per line)'
  # Se nao tem nenhuma linha com os nomes das colunas
  # interrompe e indica qual arquivo nao segue a padronizacao
  if (length(aux) < 1) stop(paste0("Ha um problema no arquivo ", arqs[i],
                                   ":\nnomes das colunas fora do padrao"))
  listCoaut[[i]] <- listCoaut[[i]][(aux[1]+1):dim(listCoaut[[i]])[1], ]
  ## VERIFICA se o arquivo tem 10 colunas
  # Se nao tem 10 colunas, interrompe e indica qual arquivo tem num col != 10
  if (dim(listCoaut[[i]])[2] != 10) stop(paste0("Ha um problema no arquivo ", 
                                                arqs[i],
                                                ":\ntem ", 
                                                dim(listCoaut[[i]])[2],
                                                " colunas"))
  ## tira as linhas exemplo
  listCoaut[[i]] <- listCoaut[[i]][!grepl("The simplest",listCoaut[[i]][,6]), ]
  ## concatena os arquivos em um data frame
  file <- arqs[i]
  ## cria um id unico para cada linha, com base no nome do arquivo
  # ordemdb <- sub("ATFlowInvInt_V[0-9]_", "", tolower(file))
  ordemdb <- sub("ATFlowInvInt_V[0-9]_", "", file)
  ordemdb <- sub("_[0-9].*", "", ordemdb)
  ordemdb <- gsub(" ", "", ordemdb)
  ordemdb <- gsub(",", "_", ordemdb)
  ordemdb <- gsub("-", "", ordemdb)
  ordemdb <- gsub("\\.", "", ordemdb)
  ordemdb <- gsub("[a-z]", "", ordemdb)
  ordemdb <- gsub("__", "", ordemdb)
  ordemdb <- gsub("_$", "", ordemdb)
  ordemdb <- paste0("aut_", ordemdb, sprintf("_%04d", 1:nrow(listCoaut[[i]])))
  listCoaut[[i]] <- cbind(ordemdb, listCoaut[[i]])
  ## adiciona coluna com o nome do arquivo
  allCoauthors <- rbind(allCoauthors, cbind(file, listCoaut[[i]]))
}
colnames(allCoauthors)[3:12] <- c("author_name",
                                  "contact_person_for_this_dataset?","e-mail",
                                  "orcid","cpf only for brazilian",
                                  "filiation_1","filiation_2","filiation_3",
                                  "funding","acknowledgements")
# padronizar o orcid
allCoauthors$orcid <- gsub("http\\:\\/\\/","",allCoauthors$orcid)
allCoauthors$orcid <- gsub("^orcid.org/", "", allCoauthors$orcid)
allCoauthors$orcid <- gsub("\\.", "", allCoauthors$orcid)
allCoauthors$orcid <- gsub("^","orcid.org/",allCoauthors$orcid)

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
# arquivo com todos os e-mails
emails <- read.xlsx("../todos_contatos_email.xlsx", sheet = "emails")
# nos dados de coautores add duas colunas
allCoauthors <- cbind(list(From = rep("no", nrow(allCoauthors) )) ,allCoauthors)
allCoauthors$From <- as.character(allCoauthors$From)
for (i in 1:nrow(allCoauthors)){
  if (is.element(allCoauthors$`e-mail`[i], emails[,1])){
    allCoauthors$From[i] <- emails[is.element(emails[,1], 
                                              allCoauthors$`e-mail`[i]),2][1]
    # cat(paste0(i," | "))
  }
}


# salva o dataframe com todos os coauthorship dos arquivos em um xlsx
write.xlsx(list(allCoauthors, unique(allCoauthors[,3:5])),
           paste0("output/", vrsN, "_allCoauthorship.xlsx"), 
           sheetName = c("all","NameEmail"))
