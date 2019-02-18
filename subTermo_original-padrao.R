###############################################################################
# 02/02/2019 - NAO ESTA PRONTO!!!
###############################################################################
# substituicao dos termos da tabela pelos termos padronizados
#install.packages("openxlsx")
setwd("")

library(openxlsx)
####################
# teste
library(openxlsx)
humi <- read.xlsx("Testes off-drive/col16_relative_humidity.xlsx")
allsem <- read.csv("Testes off-drive/v03-3_allFiles_coord_ok.csv")
x <- merge(humi[,c(1,3)],allsem)
write.xlsx(x,"Testes off-drive/test01.xlsx")
x <- merge(humi[,c(1,3)],allsem, sort = F)
write.xlsx(x,"Testes off-drive/test01.xlsx")
####################


library(openxlsx)
# leitura do arquivo com todos os dados
allsem <- read.csv("Testes off-drive/v03-3_allFiles_coord_ok.csv")
# coluna "X", com o numero das linhas, colocar novos valores (para poder reordenar)
# poderia ser a coluna "ordemdb", mas a coluna "X" tem menos informaçġes e poder 
# ser numerica (agiliza a ordenar)
allsem$X <- 1:length(allsem$X)
# converte o nome dos arquivos para factor
allsem$file <- as.factor(allsem$file)

# leitura dos arquivos com as colunas padronizadas
caminhos <- "Testes off-drive/"
arqs <- dir(caminhos, pattern = "col.*.xlsx")

# vetor para armezenar glossario de termos padronizados
glossar <- vector()
# data frame para armezenar erros nos termos
terros <- data.frame()

# loop (para cada coluna padronizada, aplicar o mesmo processo)
padrao <- allsem
for (i in 1:2){
  temp <- read.xlsx(paste0("Testes off-drive/",arqs[i]))
  temp1 <- colnames(temp)[1]
  # adiciona o nome padronizado sem alterar a ordem das linhas
  padrao <- merge(temp[,c(1,3)],padrao, sort = F)
  # col <nome da coluna> recebem o prefixo "old_": old_<nome da coluna>
  colnames(padrao)[1] <- paste0("old_", temp1)
  # col new recebem o nome original: <nome da coluna>
  colnames(padrao)[2] <- temp1
  # data frame para armezenar termos
  temp2 <- paste(unique(temp[,3]), collapse = ", ")
  temp2 <- paste(temp1, temp2, sep = ": ")
  glossar <- paste0(glossar, temp2, "\n")
  # data frame para armezanar erros dos termos
  # se tem comentarios
  temp2 <- ncol(temp)
  temp2 <- 5-temp2
  temp2 <- ifelse(temp2>0, temp2, 0)
  if(temp2 > 0){
    temp <- cbind(temp, as.list(rep(NA, temp2)))
    temp2 <- cbind(temp1, temp[!is.na(temp[,4]),1:5])
    colnames(temp2) <- c("col_name","author_term","freq","term","coment","obs")
    terros <- rbind(terros, temp2)
  }
  
}

# gera dataframe com as colunas padronizadas
# data frame final recebe colunas padronizadas a partir do data frame com tudo
# (colunas padronizadas e sem padronizar)
tdpadrao <- padrao[, colnames(allsem)]
# re-ordena as linhas
tdpadrao <- tdpadrao[order(tdpadrao$X), ]

# salva saidas em xlsx (e csv para a tabela com dados padronizados)
write.csv(glossar, "Testes off-drive/v03-3_lista_termos_aceitos.xlsx")
write.csv(terros, "Testes off-drive/v03-3_lista_termos_com_prob.xlsx")
write.csv(tdpadrao, "Testes off-drive/v03-3_allFiles_crd-ok_trm-ok.xlsx")

# salva um xlsx para cada arquivo recebido
temp <- unique(tdpadrao[,2])
for ( i in 1:length(temp)){
  temp1 <- tdpadrao[tdpadrao[,2] == temp[i], -c(1,2)]
  temp2 <- sub("..........\\.xlsx", "2019_02_02.xlsx", temp[i])
  write.csv(temp1, paste0("Testes off-drive/", temp2))
}
