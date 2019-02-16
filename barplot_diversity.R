###############################################################################  
# graficos apresentando a diversidade taxonomica das redes
## grafico de barras de diversidade
# Bárbara Nobrega Rodrigues (https://orcid.org/0000-0001-8097-7871)
###############################################################################
plotdiversity <- function(gNvplanTx,gNvanimTx,webType,output=""){
  tiff(paste0(output, webType, "_diversityColor.tif"),
       width = 174,height = 233,units = "mm",res = 300)
  # 3 linhas (ge, fa e or) duas colunas (animais e plantas)
  par(mfcol= c(3,2), mar= c(2,8,1.8,1)+0.1, mgp= c(1,1,0))
  # plantas
  for (i in c(9,7,5)){
    temp <- unique(gNvplanTx[ ,i])
    soma <- vector(mode = "numeric", length = length(temp))
    for (j in 1:length(temp)){
      soma[j] <- sum(gNvplanTx[ ,i] == temp[j])
    }
    soma <- data.frame(name = temp[order(soma, decreasing =T)], 
                       sum = soma[order(soma, decreasing =T)])
    if(i == 9){
      barplot(soma[25:1,2], names.arg = soma[25:1,1], las = 1, horiz = T,
              beside = T, col = "#008033", border = NA,
              main = paste0("Diversity of plant species by\n ", colnames(gNvplanTx)[i],
                            " - total of ",length(soma[,1]) ))
    }else{
      barplot(soma[25:1,2], names.arg = soma[25:1,1], las = 1, horiz = T,
              beside = T, col = "#008033", border = NA,
              main = paste0(colnames(gNvplanTx)[i],
                            " - total of ",length(soma[,1]) ))
    }
  }
  # animais
  for (i in c(9,7,5)){
    temp <- unique(gNvanimTx[ ,i])
    soma <- vector(mode = "numeric", length = length(temp))
    for (j in 1:length(temp)){
      soma[j] <- sum(gNvanimTx[ ,i] == temp[j])
    }
    soma <- data.frame(name = temp[order(soma, decreasing =T)], 
                       sum = soma[order(soma, decreasing =T)])
    if(i == 9){
      barplot(soma[25:1,2], names.arg = soma[25:1,1], las = 1, horiz = T,
              beside = T, col = "#ff2a2a", border = NA,
              main = paste0("Diversity of animal species by\n ", colnames(planTx)[i],
                            " - total of ",length(soma[,1]) ))
    }else{
      barplot(soma[25:1,2], names.arg = soma[25:1,1], las = 1, horiz = T,
              beside = T, col = "#ff2a2a", border = NA,
              main = paste0(colnames(planTx)[i],
                            " - total of ",length(soma[,1]) ))
    }
  }
  par(mfcol= c(1,1), mar= c(5,4,4,2)+0.1, mgp= c(3,1,0))
  dev.off()
}
###############################################################################
antaPlt <- read.csv('../output/antagonism_gNvplanTx_2018_09.csv')
antaAnm <- read.csv('../output/antagonism_gNvanimTx_2018_09.csv')
mutuPlt <- read.csv('../output/mutualism_gNvplanTx_2018_09_d20.csv')
mutuAnm <- read.csv('../output/mutualism_gNvanimTx_2018_09_d20.csv')

ttlPlt <- unique(rbind(antaPlt,mutuPlt))
ttlAnm <- unique(rbind(antaAnm,mutuAnm))

write(paste0("Number of Animals\nGenus: ", length(unique(ttlAnm$genus)),
             "\nFamily: ", length(unique(ttlAnm$family)),
             "\nOrder: ", length(unique(ttlAnm$order)),
             "\nNumber of Plants\nGenus: ", length(unique(ttlPlt$genus)),
             "\nFamily: ", length(unique(ttlPlt$family)),
             "\nOrder: ", length(unique(ttlPlt$order)) ),
      file = "txtoutput/NumberOfTaxon.txt")

webType <- "mutualism"


webType <- "antagonism"

plotdiversity(antaPlt,antaAnm,"antagonism")
plotdiversity(mutuPlt,mutuAnm,"antagonism")
