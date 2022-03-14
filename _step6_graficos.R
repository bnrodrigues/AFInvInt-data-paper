###############################################################################
######### Figuras e tabelas para representar os dados do Datapaper ############
# Gera figuras para base da parte escrita                                     #
# Unidade do que eh uma rede: site_name (ou municipality)                     #
# Unidade de esforco amostral: sampling_effort_hours                          #
# Graficos:                                                                   #
# - Fig1_Location of each record of plant-invertebrate interactions (map)     #
# - Fig2_First twenty orders families and genera (plant)                      #
# - Fig3_First twenty orders families and genera (invertebrate)               #
# - Fig4_Bipartite network of all plant-invertebrate orders interactions      #
# - Fig5_Distribution and size of each network in the dataset (map)           #
# - Fig6_Sample effort and number of sampled networks                         #
# - Fig7_Number of sampled municipalities per state                           #
# - Fig8_Number of records of each environment type at sample points          #
# - Table1_IVB_Variable information
# Para ajudar na escolha das cores:                                           #
# https://www.color-blindness.com/coblis-color-blindness-simulator/           #
###############################################################################
# versao do script e conjunto de dados. Evitar sobrescrever.
vrsN <- "07_semJP"

# define diretorio de trabalho
setwd("/run/media/bnr/arquivos/DataPaper/")

# Biblitecas a serem utilizadas
# install.packages('openxlsx')
library(openxlsx)
# install.packages("rgeos") # depende de uns programas que o qgis ja instala
library(rgeos)
# install.packages("rgdal") # depende de uns programas que o qgis ja instala
library(rgdal)
#install.packages("bipartite")
library(bipartite)

# bibliotecas para processamento de texto
# install.packages("tm")
# install.packages("wordcloud")
# install.packages("RColorBrewer")
library(tm)
library(wordcloud)
library(RColorBrewer)

source("plotweb_MaiBom.R")

###############################################################################
# IMPORTA OS DADOS PARA FAZER AS ANALISES

#allFiles <- read.xlsx("_rev_v04_allFiles_corrigido_semJPincheira.xlsx")
allFiles <- read.csv("submissao/AtlanticForestInvertFloInteractionData_2019-11.csv")
# tira JP
temp <- unique(unlist(lapply(allFiles$ordemdb,
                             function(x) unlist(strsplit(x,"_"))[1])))
allFiles <- allFiles[grep("JP_",allFiles$ordemdb, invert = T),]

# ou
# allFiles <- allFiles[grep("JP_[0-9]+$", allFiles$ordemdb, invert = T), ]

# adiciona a coluna file
file <- gsub("_[0-9].*$","",allFiles$ordemdb)
allFiles <- data.frame(file, allFiles)
rm("file")
###############################################################################



###############################################################################
# Mapas com todos os pontos
###############################################################################
# carrega o mata da mata atlantica, estados brasileiros
MataAtlantica <- readOGR(dsn="../Brazil_estados/Atlantic_MMA_miltinho_LEI_WWF_INTERNATIONAL_albers_sad69_LATLONG_WGS84.shp")
BrState <- readOGR(dsn="../Brazil_estados/Brazilian_States_Limits.shp")
amSul <- readOGR(dsn="../Brazil_estados/Lowenberg_Neto_2014.shp")


png(paste0("output/", vrsN, "_Fig1_Location of each record of plant-invertebrate interactions.png"), 650, 900)
# tiff(paste0("output/_rev_", vrsN, "todos_os_dados.tif"),
#      width = 1000, height = 900,units = "px") # ,res = 300
par(mar=c(3,3,2,0), lend = 0)

plot(amSul, asp=1, 
     main=paste(nrow(allFiles),"entries of plant-invertebrate interactions"), 
     usePolypath = FALSE, cex.main = 2, font.main = 1,
     col = "gray75", bor = "white", bg = "lightskyblue",
     xlim = c(-84,-30), ylim = c(-56,12))
plot(MataAtlantica, asp=1, usePolypath = FALSE, add = T, 
     col = "#06b131", bor = NA)
plot(BrState, asp=1, usePolypath = FALSE, add = T, lty = "dotted",
     bor = "#82948d")
axis(1, at = seq(-100,-20,10), labels = seq(-100,-20,10), cex.axis= 1.5) 
axis(2, at = seq(-50,10,10), labels = seq(-50,10,10), cex.axis= 1.5, las = 1)
points(latitude_y~longitude_x, data=allFiles, pch=19, 
       col="#00480e", cex=2)
par( lend = 1)
legend("bottomright", cex = 1.5, pt.cex = c(2, 3), 
       pch= c(19,  NA, NA, NA), 
       # col= c("#6fff07","olivedrab2", "gray75", "gray48"),
       col= c("#00480e", "#06b131", "gray75", "#82948d"),
       lwd  = c(NA, 15, 15, 2),
       lty  = c(NA, 1, 1, 3),
       seg.len = c(1, 2, 2, 2),
       legend = c("\nCoordinates of each record\n",
                  "Atlantic Forest biome\n",
                  "Biogeographical regions\n",
                  "Brazilian states") )
dev.off()


###############################################################################  
# rede de interacao por taxons superiores
###############################################################################

# tabela geral de interacoes

### plantOrXInsectOrder
temp <- allFiles[,c("plant_order", "invertebrate_order")]
gf <- as.matrix(table(temp))

png(paste0("output/", vrsN, "_Fig4_Bipartite network of all plant-invertebrate orders interactions.png"),
    width = 300,height = 174,units = "mm",res = 300)
# cores dos nos de cima e das interacoes
temp1 <- ncol(gf)

# com cores
# brewer.pal.info[brewer.pal.info$category == 'qual',]
coresB <- brewer.pal(temp1, "Paired") # rainbow(temp1, alpha = 0.5)

# tons de cinza
#coresB <- gray(seq(0.1,1,1/temp1))
# parear os cinzas
#temp2 <- c()
#for (i in 1:length(coresB)){
#  temp2 <- c(temp2,coresB[i],coresB[length(coresB)-i])
#}
#coresB <- temp2[1:temp1]
# para padrao plotweb: comentar as linhas relacionadas as cores
# par(omi = c(0,0,2,0))
plotweb_MaiBom(gf, 
               col.high= coresB, col.low= "gray40", # cores dos nos
               bor.col.high= coresB, bor.col.low= "white", # bordas dos nos
               #arrow="down.center", #bor.col.interaction= "grey18", # linhas repre as int
               col.interaction= coresB, bor.col.interaction= coresB, # representacao das int
               ybig = 3.5, low.y=2.5, high.y=4.3, # posicoes (entre as camadas, camada de baixo e a de cima)
               high.lab.dis= 0.05, low.lab.dis= 0.05, # proximidade das label dos nos
               method = "normal", # ordem dos nos 
               text.rot= 90,labsize = 1.6) # direcao do texto)
# title(main= "Pairs between plant and invertebrate orders\n(the sizes represent the number of entries)", cex.main= 1, outer= TRUE)
dev.off()
par(omi = c(0,0,0,0))




###############################################################################  
# Freq de Landuse_type_point

# quantidades
temp <- allFiles[,c("veg_landuse_type_point")]
temp <- gsub("\\(.*\\)", "", temp)
temp <- gsub("  ", " ", temp)
temp <- gsub(" $", "", temp)
temp <- gsub(" ,", ",", temp)
temp <- sort(table(temp))

temp2 <- length(temp)# tipos (28)

# total de dados (tipo)
png(paste0("output/", vrsN, "_Fig8_Number of records of each environment type at sample points.png"), 
    width = 175, height = 100,units = "mm",res = 300)

par(oma = c(0,0,1,0))
par(mfrow= c(1,2), mar= c(10,4,1,0), mgp= c(2,0.5,0))
barplot(temp[1:floor(temp2/2)], ylim = c(0,20), 
        las= 2, ylab= paste0("Frequency"))
title(xlab= "Environment type", line = 8.5)
mtext("A)", side=3, adj=-0.25, line=1, cex=1)
# box(bty = "o")
par(mar= c(10,5,1,0), mgp= c(3,0.5,0))
barplot(temp[(floor(temp2/2)+1):temp2], ylim = c(0,8000),
        las= 2, ylab= paste0("Frequency"))
title(xlab= "Environment type", line = 8.5)
mtext("B)", side=3, adj=-0.35, line=1, cex=1)

dev.off()
par(mfrow= c(1,1))
par(oma = c(0,0,0,0), mar= c(5.1, 4.1, 4.1, 2.1))
write.csv(as.data.frame(temp), 
          paste0("output/",vrsN,"_veg_landuse_type_point - tdosTipos.csv"))




###############################################################################  
# Numero de cidades amostradas por estado (img) e (tabela para Qgis)
temp <- unique(allFiles[,c("municipality", "country","state")]) # 187
# se nao eh brasil, nome do estado passa a ser o pais
# temp$state[temp$country != "Brazil"] <- temp$country[temp$country != "Brazil"]
# ordena por ordem alfabetica
temp <- temp[order(temp$state), ]
# ordena os estados por qt municipios
temp2 <- names(sort(table(temp$state)))
temp1 <- temp[temp$state == temp2[1], ]
for (i in 2:length(temp2)){
  temp1 <- rbind.data.frame(temp1, temp[temp$state == temp2[i], ])
}
temp <- temp1
# coloca os estados argentinos 1o
temp2 <- temp[temp$country == "Argentina", ]
temp <- rbind.data.frame(temp2, temp[temp$country == "Brazil", ])
# cria um vetor
temp1 <- table(temp$state)
temp1 <- temp1[match(unique(temp$state), names(temp1))]
# coloca acento nos nomes (manualmente) +++++++++++++++++++++++++++++++++++++++
temp2 <- names(temp1)
temp2[1] <- "Entre Ríos"
temp2[4] <- "Ceará"
temp2[5] <- "Goiás"
temp2[8] <- "Paraíba"
temp2[11] <- "Paraná"
temp2[13] <- "Espírito Santo"
temp2[17] <- "São Paulo"
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
png(paste0("output/", vrsN, "_Fig7_Number of sampled municipalities per state.png"), 
    width = 100, height = 100,units = "mm",res = 300)
par(mar= c(12,3,1,0), mgp= c(2,0.5,0), cex= 0.9)
barplot(temp1, names.arg = temp2,
        las= 2, ylab= "Num. of municipalities", ylim = c(0,65))
text(x= seq(0.7,(nrow(temp1)*1.2)-0.3,1.2), y= temp1+3, labels = temp1)
title(xlab= "Province/State", line = 10.5)
dev.off()

# salva em csv por nome de estado e pais
munPorEstd <- as.data.frame(temp1)
#write.csv(munPorEstd, paste0("num-municipios-por-estado", vrsN, ".csv"))





###############################################################################  
# Mapa com tamanho da rede formando circulos no mapa (tabela para Qgis)
# Unidade do que eh uma rede: site_name (ou municipality)
temp <- allFiles[,c("file","regional_name_of_sampled_area",
                    "municipality", "site_name_id","state",
                    "plant_species_complete_name", 
                    "invertebrate_species_complete_name")]
# se nao tem regional_name_of_sampled_area, recebe site_name_id
temp$regional_name_of_sampled_area[is.na(temp$regional_name_of_sampled_area)] <- 
  temp$site_name_id[is.na(temp$regional_name_of_sampled_area)]
# se nao tem regional_name_of_sampled_area nem site_name_id, recebe municipality
temp$regional_name_of_sampled_area[is.na(temp$regional_name_of_sampled_area)] <- 
  temp$municipality[is.na(temp$regional_name_of_sampled_area)]
ID <- paste(temp$regional_name_of_sampled_area, 
            temp$file, temp$state, sep = ":")
# ID <- as.numeric(as.factor(ID))
temp.allFiles <- cbind.data.frame(ID= ID, allFiles)
temp <- unique(temp.allFiles[,c("ID", 
                                "plant_species_complete_name", 
                                "invertebrate_species_complete_name")])
# cada rede (479)
temp1 <- as.data.frame(table(temp$ID)) 
rownames(temp1) <- temp1$Var1
# latitude e longitude medias
temp2 <- as.data.frame(tapply(as.numeric(temp.allFiles$latitude_y), 
                              temp.allFiles$ID, mean))
colnames(temp2) <- "latitude_y"
temp3 <- as.data.frame(tapply(as.numeric(temp.allFiles$longitude_x), 
                                          temp.allFiles$ID, mean))
colnames(temp3) <- "longitude_x"
# vincula as coordenadas ao tamanha das redes
temp1 <- cbind.data.frame(temp1, temp2, temp3)
# vicunla a info de rede e file
temp2 <- as.character(temp1$Var1)
temp2 <- as.data.frame(t(as.data.frame(strsplit(temp2, split = ":"))))
temp1 <- cbind.data.frame(temp2, temp1)
colnames(temp1)[1:5] <- c("netwk","file","state","ID","size")
# media e SD de redes por file (dataset)
mean(table(temp1$file)) # 7.484375
sd(table(temp1$file)) # 12.59124

tamRed <- temp1
rownames(temp1) <- 1:nrow(temp1)
temp2 <- data.frame(table(temp1$state))
colnames(temp2) <- c("state","n.networks")
write.xlsx(list(temp1,temp2), paste0("output/tamRedesEstado_", vrsN, ".xlsx"),row.names = F)

###############################################################################
# Mapas com todas as redes
# carrega o mata da mata atlantica, estados brasileiros
MataAtlantica <- readOGR(dsn="../Brazil_estados/Atlantic_MMA_miltinho_LEI_WWF_INTERNATIONAL_albers_sad69_LATLONG_WGS84.shp")
BrState <- readOGR(dsn="../Brazil_estados/Brazilian_States_Limits.shp")
amSul <- readOGR(dsn="../Brazil_estados/Lowenberg_Neto_2014.shp")


png(paste0("output/", vrsN, "_Fig5_Distribution and size of each network in the dataset.png"), 650,900)
# tiff(paste0("output/_rev_", vrsN, "todos_os_dados.tif"),
#      width = 1000, height = 900,units = "px") # ,res = 300
par(mar=c(3,3,2,0), lend = 0)

plot(amSul, asp=1, 
     main=paste0(nrow(temp1), " networks (size ranging from ", 
                min(temp1$size), " to ", max(temp1$size),")"), 
     usePolypath = FALSE, cex.main = 2, font.main = 1,
     col = "gray75", bor = "white", bg = "lightskyblue",
     xlim = c(-84,-30), ylim = c(-56,12))
plot(MataAtlantica, asp=1, usePolypath = FALSE, add = T, 
     col = "#06b131", bor = NA)
plot(BrState, asp=1, usePolypath = FALSE, add = T, lty = "dotted",
     bor = "#82948d")
axis(1, at = seq(-100,-20,10), labels = seq(-100,-20,10), cex.axis= 1.5) 
axis(2, at = seq(-50,10,10), labels = seq(-50,10,10), cex.axis= 1.5, las = 1)
points(latitude_y~longitude_x, data=temp1, pch=1, 
       col=rgb(0, 72, 14, alpha = 165, maxColorValue = 255), 
       cex=log(temp1$size)+0.5)
par( lend = 1)
legend("bottomright", cex = 1.5, pt.cex = c(2, 3), 
       pch= c(1,  NA, NA, NA), 
       col= c(rgb(0, 72, 14, alpha = 165, maxColorValue = 255),
              "#06b131", "gray75", "#82948d"), 
       lwd  = c(NA, 15, 15, 2),
       lty  = c(NA, 1, 1, 3),
       seg.len = c(1, 2, 2, 2),
       legend = c(" Each network",
                  "Atlantic Forest biome\n",
                  "Biogeographical regions\n",
                  "Brazilian states") )
dev.off()












###############################################################################  
# Esforco amostral (tabela para Qgis)
# media de horas de estudo (por estado) e SD

# media de horas por rede (por estado) e SD
temp <- allFiles[,c("file","regional_name_of_sampled_area",
                    "municipality", "site_name_id",
                    "country","state","sampling_effort_hours")]
# se nao tem regional_name_of_sampled_area, recebe site_name_id
temp$regional_name_of_sampled_area[is.na(temp$regional_name_of_sampled_area)] <- 
  temp$site_name_id[is.na(temp$regional_name_of_sampled_area)]
# se nao tem regional_name_of_sampled_area nem site_name_id, recebe municipality
temp$regional_name_of_sampled_area[is.na(temp$regional_name_of_sampled_area)] <- 
  temp$municipality[is.na(temp$regional_name_of_sampled_area)]
# se nao eh brasil, nome do estado passa a ser o pais
temp$state[temp$country != "Brazil"] <- temp$country[temp$country != "Brazil"]
ID <- paste(temp$regional_name_of_sampled_area, 
            temp$file, temp$state, sep = ":")
# ID <- as.numeric(as.factor(ID))
temp.allFiles <- cbind.data.frame(ID= ID, allFiles)
temp <- cbind.data.frame(ID= ID, temp)
temp <- unique(temp[,c("ID","state","sampling_effort_hours")]) # 622
# num redes por estado
statesNets <- unique(temp[,c("ID","state")])
statesNets <- data.frame(table(statesNets$state))
colnames(statesNets) <- c("state","n.networks")
statesNets$state <- as.character(statesNets$state)
# tira dados que nao estao com valores de esforco amostral
temp <- temp[!is.na(temp$sampling_effort_hours),] # 369

# calcula o esforco amostral por estado
temp1 <- as.data.frame(table(temp$ID)) 
rownames(temp1) <- temp1$Var1
colnames(temp1) <- c("ID", "size")
# latitude e longitude medias
temp2 <- as.data.frame(tapply(as.numeric(temp.allFiles$latitude_y), 
                              temp.allFiles$ID, mean))
colnames(temp2) <- "latitude_y"
temp2 <- cbind.data.frame(ID= rownames(temp2),temp2)
temp3 <- as.data.frame(tapply(as.numeric(temp.allFiles$longitude_x), 
                              temp.allFiles$ID, mean))
colnames(temp3) <- "longitude_x"
temp3 <- cbind.data.frame(ID= rownames(temp3),temp3)
# sampling_effort_hours
temp4 <- as.data.frame(tapply(as.numeric(temp$sampling_effort_hours), 
                              temp$ID, mean))
colnames(temp4) <- "mean"
temp4 <- cbind.data.frame(ID= rownames(temp4),temp4)
temp5 <- as.data.frame(tapply(as.numeric(temp$sampling_effort_hours), 
                              temp$ID, sd))
colnames(temp5) <- "sd"
temp5 <- cbind.data.frame(ID= rownames(temp5),temp5)
# vincula as coordenadas ao tamanho das redes
temp6 <- merge(merge(merge(merge(temp1, temp2), temp3), temp4),temp5)
temp6 <- temp6[temp6$size > 0, ] # fatores que tem dado de tempo
# vicunla a info de rede e file
temp2 <- as.character(temp6$ID)
temp2 <- as.data.frame(t(as.data.frame(strsplit(temp2, split = ":"))))
temp6 <- cbind.data.frame(temp2, temp6)
colnames(temp6)[1:3] <- c("netwk","file","state")

esfrcAmostRed <- temp6
#write.csv(temp6,paste0("esforcoAmostral-porRede_",vrsN,".csv"))

porStt <- as.data.frame(tapply(temp6$mean, temp6$state, mean))
porStt <- cbind.data.frame(porStt,
                           as.data.frame(tapply(temp6$mean,
                                                temp6$state, sd)))
porStt <- cbind.data.frame(porStt,
                           as.data.frame(tapply(temp6$mean,
                                                temp6$state, sum)))
colnames(porStt) <- c("mean", "sd", "sum")
porStt <- cbind.data.frame(porStt,
                           qt= as.data.frame(table(temp6$state)))
porStt$qt.Var1 <- as.character(porStt$qt.Var1)
# Deixa equivalente os estados represetados com o tbl de info de n de redes 
statesNets <- statesNets[ match(porStt$qt.Var1, statesNets$state), ]
# coloca acento nos nomes (manualmente) +++++++++++++++++++++++++++++++++++++++
porStt$qt.Var1[porStt$qt.Var1 == "Ceara"] <- "Ceará"
porStt$qt.Var1[porStt$qt.Var1 == "Goias"] <- "Goiás"
porStt$qt.Var1[porStt$qt.Var1 == "Paraiba"] <- "Paraíba"
porStt$qt.Var1[porStt$qt.Var1 == "Parana"] <- "Paraná"
porStt$qt.Var1[porStt$qt.Var1 == "Espirito Santo"] <- "Espírito Santo"
porStt$qt.Var1[porStt$qt.Var1 == "Sao Paulo"] <- "São Paulo"
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#write.csv(porStt,paste0("esforcoAmostral-porEstado_",vrsN,".csv"))


png(paste0("output/",vrsN,"_Fig6_Sample effort and number of sampled networks.png"), 
    width = 175, height = 200,units = "mm",res = 300)

layout(matrix(c(1,2,3,3), ncol= 1, nrow= 4))

par(mar= c(1,4,1,1), mgp= c(2.5,0.5,0), cex= 1, omi= c(0,0.5,0,0))
boxplot(temp6$mean~temp6$state,
        las= 2, ylab= "Hours", frame= F, xaxt = "n")
mtext("A)", side=3, adj=-0.15, line=0, cex=1)

par(mar= c(1.5,4,1,1), mgp= c(2.5,0.5,0))
barplot(porStt$sum, ylab= "Accumulated hours",las= 2)
mtext("B)",side=3, adj=-0.15, line=0, cex=1)

par(mar= c(11,4,1,1), mgp= c(2.5,0.5,0.5))
barplot(statesNets$n.networks, ylab= "N. of networks",las= 2)
axis(1, seq(0.75,(nrow(porStt)*1.2)-0.1,1.2), las= 2,
     labels = paste0(porStt$qt.Var1, " (", statesNets$n.networks, ")"))
# considerando apenas as redes que tem info nas colunas Sample effort
# barplot(porStt$qt.Freq, ylab= "N. of networks",las= 2)
# axis(1, seq(0.75,(nrow(porStt)*1.2)-0.1,1.2), las= 2,
#      labels = paste0(porStt$qt.Var1, " (", porStt$qt.Freq, ")"))
mtext("C)", side=3, adj=-0.15, line=0, cex=1)
title(xlab= "States", line= 10)

dev.off()
par(mfrow= c(1,1), omi= c(0,0,0,0)) 






##############################################################################
# salva em excel

# munPorEstd | municipios por estado 
# --qtValDifEsfPorEstd | qt de esforco amostral por arquivo # descartar
# porStt | qt de esforco amostral por rede
# tamRedEstd | tamanho das redes por estado (tem os estados da argentina separados)
tamRedEstd <- as.data.frame(tapply(tamRed$size, tamRed$state, sum))
tamRedEstd <- as.data.frame(tapply(tamRed$size, tamRed$state, mean))
tamRedEstd <- cbind.data.frame(tamRedEstd,
                               as.data.frame(tapply(tamRed$size,
                                                    tamRed$state, sd)))
tamRedEstd <- cbind.data.frame(tamRedEstd,
                               as.data.frame(tapply(tamRed$size,
                                                    tamRed$state, sum)))
colnames(tamRedEstd) <- c("mean(nt.size)", "sd(nt.size)", "sum(nt.size)")
tamRedEstd <- cbind.data.frame(qt.ntw= as.data.frame(table(tamRed$state)),
                               tamRedEstd)
colnames(tamRedEstd)[1:2] <- c("state","qt.of.networks(all)")
# juntar estados da argentina em Argentina
temp1 <- unique(allFiles[,c("country","state")])
temp1 <- temp1$state[temp1$country == "Argentina"]
tamRedEstd$state <- as.character(tamRedEstd$state)
tamRedEstd$state[is.element(tamRedEstd$state,temp1)] <- "Argentina"
temp2 <- tamRedEstd[tamRedEstd$state != "Argentina",]
temp3 <- tamRedEstd[tamRedEstd$state == "Argentina",]
temp <- rbind.data.frame(temp2, c("Argentina",
                                  sum(temp3$`qt.of.networks(all)`),
                                  mean(temp3$`mean(nt.size)`),
                                  sd(temp3$`mean(nt.size)`),
                                  sum(temp3$`sum(nt.size)`) ))

colnames(munPorEstd) <- c("state","n.of.municipalities")
colnames(porStt) <- c("mean(hours)", "sd(mean-hours)", "sum(hours)", 
                      "state", "n.of.networks")
porEstado <- merge(munPorEstd, porStt, all= T)
porEstado <- merge(porEstado, temp, all= T)


# cria um objeto no formato do documento xlsx
wb <- createWorkbook()
# cria abas
addWorksheet(wb, "byState")
### salva info nas abas
writeData(wb, "byState", porEstado) 

saveWorkbook(wb, file = paste0("output/_tabelas-info_", vrsN,
                               format(Sys.time(), "_%Y-%m-d%d-"),
                               gsub(":", "", format(Sys.time(), "%X") ),
                               ".xlsx"))















###############################################################################  
# tabela com a quantidades
# Taxons
tblTx <- data.frame(Morphotypes = NA,
                    Species = NA,
                    Genera = NA,
                    Family = NA,
                    Order = NA, stringsAsFactors = F)

tblTx["Plant", 1] <- length(na.exclude(unique(allFiles$plant_species_complete_name)))
tblTx["Plant", 2] <- length(na.exclude(unique(allFiles$plant_species)))
tblTx["Plant", 3] <- length(na.exclude(unique(allFiles$plant_genera)))
tblTx["Plant", 4] <- length(na.exclude(unique(allFiles$plant_family)))
tblTx["Plant", 5] <- length(na.exclude(unique(allFiles$plant_order)))

tblTx["Invertebrate", 1] <- length(na.exclude(unique(allFiles$invertebrate_species_complete_name)))
tblTx["Invertebrate", 2] <- length(na.exclude(unique(allFiles$invertebrate_species)))
tblTx["Invertebrate", 3] <- length(na.exclude(unique(allFiles$invertebrate_genera)))
tblTx["Invertebrate", 4] <- length(na.exclude(unique(allFiles$invertebrate_family)))
tblTx["Invertebrate", 5] <- length(na.exclude(unique(allFiles$invertebrate_order)))

# geral
qtdds <- data.frame(Summary = c("Entries","Files","Coutries",
                                "States","Municipalities","Citations"),
                    Qt = vector("numeric", 6), stringsAsFactors = F)

qtdds[1,2] <- nrow(allFiles)
qtdds[2,2] <- length(unique(allFiles$file))
qtdds[3,2] <- length(unique(allFiles$country))
qtdds[4,2] <- length(unique(allFiles$state))
qtdds[5,2] <- length(unique(allFiles$municipality))
qtdds[6,2] <- length(unique(allFiles$reference_citation))

# Create Workbook
wb <- createWorkbook()
# Add worksheets
addWorksheet(wb, "tblTx")
addWorksheet(wb, "qtdds")
# add data
writeData(wb, "tblTx", tblTx, rowNames = TRUE)
writeData(wb, "qtdds", qtdds, rowNames = TRUE)
# save xlsx file
saveWorkbook(wb,  paste0("output/", vrsN,"_qtTx.xlsx"), overwrite = TRUE)





###############################################################################  
# graficos apresentando a quantidade de entradas e diversidade das redes
## grafico de barras de quantidade de entradas
###############################################################################
plotDivAbu <- function(gNvplanTx,gNvanimTx){
  qtTtl <- vector("list", length = 15)
  names(qtTtl) <- c("dvPlOr","dvPlFa","dvPlGe",
                    "abPlOr","abPlFa","abPlGe",
                    "dvAnOr","dvAnFa","dvAnGe",
                    "abAnOr","abAnFa","abAnGe",
                    "plantSp","animaSp", "qtCdTx")
  
  # tabela com a quantidade de cada taxon
  tblTx <- data.frame(Morphotype = NA,
                      Species = NA,
                      Genera = NA,
                      Family = NA,
                      Order = NA, stringsAsFactors = F)
  tblTx["Plant", 1] <- length(na.exclude(unique(allFiles$plant_species_complete_name)))
  tblTx["Plant", 2] <- length(na.exclude(unique(allFiles$plant_species)))
  tblTx["Plant", 3] <- length(na.exclude(unique(allFiles$plant_genera)))
  tblTx["Plant", 4] <- length(na.exclude(unique(allFiles$plant_family)))
  tblTx["Plant", 5] <- length(na.exclude(unique(allFiles$plant_order)))
  
  tblTx["Invertebrate", 1] <- length(na.exclude(unique(allFiles$invertebrate_species_complete_name)))
  tblTx["Invertebrate", 2] <- length(na.exclude(unique(allFiles$invertebrate_species)))
  tblTx["Invertebrate", 3] <- length(na.exclude(unique(allFiles$invertebrate_genera)))
  tblTx["Invertebrate", 4] <- length(na.exclude(unique(allFiles$invertebrate_family)))
  tblTx["Invertebrate", 5] <- length(na.exclude(unique(allFiles$invertebrate_order)))
  tblTx <- cbind.data.frame(tblTx, Type = c(NA,"Plant", "Invertebrate"))
  qtTtl$qtCdTx <- tblTx

  # plantas
  
  # tiff(paste0("../output/", vrsN, "_nSpPlant.tif"),
  #      width = 174,height = 233,units = "mm",res = 300)
  png(paste0("output/", vrsN, "_Fig2_First twenty orders families and genera (plant).png"),
      width = 174,height = 233,units = "mm",res = 300)
  par(mfcol= c(3,2), mar= c(2,9,1.8,1)+0.1, mgp= c(1,1,0))
  
  # tira linhas sem sp (com NA como nome da sp)
  temp1 <- unique(gNvplanTx[!is.na(gNvplanTx$species), ])
  temp2 <- temp1[order(temp1$species), ]
  temp2 <- cbind.data.frame(temp2,
                            duploSp = vector("logical", length = nrow(temp2)),
                            notbiSp = vector("logical", length = nrow(temp2)),
                            errorSp = vector("logical", length = nrow(temp2)),
                            allErSp = vector("logical", length = nrow(temp2)))
  for (i in 2:nrow(temp2)){
    if (temp2$species[i] == temp2$species[i-1]){
      temp2$duploSp[i] <- temp2$duploSp[i-1] <- temp2$species[i] == temp2$species[i-1]
    }
    if ( length(unlist(strsplit(temp2$species[i], " "))) != 2 ){
      temp2$notbiSp[i] <- TRUE
    }
  }
  temp2$errorSp <- agrepl(" sp\\.", temp2$species)
  temp2$errorSp[grep(" sp$", temp2$species)] <- TRUE
  temp2$allErSp <- apply(temp2[, 5:8], 1, function(x) sum(x)>0 )
  qtTtl$plantSp <- temp2
  
  for (i in 1:3){
    # tira linhas repetidas
    tempgNvplanTx <- unique(gNvplanTx[!is.na(gNvplanTx[,i]), ])
    # taxon foco sem repeticoes
    temp <- unique(tempgNvplanTx[,i])
    # conta quantas sp tem em cada taxon foco
    soma <- vector(mode = "numeric", length = length(temp))
    for (j in 1:length(temp)){
      soma[j] <- sum(temp1[ ,i] == temp[j], na.rm = TRUE)
    }
    soma <- data.frame(name = temp[order(soma, decreasing =T)], 
                       sum = soma[order(soma, decreasing =T)])
    qtTtl[[i]] <- soma
    tam <- ifelse(nrow(soma) < 20, nrow(soma), 20)
    if(i == 1){
      barplot(soma[20:1,2], names.arg = soma[20:1,1], las = 1, horiz = T,
              beside = T, col = "#008033", border = NA,
              main = paste0("Number of plant species"))
      title(ylab = paste0(colnames(gNvplanTx)[i],
                     " (", tam, " of ",length(soma[,1]),")"),
            line = 8)
    }else{
      barplot(soma[20:1,2], names.arg = soma[20:1,1], las = 1, horiz = T,
              beside = T, col = "#008033", border = NA)
      title(ylab = paste0(colnames(gNvplanTx)[i],
                          " (", tam, " of ",length(soma[,1]),")"),
            line = 8)
    }
  }
  
  for (i in 1:3){
    soma <- as.data.frame(table(gNvplanTx[!is.na(gNvplanTx[,i]),i]))
    colnames(soma) <- c("name","sum")
    soma <- soma[order(soma$sum, decreasing =T),]
    qtTtl[[i+3]] <- soma
    if(i == 1){
      barplot(soma[20:1,2], names.arg = soma[20:1,1], las = 1, horiz = T,
              beside = T, col = "#008033", border = NA,
              main = paste0("Number of plant entries"))
    }else{
      barplot(soma[20:1,2], names.arg = soma[20:1,1], las = 1, horiz = T,
              beside = T, col = "#008033", border = NA)
    }
  }
  dev.off()
  par(mfcol= c(1,1), mar= c(5,4,4,2)+0.1, mgp= c(3,1,0))
  
  
  # animais
  
  png(paste0("output/", vrsN, "_Fig3_First twenty orders families and genera (invertebrate).png"),
      width = 174,height = 233,units = "mm",res = 300)
  par(mfcol= c(3,2), mar= c(2,9,1.8,1)+0.1, mgp= c(1,1,0))
  
  # tira linhas sem sp (com NA como nome da sp)
  temp1 <- unique(gNvanimTx[!is.na(gNvanimTx$species), ])
  temp2 <- temp1[order(temp1$species), ]
  temp2 <- cbind.data.frame(temp2,
                            duploSp = vector("logical", length = nrow(temp2)),
                            notbiSp = vector("logical", length = nrow(temp2)),
                            errorSp = vector("logical", length = nrow(temp2)),
                            allErSp = vector("logical", length = nrow(temp2)))
  for (i in 2:nrow(temp2)){
    if (temp2$species[i] == temp2$species[i-1]){
      temp2$duploSp[i] <- temp2$duploSp[i-1] <- temp2$species[i] == temp2$species[i-1]
    }
    if ( length(unlist(strsplit(temp2$species[i], " "))) != 2 ){
      temp2$notbiSp[i] <- TRUE
    }
  }
  temp2$errorSp <- agrepl(" sp\\.", temp2$species)
  temp2$errorSp[grep(" sp$", temp2$species)] <- TRUE
  temp2$allErSp <- apply(temp2[, 5:8], 1, function(x) sum(x)>0 )
  qtTtl$animaSp <- temp2
  
  for (i in 1:3){
    tempgNvanimTx <- unique(gNvanimTx[!is.na(gNvanimTx[,i]), ])
    temp <- unique(tempgNvanimTx[ ,i])
    soma <- vector(mode = "numeric", length = length(temp))
    for (j in 1:length(temp)){
      soma[j] <- sum(temp1[ ,i] == temp[j])
    }
    soma <- data.frame(name = temp[order(soma, decreasing =T)], 
                       sum = soma[order(soma, decreasing =T)])
    qtTtl[[i+6]] <- soma
    tam <- ifelse(nrow(soma) < 20, nrow(soma), 20)
    if(i == 1){
      barplot(soma[20:1,2], names.arg = soma[20:1,1], las = 1, horiz = T,
              beside = T, col = "#ff2a2a", border = NA,
              main = paste0("Number of invertebrate species"))
      title(ylab = paste0(colnames(gNvplanTx)[i],
                          " (", tam, " of ",length(soma[,1]),")"),
            line = 8)
    }else{
      barplot(soma[20:1,2], names.arg = soma[20:1,1], las = 1, horiz = T,
              beside = T, col = "#ff2a2a", border = NA)
      title(ylab = paste0(colnames(gNvplanTx)[i],
                          " (", tam, " of ",length(soma[,1]),")"),
            line = 8)
    }
  }
  
  for (i in 1:3){
    soma <- as.data.frame(table(gNvanimTx[!is.na(gNvanimTx[,i]),i]))
    colnames(soma) <- c("name","sum")
    soma <- soma[order(soma$sum, decreasing =T),]
    qtTtl[[i+9]] <- soma
    if(i == 1){
      barplot(soma[20:1,2], names.arg = soma[20:1,1], las = 1, horiz = T,
              beside = T, col = "#ff2a2a", border = NA,
              main = paste0("Number of invertebrate entries"))
    }else{
      barplot(soma[20:1,2], names.arg = soma[20:1,1], las = 1, horiz = T,
              beside = T, col = "#ff2a2a", border = NA)
    }
  }
  dev.off()
  par(mfcol= c(1,1), mar= c(5,4,4,2)+0.1, mgp= c(3,1,0))
}

Plt <- allFiles[, c("plant_order","plant_family","plant_genera","plant_species")]
colnames(Plt) <- c("order","family","genera","species")
Anm <- allFiles[, c("invertebrate_order","invertebrate_family","invertebrate_genera","invertebrate_species")]
colnames(Anm) <- c("order","family","genera","species")

plotDivAbu(Plt,Anm)
gNvplanTx <- Plt
gNvanimTx <- Anm



###############################################################################  
# Tabela com as informacoes das variaveis
###############################################################################

### tabela para armazenar as var, 
# valores, qt de valores dif, min. max. media e SD
tam <- vector("character", ncol(allFiles)) # num. de colunas
num <- vector("numeric", ncol(allFiles)) # num. de colunas
tbl1 <- data.frame(var = colnames(allFiles),
                   val = tam,
                   nQt = num,
                   tQt = tam,
                   qts = tam,
                   qNA = num,
                   pNA = num,
                   tNA = tam,
                   min = num,
                   max = num,
                   med = num,
                   sd = num, stringsAsFactors = F)
valores <- vector("list", ncol(allFiles))
names(valores) <- tbl1$var
for(i in 1:ncol(allFiles)){
  temp <- allFiles[, i]
  tbl1$qNA[[i]] <- sum(is.na(allFiles[, i]))
  tbl1$pNA[[i]] <- round(sum(is.na(allFiles[, i]))/nrow(allFiles), 3)
  tbl1$tNA[[i]] <- paste0(sum(is.na(allFiles[, i])),
                          " (", round(sum(is.na(allFiles[, i]))/nrow(allFiles), 
                                      3), 
                          "%)")
  valores[[i]] <- unique(temp)
  tbl1$val[i] <- paste(unique(temp), collapse = "; ")
  tbl1$qts[i] <- length(unique(temp))
  tbl1$nQt[i] <- length(unique(temp))
  tbl1$tQt[i] <- paste0(length(unique(temp)), " (nominal values)")
  temp2 <- temp
  if(i == 11){
    temp2 <- sub("m (.*)", "", temp2) # so para precisao precisa dessa etapa
    temp2 <- sub(" (.*)", "", temp2)
    tbl1$qts[i] <- length(unique(temp2))
  }
  temp2 <- as.numeric(temp2)
  if(!all(is.na(temp2))){
    tbl1$min[i] <- min(temp2, na.rm = T)
    tbl1$max[i] <- max(temp2, na.rm = T)
    tbl1$med[i] <- mean(temp2, na.rm = T)
    tbl1$sd[i] <- sd(temp2, na.rm = T)
    nom <- temp[is.na(temp2)] # quais sao nomes
    nmr <- temp2[!is.na(temp2)] # quais sao numeros
    tbl1$qts[i] <- paste0(tbl1$qts[i], 
                          " (nom: ", length(unique(nom)),
                          ", nmr: ", length(unique(nmr)), ")")
    tbl1$tQt[i] <- paste0(tbl1$nQt[i], 
                          " (", length(unique(nom)) - (tbl1$qNA[[i]] > 0),
                          " nominal values and ", length(unique(nmr)), 
                          " interval values: min. ", tbl1$min[i],
                          ", max. ", tbl1$max[i],
                          ", mean ", round(tbl1$med[i], 2),
                          ", sd. ", round(tbl1$sd[i], 2), ")")
    
  }
}

# apaga algumas contagens q são 0
tbl1$tQt <- gsub("0 nominal values and ", "", tbl1$tQt)
tbl1$qts <- gsub(" \\(nom: 0.*$", "", tbl1$qts)

# salva os arquivos (csv para a tabela com termos padronizados)
# write.xlsx(tbl1, paste0("output/", vrsN, "_table1_auxValues_v02.xlsx"))
write.xlsx(tbl1[,-2], paste0("output/", vrsN, "_table1_auxMedidas.xlsx"))
# temp <- as.list(tbl1$val)
# names(temp) <- tbl1$var
# for(i in 1:length(allFiles)){
#   
# }
write.xlsx(valores, "table1_auxOnlyValues_.xlsx")
