###############################################################################
################ Figuras para representar os dados do Datapaper ###############
# Gera figuras para base da parte escrita                                     #
# Unidade do que eh uma rede: site_name (ou municipality)                     #
# Unidade de esforco amostral: sampling_effort_hours                          #
# Graficos:                                                                   #
# - rede bipartida geral de ordem animais X ord plantas (img)                 #
# - Histograma de esforço amostral (img)                                      #
# - Freq de Landuse_type_point (img)                                          #
# - sampling_method_detail para fazer nuvem de termos (img)                   #
# - Número de cidades amostradas por estado (img) e (tabela para Qgis)        #
# - Mapa com tamanho da rede formando circulos no mapa (tabela para Qgis)     #
# - Mapa com esforço amostral cumulativo (tabela para Qgis)                   #
###############################################################################
# versao do script e conjunto de dados. Evitar sobrescrever.
#vrsN <- "v03-3"
vrsN <- "semJPincheira"

# define diretorio de trabalho
setwd("<your working diretory path>")

# Biblitecas a serem utilizadas
# install.packages('openxlsx')
library(openxlsx)
# install.packages("rgeos")
library(rgeos)
# install.packages("rgdal")
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


###############################################################################
# IMPORTA OS DADOS PARA FAZER AS ANALISES ABAIXO sem precisar rodar o inicio 
# do script

#allFiles <- read.xlsx("output/_rev_v03_allFiles_corrigido.xlsx")
#allFiles <- read.xlsx("output/_rev_v03_allFiles_corrigido_semChile.xlsx")
allFiles <- read.xlsx("output/_rev_v03_allFiles_corrigido_semJPincheira.xlsx")
###############################################################################



###############################################################################
# Mapas com todos os pontos
###############################################################################
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
par( lend = 1)
legend("topleft", cex = 1.5, pt.cex = c(2, 3), 
       pch= c(19,  NA, NA, NA), 
       col= c("darkorchid3","olivedrab2", "gray75", "gray48"), 
       lwd  = c(NA, 15, 15, 2),
       lty  = c(NA, 1, 1, 1),
       seg.len = c(1, 2, 2, 2),
       legend = c("all coordinates",
                  "Atlantic Forest biome\n",
                  "biogeographical regions\n",
                  "brazilian states") )
dev.off()







###############################################################################  
# rede de interacao por taxons superiores
###############################################################################

# tabela geral de interações
# plantFaXInsectOrder
temp <- allFiles[,c("plant_family", "insect_order")]
gf <- as.matrix(table(temp))

png(paste0("output/_rev_", vrsN, "_redeInteracao_plantFaXInsectOrder.png"),
    width = 600,height = 174,units = "mm",res = 300)
# cores dos nos de cima e das interacoes
temp1 <- ncol(gf)
coresB <- rainbow(temp1, alpha = 0.5)

plotweb(gf, col.high= coresB, col.low= "gray40", # cores dos nos
        bor.col.high= coresB, bor.col.low= "white", # bordas dos nos
        #arrow="down.center", #bor.col.interaction= "grey18", # linhas repre as int
        col.interaction= coresB, bor.col.interaction= coresB, # representacao das int
        ybig = 3.5, low.y=2.5, high.y=4.3, # posicoes (entre as camadas, camada de baixo e a de cima)
        high.lab.dis= 0.05, low.lab.dis= 0.05, # proximidade das label dos nos
        method = "normal", # ordem dos nos 
        text.rot= 90,labsize = 0.8) # direcao do texto)
dev.off()



# plantOrXInsectOrder
temp <- allFiles[,c("plant_order", "insect_order")]
gf <- as.matrix(table(temp))

png(paste0("output/_rev_", vrsN, "_redeInteracao_OrXOr.png"),
    width = 300,height = 174,units = "mm",res = 300)
# cores dos nos de cima e das interacoes
temp1 <- ncol(gf)
coresB <- rainbow(temp1, alpha = 0.5)

plotweb(gf, col.high= coresB, col.low= "gray40", # cores dos nos
               bor.col.high= coresB, bor.col.low= "white", # bordas dos nos
               #arrow="down.center", #bor.col.interaction= "grey18", # linhas repre as int
               col.interaction= coresB, bor.col.interaction= coresB, # representacao das int
               ybig = 3.5, low.y=2.5, high.y=4.3, # posicoes (entre as camadas, camada de baixo e a de cima)
               high.lab.dis= 0.05, low.lab.dis= 0.05, # proximidade das label dos nos
               method = "normal", # ordem dos nos 
               text.rot= 90,labsize = 1.6) # direcao do texto)
dev.off()


# plantOrXInsectOrder (sem Hymenoptera)
temp <- allFiles[,c("plant_order", "insect_order")]
temp <- temp[temp$insect_order != "Hymenoptera",]
gf <- as.matrix(table(temp))

png(paste0("output/_rev_", vrsN, "_redeInteracao_OrXOr-wthtHymenoptera.png"),
    width = 300,height = 174,units = "mm",res = 300)
# cores dos nos de cima e das interacoes
temp1 <- ncol(gf)
coresB <- rainbow(temp1, alpha = 0.5)

plotweb(gf, col.high= coresB, col.low= "gray40", # cores dos nos
               bor.col.high= coresB, bor.col.low= "white", # bordas dos nos
               #arrow="down.center", #bor.col.interaction= "grey18", # linhas repre as int
               col.interaction= coresB, bor.col.interaction= coresB, # representacao das int
               ybig = 3.5, low.y=2.5, high.y=4.3, # posicoes (entre as camadas, camada de baixo e a de cima)
               high.lab.dis= 0.05, low.lab.dis= 0.05, # proximidade das label dos nos
               method = "normal", # ordem dos nos 
               text.rot= 90,labsize = 1.6) # direcao do texto)
dev.off()


# plantOrXInsectOrder (sem abundancia)
temp <- unique(allFiles[,c("plant_order", "insect_order")])
gf <- as.matrix(table(temp))

png(paste0("output/_rev_", vrsN, "_redeInteracao_OrXOr-semAbun.png"),
    width = 300,height = 174,units = "mm",res = 300)
# cores dos nos de cima e das interacoes
temp1 <- ncol(gf)
coresB <- rainbow(temp1, alpha = 0.5)

plotweb(gf, col.high= coresB, col.low= "gray40", # cores dos nos
               bor.col.high= coresB, bor.col.low= "white", # bordas dos nos
               #arrow="down.center", #bor.col.interaction= "grey18", # linhas repre as int
               col.interaction= coresB, bor.col.interaction= coresB, # representacao das int
               ybig = 3.5, low.y=2.5, high.y=4.3, # posicoes (entre as camadas, camada de baixo e a de cima)
               high.lab.dis= 0.05, low.lab.dis= 0.05, # proximidade das label dos nos
               method = "normal", # ordem dos nos 
               text.rot= 90,labsize = 2.8) # direcao do texto)
dev.off()


# plantOrXInsectOrder (num de sp)
temp <- unique(allFiles[,c("plant_order","plant_species_complete_name",
                            "insect_order","insect_species_complete_name")])
temp <- temp[,c("plant_order", "insect_order")]
gf <- as.matrix(table(temp))

png(paste0("output/_rev_", vrsN, "_redeInteracao_OrXOr-entradasUnicas.png"),
    width = 300,height = 174,units = "mm",res = 300)
# cores dos nos de cima e das interacoes
temp1 <- ncol(gf)
coresB <- rainbow(temp1, alpha = 0.5)

plotweb(gf, col.high= coresB, col.low= "gray40", # cores dos nos
               bor.col.high= coresB, bor.col.low= "white", # bordas dos nos
               #arrow="down.center", #bor.col.interaction= "grey18", # linhas repre as int
               col.interaction= coresB, bor.col.interaction= coresB, # representacao das int
               ybig = 3.5, low.y=2.5, high.y=4.3, # posicoes (entre as camadas, camada de baixo e a de cima)
               high.lab.dis= 0.05, low.lab.dis= 0.05, # proximidade das label dos nos
               method = "normal", # ordem dos nos 
               text.rot= 90,labsize = 1.6) # direcao do texto)
dev.off()









###############################################################################  
# Histograma de esforço amostral

# Unidade de esforco amostral: sampling_effort_hours
temp <- unique(allFiles[,c("file", "sampling_effort_hours")])
summary(as.numeric(temp$sampling_effort_hours))
png(paste0("EsforcoAmostral_porDataset_", vrsN, ".png"), 
    width = 95, height = 90,units = "mm",res = 300)
par(mfrow= c(2,1), mar= c(0,3,2,1), mgp= c(1.5,0.5,0.1))
hist(as.numeric(temp$sampling_effort_hours), 
     breaks= 40, xaxp= c(0,1600,16),
     col = "gray50", cex.main= 1,
     ylab= "Frequency", xlab= "", main= "Histogram", # xaxt= "n", 
     xlim = c(0,1600))
par(mar= c(3,3,2,1), mgp= c(1.5,0.5,0.1))
boxplot(as.numeric(temp$sampling_effort_hours), ylim = c(0,1600),
        horizontal = T, frame= F, xlab= "Hours", col = "gray50")
title(main= "Boxplot", line = -1, cex.main= 1)
dev.off()
par(mfrow= c(1,1))










###############################################################################  
# Freq de Landuse_type_point

# quantidades
temp <- unique(allFiles[,c("veg_landuse_type_point")])
temp1 <- length(temp)# subtipos (72)
temp <- gsub("\\(.*\\)", "", temp)
temp <- gsub("  ", " ", temp)
temp <- gsub(" $", "", temp)
temp <- gsub(" ,", ",", temp)
temp <- unique(temp)
temp2 <- length(temp)# tipos (36)

# total de dados (tipo)
png(paste0("veg_landuse_type_point - tdos tipos", vrsN, ".png"), 
    width = 175, height = 100,units = "mm",res = 300)
temp <- allFiles[,c("veg_landuse_type_point")]
temp <- gsub("\\(.*\\)", "", temp)
temp <- gsub("  ", " ", temp)
temp <- gsub(" $", "", temp)
temp <- gsub(" ,", ",", temp)
temp <- sort(table(temp))
par(mfrow= c(1,2), mar= c(10,3,1,0), mgp= c(2,0.5,0))
barplot(temp[1:(temp2/2)], las= 2, ylab= "Frequency")
par(mar= c(10,4,1,0), mgp= c(3,0.5,0))
barplot(temp[((temp2/2)+1):temp2], las= 2, ylab= "Frequency")
dev.off()
par(mfrow= c(1,1))

# total de dados (subtipos)
png(paste0("veg_landuse_type_point - tdos subtipos", vrsN, ".png"), 
    width = 175, height = 175,units = "mm",res = 300)
temp <- allFiles[,c("veg_landuse_type_point")]
#temp <- gsub("\\(.*\\)", "", temp)
temp <- gsub("  ", " ", temp)
temp <- gsub(" $", "", temp)
temp <- gsub(" ,", ",", temp)
temp <- sort(table(temp))
par(mfrow= c(1,2), mar= c(22,3,1,0), mgp= c(2,0.5,0), cex= 0.6)
barplot(temp[1:(temp1/2)], las= 2, ylab= "Frequency")
par(mar= c(22,4,1,0), mgp= c(3,0.5,0), cex= 0.6)
barplot(temp[((temp1/2)+1):temp1], las= 2, ylab= "Frequency")
dev.off()
par(mfrow= c(1,1))


# por municipio (187 municipios) tipos
temp <- unique(allFiles[,c("municipality", "veg_landuse_type_point")])
temp <- temp[,c("veg_landuse_type_point")]
temp <- gsub("\\(.*\\)", "", temp)
temp <- gsub("  ", " ", temp)
temp <- gsub(" $", "", temp)
temp <- gsub(" ,", ",", temp)
temp <- sort(table(temp))
names(temp)[temp<2]
# ocorreram em apenas uma cidade
# [1] "agroforestry system"  "arboretum"            "botanical garden"    
# [4] "fields, Caatinga"     "forest-restinga edge" "forest, agriculture" 
# [7] "mangrove"             "marsh"                "restinga, urban"     
# [10] "savanna, forest"      "urban, agriculture"  
png(paste0("veg_landuse_type_point - municipios tipos", vrsN, ".png"), 
    width = 80, height = 100,units = "mm",res = 300)
par(mar= c(10,3,1,0), mgp= c(2,0.5,0))
barplot(temp[temp>1], las= 2, ylab= "Num. of municipalities")
dev.off()


# por municipio (187 municipios) subtipos
temp <- unique(allFiles[,c("municipality", "veg_landuse_type_point")])
temp <- temp[,c("veg_landuse_type_point")]
#temp <- gsub("\\(.*\\)", "", temp)
temp <- gsub("  ", " ", temp)
temp <- gsub(" $", "", temp)
temp <- gsub(" ,", ",", temp)
temp <- sort(table(temp))
names(temp)[temp<2]
# ocorreram em apenas uma cidade
# [1] "agriculture (coffe)"                                 
# [2] "agriculture (pumpkin)"                               
# [3] "agriculture (sugarcane)"                             
# [4] "agriculture, orchard"                                
# [5] "agroforestry system"                                 
# [6] "arboretum"                                           
# [7] "botanical garden"                                    
# [8] "fields (campo rupestre), savanna (cerrado), Caatinga"
# [9] "forest-restinga edge"                                
# [10] "forest (camping area)"                               
# [11] "forest (conserved primary forest)"                   
# [12] "forest (degraded primary forest)"                    
# [13] "forest (early successional)"                         
# [14] "forest (early)"                                      
# [15] "forest (littoral)"                                   
# [16] "forest (restoration, riparian)"                      
# [17] "forest (restored riparian)"                          
# [18] "forest (restored)"                                   
# [19] "forest (seasonal)"                                   
# [20] "forest (secondary Atlantic)"                         
# [21] "forest (tableland)"                                  
# [22] "forest, agriculture"                                 
# [23] "mangrove (border)"                                   
# [24] "marsh"                                               
# [25] "restinga, urban"                                     
# [26] "savanna (cerrado campo sujo)"                        
# [27] "savanna (cerrado stricto sensu)"                     
# [28] "savanna (cerrado), fields (rocky fields)"            
# [29] "savanna (cerrado), forest"                           
# [30] "savanna (ex situ of native fruit trees in Cerrado)"  
# [31] "urban (garden), agriculture"  
png(paste0("veg_landuse_type_point - municipios subtipos", vrsN, ".png"), 
    width = 95, height = 100,units = "mm",res = 300)
par(mar= c(18,3,1,0), mgp= c(2,0.5,0), cex= 0.6)
barplot(temp[temp>1], las= 2, ylab= "Num. of municipalities")
dev.off()



###############################################################################  
# sampling_method_detail para fazer nuvem de termos
# By Abhirami Sankar 
# https://analyticstraining.com/how-to-create-a-word-cloud-in-r/
temp <- allFiles[,c("file", "sampling_method_detail")]
modi_txt <- paste(temp$sampling_method_detail, collapse = " ")

### Converting the text file into a Corpus 
#(it can then be processed by the tm package)
# A corpus is a collection of documents (although in our case we only have one)
# converte p uma list c/ cd frase num item de uma lista, e cada item da lista 
# tem "content" q eh a frase e "meta" informacoes (idioma, autor...)
modi <- Corpus(VectorSource(modi_txt))
### Data Cleaning
# use tm_map() from the tm package for processing your text
modi_data <- tm_map(modi,stripWhitespace)
modi_data <- tm_map(modi_data,tolower)
modi_data <- tm_map(modi_data,removeNumbers)
modi_data <- tm_map(modi_data,removePunctuation)
modi_data <- tm_map(modi_data,removeWords, stopwords("english"))
# the following words stop words which I wanted to remove
modi_data<-tm_map(modi_data, removeWords, 
                  c("and","the","our","that","for","are","also","more",
                    "has","must","have","should","this","with", 
                    "had", "was", "were","per"))
### Create a Term Document Matrix
#It is a mathematical matrix that describes the frequency of terms that occur 
#in a collection of documents. In a document-term matrix, rows correspond to 
#words in the collection and columns correspond to documents.
#Now we can create a word cloud even without a TDM. But the advantage of using 
#this here is to take a look at the frequency of words.
tdm_modi <- TermDocumentMatrix(modi_data) #Creates a TDM
TDM1 <- as.matrix(tdm_modi) #Convert this into a matrix format
v <- sort(rowSums(TDM1), decreasing = TRUE) #Gives you the frequencies for every word
summary(v)

temp <- as.data.frame(v)
### Create your first word cloud!
png(paste0("wordcloud-td_", vrsN, ".png"), 
    width = 100, height = 100,units = "mm",res = 300)
par(mar= c(0,0,0,0))
wordcloud(modi_data, scale=c(5,0.5), # tam max e min das palavras
          #max.words= 60, # num max de palavras mostradas
          min.freq = 2, # num min de freq p/ a palavra ser mostrada
          random.order= F, # disposicao das palavras
          rot.per= 0.35, # % de palavras c/ rotacao
          use.r.layout=FALSE, 
          colors= brewer.pal(9, "Set1")) # PRGn BrBG
dev.off()


temp <- unique(allFiles[,c("file", "sampling_method_detail")])
modi_txt <- paste(temp$sampling_method_detail, collapse = " ")

### Converting the text file into a Corpus 
#(it can then be processed by the tm package)
# A corpus is a collection of documents (although in our case we only have one)
# converte p uma list c/ cd frase num item de uma lista, e cada item da lista 
# tem "content" q eh a frase e "meta" informacoes (idioma, autor...)
modi <- Corpus(VectorSource(modi_txt))
### Data Cleaning
# use tm_map() from the tm package for processing your text
modi_data <- tm_map(modi,stripWhitespace)
modi_data <- tm_map(modi_data,tolower)
modi_data <- tm_map(modi_data,removeNumbers)
modi_data <- tm_map(modi_data,removePunctuation)
modi_data <- tm_map(modi_data,removeWords, stopwords("english"))
# the following words stop words which I wanted to remove
modi_data<-tm_map(modi_data, removeWords, 
                  c("and","the","our","that","for","are","also","more",
                    "has","must","have","should","this","with", 
                    "had", "was", "were","per"))
### Create a Term Document Matrix
#It is a mathematical matrix that describes the frequency of terms that occur 
#in a collection of documents. In a document-term matrix, rows correspond to 
#words in the collection and columns correspond to documents.
#Now we can create a word cloud even without a TDM. But the advantage of using 
#this here is to take a look at the frequency of words.
tdm_modi <- TermDocumentMatrix(modi_data) #Creates a TDM
TDM1 <- as.matrix(tdm_modi) #Convert this into a matrix format
v <- sort(rowSums(TDM1), decreasing = TRUE) #Gives you the frequencies for every word
summary(v)

temp <- as.data.frame(v)
### Create your first word cloud!
png(paste0("wordcloud-semRep_", vrsN, ".png"), 
    width = 100, height = 100,units = "mm",res = 300)
par(mar= c(0,0,0,0))
wordcloud(modi_data, scale=c(5,0.5), # tam max e min das palavras
          #max.words= 60, # num max de palavras mostradas
          min.freq = 2, # num min de freq p/ a palavra ser mostrada
          random.order= F, # disposicao das palavras
          rot.per= 0.35, # % de palavras c/ rotacao
          use.r.layout=FALSE, 
          colors= brewer.pal(9, "Set1")) # PRGn BrBG
dev.off()









###############################################################################  
# Número de cidades amostradas por estado (img) e (tabela para Qgis)
temp <- unique(allFiles[,c("municipality", "country","state")])
# se nao eh brasil, nome do estado passa a ser o pais
temp$state[temp$country != "Brazil"] <- temp$country[temp$country != "Brazil"]
# tira repeticoes
temp <- unique(temp[,c("municipality", "state")]) # 189
temp1 <- sort(table(temp$state), decreasing = T)
png(paste0("freq municipios por estado - com num", vrsN, ".png"), 
    width = 100, height = 100,units = "mm",res = 300)
par(mar= c(10,3,1,0), mgp= c(2,0.5,0), cex= 0.9)
barplot(temp1, las= 2, ylab= "Num. of municipalities", ylim = c(0,60))
text(x= seq(0.7,(nrow(temp1)*1.2)-0.3,1.2), y= temp1+3, labels = temp1)
title(xlab= "Brazilian state / other country", line = 8)
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
                    "insect_species_complete_name")]
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
                                "insect_species_complete_name")])
# cada rede (483)
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
mean(table(temp1$file)) # 7.318182
sd(table(temp1$file)) # 12.43217

tamRed <- temp1
#write.csv(temp1, paste0("tamRedesEstado_", vrsN, ".csv"))

###############################################################################
# Mapas com todas as redes
# carrega o mata da mata atlantica, estados brasileiros
MataAtlantica <- readOGR(dsn="../Brazil_estados/Atlantic_MMA_miltinho_LEI_WWF_INTERNATIONAL_albers_sad69_LATLONG_WGS84.shp")
BrState <- readOGR(dsn="../Brazil_estados/Brazilian_States_Limits.shp")
amSul <- readOGR(dsn="../Brazil_estados/Lowenberg_Neto_2014.shp")


png(paste0("mapa", vrsN, "tamRedesEst-log.png"), 1000,900)
# tiff(paste0("output/_rev_", vrsN, "todos_os_dados.tif"),
#      width = 1000, height = 900,units = "px") # ,res = 300
par(mar=c(3,3,2,0), lend = 0)

plot(amSul, asp=1, 
     main=paste0(nrow(temp1), " networks (range from ", 
                min(temp1$size), " to ", max(temp1$size),")"), 
     usePolypath = FALSE, cex.main = 2.5,
     col = "gray75", bor = "white", bg = "lightskyblue",
     xlim = c(-84,-30), ylim = c(-56,12))
plot(MataAtlantica, asp=1, usePolypath = FALSE, add = T, 
     col = "olivedrab2", bor = NA)
plot(BrState, asp=1, usePolypath = FALSE, add = T, 
     bor = "gray48")
axis(1, at = seq(-100,-20,10), labels = seq(-100,-20,10)); axis(2)
points(latitude_y~longitude_x, data=temp1, pch=1, 
       col="darkorchid3", cex=log(temp1$size)+0.5)
par( lend = 1)
legend("topleft", cex = 1.5, pt.cex = c(2, 3), 
       pch= c(1,  NA, NA, NA), 
       col= c("darkorchid3","olivedrab2", "gray75", "gray48"), 
       lwd  = c(NA, 15, 15, 2),
       lty  = c(NA, 1, 1, 1),
       seg.len = c(1, 2, 2, 2),
       legend = c("networks",
                  "Atlantic Forest biome\n",
                  "biogeographical regions\n",
                  "brazilian states") )
dev.off()












###############################################################################  
# Mapa com esforço amostral (tabela para Qgis)
# media de horas de estudo (por estado) e SD

# Unidade de esforco amostral: sampling_effort_hours
temp <- unique(allFiles[,c("file", "country","state","sampling_effort_hours")])
# se nao eh brasil, nome do estado passa a ser o pais
temp$state[temp$country != "Brazil"] <- temp$country[temp$country != "Brazil"]
temp <- unique(temp[,c("file", "state", "sampling_effort_hours")])
# quantidade de NAs (73)
sum(is.na(temp$sampling_effort_hours))
# remove NA (resulta em 204)
temp <- temp[!is.na(temp$sampling_effort_hours),]
# calcula a media e o desvio padrao
temp1 <- tapply(as.numeric(temp$sampling_effort_hours), temp$state, 
                mean)
temp2 <- tapply(as.numeric(temp$sampling_effort_hours), temp$state, 
                sd)
temp3 <- cbind.data.frame(state= names(temp1), mean= temp1, sd= temp2)
temp4 <- as.data.frame(table(temp$state))
colnames(temp4) <- c("state","qt.of.values") 
temp3 <- merge(temp4,temp3)
temp3 <- temp3[order(temp3$mean),]
temp$state <- factor(temp$state, levels = temp3$state)
temp3$state <- factor(temp3$state, levels = temp3$state)
#temp3 <- temp3[order(temp3$mean, decreasing = T),]

png(paste0("esforcoAmost-horas_qtValoresDiferentes",vrsN,".png"), 
    width = 100, height = 100,units = "mm",res = 300)
par(mar= c(10,4,1,1), mgp= c(2.5,0.5,0))
boxplot(as.numeric(sampling_effort_hours) ~state, data= temp,
        las= 2, ylab= "Hours", frame= F, xaxt = "n")
axis(1, at= 1:14, las= 2,
     labels = paste0(temp3$state, " (", temp3$qt.of.values, ")"))
title(xlab= "States", line= 8.5)
dev.off()

qtValDifEsfPorEstd <- temp3
#write.csv(temp3,paste0("valoresDifDeEsforcoAmostral_",vrsN,".csv"))


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
temp <- temp[!is.na(temp$sampling_effort_hours),] # 369

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
# vincula as coordenadas ao tamanha das redes
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

#write.csv(porStt,paste0("esforcoAmostral-porEstado_",vrsN,".csv"))


png(paste0("esforcoAmost-horas_porEstado",vrsN,".png"), 
    width = 175, height = 200,units = "mm",res = 300)

layout(matrix(c(1,2,3,3), ncol= 1, nrow= 4))
par(mar= c(1,4,1,1), mgp= c(2.5,0.5,0), cex= 1)
boxplot(temp6$mean~temp6$state,
        las= 2, ylab= "Hours", frame= F, xaxt = "n")
par(mar= c(1,4,1,1), mgp= c(2.5,0.5,0))
barplot(porStt$sum, ylab= "Accumulated hours",las= 2)
par(mar= c(11,4,1,1), mgp= c(2.5,0.5,0.5))
barplot(porStt$qt.Freq, ylab= "N. of networks",las= 2)
axis(1, seq(0.75,(nrow(porStt)*1.2)-0.1,1.2), las= 2,
     labels = paste0(porStt$qt.Var1, " (", porStt$qt.Freq, ")"))
title(xlab= "States", line= 9.5)
dev.off()
par(mfrow= c(1,1)) 






##############################################################################
# salva em excel
porRede <- merge(tamRed,esfrcAmostRed[,c("ID","size","mean","sd")], 
                 by= "ID", all= T)
porRede$ID <- as.numeric(porRede$ID)
colnames(porRede) <- c("ID", "netwk", "file", "state", "network.size", 
                        "latitude_y", "longitude_x", 
                       "n.of.hours.types.(expect.to.be.1)", 
                       "mean(hours)", "sd(hours)")

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
addWorksheet(wb, "byNetwork")
addWorksheet(wb, "byState")
### salva info nas abas
writeData(wb, "byNetwork", porRede)
writeData(wb, "byState", porEstado) 

saveWorkbook(wb, file = paste0("_tabelas-info_", vrsN,
                               format(Sys.time(), "_%Y-%m-d%d-"),
                               gsub(":", "", format(Sys.time(), "%X") ),
                               ".xlsx"))























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
      file = paste0("output/_rev_NumberOfTaxon", vrsN, ".txt"))

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
      file = paste0("output/_rev_AbundancePerTaxon",vrsN, ".txt"))

plotAbundancia(Plt,Anm)