require("plyr")
require("TraMineR")
require("reshape2")
require("ggplot2")
require("GGally")

setwd('/Users/nazareno/Documents/workspace/urbanform_analyses')
source("../lifecourse_analyses/mobility_functions.R")

theme_set(theme_bw())

# Lendo as distâncias de cada casa 
distances <- read.csv('dados/House-postcode-degreejunto.txt')
distances <- distances[,c(1, 36, 25:33, 24)]
distances[distances == -1] <- NA
distances$House <- as.factor(distances$House)
distances$ID <- as.factor(distances$ID)
distances <- distances[tolower(distances$city) %in% tolower(c("Albrandswaard", 
                                         "Barendrecht", 
                                         "Bernisse", 
                                         "Binnenmaas", 
                                         "Brielle", 
                                         "Capelle aan den Ijssel", 
                                         "Cromstrijen", 
                                         "Dirksland", 
                                         "Goedereede", 
                                         "Hellevoetsluis", 
                                         "Korendijk", 
                                         "Krimpen aan den Ijssel", 
                                         "Lansingerland", 
                                         "Maassluis", 
                                         "Middelharnis", 
                                         "Oostflakkee", 
                                         "Oud-Beijerland", 
                                         "Ridderkerk", 
                                         "Rotterdam",
                                          "Rozenburg",
                                         "Schiedam", 
                                         "Spijkenisse", 
                                         "Strijen", 
                                         "Vlaardingen",  
                                         "Westvoorne", 
                                         "Zuidplas" )),]
distances$city <- NULL

# CORREÇÕES
distances[distances$ID == 46 & distances$House == 4,]$bos_distance <- 2927.73
distances[distances$ID == 261 & distances$House == 1,]$bos_distance <- 2919.11
distances[distances$ID == 336 & distances$House == 1,]$bus_distance <- 260.9
distances[distances$ID == 4 & distances$House == 5,]$bus_distance <- NA

distances.long <- melt(distances, id.vars = c("ID", "House"))

# m -> km
distances.long$value <- distances.long$value / 1000
cdf <- ddply(distances.long, "variable", summarise, value.mean=mean(value, na.rm = TRUE))

# HISTOGRAMAS:
pdf("distances-histograms.pdf", width = 18, height = 3 )
# apenas para plotar:
distances.long.p <- distances.long
distances.long.p[!is.na(distances.long.p$value) & distances.long.p$value >= 30,]$value <- 30
ggplot(distances.long.p, aes(x=value)) +
  geom_histogram(breaks=c(seq(0, 30, by=1)), position = "identity",
                 colour="darkblue", fill="lightblue") +
  facet_grid(. ~ variable ) +
  geom_vline(data=cdf, aes(xintercept=value.mean),
             linetype="dashed", size=0.5, colour="grey") +
  scale_x_continuous(labels= c(seq(0, 29, by=10), "30+"))
dev.off()

pdf("scatterplot-matrix-distances.pdf", width = 16, height = 12)
ggpairs(na.omit(distances)/1000, columns = 3:11)
#dev.copy(pdf, "scatterplot-matrix-distances.pdf", width = 16, height = 12)
dev.off()

# PAREI AQUI.
# REDUZIR DIMENSÕES?

# Lendo demais dados dos respondents para pegar início e fim das casas e idades
data_file = "dados/verhuisgeschiedenis_350.csv"
resps.f.long <- ler_e_corrigir_casas(data_file)
distances$time <- distances$House
distances$House <- NULL
resps.f.long$verv1werk <- NULL
resps.f.long <- merge(resps.f.long, distances, by = c("ID", "time"))
