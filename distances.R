require("plyr")
require("TraMineR")
require("reshape2")
require("ggplot2")
require("GGally")

setwd('/Users/nazareno/Documents/workspace/urbanform_analyses')
source("../lifecourse_analyses/mobility_functions.R")

OUTPUT_DIR="figures/"

corrigir_distancias <- function(distances){
  distances[distances == -1] <- NA
  distances$House <- as.factor(distances$House)
  distances$ID <- as.factor(distances$ID)
  distances <- distances[tolower(distances$city) %in% get_all_cities_lc(),]
  distances$city <- NULL
  
  # CORREÇÕES
  distances[distances$ID == 14 & distances$House == 1,]$bus_distance <- 260.9
  distances[distances$ID == 95 & distances$House == 1,]$bos_distance <- 2919.11
  distances[distances$ID == 162 & distances$House == 4,]$bos_distance <- 2927.73
  distances[distances$ID == 2 & distances$House == 5,]$bus_distance <- NA
  distances
}

adiciona_fator_niveldedistancia <- function(distances.long){
  within(distances.long, 
       part <- as.factor(cut(value, 
                             DISTANCE_LEVELS, 
                             include.lowest=TRUE)))
}

# Limites inferiores e superiores dos níveis em que separamos as distâncias
# (para perto, médio, longe)
DISTANCE_LEVELS <- c(0, 1, 3, 5, 10, 20, 100)

theme_set(theme_bw())

# Lendo as distâncias de cada casa 
anexa_distancia <- function(df, datafile, housenumber){
  tmp <- read.table(datafile, header = TRUE, fileEncoding = "UTF-16", sep = '\t')
  tmp <- cbind(tmp, House = rep(housenumber, NROW(tmp$ID)))
  rbind(df, tmp);
}  
distances <- read.table('dados/Distances//House1_271112.txt', header = TRUE, fileEncoding = "UTF-16", sep = '\t')
distances <- cbind(distances, House = rep(1, NROW(distances$ID)))
distances <- anexa_distancia(distances, 'dados/Distances//House2_271112.txt', 2)
distances <- anexa_distancia(distances, 'dados/Distances//House3_271112.txt', 3)
distances <- anexa_distancia(distances, 'dados/Distances//House4_271112.txt', 4)
distances <- anexa_distancia(distances, 'dados/Distances//House5_271112.txt', 5)

x<- read.csv('dados/House-postcode-degreejunto.txt')
names(x[,c(1, 36, 25:33, 24)])
names(distances)

# PAREI AQUI. FALTA A CIDADE. E CORRIGIR O ID EM DISTANCE? COMO FICOU? 

distances <- corrigir_distancias(distances)
distances.long <- melt(distances, id.vars = c("ID", "House"))
# m -> km
distances.long$value <- distances.long$value / 1000

cdf <- ddply(distances.long, "variable", summarise, value.mean=mean(value, na.rm = TRUE))

# HISTOGRAMAS:
pdf(paste0(OUTPUT_DIR,"distances-histograms.pdf"), width = 18, height = 3 )
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

pdf(paste0(OUTPUT_DIR,"scatterplot-matrix-distances.pdf"), width = 16, height = 12)
ggpairs(na.omit(distances)/1000, columns = 3:11)
#dev.copy(pdf, "scatterplot-matrix-distances.pdf", width = 16, height = 12)
dev.off()

# TIRANDO SUBRAY E TRAM
distances.long <- subset(distances.long, variable != "subway_distance" & variable != "tram_distance")
distances.long$variable <- droplevels(distances.long$variable)

ggplot(distances.long, aes(x=value)) +
  #geom_histogram(binwidth = 1) + 
  facet_grid(. ~ variable, scales = "free" ) 

distances.long <- adiciona_fator_niveldedistancia(distances.long)
distances.long$part = factor(distances.long$part, rev(levels(distances.long$part)))

# Salvando 3 figuras para aproveitar as melhores escalas
pdf(paste0(OUTPUT_DIR,"distances-histograms-comfaixas1.pdf"), width = 36, height = 3 )
ggplot(distances.long, aes(x=value, fill = part)) +
  geom_histogram( binwidth = .5) + 
  facet_grid(. ~ variable, scales = "free" ) +
  scale_fill_brewer(type = "seq", palette = "PuBu")
dev.off()

pdf(paste0(OUTPUT_DIR,"distances-histograms-comfaixas2.pdf"), width = 36, height = 3 )
ggplot(distances.long, aes(x=value, fill = part)) +
  geom_histogram( binwidth = 1) + 
  facet_grid(. ~ variable, scales = "free" ) +
  scale_fill_brewer(type = "seq", palette = "PuBu")
dev.off()

pdf(paste0(OUTPUT_DIR,"distances-histograms-comfaixas3.pdf"), width = 36, height = 3 )
ggplot(distances.long, aes(x=value, fill = part)) +
  geom_histogram() + 
  facet_grid(. ~ variable, scales = "free" ) +
  scale_fill_brewer(type = "seq", palette = "PuBu")
dev.off()

# Lendo demais dados dos respondents para pegar início e fim das casas e idades
data_file = "dados/verhuisgeschiedenis_350.csv"
resps.f.long.lido <- ler_e_corrigir_casas(data_file) # revisar essa função
resps.f.long.lido$verv1werk <- NULL
resps.f.long.lido$time <- as.factor(resps.f.long.lido$time)

# (lendo distâncias de novo caso a variável tenha sido editada acima)
distances <- read.csv('dados/House-postcode-degreejunto.txt')
distances <- distances[,c(4, 36, 25:33, 24)]
distances <- rename(distances, c(Respondent_ID = "ID"))
distances <- corrigir_distancias(distances)
# TIRANDO SUBRAY E TRAM
distances$subway_distance<- NULL
distances$tram_distance <- NULL

distances$time <- distances$House
distances$House <- NULL
resps.f.long <- merge(resps.f.long.lido, distances, by = c("ID", "time"))

# m -> km
resps.f.long[,6:12] <- resps.f.long[,6:12] / 1000
distances.all <- melt(resps.f.long, id.vars = 1:5, measure.vars= 6:12)
distances.all <- adiciona_fator_niveldedistancia(distances.all)
distances.all$value <- NULL
distances.all <- rename(distances.all, c(variable = "distance", part = "value",  time = "house"))

plot_distance_sequence <- function(distances.all, distance_focused, pal){
  states_alph <- levels(distances.all$value)
  states_pal <- rev(brewer.pal(6, pal))
  
  focused_data <- subset(distances.all, distance == distance_focused)
  
  resps.seq <- seqformat(focused_data, id = "ID", 
                         from = "SPELL", to = "STS", 
                         process = FALSE, 
                         begin = "woonbeg", end = "woonend", status = "value")
  
  resps.seq <- seqdef(resps.seq, 
                      #left = 0, 
                      #right = 0, gaps = 0,
                      alphabet = states_alph)
  
  # comprimento das sequencias excluindo repeticoes consecutivas de estados:
  table(seqlength(seqdss(resps.seq)))
  table(seqlength(seqdss(resps.seq))) / NROW(resps.seq)
  mean(seqlength(seqdss(resps.seq)))
  
  #print(resps.seq[1:10,], format = "STS")  
  just_the_plots(resps.seq, paste0(distance_focused, "-abs"), states_pal)
  
  # POR IDADE
  focused_data.idade <- focused_data[!is.na(focused_data$geborenjaar) & focused_data$geborenjaar != 999,]
  focused_data.idade$woonbeg <- focused_data.idade$woonbeg- focused_data.idade$geborenjaar
  focused_data.idade$woonend <- focused_data.idade$woonend- focused_data.idade$geborenjaar
  
  resps.seq.i <- seqformat(focused_data.idade, id = "ID", 
                           from = "SPELL", to = "STS", 
                           process = FALSE, 
                           begin = "woonbeg", end = "woonend", status = "value")
  
  resps.seq.i <- seqdef(resps.seq.i, 
                        #left = 0, 
                        #right = 0, gaps = 0,
                        alphabet = states_alph)
  just_the_plots(resps.seq.i, paste0(distance_focused, "-age"), states_pal)  
  
  # POR CASA
  focused_wide <- dcast(focused_data, ID ~ house, value.var = "value" )
  resps.seq.casa <- seqdef(focused_wide[,2:ncol(focused_wide)], 
                           id = focused_wide$ID)
  just_the_plots(resps.seq.casa, paste0(distance_focused, "-house"), states_pal)    
  
}

just_the_plots <- function(resps.seq, distance_focused, states_pal){
  pdf(paste0(OUTPUTDIR, "distance-abs-exemplo-", distance_focused,".pdf"), width = 6, height = 4 )
  seqiplot(resps.seq, border = NA, withlegend = "right", cex.legend = 0.5, missing.color = "white", cpal = states_pal)
  dev.off()
  
  pdf(paste0(OUTPUTDIR, "distance-abs-todos-end-", distance_focused,".pdf"), width = 6, height = 4 )
  seqIplot(resps.seq, withlegend = "right", missing.color = "white", sortv = "from.end", cex.legend = 0.5, cpal = states_pal)
  dev.off()
  
  pdf(paste0(OUTPUTDIR, "distance-abs-todos-start-", distance_focused,".pdf"), width = 6, height = 4 )
  seqIplot(resps.seq, withlegend = "right", missing.color = "white", sortv = "from.start", cex.legend = 0.5, cpal = states_pal)
  dev.off()
  
  pdf(paste0(OUTPUTDIR, "distance-dist-", distance_focused,".pdf"), width = 6, height = 4 )
  seqdplot(resps.seq, withlegend = "right", cex.legend = 0.5, cpal = states_pal)
  dev.off()
}

plot_distance_sequence(distances.all, 
                       distance_focused = "ic_distance", 
                       pal = "PuRd")
plot_distance_sequence(distances.all, 
                       distance_focused = "bus_distance", 
                       pal = "BuGn")
plot_distance_sequence(distances.all, 
                       distance_focused = "lt_distance", 
                       pal = "GnBu")
plot_distance_sequence(distances.all, 
                       distance_focused = "park_distance", 
                       pal = "PuBuGn")
plot_distance_sequence(distances.all, 
                       distance_focused = "bos_distance", 
                       pal = "Reds")
plot_distance_sequence(distances.all, 
                       distance_focused = "retail_distance", 
                       pal = "YlGnBu")
plot_distance_sequence(distances.all, 
                       distance_focused = "highway_distance", 
                       pal = "YlOrBr")


#seqfplot(resps.seq,  border = NA, withlegend = "right", missing.color = "white", 
#         weighted = FALSE, cex.legend = 0.5)

pdf(paste0(OUTPUT_DIR, "distance-abs-freq.pdf"), width = 6, height = 4 )
seqdplot(resps.seq, withlegend = "right")
dev.off()
