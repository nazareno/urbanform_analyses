require("plyr")
require("TraMineR")
require("reshape2")
require("ggplot2")
require("GGally")
require("cluster")

# gambiarra para a dependência de código entre análises de lifecourse e urban form
setwd('../urbanform_analyses')
source("../lifecourse_analyses/mobility_functions.R")
source("./seq_analysis_constants.R")

get_all_cities_lc <- function(){
  tolower(c("Albrandswaard", "Barendrecht", "Bernisse", "Binnenmaas", "Brielle", "Capelle aan den Ijssel", 
            "Cromstrijen", "Dirksland", "Goedereede", "Hellevoetsluis", "Korendijk", "Krimpen aan den Ijssel", 
            "Lansingerland", "Maassluis", "Middelharnis", "Oostflakkee", "Oud-Beijerland", "Ridderkerk", 
            "Rotterdam", "Schiedam", "Spijkenisse", "Strijen", "Vlaardingen", "Westvoorne", "Zuidplas" ))
}

theme_set(theme_bw())

# Lendo as distâncias de cada casa 
lu <- read.csv('dados/Land uses_H1.csv', sep = ";")
lu2 <- read.csv('dados/Land uses_H2.csv', sep = ";")
lu3 <- read.csv('dados/Land uses_H3.csv', sep = ";")
lu4 <- read.csv('dados/Land uses_H4.csv', sep = ";")
lu5 <- read.csv('dados/Land uses_H5.csv', sep = ";")
lu <- rbind(lu, lu2, lu3, lu4, lu5)
lu <- subset(lu, ! (ID == 322 & House == 1)) # casa com valores estranhos
lu <- lu[,c(5, 2, 25, 26, 28:44)]
lu$House <- as.factor(lu$House)
lu$ID <- as.factor(lu$Respondent_ID)
lu <- lu[tolower(lu$City) %in% get_all_cities_lc(),]
lu$City <- NULL
lu$Respondent_ID <- NULL
lu$x <- NULL
lu$SUM <- rowSums(lu[,2:17])
lu[,2:17] <- lu[,2:17] / lu[,18]
lu$SUM <- NULL

lu.long <- melt(lu, id.vars = c("ID", "House"))

cdf <- ddply(lu.long, "variable", summarise, value.mean=mean(value, na.rm = TRUE))

pdf("figures/land_uses-histograms.pdf", width = 46, height = 3 )
lu.long.p <- lu.long
#lu.long.p[!is.na(lu.long.p$value) & lu.long.p$value >= 30,]$value <- 30
ggplot(lu.long.p, aes(x=value)) +
  geom_histogram(binwidth = .1,
                 colour="salmon2", fill="lightsalmon2") +
  facet_grid(. ~ variable ) +
  geom_vline(data=cdf, aes(xintercept=value.mean),
             linetype="dashed", size=0.5, colour="grey") #+
  #scale_x_continuous(labels= c(seq(0, 29, by=10), "30+"))
dev.off()

pdf("figures/scatterplot-matrix-lu.pdf", width = 20, height = 20)
ggpairs(na.omit(lu), columns = 2:17)
dev.off()
#dev.copy(pdf, "scatterplot-matrix-lu.pdf", width = 16, height = 12)

# Tentamos essa opção, mas fez menos sentido: 
#clusters = agnes(lu[,2:17], stand = TRUE, method = "ward")
#clusters.4 = cutree(clusters, 6)
#table(clusters.4)
#lu[,2:17] <- scale(lu[,2:17])

# O melhor ateh agora é sem normalização:
clusters = hclust(dist(lu[,2:17]), method = "ward")
clusters.4 = cutree(clusters, 4)
table(clusters.4)

lu$cluster <- paste("Cluster", clusters.4)
lu.long <- melt(lu, id.vars = c("ID", "House", "cluster"))
levels(lu.long$variable) <- capwords(levels(lu.long$variable), strict = TRUE)

pdf("lu-clusters-hclust-ward-5.pdf", width = 10, height = 15)
ggplot(lu.long, aes(x=variable, y = value, colour=variable)) +
  geom_point(alpha = 0.3, position = position_jitter(width = .2)) +
  facet_grid(cluster ~. ) + geom_boxplot(alpha = 0.7, outlier.colour = lu$variable) + 
  coord_flip()
dev.off()
#scale_x_continuous(labels= c(seq(0, 29, by=10), "30+"))

pdf("figures/parcoord-lu.pdf", width = 20, height = 5)
ggparcoord(lu, columns = 2:17, 
           alphaLines = 0.07, 
           scale = "globalminmax", 
           order = "skewness", 
           groupColumn = "cluster")
dev.off()

# ------------------
# Colocando o LU junto com outras informações da casa para fazer um life trajectory
lut <- data.frame(ID = lu[,"ID"], time = lu[,"House"], cluster = lu[,"cluster"])
lut <- lut[!duplicated(lut),]

data_file = "dados/verhuisgeschiedenis_350.csv"
resps.f.long <- ler_e_corrigir_casas(data_file)
resps.f.long$verv1werk <- NULL
resps.f.long <- merge(resps.f.long, lut, by = c("ID", "time"))

todos_os_plots <- function(resps.seq, strindex){
  states_pal = LU_PAL # from the seq analysis constants file
  
  pdf(paste0("lu-abs-",strindex,"-exemplo.pdf"), width = 6, height = 4 )
  seqiplot(resps.seq, border = NA, withlegend = "right", cex.legend = 0.5, cpal = states_pal, missing.color = "white")
  dev.off()
  
  pdf(paste0("lu-abs-", strindex, "-todos-start.pdf"), width = 6, height = 4 )
  seqIplot(resps.seq, withlegend = "right", missing.color = "white", sortv = "from.start", cex.legend = 0.5, cpal = states_pal)
  dev.off()
  
  pdf(paste0("lu-abs-", strindex, "-todos-end.pdf"), width = 6, height = 4 )
  seqIplot(resps.seq, withlegend = "right", missing.color = "white", sortv = "from.end", cex.legend = 0.5, cpal = states_pal)
  dev.off()
  
  #seqfplot(resps.seq,  border = NA, withlegend = "right", missing.color = "white", 
  #         weighted = FALSE, cex.legend = 0.5, cpal = states_pal)
  
  pdf(paste0("lu-distribution-",strindex,".pdf"), width = 6, height = 4 )
  seqdplot(resps.seq, withlegend = "right", cex.legend = 0.8, cpal = states_pal)
  dev.off()
  
}

# Baseado em casas
lut.wide <- dcast(lut, ID ~ time, value.var = "cluster" )
resps.seq.casa <- seqdef(lut.wide[,2:ncol(lut.wide)], 
                         id = lut.wide$ID, 
                         alphabet = levels(lut$cluster))

todos_os_plots(resps.seq.casa, "index")

table(seqlength(seqdss(resps.seq.casa)))
table(seqlength(seqdss(resps.seq.casa))) / NROW(resps.seq.casa)
mean(seqlength(seqdss(resps.seq.casa)))

# DOU baseado em idade
resps.f.long.idade <- resps.f.long[!is.na(resps.f.long$geborenjaar) & resps.f.long$geborenjaar != 999,]
resps.f.long.idade$woonbeg <- resps.f.long.idade$woonbeg- resps.f.long.idade$geborenjaar
resps.f.long.idade$woonend <- resps.f.long.idade$woonend- resps.f.long.idade$geborenjaar

resps.seq.i <- seqformat(resps.f.long.idade, id = "ID", 
                         from = "SPELL", to = "STS", 
                         process = FALSE, 
                         begin = "woonbeg", end = "woonend", status = "cluster")

resps.seq.i <- seqdef(resps.seq.i)
#left = 0, 
#right = 0, gaps = 0,

todos_os_plots(resps.seq.i, "idade")


# ====================
# DOU baseado em anos
resps.seq <- seqformat(resps.f.long, id = "ID", 
                       from = "SPELL", to = "STS", 
                       process = FALSE, 
                       begin = "woonbeg", end = "woonend", status = "cluster")

resps.seq <- seqdef(resps.seq, 
                    #left = 0, 
                    #right = 0, gaps = 0
)

todos_os_plots(resps.seq, "ano")


# ANALISE DE EVENTOS
#
resps.seq.pe <- seqformat(resps.f.long, id = "ID", 
                          from = "SPELL", to = "STS", 
                          process = FALSE, 
                          begin = "woonbeg", end = "woonend", status = "cluster")

resps.seq.pe <- seqdef(resps.seq.pe, 
                       left = "DEL", right = "DEL",
                       alphabet = levels(resps.f.long$cluster))

resps.seq.e <- seqecreate(resps.seq.pe)
seqefsub(resps.seq.e, minSupport = 5, maxK = 1)

resps.ldist <- seqistatd(resps.seq)
n.states <- apply(resps.ldist,1,function(x) sum(x != 0))


