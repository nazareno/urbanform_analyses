require("plyr")
require("TraMineR")
require("reshape2")
require("ggplot2")
require("GGally")
require("cluster")

setwd('/Users/nazareno/workspace/urbanform_analyses')
source("../lifecourse_analyses/mobility_functions.R")

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

pdf("land_uses-histograms.pdf", width = 46, height = 3 )
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

pdf("scatterplot-matrix-lu.pdf", width = 20, height = 20)
ggpairs(na.omit(lu), columns = 2:17)
dev.off()
#dev.copy(pdf, "scatterplot-matrix-lu.pdf", width = 16, height = 12)

capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
{s <- substring(s, 2); if(strict) tolower(s) else s},
sep = "", collapse = " " )
sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# fica ruim:
# clusters = agnes(lu[,2:17], stand = TRUE, method = "ward")

clusters = agnes(lu[,2:17], stand = FALSE, method = "ward")
clusters.4 = cutree(clusters, 4)
table(clusters.4)
#aggregate(lu[,2:17],list(clusters.4),median)

# melhor ateh agora: 
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

lu[,2:17] <- scale(lu[,2:17])
lu.long <- melt(lu, id.vars = c("ID", "House", "cluster"))
levels(lu.long$variable) <- capwords(levels(lu.long$variable), strict = TRUE)
pdf("lu-clusters-hclust-ward-scaled.pdf", width = 10, height = 15)
ggplot(lu.long, aes(x=variable, y = value, colour=variable)) +
  geom_point(alpha = 0.3, position = position_jitter(width = .2)) +
  facet_grid(cluster ~. ) + geom_boxplot(alpha = 0.7, outlier.colour = lu$variable) + 
  coord_flip()
dev.off()
