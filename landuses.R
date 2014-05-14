require("plyr")
require("TraMineR")
require("reshape2")
require("ggplot2")
require("GGally")

setwd('/Users/nazareno/Documents/workspace/urbanform_analyses')
source("../lifecourse_analyses/mobility_functions.R")

theme_set(theme_bw())

# Lendo as distâncias de cada casa 
lu <- read.csv('dados/Land uses_H1.csv', sep = ";")
lu2 <- read.csv('dados/Land uses_H2.csv', sep = ";")
lu3 <- read.csv('dados/Land uses_H3.csv', sep = ";")
lu4 <- read.csv('dados/Land uses_H4.csv', sep = ";")
lu5 <- read.csv('dados/Land uses_H5.csv', sep = ";")
lu <- rbind(lu, lu2, lu3, lu4, lu5)
lu <- subset(lu, ID != 322 & House != 1) # casa com valores estranhos
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
ggpairs(na.omit(lu), columns = 2:18)
dev.off()
#dev.copy(pdf, "scatterplot-matrix-lu.pdf", width = 16, height = 12)

