#
# Análise descritiva do DOU. 
#
require("plyr")
require("TraMineR")
require("reshape2")

setwd('../urbanform_analyses')
source("./seq_analysis_constants.R")
source("../lifecourse_analyses/mobility_functions.R")

todos_os_plots <- function(resps.seq, strindex){
  states_pal = DOU_PAL # from the constants file
  
  pdf(paste0("uf-abs-",strindex,"-exemplo.pdf"), width = 6, height = 4 )
  seqiplot(resps.seq, border = NA, withlegend = "right", cex.legend = 0.5, cpal = states_pal, missing.color = "white")
  dev.off()
  
  pdf(paste0("uf-abs-", strindex, "-todos-start.pdf"), width = 6, height = 4 )
  seqIplot(resps.seq, withlegend = "right", missing.color = "white", sortv = "from.start", cex.legend = 0.5, cpal = states_pal)
  dev.off()
  
  pdf(paste0("uf-abs-", strindex, "-todos-end.pdf"), width = 6, height = 4 )
  seqIplot(resps.seq, withlegend = "right", missing.color = "white", sortv = "from.end", cex.legend = 0.5, cpal = states_pal)
  dev.off()
  
  #seqfplot(resps.seq,  border = NA, withlegend = "right", missing.color = "white", 
  #         weighted = FALSE, cex.legend = 0.5, cpal = states_pal)
  
  pdf(paste0("uf-distribution-",strindex,".pdf"), width = 6, height = 4 )
  seqdplot(resps.seq, withlegend = "right", cex.legend = 0.8, cpal = states_pal)
  dev.off()
  
}


# Lendo degree of urbanization de cada casa 
degrees <- read.csv('dados/House-postcode-degreejunto.txt')
degrees <- degrees[,c(4, 38, 39)] # Mudei para usar o RespondentID. 
degrees$ID <- degrees$Respondent_ID
degrees$Respondent_ID <- NULL
degrees$dou <- degrees$Degree.of.urbanization
degrees$Degree.of.urbanization <- NULL
levels(degrees$dou) <- cbind(levels(degrees$dou), 999)
degrees[degrees$dou == ".",]$dou <- 999
degrees$dou <- as.factor(degrees$dou)
degrees$dou <- droplevels(degrees$dou)

# fix some duplicates
degrees <- degrees[c(-43, -135, -749, -750),]

degrees.wide <- dcast(degrees, ID ~ House, value.var = "dou" )

degrees$time <- degrees$House
degrees$House <- NULL

# Lendo demais dados dos respondents para pegar início e fim das casas e idades
data_file = "dados/verhuisgeschiedenis_350.csv"
resps.f.long <- ler_e_corrigir_casas(data_file)
resps.f.long$verv1werk <- NULL
resps.f.long <- merge(resps.f.long, degrees, by = c("ID", "time"), all.x = TRUE)
resps.f.long$dou <- replace(resps.f.long$dou, is.na(resps.f.long$dou), 999)

# Distribuição do número de estados diferentes:
numbers_of_states <- ddply(resps.f.long[,c(1,6)], .(ID), summarise, count = length(unique(dou)))
total_respondents <- NROW(unique(resps.f.long[,c(1)]))
table(numbers_of_states$count) / total_respondents
print(paste("média: ", mean(numbers_of_states$count)))

# Criando sequências para o DOU baseado em casas
degrees.seq <- seqformat(resps.f.long, id = "ID", 
                       from = "SPELL", to = "STS", 
                       process = FALSE, 
                       begin = "woonbeg", end = "woonend", status = "dou")

state_labels.dou <- c("1", "2", "3", "4", "5", "missing")

degrees.seq <- seqdef(degrees.wide[,2:6], 
                      id = degrees.wide$ID, labels = state_labels.dou)
todos_os_plots(degrees.seq, "index")

table(seqlength(seqdss(degrees.seq)))
table(seqlength(seqdss(degrees.seq))) / NROW(degrees.seq)
mean(seqlength(seqdss(degrees.seq)))



# ====================
# DOU baseado em anos
resps.seq <- seqformat(resps.f.long, id = "ID", 
                       from = "SPELL", to = "STS", 
                       process = FALSE, 
                       begin = "woonbeg", end = "woonend", status = "dou")

resps.seq <- seqdef(resps.seq, 
                    #left = 0, 
                    #right = 0, gaps = 0
                    )

todos_os_plots(resps.seq, "ano")

# DOU baseado em idade
resps.f.long.idade <- resps.f.long[!is.na(resps.f.long$geborenjaar) & resps.f.long$geborenjaar != 999,]
resps.f.long.idade$woonbeg <- resps.f.long.idade$woonbeg- resps.f.long.idade$geborenjaar
resps.f.long.idade$woonend <- resps.f.long.idade$woonend- resps.f.long.idade$geborenjaar

resps.seq.i <- seqformat(resps.f.long.idade, id = "ID", 
                         from = "SPELL", to = "STS", 
                         process = FALSE, 
                         begin = "woonbeg", end = "woonend", status = "dou")

resps.seq.i <- seqdef(resps.seq.i)
                      #left = 0, 
                      #right = 0, gaps = 0,

todos_os_plots(resps.seq.i, "idade")



# Event analysis
resps.seq.pe <- seqformat(resps.f.long, id = "ID", 
                          from = "SPELL", to = "STS", 
                          process = FALSE, 
                          begin = "woonbeg", end = "woonend", status = "dou")

resps.seq.pe <- seqdef(resps.seq, 
                       left = "DEL", right = "DEL", void = "%", missing = "*",
                       alphabet = c("1", "2", "3", "4", "5", "999"), 
                       labels = c("1", "2", "3", "4", "5", "999"))


resps.seq.e <- seqecreate(resps.seq.pe)
seqefsub(resps.seq.e, minSupport = 3, maxK = 1)

resps.ldist <- seqistatd(resps.seq)
n.states <- apply(resps.ldist,1,function(x) sum(x != 0))
