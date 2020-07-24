# Environment setting

rm(list=ls())

library(psych)
library(rela)
library(qgraph)
library(pcalg)



stopifnot(require(Rgraphviz))# needed for all our graph plots

load("R Data/GenderAndStress.RData")

##########
# ASQ  ###
##########
ASQ = rbind(Source1$ASQ, Source2$ASQ, Source3$ASQ)

ASQ.inverted_items = paste("ASQ_", c(33,20,21), sep="")

ASQ[, ASQ.inverted_items] = 7 - ASQ[, ASQ.inverted_items]

ASQ.1.items = paste("ASQ_", c(1,2, 3, 19, 31, 33, 37, 38), sep="")
ASQ.2.items  = paste("ASQ_", c(4,5,16,17,20,21,23,25,26,34), sep="")
ASQ.3.items  = paste("ASQ_", c(6,7,9,10,14,36), sep="")
ASQ.4.items  = paste("ASQ_", c(11,12,13,15,24,27,35), sep="")
ASQ.5.items = paste("ASQ_", c(18,22,28,29, 32,39,40), sep="") # manca il 30

ASQ$ASQ.1.tot = apply(ASQ[, ASQ.1.items ], 1, sum)
ASQ$ASQ.2.tot = apply(ASQ[, ASQ.2.items ], 1, sum)
ASQ$ASQ.3.tot = apply(ASQ[, ASQ.3.items ], 1, sum)
ASQ$ASQ.4.tot = apply(ASQ[, ASQ.4.items ], 1, sum)
ASQ$ASQ.5.tot = apply(ASQ[, ASQ.5.items ], 1, sum)


#########
### MSP #
#########

MSP = rbind(Source1$MSP, Source2$MSP, Source3$MSP[,1:51])

MSP.inverted_items = paste("MSP_", c(22, 24, 43, 49), sep="")
MSP[, MSP.inverted_items] = 5 - MSP[, MSP.inverted_items]
MSP$MSP.tot = apply(MSP[, -grep("ID|Ordine_presentazione", names(MSP))], 1, sum)

#########  
### TAQ
######### 

# get rid of wrong columns
Source2$TAQ = Source2$TAQ[ , !names(Source2$TAQ)%in%c("Col40", "Col41", "Col42")]
Source3$TAQ = Source3$TAQ[ , !names(Source3$TAQ)%in%c("Col40", "Col41", "Col42")]

TAQ = rbind(Source1$TAQ, Source2$TAQ, Source3$TAQ)


TAQ$TAQ_9 <- as.numeric(TAQ$TAQ_9)
TAQ$TAQ_19 <- as.numeric(TAQ$TAQ_19)

TAQ.Samesex.items = paste("TAQ_", c(22,24,27), sep="")
TAQ.Samesex.invitems = paste("TAQ_", c(23, 25, 26), sep="")
TAQ.Oppsex.items = paste("TAQ_", c(28,30,33), sep="")
TAQ.Oppsex.invitems = paste("TAQ_", c(29, 31, 32), sep="")
TAQ.Family.items = paste("TAQ_", c(20, 21), sep="")
TAQ.Family.invitems = paste("TAQ_", c(16, 17, 18, 19), sep="")
TAQ.Partner.items = paste("TAQ_", c(2, 3, 4, 5, 9, 11, 14), sep="")
TAQ.Partner.invitems = paste("TAQ_", c(1, 7, 8), sep="")
TAQ.Stranger.items = paste("TAQ_", c(34, 35, 37), sep="")

TAQ[, TAQ.Samesex.invitems] = 6 - TAQ[, TAQ.Samesex.invitems]
TAQ[, TAQ.Oppsex.invitems] = 6 - TAQ[, TAQ.Oppsex.invitems]
TAQ[, TAQ.Family.invitems] = 6 - TAQ[, TAQ.Family.invitems]
TAQ[, TAQ.Partner.invitems] = 6 - TAQ[, TAQ.Partner.invitems]

TAQ$TAQ.samesex.tot = apply(TAQ[, c(TAQ.Samesex.items, TAQ.Samesex.invitems)], 1, mean, na.rm=T)
TAQ$TAQ.oppsex.tot = apply(TAQ[, c(TAQ.Oppsex.items, TAQ.Oppsex.invitems)], 1, mean, na.rm=T)
TAQ$TAQ.family.tot = apply(TAQ[, c(TAQ.Family.items, TAQ.Family.invitems)], 1, mean, na.rm=T)
TAQ$TAQ.partner.tot = apply(TAQ[, c(TAQ.Partner.items, TAQ.Partner.invitems)], 1, mean, na.rm=T)
TAQ$TAQ.stranger.tot = apply(TAQ[, c(TAQ.Stranger.items)], 1, mean, na.rm=T)

data <- data.frame(TAQ$ID, ASQ$ASQ.1.tot, ASQ$ASQ.2.tot, ASQ$ASQ.3.tot, ASQ$ASQ.4.tot, ASQ$ASQ.5.tot, 
                   MSP$MSP.tot, 
                   TAQ$TAQ.samesex.tot, TAQ$TAQ.oppsex.tot, TAQ$TAQ.family.tot, TAQ$TAQ.partner.tot, TAQ$TAQ.stranger.tot)
colnames(data) <- c("ID", "ASQ_fiducia", "ASQ_disagiointimit", "ASQ_secondarrelazioni", "ASQ_bisognoapprovaz", "ASQ_preoccupazione", 
                    "MSP_stress",
                    "TAQ_SameSex", "TAQ_OppSex", "TAQ_Family", "TAQ_Partner", "TAQ_Stranger")

# Socio-biographical data

Demo = rbind(Source1$Demogr, Source2$Demogr, Source3$Demogr)


# rename age (to avoid problems with effect package)
colnames(Demo)[4] <- "Age"

fdata = merge(data, Demo, by="ID")

 datanet <- fdata[,c(2:12)]
datanet$Gender <-  tolower(substr(fdata$sesso, 1, 1)) == "m"
datanet$Age <- fdata$Age

library(bootnet)

# Simple t-test for gender differences in stress responses
t.test(MSP_stress ~ Gender, data=datanet)

for (col in c(1:13))
{
  datanet[,col] <- scale(datanet[,col])
}

# Linear regression model
summary(lm(MSP_stress ~ ., data=datanet ))

# General network with LASSO regularization
datanet$Gender <- as.numeric(datanet$Gender)
results <- estimateNetwork(datanet, default="EBICglasso", corMethod = "cor_auto", threshold=FALSE, labels=c("ASQ1", "ASQ2", "ASQ3", "ASQ4", "ASQ5", "MSP",
                                                                                                      "TAQ \nSame Sex", "TAQ \nOpposite Sex", "TAQ \nFamily", 
                                                                                                      "TAQ \nPartner", "TAQ \nStranger", "Gender", "Age"))
plot(results, layout="spring", label.cex=2)

# Remove non-significant partial correlations

matrix <- results$results$optwi
rownames(matrix) <- colnames(datanet)
colnames(matrix) <- colnames(datanet)
diag(matrix) <- 1
psych::corr.p(r=matrix, n=nrow(datanet), adj="none")$p

results$results$optnet[psych::corr.p(r=matrix, n=nrow(datanet), adj="none")$p > .05] <- 0
results$results$optwi[psych::corr.p(r=matrix, n=nrow(datanet), adj="none")$p > .05] <- 0
results$graph[psych::corr.p(r=matrix, n=nrow(datanet), adj="none")$p > .05] <- 0

plot(results, layout="spring", label.cex=2)

# Editing the pcalgo package function to make the graph labels readable

newplotPC <- function (x, y, ...)
{
  .local <- function (x, y, main = NULL, zvalue.lwd = FALSE, 
                      lwd.max = 7, labels = NULL, ...) 
  {
    
    attrs <- nodeAttrs <- list()
    p <- numNodes(G <- x@graph)
    if (!is.null(labels)) {
      attrs$node <- list(shape = "ellipse", fixedsize = FALSE, cex=.5, fontsize=22)
      names(labels) <- nodes(G)
      nodeAttrs$label <- labels
    }
    if (zvalue.lwd && numEdges(G) != 0) {
      lwd.mat <- if (is.matrix(Z <- x@zMin) && all(dim(Z) == 
                                                   p)) 
        Z
      else qnorm(x@pMax/2, lower.tail = FALSE)
      lwd.mat <- lwd.max * lwd.mat/max(lwd.mat)
      z <- Rgraphviz::agopen(G, name = "lwdGraph", 
                             nodeAttrs = nodeAttrs, attrs = attrs)
      for (i in seq_along(z@AgEdge)) {
        z@AgEdge[[i]]@lwd <- lwd.mat[as.integer(z@AgEdge[[i]]@head), 
                                     as.integer(z@AgEdge[[i]]@tail)]
        z@AgEdge[[i]]@col <- as.integer(z@AgEdge[[i]]@head) < 0 + 2
      }
      Rgraphviz::plot(z, main = main, ...)
    }
    else {
      Rgraphviz::plot(G, nodeAttrs = nodeAttrs, main = main, 
                      attrs = attrs, ...)
    }
  }
  .local(x, y, ...)
}


# Causal discovery

suffStat <- list(C = cor(datanet, use="pair"), n = nrow(datanet))


pc.gmG <- pc(suffStat, indepTest = gaussCItest,
             p = ncol(datanet), alpha = .01, conservative=FALSE, maj=FALSE, solve= TRUE)

newplotPC(pc.gmG,  labels=c("ASQ1", "ASQ2", "ASQ3", "ASQ4", "ASQ5", "MSP",
                            "TAQ Same Sex", "TAQ Opposite Sex", "TAQ Family", 
                            "TAQ Partner", "TAQ Stranger", "Gender", "Age"))

