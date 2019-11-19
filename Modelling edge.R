#Modelling the edge of the EAC, accounting for #nets/site
#2019-08-01
#Jessie Bolin
#Updated with BRAN temp

setwd("/Users/jessicabolin/Desktop/Whale analysis/8. Resampling/Edge/.R_Cache")

source("/Users/jessicabolin/Desktop/Whale analysis/9. Modelling/Offset_function.R")
source("/Users/jessicabolin/Desktop/Whale analysis/Source code/Packages + Coordinates.R")
library(lme4); library(ggplot2)

n <- 1000

# reserve space = sneaky 
coefficientdf <- data.frame(modelno = 1:n, 
                            
                            distedgeestimate = numeric(n), pc1estimate = numeric(n), 
                            sstedgeestimate = numeric(n),PC1_varestimate = numeric(n), 
                            interceptestimate = numeric(n))

#Import modelinputs from R_Cache into environment 
for (i in 1:1000) {
  load(paste0("modelinput", i, "@.RData"))
}



# Make lookup table for numnber of nets per site
netLookUp <- data.frame(area = levels(modelinput1$area), Nets = c(11, 2, 3, 9))

# With offset for number of nets
offset <- coefficientdf

i=1

for (i in 1:1000) {
  dat <- get(paste0("modelinput", i))
  dat <- merge(dat, netLookUp, by=c("area"))# merge in number of nets
  model2 <- glmer(PA ~ scale(distedge) + scale(PC1) + scale(sstedge) + scale(PC1_var) + (1|area), family = binomial(link=logexp(dat$Nets)), data = dat)
  #model3 <- update(model2, ~. ^2)
  test <- data.frame(summary(model2)$coefficients)
  offset[i,] <- c(i,
                  test["scale(distedge)",1], test["scale(PC1)", 1], test["scale(sstedge)",1], 
                  test["scale(PC1_var)",1],
                  test["(Intercept)", "Estimate"])
  print(i)  
}

#Delete estimate from the coefficient names
names(offset) <-  unlist(strsplit(names(offset), "estimate"))
library(gridExtra)

lwr <- function(x) quantile(x, 0.025)
upr <- function(x) quantile(x, 0.975)

distedge <- ggplot() +

  geom_histogram(data = offset, aes(x = distedge), fill = "coral4", alpha = 0.2) +
 
  geom_vline(xintercept = lwr(offset$distedge), colour = "coral4", linetype = "dashed") +
  
  geom_vline(xintercept = upr(offset$distedge), colour = "coral4", linetype = "dashed") +
 
  geom_vline(xintercept = median(offset$distedge), colour = "coral4") +
  geom_vline(xintercept = 0)

pc1 <- ggplot() +
 # geom_histogram(data = nooffset, aes(x = pc1), fill = "slateblue", alpha = 0.2) +
  geom_histogram(data = offset, aes(x = pc1), fill = "coral4", alpha = 0.2) +
#  geom_vline(xintercept = lwr(nooffset$pc1), colour = "slateblue", linetype = "dashed") +
  geom_vline(xintercept = lwr(offset$pc1), colour = "coral4", linetype = "dashed") +
#  geom_vline(xintercept = upr(nooffset$pc1), colour = "slateblue", linetype = "dashed") +
  geom_vline(xintercept = upr(offset$pc1), colour = "coral4", linetype = "dashed") +
 # geom_vline(xintercept = median(nooffset$pc1), colour = "slateblue") +
  geom_vline(xintercept = median(offset$pc1), colour = "coral4") +
  geom_vline(xintercept = 0)

sstedge <- ggplot() +
 # geom_histogram(data = nooffset, aes(x = sstedge), fill = "slateblue", alpha = 0.2) +
  geom_histogram(data = offset, aes(x = sstedge), fill = "coral4", alpha = 0.2) +
#  geom_vline(xintercept = lwr(nooffset$sstedge), colour = "slateblue", linetype = "dashed") +
  geom_vline(xintercept = lwr(offset$sstedge), colour = "coral4", linetype = "dashed") +
 # geom_vline(xintercept = upr(nooffset$sstedge), colour = "slateblue", linetype = "dashed") +
  geom_vline(xintercept = upr(offset$sstedge), colour = "coral4", linetype = "dashed") +
#  geom_vline(xintercept = median(nooffset$sstedge), colour = "slateblue") +
  geom_vline(xintercept = median(offset$sstedge), colour = "coral4") +
  geom_vline(xintercept = 0)

PC1_var <- ggplot() +
  #geom_histogram(data = nooffset, aes(x = PC1_var), fill = "slateblue", alpha = 0.2) +
  geom_histogram(data = offset, aes(x = PC1_var), fill = "coral4", alpha = 0.2) +
  #geom_vline(xintercept = lwr(nooffset$PC1_var), colour = "slateblue", linetype = "dashed") +
  geom_vline(xintercept = lwr(offset$PC1_var), colour = "coral4", linetype = "dashed") +
  #geom_vline(xintercept = upr(nooffset$PC1_var), colour = "slateblue", linetype = "dashed") +
  geom_vline(xintercept = upr(offset$PC1_var), colour = "coral4", linetype = "dashed") +
  #geom_vline(xintercept = median(nooffset$PC1_var), colour = "slateblue") +
  geom_vline(xintercept = median(offset$PC1_var), colour = "coral4") +
  geom_vline(xintercept = 0)

grid.arrange(distedge, pc1, sstedge, PC1_var, nrow = 2) #YES the results are the same as RSEC analysis (broadly) 
#PHEWWWWW

write.csv(offset, "/Users/jessicabolin/Desktop/Whale analysis/9. Modelling/Edge/edge_coefficients.csv", row.names = F)

dat <- read.csv("/Users/jessicabolin/Desktop/Whale analysis/9. Modelling/Edge/edge_coefficients.csv")
lwr <- function(x) quantile(x, 0.025); upr <- function(x) quantile(x, 0.975)

median(dat$distedge); lwr(dat$distedge); upr(dat$distedge)
median(dat$sstedge); lwr(dat$sstedge); upr(dat$sstedge)
median(dat$pc1); lwr(dat$pc1); upr(dat$pc1)
median(dat$PC1_var); lwr(dat$PC1_var); upr(dat$PC1_var)

