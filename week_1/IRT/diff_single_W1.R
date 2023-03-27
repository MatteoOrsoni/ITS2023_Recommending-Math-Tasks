library(readxl)
library(eRm)
library(ltm)
library(difR)
library(psych)
library(irtoys)
library(mirt)
library(latticeExtra)
if(!require('ShinyItemAnalysis')) {
  install.packages('ShinyItemAnalysis')
  library('ShinyItemAnalysis')
}

rm(list=setdiff(ls(), "..."))
path = 'Your path here'
setwd(path)
Data <- read_excel("diff_complete_W1.xlsx")

Data$`idc8201b1b-9ab7-4afc-931e-9bea6a3f5273_4` = as.numeric(Data$`idc8201b1b-9ab7-4afc-931e-9bea6a3f5273_4`)
Data$`idc8201b1b-9ab7-4afc-931e-9bea6a3f5273_5` = as.numeric(Data$`idc8201b1b-9ab7-4afc-931e-9bea6a3f5273_5`)

#View(Data)

response.frequencies(Data[, -1])

## Analysis with MIRT 

Data_red = Data[, -1]

## Analysis on the first 5 items

mirt_two_PL_1_5 = mirt(Data_red[, 1:5], 1, itemtype = "2PL")
coef(mirt_two_PL_1_5, IRTpars = T, simplify = T)
difficulty = coef(mirt_two_PL_1_5, simplify = TRUE, IRTpars = TRUE)$items[, "b"]


plot(mirt_two_PL_1_5, type = "trace")
plot(mirt_two_PL_1_5, type = "info")
ETS_1_5 = plot(mirt_two_PL_1_5) 

## Analysis on the sixth item

mirt_two_PL_6 = mirt(Data_red[, 6:11], 1, itemtype = "2PL")
coef(mirt_two_PL_6, IRTpars = T, simplify = T)
difficulty_6 = coef(mirt_two_PL_6, simplify = TRUE, IRTpars = TRUE)$items[, "b"]

# Average difficulty Item 6

ave_6 <- mean(difficulty_6)

plot(mirt_two_PL_6, type = "trace")
plot(mirt_two_PL_6, type = "info")
ETS_6 = plot(mirt_two_PL_6)

## Analysis on the seventh item

mirt_two_PL_7 = mirt(Data_red[, 12:17], 1, itemtype = "2PL")
coef(mirt_two_PL_7, IRTpars = T, simplify = T)
difficulty_7 = coef(mirt_two_PL_7, simplify = TRUE, IRTpars = TRUE)$items[, "b"]

# Average difficulty Item 7

ave_7 <- mean(difficulty_7)

plot(mirt_two_PL_7, type = "trace")
plot(mirt_two_PL_7, type = "info")
ETS_7 = plot(mirt_two_PL_7) 

## Analysis on the eighth item

mirt_two_PL_8 = mirt(Data_red[, 18:23], 1, itemtype = "2PL")
coef(mirt_two_PL_8, IRTpars = T, simplify = T)
difficulty_8 = coef(mirt_two_PL_8, simplify = TRUE, IRTpars = TRUE)$items[, "b"]


# Average difficulty Item 8

ave_8 <- mean(difficulty_8)

plot(mirt_two_PL_8, type = "trace")
plot(mirt_two_PL_8, type = "info")
ETS_8 = plot(mirt_two_PL_8)

## Analysis on the ninth item

mirt_two_PL_9 = mirt(Data_red[, 24:26], 1, itemtype = "2PL")
coef(mirt_two_PL_9, IRTpars = T, simplify = T)
difficulty_9 = coef(mirt_two_PL_9, simplify = TRUE, IRTpars = TRUE)$items[, "b"]

# Average difficulty Item 9

ave_9 <- mean(difficulty_9)


plot(mirt_two_PL_9, type = "trace")
plot(mirt_two_PL_9, type = "info")
ETS_9 = plot(mirt_two_PL_9)

## Analysis on the tenth item

mirt_two_PL_10 = mirt(Data_red[, 27:29], 1, itemtype = "2PL")
coef(mirt_two_PL_10, IRTpars = T, simplify = T)
difficulty_10 = coef(mirt_two_PL_10, simplify = TRUE, IRTpars = TRUE)$items[, "b"]

# Average difficulty Item 10

ave_10 <- mean(difficulty_10)

plot(mirt_two_PL_10, type = "trace")
plot(mirt_two_PL_10, type = "info")
ETS_10 = plot(mirt_two_PL_10)

## Analysis on the eleventh item

mirt_two_PL_11 = mirt(Data_red[, 30:33], 1, itemtype = "2PL")
coef(mirt_two_PL_11, IRTpars = T, simplify = T)
difficulty_11 = coef(mirt_two_PL_11, simplify = TRUE, IRTpars = TRUE)$items[, "b"]

# Average difficulty Item 11

ave_11 <- mean(difficulty_11)

plot(mirt_two_PL_11, type = "trace")
plot(mirt_two_PL_11, type = "info")
ETS_11 = plot(mirt_two_PL_11)


## General Analysis


mirt_Rasch = mirt(Data_red, 1, itemtype = "Rasch")
coef(mirt_Rasch, IRTpars = T, simplify = T)
difficulty_rasch = coef(mirt_Rasch, simplify = TRUE, IRTpars = TRUE)$items[, "b"]

plot(mirt_Rasch, type = "trace")
plot(mirt_Rasch, type = "info")
ETS_rasch = plot(mirt_Rasch) 

two_PL_0 <- ltm(Data_red ~ z1, IRT.param = TRUE)
coef(two_PL_0)
plot(two_PL_0, type = "ICC", legend = TRUE) 

mirt_two_PL_0 = mirt(Data_red, 1, itemtype = "2PL")
coef(mirt_two_PL_0, IRTpars = T, simplify = T)
difficulty_0 = coef(mirt_two_PL_0, simplify = TRUE, IRTpars = TRUE)$items[, "b"]

plot(mirt_two_PL_0, type = "trace")
plot(mirt_two_PL_0, type = "info")
ETS_0 = plot(mirt_two_PL_0) 


mirt_3_PL_0 = mirt(Data_red, 1, itemtype = "3PL")
coef(mirt_3_PL_0, IRTpars = T, simplify = T)
difficulty_3 = coef(mirt_3_PL_0, simplify = TRUE, IRTpars = TRUE)$items[, "b"]

plot(mirt_3_PL_0, type = "trace", which.items = 1:5, facet_items=FALSE)
plot(mirt_3_PL_0, type = "trace", which.items = 6:11, facet_items=FALSE)
plot(mirt_3_PL_0, type = "trace", which.items = 12:17, facet_items=FALSE)
plot(mirt_3_PL_0, type = "trace", which.items = 18:23, facet_items=FALSE)
plot(mirt_3_PL_0, type = "trace", which.items = 24:26, facet_items=FALSE)
plot(mirt_3_PL_0, type = "trace", which.items = 27:29, facet_items=FALSE)
plot(mirt_3_PL_0, type = "trace", which.items = 30:33, facet_items=FALSE)

plot(mirt_3_PL_0, type = "infotrace", which.items = 1:5, facet_items=FALSE)
plot(mirt_3_PL_0, type = "infotrace", which.items = 6:11, facet_items=FALSE)
plot(mirt_3_PL_0, type = "infotrace", which.items = 12:17, facet_items=FALSE)
plot(mirt_3_PL_0, type = "infotrace", which.items = 18:23, facet_items=FALSE)
plot(mirt_3_PL_0, type = "infotrace", which.items = 24:26, facet_items=FALSE)
plot(mirt_3_PL_0, type = "infotrace", which.items = 27:29, facet_items=FALSE)
plot(mirt_3_PL_0, type = "infotrace", which.items = 30:33, facet_items=FALSE)


ETS_0 = plot(mirt_3_PL_0) 

logLik(mirt_Rasch)
logLik(mirt_two_PL_0)
logLik(mirt_3_PL_0) 


hist(fscores(object = mirt_two_PL_0, method = "EAP"))
theta = as.vector(fscores(mirt_two_PL_0))

ggWrightMap(theta, difficulty_0,
            ylab.theta = "Latent trait", ylab.b = "Difficulty estimates",
            rel_widths = c(2, 1)
)


hist(fscores(object = mirt_Rasch, method = "EAP"))
theta_rasch = as.vector(fscores(mirt_Rasch))

ggWrightMap(theta_rasch, difficulty_rasch,
            ylab.theta = "Latent trait", ylab.b = "Difficulty estimates",
            rel_widths = c(2, 1)
)

hist(fscores(object = mirt_3_PL_0, method = "EAP"))
theta_3 = as.vector(fscores(mirt_3_PL_0))

ggWrightMap(theta_3, difficulty_3,
            ylab.theta = "Latent trait", ylab.b = "Difficulty estimates",
            rel_widths = c(2, 1)
)



