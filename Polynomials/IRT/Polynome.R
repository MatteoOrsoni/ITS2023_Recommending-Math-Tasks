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
Data <- read_excel("Polynome_db_v2.xlsx")
str(Data)

chars <- sapply(Data, is.character)
Data[ , chars] <- as.data.frame(apply(Data[ , chars], 2, as.numeric))

response.frequencies(Data)

## Analysis with MIRT 

Data_red = Data

## General Analyses

Data_red = Data

mirt_two_PL_0 = mirt(Data_red, 1, itemtype = "2PL")
coef(mirt_two_PL_0, IRTpars = T, simplify = T)
difficulty_0 = coef(mirt_two_PL_0, simplify = TRUE, IRTpars = TRUE)$items[, "b"]

plot(mirt_two_PL_0, type = "trace")
plot(mirt_two_PL_0, type = "info")
ETS_0 = plot(mirt_two_PL_0) 
ETS_0$panel.args


mirt_Rasch = mirt(Data_red, 1, itemtype = "Rasch")
coef(mirt_Rasch, IRTpars = T, simplify = T)
difficulty_rasch = coef(mirt_Rasch, simplify = TRUE, IRTpars = TRUE)$items[, "b"]

plot(mirt_Rasch, type = "trace")
plot(mirt_Rasch, type = "info")
ETS_rasch = plot(mirt_Rasch)
ETS_rasch$panel.args

mirt_3_PL_0 = mirt(Data_red, 1, itemtype = "3PL")
coef(mirt_3_PL_0, IRTpars = T, simplify = T)
difficulty_3 = coef(mirt_3_PL_0, simplify = TRUE, IRTpars = TRUE)$items[, "b"]
ETS_3PL = plot(mirt_3_PL_0)
str(ETS_3PL)
ETS_3PL$panel.args


anova(mirt_Rasch, mirt_two_PL_0, mirt_3_PL_0)

logLik(mirt_Rasch)
logLik(mirt_two_PL_0)
logLik(mirt_3_PL_0) # Best Model

plot(mirt_3_PL_0, type = "info", facet_items=FALSE)


hist(fscores(object = mirt_3_PL_0, method = "EAP"))
theta_3 = as.vector(fscores(mirt_3_PL_0))

ggWrightMap(theta_3, difficulty_3,
            ylab.theta = "Latent trait", ylab.b = "Difficulty estimates",
            rel_widths = c(2, 1)
)


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


difficulty_db = data.frame(difficulty_rasch[1], difficulty_rasch[2], difficulty_rasch[3], difficulty_rasch[4])
colnames(difficulty_db) <- c('Item_1', 'Item_2','Item_3','Item_4')
rownames(difficulty_db) <- 'difficulty_test'


View(difficulty_db)

write.csv(difficulty_db, 'Difficulty_Polynome.csv')


