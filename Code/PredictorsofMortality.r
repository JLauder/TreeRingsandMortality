#Models of Tree Mortality Likelihood During and After Drought
#Script in support of Lauder and Moran (in prep): Growth Variability and Xylem Traits as Predictors of Mortality During Extreme Drought

#Authors: J. Lauder, E. Moran
#Date created:
#Date last edited: 

#Description:

#
library(zoo)
library(plyr)
library(bayesplot)
library(ggmcmc)
library(tidyverse)
library(tidybayes)
library(ggplot2)
library(brms)
library(bayestestR)
library(dplyr)
library(data.table)



setwd("~")
# Alltrees.final <- read.csv("AlltreesLiveDeadFinal.csv")
# #Alltrees.finalh <- read.csv("AlltreesLiveDeadFinalcomp.csv")
# Trachs <- read.csv("Trachs.csv")
# Trachs[,25] <- gsub(" ","",Trachs[,25], perl = TRUE)
# Trachs$ID <- substr(Trachs$ID, 1, 6)

# #Read in competition
# hegyi.all <- read.csv("HegyiAll.csv")
# hegyi.all$ID <- paste0(hegyi.all$Site,hegyi.all$Tag.)

# # write.csv(unique(Trachs$ID), file = ".csv")
# # write.csv(unique(Alltrees.final$ID), file = ".csv")

# #Trim Trachs
# Trachs <- Trachs[,c(3,9,11,12,19,20,22,23,24,25)]
# Trachs$HSFsl <- Trachs$HorizontalPosition/Trachs$HSF

# #Correct incorrect IDs
# unique(Trachs$ID)
# Trachs[Trachs$ID == "SJP91A",10] <- "SJP091"
# Trachs[Trachs$ID == "SJP69A",10] <- "SJP069"

# #Summarize if dataset is too large to run
# Trachs$ID <- as.factor(Trachs$ID)
# Trachs2 <- ddply(Trachs, c("ID", "Year"), summarize,
#                  mHSF = mean(HSF, na.rm = TRUE),
#                  sdHSF = sd(HSF, na.rm = TRUE),
#                  mLumenLength = mean(LumenLength, na.rm = TRUE),
#                  sdLumenLength = sd(LumenLength, na.rm = TRUE),
#                  mdblwall = mean(dblwall, na.rm = TRUE),
#                  sddblwall = sd(dblwall, na.rm = TRUE),
#                  mHSFsl = mean(HSFsl, na.rm = TRUE),
#                  sdHSFsl = sd(HSFsl, na.rm = TRUE))

# #For final "clean" dataset: merge ring widths and trachs, keeping all observations
# Alltrees.final2 <- merge(Alltrees.final, Trachs2, by = c("ID","Year"), all = TRUE)
# #Then impute missing trachs and Ring widths, but save one un-imputed version
# #First trim to reduce total number of "weights" (variables)
# Alltrees.final2 <- Alltrees.final2[,c(1,2,4,5,7,8,17,100,103,135,137,140,141,143,150,153,154,168:175)]
# library(mice)
# Alltrees.final2i <- parlmice(Alltrees.final2, m = 5, n.core = 4, method = "cart")

# #Then merge in hegyi and impute missing cindex for one version, keeping only values from Alltrees.final2
# final <- merge(Alltrees.final2, hegyi.all, by = "ID", all.x = TRUE)
# finali <- merge(complete(Alltrees.final2i), hegyi.all, by = "ID", all.x = TRUE)

# #Drop unnecessary columns
# names(final)
# final <- final[,c(1:25,30,34)]
# finali <- finali[,c(1:25,30,34)]

# #Remove inf in cindex
# final[,27][is.infinite(final[,27])] = NA
# finali[,27][is.infinite(finali[,27])] = NA

# #Impute missing cindex
# finalii <- parlmice(finali, m = 5, n.core = 4, method = "cart")
# final1i <- complete(finalii)
# final1 <- final

# #Add T-1 and T-2 columns
# final1 <- final1[with(final1, order(ID, Year)), ]
# final1i <- final1i[with(final1i, order(ID, Year)), ]
# # final1$TotID <- paste0(final1$Year,final1$ID)
# # final1 <- transform(final1,TotID=as.numeric(factor(TotID)))
# # final2 <- aggregate(.~c(TotID), final1[,c(6:27)], mean)
# # 
# # final1 <- merge(final1, final2, by = "TotID", all = TRUE)
# #Delete duplicates
# final1 <- final1 %>% group_by(ID,Year) %>% mutate_each(mean, -(1:5)) %>% distinct
# final1i <- final1i %>% group_by(ID,Year) %>% mutate_each(mean, -(1:5)) %>% distinct

# #Reorder again, then add T-1 and T-2 columns
# final1 <- final1[with(final1, order(ID, Year)), ]
# final1 <- as.data.frame(final1)

# final1i <- final1i[with(final1i, order(ID, Year)), ]
# final1i <- as.data.frame(final1i)

# final2 <- as.data.frame(rollapplyr(final1[,c(6:25)], 2, function(x) x[2]))
# final3 <- as.data.frame(rollapplyr(final2, 2, function(x) x[2]))

# final2i <- as.data.frame(rollapplyr(final1i[,c(6:25)], 2, function(x) x[2]))
# final3i <- as.data.frame(rollapplyr(final2i, 2, function(x) x[2]))


# #Make same number of rows as final dataset (fill with NAs for current year)
# final2 <- rbind(NA, final2)
# final3 <- rbind(NA, NA, final3)

# colnames(final2) <- paste0(colnames(final2), "1")
# colnames(final3) <- paste0(colnames(final3), "2")

# final1 <- cbind(final1, final2, final3)

# final2i <- rbind(NA, final2i)
# final3i <- rbind(NA, NA, final3i)

# colnames(final2i) <- paste0(colnames(final2i), "1")
# colnames(final3i) <- paste0(colnames(final3i), "2")

# final1i <- cbind(final1i, final2i, final3i)

# #Calculate correlations between ring width and climate, and HSF and climate, for each tree
# final1.M <- final1 %>% group_by(ID) %>% 
#   summarise(RWIDEF = cor(RWI, TerraDEF),
#             RWIDEF1 = cor(RWI, TerraDEF1),
#             RWICWD = cor(RWI,tCWD),
#             RWICWD1 = cor(RWI,tCWD1),
#             RWIPET = cor(RWI,PmET),
#             RWIPET1 = cor(RWI,PmET1),
#             RWICMD = cor(RWI, CMD),
#             RWICMD1 = cor(RWI,CMD1),
#             RWIPDSI = cor(RWI, TerraPDSI),
#             RWIPDSI1 = cor(RWI,TerraPDSI1),
#             RWITMN = cor(RWI, mTMN),
#             RWITMN1 = cor(RWI,mTMN1),
#             mHSFDEF = cor(mHSF, TerraDEF),
#             mHSFDEF1 = cor(mHSF, TerraDEF1),
#             mHSFCWD = cor(mHSF,tCWD),
#             mHSFCWD1 = cor(mHSF,tCWD1),
#             mHSFPET = cor(mHSF,PmET),
#             mHSFPET1 = cor(mHSF,PmET1),
#             mHSFCMD = cor(mHSF, CMD),
#             mHSFCMD1 = cor(mHSF,CMD1),
#             mHSFPDSI = cor(mHSF, TerraPDSI),
#             mHSFPDSI1 = cor(mHSF,TerraPDSI1),
#             mHSFTMN = cor(mHSF, mTMN),
#             mHSFTMN1 = cor(mHSF,mTMN1))

# final1 <- merge(final1,final1.M, by = "ID", all = TRUE)

# final1i.M <- final1i %>% group_by(ID) %>% 
#   summarise(RWIDEF = cor(RWI, TerraDEF),
#             RWIDEF1 = cor(RWI, TerraDEF1),
#             RWICWD = cor(RWI,tCWD),
#             RWICWD1 = cor(RWI,tCWD1),
#             RWIPET = cor(RWI,PmET),
#             RWIPET1 = cor(RWI,PmET1),
#             RWICMD = cor(RWI, CMD),
#             RWICMD1 = cor(RWI,CMD1),
#             RWIPDSI = cor(RWI, TerraPDSI),
#             RWIPDSI1 = cor(RWI,TerraPDSI1),
#             RWITMN = cor(RWI, mTMN),
#             RWITMN1 = cor(RWI,mTMN1),
#             mHSFDEF = cor(mHSF, TerraDEF),
#             mHSFDEF1 = cor(mHSF, TerraDEF1),
#             mHSFCWD = cor(mHSF,tCWD),
#             mHSFCWD1 = cor(mHSF,tCWD1),
#             mHSFPET = cor(mHSF,PmET),
#             mHSFPET1 = cor(mHSF,PmET1),
#             mHSFCMD = cor(mHSF, CMD),
#             mHSFCMD1 = cor(mHSF,CMD1),
#             mHSFPDSI = cor(mHSF, TerraPDSI),
#             mHSFPDSI1 = cor(mHSF,TerraPDSI1),
#             mHSFTMN = cor(mHSF, mTMN),
#             mHSFTMN1 = cor(mHSF,mTMN1))

# final1i <- merge(final1i,final1i.M, by = "ID", all = TRUE)


# ####Create Trachs summary chronology stats
# HSF.chron <- dcast(Trachs2, Year ~ ID, value.var = "mHSF")
# library(dplR)
# HSF.gini <- rwl.stats(HSF.chron)

# #Remove excess rows and extract gini
# HSF.gini <- HSF.gini[-1,c(1,9)]
# colnames(HSF.gini) <- c("ID","HSFgini")

# #Merge with final1
# final1 <- merge(final1, HSF.gini, by = "ID", all.x = TRUE)
# final1i <- merge(final1i, HSF.gini, by = "ID", all.x = TRUE)

# #Trim to only final year observations
# final.trim <- do.call("rbind", 
#                       by(final1, INDICES=final1$ID, FUN=function(final1) final1[which.max(final1$Year), ]))
# final.trimi <- do.call("rbind", 
#                       by(final1i, INDICES=final1i$ID, FUN=function(final1i) final1i[which.max(final1i$Year), ]))

# #Remove unnecessary columns and impute one last time
# final.trimi <- final.trimi[,-c(9,31,51)]
# final.trimii <- parlmice(final.trimi, m = 5, n.core = 4, method = "cart")
# #Deviations couldn't be imputed (probably colinear). Drop
# final.trimi <- complete(final.trimii)
# final.trimi <- final.trimi[,-c(12,13,33,34,52,53)]

# ####Models####
# #Set logistic priors
# prior.log <- c(
#   prior(student_t(3, 0, 2.5), class = "Intercept"),
#   prior(student_t(3, 0, 2.5), class = "b")
# )

# #Set Status as 0-1 binary
# final.trim$Status <- as.numeric(as.factor(final.trim$Status))
# final.trim$Status <- final.trim$Status - 1 #Status now equals "mortality"

# final.trimi$Status <- as.numeric(as.factor(final.trimi$Status))
# final.trimi$Status <- final.trimi$Status - 1

# # Status ~ (RWI ~ PmET|Year) + (mHSF ~ PmET|Year) + RWI ~ PmETx1|Year + mHSFx1 ~ PmETx1|Year + gini + cindex

# #write.csv(final.trim, file = "finaltrim.csv")
# #write.csv(final.trimi, file = "finaltrimi.csv")

##########################################################################
####Begin here if using completed/cleaned data####
final.trim <- read.csv("C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/finaltrim.csv")
final.trimi <- read.csv("C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/finaltrimi.csv")

#Set options to speed up runtime
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')

#Trim and scale final data
final4 <- final.trim[final.trimi$mHSF < 1.7041,]
final4i <- final.trimi[final.trimi$mHSF < 1.7041,]

colnames(final4) <- gsub(".x","",colnames(final4))
#delete duplicate columns
final4 <- final4[,-c(27:46)]

#Merge cindex and LastDBH into non-imputed data
final4 <- cbind(final4, final4i[,24], final4i[,25])

#final scaling
final4 <- final4 %>%
  group_by(SPP) %>%
  mutate(RWI = scale(RWI),
         mHSF = scale(mHSF),
         gini = scale(gini),
         HSFgini = scale(HSFgini)) %>%
  ungroup() %>%
  mutate(tCWD = scale(tCWD),
         mTMN = scale(mTMN),
         CMD = scale(CMD),
         PmET = scale(PmET),
         DEF = scale(TerraDEF),
         PDSI = scale(TerraPDSI),
         tCWD1 = scale(tCWD1),
         mTMN1 = scale(mTMN1),
         CMD1 = scale(CMD1),
         PmET1 = scale(PmET1),
         DEF1 = scale(TerraDEF1),
         PDSI1 = scale(TerraPDSI1),
         tCWD2 = scale(tCWD2),
         mTMN2 = scale(mTMN2),
         CMD2 = scale(CMD2),
         PmET2 = scale(PmET2),
         DEF2 = scale(TerraDEF2),
         PDSI2 = scale(TerraPDSI2),
         LastDBH = scale(LastDBH))

final4i <- final4i %>%
  group_by(SPP) %>%
  mutate(RWI = scale(RWI),
         mHSF = scale(mHSF),
         gini = scale(gini),
         HSFgini = scale(HSFgini)) %>%
  ungroup() %>%
  mutate(tCWD = scale(tCWD),
         mTMN = scale(mTMN),
         CMD = scale(CMD),
         PmET = scale(PmET),
         DEF = scale(TerraDEF),
         PDSI = scale(TerraPDSI),
         tCWD1 = scale(tCWD1),
         mTMN1 = scale(mTMN1),
         CMD1 = scale(CMD1),
         PmET1 = scale(PmET1),
         DEF1 = scale(TerraDEF1),
         PDSI1 = scale(TerraPDSI1),
         tCWD2 = scale(tCWD2),
         mTMN2 = scale(mTMN2),
         CMD2 = scale(CMD2),
         PmET2 = scale(PmET2),
         DEF2 = scale(TerraDEF2),
         PDSI2 = scale(TerraPDSI2),
         LastDBH = scale(LastDBH))


#####Models####
####No imputation first####
#No lag no imputation
####growth and competition only####
#PIPO
fit.PgrowthcompCWDni <- brm(as.factor(Status) ~ RWI + gini + tCWD + cindex,
                      data = final4[final4$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                      warmup = 1000, iter = 5000, chains = 4,
                      prior = prior.log,
                      control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PgrowthcompCWDni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CWDrgrowthcompnoLagniP.RDS")

fit.PgrowthcompCMDni <- brm(as.factor(Status) ~ RWI + gini + CMD + cindex,
                           data = final4[final4$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PgrowthcompCMDni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CMDrgrowthcompnoLagniP.RDS")


fit.PgrowthcompDEFni <- brm(as.factor(Status) ~ RWI + gini + DEF + cindex,
                           data = final4[final4$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PgrowthcompDEFni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/DEFrgrowthcompnoLagniP.RDS")


fit.PgrowthcompPDSIni <- brm(as.factor(Status) ~ RWI + gini + PDSI + cindex,
                           data = final4[final4$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PgrowthcompPDSIni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PDSIrgrowthcompnoLagniP.RDS")


fit.PgrowthcompPETni <- brm(as.factor(Status) ~ RWI + gini + PmET + cindex,
                           data = final4[final4$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PgrowthcompPETni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PETrgrowthcompnoLagniP.RDS")


#PIJE
fit.JgrowthcompCWDni <- brm(as.factor(Status) ~ RWI + gini + tCWD + cindex,
                           data = final4[final4$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JgrowthcompCWDni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CWDrgrowthcompnoLagniJ.RDS")

fit.JgrowthcompCMDni <- brm(as.factor(Status) ~ RWI + gini + CMD + cindex,
                           data = final4[final4$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JgrowthcompCMDni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CMDrgrowthcompnoLagniJ.RDS")


fit.JgrowthcompDEFni <- brm(as.factor(Status) ~ RWI + gini + DEF + cindex,
                           data = final4[final4$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JgrowthcompDEFni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/DEFrgrowthcompnoLagniJ.RDS")


fit.JgrowthcompPDSIni <- brm(as.factor(Status) ~ RWI + gini + PDSI + cindex,
                            data = final4[final4$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                            warmup = 1000, iter = 5000, chains = 4,
                            prior = prior.log,
                            control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JgrowthcompPDSIni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PDSIrgrowthcompnoLagniJ.RDS")


fit.JgrowthcompPETni <- brm(as.factor(Status) ~ RWI + gini + PmET + cindex,
                           data = final4[final4$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JgrowthcompPETni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PETrgrowthcompnoLagniJ.RDS")

#Imputed
#PIPO
fit.PgrowthcompCWDi <- brm(as.factor(Status) ~ RWI + gini + tCWD + cindex,
                           data = final4i[final4i$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PgrowthcompCWDi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CWDrgrowthcompnoLagiP.RDS")

fit.PgrowthcompCMDi <- brm(as.factor(Status) ~ RWI + gini + CMD + cindex,
                           data = final4i[final4i$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PgrowthcompCMDi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CMDrgrowthcompnoLagiP.RDS")


fit.PgrowthcompDEFi <- brm(as.factor(Status) ~ RWI + gini + DEF + cindex,
                           data = final4i[final4i$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PgrowthcompDEFi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/DEFrgrowthcompnoLagiP.RDS")


fit.PgrowthcompPDSIi <- brm(as.factor(Status) ~ RWI + gini + PDSI + cindex,
                            data = final4i[final4i$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                            warmup = 1000, iter = 5000, chains = 4,
                            prior = prior.log,
                            control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PgrowthcompPDSIi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PDSIrgrowthcompnoLagiP.RDS")


fit.PgrowthcompPETi <- brm(as.factor(Status) ~ RWI + gini + PmET + cindex,
                           data = final4i[final4i$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PgrowthcompPETi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PETrgrowthcompnoLagiP.RDS")


#PIJE
fit.JgrowthcompCWDi <- brm(as.factor(Status) ~ RWI + gini + tCWD + cindex,
                           data = final4i[final4i$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JgrowthcompCWDi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CWDrgrowthcompnoLagiJ.RDS")

fit.JgrowthcompCMDi <- brm(as.factor(Status) ~ RWI + gini + CMD + cindex,
                           data = final4i[final4i$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JgrowthcompCMDi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CMDrgrowthcompnoLagiJ.RDS")


fit.JgrowthcompDEFi <- brm(as.factor(Status) ~ RWI + gini + DEF + cindex,
                           data = final4i[final4i$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JgrowthcompDEFi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/DEFrgrowthcompnoLagiJ.RDS")


fit.JgrowthcompPDSIi <- brm(as.factor(Status) ~ RWI + gini + PDSI + cindex,
                            data = final4i[final4i$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                            warmup = 1000, iter = 5000, chains = 4,
                            prior = prior.log,
                            control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JgrowthcompPDSIi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PDSIrgrowthcompnoLagiJ.RDS")


fit.JgrowthcompPETi <- brm(as.factor(Status) ~ RWI + gini + PmET + cindex,
                           data = final4i[final4i$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JgrowthcompPETi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PETrgrowthcompnoLagiJ.RDS")

####Growth + Drought + Size####
#Same as above but size instead of comp
#PIPO
fit.PgrowthdbhCWDni <- brm(as.factor(Status) ~ RWI + gini + tCWD + LastDBH,
                            data = final4[final4$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                            warmup = 1000, iter = 5000, chains = 4,
                            prior = prior.log,
                            control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PgrowthdbhCWDni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CWDrgrowthdbhnoLagniP.RDS")

fit.PgrowthdbhCMDni <- brm(as.factor(Status) ~ RWI + gini + CMD + LastDBH,
                            data = final4[final4$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                            warmup = 1000, iter = 5000, chains = 4,
                            prior = prior.log,
                            control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PgrowthdbhCMDni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CMDrgrowthdbhnoLagniP.RDS")


fit.PgrowthdbhDEFni <- brm(as.factor(Status) ~ RWI + gini + DEF + LastDBH,
                            data = final4[final4$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                            warmup = 1000, iter = 5000, chains = 4,
                            prior = prior.log,
                            control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PgrowthdbhDEFni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/DEFrgrowthdbhnoLagniP.RDS")


fit.PgrowthdbhPDSIni <- brm(as.factor(Status) ~ RWI + gini + PDSI + LastDBH,
                             data = final4[final4$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                             warmup = 1000, iter = 5000, chains = 4,
                             prior = prior.log,
                             control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PgrowthdbhPDSIni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PDSIrgrowthdbhnoLagniP.RDS")


fit.PgrowthdbhPETni <- brm(as.factor(Status) ~ RWI + gini + PmET + LastDBH,
                            data = final4[final4$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                            warmup = 1000, iter = 5000, chains = 4,
                            prior = prior.log,
                            control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PgrowthdbhPETni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PETrgrowthdbhnoLagniP.RDS")


#PIJE
fit.JgrowthdbhCWDni <- brm(as.factor(Status) ~ RWI + gini + tCWD + LastDBH,
                            data = final4[final4$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                            warmup = 1000, iter = 5000, chains = 4,
                            prior = prior.log,
                            control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JgrowthdbhCWDni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CWDrgrowthdbhnoLagniJ.RDS")

fit.JgrowthdbhCMDni <- brm(as.factor(Status) ~ RWI + gini + CMD + LastDBH,
                            data = final4[final4$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                            warmup = 1000, iter = 5000, chains = 4,
                            prior = prior.log,
                            control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JgrowthdbhCMDni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CMDrgrowthdbhnoLagniJ.RDS")


fit.JgrowthdbhDEFni <- brm(as.factor(Status) ~ RWI + gini + DEF + LastDBH,
                            data = final4[final4$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                            warmup = 1000, iter = 5000, chains = 4,
                            prior = prior.log,
                            control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JgrowthdbhDEFni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/DEFrgrowthdbhnoLagniJ.RDS")


fit.JgrowthdbhPDSIni <- brm(as.factor(Status) ~ RWI + gini + PDSI + LastDBH,
                             data = final4[final4$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                             warmup = 1000, iter = 5000, chains = 4,
                             prior = prior.log,
                             control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JgrowthdbhPDSIni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PDSIrgrowthdbhnoLagniJ.RDS")


fit.JgrowthdbhPETni <- brm(as.factor(Status) ~ RWI + gini + PmET + LastDBH,
                            data = final4[final4$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                            warmup = 1000, iter = 5000, chains = 4,
                            prior = prior.log,
                            control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JgrowthdbhPETni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PETrgrowthdbhnoLagniJ.RDS")

#Imputed
#PIPO
fit.PgrowthdbhCWDi <- brm(as.factor(Status) ~ RWI + gini + tCWD + LastDBH,
                           data = final4i[final4i$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PgrowthdbhCWDi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CWDrgrowthdbhnoLagiP.RDS")

fit.PgrowthdbhCMDi <- brm(as.factor(Status) ~ RWI + gini + CMD + LastDBH,
                           data = final4i[final4i$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PgrowthdbhCMDi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CMDrgrowthdbhnoLagiP.RDS")


fit.PgrowthdbhDEFi <- brm(as.factor(Status) ~ RWI + gini + DEF + LastDBH,
                           data = final4i[final4i$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PgrowthdbhDEFi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/DEFrgrowthdbhnoLagiP.RDS")


fit.PgrowthdbhPDSIi <- brm(as.factor(Status) ~ RWI + gini + PDSI + LastDBH,
                            data = final4i[final4i$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                            warmup = 1000, iter = 5000, chains = 4,
                            prior = prior.log,
                            control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PgrowthdbhPDSIi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PDSIrgrowthdbhnoLagiP.RDS")


fit.PgrowthdbhPETi <- brm(as.factor(Status) ~ RWI + gini + PmET + LastDBH,
                           data = final4i[final4i$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PgrowthdbhPETi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PETrgrowthdbhnoLagiP.RDS")


#PIJE
fit.JgrowthdbhCWDi <- brm(as.factor(Status) ~ RWI + gini + tCWD + LastDBH,
                           data = final4i[final4i$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JgrowthdbhCWDi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CWDrgrowthdbhnoLagiJ.RDS")

fit.JgrowthdbhCMDi <- brm(as.factor(Status) ~ RWI + gini + CMD + LastDBH,
                           data = final4i[final4i$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JgrowthdbhCMDi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CMDrgrowthdbhnoLagiJ.RDS")


fit.JgrowthdbhDEFi <- brm(as.factor(Status) ~ RWI + gini + DEF + LastDBH,
                           data = final4i[final4i$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JgrowthdbhDEFi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/DEFrgrowthdbhnoLagiJ.RDS")


fit.JgrowthdbhPDSIi <- brm(as.factor(Status) ~ RWI + gini + PDSI + LastDBH,
                            data = final4i[final4i$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                            warmup = 1000, iter = 5000, chains = 4,
                            prior = prior.log,
                            control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JgrowthdbhPDSIi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PDSIrgrowthdbhnoLagiJ.RDS")


fit.JgrowthdbhPETi <- brm(as.factor(Status) ~ RWI + gini + PmET + LastDBH,
                           data = final4i[final4i$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JgrowthdbhPETi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PETrgrowthdbhnoLagiJ.RDS")

####"Basic tree traits": DBH, growth, competition####
fit.PbasicCWDni <- brm(as.factor(Status) ~ RWI + gini + LastDBH + cindex,
                           data = final4[final4$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PbasicCWDni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CWDrbasicnoLagniP.RDS")

fit.PbasicCMDni <- brm(as.factor(Status) ~ RWI + gini + LastDBH + cindex,
                           data = final4[final4$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PbasicCMDni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CMDrbasicnoLagniP.RDS")


fit.PbasicDEFni <- brm(as.factor(Status) ~ RWI + gini + LastDBH + cindex,
                           data = final4[final4$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PbasicDEFni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/DEFrbasicnoLagniP.RDS")


fit.PbasicPDSIni <- brm(as.factor(Status) ~ RWI + gini + LastDBH + cindex,
                            data = final4[final4$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                            warmup = 1000, iter = 5000, chains = 4,
                            prior = prior.log,
                            control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PbasicPDSIni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PDSIrbasicnoLagniP.RDS")


fit.PbasicPETni <- brm(as.factor(Status) ~ RWI + gini + LastDBH + cindex,
                           data = final4[final4$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PbasicPETni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PETrbasicnoLagniP.RDS")


#PIJE
fit.JbasicCWDni <- brm(as.factor(Status) ~ RWI + gini + LastDBH + cindex,
                           data = final4[final4$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JbasicCWDni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CWDrbasicnoLagniJ.RDS")

fit.JbasicCMDni <- brm(as.factor(Status) ~ RWI + gini + LastDBH + cindex,
                           data = final4[final4$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JbasicCMDni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CMDrbasicnoLagniJ.RDS")


fit.JbasicDEFni <- brm(as.factor(Status) ~ RWI + gini + LastDBH + cindex,
                           data = final4[final4$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JbasicDEFni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/DEFrbasicnoLagniJ.RDS")


fit.JbasicPDSIni <- brm(as.factor(Status) ~ RWI + gini + LastDBH + cindex,
                            data = final4[final4$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                            warmup = 1000, iter = 5000, chains = 4,
                            prior = prior.log,
                            control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JbasicPDSIni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PDSIrbasicnoLagniJ.RDS")


fit.JbasicPETni <- brm(as.factor(Status) ~ RWI + gini + LastDBH + cindex,
                           data = final4[final4$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JbasicPETni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PETrbasicnoLagniJ.RDS")

#Imputed
#PIPO
fit.PbasicCWDi <- brm(as.factor(Status) ~ RWI + gini + LastDBH + cindex,
                          data = final4i[final4i$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                          warmup = 1000, iter = 5000, chains = 4,
                          prior = prior.log,
                          control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PbasicCWDi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CWDrbasicnoLagiP.RDS")

fit.PbasicCMDi <- brm(as.factor(Status) ~ RWI + gini + LastDBH + cindex,
                          data = final4i[final4i$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                          warmup = 1000, iter = 5000, chains = 4,
                          prior = prior.log,
                          control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PbasicCMDi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CMDrbasicnoLagiP.RDS")


fit.PbasicDEFi <- brm(as.factor(Status) ~ RWI + gini + LastDBH + cindex,
                          data = final4i[final4i$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                          warmup = 1000, iter = 5000, chains = 4,
                          prior = prior.log,
                          control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PbasicDEFi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/DEFrbasicnoLagiP.RDS")


fit.PbasicPDSIi <- brm(as.factor(Status) ~ RWI + gini + LastDBH + cindex,
                           data = final4i[final4i$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PbasicPDSIi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PDSIrbasicnoLagiP.RDS")


fit.PbasicPETi <- brm(as.factor(Status) ~ RWI + gini + LastDBH + cindex,
                          data = final4i[final4i$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                          warmup = 1000, iter = 5000, chains = 4,
                          prior = prior.log,
                          control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PbasicPETi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PETrbasicnoLagiP.RDS")


#PIJE
fit.JbasicCWDi <- brm(as.factor(Status) ~ RWI + gini + LastDBH + cindex,
                          data = final4i[final4i$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                          warmup = 1000, iter = 5000, chains = 4,
                          prior = prior.log,
                          control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JbasicCWDi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CWDrbasicnoLagiJ.RDS")

fit.JbasicCMDi <- brm(as.factor(Status) ~ RWI + gini + LastDBH + cindex,
                          data = final4i[final4i$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                          warmup = 1000, iter = 5000, chains = 4,
                          prior = prior.log,
                          control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JbasicCMDi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CMDrbasicnoLagiJ.RDS")


fit.JbasicDEFi <- brm(as.factor(Status) ~ RWI + gini + LastDBH + cindex,
                          data = final4i[final4i$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                          warmup = 1000, iter = 5000, chains = 4,
                          prior = prior.log,
                          control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JbasicDEFi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/DEFrbasicnoLagiJ.RDS")


fit.JbasicPDSIi <- brm(as.factor(Status) ~ RWI + gini + LastDBH + cindex,
                           data = final4i[final4i$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                           warmup = 1000, iter = 5000, chains = 4,
                           prior = prior.log,
                           control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JbasicPDSIi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PDSIrbasicnoLagiJ.RDS")


fit.JbasicPETi <- brm(as.factor(Status) ~ RWI + gini + LastDBH + cindex,
                          data = final4i[final4i$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                          warmup = 1000, iter = 5000, chains = 4,
                          prior = prior.log,
                          control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JbasicPETi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PETrbasicnoLagiJ.RDS")

####Xylem + DBH####
fit.PxylemonlyCWDni <- brm(as.factor(Status) ~ mHSF + HSFgini + tCWD + LastDBH,
                       data = final4[final4$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                       warmup = 1000, iter = 5000, chains = 4,
                       prior = prior.log,
                       control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PxylemonlyCWDni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CWDrxylemonlynoLagniP.RDS")

fit.PxylemonlyCMDni <- brm(as.factor(Status) ~ mHSF + HSFgini + CMD + LastDBH,
                       data = final4[final4$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                       warmup = 1000, iter = 5000, chains = 4,
                       prior = prior.log,
                       control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PxylemonlyCMDni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CMDrxylemonlynoLagniP.RDS")


fit.PxylemonlyDEFni <- brm(as.factor(Status) ~ mHSF + HSFgini + DEF + LastDBH,
                       data = final4[final4$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                       warmup = 1000, iter = 5000, chains = 4,
                       prior = prior.log,
                       control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PxylemonlyDEFni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/DEFrxylemonlynoLagniP.RDS")


fit.PxylemonlyPDSIni <- brm(as.factor(Status) ~ mHSF + HSFgini + PDSI + LastDBH,
                        data = final4[final4$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                        warmup = 1000, iter = 5000, chains = 4,
                        prior = prior.log,
                        control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PxylemonlyPDSIni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PDSIrxylemonlynoLagniP.RDS")


fit.PxylemonlyPETni <- brm(as.factor(Status) ~ mHSF + HSFgini + PmET + LastDBH,
                       data = final4[final4$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                       warmup = 1000, iter = 5000, chains = 4,
                       prior = prior.log,
                       control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PxylemonlyPETni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PETrxylemonlynoLagniP.RDS")


#PIJE
fit.JxylemonlyCWDni <- brm(as.factor(Status) ~ mHSF + HSFgini + tCWD + LastDBH,
                       data = final4[final4$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                       warmup = 1000, iter = 5000, chains = 4,
                       prior = prior.log,
                       control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JxylemonlyCWDni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CWDrxylemonlynoLagniJ.RDS")

fit.JxylemonlyCMDni <- brm(as.factor(Status) ~ mHSF + HSFgini + CMD + LastDBH,
                       data = final4[final4$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                       warmup = 1000, iter = 5000, chains = 4,
                       prior = prior.log,
                       control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JxylemonlyCMDni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CMDrxylemonlynoLagniJ.RDS")


fit.JxylemonlyDEFni <- brm(as.factor(Status) ~ mHSF + HSFgini + DEF + LastDBH,
                       data = final4[final4$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                       warmup = 1000, iter = 5000, chains = 4,
                       prior = prior.log,
                       control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JxylemonlyDEFni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/DEFrxylemonlynoLagniJ.RDS")


fit.JxylemonlyPDSIni <- brm(as.factor(Status) ~ mHSF + HSFgini + PDSI + LastDBH,
                        data = final4[final4$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                        warmup = 1000, iter = 5000, chains = 4,
                        prior = prior.log,
                        control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JxylemonlyPDSIni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PDSIrxylemonlynoLagniJ.RDS")


fit.JxylemonlyPETni <- brm(as.factor(Status) ~ mHSF + HSFgini + PmET + LastDBH,
                       data = final4[final4$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                       warmup = 1000, iter = 5000, chains = 4,
                       prior = prior.log,
                       control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JxylemonlyPETni, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PETrxylemonlynoLagniJ.RDS")

#Imputed
#PIPO
fit.PxylemonlyCWDi <- brm(as.factor(Status) ~ mHSF + HSFgini + tCWD + LastDBH,
                      data = final4i[final4i$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                      warmup = 1000, iter = 5000, chains = 4,
                      prior = prior.log,
                      control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PxylemonlyCWDi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CWDrxylemonlynoLagiP.RDS")

fit.PxylemonlyCMDi <- brm(as.factor(Status) ~ mHSF + HSFgini + CMD + LastDBH,
                      data = final4i[final4i$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                      warmup = 1000, iter = 5000, chains = 4,
                      prior = prior.log,
                      control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PxylemonlyCMDi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CMDrxylemonlynoLagiP.RDS")


fit.PxylemonlyDEFi <- brm(as.factor(Status) ~ mHSF + HSFgini + DEF + LastDBH,
                      data = final4i[final4i$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                      warmup = 1000, iter = 5000, chains = 4,
                      prior = prior.log,
                      control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PxylemonlyDEFi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/DEFrxylemonlynoLagiP.RDS")


fit.PxylemonlyPDSIi <- brm(as.factor(Status) ~ mHSF + HSFgini + PDSI + LastDBH,
                       data = final4i[final4i$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                       warmup = 1000, iter = 5000, chains = 4,
                       prior = prior.log,
                       control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PxylemonlyPDSIi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PDSIrxylemonlynoLagiP.RDS")


fit.PxylemonlyPETi <- brm(as.factor(Status) ~ mHSF + HSFgini + PmET + LastDBH,
                      data = final4i[final4i$SPP == "PIPO",], family =  bernoulli(link = "logit"),
                      warmup = 1000, iter = 5000, chains = 4,
                      prior = prior.log,
                      control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.PxylemonlyPETi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PETrxylemonlynoLagiP.RDS")


#PIJE
fit.JxylemonlyCWDi <- brm(as.factor(Status) ~ mHSF + HSFgini + tCWD + LastDBH,
                      data = final4i[final4i$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                      warmup = 1000, iter = 5000, chains = 4,
                      prior = prior.log,
                      control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JxylemonlyCWDi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CWDrxylemonlynoLagiJ.RDS")

fit.JxylemonlyCMDi <- brm(as.factor(Status) ~ mHSF + HSFgini + CMD + LastDBH,
                      data = final4i[final4i$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                      warmup = 1000, iter = 5000, chains = 4,
                      prior = prior.log,
                      control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JxylemonlyCMDi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/CMDrxylemonlynoLagiJ.RDS")


fit.JxylemonlyDEFi <- brm(as.factor(Status) ~ mHSF + HSFgini + DEF + LastDBH,
                      data = final4i[final4i$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                      warmup = 1000, iter = 5000, chains = 4,
                      prior = prior.log,
                      control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JxylemonlyDEFi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/DEFrxylemonlynoLagiJ.RDS")


fit.JxylemonlyPDSIi <- brm(as.factor(Status) ~ mHSF + HSFgini + PDSI + LastDBH,
                       data = final4i[final4i$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                       warmup = 1000, iter = 5000, chains = 4,
                       prior = prior.log,
                       control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JxylemonlyPDSIi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PDSIrxylemonlynoLagiJ.RDS")


fit.JxylemonlyPETi <- brm(as.factor(Status) ~ mHSF + HSFgini + PmET + LastDBH,
                      data = final4i[final4i$SPP == "PIJE",], family =  bernoulli(link = "logit"),
                      warmup = 1000, iter = 5000, chains = 4,
                      prior = prior.log,
                      control = list(adapt_delta = 0.95, max_treedepth = 15), cores = 4, future = TRUE)

saveRDS(fit.JxylemonlyPETi, file = "C:/Users/jlaud/Box/Jeff Working Docs/Papers/In prep/TestingFightorFlight/PETrxylemonlynoLagiJ.RDS")

####

####Aggregate output/fits####