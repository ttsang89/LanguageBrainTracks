# install.packages('lme4')
library(lme4)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(readxl)
library(lmertest)
library(bild)

library(nlme)

MCDI_long_dat <- read_excel("/Users/tawnytsang/Desktop/Projects/imaging/Native Language/6wk/spreadsheets/MCDI_02-04-18.xlsx", sheet = "long_MCDI")
MCDI_long_dat <- as.data.frame(MCDI_long_dat)

Mullen_long_dat <- read_excel("/Users/tawnytsang/Desktop/Projects/imaging/Native Language/6wk/spreadsheets/Mullen_02-04-18.xlsx", sheet = "LONG")
Mullen_long_dat <- as.data.frame(Mullen_long_dat)

NativeLang_data <- read_excel("/Users/tawnytsang/Desktop/Projects/imaging/Native Language/6wk/spreadsheets/05-03-18_PE_language-laterality_checks.xlsx", sheet = "LONG")
NativeLang<- as.data.frame(NativeLang_data)

fm.lang_empty <- lme(ENG_LeftvsR ~ visit*rx, 
                                 random =~visit|ID, 
                                 # weight=varPower(form=~fitted(.)),
                                 #correlation=corCAR1(),
                                 data=NativeLang)
summary(fm.lang_empty)


fm.MCDI <- lme(Radv ~ age_centered*RX, 
                     random =~1|PROJECTID, 
                     # weight=varPower(form=~fitted(.)),
                     #correlation=corCAR1(),
                     data=MCDI_long_dat)
summary(fm.MCDI)

fm.MullenVDQ <- lme(Verbal_Norm ~ age_centered*Risk, 
               random =~1|PROJECTID, 
               # weight=varPower(form=~fitted(.)),
               #correlation=corCAR1(),
               data=Mullen_long_dat)
summary(fm.MullenVDQ)


fm.rx_language_laterality <- lme(SHULTZ ~ visit*RX, 
                         random =~visit|ID, 
                         # weight=varPower(form=~fitted(.)),
                         #correlation=corCAR1(),
                         data=NativeLang)
summary(fm.rx_language_laterality)



fm.rx_laterality <- lme(ENG_LeftvsR ~ visit*RX, 
                                 random =~visit|ID, 
                                 # weight=varPower(form=~fitted(.)),
                                 #correlation=corCAR1(),
                                 data=NativeLang)
summary(fm.rx_laterality)


fm.rx_laterality_lmr <- lmer(ENG_SHULTZ ~ age*RX + 
                        (visit|ID), 
                        # weight=varPower(form=~fitted(.)),
                        #correlation=corCAR1(),
                        data=NativeLang)
fm.rx_laterality_language_lmr <- lmer(ENG_SHULTZ ~ visit*RX*Language + 
                               (visit|ID), 
                             # weight=varPower(form=~fitted(.)),
                             #correlation=corCAR1(),
                             data=NativeLang)
anova(fm.rx_laterality_language_lmr, fm.rx_laterality_lmr)



fm.rx_lang_act <- lme(ENG_SHULTZ ~ age*RX + L1, 
                        random =~age|ID, 
                       # weight=varPower(form=~fitted(.)),
                        #correlation=corCAR1(),
                        data=NativeLang)

summary(fm.rx_lang_act)

LR <- subset(NativeLang, RX==0)

fm.lang_LR <- lme(ENG_SHULTZ ~ visit*Language, 
                      random =~visit|ID, 
                      #weight=varPower(form=~fitted(.)),
                      #correlation=corCAR1(),
                      data=LR)

summary(fm.lang_LR)

