## ----setup, include=FALSE-------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)


## -------------------------------------------------------------------------------------------------------------
library(readxl)
loyn <- read_xlsx("mlr.xlsx", "Loyn")


## -------------------------------------------------------------------------------------------------------------
#par(mfrow=c(3,3))
hist(loyn$ABUND)
hist(loyn$ALT)
hist(loyn$YR.ISOL)
hist(loyn$GRAZE)
hist(loyn$AREA)
hist(loyn$DIST)
hist(loyn$LDIST)
#par(mfrow=c(1,1))


## ----eval=FALSE-----------------------------------------------------------------------------------------------
#| message: false
## # install.packages("Hmisc")
## library(Hmisc)
## hist(loyn, nclass = 50)


## -------------------------------------------------------------------------------------------------------------
#| message: false
# tidy the data
loyn_tidy <- pivot_longer(loyn, cols = everything())

# plot
ggplot(loyn_tidy, aes(x = value)) + 
  geom_histogram() + 
  facet_wrap(~name, scales = "free") +
  theme_bw()


## -------------------------------------------------------------------------------------------------------------
#| message: false
loyn %>% 
  pivot_longer(cols = everything()) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() + 
  facet_wrap(~name, scales = "free") +
  theme_bw()


## -------------------------------------------------------------------------------------------------------------
cor(loyn)


## ----corLoyn, eval=F, echo=F----------------------------------------------------------------------------------
## cor(loyn)


## -------------------------------------------------------------------------------------------------------------
pairs(loyn)


## -------------------------------------------------------------------------------------------------------------
#| message: false
library(corrplot)
corrplot(cor(loyn))


## -------------------------------------------------------------------------------------------------------------
#| eval: false
## loyn$L10AREA <- log10(loyn$AREA)


## -------------------------------------------------------------------------------------------------------------
#| eval: false
## hist(loyn$L10AREA)
## pairs(loyn)


## -------------------------------------------------------------------------------------------------------------
#| eval: false
#| include: false
## loyn$L10DIST <- log10(loyn$DIST)
## hist(loyn$L10DIST)
## 
## loyn$L10LDIST <- log10(loyn$LDIST)
## hist(loyn$L10LDIST)
## 
## pairs(loyn)


## -------------------------------------------------------------------------------------------------------------
# reset the data import just in case it has been modified
loyn <- read_xlsx("mlr.xlsx", "Loyn")
# make transformations

loyn <- loyn %>%
  mutate(L10AREA = log10(AREA),
    L10DIST = log10(DIST),
    L10LDIST = log10(LDIST))

# check
glimpse(loyn)


## -------------------------------------------------------------------------------------------------------------
#| eval: false
## cor(loyn)


## -------------------------------------------------------------------------------------------------------------
cor(loyn)


## -------------------------------------------------------------------------------------------------------------
#| eval: false
## lm.mod1 <- lm(ABUND~GRAZE + L10AREA, data=loyn)
## 
## par(mfrow=c(2,2))
## plot(lm.mod1)
## par(mfrow=c(1,1))
## 
## summary(lm.mod1)


## ----mlmod----------------------------------------------------------------------------------------------------
lm.mod1 <- lm(ABUND~GRAZE + L10AREA, data=loyn)

par(mfrow=c(2,2))
plot(lm.mod1)
par(mfrow=c(1,1))

summary(lm.mod1)


## -------------------------------------------------------------------------------------------------------------
#| eval: false
## summary(lm.mod1)$r.squared
## summary(lm.mod1)$adj.r.squared


## -------------------------------------------------------------------------------------------------------------
lm.mod2 <- lm(ABUND ~ GRAZE + L10AREA + YR.ISOL, data=loyn)


## -------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(lm.mod2)
par(mfrow=c(1,1))


## **ANSWER**
## The residuals look OK, but YR.ISOL is borderline significant (p = 0.0768).

## -------------------------------------------------------------------------------------------------------------
#| eval: false
## summary(lm.mod2)


## ----readDataStream-------------------------------------------------------------------------------------------
# read in the data
s.data <- read_xlsx("mlr.xlsx", "California_streamflow")
names(s.data)


## ----Exercise1, eval=T, echo=T--------------------------------------------------------------------------------
s.mod_full <-lm(L10BSAAM~L10APSAB + L10OBPC + L10OPRC, data=s.data)
s.mod_full <-lm(L10BSAAM~., data=s.data) ## you can also use the . to indicate use all variables
summary(s.mod_full)


## ----PartialFtests1, echo=T, eval = F-------------------------------------------------------------------------
## s.mod_reduced <- lm(L10BSAAM ~ L10OPRC + L10OBPC, data=s.data)
## anova(s.mod_reduced, s.mod_full)


## -------------------------------------------------------------------------------------------------------------
s.mod_reduced2  <- lm(L10BSAAM ~ L10APSAB + L10OBPC,data=s.data)
anova(s.mod_reduced2, s.mod_full)


## ----SolutionPartialF_again, echo = T, eval = F---------------------------------------------------------------
## s.mod_reduced3  <- lm(L10BSAAM ~ L10OPRC,data=s.data)
## anova(s.mod_reduced3, s.mod_full)

