#K.Palof review of Carl's code - called Abalone_length_R.R

# Set random seed to keep alkIndivAge results constant
set.seed(14354454)

#library(dplyr)
library(tidyverse)
library(magrittr)
library(FSA)
library(nnet)
library(nlstools)
library(nlsMicrobio)
rm(list = ls()) # clear workspace since data frames have same names
###Age-length key from sheppard data###
shpab_raw <- read.csv("./data/length_weight_shepard.csv", header = TRUE)  
shpab <- data.frame(len=shpab_raw$len, age=shpab_raw$age)
headtail(shpab)

shpab %<>% mutate(lcat5=lencat(len, w=5))       # determine length categories (in this case by 10s)
headtail(shpab)
 
is.na(headtail(shpab)$age)        # demonstration purposes only

shpab.unaged <- filter(shpab, is.na(age))       # data.frame of unaged abalone
headtail(shpab.unaged)

shpab.aged <- filter(shpab, !is.na(age))        # data.frame of aged abalone
headtail(shpab.aged)

all(is.na(shpab.unaged$age))        # should be TRUE
any(is.na(shpab.aged$age))        #should be FALSE

(alkey.freq <- xtabs(~lcat5+age, shpab.aged))        # create contingency table: number of abalone per age, by length category

rowSums(alkey.freq)       # number of aged.abalone per len.category

alkey <- prop.table(alkey.freq, margin=1)       # create age length key. probabilities that an abalone in length.category can be found in a year class 

round(alkey, 3)       # rounded for display purposes only

(shpab.mlr <- multinom(age~lcat5,data=shpab.aged,maxit=500))       # fits a multinomial log-linear model via neural networks - is this the call that's needed here?
#Ven-ables and Ripley 2002 

lens <- seq(0,130,5)
(alkey.sm <- predict(shpab.mlr,data.frame(lcat5=lens),type="probs"))       # calculate length category probabilities 
row.names(alkey.sm) <- lens   # for clarity
round(alkey.sm,3)             # round for display purposes only

alkPlot(alkey,type="area",pal="gray",showLegend=TRUE,
        leg.cex=0.7,xlab="Total Length (mm)")       # plot area graph of probabilities

alkPlot(alkey,type="bubble",xlab="Total Length (mm)")       # plot bubble graph of probabilities

( len.n <- xtabs(~lcat5,data=shpab) )       # number of abalone in each length category 

( tmp <- sweep(alkey,MARGIN=1,FUN="*",STATS=len.n) )        # number of abalone in each age class and length.category

( ad1 <- colSums(tmp) )       # sums of columns

round(prop.table(ad1),3)       # sum of abalone in each age class divided by total sum of abalone rounded for display purposes only

# note: alk.freq and len.n were calculated previously 
alkAgeDist(alkey,lenA.n=rowSums(alkey.freq),len.n=len.n) # alkey is prop at age/length, len.n is number in each length category

tmp <- alkAgeDist(alkey,lenA.n=rowSums(alkey.freq),len.n=len.n)


alkMeanVar(alkey,len~lcat5+age,data=shpab.aged,len.n=len.n)

tmp2 <- alkMeanVar(alkey,len~lcat5+age,data=shpab.aged,len.n=len.n)        # comoputes men value(length)-at-age from age length key

shpab.unaged.mod <- alkIndivAge(alkey, age~len,data=shpab.unaged)       # assign ages to unaged abalone using age length key
head(shpab.unaged.mod)

shpab.fnl <- rbind(shpab.aged, shpab.unaged.mod)

( ad3 <- xtabs(~age,data=shpab.fnl) )        # number of abalone in each age category
round(prop.table(ad3),3)   # frequency of each age class, rounded for display purposes only

shpab.sumlen <- shpab.fnl %>% group_by(age) %>%
  summarize(n=validn(len),mn=mean(len,na.rm=TRUE),
            sd=sd(len,na.rm=TRUE),se=se(len,na.rm=TRUE)) %>%
  as.data.frame()
shpab.sumlen        # summary of age-length key


plot(len~age,data=shpab.fnl,pch=19,col=rgb(0,0,0,1/10),
     xlab="Age",ylab="Total Length (mm)",ylim=c(0,140))       # plot of age frequencies by length
lines(mn~age,data=shpab.sumlen,lwd=2,lty=2)

###age length key for Gravina and Meares Pass timed swim data using sheppard###
#Gravina_Abalone
grvab_raw <- read.csv("./data/Gravina_Abalone.csv", header = TRUE)
grvab <- data.frame(len=grvab_raw$len, age=grvab_raw$age)
grvab <- grvab[-c(192,301),]        #removed rows 192 and 301 b/c both were under 10mm. sheppard did not produce a '0mm' category
headtail(grvab)

grvab %<>% mutate(lcat5=lencat(len, w=5), lcat10 = lencat(len, w=10))       # determine length categories (in this case by 5s)
headtail(grvab)

# all these are unaged?? right?
grvab.unaged <- filter(grvab, is.na(age))       # data.frame of unaged abalone
headtail(grvab.unaged)

all(is.na(grvab.unaged$age))        # should be TRUE

grvab.unaged <- (transform(grvab.unaged, age = as.numeric(age)))        # have to make age column numeric for next line

grvab.unaged.mod <- alkIndivAge(alkey, age~len,data=grvab.unaged)       # assign ages to unaged abalone using age length key
head(grvab.unaged.mod)

grvab.sumlen <- grvab.unaged.mod %>% group_by(age) %>%
  summarize(n=validn(len),mn=mean(len,na.rm=TRUE),
            sd=sd(len,na.rm=TRUE),se=se(len,na.rm=TRUE)) %>%
  as.data.frame()
grvab.sumlen        # summary of age-length key


plot(len~age,data=grvab.unaged.mod,pch=19,col=rgb(0,0,0,1/10),
     xlab="Age",ylab="Total Length (mm)",ylim=c(0,140))       # plot of age frequencies by length
lines(mn~age,data=grvab.sumlen,lwd=2,lty=2)

#MearesPass_Abalone
mrsab <- read.csv("./data/MearesPass_Abalone.csv", header = TRUE)
mrsab <- data.frame(len=mrsab$len, age=mrsab$age)
mrsab <- mrsab[-c(20,21,22,23,108,126),]        #removed rows b/c each was under 10mm or had no abalone. sheppard did not produce a '0mm' category
headtail(mrsab)

mrsab %<>% mutate(lcat10=lencat(len, w=10), lcat5 =lencat(len, w=5))       # determine length categories (in this case by 10s)
headtail(mrsab)

mrsab.unaged <- filter(mrsab, is.na(age))       # data.frame of unaged abalone
headtail(mrsab.unaged)

all(is.na(mrsab.unaged$age))        # should be TRUE

mrsab.unaged <- (transform(mrsab.unaged, age = as.numeric(age)))        # have to make age column numeric for next line

mrsab.unaged.mod <- alkIndivAge(alkey, age~len,data=mrsab.unaged)       # assign ages to unaged abalone using age length key
head(mrsab.unaged.mod)

mrsab.sumlen <- mrsab.unaged.mod %>% group_by(age) %>%
  summarize(n=validn(len),mn=mean(len,na.rm=TRUE),
            sd=sd(len,na.rm=TRUE),se=se(len,na.rm=TRUE)) %>%
  as.data.frame()
mrsab.sumlen        # summary of age-length key

# combined Gravina and Meares Pass
combab.mod <- rbind(grvab.unaged.mod, mrsab.unaged.mod)
combab.sumlen <- combab.mod %>% group_by(age) %>%
  summarize(n=validn(len),mn=mean(len,na.rm=TRUE),
            sd=sd(len,na.rm=TRUE),se=se(len,na.rm=TRUE)) %>%
  as.data.frame()
combab.sumlen 

###Catch Curve longhand###
#Gravina_Abalone
hist(grvab.unaged.mod$age, plot = TRUE)
#for (n in 1:12){
#  grvabcc <- grvabcc(nrow(subset(grvab.unaged.mod, age == n)))
#}

grvabcc <- grvab.sumlen[,1:2]
grvabcc$logn <- log(grvabcc$n)
grvabcc

plot(logn~age, data = grvabcc, ylab = "log(Catch)", pch = 19)
grvabcc.d <- subset(grvabcc, age >= 2)
gravcc <- lm(logn~age, data = grvabcc.d)
coef(gravcc)
confint(gravcc)

#MearesPass_Abalone
hist(mrsab.unaged.mod$age, plot = TRUE)
#for (n in 1:12){
#  mrsabcc <- mrsabcc(nrow(subset(mrsab.unaged.mod, age == n)))
#}

mrsabcc <- mrsab.sumlen[,1:2]
mrsabcc$logn <- log(mrsabcc$n)
mrsabcc

plot(logn~age, data = mrsabcc, ylab = "log(Catch)", pch = 19)
mrsabcc.d <- subset(mrsabcc, age >= 2)
mrscc <- lm(logn~age, data = mrsabcc.d)
coef(mrscc)
confint(mrscc)

### Catch curve Sheppard ----------------
write.csv(shpab.sumlen, file = "./output/shp_catchcurve.csv")
#peak to all ages - Regression
shpcc <- catchCurve(n~age, data = shpab.sumlen, ages2use = 5:13)
summary(shpcc)
confint(shpcc)
plot(shpcc, main = "Sheppard CC")
# peak plus to all - none under 2.
shpcc1 <- catchCurve(n~age, data = shpab.sumlen, ages2use = 6:13)
summary(shpcc1)
confint(shpcc1)
plot(shpcc1, main = "Sheppard CC")

Shp <- cbind(Est = coef(shpcc), confint(shpcc))
Shp1 <- cbind(Est = coef(shpcc1), confint(shpcc1))

shp_reg <- cbind (peak = Shp[1, ], peak_plus = Shp1[1, ])

#peak to all ages - CR method -----------
shpCR <- chapmanRobson(n~age, data = shpab.sumlen, ages2use = 5:13)
summary(shpCR)
confint(shpCR)
plot(shpCR, main = "Sheppard CC CR method")
# peak plus to all - none under 2.
shpCR1 <- chapmanRobson(n~age, data = shpab.sumlen, ages2use = 6:13)
summary(shpCR1)
confint(shpCR1)
plot(shpCR1, main = "Sheppard CC CR method")

Shp_cr <- cbind(Est = coef(shpCR), confint(shpCR))
Shp_cr1 <- cbind(Est = coef(shpCR1), confint(shpCR1))

shp_CR <- cbind (peak = Shp_cr[1, ], peak_plus = Shp_cr1[1, ])


#peak to all ages - Weighted Regression method -----------
shpccW <- catchCurve(n~age, data = shpab.sumlen, ages2use = 5:13, use.weights = TRUE)
summary(shpccW)
confint(shpccW)
plot(shpccW, main = "Sheppard CC weighted")
# peak plus to all - none under 2.
shpccW1 <- catchCurve(n~age, data = shpab.sumlen, ages2use = 6:13, use.weights = TRUE)
summary(shpccW1)
confint(shpccW1)
plot(shpccW1, main = "Sheppard CC weighted")

Shp_W <- cbind(Est = coef(shpccW), confint(shpccW))
Shp_W1 <- cbind(Est = coef(shpccW1), confint(shpccW1))

shp_WR <- cbind (peak = Shp_W[1, ], peak_plus = Shp_W1[1, ])


###Catch Curve shorthand###
#est. gravina data (2016 timed swims)
write.csv(grvab.sumlen, file = "./output/grv_catchcurve.csv")
## simple regression
## peak to all ages
grvcc <- catchCurve(n~age, data = grvab.sumlen, ages2use = 2:12)
summary(grvcc)
confint(grvcc)
plot(grvcc, main = "Gravina, ages 2 to 12, regression method")
## peak to last age before 0.
grvcc1 <- catchCurve(n~age, data = grvab.sumlen, ages2use = 2:10)
summary(grvcc1)
confint(grvcc1)
plot(grvcc1, main = "Gravina CC1")
#peak plus all 
grvcc2 <- catchCurve(n~age, data = grvab.sumlen, ages2use = 3:12)
summary(grvcc2)
confint(grvcc2)
plot(grvcc2, main = "Gravina CC2")
#peak plus til last 0 age
grvcc3 <- catchCurve(n~age, data = grvab.sumlen, ages2use = 3:10)
summary(grvcc3)
confint(grvcc3)
plot(grvcc3, main = "Gravina CC3")

grvr <- cbind(Est = coef(grvcc), confint(grvcc))
grvr1 <- cbind(Est = coef(grvcc1), confint(grvcc1))
grvr2 <- cbind(Est = coef(grvcc2), confint(grvcc2))
grvr3 <- cbind(Est = coef(grvcc3), confint(grvcc3))

grv_R <- cbind (peak_all = grvr[1, ], peak_plus_all = grvr1[1, ], peak_10 = grvr2[1, ], peak_plus_10 = grvr3[1, ])

## weighted regression (see Maceina and Bettoli 1998 or Smith et al. 2012)
grvccW <- catchCurve(n~age, data = grvab.sumlen, ages2use = 2:12, use.weights = TRUE)
summary(grvccW)
confint(grvccW)
plot(grvccW, main = "Gravina CC weighted")
## peak to last age before 0.
grvccW1 <- catchCurve(n~age, data = grvab.sumlen, ages2use = 2:10, use.weights = TRUE)
summary(grvccW1)
confint(grvccW1)
plot(grvccW1, main = "Gravina CC1")
#peak plus all 
grvccW2 <- catchCurve(n~age, data = grvab.sumlen, ages2use = 3:12, use.weights = TRUE)
summary(grvccW2)
confint(grvccW2)
plot(grvccW2, main = "Gravina CC2")
#peak plus til last 0 age
grvccW3 <- catchCurve(n~age, data = grvab.sumlen, ages2use = 3:10, use.weights = TRUE)
summary(grvccW3)
confint(grvccW3)
plot(grvccW3, main = "Gravina CC3")

grvrW <- cbind(Est = coef(grvccW), confint(grvccW))
grvrW1 <- cbind(Est = coef(grvccW1), confint(grvccW1))
grvrW2 <- cbind(Est = coef(grvccW2), confint(grvccW2))
grvrW3 <- cbind(Est = coef(grvccW3), confint(grvccW3))

grv_WR <- cbind (peak_all = grvrW[1, ], peak_plus_all = grvrW1[1, ], peak_10 = grvrW2[1, ], peak_plus_10 = grvrW3[1, ])

## Chapman Robson 
# for this estimator use all ages after peak catch was observed
grvccC <- chapmanRobson(n~age, data = grvab.sumlen, ages2use = 2:12)
summary(grvccC)
confint(grvccC)
plot(grvccC, main = "Gravina CC chapman")
## peak to last age before 0.
grvccC1 <- chapmanRobson(n~age, data = grvab.sumlen, ages2use = 2:10)
summary(grvccC1)
confint(grvccC1)
plot(grvccC1, main = "Gravina CC1")
#peak plus all 
grvccC2 <- chapmanRobson(n~age, data = grvab.sumlen, ages2use = 3:12)
summary(grvccC2)
confint(grvccC2)
plot(grvccC2, main = "Gravina CC2")
#peak plus til last 0 age
grvccC3 <- chapmanRobson(n~age, data = grvab.sumlen, ages2use = 3:10)
summary(grvccC3)
confint(grvccC3)
plot(grvccC3, main = "Gravina CC3")

grvC <- cbind(Est = coef(grvccC), confint(grvccC))
grvC1 <- cbind(Est = coef(grvccC1), confint(grvccC1))
grvC2 <- cbind(Est = coef(grvccC2), confint(grvccC2))
grvC3 <- cbind(Est = coef(grvccC3), confint(grvccC3))

grv_CR <- cbind (peak_all = grvC[2, ], peak_plus_all = grvC1[2, ], peak_10 = grvC2[2, ], peak_plus_10 = grvC3[2, ])


#est. meares pass data (2016 timed swims) 
write.csv(mrsab.sumlen, file = "./output/mrs_catchcurve.csv")
## simple regression
## peak to all ages
mrscc <- catchCurve(n~age, data = mrsab.sumlen, ages2use = 2:9)
summary(mrscc)
confint(mrscc)
plot(mrscc, main = "Meares Pass CC w/ 2 year olds")
## peak to last age before 0.
mrscc1 <- catchCurve(n~age, data = mrsab.sumlen, ages2use = 2:6)
summary(mrscc1)
confint(mrscc1)
plot(mrscc1, main = "Meares Pass CC w/ 2 year olds")
#peak plus all 
mrscc2 <- catchCurve(n~age, data = mrsab.sumlen, ages2use = 3:9)
summary(mrscc2)
confint(mrscc2)
plot(mrscc2, main = "Meares Pass CC w/o 2 year olds")
#peak plus til last 0 age
mrscc3 <- catchCurve(n~age, data = mrsab.sumlen, ages2use = 3:6)
summary(mrscc3)
confint(mrscc3)
plot(mrscc3, main = "Meares Pass CC w/o 2 year olds")

mrsR <- cbind(Est = coef(mrscc), confint(mrscc))
mrsR1 <- cbind(Est = coef(mrscc1), confint(mrscc1))
mrsR2 <- cbind(Est = coef(mrscc2), confint(mrscc2))
mrsR3 <- cbind(Est = coef(mrscc3), confint(mrscc3))

mrs_R <- cbind (peak_all = mrsR[1, ], peak_plus_all = mrsR1[1, ], peak_10 = mrsR2[1, ], peak_plus_10 = mrsR3[1, ])


#w/2yearolds
mrscc <- catchCurve(n~age, data = mrsab.sumlen, ages2use = 2:9)
summary(mrscc)
confint(mrscc)
plot(mrscc, main = "Meares Pass CC w/ 2 year olds")
#w/o 2yearolds
mrscc <- catchCurve(n~age, data = mrsab.sumlen, ages2use = 3:9)
summary(mrscc)
confint(mrscc)
plot(mrscc, main = "Meares Pass CC w/o 2 year olds")

#est. Meares Pass and Gravina data combined (2016 timed swims)
combcc <- catchCurve(n~age, data = combab.sumlen, ages2use = 3:10)
summary(combcc)
confint(combcc)
plot(combcc, main = "Gravina & Meares Pass CC combined")


# LVB models - and plots -----------------------
# data sets on their own are: shpab, grvab.unaged.mod, mrsab.unaged.mod
shpab.fnl %>% bind_rows(grvab.unaged.mod) -> all_aged
all_aged %>% bind_rows(mrsab.unaged.mod) -> all_aged
all_aged %>% filter(is.na (age))

all_aged %>% mutate(AREA = ifelse(area == "shp", 1, ifelse (area == "grv", 2, ifelse(area =="mrs", 3, 0)) )) ->all_aged
# combined data is 

ggplot(shpab, aes(age, len)) + geom_point() +ylab("valve length (mm)")

ggplot(all_aged, aes(age, len, color = area)) +geom_point(position = "jitter") +ylab("valve length (mm)")
ggplot(all_aged, aes(age, len)) +geom_point(position = "jitter") +ylab("valve length (mm)") +facet_wrap(~area)

any(is.na(all_aged$age))

# General model -all the same -------------
(svCom <- vbStarts(len~age, data = all_aged))
#(svGen <- lapply(svCom, rep , 3))
svGen1 <- list(Linf=104, K=0.21)
#svGen1a <- lapply(svGen1, rep, 3)

abGen <- len ~ Linf*(1-exp(-K*(age)))
fitGen <- nls(abGen, data = all_aged, start = svGen1)

# just vary K and Linf
svGen2 <- list(Linf = 124, K = 0.16)
ab1L <- len ~ Linf*(1-exp(-K[AREA]*(age)))
sv1L <- mapply(rep, svGen2, c(1, 3))
fit1L <- nls(ab1L, data = all_aged, start = sv1L)

sv3 <- list(Linf = 124, K = 0.16)
ab1K <- len~Linf[AREA]*(1-exp(-K*age))
sv1K <- mapply(rep, sv3, c(3,1))
fit1K <- nls(ab1K, data = all_aged, start = sv1K)

###  all seperate 
svGen2 <- list(Linf = 127, K = 0.18)
svGen2a <- lapply(svGen2, rep, 3)
abSep <- len ~ Linf[AREA]*(1-exp(-K[AREA]*(age)))
fitSep <- nls(abSep, data = all_aged, start = svGen2a)


fitGen
fitSep
fit1L
fit1K

anova(fit1L, fitSep) # sig. different
anova(fit1K, fitSep)
anova(fitGen, fitSep)
AIC(fitGen, fitSep, fit1L, fit1K)


plot(len ~ jitter(age, 0.3), data = all_aged, subset = area == "shp", pch = 19, xlab = "Age (yrs)", 
     ylab = "Valve length (mm)")
points(len~jitter(age, 0.3), data = all_aged, subset =area == "grv", pch= 19, col = "blue")
points(len~jitter(age, 0.3), data = all_aged, subset =area == "mrs", pch= 19, col = "red")
curve(model1(x, Linf=coef(fitSep)[-c(2:3, 5:6)]), from =1, to = 12, lwd=2, add=TRUE)
curve(model1(x, Linf=coef(fitSep)[-c(1, 3:4, 6)]), from =1, to = 12, lwd=2, add=TRUE, col = "blue")
curve(model1(x, Linf=coef(fitSep)[-c(1:2, 4:5)]), from =1, to = 12, lwd=2, add=TRUE, col = "red")
legend("topleft", legend = c("Sheppard", "Gravina", "Meares Pass"), col = c("black", "blue", "red"), 
       lwd=2, lty=1, cex=0.75)

model1  <- function(t, Linf, K = NULL) 
{
  if (length(Linf) == 2) {
    K <- Linf[[2]]
    Linf <- Linf[[1]]
  }
  Linf * (1 - exp(-K * (t)))
}



