#K.Palof review of Carl's code - called Abalone_length_R.R

# Set random seed to keep alkIndivAge results constant
set.seed(14354454)

#library(dplyr)
library(tidyverse)
library(magrittr)
library(FSA)
library(nnet)
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

grvab %<>% mutate(lcat5=lencat(len, w=5))       # determine length categories (in this case by 5s)
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

mrsab %<>% mutate(lcat10=lencat(len, w=10))       # determine length categories (in this case by 10s)
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



###Catch Curve shorthand###
#est. gravina data (2016 timed swims)
## simple regression
grvcc <- catchCurve(n~age, data = grvab.sumlen, ages2use = 3:10)
summary(grvcc)
confint(grvcc)
plot(grvcc, main = "Gravina CC")

R1 <- cbind(Est = coef(grvcc), confint(grvcc))

grvcc1 <- catchCurve(n~age, data = grvab.sumlen, ages2use = 5:10)
summary(grvcc1)
confint(grvcc1)
plot(grvcc1, main = "Gravina CC1")

grvcc2 <- catchCurve(n~age, data = grvab.sumlen, ages2use = 2:12)
summary(grvcc2)
confint(grvcc2)
plot(grvcc2, main = "Gravina CC2")

## weighted regression (see Maceina and Bettoli 1998 or Smith et al. 2012)
grvccW <- catchCurve(n~age, data = grvab.sumlen, ages2use = 3:10, use.weights = TRUE)
summary(grvccW)
confint(grvccW)
plot(grvccW, main = "Gravina CC weighted")
## Chapman Robson 
grvcc <- chapmanRobson(n~age, data = grvab.sumlen, ages2use = 2:12)
# for this estimator use all ages after peak catch was observed
summary(grvcc)
confint(grvcc)
plot(grvcc, main = "Gravina CC")

#est. meares pass data (2016 timed swims) 
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


