###############################################################################################
##          Scavenger Arrival Times Statistical Analysis  - 22nd Feb 2016                    ##
###############################################################################################
rm(list=ls())

###############################################################################################
## NetLogo Model Arrival Times 
###############################################################################################
wbv<-116 
lfv<-8
eagles<-80
jackals<-65
hyenas<-42
totalIndividuals <- 100*c(wbv,lfv,eagles,jackals,hyenas) # 11600, 800, 8000, 6500, 4200
totalPop<-sum(wbv,lfv,eagles,jackals,hyenas)*100 # x 100 because there are 100 model runs = 31100
proportion<-totalIndividuals/totalPop # to get proportion, 0.37299035, 0.02572347, 0.25723473, 0.20900322, 0.13504823 
##############################################################
## Model A
##############################################################
## counts of first species to arrive, this acts as the observed 
firstwbv <-578
firstlfv <- 125
firsteagle <-904
firstjackal <- 66
firsthyena <- 37
totalObs<-sum(firstwbv, firstlfv, firsteagle, firstjackal, firsthyena) # 1710

## The proportions multiplied by the total number of observations (1710) would give you the expected counts.
expectedCounts<-totalObs*proportion # 637.8135,  43.98714, 439.87138, 357.3955, 230.93248
## run a chi-squared test of independence
freqs = c(578, 125, 904, 66, 37,
                          673.8, 44, 440, 357.4, 231)
data = matrix(freqs, nrow=5)
dimnames(data) = list( 
  c("wbv", "lfv", "eagle", "jackal", "hyena"),         
  c("observed", "expected"))
data

result <- chisq.test(data)
result
result$observed   # observed counts 
result$expected   # expected counts under the null
result$residuals  # Pearson residuals
result$stdres     # standardized residuals
## If the p value is smaller than 0.05 you reject the null hypothesis that the first arrivals are identically distributed across all species.

Input =(
  
  "Value     AWBV  LFV    Eagle   Jackal  Hyena
  Observed  578    125    904      66      37
  Expected  673.8  44      440      357.4  231   
  ")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE, 
                              row.names=1))

Matriz

par(mfrow=c(1,2)) ## plot a multipanel plot 

barplot(Matriz, 
        beside=TRUE, 
        #legend=TRUE,  
        xlab="Species",
        ylab="First to arrive",
        main= "(A)"
)

legend("topright", 
       legend = c("Observed", "Expected"), 
       fill = c("black", "grey"), bty = "n")
##############################################################
## Model B
##############################################################
## counts of first species to arrive, this acts as the observed 
firstwbv <-1116
firstlfv <- 79
firsteagle <- 524
firstjackal <- 34
firsthyena <- 63
totalObs<-sum(firstwbv, firstlfv, firsteagle, firstjackal, firsthyena) # 1816

## The proportions multiplied by the total number of observations (1816) would give you the expected counts.
expectedCounts<-totalObs*proportion # 677.35048,  46.71383, 467.13826, 379.54984, 245.24759
## run a chi-squared test of independence
freqs = c(1116, 79, 524, 34, 63,
          677.3, 46.7, 467.1, 379.5, 245.2)

data = matrix(freqs, nrow=5)
dimnames(data) = list( 
  c("wbv", "lfv", "eagle", "jackal", "hyena"),         
  c("observed", "expected"))
data

result <- chisq.test(data)
result

Input =(
  
  "Value     AWBV  LFV  Eagle   Jackal  Hyena
  Observed  1116    79   524      34      63
  Expected  677.3  46.7  467.1   379.5  245.2   
  ")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE, 
                              row.names=1))

Matriz


barplot(Matriz, 
        beside=TRUE, 
        #legend=TRUE, 
        xlab="Species",
        ylab="First to arrive",
        main = "(B)"
)

legend("topright", 
       legend = c("Observed", "Expected"), 
       fill = c("black", "grey"), bty = "n")
##############################################################
## Model C
##############################################################
## counts of first species to arrive, this acts as the observed 
firstwbv <- 1083
firstlfv <- 101
firsteagle <- 520
firstjackal <- 50
firsthyena <- 60
totalObs<-sum(firstwbv, firstlfv, firsteagle, firstjackal, firsthyena) # 1814

## The proportions multiplied by the total number of observations (1814) would give you the expected counts.
expectedCounts<-totalObs*proportion # 676.6045,  46.66238, 466.62379, 379.13183, 244.97749
## run a chi-squared test of independence
freqs = c(1083,101,520,50,60,
                     676.6, 46.7, 466.6, 379.1, 245)

data = matrix(freqs, nrow=5)
dimnames(data) = list( 
    c("wbv", "lfv", "eagle", "jackal", "hyena"),         
    c("observed", "expected"))
data

result <- chisq.test(data)
result
## If the p value is smaller than 0.05 you reject the null hypothesis that the first arrivals are identically distributed across all species.


Input =(
  
  "Value     AWBV  LFV  Eagle   Jackal  Hyena
  Observed  1083   101  520      50       60
  Expected  676.6  46.7 466.6    379.1   245   
  ")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE, 
                              row.names=1))

Matriz


barplot(Matriz, 
        beside=TRUE, 
        #legend=TRUE, 
        xlab="Species",
        ylab="First to arrive"
)

legend("topright", 
       legend = c("Observed", "Expected"), 
       fill = c("black", "grey"), bty = "n")

##############################################################
## Model D
##############################################################
## counts of first species to arrive, this acts as the observed 
firstwbv <- 1072
firstlfv <- 85
firsteagle <- 540
firstjackal <- 59
firsthyena <- 55
totalObs<-sum(firstwbv, firstlfv, firsteagle, firstjackal, firsthyena) # 1811

## The proportions multiplied by the total number of observations (1814) would give you the expected counts.
expectedCounts<-totalObs*proportion #  675.48553,  46.58521, 465.85209, 378.50482, 244.57235
## run a chi-squared test of independence
freqs = c(1072,85,540,59,55,
          675.4, 46.6, 466, 378.5, 244.6)

data = matrix(freqs, nrow=5)
dimnames(data) = list( 
  c("wbv", "lfv", "eagle", "jackal", "hyena"),         
  c("observed", "expected"))
data

result <- chisq.test(data)
result
## If the p value is smaller than 0.05 you reject the null hypothesis that the first arrivals are identically distributed across all species.


Input =(
  
  "Value     AWBV  LFV  Eagle   Jackal  Hyena
  Observed  1072    85   540      59      55
  Expected  675.4  46.6  466     378.5  244.6   
  ")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE, 
                              row.names=1))

Matriz


barplot(Matriz, 
        beside=TRUE, 
        #legend=TRUE, 
        xlab="Species",
        ylab="First to arrive"
)

legend("topright", 
       legend = c("Observed", "Expected"), 
       fill = c("black", "grey"), bty = "n")

##############################################################
## Model E
##############################################################
## counts of first species to arrive, this acts as the observed 
firstwbv <- 858
firstlfv <- 40
firsteagle <- 517
firstjackal <- 135
firsthyena <- 160
totalObs<-sum(firstwbv, firstlfv, firsteagle, firstjackal, firsthyena) # 1710

## The proportions multiplied by the total number of observations (1710) would give you the expected counts.
expectedCounts<-totalObs*proportion # 637.8135  43.98714 439.87138 357.39550 230.93248
## run a chi-squared test of independence
freqs = c(858,40,517,135,160,
          637.8, 44, 440, 357.4, 231)

data = matrix(freqs, nrow=5)
dimnames(data) = list( 
  c("wbv", "lfv", "eagle", "jackal", "hyena"),         
  c("observed", "expected"))
data

result <- chisq.test(data)
result
## If the p value is smaller than 0.05 you reject the null hypothesis that the first arrivals are identically distributed across all species.


Input =(
  
  "Value     AWBV  LFV  Eagle   Jackal  Hyena
  Observed  858    40   517      135     160
  Expected  637.8  44   440      357.4   231   
  ")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE, 
                              row.names=1))

Matriz


barplot(Matriz, 
        beside=TRUE, 
        #legend=TRUE, 
        xlab="Species",
        ylab="First to arrive"
)

legend("topright", 
       legend = c("Observed", "Expected"), 
       fill = c("black", "grey"), bty = "n")

 
##############################################################
## Model F
##############################################################
## counts of first species to arrive, this acts as the observed 
firstwbv <- 663
firstlfv <- 47
firsteagle <- 336
firstjackal <- 311
firsthyena <- 426
totalObs<-sum(firstwbv, firstlfv, firsteagle, firstjackal, firsthyena) # 1783

## The proportions multiplied by the total number of observations (1783) would give you the expected counts.
expectedCounts<-totalObs*proportion #  665.0418  45.86495 458.64952 372.65273 240.791
## run a chi-squared test of independence
freqs = c(663,47,336,311,426,
          665, 46, 459, 373, 241)

data = matrix(freqs, nrow=5)
dimnames(data) = list( 
  c("wbv", "lfv", "eagle", "jackal", "hyena"),         
  c("observed", "expected"))
data

result <- chisq.test(data)
result
## If the p value is smaller than 0.05 you reject the null hypothesis that the first arrivals are identically distributed across all species.


Input =(
  
  "Value     AWBV  LFV  Eagle   Jackal  Hyena
  Observed  663    47    336     311     426
  Expected  665    46    459     373     241   
  ")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE, 
                              row.names=1))

Matriz


barplot(Matriz, 
        beside=TRUE, 
        #legend=TRUE, 
        xlab="Species",
        ylab="First to arrive"
)

legend("topright", 
       legend = c("Observed", "Expected"), 
       fill = c("black", "grey"), bty = "n")

##############################################################
## Model G
##############################################################
## counts of first species to arrive, this acts as the observed 
firstwbv <- 1076
firstlfv <- 80
firsteagle <- 560
firstjackal <- 61
firsthyena <- 38
totalObs<-sum(firstwbv, firstlfv, firsteagle, firstjackal, firsthyena) # 1815

## The proportions multiplied by the total number of observations (1815) would give you the expected counts.
expectedCounts<-totalObs*proportion #  676.9775  46.6881 466.8810 379.3408 245.1125
## run a chi-squared test of independence
freqs = c(1076,80,560,61,38,
          677, 47, 467, 379, 245)

data = matrix(freqs, nrow=5)
dimnames(data) = list( 
  c("wbv", "lfv", "eagle", "jackal", "hyena"),         
  c("observed", "expected"))
data

result <- chisq.test(data)
result
## If the p value is smaller than 0.05 you reject the null hypothesis that the first arrivals are identically distributed across all species.


Input =(
  
  "Value     AWBV  LFV  Eagle   Jackal  Hyena
  Observed  1076    80   560       61      38
  Expected  677     47   467      379     245   
  ")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE, 
                              row.names=1))

Matriz


barplot(Matriz, 
        beside=TRUE, 
        #legend=TRUE, 
        xlab="Species",
        ylab="First to arrive"
)

legend("topright", 
       legend = c("Observed", "Expected"), 
       fill = c("black", "grey"), bty = "n")

##############################################################
## Model H
##############################################################
## counts of first species to arrive, this acts as the observed 
firstwbv <- 857
firstlfv <- 61
firsteagle <- 489
firstjackal <- 157
firsthyena <- 260
totalObs<-sum(firstwbv, firstlfv, firsteagle, firstjackal, firsthyena) # 1824

## The proportions multiplied by the total number of observations (1824) would give you the expected counts.
expectedCounts<-totalObs*proportion #  680.33441  46.91961 469.19614 381.22186 246.32797
## run a chi-squared test of independence
freqs = c(857,61,489,157,260,
          680, 47, 469, 381, 246)

data = matrix(freqs, nrow=5)
dimnames(data) = list( 
  c("wbv", "lfv", "eagle", "jackal", "hyena"),         
  c("observed", "expected"))
data

result <- chisq.test(data)
result
## If the p value is smaller than 0.05 you reject the null hypothesis that the first arrivals are identically distributed across all species.


Input =(
  
  "Value     AWBV  LFV  Eagle   Jackal  Hyena
  Observed  857     61   489       157    260
  Expected  680     47   469       381    246   
  ")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE, 
                              row.names=1))

Matriz


barplot(Matriz, 
        beside=TRUE, 
        #legend=TRUE, 
        xlab="Species",
        ylab="First to arrive"
)

legend("topright", 
       legend = c("Observed", "Expected"), 
       fill = c("black", "grey"), bty = "n")


##############################################################
## Model I
##############################################################
## counts of first species to arrive, this acts as the observed 
firstwbv <- 673
firstlfv <- 56
firsteagle <- 487
firstjackal <- 360
firsthyena <- 248
totalObs<-sum(firstwbv, firstlfv, firsteagle, firstjackal, firsthyena) # 1824

## The proportions multiplied by the total number of observations (1783) would give you the expected counts.
expectedCounts<-totalObs*proportion #  680  47 469 381 246
## run a chi-squared test of independence
freqs = c(673,56,487,360,248,
          680, 47, 469, 381, 246)

data = matrix(freqs, nrow=5)
dimnames(data) = list( 
  c("wbv", "lfv", "eagle", "jackal", "hyena"),         
  c("observed", "expected"))
data

result <- chisq.test(data)
result
## If the p value is smaller than 0.05 you reject the null hypothesis that the first arrivals are identically distributed across all species.


Input =(
  
  "Value     AWBV  LFV  Eagle   Jackal  Hyena
  Observed  673    56    487     360     248
  Expected  680    47    469     381     246   
  ")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE, 
                              row.names=1))

Matriz


barplot(Matriz, 
        beside=TRUE, 
        #legend=TRUE, 
        xlab="Species",
        ylab="First to arrive"
)

legend("topright", 
       legend = c("Observed", "Expected"), 
       fill = c("black", "grey"), bty = "n")

##############################################################
## Model L
##############################################################
## counts of first species to arrive, this acts as the observed 
firstwbv <-568
firstlfv <- 118
firsteagle <-957
firstjackal <- 71
firsthyena <- 130
totalObs<-sum(firstwbv, firstlfv, firsteagle, firstjackal, firsthyena) # 1844

## The proportions multiplied by the total number of observations (1844) would give you the expected counts.
expectedCounts<-totalObs*proportion # 687.79421  47.43408 474.34084 385.40193 249.02894
## run a chi-squared test of independence
freqs = c(568, 118, 957, 71, 130,
          688, 47, 474, 385, 249)
data = matrix(freqs, nrow=5)
dimnames(data) = list( 
  c("wbv", "lfv", "eagle", "jackal", "hyena"),         
  c("observed", "expected"))
data

result <- chisq.test(data)
result
result$observed   # observed counts 
result$expected   # expected counts under the null
result$residuals  # Pearson residuals
result$stdres     # standardized residuals
## If the p value is smaller than 0.05 you reject the null hypothesis that the first arrivals are identically distributed across all species.

Input =(
  
  "Value     AWBV  LFV    Eagle   Jackal  Hyena
  Observed  568    118     957      71    130
  Expected  688    47       474    385    249  
  ")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE, 
                              row.names=1))

Matriz

par(mfrow=c(1,2)) ## plot a multipanel plot 

barplot(Matriz, 
        beside=TRUE, 
        #legend=TRUE,  
        xlab="Species",
        ylab="First to arrive",
        main= "(A)"
)

legend("topright", 
       legend = c("Observed", "Expected"), 
       fill = c("black", "grey"), bty = "n")


###############################################################################################
## Empirical Arrival Times 
###############################################################################################
rm(list=ls())

library(plyr)
library(dplyr) 
library(qdapTools)
setwd("C:/Users/akane/Desktop/Science/Manuscripts/Corinne Scavengers/Carcass Data")
mydata<-read.csv("CleanedData.csv", header = T, sep = ",")
class(mydata$AWBO)
names(mydata)

## subset to keep only columns with species order data 
subData <- mydata[, 4:13]
subData

## Can count the number of species by summing the number of elements in each row which correspond to different carcaasses 
Richness<-rowSums(subData>0)

## Can count the number of times each species arrived to a carcass over the whole dataset 
speciesSuccess<-colSums(subData>0)
## plot it
barplot(speciesSuccess)

## Find out which species arrived first at each carcass
firstSpecies<-apply(subData,1,function(x) which(x==1))
## dput(firstSpecies, file = "firstSpecies.txt") 
## sapply is another option for the same output 
sapply(subData, function(x) which(x==1))

## Look at the summary of those who arrived first
levels(mydata$First)
summary(mydata$First) # Bateleurs first 25 times, Tawnys first 19 times 
## 75 carcasses
foundFood<-75
eaglesFirst<-25+19
## Run a binomial test to see if this number is significant 
binom.test(eaglesFirst, foundFood, p=0.2) ## p should be weighted according to number of species or assumed proportions in the area 

## Counting the number of times each species arrived first, second, third etc.  
## There are a number of options: 

## qdapTools option
# arrivalFreq1<-t(mtabulate(subData)) # matrix class
## for loop option
# arrivalFreq2 <- matrix(0, nrow= 11, ncol=14, dimnames= list(0:10, LETTERS[1:14]))
# for (i in 0:10) {
#  arrivalFreq2[i,] <- unlist(lapply(subData, function(j) sum(j == i)))
# }
# arrivalFreq2
## apply function option
arrivalFreq3<-do.call(cbind, lapply(subData, function(x){table(factor(x,levels=0:10))}))
## base R option, note that this doesn't preserve order of the columns 
# arrivalFreq4<-table(unlist(subData), rep(names(subData),each=nrow(subData)))

arrivalFreq3[2,] ## animals that arrived first 
length(mydata$carcass) ## 75
firstArrivals<-(arrivalFreq3[2,]/75)*100
totalObsEmpirical<-sum(arrivalFreq3[2,]) ## 75

arrivalFreq3[3,] ## animals that arrived second 
secondArrivals<-(arrivalFreq3[3,]/75)*100

## prop of arrivals at different times 
(arrivalFreq3/75)
## round the values 
round((arrivalFreq3/75),digits = 2)

## % of arrivals at different times 
(arrivalFreq3/75)*100
## round the values 
round((arrivalFreq3/75)*100,digits = 2)

df <- read.table(header=T, text="
  AWBO  BatO TawnyO JackalO   HVO   WHO HyenaO  LFVO    MO   RVO
0  17.33 60.00  17.33   54.67 61.33 80.00  66.67 20.00 54.67 48.00
1  17.33 33.33  25.33    2.67  9.33  1.33   1.33  5.33  4.00  0.00
2  14.67  4.00  38.67    1.33  6.67  5.33   0.00 20.00  2.67  4.00
3  33.33  1.33   9.33    6.67  4.00  5.33   1.33 16.00  5.33  9.33
4  14.67  1.33   4.00    8.00  4.00  1.33   2.67 28.00  5.33 14.67
5   1.33  0.00   2.67   14.67  4.00  5.33  12.00  9.33 10.67 13.33
6   1.33  0.00   2.67    4.00  6.67  1.33   8.00  1.33 13.33  9.33
7   0.00  0.00   0.00    6.67  1.33  0.00   5.33  0.00  1.33  1.33
8   0.00  0.00   0.00    1.33  2.67  0.00   2.67  0.00  0.00  0.00
9   0.00  0.00   0.00    0.00  0.00  0.00   0.00  0.00  2.67  0.00
10  0.00  0.00   0.00    0.00  0.00  0.00   0.00  0.00  0.00  0.00
")
df$AWBO
################################################################
## Count who followed whom 
################################################################
## counts the number of instances where there was a positive 
## difference of 1 in arrival order
## the function as it stands takes into account 0 values
## can get around this by replacing them with 100s
subData[subData==0]<-100
sapply(colnames(subData),function(v1){
  return(sapply(colnames(subData),
                function(v2){
                  return(sum(subData[,v1]-subData[,v2] == 1))
                }))
})
dfFollowing <- read.table(header=T, text="
         AWBO BatO TawnyO JackalO HVO WHO HyenaO LFVO MO RVO
AWBO       0    1      9       6   1   4      2   19  5  13
BatO       3    0     18       0   1   2      0    4  0   1
TawnyO    21    1      0       3   5   6      0   13  5   2
JackalO    1    0      1       0   5   1      7    3  2   2
HVO        4    0      6       1   0   0      3    3  3   0
WHO        1    0      3       0   2   0      0    5  2   1
HyenaO     1    0      1       2   1   0      0    0  4   1
LFVO      12    2      3       9   5   1      3    0  4  15
MO         4    1      0       4   1   0      1    3  0   4
RVO        2    0      2       7   1   0      8    6  6   0
")
dfFollowing

totalFollowed<-colSums(dfFollowing) # the total number of times the focal species followed another species
maxFollowed<-apply(dfFollowing,2,max) # the max gives the species the focal animal followed most frequently
maxFollowed/totalFollowed
################################################################
## Plotting data based on average arrival order
################################################################
library("Hmisc")
subData[subData==0]<-NA
colMeans(subData,na.rm = TRUE)
colSd <- function (x, na.rm=FALSE) apply(X=x, MARGIN=2, FUN=sd, na.rm=na.rm)
colSd(subData,na.rm = TRUE)

orderMean<-colMeans(subData,na.rm = TRUE)
orderSD<-colSd(subData,na.rm = TRUE)

MeanOrder<-sort(colMeans(subData,na.rm = TRUE)) ## ordered from lowest to highest 
SdByMeanOrder<-c(0.6914918,1.1940271,1.1443883,1.2020463,1.5023791,2.2877915, 1.2667235, 1.7006343,1.9775928, 1.5307950)
species<-c("Bateleur", "Tawny", "AWBV","LFV", "WHV","HV","RV","Jackal","Marabou","Hyena" )
## species<-data.frame(f=c("AWBO", "BatO", "TawnyO", "JackalO",  "HVO", "WHO", "HyenaO", "LFVO","MO", "RVO"))
## species$f <- factor(species$f, levels=unique(species$f))

## plot with error bars 
d = data.frame(
  x  = species
  , y  = MeanOrder
  , sd = SdByMeanOrder
)

plot(d$x, d$y, type="n")
with (
  data = d
  , expr = errbar(x, y, y+sd, y-sd, add=F, pch=16, cap=.015, lty= 2, ylab="Arrival order")
)

## The above produces a graph with error bars that go beyond 0
## Use boxplots instead

library(dplyr) ## for 'order by' function
arrivalOrder<-read.csv("ArrivalOrderSummary.csv", header = T, sep = ",")
head(arrivalOrder)

## Specify the order the species should be in 
orderedArrival <- factor(arrivalOrder$species,
                       levels = c("Bateleur", "Tawny", "AWBV","LFV", "WHV","HV","RV","Jackal","Marabou","Hyena" ),ordered = TRUE)

## base R version
par(mar=c(4,6,1,1))
boxplot(arrivalOrder$order~orderedArrival, horizontal = T, pch = 16, las=1, xlab="arrival order")
## add all of the data points
stripchart(arrivalOrder$order~orderedArrival,  
           method = "jitter", add = TRUE, pch = 1, col = 'grey', jitter=0.2)


## ggplot version
ggplot(arrivalOrder, aes(orderedArrival,order )) + geom_boxplot() + coord_flip() +
labs(x="species", y = "arrival order") +   
   theme_grey() +
  scale_y_continuous(breaks=seq(0,9,1)) + theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
theme(axis.line.x = element_line(color="black", size = 1),
      axis.line.y = element_line(color="black", size = 1))

## Jitter Plot 
# par(mfrow=c(1,2)) ## plot a multipanel plot 

jitterplot1 <- ggplot(arrivalOrder, aes(orderedArrival,arrivalOrder$order )) + geom_jitter(alpha = I(1),width = 0.25, height = 0.25, aes(color = arrivalOrder$species)) +
  labs(x="species", y = "arrival order") +   
  theme_bw() +
  scale_y_continuous(breaks=seq(0,9,1)) + theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + coord_flip() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) + theme(legend.position = "none") + 
  stat_summary(fun.y=median, geom="point", shape=18, size=3, color="black")+ stat_summary(fun.y=median, geom="point", shape=18,size=3, color="black")

################################################################
## Plotting data based on average arrival time
################################################################
## http://www.sthda.com/english/wiki/ggplot2-stripchart-jitter-quick-start-guide-r-software-and-data-visualization
setwd("C:/Users/akane/Desktop/Science/Manuscripts/Corinne Scavengers/Carcass Data")
arrivalTimes<-read.csv("ArrivalTimesSummary.csv", header = T,sep = ",")
head(arrivalTimes)

colMeans(arrivalTimes,na.rm = TRUE)
colSd <- function (x, na.rm=FALSE) apply(X=x, MARGIN=2, FUN=sd, na.rm=na.rm)
colSd(arrivalTimes,na.rm = TRUE)

MeanOrder<-sort(colMeans(arrivalTimes,na.rm = TRUE)) 
species<-c("Bateleur", "Tawny", "AWBV","Jackal", "WHV","LFV","RV","Hyena","Marabou","HV" )
SdByMeanOrder<-c(28.84346,30.44253,43.12647,20.034483,33.34381,33.17669,34.89172,29.79805,43.36864,65.70935)

d = data.frame(
  x  = species
  , y  = MeanOrder
  , sd = SdByMeanOrder
)


plot(d$x, d$y, type="n")
with (
  data = d
  , expr = errbar(x, y, y+sd, y-sd, add=F, pch=16, cap=.015, lty= 2, ylab="Arrival time (mins)")
)

## Calculate the mode for arrival times
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
apply(arrivalTimes,2,Mode,na.rm=1)


## Specify order of species 
orderedArrivaltime <- factor(arrivalTimes$species,
                         levels = c("Bateleur", "Tawny", "AWBV","LFV", "WHV","HV","RV","Jackal","Marabou","Hyena" ),ordered = TRUE)
## Boxplot for arrival times 
## Base R
par(mar=c(4,6,1,1))
boxplot(arrivalTimes$time~orderedArrivaltime, horizontal = T, pch = 16, las=1, xlab="arrival time (mins)")

## ggplot
ggplot(arrivalTimes, aes(orderedArrivaltime,time )) + geom_boxplot() + coord_flip() +
  labs(x="species", y = "arrival order") +   
  theme_grey() +
  scale_y_continuous(breaks=seq(0,300,10)) + theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))

## dotplot
dotplot(orderedArrivaltime~time, data = arrivalTimes)
library(HH)
stripplot(orderedArrivaltime~time, data = arrivalTimes, panel=HH::panel.dotplot.tb, factor=0.5)

## Jitter Plot 
jitterplot2 <- ggplot(arrivalTimes, aes(orderedArrivaltime,arrivalTimes$time )) + geom_jitter(alpha = I(1),width = 0.25, aes(color = arrivalTimes$species)) +
  labs(x="species", y = "time (mins)") +   
  theme_bw() +
  scale_y_continuous(breaks=seq(0,300,10)) + theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + coord_flip() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) + theme(legend.position = "none") + stat_summary(fun.y=median, geom="point", shape=18, size=3, color="black")


multiplot(jitterplot1, jitterplot2, cols=1)


#################################################################################################
## Visual Acuity of Scavengers 
#################################################################################################
# https://petavoxel.wordpress.com/2010/02/26/cycles-per-degree/
# https://sites.oxy.edu/clint/physio/article/topographyofphotoreceptorsandretinalganglioncellsinthespottedhyena.pdf
# paper gives a measure of 8.4 cycles/deg. for a Spotted Hyena
library(aspace)
AL<-c(28.14,27.8,20.71) # Axial length Lappet, Tawny, AWBV
10^(1.42*log10(AL)-0.11)
target<-0.0139 # cycle width, the width of your "cycle" (one black line and the white gap).
distance<-36 # distance to target point where you'll find it impossible to see the white gap between the lines. 
target/distance
atan_d(0.0003861111) # degrees per cycle 
# To get cycles per degree, divide one by that number.
1/atan_d(0.0003861111) # cycles per degree

# Lappet-faced Vulture test
target<-2
distance<-10178
target/distance
atan_d(0.0001965023) # degrees per cycle 
# To get cycles per degree, divide one by that number.
1/atan_d(0.0001965023) # cycles per degree

# African white-backed Vulture test
target<-2
distance<-6594
target/distance
atan_d(0.000303306) # degrees per cycle 
# To get cycles per degree, divide one by that number.
1/atan_d(0.000303306) # cycles per degree

# Spotted hyena
target<-2.2
distance<-1000
target/distance
atan_d(0.0022) # degrees per cycle 
# To get cycles per degree, divide one by that number.
1/atan_d(0.0022) # cycles per degree
#################################################################################################
## hyena
#################################################################################################
# takes a hyena travelling at 10km/hr 6 minutes to reach a target 1 km away 
(1/10) * 60 
# + 20% to account for uneven terrain etc = 7.2 minutes

# takes a hyena travelling at 10km/hr 9 minutes to reach a target 1.5 km away 
(1.5/10) * 60 
# + 20% to account for uneven terrain etc = 11 minutes 

# takes a hyena travelling at 10km/hr 12 minutes to reach a target 2 km away 
(2/10) * 60 
# + 20% to account for uneven terrain etc = 14 minutes 

# takes a hyena travelling at 10km/hr 24 minutes to reach a target 4 km away 
(4/10) * 60 
#################################################################################################
## jackal
#################################################################################################
# takes a jackal travelling at 16km/hr 3 minutes 45 seconds to reach a target 1 km away 
(1/16) * 60 

# takes a jackal travelling at 12km/hr 5 minutes to reach a target 1 km away 
(1/12) * 60 
# + 20% to account for uneven terrain etc = 6 minutes 

# takes a jackal travelling at 10km/hr 7.5 minutes to reach a target 1.5 km away 
(1.5/12) * 60 
# + 20% to account for uneven terrain etc = 9 minutes

# takes a jackal travelling at 12km/hr 10 minutes to reach a target 2 km away 
(2/12) * 60 
# + 20% to account for uneven terrain etc = 12 minutes 

# takes a jackal travelling at 16km/hr 7 minutes 30 seconds to reach a target 2 km away 
(2/16) * 60 
# takes a jackal travelling at 16km/hr 15 minutes to reach a target 4 km away 
(4/16) * 60 


#################################################################################################
## Wilcox test for species associations
#################################################################################################
library(exactRankTests)
## Example 
## x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
## y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
## wilcox.test(x, y, paired = TRUE, alternative = "two.sided")
#################################################################################################
## Jackal Interactions
#################################################################################################
wilcoxDataJackal<-read.csv("wilcoxJackal.csv", header = T, sep = ",")
head(wilcoxDataJackal)
# wilcoxDataJackal <- wilcoxDataJackal[-c(1,6,7,8,9,12,13,21), ]

## Lappets and Jackals
wilcox.test(wilcoxDataJackal$LFV, wilcoxDataJackal$Jackal1, paired = TRUE, alternative = "two.sided")
wilcox.exact(wilcoxDataJackal$LFV, wilcoxDataJackal$Jackal1, paired = TRUE, alternative = "two.sided")
## AWBVs and Jackals 
wilcox.test(wilcoxDataJackal$AWBV, wilcoxDataJackal$Jackal2, paired = TRUE, alternative = "two.sided")
wilcox.exact(wilcoxDataJackal$AWBV, wilcoxDataJackal$Jackal2, paired = TRUE, alternative = "two.sided")
## RV and Jackals 
wilcox.test(wilcoxDataJackal$RV, wilcoxDataJackal$Jackal3, paired = TRUE, alternative = "two.sided")
wilcox.exact(wilcoxDataJackal$RV, wilcoxDataJackal$Jackal3, paired = TRUE, alternative = "two.sided")
## Bateleur and Jackals 
wilcox.test(wilcoxDataJackal$Bateleur, wilcoxDataJackal$Jackal4, paired = TRUE, alternative = "two.sided")
wilcox.exact(wilcoxDataJackal$Bateleur, wilcoxDataJackal$Jackal4, paired = TRUE, alternative = "two.sided")
## Tawny and Jackals 
wilcox.test(wilcoxDataJackal$Tawny, wilcoxDataJackal$Jackal5, paired = TRUE, alternative = "two.sided")
wilcox.exact(wilcoxDataJackal$Tawny, wilcoxDataJackal$Jackal5, paired = TRUE, alternative = "two.sided")
## WHV and Jackals 
wilcox.test(wilcoxDataJackal$WHV, wilcoxDataJackal$Jackal6, paired = TRUE, alternative = "two.sided")
wilcox.exact(wilcoxDataJackal$WHV, wilcoxDataJackal$Jackal6, paired = TRUE, alternative = "two.sided")
## HV and Jackals 
wilcox.test(wilcoxDataJackal$HV, wilcoxDataJackal$Jackal7, paired = TRUE, alternative = "two.sided")
wilcox.exact(wilcoxDataJackal$HV, wilcoxDataJackal$Jackal7, paired = TRUE, alternative = "two.sided")
## Marabou and Jackals 
wilcox.test(wilcoxDataJackal$Marabou, wilcoxDataJackal$Jackal8, paired = TRUE, alternative = "two.sided")
wilcox.exact(wilcoxDataJackal$Marabou, wilcoxDataJackal$Jackal8, paired = TRUE, alternative = "two.sided")
## Hyenas and Jackals 
wilcox.test(wilcoxDataJackal$Hyena, wilcoxDataJackal$Jackal9, paired = TRUE, alternative = "two.sided")
wilcox.exact(wilcoxDataJackal$Hyena, wilcoxDataJackal$Jackal9, paired = TRUE, alternative = "two.sided")

#################################################################################################
## Hyena Interactions
#################################################################################################
wilcoxDataHyena<-read.csv("wilcoxHyena.csv", header = T, sep = ",")
head(wilcoxDataHyena)

## Marabou and Hyenas  
wilcox.test(wilcoxDataHyena$Marabou, wilcoxDataHyena$Hyena, paired = TRUE, alternative = "two.sided")
wilcox.exact(wilcoxDataHyena$Marabou, wilcoxDataHyena$Hyena, paired = TRUE, alternative = "two.sided")
## AWBVs and Hyenas  
wilcox.test(wilcoxDataHyena$AWBV, wilcoxDataHyena$Hyena2, paired = TRUE, alternative = "two.sided")
wilcox.exact(wilcoxDataHyena$AWBV, wilcoxDataHyena$Hyena2, paired = TRUE, alternative = "two.sided")
## LFVs and Hyenas  
wilcox.test(wilcoxDataHyena$LFV, wilcoxDataHyena$Hyena3, paired = TRUE, alternative = "two.sided")
wilcox.exact(wilcoxDataHyena$LFV, wilcoxDataHyena$Hyena3, paired = TRUE, alternative = "two.sided")
## Bateleur and Hyenas  
wilcox.test(wilcoxDataHyena$Bateleur, wilcoxDataHyena$Hyena4, paired = TRUE, alternative = "two.sided")
wilcox.exact(wilcoxDataHyena$Bateleur, wilcoxDataHyena$Hyena4, paired = TRUE, alternative = "two.sided")
## RV and Hyenas  
wilcox.test(wilcoxDataHyena$RV, wilcoxDataHyena$Hyena5, paired = TRUE, alternative = "two.sided")
wilcox.exact(wilcoxDataHyena$RV, wilcoxDataHyena$Hyena5, paired = TRUE, alternative = "two.sided")
## WHV and Hyenas  
wilcox.test(wilcoxDataHyena$WHV, wilcoxDataHyena$Hyena6, paired = TRUE, alternative = "two.sided")
wilcox.exact(wilcoxDataHyena$WHV, wilcoxDataHyena$Hyena6, paired = TRUE, alternative = "two.sided")
## Tawny and Hyenas  
wilcox.test(wilcoxDataHyena$Tawny, wilcoxDataHyena$Hyena7, paired = TRUE, alternative = "two.sided")
wilcox.exact(wilcoxDataHyena$Tawny, wilcoxDataHyena$Hyena7, paired = TRUE, alternative = "two.sided")
## HV and Hyenas  
wilcox.test(wilcoxDataHyena$HV, wilcoxDataHyena$Hyena8, paired = TRUE, alternative = "two.sided")
wilcox.exact(wilcoxDataHyena$HV, wilcoxDataHyena$Hyena8, paired = TRUE, alternative = "two.sided")

#################################################################################################
## Probability of discovering a carcass, Jackson et al 2008
#################################################################################################
probLocate <- function(M,r,v,t,pi,A){
  (M*((2*r*v*t) + (pi*r^2)))/A
  }
M<-18 # number of carcasses
r<-0.5 # detection distance
v<-10 # speed km-1
t<-10 # foraging time hours
A<-400 # area in km^2

probLocate(18,4,45,10,pi,400) ## vulture
probLocate(18,0.5,10,10,pi,400) ## mammal

probLocate(10,0.3,33,3,pi,2500) ## Jackson version

#################################################################################################
## Kendall Coefficient of Concordance for Vague Data 
#################################################################################################
## http://www.sciencedirect.com/science/article/pii/S0167947306001277
rm(list =ls() )
#################################################################################################
## Create the function
#################################################################################################
vagueKendall<-function(u,v,n){
  u<-as.matrix (u)
  v<-as.matrix (v)
  ## divide by the number of films -1 
  uColNo<-u/(n-1)
  vColNo<-v/(n-1)
  ## take the averages
  colMeansU<-colMeans(uColNo)
  colMeansV<-colMeans(vColNo)
  ## measure the distances from the averages 
  au = (colMeansU - 1/2)^2
  av = (colMeansV - 1/2)^2
  ## calculate component before sum
  outside<-6*(n-1)/(n*(n+1))
  ## sum of squared distances from averages 
  sumSqdDiff<-sum(au+av)
  ## The product of these gives the modified Kendall's W
  W<-outside*sum(au+av)
  return(W)
}
#################################################################################################
## Extract p-value function
#################################################################################################
vagueKendallP<-function(W,k,n){
  ## Calculate correlation coefficient 
  r<-(k*W-1)/(k-1)
  ## Calculate Chi Squared
  Chi<-k*(n-1)*W
  ## degrees of freedom
  df<-n-1
  ## p-value 
  pValue<-pchisq(Chi,df, lower.tail = FALSE)
  return(pValue)
}
#################################################################################################
## Empirical Data
#################################################################################################
## deleted 3rd and 42nd entry because only one animal arrived 
## setwd("C:/Users/akane/Desktop/Science/Manuscripts/Corinne Scavengers/Carcass Data")
## mydata<-read.csv("ArrivalCombined.csv", header = T, sep = ",")
#################################################################################################
## Empirical Data with mammals split into jackals & hyenas 
#################################################################################################
setwd("C:/Users/akane/Desktop/Science/Manuscripts/Corinne Scavengers/Carcass Data")
mydata<-read.csv("orderDataJackal.csv", header = T, sep = ",")
## transform into a matrix
M<-as.matrix(mydata)
## check the dimensions of the matrix with:
dim(M)
n<-6 # number of columns 
k<-73 # number of rows 
## Clean up the matrix to create the worse element matrix u and better element matrix v
M[M==0]<-NA;apply(M,1,rank,na.last="keep")
rowSums(M>0, na.rm = TRUE)
sumsOfRows<-rowSums(M>0, na.rm = TRUE)
u<-abs(sweep(M,MARGIN=1,sumsOfRows,`-`))
v<-abs(sweep(M,MARGIN=1,1,`-`))
u[is.na(u)] <- 0;u
v[is.na(v)] <- 0;v
## apply our functions to get a value W for concordance
vagueKendall(u,v,n)
W <- vagueKendall(u,v,n)
## and a p-value associated with it 
vagueKendallP(W,k,n)

####################################################################################
##               CODE TO ANALYSE NETLOGO ARRIVAL TIMES 03/DEC/2105                ##
####################################################################################

# clear everything and set the path to the folder with the output files from NetLogo 
rm(list=ls()) 
# path = "C:/Users/akane/Desktop/Science/Manuscripts/Corinne Scavengers/NetLogoOutput/"
# path wasn't working last time so use setwd
setwd("C:/Users/akane/Desktop/Science/Manuscripts/Corinne Scavengers/NetLogoOutput/Arrival Model Results L")

# select the variables of interest that we want to keep hold of          
myvars <- c("who", "breed", "got.here", "mycarcass")
# loop over the output files, clean them up and stitch them together in a new csv = "arrivals.csv"
out.file<-""
filenames <- dir(pattern =".csv")
lst <- vector("list", length(filenames ))

for (i in 1:length(filenames)) {
  tmp.file <- read.csv(filenames[i],header=TRUE, sep=",", skip=12) ##  
  tmp.file<-head(tmp.file, -451) ## trim off the useless data from the end
  tmp.file<- tmp.file[myvars]
  tmp.file<-tmp.file[with(tmp.file, order(mycarcass, got.here)), ] ## order the arrival times
  out.file <- rbind(out.file, tmp.file)
}

write.table(out.file, file = "arrivals.csv", row.names=F, sep=",")
## have to clean the resulting table by removing the first row, manually changing the {} and filling 
## the spaces with _

####################################################################################
## Corinne Arrival Time Code Analysis
####################################################################################
setwd("C:/Users/akane/Desktop/Science/Manuscripts/Corinne Scavengers/NetLogoOutput/Arrival Model Results L")
mydata<-read.csv("arrivals.csv", header = T, sep = ",")
head(mydata)
names(mydata)
length(mydata$breed)
levels(mydata$breed)
## drop the carcass breed because we're not interested in it as it doesn't arrive to anything 
mydata <- subset(mydata, breed != "breed_carcasses")
mydata <- subset(mydata, mycarcass != "0")
mydata$breed <- factor(mydata$breed)
mydata$mycarcass <- factor(mydata$mycarcass)

length(mydata$breed)
levels(mydata$breed)

## can extract the arrival times of specific species as follows
mydata$got.here[mydata$breed=="breed_hyenas"]
mean(mydata$got.here[mydata$breed=="breed_hyenas"])

####################################################################################
## First Species to Arrive at Every Carcass
####################################################################################
## Note that sometimes multiple animals can arrive to a carcass at exactly the same time thereby 
## inflating the count of first arrival times above 1800, which is the number of carcasses per run (18) 
## multiplied by the number of runs (100).There can be fewer than 1800 results for mycarcass because some 
## simply go undiscovered 

firstArrivalData<-mydata[ mydata$got.here == ave(mydata$got.here, mydata$mycarcass, FUN=min), ]
summary(firstArrivalData)
length(firstArrivalData$mycarcass)
length(unique(firstArrivalData$mycarcass))

lastArrivalData<-mydata[ mydata$got.here == ave(mydata$got.here, mydata$mycarcass, FUN=max), ]
summary(lastArrivalData)
length(lastArrivalData$mycarcass)
length(unique(lastArrivalData$mycarcass))
####################################################################################
## Arrival Times - Jackals
####################################################################################
# for each carcass, calculate the first jackal arrival
first_jackals <- aggregate(got.here~mycarcass,
                           data=mydata[mydata$breed=="breed_jackals",], FUN=min)

# tabulate the number of other animals arriving before the jackal
beat_jackals <- sapply(unique(mydata$mycarcass), function(i) {
  table(mydata$breed[mydata$mycarcass==i & 
                       mydata$got.here < first_jackals$got.here[first_jackals$mycarcass==i]])})

# drop unwanted breeds
beat_jackals <-   beat_jackals[row.names(beat_jackals) != "breed_jackals",]
# add carcass names to the columns
colnames(beat_jackals) <- unique(mydata$mycarcass)

arrival_order_jackal <- sapply(unique(mydata$mycarcass), function(i) {
  unique(mydata[mydata$mycarcass==i, "breed"])})

sapply(arrival_order_jackal, function(i) i[(which(i=="breed_jackals"))-1])
precedeJackals<-sapply(arrival_order_jackal, function(i) i[(which(i=="breed_jackals"))-1])
tableBeatJackals<-sapply(precedeJackals, table)
rowSums(tableBeatJackals)
## compare to the number of occasions a jackal arrived which should be more because
## they will have arrived first, and therefore not followed anyone, in some instances
length(first_jackals$mycarcass)

## Observed Proportion 
wbvObs<-157
lfvObs<-177
eaglesObs<-470
hyenaObs<-442
speciesPopJackObs <-c(wbvObs, lfvObs, eaglesObs, hyenaObs)
sumSpeciesPopJackObs<-sum(speciesPopJackObs)
speciesPopJackObs/sumSpeciesPopJackObs

## Expected proportion 
wbvEx<-116 
lfvEx<-8
eaglesEx<-80
hyenasEx<-42
speciesPopJackEx <-c(wbvEx, lfvEx, eaglesEx, hyenasEx)
sumSpeciesPopJackEx<-sum(speciesPopJackEx)
speciesPopJackEx/sumSpeciesPopJackEx
####################################################################################
## Arrival Times - Hyenas
####################################################################################
# for each carcass, calculate the first hyena arrival
first_hyenas <- aggregate(got.here~mycarcass,
                          data=mydata[mydata$breed=="breed_hyenas",], FUN=min)

# tabulate the number of other animals arriving before the Hyenas
beat_hyenas <- sapply(unique(mydata$mycarcass), function(i) {
  table(mydata$breed[mydata$mycarcass==i & 
                       mydata$got.here < first_hyenas$got.here[first_hyenas$mycarcass==i]])})

# drop unwanted breeds
beat_jackals <-   beat_hyenas[row.names(beat_hyenas) != "breed_hyenas",]
# add carcass names to the columns
colnames(beat_hyenas) <- unique(mydata$mycarcass)

arrival_order_hyena <- sapply(unique(mydata$mycarcass), function(i) {
  unique(mydata[mydata$mycarcass==i, "breed"])})

sapply(arrival_order_hyena, function(i) i[(which(i=="breed_hyenas"))-1])
precedeHyenas<-sapply(arrival_order_hyena, function(i) i[(which(i=="breed_hyenas"))-1])
tableBeatHyenas<-sapply(precedeHyenas, table)
rowSums(tableBeatHyenas)
## compare to the number of occasions a hyena arrived which should be more because
## they will have arrived first, and therefore not followed anyone, in some instances
length(first_hyenas$mycarcass)

## Observed Proportion 
wbvObs<-118
lfvObs<-128
eaglesObs<-357
jackalsObs<-578
speciesPopHyObs <-c(wbvObs,lfvObs,eaglesObs,hyenaObs)
sumSpeciesPopHyObs<-sum(speciesPopHyObs)
speciesPopHyObs/sumSpeciesPopHyObs

## Expected Proportion
wbvEx<-116 
lfvEx<-8
eaglesEx<-80
jackalsEx<-65
speciesPopHyEx <-c(wbvEx,lfvEx,eaglesEx,jackalsEx)
sumSpeciesPopHyEx<-sum(speciesPopHyEx)
speciesPopHyEx/sumSpeciesPopHyEx
####################################################################################
## Compare Arrival Times of Model B without hyena local enhancement to Model C with enhancement
####################################################################################
rm(list=ls()) 
setwd("C:/Users/akane/Desktop/Science/Manuscripts/Corinne Scavengers/NetLogoOutput")
hydata<-read.csv("hyena_local_enhancement_ModelB_ModelC_ModelJ.csv", header=T, sep=",")
head(hydata)
tail(hydata)
summary(hydata)

# hydata <- na.omit(hydata)
length(hydata$enhancement)
mean(hydata$enhancement, na.rm = T)
sd(hydata$enhancement, na.rm = T)
mean(hydata$none, na.rm = T)
sd(hydata$none, na.rm = T)
mean(hydata$following, na.rm = T)
sd(hydata$following, na.rm = T)

hist(hydata$enhancement)
hist(hydata$none)
hist(hydata$following)

# log10enhance <- log10(hydata$enhancement)
# log10none <- log10(hydata$none)

sqrtenhance <- sqrt(hydata$enhancement)
sqrtnone <- sqrt(hydata$none)
sqrtfollowing <- sqrt(hydata$following)

## compare enhancement with none 
boxplot(sqrtenhance, sqrtnone, pch = 16, names = c("local enhacement", "no enhancement"), 
        xlab="Information use", ylab= "square root of arrival time")

t.test(sqrtnone,sqrtenhance)

## compare followuing with none 
boxplot(sqrtfollowing, sqrtnone, pch = 16, names = c("following", "no enhancement"), 
        xlab="Information use", ylab= "square root of arrival time")

t.test(sqrtnone,sqrtfollowing)




###############################################################################################
## Measuring Diversity 
###############################################################################################
## Test of Richness (number of different species) as a function of ID of first to arrive 
subRichData <- mydata[, 18:19]
subRichData
m1<-aov(Richness~First, data = subRichData)
summary(m1)
plot(m1)
boxplot(Richness~First, xlab = "Species", ylab = "Richness", data = subRichData)

## Shannon Diversity 
## Example from the package
## https://cran.r-project.org/web/packages/vegan/vignettes/diversity-vegan.pdf
library(vegan)
data(BCI)
head(BCI)
names(BCI)
length(BCI$Abarema.macradenia)
H <- diversity(BCI)

## Load in our data
setwd("C:/Users/akane/Desktop/Science/Manuscripts/Corinne Scavengers/Carcass Data")
mydata<-read.csv("empArrivals.csv", header = TRUE, sep = ",")
names(mydata)

## subset to keep only columns with species order data 
diversityData <- mydata[, 23:36]
diversityData

## Apply Shannon Diversity Measure 
H <- diversity(diversityData)

##  Pielou's evenness J = H'/ log(S) is easily found as
J <- H/log(specnumber(diversityData))

## write.table(J, file = "Pielou.csv", row.names=F, sep=",") ## export the data

###############################################################################################
## Diversity as a function of who arrived first
###############################################################################################
## Drop the cases where nobody arrived to the carcass
mydata <- subset(mydata, First != "0")
mydata$First <- factor(mydata$First)


m2<-aov(ShannonsH~First, data = mydata)
summary(m2)
TukeyHSD(m2)

qqnorm(mydata$ShannonsH)
qqline(mydata$ShannonsH)


qqnorm(log10(mydata$ShannonsH))
qqline(log10(mydata$ShannonsH))

kruskal.test(ShannonsH~First, data=mydata)
## Post-hoc comparison 
library(dunn.test)
dunn.test(mydata$ShannonsH, mydata$First, kw=TRUE, method="bonferroni")


#################################################################################################
## Species Associations: The Kendall Coefficient of Concordance Revisited
## http://cc.oulu.fi/~jarioksa/softhelp/vegan/html/kendall.global.html
## http://www.real-statistics.com/reliability/kendalls-w/
## Numerical Ecology by Pierre Legendre, Louis Legendre

data(mite)
mite.hel <- decostand(mite, "hel")

# Reproduce the results shown in Table 2 of Legendre (2005), a single group
mite.small <- mite.hel[c(4,9,14,22,31,34,45,53,61,69),c(13:15,23)]
kendall.global(mite.small)
kendall.post(mite.small, mult="holm")


# Reproduce the results shown in Tables 3 and 4 of Legendre (2005), 2 groups
group <-c(1,1,2,1,1,1,1,1,2,1,1,1,1,1,1,2,1,2,1,1,1,1,2,1,2,1,1,1,1,1,2,2,2,2,2)
kendall.global(mite.hel, group=group)
kendall.post(mite.hel, group=group, mult="holm", nperm=99)


#################################################################################################
## http://stackoverflow.com/questions/35582835/extracting-a-series-of-integers-using-a-loop/35583020#35582885
## http://www.upa.pdx.edu/IOA/newsom/pa551/lectur13.htm
## http://www.amstat.org/publications/jse/v18n1/eudey.pdf
## A very easy and simple example of chi-square is when the frequencies of response to one dichotomous variable are compared. 
## For this, we have just one variable that is tested. Examples of this are very common in the newspapers when polls are published. 
## For instance, we might conduct a statewide survey of a sample of 300 hospital workers and ask the question "Do you think that 
## staffing is adequate at your hospital, yes or no?" The following might be a hypothetical result:

# No  Yes
# 180 120

## The chi-square test asks if frequency of no responses is statistically larger (or different) from the frequency of yes responses. 
## To compute this, we first need to know the expected frequency. Because we expect equal to imply a .5 chance of answering no or yes, 
## we should expect half, or 150, of the survey participants to say yes or no. Thus we have this table of observed and expected frequencies:

#          No  Yes
# Observed 180 120
# Expected 150 150

M<-as.table(c(180,120))

## Xsq = sum((Oi - Ei)^2)/Ei
##     = ((180-150)^2)/150 + ((120-150)^2)/150
##     =        6          +         6
##     =                   12
Xsq<-chisq.test(M)
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals

## This test can be conducted another way, using proportions. Usually survey responses are reported as the percentage (or, less often, proportion)
## of respondents indicating yes or no. These same results could be described as 60% indicating no and 40% indicating yes. The test to see 
## whether these proportions are different is also appropriate for this example:

## z = (p_hat-pi)/sqrt(pi(1-pi)/n)

## where p_hat is the obtained proportion and pi is the proportion expected in the population (usually .5), 
## and n is the sample size for the whole sample. Plugging in our results we get:

##   = (0.6-0.5)/sqrt(0.5*(1-0.5)/300)

## Because chi-square and z are related, we should be able to show the equivalence of the two tests:
##      z^2   = Xsq
## 3.464102^2 = 12

N<-as.table(c(0.6,0.4))
prop.test(N)
#################################################################################################
## You can use the duplicated() function to find unique combinations
#################################################################################################
## df <- read.table(header=T, text="
# v1  v2  v3   v4  v5
# 1  7   1   A  100  98 
# 2  7   2   A   98  97
# 3  8   1   C   NA  80
# 4  8   1   C   78  75
# 5  8   1   C   50  62
# 6  9   3   C   75  75 
# ")

## duplicated(df)
## df[duplicated(df[1:3]),]
