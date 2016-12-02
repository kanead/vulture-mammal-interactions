####################################################################################
##               CODE TO ANALYSE NETLOGO ARRIVAL TIMES 03/DEC/2105                ##
####################################################################################

# clear everything and set the path to the folder with the output files from NetLogo 
rm(list=ls()) 
# path = "C:/Users/akane/Desktop/Science/Manuscripts/Corinne Scavengers/NetLogoOutput/"
# path wasn't working last time so use setwd
setwd("C:/Users/akane/Desktop/Science/Manuscripts/Corinne Scavengers/NetLogoOutput/Resub Results 2")

# select the variables of interest that we want to keep hold of          
myvars <- c("who", "breed", "got.here", "mycarcass")
# loop over the output files, clean them up and stitch them together in a new csv = "arrivals.csv"
out.file<-""
filenames <- dir(pattern =".csv")
lst <- vector("list", length(filenames ))

for (i in 1:length(filenames)) {
  tmp.file <- read.csv(filenames[i],header=TRUE, sep=",", skip=12, stringsAsFactors=FALSE) ##  
 # tmp.file<-head(tmp.file, -451) ## trim off the useless data from the end
  # alternative way to do so by only keeping the rows with useful data
  tmp.file<-tmp.file[tmp.file$hidden. == "false", ]
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
setwd("C:/Users/akane/Desktop/Science/Manuscripts/Corinne Scavengers/NetLogoOutput/Resub Results 4")
mydata<-read.csv("arrivals.csv", header = T, sep = ",")
head(mydata)
names(mydata)
mydata<-mydata[complete.cases(mydata),]
head(mydata)
# get rid of curly braces 
mydata$breed<-gsub("\\{|\\}", "", mydata$breed)
mydata$mycarcass<-gsub("\\{|\\}", "", mydata$mycarcass)
head(mydata)
# replace spaces with underscores 
mydata$breed<-gsub("[ ]", "_", mydata$breed)
mydata$mycarcass<-gsub("[ ]", "_", mydata$mycarcass)
head(mydata)
# change breed and mycarcass columns to factors 
mydata$breed<-as.factor(mydata$breed)
mydata$mycarcass<-as.factor(mydata$mycarcass)
## drop the carcass breed because we're not interested in it as it doesn't arrive to anything 
mydata <- subset(mydata, breed != "breed_carcasses")
mydata <- subset(mydata, mycarcass != "0")
length(mydata$breed)
levels(mydata$breed)

## can extract the arrival times of specific species as follows
mydata$got.here[mydata$breed=="breed_hyenas"]
mean(mydata$got.here[mydata$breed=="breed_hyenas"])

# can export this data as well 
#hyenaArrival3<-mydata$got.here[mydata$breed=="breed_hyenas"]
#jackalArrival3<-mydata$got.here[mydata$breed=="breed_jackals"]
#write.table(hyenaArrival3, file = "hyenaArrival3.csv", row.names=F, sep=",")
#write.table(jackalArrival3, file = "jackalArrival3.csv", row.names=F, sep=",")


####################################################################################
## Average Arrival Time of Species to Carcasses
####################################################################################
meanArrivalData<-with(mydata,tapply(mydata$got.here, mydata$breed, mean));meanArrivalData
sdArrivalData<-with(mydata,tapply(mydata$got.here, mydata$breed, sd));sdArrivalData
####################################################################################
## First Species to Arrive at Every Carcass
####################################################################################
## Note that sometimes multiple animals can arrive to a carcass at exactly the same time thereby 
## inflating the count of first arrival times above 1800, which is the number of carcasses per run (18) 
## multiplied by the number of runs (100). There can be fewer than 1800 results for mycarcass because some 
## simply go undiscovered 

firstArrivalData<-mydata[mydata$got.here == ave(mydata$got.here, mydata$mycarcass, FUN=min), ]
summary(firstArrivalData)
length(firstArrivalData$mycarcass)
length(unique(firstArrivalData$mycarcass))

lastArrivalData<-mydata[mydata$got.here == ave(mydata$got.here, mydata$mycarcass, FUN=max), ]
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
wbvObs<-229
lfvObs<-80
eaglesObs<-272
jackalsObs<-641
speciesPopHyObs <-c(wbvObs,lfvObs,eaglesObs,jackalsObs)
sumSpeciesPopHyObs<-sum(speciesPopHyObs)
speciesPopHyObs/sumSpeciesPopHyObs

## Expected Proportion
wbvEx<-116 
lfvEx<-8
eaglesEx<-80
jackalsEx<-65
speciesPopHyEx <-c(wbvEx,lfvEx,eaglesEx,jackalsEx)
sumSpeciesPopHyEx<-sum(speciesPopHyEx)
proportion<-speciesPopHyEx/sumSpeciesPopHyEx

# Compare Expected Preceding Species Proportion to Observed Preceding Species Proportion

## The proportions multiplied by the total number of observations (1222) would give you the expected counts.

expectedCounts<-sumSpeciesPopHyObs*proportion # 526.95911  36.34201 363.42007 295.27881
## run a chi-squared test of independence (observed first then expected)
freqs = c(229, 80, 272, 641,
          526.95911, 36.34201, 363.42007, 295.27881)
data = matrix(freqs, nrow=4)
dimnames(data) = list( 
  c("wbv", "lfv", "eagle", "jackal"),         
  c("observed", "expected"))
data

result <- chisq.test(data)
result

Input =(
  
  "Value     AWBV  LFV    Eagle   Jackal  
  Observed  229    80     272     641
  Expected  526.95911 36.34201 363.42007 295.27881  
  ")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE, 
                              row.names=1))

Matriz

barplot(Matriz, 
        beside=TRUE, 
        #legend=TRUE,  
        xlab="Species",
        ylab="number instances where a species preceded a hyena",
        main= "(A)"
)

legend("topleft", 
       legend = c("Observed", "Expected"), 
       fill = c("black", "grey"), bty = "n")
####################################################################################
## Compare Arrival Times of Model B without hyena local enhancement to Model C with enhancement
####################################################################################
rm(list=ls()) 
setwd("C:/Users/akane/Desktop/Science/Manuscripts/Corinne Scavengers/NetLogoOutput")
hydata<-read.csv("hyenaArrivals.csv", header=T, sep=",")
head(hydata)
tail(hydata)
summary(hydata)




t.test(hydata$time~hydata$info)


# hydata <- na.omit(hydata)
length(hydata$info)
mean(hydata$info, na.rm = T)
sd(hydata$info, na.rm = T)
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
boxplot(info$no,info$yes, pch = 16, data= hydata,names = c("local enhacement", "no enhancement"), 
        xlab="Information use", ylab= "square root of arrival time")
        
t.test(sqrtnone,sqrtenhance)
     
## compare followuing with none 
boxplot(sqrtfollowing, sqrtnone, pch = 16, names = c("following", "no enhancement"), 
        xlab="Information use", ylab= "square root of arrival time")

t.test(sqrtnone,sqrtfollowing)


