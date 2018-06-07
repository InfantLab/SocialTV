#load the necessary R packages
install.packages(c('readr','reshape2','ez','effects','ggplot2','ggsignif', 'Rmisc', 'svglite','sjmisc', 'repolr'))
library(tidyverse)
library(readxl) 
library(ggplot2)
library(ggsignif)
library(reshape2)
library(ez)
library(effects)
library(Rmisc)
library(svglite)
library(pwr)
library(sjPlot)
library(sjmisc)
library(repolr)

####################
# power calculations
####################
# some useful information found here
# http://r-video-tutorial.blogspot.co.uk/2017/07/power-analysis-and-sample-size.html
####################

eta2_p <- function(dfeffect,dferror,FVAL) {
  #If we have a reported effect
  # F(dfeffect,dferror) = FVAL
  # then partial eta square is calculated as
  
  eta = (FVAL * dfeffect) / (FVAL * dfeffect + dferror) 
  # see Lakens(2013) doi:10.3389/fpsyg.2013.00863
  return(eta)
}

#What was the effect size in Chapman (1973)?
#For laughter F( 2,74) = 27.86, p < .005]
EtaSQ = eta2_p(2,74,27.86) # returns 0.4295405

#For smiling [ F( 2,74 ) = 49.43
eta2_p(2,74,49.43) # returns 0.5719079

##To calculate the group size we convert etasq into f squared aka f2
Chapmanf2 = EtaSQ / (1-EtaSQ) 

##the pwr package lets us calculate the group size 
pwr.f2.test(u=2, f2=Chapmanf2, sig.level=0.05,power = .80)

#this gives v = 13.2 which implies a mimumum group size
# of 17 (so that degrees of freedom is greater than 13.2)
#alternatively we have chosen a group size of 20 then our predicted power is
pwr.f2.test(u=2, v=17, f2=Chapmanf2, sig.level=0.05)

#If we were to use a more conservative general large effect size
# suggested by Cohen (1988) as provided by the cohen.ES function
# then we would need a larger sample
cohenLarge = cohen.ES(test="f2",size="large")$effect.size
pwr.f2.test(u=2, f2=cohenLarge, sig.level=0.05,power = .80)


##############################
#load the raw data
##############################

setwd('C:\\Users\\caspar\\Dropbox\\projects\\Baby Laughter\\SocialTV')
alldata<- read_excel('SocialTV.xlsx', 'RatingData')

#specify which variables are factors
alldata$SEX <- factor(alldata$SEX)
alldata$ORDER <- factor(alldata$ORDER)


##############################
# descriptive stats & correlations
##############################

#Descriptive stats
mean(alldata$`Laughs Groups`)
sd(alldata$`Laughs Groups`)
mean(alldata$`Laughs Pairs`)
sd(alldata$`Laughs Pairs`)
mean(alldata$`Laughs Indiv`)
sd(alldata$`Laughs Indiv`)

# we use sjmisc to output descriptive stats table
descr(alldata[,c(3,6:11)],out=)

# use sjPlot to output a nicely formatted correlation table.
sjt.corr(alldata[,c(3,6:11)],p.numeric = TRUE, corr.method = "pearson")


##############################
#load the helper functions for plotting
source("socialTvplots.R")

#################################
#Analyse the laughter data

#To visualise the data we need to reorder it. 
longLaughs<-groupLongData(alldata, "Laughs Groups", "Laughs Pairs", "Laughs Indiv")
#plot the graph
plotGroupData(longLaughs,"Laughs")
#save it as a svg file. 
svglite("SocialTV laughs.svg", height=4, width = 5)
plotGroupData(longLaughs,"Laughs")
dev.off()

#initial analysis including sex as variable
ezANOVA(longLaughs,
        dv = value,
        wid = ID,
        between = .(ORDER, SEX),
        within = groupVar )

# 3x3 mixed ANOVA from paper 
ezANOVA(longLaughs,
        dv = value,
        wid = ID,
        between = .(ORDER),
        within = groupVar )

#same data plotted as oneway anova with error bars
plotOneWayANOVA(longLaughs,"Laughs")

#planned comparisons
t.test(alldata$`Laughs Groups`,alldata$`Laughs Pairs`, paired = TRUE)
t.test(alldata$`Laughs Groups`,alldata$`Laughs Indiv`, paired = TRUE)
t.test(alldata$`Laughs Pairs`, alldata$`Laughs Indiv`, paired = TRUE)


#Analyse the smiles data
#Descriptive stats
mean(alldata$`Smiles Groups`)
sd(alldata$`Smiles Groups`)
mean(alldata$`Smiles Pairs`)
sd(alldata$`Smiles Pairs`)
mean(alldata$`Smiles Indiv`)
sd(alldata$`Smiles Indiv`)

longSmiles<-groupLongData(alldata, "Smiles Groups", "Smiles Pairs", "Smiles Indiv")
#plot the graph
plotGroupData(longSmiles,"Smiles")
#save it as a svg file. 
svglite("SocialTV smiles.svg", height=4, width = 5)
plotGroupData(longSmiles,"Smiles")
dev.off()

ezANOVA(longSmiles,
        dv = value,
        wid = ID,
        between = .(ORDER,SEX),
        within = groupVar )

ezANOVA(longSmiles,
        dv = value,
        wid = ID,
        between = .(ORDER),
        within = groupVar )


#same data plotted as oneway anova with error bars
plotOneWayANOVA(longSmiles,"Smiles")
ezANOVA(longSmiles,
        dv = value,
        between = 
        wid = ID,
        within = groupVar )

#pair wise comparisons
t.test(alldata$`Smiles Pairs`, alldata$`Smiles Indiv`, paired = TRUE)
t.test(alldata$`Smiles Groups`,alldata$`Smiles Indiv`, paired = TRUE)
t.test(alldata$`Smiles Groups`,alldata$`Smiles Pairs`, paired = TRUE)


#Can also look at "mirth" = Laughter + smiles
longBoth<-groupLongData(alldata, "SmilesLaughs Groups", "SmilesLaughs Pairs", "SmilesLaughs Indiv")
showGroupData(longLaughs,"Laughs and Smiles")
plotGroupData(longBoth,"Laughs and Smiles")
plotOneWayANOVA(longBoth,"Laughs and Smiles")
ezANOVA(longBoth,
        dv = value,
        wid = ID,
        within = groupVar )


#But do they find it funny?
#Let's look at the ratings?

longRatings<-groupLongData(alldata,"Funniness Groups", "Funniness Pairs", "Funniness Indiv")
#let R know this is categorical data
longRatings$value<-factor(longRatings$value)
#lets collect responses into table (pick the appropriate columns)
tableRatings<-table(longRatings[,4:5])
#and a data frame for plotting
dfsummaryRatings<-as.data.frame(tableRatings)

ggplot(dfsummaryRatings,aes(groupVar,Freq,fill=value)) +
  geom_bar(stat="identity") + 
  ylab("Count") +
  xlab(NULL) +
  theme_bw(base_size =24)

#is it significant?
chisq.test(tableRatings)
mcnemar.test(tableRatings)

#if we are being strictly accurate then repolr package is best way to test ordinal rating data with repeated measures!
longRatings$valueNum<-as.numeric(longRatings$value)
longRatings$groupVarNum<-as.numeric(longRatings$groupVar)
longRatings$ORDERNum<-as.numeric(longRatings$ORDER)
funi<-repolr(valueNum~groupVarNum*ORDERNum,subjects = "ID", times=c(1,2,3),data=longRatings,categories =3)
summary.repolr(funi)

longRatings<-rename(longRatings,c("value"="Rating"))
#Finally
#are all videos equally funny?
videoRatings<-groupLongData(alldata,"Rating Video1", "Rating Video2", "Rating Video3")
#let R know this is categorical data
videoRatings$value<-factor(videoRatings$value)
#lets collect responses into table (pick the appropriate columns)
tableVideoRatings<-table(videoRatings[,4:5])
#and a data frame for plotting
dfsummaryRatings<-as.data.frame(tableVideoRatings)
ggplot(dfsummaryRatings,aes(groupVar,Freq,fill=value)) +
  geom_bar(stat="identity") + 
  ylab("Count") +
  xlab(NULL) +
  theme_bw(base_size =24)

#is it significant?
chisq.test(tableVideoRatings)

#another check look at amount of laughter by funniess rating
laughsvsfunny<-cbind(longRatings,longLaughs$value)
laughsvsfunny["groupVar"]=NULL
laughsvsfunny<-rename(laughsvsfunny,c("Rating"= "groupVar", "longLaughs$value"="value"))


ezANOVA(laughsvsfunny, dv=value,wid =ID, between=groupVar)

p<-plotOneWayANOVA(laughsvsfunny,varName="Laughs")
p+ylim(0,13)
#save it as a svg file. 
svglite("SocialTV laughsfunny.svg", height=4, width = 5)
p+ylim(0,13)
dev.off()


#another check look at amount of smiles by funniess rating
smilesfunny<-cbind(longRatings,longSmiles$value)
smilesfunny["groupVar"]=NULL
smilesfunny<-rename(smilesfunny,c("Rating"= "groupVar", "longSmiles$value"="value"))


ezANOVA(smilesfunny, dv=value,wid =ID, between=groupVar)

p<-plotOneWayANOVA(smilesfunny,varName="Smiles")
p+ylim(0,13)
#save it as a svg file. 
svglite("SocialTV smilesfunny.svg", height=4, width = 5)
p+ylim(0,13)
dev.off()
