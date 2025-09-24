library(dkstat)
library(dplyr)
library(tidyr)

# TEENPREGNANCY
# 1) ResearchGoal: Er antallet af ungre mødre faldet?
# 1.1) ResearchGoal: ud fra en graf over moderens alder for førstfødte over tid

# 2) Retrieve data
# 2.1) External source: DST
# a) get all tables
alltabs <- dst_get_tables(lang = "da")
colnames(alltabs)
head(alltabs)
# b) finder relevant tabel
lv=grepl("fød",alltabs$text,ignore.case = T)
ft=dst_search(string = "fød", field = "text")
foddf=alltabs[lv,]
# c) get metadata for tabellen
fodpmMeta=dst_meta("FODPM")

# lav liste med filter-variabler
# variabler
fodpmMeta$variables
fodpmMeta$values$LFNR

my_query <- list(
  MODERSALDER="*",
  LFNR="1. barn",
  Tid="*"
)

# hent tabel 
fodpmdf=dst_get_data(table = "fodpm", query = my_query)

# 3) Data Prep
# 3.0) Data Description
str(fodpmdf)
head(fodpmdf)
unique(fodpmdf$MODERSALDER)
# 3.1) Data Cleansing - ikke nødv fra DST
# 3.2) Data transformation 
# vi skal have modersalder lavet om til tal
fodpmdf$agecat=as.numeric(gsub(" år","",fodpmdf$MODERSALDER))
# lav nu agecat
agelabs=c("preteen","teen","very young", "young", "normal", "older", "old", "very old")
agelabs = c("Y10T14","Y15T19","Y20T24","Y25T29","Y30T34","Y35T39","Y40T44","Y45T49","YTGE50")
agebins=c(10,14,19,24,29,34,39,44,49,Inf)
fodpmdf$agebins=cut(fodpmdf$agecat,labels = agelabs, breaks = agebins)
barplot(table(fodpmdf$agebins))
par(mfrow=c(1,1))
#fodpmdf=fodpmdf[,-c(1,5)]

# aggregér values på kategori og tid
fodpmdfKat=aggregate(fodpmdf,value ~ agebins+TID, FUN=sum)
#fodpmdfKat=aggregate(fodpmdf,value ~ agebins, FUN=mean)

# subsetting mhp plot
subt=fodpmdfKat[ fodpmdfKat$agebins=="Y15T19" | fodpmdfKat$agebins=="Y30T34" , ]
#subt$value2=scale(subt$value)

##normalize
# base R and pivot
subtw=pivot_wider(subt,names_from = agebins,values_from = value)
subtw$nmY1519=myMinMax(subtw$Y15T19)
subtw$nmY3034=myMinMax(subtw$Y30T34)
subtl=subtw %>% pivot_longer(c("nmY1519","nmY3034"), 
                   names_to = "agebins", 
                   values_to = "value"
                   ) %>% select(agebins,value,TID)

myMinMax <- function(xv) {
}
# plot
ggplot(subt, aes(x=TID,y=value))+
  geom_line(aes(color=agebins))+
  facet_wrap(~agebins, scales = "free")

ggplot(subtl, aes(x=TID,y=value, colour = agebins))+
  geom_line()

# mange
# 4) Data exploration
# 4.1) Simple graphs
# 4.2) Complex graphs
# linjeplot med agekategori over tid. Først skal der aggregeres!
ggplot(fodpmdfKat, aes(x=TID,y=value, color=agebins))+geom_line()

# og indexeres
fodpmdfKat = fodpmdfKat %>% group_by(agebins) %>% mutate(validx=value / value[TID=='2007-01-01']) %>% 
  ungroup()
ggplot(fodpmdfKat, aes(y=validx))+geom_line(aes(x=TID,color=agebins))

