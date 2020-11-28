########################################### Assignment 3 ===========
library(readxl)
library(tidyverse)
library(factoextra)

bsData <- read_excel("C:/Users/Olga/Downloads/Assgt3_BathSoap_Data.xls", sheet = "DM_Sheet")

#remove rows, replace na's with med, ignore them
bsData <- bsData[1:600, 1:46] #this still some blanks in
#rename column 10
colnames(bsData)[colnames(bsData)=="...10"] <- "CS"
colnames(bsData)[colnames(bsData)=="Brand Runs"] <- "brand_runs"
bsData$`Member id` <- NULL

##Better to change the colNames which contain punctuation, space
names(bsData) <- gsub("[[:punct:]]|\\s", "_", names(bsData)) 

##The data with '%' in values are read in as 'chr' type - change these to numeric
bsData[20:45]<-lapply(bsData[20:45], function(x) as.numeric(sub("%", "e-2", x)))
bsd<- bsData

##for brLoyalty, suppose we calculate maxBr as max of purchase by different major brand (excl others)
bsd<-bsd %>% rowwise() %>% mutate(maxBr=max(Br__Cd__57__144, Br__Cd__55, Br__Cd__272,
                                            Br__Cd__286, Br__Cd__24, Br__Cd__481, Br__Cd__352, Br__Cd__5))


##convert this to dummies, since the values are not ordinal, and remove the '0' level dummy
bsd<-bsd %>% mutate(fehDummy=1) %>% pivot_wider(names_from = FEH, values_from = fehDummy, names_prefix= "FEH_",values_fill = list(fehDummy=0))
##can append this to the last line of code if you want


##explore MT (language spoken - mother tongue)
summary(as.factor(bsd$MT))
##Many of the category levels have very few values..so
##keep levels 0, 4, 5, 10, 25 as dummies, with 0 in the dummies indicating 'other'
bsd<- bsd %>% mutate(MT=if_else(MT %in% c(0, 4, 5, 10, 25), MT, -1))
bsd<-bsd %>% mutate(mtDummy=1) %>% pivot_wider(names_from = MT, values_from = mtDummy, names_prefix = "MT_",
                                               values_fill = list(mtDummy=0))
bsd<- bsd %>% select(- `MT_-1`)

########################################### Question 2 ===========

########################################### Converting some variables to a factors!===========

bsData$EDU <- as.factor(bsData$EDU)
levels(bsData$EDU)  
bsData$EDU <- factor(bsData$EDU, labels=c("illiterate", "literate-no formal school", "up to 4yrs", "5-9 years","10-12 years","some college","some graduate","graduate or prof. degree", "Na"))

bsData$SEC <- as.factor(bsData$SEC)
levels(bsData$SEC)

bsData$MT <- as.factor(bsData$MT)
(levels(bsData$MT))
bsData$MT <- factor(bsData$MT, labels = c("English", "Gujarati", "Hindi", "Kannada", "Konkani",
                                          "Malayalam", "Marathi", "Punjabi", "Rajasthani", "Sindhi", 
                                          "Tamil", "Telugu", "Urdu", "Other"))


bsData$SEX <- factor(bsData$SEX, labels = c("Male", "Female"))
bsData$SEX


bsData$FEH <- factor(bsData$FEH, labels = c("Pure vegetarian", "Vegetarian but eggs ok", "Non-vegetarian"))
head(bsData$FEH)

#age
bsData$AGE <- as.factor(bsData$AGE)
bsData$AGE <- factor(bsData$AGE, labels=c("Up to 24", "25-34", "35-44", "45+"))
levels(bsData$AGE)

#child presence
bsData$CHILD <- as.factor(bsData$CHILD)
levels((bsData$CHILD))
bsData$CHILD <- factor(bsData$CHILD, labels=c("Children up to age 6 present (only)", "Children 7-14 present (only)",
                                              "Both", "None", "Not specified"))


#TV
bsData$CS <- factor(bsData$CS, labels=c("Cable or broadcast TV available", "Unavailable"))
bsData$CS

table(bsData$AGE, bsData$FEH)


########################################### The First Database===========
#Doing the all the dataset with zeros 
bsd1<- bsd

bsd1[is.na(bsd)] <- 0

summary(bsd1)

bsd1PP <- bsd1

bsd1PP[,10:45] = scale(bsd1PP[,10:45]) #scale the NON FACTORS- so these ones-make sure you have 45 col

summary(bsd1PP)


########################################### The Second Database===========
#Doing the median value with NA 
df<-bsd

view(df)

df<-bsd #this will be for the median replace for blanks df
#replace blanks with the median
df<- df %>% replace_na(list(SEX=median(df$SEX, na.rm=TRUE)))
df<- df %>% replace_na(list(EDU=median(df$EDU, na.rm=TRUE)))
df<- df %>% replace_na(list(HS=median(df$HS, na.rm=TRUE)))
df<- df %>% replace_na(list(CS=median(df$CS, na.rm=TRUE)))

colMeans(is.na(df))[colMeans(is.na(df))>0]
###summary of data in these columns
nm<- names(df)[colSums(is.na(df))>0]
str(df[, nm])

dfPP <- df

dfPP[,10:45] = scale(dfPP[,10:45]) #scale the NON FACTORS- so these ones-make sure you have 45 col

summary(dfPP)

########################################### Summary===========

#SEC 
ggplot(bsData, mapping = aes(SEC))+ geom_histogram(color="blue") + geom_bar(width = .5, 
                                                                            fill="blue")+labs(title = "  Socioeconomic Status of Household from high (1) to low (4)")


#food eating habits
ggplot(bsData, mapping = aes(FEH)) + geom_bar(width = .8, fill="blue")+
  # labs(title = " Food Eating Habits of Household")   
  
  ggplot(bsData, mapping = aes(MT))+ geom_bar(width = .9, fill="blue")+
  labs(title = "    Native Language") +
  theme(axis.text.x = element_text(angle = 45))


#sex hist)

ggplot(bsData, mapping = aes(SEX)) + geom_bar(width = .9, fill="blue")+
  labs(title = " Sex of Head of Household")


#age hist

ggplot(bsData, mapping = aes(AGE)) + geom_bar(width = .9, fill="blue")+
  labs(title = " Age of Head of Household")

#Household size

ggplot(bsData, mapping = aes(HS)) + geom_bar(width = .9, fill="blue")+
  labs(title = "   Member Household Size")

#educ hist

ggplot(bsData, mapping = aes(EDU)) + geom_bar(width = .9, fill="blue")+
  labs(title = "   Member Education Attainment Level") +
  theme(axis.text.x = element_text(angle = 45))

#child presence hist
ggplot(bsData, mapping = aes(CHILD)) + geom_bar(width = .7, fill="blue")+
  labs(title = "                          Child Presence") +
  theme(axis.text.x = element_text(angle = 45))


#TV yes or no

ggplot(bsData, mapping = aes(CS)) + geom_bar(width = .9, fill="blue")+
  labs(title = "    Television Availability")

##Affluence Index 
ggplot(bsd, aes( x = Affluence_Index)) + geom_histogram(col="blue", fill="blue") + labs(title="Distribution Household's Economic and Financial Advantage")

ggplot(bsd, aes(x = Affluence_Index, y=Total_Volume)) + geom_histogram(col='blue', stat='identity', fill="blue") + labs(title="Affluence Index vs. Total Volume")

##No__of_Brands
ggplot(bsd, aes( x = No__of_Brands)) + geom_histogram(col='blue', fill="blue") + scale_x_discrete(name= "No__of_Brands", limits=c("1","2","3","4","5","6","7","8","9")) + labs(title="Distribution of No of Brands")

ggplot(bsd, aes(x = AGE, y=No__of_Brands)) + geom_histogram(col='blue', stat='identity', fill="blue") + labs(title="                          Age vs. No of Brands")

ggplot(bsd, aes(x = EDU, y=No__of_Brands)) + geom_histogram(col='blue', stat='identity',fill="blue") + scale_x_discrete(name= "EDU", limits=c("1","2","3","4","5","6","7","8","9")) + labs(title="                   Education vs. No of Brands") 

ggplot(bsd, aes(x = SEC, y=No__of_Brands)) + geom_histogram(col='blue', stat='identity',fill="blue") + labs(title="Socioeconomic Status Household vs. No of Brands", caption="Socioeconomic Status Household from high(1) to low(4)") 

ggplot(bsData, aes(x = FEH, y=No__of_Brands)) + geom_histogram(col='blue', stat='identity',fill="blue") + labs(title="Food Eating Habits of Household vs No of Brands ")

##Brand Runs
ggplot(bsd, aes(x = No__of_Brands, y=brand_runs)) + geom_histogram(col='blue', stat='identity', fill="blue") + scale_x_discrete(name= "No__of_Brands", limits=c("1","2","3","4","5","6","7","8","9")) + labs(title="No of Brands vs. Brands Runs")

##No. of Transaction 
ggplot(bsd, aes(x = EDU, y=No__of__Trans)) + geom_histogram(col='blue', stat='identity', fill="blue") + scale_x_discrete(name= "EDU", limits=c("1","2","3","4","5","6","7","8","9")) + labs(title="Education vs. Number of Purchase Transactions")

ggplot(bsd, aes(x = SEC, y=No__of__Trans)) + geom_histogram(col='blue', stat='identity',fill="blue") + labs(title="Socioeconomic Status Household vs. # of Purchase Transactions", caption="Socioeconomic Status Household from high(1) to low(4)") 

##Value
ggplot(bsd, aes(x = No__of_Brands, y=Value)) + geom_histogram(col='blue', stat='identity', fill="blue") + scale_x_discrete(name= "No__of_Brands", limits=c("1","2","3","4","5","6","7","8","9")) + labs(title="No of Brands vs. Value")

##Trans/Brands Run 
ggplot(bsd, aes(x = No__of_Brands, y=Trans___Brand_Runs)) + geom_point(col="blue") + scale_x_discrete(name= "No__of_Brands", limits=c("1","2","3","4","5","6","7","8","9")) + labs(title="No of Brands vs.Avg Trans per Brand Run")

##Vol/Tran 
ggplot(bsd, aes(x = No__of_Brands, y=Vol_Tran)) + geom_point(col="blue") + scale_x_discrete(name= "No__of_Brands", limits=c("1","2","3","4","5","6","7","8","9")) + labs(title="Numbers of Brands Purchased vs. Avg. Volume per Transactions")

##Total Volume
ggplot(bsd, aes(x = EDU, y=Total_Volume)) + geom_histogram(col='blue', stat='identity', fill="blue") + scale_x_discrete(name= "EDU", limits=c("1","2","3","4","5","6","7","8","9")) + labs(title="Education vs. Total Volume Brought")

ggplot(bsd, aes(x = SEC, y=Total_Volume)) + geom_histogram(col='blue', stat='identity',fill="blue") + labs(title="Socioeconomic Status Household vs. Total Volume Brought", caption="Socioeconomic Status Household from high(1) to low(4)")

ggplot(bsData, aes(x = FEH, y=Total_Volume)) + geom_histogram(col='blue', stat='identity',fill="blue") + labs(title="Food Eating Habits of Household vs Total Volume Brought ")

##Purchase within Promotion
#ggplot(bsd, aes(x = Pur_Vol_No_Promo-%, y= Purchase_within_Promotion  )) + geom_histogram(col='blue', stat='identity')

ggplot(bsd, aes(x = EDU, y=Pr_Cat_1)) + geom_histogram(col='blue', stat='identity')

ggplot(bsd, aes(x = AGE, y=Pr_Cat_2)) + geom_histogram(col='blue', stat='identity')

ggplot(bsd, aes(x = EDU, y=Pr_Cat_3)) + geom_histogram(col='blue', stat='identity')

ggplot(bsd, aes(x = AGE, y= Pr_Cat_4 )) + geom_histogram(col='blue', stat='identity')

ggplot(bsd, aes(x = EDU, y= PropCat_5)) + geom_histogram(col='blue', stat='identity')

ggplot(bsd, aes(x = AGE, y= PropCat_6)) + geom_histogram(col='blue', stat='identity')

###Brand wise purchase
ggplot(bsData, mapping = aes(SEX)) + geom_bar(width = .9, fill="blue")+labs(title="Freshness Product Bought by Each Gender")

ggplot(bsData, mapping = aes(SEX)) + geom_bar(width = .9, fill="blue")+labs(title="Hair Products Bought by Each Gender")

ggplot(bsData, mapping = aes(AGE)) + geom_bar(width = .9, fill="blue")+labs(title = "Baby Products Bought By Each Age")

ggplot(bsd, aes(x = SEC, y=PropCat_6)) + geom_histogram(col='blue', stat='identity',fill="blue") + labs(title="Health Products Purchased Based on Socio Economic Class")

ggplot(bsd, aes(x = HS, y=PropCat_7)) + geom_histogram(col='blue', stat='identity',fill="blue") + labs(title="Number of Households who Purchase Herbal")

ggplot(bsd, aes(x = No__of_Brands, y=PropCat_14)) + geom_histogram(col='blue', stat='identity',fill="blue") + labs(title="Number of Brands Purchased for Carbolic")

ggplot(bsd, aes(x = Avg__Price, y=PropCat_14)) + geom_histogram(col='blue', stat='identity',fill="blue") + labs(title="Average Price for Carbolic")

ggplot(bsd, aes(x = Avg__Price, y= No__of_Brands)) + geom_histogram(col='blue', stat='identity',fill="blue") + labs(title="Average Price for Fresh Products")

ggplot(bsd, aes(x =No__of_Brands, y=Avg__Price)) + geom_histogram(col='blue', stat='identity',fill="blue") + labs(title="Price paid for a brand")

############################# Question 3 ####################################################
########################################### First Model Purchase Behavior Dataset 1===========
#k mean 

#for clustering on purchase behavior variables
PURCHASE_BEHAVIOR <- c('No__of_Brands', 'brand_runs', 'Total_Volume', 'No__of__Trans', 'Value', 'Trans___Brand_Runs', 'Vol_Tran',
                       'Avg__Price', 'maxBr', 'Others_999')

#First Model No PP
x<- bsd1
kmClus_pb<- x %>% select(PURCHASE_BEHAVIOR) %>% scale() %>% kmeans(centers=3, nstart=25)
kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=x %>% select(PURCHASE_BEHAVIOR))

#Or you can plot by specific variables
fviz_cluster(kmClus_pb, data=x %>% select(maxBr, Total_Volume))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
x <- x %>% mutate(clusKM=kmClus_pb$cluster)

x %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD',
                                          'maxBr', 'No__of_Brands', 'No__of__Trans', 'brand_runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean, ) %>% view()

kmClus_pb

fviz_nbclust(x, kmeans, method = "wss")

fviz_nbclust(x, kmeans, method = "silhouette")

#Second Model Different values for k 
y<- bsd1
kmClus_pb<- y %>% select(PURCHASE_BEHAVIOR) %>% scale() %>% kmeans(centers=5, nstart=25)
kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=y %>% select(PURCHASE_BEHAVIOR))

#Or you can plot by specific variables
fviz_cluster(kmClus_pb, data=y %>% select(maxBr, Total_Volume))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
y <- y %>% mutate(clusKM=kmClus_pb$cluster)

y %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD',
                                          'maxBr', 'No__of_Brands', 'No__of__Trans', 'brand_runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean, ) %>% view()

kmClus_pb

fviz_nbclust(y, kmeans, method = "wss")

fviz_nbclust(y, kmeans, method = "silhouette")

#Third Model Different values for K 
a <- bsd1
kmClus_pb<- a %>% select(PURCHASE_BEHAVIOR) %>% scale() %>% kmeans(centers=2, nstart=25)
kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=a %>% select(PURCHASE_BEHAVIOR))

#Or you can plot by specific variables
fviz_cluster(kmClus_pb, data=a %>% select(maxBr, Total_Volume))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
a <- a %>% mutate(clusKM=kmClus_pb$cluster)

a %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD',
                                          'maxBr', 'No__of_Brands', 'No__of__Trans', 'brand_runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean, ) %>% view()

kmClus_pb

fviz_nbclust(a, kmeans, method = "wss")

fviz_nbclust(a, kmeans, method = "silhouette")

#Fourth Model PP Data 
b<- bsd1PP
kmClus_pb<- b %>% select(PURCHASE_BEHAVIOR) %>% scale() %>% kmeans(centers=3, nstart=25)
kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=b %>% select(PURCHASE_BEHAVIOR))

#Or you can plot by specific variables
fviz_cluster(kmClus_pb, data=b %>% select(maxBr, Total_Volume))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
b <- b %>% mutate(clusKM=kmClus_pb$cluster)

b %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD',
                                          'maxBr', 'No__of_Brands', 'No__of__Trans', 'brand_runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean, ) %>% view()

kmClus_pb

fviz_nbclust(b, kmeans, method = "wss")

fviz_nbclust(b, kmeans, method = "silhouette")

#Fifth Model Different values for k 
c<- bsd1PP
kmClus_pb<- c %>% select(PURCHASE_BEHAVIOR) %>% scale() %>% kmeans(centers=2, nstart=25)
kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=c %>% select(PURCHASE_BEHAVIOR))

#Or you can plot by specific variables
fviz_cluster(kmClus_pb, data=c %>% select(maxBr, Total_Volume))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
c <- c %>% mutate(clusKM=kmClus_pb$cluster)

c %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD',
                                          'maxBr', 'No__of_Brands', 'No__of__Trans', 'brand_runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean, ) %>% view()

kmClus_pb

fviz_nbclust(c, kmeans, method = "wss")

fviz_nbclust(c, kmeans, method = "silhouette")
########################################### First Model Purchase Behavior Dataset 2===========
#Six Model No PP
d<- df
kmClus_pb<- d %>% select(PURCHASE_BEHAVIOR) %>% scale() %>% kmeans(centers=3, nstart=25)
kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=d %>% select(PURCHASE_BEHAVIOR))

#Or you can plot by specific variables
fviz_cluster(kmClus_pb, data=d %>% select(maxBr, Total_Volume))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
d <- d %>% mutate(clusKM=kmClus_pb$cluster)

d %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD',
                                          'maxBr', 'No__of_Brands', 'No__of__Trans', 'brand_runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean, ) %>% view()

kmClus_pb

fviz_nbclust(d, kmeans, method = "wss")

fviz_nbclust(d, kmeans, method = "silhouette")

d1<- df
kmClus_pb<- d1 %>% select(PURCHASE_BEHAVIOR) %>% scale() %>% kmeans(centers=5, nstart=25)
kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=d1 %>% select(PURCHASE_BEHAVIOR))

#Or you can plot by specific variables
fviz_cluster(kmClus_pb, data=d1 %>% select(maxBr, Total_Volume))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
d1 <- d1 %>% mutate(clusKM=kmClus_pb$cluster)

d1 %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD',
                                           'maxBr', 'No__of_Brands', 'No__of__Trans', 'brand_runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean, ) %>% view()

kmClus_pb

fviz_nbclust(d1, kmeans, method = "wss")

fviz_nbclust(d1, kmeans, method = "silhouette")

d2<- df
kmClus_pb<- d2 %>% select(PURCHASE_BEHAVIOR) %>% scale() %>% kmeans(centers=2, nstart=25)
kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=d2 %>% select(PURCHASE_BEHAVIOR))

#Or you can plot by specific variables
fviz_cluster(kmClus_pb, data=d2 %>% select(maxBr, Total_Volume))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
d2 <- d2 %>% mutate(clusKM=kmClus_pb$cluster)

d2 %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD',
                                           'maxBr', 'No__of_Brands', 'No__of__Trans', 'brand_runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean, ) %>% view()

kmClus_pb

fviz_nbclust(d2, kmeans, method = "wss")

fviz_nbclust(d2, kmeans, method = "silhouette")

d3<- dfPP
kmClus_pb<- d3 %>% select(PURCHASE_BEHAVIOR) %>% scale() %>% kmeans(centers=2, nstart=25)
kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=d3 %>% select(PURCHASE_BEHAVIOR))

#Or you can plot by specific variables
fviz_cluster(kmClus_pb, data=d3 %>% select(maxBr, Total_Volume))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
d3 <- d3 %>% mutate(clusKM=kmClus_pb$cluster)

d3 %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD',
                                           'maxBr', 'No__of_Brands', 'No__of__Trans', 'brand_runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean, ) %>% view()

kmClus_pb

fviz_nbclust(d3, kmeans, method = "wss")

fviz_nbclust(d3, kmeans, method = "silhouette")

d4<- dfPP
kmClus_pb<- d4 %>% select(PURCHASE_BEHAVIOR) %>% scale() %>% kmeans(centers=3, nstart=25)
kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=d4 %>% select(PURCHASE_BEHAVIOR))

#Or you can plot by specific variables
fviz_cluster(kmClus_pb, data=d4 %>% select(maxBr, Total_Volume))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
d4 <- d4 %>% mutate(clusKM=kmClus_pb$cluster)

d4 %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD',
                                           'maxBr', 'No__of_Brands', 'No__of__Trans', 'brand_runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean, ) %>% view()

kmClus_pb

fviz_nbclust(d4, kmeans, method = "wss")

fviz_nbclust(d4, kmeans, method = "silhouette")

########################################### First Model Basis for Purchase Dataset 1 ===========
BASIS_FOR_PURCHASE <- c('Pur_Vol_No_Promo____','Pur_Vol_Promo_6__',
                        'Pur_Vol_Other_Promo__','Br__Cd__57__144',
                        'Br__Cd__55','Br__Cd__272','Br__Cd__286',
                        'Br__Cd__286','Br__Cd__24','Br__Cd__481',
                        'Br__Cd__352','Br__Cd__5','Others_999',
                        'Pr_Cat_1','Pr_Cat_2','Pr_Cat_3','Pr_Cat_4',
                        'PropCat_5','PropCat_6','PropCat_7','PropCat_8',
                        'PropCat_9','PropCat_10','PropCat_11','PropCat_12',
                        'PropCat_13','PropCat_14','PropCat_15')
#First Model No PP
r<- bsd1
kmClus_pb<- r %>% select(BASIS_FOR_PURCHASE) %>% scale() %>% kmeans(centers=3, nstart=25)
kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=r %>% select(BASIS_FOR_PURCHASE))

#Or you can plot by specific variables
fviz_cluster(kmClus_pb, data=r %>% select(maxBr, Total_Volume))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
r <- r %>% mutate(clusKM=kmClus_pb$cluster)

r %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD',
                                          'maxBr', 'No__of_Brands', 'No__of__Trans', 'brand_runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean, ) %>% view()

kmClus_pb

fviz_nbclust(r, kmeans, method = "wss")

fviz_nbclust(r, kmeans, method = "silhouette")

#Second Model with a differnt value for k 
r1<- bsd1
kmClus_pb<- r1 %>% select(BASIS_FOR_PURCHASE) %>% scale() %>% kmeans(centers=2, nstart=25)
kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=r1 %>% select(BASIS_FOR_PURCHASE))

#Or you can plot by specific variables
fviz_cluster(kmClus_pb, data=r1 %>% select(maxBr, Total_Volume))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
r1 <- r1 %>% mutate(clusKM=kmClus_pb$cluster)

r1 %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD',
                                           'maxBr', 'No__of_Brands', 'No__of__Trans', 'brand_runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean, ) %>% view()

kmClus_pb

fviz_nbclust(r1, kmeans, method = "wss")

fviz_nbclust(r1, kmeans, method = "silhouette")

#Third Model with a different value for k 
r2<- bsd1
kmClus_pb<- r2 %>% select(BASIS_FOR_PURCHASE) %>% scale() %>% kmeans(centers=5, nstart=25)
kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=r2 %>% select(BASIS_FOR_PURCHASE))

#Or you can plot by specific variables
fviz_cluster(kmClus_pb, data=r2 %>% select(maxBr, Total_Volume))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
r2 <- r2 %>% mutate(clusKM=kmClus_pb$cluster)

r2 %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD',
                                           'maxBr', 'No__of_Brands', 'No__of__Trans', 'brand_runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean, ) %>% view()

kmClus_pb

fviz_nbclust(r2, kmeans, method = "wss")

fviz_nbclust(r2, kmeans, method = "silhouette")

#Fourth Model with a different value for k 
r3<- bsd1
kmClus_pb<- r3 %>% select(BASIS_FOR_PURCHASE) %>% scale() %>% kmeans(centers=4, nstart=25)
kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=r3 %>% select(BASIS_FOR_PURCHASE))

#Or you can plot by specific variables
fviz_cluster(kmClus_pb, data=r3 %>% select(maxBr, Total_Volume))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
r3 <- r3 %>% mutate(clusKM=kmClus_pb$cluster)

r3 %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD',
                                           'maxBr', 'No__of_Brands', 'No__of__Trans', 'brand_runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean, ) %>% view()

kmClus_pb

fviz_nbclust(r3, kmeans, method = "wss")

fviz_nbclust(r3, kmeans, method = "silhouette")

#Fourth Model PP Data 
s<- bsd1PP
kmClus_pb<- s %>% select(BASIS_FOR_PURCHASE) %>% scale() %>% kmeans(centers=3, nstart=25)
kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=s %>% select(BASIS_FOR_PURCHASE))

#Or you can plot by specific variables
fviz_cluster(kmClus_pb, data=s %>% select(maxBr, Total_Volume))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
s <- s %>% mutate(clusKM=kmClus_pb$cluster)

s %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD',
                                          'maxBr', 'No__of_Brands', 'No__of__Trans', 'brand_runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean, ) %>% view()

kmClus_pb

fviz_nbclust(s, kmeans, method = "wss")

fviz_nbclust(s, kmeans, method = "silhouette")

#Fifth Model PP Data Different k values
s1<- bsd1PP
kmClus_pb<- s1 %>% select(BASIS_FOR_PURCHASE) %>% scale() %>% kmeans(centers=2, nstart=25)
kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=s1 %>% select(BASIS_FOR_PURCHASE))

#Or you can plot by specific variables
fviz_cluster(kmClus_pb, data=s1 %>% select(maxBr, Total_Volume))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
s1 <- s1 %>% mutate(clusKM=kmClus_pb$cluster)

s1 %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD',
                                           'maxBr', 'No__of_Brands', 'No__of__Trans', 'brand_runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean, ) %>% view()

kmClus_pb

fviz_nbclust(s1, kmeans, method = "wss")

fviz_nbclust(s1, kmeans, method = "silhouette")

#Sixth Model PP Data Different k values
s2<- bsd1PP
kmClus_pb<- s2 %>% select(BASIS_FOR_PURCHASE) %>% scale() %>% kmeans(centers=4, nstart=25)
kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=s2 %>% select(BASIS_FOR_PURCHASE))

#Or you can plot by specific variables
fviz_cluster(kmClus_pb, data=s2 %>% select(maxBr, Total_Volume))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
s2 <- s2 %>% mutate(clusKM=kmClus_pb$cluster)

s2 %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD',
                                           'maxBr', 'No__of_Brands', 'No__of__Trans', 'brand_runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean, ) %>% view()

kmClus_pb

fviz_nbclust(s2, kmeans, method = "wss")

fviz_nbclust(s2, kmeans, method = "silhouette")

########################################### First Model Basis for Purchase Dataset 2 ===========
# First No PP
z<- df
kmClus_pb<- z %>% select(BASIS_FOR_PURCHASE) %>% scale() %>% kmeans(centers=2, nstart=25)
kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=z %>% select(BASIS_FOR_PURCHASE))

#Or you can plot by specific variables
fviz_cluster(kmClus_pb, data=z %>% select(maxBr, Total_Volume))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
z <- z %>% mutate(clusKM=kmClus_pb$cluster)

z %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD',
                                          'maxBr', 'No__of_Brands', 'No__of__Trans', 'brand_runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean, ) %>% view()

kmClus_pb

fviz_nbclust(z, kmeans, method = "wss")

fviz_nbclust(z, kmeans, method = "silhouette")

# Second Model different value for k 
z1<- df
kmClus_pb<- z1 %>% select(BASIS_FOR_PURCHASE) %>% scale() %>% kmeans(centers=3, nstart=25)
kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=z1 %>% select(BASIS_FOR_PURCHASE))

#Or you can plot by specific variables
fviz_cluster(kmClus_pb, data=z1 %>% select(maxBr, Total_Volume))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
z1 <- z1 %>% mutate(clusKM=kmClus_pb$cluster)

z1 %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD',
                                           'maxBr', 'No__of_Brands', 'No__of__Trans', 'brand_runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean, ) %>% view()

kmClus_pb

fviz_nbclust(z1, kmeans, method = "wss")

fviz_nbclust(z1, kmeans, method = "silhouette")

# Third Model different value for k 
z2<- df
kmClus_pb<- z2 %>% select(BASIS_FOR_PURCHASE) %>% scale() %>% kmeans(centers=4, nstart=25)
kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=z2 %>% select(BASIS_FOR_PURCHASE))

#Or you can plot by specific variables
fviz_cluster(kmClus_pb, data=z2 %>% select(maxBr, Total_Volume))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
z2 <- z2 %>% mutate(clusKM=kmClus_pb$cluster)

z2 %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD',
                                           'maxBr', 'No__of_Brands', 'No__of__Trans', 'brand_runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean, ) %>% view()

kmClus_pb

fviz_nbclust(z2, kmeans, method = "wss")

fviz_nbclust(z2, kmeans, method = "silhouette")

#Fourth Model PP
z3<- dfPP
kmClus_pb<- z3 %>% select(BASIS_FOR_PURCHASE) %>% scale() %>% kmeans(centers=2, nstart=25)
kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=z3 %>% select(BASIS_FOR_PURCHASE))

#Or you can plot by specific variables
fviz_cluster(kmClus_pb, data=z3 %>% select(maxBr, Total_Volume))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
z3 <- z3 %>% mutate(clusKM=kmClus_pb$cluster)

z3 %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD',
                                           'maxBr', 'No__of_Brands', 'No__of__Trans', 'brand_runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean, ) %>% view()

kmClus_pb

fviz_nbclust(z3, kmeans, method = "wss")

fviz_nbclust(z3, kmeans, method = "silhouette")

#Sixth Model PP
z4<- dfPP
kmClus_pb<- z4 %>% select(BASIS_FOR_PURCHASE) %>% scale() %>% kmeans(centers=3, nstart=25)
kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=z4 %>% select(BASIS_FOR_PURCHASE))

#Or you can plot by specific variables
fviz_cluster(kmClus_pb, data=z4 %>% select(maxBr, Total_Volume))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
z4 <- z4 %>% mutate(clusKM=kmClus_pb$cluster)

z4 %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD',
                                           'maxBr', 'No__of_Brands', 'No__of__Trans', 'brand_runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean, ) %>% view()

kmClus_pb

fviz_nbclust(z4, kmeans, method = "wss")

fviz_nbclust(z4, kmeans, method = "silhouette")

#Seventh Model PP
z5<- dfPP
kmClus_pb<- z5 %>% select(BASIS_FOR_PURCHASE) %>% scale() %>% kmeans(centers=4, nstart=25)
kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=z5 %>% select(BASIS_FOR_PURCHASE))

#Or you can plot by specific variables
fviz_cluster(kmClus_pb, data=z5 %>% select(maxBr, Total_Volume))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
z5 <- z5 %>% mutate(clusKM=kmClus_pb$cluster)

z5 %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD',
                                           'maxBr', 'No__of_Brands', 'No__of__Trans', 'brand_runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean, ) %>% view()

kmClus_pb

fviz_nbclust(z5, kmeans, method = "wss")

fviz_nbclust(z5, kmeans, method = "silhouette")

#############################  Question 4####################################################
########################################### Second Model ===========
#DBscan Models

library(dbscan)
msDbscan <- dbscan(multishapes[,1:2], eps = 0.5, minPts = 5)
#testing which eps and minPts is the best
#fviz_cluster(msDbscan, data=multishapes[,1:2], geom="point", ellipse = FALSE, main="dbscan eps=0.5, minPts=5")
dbClus_pb <- x %>%  select(PURCHASE_BEHAVIOR) %>% scale() %>% dbscan(eps=0.5, minPts = 5)
dbClus_pb

dbClus_pb <- x %>%  select(PURCHASE_BEHAVIOR) %>% scale() %>% dbscan(eps=5, minPts = 5)
dbClus_pb

dbClus_pb <- x %>%  select(PURCHASE_BEHAVIOR) %>% scale() %>% dbscan(eps=2, minPts = 5)
dbClus_pb

dbClus_pb <- x %>%  select(PURCHASE_BEHAVIOR) %>% scale() %>% dbscan(eps=1.5, minPts = 5)
dbClus_pb

dbClus_pb <- x %>%  select(PURCHASE_BEHAVIOR) %>% scale() %>% dbscan(eps=1, minPts = 5)
dbClus_pb

dbClus_pb <- x %>%  select(PURCHASE_BEHAVIOR) %>% scale() %>% dbscan(eps=0.8, minPts = 5)
dbClus_pb

dbClus_pb <- x %>%  select(PURCHASE_BEHAVIOR) %>% scale() %>% dbscan(eps=0.8, minPts = 3)
dbClus_pb

dbClus_pb <- x %>%  select(PURCHASE_BEHAVIOR) %>% scale() %>% dbscan(eps=0.8, minPts = 3)
dbClus_pb

dbClus_pb <- x %>%  select(PURCHASE_BEHAVIOR) %>% scale() %>% dbscan(eps=0.6, minPts = 3)
dbClus_pb

dbClus_pb <- x %>%  select(PURCHASE_BEHAVIOR) %>% scale() %>% dbscan(eps=0.55, minPts = 3)
dbClus_pb

dbClus_pb <- x %>%  select(PURCHASE_BEHAVIOR) %>% scale() %>% dbscan(eps=0.5, minPts = 5)
dbClus_pb

dbClus_pb <- x %>%  select(PURCHASE_BEHAVIOR) %>% scale() %>% dbscan(eps=0.5, minPts = 3)
dbClus_pb

dbClus_pb <- x %>%  select(PURCHASE_BEHAVIOR) %>% scale() %>% dbscan(eps=1, minPts = 3)
dbClus_pb       

dbClus_pb <- x %>%  select(PURCHASE_BEHAVIOR) %>% scale() %>% dbscan(eps=3, minPts = 3)
dbClus_pb

dbClus_pb <- x %>%  select(PURCHASE_BEHAVIOR) %>% scale() %>% dbscan(eps=2, minPts = 3)
dbClus_pb

dbClus_pb <- x %>%  select(PURCHASE_BEHAVIOR) %>% scale() %>% dbscan(eps=2, minPts = 6)
dbClus_pb

dbClus_pb <- x %>%  select(PURCHASE_BEHAVIOR) %>% scale() %>% dbscan(eps=2, minPts = 15)
dbClus_pb

dbClus_pb <- x %>%  select(PURCHASE_BEHAVIOR) %>% scale() %>% dbscan(eps=1.05, minPts = 5)
dbClus_pb

dbClus_pb <- x %>%  select(PURCHASE_BEHAVIOR) %>% scale() %>% dbscan(eps=1.02, minPts = 4)
dbClus_pb

dbClus_pb <- x %>%  select(PURCHASE_BEHAVIOR) %>% scale() %>% dbscan(eps=1.02, minPts = 3)
dbClus_pb

dbClus_pb <- x %>%  select(PURCHASE_BEHAVIOR) %>% scale() %>% dbscan(eps=1.03, minPts = 2)
dbClus_pb

dbClus_pb <- x %>%  select(PURCHASE_BEHAVIOR) %>% scale() %>% dbscan(eps=0.9, minPts = 2)
dbClus_pb

dbClus_pb <- x %>%  select(BASIS_FOR_PURCHASE) %>% scale() %>% dbscan(eps=0.5, minPts = 5)
dbClus_pb

dbClus_pb <- x %>%  select(BASIS_FOR_PURCHASE) %>% scale() %>% dbscan(eps=5, minPts = 5)
dbClus_pb

dbClus_pb <- x %>%  select(BASIS_FOR_PURCHASE) %>% scale() %>% dbscan(eps=0.79, minPts = 2)
dbClus_pb

dbClus_pb <- x %>%  select(BASIS_FOR_PURCHASE) %>% scale() %>% dbscan(eps=0.81, minPts = 3)
dbClus_pb

dbClus_pb <- x %>%  select(BASIS_FOR_PURCHASE) %>% scale() %>% dbscan(eps=0.83, minPts = 3)
dbClus_pb

data("multishapes")
#Plot the points
multishapes %>% ggplot(aes(x=x,y=y, col=as.factor(shape)))+geom_point()
msDbscan <- dbscan(multishapes[,1:2], eps = 0.5, minPts = 5)
fviz_cluster(msDbscan, data=multishapes[,1:2], geom="point", ellipse = FALSE, main="dbscan eps=0.5, minPts=5")
fviz_cluster(msDbscan, data=multishapes[,1:2], geom="point", ellipse = FALSE, main="dbscan eps=5, minPts=5")
fviz_cluster(msDbscan, data=multishapes[,1:2], geom="point", ellipse = FALSE, main="dbscan eps=0.79, minPts=2")
fviz_cluster(msDbscan, data=multishapes[,1:2], geom="point", ellipse = FALSE, main="dbscan eps=0.81, minPts=3")
fviz_cluster(msDbscan, data=multishapes[,1:2], geom="point", ellipse = FALSE, main="dbscan eps=0.83, minPts=3")


#basis of purchase
fviz_cluster(msDbscan, data=multishapes[,1:2], geom="point", ellipse = FALSE, main="dbscan eps=0.5, minPts=5")
fviz_cluster(msDbscan, data=multishapes[,1:2], geom="point", ellipse = FALSE, main="dbscan eps=5, minPts=5")
fviz_cluster(msDbscan, data=multishapes[,1:2], geom="point", ellipse = FALSE, main="dbscan eps=0.79, minPts=2")
fviz_cluster(msDbscan, data=multishapes[,1:2], geom="point", ellipse = FALSE, main="dbscan eps=0.81, minPts=3")
fviz_cluster(msDbscan, data=multishapes[,1:2], geom="point", ellipse = FALSE, main="dbscan eps=0.83, minPts=3")

kNNdistplot(multishapes[,1:2], k=4)




########################################### Third Model (problem 4)===========
# Kernel K means

# figuring out optimal clusters and which kernel type 
center <- c(2,3,4,5)
length(center)
bestit <- rep(0, length(center))
bestit2 <- rep(0, length(center))

#################### basis for purchase###########

for (i in 1:length(center)) {
  xpb<-bsd %>% select(BASIS_FOR_PURCHASE) %>% scale()
  kkc_pb<-kkmeans(xpb, centers=center[i], kernel='rbfdot')
  bestit[i]<-sum(withinss(kkc_pb))/sum(size(kkc_pb))
  
}
table(bestit, center)  #so 2 is best by a good amount then 3 then 5. if we use default-2 clusters


for (i in 1:length(center)) {
  xpb<-bsd %>% select(BASIS_FOR_PURCHASE) %>% scale()
  kkc_pb<-kkmeans(xpb, centers=center[i], kernel='rbfdot', kpar=list(sigma=0.005))
  bestit2[i]<-sum(withinss(kkc_pb))/sum(size(kkc_pb))
  
}
table(bestit2, center)  #so we can't determine between 2/3 which is better- for sigma = .005


######################### Purchase Behavior  ########################
for (i in 1:length(center)) {
  xpb<-bsd %>% select(PURCHASE_BEHAVIOR) %>% scale()
  kkc_pb<-kkmeans(xpb, centers=center[i], kernel='rbfdot', kpar=list(sigma=0.005))
  bestit2[i]<-sum(withinss(kkc_pb))/sum(size(kkc_pb))
  
}
#table(bestit2, center)    #2 nearly always best then 3 then either 4 or 5 if we do sigma= .005
min(bestit2)

for (i in 1:length(center)) {
  xpb<-bsd %>% select(PURCHASE_BEHAVIOR) %>% scale()
  kkc_pb<-kkmeans(xpb, centers=center[i], kernel='rbfdot')
  bestit2[i]<-sum(withinss(kkc_pb))/sum(size(kkc_pb))
  
}
#table(bestit2, center)   #2 usually best- then 3 or 4
min(bestit2)

######################################### Linear Cluster pred ################
for (i in 1:length(center)) {
  xpb<-bsd %>% select(PURCHASE_BEHAVIOR) %>% scale()
  xkkc_pb_lin<-kkmeans( xpb,centers=3, kernel='vanilladot')
  bestit2[i]<-sum(withinss(xkkc_pb_lin))/sum(size(xkkc_pb_lin))
  
}
#table(bestit2, center)   #2 or 3 then 5 or 4-inconsistent
min(bestit2)


for (i in 1:length(center)) {
  xpb<-bsd %>% select(BASIS_FOR_PURCHASE) %>% scale()
  xkkc_pb_lin<-kkmeans( xpb,centers=3, kernel='vanilladot')
  bestit2[i]<-sum(withinss(xkkc_pb_lin))/sum(size(xkkc_pb_lin))
  
}
table(bestit2, center)   #2 usually best- then 3 or 4

# no improvement shown when running linear- stick to radial

######### Purchase Behavior ###################################################
#kernel k- means
library(kernlab)
#check the naming from earlier on brand runs

PURCHASE_BEHAVIOR <- c('No__of_Brands', 'brand_runs', 'Total_Volume', 'No__of__Trans', 'Value', 'Trans___Brand_Runs', 'Vol_Tran',
                       'Avg__Price', 'maxBr', 'Others_999')


# bsd1 PP clusters - 2/3 

xpb<-bsd1PP %>% select(PURCHASE_BEHAVIOR) %>% scale() #radial basis kernel gaussian

#### with the .005 sigma #######
kkc_pb<-kkmeans(xpb, centers=2, kernel='rbfdot', kpar=list(sigma=0.005))  
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="Purchase Behavior- kkmeans with bds1PP radial-3 clusters") #get it to plot
withinss(kkc_pb)/(size(kkc_pb))
sum(withinss(kkc_pb))/sum(size(kkc_pb))

kkc_pb<-kkmeans(xpb, centers=3, kernel='rbfdot', kpar=list(sigma=0.005))  
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="Purchase Behavior- kkmeans with bds1PP radial-3 clusters") #get it to plot
withinss(kkc_pb)/(size(kkc_pb))
sum(withinss(kkc_pb))/sum(size(kkc_pb))

#for default sigma

kkc_pb<-kkmeans(xpb, centers=2, kernel='rbfdot')
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="kkmeans with bds1PP radial-5 clusters") #get it to plot
withinss(kkc_pb)/(size(kkc_pb))
sum(withinss(kkc_pb))/sum(size(kkc_pb))

kkc_pb<-kkmeans(xpb, centers=3, kernel='rbfdot')
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="kkmeans with bds1PP radial-5 clusters") #get it to plot
withinss(kkc_pb)/(size(kkc_pb))
sum(withinss(kkc_pb))/sum(size(kkc_pb))


# now bsd1  
xpb<-bsd1 %>% select(PURCHASE_BEHAVIOR) %>% scale() 

#radial basis kernel gaussian
kkc_pb<-kkmeans(xpb, centers=2, kernel='rbfdot')
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="kkmeans with bsd1") #get it to plot
withinss(kkc_pb)/(size(kkc_pb))
sum(withinss(kkc_pb))/sum(size(kkc_pb))

kkc_pb<-kkmeans(xpb, centers=3, kernel='rbfdot')
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="kkmeans with bsd1") #get it to plot
withinss(kkc_pb)/(size(kkc_pb))
sum(withinss(kkc_pb))/sum(size(kkc_pb))


# original bsd and just ignore the na's 
xpb<-bsd %>% select(PURCHASE_BEHAVIOR) %>% scale()

kkc_pb<-kkmeans(xpb, centers=2, kernel='rbfdot')
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="Purchase Behavior-kkmeans with bsd- 2 clusters") #get it to plot
withinss(kkc_pb)/(size(kkc_pb))
sum(withinss(kkc_pb))/sum(size(kkc_pb))

#use default sigma
kkc_pb<-kkmeans(xpb, centers=3, kernel='rbfdot')
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="Purchase Behavior-kkmeans with bsd-3 clusters") #get it to plot
withinss(kkc_pb)/(size(kkc_pb))
sum(withinss(kkc_pb))/sum(size(kkc_pb))

#just a plot to reference
kkc_pb<-kkmeans(xpb, centers=3, kernel='rbfdot', kpar=list(sigma=0.005))
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="Purchase Behavior-kkmeans with bsd-3 clusters") #get it to plot
withinss(kkc_pb)/(size(kkc_pb))
sum(withinss(kkc_pb))/sum(size(kkc_pb))

#include this also- just a different kernel type--linear to PLOT
xkkc_pb_lin<-kkmeans( xpb,centers=3, kernel='vanilladot')
fviz_cluster(list(data=xpb, cluster=xkkc_pb_lin@.Data), geom = "point", main="kkmeans with bds1PP linear")
withinss(xkkc_pb_lin)/(size(xkkc_pb_lin))
sum(withinss(xkkc_pb_lin))/sum(size(xkkc_pb_lin))

#original bsd with PP 
bsdPP <- bsd
bsdPP[,10:45] = scale(bsdPP[,10:45])

xpb<-bsdPP %>% select(PURCHASE_BEHAVIOR) %>% scale()

kkc_pb<-kkmeans(xpb, centers=3, kernel='rbfdot')  
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="kkmeans with bsd default sigma") #get it to plot
withinss(kkc_pb)/(size(kkc_pb))
sum(withinss(kkc_pb))/sum(size(kkc_pb))

kkc_pb<-kkmeans(xpb, centers=2, kernel='rbfdot')  
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="kkmeans with bsd default sigma") #get it to plot
withinss(kkc_pb)/(size(kkc_pb))
sum(withinss(kkc_pb))/sum(size(kkc_pb))

kkc_pb<-kkmeans(xpb, centers=2, kernel='rbfdot', kpar=list(sigma=0.005))  
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="kkmeans with bsd sigma=.005") #get it to plot
withinss(kkc_pb)/(size(kkc_pb))
sum(withinss(kkc_pb))/sum(size(kkc_pb))

# now with median as replacement 
xpb<-df %>% select(PURCHASE_BEHAVIOR) %>% scale() #radial basis kernel gaussian

kkc_pb<-kkmeans(xpb, centers=2, kernel='rbfdot')
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="kkmeans with bds1PP") #get it to plot
withinss(kkc_pb)/(size(kkc_pb))
sum(withinss(kkc_pb))/sum(size(kkc_pb))

kkc_pb<-kkmeans(xpb, centers=3, kernel='rbfdot')
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="kkmeans with bds1PP") #get it to plot
withinss(kkc_pb)/(size(kkc_pb))
sum(withinss(kkc_pb))/sum(size(kkc_pb))

# now median but with PP 
dfPP <- df
dfPP[,10:45] = scale(dfPP[,10:45])

xpb<-dfPP %>% select(PURCHASE_BEHAVIOR) %>% scale() #radial basis kernel gaussian

kkc_pb<-kkmeans(xpb, centers=2, kernel='rbfdot')
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="kkmeans with bds1PP") #get it to plot
withinss(kkc_pb)/(size(kkc_pb))
sum(withinss(kkc_pb))/sum(size(kkc_pb))

kkc_pb<-kkmeans(xpb, centers=3, kernel='rbfdot')
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="kkmeans with bds1PP") #get it to plot
withinss(kkc_pb)/(size(kkc_pb))
sum(withinss(kkc_pb))/sum(size(kkc_pb))


################# Basis for Purchase ###############

BASIS_FOR_PURCHASE <- c('Pur_Vol_No_Promo____','Pur_Vol_Promo_6__',
                        'Pur_Vol_Other_Promo__','Br__Cd__57__144',
                        'Br__Cd__55','Br__Cd__272','Br__Cd__286',
                        'Br__Cd__286','Br__Cd__24','Br__Cd__481',
                        'Br__Cd__352','Br__Cd__5','Others_999',
                        'Pr_Cat_1','Pr_Cat_2','Pr_Cat_3','Pr_Cat_4',
                        'PropCat_5','PropCat_6','PropCat_7','PropCat_8',
                        'PropCat_9','PropCat_10','PropCat_11','PropCat_12',
                        'PropCat_13','PropCat_14','PropCat_15')

# bsd1 PP 
xpb<-bsd1PP %>% select(BASIS_FOR_PURCHASE) %>% scale()

#try 3 clusters
kkc_pb<-kkmeans(xpb, centers=2, kernel='rbfdot', na.action = na.omit)  
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="BFP kkmeans with bsd1PP- radial- 2 clusters") 
withinss(kkc_pb)/size(kkc_pb)
#overall MCSS for the whole thing
sum(withinss(kkc_pb))/sum(size(kkc_pb))


#try 5 clusters
kkc_pb<-kkmeans(xpb, centers=3, kernel='rbfdot', na.action = na.omit)  
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="BFP kkmeans with bsd1PP- radial- 3 clusters") 
withinss(kkc_pb)/size(kkc_pb)
#overall MCSS for the whole thing
sum(withinss(kkc_pb))/sum(size(kkc_pb))

#now with sigma =.005
kkc_pb<-kkmeans(xpb, centers=2, kernel='rbfdot', kpar=list(sigma=0.005), na.action = na.omit)  
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="BFP kkmeans with bsd1PP- radial- 2 clusters") 
withinss(kkc_pb)/size(kkc_pb)
#overall MCSS for the whole thing
sum(withinss(kkc_pb))/sum(size(kkc_pb))

#sigma = .005
kkc_pb<-kkmeans(xpb, centers=3, kernel='rbfdot', kpar=list(sigma=0.005), na.action = na.omit)  
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="BFP kkmeans with bsd1PP- radial- 3 clusters") 
withinss(kkc_pb)/size(kkc_pb)
#overall MCSS for the whole thing
sum(withinss(kkc_pb))/sum(size(kkc_pb))

#now see the linear kernel  to plot ONLY
xkkc_pb_lin<-kkmeans(xpb,centers=3, kernel='vanilladot')
fviz_cluster(list(data=xpb, cluster=xkkc_pb_lin@.Data), geom = "point", main="kkmeans with bds1PP linear") #get it to plot
(withinss(xkkc_pb_lin))/(size(xkkc_pb_lin))
sum(withinss(xkkc_pb_lin))/sum(size(xkkc_pb_lin))

# bsd 
xpb<-bsd %>% select(BASIS_FOR_PURCHASE) %>% scale()

kkc_pb<-kkmeans(xpb, centers=3, kernel='rbfdot')  
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="kkmeans with bsd default sigma") #get it to plot
withinss(kkc_pb)/(size(kkc_pb))
sum(withinss(kkc_pb))/sum(size(kkc_pb))

kkc_pb<-kkmeans(xpb, centers=2, kernel='rbfdot')  
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="kkmeans with bsd default sigma") #get it to plot
withinss(kkc_pb)/(size(kkc_pb))
sum(withinss(kkc_pb))/sum(size(kkc_pb))

kkc_pb<-kkmeans(xpb, centers=2, kernel='rbfdot', kpar=list(sigma=0.005))  
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="kkmeans with bsd sigma=.005") #get it to plot
withinss(kkc_pb)/(size(kkc_pb))
sum(withinss(kkc_pb))/sum(size(kkc_pb))

# bsd with PP 
xpb<-bsdPP %>% select(BASIS_FOR_PURCHASE) %>% scale()

kkc_pb<-kkmeans(xpb, centers=2, kernel='rbfdot', na.action = na.omit)
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data),geom = "point", main="kkmeans with bsdPP- radial-2 clusters- DEFAULT SIGMA") 
withinss(kkc_pb)/size(kkc_pb)
sum(withinss(kkc_pb))/sum(size(kkc_pb))

#try default sigma
kkc_pb<-kkmeans(xpb, centers=3, kernel='rbfdot', na.action = na.omit)
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data),geom = "point", main="kkmeans with bsdPP- radial-3 clusters- DEFAULT SIGMA") 
withinss(kkc_pb)/size(kkc_pb)
sum(withinss(kkc_pb))/sum(size(kkc_pb))

#try default sigma
kkc_pb<-kkmeans(xpb, centers=3, kernel='rbfdot',  kpar=list(sigma=0.005), na.action = na.omit)
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data),geom = "point", main="kkmeans with bsdPP- radial-3 clusters") 
withinss(kkc_pb)/size(kkc_pb)
sum(withinss(kkc_pb))/sum(size(kkc_pb))

## bsd1 
xpb<-bsd1 %>% select(BASIS_FOR_PURCHASE) %>% scale()

kkc_pb<-kkmeans(xpb, centers=2, kernel='rbfdot', na.action = na.omit)  
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="BFP kkmeans with bsd1") 
withinss(kkc_pb)/size(kkc_pb)
sum(withinss(kkc_pb))/sum(size(kkc_pb))

kkc_pb<-kkmeans(xpb, centers=3, kernel='rbfdot', na.action = na.omit)  
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="BFP kkmeans with bsd1") 
withinss(kkc_pb)/size(kkc_pb)
sum(withinss(kkc_pb))/sum(size(kkc_pb))

# df (median replace) 
xpb<-df %>% select(BASIS_FOR_PURCHASE) %>% scale() #radial basis kernel gaussian

kkc_pb<-kkmeans(xpb, centers=2, kernel='rbfdot')  
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="kkmeans with bds1PP") #get it to plot
withinss(kkc_pb)/(size(kkc_pb))
sum(withinss(kkc_pb))/sum(size(kkc_pb))

kkc_pb<-kkmeans(xpb, centers=3, kernel='rbfdot')  
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="kkmeans with bds1PP") #get it to plot
withinss(kkc_pb)/(size(kkc_pb))
sum(withinss(kkc_pb))/sum(size(kkc_pb))

# now median but with PP 
dfPP <- df
dfPP[,10:45] = scale(dfPP[,10:45])

xpb<-dfPP %>% select(BASIS_FOR_PURCHASE) %>% scale() #radial basis kernel gaussian

kkc_pb<-kkmeans(xpb, centers=2, kernel='rbfdot')  
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="kkmeans with bds1PP") #get it to plot
withinss(kkc_pb)/(size(kkc_pb))
sum(withinss(kkc_pb))/sum(size(kkc_pb))

kkc_pb<-kkmeans(xpb, centers=3, kernel='rbfdot')  
fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="kkmeans with bds1PP") #get it to plot
withinss(kkc_pb)/(size(kkc_pb))
sum(withinss(kkc_pb))/sum(size(kkc_pb))

######### Question 5 ################

######### Decision Tree ################

library(rpart)
library(rpart.plot)

#how to determine customer loyalty- total volume or purchases

bsd$CHILD <- (as.numeric(bsd$CHILD))
bsd <- bsd %>%  filter(CHILD !="5")  #need to remove the "not specified"
#also we omit the blank values since they do not help us

# total volume without affluence index 
dfnew <- subset(bsd, select = c(SEX, EDU, SEC, FEH_1, FEH_2, FEH_3, MT_10, MT_4, MT_5,
                                CS, CHILD, HS, AGE, Total_Volume))
dt1=rpart(Total_Volume ~ ., data= dfnew, control = rpart.control(minsplit = 30), na.action = na.omit)
dt1$variable.importance

# plotting the tree
rpart.plot::prp(dt1, type=2, extra=100)

# total volume WITH affluence index included
dfnew <- subset(bsd, select = c(SEX, EDU, SEC, FEH_1, FEH_2, FEH_3, MT_10, MT_4, MT_5,
                                CS, CHILD, HS, AGE, Total_Volume, Affluence_Index))

dt1=rpart(Total_Volume ~ ., data= dfnew, control = rpart.control(minsplit = 30), na.action = na.omit)
dt1$variable.importance

# plotting the tree
rpart.plot::prp(dt1, type=2, extra=100)

# brand runs  without affluence index  
dfnew <- subset(bsd, select = c(SEX, EDU, SEC, FEH_1, FEH_2, FEH_3, MT_10, MT_4, MT_5,
                                CS, CHILD, HS, AGE, brand_runs))
dt1=rpart(brand_runs ~ ., data= dfnew, control = rpart.control(minsplit = 30), na.action = na.omit)
dt1$variable.importance
# plotting the tree
rpart.plot::prp(dt1, type=2, extra=100)


###brand runs  with affluence index
dfnew <- subset(bsd, select = c(SEX, EDU, SEC, FEH_1, FEH_2, FEH_3, MT_10, MT_4, MT_5,
                                CS, CHILD, HS, AGE, brand_runs, Affluence_Index))
dt1=rpart(brand_runs ~ ., data= dfnew, control = rpart.control(minsplit = 30), na.action = na.omit)
dt1$variable.importance

# plotting the tree
rpart.plot::prp(dt1, type=2, extra=100)

dfnew <- subset(bsd, select = c(SEX, EDU, SEC, FEH_1, FEH_2, FEH_3, MT_10, MT_4, MT_5,
                                CS, CHILD, HS, AGE, brand_runs))
dt1=rpart(brand_runs ~ ., data= dfnew, control = rpart.control(minsplit = 30))
dt1$variable.importance

rpart.plot::prp(dt1, type=2, extra=100)