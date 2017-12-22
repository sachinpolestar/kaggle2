list.files()
df<-read.csv("house_price_train.csv",stringsAsFactors = F)
df<-tbl_df(df)
dim(df)
df

str(df)

#data cleaning

na_data<-sapply(df1, function(x)sum(is.na(x)))
na_data<-as.data.frame(na_data)

View(na_data)

#########################
# handling NAs
is.na(df$BsmtQual)
summary(BsmtQual)
table(BsmtQual)
sum(complete.cases(BsmtQual))


#Alley
anyNA(df$Alley)
sum(is.na(df$Alley))
df$Alley[is.na(df$Alley)] <- "NA"
View(df)
unique(df$Alley)

chisq.test(ExterQual,BsmtQual)

df[sort(complete.cases(BsmtQual)),]
ggplot(df,aes(x=ExterQual,y=BsmtQual,color=BsmtQual))+geom_point()
table(ExterQual)
table(BsmtQual)
str(BsmtQual)

unique(BsmtQual)

a<-df$BsmtQual
a

df$BsmtQual<-as.character(df$BsmtQual)
detach(df)
anyNA(df$BsmtQual)
table(df$BsmtQual)
anyNA(df$BsmtCond)
str(df$BsmtCond)
sum(is.na(df$BsmtCond))
df$BsmtQual[is.na(df$BsmtQual)] <- "NB"
df$BsmtCond[is.na(df$BsmtCond)] <- "NB"

#BsmtExposure
df$BsmtExposure[is.na(df$BsmtExposure)] <- "NB"
table(df$BsmtExposure)
anyNA(df$BsmtExposure)

#BsmtFinType1
df$BsmtFinType1[is.na(df$BsmtFinType1)] <- "NB"
table(df$BsmtFinType1)
anyNA(df$BsmtFinType1)

#BsmtFinType2
df$BsmtFinType2[is.na(df$BsmtFinType2)] <- "NB"
table(df$BsmtFinType2)
anyNA(df$BsmtFinType2)

anyNA(df$BsmtCond)
table(df$BsmtCond)

#Electrical

anyNA(df$Electrical)
table(df$Electrical)
which(is.na(df$Electrical))
df$Electrical[1380]<-"SBrkr"

#FireplaceQu
anyNA(df$FireplaceQu)
table(df$FireplaceQu)
df$FireplaceQu[is.na(df$FireplaceQu)] <- "NF"

#GarageType
anyNA(df$GarageType)
table(df$GarageType)
sum(complete.cases(df$GarageType))
df$GarageType[is.na(df$GarageType)] <- "NG"

#GarageYrBlt - Using Knn imputation
anyNA(df$GarageYrBlt)
table(df$GarageYrBlt)
sum(is.na(df$GarageYrBlt))
a<-which(is.na(df$GarageYrBlt))

subset(df,is.na(df$GarageYrBlt),select =c(GarageYrBlt,SalePrice) )
which(is.na(df$GarageYrBlt))

#changing garage year to 0 where no garage is present.
df$GarageYrBlt[is.na(df$GarageYrBlt)] <- 0
sum(is.na(df$GarageYrBlt))
anyNA(df$GarageYrBlt)
table(df$GarageYrBlt)
#normalizing year into weights
norm<-function(x){
  a<-((x-min(x))/(max(x)-min(x)))
  return(a)
}

df$GarageYrBlt<-norm(df$GarageYrBlt)
View(df)

#GarageCond
anyNA(df$GarageCond)
table(df$GarageCond)
sum(complete.cases(df$GarageCond))
df$GarageCond[is.na(df$GarageCond)] <- "NG"


#GarageFinish

anyNA(df$GarageFinish)
table(df$GarageFinish)
sum(complete.cases(df$GarageFinish))
df$GarageFinish[is.na(df$GarageFinish)] <- "NG"

#GarageQual
anyNA(df$GarageQual)
table(df$GarageQual)
sum(complete.cases(df$GarageQual))
df$GarageQual[is.na(df$GarageQual)] <- "NG"
View(df)

#PoolQC
anyNA(df$PoolQC)
table(df$PoolQC)
sum(complete.cases(df$PoolQC))
df$PoolQC[is.na(df$PoolQC)] <- "NP"
View(df)

qplot(SalePrice,data=df,fill=PoolQC,geom="density",alpha=I(0.5))
filter(df,PoolQC %in% c("Ex","Fa","Gd")) %>%
  select(PoolQC,SalePrice,LotArea,GarageYrBlt)
cor(df$SalePrice,df$LotArea)



#Fence
anyNA(df$Fence)
table(df$Fence)
sum(complete.cases(df$Fence))
df$Fence[is.na(df$Fence)] <- "NF"
View(df)
qplot(SalePrice,data=df,fill=Fence,geom="density",alpha=I(0.5))


#MasVnrType

i<-which(is.na(df$MasVnrType))
summary(aov(df$SalePrice~df$MasVnrType))

filter(df,is.na(MasVnrType)) %>%
  select(MasVnrType,MasVnrArea,SalePrice)
table(df$MasVnrType)

af<-filter(df,SalePrice>400000) %>%
  select(SalePrice,MasVnrType,MasVnrArea)
View(af)
str(af)
qplot(SalePrice,data=df,fill=MasVnrType,geom="density",alpha=I(0.5))

df$MasVnrType[i]<-"Stone"

#MasVnrArea

df %>% summarise(group_by(MasVnrType),mean(MasVnrArea))

group_by(df,MasVnrType) %>%
  summarise(mean(MasVnrArea,na.rm=T))


ag<-filter(df,MasVnrType == "None",MasVnrArea>0) %>%
  select(MasVnrType,MasVnrArea)
View(ag) 
mean(ag$MasVnrArea)


ah<-filter(df,MasVnrArea > 270) %>%
  select(MasVnrType,MasVnrArea,SalePrice)
View(ah)

qplot(MasVnrArea,data=ah,geom="density",alpha=I(0.5),fill=MasVnrType)
table(df$MasVnrType)
qplot(SalePrice,data=ah,geom="density",alpha=I(0.5),fill=MasVnrType)

#concluded its BrkCmn in ag (NONE) where area>0

ag1<-filter(df,df$MasVnrArea > 0 , df$MasVnrType == "None") %>%
  select(Id,MasVnrType,MasVnrArea)

df$MasVnrType[ag1$Id] <- "BrkCmn"



ah1<-filter(df,MasVnrArea > 270) %>%
  select(MasVnrType,MasVnrArea,SalePrice)
View(ah1)
qplot(MasVnrArea,data=ah1,geom="density",alpha=I(0.5),fill=MasVnrType)
qplot(SalePrice,data=ah1,geom="density",alpha=I(0.5),fill=MasVnrType)


#MasVnrType2
anyNA(df$MasVnrType)
anyNA(df$MasVnrArea)
sum(is.na(df$MasVnrArea))

filter(df,is.na(MasVnrArea)) %>%
  select(Id,MasVnrArea,MasVnrType,SalePrice)


group_by(df,MasVnrType) %>%
  summarise(mean(MasVnrArea,na.rm=T))

#mean imputation
df$MasVnrArea[is.na(df$MasVnrArea)] <- mean(df$MasVnrArea[df$MasVnrType == "BrkCmn"])
#or
df$MasVnrArea[is.na(df$MasVnrArea)] <- 233.05
group_by(df,MasVnrType) %>%
  summarise(mean(MasVnrArea,na.rm=T))

#in garages # garage finish can be removed
qplot(SalePrice,data=df,geom="density",fill=GarageFinish,alpha=I(.5))
unique(df$GarageFinish)
unique(df$GarageType)
unique(aa)
summary(df$GarageYrBlt)
chisq.test(df$GarageYrBlt,df$GarageType)
aov1<-aov(df$SalePrice~df$GarageYrBlt)

##KNN imputation
detach(DMwR)
#install.packages("DMwR")
#knnImputation(df$GarageYrBlt,k=10,scale=F,meth = "weighAvg")



summary(aov1)
#GarageFinish 
anyNA(df$GarageFinish)
table(df$GarageFinish)
df$GarageFinish[is.na(df$GarageFinish)] <- "NG"


#LotFrontage + Mean imputation

anyNA(df$LotFrontage)
sum(is.na(df$LotFrontage))
table(df$LotFrontage)
df$MSSubClass<-as.factor(df$MSSubClass)
ae<-filter(df,is.na(LotFrontage))%>%
  select(Id,MSSubClass,LotFrontage,LotArea)
View(ae)
qplot(LotFrontage,data=df,geom = "density",fill=MSSubClass,alpha=I(0.5))

ae1<-group_by(ae,MSSubClass) %>%
  summarise(mean(LotArea,na.rm=T),sum(is.na(LotFrontage)))

ae_df<-group_by(df,MSSubClass) %>%
  summarise(mean(LotFrontage,na.rm = T),sum(is.na(LotFrontage)),
            mean(LotArea,na.rm=T))
ae_df1<-group_by(df,MSSubClass,MSZoning) %>%
  summarise(mean(LotFrontage,na.rm = T),sum(is.na(LotFrontage)),
            mean(LotArea,na.rm=T))

View(ae_df)
View(ae_df1)
dim(ae1)  
dim(ae2)

#concluded LotFrntage is in accordance with MSSubclass.
#mean imputation.

#converting MSZoning column a bit for C(all) 


df$MSZoning[df$MSZoning == "C (all)"] <-"C"


filter(df,is.na(LotFrontage),MSSubClass == 20,
       MSZoning == "C") %>%
  select(Id,LotFrontage,MSSubClass)
#checking total non zero values in ae_df1
a<-which(ae_df1$`sum(is.na(LotFrontage))` >0)
a
#making a copy of df so far
df1<-df
df2<-df

#subsetting non zero ae_df1

ae_df2<-ae_df1[a,]
View(ae_df2)
#start here Thu
#game changer For Loop
#objects for cross check ae_df2, ae_df1,ae_df
for( i in unique(ae_df2$MSSubClass) ){
  
  for (j in ae_df2$MSZoning) {
    df1$LotFrontage[c(is.na(df1$LotFrontage) & df1$MSZoning == j 
                      & df1$MSSubClass == i)]<-ae_df2$`mean(LotFrontage, na.rm = T)`[c(ae_df2$MSSubClass == i & ae_df2$MSZoning == j)]
    aa<-NULL
    j<-NULL
  }
  
}
write.csv(df1, file="df1.csv")

which(df1$LotFrontage[c(df1$LotFrontage == "NA" & df1$MSZoning == "FV" 
                  & df1$MSSubClass == 20)])
which(df1$Id[c(df1$MSZoning == "FV" & df1$MSSubClass == 20)])
df1$LotFrontage[c(is.na(df1$LotFrontage) & df1$MSSubClass == 85 & df1$MSZoning == "RH")]




l=20
m="FV"
ae_df2$`mean(LotFrontage, na.rm = T)`[c(ae_df2$MSSubClass == l & ae_df2$MSZoning == m)]

#For Loop saved repeated iteration of below line of code 24 times
df1$LotFrontage[c(df1$LotFrontage == "NA" & df$MSZoning == "RL" 
                  & df$MSSubClass == 20)]


#Mean Imputataion complete
#checkpoints
#df2 as df before Lot Frontage Mean Imputation
#saving caryying forward df1


#going feature wise









#----------------------------------------------------------
#mutating some columns
summary(df$LotArea_100)
df<-mutate(df, LotArea = (LotArea/100))
?mutate()
df$LotArea_100
View(df)
dim(df)

table(MSZoning)




table(Street)
sum(is.na(Street))
qplot(SalePrice,data=df,fill=Street,geom = "density",alpha=I(0.5))
ggplot(df,aes(x=Street,y=SalePrice,color = Street))+geom_point()


##########################3
install.packages(c("dplyr","ggplot2"))

library(dplyr)
library(ggplot2)

glimpse(df)



ggplot(df, aes(x= TotalBsmtSF, y = SalePrice))+
  geom_point()

df$SalePrice<-df$SalePrice/100
head(df$SalePrice,10)

which(df$TotalBsmtSF > 6000)
df$TotalBsmtSF[1299]
df[1299,]

df$GarageArea[1299]
summary(df$GarageArea)

ggplot(df, aes(x= GarageArea, y = SalePrice))+
  geom_point()

cor(df$SalePrice,df$GarageArea)
cor(df$TotalBsmtSF,df$SalePrice)
cor(df$GarageArea,df$TotalBsmtSF)


ggplot(df, aes(x= Street, y = SalePrice))+
  geom_point()
dim(df)

table(df$Street)




ggplot(df, aes(x= LotArea, y = SalePrice))+
  geom_point()

# lot area outliers
index<-c(250,314,336,707)

table(df$CentralAir)

summary(df$LotFrontage)
table(df$MSZoning)
head(df$LotFrontage)

ggplot(df, aes(x= LotFrontage, y = SalePrice))+
  geom_point()
glimpse(df$LotFrontage)
mean(df$LotFrontage,na.rm = T)

ggplot(df,aes(x=LotFrontage,y=LotArea))+
  geom_point()

table(df$Street)
unique(df$Street)
attach(df)
table(df$Alley)

length(Alley)
head(Alley)
a<-complete.cases(df$Alley)


df[a,c("Street","Alley","SalePrice")]

table(LotShape)
select(df,LotShape,SalePrice)
?hist()

ggplot(df,aes(x=LotFrontage,y=SalePrice,color = LotShape))+
  geom_point()

summarise(df,SalePrice,LotShape)
group_by(df,LotShape) %>%
  summarise(summary = IQR(SalePrice), n=n())

table(LandContour)

ggplot(df,aes(x=LandContour,y=SalePrice,color = LandContour))+
  geom_point()


table(LandContour)
df[Utilities == "NoSeWa",]

table(LotConfig)
ggplot(df,aes(x=LotConfig,y=SalePrice,color = LotConfig))+
  geom_point()


which(df[c(SalePrice>6000),])
filter(df,SalePrice > 6000) %>%
  select(SalePrice,LotConfig)

table(LandSlope)
ggplot(df,aes(x=LandSlope,y=SalePrice,color = LandSlope))+
  geom_point()

#Condition1
table(Condition1)
chisq.test(SalePrice,Condition1)

#All Chisquares
table(MSZoning)
str(MSZoning)
MSZoning<-as.factor(MSZoning)
head(MSZoning)
qplot(SalePrice,data=df,geom="density",fill=MSZoning,alpha=I(.5))
