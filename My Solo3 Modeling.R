##### LOAD LIBRAR #####
library(ggplot2) # For graphical tools
library(MASS) # For some advanced statistics
install.packages("pscl")
library(pscl) # For "counting" models (e.g., Poisson and Negative Binomial)
library(dplyr) # For general needs and functions
library(readr)
install.packages("corrplot")
library(corrplot)
install.packages("ROCR")
library(ROCR)
install.packages("InformationValue")
library(InformationValue)
install.packages("qcc")
library(qcc)
install.packages("car")
library(car) #VIF, powerTransform()boxcox transformation, boxTidwell()to use MLE to improve linearity, spread-levelPlot() to improve hemoscedasticity
# Setup initial MICE
#install.packages("randomForest")
library(randomForest)
#install.packages("mice")
library(mice)
library(boot)
#install.packages("elrm")
#library(elrm)
library(grid)
library(gridExtra)
##### New packages from Solo 3 skeleton code
require(rpart)
install.packages("rpart.plot")
require(rpart.plot)
install.packages("tree")
require(tree)
install.packages("rattle")
require(rattle)
require(caTools)
install.packages("ResourceSelection")
require(ResourceSelection)
install.packages("corrgram")
library(corrgram)
install.packages("inTrees")
library(inTrees)
install.packages("pROC")
library(pROC)
install.packages("caret")
library(caret)
# Note, some of these libraries are not needed for this template code.
#library(zoo)
#library(psych)
#library(rJava)
#library(pbkrtest)
#library(leaps)
#library(glm2)
#library(aod)


####
setwd("C:\\Users\\joyce\\Desktop\\My Document\\Data Analytics\\Northwestern\\PREDICT450\\Course Materials\\Projects\\Solo 3")
###############################################################################################
###########  Step 1 - Load data and create some new variables 
###################################################################################################
load("XYZ_complete_customer_data_frame.RData")
ls()
mydata <- complete.customer.data.frame
names(mydata)

table(mydata$BUYER_STATUS)
table(mydata$RESPONSE16)
table(mydata$ANY_MAIL_16)
xtabs(~RESPONSE16 + ANY_MAIL_16, data = mydata)
addmargins(table(mydata$RESPONSE16,mydata$ANY_MAIL_16))

mydata$cum15QTY <- mydata$QTY0 + mydata$QTY1 + mydata$QTY2 + mydata$QTY3 +
  mydata$QTY4 + mydata$QTY5 + mydata$QTY6 + mydata$QTY7 + mydata$QTY8 +
  mydata$QTY9 + mydata$QTY10 + mydata$QTY11 + mydata$QTY12 + mydata$QTY13 +
  mydata$QTY14 + mydata$QTY15
mydata$cum15TOTAMT <- mydata$TOTAMT0 + mydata$TOTAMT1 + mydata$TOTAMT2 + mydata$TOTAMT3 +
  mydata$TOTAMT4 + mydata$TOTAMT5 + mydata$TOTAMT6 + mydata$TOTAMT7 + mydata$TOTAMT8 +
  mydata$TOTAMT9 + mydata$TOTAMT10 + mydata$TOTAMT11 + mydata$TOTAMT12 + mydata$TOTAMT13 +
  mydata$TOTAMT14 + mydata$TOTAMT15
mydata$cum15RESPONSE <- mydata$RESPONSE0 + mydata$RESPONSE1 + mydata$RESPONSE2 + mydata$RESPONSE3 +
  mydata$RESPONSE4 + mydata$RESPONSE5 + mydata$RESPONSE6 + mydata$RESPONSE7 + mydata$RESPONSE8 +
  mydata$RESPONSE9 + mydata$RESPONSE10 + mydata$RESPONSE11 + mydata$RESPONSE12 + mydata$RESPONSE13 +
  mydata$RESPONSE14 + mydata$RESPONSE15
## After several round of EDA, the following have high corrl with the above three variables 
#  and represent the same predictive power so OMIT
#mydata$cum14QTY <- mydata$QTY0 + mydata$QTY1 + mydata$QTY2 + mydata$QTY3 +
#  mydata$QTY4 + mydata$QTY5 + mydata$QTY6 + mydata$QTY7 + mydata$QTY8 +
#  mydata$QTY9 + mydata$QTY10 + mydata$QTY11 + mydata$QTY12 + mydata$QTY13 +
#  mydata$QTY14
#mydata$cum14TOTAMT <- mydata$TOTAMT0 + mydata$TOTAMT1 + mydata$TOTAMT2 + mydata$TOTAMT3 +
#  mydata$TOTAMT4 + mydata$TOTAMT5 + mydata$TOTAMT6 + mydata$TOTAMT7 + mydata$TOTAMT8 +
#  mydata$TOTAMT9 + mydata$TOTAMT10 + mydata$TOTAMT11 + mydata$TOTAMT12 + mydata$TOTAMT13 +
#  mydata$TOTAMT14
#mydata$cum14RESPONSE <- mydata$RESPONSE0 + mydata$RESPONSE1 + mydata$RESPONSE2 + mydata$RESPONSE3 +
#  mydata$RESPONSE4 + mydata$RESPONSE5 + mydata$RESPONSE6 + mydata$RESPONSE7 + mydata$RESPONSE8 +
#  mydata$RESPONSE9 + mydata$RESPONSE10 + mydata$RESPONSE11 + mydata$RESPONSE12 + mydata$RESPONSE13 +
#  mydata$RESPONSE14
#
#mydata$cum13QTY <- mydata$QTY0 + mydata$QTY1 + mydata$QTY2 + mydata$QTY3 +
#  mydata$QTY4 + mydata$QTY5 + mydata$QTY6 + mydata$QTY7 + mydata$QTY8 +
#  mydata$QTY9 + mydata$QTY10 + mydata$QTY11 + mydata$QTY12 + mydata$QTY13
#mydata$cum13TOTAMT <- mydata$TOTAMT0 + mydata$TOTAMT1 + mydata$TOTAMT2 + mydata$TOTAMT3 +
#  mydata$TOTAMT4 + mydata$TOTAMT5 + mydata$TOTAMT6 + mydata$TOTAMT7 + mydata$TOTAMT8 +
#  mydata$TOTAMT9 + mydata$TOTAMT10 + mydata$TOTAMT11 + mydata$TOTAMT12 + mydata$TOTAMT13
#mydata$cum13RESPONSE <- mydata$RESPONSE0 + mydata$RESPONSE1 + mydata$RESPONSE2 + mydata$RESPONSE3 +
#  mydata$RESPONSE4 + mydata$RESPONSE5 + mydata$RESPONSE6 + mydata$RESPONSE7 + mydata$RESPONSE8 +
#  mydata$RESPONSE9 + mydata$RESPONSE10 + mydata$RESPONSE11 + mydata$RESPONSE12 + mydata$RESPONSE13


mydata$salepertrans <- ifelse(mydata$PRE2009_TRANSACTIONS==0,0,mydata$PRE2009_SALES/mydata$PRE2009_TRANSACTIONS)
mydata$salepercamp <- ifelse(mydata$TOTAL_MAIL_15==0,0,mydata$cum15TOTAMT/mydata$TOTAL_MAIL_15)

# some statistics:
mean(mydata$salepertrans, trim = 0.01, na.rm = TRUE)
mean(mydata$salepercamp, trim = 0.05, na.rm = TRUE)

mydata.save <- mydata
table(mydata$ANY_MAIL_16) #TRAINING SET is those 14922 who got ANY_MAIL_16 =1, 15857 ANY_MAIL_16=0
################################################################################################
############### Step 2 - Pick only the people who were sent mailers in Campaign 16 ##########
##############   AND  prtend you are a domain expert and pick upto 50 'important' predictors
###############################################################################################
### laterL: mydata <- mydata %>% subset(ANY_MAIL_16>0)
mydata$COMPUTER_ELECTRONIC_flag <- ifelse(mydata$COMPUTER_ELECTRONIC==""|mydata$COMPUTER_ELECTRONIC=="U", 0,1)
mydata$SPORTS_RELATED_flag <- ifelse(mydata$SPORTS_RELATED==""|mydata$SPORTS_RELATED=="U", 0,1)
mydata$HOMEOWNR_flag <- ifelse(mydata$HOMEOWNR==""|mydata$HOMEOWNR=="U", 0,1)
mydata$HH_MULTI_flag <- ifelse(mydata$HH_MULTI==""|mydata$HH_MULTI=="U", 0,1)
mydata$ZGOURMET_flag <- ifelse(mydata$ZGOURMET==""|mydata$ZGOURMET=="U", 0,1)
mydata$ZHITECH_flag <- ifelse(mydata$ZHITECH==""|mydata$ZHITECH=="U", 0,1)
mydata$ZONLINE_flag <- ifelse(mydata$ZONLINE==""|mydata$ZONLINE=="U", 0,1)
mydata$ZBOATS_flag <- ifelse(mydata$ZBOATS==""|mydata$ZBOATS=="U", 0,1)
mydata$ZGOLFERS_flag <- ifelse(mydata$ZGOLFERS==""|mydata$ZGOLFERS=="U", 0,1)


########Pick varibles (this was done with lots of triel and error)
#keep.list.1 <- c('RESPONSE16','TOTAMT16','PRE2009_SALES','PRE2009_TRANSACTIONS','cum15QTY','cum15TOTAMT','cum15RESPONSE', 'cum14QTY','cum14TOTAMT','cum14RESPONSE',
#                 'cum13QTY','cum13TOTAMT','cum13RESPONSE', 'SUM_MAIL_16','TOTAL_MAIL_15','TOTAL_MAIL_14','TOTAL_MAIL_13',
#                 "ZIP","ZIP4","ZIP9_Supercode",'CHANNEL_ACQUISITION','BUYER_STATUS', "COMPUTER_ELECTRONIC","SPORTS_RELATED",
#                 "INC_SCS_AMT_V4","FIPSCNTY","BLOCK_ID","MCD_CCD","POP_UN18","PHHWHITE","P_ASIAN","HH_HISPA","P_HHFAMI",
#                 "POP_CHLDHHS","POP_ADPCHLDFAM","P_HHNOHISWHT","P_FEMALE","P_FAMHHNOCHD","P_MARRY","P_HHINCOM35_39","P_HHINCOM40_44","P_HHINCOM45_49",
#                 "P_HHINCOM50_59","P_HHINCOM60_74","P_HHINCOM150_199","P_HHINCOM20_UP","MED_FAMINCOM","MED_NOFAMINCOM","P_CAPITA_INCOM",
#                 "P_IND_MANUFACT","P_IND_RETAILTRD","P_IND_FINALNCE","P_IND_PROFFES","MED_HOME","MED_DWELL_AGE","CUR_EST_MED_INC","STATE_INC_INDEX",
#                 "CUR_ST_EST_FAM_INC","CENSUS_FACT1","CENSUS_FACT4","CENSUS_SEG1","CENSUS_SEG2","CENSUS_SEG4","GEOPIXELCODE",
#                 "EXAGE","ADULT1_G","MARRIED","ETHNIC_DETAIL","ETHNIC_GROUP","E_TECH","ETHNIC_COUNTRY","RRC","ADULT1_R","HOMEOWNR","ADD_TYPE","LOR1",
#                 "DUS","NUM_CHILD","NUMBADLT","HH_DMR","HH_MULTI",
#                 "DM_GIFTS","DM_BOOKS","DM_FEM","DM_MALE","DM_UP","DM_GEN","MGZ_HLTH","MGZ_FAM","CON_HLTH","CON_POLT","NEWS","RESPODDS","OCCUPATION","OCCUPATION_GROUP",
#                 "IND_ED","CHILDPROB","PHONEMATCH","ITMM","NEWCAR","USEDCAR","EMAIL_RECEPTIVE","CENTSCH","CHANNEL_DOMINANCE","AD_WEB","AD_MAGAZINE","AD_NEWSPAPER","AD_RADIO",
#                 "AD_TV","STINCIND","CTINCIND","ESTHMVL","ECHVINX","ECHVPCT","SALES","TRANSTYP","LOAN_AMT","LOAN_TYP","YEAR_BLT","ESTAERNG","ESTMORTPAYRNG","ESTMORTAMTRNG",
#                 "M_HH_LEVEL","M_GRPTYPE_MEDIAN","ZGOURMET","ZCOMPUTR","ZHITECH","ZONLINE","ZGOLFERP","ZPETSP","ZARTSP","ZMOBP","ZFITNESP","ZOUTDOOP","ZAUTOOWP","ZCOLLECP",
#                 "ZCRUISEP","ZSWEEPSP","ZPOLITIP","ZMUSICP","ZGRANDPP","ZPRCHPHP","ZPRCHTVP","ZAUTOINP","ZCELLP",
#                 "ZHMDECOP","ZHOMEENP","ZKITCHEP","ZMOBGIFP"
#                 )
keep.list.final <- c('RESPONSE16','TOTAMT16','PRE2009_SALES','PRE2009_TRANSACTIONS','cum15QTY','cum15TOTAMT','cum15RESPONSE','TOTAL_MAIL_15',"TOTAL_MAIL_14","TOTAL_MAIL_13",
                     "POP_UN18","PHHWHITE","P_HHINCOM20_UP","MED_FAMINCOM","P_CAPITA_INCOM","P_IND_RETAILTRD","P_IND_FINALNCE","P_IND_PROFFES",
                     "MED_HOME","MED_DWELL_AGE","CUR_EST_MED_INC","STATE_INC_INDEX","CENSUS_FACT1","ECHVINX","ESTHMVL","ZHMDECOP",
                     "YEAR_BLT","salepertrans","salepercamp","ZIP4",'CHANNEL_ACQUISITION',"MEDIANAGE","MED_INC",
                     "FIPSCNTY","MCD_CCD","CUR_ST_EST_FAM_INC","CENSUS_SEG1","CENSUS_SEG2","MARRIED","ETHNIC_DETAIL",
                     "E_TECH","ADD_TYPE","DUS","HH_DMR","OCCUPATION_GROUP","AD_WEB","LOAN_AMT","M_HH_LEVEL","ZKITCHEP",
                     "HOMEOWNR_flag","COMPUTER_ELECTRONIC_flag","SPORTS_RELATED_flag","HH_MULTI_flag","ZGOURMET_flag","ZHITECH_flag","ZONLINE_flag",
                     "ZBOATS_flag","ZGOLFERS_flag")
length(keep.list.final)
df <- mydata[ , keep.list.final]
str(df[,1:58])
as.matrix(names(df))
#xtabs(~ZIP9_Supercode + ZHOMEENP, data = mydata)
############################################
table(mydata.save$ZIP4,mydata.save$RESPONSE16)
###############################################
## After several round - we decide to convert to num and factor respectively
# CONVERT to as.numeric() type
for (i in 35:35){
  df[,i] <- as.numeric(df[,i]) 
  #print(table(df.cat$RESPONSE16,df.cat[,i]))
  print(sum(sapply(df[,i]  , function(x) sum(is.na(x)))))
}

# U is missing value
for (i in 36:58){
  print(table(df$RESPONSE16,df[,i]))
  df[,i] <- ifelse(df[,i]=="U"|df[,i]=="", NA,df[,i])
}
for (i in 36:58){
  df[,i] <- as.factor(df[,i])  
  print(table(df$RESPONSE16,df[,i]))
}
str(df)

####### CHECK NA After conversion  #####
sapply(df, function(x) sum(is.na(x))) #check NA is 0
drop.final <- c("LOAN_AMT","ZIP4")
df$ANY_MAIL_16 <- mydata$ANY_MAIL_16
df.clean <-df[, !(names(df) %in% drop.final)]; 
sapply(df.clean, function(x) sum(is.na(x))) #check NA is 0
df.clean.final <- na.omit(df.clean)
dim(df.clean.final)
df.train <- df.clean.final %>% subset(ANY_MAIL_16==1)
dim(df.train)
df.test <- df.clean.final %>% subset(ANY_MAIL_16==0)
dim(df.test)
str(df.test)
### Save R object:#####
saveRDS(df.train,file='./df.train.Rdata');# there are 9918 records
saveRDS(df.test,file='./df.test.Rdata');# there are 10060 records


mydf<- readRDS('./df.train.Rdata')

##### LOOK at CORRELATIONS ####3
df.noNA <- na.omit(df)
df.noNA.numeric <- df.noNA[sapply(df.noNA, is.numeric)]
as.matrix(names(df.noNA.numeric))
df.corrl <- cor(df.noNA.numeric)
as.matrix(df.corrl[,c('RESPONSE16')] ) #filter out only correlation with Wins
corrplot(df.corrl,method="number",tl.cex = 0.5) 

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(df[2:6], upper.panel =panel.smooth, lower.panel= panel.cor)
pairs(~ df$RESPONSE16 + df$SUM_MAIL_16 + (df$DM_GIFTS) + (df$DM_BOOKS)+(df$EMAIL_RECEPTIVE), upper.panel =panel.smooth, lower.panel= panel.cor)


gg01 <- ggplot(data =df, aes(x = df$ZIP9_Supercode, y = df$cum15TOTAMT)) + 
  geom_point(aes(color = df$RESPONSE16),size = 3)+ ggtitle("cum15TOTAMT vs zip9 color by RESP16")

num_var <-names(df.noNA.numeric)
length(num_var)
length(names(df))
cat_var <- keep.list.1[!(names(df) %in% num_var)]
as.matrix(cat_var)
length(cat_var)
df.cat <- df[ , cat_var]
dim(df.cat)
df.cat$RESPONSE16<- df$RESPONSE16
table(df.cat$RESPONSE16,df.cat$HOMEOWNR)
# look at categorical data
for (i in 81:98){
  print(names(df.cat)[i])
  print(table(df.cat$RESPONSE16,df.cat[,i]))
  #plot(mydata$RESPONSE16,mydata[,i],main=names(mydata)[i])
}

df.cat$HOMEOWNR_flag <- ifelse(df.cat$HOMEOWNR==""|df.cat$HOMEOWNR=="U", 0,1)
addmargins(table(df.cat$RESPONSE16,df.cat$HOMEOWNR_flag))
boxplot(df.cat$RESPONSE16,as.numeric(df.cat$MCD_CCD),main="MCD_CCD")

# look at particular cat that can be converted to num ###
a=df.cat %>% subset(RESPONSE16==0)
b=df.cat %>% subset(RESPONSE16==1)


for (i in 12:14){
  par(mfrow=c(1,2))
  boxplot(as.numeric(a[,i]),main=names(a)[i])
  boxplot(as.numeric(b[,i]),main=names(b)[i])
  par(mfrow=c(1,1))
}

## CONVERT variables ###
str(df.cat)
# CONVERT to as.numeric() type
for (i in 78:98){
  df.cat[,i] <- as.numeric(df.cat[,i]) 
  #print(table(df.cat$RESPONSE16,df.cat[,i]))
  print(sum(sapply(df.cat[,i]  , function(x) sum(is.na(x)))))
}
# look at df.cat.num correlation to response16: 
df.cat.num <- na.omit(df.cat)
df.cat.num <- df.cat.num[sapply(df.cat.num, is.numeric)]
as.matrix(names(df.cat.num))
df.cat.num.corrl <- cor(df.cat.num)
as.matrix(sort(df.cat.num.corrl[,c('RESPONSE16')],decreasing = TRUE) ) #filter out only correlation with Wins
corrplot(df.cat.num.corrl,method="number",tl.cex = 0.5) 
#
#for (i in 1:length(as.matrix(names(df.cat.num)))){
i=53
  par(mfrow=c(2,4))
  hist(df.cat.num[,i], col = "red", xlab = names(df.cat.num)[i], main =paste(names(df.cat.num)[i],"hist"))
  hist(df.cat.num[,i+1], col = "green", xlab = names(df.cat.num)[i+1], main = paste(names(df.cat.num)[i+1],"hist"))
  hist(df.cat.num[,i+2], col = "blue",xlab = names(df.cat.num)[i+2], main = paste(names(df.cat.num)[i+2],"hist"))
  hist(df.cat.num[,i+3], col = "grey", xlab = names(df.cat.num)[i+3], main = paste(names(df.cat.num)[i+3],"hist"))
  boxplot(df.cat.num[,i], col = "red", main =paste(names(df.cat.num)[i],"boxplot"))
  boxplot(df.cat.num[,i+1], col = "green", main =paste(names(df.cat.num)[i+1],"hist"))
  boxplot(df.cat.num[,i+2], col = "blue", main =paste(names(df.cat.num)[i+2],"boxplot"))
  boxplot(df.cat.num[,i+3], col = "grey", main =paste(names(df.cat.num)[i+3],"boxplot"))
  par(mfrow=c(1,1))
#}
  par(mfrow=c(2,4))
  hist(log(df.cat.num[,i]+1), col = "red", xlab = names(df.cat.num)[i], main =paste(names(df.cat.num)[i],"log hist"))
  hist(log(df.cat.num[,i+1]+1), col = "green", xlab = names(df.cat.num)[i+1], main = paste(names(df.cat.num)[i+1],"log hist"))
  hist(log(df.cat.num[,i+2]+1), col = "blue",xlab = names(df.cat.num)[i+2], main = paste(names(df.cat.num)[i+2],"log hist"))
  hist(log(df.cat.num[,i+3]+1), col = "grey", xlab = names(df.cat.num)[i+3], main = paste(names(df.cat.num)[i+3],"log hist"))
  boxplot(log(df.cat.num[,i]+1), col = "red", main =paste(names(df.cat.num)[i],"log boxplot"))
  boxplot(log(df.cat.num[,i+1]+1), col = "green", main =paste(names(df.cat.num)[i+1],"log boxplot"))
  boxplot(log(df.cat.num[,i+2]+1), col = "blue", main =paste(names(df.cat.num)[i+2],"log boxplot"))
  boxplot(log(df.cat.num[,i+3]+1), col = "grey", main =paste(names(df.cat.num)[i+3],"log boxplot"))
  par(mfrow=c(1,1))
#### Comments: hold on to convert to log for the categorical var ##
  
########
# CONVERT to as.factor() type
df.cat.save <- df.cat    ## SAVE a copy of df.cat
df.cat <- df.cat.save
# create flags for missing values
df.cat$COMPUTER_ELECTRONIC_flag <- ifelse(df.cat$COMPUTER_ELECTRONIC==""|df.cat$COMPUTER_ELECTRONIC=="U", 0,1)
df.cat$SPORTS_RELATED_flag <- ifelse(df.cat$SPORTS_RELATED==""|df.cat$SPORTS_RELATED=="U", 0,1)
df.cat$HOMEOWNR_flag <- ifelse(df.cat$HOMEOWNR==""|df.cat$HOMEOWNR=="U", 0,1)
df.cat$HH_MULTI_flag <- ifelse(df.cat$HH_MULTI==""|df.cat$HH_MULTI=="U", 0,1)

tail(df.cat)


# U is missing value
for (i in 30:30){
  #print(table(df.cat$RESPONSE16,df.cat[,i]))
  df.cat[,i] <- ifelse(df.cat[,i]=="U"|df.cat[,i]=="", NA,df.cat[,i])
}
for (i in 30:30){
  df.cat[,i] <- as.factor(df.cat[,i])  
  print(table(df.cat$RESPONSE16,df.cat[,i]))
}

str(df.cat)

############################################
## Drop some varaibles:
drop.num <- c("INC_SCS_AMT_V4","P_ASIAN","HH_HISPA","P_HHFAMI","POP_CHLDHHS","POP_ADPCHLDFAM","P_HHNOHISWHT",
               "P_FEMALE","P_FAMHHNOCHD","P_MARRY","P_HHINCOM35_39","P_HHINCOM40_44","P_HHINCOM45_49",
               "P_HHINCOM50_59","P_HHINCOM60_74","P_HHINCOM150_199","MED_NOFAMINCOM","P_IND_MANUFACT",
               "CENSUS_FACT4","NUMBADLT","ECHVPCT")
keep.cat <- c("ECHVINX","ESTHMVL","ZIP","ZHMDECOP","YEAR_BLT","CHANNEL_ACQUISITION","BUYER_STATUS","FIPSCNTY","MCD_CCD","CUR_ST_EST_FAM_INC",
              "CENSUS_SEG1","CENSUS_SEG2","GEOPIXELCODE","ADULT1_G","MARRIED","ETHNIC_DETAIL","E_TECH","ADD_TYPE","DUS","HH_DMR","OCCUPATION_GROUP",
              "PHONEMATCH","AD_WEB","SALES","TRANSTYP","LOAN_AMT","M_HH_LEVEL","ZGOURMET","ZCOMPUTR","ZKITCHEP",
              "ZHITECH","ZONLINE","HOMEOWNR_flag","COMPUTER_ELECTRONIC_flag","SPORTS_RELATED_flag","HH_MULTI_flag"
               )

dim(df)
dim(df.clean)
df.clean <-df[, num_var];
df.clean <-df.clean[, !(names(df.clean) %in% drop.num)];  #there are 30 numeric variables
dim(df.clean)
dim(df.cat)
df.cat.clean <-df.cat[,keep.cat];
dim(df.cat.clean)
df.clean.combine <- cbind(df.clean,df.cat.clean)
dim(df.clean.combine)


##################################################################################
################### Step 3 - EDA - Cleaning up of the data  #####################
#################################################################################
# clean up some chr variable conversion
str(df.clean.combine)
df.clean.combine$ZGOURMET_flag <- ifelse(df.clean.combine$ZGOURMET==""|df.clean.combine$ZGOURMET=="U", 0,1)
df.clean.combine$ZCOMPUTR_flag <- ifelse(df.clean.combine$ZCOMPUTR==""|df.clean.combine$ZCOMPUTR=="U", 0,1)
df.clean.combine$ZHITECH_flag <- ifelse(df.clean.combine$ZHITECH==""|df.clean.combine$ZHITECH=="U", 0,1)
df.clean.combine$ZONLINE_flag <- ifelse(df.clean.combine$ZONLINE==""|df.clean.combine$ZONLINE=="U", 0,1)
tail(df.clean.combine)
df.clean.combine.save <- df.clean.combine
#drop varible
drop <- c("ZGOURMET","ZCOMPUTR","ZHITECH","ZONLINE")
df.clean.combine <-df.clean.combine[, !(names(df.clean.combine) %in% drop)];

# U is missing value######
for (i in 58:59){
  print(table(df.clean.combine$RESPONSE16,df.clean.combine[,i]))
  #df.clean.combine[,i] <- ifelse(df.clean.combine[,i]=="U"|df.clean.combine[,i]=="", NA,df.cat[,i])
}
for (i in 58:5){
  df.clean.combine[,i] <- as.factor(df.clean.combine[,i])  
  print(table(df.clean.combine$RESPONSE16,df.clean.combine[,i]))
}

##### Drop NA  ####
df.clean.combine <- na.omit(df.clean.combine)

###################################################
### Save a copy of ready df ####
#saveRDS(df.clean.combine,file='./df.clean.combine.Rdata');# there are 9133 records
#mydf<- readRDS('./df.clean.combine.Rdata')
#dim(mydf) #9133 records

#########################################################################
######## Step 4 build models #############
##########################################################################
#mydf<- readRDS('./df.clean.combine.Rdata')
mydf<- readRDS('./df.train.Rdata')

#mydf$salepertrans <- ifelse(mydf$PRE2009_TRANSACTIONS==0,0,mydf$PRE2009_SALES/mydf$PRE2009_TRANSACTIONS)
#mydf$salepercamp <- ifelse(mydf$TOTAL_MAIL_15==0,0,mydf$cum15TOTAMT/mydf$TOTAL_MAIL_15)
sum(sapply(mydf, function(x) sum(is.na(x)))) #check NA is 0
summary(mydf)
dim(mydf)
plot(mydf$salepercamp,mydf$salepertrans)
print(paste(100*round(addmargins(table(mydf$RESPONSE16))[1]/addmargins(table(mydf$RESPONSE16))[3],4),"% RESPONSE16=0"))
print(paste(100*round(addmargins(table(mydf$RESPONSE16))[2]/addmargins(table(mydf$RESPONSE16))[3],4),"% RESPONSE16=1"))
#
mydf.noNA <- na.omit(mydf)
mydf.numeric <- mydf.noNA[sapply(mydf.noNA, is.numeric)]
mydf.corrl <- cor(mydf.numeric)
as.matrix(sort(mydf.corrl[,c('RESPONSE16')],decreasing = TRUE )) #filter out only correlation with Wins
#as.matrix(sort(mydf.corrl[,c('TOTAMT16')],decreasing = TRUE )) #filter out only correlation with Wins
corrplot(mydf.corrl,method="number",tl.cex = 0.5) 



### pick variables  ####
drop.list <- c("TOTAMT16","ANY_MAIL_16")
#cor(mydf$TOTAL_MAIL_14,mydf$TOTAL_MAIL_13)

mydf.1 <-mydf[, !(names(mydf) %in% drop.list)];
as.matrix(names(mydf.1))
str(mydf.1)

mydf.1$HOMEOWNR_flag <- as.factor(mydf.1$HOMEOWNR_flag)
mydf.1$COMPUTER_ELECTRONIC_flag <- as.factor(mydf.1$COMPUTER_ELECTRONIC_flag)
mydf.1$SPORTS_RELATED_flag <- as.factor(mydf.1$SPORTS_RELATED_flag)
mydf.1$HH_MULTI_flag <- as.factor(mydf.1$HH_MULTI_flag)
mydf.1$ZGOURMET_flag <- as.factor(mydf.1$ZGOURMET_flag)
mydf.1$ZCOMPUTR_flag <- as.factor(mydf.1$ZCOMPUTR_flag)
mydf.1$ZHITECH_flag <- as.factor(mydf.1$ZHITECH_flag)
mydf.1$ZONLINE_flag <- as.factor(mydf.1$ZONLINE_flag)
mydf.1$PRE2009_TRANSACTIONS_log <- log(mydf.1$PRE2009_TRANSACTIONS+1)


# mean of target variable is greater than var: POISSON IF EQUAL, NEGATIVE BINOMIAL IF VAR >MEAN
table(mydf$RESPONSE16)[2]/(dim(mydf)[1]) #10%
table(mydf$RESPONSE16)[1]/(dim(mydf)[1]) 
mean(mydf$RESPONSE16)
sd(mydf$RESPONSE16)^2

####################################################################
######## Step 4.a Random Forest model ###############################
##################################################################
### putting '.' means includes all the variables
# NOTE: RF can not handle categorical predictors with more than 53 categories. So drop some
str(mydf.1)
table(mydf.1$AD_WEB)
drop.list.2 <- c("ETHNIC_DETAIL","M_HH_LEVEL")
#cor(mydf$TOTAL_MAIL_14,mydf$TOTAL_MAIL_13)
mydf.2 <-mydf.1[, !(names(mydf.1) %in% drop.list.2)];
#mydf.2$MCD_CCD <- as.numeric(mydf.2$MCD_CCD)
str(mydf.2)
start_time <- Sys.time()
rf1 <- randomForest(RESPONSE16 ~ .
                    ,data=mydf.2,importance=TRUE,ntree=100)
end_time <- Sys.time()
run_time <- end_time-start_time
print(run_time)
summary(rf1)



getTree(rf1,1,labelVar=TRUE)
##?getTree
print(rf1)
plot(rf1)

round(importance(rf1), 2)
randomForest::importance(rf1)
#importance(rf1)
varImpPlot(rf1)
##how much each variable improves the prediction of its tree##
##compared to the exact same tree without that variable##  ----???

#get the prediction probabilities##
rf1p  <- predict(rf1, newdata=mydf.2,type="response")
#?hoslem.test()
hoslem.test(mydf.2$RESPONSE16, rf1p)

head(rf1p)
hist(rf1p)
rf1pdf <- as.data.frame(rf1p)
rf1.pred = prediction(rf1p, mydf.2$RESPONSE16)
rf1.perf = performance(rf1.pred,"tpr","fpr")
plot(rf1.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
auc(mydf.2$RESPONSE16,rf1p)

#Make a confusion matrix and compute accuracy
##confusion.matrix <- table(rf1p, subdat3$RESPONSE16)
##confusion.matrix
#rf1p  <- predict(rf1, newdata=mydf.2,type="response")
rf1pround <- round(rf1p,0)
cm <- xtabs(~RESPONSE16 + rf1pround, data = mydf.2)
cm
accuracy <- sum(diag(cm))/sum(cm)
accuracy  #0.9943537

data_alldf <- cbind(mydf.2,rf1pdf)
str(data_alldf)
head(data_alldf)
mean(data_alldf$salepertrans)
mean(mydf.2$salepertrans)
mean(data_alldf$salepertrans [data_alldf$rf1p>0.80])
target_customer <- data_alldf %>% subset (rf1p>0.80)
dim(target_customer)
###if the cutoff is 0.8 ###

rf2 <- randomForest(RESPONSE16 ~ salepertrans+PRE2009_TRANSACTIONS+cum15TOTAMT+TOTAL_MAIL_13+PRE2009_SALES
                      +TOTAL_MAIL_15+MED_INC+ESTHMVL+P_HHINCOM20_UP+P_CAPITA_INCOM+ECHVINX
                      +P_IND_PROFFES+MED_HOME+TOTAL_MAIL_14+salepercamp+PHHWHITE+CENSUS_FACT1+cum15QTY
                      +P_IND_FINALNCE+MED_DWELL_AGE+CUR_EST_MED_INC+MED_FAMINCOM+cum15RESPONSE+MEDIANAGE+STATE_INC_INDEX
                      +P_IND_RETAILTRD+AD_WEB+POP_UN18+CENSUS_SEG1+MCD_CCD+CENSUS_SEG2
                    
                    ,data=mydf.2,importance=TRUE,ntree=100) #+EXAGE 
summary(rf2)
plot(rf2)
#round(importance(rf1), 2)
randomForest::importance(rf2)
#importance(rf1)
varImpPlot(rf2)
##how much each variable improves the prediction of its tree##
##compared to the exact same tree without that variable##  ----???

#get the prediction probabilities##
rf2p  <- predict(rf2, newdata=mydf.2,type="response")
?hoslem.test()
hoslem.test(mydf.2$RESPONSE16, rf2p)
head(rf2p)
hist(rf2p)
rf2pdf <- as.data.frame(rf2p)
rf2.pred = prediction(rf2p, mydf.2$RESPONSE16)
rf2.perf = performance(rf2.pred,"tpr","fpr")
plot(rf2.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
auc(mydf.2$RESPONSE16,rf2p)

#Make a confusion matrix and compute accuracy
##confusion.matrix <- table(rf1p, subdat3$RESPONSE16)
##confusion.matrix
#rf1p  <- predict(rf1, newdata=mydf.2,type="response")
rf2pround <- round(rf2p,0)
cm <- xtabs(~RESPONSE16 + rf2pround, data = mydf.2)
cm
accuracy <- sum(diag(cm))/sum(cm)
accuracy  #0.8883848

##########
rf3 <- randomForest(RESPONSE16 ~ salepertrans+PRE2009_TRANSACTIONS+ZKITCHEP+OCCUPATION_GROUP+YEAR_BLT+ESTHMVL
                      +E_TECH+ECHVINX+salepercamp+cum15TOTAMT+CENSUS_SEG2+AD_WEB+P_IND_RETAILTRD+P_IND_FINALNCE
                    +cum15QTY+POP_UN18+PHHWHITE+MED_DWELL_AGE+P_IND_PROFFES+MEDIANAGE+TOTAL_MAIL_15
                    +cum15RESPONSE+TOTAL_MAIL_13+MARRIED+TOTAL_MAIL_14+MED_HOME+P_CAPITA_INCOM
                    +MED_INC+CUR_EST_MED_INC+P_HHINCOM20_UP
                    
                    ,data=mydf.2,importance=TRUE,ntree=100) #+EXAGE 
summary(rf3)
plot(rf3)
#round(importance(rf1), 2)
randomForest::importance(rf3)
#importance(rf1)
varImpPlot(rf3)
##how much each variable improves the prediction of its tree##
##compared to the exact same tree without that variable##  ----???

#get the prediction probabilities##
rf3p  <- predict(rf3, newdata=mydf.3,type="response")
##GOF tets - Hosmer & lemeshow
#?hoslem.test()  
hoslem.test(mydf.3$RESPONSE16, rf3p)
head(rf3p)
hist(rf3p)
rf3pdf <- as.data.frame(rf3p)
rf3.pred = prediction(rf3p, mydf.2$RESPONSE16)
rf3.perf = performance(rf3.pred,"tpr","fpr")
plot(rf3.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
auc(mydf.2$RESPONSE16,rf3p)

#Make a confusion matrix and compute accuracy
##confusion.matrix <- table(rf1p, subdat3$RESPONSE16)
##confusion.matrix
rf3pround <- round(rf3p,0)
cm <- xtabs(~RESPONSE16 + rf3pround, data = mydf.2)
cm
accuracy <- sum(diag(cm))/sum(cm)
accuracy  #0.8883848




#################################################
#### STEP 4.b - Logistic Regression Model 
#############################################
# interpretation: https://www.displayr.com/how-to-interpret-logistic-regression-coefficients/
mydf.3 <- mydf.1
mydf.3$PRE2009_SALES_log <- log(mydf.3$PRE2009_SALES+1)
drop.list.3 <- c("ETHNIC_DETAIL","PRE2009_SALES")
#cor(mydf$TOTAL_MAIL_14,mydf$TOTAL_MAIL_13)
mydf.3 <-mydf.3[, !(names(mydf.1) %in% drop.list.3)];

# full train set
upper.glm <- glm(mydf.3$RESPONSE16~., data=mydf.3,family="binomial")
lower.glm <- glm(mydf.3$RESPONSE16~1, data=mydf.3,family="binomial")
#
start_time <- Sys.time()
forward.glm <- stepAIC(object=lower.glm,scope=list(upper=formula(upper.glm),lower=~1), direction=c('forward'));
end_time <- Sys.time()
run_time <- end_time-start_time
print(run_time)
summary(forward.glm)   
# deviante
pvalue <- 1 - pchisq((6670-6161.8), df=(9917-9901))
pvalue # pvalue is very small

as.matrix(sort(vif(forward.glm),decreasing=TRUE))
formula(forward.glm)

# performance:
pred.forward.glm <- predict(forward.glm,data=mydf.1, type="response")
head(pred.forward.glm)
str(pred.forward.glm)

cm <- xtabs(~RESPONSE16 + round(pred.forward.glm,0), data = mydf.1)
cm
accuracy <- sum(diag(cm))/sum(cm)
accuracy   #0.8957451

###Another GOF tets - Hosmer & lemeshow ###
hoslem.test(mydf.1$RESPONSE16, fitted(forward.glm))

### ROCR curve
ROCRpred <- prediction(pred.forward.glm,mydf.1$RESPONSE16)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
abline(a=0,b=1)
auc(mydf.1$RESPONSE16,pred.forward.glm)  #0.8507

hist(pred.forward.glm)
pred.forward.glm.df <- as.data.frame(pred.forward.glm)
data_all <- cbind(mydf.1,pred.forward.glm)
str(data_all)
sapply(data_all  , function(x) sum(is.na(x)))
mean(data_all$salepertrans)
mean(data_all$salepertrans [data_all$pred.forward.glm>0.05])
###if the cutoff is 0.8 ###


### my code on ROC
mydf.1$forwardPrediction <- predict(forward.glm, type = "response")
optCutoff.forward <- optimalCutoff(mydf.1$RESPONSE16,mydf.1$forwardPrediction)[1]
misClassError.forward <- misClassError(mydf.1$RESPONSE16,mydf.1$forwardPrediction,threshold = optCutoff.forward)
plotROC(mydf.1$RESPONSE16,mydf.1$forwardPrediction)
print(-2*logLik(forward.glm, REML = TRUE))
Concordance(mydf.1$RESPONSE16,mydf.1$forwardPrediction)
head(mydf.1)
#conf.forward <- confusionMatrix(mydf.1$RESPONSE16,mydf.1$forwardPrediction,threshold = optCutoff.forward)
#conf.forward[3,1] <- ( conf.forward[1,1]/(conf.forward[1,1]+conf.forward[2,1] ))#false detection rate
#conf.stepwise[3,2] <- (conf.forward[2,2]/(conf.forward[1,2]+conf.forward[2,2] ))#truth detection rate
#####################
###########s
# stepwise
str(mydf.3)
# full train set
upper.glm <- glm(mydf.1$RESPONSE16~., data=mydf.3,family="binomial")
lower.glm <- glm(mydf.1$RESPONSE16~1, data=mydf.3,family="binomial")

base.glm <- glm(mydf.1$RESPONSE16 ~PRE2009_SALES_log+PRE2009_TRANSACTIONS+salepertrans+salepercamp
                 + cum15TOTAMT+cum15QTY+TOTAL_MAIL_13 +TOTAL_MAIL_15+MED_INC +ESTHMVL
                +MED_FAMINCOM +ECHVINX +P_CAPITA_INCOM +P_IND_PROFFES+MED_HOME+PHHWHITE+
                  +OCCUPATION_GROUP+MARRIED+ZKITCHEP+YEAR_BLT+E_TECH+CENSUS_SEG2+
                +AD_WEB +P_IND_RETAILTRD +CUR_EST_MED_INC+P_IND_FINALNCE+M_HH_LEVEL+MED_DWELL_AGE+MEDIANAGE
                   ,data=mydf.3,family = binomial()); # 

summary(base.glm)
start_time <- Sys.time()
stepwise.glm <- stepAIC(object=base.glm,scope=list(upper=formula(upper.glm),lower=~1), direction=c('both'));
end_time <- Sys.time()
run_time <- end_time-start_time
print(run_time)
summary(stepwise.glm)
pvalue <- 1 - pchisq((6670-6143), df=(9917-9897))
pvalue # pvalue is very small

as.matrix(sort(vif(stepwise.glm),decreasing=TRUE))
formula(stepwise.glm)
as.matrix(stepwise.glm$coefficients)

###Another GOF tets - Hosmer & lemeshow ###
hoslem.test(mydf.3$RESPONSE16, fitted(stepwise.glm))

### performance:
pred.stepwise.glm <- predict(stepwise.glm,data=mydf.3, type="response")
cm <- xtabs(~RESPONSE16 + round(pred.stepwise.glm,0), data = mydf.3)
cm
accuracy <- sum(diag(cm))/sum(cm)
accuracy  #0.8942327
### ROCR curve
ROCRpred <- prediction(pred.stepwise.glm,mydf.3$RESPONSE16)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
abline(a=0,b=1)
auc(mydf.3$RESPONSE16,pred.stepwise.glm)  #0.8441

hist(pred.stepwise.glm)

mydf.3$stepwisePrediction <- predict(stepwise.glm, type = "response")
optCutoff.stepwise <- optimalCutoff(mydf.3$RESPONSE16,mydf.3$stepwisePrediction)[1]
misClassError.stepwise <- misClassError(mydf.3$RESPONSE16,mydf.3$stepwisePrediction,threshold = optCutoff.stepwise)
plotROC(mydf.3$RESPONSE16,mydf.3$stepwisePrediction)

####################################################################
##################### Navie Bayesian #############################
##################################################################
install.packages("ElemStatLearn")
install.packages("klaR")
library(klaR)
table(mydf.1$MARRIED)
# apply NB classifier
mydf.3$RESPONSE16 <- as.factor(mydf.3$RESPONSE16)
mydf.2$RESPONSE16<- as.factor(mydf.2$RESPONSE16)
nb.res <- NaiveBayes(RESPONSE16~ ., data=mydf.2)
# predict on in-sample
nb.pred <- predict(nb.res, data=mydf.2)
cm <- xtabs(~RESPONSE16 + nb.pred$class, data = mydf.2)
cm
accuracy <- sum(diag(cm))/sum(cm)
accuracy  

### my code on ROC
temp <- nb.pred$posterior
plotROC(mydf.2$RESPONSE16,temp[,2])
auc(mydf.1$RESPONSE16,temp[,2])  #0.6718


####################################################################
#####################  SVM Linear #############################
##################################################################
install.packages("e1071", dep = TRUE,type="source")
library("e1071")
library(caret)
str(mydf.3)
keep.mydf4 <- c("RESPONSE16","PRE2009_SALES","PRE2009_TRANSACTIONS","salepertrans","OCCUPATION_GROUP",
                "ZKITCHEP","YEAR_BLT","ESTHMVL"
                )
mydf.4 <- mydf.2[,keep.mydf4]
#mydf.4$RESPONSE16 <- as.factor(mydf.3$RESPONSE16)
####### repeated cross validation, 10-fold CV, 3 times repeated ##
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
start_time <- Sys.time()
set.seed(3233)
svm_Linear <- train(RESPONSE16 ~., data = mydf.4, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
end_time <- Sys.time()
run_time <- end_time-start_time
print(run_time)

svm_Linear

####################### test the model on the test set #####################
test_pred <- predict(svm_Linear, newdata = mydf.4)
head(test_pred)
hoslem.test(mydf.4$RESPONSE16, test_pred)  
summary(test_pred)
test_pred <- as.data.frame(test_pred)
head(test_pred)

optCutoff.SVM <- optimalCutoff(mydf.4$RESPONSE16,test_pred)[1]
t <- quantile(test_pred$test_pred,0.99)
test_pred$test_pred <- ifelse(test_pred>t,1,0)
#cm <- confusionMatrix(test_pred$test_pred, mydf.3$RESPONSE16) ## NOT working why?
cm <- xtabs(~RESPONSE16 +test_pred$test_pred , data = mydf.3)
cm
accuracy <- sum(diag(cm))/sum(cm)
accuracy  #88.84%

#ROC
plotROC(mydf.4$RESPONSE16,test_pred$test_pred)
auc(mydf.1$RESPONSE16,temp[,2])  #0.6718
hist(test_pred)

####### tune the linear model for different values of C  ##########################
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5,7,10))

start_time=Sys.time()
set.seed(3233)
svm_Linear_Grid <- train(RESPONSE16 ~., data = mydf.3, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)
end_time <- Sys.time()
run_time <- end_time-start_time
print(run_time)

svm_Linear_Grid
plot(svm_Linear_Grid)

test_pred_grid <- predict(svm_Linear_Grid, newdata = mydf.2)
test_pred_grid
confusionMatrix(test_pred_grid, mydf.2$RESPONSE16 )

#######################################################################################
########## Non linear classification with RBF kernel #################################
######################################################################################

start_time=Sys.time()
set.seed(3233)
svm_Radial <- train(RESPONSE16 ~., data = mydf.4, method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
end_time <- Sys.time()
run_time <- end_time-start_time
print(run_time)
      
svm_Radial
plot(svm_Radial)

test_pred_Radial <- predict(svm_Radial, newdata = mydf.4)
confusionMatrix(test_pred_Radial, mydf.2$RESPONSE16)

svmnonlinearpred <- as.data.frame(test_pred_Radial)
colnames(svmnonlinearpred) <- "SVMNonLinPred"
####################################################

grid_radial <- expand.grid(sigma = c(0,0.01, 0.02, 0.025, 0.03, 0.04,
                                     0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
                           C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75,
                                 1, 1.5, 2,5))
set.seed(3233)
svm_Radial_Grid <- train(RESPONSE16 ~., data = mydf.2, method = "svmRadial",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid_radial,
                         tuneLength = 10)

svm_Radial_Grid
plot(svm_Radial_Grid)

test_pred_Radial_Grid <- predict(svm_Radial_Grid, newdata = mydf.2)
confusionMatrix(test_pred_Radial_Grid, subdat3$RESPONSE16)


#######
## Predict the non-campagin 16 customers #
#####
testdf<- readRDS('./df.test.Rdata')
str(testdf)
addmargins(table(testdf$RESPONSE16))
drop.test <- c("TOTAMT16","PRE2009_SALES","ANY_MAIL_16","ETHNIC_DETAIL","M_HH_LEVEL")
testdf.1 <-testdf[, !(names(testdf) %in% drop.test)];
str(testdf.1)
rf1Testp  <- predict(rf1, newdata=testdf.1,type="response")
rf1Testpround <- round(rf1Testp,0)
rf1Testpdf <- as.data.frame(rf1Testp)

hist(rf1Testp)
cm <- xtabs(~RESPONSE16 + rf1Testpround, data = testdf.1)
cm
accuracy <- sum(diag(cm))/sum(cm)
accuracy  #0.9597416

data_Testdf <- cbind(testdf.1,rf1Testpdf)
str(data_Testdf)
head(data_Testdf)
mean(data_Testdf$salepertrans)
mean(mydf.2$salepertrans)
mean(data_Testdf$salepertrans [data_Testdf$rf1Testp>=0.0])
target_customer <- data_Testdf %>% subset(rf1Testp>=0.0)
dim(target_customer)
dim(data_Testdf)

###########################################
## some plots
str(mydf.2)
count <- table(mydf.2$RESPONSE16,mydf.2[,"CHANNEL_ACQUISITION"])
count2 <- table(mydf.2$RESPONSE16,mydf.2[,"DUS"])
dim(count)
par(mfrow=c(1,2))
barplot(count,col=c("navy","yellow"), main = paste("RESPONSE16 by","CHANNEL_ACQUISITION"),xlab ="CHANNEL_ACQUISITION" ,
        ylab = "Frequency",beside=TRUE)#, ylim=c(0,50),legend=rownames(count)
barplot(count2,col=c("navy","yellow"), main = paste("RESPONSE16 by","DUS"),xlab ="DUS" ,
        ylab = "Frequency",beside=TRUE,legend=rownames(count))#, ylim=c(0,50)
par(mfrow=c(1,1))

count <- table(df$RESPONSE16,df[,36])
count2 <- table(df$RESPONSE16,df[,37])
dim(count)
par(mfrow=c(1,2))
barplot(count,col=c("navy","yellow"), main = paste("RESPONSE16 by","CHANNEL_ACQUISITION"),xlab ="CHANNEL_ACQUISITION" ,
        ylab = "Frequency",beside=TRUE)#, ylim=c(0,50),legend=rownames(count)
barplot(count2,col=c("navy","yellow"), main = paste("RESPONSE16 by","DUS"),xlab ="DUS" ,
        ylab = "Frequency",beside=TRUE,legend=rownames(count))#, ylim=c(0,50)
par(mfrow=c(1,1))

### Look at zip code
t <- mydata %>% subset(RESPONSE16>0)
t1 <- table(t$ZIP9_Supercode)
plot(t1)
head(t1[1])
t2 <- t1 %>% subset(t1>3)
as.matrix(table(df$ZIP4,df$RESPONSE16))
count <- table(df$ZIP4,df$RESPONSE16)

t3 <- mydata %>% subset(ZIP==60093)
table(t3$RESPONSE16,t3$ZIP)
barplot(table(mydata$RESPONSE16,mydata$ZIP))
t4 <- mydata %>% subset(ANY_MAIL_16>0)
barplot(table(t4$RESPONSE16,t4$ZIP))

