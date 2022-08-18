library(moments)
library(tidyverse)
library(psych)
setwd("~/Langara/Sem 4/Dana/project")
df_org<-read.csv("data.csv")
dim(df_org)
#214 33

# checking column-wise null percentage
Null_Cnt <- sapply(df_org, function(x){ sum(is.na(x))})
Null_percnt <- sapply(df_org, function(x){ round((sum(is.na(x))/length(x))*100,2) })
Null_Smry <- cbind(Null_Cnt,Null_percnt)
Null_Smry


# adding a column to check row-wise null percentage
df_org$missingperc <- c(round((rowSums(is.na(df_org))/dim(df_org)[2])*100, 2))
View(df_org)

# removing rows with more than 30% null values & removing columns with more than 50% null values
# every row is representing a country's feature if a country is missing more than 30% of its features 
# and we will impute the missing data and do analysis ; this will create bias
# same goes in column wise more  than 50% data is missing then that variable can be excluded from the analysis
df_cleaned<- df_org[which(round(rowSums(is.na(df_org))/dim(df_org)[2]*100)<30),
                    which(round(colSums(is.na(df_org))/dim(df_org)[1]*100)<50)]

dim(df_cleaned)
#180 29
View(df_cleaned)

#Drop column - year and year code
df_cleaned <- df_cleaned[,-c(1,2)]
dim(df_cleaned)#180  27
View(df_cleaned)

################################################################################################

#inaccuracy - none (precision of the numbers)
df_cleaned <- data.frame(lapply(df_cleaned, function(y) if(is.numeric(y)) round(y, 2) else y))
View(df_cleaned)
#before imputation summary (min Max std dev)
nums <- vapply(df_cleaned, is.numeric, FUN.VALUE = logical(1))

#imputation (Method?)

#Missing values imputation
# The following input arguments are used in mice for multiple imputation:
# m: Number of imputed datasets (default is m=5)
# seed: Random seed for reproducible results
# method: method to use to impute missing values (default method for imputation of numeric variables is PMM)

df_cleaned_final = df_cleaned[,-c(1,2,27)] # final data for data missing imputation

View(df_cleaned_final)

urlPackage <- "https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-12.tar.gz"
install.packages(urlPackage, repos=NULL, type="source") 
library(missForest)

missforest <- missForest(df_cleaned_final, ntree = 100, replace = TRUE, verbose = TRUE,mtry = 5, maxiter = 3)
missforest$OOBerror
#NRMSE 
#1.001438
View(missforest)

df_new = missforest$ximp

View(df_new)
sum(is.na(df_new)) #0
 
write.csv(df_new,'')