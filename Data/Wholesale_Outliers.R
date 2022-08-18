###Detecting outliers using PCA and DBSCAN
library(tidyverse)

## IMPORT DATA
file_path<- file.path("D:/Lien - Langara/Year 2/Past semester(S&J)/DANA4840 materials/Lecture9/Wholesale-customers-data.csv")
customers <- read_csv(file_path)
View(customers)

#PCA method
pca <- princomp(customers[,-c(1,2)],scores = T)
summary(pca)
#Importance of components:
#  Comp.1       Comp.2       Comp.3       Comp.4       Comp.5       Comp.6
#Standard deviation     1.283047e+04 1.204664e+04 5.008277e+03 3.970892e+03 2.319592e+03 1.482779e+03
#Proportion of Variance 4.596136e-01 4.051723e-01 7.003008e-02 4.402344e-02 1.502212e-02 6.138475e-03
#Cumulative Proportion  4.596136e-01 8.647859e-01 9.348160e-01 9.788394e-01 9.938615e-01 1.000000e+00

#The first 2 component can explain 86.4% of total variance

library(dplyr)
library(ggplot2)

pca.scores=as.data.frame(pca$scores)
pca.scores %>% select(.,Comp.1,Comp.2) %>% qplot(Comp.1,Comp.2,data=.)

#DBSCAN method
# Scaled distance matrix
dist.Mat=dist(scale(customers[,-c(1,2)]),method = "euclidean")

hist=hist(dist.Mat,breaks = 100,col = "red",main = "Histogram of Distance Matrix",xlab="Distance")
h = head(hist$breaks,n=15) 
#[1] 0.0 0.2 0.4 0.6 0.8 1.0 1.2 1.4 1.6 1.8 2.0 2.2 2.4 2.6 2.8
median(h) #1.4
mean(h) #1.4

#1.4 is the middle point
#The peak occurs at around 1.5 value.It means, as the inter point distance is increased, 
#more and more data points are becoming neighbors till the distance reaches 1.5 and afterward, the density starts decreasing

head(hist$counts,n=15) # 95 1115 2920 4617 5583 6810 7371 7300 7041 6147 5566 4998 4782 4033 3614

#The next thing is to decide MinPts value. To get an approximate MinPts value the following codes are run.
mat=as.matrix(dist.Mat)
View(mat)
dim(mat) #440 440

mat.long=reshape2::melt(mat)# takes wide-format data and melts it into long-format data

#melt has assumed that all columns with numeric values are variables with values
mat.long$flag=ifelse(mat.long$value>0.0 & mat.long$value<=1.5,1,0)

#> mat.long
#Var1 Var2      value flag
#1      1    1  0.0000000    0
#2      2    1  0.6201520    1
#3      3    1  2.4124496    0
#4      4    1  1.8159083    0
#5      5    1  1.8515740    0
#...........
#249  249    1  0.9886156    1
#250  250    1  1.7217094    0
#[ reached 'max' / getOption("max.print") -- omitted 193350 rows ]


library(dplyr)
DF = summarise(group_by(mat.long,Var1),count=sum(flag))

#DF 
# A tibble: 440 x 2
#    Var1 count
#    <int> <dbl>
# 1     1   183
# 2     2   165
# 3     3     2
# 4     4   204
# 5     5    30
# 6     6   258
# 7     7   267
# 8     8   239
# 9     9   258
# 10    10    71
# ... with 430 more rows

hist(DF$count,col="red", main="Histogram of # of points within 1.5", 
     xlab="# of points within 1.5")

#As per the histogram, from 150 onwards there is, a kind of, steady increase and 
#thereafter decrease in the frequencies. Thus, in this case, MinPts value can be fixed at 150.

library(dbscan)
library(fpc)
dbscan.fit=dbscan(customers[,-c(1,2)],eps = 1.5, MinPts = 150,scale = T,method = "raw")
dbscan.fit #72 outliers

#        0   1
#border 72 127
#seed    0 241
#total  72 368

pca.scores %>% select(.,Comp.1,Comp.2) %>%
  qplot(Comp.1,Comp.2,data=.)+geom_point(col=dbscan.fit$cluster+2)

#The red points are outliers in the plot given below
#A few red data points are lying within the green data points and that is due to 
#third component which is not shown. Those red points are outliers in vertical direction 
#and hence their projections are lying within the green points.

