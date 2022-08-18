

df_final <- read.csv("C:/3_DANA 4840/Project/df_final_v2_clean.csv")
View(df_final)
df_final <- df_final[,-1]
str(df_final)#all numeric
sum(is.na(df_final))#re-check NA=0


# CHECK NOISE USING DBSCAN ------------------------------------------------
# Scaled distance matrix
dist.Mat=dist(scale(df_final),method = "euclidean")

hist=hist(dist.Mat,breaks = 100,col = "red",main = "Histogram of Distance Matrix",xlab="Distance")
h = head(hist$breaks,n=15) 
h

# [1] 1.0 1.2 1.4 1.6 1.8 2.0 2.2 2.4 2.6 2.8 3.0 3.2 3.4 3.6 3.8
median(h) #2.4
mean(h) #2.4

#2.4 is the middle point


head(hist$counts,n=15) # [1]   4   8  20  28  66  76 120 190 227 282 332 368 424 426 467

#The next thing is to decide MinPts value. To get an approximate MinPts value the following codes are run.
mat=as.matrix(dist.Mat)
View(mat)
dim(mat) #180 180

mat.long=reshape2::melt(mat)# takes wide-format data and melts it into long-format data

#melt has assumed that all columns with numeric values are variables with values
mat.long$flag=ifelse(mat.long$value>0.0 & mat.long$value<=2.5,1,0)

library(dplyr)
DF = summarise(group_by(mat.long,Var1),count=sum(flag))

hist(DF$count,col="red", main="Histogram of # of points within 2.5", 
     xlab="# of points within 2.5")

#Use 5 as min points (to-recheck)

library(dbscan)
library(fpc)
dbscan.fit = dbscan(df_final,eps = 2.4, MinPts = 5, scale = TRUE, method = "raw")
dbscan.fit #86 outliers


#         0  1 2 3
# border 86 27 5 4
# seed    0 55 2 1
# total  86 82 7 5

#PCA method
pca <- princomp(df_final,scores = T)
summary(pca)

# Importance of components:
#   Comp.1       Comp.2       Comp.3       Comp.4
# Standard deviation     1.157092e+12 1.263790e+04 1.304429e+03 2.483031e+01
# Proportion of Variance 1.000000e+00 1.192928e-16 1.270881e-18 4.604987e-22
# Cumulative Proportion  1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00

library(dplyr)
library(ggplot2)

pca.scores=as.data.frame(pca$scores)
pca.scores %>% select(.,Comp.1,Comp.2) %>% qplot(Comp.1,Comp.2,data=.)


pca.scores %>% select(.,Comp.1,Comp.2) %>%
  qplot(Comp.1,Comp.2,data=.)+geom_point(col=dbscan.fit$cluster+2)

#The red points are outliers in the plot given below


# OUTLIER CHECK: MAHALANOBIS DISTANCE -------------------------------------

df_eu <- dist(df_final, method="euclidean")

md<-mahalanobis(df_final, colMeans(df_final), cov(df_final), tol = 3.9262e-31)

cutoff<-qchisq(0.95, ncol(df_final))

length(which(md > cutoff))#27 outliers

df_noout <- df_final[md<cutoff,]

write.csv(df_noout, "C:/3_DANA 4840/Project/df_noout.csv")
