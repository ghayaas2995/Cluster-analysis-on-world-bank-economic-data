

df_final <- read.csv("C:/3_DANA 4840/Project/df_final.csv")
rownames(df_final)<-df_final[,1]#change the name
df_final <- df_final[,-1]
str(df_final)


# CHECK NOISE USING DBSCAN ------------------------------------------------
# Scaled distance matrix
dist.Mat=dist(scale(df_final),method = "euclidean")

hist=hist(dist.Mat,breaks = 100,col = "red",main = "Histogram of Distance Matrix",xlab="Distance")
h = head(hist$breaks,n=15) 
h

#[1] 1.0 1.2 1.4 1.6 1.8 2.0 2.2 2.4 2.6 2.8 3.0 3.2 3.4 3.6 3.8
median(h) #2.4
mean(h) #2.4

#2.4 is the middle point
#The peak occurs at around 2.5 value.It means, as the inter point distance is increased, 
#more and more data points are becoming neighbors till the distance reaches 2.5 and afterward, the density starts decreasing

head(hist$counts,n=15) # [1]   5  17  21  36  79 103 125 184 263 303 349 402 448 467 489

#The next thing is to decide MinPts value. To get an approximate MinPts value the following codes are run.
mat=as.matrix(dist.Mat)
View(mat)
dim(mat) #180 180

mat.long=reshape2::melt(mat)# takes wide-format data and melts it into long-format data

#melt has assumed that all columns with numeric values are variables with values
mat.long$flag=ifelse(mat.long$value>0.0 & mat.long$value<=2.5,1,0)

hist(DF$count,col="red", main="Histogram of # of points within 2.5", 
     xlab="# of points within 2.5")

#to use 5?

library(dbscan)
library(fpc)
dbscan.fit = dbscan(df_final,eps = 2.4, MinPts = 5, scale = TRUE, method = "raw")
dbscan.fit #82 outliers

#         0  1 2 3
# border 82 26 5 4
# seed    0 60 2 1
# total  82 86 7 5

#PCA method
pca <- princomp(df_final,scores = T)
summary(pca)

# Importance of components:
#                             Comp.1       Comp.2       Comp.3       Comp.4
# Standard deviation     1.157092e+12 1.263791e+04 1.304348e+03 2.483253e+01
# Proportion of Variance 1.000000e+00 1.192929e-16 1.270724e-18 4.605814e-22
# Cumulative Proportion  1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00

library(dplyr)
library(ggplot2)

pca.scores=as.data.frame(pca$scores)
pca.scores %>% select(.,Comp.1,Comp.2) %>% qplot(Comp.1,Comp.2,data=.)


pca.scores %>% select(.,Comp.1,Comp.2) %>%
  qplot(Comp.1,Comp.2,data=.)+geom_point(col=dbscan.fit$cluster+2)




# OUTLIER CHECK: MAHALANOBIS DISTANCE -------------------------------------

df_eu <- dist(df_final, method="euclidean")

md<-mahalanobis(df_final, colMeans(df_final), cov(df_final), tol = 3.9262e-31)

cutoff<-qchisq(0.95, ncol(df_final))

length(which(md > cutoff))#26 outliers



# SUMMARY/VISUALIZATION ---------------------------------------------------

#check min,mean, max and IQR
summary(df_final)


#Visualization
#Boxplot
library(RColorBrewer)
par(mfrow=c(3,3))
plot_list<-list()
for (i in 1:(ncol(df_final)))
  {boxplot(df_final[i], main= colnames(df_final[i]), col=brewer.pal(n=6, name = "Set2"))}
        

#histogram
par(mfrow=c(3,3))
plot_list<-list()
for (i in 1:(ncol(df_final)))
  {hist(df_final[, i], xlab = colnames(df_final[i]), main= colnames(df_final[i]), col=brewer.pal(n=6, name = "Blues"))}



#check correlation
df_corr<-cor(df_final)
head(round(df_corr,2))

library(corrplot)
par(mfrow=c(1,1))
corrplot(df_corr, method="number")        
corrplot(df_corr, type="upper", order="hclust", 
         col=brewer.pal(n=8, name="RdBu"))



