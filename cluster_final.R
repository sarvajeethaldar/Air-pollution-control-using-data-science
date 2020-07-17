#Clustering_libraries
library(factoextra)
library(cluster)
library(fpc)
library(NbClust)
library(clValid)
library(magrittr)
library(clustertend)


setwd("F:/GRAMMEN")


library(openxlsx)
data <- read.csv("C_1.csv")

head(data)
str(data)



# check missing value percentage
sapply(data,function(x) 100*(sum(is.na(x))/nrow(data)))

## pm2_5 has 97.5% missing value, hence ignored
## spm has 55.4% missing value, hence ignored
## we will consider so2, no2, and rspm
#rest of the variables are not useful for cluster analysis
# we are going to replace all the missing value with their mean

#creating a dummy variable for single variable clustering
data$dummy <- 5

## keeping only rspm and dummy

small_data <- data[,c("rspm", "dummy")]
large_data <- data[,c("stn_code","sampling_date","state","location","agency","type","location_monitoring_station","date","so2","no2","pm2_5","spm")]



## replace all missing values with their respective mean

for(i in 1:ncol(small_data)){
  small_data[is.na(small_data[,i]), i] <- mean(small_data[,i], na.rm = TRUE)
}

# reconfirmation missing value percentage
sapply(small_data,function(x) 100*(sum(is.na(x))/nrow(data)))



small_copy <- small_data #making a copy befr standardizing 


#Scaling is not needed for single variable clustering
# To standarize the variables 
#small_data = scale(small_data) 




#Method III : Scree plot to determine the number of clusters
set.seed(6)
wcss<- vector()
for(i in 1:15) wcss[i]<- sum(kmeans(small_data,i)$withinss)
plot(1:15,wcss,type='b', main=paste('Elbow graph showing optimal no. of clusters '), xlab ='Number of clusters', ylab ='WCSS')

#After running the above code we get 4 clusters as the optimal no. of clusters.






########################## Running k means #######################################

# K-Means Cluster Analysis
fit <- kmeans(small_data,4)
plot(small_data,main = paste('Clustered Data'), col = fit$cluster, pch=16)




small_copy$clust <- fit$cluster

final_data <- cbind(large_data,small_copy)


head(final_data)
head(small_copy)

small_copy$dummy <- NULL
final_data$dummy <- NULL



library(reshape2)
library(data.table)


## shows the characteristics of the clusters
dcast(setDT(final_data), clust ~ "", value.var=c("rspm"),c(mean),na.rm=TRUE)

dcast(setDT(final_data), clust ~ "", value.var=c("rspm"),c(sd),na.rm=TRUE)




# according to the characteristics of the clusters we label them into (High, Medium-I/II, Low)

final_data$clust <- as.character(final_data$clust)

final_data$clust <- gsub("1","High", final_data$clust)
final_data$clust <- gsub("2","Medium-I", final_data$clust)
final_data$clust <- gsub("3","Low", final_data$clust)
final_data$clust <- gsub("4","Medium-II", final_data$clust)




write.csv(final_data,"clust_new.csv")#exporting the data set









