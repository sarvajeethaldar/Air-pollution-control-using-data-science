library(rpart)# tree models 
library(caret) # feature selection
library(rpart.plot) # plot dtree
library(ROCR) # model evaluation
library(e1071) # tuning model
library(RColorBrewer)
library(rattle)# optional, if you can't install it, it's okay
library(tree)
library(ISLR)

setwd("F:/GRAMMEN")

data <- read.csv("clust_new.csv")


head(data)


library(caTools)
set.seed(123)
split = sample.split(data$clust, SplitRatio = 0.003)
training = subset(data, split == TRUE)
test = subset(data, split == FALSE)
clust <- training$clust

sapply(training,function(x) 100*(sum(is.na(x))/nrow(training)))

# pm2_5 has 97.5% missing value, hence ignored
# spm has 55.4% missing value, hence ignored
# since the clustering has been done on rspm we need to remove it while predicting the labels

# state, location, so2, no2, type are the variables requried for making the descision tree
# rest of the variables are not useful for cluster analysis.

training <- training[, c(4,5,7,10,11,15)]

sapply(training,function(x) 100*(sum(is.na(x))/nrow(training)))

training <- na.omit(training) # as for type there are 1.347% missing values, so we must remove them


ind_var <- training[,-6]
dep_var <- training[,6]


library(rpart)
mtree <- rpart(clust ~., data = training, method="class", 
               control = rpart.control(minsplit = 20, minbucket = 7, 
                                       maxdepth = 10, usesurrogate = 2, xval =10 ))



#Beautify tree
#view1
prp(mtree, faclen = 0, cex = 0.8, extra = 1)


#view2 - total count at each node
tot_count <- function(x, labs, digits, varlen)
{paste(labs, "\n\nn =", x$frame$n)}

prp(mtree, faclen = 0, cex = 0.8, node.fun=tot_count)



predictions <- predict(mtree, training, type="class")
mean(predictions != Species)


#training$predictions <- predictions
#setwd("C:\\Users\\Subhojit\\Desktop\\R class July 2017\\Class 9 Decision Tree")
#write.csv(training,"bhul.csv")

predictions <- predict(mtree, ind_var, type="class")
confusionMatrix(data=predictions, reference=dep_var, positive="Yes")

# kappa will never be greater than 1.
# more than .8 means excellent
# .6 to .8 very good


############################
########Pruning#############
############################


printcp(mtree)## check where xerror is lowest
bestcp <- mtree$cptable[which.min(mtree$cptable[,"xerror"]),"CP"]

# Prune the tree using the best cp.
pruned <- prune(mtree, cp = bestcp)


# Plot pruned tree
prp(pruned, faclen = 0, cex = 0.8, extra = 1)


predictions <- predict(pruned, training, type="class")
mean(predictions != Species)


predictions <- predict(pruned, ind_var, type="class")
confusionMatrix(data=predictions, reference=dep_var, positive="Yes")

# Advanced Plot
prp(pruned, main="Beautiful Tree",
    extra=106, 
    nn=TRUE, 
    fallen.leaves=TRUE, 
    branch=.5, 
    faclen=0, 
    trace=1, 
    shadow.col="gray", 
    branch.lty=3, 
    split.cex=1.2, 
    split.prefix="is ", 
    split.suffix="?", 
    split.box.col="lightgray", 
    split.border.col="darkgray", 
    split.round=.5)
