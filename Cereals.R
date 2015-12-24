cereals=read.csv("cereals.csv")
str(cereals)

cor(cereals$calories,cereals$rating)
plot(cereals$calories,cereals$rating)
summary(cereals)

cereals_clean=na.omit(cereals)
summary(cereals_clean)

#Put the cereal names in cname
cname <- cereals_clean[,1]
cname
#Replac row numbers with cereal names
row.names(cereals_clean) <- cname 

library("car")
# check the co-relation between Calories and Rating
windows()
scatterplotMatrix(cereals_clean[4:16])

#Run principal components on the correlation matrix
cereals_pca <- prcomp(cereals_clean[,4:16],scale=T)

summary(cereals_pca)

screeplot(cereals_pca, type="lines")
abline(h=1)

# Scores of the PC
cereals_pca$rotation[,1:3]

#    values of the first principal component are stored in the variable cereals_pca$x[,1] that was returned by
#    the "prcomp()" function

cereals_pca$x[,1]

#scatterplot of the first two principal components
plot(cereals_pca$x[,1],cereals_pca$x[,2]) # make a scatterplot

text(cereals_pca$x[,1],cereals_pca$x[,2], cereals$name, cex=0.7, pos=4, col="red") # add labels






