wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",sep=",")
wine

# V1 gives the type of wine as 1 2 and 3. V2 to V14 are the concentration of different chemicals.


# use the scatterplotMatrix function of CAR package

library("car")
scatterplotMatrix(wine[2:6])

# there seems to be a positive relationship between V4 and V5. let us see it in details

plot(wine$V4, wine$V5)

# let us label the data points by their group V1

# pos=4" will plot the text just to the right
# of the symbol for a data point. The "cex=0.5" option will plot the text at half the default size, and the "col=red"
# option will plot the text in red.

text(wine$V4, wine$V5, wine$V1, cex=0.7, pos=4, col="red")


sapply(wine[2:14],mean)
sapply(wine[2:14],sd)

# Sd varies a lot so lets standardize


table(wine$V1)

# let us find the between group sd and mean in all 13 concentrations

wine1=wine[wine$V1=="1",]
wine2=wine[wine$V1=="2",]
wine3=wine[wine$V1=="3",]


sapply(wine1[,2:14],mean)
sapply(wine1[,2:14],sd)

sapply(wine2[,2:14],mean)
sapply(wine2[,2:14],sd)


sapply(wine3[,2:14],mean)
sapply(wine3[,2:14],sd)

cor(wine[,2:14])

# there is a lot of corelation between variables. hence PCA is a viable option


# to standardise a data frame

stdwine=as.data.frame(scale(wine[,2:14]))

#============================          PCA        ====================
#*********************************************************************

#carry out a principal component analysis using the "prcomp()"
set.seed(123)

wine.pca <- prcomp(wine,scale=TRUE)



summary(wine.pca)

wine.pca$sdev


#decide how many principal components should be retained

screeplot(wine.pca, type="lines")
abline(h=1)

#Another way of deciding how many components to retain is to use Kaiser's criterion: that we should only retain
#principal components for which the variance is above 1 (when principal component analysis was applied to
 #standardised data).

(wine.pca$sdev)^2



#Loadings/scores for the Principal Components


#The loadings for the principal components are stored in a named element "rotation" of the variable returned by
# "prcomp()".

#This contains a matrix with the loadings of each principal component, where the first column in the
#matrix contains the loadings for the first principal component, the second column contains the loadings for the
#second principal component, and so on.


wine.pca$rotation[,1:3]



#    values of the first principal component are stored in the variable wine.pca$x[,1] that was returned by
#    the "prcomp()" function


wine.pca$x[,1]


#The first principal component has highest (in absolute value) loadings for V1(0.393), V7 (-0.359), V8 (-0.390),
#V13 (-0.350), V10 (-0.279), V9(0.2267),V12(-0.27), V14 (-0.269), V3 (0.222), and V5 (0.224). The loadings for V8, V7,
#V13, V10, V12 and V14 are negative, while those for V9, V3, and V5 are positive. Therefore, an interpretation
#of the first principal component is that it represents a contrast between the concentrations of V8, V7, V13, V10,
#V12, and V14, and the concentrations of V9, V3 and V5.



#Scatterplots of the Principal Components


#scatterplot of the first two principal components

plot(wine.pca$x[,1],wine.pca$x[,2]) # make a scatterplot

text(wine.pca$x[,1],wine.pca$x[,2], wine$V1, cex=0.7, pos=4, col="red") # add labels


#The scatterplot shows the first principal component on the x-axis, and the second principal component on the yaxis.
#We can see from the scatterplot that wine samples of cultivar 1 have much lower values of the first principal
#component than wine samples of cultivar 3. Therefore, the first principal component separates wine samples of
#cultivars 1 from those of cultivar 3.
#We can also see that wine samples of cultivar 2 have much higher values of the second principal component than
#wine samples of cultivars 1 and 3. Therefore, the second principal component separates samples of cultivar 2 from
#samples of cultivars 1 and 3.
#Therefore, the first two principal components are reasonably useful for distinguishing wine samples of the three
#different cultivars.













