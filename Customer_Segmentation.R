# Importing Libraries and Datasets

library(ggplot2)
library(purrr)

# Loading The Dataset

customer_data <- read.csv("Mall_Customers.csv")

head(customer_data)
str(customer_data)
names(customer_data)
summary(customer_data$Age)
sd(customer_data$Age)
summary(customer_data$Annual.Income..k..)
sd(customer_data$Annual.Income..k..)
summary(customer_data$Age)
sd(customer_data$Spending.Score..1.100.)

# We check the Null and duplicate Values in the dataset.
sum(is.na(customer_data))
sum(duplicated(customer_data))

# Visualising the Dataset
# Let’s count the number of Male’s and Female’s

gender <- table(customer_data$Gender)
print(gender)
barplot(gender,main='Bar plot of Gender',xlab='Gender',ylab='Count',
        col=rainbow(2),legend=rownames(gender))


# Now, let’s visualise a pie chart to observe the ratio of male and female distribution.

percent <- gender/sum(gender) * 100
print(percent)
labels <- paste(c('Female','Male'),percent,'%')
print(labels)
pie(percent,col=rainbow(2),labels=labels)

# Visualisation of Age Distribution

hist(customer_data$Age,
     col="blue",
     main="Histogram to Show Count of Age Class",
     xlab="Age Class",
     ylab="Frequency",
     labels=TRUE)

# Descriptive Analysis of Age

boxplot(customer_data$Age,
        col="#ff0066",
        main="Boxplot for Descriptive Analysis of Age")

# Analysing of the Annual Income of the Customers

summary(customer_data$Annual.Income..k..)
hist(customer_data$Annual.Income..k..,
     col="red",
     main="Histogram for Annual Income",
     xlab="Annual Income Class",
     ylab="Frequency",
     labels=TRUE)

# Density Plot for Annual Income¶

plot(density(customer_data$Annual.Income..k..),
     col="yellow",
     main="Density Plot for Annual Income",
     xlab="Annual Income Class",
     ylab="Density")
polygon(density(customer_data$Annual.Income..k..),
        col="#ccff66")

# Analyzing Spending Score of the Customers

hist(customer_data$Spending.Score..1.100.,col='orange',labels=T,main='Distribution of Spending
													Amount')

# Descriptive Analysis of Spending Score

boxplot(customer_data$Spending.Score..1.100.,
        horizontal=TRUE,
        col="#990000",
        main="BoxPlot for Descriptive Analysis of Spending Score")

# Analysing the relationship between Age and Annual Income

plot(customer_data$Age,customer_data$Annual.Income..k..,col='green')

# Determining the Optimal value of K using Elbow Method
# Elbow Method

library(purrr)
set.seed(123)

# function to calculate total intra-cluster sum of square 
iss <- function(k) {
  kmeans(customer_data[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}
k.values <- 1:10
iss_values <- map_dbl(k.values, iss)
plot(k.values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")

# Average Silhouette Method

library(cluster)  # Finding Groups in Data
library(gridExtra)  # Miscellaneous Functions for "Grid" Graphics
library(grid)

k2<-kmeans(customer_data[,3:5],2,iter.max=100,nstart=50,algorithm="Lloyd")
s2<-plot(silhouette(k2$cluster,dist(customer_data[,3:5],"euclidean")))

k3<-kmeans(customer_data[,3:5],3,iter.max=100,nstart=50,algorithm="Lloyd")
s3<-plot(silhouette(k3$cluster,dist(customer_data[,3:5],"euclidean")))

k4<-kmeans(customer_data[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")
s4<-plot(silhouette(k3$cluster,dist(customer_data[,3:5],"euclidean")))

k5<-kmeans(customer_data[,3:5],5,iter.max=100,nstart=50,algorithm="Lloyd")
s5<-plot(silhouette(k3$cluster,dist(customer_data[,3:5],"euclidean")))

# Now, we make use of the fviz_nbclust() function to determine and visualize the optimal number of clusters as follows.

library(NbClust)
library(factoextra)

fviz_nbclust(customer_data[,3:5], kmeans, method = "silhouette")

# Visualising the Cluster Results

ggplot(customer_data,
       aes(x = Annual.Income..k..,y = Spending.Score..1.100.)) +
  geom_point(stat = 'identity',aes(col = as.factor(k5$cluster))) +
  scale_color_discrete(breaks = c('1','2','3','4','5'),
                       labels = c('C1','C2','C3','C4','C5')) +
  ggtitle('Customer Segmentation using K-means')