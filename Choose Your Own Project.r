#############################
# CLUSTERING HEART DISEASE 
#############################
## Name : Gael Penessot
## Version : 1.0
## Date : 02 jan. 2020
#############################

# Load the data
heart_disease <- read.csv("heart_disease_patients.csv")

# Print the first ten rows
head(heart_disease, 10)

# dimenisons
dim(heart_disease)

# We check if we need to scale the data
summary(heart_disease)

# Remove id
heart_disease <- heart_disease[ , !(names(heart_disease) %in% c("id"))]

# Scaling data and saving as a data frame
scaled <- scale(heart_disease)

# What do the data look like now?
summary(scaled)

#######################################################
#    install package if not available on computer     #
#######################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("data.table", repos = "http://cran.us.r-project.org")

#######################################################
#                   load packages                     #
#######################################################

library(tidyverse, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)

# Set the seed so that results are reproducible
seed_val <- 10
set.seed(seed_val)

# Select a number of clusters
k <- 5

# Run the k-means algorithm
first_clust <- kmeans(scaled, centers = k, nstart = 1)

# How many patients are in each cluster?
first_clust$size

# Set the seed
seed_val <- 38
set.seed(seed_val)

# Select a number of clusters and run the k-means algorithm
k <- 5
second_clust <- kmeans(scaled, centers = k, nstart = 1)

# How many patients are in each cluster?
second_clust$size

# Add cluster assignments to the data
heart_disease["first_clust"] <- first_clust$cluster
heart_disease["second_clust"] <- second_clust$cluster

# Create and print the plot of age and chol for the first clustering algorithm
plot_one  <- ggplot(heart_disease, 
                    aes(x = age, 
                        y = chol, 
                        col = as.factor(first_clust))) + 
             geom_point()
plot_one 

# Create and print the plot of age and chol for the second clustering algorithm
plot_two  <- ggplot(heart_disease, 
                    aes(x = age, 
                        y = chol, 
                        col = as.factor(second_clust))) + 
             geom_point()
plot_two

# Execute hierarchical clustering with complete linkage
hier_clust_1 <- hclust(dist(scaled), method = "complete")

# Print the dendrogram
plot(hier_clust_1)

# Get cluster assignments based on number of selected clusters
hc_1_assign <- cutree(hier_clust_1, 5)

# Execute hierarchical clustering a single linkage
hier_clust_2 <- hclust(dist(scaled), method = "single")

# Print the dendrogram
plot(hier_clust_2)

# Get cluster assignments based on number of selected clusters
hc_2_assign <- cutree(hier_clust_2, 5)

# Add assignment of chosen hierarchical linkage
heart_disease["hc_clust"] <- hc_1_assign

# Remove the sex, first_clust, and second_clust variables
hd_simple <- heart_disease[, !(names(heart_disease) %in% c("sex", "first_clust", "second_clust"))]

# Get the mean and standard deviation summary statistics
clust_summary <- do.call(data.frame, 
                         aggregate(. ~hc_clust, data = hd_simple, 
                                   function(x) c(avg = mean(x), sd = sd(x))))
clust_summary

# Plot age and chol
plot_one <- ggplot(heart_disease, 
                   aes(x = age, 
                       y = chol, 
                       color = as.factor(hc_clust))) + 
            geom_point()
plot_one 

# Plot oldpeak and trestbps
plot_two <- ggplot(heart_disease, 
                   aes(x = oldpeak, 
                       y = trestbps, 
                       color = as.factor(hc_clust))) + 
            geom_point()
plot_two


