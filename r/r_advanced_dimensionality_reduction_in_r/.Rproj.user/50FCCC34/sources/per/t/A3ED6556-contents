load("mnist-sample-200.Rdata")

# Have a look at the MNIST dataset names
names(mnist_sample)

# Show the first records
str(mnist_sample)

# Show the labels of the first 6 digits
head(mnist_sample$label)

# Plot the histogram of the digit labels
hist(mnist_sample$label)

# Compute the basic statistics of all records
summary(mnist_sample)

# Compute the basic statistics of digits with label 0
summary(mnist_sample[,mnist_sample$label==0])

# Show the labels of the first 10 records
mnist_sample$label[1:10]

# Compute the Euclidean distance of the first 10 records
distances <- dist(mnist_sample[1:10, -1])

# Show the distances values
distances

# Plot the numeric matrix of the distances in a heatmap
heatmap(as.matrix(distances), 
        Rowv = NA, symm = TRUE, 
        labRow = mnist_sample$label[1:10], 
        labCol = mnist_sample$label[1:10])

# Minkowski distance or order 3
distances_3 <- dist(mnist_sample[1:10, -1], method = "minkowski", p = 3)
distances_3
heatmap(as.matrix(distances_3), 
        Rowv = NA, symm = TRUE, 
        labRow = mnist_sample$label[1:10], 
        labCol = mnist_sample$label[1:10])

# Minkowski distance of order 2
distances_2 <- dist(mnist_sample[1:10, -1], method = "minkowski", p = 2)
distances_2
heatmap(as.matrix(distances_2), 
        Rowv = NA, symm = TRUE, 
        labRow = mnist_sample$label[1:10], 
        labCol = mnist_sample$label[1:10])

# Get the first 10 records
mnist_10 <- mnist_sample[1:10, -1]

# Add 1 to avoid NaN when rescaling
mnist_10_prep <- mnist_10 + 1 

# Compute the sums per row
sums <- rowSums(mnist_10_prep)

library(philentropy)
# Compute KL divergence
distances <- philentropy::distance(mnist_10/sums, method = "kullback-leibler")
heatmap(as.matrix(distances), 
        Rowv = NA, symm = TRUE, 
        labRow = mnist_sample$label, 
        labCol = mnist_sample$label)

# Get the principal components from PCA
pca_output <- prcomp(mnist_sample[, -1])

# Observe a summary of the output
summary(pca_output)

# Store the first two coordinates and the label in a data frame
pca_plot <- data.frame(pca_x = pca_output$x[, 1], pca_y = pca_output$x[, 2], 
                       label = as.factor(mnist_sample$label))

library(tidyverse)
# Plot the first two principal components using the true labels as color and shape
ggplot(pca_plot, aes(x = pca_x, y = pca_y, color = label)) + 
  ggtitle("PCA of MNIST sample") + 
  geom_text(aes(label = label)) + 
  theme(legend.position = "none")


# Explore the tsne_output structure
str(tsne_output)

# Have a look at the first records from the t-SNE output
head(tsne_output$Y)

# Store the first two coordinates and the label in a data.frame
tsne_plot <- data.frame(tsne_x = tsne_output$Y[, 1], tsne_y = tsne_output$Y[, 2], 
                        label = as.factor(mnist_sample$label))

# Plot the t-SNE embedding using the true labels as color and shape
ggplot(tsne_plot, aes(x = tsne_x, y = tsne_y, color = label)) + 
  ggtitle("T-Sne output") + 
  geom_text(aes(label = label)) + 
  theme(legend.position = "none")


library(tsne)

tsne_output <- tsne(mnist_sample[, -1])
# Explore the tsne_output structure
str(tsne_output)

# Have a look at the first records from the t-SNE output
head(tsne_output)

# Store the first two coordinates and the label in a data.frame
tsne_plot <- data.frame(tsne_x = tsne_output[, 1], tsne_y = tsne_output[, 2], 
                        label = as.factor(mnist_sample$label))

# Plot the t-SNE embedding using the true labels as color and shape
ggplot(tsne_plot, aes(x = tsne_x, y = tsne_y, color = label)) + 
  ggtitle("T-Sne output") + 
  geom_text(aes(label = label)) + 
  theme(legend.position = "none")

library(Rtsne)

# Compute t-SNE without doing the PCA step
tsne_output <- Rtsne(mnist_sample[, -1], PCA = FALSE, dims = 3)

# Show the obtained embedding coordinates
head(tsne_output$Y)

# Store the first two coordinates and plot them 
tsne_plot <- data.frame(tsne_x = tsne_output$Y[, 1], tsne_y = tsne_output$Y[, 2], 
                        digit = as.factor(mnist_sample$label))

# Plot the coordinates
ggplot(tsne_plot, aes(x = tsne_x, y = tsne_y, color = digit)) + 
  ggtitle("t-SNE of MNIST sample") + 
  geom_text(aes(label = digit)) + 
  theme(legend.position = "none")

# Inspect the output object's structure
str(tsne_output)

# Show total costs after each 50th iteration
tsne_output$itercosts

# Plot the evolution of the KL divergence at each 50th iteration
plot(tsne_output$itercosts, type = "l")

# Generate a three-dimensional t-SNE embedding without PCA
tsne_output <- Rtsne(mnist_sample[, -1], PCA = FALSE, dims = 3)

# Generate a new t-SNE embedding with the same hyper-parameter values
tsne_output_new <- Rtsne(mnist_sample[, -1], PCA = FALSE, dims = 3)

# Check if the two outputs are identical
identical(tsne_output, tsne_output_new)

# Set seed to ensure reproducible results
set.seed(1234)

# Execute a t-SNE with 2000 iterations
tsne_output <- Rtsne(mnist_sample[, -1], PCA = TRUE, dims = 2, max_iter = 2000)

# Observe the output costs 
tsne_output$itercosts

# Get the 50th iteration with the minimum K-L cost
which.min(tsne_output$itercosts)

# Set seed to ensure reproducible results
set.seed(1234)

# Execute a t-SNE with perplexity 5
tsne_output <- Rtsne(mnist_sample[, -1], perplexity = 5, max_iter = 1200)

# Observe the returned K-L divergence costs at every 50th iteration
tsne_output$itercosts

tsne_output_5 <- tsne_output

# Observe the K-L divergence costs with perplexity 5 and 50
tsne_output_5$itercosts

tsne_output_50 <- Rtsne(mnist_sample[, -1], perplexity = 5, max_iter = 1200)
tsne_output_50$itercosts

load("mnist-sample-200.Rdata")
mnist_sample$label

# Generate the data frame to visualize the embedding
tsne_plot_5 <- data.frame(tsne_x = tsne_output_5$Y[,1], tsne_y = tsne_output_5$Y[,2], digit = as.factor(mnist_sample$label))

tsne_plot_50 <- data.frame(tsne_x = tsne_output_50$Y[,1], tsne_y = tsne_output_50$Y[,2], digit = as.factor(mnist_sample$label))

# Plot the obtained embeddings
ggplot(tsne_plot_5, aes(x = tsne_x, y = tsne_y, color = digit)) + 
  ggtitle("MNIST t-SNE with 1300 iter and Perplexity=5") + geom_text(aes(label = digit)) + 
  theme(legend.position="none")
ggplot(tsne_plot_50, aes(x = tsne_x, y = tsne_y, color = digit)) + 
  ggtitle("MNIST t-SNE with 1300 iter and Perplexity=50") + geom_text(aes(label = digit)) + 
  theme(legend.position="none")

# Prepare the data.frame
tsne_plot <- data.frame(tsne_x = tsne_output$Y[, 1], 
                        tsne_y = tsne_output$Y[, 2], 
                        digit = as.factor(mnist_sample$label))

# Plot the obtained embedding
ggplot(tsne_plot, aes(x = tsne_x, y = tsne_y, color = digit)) + 
  ggtitle("MNIST embedding of the first 5K digits") + 
  geom_text(aes(label = digit)) + 
  theme(legend.position="none")

library(data.table)
# Get the first 5K records and set the column names
dt_prototypes <- as.data.table(tsne_output$Y)

setnames(dt_prototypes, c("X", "Y"))

# Paste the label column as factor
dt_prototypes[, label := as.factor(mnist_sample$label)]

# Compute the centroids per label
dt_prototypes[, mean_X := mean(X), by = label]
dt_prototypes[, mean_Y := mean(Y), by = label]

# Get the unique records per label
dt_prototypes <- unique(dt_prototypes, by = "label")
dt_prototypes

# Store the last 5000 records in distances and set column names
distances <- as.data.table(tsne_output$Y)
setnames(distances, c("X", "Y"))

# Paste the true label
distances[, label := mnist_sample$label]

# Filter only those labels that are 1 or 0 
distances <- distances[label == 1 | label == 0]

# Compute Euclidean distance to prototype of digit 1
distances[, dist_1 := sqrt(( (X - dt_prototypes[label == 1,]$mean_X) + 
                               (Y - dt_prototypes[label == 1,]$mean_Y))^2)]

# Compute the basic statistics of distances from records of class 1
summary(distances[label == 1]$dist_1)

# Compute the basic statistics of distances from records of class 1
summary(distances[label == 0]$dist_1)

# Plot the histogram of distances of each class
ggplot(distances, aes(x = dist_1, fill = as.factor(label))) +
  geom_histogram(binwidth = 5, alpha = .5, position = "identity", show.legend = FALSE) + 
  ggtitle("Distribution of Euclidean distance 1 vs 0")

load("creditcard.Rdata")

# Look at the data dimensions
dim(creditcard)

# Explore the column names
names(creditcard)

# Observe some records
str(creditcard)

# Generate a summary
summary(creditcard)

# Plot a histogram of the transaction time
ggplot(creditcard, aes(x = Time)) + 
  geom_histogram()

# Extract positive and negative instances of fraud
creditcard_pos <- creditcard["Class" == 1]
creditcard_neg <- creditcard["Class" == 0]

# Fix the seed
set.seed(1234)

# Separate the features and label
train_x <- creditcard[, c(-1,-30,-31)]
train_y <- factor(as.numeric(creditcard$Class))

library(randomForest)

# Train a random forests
rf_model <- randomForest(x = train_x, y = train_y, ntree = 100) 

# Plot the error evolution and variable importance
plot(rf_model)
varImpPlot(rf_model)

# Set the seed
set.seed(1234)

library(Rtsne)
library(tidyverse)
# Generate the t-SNE embedding
tsne_output <- Rtsne(as.matrix(creditcard[, c(-1,-30,-31)]), check_duplicates = FALSE, PCA = FALSE)

# Generate a data frame to plot the result
tsne_plot <- data.frame(tsne_x = tsne_output$Y[,1], 
                        tsne_y = tsne_output$Y[,2], 
                        Class = as.numeric(creditcard$Class))

# Plot the embedding usign ggplot and the label
ggplot(tsne_plot, aes(x = tsne_x, y = tsne_y, color = Class)) + 
  ggtitle("t-SNE of credit card fraud train set") + 
  geom_text(aes(label = Class)) + theme(legend.position = "none")

# Fix the seed
set.seed(1234)

# Train a random forest
rf_model_tsne <- randomForest(x = tsne_output$Y, y = factor(as.numeric(creditcard$Class)), ntree = 100) 

# Plot the error evolution
plot(rf_model_tsne)

# Plot the variable importance
varImpPlot(rf_model_tsne)

# Predict on the test set using the random forest 
pred_rf <- predict(rf_model, creditcard, type = "prob")

# Plot a probability distibution of the target class
hist(pred_rf[,2])

library(ROCR)

# Compute the area under the curve
pred <- prediction(pred_rf[,2], as.numeric(creditcard$Class))
perf <- performance(pred, measure = "auc") 
perf@y.values

# Predict on the test set using the random forest generated with t-SNE features
pred_rf <- predict(rf_model_tsne, test_x, type = "prob")

# Plot a probability distibution of the target class
hist(pred_rf[,2])

# Compute the area under the curve
pred <- prediction(pred_rf[,2], creditcard$Class)
perf <- performance(pred, measure = "auc") 
perf@y.values

load("fashion_mnist_500.Rdata")

# Observe the dimensions
dim(layer_128_train)

# Show the first six records of the last ten columns
head(layer_128_train[, 118:128])

# Generate a summary of all columns
summary(layer_128_train)

# Set the seed
set.seed(1234)

# Generate the t-SNE
tsne_output <- Rtsne(as.matrix(layer_128_train), perplexity = 50, 
                     max_iter = 400, check_duplicates = FALSE)

# Prepare data.frame
tsne_plot <- data.frame(tsne_x = tsne_output$Y[,1], tsne_y = tsne_output$Y[,2], 
                        Class = creditcard_train$Class)

# Plot the data 
ggplot(tsne_plot, aes(x = tsne_x, y = tsne_y, color = Class)) + 
  geom_point() + 
  ggtitle("Credit card embedding of Last Neural Network Layer")

# Show the dimensions
dim(fashion_mnist)

# Create a summary of the last six columns 
summary(fashion_mnist[, 780:785])

# Table with the class distribution
table(fashion_mnist$label)

# Get the data from the last image
plot_data <- cbind(xy_axis, fill = as.data.frame(t(fashion_mnist[500, -1]))[,1])

# Observe the first records
head(plot_data)

# Plot the image using ggplot()
ggplot(plot_data, aes(x, y, fill = fill)) + 
  ggtitle(class_names[as.integer(fashion_mnist[500, 1])]) + 
  plot_theme 

# Start a connection with the h2o cluster
h2o.init()

# Store the data into the h2o cluster
fashion_mnist.hex <- as.h2o(fashion_mnist, "fashion_mnist.hex")

# Start a connection with the h2o cluster
h2o.init()

# Store the data into the h2o cluster
fashion_mnist.hex <- as.h2o(fashion_mnist, "fashion_mnist.hex")

# Dimension of X_matrix
dim(X_matrix)

# First records of X_matrix
head(X_matrix)

# Plot the records in the new two dimensional space
ggplot(as.data.table(X_matrix), aes(x= Arch1, y = Arch2, color = fashion_mnist$label)) + 
  ggtitle("Fashion Mnist GLRM Archetypes") + 
  geom_text(aes(label = fashion_mnist$label)) + 
  theme(legend.position="none")

# Store the label of each record and compute the centroids
X_matrix[, label := as.numeric(fashion_mnist$label)]
X_matrix[, mean_x := mean(Arch1), by = label]
X_matrix[, mean_y := mean(Arch2), by = label]

# Store the input data in h2o
fashion_mnist_miss.hex <- as.h2o(fashion_mnist_miss, "fashion_mnist_miss.hex")

# Build a GLRM model
model_glrm <- h2o.glrm(training_frame = fashion_mnist_miss.hex,
                       transform = "NORMALIZE",
                       k = 2,
                       max_iterations = 100)

# Impute missing values
fashion_pred <- h2o.predict(model_glrm, fashion_mnist_miss.hex)

# Observe the statistics of the first 5 pixels 
summary(fashion_pred[, 1:5])

# Get the starting timestamp
time_start <- proc.time()

# Train the random forest
rf_model <- randomForest(x = fashion_mnist[, -1], y = fashion_mnist$label, ntree = 20)

# Get the end timestamp
time_end <- timetaken(time_start)

# Show the error and the time
rf_model$err.rate[20]
time_end

# Get the starting timestamp
time_start <- proc.time()

# Train the random forest
rf_model <- randomForest(x = train_x, y = train_y, ntree = 500)

# Get the end timestamp
time_end <- timetaken(time_start)

# Show the error and the time
rf_model$err.rate[500]
time_end