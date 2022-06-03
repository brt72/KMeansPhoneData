library(data.table)
library(base)
if(!requireNamespace("R.utils")){
  install.packages("R.utils")
}

train.set.dt <- fread(file="KMeans/UCI_HAR_Dataset/test/X_test.txt")
train.set.len <- nrow(train.set.dt)
train.set.labels <- fread(file="KMeans/UCI_HAR_Dataset/test/y_test.txt")

size <- ncol(train.set.dt) # Dimension Size
clusters <- 9 # Clusters

# Cluster Initialization
cluster.dt <- data.frame(train.set.dt[sample(nrow(train.set.dt),clusters), 
                                                                        1:size])

repeat # K-Means Algorithm/Cluster Training
{
  # Cluster Assignment
  train.len <- nrow(train.set.dt)
  cluster.assignments <- vector()
  for(point in 1:train.len)
  {
    selected.point = as.numeric(as.vector(train.set.dt[point,1:size]))
    distance = sqrt(rowSums(sweep(cluster.dt, 2, selected.point, '-')^2))
    cluster.assignments[point] <- which.min(distance)
  }
  
  # Cluster Update
  new.cluster.dt <- data.frame()
  for(cluster in 1:clusters)
  {
    cluster.selection <- c(cluster.assignments == cluster)
    selected.cluster.data <- train.set.dt[cluster.selection][,1:size]
    selected.len <- nrow(selected.cluster.data)
    clust.row <- colSums(selected.cluster.data)/selected.len
    new.cluster.dt <- rbind(new.cluster.dt, clust.row)
  }
  
  # Check if Clusters Changed
  difference <- sum(abs(new.cluster.dt-cluster.dt))
  cluster.dt = new.cluster.dt
  if(difference < .01)
  {
    table(cluster.assignments)
    break
  }
}

# Set the Frequency of each Label in each Cluster 
max.label <- max(train.set.labels)
cluster.assignment.dt <- data.frame()
for(cluster in 1:clusters)
{
  cluster.assign.amount <- vector()
  selected.cluster <- train.set.labels[cluster.assignments==cluster,1]
  for(label in 1:max.label)
  {
    selected.label <- selected.cluster[[1]][selected.cluster[[1]] == label]
    cluster.assign.amount <- cbind(cluster.assign.amount, 
                                                         length(selected.label))
  }
  cluster.assignment.dt <- rbind(cluster.assignment.dt, cluster.assign.amount)
}

# Set the Probability of each Label in each Cluster
assignment.totals <- rowSums(cluster.assignment.dt)
probabilities.dt <- data.frame()
for(cluster in 1:clusters)
{
  current.prob <- cluster.assignment.dt[cluster,]/assignment.totals[cluster]
  for(prob in ncol(current.prob):2)
  {
    current.prob[prob] = sum(current.prob[1:prob])
  }
  probabilities.dt <- rbind(probabilities.dt, current.prob)
}


validation.set <- fread(file="KMeans/UCI_HAR_Dataset/train/X_train.txt")
validation.labels <- fread(file="KMeans/UCI_HAR_Dataset/train/y_train.txt")

# Cluster Validation
test.dt <- matrix(0, nrow=6, ncol=6)
for(point in 1:nrow(validation.set))
{
  # Assigns each Data Point to the Closest Cluster
  selected.point = as.numeric(as.vector(validation.set[point,1:size]))
  distance = sqrt(rowSums(sweep(cluster.dt, 2, selected.point, '-')^2))
  
  # Generates a Random Number between 0 and 1 and assigns a label to the point 
  # based on the Cluster's Label Probability
  chosen.label = runif(1)
  selected.prob = probabilities.dt[which.min(distance),]
  correct.label = as.numeric(validation.labels[point])
  
  # Checks if the assigned label matches the actual label
  for(prob in 1:ncol(selected.prob))
  {
    if(chosen.label < selected.prob[prob])
    {
      test.dt[correct.label, prob]
      test.dt[correct.label, prob] = test.dt[correct.label, prob] + 1
      break
    }
  }
}

# Calculates how many labels were assigned correctly
total.matches = 0
for(index in 1:nrow(test.dt))
{
  total.matches = total.matches + test.dt[index,index]
}
accuracy = total.matches/nrow(validation.set)
print(accuracy)