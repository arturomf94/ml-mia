library(datasets)
data(iris)
summary(iris)

caim_value <- function(partition, attribute_index, data){
  quanta <- as.data.frame(unique(data[,ncol(data)]))
  colnames(quanta) <- c('class')
  quanta_inner <- as.data.frame(matrix(0, nrow = nrow(quanta), ncol = (length(partition) - 1)))
  quanta <- cbind(quanta, quanta_inner)
  for (i in 1:nrow(data)){
    value <- data[i,attribute_index]
    class <- data[i, ncol(data)]
    partition_index <- 1
    for (j in 1:(length(partition) - 1)){
      if (partition[j + 1] >= value){
        partition_index <- partition_index + 1
      }
    }
    quanta[quanta$class == class, partition_index] <- quanta[quanta$class == class, partition_index] + 1
  }
  return(quanta)
}

caim <- function(data){
  for (i in 1:(ncol(data) - 1)){
    attribute <- data[,i]
    potential_points <- sort(unique(attribute))
    mid_points <- as.vector(head(filter(potential_points, c(0.5,0.5)), -1))
    potential_points <- sort(append(potential_points, mid_points))
    schemes <- as.data.frame(potential_points[2:(length(potential_points) - 1)])
    schemes$bottom <- potential_points[1]
    schemes$top <- potential_points[length(potential_points)]
    colnames(schemes) <- c('mid', 'bottom', 'top')
    schemes <- schemes[,c(2,1,3)]
    schemes$caim_value <- 0
    for (j in 1:nrow(schemes)){
      partition <- as.vector(schemes[j,1:(ncol(schemes) - 1)])
      caim_value <- caim_value(partition, i, data)
      print(partition)
      print(caim_value)
    }
  }
}