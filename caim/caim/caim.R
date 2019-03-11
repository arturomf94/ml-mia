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
  n <- length(partition) - 1
  caim_value_result <- 0 
  for (k in 2:n){
    if (sum(quanta[,k]) != 0){
      caim_value_result <- caim_value_result + max(quanta[,k]) ^ 2 / sum(quanta[,k])  
    }
  }
  return(caim_value_result)
}

caim <- function(data){
  for (i in 1:(ncol(data) - 1)){
    print('Atributo:')
    print(colnames(data)[i])
    attribute <- data[,i]
    potential_points <- sort(unique(attribute))
    mid_points <- as.vector(head(filter(potential_points, c(0.5,0.5)), -1))
    potential_points <- sort(append(potential_points, mid_points))
    bottom <- potential_points[1]
    top <- potential_points[length(potential_points)]
    potential_points <- potential_points[2:(length(potential_points) - 1)]
    scheme <- c(bottom, top)
    prev_caim_value <- 0
    while (length(potential_points) > 0){
      max_caim_value <- -1
      for (point in potential_points){
        potential_scheme <- sort(append(scheme, point))
        caim_value <- caim_value(potential_scheme, i, data)
        if (caim_value > max_caim_value){
          max_caim_value <- caim_value
          best_point <- point
          best_potential_scheme <- potential_scheme
        }
      }
      scheme <- best_potential_scheme
      potential_points <- potential_points[potential_points != best_point]
      if (prev_caim_value > max_caim_value){
        break
      }
      prev_caim_value <- max_caim_value
      print(scheme)
      print(max_caim_value)
    }
  }
}