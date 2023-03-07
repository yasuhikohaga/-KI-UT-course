#1. Write a function that calculates the ratio of the mean and the median of a given vector.
ratio_of_mean_and_median <- function(x){
  return(mean(x)/median(x))
}
ratio_of_mean_and_median(c(2,4,5,71,5,21,4,21))#example

#2. Write a function that ignores the lowest and the highest value from a given vector and calculate the mean.
adjusted_mean <- function(x){
  if (length(x) < 3){
    return("can't calculate adjusted mean")
  }else{
    max_index <- which(x==max(x))[1]
    min_index <- which(x==min(x))[1]
    x <- x[-max_index]
    x <- x[-min_index]
    return(mean(x))
  }
}
#examples
adjusted_mean(c(21,42))#"can't calculate adjusted mean"
adjusted_mean(c(1,3,6,32,4,12,54,65,1,43))#19.375
mean(c(1,3,6,32,4,12,54,65,1,43))#22.1

#3. Write a short explanation of why, how, and when not to use pipes.
The pipe, %>%, is a powerful tool for writing a sequence of multiple operations.
There are three situations should not use the pipe.
First, the pipes are longer than ten steps, because it is not easy to debug codes.
Second, there are multiple inputs or outputs.
Finally, handling a directed graph with a complex dependency structures.

#4. Write a short explanation of why the apply-family of functions (apply, lapply, sapply etc.) could useful in your work.
Apply family functions can apply functions, like "mean" "median" "sum" etc., to a data object and return a data set.
This helps us to minimize need to explicitly create loops, and make our codes look clear and easily understandable.