## Functions
# a. Calculate time difference. 
# Input: columns contain data characters (format: %I:%M%p)
# Output: time difference in minutes
Cal_time_diff <- function(col){
  # convert Timestampe from char to time format
  temp <- strptime(col, "%I:%M%p")
  N <- length(temp)
  time_diff <- difftime(temp[2:N], temp[1:(N-1)], units = "mins")
  return (time_diff)
}


# b. Main function
# Print userIDs that have more than user-defined amount of activities within any given user-defined amount of minutes.
# Input data: dataframe, maximum actions, duration
FindRobot <- function(dataframe, max_act, duration){
  names(df) <- c("Id", "Time", "Count")
  userID <- split(df, df$Id)
  num_Col <- dim(df)[2]
  for (i in 1:length(userID)){
    userID[[i]][,(num_Col+1)] <- cbind(c(0,Cal_time_diff(userID[[i]]$Time)))
    for (k in 2:dim(userID[[i]])[1]){
      timestamp <- 0
      sum_count <- userID[[i]][(k-1), num_Col]
      while (k <= dim(userID[[i]])[1] & timestamp < duration){
        sum_count <- sum_count + userID[[i]][k, num_Col]
        timestamp <- timestamp + userID[[i]][k, (num_Col+1)]
        if (userID[[i]][k, (num_Col+1)] > duration){ sum_count <- sum_count - userID[[i]][k, num_Col] }
        if (sum_count > max_act){
          print(paste("Robotic userID:", userID[[i]][1,1]))
        }
        k <- k+1
      }
    }
  }
}




## Test
df <- read.table("ZillowHWQuestion.txt", sep = "\t", header = T, stringsAsFactors = F)
FindRobot(df, 500, 10)
# Results: "Robotic userID: 123"
