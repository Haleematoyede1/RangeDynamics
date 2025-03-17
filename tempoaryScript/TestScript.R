A <- c(1,2,3,4,5)
B <- c(6,7,8,9,10)
c <- A + B
c
data <- data.frame(x = 1:10, y = 11:20)
write.csv(data, "test_file.csv", row.names = FALSE)
