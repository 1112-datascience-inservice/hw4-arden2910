
library(shiny)
runApp("C:\\project\\R_hw\\hw4-arden2910\\111971011\\app.R")

# 創建一個數值向量
x <- rnorm(100)

# 繪製直方圖
hist(x, breaks = 23)

x2to4 <- x[2,]
str(x2to4)
