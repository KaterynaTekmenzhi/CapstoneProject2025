library(readxl)
library(ggplot2)
library(dplyr)
library(broom)

x <- read.csv("C:/Users/Kateryna.Tekmenzhi/OneDrive - Bellevue College/Student Performance Metrics Dataset/ResearchInformation3.csv")
View(x)

library(corrplot)

corplott <- cor(x)
corrplot(corplott, method = "color", tl.cex = 0.5)


