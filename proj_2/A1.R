# TMA4300: Exercise 2
# Problem A, 1) First impression of the data

library("boot")
library("ggplot2")

# Data: Time intervals between successive coal-mining disasters
# years 1851-1962
frame <- data.frame(year = data <- coal[2:190,], disasters = c(1:189))
ggplot(frame) + geom_point(aes(year,disasters))

# Disaster rate bigger before 1900, smaller after 