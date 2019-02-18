library(ggplot2)

df <- read.csv("data/samples.csv", header=TRUE)

ggplot(df, aes(x=x, y=y, color=prediction)) + geom_point()
