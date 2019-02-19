library(ggplot2)

df_qda <- read.csv("samples_grid_qda.csv", header=FALSE)
ggplot(data = df_qda, aes(x = V1, y = V2)) + geom_tile(aes(fill = V3))

df_fda <- read.csv("samples_grid_fda.csv", header=FALSE)
ggplot(data = df_fda, aes(x = V1, y = V2)) + geom_tile(aes(fill = V3)) 