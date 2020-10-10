####
## barchart
####

tattoos <- read.csv("tattoos.csv", header = TRUE, sep = ";")

library(ggplot2)

ggplot(data = tattoos) +
  geom_bar (
    mapping = aes(x = Score)
  )
### with proportions
ggplot(data = tattoos) +
  geom_bar (
    mapping = aes(x = Score, y = ..prop..)
  )

#####


workforce <- read.csv("workforce.csv", header = TRUE, sep = ";")
colnames(workforce) <- c("Occupation", "Male", "Female", "Total")
str(workforce)

ggplot(data = workforce, aes(x = reorder(Occupation, -Total), y = Total) ) +
  geom_bar ( stat = "identity")
  