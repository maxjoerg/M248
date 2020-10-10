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
  geom_bar ( stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#########

library(tidyr)
workforce_long <- gather(workforce, gender, count, Male:Female, factor_key=TRUE)
str(workforce_long) 

ggplot(data = workforce_long, aes(x = reorder(Occupation, -Total), y = count, fill = gender) ) +
  geom_bar ( stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





  