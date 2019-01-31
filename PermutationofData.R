#This script takes a series of ordinal values (classes) and an associated value (species richness)
#and permutes the assocaited value while holding the ordinal values constant. This is then visualized
#as a null distribution overlayed with the original observed values. Significance values
#(whether the ovserved falls outside 95% of randomizations can be calculated from the table of permuted)
#data also produced.

require(dplyr)
require(ggplot2)

#Code assumes two columns in input: [1] number of classes and [2] richness value associated with each

datain <- read.csv("Datain.csv")
datain2 <- read.csv("Datain.csv") #duplicate to above for working with
num <- 999 #desired number of permutations

#loop to produce n number of randomizations of the data
for(i in 1:num){
perm <- sample(datain$Richness, replace=FALSE)
datain[ , paste0("perm", i)] <- perm
}

write.table(datain, "permutations.csv", row.names=FALSE, sep=",") #table output of all randomizations

means.by.group <- aggregate(datain[, 2:1001], list(datain$SUMofClasses), mean) #calculate an average for each class

#Plot results
library(ggplot2)
library(reshape2)

obs.to.plot <- aggregate(datain[, 2], list(datain$SUMofClasses), mean) #aggregate by each class
dfm <- melt(means.by.group, id.vars = "Group.1")
mfd <- melt(obs.to.plot, id.vars = "Group.1")

#color observed data differently from null distributiuon
p <- ggplot(dfm, aes(x = Group.1, y = value, group = variable)) +
  
  geom_point(data = datain2, aes(x = SUMofClasses, y = Richness, group = SUMofClasses), shape = 1, colour = "grey") + #plots raw data points underneath
  
  geom_line(colour = "blue", alpha = 0.1) + theme(legend.position="none") +
  geom_line(data = mfd, aes(x= Group.1, y=value), colour = "red") +
  
  theme(
  panel.grid.major = element_line(colour = "white")) +
    scale_x_continuous(breaks = seq(0, 19, 1), minor_breaks = NULL) +
    scale_y_continuous(breaks = seq(0, 5000, 500), minor_breaks = NULL)
  
p #plot figure, save manually


