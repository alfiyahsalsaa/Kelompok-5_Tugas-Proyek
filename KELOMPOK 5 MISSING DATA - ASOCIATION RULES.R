library(arules)
library(arulesViz)
mydata <- read.delim("clipboard")
str(mydata)
class(mydata)
mydata
SD <- eclat (mydata, parameter = list(supp = 0.07, maxlen = 15)) 
inspect(SD)
rules1 <- apriori (mydata, parameter = list(supp = 0.001, conf = 0.5))
rules1_conf <- sort (rules1, by="confidence", decreasing=TRUE)
inspect(head(rules1_conf))
rules_lift <- sort (rules1, by="lift", decreasing=TRUE)
inspect(head(rules_lift))
rules <- apriori(mydata, parameter = list (supp = 0.001, conf = 0.5, maxlen=3))

#REMOVE REDUNANT RULES
redunant <- is.redundant(rules_lift)
which(redunant)
rules.pruned <- rules_lift[!redundant]
inspect(rules.pruned)

plot(rules1)
plot(rules1, method="grouped")
plot(rules1, method="graph")
plot(rules1, method="graph", control=list(type="items"))
plot(rules1, method="paracoord", control=list(reorder=TRUE))
