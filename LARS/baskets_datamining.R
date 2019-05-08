
library(RCurl)
library(tidyverse)
library(arules)
library(arulesViz)

df_groceries <- read.csv(text = getURL("https://raw.githubusercontent.com/nupur1492/RProjects/master/MarketBasketAnalysis/groceries.csv"))


df_sorted <- df_groceries[order(df_groceries$Member_number),]
df_sorted$Member_number <- as.numeric(df_sorted$Member_number)


df_itemList <- df_sorted %>% 
  group_by(Member_number, Date) %>% 
  summarize(items = paste(itemDescription, collapse = ","))

df_itemList$Member_number <- NULL
df_itemList$Date <- NULL

colnames(df_itemList) <- c("itemList")

write.csv(df_itemList,"ItemList.csv", row.names = TRUE)

txn <-  read.transactions(file="ItemList.csv", sep = ",")


txn@itemInfo$labels <- gsub("\"","",txn@itemInfo$labels)
arules::apriori(txn)

inspect(basket_rules)

df_basket <- as(basket_rules,"data.frame")
View(df_basket)






plot(basket_rules)
plot(basket_rules, method = "grouped", control = list(k = 5))
plot(basket_rules, method="graph", control=list(type="items"))
plot(basket_rules, method="paracoord",  control=list(alpha=.5, reorder=TRUE))
plot(basket_rules,measure=c("support","lift"),shading="confidence",interactive=T)

itemFrequencyPlot(txn, topN = 15)



data("Adult")
## Mine association rules.
rules <- apriori(Adult, parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
summary(rules)

plot(rules)
plot(rules, method = "grouped", control = list(k = 5))
plot(rules, method="graph", control=list(type="items"))
plot(rules, method="paracoord",  control=list(alpha=.5, reorder=TRUE))
plot(rules,measure=c("support","lift"),shading="confidence",interactive=T)
