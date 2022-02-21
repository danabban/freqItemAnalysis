library(tidyverse)
library(arules)
library(arulesViz)
library(knitr)


transactions <- read_csv("data.csv",
                         col_types = cols(order_id = col_character()))


transactions.obj <- read.transactions(file = "data.csv", format = "single",
                                      sep = ",",
                                      cols = c("order_id", "product_id"),
                                      rm.duplicates = TRUE,
                                      quote = "", skip = 0,
                                      encoding = "unknown",
                                      header = TRUE)


transactions %>%
  rename(OrderID = order_id, Product = product_id) %>%
  head(5) %>%
  kable("html")


transactions %>%
  rename(OrderID = order_id, Product = product_id) %>%
  group_by(Product) %>%
  summarise(ProductCount = n()) %>%
  arrange(desc(ProductCount)) %>%
  head() %>%
  kable('html')

itemFrequencyPlot(transactions.obj, topN = 10)

# Interest Measures
support <- 0.01
# Frequent item sets
parameters = list(
  support = support,
  minlen = 2, # Minimal number of items per item set
  maxlen = 10, # Maximal number of items per item set
  target = "frequent itemsets")
freq.items <- apriori(transactions.obj, parameter = parameters)

freq.items.df <- data.frame(item_set = labels(freq.items),
                            support = freq.items@quality)

head(freq.items.df) %>%
  kable("html")

exclusion.items <- c("Banana", "Bag of Organic Bananas")
get.rules <- function(transactions, support, confidence){
  # Get Apriori rules for given support and confidence values
  #
  # Args:
  # support: support parameter
  # confidence: confidence parameter
  #
  # Reurns:
  # rules object
  parameters = list(
    support = support,
    confidence = confidence,
    minlen = 2, # Minimal number of items per item set
    maxlen = 10, # Maximal number of items per item set
    target = "rules")

  rules <- apriori(transactions,
                   parameter = parameters,
                   appearance = list(none = exclusion.items))

  return(rules)

}

rules <- get.rules( transaction = transactions.obj, support = 0.007, confidence = 0.3)

rules.df <- data.frame(rules = labels(rules),
                       rules@quality) %>%
  select(c(1,2,3,5))


rules.df %>%
  arrange(desc(confidence)) %>%
  kable("html")


topRules <- head(rules, n = 5, by = "confidence")
plot(topRules, method = "graph",  engine = "htmlwidget")
