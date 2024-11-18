#install.packages("arules")
#install.packages("arulesViz")
library(arules)

#Understanding the dataset
  data(Groceries)
summary(Groceries)
 
# create a list of baskets
trans <- as(Groceries, "transactions")

#Inspect data
#Take a look at the dimensions of this object:
dim(trans)
## [1] 9835  169 #This means we have 9835 transactions and 169 distinct items.

#Obtain a list of the distinct items in the data:
itemLabels(trans)

#View the summary of the transaction data:
summary(trans)


#The summary() gives us information about our transaction object:
#	There are 9835  transactions (rows) and 169 items (columns) and we can view the most frequent items.
#	Density tells us the percentage of non-zero cells in this 9835 x 169-matrix.
#	Element length distribution: a set of 2 items in 4 transactions; 3 items in 2 of the transactions and 4 items in 2 transactions.

image(trans)  #	Density tells us the percentage of non-zero cells in this 9835 x 169-matrix.

#Display the relative item frequency:

itemFrequencyPlot(trans, topN=10,  cex.names=1) 

#The items {whole milk}, {other vegetables} , {rolls/buns} and {soda} all have a relative item frequency (i.e. support) of above 15%.

#A-Priori Algorithm

#Min Support 0.03, confidence as 0.15.
rules <- apriori(trans, parameter = list(supp=0.03, conf=0.15, maxlen=20, target= "rules"))

summary(rules)

#	Set of rules: 37.
#	Rule length distribution (LHS + RHS): 1 rule  with a length of 2 items; 4 rules with a length of 33 items.
#	Summary of quality measures: min, max, median, mean and quantile values for support, confidence and lift.
#	Mining info: number of transactions, support-threshold and confidence-threshold.

#Inspect the 37 rules we obtained:
inspect(rules)
#See rule 14 for an idea on new product

#The rules 1 to 4 with an empty LHS mean that no matter
 #what other items are involved the item in the RHS will appear with
 #the probability given by the rule’s confidence (which equals the support).


#Setting  LHS and RHS
#If you want to analyze a specific rule, 
#you can use the option appearance to set a LHS (if part) or RHS (then part) of the rule.
#For example, to analyze what items customers buy before buying {soda}, 
#we set rhs=soda  and default = lhs:
soda_rules_rhs <- apriori(trans,parameter = list(supp=0.1, conf=0.1, maxlen=10, minlen=1),appearance = list(default="lhs", rhs="soda"))

summary(soda_rules_rhs) 

inspect(soda_rules_rhs)

#It is also possible to analyze what items customers buy after buying {soda}:
soda_rules_lhs <- apriori(trans, parameter = list(supp=0.03, conf=0.1, maxlen=10,minlen=2),appearance = list(lhs="soda", default="rhs"))
summary(soda_rules_lhs ) 

inspect(soda_rules_lhs )
# The results indicate that if a customer buys soda, he is likely to buy rolls/buns as well: 
#{soda} => {rolls/buns} . This is because the lift is above 1.
