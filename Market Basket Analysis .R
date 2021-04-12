## ---------------------------
#Load Packages 
## ---------------------------
#arules - Is a package for analyzing transactional data. 
#arulesViz - Is a package that provides visual techniques for the arules package.
library(arules)
library(arulesViz)

## ---------------------------
#Install, Upload, and Get to Know Your Dataset
## ---------------------------
mb<-file.choose()
tr<-read.transactions(mb, format = "basket", sep=",",encoding="unknown", rm.duplicates=FALSE)



#mb1<-file.choose()
#td<-read.transactions(mb1, format = "basket", sep=",",encoding="unknown", rm.duplicates=TRUE)
#summary(td)
#density: The percentage of non-empty cells in the sparse matrix. 
#In another words, the total number of items that are purchased divided by the total number of possible items in that matrix. 
#We can calculate how many items were purchased using density like so: 9835*125*0.035 = 43,028
summary(tr)
## make always sure that the items were properly separated
itemLabels(tr)
inspect(tr) # You can view the transactions. Is there a way to see a certain # of transactions?
length(tr) # Number of transactions.
size(tr) # Number of items per transaction
LIST(tr) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(tr)# To see the item labels


## ---------------------------
#Visualize Your Dataset
## ---------------------------

itemFrequencyPlot(tr, topN=20, type='absolute')
image(sample(tr, 98))



## ---------------------------
#Apply the Apriori Algorithm
## ---------------------------
#We use the Apriori algorithm in Arules library to mine frequent itemsets and association rules.
#The algorithm employs level-wise search for frequent itemsets.
#We pass supp=0.001 and conf=0.8 to return all the rules that have a support of at least 0.1% and confidence of at least 80%.
#We sort the rules by decreasing confidence.
#Have a look at the summary of the rules.
tr_rules<- apriori(tr)
rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))

rules <- sort(rules, by='confidence', decreasing = TRUE)
rules_sup <- sort(rules, by='support', decreasing = TRUE)
rules_lift <- sort(rules, by='lift', decreasing = TRUE)

singleitem<-apriori(tr,parameter=list(support=0.001,confidence=0.1,minlen=1,maxlen=1))
summary(singleitem)
inspect(singleitem[1:10])

#purchased only one product
oneCat <- tr[which(size(tr) == 1), ]
length(oneCat) #2163
barplot(sort(itemFrequency(oneCat, type="absolute"), decreasing=T))


#rules1 <- apriori(td, parameter = list(supp=0.001, conf=0.8))
#rules1 <- sort(rules1, by='confidence', decreasing = TRUE)
#summary(rules1)

#Support measurement, which measures itemsets or rules frequency within your transactional data
#The second statistical measure is the Confidence measurement, which measures the accuracy of the rules.
#When analyzing the Confidence measurement, keep in mind that correlation does not imply causation

#minval is the minimum value of the support an itemset should satisfy to be a part of a rule.
#smax is the maximum support value for an itemset.

#arem is an Additional Rule Evaluation Parameter. In the above code we have constrained the number of rules using Support and Confidence. 
#There are several other ways to constrain the rules using the arem parameter in the function and we will discuss more about it later in the article.

#aval is a logical indicating whether to return the additional rule evaluation measure selected with arem.
#originalSupport The traditional support value only considers both LHS and RHS items for calculating support. If you want to use only the LHS items for the calculation then you need to set this to FALSE.
#maxtime is the maximum amount of time allowed to check for subsets.
#minlen is the minimum number of items required in the rule.
#maxlen is the maximum number of items that can be present in the rule.

#minlen=2 
#rules_min2 <- apriori(tr, parameter = list(supp=0.001, conf=0.8, minlen=2))
#rules_min2  <- sort(rules_min2, by='confidence', decreasing = TRUE)
#summary(rules_min2)
#inspect(rules_min2[1:10])
## ---------------------------
#Evaluate Your Model
## ---------------------------
summary(rules)
summary(rules_sup)
summary(rules_lift)

inspect(rules[1:10])
inspect(rules_sup[1:10])
inspect(rules_lift[1:10])


#subset
ItemRules_1 <- subset(rules, items %in% "iMac")
inspect(ItemRules_1[1:10])

topRules <- rules
plot(topRules)
plot(topRules[1:10], method="graph")
plot(topRules, method = "grouped")

## ---------------------------
#Improve Your Model
## ---------------------------
#To Remove Redundant Rules 
is.redundant(rules)
subsetRules <- which(colSums(is.subset(rules, rules)) > 1) # get subset rules in vector
length(subsetRules)  #> 208
plot(subsetRules)
rules <- rules[-subsetRules] # remove subset rules. 

summary(rules)
inspect(rules[1:10])
plot(rules, method = "grouped")

# Second, get itemsets of length 2
itemsets<-apriori(tr,parameter=list(support=0.001,confidence=0.8,minlen=2))
summary(itemsets)
inspect(head(itemsets,5))
inspect(head(sort(itemsets,by="lift"),10))


#Most frequent data -http://r-statistics.co/Association-Mining-With-R.html
frequentItems <- eclat(tr, parameter = list(supp = 0.001, maxlen = 25)) # calculates support for frequent items
inspect(frequentItems)
itemFrequencyPlot(tr, topN=20, type="absolute", main="Item Frequency") 


#How to get the product recommendation rules?
rules_conf <- sort (rules, by="confidence", decreasing=TRUE)
inspect(rules_conf[1:10])

rules_lift <- sort (rules, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(rules_lift[1:10])
#The rules with confidence of 1 (see rules_conf above) imply that, whenever the LHS item was purchased, the RHS item was also purchased 100% of the time.
#A rule with a lift of 10 (see rules_lift above) imply that, 
#the items in LHS and RHS are 10 times more likely to be purchased together compared to the purchases 
#when they are assumed to be unrelated.

# How To Control The Number Of Rules in Output ?
#Adjust the maxlen, supp and conf arguments in the apriori function to control the number of rules generated. 

#How to Find Rules Related To Given Item/s ?
rules3 <- apriori (data=tr, parameter=list (supp=0.001,conf = 0.8), appearance = list (default="lhs",rhs="iMac"), control = list (verbose=F))
rules_conf1 <- sort (rules3, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf1))

#To find out what products were purchased after/along with product X
rules4 <- apriori (data=tr, parameter=list (supp=0.001,conf = 0.1,minlen=2), appearance = list(default="rhs",lhs="iMac"), control = list (verbose=F)) # those who bought 'milk' also bought..
rules_conf2 <- sort (rules4, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf2))


## ---------------------------
#Visualize Your Results
## ---------------------------

plot(rules3, measure=c("support", "confidence"), shading="lift", main="Scatterplot of  Rulesets")

plot(rules[1:10], method="graph", control=list(type="items")) 
                 

## ---------------------------
## Category file 
## ---------------------------
c <- file.choose()
cat <- read.csv(c, sep=",")
summary(cat)
tr1<-read.transactions(mb, format = "basket", sep=",",encoding="unknown", rm.duplicates=FALSE)
#remove "" as they are not necessary and may give  wrong results
#The gsub() function in R is used for replacement operations. The functions takes the input and substitutes it against the specified values.
tr1@itemInfo$labels <- gsub("\"","",tr1@itemInfo$labels)
#add level1 to categories our products
tr1@itemInfo$level1 <- cat$Categories
head(tr1@itemInfo$labels)
head(tr1@itemInfo$level1)
head(tr1)
trByType <- aggregate(tr1, by= tr1@itemInfo$level1)
itemFrequencyPlot(trByType,
                  topN=20,
                  #col=brewer.pal(8,'Pastel2'),
                  main='Product Category plot',
                  type="absolute",
                  ylab="Item Frequency ")


finalrules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
dup<-is.redundant(finalrules)
summary(dup)
finalrules <- finalrules[!dup] # remove dup rules. 
summary(finalrules)

#10 rule set strongest and most frequent -sort by support

mostfrequent <- sort(finalrules, by='support', decreasing = TRUE)
inspect(mostfrequent[1:10])

#sort by confidence - 10 rule set highest confidence 

mostconf <- sort(finalrules, by='confidence', decreasing = TRUE)
inspect(mostconf[1:10])

#10 rule set strongest, higher frequency, and confident 


mostlift <- sort(finalrules, by='lift', decreasing = TRUE)
inspect(mostlift[1:10])



