# Eve-Central-Market-Data-RStudio
---
title: "Eve Central Market Data"
author: "Matt Tharby - Mohammed Mamdani"
date: "November 6, 2015"
output: html_document
---

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r}

library(data.table)
library(plyr)
library(dplyr)
library(fpc)
library(class)
library(tree)

```

Download csv from http://dev.eve-central.com/evec-api/start, the section at the bottom. They come in a gzip format and are roughly 1.5gb each. This filters data from the csv into workable chunks


```{r}

importedCSV = data.table(fread("2015-11-05.csv")) #import all data

head(importedCSV) #check data
manip1 = subset(importedCSV,bid==0) #sell orders only
rm(importedCSV) #dump all import from environment to save memory

setkey(manip1,orderid,reportedtime) #set key to unique order ID and reported time

manip1[1:15,]

manip2 = subset(manip1,duration!="365 days, 0:00:00")

setkey(manip2,orderid,reportedtime)

manip2[1:15,]

manip1 <- manip2[,.SD[.N], by = "orderid"]
manip1 <- as.data.table(manip1)

manip3 <- manip2[,.SD[1L], by = "orderid"]
manip3 <-as.data.table(manip3)

setkey(manip1,orderid)
setkey(manip3,orderid)

head(manip1) # last orders
head(manip3) # first orders
```

This exports useful data to smaller more manageable CSV which can be exported and imported. I would like to find a way to export them with a unique name each time this runs on a different set. 

```{r}
write.csv(manip3, file = "firstOrder.csv")
write.csv(manip1, file = "lastOrder.csv")


firstOrderTable <- fread("firstOrder.csv")
last <- fread.csv("lastOrder.csv")
head(firstOrderTable)
```

begin data manipulation within the table

```{r}

firstOrderTable.asNumerics <- data.table(as.numeric(firstOrderTable$price), as.numeric(firstOrderTable$typeid), as.numeric(firstOrderTable$volremain))

firstOrderTable.wregion <- data.table(as.numeric(firstOrderTable$price), as.numeric(firstOrderTable$typeid), as.numeric(firstOrderTable$volremain),firstOrderTable$regionid)


colnames(firstOrderTable.asNumerics) <- c("price", "typeid", "volremain")
colnames(firstOrderTable.wregion) <- c("price", "typeid", "volremain","regionid")

firstOrderTable.wregion <- filter(firstOrderTable.wregion, typeid >= 34 & typeid <= 40)


price.wm.table <- ddply(firstOrderTable.asNumerics,.(typeid),summarise, price.weighted.mean = weighted.mean(price,volremain))

price.wm.table.region <- ddply(firstOrderTable.wregion,.(typeid,regionid),summarise, price.weighted.mean = weighted.mean(price,volremain))

first.volume.sum <- aggregate(firstOrderTable.asNumerics$volremain, list(typeid=firstOrderTable.asNumerics$typeid), sum)

first.volume.region.sum <- aggregate(firstOrderTable.wregion$volremain, list(regionid=firstOrderTable.wregion$regionid,typeid=firstOrderTable.wregion$typeid),sum)

comb.first.table <- data.table(as.factor(first.volume.sum$typeid), first.volume.sum$x, price.wm.table$price.weighted.mean)
comb.region.table <- data.table(as.factor(first.volume.region.sum$regionid),as.factor(first.volume.region.sum$typeid),first.volume.region.sum$x,price.wm.table.region$price.weighted.mean)
colnames(comb.first.table) <- c("typeid","volremain","wm.price")
colnames(comb.region.table) <- c("regionid","typeid", "volremain", "wm.price")
head(comb.first.table)

region.to.npc <- read.csv("region_to_npc_status.csv")

comb.region.table.test <- data.table(merge(data.frame(comb.region.table),y=region.to.npc[,c("regionid","npc.controlled")], by = "regionid"),key="typeid")

comb.region.table.test <- data.table(as.character(comb.region.table.test$regionid),as.character(comb.region.table.test$typeid),comb.region.table.test$volremain,comb.region.table.test$wm.price,as.factor(comb.region.table.test$npc.controlled))

colnames(comb.region.table.test) <- c("regionid","typeid", "volremain", "wm.price","npc.controlled")

```

analysis below

```{r}

kTrainingSize = .7
set.seed(1)
training.rows <- sample(1: nrow(comb.region.table.test), as.integer(kTrainingSize * nrow(comb.region.table.test)))

tree.results.npc <- tree(comb.region.table.test$npc.controlled ~ wm.price + volremain, data=comb.region.table.test)

plot(tree.results.npc); text(tree.results.npc)
summary(tree.results.npc)

tree.results.typeid <- tree(as.factor(comb.region.table.test$typeid) ~ wm.price + volremain, data=comb.region.table.test)

plot(tree.results.typeid); text(tree.results.typeid)
summary(tree.results.typeid)
```


```{r}


kcluster1 <- kmeans(comb.region.table.test[,], centers = 7, iter.max = 250)
plot(comb.region.table.test$volremain~comb.region.table.test$wm.price,comb.first.table,col=kcluster1$cluster,xlab="Weighted Mean Price",ylab="Volume")

kcluster1
```

