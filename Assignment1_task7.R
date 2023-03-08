library(ggplot2)
library(tidyverse)
#1-a. 
#load magic_guys data
magic_guys <- read.csv(file = "/Users/yasuhikohaga/desktop/KI-UT_GIT/magic_guys.csv", header = TRUE)
#make plot
plot1_a_1 <- ggplot(magic_guys, aes(x = length)) + geom_histogram(fill = "white", colour = "black", bins = 30) + facet_grid(species ~ .)
plot1_a_1#plot hist independently
plot1_a_2 <- ggplot(magic_guys, aes(x = length, fill = species)) + geom_histogram(position = "identity", alpha = 0.4, bins = 30)
plot1_a_2#plot hist over-lapping

#1-b
plot1_b <- ggplot(magic_guys, aes(y = length, x = species, fill = species)) + geom_boxplot() + geom_violin(alpha=0.2)
plot1_b

#1-c
#png
png("/Users/yasuhikohaga/desktop/KI-UT_GIT/magic_guy_hist_overlapping.png", width = 1024, height = 1024)
ggplot(magic_guys, aes(x = length, fill = species)) + geom_histogram(position = "identity", alpha = 0.4, bins = 30)
dev.off()
#pdf
pdf("/Users/yasuhikohaga/desktop/KI-UT_GIT/magic_guy_hist_overlapping.pdf", width = 4, height = 4)
ggplot(magic_guys, aes(x = length)) + geom_histogram(fill = "white", colour = "black", bins = 30) + facet_grid(species ~ .)
dev.off()
#svg
install.packages("svglite")
library(svglite)
ggsave("/Users/yasuhikohaga/desktop/KI-UT_GIT/magic_guy_hist_overlapping.svg", width = 8, height = 8, units = "cm")
#PNG format usually has more data size than PDF format. But, when plotting point plots, PDF format often has more data size than PNG format.
#Because PDF format has information that is a mass of drawing order, and PNG format only has color information of bits.
#SVG format is better used for a web browser.

#2. 
#load microarray data
microarray <- read.table("/Users/yasuhikohaga/desktop/KI-UT_GIT/microarray_data.tab", sep = "\t", header = TRUE)
#2-a
nrow(microarray) #553
ncol(microarray) #1000

#2-b
#row is gene
counted_missing_values <- matrix(data = NA, ncol = 2, nrow = ncol(microarray))
rownames(counted_missing_values) <- colnames(microarray)
colnames(counted_missing_values) <- c("missing_number", "expressed_gene_number")
#using for loop
for (i in colnames(microarray)) {
  counted_missing_values[i,1] <- sum(is.na(microarray[,i]))
  counted_missing_values[i,2] <- nrow(microarray) - sum(is.na(microarray[,i]))
}
ggplot(as.data.frame(counted_missing_values), aes(x = missing_number)) + geom_bar()
ggplot(as.data.frame(counted_missing_values), aes(x = missing_number)) + geom_histogram()

#2-c
genes_miss_10per <- c()
genes_miss_20per <- c()
genes_miss_50per <- c()
for (i in rownames(counted_missing_values)) {
  missing_percent <- counted_missing_values[i,1] / (counted_missing_values[i,1] + counted_missing_values[i,2])
  if (missing_percent > 0.5) {
    genes_miss_50per[length(genes_miss_50per) + 1] <- i
  } else if (missing_percent > 0.2) {
    genes_miss_20per[length(genes_miss_20per) + 1] <- i
  } else if (missing_percent > 0.1) {
    genes_miss_10per[length(genes_miss_10per) + 1] <- i
  }
}
#genes_miss_10per, genes_miss_20per, genes_miss_50per are the answer.

#2-d
replaced_microarray <- microarray
for (i in colnames(microarray)) {
  mean_expression <- mean(microarray[,i], na.rm =TRUE)
  replaced_microarray[which(is.na(replaced_microarray[,i])),i] <- mean_expression
}
#replaced_microarray is the answer

#3
#load CO2 data
data("CO2")
#compare uptake between Plants
ggplot(CO2, aes(x = Treatment, y = uptake, fill = Treatment)) + geom_boxplot()
wilcox.test(x = CO2[CO2[,"Treatment"]=="nonchilled","uptake"], y = CO2[CO2[,"Treatment"]=="chilled","uptake"])
#There is significant difference between nonchilled and chilled. (p_val = 0.006358, by wilcox.test)
#It is inferred that chilled treatment decrease plant's CO2 uptake.




