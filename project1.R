#Jiayuan Xu
# packages installment.
library(ggplot2)
library(hexbin)
library(RColorBrewer)
library(factoextra)
library(cluster)
library(NbClust)
library(tidyverse)
library(reshape2)
library(gridExtra)
library(data.table)
require(reshape2)

# Csv import.
FordKaDemographicData <- read.csv("FordKaDemographicData.csv")
attach(FordKaDemographicData)
FordKaPsychographicData <- read.csv("FordKaPsychographicData.csv")
attach(FordKaDemographicData)

combined <- merge(FordKaPsychographicData,FordKaDemographicData,by="Respondent")
selected_FordKaPsychographicData = combined[,c(2:26,28:30,32:38,40:43,45:55,57:61,64)]

#clusters vs preference group
ggplot(selected_FordKaPsychographicData, aes(x=PreferenceGroup, y=cluster) ) +
   geom_hex(bins = 13) +
   xlim(0.5, 3.5) +
   ylim(0.5, 4.5) + 
   theme(text = element_text(face = "bold", size = 12)) + 
   xlab("Preference Group") + ylab("Cluster") +
   scale_fill_gradientn(colors = brewer.pal(9,"Blues"))+
   ggtitle("Clusters vs PreferenceGroup of Ford Ka Psychographic Data")

# Finding Optimal Number of Clusters for PsychographicData.
fviz_nbclust(selected_FordKaPsychographicData, kmeans, method = "wss")+
	geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

#combined$mean = rowMeans(combined[,c('Q1', 'Q2')], na.rm=TRUE)

# Plot hexbin for preferencegroup and q1.
base <- ggplot(combined, aes(x=PreferenceGroup, y=Q1) ) +
  geom_hex(bins = 13) +
  xlim(0.5, 3.5) +
  ylim(0.5, 7.5) + 
  theme(text = element_text(face = "bold", size = 12)) + 
  xlab("Preference Group") + ylab("Q1") +
  scale_fill_gradientn(colors = brewer.pal(9,"Reds"))
base + guides(fill = guide_colorbar(barheight = unit(10, "cm")))

#Fashion and Trendy group
Fashion_and_Trendy = combined[,c(2:3,18,30,45:47,49,52,55,58)]

#Practical group 
Practical = combined[,c(5,8:11,13,15,17,25,32:35,38,54,60)]

#Emotional group
Emotional = combined[,c(4,6,14,16,24,28:29,36:37,43,48,51,53,59)]

#Rational group
Rational = combined[,c(7,12,19:22,26,50,57)]

#Prefer Small Cars group
Prefer_Small_Cars = combined[,c(23,40:42,63)]


# finding the kmeans of Psychographicdataset.
num_of_k = 4
k <- kmeans(selected_FordKaPsychographicData, centers = num_of_k, nstart=31, iter.max=30, algorithm="Lloyd")

# Add cluster number column.
selected_FordKaPsychographicData$cluster = k$cluster
#iris

# Each questions' means for each clusters. 
list_by_cluster_Psy = aggregate(selected_FordKaPsychographicData, by=list(cluster=k$cluster), mean)

#Bar graph of means for cluster 2
Mean2 = colMeans(list_by_cluster_Psy[2,])[2:35]
dc2 = data.frame(Mean2)
#dc2[order(-Mean2),]
Each_Questions_Mean2 = setDT(dc2, keep.rownames = TRUE)[]
colnames(Each_Questions_Mean2)[1] = "Questions"
melted2 = melt(Each_Questions_Mean2,"Questions")

ggplot(melted2, aes(x = Questions, y=value)) +
  geom_bar(aes(fill=variable),stat="identity", position="dodge",width = .5) + 
  theme_bw()+ 
  theme(axis.text.x = element_text(angle=120, hjust=.1),plot.margin = unit(c(.1,.1,.1,.1),"cm"))+
  geom_hline(yintercept = 4)+
  scale_fill_manual(values=c("#56B4E9"))+
  ggtitle("Cluster 2's Mean Value of Psychographic Data")

#Bar graph of means for cluster 3
Mean3 = colMeans(list_by_cluster_Psy[3,])[c(2:5,24,38:56)]
dc3 = data.frame(Mean3)
#dc3[order(-Mean3),]
Each_Questions_Mean3 = setDT(dc3, keep.rownames = TRUE)[]
colnames(Each_Questions_Mean3)[1] = "Questions"
melted3 = melt(Each_Questions_Mean3,"Questions")

ggplot(melted3, aes(x = Questions, y=value)) +
  geom_bar(aes(fill=variable),stat="identity", position="dodge",width = .5) + 
  theme_bw()+ 
  theme(axis.text.x = element_text(angle=120, hjust=.1),plot.margin = unit(c(.1,.1,.1,.1),"cm"))+
  geom_hline(yintercept = 4)+
  scale_fill_manual(values=c("#56B4E9"))+
  ggtitle("Cluster 3's Mean Value of Psychographic Data")




# plot the data points according to the first two principal components.
fviz_cluster(k, data = selected_FordKaPsychographicData, ellipse.type = "norm")


############################################################################
selected_FordKaDemographicData = scale(FordKaDemographicData[,3:10])

# Finding Optimal Number of Clusters for DemographicDataset.
fviz_nbclust(selected_FordKaDemographicData, kmeans, method = "wss")+
	geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")


# finding the kmeans of DemographicDataset.
num_of_k_Demo = 4
k_Demo <- kmeans(selected_FordKaDemographicData, centers = num_of_k_Demo, nstart=4, iter.max=30, algorithm="Lloyd")
#k_Demo$centers
# Add cluster number column.
FordKaDemographicData$cluster = k_Demo$cluster

#clusters vs preference group
ggplot(FordKaDemographicData, aes(x=PreferenceGroup, y=cluster) ) +
   geom_hex(bins = 13) +
   xlim(0.5, 3.5) +
   ylim(0.5, 4.5) + 
   theme(text = element_text(face = "bold", size = 12)) + 
   xlab("Preference Group") + ylab("Cluster") +
   scale_fill_gradientn(colors = brewer.pal(9,"Blues"))+
   ggtitle("Clusters vs PreferenceGroup of Demographic Data")

# Each questions' means for each clusters. 
list_by_cluster_Demo = aggregate(selected_FordKaDemographicData, by=list(cluster=k_Demo$cluster), mean)
list_by_cluster_Demo1 = list_by_cluster_Demo[1,][c(2,3,4,5,9)]
list_by_cluster_Demo2 = list_by_cluster_Demo[2,][c(2,3,4,5,9)]
list_by_cluster_Demo3 = list_by_cluster_Demo[3,][c(2,3,4,5,9)]
list_by_cluster_Demo4 = list_by_cluster_Demo[4,][c(2,3,4,5,9)]

#bar graph of mean of cluster 1
d1=data.frame(colMeans(list_by_cluster_Demo1))
cluster1 = setDT(d1, keep.rownames = TRUE)[]
colnames(cluster1)[1] = "cluster1"
colnames(cluster1)[2] = "Value"
melted1 = melt(cluster1,"cluster1")

ggplot(melted1, aes(x = cluster1, y=value)) +
  geom_bar(aes(fill=variable),stat="identity", position="dodge",width = .7) + 
  theme_bw()+ 
  theme(axis.text.x = element_text(face = "bold",angle=-30, hjust=.1),plot.margin = unit(c(.1,.1,.1,.1),"cm"))+
  scale_fill_manual(values=c("#E69F00"))+
  ylim(-1, 2)


#bar graph of mean of cluster 2
d2=data.frame(colMeans(list_by_cluster_Demo2))
cluster2 = setDT(d2, keep.rownames = TRUE)[]
colnames(cluster2)[1] = "cluster2"
colnames(cluster2)[2] = "Value"
melted2 = melt(cluster2,"cluster2")

ggplot(melted2, aes(x = cluster2, y=value)) +
  geom_bar(aes(fill=variable),stat="identity", position="dodge",width = .7) + 
  theme_bw()+ 
  theme(axis.text.x = element_text(face = "bold",angle=-30, hjust=.1),plot.margin = unit(c(.1,.1,.1,.1),"cm"))+
  scale_fill_manual(values=c("#009E73"))+
  ylim(-1, 2)



#bar graph of mean of cluster 3
d3=data.frame(colMeans(list_by_cluster_Demo3))
cluster3 = setDT(d3, keep.rownames = TRUE)[]
colnames(cluster3)[1] = "cluster3"
colnames(cluster3)[2] = "Value"
melted3 = melt(cluster3,"cluster3")

ggplot(melted3, aes(x = cluster3, y=value)) +
  geom_bar(aes(fill=variable),stat="identity", position="dodge",width = .7) + 
  theme_bw()+ 
  theme(axis.text.x = element_text(face = "bold",angle=-30, hjust=.1),plot.margin = unit(c(.1,.1,.1,.1),"cm"))+
  scale_fill_manual(values=c("#D55E00"))+
  ylim(-1, 2)


#bar graph of mean of cluster 4
d4=data.frame(colMeans(list_by_cluster_Demo4))
cluster4 = setDT(d4, keep.rownames = TRUE)[]
colnames(cluster4)[1] = "cluster4"
colnames(cluster4)[2] = "Value"
melted4 = melt(cluster4,"cluster4")

ggplot(melted4, aes(x = cluster4, y=value)) +
  geom_bar(aes(fill=variable),stat="identity", position="dodge",width = .7) + 
  theme_bw()+ 
  theme(axis.text.x = element_text(face = "bold",angle=-30, hjust=.1),plot.margin = unit(c(.1,.1,.1,.1),"cm"))+
  scale_fill_manual(values=c("#CC79A7"))+
  ylim(-1, 2)

# plot the data points according to the first two principal components.
fviz_cluster(k_Demo, data = selected_FordKaDemographicData, ellipse.type = "norm")
