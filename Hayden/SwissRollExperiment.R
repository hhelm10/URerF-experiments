source("swiss_roll.R")
source("precision_recall.R")
source("tree_distance.R")

# install.packages('rerf')
# print("I installed 'rerf'!")
# install.packages('stats')
# install.packages('rgl', 'MASS')
# install.packages('Matrix', 'scatterplot3d', 'vegan', 'randomForest')
library(rerf)
library(stats)
library(rgl)
library(MASS)
library(Matrix)
library(scatterplot3d)
library(vegan)
library(randomForest)

num_of_points=1000

data=SwissRoll(num_of_points, Height=20, Plot = TRUE)[[1]]

plot3d(data[1:(num_of_points/5),1], data[1:(num_of_points/5),2], data[1:(num_of_points/5),3], col='red', size=1)
plot3d(data[(num_of_points/5+1):(2*num_of_points/5),1], data[(num_of_points/5+1):(2*num_of_points/5),2], data[(num_of_points/5+1):(2*num_of_points/5),3], col='green', size=3, add=TRUE)
plot3d(data[(2*num_of_points/5+1):(3*num_of_points/5),1], data[(2*num_of_points/5+1):(3*num_of_points/5),2], data[(2*num_of_points/5+1):(3*num_of_points/5),3], col='orange', size=3, add=TRUE)
plot3d(data[(3*num_of_points/5+1):(4*num_of_points/5),1], data[(3*num_of_points/5+1):(4*num_of_points/5),2], data[(3*num_of_points/5+1):(4*num_of_points/5),3], col='blue', size=3, add=TRUE)
plot3d(data[(4*num_of_points/5+1):(num_of_points),1], data[(4*num_of_points/5+1):(num_of_points),2], data[(4*num_of_points/5+1):(num_of_points),3], size=3, add=TRUE)

data_label= c(rep('1', num_of_points/5), rep('2', num_of_points/5), rep('3', num_of_points/5), rep('4', num_of_points/5), rep('5', num_of_points/5))
at_K=seq(10, 100, by=10)


min_par = seq(1, 19, by = 20)
D_list1=list()
D_list2=list()
D_list3=list()
index=1

n_trees <- 300
for (i in min_par){
  g=Urerf(data, trees = n_trees, Progress = TRUE, splitCrit = "bicfast", min.parent = i, mtry = 3)
  
  W=g$similarityMatrix
  D=as.matrix(1-W)
  D_list1[[index]] = D
  
  tree_dist <- tree_distance(g, type = "no_transform")
  D_list2[[index]] = tree_dist
  
  D_list3[[index]] = isomap_heuristic(tree_dist)
  
  index=index+1
}

# tree_dist <- tree_distance(g, type = "power")

urerf_precision_list1 = list()
urerf_precision_list2 = list()
urerf_precision_list3 = list()
index = 1
for (D in D_list2){
  
  # 0-1
  D_p1 = p_r_list(D_list1[[index]], data_label, at_K, num_of_points)$precisionList
  urerf_precision_list1[[index]] =  D_p1
  
  # distance
  D_p2 = p_r_list(D_list2[[index]], data_label, at_K, num_of_points)$precisionList
  urerf_precision_list2[[index]] = D_p2
  
  # isomap(distance) 
  D_p3 = p_r_list(D_list3[[index]], data_label, at_K, num_of_points)$precisionList
  urerf_precision_list3[[index]] = D_p3
  
  index = index + 1
}

## isomap
D_eucd = as.matrix(dist(data))
iso_precision_list=list()

iso_dist = isomap_heuristic(D_eucd)
iso_map_precision=p_r_list(iso_dist, data_label, at_K, num_of_points)$precisionList
iso_precision_list[[1]] = iso_map_precision

save(urerf_precision_list1, file="og_SR_minpar1_height20.Rdata")
save(urerf_precision_list2, file="distance_SR_height20.Rdata")
save(urerf_precision_list2, file="distanceiso_SR_height20.Rdata")
save(iso_precision_list, file = "iso_SR_height20.Rdata")
