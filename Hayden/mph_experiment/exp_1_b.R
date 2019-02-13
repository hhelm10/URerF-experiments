# no normalization
# increase mtry
library(scatterplot3d)
library(rgl)
library(rerf)
library(umap)
library(vegan)
source("precision_recall.R")

N=1000
num_of_points=N
Height=100
p = (3 * pi / 2) * (1 + 2*sort(runif(N, 0, 1)));  
samples = as.matrix(cbind(cbind(2*p*cos(2*p), 2*p*sin(2*p)), 2*p))
plot3d(samples[,1], samples[,2], samples[,3])

rota_high=as.matrix(rbind( rbind(c(12,13,14), c(0,11,17)), c(0,0,50)))
rota_samples=samples%*%rota_high
data=rota_samples
plot3d(rota_samples[,1], rota_samples[,2], rota_samples[,3])

data_label= c(rep('1', num_of_points/5), rep('2', num_of_points/5), rep('3', num_of_points/5), rep('4', num_of_points/5), rep('5', num_of_points/5))
at_K=seq(5, 45, by=10)

#isomap
D_eucd = as.matrix(dist(data))
iso_dist = isomap_heuristic(D_eucd)
D_iso_p_r_list = p_r_list(iso_dist, data_label, at_K, num_of_points)
D_iso_precision_list= D_iso_p_r_list$precisionList
D_iso_recall_list=D_iso_p_r_list$recallList


#umap
custom.settings = umap.defaults
custom.settings$n_neighbors=length(data_label)
a = umap(data, config = custom.settings)
D_umap=as.matrix(dist(a$layout))
D_umap_p_r_list = p_r_list(D_umap, data_label, at_K, num_of_points)
D_umap_precision_list= D_umap_p_r_list$precisionList
D_umap_recall_list=D_umap_p_r_list$recallList

#urerf
g=Urerf(data, trees = 300, Progress = TRUE, mtry=3, splitCrit = "bicfast")
W=g$similarityMatrix
D_rf=1-W
D_rf_p_r_list = p_r_list(D_rf, data_label, at_K, num_of_points)
D_rf_precision_list= D_rf_p_r_list$precisionList
D_rf_recall_list=D_rf_p_r_list$recallList

#urerf2
power_tree_dist <- tree_distance(g, type = "power")
D_rf2_p_r_list = p_r_list(power_tree_dist, data_label, at_K, num_of_points)
D_rf2_precision_list= D_rf2_p_r_list$precisionList
D_rf2_recall_list=D_rf2_p_r_list$recallList

#urerf3
tree_dist <- tree_distance(g, type = "no_transform")
D_rf3_p_r_list = p_r_list(power_tree_dist, data_label, at_K, num_of_points)
D_rf3_precision_list= D_rf3_p_r_list$precisionList
D_rf3_recall_list=D_rf3_p_r_list$recallList

save(D_iso_p_r_list, file = "mph_experiment/exp_1_b_iso.RData")
save(D_umap_p_r_list, file = "mph_experiment/exp_1_b_umap.RData")
save(D_rf_p_r_list, file = "mph_experiment/exp_1_b_rf.RData")
save(D_rf2_p_r_list, file = "mph_experiment/exp_1_b_power.RData")
save(D_rf3_p_r_list, file = "mph_experiment/exp_1_b_td.RData")

