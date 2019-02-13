#normalized, with rotation, default urerf parameters, with noise, but smaller rotation and smaller noise
library(scatterplot3d)
library(rgl)
library(rerf)
library(umap)
library(vegan)
library(Matrix)
source("precision_recall.R")

set.seed(1)

N=1000
num_of_points=N
Height=100
p = (3 * pi / 2) * (1 + 2*sort(runif(N, 0, 1)));  
samples = as.matrix(cbind(cbind(2*p*cos(2*p), 2*p*sin(2*p)), 2*p))
plot3d(samples[,1], samples[,2], samples[,3])
data=samples

highdimnoise<-function(high_dim){
  matrix_of_0 = matrix(rep(0, num_of_points*(high_dim-3)), nrow = num_of_points, ncol = high_dim -3)
  high_dim_data = cbind(data, matrix_of_0)
  dim <- high_dim
  cov_matrix = matrix(rep(0, dim*dim), nrow = dim, ncol = dim)
  diag(cov_matrix) = c(0, 0, 0, rep(7000, high_dim-3))
  Sig1 = cov_matrix
  noise = mvrnorm(n = num_of_points, (rep(0, high_dim)), Sig1, tol = 1e-7, empirical = FALSE, EISPACK = FALSE)
  high_dim_noise_data = high_dim_data + noise
  rownames(high_dim_noise_data) <- c()
  colnames(high_dim_noise_data) <- c()
  return(high_dim_noise_data)
}

data_label= c(rep('1', num_of_points/5), rep('2', num_of_points/5), rep('3', num_of_points/5), rep('4', num_of_points/5), rep('5', num_of_points/5))
at_K=seq(5, 45, by=10)

high_dim=9

high_dim_noise_data=highdimnoise(high_dim)
rota=replicate(high_dim, rep(1,high_dim))
rota_high_dim_noise_data=high_dim_noise_data%*%rota

normalizeData <- function(X) {
  X <- sweep(X, 2, apply(X, 2, min), "-")
  sweep(X, 2, apply(X, 2, max), "/")
}
rota_high_dim_noise_data=normalizeData(rota_high_dim_noise_data)


g_noise1=Urerf(rota_high_dim_noise_data, trees = 300, Progress = TRUE, splitCrit = "bicfast")
W_noise1=g_noise1$similarityMatrix
D_rf_noise1=1-W_noise1
D_rf_noise1_p_r_list = p_r_list(D_rf_noise1, data_label, at_K, num_of_points)
D_rf_noise1_precision_list= D_rf_noise1_p_r_list$precisionList
D_rf_noise1_recall_list=D_rf_noise1_p_r_list$recallList

# generate D_eucd
D_eucd_noise1 = as.matrix(dist(rota_high_dim_noise_data))
iso_dist_noise1 = as.matrix(isomapdist(D_eucd_noise1, k=10))
D_iso_noise1_p_r_list = p_r_list(iso_dist_noise1, data_label, at_K, num_of_points)
D_iso_noise1_precision_list= D_iso_noise1_p_r_list$precisionList
D_iso_noise1_recall_list=D_iso_noise1_p_r_list$recallList

#umap
custom.settings = umap.defaults
custom.settings$n_neighbors=length(data_label)
a_noise1 = umap(rota_high_dim_noise_data, config = custom.settings)
D_umap_noise1=as.matrix(dist(a_noise1$layout))
D_umap_noise1_p_r_list = p_r_list(D_umap_noise1, data_label, at_K, num_of_points)
D_umap_noise1_precision_list= D_umap_noise1_p_r_list$precisionList
D_umap_noise1_recall_list=D_umap_noise1_p_r_list$recallList


save(D_rf_noise1_precision_list, D_iso_noise1_precision_list, D_umap_noise1_precision_list, file="exp_2_g.Rdata")
