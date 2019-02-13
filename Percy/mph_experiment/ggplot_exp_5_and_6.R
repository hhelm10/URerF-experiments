# new ggplot
library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
load("exp7.Rdata")


pdf("exp77.pdf")
#at_K = seq(5, 80, by=10)
at_K = seq(1, 10, by=2)
#####################################################################################
D_rf_noise1_precision_list= final_prec_rf
print (D_rf_noise1_precision_list)
D_rf_noise1_recall_list= final_rec_rf
print (D_rf_noise1_recall_list)

#####################################################################################
D_iso_noise1_precision_list= final_prec_iso
D_iso_noise1_recall_list=final_rec_iso
print (D_iso_noise1_precision_list)
print (D_iso_noise1_recall_list)

###################################################################
D_umap_noise1_precision_list= final_prec_umap 
D_umap_noise1_recall_list= final_rec_umap
print (D_umap_noise1_precision_list)
print (D_umap_noise1_recall_list)

precision_df1=data.frame(at_K, prec=D_rf_noise1_precision_list, Algo = as.factor("URerf"), DIM =as.factor("d'=6"))
precision_df2=data.frame(at_K, prec=D_iso_noise1_precision_list, Algo = as.factor("Isomap"), DIM =as.factor("d'=6"))
precision_df3=data.frame(at_K, prec=D_umap_noise1_precision_list, Algo = as.factor("UMAP"), DIM =as.factor("d'=6"))
precision_df=rbind(precision_df1, precision_df2, precision_df3)
p <- ggplot(precision_df, aes(at_K, prec, colour = Algo, shape = DIM)) + geom_line(alpha=0.9, show.legend = FALSE) + geom_point(alpha=0.9, show.legend = FALSE)  + xlab('noise_mag') + ylab('Precision') + scale_color_brewer(palette="Dark2")
#plot(p)                   

recall_df1=data.frame(at_K, reca=D_rf_noise1_recall_list, Algo = as.factor("URerf"), DIM =as.factor("d'=6"))
recall_df2=data.frame(at_K, reca=D_iso_noise1_recall_list, Algo = as.factor("Isomap"), DIM =as.factor("d'=6"))
recall_df3=data.frame(at_K, reca=D_umap_noise1_recall_list, Algo = as.factor("UMAP"), DIM =as.factor("d'=6"))
recall_df=rbind(recall_df1, recall_df2, recall_df3)
r <- ggplot(recall_df, aes(at_K, reca, colour = Algo, shape = DIM)) + geom_line(alpha=0.9, show.legend = FALSE) + geom_point(alpha=0.9, show.legend = FALSE) + xlab('noise_mag') + ylab('Recall') + scale_color_brewer(palette="Dark2")
#plot(r)

grid.arrange(p, r)
