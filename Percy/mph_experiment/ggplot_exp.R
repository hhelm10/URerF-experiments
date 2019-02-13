# new ggplot
library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
load("exp2d.Rdata")


pdf("exp2d.pdf")
at_K = seq(5, 45, by=10)

#####################################################################################
D_rf_noise1_precision_list= D_rf_noise1_p_r_list$precisionList
D_rf_noise1_recall_list=D_rf_noise1_p_r_list$recallList

#####################################################################################
D_iso_noise1_precision_list= D_iso_noise1_p_r_list$precisionList
D_iso_noise1_recall_list=D_iso_noise1_p_r_list$recallList

###################################################################
D_umap_noise1_precision_list= D_umap_noise1_p_r_list$precisionList
D_umap_noise1_recall_list=D_umap_noise1_p_r_list$recallList

precision_df1=data.frame(at_K, prec=D_rf_noise1_precision_list, Algo = as.factor("URerf"), DIM =as.factor("d'=6"))
precision_df2=data.frame(at_K, prec=D_iso_noise1_precision_list, Algo = as.factor("Isomap"), DIM =as.factor("d'=6"))
precision_df3=data.frame(at_K, prec=D_umap_noise1_precision_list, Algo = as.factor("UMAP"), DIM =as.factor("d'=6"))
precision_df=rbind(precision_df1, precision_df2, precision_df3)
p <- ggplot(precision_df, aes(at_K, prec, colour = Algo, shape = DIM)) + geom_line(alpha=0.9, show.legend = FALSE) + geom_point(alpha=0.9, show.legend = FALSE)  + xlab('@K') + ylab('Precision') + scale_color_brewer(palette="Dark2")
#plot(p)                   

recall_df1=data.frame(at_K, reca=D_rf_noise1_recall_list, Algo = as.factor("URerf"), DIM =as.factor("d'=6"))
recall_df2=data.frame(at_K, reca=D_iso_noise1_recall_list, Algo = as.factor("Isomap"), DIM =as.factor("d'=6"))
recall_df3=data.frame(at_K, reca=D_umap_noise1_recall_list, Algo = as.factor("UMAP"), DIM =as.factor("d'=6"))
recall_df=rbind(recall_df1, recall_df2, recall_df3)
r <- ggplot(recall_df, aes(at_K, reca, colour = Algo, shape = DIM)) + geom_line(alpha=0.9, show.legend = FALSE) + geom_point(alpha=0.9, show.legend = FALSE) + xlab('@K') + ylab('Recall') + scale_color_brewer(palette="Dark2")
#plot(r)


#if need to change line thickness, then just set size=DIM but not geom_point, notice the difference with above
pr_df1=data.frame(prec=D_rf_noise1_precision_list, reca=D_rf_noise1_recall_list, Algo = as.factor("URerf"), DIM =as.factor("d'=6"))
pr_df2=data.frame(prec=D_iso_noise1_precision_list, reca=D_iso_noise1_recall_list, Algo = as.factor("Isomap"), DIM =as.factor("d'=6"))
pr_df3=data.frame(prec=D_umap_noise1_precision_list, reca=D_umap_noise1_recall_list, Algo = as.factor("UMAP"), DIM =as.factor("d'=6"))
pr_df=rbind(pr_df1, pr_df2, pr_df3)
pr <- ggplot(pr_df, aes(reca, prec, colour = Algo, shape = DIM)) + geom_line(alpha=0.9) + geom_point(alpha=0.9) + xlab('Recall') + ylab('Precision') + scale_color_brewer(palette="Dark2")#+ theme(legend.text=element_text(size=1))


grid.arrange(p, r, pr, layout_matrix=rbind(cbind(1,2),cbind(3,3)))


