# plot exp_1
library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
#load("exp_1_a.Rdata")
load("exp_1_c.Rdata")

at_K=seq(5, 45, by=10)

precision_df1=data.frame(at_K, prec=D_rf_precision_list, Algo = as.factor("URerF"), DIM =as.factor("d'=0"))
precision_df2=data.frame(at_K, prec=D_iso_precision_list, Algo = as.factor("Isomap"), DIM =as.factor("d'=0"))
precision_df3=data.frame(at_K, prec=D_umap_precision_list, Algo = as.factor("UMAP"), DIM =as.factor("d'=0"))
precision_df=rbind(precision_df1, precision_df2, precision_df3)
p <- ggplot(precision_df, aes(at_K, prec, colour = Algo, shape = DIM)) + geom_line(alpha=0.9) + geom_point(alpha=0.9, show.legend = FALSE)  + xlab('@K') + ylab('Precision') + scale_color_brewer(palette="Dark2")
plot(p)
