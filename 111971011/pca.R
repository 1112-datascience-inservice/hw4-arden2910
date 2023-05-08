
# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]
# apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)

library(ggbiplot)
g <- ggbiplot(ir.pca, choices=2:3 ,obs.scale = 1, var.scale = 1, groups = ir.species,ellipse=TRUE)
g <- g + scale_color_discrete(name = 'arden')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)
