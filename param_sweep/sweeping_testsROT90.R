###
### ### INITIAL COMMENTS HERE ###
###
### Jesse Leigh Patsolic 
### 2018 <jpatsol1@jhu.edu>
### S.D.G 
#

require(devtools)
devtools::install_github("mrae/R-Rerf", ref = "jtest20180426")
require(rerf)

if ("jpatsol1@jhu.edu" %in% Sys.info()) {
  #require(doMPI)
  #cl <- startMPIcluster()
  #registerDoMPI(cl)
  #registerDoMC(nCores)
  require(doMC)
  registerDoMC(24)
} else {
  require(doMC)
  registerDoMC(4)
}

data(mnist)

rot90 <- mnist
s <- Reduce('c', lapply(1:28, function(x) seq(x,28^2, by = 28)))
rot90$Xtrain <- mnist$Xtrain[, s]
rot90$Xtest <- mnist$Xtest[, s]

#mnist <- readRDS('../R/mnist.Rdat')

# p is number of dimensions, d is the number of random features to
# evaluate, iw is image width, ih is image height, patch.min is min
# width of square patch to sample pixels from, and patch.max is the max
# width of square patch
p <- ncol(rot90$Xtrain)
d <- ceiling(sqrt(p))
iw <- sqrt(p)
ih <- iw
rectMM <- expand.grid(width = 1L:8L, height = 1L:8L)[, c(2,1)]

opt.image.rect <- lapply(1:nrow(rectMM), 
			  function(i) { 
				  list(p, d, "image-patch-rectangles", 
					     iw, ih, 
					     patch.min = rectMM[i,1], 
					     patch.max = rectMM[i,2]) 
        }
        )

#opt.list <- c(opt.rf, opt.poisson, opt.image.squares, opt.image.rect)
opt.list <- opt.image.rect
length(opt.list)

run1 <- foreach(oi = opt.list) %dopar% {
 forest <- RerF(rot90$Xtrain, rot90$Ytrain, num.cores = 1L, mat.options = oi, seed = 1L)
  predictions <- Predict(rot90$Xtest, forest, num.cores = 1L)
  error.rate <- mean(predictions != rot90$Ytest)
  list(forest = forest, pred = predictions, error = error.rate)
}

Lhat <- sapply(run1, '[[', 3)
rectMM$Lhat <- Lhat
df1 <- data.frame(x = c(1:length(opt.list)),y = Lhat)


require(ggplot2)
png("height_width_Lhat_rot90.png", height = 1080, width = 1080, res = 150)
#plot(ggplot(data = df1, aes(x = x, y = y)) + geom_point())
p1 <- ggplot(data = rectMM, aes(x = width, y = height)) + 
        geom_raster(aes(fill = Lhat))
p2 <- p1 + scale_fill_distiller(palette = "Greens", direction = 1) + 
        scale_x_continuous(breaks = seq(1, 8), minor_breaks = 0) +
        scale_y_continuous(breaks = seq(1, 8), minor_breaks = 0) +
        annotate("rect", xmin = 5.5, xmax = 6.5, 
                 ymin = 0.5, ymax = 1.5, alpha = 0,
                 colour = 'red')
plot(p2 + ggtitle("L hat for Structured R-Rerf on MNIST"))
dev.off()

save.image(file = "session_sweeping_tests_rot90.RData")


#   Time:
##  Working status:
### Comments:
####Soli Deo Gloria
