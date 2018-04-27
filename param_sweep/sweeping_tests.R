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

if ("jpatsol1" %in% Sys.info()) {
  require(doMPI)
  cl <- startMPIcluster()
  registerDoMPI(cl)
  registerDoMC(nCores)
} else {
  require(doMC)
  registerDoMC(24)
}

data(mnist)
#mnist <- readRDS('../R/mnist.Rdat')

# p is number of dimensions, d is the number of random features to
# evaluate, iw is image width, ih is image height, patch.min is min
# width of square patch to sample pixels from, and patch.max is the max
# width of square patch
p <- ncol(mnist$Xtrain)
d <- ceiling(sqrt(p))
iw <- sqrt(p)
ih <- iw
sq <- 2L:9L
rectMM <- expand.grid(1L:8L, 1L:8L)[, c(2,1)]

opt.image.squares <- lapply(1:length(sq), 
			  function(i) { 
				       list(p, d, "image-patch", 
					    iw, ih, 
					    patch.min = sq[i], 
					    patch.max = sq[i]) })

opt.image.rect <- lapply(1:nrow(rectMM), 
			  function(i) { 
				       list(p, d, "image-patch-rectangles", 
					    iw, ih, 
					    patch.min = rectMM[i,1], 
					    patch.max = rectMM[i,2]) })

#opt.list <- c(opt.rf, opt.poisson, opt.image.squares, opt.image.rect)
opt.list <- opt.image.rect
length(opt.list)

run1 <- foreach(oi = opt.list) %dopar% {
 forest <- RerF(mnist$Xtrain, mnist$Ytrain, num.cores = 1L, mat.options = oi, seed = 1L)
  predictions <- Predict(mnist$Xtest, forest, num.cores = 1L)
  error.rate <- mean(predictions != mnist$Ytest)
  list(forest = forest, pred = predictions, error = error.rate)
}

tmp <- sapply(run1, '[[', 3)
df1 <- data.frame(x = c(1:length(opt.list)),y = tmp)

require(ggplot2)
pdf("errors_with_types2.pdf")
show(ggplot(data = df1, aes(x = x, y = y, color = typ)) + geom_point())
dev.off()

pdf("tmp.pdf")
plot(tmp, type = 'l')
dev.off()





#   Time:
##  Working status:
### Comments:
####Soli Deo Gloria
