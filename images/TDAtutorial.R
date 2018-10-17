library("TDA")
library("rgl")

circleSample <- circleUnif(n = 400, r = 1)
plot(circleSample)

sphereSample <- sphereUnif(n = 10000, d=2, r=1)
plot3d(sphereSample)
torusSample <- torusUnif(n = 10000, a = 1.8, c = 5)
plot3d(torusSample)

#######################

X <- circleUnif(n = 400, r = 1)
Xlim <- c(-1.6, 1.6)
Ylim <- c(-1.7, 1.7)
by <- 0.065
h <- 0.3
Xseq <- seq(from = Xlim[1], to = Xlim[2], by = by)
Yseq <- seq(from = Ylim[1], to = Ylim[2], by = by)

Grid <- expand.grid(Xseq, Yseq)
View(Grid)
plot3d(Grid)

distance <- distFct(X = X, Grid = Grid)
par(mfrow = c(1,2))
plot(X, xlab = "", ylab = "", main = "Sample X")
persp(x = Xseq, y = Yseq, 
      z = matrix(distance, nrow = length(Xseq), ncol = length(Yseq)),
      xlab = "", ylab = "", zlab = "", theta = -20, phi = 35, scale = FALSE,
      expand = 3, col = "red", border = NA, ltheta = 50, shade = 0.5,
      main = "Distance Function")

band <- bootstrapBand(X = X, FUN = kde, Grid = Grid, B = 100,
                      parallel = FALSE, alpha = 0.1, h = h)

Diag <- gridDiag(X = X, FUN = kde, lim = cbind(Xlim, Ylim), by = by,
                 sublevel = FALSE, library = "Dionysus", printProgress = FALSE, h = 0.3)

plot(x = Diag[["diagram"]], band = 2 * band[["width"]], main = "KDE Diagram")

par(mfrow = c(1,3))
plot(Diag[["diagram"]], band = 2 * band[["width"]], main = "KDE Diagram")
plot(Diag[["diagram"]], rotated = TRUE, band = band[["width"]],
     main = "Rotated Diagram")
plot(Diag[["diagram"]], barcode = TRUE, main = "Barcode")

##########################################################################
# generating samples from two circles
##########################################################################
Circle1 <- circleUnif(n = 60)
Circle2 <- circleUnif(n = 60, r = 2) + 3
Circles <- rbind(Circle1, Circle2)
par(mfrow = c(1,1))
plot(Circles, xlab="", ylab="")
Diag <- ripsDiag(X = Circles, maxdimension = 1, maxscale = 5,
                 library = "GUDHI", printProgress = FALSE)
par(mfrow=c(1,2))
plot(Circles, xlab="", ylab="")
plot(Diag[["diagram"]])
plot(Diag[["diagram"]], barcode = TRUE, main = "Barcode")

Diag1 <- ripsDiag(X = Circle1, maxdimension = 1, maxscale = 5)
Diag2 <- ripsDiag(X = Circle2, maxdimension = 1, maxscale = 5)
par(mfrow=c(1,2))
plot(Diag[["diagram"]])
plot(Diag[["diagram"]], barcode = TRUE, main = "Barcode")
plot(Diag1[["diagram"]], barcode = TRUE, main = "Barcode")
plot(Diag2[["diagram"]], barcode = TRUE, main = "Barcode")

print(bottleneck(Diag1 = Diag1[["diagram"]], Diag2 = Diag2[["diagram"]],
                 dimension = 1))
print(wasserstein(Diag1 = Diag1[["diagram"]], Diag2 = Diag2[["diagram"]],
                  p = 2, dimension = 1))
tseq <- seq(from = 0, to = 5, length = 1000)
#domain
Land <- landscape(Diag = Diag[["diagram"]], dimension = 1, KK = 1, tseq = tseq)
par(mfrow=c(1,1))
plot(tseq, Land, type = "l", main = "1st Landscape, dim = 1", ylab = "",
     asp = 1, col = "red", lwd = 3)
