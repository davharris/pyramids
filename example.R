# Iterated difference of Gaussians.
# If we threw away 3/4 of the pixels after each step, this would be a Laplacian pyramid.

library(raster)
library(EBImage) # For Gaussian blur
library(scales)  # For plotting color scales

N = 15 # Final dimensionality

elevation <- raster::as.matrix(raster::getData("alt", country="US", path="data/")[[1]])

# returns 1 if non-missing, NA if missing
missing_elev = elevation / elevation

# Oceans, Canada, and Mexico are at sea level
elevation[is.na(elevation)] = 0

blurs = array(NA, c(nrow(elevation), ncol(elevation), N))
pyramid = blurs
blurs[ , , 1] = elevation # First image is unblurred

# "i" indexes pyramid layers, not blur layers
for (i in seq(1, N - 1)) {
  # Difference of Gaussians; Choice of 1.6 based on
  # doi.org/10.1098/rspb.1980.0020. Apparently it gives the closest 
  # approximation to the Laplacian.
  blurs[ , , i + 1] = gblur(blurs[ , , i], sigma = 1.6^(i - 1))
  pyramid[ , , i] = blurs[ , , i + 1] - blurs[ , , i]
  cat(".")
}

# Final pyramid layer is just maximum blur
pyramid[ , , N] = blurs[ , , N]


# Plot the results

col = gradient_n_pal(c(muted("blue"), "white", muted("red")))(seq(0, 1, length = 250))
pdf("pyramid.pdf")
par(mar = c(0.1, 0.1, 0.1, 0.1)) 
for (i in 1:N){
  plot(raster(pyramid[, , i] * missing_elev), col = col, legend = FALSE, axes = FALSE)
}
dev.off()
