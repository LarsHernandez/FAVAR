

library(forecast)
library(tidyverse)
library(Quandl)
library(tseries)
library(vars)
library(xtable)
library(RColorBrewer)

key <- "WB3WH-RUprDSyH2xaLbu"


FFR <- Quandl("FRED/FEDFUNDS", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date)

CPI <- Quandl("FRED/CPIAUCSL", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date)

PROD <- Quandl("FRED/INDPRO", api_key = key) %>% 
  filter(Date >= "1959-01-01", Date < "2019-01-01") %>% 
  arrange(Date)


pp <- data.frame(Date = FFR$Date[-c(1)],
                 INFL = diff(log(CPI$Value))*100,
                 PROD = diff(log(PROD$Value))*100,
                 FFR  = diff(FFR$Value))


n.columns <- 646 # 718m - 72m
irff <- matrix(nrow = 24, ncol = n.columns)


for (i in 1:n.columns) {
  result <- pp %>% 
    filter(Date > as.Date("1960-02-01") %m+% months(i) & Date < as.Date("1960-02-01") %m+% months(72 + i)) %>% 
    dplyr::select(-Date) %>% VAR(p = 1)
  irff[,i] <- irf(result, impulse = "FFR", response = "INFL", n.ahead = 23)$irf$FFR
}






# safdsf ------------------------------------------------------------------

library(RColorBrewer)


# Functions for adding alpha ----------------------------------------------


addalpha <- function(colors, alpha = 1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

# colorRampPaletteAlpha()
colorRampPaletteAlpha <- function(colors, n=32, interpolate='linear') {
  # Create the color ramp normally
  cr <- colorRampPalette(colors, interpolate=interpolate)(n)
  # Find the alpha channel
  a <- col2rgb(colors, alpha=T)[4,]
  # Interpolate
  if (interpolate=='linear') {
    l <- approx(a, n=n)
  } else {
    l <- spline(a, n=n)
  }
  l$y[l$y > 255] <- 255 # Clamp if spline is > 255
  cr <- addalpha(cr, l$y/255.0)
  return(cr)
}


# Plot --------------------------------------------------------------------


x <- c(1:24)
y <- seq(1966, 2019, length.out = 646)
z <- irff

# Create a function interpolating colors in the range of specified colors
color <- colorRampPaletteAlpha(addalpha(brewer.pal(9,"YlGnBu"),1),100)

# Generate the desired number of colors from this palette
nbcol <- 100
#color <- jet.colors(nbcol)

# Compute the z-value at the facet centres
zfacet <- (z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] + z[-nrow(z), -ncol(z)])/4

# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)


#par(mfrow=c(1,2))
a <- persp(x, y, z, col = color[facetcol],
           xlab = "Months", ylab = "", zlab = "%",
           theta = 40, phi = 30, expand = 0.45,
           ticktype = "detailed", lwd = 0.1,
           border = rgb(red = 0, green = 0, blue = 0, alpha = 0.2))


text(trans3d(0, 1970.0, 0.145, a), "Burns",     col = "black")
text(trans3d(0, 1979.8, 0.145, a), "Volcker",   col = "black")
text(trans3d(0, 1987.8, 0.145, a), "Greenspan", col = "black")
text(trans3d(0, 2006.0, 0.145, a), "Bernanke",  col = "black")
text(trans3d(0, 2014.0, 0.145, a), "Yellen",    col = "black")


lines(trans3d(x = 0, y = 1970, z = c(0.13,-0.01), pmat = a), lwd = 0.2, lty = 2,  col = "black")
lines(trans3d(x = 0, y = 1979, z = c(0.13, 0.045), pmat = a), lwd = 0.2, lty = 2,  col = "black")
lines(trans3d(x = 0, y = 1987, z = c(0.13, 0.05), pmat = a), lwd = 0.2, lty = 2,  col = "black")
lines(trans3d(x = 0, y = 2006, z = c(0.13, 0.01), pmat = a), lwd = 0.2, lty = 2,  col = "black")
lines(trans3d(x = 0, y = 2014, z = c(0.13, 0.04), pmat = a), lwd = 0.2, lty = 2,  col = "black")





