library(pracma)

set.seed(8421)
n  <- 20
w  <- 2*pi*runif(n)
xp <- cos(w) + 1 + 0.25 * (runif(n) - 0.5)
yp <- sin(w) + 1 + 0.25 * (runif(n) - 0.5)

circlefit(xp, yp, fast = TRUE)  #=> 0.9899628 1.0044920 1.0256633
                                # RMS error: 0.07631986
rslt <- circlefit(xp, yp)       #=> 0.9965782 1.0009066 1.0240452
                                # RMS error: 0.07611598
## Not run:

x0 <- rslt[1]; y0 <- rslt[2]; r0 <- rslt[3]

png("teste.png",
    units="px", width=1600, height=1600, res=250)
plot(c(-0.2, 2.2), c(-0.2, 2.2), type="n", asp=1)
grid()
abline(h=0, col="gray"); abline(v=0, col="gray")
points(xp, yp, col="darkred")

w  <- seq(0, 2*pi, len=100)
xx <- r0 * cos(w) + x0
yy <- r0 * sin(w) + y0
lines(xx, yy, col="blue")
dev.off()

