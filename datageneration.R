# Online time series outlier detection
# Data generating

N <- 1200
t <- 1:400

n0 <- rnorm(400, mean = 0, sd = 0.1)
n1 <- rnorm(21, mean = 0, sd = 0.5)
n2 <- 0.4*sin(40*pi*t/N)

x1 <- sin(40*pi*t/N) + n0

x2 <- x1 
x2[600:620] <- x2[600:620] + n1

x3 <- x1
x3[600:620] <- x3[600:620] + n1
x3[820:870] <- x3[820:870] + n2[820:870]

