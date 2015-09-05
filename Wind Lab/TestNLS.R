len <- 24
x = runif(len)
y = x^3 + runif(len, min = -0.1, max = 0.1)
plot(x, y)
s <- seq(from = 0, to = 1, length = 50)
lines(s, s^3, lty = 2)

df <- data.frame(x, y)
m <- nls(y ~ power*atan(x), data = df, start = list(power = 1), trace = T)
lines(s, predict(m, list(x = s)), col = "green")