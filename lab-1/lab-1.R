ConstructLinearRegression <- function (x, y, x.name, y.name) {
  plot(x, y, xlab = x.name, ylab = y.name)
  abline(lm(y ~ x))
}

PrintCorrelationCoefficient <- function(x, y) {
  print("correlation coefficient: ", noquote = TRUE)
  print(cor(x, y), digits = 7)
}

# read data
data.matrix <- read.table("21-arrhythmia.txt", sep = ",")
q.wave <- data.matrix[, 161]
r.wave <- data.matrix[, 162]

# perform tasks
PrintCorrelationCoefficient(q.wave, r.wave)
ConstructLinearRegression(q.wave, r.wave, "QWave", "RWave")

