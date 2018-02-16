confidence.level <- 0.90

FindMeanConfidenceIntervalWithKnownVariance <- function(data, variance = var(data), conf.level = 0.95) {
  z = qnorm((1 - conf.level) / 2, lower.tail = FALSE)
  xbar = (mean(data))
  sdx = sqrt(variance / length(data))
  c(xbar - z * sdx, xbar + z * sdx)
}

FindMeanConfidenceIntervalWithUnknownVariance <- function(data, conf.level = 0.95) {
  t.test(q.wave, conf.level = conf.level)
}

FindVarianceConfidenceInterval <- function(data, conf.level = 0.95) {
  df = length(data) - 1
  chilower = qchisq((1 - conf.level) / 2, df)
  chiupper = qchisq((1 - conf.level) / 2, df, lower.tail = FALSE)
  v = var(data)
  c(df * v / chiupper, df * v / chilower)
}

# read data
data.matrix <- read.table("21-arrhythmia.txt", sep = ",")
q.wave <- data.matrix[, 161]
r.wave <- data.matrix[, 162]

# compute expected value
q.wave.expected.value <- mean(q.wave)
r.wave.expected.value <- mean(r.wave)

# compute variance
q.wave.variance <- var(q.wave)
r.wave.variance <- var(r.wave)

# standart deviation
# q.wave.standart.deviation <- sd(q.wave)
# r.wave.standart.deviation <- sd(r.wave)

# confidence interval
FindMeanConfidenceIntervalWithUnknownVariance(q.wave, conf.level = confidence.level)
FindMeanConfidenceIntervalWithKnownVariance(q.wave, conf.level = confidence.level)
FindVarianceConfidenceInterval(data, conf.level = confidence.level)

