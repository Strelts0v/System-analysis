confidence.probability = 0.90
student.coefficient = 1.6482

PrintMeanConfidenceInterval <- function(mean, student, var, count) {
    a = mean - student * (var / sqrt(count))
    b = mean + student * (var / sqrt(count))
    print("confidence interval of mean: ", noquote = TRUE)
    print(a)
    print(b)
}

#read data
data.matrix <- read.table("21-arrhythmia.txt", sep = ",")
q.wave <- data.matrix[, 161]
r.wave <- data.matrix[, 162]

# mean
q.wave.mean <- mean(q.wave)
r.wave.mean <- mean(r.wave)

# variance
q.wave.var <- var(q.wave)
r.wave.var <- var(r.wave)

# confidence interval of mean
PrintMeanConfidenceInterval(q.wave.mean, student.coefficient, q.wave.var, length(q.wave))
quantile(replicate(length(r.wave), var(sample(r.wave, rep=TRUE))), c(1 - confidence.interval, confidence.interval))

var.test(q.wave, r.wave, conf.level = confidence.interval)

t.test(q.wave, r.wave, conf.level = confidence.interval)
