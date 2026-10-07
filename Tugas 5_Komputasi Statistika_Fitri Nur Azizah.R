#Nama: Fitri Nur Azizah
#NIM: 3338250002
#Kelas: 3A
#Mata Kuliah: Komputasi Statistika

# ============================================================
# 1. Peluang waktu tunggu lebih dari 5 menit
# Distribusi Eksponensial
# ============================================================

# Diketahui:
# Rata-rata (mu) = 5 menit
mu <- 5

# Parameter rate (lambda)
lambda <- 1 / mu

# Menghitung P(X > 5)
prob <- pexp(5, rate = lambda, lower.tail = FALSE)
prob

# Membuat data untuk grafik
x_dexp <- seq(0, 30, by = 1)
y_dexp <- dexp(x_dexp, rate = lambda)

# Plot PDF Distribusi Eksponensial
plot(x_dexp, y_dexp,
     type = "l",
     col = "blue",
     lwd = 2,
     main = "PDF Distribusi Eksponensial (λ = 0.2)",
     xlab = "Waktu Tunggu (Menit)",
     ylab = "f(x)")

# Menambahkan garis pada x = 5
abline(v = 5, col = "red", lty = 2, lwd = 2)

# ============================================================
# 2. Varians waktu tunggu kereta komuter
# Distribusi Uniform
# ============================================================

# Diketahui:
# Kereta tiba secara acak antara 07.00 hingga 07.20
set.seed(2026)
n <- 1000
a <- 0
b <- 20

# Generate sampel waktu tunggu penumpang
x <- runif(n, min = a, max = b)

# Nilai density, CDF, dan quantile waktu tunggu
d_values <- dunif(c(0, 10, 20), min = a, max = b)
p_values <- punif(c(0, 10, 20), min = a, max = b)
q_values <- qunif(c(0.25, 0.5, 0.75), min = a, max = b)

# Plot histogram waktu tunggu penumpang dan PDF teoritis
hist(x, breaks = 20, probability = TRUE,
     main = "Histogram Waktu Tunggu Kereta Komuter U(0,20)",
     xlab = "Waktu Tunggu (menit)")
curve(dunif(x, min = a, max = b),
      from = a, to = b,
      add = TRUE, lwd = 2)

# Menampilkan rata-rata dan varians waktu tunggu
mean(x)
var(x)

# Menghitung varians teoritis waktu tunggu
varians_teoritis <- (b - a)^2 / 12
varians_teoritis

# ============================================================
# 3. Peluang sensor rusak sebelum usia 5 tahun
# Distribusi Eksponensial
# ============================================================

# Diketahui:
# Rata-rata masa pakai (mu) = 10 tahun
mu <- 10

# Parameter rate (lambda)
lambda <- 1 / mu

# Menghitung P(X < 5)
prob <- pexp(5, rate = lambda)
prob

# Plot Distribusi Eksponensial
x <- seq(0, 50, by = 0.1)
y <- dexp(x, rate = lambda)
plot(x, y,
     type = "l",
     col = "blue",
     lwd = 2,
     main = "Distribusi Eksponensial - Masa Pakai Sensor",
     xlab = "Masa Pakai Sensor (tahun)",
     ylab = "f(x)")

# Arsiran area X < 5
x_area <- seq(0, 5, by = 0.1)
y_area <- dexp(x_area, rate = lambda)
polygon(c(0, x_area, 5),
        c(0, y_area, 0),
        col = "lightblue",
        border = NA)

# Garis batas pada 5 tahun
abline(v = 5, col = "red", lwd = 2, lty = 2)

# ============================================================
# 4. Proporsi kemasan kopi yang underweight
# Distribusi Normal
# ============================================================

# Diketahui:
# Mean (mu) = 250 gram
# Standar deviasi (sigma) = 5 gram
mu <- 250
sigma <- 5

# Batas underweight = 240 gram
batas <- 240

# Menghitung P(X < 240)
P_underweight <- pnorm(batas, mean = mu, sd = sigma)
P_underweight

# Menentukan banyak data simulasi
n <- 100

# Membuat data simulasi
x <- rnorm(n, mean = mu, sd = sigma)

# Menghitung ukuran statistik sampel
(x_bar <- mean(x))  #rata-rata sampel
(mle_sigma2 <- mean((x - x_bar)^2))  #estimasi MLE varians
(sd_sample <- sd(x))   #simpangan baku sampel            

# Menampilkan histogram dan kurva PDF
hist(x, breaks = 30, probability = TRUE,
     main = "Histogram Sampel N(250, 5^2) dengan PDF Teoritis",
     xlab = "Berat Bersih Kopi (gram)")

curve(dnorm(x, mean = mu, sd = sigma),
      from = mu - 4*sigma, to = mu + 4*sigma,
      add = TRUE, lwd = 2, col = "darkgreen")

# Menambahkan garis rata-rata pada grafik
abline(v = x_bar, col = "orange", lwd = 2)
abline(v = mu, col = "purple", lwd = 2, lty = 2)

# Menambahkan keterangan grafik
legend("topright",
       legend = c("PDF teoritis", "mean sampel", "mean sebenarnya"),
       lty = c(1, 1, 2),
       col = c("darkgreen", "orange", "purple"),
       bty = "n")