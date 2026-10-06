#Nama: Fitri Nur Azizah
#NIM: 3338250002
#Kelas: 3A
#Mata Kuliah:Komputasi Statistika

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
P_X_lebih_5 <- pexp(5, rate = lambda, lower.tail = FALSE)
P_X_lebih_5

# ============================================================
# 2. Varians waktu tunggu kereta komuter
# Distribusi Uniform
# ============================================================

# Diketahui:
# Kereta tiba secara acak antara 07.00 hingga 07.20
a <- 0
b <- 20

# Varians distribusi Uniform(a, b)
varians <- (b - a)^2 / 12
varians

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
P_X_kurang_5 <- pexp(5, rate = lambda)
P_X_kurang_5

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