#1. Jika rata-rata pelanggan datang ke toko adalah 3 orang per jam, modelkan dengan Poisson dan hitung P(X≥5)
#2. Dari 100 bola (20 berwarna merah), diambil 10 tanpa pengembalian. Modelkan jumlah bola merah yang diambil dengan distribusi yang tepat.
#3. Simulasikan 1.000 percobaan Binomial (n=15,p=0.4)dan bandingkan histogram hasil simulasi dengan PMF teoretis.


# 1. DISTRIBUSI POISSON

# Rata-rata pelanggan = 3 orang per jam
lambda <- 3

# P(X >= 5)
P_X_ge_5 <- 1 - ppois(4, lambda)
P_X_ge_5

# PMF Poisson
x_pois <- 0:15
pmf_pois <- dpois(x_pois, lambda)

plot(x_pois, pmf_pois,
     type = "h",
     lwd = 3,
     main = "Poisson (λ = 3)",
     xlab = "k",
     ylab = "P(X = k)")



# 2. DISTRIBUSI HIPERGEOMETRIK


# Populasi = 100 bola
# Bola merah = 20
# Sampel = 10
N <- 100
K <- 20
n_hyper <- 10

# Domain k
k_hyper <- seq(
  from = max(0, n_hyper + K - N),
  to = min(n_hyper, K)
)

# PMF Hipergeometrik
pmf_hyper <- dhyper(
  k_hyper,
  m = K,
  n = N - K,
  k = n_hyper
)

data.frame(
  k = k_hyper,
  P = pmf_hyper
)

# Plot PMF
plot(k_hyper, pmf_hyper,
     type = "h",
     lwd = 3,
     main = paste0(
       "Hypergeometric(N=", N,
       ", K=", K,
       ", n=", n_hyper, ")"
     ),
     xlab = "k (banyak bola merah)",
     ylab = "P(X = k)")

# Simulasi
m_hyper <- 10000

samp_hyper <- rhyper(
  m_hyper,
  m = K,
  n = N - K,
  k = n_hyper
)

# Rata-rata simulasi
mean(samp_hyper)

# Rata-rata teoritis
n_hyper * K / N



# 3. DISTRIBUSI BINOMIAL


# Parameter Binomial
n_binom <- 15
p_binom <- 0.4
m_binom <- 1000

# Simulasi 1.000 percobaan
set.seed(2025)

samp_binom <- rbinom(
  m_binom,
  size = n_binom,
  prob = p_binom
)

# Melihat 10 hasil pertama
head(samp_binom, 10)

# PMF teoretis
x_binom <- 0:n_binom

pmf_binom <- dbinom(
  x_binom,
  size = n_binom,
  prob = p_binom
)

data.frame(
  x = x_binom,
  P = pmf_binom
)

# Histogram hasil simulasi
hist(
  samp_binom,
  breaks = seq(-0.5, 15.5, by = 1),
  probability = TRUE,
  main = "Simulasi Binomial vs PMF Teoretis",
  xlab = "Jumlah sukses",
  ylab = "Probabilitas"
)

# PMF teoretis
points(
  x_binom,
  pmf_binom,
  type = "h",
  lwd = 3
)

points(
  x_binom,
  pmf_binom,
  pch = 16
)