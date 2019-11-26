library(data.table)

lambda <- 20
mu <- 5
s <- 1:10

Cs <- 20
Cw <- 25

sim <- data.table(N = s)
sim[, AvgUtilization := lambda / (mu * N)]

sim[AvgUtilization < 1][1] # N = 5, 80%

sim <- data.table(N = 0:30)
sim[, P := (lambda/mu)^N/factorial(N)]

sim$C <- c(0, cumsum(sim[1:.N-1]$P))

sim[, T2 := P / (1 - lambda / (N * mu))]
sim[, P0 := 1/(C + T2)]
sim[, Rho := lambda / (mu * N)]
sim[, Lq := P0 * P * Rho / (1 - Rho) ^ 2]
sim[, L := Lq + lambda / mu]

sim

avg.queue <- sim[N == 5]$Lq

cost <- merge(data.table(N = 5:20), sim, by = c('N'))

cost[, ':='(Cs = Cs * N, Cw = Cw * L)][, Total := Cs + Cw]

cost

cost[Total == min(cost$Total)]

#    N        P        C       T2         P0          Rho         Lq          L           Cs   Cw         Total
#    6    5.688889  42.86667  17.06667  0.01668521    0.6666667   0.5695217   4.569522    120  114.238   234.238

# A.)

cost[Total == min(cost$Total)]$N # Optimal Servers
# 6

# B.)

cost[Total == min(cost$Total)][, .(SvcCostHR = Cs, WaitCostHr = Cw, TotalCostHr = Total)]

# SvcCostHr WaitCostHr TotalCostHr
# 120 114.238 234.238
