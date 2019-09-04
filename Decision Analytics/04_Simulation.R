library(data.table)

set.seed(42)

rooms.min <- 80
rooms.max <- 130
capacity <- 100

prob.noshow <- 0.05
alpha <- .75

qbinom(alpha, size = 117, prob = prob.noshow)

sims <- data.table(Reservations = replicate(1000, sample(rooms.min:rooms.max, 1)))
sims[, N := .I]
sims$NoShow <- qbinom(alpha, size = sims$Reservations, prob = prob.noshow)

sims <- sims[, .(N, Reservations, NoShow) ]

sims[, OverBooked := max(0, Reservations - NoShow - capacity), by = rownames(sims)]
sims[, Booked := Reservations - NoShow - OverBooked, by = rownames(sims)]
sims[, ':='(Profit = 150 * Booked, FixedCost = -1 * 30 * Booked, Fees = -1 * 200 * OverBooked)]
sims[, Total := Profit + FixedCost + Fees]

sims

max(sims$Total) # max total = $12,000

# Top sim with max profit.
sims[Total == max(sims$Total)][1]

# N Reservations NoShow OverBooked Booked Profit FixedCost Fees Total
# 109 107           7      0         100    15000 - 3000      0   12000
