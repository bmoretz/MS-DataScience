require(lpSolve)

# Flair Furniture Monthly Plan:  
# Tables and Chairs. 

# Each Table requires 3 hrs Carpentry & 2 hrs Paint and returns a profit of $7 each.
# Each Chair provides $5 profit and requires 4 hrs Carpentry & 1 hr Paint. 

# Flair employs 15 carpenters 40 hrs a week for a total of 2400 hrs over the next month. They contract their Painting and have budgeted for 1000 Hrs this month.  
# Based on current orders and forecasts of future orders, Mgt has stipulated a maximum of 450 Chairs and a minimum of 100 Tables. 

#Given those guidelines, how many tables and chairs should Flair plan to make this month to maximize total profit.

obj.fun <- c(7, 5)

constr <- matrix(c(3, 4,
                  2, 1,
                  1, 0,
                  0, 1), ncol = 2, byrow = TRUE)

# Direction of the constraints
constranints_direction <- c("<=", "<=", ">=", "<=")

# Right hand side for the constraints
rhs <- c(2400, 1000, 100, 450)

# Find the optimal solution
optimum <- lp(direction = "max",
               objective.in = obj.fun,
               const.mat = constr,
               const.dir = constranints_direction,
               const.rhs = rhs,
               all.int = T,
               compute.sens = T)

# Print status: 0 = success, 2 = no feasible solution
if (optimum$status == 2) {
  print("Unsolvable")
} else if (optimum$status == 0) {

  print("Solution Found")

  # Display the optimum values
  best_sol <- optimum$solution
  names(best_sol) <- c("Tables", "Chairs")
  print(best_sol)

  # Check the value of objective function at optimal point
  print(paste("Maximum Revenue: ", optimum$objval, sep = ""))

  print(optimum$duals)

  # Sensibility Analysis Results
  print("Sensibility Analysis Results")
  print(optimum$duals.from)
  print(optimum$duals.to)
  print(optimum$sens.coef.from)
  print(optimum$sens.coef.to)

  rm(best_sol)
}

rm(optimum, obj.fun, constr, constranints_direction, rhs)

# Output:

# Tables Chairs
# 320 360
# "Maximum Revenue: 4040"