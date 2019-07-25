require(lpSolve)

# Karen is in charge of internship assignments for all students at Northwestern. Karen understands that 
# there is a limited supply of Northwestern students from either the College or the Analytics School. The four 
# main locations she can assign to are Texas, California, DC, or New York; each having their own maximum 
# demand requirements(i.e., Texas can accommodate no more than 750 students) . Karen understands that the 
# Analytics School must supply AT LEAST 1000 and the College AT LEAST 1, 400 students, respectively and there is limited demand for
# them at the locations.

# MAX Students

obj.fun <- c(3000, 4500, 2500, 4000, 5000, 1000, 4000, 2000)

constr <- matrix(c(1, 0, 1, 0, 1, 0, 1, 0,
                  0, 1, 0, 1, 0, 1, 0, 1,
                  1, 1, 0, 0, 0, 0, 0, 0,
                  0, 0, 1, 1, 0, 0, 0, 0,
                  0, 0, 0, 0, 1, 1, 0, 0,
                  0, 0, 0, 0, 0, 0, 1, 1), ncol = 8, byrow = TRUE)

# Direction of the constraints
constranints_direction <- c(">=", ">=", "<=", "<=", "<=", "<=")

# Right hand side for the constraints
rhs <- c(1000, 1400, 750, 650, 300, 800)

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
  names(best_sol) <- paste0(rep("x", 6), 1:8)
  print(best_sol)

  print(paste("Total Students:", sum(best_sol)))

  # Check the value of objective function at optimal point
  print(paste("Maximum Cost: ", optimum$objval, sep = ""))

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

# Solution Found
# x1 x2 x3 x4 x5 x6 x7 x8
# 0 750 0 650 300 0 800 0
# Total Students: 2500
# Maximum Cost: 10,675,000

# Min Cost

obj.fun <- c(3000, 4500, 2500, 4000, 5000, 1000, 4000, 2000)

constr <- matrix(c(1, 0, 1, 0, 1, 0, 1, 0,
                  0, 1, 0, 1, 0, 1, 0, 1,
                  1, 1, 0, 0, 0, 0, 0, 0,
                  0, 0, 1, 1, 0, 0, 0, 0,
                  0, 0, 0, 0, 1, 1, 0, 0,
                  0, 0, 0, 0, 0, 0, 1, 1), ncol = 8, byrow = TRUE)

# Direction of the constraints
constranints_direction <- c(">=", ">=", "<=", "<=", "<=", "<=")

# Right hand side for the constraints
rhs <- c(1000, 1400, 750, 650, 300, 800)

# Find the optimal solution
optimum <- lp(direction = "min",
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
  names(best_sol) <- paste0(rep("x", 6), 1:8)
  print(best_sol)

  print(paste("Total Students:", sum(best_sol)))

  # Check the value of objective function at optimal point
  print(paste("Maximum Cost: ", optimum$objval, sep = ""))

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

# Solution Found
# x1 x2 x3 x4 x5 x6 x7 x8
# 650 0 350 300 0 300 0 800
# Total Students: 2400
# Maximum Cost: 5925000