# Secant Method Implementation
# Based on: Numerical Analysis - Solutions of Nonlinear Equations and Linear Systems
# By: Elmer S. Poliquit
#
# Formula: x_{n+1} = x_n - f(x_n) * (x_n - x_{n-1}) / (f(x_n) - f(x_{n-1})),  n >= 1

secant <- function(f, x0, x1, tol = 0.0001, m = 100) {
  # Print header
  cat(sprintf("%-5s %-15s %-15s %-15s\n", "n", "x_n", "f(x_n)", "Error"))
  cat(paste(rep("-", 55), collapse = ""), "\n")

  f0 <- f(x0)
  f1 <- f(x1)

  cat(sprintf("%-5d %-15.6f %-15.6f\n", 0, x0, f0))
  cat(sprintf("%-5d %-15.6f %-15.6f %-15.6f\n", 1, x1, f1, abs(x1 - x0)))

  for (n in 2:m) {
    # Secant method formula
    x2 <- x1 - f1 * (x1 - x0) / (f1 - f0)
    f2 <- f(x2)
    error <- abs(x2 - x1)

    cat(sprintf("%-5d %-15.6f %-15.6f %-15.6f\n", n, x2, f2, error))

    # Check for convergence
    if (error < tol) {
      cat("\n")
      cat("Converged!\n")
      cat(sprintf("Root: %.6f\n", x2))
      cat(sprintf("f(root): %.10f\n", f2))
      cat(sprintf("Iterations: %d\n", n))
      cat(sprintf("Tolerance: %g\n", tol))
      return(x2)
    }

    # Update for next iteration
    x0 <- x1
    f0 <- f1
    x1 <- x2
    f1 <- f2
  }

  cat("\nMethod did not converge within", m, "iterations.\n")
  return(x1)
}

# ============================================================
# Example 1: f(x) = x^6 - x - 1 = 0, x0 = 1, x1 = 2
# From the lecture notes (page 21-23)
# ============================================================
cat("=== Example 1: f(x) = x^6 - x - 1 ===\n")
cat("x0 = 1, x1 = 2, tol = 0.0001\n\n")

f <- function(x) { x^6 - x - 1 }
root1 <- secant(f, x0 = 1, x1 = 2, tol = 0.0001, m = 100)

cat("\n\n")

# ============================================================
# Example 2: f(x) = x^5 - 10x^3 - 1 = 0, x0 = 2, x1 = 4
# From the lecture notes (page 25)
# ============================================================
cat("=== Example 2: f(x) = x^5 - 10x^3 - 1 ===\n")
cat("x0 = 2, x1 = 4, tol = 0.0001\n\n")

g <- function(x) { x^5 - 10 * x^3 - 1 }
root2 <- secant(g, x0 = 2, x1 = 4, tol = 0.0001, m = 100)
