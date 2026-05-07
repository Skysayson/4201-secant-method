# Secant Method Calculator

A Shiny web application for solving nonlinear equations using the Secant Method. Built for MATH 4201 - Numerical Analysis.

Based on lecture notes by Elmer S. Poliquit.

## Formula

```
x_{n+1} = x_n - f(x_n) * (x_n - x_{n-1}) / (f(x_n) - f(x_{n-1}))
```

## Features

- Interactive function input (any valid R expression)
- Configurable initial guesses, tolerance, and max iterations
- Step-by-step iteration table
- Plot of f(x) with the root marked
- Preset examples from lecture notes

## Setup

### Prerequisites

- [R](https://cran.r-project.org/) (version 4.0 or higher)
- [RStudio](https://posit.co/download/rstudio-desktop/) (optional, but recommended)

### Install Dependencies

Open R or RStudio and run:

```r
install.packages("shiny")
```

### Run the App

**Option 1 — From RStudio:**

Open `app.R` in RStudio and click the "Run App" button.

**Option 2 — From the R console:**

```r
setwd("/path/to/4201-secant-method")
shiny::runApp("app.R")
```

**Option 3 — From the terminal:**

```bash
Rscript -e "shiny::runApp('app.R')"
```

The app will open in your default browser at `http://127.0.0.1:XXXX`.

## Usage

1. Enter a function f(x) as an R expression (e.g., `x^6 - x - 1`)
2. Set initial guesses x0 and x1
3. Adjust tolerance and max iterations if needed
4. Click **Solve**

### Preset Examples

| Function | x0 | x1 |
|----------|----|----|
| `x^6 - x - 1` | 1 | 2 |
| `x^5 - 10*x^3 - 1` | 2 | 4 |
| `cos(x) - x` | 0 | 1 |
| `exp(x) - 3*x` | 0 | 1 |

## File Structure

```
4201-secant-method/
├── app.R              # Shiny web application
├── secant_method.R    # Standalone R script (console version)
└── README.md
```
