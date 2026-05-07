library(shiny)

# --- Secant Method Core Logic ---
secant_solve <- function(f, x0, x1, tol = 0.0001, m = 100) {
  results <- data.frame(
    n = integer(),
    x_n = numeric(),
    f_xn = numeric(),
    error = numeric(),
    stringsAsFactors = FALSE
  )

  f0 <- f(x0)
  f1 <- f(x1)

  results <- rbind(results, data.frame(n = 0, x_n = x0, f_xn = f0, error = NA))
  results <- rbind(results, data.frame(n = 1, x_n = x1, f_xn = f1, error = abs(x1 - x0)))

  converged <- FALSE
  root <- x1

  for (n in 2:m) {
    if (abs(f1 - f0) < .Machine$double.eps) {
      break
    }

    x2 <- x1 - f1 * (x1 - x0) / (f1 - f0)
    f2 <- f(x2)
    error <- abs(x2 - x1)

    results <- rbind(results, data.frame(n = n, x_n = x2, f_xn = f2, error = error))

    if (error < tol) {
      converged <- TRUE
      root <- x2
      break
    }

    x0 <- x1
    f0 <- f1
    x1 <- x2
    f1 <- f2
  }

  if (!converged) {
    root <- x1
  }

  list(
    results = results,
    converged = converged,
    root = root,
    f_root = f(root),
    iterations = nrow(results) - 1
  )
}

# --- UI ---
ui <- fluidPage(
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Cormorant+Garamond:ital,wght@0,400;0,600;0,700;1,400&family=JetBrains+Mono:wght@300;400;500&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
      :root {
        --ink: #1a1a2e;
        --paper: #f8f6f1;
        --accent: #c0392b;
        --accent-light: #e74c3c;
        --rule: #2c2c54;
        --muted: #6b6b7b;
        --highlight: #fef3e2;
        --card-bg: #fffefa;
        --shadow: rgba(26, 26, 46, 0.08);
      }

      body {
        background: var(--paper);
        color: var(--ink);
        font-family: 'Cormorant Garamond', Georgia, serif;
        font-size: 18px;
        line-height: 1.6;
        margin: 0;
        padding: 0;
      }

      .container-fluid {
        max-width: 1100px;
        margin: 0 auto;
        padding: 40px 30px;
      }

      /* Header */
      .app-header {
        text-align: center;
        margin-bottom: 50px;
        padding-bottom: 30px;
        border-bottom: 2px solid var(--rule);
        position: relative;
      }

      .app-header::after {
        content: '';
        position: absolute;
        bottom: -6px;
        left: 50%;
        transform: translateX(-50%);
        width: 60px;
        height: 2px;
        background: var(--accent);
      }

      .app-header h1 {
        font-family: 'Cormorant Garamond', serif;
        font-size: 2.8em;
        font-weight: 700;
        letter-spacing: -0.5px;
        margin: 0 0 8px 0;
        color: var(--ink);
      }

      .app-header .subtitle {
        font-size: 1.1em;
        color: var(--muted);
        font-style: italic;
        margin: 0;
      }

      .formula-display {
        font-family: 'JetBrains Mono', monospace;
        font-size: 0.75em;
        background: var(--highlight);
        padding: 12px 24px;
        border-radius: 4px;
        display: inline-block;
        margin-top: 18px;
        letter-spacing: -0.3px;
        border: 1px solid rgba(192, 57, 43, 0.15);
      }

      /* Input Panel */
      .input-panel {
        background: var(--card-bg);
        border: 1px solid rgba(44, 44, 84, 0.12);
        border-radius: 8px;
        padding: 32px;
        margin-bottom: 40px;
        box-shadow: 0 4px 20px var(--shadow);
      }

      .input-panel h3 {
        font-family: 'Cormorant Garamond', serif;
        font-size: 1.5em;
        font-weight: 600;
        margin: 0 0 24px 0;
        padding-bottom: 12px;
        border-bottom: 1px solid rgba(44, 44, 84, 0.1);
        color: var(--rule);
      }

      .form-group label {
        font-family: 'Cormorant Garamond', serif;
        font-size: 1em;
        font-weight: 600;
        color: var(--ink);
        margin-bottom: 4px;
      }

      .form-control {
        font-family: 'JetBrains Mono', monospace;
        font-size: 14px;
        border: 1px solid rgba(44, 44, 84, 0.2);
        border-radius: 4px;
        padding: 10px 14px;
        background: #fff;
        transition: border-color 0.2s, box-shadow 0.2s;
      }

      .form-control:focus {
        border-color: var(--accent);
        box-shadow: 0 0 0 3px rgba(192, 57, 43, 0.1);
        outline: none;
      }

      #calculate {
        font-family: 'Cormorant Garamond', serif;
        font-size: 1.1em;
        font-weight: 600;
        background: var(--rule);
        color: #fff;
        border: none;
        border-radius: 4px;
        padding: 12px 36px;
        cursor: pointer;
        transition: all 0.2s;
        letter-spacing: 0.5px;
        margin-top: 10px;
      }

      #calculate:hover {
        background: var(--accent);
        transform: translateY(-1px);
        box-shadow: 0 4px 12px rgba(192, 57, 43, 0.25);
      }

      /* Results */
      .results-section {
        margin-top: 10px;
      }

      .results-section h3 {
        font-family: 'Cormorant Garamond', serif;
        font-size: 1.5em;
        font-weight: 600;
        color: var(--rule);
        margin: 0 0 20px 0;
      }

      /* Summary Card */
      .summary-card {
        background: var(--card-bg);
        border: 1px solid rgba(44, 44, 84, 0.12);
        border-left: 4px solid var(--accent);
        border-radius: 4px;
        padding: 24px 28px;
        margin-bottom: 30px;
        box-shadow: 0 2px 12px var(--shadow);
      }

      .summary-card .converged {
        color: #27ae60;
        font-weight: 600;
      }

      .summary-card .not-converged {
        color: var(--accent);
        font-weight: 600;
      }

      .summary-item {
        font-family: 'JetBrains Mono', monospace;
        font-size: 0.8em;
        margin: 8px 0;
        padding: 6px 0;
        border-bottom: 1px dashed rgba(44, 44, 84, 0.08);
      }

      .summary-item:last-child {
        border-bottom: none;
      }

      .summary-label {
        font-family: 'Cormorant Garamond', serif;
        font-size: 1.2em;
        font-weight: 600;
        color: var(--muted);
        display: inline-block;
        width: 140px;
      }

      /* Table */
      .iteration-table {
        background: var(--card-bg);
        border: 1px solid rgba(44, 44, 84, 0.12);
        border-radius: 8px;
        padding: 24px;
        margin-bottom: 30px;
        box-shadow: 0 2px 12px var(--shadow);
        overflow-x: auto;
      }

      .table {
        font-family: 'JetBrains Mono', monospace;
        font-size: 13px;
        width: 100%;
        border-collapse: collapse;
      }

      .table thead th {
        font-family: 'Cormorant Garamond', serif;
        font-size: 15px;
        font-weight: 700;
        color: var(--rule);
        border-bottom: 2px solid var(--rule);
        padding: 10px 16px;
        text-align: left;
      }

      .table tbody td {
        padding: 8px 16px;
        border-bottom: 1px solid rgba(44, 44, 84, 0.06);
      }

      .table tbody tr:hover {
        background: var(--highlight);
      }

      .table tbody tr:last-child td {
        border-bottom: none;
        font-weight: 500;
        color: var(--accent);
      }

      /* Plot */
      .plot-container {
        background: var(--card-bg);
        border: 1px solid rgba(44, 44, 84, 0.12);
        border-radius: 8px;
        padding: 24px;
        box-shadow: 0 2px 12px var(--shadow);
      }

      /* Preset buttons */
      .preset-btn {
        font-family: 'JetBrains Mono', monospace;
        font-size: 11px;
        background: var(--highlight);
        border: 1px solid rgba(192, 57, 43, 0.2);
        border-radius: 3px;
        padding: 4px 10px;
        cursor: pointer;
        margin: 2px 4px 2px 0;
        transition: all 0.15s;
        color: var(--ink);
      }

      .preset-btn:hover {
        background: var(--accent);
        color: #fff;
        border-color: var(--accent);
      }

      .presets-label {
        font-size: 0.85em;
        color: var(--muted);
        font-style: italic;
        margin-bottom: 6px;
        display: block;
      }

      /* Footer */
      .app-footer {
        text-align: center;
        margin-top: 50px;
        padding-top: 20px;
        border-top: 1px solid rgba(44, 44, 84, 0.1);
        font-size: 0.85em;
        color: var(--muted);
        font-style: italic;
      }
    "))
  ),

  # Header
  div(class = "app-header",
    h1("The Secant Method"),
    p(class = "subtitle", "Numerical Root-Finding for Nonlinear Equations"),
    div(class = "formula-display",
      HTML("x<sub>n+1</sub> = x<sub>n</sub> \u2212 f(x<sub>n</sub>) \u00b7 (x<sub>n</sub> \u2212 x<sub>n-1</sub>) / (f(x<sub>n</sub>) \u2212 f(x<sub>n-1</sub>))")
    )
  ),

  # Input Panel
  div(class = "input-panel",
    h3("Parameters"),
    fluidRow(
      column(6,
        textInput("func", "f(x) — enter as R expression",
                  value = "x^6 - x - 1", width = "100%"),
        div(
          span(class = "presets-label", "Examples:"),
          tags$button(class = "preset-btn", onclick = "Shiny.setInputValue('func', 'x^6 - x - 1'); document.getElementById('func').value = 'x^6 - x - 1';",
                      "x\u2076 - x - 1"),
          tags$button(class = "preset-btn", onclick = "Shiny.setInputValue('func', 'x^5 - 10*x^3 - 1'); document.getElementById('func').value = 'x^5 - 10*x^3 - 1';",
                      "x\u2075 - 10x\u00b3 - 1"),
          tags$button(class = "preset-btn", onclick = "Shiny.setInputValue('func', 'cos(x) - x'); document.getElementById('func').value = 'cos(x) - x';",
                      "cos(x) - x"),
          tags$button(class = "preset-btn", onclick = "Shiny.setInputValue('func', 'exp(x) - 3*x'); document.getElementById('func').value = 'exp(x) - 3*x';",
                      "e\u02e3 - 3x")
        )
      ),
      column(6,
        fluidRow(
          column(6, numericInput("x0", "x\u2080 (first guess)", value = 1, width = "100%")),
          column(6, numericInput("x1", "x\u2081 (second guess)", value = 2, width = "100%"))
        ),
        fluidRow(
          column(6, numericInput("tol", "Tolerance", value = 0.0001, step = 0.0001, width = "100%")),
          column(6, numericInput("maxiter", "Max iterations", value = 100, min = 1, width = "100%"))
        )
      )
    ),
    actionButton("calculate", "Solve", icon = icon("square-root-variable"))
  ),

  # Results
  conditionalPanel(
    condition = "output.has_results",
    div(class = "results-section",

      # Summary
      h3("Result"),
      div(class = "summary-card",
        uiOutput("summary")
      ),

      # Plot
      div(class = "plot-container",
        plotOutput("func_plot", height = "350px")
      ),

      # Iteration Table
      br(),
      div(class = "iteration-table",
        h3("Iteration Table"),
        tableOutput("iter_table")
      )
    )
  ),

  # Footer
  div(class = "app-footer",
    HTML("Secant Method &mdash; Numerical Analysis &mdash; Based on lecture notes by Elmer S. Poliquit")
  )
)

# --- Server ---
server <- function(input, output, session) {

  result <- reactiveVal(NULL)

  observeEvent(input$calculate, {
    req(input$func, input$x0, input$x1, input$tol, input$maxiter)

    f <- tryCatch(
      eval(parse(text = paste0("function(x) { ", input$func, " }"))),
      error = function(e) NULL
    )

    if (is.null(f)) {
      result(list(error = "Invalid function expression. Use valid R syntax (e.g., x^2 - 4)."))
      return()
    }

    # Test the function
    test <- tryCatch(f(input$x0), error = function(e) NULL)
    if (is.null(test)) {
      result(list(error = "Function could not be evaluated at x0."))
      return()
    }

    sol <- tryCatch(
      secant_solve(f, input$x0, input$x1, input$tol, input$maxiter),
      error = function(e) list(error = paste("Error:", e$message))
    )

    sol$func <- f
    sol$expr <- input$func
    result(sol)
  })

  output$has_results <- reactive({ !is.null(result()) })
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)

  output$summary <- renderUI({
    res <- result()
    if (is.null(res)) return(NULL)

    if (!is.null(res$error)) {
      return(div(style = "color: var(--accent); font-weight: 600;", res$error))
    }

    status <- if (res$converged) {
      span(class = "converged", "\u2713 Converged")
    } else {
      span(class = "not-converged", "\u2717 Did not converge")
    }

    tagList(
      div(style = "margin-bottom: 12px; font-size: 1.2em;", status),
      div(class = "summary-item",
        span(class = "summary-label", "Root:"),
        sprintf("%.10f", res$root)
      ),
      div(class = "summary-item",
        span(class = "summary-label", "f(root):"),
        sprintf("%.2e", res$f_root)
      ),
      div(class = "summary-item",
        span(class = "summary-label", "Iterations:"),
        as.character(res$iterations)
      ),
      div(class = "summary-item",
        span(class = "summary-label", "Function:"),
        tags$code(style = "font-family: 'JetBrains Mono', monospace; font-size: 0.9em;", res$expr)
      )
    )
  })

  output$iter_table <- renderTable({
    res <- result()
    if (is.null(res) || !is.null(res$error)) return(NULL)

    df <- res$results
    df$x_n <- sprintf("%.10f", df$x_n)
    df$f_xn <- sprintf("%.10f", df$f_xn)
    df$error <- ifelse(is.na(df$error), "\u2014", sprintf("%.10f", df$error))
    colnames(df) <- c("n", "x\u2099", "f(x\u2099)", "Error")
    df
  }, striped = FALSE, hover = TRUE, bordered = FALSE, align = "lrrr")

  output$func_plot <- renderPlot({
    res <- result()
    if (is.null(res) || !is.null(res$error)) return(NULL)

    f <- res$func
    root <- res$root

    # Determine plot range around the root and initial guesses
    all_x <- res$results$x_n
    x_range <- range(all_x, na.rm = TRUE)
    padding <- max(abs(diff(x_range)) * 0.3, 1)
    x_min <- x_range[1] - padding
    x_max <- x_range[2] + padding

    x_seq <- seq(x_min, x_max, length.out = 500)
    y_seq <- tryCatch(sapply(x_seq, f), error = function(e) rep(NA, 500))

    # Limit y range for better visualization
    y_finite <- y_seq[is.finite(y_seq)]
    if (length(y_finite) == 0) return(NULL)
    y_lim <- range(y_finite)
    y_span <- diff(y_lim)
    y_lim <- c(y_lim[1] - y_span * 0.1, y_lim[2] + y_span * 0.1)

    # Clamp extreme values
    y_cap <- max(abs(y_lim)) * 2
    y_seq[y_seq > y_cap] <- NA
    y_seq[y_seq < -y_cap] <- NA

    par(
      bg = "#fffefa",
      fg = "#1a1a2e",
      col.axis = "#6b6b7b",
      col.lab = "#2c2c54",
      col.main = "#1a1a2e",
      family = "serif",
      mar = c(4, 4, 3, 2)
    )

    plot(x_seq, y_seq, type = "l", lwd = 2.5, col = "#2c2c54",
         xlab = "x", ylab = "f(x)",
         main = paste0("f(x) = ", res$expr),
         ylim = y_lim,
         cex.main = 1.4, cex.lab = 1.2, cex.axis = 1.0)

    # Zero line
    abline(h = 0, col = "#6b6b7b", lty = 2, lwd = 1)

    # Mark the root
    points(root, 0, pch = 21, bg = "#c0392b", col = "#c0392b", cex = 2.5, lwd = 2)

    # Label
    text(root, 0, labels = sprintf("  root = %.6f", root),
         pos = 4, col = "#c0392b", font = 2, cex = 1.1)

    # Grid
    grid(col = rgba(44, 44, 84, 0.05), lty = 1)
  })
}

# Helper for grid color
rgba <- function(r, g, b, a) {
  rgb(r/255, g/255, b/255, a)
}

shinyApp(ui = ui, server = server)
