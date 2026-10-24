# =========================================================
# Shiny App: Diagnostics for Simple Linear Regression
# Derived from http://econometricsbysimulation.shinyapps.io/OLS-App/
# Requires: shiny only (all plots use base R graphics)
# =========================================================

# https://hbctraining.github.io/Training-modules/RShiny/lessons/shinylive.html
# Run the shinylive::export line to populate the docs folder 
# so that shinylive works from github
#shinylive::export(appdir = "../RegressionDiagnostics/", destdir = "docs")
#httpuv::runStaticServer("docs/", port = 8008)

# ---------------------------
# Data generator
# ---------------------------
draw.data <- function(type) {
  
  n <- 250
  
  if (type == "linear.up") {
    x <- c(runif(n-2, 0, 4), 2, 2.1)
    y <- 2*x + rnorm(n, sd=2)
  }
  if (type == "linear.down") {
    x <- c(runif(n-2, 0, 4), 2, 2.1)
    y <- -2*x + rnorm(n, sd=2)
  }
  if (type == "curved.up") {
    x <- c(runif(n-2, 0, 4), 2, 2.1)
    y <- 2*x^4 + rnorm(n, sd=16)
  }
  if (type == "curved.down") {
    x <- c(runif(n-2, 0, 4), 2, 2.1)
    y <- -2*x^3 + rnorm(n, sd=9)
  }
  if (type == "fan.shaped") {
    x <- seq(0, 3.99, 4/n)
    y <- c(rnorm(n/8,3,1), rnorm(n/8,3.5,2), rnorm(n/8,4,2.5),
           rnorm(n/8,4.5,3), rnorm(n/4,5,4), rnorm((n/4)+2,6,5))
  }
  
  data.frame(x=x, y=y)
}

# ---------------------------
# Safari-safe plotting
# Draws a base-R plot as a PNG image. Stand-in for renderPlot(), which
# currently fails in Safari under Shinylive. Use with imageOutput().
# ---------------------------
render_base <- function(id, session, draw) {
  renderImage({
    cd <- session$clientData
    w  <- cd[[paste0("output_", id, "_width")]]
    h  <- cd[[paste0("output_", id, "_height")]]
    req(w > 0, h > 0)
    pr <- cd$pixelratio
    if (is.null(pr)) pr <- 1
    f <- tempfile(fileext = ".png")
    png(f, width = w * pr, height = h * pr, res = 72 * pr)
    tryCatch(draw(), finally = dev.off())
    list(src = f, contentType = "image/png", width = w, height = h)
  }, deleteFile = TRUE)
}

# ---------------------------
# UI
# ---------------------------
ui <- pageWithSidebar(
  
  headerPanel("Diagnostics for Simple Linear Regression"),
  
  sidebarPanel(
    
    radioButtons("type", "Select a trend:",
                 list("Linear up"    = "linear.up",
                      "Linear down"  = "linear.down",
                      "Curved up"    = "curved.up",
                      "Curved down"  = "curved.down",
                      "Fan-shaped"   = "fan.shaped")),
    br(),
    
    checkboxInput("show.resid", "Show residuals", FALSE),
    
    br(),
    
    actionButton("resample", "Resample", class = "btn-primary"),
    
    br(),
    br(),
    
    helpText("This app uses ordinary least squares (OLS) to fit a regression
              line to the data with the selected trend. The app is designed to
              help you practice evaluating whether or not the linear model is an
              appropriate fit to the data. The three diagnostic plots on the
              lower half of the page are provided to help you identify
              undesirable patterns in the residuals that may arise from
              non-linear trends in the data."),
    br(),
    
    helpText("_____________________"),
    helpText("Glenn Tattersall, PhD"),
    helpText("For use in BIOL 3P96 - Biostatistics")
    #helpText("Original code from openintro.org")
  ),
  
  mainPanel(
    imageOutput("scatter", height = "400px"),
    br(),
    br(),
    imageOutput("residuals", height = "280px")
  )
)

# ---------------------------
# SERVER
# ---------------------------
server <- function(input, output, session) {
  
  mydata <- reactive({
    input$resample        # dependency trigger
    draw.data(input$type)
  })
  
  lmResults <- reactive({
    lm(y ~ x, data = mydata())
  })
  
  # ---------------------------
  # Scatter plot with regression line, CI, PI, optional residuals
  # ---------------------------
  output$scatter <- render_base("scatter", session, function() {
    
    data1 <- mydata()
    x     <- data1$x
    y     <- data1$y
    
    xcon      <- seq(min(x) - 0.1, max(x) + 0.1, 0.025)
    predictor <- data.frame(x = xcon)
    
    yhat  <- predict(lmResults())
    yline <- predict(lmResults(), predictor)
    
    newx  <- seq(min(x), max(x), length.out = 400)
    confs <- predict(lmResults(), newdata = data.frame(x = newx),
                     interval = "confidence")
    preds <- predict(lmResults(), newdata = data.frame(x = newx),
                     interval = "predict")
    
    r.squared <- round(summary(lmResults())$r.squared, 4)
    corr.coef <- round(sqrt(r.squared), 4)
    
    par(cex.main=1.5, cex.lab=1.5, cex.axis=1.5, mar=c(4,4,4,1))
    
    plot(c(min(x), max(x)),
         c(min(y, yline), max(y, yline)),
         type = "n",
         xlab = "x", ylab = "y",
         main = paste0("Regression Model\n",
                       "(R = ", corr.coef, ", R-squared = ", r.squared, ")"))
    
    # Prediction interval band (lighter)
    polygon(c(rev(newx), newx),
            c(rev(preds[, 3]), preds[, 2]),
            col = grey(0.95), border = NA)
    
    # Confidence interval band (darker)
    polygon(c(rev(newx), newx),
            c(rev(confs[, 3]), confs[, 2]),
            col = grey(0.75), border = NA)
    
    points(x, y, pch = 19, col = "#569BBDC0")
    lines(xcon, yline, lwd = 2, col = "#569BBD")
    
    # Optional residual lines
    if (input$show.resid)
      for (j in seq_along(x))
        lines(rep(x[j], 2), c(yhat[j], y[j]), col = "#F05133")
    
    legend_pos <- ifelse(lmResults()$coefficients[1] < 1, "topleft", "topright")
    if (input$type == "linear.down") legend_pos <- "topright"
    if (input$type == "fan.shaped")  legend_pos <- "topleft"
    
    legend(legend_pos, inset = 0.05,
           legend = c("Regression Line", "Confidence Interval", "Prediction Interval"),
           fill   = c("#569BBD", grey(0.75), grey(0.95)))
    box()
  })
  
  # ---------------------------
  # Diagnostic plots: residuals vs fitted, histogram, Q-Q
  # ---------------------------
  output$residuals <- render_base("residuals", session, function() {
    
    residuals <- summary(lmResults())$residuals
    predicted <- predict(lmResults(), newdata = data.frame(x = mydata()$x))
    
    par(mfrow=c(1,3), cex.main=2, cex.lab=2, cex.axis=2, mar=c(4,5,4,2))
    
    # Residuals vs Fitted
    plot(residuals ~ predicted,
         main = "Residuals vs. Fitted Values",
         xlab = "Fitted Values", ylab = "Residuals",
         pch = 19, col = "#569BBDC0")
    abline(h = 0, lty = 2)
    
    # Histogram of residuals with density overlay
    d <- density(residuals)
    h <- hist(residuals, plot = FALSE)
    hist(residuals,
         main = "Histogram of Residuals", xlab = "Residuals",
         col  = "#569BBDC0", prob = TRUE,
         ylim = c(0, max(max(d$y), max(h$density))))
    lines(d, col = "#569BBD", lwd = 2)
    
    # Normal Q-Q plot
    qqnorm(residuals, pch = 19, col = "#569BBDC0",
           main = "Normal Q-Q Plot of Residuals")
    qqline(residuals, col = "#569BBD", lwd = 2)
    
  })
}

shinyApp(ui = ui, server = server)