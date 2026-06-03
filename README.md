# Diagnostics for Simple Linear Regression

An interactive Shiny app for BIOL 3P96 (Biostatistics) at Brock University.

## What this app does

This app fits a straight-line (ordinary least squares) regression to simulated
data and displays four diagnostic plots to help you judge whether that line is
an appropriate summary of the relationship.

You choose from five data patterns — linear up, linear down, curved up, curved
down, and fan-shaped — and can toggle residual lines on or off. Pressing
**Resample** generates a fresh dataset from the same pattern so you can see
how much the plots vary by chance.

## Diagnostic plots shown

- **Scatter plot** with the fitted regression line (and optional residuals)
- **Residuals vs. Fitted** — should show no pattern if the linear model fits
- **Normal Q-Q** — checks whether residuals follow a normal distribution
- **Scale-Location** — checks for unequal spread (heteroscedasticity)

## Learning goals

- Recognise when a linear model is appropriate and when it is not
- Identify curved patterns and fan-shaped spread in residual plots
- Understand what "well-behaved" residuals look like

## How to use

1. Select a trend from the radio buttons on the left.
2. Toggle **Show residuals** to see how far each point sits from the line.
3. Press **Resample** to draw a new dataset and observe natural sampling variation.
4. Compare the three diagnostic plots to the ideal (random scatter, straight Q-Q line, flat scale-location).

## Course context

Developed for BIOL 3P96 — Biostatistics, Brock University.
Built with R and Shiny (base R graphics only).
