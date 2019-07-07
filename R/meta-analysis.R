#' @title Calculate I^2 for a multilevel meta-analysis
#' @description Takes an object of class \code{rma}, containing a multi-level
#' meta-analysis with two variance components, and calculates the I^2 for
#' the variance components.
#' @param x An object of class \code{rma}.
#' @return A numeric vector
#' @rdname calc_i2
#' @export
calc_i2 <- function(x) {
  W <- diag(1 / x$vi)
  X <- model.matrix(x)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  outval <- 100 * x$sigma2 / (sum(x$sigma2) + (x$k - x$p) / sum(diag(P)))
  outval <-  c(I2 = (100 * sum(x$sigma2) / (sum(x$sigma2) + (x$k - x$p) / sum(
    diag(P)))), outval)
  names(outval)[c(2, 3)] <- c("Between cluster", "Within cluster")
  outval
}

#' @title Create an interactive funnel plot
#' @description Takes an object of class \code{rma}, and plots an interactive
#' funnel plot using \code{plotly}.
#' @param x An object of class \code{rma}.
#' @param labels A character vector with labels for the data points.
#' @return A plotly plot.
#' @rdname funnel_plotlyfi
#' @export
funnel_plotlyfi <- function(x, labels = NULL) {
  tau2 <- x$tau2
  weights <- 1 / (x$vi + tau2)

  estimate = x$b[1]
  se <- x$se

  ses <- sqrt(x$vi)

  se.seq = seq(0, max(ses), 0.001)

  #Compute vectors of the lower-limit and upper limit values for
  #the 95% CI region
  ll95 = estimate - (1.96 * se.seq)
  ul95 = estimate + (1.96 * se.seq)

  #Do this for a 99% CI region too
  ll99 = estimate - (3.29 * se.seq)
  ul99 = estimate + (3.29 * se.seq)

  #Confidence interval for summary effect size
  meanll95 = estimate - (1.96 * se)
  meanul95 = estimate + (1.96 * se)

  dfCI = data.frame(ll95, ul95, ll99, ul99, se.seq, estimate, meanll95, meanul95)
  dat <-
    data.frame(
      se = ses,
      R = x$yi,
      weights = weights / sum(weights),
      label = labels
    )

  #Draw Plot
  fp = ggplot(data = dat) +
    geom_line(aes(x = ll95, y = se.seq), linetype = 'dotted', data = dfCI) +
    geom_line(aes(x = ul95, y = se.seq), linetype = 'dotted', data = dfCI) +
    geom_line(aes(x = ll99, y = se.seq), linetype = 'dashed', data = dfCI) +
    geom_line(aes(x = ul99, y = se.seq), linetype = 'dashed', data = dfCI) +
    geom_segment(aes(
      x = meanll95,
      y = min(se.seq),
      xend = meanll95,
      yend = max(se.seq)
    ), data = dfCI) +
    geom_segment(aes(
      x = meanul95,
      y = min(se.seq),
      xend = meanul95,
      yend = max(se.seq)
    ), data = dfCI) +
    geom_point(aes(
      x = R,
      y = se,
      size = weights,
      colour = label,
      text = label
    ), shape = 1) +
    ylab('Standard Error') + xlab('R') +
    scale_y_continuous(trans = "reverse",
                       limits = c(max(dat$se), 0),
                       expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(-1.25, 2, 0.25)) +
    theme_bw()+
    theme(legend.position="none")
    layout(ggplotly(fp, tooltip = "text") %>% config(displayModeBar = F), yaxis=list(fixedrange=TRUE), xaxis=list(fixedrange=TRUE))
}
