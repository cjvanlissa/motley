#   \hat{y} = \hat{b}_0 + \hat{b}_1x + \hat{b}_2z + \hat{b}_3xz

x <- rnorm(100)
z <- rnorm(100)
y <- .5 + 3*x-.4*z-2.7*x*z + rnorm(100)

x <- lm(y~x*z)


find_significance <- function(x){
  index<-which(sign(x[-length(x)])!=sign(x[-1]))
  if(sign(x[index])!=sign(x[index+1])){
    return(c(index, index+1))} else{
      return(c(index-1, index))
    }
}

simple_slopes <- function(x, pred = "x", mod = "z"){
  coefs <- coef(x)
  b1 <- coefs[pred]
  int <- c(paste(pred, mod, sep = ":"), paste(mod, pred, sep = ":"))
  int <- int[which(int %in% names(coefs))]
  b3 <- coefs[int]

  sigma <- vcov(x)
  vb1 <- sigma[pred, pred]
  vb3 <- sigma[int, int]
  covb3b1 <- sigma[pred, int]

  z1 <- min(x$model[[mod]])  #supply lower bound for z
  z2 <- max(x$model[[mod]])  #supply lower bound for z
  z <- seq(z1,z2,length.out = 2000)
  plotdat <- data.frame(x = z,
                        y = (b1+b3*z),
                        upper = (b1+b3*z)+(1.9901*sqrt(vb1+(vb1*z*covb3b1)+((z^2)*vb3))),
                        lower = (b1+b3*z)-(1.9901*sqrt(vb1+(vb1*z*covb3b1)+((z^2)*vb3))))


  ggplot(plotdat, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .4) +
    geom_vline(xintercept = mean(plotdat$x[find_significance(plotdat$upper)]), linetype=2) +
    geom_vline(xintercept = mean(plotdat$x[find_significance(plotdat$lower)]), linetype=2) +
    scale_x_continuous(expand = c(0, 0)) +
    labs(x = mod, y = paste0("Effect of ", pred, " on ", names(x$model)[1])) +
    geom_hline(yintercept = 0) +
    theme_bw() +
    annotate("text", x=mean(plotdat$x[find_significance(plotdat$lower)]), hjust = 1, y=max(plotdat[-1])-(max(plotdat[-1])-min(plotdat[-1]))/20, label=paste0(mod, " < ", formatC(mean(plotdat$x[find_significance(plotdat$lower)]), digits = 2, format = "f"), " "))+
    annotate("text", x=mean(plotdat$x[find_significance(plotdat$upper)]), hjust = 0, y=max(plotdat[-1])-(max(plotdat[-1])-min(plotdat[-1]))/20, label=paste0(" ", mod, " > ", formatC(mean(plotdat$x[find_significance(plotdat$upper)]), digits = 2, format = "f")))
}

