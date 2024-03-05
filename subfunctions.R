## cor_spearman.test ----
cor_spearman.test <-
  function (x, y, method = "pearson", conf.level = 0.95, ...) {
    d1 <- deparse(substitute(x))
    d2 <- deparse(substitute(y))
    data.name <- paste0(d1, " and ", d2)
    method <- match.arg(method)
    Method <- paste0("Spearman's rank correlation rho")
    alternative <- "true rho is not equal to 0"
    
    xy <- data.frame(x, y)
    # 1st for exact p-value
    scor <- cor.test(x,y, method = "spearman")
    estimate <- scor$estimate
    p.value <- scor$p.value
    x <- as.vector(rank(x))
    y <- as.vector(rank(y))
    # 2nd for CI
    pcor <- cor.test(x,y, method = method)
    parameter <- pcor$parameter
    statistic <- c(Stat = c(sprintf("%g",pcor$statistic),sprintf("%g",scor$statistic)))
    names(statistic) <- c("t","S")
    ci <- pcor$conf.int
    
    ht <- list(
      statistic = statistic,
      parameter = parameter,
      p.value = p.value,
      estimate = estimate,
      alternative = alternative,
      method = Method,
      data.name = data.name,
      conf.int = ci
    )
    class(ht) <- "htest"
    ht
  }

## pcor_ci_spearman.test ----
pcor_ci_spearman.test <-
  function (x, y, z, method = "pearson", conf.level = 0.95, ...) {
    d1 <- deparse(substitute(x))
    d2 <- deparse(substitute(y))
    d3 <- deparse(substitute(z))
    data.name <- paste0(d1, " and ", d2, "; controlling: ", d3)
    method <- match.arg(method)
    Method <- paste0("Partial correlation (Spearman)")
    alternative <- "true partial correlation is not equal to 0"
    
    # 1st for exact p-value
    xyz <- data.frame(x, y, z)
    scor <- ppcor::pcor(xyz, method = "spearman")
    p.value <- scor$p.value[1, 2]
    x <- as.vector(rank(x))
    y <- as.vector(rank(y))
    z <- rank(z)
    z <- as.data.frame(z)
    # 2nd for CI
    xyz <- data.frame(x, y, z)
    pcor <- ppcor::pcor(xyz, method = "pearson")
    estimate <- pcor$est[1, 2]
    parameter <- c(n = pcor$n, gp = pcor$gp)
    statistic <- c(Stat = scor$statistic[2])
    
    fit1 <- lm(x ~ z, data = xyz)
    fit2 <- lm(y ~ z, data = xyz)
    cortest <- cor.test(resid(fit1), resid(fit2), method = method, conf.level = conf.level, ...)
    ci <- cortest$conf.int
    
    ht <- list(
      statistic = statistic,
      parameter = parameter,
      p.value = p.value,
      estimate = c(partial.cor = estimate),
      alternative = alternative,
      method = Method,
      data.name = data.name,
      conf.int = ci
    )
    class(ht) <- "htest"
    ht
  }

## cohens_d for one-sample or paired ----
cohens_d <- function(x, mu = 0, conf.level = .95) {
  md  <- mean(x-mu)
  csd <- sd(x)
  cd  <- md/csd
  TT <- t.test(x=x, mu=mu,conf.level=conf.level,na.rm=T)
  cdl <- (TT$conf.int[1]-mu)/csd
  cdu <- (TT$conf.int[2]-mu)/csd
  cat(sprintf('d = %.6f [%g%% CI: %.6f, %.6f]',cd,conf.level*100,cdl,cdu))
  return(invisible(c(cd,cdl,cdu)))
}

## bw_silverman ----
# Silverman's rule
# https://en.wikipedia.org/wiki/Median_absolute_deviation#Relation_to_standard_deviation
bw_silverman <-  function(x){
  bw <- 1.4826 * median(abs(x-median(x))) * (4/(3*length(x)))^(1/5)
  return(bw)
}
