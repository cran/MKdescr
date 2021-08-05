## -----------------------------------------------------------------------------
library(MKdescr)

## ---- fig.width=7, fig.height=7-----------------------------------------------
x <- 1:10
illustrate.quantile(x, alpha = 0.2)
illustrate.quantile(x, alpha = 0.5, type = 7)
illustrate.quantile(x, alpha = 0.8, type = c(2, 7))

## ---- fig.width=7, fig.height=7-----------------------------------------------
set.seed(123)
illustrate.boxplot(rt(50, df = 5))

## -----------------------------------------------------------------------------
x <- rnorm(100)
IQrange(x)
IQR(x)

## -----------------------------------------------------------------------------
sIQR(x)
sd(x)

## -----------------------------------------------------------------------------
meanAD(x)

## -----------------------------------------------------------------------------
skippedMean(x)
skippedSD(x)

## -----------------------------------------------------------------------------
fiveNS(x)

## -----------------------------------------------------------------------------
## 5% outliers
out <- rbinom(100, prob = 0.05, size = 1)
sum(out)
x <- (1-out)*rnorm(100, mean = 10, sd = 2) + out*25
CV(x)
medCV(x)
iqrCV(x)

## -----------------------------------------------------------------------------
SNR(x)
medSNR(x)
iqrSNR(x)

## -----------------------------------------------------------------------------
n1 <- 100
x <- rnorm(100)
n2 <- 150
y <- rnorm(n2, mean = 3, sd = 2)
## true value
(0-3)/sqrt((1 + n1/n2*2^2)/(n1/n2+1))
## estimates
## Aoki's e
SMD(x, y)
## Hedges' g
SMD(x, y, var.equal = TRUE)
## standardized test statistic of Welch's t-test
SMD(x, y, bias.cor = FALSE)
## Cohen's d
SMD(x, y, bias.cor = FALSE, var.equal = TRUE)

## -----------------------------------------------------------------------------
## 10% outliers
out <- rbinom(100, prob = 0.1, size = 1)
sum(out)
x <- (1-out)*rnorm(100, mean = 10, sd = 2) + out*25
z <- zscore(x)
z.med <- medZscore(x)
z.iqr <- iqrZscore(x)
## mean without outliers (should by close to 0)
mean(z[!out])
mean(z.med[!out])
mean(z.iqr[!out])
## sd without outliers (should by close to 1)
sd(z[!out])
sd(z.med[!out])
sd(z.iqr[!out])

## ---- fig.width=7, fig.height=7-----------------------------------------------
x <- rt(10, df = 3)
par(mfrow = c(1,2))
qboxplot(x, main = "1st and 3rd quartile")
boxplot(x, main = "Lower and upper hinge")

## ---- fig.width = 7, fig.height = 7-------------------------------------------
curve(log, from = -3, to = 5)
curve(glog, from = -3, to = 5, add = TRUE, col = "orange")
legend("topleft", fill = c("black", "orange"), legend = c("log", "glog"))

## ---- fig.width = 7, fig.height = 7-------------------------------------------
curve(log10(x), from = -3, to = 5)
curve(glog10(x), from = -3, to = 5, add = TRUE, col = "orange")
legend("topleft", fill = c("black", "orange"), legend = c("log10", "glog10"))

## -----------------------------------------------------------------------------
inv.glog(glog(10))
inv.glog(glog(10, base = 3), base = 3)
inv.glog10(glog10(10))
inv.glog2(glog2(10))

## ---- fig.width=7, fig.height=7-----------------------------------------------
res <- simCorVars(n = 500, r = 0.8)
cor(res$Var1, res$Var2)

## ---- fig.width=7, fig.height=7-----------------------------------------------
thyroid(TSH = 1.5, fT3 = 2.5, fT4 = 14, TSHref = c(0.2, 3.0),
        fT3ref = c(1.7, 4.2), fT4ref = c(7.6, 15.0))

## ---- fig.width=7, fig.height=7-----------------------------------------------
library(ggplot2)
data(mpg)
p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
p1
p1 + scale_x_log10()
p1 + scale_x_glog10()
p1 + scale_y_log10()
p1 + scale_y_glog10()

## ---- fig.width=7, fig.height=7-----------------------------------------------
x <- matrix(rnorm(1000, mean = 10), nrow = 10)
g1 <- rep("control", 10)
y1 <- matrix(rnorm(500, mean = 11.25), nrow = 10)
y2 <- matrix(rnorm(500, mean = 9.75), nrow = 10)
g2 <- rep("treatment", 10)
group <- factor(c(g1, g2))
Data <- rbind(x, cbind(y1, y2))
pvals <- apply(Data, 2, function(x, group) t.test(x ~ group)$p.value,
               group = group)
## compute log-fold change
logfc <- function(x, group){
  res <- tapply(x, group, mean)
  log2(res[1]/res[2])
}
lfcs <- apply(Data, 2, logfc, group = group)
ps <- data.frame(pvals = pvals, logfc = lfcs)
ggplot(ps, aes(x = logfc, y = pvals)) + geom_point() +
    geom_hline(yintercept = 0.05) + scale_y_neglog10() +
    geom_vline(xintercept = c(-0.1, 0.1)) + xlab("log-fold change") +
    ylab("-log10(p value)") + ggtitle("A Volcano Plot")

## ---- fig.width=7, fig.height=7-----------------------------------------------
## some random data
test <- data.frame(x = rnorm(10), y = rnorm(10), z = rnorm(10))
test.long <- melt.long(test)
test.long
ggplot(test.long, aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = variable))
## introducing an additional grouping variable
group <- factor(rep(c("a","b"), each = 5))
test.long.gr <- melt.long(test, select = 1:2, group = group)
test.long.gr
ggplot(test.long.gr, aes(x = variable, y = value, fill = group)) +
  geom_boxplot()

## -----------------------------------------------------------------------------
sessionInfo()

