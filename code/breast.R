
pdf(file = "figures/breast.pdf", width = 5, height = 8)
library(ggplot2)
f <- function(a, b) {
  function(y) {
    a * y * log10(y) - 1 / b * exp(-(b * y - b / exp(1))^4)
  }
}

y <- seq(0.01, 1, length.out = 100)
cols <- colorspace::rainbow_hcl(5)
d5 <- data.frame(x = f(3, 30)(y), y = y, color = cols[1])
d4 <- data.frame(x = f(2.8, 33)(y), y = y, color = cols[2])
d3 <- data.frame(x = f(2.5, 36)(y), y = y, color = cols[3])
d2 <- data.frame(x = f(2.2, 40)(y), y = y, color = cols[4])
d1 <- data.frame(x = f(2, 50)(y), y = y, color = cols[5])

library(latex2exp)
ggplot(rbind(d1, d2, d3, d4, d5)) +
  geom_path(aes(x, y, color = I(color))) +
  labs(x = "", y = "") +
  ggtitle(TeX("$f(x;\\theta,\\phi) = \\theta x\\log(x)-\\frac{1}{\\phi}\\mathit{e}^{-\\phi^4(x-\\frac{1}{\\mathit{e}})^4}$")) +
  theme_minimal()
dev.off()


ggplot(rbind(d1, d2, d3, d4, d5)) +
  geom_path(aes(x, y, color = I(color))) +
  labs(x = "", y = "") +
  ggtitle(quote(f(x, theta, phi) == theta * x * log(x) - frac(1, phi) * italic(e)^(-phi^4 * (x - frac(1, italic(e))^4)))) +
  theme_minimal()

# 类似的博客 https://trinkerrstuff.wordpress.com/2018/03/15/2246/