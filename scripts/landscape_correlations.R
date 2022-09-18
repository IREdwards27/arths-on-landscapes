
# setup -------------------------------------------------------------------

library(tidyverse)
library(corrplot)

sites <- read.csv('data/sites_2022-04-25.csv')


# covariation assessment --------------------------------------------------

pMatrix <- function(mat, ...){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p_vals <- pMatrix(sites[7:16])

sites[7:16] %>% 
  cor(use = 'complete.obs') %>%
  corrplot(
    method = 'circle',
    type = 'upper',
    tl.col = 'black',
    tl.cex = 0.6,
    p.mat = p_vals,
    sig.level = 0.1,
    insig = 'blank')

p_vals2 <- pMatrix(sites[c(7,10:12,15:16)])

sites[c(7,10:12,15:16)] %>% 
  cor(use = 'complete.obs') %>%
  corrplot(
    method = 'circle',
    type = 'upper',
    tl.col = 'black',
    tl.cex = 0.6,
    p.mat = p_vals2,
    sig.level = 0.1,
    insig = 'blank')
