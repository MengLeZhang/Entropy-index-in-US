
entropy <- function(x, sub.nms){
  
  i <- length(sub.nms)
  nhoods <- nrow(x[, sub.nms])
  
  p_i <- colSums(x[, sub.nms]) / sum(x[, sub.nms])
  n_u <- rowSums(x[, sub.nms], na.rm = T)
  n_u.mat <- matrix(rep(n_u, i), ncol = i, byrow = F)
  p_iu.mat <- x[, sub.nms] / n_u.mat
  
  p_i.mat <- matrix(rep(p_i, nhoods), ncol = i, byrow = T) ## of course it is 0.2
  
  result.mat <- p_iu.mat * log(p_iu.mat / p_i.mat)
  
  
  les <- rowSums(result.mat, na.rm = T) ## so if there is na due to log(0) we ignore
  ##  matrix(c(NA, 1, NA, 2), ncol = 2, byrow = T ) %>% rowSums(na.rm = T) ## example
  
  aces <- colSums(result.mat * (n_u / sum(n_u)), na.rm = T) #will sum vector by vector (or until rep)
  ces <- sum(aces)
  
  ##  Return results
  return(list(aces = aces, ces = ces, pop = sum(n_u, na.rm = T)))
}
