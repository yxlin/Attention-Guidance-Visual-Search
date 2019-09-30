information <- function(x0, xqa, xqz, xqv, xqt, xsa, xsz, xsv, xst) {
  cat("Intercept, DIC: ")
  dic2   <- h.IC.dmc(x0,  DIC = TRUE)
  cat("a-Q, DIC: ")
  dicaq2 <- h.IC.dmc(xqa, DIC = TRUE)
  cat("z-Q, DIC: ")
  diczq2 <- h.IC.dmc(xqz, DIC = TRUE)
  cat("v-Q, DIC: ")
  dicvq2 <- h.IC.dmc(xqv, DIC = TRUE)
  cat("t0-Q, DIC: ")
  dictq2 <- h.IC.dmc(xqt, DIC = TRUE)
  cat("a-S, DIC: ")
  dicas2 <- h.IC.dmc(xsa, DIC = TRUE)
  cat("z-S, DIC: ")
  diczs2 <- h.IC.dmc(xsz, DIC = TRUE) 
  cat("v-S, DIC: ")
  dicvs2 <- h.IC.dmc(xsv, DIC = TRUE)
  cat("t0-S, DIC: ")
  dicts2 <- h.IC.dmc(xst, DIC = TRUE)
  imodel <- 
  
  qm0 <- colSums(dic2)[2] - c(colSums(dicaq2)[2], colSums(diczq2)[2], 
                    colSums(dicvq2)[2], colSums(dictq2)[2])
  sm0 <- colSums(dic2)[2] - c(colSums(dicas2)[2], colSums(diczs2)[2], 
                    colSums(dicvs2)[2], colSums(dicts2)[2])
  
  cat("Intercept, BPIC: ")
  dic2   <- h.IC.dmc(x0)
  cat("a-Q, BPIC: ")
  dicaq2 <- h.IC.dmc(xqa)
  cat("z-Q, BPIC: ")
  diczq2 <- h.IC.dmc(xqz)
  cat("v-Q, BPIC: ")
  dicvq2 <- h.IC.dmc(xqv)
  cat("t0-Q, BPIC: ")
  dictq2 <- h.IC.dmc(xqt)
  cat("a-S, BPIC: ")
  dicas2 <- h.IC.dmc(xsa)
  cat("z-S, BPIC: ")
  diczs2 <- h.IC.dmc(xsz) 
  cat("v-S, BPIC: ")
  dicvs2 <- h.IC.dmc(xsv)
  cat("t0-S, BPIC: ")
  dicts2 <- h.IC.dmc(xst)
  
  qm1 <- colSums(dic2)[2] - c(colSums(dicaq2)[2], colSums(diczq2)[2], 
                    colSums(dicvq2)[2], colSums(dictq2)[2])
  sm1 <- colSums(dic2)[2] - c(colSums(dicas2)[2], colSums(diczs2)[2], 
                    colSums(dicvs2)[2], colSums(dicts2)[2])
  
  names(qm0) <- c("a", "z", "v", "t")
  names(sm0) <- c("a", "z", "v", "t")
  names(qm1) <- c("a", "z", "v", "t")
  names(sm1) <- c("a", "z", "v", "t")
  return(rbind(qm0, qm1, sm0, sm1))
}
