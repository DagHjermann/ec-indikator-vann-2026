### mEQR
# Funksjoner til WFD2ECA
# ved Hanno Sandvik
# mai 2025
# se https://github.com/NINAnor/WFD2ECA
###

## modified Nicolas Dupont (nicolas.dupont@niva.no)
## nEQR is adapted to deal with indicators with worst condition value and reference condition value = Inf

mEQR <- function(x, klassegrenser) {
  # Funksjonen beregner mEQR-verdier ("modifiserte EQR-verdier")
  K <- klassegrenser
  if (is.infinite(K[2]) & K[8] %=% 0) {
    K[8] <- K[7] / 100
    x <- sapply(x, max, K[8])
    x <- K[7] / x
    K <- K[7] / K
  }
  
  if ((K[8] < K[1]) %=% TRUE) {
    K <- -K
    x <- -x
  }
  x <- ifelse(x < K[1], NA,
              ifelse(x < K[2], ifelse(   rep(K[2] - K[1] > K[3] - K[2], length(x)),
                                  -atan(    (x    - K[2]) * 
                                        iNv((K[2] - K[1]) / (K[3] - K[2])) / 
                                            (K[1] - K[2])) /
                                   atan(iNv((K[2] - K[1]) / (K[3] - K[2]))),
                               (x - K[2]) / (K[3] - K[2]) + 0),
              ifelse(x < K[3], (x - K[2]) / (K[3] - K[2]) + 0,
              ifelse(x < K[4], (x - K[3]) / (K[4] - K[3]) + 1,
              ifelse(x < K[5], (x - K[4]) / (K[5] - K[4]) + 2,
              ifelse(x < K[6], (x - K[5]) / (K[6] - K[5]) + 3,
              ifelse(x <=K[7], (x - K[6]) / (K[7] - K[6]) + 4,
              ifelse(x <=K[8], ifelse(   rep(K[7] - K[6] > K[8] - K[7], length(x)),
                               (x - K[6]) / (K[7] - K[6]) + 4,
                                   atan(    (x    - K[7]) * 
                                        iNv((K[8] - K[7]) / (K[7] - K[6])) /
                                            (K[8] - K[7])) /
                                   atan(iNv((K[8] - K[7]) / (K[7] - K[6]))) + 5),
                               NA))))))))
  return(x * 0.2)
}



mEQR.knekk <- function(x, klassegrenser) {
  # Funksjonen beregner mEQR-verdier, versjon "knekk"
  K <- klassegrenser
  if (is.infinite(K[2]) & K[8] %=% 0) {
    K[8] <- K[7] / 100
    x <- sapply(x, max, K[8])
    x <- K[7] / x
    K <- K[7] / K
  }
  if ((K[8] < K[1]) %=% TRUE) {
    K <- -K
    x <- -x
  }
  x <- ifelse(x < K[1], NA,
       ifelse(x < K[2], (x - K[1]) / (K[2] - K[1]) - 1,
       ifelse(x < K[3], (x - K[2]) / (K[3] - K[2]) + 0,
       ifelse(x < K[4], (x - K[3]) / (K[4] - K[3]) + 1,
       ifelse(x < K[5], (x - K[4]) / (K[5] - K[4]) + 2,
       ifelse(x < K[6], (x - K[5]) / (K[6] - K[5]) + 3,
       ifelse(x <=K[7], (x - K[6]) / (K[7] - K[6]) + 4,
       ifelse(x <=K[8], (x - K[7]) / (K[8] - K[7]) + 5,
                        NA))))))))
  return(x * 0.2)
}



mEQR.forlenga <- function(x, klassegrenser) {
  # Funksjonen beregner mEQR-verdier, versjon "forlengelse"
  K <- klassegrenser
  if (is.infinite(K[2]) & K[8] %=% 0) {
    K[8] <- K[7] / 100
    x <- sapply(x, max, K[8])
    x <- K[7] / x
    K <- K[7] / K
  }
  if ((K[8] < K[1]) %=% TRUE) {
    K <- -K
    x <- -x
  }
  x <- ifelse(x < K[1], NA,
       ifelse(x < K[3], (x - K[2]) / (K[3] - K[2]) + 0,
       ifelse(x < K[4], (x - K[3]) / (K[4] - K[3]) + 1,
       ifelse(x < K[5], (x - K[4]) / (K[5] - K[4]) + 2,
       ifelse(x < K[6], (x - K[5]) / (K[6] - K[5]) + 3,
       ifelse(x <=K[8], (x - K[6]) / (K[7] - K[6]) + 4,
                        NA))))))
  return(x * 0.2)
}



nEQR <- function(x, klassegrenser) {
  # Funksjonen beregner nEQR-verdier
  K <- klassegrenser

  if (is.infinite(K[7])) { ## ADDED ND nEQR when infinite reference tilstand
    K[7] <- K[6] * 10
  }

  if ((K[8] < K[1]) %=% TRUE) {
    K <- -K
    x <- -x
    if (is.infinite(K[2])) { ## ADDED ND nEQR when infinite is the lower grense for "svært dårlig"
      K[2] <- K[3] *10
    }
  }

  if (is.infinite(K[2]) & K[8] %=% 0) {
    K[8] <- K[7] / 100
    x <- sapply(x, max, K[8])
    x <- K[7] / x
    K <- K[7] / K
  }


  x <- ifelse(x < K[1], NA,
       ifelse(x < K[2],                              0,
       ifelse(x < K[3], (x - K[2]) / (K[3] - K[2]) + 0,
       ifelse(x < K[4], (x - K[3]) / (K[4] - K[3]) + 1,
       ifelse(x < K[5], (x - K[4]) / (K[5] - K[4]) + 2,
       ifelse(x < K[6], (x - K[5]) / (K[6] - K[5]) + 3,
       ifelse(x <=K[7], (x - K[6]) / (K[7] - K[6]) + 4,
       ifelse(x <=K[8],                              5,
                        NA))))))))
  return(x * 0.2)
}

