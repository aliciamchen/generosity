# This file contains R functions to calculate the statistics discussed in Sagarin, Ambler, and Lee's (2014)
# "An ethical approach to peeking at data. Perspectives on Psychological Science, 9, 293-304"
# The paper, the R functions, and Excel spreadsheets to calculate the statistics are available at www.paugmented.com
# File updated May 6, 2014

pactual <-
  function(ns,
           pmax = 1,
           pcrit = .05,
           slices = 1000,
           tails = 2,
           indent = "") {
    slicewidth <- 1 / slices
    pslices <- seq(.5 * (1 / slices), 1 - .5 * (1 / slices), length = slices)
    oldweights <- rep(slicewidth, times = slices)
    naccumulator <- ns[1]
    paccumulator <- 0
    if (length(ns) > 2) {
      for (i in 2:(length(ns) - 1)) {
        newweights <- rep(0, times = slices)
        cat(indent,
            "Calculating augmentation round ",
            i - 1,
            " of ",
            length(ns) - 1,
            ":",
            sep = "")
        for (j in 1:slices) {
          if ((j - 1) * 10 / slices == trunc((j - 1) * 10 / slices)) {
            cat("", 10 - (j - 1) * 10 / slices)
          }
          if (((tails == 1) & (pslices[j] > (1 - pcrit))) |
              ((tails == 2) &
               ((pslices[j] > (1 - pcrit / 2)) | (pslices[j] < (pcrit / 2))))) {
            paccumulator <- paccumulator + oldweights[j]
          } else if (((tails == 1) & (pslices[j] > (1 - pmax))) |
                     ((tails == 2) &
                      ((pslices[j] > (1 - pmax / 2)) | (pslices[j] < (pmax / 2))))) {
            for (k in 1:slices) {
              pcombined <-
                pnorm((
                  naccumulator ^ .5 * qnorm(pslices[j]) + ns[i] ^ .5 * qnorm(pslices[k])
                ) / (naccumulator + ns[i]) ^ .5)
              newweights[pcombined * slices + 1] <-
                newweights[pcombined * slices + 1] + oldweights[j] * slicewidth
            }
          }
        }
        naccumulator <- naccumulator + ns[i]
        oldweights <- newweights
        cat("\n")
      }
      cat(
        indent,
        "Calculating augmentation round ",
        length(ns) - 1,
        " of ",
        length(ns) - 1,
        ": 10 9 8 7 6 5 4 3 2 1\n",
        sep = ""
      )
    }
    if (tails == 1) {
      paccumulator <- paccumulator + sum(oldweights * ((pslices > (1 - pcrit)) +
                                                         (pslices > (1 - pmax)) * (pslices <= (1 - pcrit)) *
                                                         (1 - pnorm((
                                                           qnorm(1 - pcrit) * (naccumulator + ns[length(ns)]) ^ .5 - naccumulator ^
                                                             .5 * qnorm(pslices)
                                                         ) / ns[length(ns)] ^ .5
                                                         ))))
    } else {
      paccumulator <-
        paccumulator + sum(oldweights * ((pslices > (1 - pcrit / 2)) + (pslices <
                                                                          (pcrit / 2)) +
                                           ((pslices > (1 - pmax / 2)) * (pslices <= (1 - pcrit / 2)) + (pslices <
                                                                                                           (pmax / 2)) * (pslices >= (pcrit / 2))
                                           ) *
                                           (1 - pnorm((
                                             qnorm(1 - pcrit / 2) * (naccumulator + ns[length(ns)]) ^ .5 - naccumulator ^
                                               .5 * qnorm(pslices)
                                           ) / ns[length(ns)] ^ .5
                                           ) +
                                             pnorm((
                                               -qnorm(1 - pcrit / 2) * (naccumulator + ns[length(ns)]) ^ .5 - naccumulator ^
                                                 .5 * qnorm(pslices)
                                             ) / ns[length(ns)] ^ .5
                                             ))))
    }
    return(paccumulator)
  }

pcrit <- function(ns,
                  pmax = 1,
                  pdesired = .05,
                  slices = 1000,
                  tails = 2) {
  pcrit <- pdesired / 2
  modification <- pcrit / 2
  for (i in 1:15) {
    cat("Calculating adjustment iteration ", i, " of ", 15, "\n", sep = "")
    if (pactual(ns, pmax, pcrit, slices, tails, "  ") < pdesired) {
      pcrit <- pcrit + modification
    } else {
      pcrit <- pcrit - modification
    }
    modification <- modification / 2
  }
  return(pcrit)
}

paugmented <-
  function(ns,
           plargest,
           pfinal,
           pcrit = .05,
           slices = 1000,
           tails = 2) {
    slicewidth <- 1 / slices
    pslices <- seq(.5 * (1 / slices), 1 - .5 * (1 / slices), length = slices)
    lowboundoldweights <- rep(slicewidth, times = slices)
    highboundoldweights <- rep(slicewidth, times = slices)
    naccumulator <- ns[1]
    lowboundpaccumulator <- 0
    highboundpaccumulator <- 0
    if (length(ns) > 2) {
      for (i in 2:(length(ns) - 1)) {
        lowboundnewweights <- rep(0, times = slices)
        highboundnewweights <- rep(0, times = slices)
        cat("Calculating augmentation round ",
            i - 1,
            " of ",
            length(ns) - 1,
            ":",
            sep = "")
        for (j in 1:slices) {
          if ((j - 1) * 10 / slices == trunc((j - 1) * 10 / slices)) {
            cat("", 10 - (j - 1) * 10 / slices)
          }
          if (((tails == 1) & (pslices[j] > (1 - pcrit))) |
              ((tails == 2) &
               ((pslices[j] > (1 - pcrit / 2)) | (pslices[j] < (pcrit / 2))))) {
            lowboundpaccumulator <- lowboundpaccumulator + lowboundoldweights[j]
            highboundpaccumulator <-
              highboundpaccumulator + highboundoldweights[j]
          } else if (((tails == 1) & (pslices[j] > (1 - plargest))) |
                     ((tails == 2) &
                      ((pslices[j] > (
                        1 - plargest / 2
                      )) | (pslices[j] < (plargest / 2))))) {
            for (k in 1:slices) {
              pcombined <-
                pnorm((
                  naccumulator ^ .5 * qnorm(pslices[j]) + ns[i] ^ .5 * qnorm(pslices[k])
                ) / (naccumulator + ns[i]) ^ .5)
              lowboundnewweights[pcombined * slices + 1] <-
                lowboundnewweights[pcombined * slices + 1] + lowboundoldweights[j] * slicewidth
              highboundnewweights[pcombined * slices + 1] <-
                highboundnewweights[pcombined * slices + 1] + highboundoldweights[j] *
                slicewidth
            }
          } else {
            for (k in 1:slices) {
              pcombined <-
                pnorm((
                  naccumulator ^ .5 * qnorm(pslices[j]) + ns[i] ^ .5 * qnorm(pslices[k])
                ) / (naccumulator + ns[i]) ^ .5)
              highboundnewweights[pcombined * slices + 1] <-
                highboundnewweights[pcombined * slices + 1] + highboundoldweights[j] *
                slicewidth
            }
          }
        }
        naccumulator <- naccumulator + ns[i]
        lowboundoldweights <- lowboundnewweights
        highboundoldweights <- highboundnewweights
        cat("\n")
      }
      cat(
        "Calculating augmentation round ",
        length(ns) - 1,
        " of ",
        length(ns) - 1,
        ": 10 9 8 7 6 5 4 3 2 1\n",
        sep = ""
      )
    }
    if (tails == 1) {
      lowboundpaccumulator <-
        lowboundpaccumulator + sum(lowboundoldweights * ((pslices > (1 - pcrit)) +
                                                           (pslices > (1 - plargest)) *
                                                           (pslices <= (1 - pcrit)) *
                                                           (1 - pnorm((
                                                             qnorm(1 - pfinal) * (naccumulator + ns[length(ns)]) ^ .5 - naccumulator ^
                                                               .5 * qnorm(pslices)
                                                           ) / ns[length(ns)] ^ .5
                                                           ))))
      highboundpaccumulator <-
        highboundpaccumulator + sum(highboundoldweights * ((pslices > (1 - pcrit)) +
                                                             (pslices > (1 - 1)) *
                                                             (pslices <= (1 - pcrit)) *
                                                             (1 - pnorm((
                                                               qnorm(1 - pfinal) * (naccumulator + ns[length(ns)]) ^ .5 - naccumulator ^
                                                                 .5 * qnorm(pslices)
                                                             ) / ns[length(ns)] ^ .5
                                                             ))))
    } else {
      lowboundpaccumulator <-
        lowboundpaccumulator + sum(lowboundoldweights * ((pslices > (1 - pcrit /
                                                                       2)) + (pslices < (pcrit / 2)) +
                                                           ((pslices > (1 - plargest /
                                                                          2)) * (pslices <= (1 - pcrit / 2)) + (pslices < (plargest / 2)) * (pslices >=
                                                                                                                                               (pcrit / 2))
                                                           ) *
                                                           (1 - pnorm((
                                                             qnorm(1 - pfinal / 2) * (naccumulator + ns[length(ns)]) ^ .5 - naccumulator ^
                                                               .5 * qnorm(pslices)
                                                           ) / ns[length(ns)] ^ .5
                                                           ) +
                                                             pnorm((
                                                               -qnorm(1 - pfinal / 2) * (naccumulator + ns[length(ns)]) ^ .5 - naccumulator ^
                                                                 .5 * qnorm(pslices)
                                                             ) / ns[length(ns)] ^ .5
                                                             ))))
      highboundpaccumulator <-
        highboundpaccumulator + sum(highboundoldweights * ((pslices > (1 - pcrit /
                                                                         2)) + (pslices < (pcrit / 2)) +
                                                             ((pslices >
                                                                 (1 - 1 / 2)) * (pslices <= (1 - pcrit / 2)) + (pslices < (1 / 2)) * (pslices >=
                                                                                                                                        (pcrit / 2))
                                                             ) *
                                                             (1 -
                                                                pnorm((
                                                                  qnorm(1 - pfinal / 2) * (naccumulator + ns[length(ns)]) ^ .5 - naccumulator ^
                                                                    .5 * qnorm(pslices)
                                                                ) / ns[length(ns)] ^ .5
                                                                ) +
                                                                pnorm((
                                                                  -qnorm(1 - pfinal / 2) * (naccumulator + ns[length(ns)]) ^ .5 - naccumulator ^
                                                                    .5 * qnorm(pslices)
                                                                ) / ns[length(ns)] ^ .5
                                                                ))))
    }
    return(paste(
      "[",
      lowboundpaccumulator,
      ", ",
      highboundpaccumulator,
      "]",
      sep = ""
    ))
  }


p_crit <- pcrit(c(60, 120), pmax = 0.2, pdesired = 0.05, slices = 1000, tails = 2)
