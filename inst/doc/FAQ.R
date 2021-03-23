## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, eval = TRUE, echo = FALSE-----------------------------------------
library(bpnreg)

## ---- eval = FALSE------------------------------------------------------------
#  library(bpnreg)
#  ?Maps
#  ?Motor

## ---- eval = FALSE------------------------------------------------------------
#  bpnr(Phaserad ~ Cond + AvAmp, data = Motor)

## ---- eval = FALSE------------------------------------------------------------
#  bpnme(Error.rad ~ Maze + Trial.type + (1|Subject),data =  Maps, its = 100)

## ---- eval = FALSE------------------------------------------------------------
#  bpnr(Phaserad ~ Cond + AvAmp + Cond:AvAmp, data = Motor)

## ---- eval = FALSE------------------------------------------------------------
#  bpnr(Phaserad ~ Cond*AvAmp, data = Motor)

## ---- eval = FALSE------------------------------------------------------------
#  bpnr(Phaserad ~ Cond + AvAmp, data = Motor, seed = 101)

## ---- echo = FALSE, results = FALSE-------------------------------------------
fit <- bpnr(Phaserad ~ Cond + AvAmp, data = Motor, seed = 101)

## ---- eval = FALSE------------------------------------------------------------
#  fit <- bpnr(Phaserad ~ Cond + AvAmp, data = Motor, seed = 101)

## ---- results = FALSE---------------------------------------------------------
coef_circ(fit, type = "continuous", units = "degrees")
coef_circ(fit, type = "categorical", units = "degrees")
coef_circ(fit, type = "continuous", units = "radians")
coef_circ(fit, type = "categorical", units = "radians")

## ---- eval = FALSE------------------------------------------------------------
#  fit <- bpnr(Phaserad ~ Cond + AvAmp, data = Motor, seed = 101)

## ---- eval = FALSE------------------------------------------------------------
#  coef_circ(fit, type = "continuous", units = "degrees")

## ---- echo = FALSE------------------------------------------------------------
coef_circ(fit, type = "continuous", units = "degrees")

## ---- eval = FALSE------------------------------------------------------------
#  coef_circ(fit, type = "categorical", units = "degrees")

## ---- echo = FALSE------------------------------------------------------------
coef_circ(fit, type = "categorical", units = "degrees")

## -----------------------------------------------------------------------------
fit(fit)

## -----------------------------------------------------------------------------
head(fit$beta1)

## -----------------------------------------------------------------------------
head(fit$a.x)

## ---- echo = FALSE, results = FALSE-------------------------------------------
fitme <- bpnme(Error.rad ~ Maze + Trial.type + (1|Subject), Maps)

## ---- eval = FALSE------------------------------------------------------------
#  fitme <- bpnme(Error.rad ~ Maze + Trial.type + (1|Subject), Maps)

## -----------------------------------------------------------------------------
mean(fitme$cRI)

## -----------------------------------------------------------------------------
apply(fitme$circular.ri, 1, mean_circ)*180/pi

## -----------------------------------------------------------------------------

a1 <- fitme$beta1[,"(Intercept)"]
a2 <- fitme$beta2[,"(Intercept)"]
b1 <- fitme$beta1[,"Trial.type1"]
b2 <- fitme$beta2[,"Trial.type1"]

zeta_standard <- sqrt((a1)^2 + (a2 + b2)^2)^2/4
var_standard  <- 1 - sqrt((pi * zeta_standard)/2) * exp(-zeta_standard) *
                        (besselI(zeta_standard, 0) + besselI(zeta_standard, 1))

zeta_probe <- sqrt((a1 + b1)^2 + (a2 + b2)^2)^2/4
var_probe  <- 1 - sqrt((pi * zeta_probe)/2) * exp(-zeta_probe) *
                        (besselI(zeta_probe, 0) + besselI(zeta_probe, 1))

standard <- c(mode_est(var_standard),
              mean(var_standard),
              sd(var_standard),
              hpd_est(var_standard))
probe <- c(mode_est(var_probe),
           mean(var_probe),
           sd(var_probe),
           hpd_est(var_probe))

results <- rbind(standard, probe)

colnames(results) <- c("mode", "mean", "sd", "HPD LB", "HPD UB")
rownames(results) <- c("standard", "probe")


## -----------------------------------------------------------------------------
results

