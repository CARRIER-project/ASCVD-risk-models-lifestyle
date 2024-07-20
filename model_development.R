# Packages ----

library(haven)
library(mice)
library(cmprsk)
library(survival)
library(riskRegression)
library(prodlim)

# Set seed ----

set.seed(...)

# Data ----

...

# Descriptive analysis ----

## Participants without diabetes ----

nrow(subset(data, GTS.diabetes_all_types_1 == 0))

table(subset(data, GTS.diabetes_all_types_1 == 0)$event, exclude = NULL)
prop.table(table(subset(data, GTS.diabetes_all_types_1 == 0)$event, exclude = NULL))

summary(subset(data, GTS.diabetes_all_types_1 == 0)$time)
sd(subset(data, GTS.diabetes_all_types_1 == 0)$time, na.rm = TRUE)

summary(subset(data, GTS.diabetes_all_types_1 == 0)$max_time)
sd(subset(data, GTS.diabetes_all_types_1 == 0)$max_time, na.rm = TRUE)

table(subset(data, GTS.diabetes_all_types_1 == 0)$sex.male_1, exclude = NULL)
prop.table(table(subset(data, GTS.diabetes_all_types_1 == 0)$sex.male_1, exclude = NULL))

summary(subset(data, GTS.diabetes_all_types_1 == 0)$age)
sd(subset(data, GTS.diabetes_all_types_1 == 0)$age, na.rm = TRUE)

table(subset(data, GTS.diabetes_all_types_1 == 0)$pre_existing_ASCVD.yes_1, exclude = FALSE)
prop.table(table(subset(data, GTS.diabetes_all_types_1 == 0)$pre_existing_ASCVD.yes_1, exclude = FALSE))

summary(subset(data, GTS.diabetes_all_types_1 == 0)$SESWOA_2014)
sd(subset(data, GTS.diabetes_all_types_1 == 0)$SESWOA_2014, na.rm = TRUE)
sum(is.na(subset(data, GTS.diabetes_all_types_1 == 0)$SESWOA_2014)) / nrow(subset(data, GTS.diabetes_all_types_1 == 0))

summary(subset(data, GTS.diabetes_all_types_1 == 0)$SBP)
sd(subset(data, GTS.diabetes_all_types_1 == 0)$SBP, na.rm = TRUE)
sum(is.na(subset(data, GTS.diabetes_all_types_1 == 0)$SBP)) / nrow(subset(data, GTS.diabetes_all_types_1 == 0))

table(subset(data, GTS.diabetes_all_types_1 == 0)$treatment_current.BP_1, exclude = FALSE)
prop.table(table(subset(data, GTS.diabetes_all_types_1 == 0)$treatment_current.BP_1, exclude = FALSE))

summary(subset(data, GTS.diabetes_all_types_1 == 0)$TC)
sd(subset(data, GTS.diabetes_all_types_1 == 0)$TC, na.rm = TRUE)
sum(is.na(subset(data, GTS.diabetes_all_types_1 == 0)$TC)) / nrow(subset(data, GTS.diabetes_all_types_1 == 0))

summary(subset(data, GTS.diabetes_all_types_1 == 0)$HDL)
sd(subset(data, GTS.diabetes_all_types_1 == 0)$HDL, na.rm = TRUE)
sum(is.na(subset(data, GTS.diabetes_all_types_1 == 0)$HDL)) / nrow(subset(data, GTS.diabetes_all_types_1 == 0))

table(subset(data, GTS.diabetes_all_types_1 == 0)$treatment_current.lipids_1, exclude = FALSE)
prop.table(table(subset(data, GTS.diabetes_all_types_1 == 0)$treatment_current.lipids_1, exclude = FALSE))

table(subset(data, GTS.diabetes_all_types_1 == 0)$smoking.3_cat, exclude = FALSE)
prop.table(table(subset(data, GTS.diabetes_all_types_1 == 0)$smoking.3_cat, exclude = FALSE))

summary(subset(data, GTS.diabetes_all_types_1 == 0)$MVPA)
sd(subset(data, GTS.diabetes_all_types_1 == 0)$MVPA, na.rm = TRUE)
sum(is.na(subset(data, GTS.diabetes_all_types_1 == 0)$MVPA)) / nrow(subset(data, GTS.diabetes_all_types_1 == 0))

summary(subset(data, GTS.diabetes_all_types_1 == 0)$DHD)
sd(subset(data, GTS.diabetes_all_types_1 == 0)$DHD, na.rm = TRUE)
sum(is.na(subset(data, GTS.diabetes_all_types_1 == 0)$DHD)) / nrow(subset(data, GTS.diabetes_all_types_1 == 0))

## Participants with diabetes ----

nrow(subset(data, GTS.diabetes_all_types_1 == 1))

table(subset(data, GTS.diabetes_all_types_1 == 1)$event, exclude = NULL)
prop.table(table(subset(data, GTS.diabetes_all_types_1 == 1)$event, exclude = NULL))

summary(subset(data, GTS.diabetes_all_types_1 == 1)$time)
sd(subset(data, GTS.diabetes_all_types_1 == 1)$time, na.rm = TRUE)

summary(subset(data, GTS.diabetes_all_types_1 == 1)$max_time)
sd(subset(data, GTS.diabetes_all_types_1 == 1)$max_time, na.rm = TRUE)

table(subset(data, GTS.diabetes_all_types_1 == 1)$sex.male_1, exclude = NULL)
prop.table(table(subset(data, GTS.diabetes_all_types_1 == 1)$sex.male_1, exclude = NULL))

summary(subset(data, GTS.diabetes_all_types_1 == 1)$age)
sd(subset(data, GTS.diabetes_all_types_1 == 1)$age, na.rm = TRUE)

table(subset(data, GTS.diabetes_all_types_1 == 1)$pre_existing_ASCVD.yes_1, exclude = FALSE)
prop.table(table(subset(data, GTS.diabetes_all_types_1 == 1)$pre_existing_ASCVD.yes_1, exclude = FALSE))

summary(subset(data, GTS.diabetes_all_types_1 == 1)$SESWOA_2014)
sd(subset(data, GTS.diabetes_all_types_1 == 1)$SESWOA_2014, na.rm = TRUE)
sum(is.na(subset(data, GTS.diabetes_all_types_1 == 1)$SESWOA_2014)) / nrow(subset(data, GTS.diabetes_all_types_1 == 1))

summary(subset(data, GTS.diabetes_all_types_1 == 1)$SBP)
sd(subset(data, GTS.diabetes_all_types_1 == 1)$SBP, na.rm = TRUE)
sum(is.na(subset(data, GTS.diabetes_all_types_1 == 1)$SBP)) / nrow(subset(data, GTS.diabetes_all_types_1 == 1))

table(subset(data, GTS.diabetes_all_types_1 == 1)$treatment_current.BP_1, exclude = FALSE)
prop.table(table(subset(data, GTS.diabetes_all_types_1 == 1)$treatment_current.BP_1, exclude = FALSE))

summary(subset(data, GTS.diabetes_all_types_1 == 1)$TC)
sd(subset(data, GTS.diabetes_all_types_1 == 1)$TC, na.rm = TRUE)
sum(is.na(subset(data, GTS.diabetes_all_types_1 == 1)$TC)) / nrow(subset(data, GTS.diabetes_all_types_1 == 1))

summary(subset(data, GTS.diabetes_all_types_1 == 1)$HDL)
sd(subset(data, GTS.diabetes_all_types_1 == 1)$HDL, na.rm = TRUE)
sum(is.na(subset(data, GTS.diabetes_all_types_1 == 1)$HDL)) / nrow(subset(data, GTS.diabetes_all_types_1 == 1))

table(subset(data, GTS.diabetes_all_types_1 == 1)$treatment_current.lipids_1, exclude = FALSE)
prop.table(table(subset(data, GTS.diabetes_all_types_1 == 1)$treatment_current.lipids_1, exclude = FALSE))

table(subset(data, GTS.diabetes_all_types_1 == 1)$smoking.3_cat, exclude = FALSE)
prop.table(table(subset(data, GTS.diabetes_all_types_1 == 1)$smoking.3_cat, exclude = FALSE))

summary(subset(data, GTS.diabetes_all_types_1 == 1)$MVPA)
sd(subset(data, GTS.diabetes_all_types_1 == 1)$MVPA, na.rm = TRUE)
sum(is.na(subset(data, GTS.diabetes_all_types_1 == 1)$MVPA)) / nrow(subset(data, GTS.diabetes_all_types_1 == 1))

summary(subset(data, GTS.diabetes_all_types_1 == 1)$DHD)
sd(subset(data, GTS.diabetes_all_types_1 == 1)$DHD, na.rm = TRUE)
sum(is.na(subset(data, GTS.diabetes_all_types_1 == 1)$DHD)) / nrow(subset(data, GTS.diabetes_all_types_1 == 1))

# CI ----

CI <- cuminc(ftime = data$time,
             fstatus = data$event,
             cencode = 0)
CI
plot(CI)

timepoints(CI, c(5, 10))

table(subset(data, max_time > 9.5)$event)
table(subset(data, max_time > 10)$event)

CI.no_diabetes <- cuminc(ftime = subset(data, GTS.diabetes_all_types_1 == 0)$time,
                         fstatus = subset(data, GTS.diabetes_all_types_1 == 0)$event,
                         cencode = 0)
CI.no_diabetes
plot(CI.no_diabetes)

timepoints(CI.no_diabetes, c(5, 10))

table(subset(data, (GTS.diabetes_all_types_1 == 0) & (max_time > 9.5))$event)
table(subset(data, (GTS.diabetes_all_types_1 == 0) & (max_time > 10))$event)

CI.diabetes <- cuminc(ftime = subset(data, GTS.diabetes_all_types_1 == 1)$time,
                      fstatus = subset(data, GTS.diabetes_all_types_1 == 1)$event,
                      cencode = 0)
CI.diabetes
plot(CI.no_diabetes)

timepoints(CI.diabetes, c(5, 10))

table(subset(data, (GTS.diabetes_all_types_1 == 1) & (max_time > 9.5))$event)
table(subset(data, (GTS.diabetes_all_types_1 == 1) & (max_time > 10))$event)

# Standardising continuous variables ----

data$age.std <- (data$age - mean(data$age, na.rm = TRUE)) / sd(data$age, na.rm = TRUE)

data$SESWOA_2014.std <- (data$SESWOA_2014 - mean(data$SESWOA_2014, na.rm = TRUE)) / sd(data$SESWOA_2014, na.rm = TRUE)

data$SBP.std <- (data$SBP - mean(data$SBP, na.rm = TRUE)) / sd(data$SBP, na.rm = TRUE)

data$TC.std <- (data$TC - mean(data$TC, na.rm = TRUE)) / sd(data$TC, na.rm = TRUE)

data$HDL.std <- (data$HDL - mean(data$HDL, na.rm = TRUE)) / sd(data$HDL, na.rm = TRUE)

data$MVPA.std <- (data$MVPA - mean(data$MVPA, na.rm = TRUE)) / sd(data$MVPA, na.rm = TRUE)

data$DHD.std <- (data$DHD - mean(data$DHD, na.rm = TRUE)) / sd(data$DHD, na.rm = TRUE)

# Model 1 ----

# Imputations

n_imp <- 10

data <- data[, c("time",
                 "event",
                 "age.std", 
                 "sex.male_1",
                 "SESWOA_2014.std",
                 "GTS.diabetes_all_types_1",
                 "pre_existing_ASCVD.yes_1",
                 "SBP.std",
                 "treatment_current.BP_1",
                 "TC.std",
                 "HDL.std",
                 "treatment_current.lipids_1",
                 "smoking.3_cat",
                 "MVPA.std",
                 "DHD.std")]

data <- sapply(data, zap_labels)

imp <- mice(data,
            method = "pmm", 
            m = n_imp)

imp.stacked <- list()
for(i in 1:n_imp) {
  imp.stacked[[i]] <- complete(imp, 
                               i)
  
  imp.stacked[[i]][["smoking.current_1"]] <- ifelse(imp.stacked[[i]][["smoking.3_cat"]] == 3,
                                                    1,
                                                    0)
  imp.stacked[[i]][["smoking.ex_1"]] <- ifelse(imp.stacked[[i]][["smoking.3_cat"]] == 2,
                                               1,
                                               0)
}

# Model

fit_1.stacked <- list()
for(i in 1:n_imp) {
  fit_1.stacked[[i]] <- FGR(Hist(time, event) ~
                              age.std +
                              sex.male_1 +
                              SESWOA_2014.std +
                              GTS.diabetes_all_types_1 +
                              pre_existing_ASCVD.yes_1 +
                              SBP.std +
                              treatment_current.BP_1 +
                              TC.std +
                              HDL.std +
                              treatment_current.lipids_1 +
                              smoking.current_1 +
                              smoking.ex_1 +
                              MVPA.std +
                              DHD.std, 
                            cause = 1,
                            data = imp.stacked[[i]])
}

new_data <- as.data.frame(matrix(nrow = 1,
                                 ncol = 14))
colnames(new_data) <- c("age.std",
                        "sex.male_1",
                        "SESWOA_2014.std",
                        "GTS.diabetes_all_types_1",
                        "pre_existing_ASCVD.yes_1",
                        "SBP.std",
                        "treatment_current.BP_1",
                        "TC.std",
                        "HDL.std",
                        "treatment_current.lipids_1",
                        "smoking.current_1",
                        "smoking.ex_1",
                        "MVPA.std",
                        "DHD.std")
new_data[1,] <- 0

fit_1.baseline.5Y <- vector()
for(i in 1:n_imp) {
  fit_1.baseline.5Y <- append(fit_1.baseline.5Y, predictRisk(fit_1.stacked[[i]], new_data, times = 5))
}
fit_1.baseline.5Y <- mean(fit_1.baseline.5Y)
fit_1.baseline.5Y

fit_1.baseline.10Y <- vector()
for(i in 1:n_imp) {
  fit_1.baseline.10Y <- append(fit_1.baseline.10Y, predictRisk(fit_1.stacked[[i]], new_data, times = 10))
}
fit_1.baseline.10Y <- mean(fit_1.baseline.10Y)
fit_1.baseline.10Y

fit_1.coef <- as.data.frame(matrix(nrow = 14,
                                   ncol = (1 + n_imp)))
colnames(fit_1.coef) <- c("x",
                          paste0("imp_", 1:n_imp))
fit_1.coef[1, "x"] <- "age.std"
fit_1.coef[2, "x"] <- "sex.male_1"
fit_1.coef[3, "x"] <- "SESWOA_2014.std"
fit_1.coef[4, "x"] <- "GTS.diabetes_all_types_1"
fit_1.coef[5, "x"] <- "pre_existing_ASCVD.yes_1"
fit_1.coef[6, "x"] <- "SBP.std"
fit_1.coef[7, "x"] <- "treatment_current.BP_1"
fit_1.coef[8, "x"] <- "TC.std"
fit_1.coef[9, "x"] <- "HDL.std"
fit_1.coef[10, "x"] <- "treatment_current.lipids_1"
fit_1.coef[11, "x"] <- "smoking.current_1"
fit_1.coef[12, "x"] <- "smoking.ex_1"
fit_1.coef[13, "x"] <- "MVPA.std"
fit_1.coef[14, "x"] <- "DHD.std"

for(i in 1:n_imp) {
  for(j in 1:14) {
    fit_1.coef[j, 1+i] <- fit_1.stacked[[i]][["crrFit"]][["coef"]][[j]] 
  }
}

fit_1.coef$coef <- apply(fit_1.coef[, 2:(1 + n_imp)],
                         1,
                         mean)
fit_1.coef <- fit_1.coef[, c("x",
                             "coef")]
fit_1.coef$e_coef <- exp(fit_1.coef$coef)

fit_1.coef

fit_1.pred.5Y.fun <- function(age.std,
                              sex.male_1,
                              SESWOA_2014.std,
                              GTS.diabetes_all_types_1,
                              pre_existing_ASCVD.yes_1,
                              SBP.std,
                              treatment_current.BP_1,
                              TC.std,
                              HDL.std,
                              treatment_current.lipids_1,
                              smoking.current_1,
                              smoking.ex_1,
                              MVPA.std,
                              DHD.std) {
  (1 - ((1 - fit_1.baseline.5Y) ** exp(subset(fit_1.coef, x == "age.std")$coef * age.std +
                                       subset(fit_1.coef, x == "sex.male_1")$coef * sex.male_1 +
                                       subset(fit_1.coef, x == "SESWOA_2014.std")$coef * SESWOA_2014.std +
                                       subset(fit_1.coef, x == "GTS.diabetes_all_types_1")$coef * GTS.diabetes_all_types_1 +
                                       subset(fit_1.coef, x == "pre_existing_ASCVD.yes_1")$coef * pre_existing_ASCVD.yes_1 +
                                       subset(fit_1.coef, x == "SBP.std")$coef * SBP.std +
                                       subset(fit_1.coef, x == "treatment_current.BP_1")$coef * treatment_current.BP_1 +
                                       subset(fit_1.coef, x == "TC.std")$coef * TC.std +
                                       subset(fit_1.coef, x == "HDL.std")$coef * HDL.std +
                                       subset(fit_1.coef, x == "treatment_current.lipids_1")$coef * treatment_current.lipids_1 +
                                       subset(fit_1.coef, x == "smoking.current_1")$coef * smoking.current_1 +
                                       subset(fit_1.coef, x == "smoking.ex_1")$coef * smoking.ex_1 +
                                       subset(fit_1.coef, x == "MVPA.std")$coef * MVPA.std +
                                       subset(fit_1.coef, x == "DHD.std")$coef * DHD.std          
  )))
}

fit_1.pred.10Y.fun <- function(age.std,
                               sex.male_1,
                               SESWOA_2014.std,
                               GTS.diabetes_all_types_1,
                               pre_existing_ASCVD.yes_1,
                               SBP.std,
                               treatment_current.BP_1,
                               TC.std,
                               HDL.std,
                               treatment_current.lipids_1,
                               smoking.current_1,
                               smoking.ex_1,
                               MVPA.std,
                               DHD.std) {
  (1 - ((1 - fit_1.baseline.10Y) ** exp(subset(fit_1.coef, x == "age.std")$coef * age.std +
                                        subset(fit_1.coef, x == "sex.male_1")$coef * sex.male_1 +
                                        subset(fit_1.coef, x == "SESWOA_2014.std")$coef * SESWOA_2014.std +
                                        subset(fit_1.coef, x == "GTS.diabetes_all_types_1")$coef * GTS.diabetes_all_types_1 +
                                        subset(fit_1.coef, x == "pre_existing_ASCVD.yes_1")$coef * pre_existing_ASCVD.yes_1 +
                                        subset(fit_1.coef, x == "SBP.std")$coef * SBP.std +
                                        subset(fit_1.coef, x == "treatment_current.BP_1")$coef * treatment_current.BP_1 +
                                        subset(fit_1.coef, x == "TC.std")$coef * TC.std +
                                        subset(fit_1.coef, x == "HDL.std")$coef * HDL.std +
                                        subset(fit_1.coef, x == "treatment_current.lipids_1")$coef * treatment_current.lipids_1 +
                                        subset(fit_1.coef, x == "smoking.current_1")$coef * smoking.current_1 +
                                        subset(fit_1.coef, x == "smoking.ex_1")$coef * smoking.ex_1 +
                                        subset(fit_1.coef, x == "MVPA.std")$coef * MVPA.std +
                                        subset(fit_1.coef, x == "DHD.std")$coef * DHD.std          
  )))
}

# AUC

fit_1.AUC.5Y <- as.data.frame(matrix(nrow = 1,
                                     ncol = n_imp))
colnames(fit_1.AUC.5Y) <- c(paste0("imp_", 1:n_imp))
  
for(i in 1:n_imp) {
  fit_1.AUC.5Y[1, i] <- Score(list(fit_1.stacked[[1]]), 
                              formula = Hist(time, event) ~ 1,
                              data = imp.stacked[[i]],
                              metrics = "auc",
                              times = 5,
                              se.fit = FALSE)[["AUC"]][["score"]][["AUC"]]
}

fit_1.AUC.5Y <- apply(fit_1.AUC.5Y,
                      1,
                      mean)
fit_1.AUC.5Y

fit_1.AUC.10Y <- as.data.frame(matrix(nrow = 1,
                                      ncol = n_imp))
colnames(fit_1.AUC.10Y) <- c(paste0("imp_", 1:n_imp))

for(i in 1:n_imp) {
  fit_1.AUC.10Y[1, i] <- Score(list(fit_1.stacked[[1]]), 
                               formula = Hist(time, event) ~ 1,
                               data = imp.stacked[[i]],
                               metrics = "auc",
                               times = 10,
                               se.fit = FALSE)[["AUC"]][["score"]][["AUC"]]
}

fit_1.AUC.10Y <- apply(fit_1.AUC.10Y,
                       1,
                       mean)
fit_1.AUC.10Y

# Calibration

for(i in 1:n_imp) {
  imp.stacked[[i]][["fit_1.pred.5Y"]] <- fit_1.pred.5Y.fun(age.std = imp.stacked[[i]][["age.std"]],
                                                           sex.male_1 = imp.stacked[[i]][["sex.male_1"]],
                                                           SESWOA_2014.std = imp.stacked[[i]][["SESWOA_2014.std"]],
                                                           GTS.diabetes_all_types_1 = imp.stacked[[i]][["GTS.diabetes_all_types_1"]],
                                                           pre_existing_ASCVD.yes_1 = imp.stacked[[i]][["pre_existing_ASCVD.yes_1"]],
                                                           SBP.std = imp.stacked[[i]][["SBP.std"]],
                                                           treatment_current.BP_1 = imp.stacked[[i]][["treatment_current.BP_1"]],
                                                           TC.std = imp.stacked[[i]][["TC.std"]],
                                                           HDL.std = imp.stacked[[i]][["HDL.std"]],
                                                           treatment_current.lipids_1 = imp.stacked[[i]][["treatment_current.lipids_1"]],
                                                           smoking.current_1 = imp.stacked[[i]][["smoking.current_1"]],
                                                           smoking.ex_1 = imp.stacked[[i]][["smoking.ex_1"]],
                                                           MVPA.std = imp.stacked[[i]][["MVPA.std"]],
                                                           DHD.std = imp.stacked[[i]][["DHD.std"]])
}

fit_1.CAL.5Y.x <- list()
fit_1.CAL.5Y.y <- list()
for(i in 1:n_imp) {
  imp.stacked[[i]][["fit_1.pred.5Y.decile"]] <- cut(imp.stacked[[i]][["fit_1.pred.5Y"]],
                                                    breaks = quantile(imp.stacked[[i]][["fit_1.pred.5Y"]], prob = seq(0, 1, 0.1)),
                                                    labels = c(1:10),
                                                    include.lowest = TRUE)
  temp.fit_1.CAL.5Y.x <- aggregate(imp.stacked[[i]][["fit_1.pred.5Y"]],
                                   list(imp.stacked[[i]][["fit_1.pred.5Y.decile"]]),
                                   mean)
  colnames(temp.fit_1.CAL.5Y.x) <- c("fit_1.pred.5Y.decile",
                                     "fit_1.pred.5Y.mean")
  fit_1.CAL.5Y.x <- append(fit_1.CAL.5Y.x, list(temp.fit_1.CAL.5Y.x))
  
  temp.fit_1.CAL.5Y.y <- as.data.frame(matrix(nrow = 10,
                                              ncol = 2))
  colnames(temp.fit_1.CAL.5Y.y) <- c("fit_1.pred.5Y.decile",
                                     "CI.ASCVD_composite_event.5Y")
  temp.fit_1.CAL.5Y.y$fit_1.pred.5Y.decile <- 1:10
  for(d in 1:10) {
    temp.CI <- cuminc(ftime = subset(imp.stacked[[i]], fit_1.pred.5Y.decile == d)$time,
                      fstatus = subset(imp.stacked[[i]], fit_1.pred.5Y.decile == d)$event,
                      cencode = 0)
    
    temp.fit_1.CAL.5Y.y[d, "CI.ASCVD_composite_event.5Y"] <- timepoints(temp.CI, 5)[[1]][[1]]
  }
  fit_1.CAL.5Y.y <- append(fit_1.CAL.5Y.y, list(temp.fit_1.CAL.5Y.y))
}

fit_1.CAL.5Y <- as.data.frame(matrix(nrow = 10,
                                     ncol = 3))
colnames(fit_1.CAL.5Y) <- c("fit_1.pred.5Y.decile",
                            "fit_1.pred.5Y.mean.mean",
                            "ASCVD_composite_event.mean.mean")
fit_1.CAL.5Y$fit_1.pred.5Y.decile <- 1:10
temp <- vector()
for(d in 1:10) {
  for(i in 1:n_imp){
    temp <- append(temp, fit_1.CAL.5Y.x[[i]][[2]][[d]])
  }
  fit_1.CAL.5Y[d, "fit_1.pred.5Y.mean.mean"] <- mean(temp)
}
temp <- vector()
for(d in 1:10) {
  for(i in 1:n_imp) {
    temp <- append(temp, fit_1.CAL.5Y.y[[i]][[2]][[d]])
  }
  fit_1.CAL.5Y[d, "ASCVD_composite_event.mean.mean"] <- mean(temp)
}

fit_1.CAL.5Y

for(i in 1:n_imp) {
  imp.stacked[[i]][["fit_1.pred.10Y"]] <- fit_1.pred.10Y.fun(age.std = imp.stacked[[i]][["age.std"]],
                                                             sex.male_1 = imp.stacked[[i]][["sex.male_1"]],
                                                             SESWOA_2014.std = imp.stacked[[i]][["SESWOA_2014.std"]],
                                                             GTS.diabetes_all_types_1 = imp.stacked[[i]][["GTS.diabetes_all_types_1"]],
                                                             pre_existing_ASCVD.yes_1 = imp.stacked[[i]][["pre_existing_ASCVD.yes_1"]],
                                                             SBP.std = imp.stacked[[i]][["SBP.std"]],
                                                             treatment_current.BP_1 = imp.stacked[[i]][["treatment_current.BP_1"]],
                                                             TC.std = imp.stacked[[i]][["TC.std"]],
                                                             HDL.std = imp.stacked[[i]][["HDL.std"]],
                                                             treatment_current.lipids_1 = imp.stacked[[i]][["treatment_current.lipids_1"]],
                                                             smoking.current_1 = imp.stacked[[i]][["smoking.current_1"]],
                                                             smoking.ex_1 = imp.stacked[[i]][["smoking.ex_1"]],
                                                             MVPA.std = imp.stacked[[i]][["MVPA.std"]],
                                                             DHD.std = imp.stacked[[i]][["DHD.std"]])
}

fit_1.CAL.10Y.x <- list()
fit_1.CAL.10Y.y <- list()
for(i in 1:n_imp) {
  imp.stacked[[i]][["fit_1.pred.10Y.decile"]] <- cut(imp.stacked[[i]][["fit_1.pred.10Y"]],
                                                     breaks = quantile(imp.stacked[[i]][["fit_1.pred.10Y"]], prob = seq(0, 1, 0.1)),
                                                     labels = c(1:10),
                                                     include.lowest = TRUE)
  temp.fit_1.CAL.10Y.x <- aggregate(imp.stacked[[i]][["fit_1.pred.10Y"]],
                                    list(imp.stacked[[i]][["fit_1.pred.10Y.decile"]]),
                                    mean)
  colnames(temp.fit_1.CAL.10Y.x) <- c("fit_1.pred.10Y.decile",
                                      "fit_1.pred.10Y.mean")
  fit_1.CAL.10Y.x <- append(fit_1.CAL.10Y.x, list(temp.fit_1.CAL.10Y.x))
  
  temp.fit_1.CAL.10Y.y <- as.data.frame(matrix(nrow = 10,
                                               ncol = 2))
  colnames(temp.fit_1.CAL.10Y.y) <- c("fit_1.pred.10Y.decile",
                                      "CI.ASCVD_composite_event.10Y")
  temp.fit_1.CAL.10Y.y$fit_1.pred.10Y.decile <- 1:10
  for(d in 1:10) {
    temp.CI <- cuminc(ftime = subset(imp.stacked[[i]], fit_1.pred.10Y.decile == d)$time,
                      fstatus = subset(imp.stacked[[i]], fit_1.pred.10Y.decile == d)$event,
                      cencode = 0)
    
    temp.fit_1.CAL.10Y.y[d, "CI.ASCVD_composite_event.10Y"] <- timepoints(temp.CI, 10)[[1]][[1]]
  }
  fit_1.CAL.10Y.y <- append(fit_1.CAL.10Y.y, list(temp.fit_1.CAL.10Y.y))
}

fit_1.CAL.10Y <- as.data.frame(matrix(nrow = 10,
                                     ncol = 3))
colnames(fit_1.CAL.10Y) <- c("fit_1.pred.10Y.decile",
                             "fit_1.pred.10Y.mean.mean",
                             "CI.ASCVD_composite_event.10Y.mean")
fit_1.CAL.10Y$fit_1.pred.10Y.decile <- 1:10
temp <- vector()
for(d in 1:10) {
  for(i in 1:n_imp) {
    temp <- append(temp, fit_1.CAL.10Y.x[[i]][[2]][[d]])
  }
  fit_1.CAL.10Y[d, "fit_1.pred.10Y.mean.mean"] <- mean(temp)
}
temp <- vector()
for(d in 1:10) {
  for(i in 1:n_imp) {
    temp <- append(temp, fit_1.CAL.10Y.y[[i]][[2]][[d]])
  }
  fit_1.CAL.10Y[d, "CI.ASCVD_composite_event.10Y.mean"] <- mean(temp)
}

fit_1.CAL.10Y

## Bootstrap ----

n_b <- 100

# Object to capture output model coefficients 

boot.fit_1.coef <- as.data.frame(matrix(nrow = 14,
                                        ncol = 1 + n_b))
colnames(boot.fit_1.coef) <- c("x",
                               paste0("boot_", 1:n_b, ".coef"))
boot.fit_1.coef[1, "x"] <- "age.std"
boot.fit_1.coef[2, "x"] <- "sex.male_1"
boot.fit_1.coef[3, "x"] <- "SESWOA_2014.std"
boot.fit_1.coef[4, "x"] <- "GTS.diabetes_all_types_1"
boot.fit_1.coef[5, "x"] <- "pre_existing_ASCVD.yes_1"
boot.fit_1.coef[6, "x"] <- "SBP.std"
boot.fit_1.coef[7, "x"] <- "treatment_current.BP_1"
boot.fit_1.coef[8, "x"] <- "TC.std"
boot.fit_1.coef[9, "x"] <- "HDL.std"
boot.fit_1.coef[10, "x"] <- "treatment_current.lipids_1"
boot.fit_1.coef[11, "x"] <- "smoking.current_1"
boot.fit_1.coef[12, "x"] <- "smoking.ex_1"
boot.fit_1.coef[13, "x"] <- "MVPA.std"
boot.fit_1.coef[14, "x"] <- "DHD.std"

# Objects to capture output AUC

boot.fit_1.AUC.5Y <- as.data.frame(matrix(nrow = 1,
                                          ncol = n_b))
colnames(boot.fit_1.AUC.5Y ) <- c(paste0("boot_", 1:n_b, ".AUC.5Y"))

boot.fit_1.AUC.10Y <- as.data.frame(matrix(nrow = 1,
                                           ncol = n_b))
colnames(boot.fit_1.AUC.10Y) <- c(paste0("boot_", 1:n_b, ".AUC.10Y"))

# Objects to capture output calibration

boot.fit_1.CAL.5Y <- as.data.frame(matrix(nrow = 0,
                                          ncol = 3))
colnames(boot.fit_1.CAL.5Y) <- c("boot",
                                 "boot.fit_1.pred.5Y.decile",
                                 "boot.fit_1.pred.5Y.mean.mean")

# Loop

for(b in 1:n_b) {
  boot.index <- sample(1:nrow(data), size = nrow(data), replace = TRUE)
  
  boot.data <- data[boot.index, ] 
  
  # Imputations
  
  boot.imp <- mice(boot.data,
                   method = "pmm", 
                   m = n_imp, 
                   print = FALSE)
  
  boot.imp.stacked <- list()
  for(i in 1:n_imp) {
    boot.imp.stacked[[i]] <- complete(boot.imp, 
                                      i)
    
    boot.imp.stacked[[i]][["smoking.current_1"]] <- ifelse(boot.imp.stacked[[i]][["smoking.3_cat"]] == 3,
                                                           1,
                                                           0)
    boot.imp.stacked[[i]][["smoking.ex_1"]] <- ifelse(boot.imp.stacked[[i]][["smoking.3_cat"]] == 2,
                                                      1,
                                                      0)
  }
  
  # Model
  
  boot.fit_1.stacked <- list()
  for(i in 1:n_imp) {
    boot.fit_1.stacked[[i]] <- FGR(Hist(time, event) ~
                                     age.std +
                                     sex.male_1 +
                                     SESWOA_2014.std +
                                     GTS.diabetes_all_types_1 +
                                     pre_existing_ASCVD.yes_1 +
                                     SBP.std +
                                     treatment_current.BP_1 +
                                     TC.std +
                                     HDL.std +
                                     treatment_current.lipids_1 +
                                     smoking.current_1 +
                                     smoking.ex_1 +
                                     MVPA.std +
                                     DHD.std, 
                                   cause = 1,
                                   data = boot.imp.stacked[[i]])
  }
  
  boot.fit_1.baseline.5Y <- vector()
  for(i in 1:n_imp) {
    boot.fit_1.baseline.5Y <- append(boot.fit_1.baseline.5Y, predictRisk(boot.fit_1.stacked[[i]], new_data, times = 5))
  }
  boot.fit_1.baseline.5Y <- mean(boot.fit_1.baseline.5Y)
  boot.fit_1.baseline.5Y
  
  temp.boot.fit_1.coef <- as.data.frame(matrix(nrow = 14,
                                               ncol = (1 + n_imp)))
  colnames(temp.boot.fit_1.coef) <- c("x",
                                      paste0("imp_", 1:n_imp))
  temp.boot.fit_1.coef[1, "x"] <- "age.std"
  temp.boot.fit_1.coef[2, "x"] <- "sex.male_1"
  temp.boot.fit_1.coef[3, "x"] <- "SESWOA_2014.std"
  temp.boot.fit_1.coef[4, "x"] <- "GTS.diabetes_all_types_1"
  temp.boot.fit_1.coef[5, "x"] <- "pre_existing_ASCVD.yes_1"
  temp.boot.fit_1.coef[6, "x"] <- "SBP.std"
  temp.boot.fit_1.coef[7, "x"] <- "treatment_current.BP_1"
  temp.boot.fit_1.coef[8, "x"] <- "TC.std"
  temp.boot.fit_1.coef[9, "x"] <- "HDL.std"
  temp.boot.fit_1.coef[10, "x"] <- "treatment_current.lipids_1"
  temp.boot.fit_1.coef[11, "x"] <- "smoking.current_1"
  temp.boot.fit_1.coef[12, "x"] <- "smoking.ex_1"
  temp.boot.fit_1.coef[13, "x"] <- "MVPA.std"
  temp.boot.fit_1.coef[14, "x"] <- "DHD.std"
  
  for(i in 1:n_imp) {
    for(j in 1:14) {
      temp.boot.fit_1.coef[j, 1+i] <- boot.fit_1.stacked[[i]][["crrFit"]][["coef"]][[j]] 
    }
  }
  
  temp.boot.fit_1.coef$coef <- apply(temp.boot.fit_1.coef[, 2:(1 + n_imp)],
                                     1,
                                     mean)
  
  boot.fit_1.coef[, 1 + b] <- temp.boot.fit_1.coef$coef
  
  boot.fit_1.pred.5Y.fun <- function(age.std,
                                     sex.male_1,
                                     SESWOA_2014.std,
                                     GTS.diabetes_all_types_1,
                                     pre_existing_ASCVD.yes_1,
                                     SBP.std,
                                     treatment_current.BP_1,
                                     TC.std,
                                     HDL.std,
                                     treatment_current.lipids_1,
                                     smoking.current_1,
                                     smoking.ex_1,
                                     MVPA.std,
                                     DHD.std) {
    (1 - ((1 - boot.fit_1.baseline.5Y) ** exp(subset(boot.fit_1.coef, x == "age.std")[, 1 + b] * age.std +
                                              subset(boot.fit_1.coef, x == "sex.male_1")[, 1 + b] * sex.male_1 +
                                              subset(boot.fit_1.coef, x == "SESWOA_2014.std")[, 1 + b] * SESWOA_2014.std +
                                              subset(boot.fit_1.coef, x == "GTS.diabetes_all_types_1")[, 1 + b] * GTS.diabetes_all_types_1 +
                                              subset(boot.fit_1.coef, x == "pre_existing_ASCVD.yes_1")[, 1 + b] * pre_existing_ASCVD.yes_1 +
                                              subset(boot.fit_1.coef, x == "SBP.std")[, 1 + b] * SBP.std +
                                              subset(boot.fit_1.coef, x == "treatment_current.BP_1")[, 1 + b] * treatment_current.BP_1 +
                                              subset(boot.fit_1.coef, x == "TC.std")[, 1 + b] * TC.std +
                                              subset(boot.fit_1.coef, x == "HDL.std")[, 1 + b] * HDL.std +
                                              subset(boot.fit_1.coef, x == "treatment_current.lipids_1")[, 1 + b] * treatment_current.lipids_1 +
                                              subset(boot.fit_1.coef, x == "smoking.current_1")[, 1 + b] * smoking.current_1 +
                                              subset(boot.fit_1.coef, x == "smoking.ex_1")[, 1 + b] * smoking.ex_1 +
                                              subset(boot.fit_1.coef, x == "MVPA.std")[, 1 + b] * MVPA.std +
                                              subset(boot.fit_1.coef, x == "DHD.std")[, 1 + b] * DHD.std          
    )))
    }
  
  # AUC
  
  temp.boot.fit_1.AUC.5Y <- as.data.frame(matrix(nrow = 1,
                                                 ncol = n_imp))
  colnames(temp.boot.fit_1.AUC.5Y) <- c(paste0("imp_", 1:n_imp))
  
  for(i in 1:n_imp) {
    temp.boot.fit_1.AUC.5Y[1, i] <- Score(list(boot.fit_1.stacked[[i]]), 
                                          formula = Hist(time, event) ~ 1,
                                          data = boot.imp.stacked[[i]],
                                          metrics = "auc",
                                          times = 5,
                                          se.fit = FALSE)[["AUC"]][["score"]][["AUC"]]
  }
  
  boot.fit_1.AUC.5Y[ ,b] <- apply(temp.boot.fit_1.AUC.5Y,
                                  1,
                                  mean)
  
  temp.boot.fit_1.AUC.10Y <- as.data.frame(matrix(nrow = 1,
                                                  ncol = n_imp))
  colnames(temp.boot.fit_1.AUC.10Y) <- c(paste0("imp_", 1:n_imp))
  
  for(i in 1:n_imp) {
    temp.boot.fit_1.AUC.10Y[1, i] <- Score(list(boot.fit_1.stacked[[i]]), 
                                           formula = Hist(time, event) ~ 1,
                                           data = boot.imp.stacked[[i]],
                                           metrics = "auc",
                                           times = 10,
                                           se.fit = FALSE)[["AUC"]][["score"]][["AUC"]]
  }
  
  boot.fit_1.AUC.10Y[ ,b ] <- apply(temp.boot.fit_1.AUC.10Y,
                                    1,
                                    mean)
  
  # Calibration
  
  for(i in 1:n_imp) {
    boot.imp.stacked[[i]][["boot.fit_1.pred.5Y"]] <- boot.fit_1.pred.5Y.fun(age.std = boot.imp.stacked[[i]][["age.std"]],
                                                                            sex.male_1 = boot.imp.stacked[[i]][["sex.male_1"]],
                                                                            SESWOA_2014.std = boot.imp.stacked[[i]][["SESWOA_2014.std"]],
                                                                            GTS.diabetes_all_types_1 = boot.imp.stacked[[i]][["GTS.diabetes_all_types_1"]],
                                                                            pre_existing_ASCVD.yes_1 = boot.imp.stacked[[i]][["pre_existing_ASCVD.yes_1"]],
                                                                            SBP.std = boot.imp.stacked[[i]][["SBP.std"]],
                                                                            treatment_current.BP_1 = boot.imp.stacked[[i]][["treatment_current.BP_1"]],
                                                                            TC.std = boot.imp.stacked[[i]][["TC.std"]],
                                                                            HDL.std = boot.imp.stacked[[i]][["HDL.std"]],
                                                                            treatment_current.lipids_1 = boot.imp.stacked[[i]][["treatment_current.lipids_1"]],
                                                                            smoking.current_1 = boot.imp.stacked[[i]][["smoking.current_1"]],
                                                                            smoking.ex_1 = boot.imp.stacked[[i]][["smoking.ex_1"]],
                                                                            MVPA.std = boot.imp.stacked[[i]][["MVPA.std"]],
                                                                            DHD.std = boot.imp.stacked[[i]][["DHD.std"]])
  }
  
  boot.fit_1.CAL.5Y.x <- list()
  boot.fit_1.CAL.5Y.y <- list()
  for(i in 1:n_imp) {
    boot.imp.stacked[[i]][["boot.fit_1.pred.5Y.decile"]] <- cut(boot.imp.stacked[[i]][["boot.fit_1.pred.5Y"]],
                                                                breaks = quantile(boot.imp.stacked[[i]][["boot.fit_1.pred.5Y"]], prob = seq(0, 1, 0.1)),
                                                                labels = c(1:10),
                                                                include.lowest = TRUE)
    boot.temp.fit_1.CAL.5Y.x <- aggregate(boot.imp.stacked[[i]][["boot.fit_1.pred.5Y"]],
                                          list(boot.imp.stacked[[i]][["boot.fit_1.pred.5Y.decile"]]),
                                          mean)
    colnames(boot.temp.fit_1.CAL.5Y.x) <- c("boot.fit_1.pred.5Y.decile",
                                            "boot.fit_1.pred.5Y.mean")
    boot.fit_1.CAL.5Y.x <- append(boot.fit_1.CAL.5Y.x, list(boot.temp.fit_1.CAL.5Y.x))
    }
  
  temp.boot.fit_1.CAL.5Y <- as.data.frame(matrix(nrow = 10,
                                                 ncol = 2))
  colnames(temp.boot.fit_1.CAL.5Y) <- c("boot.fit_1.pred.5Y.decile",
                                        "boot.fit_1.pred.5Y.mean.mean")
  temp.boot.fit_1.CAL.5Y$boot.fit_1.pred.5Y.decile <- 1:10
  boot.temp <- vector()
  for(d in 1:10) {
    for(i in 1:n_imp) {
      boot.temp <- append(boot.temp, boot.fit_1.CAL.5Y.x[[i]][[2]][[d]])
    }
    temp.boot.fit_1.CAL.5Y[d, "boot.fit_1.pred.5Y.mean.mean"] <- mean(boot.temp)
  }

  temp.boot.fit_1.CAL.5Y$boot <- b
  temp.boot.fit_1.CAL.5Y <- temp.boot.fit_1.CAL.5Y[, c("boot",
                                                       "boot.fit_1.pred.5Y.decile",
                                                       "boot.fit_1.pred.5Y.mean.mean")]
  boot.fit_1.CAL.5Y <- rbind(boot.fit_1.CAL.5Y, temp.boot.fit_1.CAL.5Y)
}

# 95% CI fit_1 coefficients

quantile(t(subset(boot.fit_1.coef, x =="age.std")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975))
quantile(t(subset(boot.fit_1.coef, x =="sex.male_1")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975))
quantile(t(subset(boot.fit_1.coef, x =="SESWOA_2014.std")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975))
quantile(t(subset(boot.fit_1.coef, x =="GTS.diabetes_all_types_1")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975))
quantile(t(subset(boot.fit_1.coef, x =="pre_existing_ASCVD.yes_1")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975))
quantile(t(subset(boot.fit_1.coef, x =="SBP.std")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975))
quantile(t(subset(boot.fit_1.coef, x =="treatment_current.BP_1")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975))
quantile(t(subset(boot.fit_1.coef, x =="TC.std")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975))
quantile(t(subset(boot.fit_1.coef, x =="HDL.std")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975))
quantile(t(subset(boot.fit_1.coef, x =="treatment_current.lipids_1")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975))
quantile(t(subset(boot.fit_1.coef, x =="smoking.current_1")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975))
quantile(t(subset(boot.fit_1.coef, x =="smoking.ex_1")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975))
quantile(t(subset(boot.fit_1.coef, x =="MVPA.std")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975))
quantile(t(subset(boot.fit_1.coef, x =="DHD.std")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975))

exp(quantile(t(subset(boot.fit_1.coef, x =="age.std")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975)))
exp(quantile(t(subset(boot.fit_1.coef, x =="sex.male_1")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975)))
exp(quantile(t(subset(boot.fit_1.coef, x =="SESWOA_2014.std")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975)))
exp(quantile(t(subset(boot.fit_1.coef, x =="GTS.diabetes_all_types_1")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975)))
exp(quantile(t(subset(boot.fit_1.coef, x =="pre_existing_ASCVD.yes_1")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975)))
exp(quantile(t(subset(boot.fit_1.coef, x =="SBP.std")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975)))
exp(quantile(t(subset(boot.fit_1.coef, x =="treatment_current.BP_1")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975)))
exp(quantile(t(subset(boot.fit_1.coef, x =="TC.std")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975)))
exp(quantile(t(subset(boot.fit_1.coef, x =="HDL.std")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975)))
exp(quantile(t(subset(boot.fit_1.coef, x =="treatment_current.lipids_1")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975)))
exp(quantile(t(subset(boot.fit_1.coef, x =="smoking.current_1")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975)))
exp(quantile(t(subset(boot.fit_1.coef, x =="smoking.ex_1")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975)))
exp(quantile(t(subset(boot.fit_1.coef, x =="MVPA.std")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975)))
exp(quantile(t(subset(boot.fit_1.coef, x =="DHD.std")[, 2:ncol(boot.fit_1.coef)]), c(0.025, 0.975)))

# 95% CI AUC

quantile(t(boot.fit_1.AUC.5Y), c(0.025, 0.975))

quantile(t(boot.fit_1.AUC.10Y), c(0.025, 0.975))

# 95% CI calibration

for(d in 1:10) {
  print(d)
  print(quantile(subset(boot.fit_1.CAL.5Y, boot.fit_1.pred.5Y.decile == d)$boot.fit_1.pred.5Y.mean.mean, c(0.025, 0.975)))
}

# Model 2 ----

# Model

fit_2.stacked <- list()
for(i in 1:n_imp) {
  fit_2.stacked[[i]] <- FGR(Hist(time, event) ~
                              age.std +
                              sex.male_1 +
                              SESWOA_2014.std +
                              GTS.diabetes_all_types_1 +
                              pre_existing_ASCVD.yes_1 +
                              smoking.current_1 +
                              smoking.ex_1 +
                              MVPA.std +
                              DHD.std, 
                            cause = 1,
                            data = imp.stacked[[i]])
}

fit_2.baseline.5Y <- vector()
for(i in 1:n_imp) {
  fit_2.baseline.5Y <- append(fit_2.baseline.5Y, predictRisk(fit_2.stacked[[i]], new_data, times = 5))
}
fit_2.baseline.5Y <- mean(fit_2.baseline.5Y)
fit_2.baseline.5Y

fit_2.baseline.10Y <- vector()
for(i in 1:n_imp) {
  fit_2.baseline.10Y <- append(fit_2.baseline.10Y, predictRisk(fit_2.stacked[[i]], new_data, times = 10))
}
fit_2.baseline.10Y <- mean(fit_2.baseline.10Y)
fit_2.baseline.10Y

fit_2.coef <- as.data.frame(matrix(nrow = 9,
                                   ncol = (1 + n_imp)))
colnames(fit_2.coef) <- c("x",
                          paste0("imp_", 1:n_imp))
fit_2.coef[1, "x"] <- "age.std"
fit_2.coef[2, "x"] <- "sex.male_1"
fit_2.coef[3, "x"] <- "SESWOA_2014.std"
fit_2.coef[4, "x"] <- "GTS.diabetes_all_types_1"
fit_2.coef[5, "x"] <- "pre_existing_ASCVD.yes_1"
fit_2.coef[6, "x"] <- "smoking.current_1"
fit_2.coef[7, "x"] <- "smoking.ex_1"
fit_2.coef[8, "x"] <- "MVPA.std"
fit_2.coef[9, "x"] <- "DHD.std"

for(i in 1:n_imp) {
  for (j in 1:9) {
    fit_2.coef[j, 1+i] <- fit_2.stacked[[i]][["crrFit"]][["coef"]][[j]] 
  }
}

fit_2.coef$coef <- apply(fit_2.coef[, 2:(1 + n_imp)],
                         1,
                         mean)
fit_2.coef <- fit_2.coef[, c("x",
                             "coef")]
fit_2.coef$e_coef <- exp(fit_2.coef$coef)

fit_2.coef

fit_2.pred.5Y.fun <- function(age.std,
                              sex.male_1,
                              SESWOA_2014.std,
                              GTS.diabetes_all_types_1,
                              pre_existing_ASCVD.yes_1,
                              smoking.current_1,
                              smoking.ex_1,
                              MVPA.std,
                              DHD.std) {
  (1 - ((1 - fit_2.baseline.5Y) ** exp(subset(fit_2.coef, x == "age.std")$coef * age.std +
                                       subset(fit_2.coef, x == "sex.male_1")$coef * sex.male_1 +
                                       subset(fit_2.coef, x == "SESWOA_2014.std")$coef * SESWOA_2014.std +
                                       subset(fit_2.coef, x == "GTS.diabetes_all_types_1")$coef * GTS.diabetes_all_types_1 +
                                       subset(fit_2.coef, x == "pre_existing_ASCVD.yes_1")$coef * pre_existing_ASCVD.yes_1 +
                                       subset(fit_2.coef, x == "smoking.current_1")$coef * smoking.current_1 +
                                       subset(fit_2.coef, x == "smoking.ex_1")$coef * smoking.ex_1 +
                                       subset(fit_2.coef, x == "MVPA.std")$coef * MVPA.std +
                                       subset(fit_2.coef, x == "DHD.std")$coef * DHD.std          
  )))
}

fit_2.pred.10Y.fun <- function(age.std,
                               sex.male_1,
                               SESWOA_2014.std,
                               GTS.diabetes_all_types_1,
                               pre_existing_ASCVD.yes_1,
                               smoking.current_1,
                               smoking.ex_1,
                               MVPA.std,
                               DHD.std) {
  (1 - ((1 - fit_2.baseline.10Y) ** exp(subset(fit_2.coef, x == "age.std")$coef * age.std +
                                        subset(fit_2.coef, x == "sex.male_1")$coef * sex.male_1 +
                                        subset(fit_2.coef, x == "SESWOA_2014.std")$coef * SESWOA_2014.std +
                                        subset(fit_2.coef, x == "GTS.diabetes_all_types_1")$coef * GTS.diabetes_all_types_1 +
                                        subset(fit_2.coef, x == "pre_existing_ASCVD.yes_1")$coef * pre_existing_ASCVD.yes_1 +
                                        subset(fit_2.coef, x == "smoking.current_1")$coef * smoking.current_1 +
                                        subset(fit_2.coef, x == "smoking.ex_1")$coef * smoking.ex_1 +
                                        subset(fit_2.coef, x == "MVPA.std")$coef * MVPA.std +
                                        subset(fit_2.coef, x == "DHD.std")$coef * DHD.std          
  )))
}

# AUC

fit_2.AUC.5Y <- as.data.frame(matrix(nrow = 1,
                                     ncol = n_imp))
colnames(fit_2.AUC.5Y) <- c(paste0("imp_", 1:n_imp))

for(i in 1:n_imp) {
  fit_2.AUC.5Y[1, i] <- Score(list(fit_2.stacked[[1]]), 
                              formula = Hist(time, event) ~ 1,
                              data = imp.stacked[[i]],
                              metrics = "auc",
                              times = 5,
                              se.fit = FALSE)[["AUC"]][["score"]][["AUC"]]
}

fit_2.AUC.5Y <- apply(fit_2.AUC.5Y,
                      1,
                      mean)
fit_2.AUC.5Y

fit_2.AUC.10Y <- as.data.frame(matrix(nrow = 1,
                                      ncol = n_imp))
colnames(fit_2.AUC.10Y) <- c(paste0("imp_", 1:n_imp))

for(i in 1:n_imp) {
  fit_2.AUC.10Y[1, i] <- Score(list(fit_2.stacked[[1]]), 
                               formula = Hist(time, event) ~ 1,
                               data = imp.stacked[[i]],
                               metrics = "auc",
                               times = 10,
                               se.fit = FALSE)[["AUC"]][["score"]][["AUC"]]
}

fit_2.AUC.10Y <- apply(fit_2.AUC.10Y,
                       1,
                       mean)
fit_2.AUC.10Y

# Calibration

for(i in 1:n_imp) {
  imp.stacked[[i]][["fit_2.pred.5Y"]] <- fit_2.pred.5Y.fun(age.std = imp.stacked[[i]][["age.std"]],
                                                           sex.male_1 = imp.stacked[[i]][["sex.male_1"]],
                                                           SESWOA_2014.std = imp.stacked[[i]][["SESWOA_2014.std"]],
                                                           GTS.diabetes_all_types_1 = imp.stacked[[i]][["GTS.diabetes_all_types_1"]],
                                                           pre_existing_ASCVD.yes_1 = imp.stacked[[i]][["pre_existing_ASCVD.yes_1"]],
                                                           smoking.current_1 = imp.stacked[[i]][["smoking.current_1"]],
                                                           smoking.ex_1 = imp.stacked[[i]][["smoking.ex_1"]],
                                                           MVPA.std = imp.stacked[[i]][["MVPA.std"]],
                                                           DHD.std = imp.stacked[[i]][["DHD.std"]])
}

fit_2.CAL.5Y.x <- list()
fit_2.CAL.5Y.y <- list()
for(i in 1:n_imp) {
  imp.stacked[[i]][["fit_2.pred.5Y.decile"]] <- cut(imp.stacked[[i]][["fit_2.pred.5Y"]],
                                                    breaks = quantile(imp.stacked[[i]][["fit_2.pred.5Y"]], prob = seq(0, 1, 0.1)),
                                                    labels = c(1:10),
                                                    include.lowest = TRUE)
  temp.fit_2.CAL.5Y.x <- aggregate(imp.stacked[[i]][["fit_2.pred.5Y"]],
                                   list(imp.stacked[[i]][["fit_2.pred.5Y.decile"]]),
                                   mean)
  colnames(temp.fit_2.CAL.5Y.x) <- c("fit_2.pred.5Y.decile",
                                     "fit_2.pred.5Y.mean")
  fit_2.CAL.5Y.x <- append(fit_2.CAL.5Y.x, list(temp.fit_2.CAL.5Y.x))
  
  temp.fit_2.CAL.5Y.y <- as.data.frame(matrix(nrow = 10,
                                              ncol = 2))
  colnames(temp.fit_2.CAL.5Y.y) <- c("fit_2.pred.5Y.decile",
                                     "CI.ASCVD_composite_event.5Y")
  temp.fit_2.CAL.5Y.y$fit_2.pred.5Y.decile <- 1:10
  for(d in 1:10) {
    temp.CI <- cuminc(ftime = subset(imp.stacked[[i]], fit_2.pred.5Y.decile == d)$time,
                      fstatus = subset(imp.stacked[[i]], fit_2.pred.5Y.decile == d)$event,
                      cencode = 0)
    
    temp.fit_2.CAL.5Y.y[d, "CI.ASCVD_composite_event.5Y"] <- timepoints(temp.CI, 5)[[1]][[1]]
  }
  fit_2.CAL.5Y.y <- append(fit_2.CAL.5Y.y, list(temp.fit_2.CAL.5Y.y))
}

fit_2.CAL.5Y <- as.data.frame(matrix(nrow = 10,
                                     ncol = 3))
colnames(fit_2.CAL.5Y) <- c("fit_2.pred.5Y.decile",
                            "fit_2.pred.5Y.mean.mean",
                            "ASCVD_composite_event.mean.mean")
fit_2.CAL.5Y$fit_2.pred.5Y.decile <- 1:10
temp <- vector()
for(d in 1:10) {
  for(i in 1:n_imp){
    temp <- append(temp, fit_2.CAL.5Y.x[[i]][[2]][[d]])
  }
  fit_2.CAL.5Y[d, "fit_2.pred.5Y.mean.mean"] <- mean(temp)
}
temp <- vector()
for(d in 1:10) {
  for(i in 1:n_imp) {
    temp <- append(temp, fit_2.CAL.5Y.y[[i]][[2]][[d]])
  }
  fit_2.CAL.5Y[d, "ASCVD_composite_event.mean.mean"] <- mean(temp)
}

fit_2.CAL.5Y

for(i in 1:n_imp) {
  imp.stacked[[i]][["fit_2.pred.10Y"]] <- fit_2.pred.10Y.fun(age.std = imp.stacked[[i]][["age.std"]],
                                                             sex.male_1 = imp.stacked[[i]][["sex.male_1"]],
                                                             SESWOA_2014.std = imp.stacked[[i]][["SESWOA_2014.std"]],
                                                             GTS.diabetes_all_types_1 = imp.stacked[[i]][["GTS.diabetes_all_types_1"]],
                                                             pre_existing_ASCVD.yes_1 = imp.stacked[[i]][["pre_existing_ASCVD.yes_1"]],
                                                             smoking.current_1 = imp.stacked[[i]][["smoking.current_1"]],
                                                             smoking.ex_1 = imp.stacked[[i]][["smoking.ex_1"]],
                                                             MVPA.std = imp.stacked[[i]][["MVPA.std"]],
                                                             DHD.std = imp.stacked[[i]][["DHD.std"]])
}

fit_2.CAL.10Y.x <- list()
fit_2.CAL.10Y.y <- list()
for(i in 1:n_imp) {
  imp.stacked[[i]][["fit_2.pred.10Y.decile"]] <- cut(imp.stacked[[i]][["fit_2.pred.10Y"]],
                                                     breaks = quantile(imp.stacked[[i]][["fit_2.pred.10Y"]], prob = seq(0, 1, 0.1)),
                                                     labels = c(1:10),
                                                     include.lowest = TRUE)
  temp.fit_2.CAL.10Y.x <- aggregate(imp.stacked[[i]][["fit_2.pred.10Y"]],
                                    list(imp.stacked[[i]][["fit_2.pred.10Y.decile"]]),
                                    mean)
  colnames(temp.fit_2.CAL.10Y.x) <- c("fit_2.pred.10Y.decile",
                                      "fit_2.pred.10Y.mean")
  fit_2.CAL.10Y.x <- append(fit_2.CAL.10Y.x, list(temp.fit_2.CAL.10Y.x))
  
  temp.fit_2.CAL.10Y.y <- as.data.frame(matrix(nrow = 10,
                                               ncol = 2))
  colnames(temp.fit_2.CAL.10Y.y) <- c("fit_2.pred.10Y.decile",
                                      "CI.ASCVD_composite_event.10Y")
  temp.fit_2.CAL.10Y.y$fit_2.pred.10Y.decile <- 1:10
  for(d in 1:10) {
    temp.CI <- cuminc(ftime = subset(imp.stacked[[i]], fit_2.pred.10Y.decile == d)$time,
                      fstatus = subset(imp.stacked[[i]], fit_2.pred.10Y.decile == d)$event,
                      cencode = 0)
    
    temp.fit_2.CAL.10Y.y[d, "CI.ASCVD_composite_event.10Y"] <- timepoints(temp.CI, 10)[[1]][[1]]
  }
  fit_2.CAL.10Y.y <- append(fit_2.CAL.10Y.y, list(temp.fit_2.CAL.10Y.y))
}

fit_2.CAL.10Y <- as.data.frame(matrix(nrow = 10,
                                      ncol = 3))
colnames(fit_2.CAL.10Y) <- c("fit_2.pred.10Y.decile",
                             "fit_2.pred.10Y.mean.mean",
                             "CI.ASCVD_composite_event.10Y.mean")
fit_2.CAL.10Y$fit_2.pred.10Y.decile <- 1:10
temp <- vector()
for(d in 1:10) {
  for(i in 1:n_imp) {
    temp <- append(temp, fit_2.CAL.10Y.x[[i]][[2]][[d]])
  }
  fit_2.CAL.10Y[d, "fit_2.pred.10Y.mean.mean"] <- mean(temp)
}
temp <- vector()
for(d in 1:10) {
  for(i in 1:n_imp) {
    temp <- append(temp, fit_2.CAL.10Y.y[[i]][[2]][[d]])
  }
  fit_2.CAL.10Y[d, "CI.ASCVD_composite_event.10Y.mean"] <- mean(temp)
}

fit_2.CAL.10Y

## Bootstrap ----

n_b <- 100

# Object to capture output model coefficients 

boot.fit_2.coef <- as.data.frame(matrix(nrow = 9,
                                        ncol = 1 + n_b))
colnames(boot.fit_2.coef) <- c("x",
                               paste0("boot_", 1:n_b, ".coef"))
boot.fit_2.coef[1, "x"] <- "age.std"
boot.fit_2.coef[2, "x"] <- "sex.male_1"
boot.fit_2.coef[3, "x"] <- "SESWOA_2014.std"
boot.fit_2.coef[4, "x"] <- "GTS.diabetes_all_types_1"
boot.fit_2.coef[5, "x"] <- "pre_existing_ASCVD.yes_1"
boot.fit_2.coef[6, "x"] <- "smoking.current_1"
boot.fit_2.coef[7, "x"] <- "smoking.ex_1"
boot.fit_2.coef[8, "x"] <- "MVPA.std"
boot.fit_2.coef[9, "x"] <- "DHD.std"

# Objects to capture output AUC

boot.fit_2.AUC.5Y <- as.data.frame(matrix(nrow = 1,
                                          ncol = n_b))
colnames(boot.fit_2.AUC.5Y) <- c(paste0("boot_", 1:n_b, ".AUC.5Y"))

boot.fit_2.AUC.10Y <- as.data.frame(matrix(nrow = 1,
                                           ncol = n_b))
colnames(boot.fit_2.AUC.10Y) <- c(paste0("boot_", 1:n_b, ".AUC.10Y"))

# Objects to capture output calibration

boot.fit_2.CAL.5Y <- as.data.frame(matrix(nrow = 0,
                                          ncol = 3))
colnames(boot.fit_2.CAL.5Y) <- c("boot",
                                 "boot.fit_2.pred.5Y.decile",
                                 "boot.fit_2.pred.5Y.mean.mean")

# Loop

for(b in 1:n_b) {
  boot.index <- sample(1:nrow(data), size = nrow(data), replace = TRUE)
  
  boot.data <- data[boot.index, ] 
  
  # Imputations
  
  boot.imp <- mice(boot.data,
                   method = "pmm", 
                   m = n_imp, 
                   print = FALSE)
  
  boot.imp.stacked <- list()
  for(i in 1:n_imp) {
    boot.imp.stacked[[i]] <- complete(boot.imp, 
                                      i)
    
    boot.imp.stacked[[i]][["smoking.current_1"]] <- ifelse(boot.imp.stacked[[i]][["smoking.3_cat"]] == 3,
                                                           1,
                                                           0)
    boot.imp.stacked[[i]][["smoking.ex_1"]] <- ifelse(boot.imp.stacked[[i]][["smoking.3_cat"]] == 2,
                                                      1,
                                                      0)
  }
  
  # Model
  
  boot.fit_2.stacked <- list()
  for(i in 1:n_imp) {
    boot.fit_2.stacked[[i]] <- FGR(Hist(time, event) ~
                                     age.std +
                                     sex.male_1 +
                                     SESWOA_2014.std +
                                     GTS.diabetes_all_types_1 +
                                     pre_existing_ASCVD.yes_1 +
                                     smoking.current_1 +
                                     smoking.ex_1 +
                                     MVPA.std +
                                     DHD.std, 
                                   cause = 1,
                                   data = boot.imp.stacked[[i]])
  }
  
  boot.fit_2.baseline.5Y <- vector()
  for(i in 1:n_imp) {
    boot.fit_2.baseline.5Y <- append(boot.fit_2.baseline.5Y, predictRisk(boot.fit_2.stacked[[i]], new_data, times = 5))
  }
  boot.fit_2.baseline.5Y <- mean(boot.fit_2.baseline.5Y)
  boot.fit_2.baseline.5Y
  
  temp.boot.fit_2.coef <- as.data.frame(matrix(nrow = 9,
                                               ncol = (1 + n_imp)))
  colnames(temp.boot.fit_2.coef) <- c("x",
                                      paste0("imp_", 1:n_imp))
  temp.boot.fit_2.coef[1, "x"] <- "age.std"
  temp.boot.fit_2.coef[2, "x"] <- "sex.male_1"
  temp.boot.fit_2.coef[3, "x"] <- "SESWOA_2014.std"
  temp.boot.fit_2.coef[4, "x"] <- "GTS.diabetes_all_types_1"
  temp.boot.fit_2.coef[5, "x"] <- "pre_existing_ASCVD.yes_1"
  temp.boot.fit_2.coef[6, "x"] <- "smoking.current_1"
  temp.boot.fit_2.coef[7, "x"] <- "smoking.ex_1"
  temp.boot.fit_2.coef[8, "x"] <- "MVPA.std"
  temp.boot.fit_2.coef[9, "x"] <- "DHD.std"
  
  for(i in 1:n_imp) {
    for(j in 1:9) {
      temp.boot.fit_2.coef[j, 1+i] <- boot.fit_2.stacked[[i]][["crrFit"]][["coef"]][[j]] 
    }
  }
  
  temp.boot.fit_2.coef$coef <- apply(temp.boot.fit_2.coef[, 2:(1 + n_imp)],
                                     1,
                                     mean)
  
  boot.fit_2.coef[, 1 + b] <- temp.boot.fit_2.coef$coef 
  
  boot.fit_2.pred.5Y.fun <- function(age.std,
                                     sex.male_1,
                                     SESWOA_2014.std,
                                     GTS.diabetes_all_types_1,
                                     pre_existing_ASCVD.yes_1,
                                     smoking.current_1,
                                     smoking.ex_1,
                                     MVPA.std,
                                     DHD.std) {
    (1 - ((1 - boot.fit_2.baseline.5Y) ** exp(subset(boot.fit_2.coef, x == "age.std")[, 1 + b] * age.std +
                                              subset(boot.fit_2.coef, x == "sex.male_1")[, 1 + b] * sex.male_1 +
                                              subset(boot.fit_2.coef, x == "SESWOA_2014.std")[, 1 + b] * SESWOA_2014.std +
                                              subset(boot.fit_2.coef, x == "GTS.diabetes_all_types_1")[, 1 + b] * GTS.diabetes_all_types_1 +
                                              subset(boot.fit_2.coef, x == "pre_existing_ASCVD.yes_1")[, 1 + b] * pre_existing_ASCVD.yes_1 +
                                              subset(boot.fit_2.coef, x == "smoking.current_1")[, 1 + b] * smoking.current_1 +
                                              subset(boot.fit_2.coef, x == "smoking.ex_1")[, 1 + b] * smoking.ex_1 +
                                              subset(boot.fit_2.coef, x == "MVPA.std")[, 1 + b] * MVPA.std +
                                              subset(boot.fit_2.coef, x == "DHD.std")[, 1 + b] * DHD.std          
    )))
  }
  
  # AUC
  
  temp.boot.fit_2.AUC.5Y <- as.data.frame(matrix(nrow = 1,
                                                 ncol = n_imp))
  colnames(temp.boot.fit_2.AUC.5Y) <- c(paste0("imp_", 1:n_imp))
  
  for(i in 1:n_imp) {
    temp.boot.fit_2.AUC.5Y[1, i] <- Score(list(boot.fit_2.stacked[[i]]), 
                                          formula = Hist(time, event) ~ 1,
                                          data = boot.imp.stacked[[i]],
                                          metrics = "auc",
                                          times = 5,
                                          se.fit = FALSE)[["AUC"]][["score"]][["AUC"]]
  }
  
  boot.fit_2.AUC.5Y[ , b] <- apply(temp.boot.fit_2.AUC.5Y,
                                   1,
                                   mean)
  
  temp.boot.fit_2.AUC.10Y <- as.data.frame(matrix(nrow = 1,
                                                  ncol = n_imp))
  colnames(temp.boot.fit_2.AUC.10Y) <- c(paste0("imp_", 1:n_imp))
  
  for(i in 1:n_imp) {
    temp.boot.fit_2.AUC.10Y[1, i] <- Score(list(boot.fit_2.stacked[[i]]), 
                                           formula = Hist(time, event) ~ 1,
                                           data = boot.imp.stacked[[i]],
                                           metrics = "auc",
                                           times = 10,
                                           se.fit = FALSE)[["AUC"]][["score"]][["AUC"]]
  }
  
  boot.fit_2.AUC.10Y[ , b] <- apply(temp.boot.fit_2.AUC.10Y,
                                    1,
                                    mean)

  # Calibration
  
  for(i in 1:n_imp) {
    boot.imp.stacked[[i]][["boot.fit_2.pred.5Y"]] <- boot.fit_2.pred.5Y.fun(age.std = boot.imp.stacked[[i]][["age.std"]],
                                                                            sex.male_1 = boot.imp.stacked[[i]][["sex.male_1"]],
                                                                            SESWOA_2014.std = boot.imp.stacked[[i]][["SESWOA_2014.std"]],
                                                                            GTS.diabetes_all_types_1 = boot.imp.stacked[[i]][["GTS.diabetes_all_types_1"]],
                                                                            pre_existing_ASCVD.yes_1 = boot.imp.stacked[[i]][["pre_existing_ASCVD.yes_1"]],
                                                                            smoking.current_1 = boot.imp.stacked[[i]][["smoking.current_1"]],
                                                                            smoking.ex_1 = boot.imp.stacked[[i]][["smoking.ex_1"]],
                                                                            MVPA.std = boot.imp.stacked[[i]][["MVPA.std"]],
                                                                            DHD.std = boot.imp.stacked[[i]][["DHD.std"]])
  }
  
  boot.fit_2.CAL.5Y.x <- list()
  for(i in 1:n_imp) {
    boot.imp.stacked[[i]][["boot.fit_2.pred.5Y.decile"]] <- cut(boot.imp.stacked[[i]][["boot.fit_2.pred.5Y"]],
                                                                breaks = quantile(boot.imp.stacked[[i]][["boot.fit_2.pred.5Y"]], prob = seq(0, 1, 0.1)),
                                                                labels = c(1:10),
                                                                include.lowest = TRUE)
    boot.temp.fit_2.CAL.5Y.x <- aggregate(boot.imp.stacked[[i]][["boot.fit_2.pred.5Y"]],
                                          list(boot.imp.stacked[[i]][["boot.fit_2.pred.5Y.decile"]]),
                                          mean)
    colnames(boot.temp.fit_2.CAL.5Y.x) <- c("boot.fit_2.pred.5Y.decile",
                                            "boot.fit_2.pred.5Y.mean")
    boot.fit_2.CAL.5Y.x <- append(boot.fit_2.CAL.5Y.x, list(boot.temp.fit_2.CAL.5Y.x))
    }
  
  temp.boot.fit_2.CAL.5Y <- as.data.frame(matrix(nrow = 10,
                                                 ncol = 2))
  colnames(temp.boot.fit_2.CAL.5Y) <- c("boot.fit_2.pred.5Y.decile",
                                        "boot.fit_2.pred.5Y.mean.mean")
  temp.boot.fit_2.CAL.5Y$boot.fit_2.pred.5Y.decile <- 1:10
  boot.temp <- vector()
  for(d in 1:10) {
    for(i in 1:n_imp) {
      boot.temp <- append(boot.temp, boot.fit_2.CAL.5Y.x[[i]][[2]][[d]])
    }
    temp.boot.fit_2.CAL.5Y[d, "boot.fit_2.pred.5Y.mean.mean"] <- mean(boot.temp)
  }
  
  temp.boot.fit_2.CAL.5Y$boot <- b
  temp.boot.fit_2.CAL.5Y <- temp.boot.fit_2.CAL.5Y[, c("boot",
                                                       "boot.fit_2.pred.5Y.decile",
                                                       "boot.fit_2.pred.5Y.mean.mean")]
  boot.fit_2.CAL.5Y <- rbind(boot.fit_2.CAL.5Y, temp.boot.fit_2.CAL.5Y)
}

# 95% CI fit_2 coefficients

quantile(t(subset(boot.fit_2.coef, x =="age.std")[, 2:ncol(boot.fit_2.coef)]), c(0.025, 0.975))
quantile(t(subset(boot.fit_2.coef, x =="sex.male_1")[, 2:ncol(boot.fit_2.coef)]), c(0.025, 0.975))
quantile(t(subset(boot.fit_2.coef, x =="SESWOA_2014.std")[, 2:ncol(boot.fit_2.coef)]), c(0.025, 0.975))
quantile(t(subset(boot.fit_2.coef, x =="GTS.diabetes_all_types_1")[, 2:ncol(boot.fit_2.coef)]), c(0.025, 0.975))
quantile(t(subset(boot.fit_2.coef, x =="pre_existing_ASCVD.yes_1")[, 2:ncol(boot.fit_2.coef)]), c(0.025, 0.975))
quantile(t(subset(boot.fit_2.coef, x =="smoking.current_1")[, 2:ncol(boot.fit_2.coef)]), c(0.025, 0.975))
quantile(t(subset(boot.fit_2.coef, x =="smoking.ex_1")[, 2:ncol(boot.fit_2.coef)]), c(0.025, 0.975))
quantile(t(subset(boot.fit_2.coef, x =="MVPA.std")[, 2:ncol(boot.fit_2.coef)]), c(0.025, 0.975))
quantile(t(subset(boot.fit_2.coef, x =="DHD.std")[, 2:ncol(boot.fit_2.coef)]), c(0.025, 0.975))

exp(quantile(t(subset(boot.fit_2.coef, x =="age.std")[, 2:ncol(boot.fit_2.coef)]), c(0.025, 0.975)))
exp(quantile(t(subset(boot.fit_2.coef, x =="sex.male_1")[, 2:ncol(boot.fit_2.coef)]), c(0.025, 0.975)))
exp(quantile(t(subset(boot.fit_2.coef, x =="SESWOA_2014.std")[, 2:ncol(boot.fit_2.coef)]), c(0.025, 0.975)))
exp(quantile(t(subset(boot.fit_2.coef, x =="GTS.diabetes_all_types_1")[, 2:ncol(boot.fit_2.coef)]), c(0.025, 0.975)))
exp(quantile(t(subset(boot.fit_2.coef, x =="pre_existing_ASCVD.yes_1")[, 2:ncol(boot.fit_2.coef)]), c(0.025, 0.975)))
exp(quantile(t(subset(boot.fit_2.coef, x =="smoking.current_1")[, 2:ncol(boot.fit_2.coef)]), c(0.025, 0.975)))
exp(quantile(t(subset(boot.fit_2.coef, x =="smoking.ex_1")[, 2:ncol(boot.fit_2.coef)]), c(0.025, 0.975)))
exp(quantile(t(subset(boot.fit_2.coef, x =="MVPA.std")[, 2:ncol(boot.fit_2.coef)]), c(0.025, 0.975)))
exp(quantile(t(subset(boot.fit_2.coef, x =="DHD.std")[, 2:ncol(boot.fit_2.coef)]), c(0.025, 0.975)))

# 95% CI AUC

quantile(t(boot.fit_2.AUC.5Y), c(0.025, 0.975))

quantile(t(boot.fit_2.AUC.10Y), c(0.025, 0.975))

# 95% CI calibration

for(d in 1:10) {
  print(d)
  print(quantile(subset(boot.fit_2.CAL.5Y, boot.fit_2.pred.5Y.decile == d)$boot.fit_2.pred.5Y.mean.mean, c(0.025, 0.975)))
}

# Session info ----

sessionInfo()
