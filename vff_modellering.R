
# prerequisites ----------------------------------------------------------
# load packages

pacman::p_load(tidyverse, rvest, janitor, RSQLite, slider, leaps, glmnet)

# indlæser data fra rds fil

vff_all <- read_rds("data/vff_all.rds")

# for at sørge for der ikke opstår problemer med factor levels, som findes i testsættet
# men ikke i træningssættet, laves helligdagsvariablen om til om der blot var en hvilken som helst
# helligdag på dagen, eller ej. derefter grupperes de levels i modstander variablen, der kun optræder
# en enkelt gang, sådan at de får et fælles niveau

vff_all <- vff_all |> 
  mutate(
    er_helligdag = as.factor(if_else(helligdag == "ingen", 0, 1)),
    modstander = fct_lump_min(modstander, min = 2, other_level = "andre_hold")
  ) |> 
  select(-helligdag)

# sætter et seed

set.seed(2)

# opdeler dataen i træningssæt og testsæt

train_size <- floor(0.7 * nrow(vff_all))

train_data <- sample(nrow(vff_all), size = train_size)

test_data <- -train_data

vff_train <- vff_all[train_data, ]

vff_test <- vff_all[test_data, ]

# opdeler datasættene, sådan at vi får de datasæt vi skal bruge 
# for at forudsige 1 måned før, 10 dage før, 7 dage før og 3 dage før

vff_train_1m <- vff_train |> 
  select(-d10_tilskuere, -d7_tilskuere, -d3_tilskuere)

vff_train_d10 <- vff_train |> 
  select(-d7_tilskuere, -d3_tilskuere)

vff_train_d7 <- vff_train |> 
  select(-d10_tilskuere, -d3_tilskuere)

vff_train_d3 <- vff_train |> 
  select(-d10_tilskuere, -d7_tilskuere)

vff_test_1m <- vff_test |> 
  select(-d10_tilskuere, -d7_tilskuere, -d3_tilskuere)

vff_test_d10 <- vff_test |> 
  select(-d7_tilskuere, -d3_tilskuere)

vff_test_d7 <- vff_test |> 
  select(-d10_tilskuere, -d3_tilskuere)

vff_test_d3 <- vff_test |> 
  select(-d10_tilskuere, -d7_tilskuere)

# opretter en funktion til at lave de predictede værdier

predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

# fuld linear model ------------------------------------------------------

lm_full_1m <- lm(tilskuere ~ ., vff_train_1m)

# finder predictede værdier

pred_full_1m <- predict(lm_full_1m, 
                        vff_test_1m)

# test MSE

testmse_full_1m <- mean((vff_test_1m$tilskuere - pred_full_1m)^2)

## 10 dage inden kampstart ------------------------------------------------

lm_full_d10 <- lm(tilskuere ~ ., vff_train_d10)

# finder de predictede værdier

pred_full_d10 <- predict(lm_full_d10, 
                         vff_test_d10)

# test MSE

testmse_full_d10 <- mean((vff_test_d10$tilskuere - pred_full_d10)^2)

## 7 dage inden kampstart ------------------------------------------------

lm_full_d7 <- lm(tilskuere ~ ., vff_train_d7)

# finder de predictede værdier

pred_full_d7 <- predict(lm_full_d7, 
                        vff_test_d7)

# test MSE

testmse_full_d7 <- mean((vff_test_d7$tilskuere - pred_full_d7)^2)

## 3 dage inden kampstart ------------------------------------------------

lm_full_d3 <- lm(tilskuere ~ ., vff_train_d3)

# finder de predictede værdier

predict_full_d3 <- predict(lm_full_d3, 
                           vff_test_d3)

# test MSE

testmse_full_d3 <- mean((vff_test_d3$tilskuere - predict_full_d3)^2)

# forward selection ------------------------------------------------------

k <- 10 # Vi danner 10 folds
n <- nrow(vff_train) # registrerer hvor mange observationer, vi har.

folds <- sample(rep(1:k, length = n)) #Vi tildeler en værdi mellem 1 og

## model for 1 måned før kampstart --------------------------------------------------

# laver en model matrix, for at finde nvmax, altså den maksimale variabelstørrele på kandidatmodellerne

model_matrix <- model.matrix(tilskuere ~ ., data = vff_train_1m)[, -1]

nvmax <- ncol(model_matrix)

cv.errors <- matrix(NA, k, nvmax,
                    dimnames = list(NULL, paste(1:nvmax)))

for (j in 1:k) {
  best.fit <- regsubsets(tilskuere ~ .,
                         data = vff_train_1m[folds != j, ],
                         nvmax = nvmax,
                         method = "forward")
  
    n_models <- length(summary(best.fit)$which[,1]) # pga. multikollinnearitet laves der ikke det samme
  # antal modeller i hver fold, så vi finder det reelle antal der laves i hver fold og bruger det i næste del
  # af loopet
  
  for (i in 1:n_models) {
    pred <- predict(best.fit, vff_train_1m[folds == j, ], id = i)
    cv.errors[j, i] <- mean((vff_train$tilskuere[folds == j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean) # apply er en smart funktion, der 
# gennemløber alle rækker og tager gennemsnittet henover hver søjle, som svarer 
# til hver kandidatmodel.
mean.cv.errors # vi får færre end 51 kandidatmodeller pga. kollinearitet

best_nvars_1m_fwd <- which.min(mean.cv.errors)

best_fwd_1m <- regsubsets(tilskuere ~ .,
                         data = vff_train_1m,
                         nvmax = nvmax,
                         method = "forward")

pred_fwd_1m <- predict(best_fwd_1m, vff_test_1m, id = best_nvars_1m_fwd)

testmse_fwd_1m <- mean((vff_test_1m$tilskuere - pred_fwd_1m)^2)

## model for 10 dage før kampstart --------------------------------------------------

# laver en model matrix, for at finde nvmax, altså den maksimale variabelstørrele på kandidatmodellerne

model_matrix <- model.matrix(tilskuere ~ ., data = vff_train_d10)[ , -1]

nvmax <- ncol(model_matrix)

cv.errors <- matrix(NA, k, nvmax,
                    dimnames = list(NULL, paste(1:nvmax)))

for (j in 1:k) {
  best.fit <- regsubsets(tilskuere ~ .,
                         data = vff_train_d10[folds != j, ],
                         nvmax = nvmax,
                         method = "forward")
  
  n_models <- length(summary(best.fit)$which[,1]) # pga. multikollinnearitet laves der ikke det samme
  # antal modeller i hver fold, så vi finder det reelle antal der laves i hver fold og bruger det i næste del
  # af loopet
  
  for (i in 1:n_models) {
    pred <- predict(best.fit, vff_train_d10[folds == j, ], id = i)
    cv.errors[j, i] <- mean((vff_train$tilskuere[folds == j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean) # apply er en smart funktion, der 
# gennemløber alle rækker og tager gennemsnittet henover hver søjle, som svarer 
# til hver kandidatmodel.
mean.cv.errors # vi får færre end 51 kandidatmodeller pga. kollinearitet

best_nvars_d10_fwd <- which.min(mean.cv.errors)

best_fwd_d10 <- regsubsets(tilskuere ~ .,
                         data = vff_train_d10,
                         nvmax = nvmax,
                         method = "forward")

pred_fwd_d10 <- predict(best_fwd_d10, vff_test_d10, id = best_nvars_d10_fwd)

testmse_fwd_d10 <- mean((vff_test_d10$tilskuere - pred_fwd_d10)^2)

## model for 7 dage før kampstart --------------------------------------------------

# laver en model matrix, for at finde nvmax, altså den maksimale variabelstørrele på kandidatmodellerne

model_matrix <- model.matrix(tilskuere ~ ., data = vff_train_d7)[ , -1]

nvmax <- ncol(model_matrix)

cv.errors <- matrix(NA, k, nvmax,
                    dimnames = list(NULL, paste(1:nvmax)))

for (j in 1:k) {
  best.fit <- regsubsets(tilskuere ~ .,
                         data = vff_train_d7[folds != j, ],
                         nvmax = nvmax,
                         method = "forward")
  
  n_models <- length(summary(best.fit)$which[,1]) # pga. multikollinnearitet laves der ikke det samme
  # antal modeller i hver fold, så vi finder det reelle antal der laves i hver fold og bruger det i næste del
  # af loopet
  
  for (i in 1:n_models) {
    pred <- predict(best.fit, vff_train_d7[folds == j, ], id = i)
    cv.errors[j, i] <- mean((vff_train$tilskuere[folds == j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean) # apply er en smart funktion, der 
# gennemløber alle rækker og tager gennemsnittet henover hver søjle, som svarer 
# til hver kandidatmodel.
mean.cv.errors # vi får færre end 51 kandidatmodeller pga. kollinearitet

best_nvars_d7_fwd <- which.min(mean.cv.errors)

best_fwd_d7 <- regsubsets(tilskuere ~ .,
                         data = vff_train_d7,
                         nvmax = nvmax,
                         method = "forward")

pred_fwd_d7 <- predict(best_fwd_d7, vff_test_d7, id = best_nvars_d7_fwd)

testmse_fwd_d7 <- mean((vff_test_d7$tilskuere - pred_fwd_d7)^2)

## model for 3 dage før kampstart --------------------------------------------------

# laver en model matrix, for at finde nvmax, altså den maksimale variabelstørrele på kandidatmodellerne

model_matrix <- model.matrix(tilskuere ~ ., data = vff_train_d3)[ , -1]

nvmax <- ncol(model_matrix)

cv.errors <- matrix(NA, k, nvmax,
                    dimnames = list(NULL, paste(1:nvmax)))

for (j in 1:k) {
  best.fit <- regsubsets(tilskuere ~ .,
                         data = vff_train_d3[folds != j, ],
                         nvmax = nvmax,
                         method = "forward")
  
  n_models <- length(summary(best.fit)$which[,1]) # pga. multikollinnearitet laves der ikke det samme
  # antal modeller i hver fold, så vi finder det reelle antal der laves i hver fold og bruger det i næste del
  # af loopet
  
  for (i in 1:n_models) {
    pred <- predict(best.fit, vff_train_d3[folds == j, ], id = i)
    cv.errors[j, i] <- mean((vff_train$tilskuere[folds == j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean) # apply er en smart funktion, der 
# gennemløber alle rækker og tager gennemsnittet henover hver søjle, som svarer 
# til hver kandidatmodel.
mean.cv.errors # vi får færre end 51 kandidatmodeller pga. kollinearitet

best_nvars_d3_fwd <- which.min(mean.cv.errors)

best_fwd_d3 <- regsubsets(tilskuere ~ .,
                         data = vff_train_d3,
                         nvmax = nvmax,
                         method = "forward")

pred_fwd_d3 <- predict(best_fwd_d3, vff_test_d3, id = best_nvars_d3_fwd)

testmse_fwd_d3 <- mean((vff_test_d3$tilskuere - pred_fwd_d3)^2)

# backward selection ------------------------------------------------------

k <- 10 # Vi danner 10 folds
n <- nrow(vff_train) # registrerer hvor mange observationer, vi har.

folds <- sample(rep(1:k, length = n)) #Vi tildeler en værdi mellem 1 og

## model for 1 måned før kampstart --------------------------------------------------

# laver en model matrix, for at finde nvmax, altså den maksimale variabelstørrele på kandidatmodellerne

model_matrix <- model.matrix(tilskuere ~ ., data = vff_train_1m)[, -1]

nvmax <- ncol(model_matrix)

cv.errors <- matrix(NA, k, nvmax,
                    dimnames = list(NULL, paste(1:nvmax)))

for (j in 1:k) {
  best.fit <- regsubsets(tilskuere ~ .,
                         data = vff_train_1m[folds != j, ],
                         nvmax = nvmax,
                         method = "backward")
  
    n_models <- length(summary(best.fit)$which[,1]) # pga. multikollinnearitet laves der ikke det samme
  # antal modeller i hver fold, så vi finder det reelle antal der laves i hver fold og bruger det i næste del
  # af loopet
  
  for (i in 1:n_models) {
    pred <- predict(best.fit, vff_train_1m[folds == j, ], id = i)
    cv.errors[j, i] <- mean((vff_train$tilskuere[folds == j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean) # apply er en smart funktion, der 
# gennemløber alle rækker og tager gennemsnittet henover hver søjle, som svarer 
# til hver kandidatmodel.
mean.cv.errors # vi får færre end 51 kandidatmodeller pga. kollinearitet

best_nvars_1m_bwd <- which.min(mean.cv.errors)

best_bwd_1m <- regsubsets(tilskuere ~ .,
                         data = vff_train_1m,
                         nvmax = nvmax,
                         method = "backward")

coef(best_bwd_1m, best_nvars_1m_bwd)

pred_bwd_1m <- predict(best_bwd_1m, vff_test_1m, id = best_nvars_1m_bwd)

testmse_bwd_1m <- mean((vff_test_1m$tilskuere - pred_bwd_1m)^2)

## model for 10 dage før kampstart --------------------------------------------------

# laver en model matrix, for at finde nvmax, altså den maksimale variabelstørrele på kandidatmodellerne

model_matrix <- model.matrix(tilskuere ~ ., data = vff_train_d10)[ , -1]

nvmax <- ncol(model_matrix)

cv.errors <- matrix(NA, k, nvmax,
                    dimnames = list(NULL, paste(1:nvmax)))

for (j in 1:k) {
  best.fit <- regsubsets(tilskuere ~ .,
                         data = vff_train_d10[folds != j, ],
                         nvmax = nvmax,
                         method = "backward")
  
  n_models <- length(summary(best.fit)$which[,1]) # pga. multikollinnearitet laves der ikke det samme
  # antal modeller i hver fold, så vi finder det reelle antal der laves i hver fold og bruger det i næste del
  # af loopet
  
  for (i in 1:n_models) {
    pred <- predict(best.fit, vff_train_d10[folds == j, ], id = i)
    cv.errors[j, i] <- mean((vff_train$tilskuere[folds == j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean) # apply er en smart funktion, der 
# gennemløber alle rækker og tager gennemsnittet henover hver søjle, som svarer 
# til hver kandidatmodel.
mean.cv.errors # vi får færre end 51 kandidatmodeller pga. kollinearitet

best_nvars_d10_bwd <- which.min(mean.cv.errors)

best_bwd_d10 <- regsubsets(tilskuere ~ .,
                         data = vff_train_d10,
                         nvmax = nvmax,
                         method = "backward")

pred_bwd_d10 <- predict(best_bwd_d10, vff_test_d10, id = best_nvars_d10_bwd)

testmse_bwd_d10 <- mean((vff_test_d10$tilskuere - pred_bwd_d10)^2)

## model for 7 dage før kampstart --------------------------------------------------

# laver en model matrix, for at finde nvmax, altså den maksimale variabelstørrele på kandidatmodellerne

model_matrix <- model.matrix(tilskuere ~ ., data = vff_train_d7)[ , -1]

nvmax <- ncol(model_matrix)

cv.errors <- matrix(NA, k, nvmax,
                    dimnames = list(NULL, paste(1:nvmax)))

for (j in 1:k) {
  best.fit <- regsubsets(tilskuere ~ .,
                         data = vff_train_d7[folds != j, ],
                         nvmax = nvmax,
                         method = "backward")
  
  n_models <- length(summary(best.fit)$which[,1]) # pga. multikollinnearitet laves der ikke det samme
  # antal modeller i hver fold, så vi finder det reelle antal der laves i hver fold og bruger det i næste del
  # af loopet
  
  for (i in 1:n_models) {
    pred <- predict(best.fit, vff_train_d7[folds == j, ], id = i)
    cv.errors[j, i] <- mean((vff_train$tilskuere[folds == j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean) # apply er en smart funktion, der 
# gennemløber alle rækker og tager gennemsnittet henover hver søjle, som svarer 
# til hver kandidatmodel.
mean.cv.errors # vi får færre end 51 kandidatmodeller pga. kollinearitet

best_nvars_d7_bwd <- which.min(mean.cv.errors)

best_bwd_d7 <- regsubsets(tilskuere ~ .,
                         data = vff_train_d7,
                         nvmax = nvmax,
                         method = "backward")

pred_bwd_d7 <- predict(best_bwd_d7, vff_test_d7, id = best_nvars_d7_bwd)

testmse_bwd_d7 <- mean((vff_test_d7$tilskuere - pred_bwd_d7)^2)

## model for 3 dage før kampstart --------------------------------------------------

# laver en model matrix, for at finde nvmax, altså den maksimale variabelstørrele på kandidatmodellerne

model_matrix <- model.matrix(tilskuere ~ ., data = vff_train_d3)[ , -1]

nvmax <- ncol(model_matrix)

cv.errors <- matrix(NA, k, nvmax,
                    dimnames = list(NULL, paste(1:nvmax)))

for (j in 1:k) {
  best.fit <- regsubsets(tilskuere ~ .,
                         data = vff_train_d3[folds != j, ],
                         nvmax = nvmax,
                         method = "backward")
  
  n_models <- length(summary(best.fit)$which[,1]) # pga. multikollinnearitet laves der ikke det samme
  # antal modeller i hver fold, så vi finder det reelle antal der laves i hver fold og bruger det i næste del
  # af loopet
  
  for (i in 1:n_models) {
    pred <- predict(best.fit, vff_train_d3[folds == j, ], id = i)
    cv.errors[j, i] <- mean((vff_train$tilskuere[folds == j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean) # apply er en smart funktion, der 
# gennemløber alle rækker og tager gennemsnittet henover hver søjle, som svarer 
# til hver kandidatmodel.
mean.cv.errors # vi får færre end 51 kandidatmodeller pga. kollinearitet

best_nvars_d3_bwd <- which.min(mean.cv.errors)

best_bwd_d3 <- regsubsets(tilskuere ~ .,
                         data = vff_train_d3,
                         nvmax = nvmax,
                         method = "backward")

pred_bwd_d3 <- predict(best_bwd_d3, vff_test_d3, id = best_nvars_d3_bwd)

testmse_bwd_d3 <- mean((vff_test_d3$tilskuere - pred_bwd_d3)^2)

# ridge og lasso regression ------------------------------------------------------------

## modeller for 1 måned inden kampen -----------------------------------------

# opretter objekter for x variablerne og y variablen. x variablerne sættes i en matrice,
# mens y blot er outputtet, her antal tilskuere

x_train_1m <- model.matrix(tilskuere ~ ., data = vff_train_1m)[, -1]

y_train_1m <- vff_train_1m$tilskuere

x_test_1m <- model.matrix(tilskuere ~ ., data = vff_test_1m)[, -1]

### ridge regression -------------------------------------------------------

# optimerer tuning parameteren med k-fold cross validation (10 folds), for at finde den bedste lambda

cv_ridge_1m <- cv.glmnet(x_train_1m, y_train_1m, alpha = 0, nfolds = 10)

# bedste lambda

bestlambda_ridge_1m <- cv_ridge_1m$lambda.min

# bedste lambda indenfor 1 standardafvigelse af den optimale lambda (simpleste model)

ridgelambda_1se_1m <- cv_ridge_1m$lambda.1se

# endelige model

final_ridge_1m <- glmnet(x_train_1m, y_train_1m, alpha = 0, lambda = bestlambda_ridge_1m)

# modellens koefficienter

ridge_coefs_1m <- coef(final_ridge_1m)

# predictede værdier

ridge_pred_1m <- predict(final_ridge_1m, s = bestlambda_ridge_1m, newx = x_test_1m)

# test MSE

testmse_ridge_1m <- mean((vff_test_1m$tilskuere - ridge_pred_1m)^2)

### lasso regression -------------------------------------------------------

# optimerer tuning parameteren med k-fold cross validation (10 folds), for at finde den bedste lambda

cv_lasso_1m <- cv.glmnet(x_train_1m, y_train_1m, alpha = 1, nfolds = 10)

# bedste lambda

bestlambda_lasso_1m <- cv_lasso_1m$lambda.min

# bedste lambda indenfor 1 standardafvigelse af den optimale lambda (simpleste model)

lassolambda_1se_1m <- cv_lasso_1m$lambda.1se

# endelige model

final_lasso_1m <- glmnet(x_train_1m, y_train_1m, alpha = 1, lambda = bestlambda_lasso_1m)

# modellens koefficienter

lasso_coefs_1m <- coef(final_lasso_1m)

# predictede værdier

lasso_pred_1m <- predict(final_lasso_1m, s = bestlambda_lasso_1m, newx = x_test_1m)

# test MSE

testmse_lasso_1m <- mean((vff_test_1m$tilskuere - lasso_pred_1m)^2)

## modeller for 10 dage inden kampen -----------------------------------------

# opretter objekter for x variablerne og y variablen. x variablerne sættes i en matrice,
# mens y blot er outputtet, her antal tilskuere

x_train_d10 <- model.matrix(tilskuere ~ ., data = vff_train_d10)[, -1] 

y_train_d10 <- vff_train_d10$tilskuere

x_test_d10 <- model.matrix(tilskuere ~ ., data = vff_test_d10)[, -1]

### ridge regression -------------------------------------------------------

# optimerer tuning parameteren med k-fold cross validation (10 folds), for at finde den bedste lambda

cv_ridge_d10 <- cv.glmnet(x_train_d10, y_train_d10, alpha = 0, nfolds = 10)

# bedste lambda

bestlambda_ridge_d10 <- cv_ridge_d10$lambda.min

# bedste lambda indenfor 1 standardafvigelse af den optimale lambda (simpleste model)

ridgelambda_1se_d10 <- cv_ridge_d10$lambda.1se

# endelige model

final_ridge_d10 <- glmnet(x_train_d10, y_train_d10, alpha = 0, lambda = bestlambda_ridge_d10)

# modellens koefficienter

ridge_coefs_d10 <- coef(final_ridge_d10)

# predictede værdier

ridge_pred_d10 <- predict(final_ridge_d10, s = bestlambda_ridge_d10, newx = x_test_d10)

# test MSE

testmse_ridge_d10 <- mean((vff_test_d10$tilskuere - ridge_pred_d10)^2)

### lasso regression -------------------------------------------------------

# optimerer tuning parameteren med k-fold cross validation (10 folds), for at finde den bedste lambda

cv_lasso_d10 <- cv.glmnet(x_train_d10, y_train_d10, alpha = 1, nfolds = 10)

# bedste lambda

bestlambda_lasso_d10 <- cv_lasso_d10$lambda.min

# bedste lambda indenfor 1 standardafvigelse af den optimale lambda (simpleste model)

lassolambda_1se_d10 <- cv_lasso_d10$lambda.1se

# endelige model

final_lasso_d10 <- glmnet(x_train_d10, y_train_d10, alpha = 1, lambda = bestlambda_lasso_d10)

# modellens koefficienter

lasso_coefs_d10 <- coef(final_lasso_d10)

# predictede værdier

lasso_pred_d10 <- predict(final_lasso_d10, s = bestlambda_lasso_d10, newx = x_test_d10)

# test MSE

testmse_lasso_d10 <- mean((vff_test_d10$tilskuere - lasso_pred_d10)^2)

## modeller for 7 dage inden kampen -----------------------------------------

# opretter objekter for x variablerne og y variablen. x variablerne sættes i en matrice,
# mens y blot er outputtet, her antal tilskuere

x_train_d7 <- model.matrix(tilskuere ~ ., data = vff_train_d7)[, -1] 

y_train_d7 <- vff_train_d7$tilskuere

x_test_d7 <- model.matrix(tilskuere ~ ., data = vff_test_d7)[, -1]

### ridge regression -------------------------------------------------------

# optimerer tuning parameteren med k-fold cross validation (10 folds), for at finde den bedste lambda

cv_ridge_d7 <- cv.glmnet(x_train_d7, y_train_d7, alpha = 0, nfolds = 10)

# bedste lambda

bestlambda_ridge_d7 <- cv_ridge_d7$lambda.min

# bedste lambda indenfor 1 standardafvigelse af den optimale lambda (simpleste model)

ridgelambda_1se_d7 <- cv_ridge_d7$lambda.1se

# endelige model

final_ridge_d7 <- glmnet(x_train_d7, y_train_d7, alpha = 0, lambda = bestlambda_ridge_d7)

# modellens koefficienter

ridge_coefs_d7 <- coef(final_ridge_d7)

# predictede værdier

ridge_pred_d7 <- predict(final_ridge_d7, s = bestlambda_ridge_d7, newx = x_test_d7)

# test MSE

testmse_ridge_d7 <- mean((vff_test_d7$tilskuere - ridge_pred_d7)^2)

### lasso regression -------------------------------------------------------

# optimerer tuning parameteren med k-fold cross validation (10 folds), for at finde den bedste lambda

cv_lasso_d7 <- cv.glmnet(x_train_d7, y_train_d7, alpha = 1, nfolds = 10)

# bedste lambda

bestlambda_lasso_d7 <- cv_lasso_d7$lambda.min

# bedste lambda indenfor 1 standardafvigelse af den optimale lambda (simpleste model)

lassolambda_1se_d7 <- cv_lasso_d7$lambda.1se

# endelige model

final_lasso_d7 <- glmnet(x_train_d7, y_train_d7, alpha = 1, lambda = bestlambda_lasso_d7)

# modellens koefficienter

lasso_coefs_d7 <- coef(final_lasso_d7)

# predictede værdier

lasso_pred_d7 <- predict(final_lasso_d7, s = bestlambda_lasso_d7, newx = x_test_d7)

# test MSE

testmse_lasso_d7 <- mean((vff_test_d7$tilskuere - lasso_pred_d7)^2)

## modeller for 3 dage inden kampen -----------------------------------------

# opretter objekter for x variablerne og y variablen. x variablerne sættes i en matrice,
# mens y blot er outputtet, her antal tilskuere

x_train_d3 <- model.matrix(tilskuere ~ ., data = vff_train_d3)[, -1] 

y_train_d3 <- vff_train_d3$tilskuere

x_test_d3 <- model.matrix(tilskuere ~ ., data = vff_test_d3)[, -1] 

### ridge regression -------------------------------------------------------

# optimerer tuning parameteren med k-fold cross validation (10 folds), for at finde den bedste lambda

cv_ridge_d3 <- cv.glmnet(x_train_d3, y_train_d3, alpha = 0, nfolds = 10)

# bedste lambda

bestlambda_ridge_d3 <- cv_ridge_d3$lambda.min

# bedste lambda indenfor 1 standardafvigelse af den optimale lambda (simpleste model)

ridgelambda_1se_d3 <- cv_ridge_d3$lambda.1se

# endelige model

final_ridge_d3 <- glmnet(x_train_d3, y_train_d3, alpha = 0, lambda = bestlambda_ridge_d3)

# modellens koefficienter

ridge_coefs_d3 <- coef(final_ridge_d3)

# predictede værdier

ridge_pred_d3 <- predict(final_ridge_d3, s = bestlambda_ridge_d3, newx = x_test_d3)

# test MSE

testmse_ridge_d3 <- mean((vff_test_d3$tilskuere - ridge_pred_d3)^2)

### lasso regression -------------------------------------------------------

# optimerer tuning parameteren med k-fold cross validation (10 folds), for at finde den bedste lambda

cv_lasso_d3 <- cv.glmnet(x_train_d3, y_train_d3, alpha = 1, nfolds = 10)

# bedste lambda

bestlambda_lasso_d3 <- cv_lasso_d3$lambda.min

# bedste lambda indenfor 1 standardafvigelse af den optimale lambda (simpleste model)

lassolambda_1se_d3 <- cv_lasso_d3$lambda.1se

# endelige model

final_lasso_d3 <- glmnet(x_train_d3, y_train_d3, alpha = 1, lambda = bestlambda_lasso_d3)

# modellens koefficienter

lasso_coefs_d3 <- coef(final_lasso_d3)

# predictede værdier

lasso_pred_d3 <- predict(final_lasso_d3, s = bestlambda_lasso_d3, newx = x_test_d3)

# test MSE

testmse_lasso_d3 <- mean((vff_test_d3$tilskuere - lasso_pred_d3)^2)

# visualiseringer af MSE og RMSE -----------------------------------------

# laver dataframes med MSE og RMSE for alle modellerne
test_mse_df <- data.frame(
  måned1 = c(testmse_full_1m, testmse_fwd_1m, testmse_bwd_1m, testmse_ridge_1m, testmse_lasso_1m),
  dag10 = c(testmse_full_d10, testmse_fwd_d10, testmse_bwd_d10, testmse_ridge_d10, testmse_lasso_d10),
  dag7 = c(testmse_full_d7, testmse_fwd_d7, testmse_bwd_d7, testmse_ridge_d7, testmse_lasso_d7),
  dag3 = c(testmse_full_d3, testmse_fwd_d3, testmse_bwd_d3, testmse_ridge_d3, testmse_lasso_d3),
  row.names = c("Full Linear", "Forward Selection", "Backward Selection", 
            "Ridge", "Lasso")
)

test_rmse_df <- data.frame(
  måned1 = c(sqrt(testmse_full_1m), sqrt(testmse_fwd_1m), sqrt(testmse_bwd_1m), sqrt(testmse_ridge_1m), sqrt(testmse_lasso_1m)),
  dag10 = c(sqrt(testmse_full_d10), sqrt(testmse_fwd_d10), sqrt(testmse_bwd_d10), sqrt(testmse_ridge_d10), sqrt(testmse_lasso_d10)),
  dag7 = c(sqrt(testmse_full_d7), sqrt(testmse_fwd_d7), sqrt(testmse_bwd_d7), sqrt(testmse_ridge_d7), sqrt(testmse_lasso_d7)),
  dag3 = c(sqrt(testmse_full_d3), sqrt(testmse_fwd_d3), sqrt(testmse_bwd_d3), sqrt(testmse_ridge_d3), sqrt(testmse_lasso_d3)),
  row.names = c("Full Linear", "Forward Selection", "Backward Selection", 
            "Ridge", "Lasso")
)

best_models <- data.frame(
  best_model = apply(test_mse_df, 2, function(x) rownames(test_mse_df)[which.min(x)]),
  MSE = apply(test_mse_df, 2, min)
) |> 
  mutate(RMSE = sqrt(MSE))

best_models

# laver plots over sammenligninger på modellerne

test_rmse_long <- test_rmse_df |> 
  rownames_to_column("Model") |>
  pivot_longer(cols = -Model, names_to = "Timeframe", values_to = "RMSE")

test_rmse_long <- test_rmse_long |> 
  mutate(
    Timeframe = factor(
      Timeframe,
      levels = c("måned1", "dag10", "dag7", "dag3")
    ))

ggplot(test_rmse_long, aes(x = RMSE, y = reorder(Model, -RMSE), fill = Model)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(RMSE, 0)), hjust = -0.2, size = 3.5) +
  facet_wrap(~Timeframe, scales = "free_x", ncol = 2,
             labeller = labeller(Timeframe = c("dag3" = "3 Dage", "dag7" = "7 Dage",
                                               "dag10" = "10 Dage", "måned1" = "1 Måned"))) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Model Sammenligning Over Forskellige Prediction Tidspunkter",
       x = "Root Mean Squared Error (RMSE)",
       y = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 12))

test_rmse_long_best <- test_rmse_long |> 
  filter((Model == "Ridge" & Timeframe == "måned1") | (Model == "Lasso" & Timeframe == "dag10") | 
         (Model == "Lasso" & Timeframe == "dag7") | (Model == "Lasso" & Timeframe == "dag3"))

test_rmse_long_best <- test_rmse_long_best |> 
    mutate(
    Timeframe = factor(Timeframe, 
                       levels = c("måned1", "dag10", "dag7", "dag3"))
  )

test_rmse_long_best <- test_rmse_long_best |> 
  mutate(
    Timeframe = recode(
      Timeframe,
      "dag3"    = "3 Dage",
      "dag7"    = "7 Dage",
      "dag10"   = "10 Dage",
      "måned1"  = "1 Måned"
    ),
    Timeframe = factor(
      Timeframe,
      levels = c("1 Måned", "10 Dage", "7 Dage", "3 Dage")
    )
  )

ggplot(test_rmse_long_best,
       aes(x = Timeframe, y = RMSE, fill = Model)) +
  geom_col(width = 0.6) +
  coord_flip() +
  geom_text(aes(label = round(RMSE, 0)), hjust = -0.1, size = 5) +
  labs(
    title = "Bedst performende ML-modeller",
    subtitle = "Sammenligning på tværs af tid før kampstart",
    x = "Tid før kampstart",
    y = "RMSE",
    fill = "Model"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold")) +
  scale_fill_brewer(palette = "Set2")


# nye predictions --------------------------------------------------------
# nyt datasæt for den næste hjemmekampe i sæsonen mod BIF, som endnu ikke er spillet
# der er to observationer af den samme kamp, hvoraf den ene er et best case scenario 
# og den anden er et worst case scenario

predictions_nye <- data.frame(
  ugedag = c("Søn", "Søn"),
  vff_resultat_lagged = c("uafgjort", "uafgjort"),
  mål_sidste_kamp = c(1, 1),
  sæson_år = c("2026", "2026"),
  runde_nr = c(20, 20),
  mål_sidste3_lagged = c(8, 3),
  point_sidste3_lagged = c(5, 2),
  point_sæson_lagged = c(27, 24),
  tidspunkt = c("aften", "aften"),
  sidste_møde_tilskuere = c(7658, 7658),
  modstander = c("BIF", "BIF"),
  sidste_møde_resultat = c("vundet", "vundet"),
  temp_dry = c(8, -2),
  wind_speed = c(0.1, 7.4),
  precip_past1h = c(0, 1.5),
  d10_tilskuere = c(3987, 2264),
  d7_tilskuere = c(5021, 3191),
  d3_tilskuere = c(6183, 3972),
  sommerferie = c(0, 0),
  måned = c(2, 2),
  uge_nr = c(7, 7),
  er_helligdag = c(0, 0)
)

# sørger for at alt er faktorer, og at faktorerne har samme levels som det originale datasæt

predictions_nye <- predictions_nye |> 
  mutate(
    ugedag = factor(ugedag, levels = levels(vff_all$ugedag)),
    vff_resultat_lagged = factor(vff_resultat_lagged, levels = levels(vff_all$vff_resultat_lagged)),
    sæson_år = factor(sæson_år, levels = levels(vff_all$sæson_år)),
    tidspunkt = factor(tidspunkt, levels = levels(vff_all$tidspunkt)),
    modstander = factor(modstander, levels = levels(vff_all$modstander)),
    sidste_møde_resultat = factor(sidste_møde_resultat, levels = levels(vff_all$sidste_møde_resultat)),
    sommerferie = factor(sommerferie, levels = levels(vff_all$sommerferie)),
    er_helligdag = factor(er_helligdag, levels = levels(vff_all$er_helligdag))
  )

# datasæt med de rigtige variabler for hvert tidspunkt før kampstart

nye_1m <- predictions_nye |> 
  select(-d10_tilskuere, -d7_tilskuere, -d3_tilskuere)

nye_d10 <- predictions_nye |> 
  select(-d7_tilskuere, -d3_tilskuere)

nye_d7 <- predictions_nye |> 
  select(-d10_tilskuere, -d3_tilskuere)

nye_d3 <- predictions_nye |> 
  select(-d10_tilskuere, -d7_tilskuere)

# predictions for den nye kamp, lavet på de modeller der performede bedst på hvert tidspunkt

x_nye_1m <- model.matrix(~ ., data = nye_1m)[, -1]
nye_pred_1m <- predict(final_ridge_1m, newx = x_nye_1m)

x_nye_d10 <- model.matrix(~ ., data = nye_d10)[, -1]
nye_pred_d10 <- predict(final_lasso_d10, newx = x_nye_d10)

x_nye_d7 <- model.matrix(~ ., data = nye_d7)[, -1]
nye_pred_d7 <- predict(final_lasso_d7, newx = x_nye_d7)

x_nye_d3 <- model.matrix(~ ., data = nye_d3)[, -1]
nye_pred_d3 <- predict(final_lasso_d3, s = bestlambda_lasso_d3, newx = x_nye_d3)

# visualisering af de nye predictions
# laver en dataframe med resultaterne fra predictions

results <- data.frame(
  Scenario = c("Best Case", "Worst Case"),
  `1 Måned før` = c(nye_pred_1m[1], nye_pred_1m[2]),
  `10 Måned før` = c(nye_pred_d10[1], nye_pred_d10[2]),
  `7 Måned før` = c(nye_pred_d7[1], nye_pred_d7[2]),
  `3 Måned før` = c(nye_pred_d3[1], nye_pred_d3[2]),
  check.names = FALSE
)

results_long <- results |> 
  pivot_longer(cols = -Scenario, names_to = "Timeframe", values_to = "Predicted_Attendance") |>
  mutate(Timeframe = factor(Timeframe, levels = c("1 Måned før", "10 Måned før", 
                                                    "7 Måned før", "3 Måned før")))

results_long <- results_long |>
  mutate(
    days_before = case_when(
      Timeframe == "1 Måned før" ~ 30,
      Timeframe == "10 Måned før" ~ 10,
      Timeframe == "7 Måned før" ~ 7,
      Timeframe == "3 Måned før" ~ 3
    ),
    label_text = paste0(round(Predicted_Attendance, 0), " tilskuere\n(",
                        days_before, " dage før)")
  )

# plot af resultater

ggplot(results_long, aes(x = Timeframe, y = Predicted_Attendance, fill = Scenario)) +
  geom_col(position = "dodge", width = 0.7, color = "white", linewidth = 0.8) +
  geom_text(aes(label = paste0(round(Predicted_Attendance, 0), "\ntilskuere")), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 4.5, fontface = "bold") +
  scale_fill_manual(
    values = c("Best Case" = "#4CAF50", "Worst Case" = "#F44336"),
    labels = c("Bedste forhold", "Værste forhold")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     labels = function(x) format(x, big.mark = ".", decimal.mark = ",")) +
  labs(title = "Forudsigelser af VFF's næste hjemmekamp",
       subtitle = "Samme kamp (VFF vs BIF), forskellige predictiontidspunkter og forhold",
       x = NULL,
       y = "Forudsiget antal tilskuere",
       fill = "Scenarie") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 15, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, color = "gray30", margin = margin(b = 15)),
    plot.caption = element_text(hjust = 0, face = "italic", color = "gray50"),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    panel.grid.major.x = element_blank(),
    legend.title = element_text(face = "bold")
  )
