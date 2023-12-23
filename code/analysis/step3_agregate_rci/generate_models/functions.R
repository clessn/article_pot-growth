# Training ----------------------------------------------------------------

## generate one model
train_model <- function(
  data,
  right_equation,
  party,
  mix_model = FALSE
  ){
  vd <- paste0("rci_", party)
  equation <- paste0(vd, " ~ ", right_equation)
  if (mix_model){
    model <- lme4::lmer(formula = formula(equation),
                        data = data)
  } else {
    model <- lm(formula = formula(equation),
                data = data)
  }
  return(model)
}

## function to generate models for each party
train_models <- function(
    data,
    right_equation,
    parties,
    mix_model = FALSE
  ){
  models <- list()
  for (i in 1:length(parties)){
    partyi <- parties[i]
    models[[partyi]] <- train_model(
      data = data,
      right_equation = right_equation,
      party = partyi,
      mix_model = mix_model
    )
    message(paste0(partyi, " done"))
    }
  return(models)
}

## function to train and save a list of models to the wanted path
save_model_list <- function(
    data,
    path,
    model_name,
    right_equation,
    parties,
    mix_model = FALSE
  ){
  models <- train_models(data = Data,
                         right_equation = right_equation,
                         parties = parties,
                         mix_model = mix_model)
  filename <- paste0(model_name, ".rds")
  saveRDS(models, file.path(path, filename))
}


# Evaluate ----------------------------------------------------------------

evaluate_model <- function(model, test_data){
  if (class(model)[1] == "lm"){
    vd <- model[["terms"]][[2]]
    predictions <- predict(model, test_data)
    # Calculer R2, MSE, AIC, BIC
    r_squared <- summary(model)$r.squared
    adj_marginal_r2 <- summary(model)$r.squared
    mse <- mean((test_data[[vd]] - predictions)^2, na.rm = TRUE)  # Remplacez 'vd' par le nom de votre VD
    aic <- AIC(model)
    bic <- BIC(model)
  } else if (class(model)[1] == "lmerMod"){
    vd <- model@call[["formula"]][[2]]
    predictions <- predict(model, test_data)
    # Calculer R2, MSE, AIC, BIC
    r_squared <- unname(performance::r2(model)[[1]])
    if (is.na(r_squared)){
      r_squared <- MuMIn::r.squaredGLMM(model)[2]
    }
    adj_marginal_r2 <- unname(performance::r2(model)[[2]])
    mse <- mean((test_data[[vd]] - predictions)^2, na.rm = TRUE)  # Remplacez 'vd' par le nom de votre VD
    aic <- AIC(model)
    bic <- BIC(model)
  }
  results <- data.frame(r_squared, adj_marginal_r2, mse, aic, bic)
  return(results)
}

# Fonction pour évaluer les modèles de régression
evaluate_models_file <- function(path, test_data) {
  # Lire les modèles depuis le fichier RDS
  models_list <- readRDS(path)
  model_name <- gsub("\\.rds", "", basename(path))
  # Initialiser un dataframe pour les résultats
  results <- data.frame(
    model_name = character(),
    party = character(),
    r_squared = numeric(),
    adj_marginal_r2 = numeric(),
    mse = numeric(),
    aic = numeric(),
    bic = numeric(),
    stringsAsFactors = FALSE
  )
  # Évaluer chaque modèle
  for (party in names(models_list)) {
    model <- models_list[[party]]
    resultsi <- data.frame(model_name, party) %>%
      cbind(., evaluate_model(model, test_data))
    # Ajouter les résultats au dataframe
    results <- rbind(results, resultsi)
  }
  return(results)
}
