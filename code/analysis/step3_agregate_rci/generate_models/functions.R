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




## valid_model

