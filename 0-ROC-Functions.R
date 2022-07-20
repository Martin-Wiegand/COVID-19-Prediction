### ROC/ C-statistic computation
# Normal Kernel
K <- function(x,mu,h){
  return(dnorm(x = x,mean = mu,sd = h)/h)
}

# Kernel Weighted Survival function
Sk <- function(Ui,time,data){
  
  # Non censored - unique event times
  non_censored <- data %>% 
    filter(STATUS_WITHIN_W_OF_LM != 0)
  
  # relevant times, below the function argument "time"
  xi <- data %>% 
    filter(STATUS_TIME_FROM_LM <= time) %>%
    filter(STATUS_WITHIN_W_OF_LM != 0) %>%
    pull(STATUS_TIME_FROM_LM) %>% 
    as.numeric
  
  h <- 1.06*nrow(data)^(-1/5)*sd(data$Uj)
  
  data <- data %>% ungroup %>%
    arrange(desc(STATUS_TIME_FROM_LM)) %>%
    mutate(U_high = cumsum(K(x = Uj,mu = Ui,h=h)))
  
  # Compute inner fraction
  fraction <- function(xi_temp){
    U_temp <- non_censored %>% filter(STATUS_TIME_FROM_LM == xi_temp) %>% 
      pull(Uj)
    U_high_sum <- data %>% filter(STATUS_TIME_FROM_LM == xi_temp) %>% 
      pull(U_high)
    
    return(1-K(x = U_temp,mu = Ui,h = h)/U_high_sum)
  }
  
  return(prod(unlist(sapply(X = xi,FUN = fraction))))
}



# kernel weighted CIF
Fk <- function(k,Ui,time,data){
  # Relevant data, outcome "k"
  rel_data <- data %>% 
    filter(STATUS_WITHIN_W_OF_LM == k)
  
  # Relevant times, lower than "time"
  xi <- data %>% 
    filter(STATUS_TIME_FROM_LM <= time) %>%
    filter(STATUS_WITHIN_W_OF_LM != 0) %>%
    pull(STATUS_TIME_FROM_LM) %>% 
    as.numeric
  
  h <- 1.06*nrow(data)^(-1/5)*sd(data$Uj)
  
  fraction <- function(xi_temp){
    if(data %>% 
       filter(STATUS_TIME_FROM_LM == xi_temp) %>% 
       pull(STATUS_WITHIN_W_OF_LM) == k){
      
      U_temp <- rel_data %>% filter(STATUS_TIME_FROM_LM == xi_temp) %>% 
        pull(Uj)
      
      U_high <- data %>% filter(STATUS_TIME_FROM_LM >= xi_temp) %>% 
        pull(Uj)
      
      Sk_temp <- data %>% filter(STATUS_TIME_FROM_LM == xi_temp) %>%
        pull(Survival)
      
      return(K(x = U_temp,mu = Ui,h = h)/sum(K(x = U_high,mu = Ui,h = h))*Sk_temp)
    }else{
      return(0)
    }
  }
  
  return(sum(unlist(sapply(X = xi,FUN = fraction))))
}

# Weight function for spec/sens computation
Wk <- function(k,event,Ui,time,w,data){
  if(time >= w) return(0)
  if(time < w){5
    if(event %in% k){
      return(1)
    }else{
      if(event == 0){
        return((Fk(k = k,Ui = Ui,time = w,data = data)-Fk(k = k,Ui = Ui,time = time,data = data))/Sk(time = time,Ui = Ui,data = data))
      }else{
        return(0)
      }
    }
  }
}

# Sensitivity
Se <- function(weight,risk){
  eval <- function(c){
    return(sum(weight*as.numeric(risk > c))/sum(weight))
  }
  
  return(sapply(X = risk,FUN = eval))
}

SpA <- function(weight,risk){
  eval <- function(c){
    return(sum((1 - weight)*as.numeric(risk <= c))/sum(1-weight))
  }
  
  return(sapply(X = risk,FUN = eval))
}

SpB <- function(weight,weightcr,risk){
  eval <- function(c){
    return(sum((1 - (weight + weightcr))*as.numeric(risk <= c))/sum(1 - (weight + weightcr)))
  }
  
  return(sapply(X = risk,FUN = eval))
}



ROC_crr <- function(crr_mod,data_sample,selection,event,alt_event,w){
  # Evaluate FGR individual CIFs for each patient
  Uj <- predict(crr_mod,
                cov1 = as.matrix(data_sample %>% 
                                   ungroup %>% 
                                   select(all_of(selection))))[,-1]
  
  # data_sample <- data_sample %>% select(STUDY_SUBJECT_DIGEST:STATUS_WITHIN_W_OF_LM,selection)
  
  Uj <- Uj[nrow(Uj),]
  
  data_sample <-
    data_sample %>% ungroup %>% mutate(Uj = Uj)
  
  if(nrow(data_sample %>% filter(STATUS_TIME_FROM_LM < w & STATUS_WITHIN_W_OF_LM == 0)) > 0){
    
    
    # CompVals <- intersect(which(data_sample$STATUS_TIME_FROM_LM < w),which(data_sample$STATUS_WITHIN_W_OF_LM == 0))
    
    # Sk_values <- mapply(time = as.numeric(data_sample$STATUS_TIME_FROM_LM),
    #                     Ui = data_sample$Uj,
    #                     FUN = Sk,
    #                     MoreArgs = list(data_sample))
    
    # Sk_values <- rep(0,nrow(data_sample))
    # Sk_values[CompVals] <- Sk_values_cv
    
    data_sample <- data_sample %>% mutate(Survival = 0.5)
  }
  
  data_sample <- data_sample %>% 
    rowwise() %>%
    mutate(Weight = Wk(k = event,event = STATUS_WITHIN_W_OF_LM,Ui = Uj,time = STATUS_TIME_FROM_LM,data = data_sample,w = w)) %>%
    mutate(Weight_alt = Wk(k = alt_event,event = STATUS_WITHIN_W_OF_LM,Ui = Uj,time = STATUS_TIME_FROM_LM,data = data_sample,w = w))
  
  
  plot.data <- tibble(Sensitivity =  Se(weight = data_sample$Weight,risk = data_sample$Uj),
                      SpecificityA = SpA(weight = data_sample$Weight,risk = data_sample$Uj),
                      SpecificityB = SpB(weight = data_sample$Weight,weightcr = data_sample$Weight_alt,risk = data_sample$Uj))
  
  brier <- mean(data_sample$Weight*(1-data_sample$Uj)^2 + (1-data_sample$Weight)*data_sample$Uj^2)
  
  return(list(AUC = integrate.xy(1-plot.data$SpecificityA,plot.data$Sensitivity),
              Brier = brier,
              Spec = plot.data$SpecificityA,
              Sens = plot.data$Sensitivity))
}


# Variable selection for respective outcomes
variable_selection_subdist <- function(data_matrix,failcode,nlambda,niter,eps,threshold,LASSO_group,censored = F){
  
  status = as.numeric(data_matrix$STATUS_WITHIN_W_OF_LM)
  status_time = as.numeric(data_matrix$STATUS_TIME_FROM_LM)
  
  # Remove covariates not of interest (LDH, ASPARTATE, TROPONIN are zero)
  if(censored){
    data_matrix_FGR_cropped <- data_matrix %>%
      ungroup %>%
      select(-STUDY_SUBJECT_DIGEST,-STAY,-INDEX,-INDEX_W,
             -STATUS_TIME_FROM_LM,-STATUS_WITHIN_W_OF_LM,-LDH,-LDH_WITHIN_THRESHOLD,
             -TROPONIN,-TROPONIN_WITHIN_THRESHOLD,-ASPARTATE,-ASPARTATE_WITHIN_THRESHOLD,
             -BASOPHIL,-BASOPIL_WITHIN_THRESHOLD)
  }else{
    data_matrix_FGR_cropped <- data_matrix %>%
      ungroup %>%
      select(-STUDY_SUBJECT_DIGEST,-STAY,-INDEX,-INDEX_W,
             -STATUS_TIME_FROM_LM,-STATUS_WITHIN_W_OF_LM)
  }
  
  
  exclude <- which(apply(X = data_matrix_FGR_cropped,MARGIN = 2,FUN = function(x){return(length(unique(x)) == 1)}))
  
  if(length(exclude) > 0) data_matrix_FGR_cropped <- data_matrix_FGR_cropped[,-exclude]
  
  if(missing(LASSO_group)){
    fit <-
      crrp(time = status_time,
           fstatus = as.integer(status),
           failcode = failcode,
           cencode = 0,
           penalty = "LASSO",
           nlambda = nlambda,
           max.iter = niter,
           eps = eps,
           X = as.matrix(data_matrix_FGR_cropped 
                         # %>% select(ICU:RF_LAST_24)
           ))
  }else{
    duplicates <- t(data_matrix_FGR_cropped) %>% duplicated()
    data_matrix_FGR_cropped <- data_matrix_FGR_cropped[,!duplicates]
    
    group_membership <- sapply(X = names(data_matrix_FGR_cropped),FUN = function(y) which(unlist(lapply(X = LASSO_group,FUN = function(x) any(x == y)))))
    group_membership <- lapply(X = group_membership,FUN = function(x) ifelse(length(x) == 0,0,x))
    group_membership <- unlist(group_membership)
    unassigned_group <- which(unlist(group_membership) == 0)
    group_membership[unassigned_group] <- max(group_membership) + 1:length(unassigned_group)
    
    groupmat = tibble(VARNAME = names(unlist(group_membership)),GROUPNUMBER = unlist(group_membership)) %>% arrange(GROUPNUMBER)
    groupmat <- groupmat %>% group_by(GROUPNUMBER) %>% mutate(n = n()) 
    groups_n <- groupmat %>% n_groups
    groupmat <- groupmat %>% ungroup %>% 
      mutate(NEWGROUP = rep(1:groups_n,groupmat %>% group_by(GROUPNUMBER) %>% slice(1) %>% pull(n)))
    
    fit <-
      gcrrp(group = groupmat %>% pull(NEWGROUP),
            time = status_time,
            fstatus = as.integer(status),
            failcode = failcode,
            cencode = 0,
            penalty = "gLASSO",
            nlambda = nlambda,
            max.iter = niter,
            eps = eps,
            weighted = T,
            X = as.matrix(data_matrix_FGR_cropped %>% select(groupmat$VARNAME)))
  }
  
  
  beta <- fit$beta[,which.min(abs(fit$BIC))]
  
  return(names(beta[which(abs(beta) > threshold)]))
}

add_dummies <- function(var_selection,model_groups){
  values <- which(!grepl("THRESHOLD|COUNT|DUMMY",var_selection))
  dummies <-  which(grepl("THRESHOLD|COUNT|DUMMY",var_selection))
  
  return(unlist(lapply(X = model_groups,FUN = add_internal,var_sel = var_selection)))
}

add_internal <- function(x,var_sel){
  choice <- which(x %in% var_sel)
  
  if(1 %in% choice & !2 %in% choice){
    return(x)
  }else{
    return(x[choice])
  }
}


# Build crr object based on data set cov1 with the variable selection "covariates"
# for event "failcode"
build_crr <- function(cov1,covariates,failcode){
  status = cov1$STATUS_WITHIN_W_OF_LM
  status_time = cov1$STATUS_TIME_FROM_LM
  
  data_selection <- cov1 %>% 
    ungroup %>% 
    select(all_of(covariates))
  duplicates <- t(data_selection) %>% duplicated()
  data_selection <- data_selection[,!duplicates]
  
  crr(ftime = status_time,
      fstatus = status,
      cov1 = data_selection,
      cencode = 0,
      failcode = failcode) %>%
    return
}

############## Adjust BIC

duplicate_covariates = function(y){
  y[lower.tri(y,diag = T)] = 0
  y[upper.tri(y,diag = F)] = ifelse(y[upper.tri(y,diag = F)] == 1,1,0)
  return(sum(y))
}

duplicate_covariates_which = function(y){
  y[lower.tri(y,diag = T)] = 0
  y[upper.tri(y,diag = F)] = ifelse(y[upper.tri(y,diag = F)] == 1,1,0)
  return(which(colSums(y) > 0))
}

adjust_BIC_rob <- function(BIC,beta,data){
  BIC[which(BIC == 0)] <- Inf
  reductions <- numeric(0)
  
  for(l in 1:ncol(beta)){
    z <- beta[,l]
    if(length(na.omit(z[1] != 0))){
      print(l)
      if(any(z != 0)){
        reductions[l] <-   names(z[z != 0])[(as.numeric(factor(apply(data %>% 
                                                                       ungroup %>% 
                                                                       select(-(STUDY_SUBJECT_DIGEST:STATUS_WITHIN_W_OF_LM)) %>% 
                                                                       as.matrix, 2, paste, collapse=",")))[z != 0] %>% 
                                               duplicated)] %>% length
      }else{
        reductions[l] <- 0}
      
      # if(l == ncol(beta)) reductions[l] <- 0
    }else{
      reductions[l] <- 0
    }
    
  }
  
  adjusted_BIC = BIC - log(nrow(data))*unlist(reductions)
  return(adjusted_BIC)
}


adjust_BIC <- function(BIC,beta,data){
  BIC[which(BIC == 0)] <- Inf
  
  reductions <- numeric(0)
  
  for(l in 1:ncol(beta)){
    z <- beta[,l]
    if(length(na.omit(z[1] != 0))){
      if(any(z != 0)){
        reductions[l] <- duplicate_covariates(y = cor(data %>% ungroup %>% select(names(which(z != 0))))) %>% as.numeric
      }else{
        reductions[l] <- 0}
      
      # if(l == ncol(beta)) reductions[l] <- 0
    }else{
      reductions[l] <- 0
    }
    
  }
  
  adjusted_BIC = BIC - log(nrow(data))*unlist(reductions)
  return(adjusted_BIC)
}


#############

string_separate <- function(x){
  
  if(is.na(as.numeric(tail(strsplit(x = x,split = "_") %>% unlist,1))) | grepl("BELOW",x)){
    return(x)
  }else{
    return(sub("_[^_]+$", "", x))
  }
}

evaluate_model <- function(model,test,train, failcode = 2,base_group,bic.min){ 
  if(missing(bic.min)){adjusted_BIC = adjust_BIC_rob(BIC = model$BIC,beta = model$beta,data = train)
  bic.min <- adjusted_BIC %>% which.min}
  beta.min <- model$beta[,bic.min]
  cbind(beta.min[beta.min != 0],base_group[beta.min != 0]) %>% print
  nonzero_beta <- which(beta.min != 0)
  
  beta_min_tibble = tibble(COVARIATE = names(beta.min),BETA = beta.min)
  
  beta_min_tibble = beta_min_tibble %>% filter(BETA != 0) %>% 
    rowwise %>% 
    mutate(RAW_NAME = string_separate(COVARIATE)) %>% 
    group_by(RAW_NAME) %>% 
    summarise(COVARIATE = first(COVARIATE),BETA = sum(BETA)) 
  
  # Create crr model
  # Crop training and test matrices
  train.matrix <- as.matrix(train %>% ungroup %>% select(-(STUDY_SUBJECT_DIGEST:STATUS_WITHIN_W_OF_LM)) %>% select(beta_min_tibble$COVARIATE))
  test.matrix <- as.matrix(test %>% ungroup %>% select(-(STUDY_SUBJECT_DIGEST:STATUS_WITHIN_W_OF_LM)) %>% select(beta_min_tibble$COVARIATE))
  
  
  # Build CRR
  crr_model <-
    crr(ftime = train$STATUS_TIME_FROM_LM,
        fstatus = train$STATUS_WITHIN_W_OF_LM,
        cov1 = train.matrix,
        cencode = 0,
        failcode = failcode,
        maxiter = 0,
        init = beta_min_tibble$BETA)
  
  saveRDS(crr_model,rds_file("crr_model_trained"))
  
  print("compute ROC/Brier")
  # Compute ROC/ Brier
  ROC <-
    ROC_crr(crr_mod = crr_model,
            data_sample = test,
            selection = colnames(test.matrix),
            event = 2,
            alt_event = c(1,3),
            w = 2)
  
  
  #### Calibration
  
  Uj <- predict(crr_model,
                cov1 = train.matrix)[,-1]
  Uj_test <- predict(crr_model,
                     cov1 = test.matrix)[,-1]
  
  # where does selection come from? Training probabilities
  retmat <- tibble(train %>% select(STUDY_SUBJECT_DIGEST:STATUS_WITHIN_W_OF_LM,colnames(train.matrix)),
                   Probability = Uj[nrow(Uj),])
  
  # Probabilities test data
  retmat_test <- tibble(test %>% select(STUDY_SUBJECT_DIGEST:STATUS_WITHIN_W_OF_LM,colnames(test.matrix)),
                        Probability = Uj_test[nrow(Uj_test),])
  
  emp_dec = quantile(retmat$Probability,probs = seq(0.1,1,by = 0.1))
  
  retmat <-
    retmat %>% mutate(Probability_Decile = case_when(Probability < 0.1 ~ 0.1,
                                                     0.1 <= Probability & Probability < 0.2 ~ 0.2,
                                                     0.2 <= Probability & Probability < 0.3 ~ 0.3,
                                                     0.3 <= Probability & Probability < 0.4 ~ 0.4,
                                                     0.4 <= Probability & Probability < 0.5 ~ 0.5,
                                                     0.5 <= Probability & Probability < 0.6 ~ 0.6,
                                                     0.6 <= Probability & Probability < 0.7 ~ 0.7,
                                                     0.7 <= Probability & Probability < 0.8 ~ 0.8,
                                                     0.8 <= Probability & Probability < 0.9 ~ 0.9,
                                                     0.9 <= Probability ~ 1))  %>%
    mutate(Probability_Decile_Emp = case_when(Probability < emp_dec[1] ~ 0.1,
                                              emp_dec[1] <= Probability & Probability < emp_dec[2] ~ 0.2,
                                              emp_dec[2] <= Probability & Probability < emp_dec[3] ~ 0.3,
                                              emp_dec[3] <= Probability & Probability < emp_dec[4] ~ 0.4,
                                              emp_dec[4] <= Probability & Probability < emp_dec[5] ~ 0.5,
                                              emp_dec[5] <= Probability & Probability < emp_dec[6] ~ 0.6,
                                              emp_dec[6] <= Probability & Probability < emp_dec[7] ~ 0.7,
                                              emp_dec[7] <= Probability & Probability < emp_dec[8] ~ 0.8,
                                              emp_dec[8] <= Probability & Probability < emp_dec[9] ~ 0.9,
                                              emp_dec[9] <= Probability ~ 1))
  
  plotmat <-
    retmat %>% group_by(Probability_Decile) %>%
    summarise(INCIDENCE = mean(STATUS_WITHIN_W_OF_LM == failcode),
              MEAN_PROB = mean(Probability),
              SD_INCIDENCE = sd(STATUS_WITHIN_W_OF_LM == failcode),
              N = n())
  
  plotmat <- plotmat %>%
    mutate(U = INCIDENCE + SD_INCIDENCE*1.96) %>% 
    mutate(L = INCIDENCE + SD_INCIDENCE*1.96)
  
  g_deciles = ggplot(data = plotmat) +
    geom_point(aes(x = MEAN_PROB,y = INCIDENCE)) +
    geom_text(aes(x = MEAN_PROB,y = INCIDENCE-0.05,label = N)) +
    geom_abline(slope = 1,intercept = 0) +
    labs(x = "Mean Probability (by decile)",
         y = "Incidence within decile")
  
  
  plotmat_emp <-
    retmat %>% group_by(Probability_Decile_Emp) %>%
    summarise(INCIDENCE = mean(STATUS_WITHIN_W_OF_LM == failcode),
              MEAN_PROB = mean(Probability),
              SD_INCIDENCE = sd(STATUS_WITHIN_W_OF_LM == failcode),
              N = n())
  
  g_deciles_emp = ggplot(data = plotmat_emp) +
    geom_point(aes(x = MEAN_PROB,y = INCIDENCE)) +
    geom_text(aes(x = MEAN_PROB,y = INCIDENCE-0.01,label = N)) +
    geom_abline(slope = 1,intercept = 0) +
    labs(x = "Mean Probability (by empirical decile)",
         y = "Incidence within empirical decile")
  
  return(list(ROC = ROC,
              nonzero_beta = nonzero_beta,
              beta = beta_min_tibble,
              crr_model = crr_model,
              calibration_plot = g_deciles,
              calibration_plot_emp = g_deciles_emp,
              retmat = retmat,
              retmat_test = retmat_test))
}


crr_eval = function(crr_model,cov1,results){
  Uj = predict(crr_model,cov1 = cov1)[,-1]
  Uj = Uj[nrow(Uj),]
  
  return(list(pROC::auc(results,Uj),
              ci.auc(results,Uj)))
}
