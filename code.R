# =============================================================================
# Hidden World Latent Variable Analysis
# =============================================================================
# This script demonstrates various latent variable modeling techniques:
# - Exploratory Factor Analysis (EFA)
# - Confirmatory Factor Analysis (CFA) 
# - Structural Equation Modeling (SEM)
# - Latent Class Analysis (LCA)
# =============================================================================

# Package Installation (uncomment if packages need to be installed)
# install.packages(c("psych", "lavaan", "poLCA", "GPArotation", "corrplot", 
#                    "ggplot2", "parameters"))

# Load required packages
suppressPackageStartupMessages({
  library(psych)        # Exploratory Factor Analysis
  library(lavaan)       # Confirmatory Factor Analysis and SEM
  library(poLCA)        # Latent Class Analysis
  library(GPArotation)  # Factor rotation methods
  library(corrplot)     # Correlation plot visualization
  library(ggplot2)      # Advanced plotting
  library(stats)        # Built-in statistics functions
  library(parameters)   # Parameter processing for psych
})

# =============================================================================
# EXPLORATORY FACTOR ANALYSIS (EFA)
# =============================================================================

# Prepare data for EFA analysis
prepare_bfi_data <- function() {
  data(bfi) # Big Five Personality Trait Dataset from psych package
  bfi <- as.data.frame(bfi)
  # Remove rows with NAs for indicator variables (not the best way to handle missing data)
  bfi <- bfi[complete.cases(bfi[, 1:25]), ]
  return(bfi)
}

# Correlation analysis and visualization
analyze_correlations <- function(data, vars = 1:25) {
  # Create correlation matrix and test for significance
  test_res <- cor.mtest(data[, vars], conf.level = 0.95)
  cor_matrix <- cor(data[, vars])
  
  # Visualize correlation matrix
  corrplot(cor_matrix, p.mat = test_res$p, insig = 'blank')
  
  return(list(correlations = cor_matrix, test_results = test_res))
}

# Test sampling adequacy
test_sampling_adequacy <- function(data, vars = 1:25) {
  # KMO Test of sampling adequacy (higher value is better)
  kmo_result <- KMO(data[, vars])
  print(paste("KMO Test Result:", kmo_result$MSA))
  
  # Bartlett test (test of whether correlation matrix is identity)
  cor_matrix <- cor(data[, vars])
  bartlett_result <- cortest.bartlett(cor_matrix, n = nrow(data))
  print(paste("Bartlett test p-value:", bartlett_result$p.value))
  
  return(list(kmo = kmo_result, bartlett = bartlett_result))
}

# Run EFA with specified parameters
run_efa <- function(data, vars = 1:25, n_factors = 5, rotation = 'oblimin') {
  efa_result <- factanal(data[, vars], factors = n_factors, rotation = rotation)
  print(efa_result, sort = FALSE, cutoff = 0.3)
  return(efa_result)
}

# Generate scree plot and eigenvalues
create_scree_plot <- function(data, vars = 1:25) {
  cor_matrix <- cor(data[, vars])
  eigenvals <- eigen(cor_matrix)
  scree(cor_matrix, factors = FALSE)
  return(eigenvals)
}

# Execute EFA analysis
execute_efa_analysis <- function() {
  cat("=== EXPLORATORY FACTOR ANALYSIS ===\n")
  
  # Prepare data
  bfi <- prepare_bfi_data()
  cat("Data prepared. Sample size:", nrow(bfi), "\n")
  
  # Analyze correlations
  cor_analysis <- analyze_correlations(bfi)
  
  # Test sampling adequacy
  adequacy_tests <- test_sampling_adequacy(bfi)
  
  # Run EFA with 5 factors
  efa_result <- run_efa(bfi, n_factors = 5, rotation = 'oblimin')
  
  # Create scree plot
  eigenvals <- create_scree_plot(bfi)
  
  return(list(
    data = bfi,
    correlations = cor_analysis,
    adequacy = adequacy_tests,
    efa = efa_result,
    eigenvalues = eigenvals
  ))
}

# Run EFA analysis
efa_results <- execute_efa_analysis()



# =============================================================================
# CONFIRMATORY FACTOR ANALYSIS (CFA)
# =============================================================================

execute_cfa_analysis <- function(data = NULL) {
  cat("=== CONFIRMATORY FACTOR ANALYSIS ===\n")
  
  # Use BFI data if not provided
  if (is.null(data)) {
    data <- prepare_bfi_data()
  }
  
  # Create gender dummy variable
  data$female <- ifelse(data$gender == 2, 1, 0)
  
  # Define CFA model with Big Five factors and covariates
  model <- '
    # Latent variables (Big Five personality factors)
    A =~ A1 + A2 + A3 + A4 + A5  # Agreeableness
    C =~ C1 + C2 + C3 + C4 + C5  # Conscientiousness
    E =~ E1 + E2 + E3 + E4 + E5  # Extraversion
    N =~ N1 + N2 + N3 + N4 + N5  # Neuroticism
    O =~ O1 + O2 + O3 + O4 + O5  # Openness
    
    # Regression relationships with demographics
    A ~ age + female
    C ~ age + female
    E ~ age + female
    N ~ age + female
    O ~ age + female
  '
  
  # Fit the model using robust WLSMV estimator for ordinal data
  fit <- sem(model, 
             data = data, 
             std.lv = TRUE, 
             ordered = c('A1', 'A2', 'A3', 'A4', 'A5',
                        'C1', 'C2', 'C3', 'C4', 'C5',
                        'E1', 'E2', 'E3', 'E4', 'E5',
                        'N1', 'N2', 'N3', 'N4', 'N5',
                        'O1', 'O2', 'O3', 'O4', 'O5'), 
             estimator = 'WLSMV')
  
  # Print comprehensive results
  summary(fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
  
  return(fit)
}

# Run CFA analysis
cfa_results <- execute_cfa_analysis()



# =============================================================================
# LATENT CLASS ANALYSIS (LCA)
# =============================================================================

# Analyze cheating behavior dataset
execute_lca_analysis <- function() {
  cat("=== LATENT CLASS ANALYSIS ===\n")
  
  # Load cheating dataset
  data(cheating)
  
  # Analyze correlations among cheating indicators
  analyze_cheating_correlations()
  
  # Run unconditional LCA
  unconditional_results <- run_unconditional_lca()
  
  # Analyze relationship between LCA classes and GPA
  analyze_lca_gpa_relationship(unconditional_results)
  
  # Run conditional LCA with GPA as covariate
  conditional_results <- run_conditional_lca()
  
  return(list(
    unconditional = unconditional_results,
    conditional = conditional_results
  ))
}

# Analyze correlations among cheating indicators
analyze_cheating_correlations <- function() {
  # Create correlation matrix for cheating indicators
  test_res <- cor.mtest(cheating[, 1:4], conf.level = 0.95)
  cor_matrix <- cor(cheating[, 1:4])
  corrplot(cor_matrix, p.mat = test_res$p)
  
  cat("Cheating indicators correlation analysis completed.\n")
  cat("Variables:\n")
  cat("- LIEEXAM: Lied to avoid taking an exam\n")
  cat("- LIEPAPER: Lied to avoid handing in term paper on time\n")
  cat("- FRAUD: Purchased term paper or obtained exam copy\n")
  cat("- COPYEXAM: Copied answers during exam\n")
  cat("- GPA: Student GPA on 5-point scale\n")
}

# Run unconditional LCA to determine optimal number of classes
run_unconditional_lca <- function() {
  # Define formula for unconditional LCA
  f <- cbind(LIEEXAM, LIEPAPER, FRAUD, COPYEXAM) ~ 1
  
  # Test different numbers of classes
  BIC <- c()
  for (i in 2:5) {
    ch2 <- poLCA(f, cheating, nclass = i, nrep = 100, verbose = FALSE)
    BIC <- append(BIC, ch2$bic)
  }
  
  # Plot BIC values
  plot(c(2, 3, 4, 5), BIC, type = 'b', 
       xlab = 'Number of Classes', ylab = 'BIC',
       main = 'BIC for Different Numbers of Classes')
  
  # Fit final model with 2 classes (based on BIC)
  final_model <- poLCA(f, cheating, nclass = 2, nrep = 100, graphs = TRUE)
  
  return(list(bic_values = BIC, model = final_model))
}

# Analyze relationship between LCA classes and GPA
analyze_lca_gpa_relationship <- function(lca_results) {
  # Add posterior probabilities to dataset
  cheating_extended <- cbind(cheating, lca_results$model$posterior)
  
  # Calculate mean probability of being in class 2 for each GPA level
  for (i in 1:5) {
    mean_prob <- mean(cheating_extended[which(cheating_extended$GPA == i), ]$'2')
    cat(sprintf("GPA level %d: Mean probability of Class 2 = %.3f\n", i, mean_prob))
  }
  
  # Create visualization
  mean_prob_vector <- c()
  for (i in 1:5) {
    mgpa <- mean(cheating_extended[which(cheating_extended$GPA == i), ]$'2')
    mean_prob_vector <- append(mean_prob_vector, mgpa)
  }
  
  plot(c(1, 2, 3, 4, 5), mean_prob_vector, type = 'b', 
       xlab = "GPA Level", ylab = "Mean Probability of Class 2",
       main = "Relationship between GPA and Cheating Class Membership")
}

# Run conditional LCA with GPA as covariate
run_conditional_lca <- function() {
  # Define formula for conditional LCA
  f2 <- cbind(LIEEXAM, LIEPAPER, FRAUD, COPYEXAM) ~ GPA
  
  # Fit conditional model
  conditional_model <- poLCA(f2, cheating, nclass = 2, graphs = TRUE, nrep = 100)
  
  return(conditional_model)
}

# Run LCA analysis
lca_results <- execute_lca_analysis()


# =============================================================================
# ADDITIONAL SEM EXAMPLES
# =============================================================================

# Example 1: Social Capital SEM Model (requires custom dataset)
execute_social_capital_sem <- function(data = NULL) {
  cat("=== SOCIAL CAPITAL SEM ANALYSIS ===\n")
  
  if (is.null(data)) {
    cat("Note: This example requires a custom dataset (my_data) to run.\n")
    return(NULL)
  }
  
  # Define comprehensive social capital model
  model <- '
    # Measurement model (latent variables)
    Netnov =~ q27cnt + Prestige_entropy + Prestige_score
    NeighEngage =~ q23b + q24a + q24b
    CommEngage =~ q25e + q25a + q25c
    SocialSup =~ proximity + gender_hom + strength2
    
    # Structural model (regression relationships)
    Netnov ~ CommEngage
    act_diverse ~ SocialSup + Netnov + worker + inc + age40to60 + mt60 + graduate
    act_count ~ SocialSup + Netnov + graduate + inc + age40to60 + mt60 + worker
    
    # Residual correlations
    act_diverse ~~ act_count
    CommEngage ~~ NeighEngage
    q27cnt ~~ Prestige_score
    q27cnt ~~ Prestige_entropy
    q23b ~~ q24b
    
    # Constrained correlations (set to zero)
    Netnov ~~ 0*NeighEngage
    Netnov ~~ 0*CommEngage
    SocialSup ~~ 0*NeighEngage
    SocialSup ~~ 0*CommEngage
  '
  
  # Fit model with ordinal indicators
  fit <- sem(model, data = data, std.lv = TRUE, 
             ordered = c('q23b', 'q24a', 'q24b', 'q25e', 'q25a', 'q25c'),
             estimator = 'WLSMV', se = 'robust')
  
  # Display results
  summary(fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
  
  return(fit)
}

# Example 2: Telework Satisfaction SEM Model (requires custom dataset)
execute_telework_satisfaction_sem <- function(data = NULL) {
  cat("=== TELEWORK SATISFACTION SEM ANALYSIS ===\n")
  
  if (is.null(data)) {
    cat("Note: This example requires a custom dataset to run.\n")
    return(NULL)
  }
  
  # Define telework satisfaction model
  model <- '
    # Measurement model
    benefits =~ V25 + V24 + V39 + V26 + workflex
    barriers =~ V21 + V22 + V26 + workflex
    
    # Structural model for latent variables
    benefits ~ V10 + V12 + V40 + V29
    barriers ~ V20 + V38 + V40
    
    # Regression equation for satisfaction outcome
    V2 ~ V6 + age2 + V11 + V18 + V30 + V31 + V40 + V28 + benefits + barriers
    
    # Residual correlations
    benefits ~~ 0*barriers
  '
  
  # Fit model with ordinal indicators
  fit <- sem(model, data = data, 
             ordered = c("V25", "V24", "V21", "V22", "V26", "V39", "V2", "workflex"))
  
  # Display results
  summary(fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
  
  return(fit)
}

# Note: These examples require custom datasets that are not provided
# To run these analyses, you would need to load your own data first:
# social_capital_results <- execute_social_capital_sem(my_data)
# telework_results <- execute_telework_satisfaction_sem(telework_data)

# =============================================================================
# STRUCTURAL EQUATION MODELING (SEM) - Basic Example
# =============================================================================

execute_basic_sem <- function() {
  cat("=== BASIC STRUCTURAL EQUATION MODELING ===\n")
  
  # Load dataset
  data(HolzingerSwineford1939)
  
  # Define SEM model with measurement and structural components
  model <- '
    # Measurement model (factor structure)
    visual  =~ x1 + x2 + x3      # Visual perception
    textual =~ x4 + x5 + x6      # Textual ability
    speed   =~ x7 + x8 + x9      # Processing speed
    
    # Structural model (relationships between latent variables)
    visual ~ ageyr + grade
    textual ~ ageyr + grade
    speed ~ ageyr + grade
  '
  
  # Fit the SEM model
  fit <- sem(model, data = HolzingerSwineford1939)
  
  # Display comprehensive results
  summary(fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
  
  # Create parameter estimates table
  param_table <- parameterestimates(fit, standardized = TRUE)
  print(param_table)
  
  return(list(
    model = fit,
    parameters = param_table
  ))
}

# Run basic SEM analysis
basic_sem_results <- execute_basic_sem()


# =============================================================================
# ADDITIONAL LCA EXAMPLE
# =============================================================================

# Example: Remote Work Impact LCA (requires custom dataset)
execute_remote_work_lca <- function(opinion_data = NULL) {
  cat("=== REMOTE WORK IMPACT LCA ANALYSIS ===\n")
  
  if (is.null(opinion_data)) {
    cat("Note: This example requires a custom dataset (opinion_data) to run.\n")
    return(NULL)
  }
  
  # Data preprocessing function
  preprocess_opinion_data <- function(data) {
    # Recode response categories
    data[data == "missing"] <- 1
    data[data == "Very positive"] <- 2
    data[data == "Somewhat positive"] <- 2
    data[data == "Neither negative nor positive"] <- 3
    data[data == "Somewhat negative"] <- 4
    data[data == "Very negative"] <- 4
    
    # Convert to numeric
    opinion_vars <- paste0("Q322_", 1:12)
    for (var in opinion_vars) {
      if (var %in% names(data)) {
        data[[var]] <- as.numeric(data[[var]])
      }
    }
    
    return(data)
  }
  
  # Preprocess data
  opinion_data <- preprocess_opinion_data(opinion_data)
  
  # Define LCA formula for work aspects
  f <- cbind(Q322_1, Q322_2, Q322_3, Q322_4, Q322_5, Q322_6,
             Q322_7, Q322_8, Q322_9, Q322_10, Q322_11, Q322_12) ~ 1
  
  # Test different numbers of classes
  bic_values <- c()
  for (i in 1:8) {
    tryCatch({
      lca_model <- poLCA(f, opinion_data, nclass = i, nrep = 50, verbose = FALSE)
      bic_values <- c(bic_values, lca_model$bic)
      cat(sprintf("Classes: %d, BIC: %.2f\n", i, lca_model$bic))
    }, error = function(e) {
      cat(sprintf("Error fitting model with %d classes: %s\n", i, e$message))
    })
  }
  
  # Fit final model with optimal number of classes (6 based on original code)
  final_model <- poLCA(f, opinion_data, nclass = 6, nrep = 50)
  
  return(list(
    bic_values = bic_values,
    final_model = final_model
  ))
}

# Note: This example requires a custom dataset (opinion_data) that is not provided
# To run this analysis, you would need to load your opinion data first:
# remote_work_results <- execute_remote_work_lca(opinion_data)

# =============================================================================
# MAIN EXECUTION SECTION
# =============================================================================

# Function to run all analyses with available data
run_all_analyses <- function() {
  cat("\n")
  cat("===============================================\n")
  cat("   HIDDEN WORLD LATENT VARIABLE ANALYSIS      \n")
  cat("===============================================\n")
  cat("\n")
  
  # Run EFA analysis
  cat("Running Exploratory Factor Analysis...\n")
  efa_results <- execute_efa_analysis()
  
  cat("\n")
  
  # Run CFA analysis
  cat("Running Confirmatory Factor Analysis...\n")
  cfa_results <- execute_cfa_analysis()
  
  cat("\n")
  
  # Run LCA analysis
  cat("Running Latent Class Analysis...\n")
  lca_results <- execute_lca_analysis()
  
  cat("\n")
  
  # Run basic SEM analysis
  cat("Running Structural Equation Modeling...\n")
  sem_results <- execute_basic_sem()
  
  cat("\n")
  cat("===============================================\n")
  cat("   ALL ANALYSES COMPLETED                     \n")
  cat("===============================================\n")
  
  # Return all results
  return(list(
    efa = efa_results,
    cfa = cfa_results,
    lca = lca_results,
    sem = sem_results
  ))
}

# Example of how to run analyses individually:
# efa_results <- execute_efa_analysis()
# cfa_results <- execute_cfa_analysis()
# lca_results <- execute_lca_analysis()
# sem_results <- execute_basic_sem()

# Example of how to run additional analyses with custom data:
# social_capital_results <- execute_social_capital_sem(my_data)
# telework_results <- execute_telework_satisfaction_sem(telework_data)
# remote_work_results <- execute_remote_work_lca(opinion_data)

# Uncomment the following line to run all analyses:
# all_results <- run_all_analyses()

