# Hidden World of Latent Variables

Welcome to the **Hidden World of Latent Variables** project! This repository provides a comprehensive tutorial on latent variable modeling techniques in R, including Exploratory Factor Analysis (EFA), Confirmatory Factor Analysis (CFA), Structural Equation Modeling (SEM), and Latent Class Analysis (LCA).

## Project Overview

This project demonstrates various latent variable modeling techniques through practical examples and interactive presentations. Originally developed for the Northwestern University Transportation Center (NTC), it serves as an educational resource for understanding and applying latent variable methods in research.

## Repository Contents

```
.
├── code.R                                    # Main R script with all latent variable analyses
├── Hidden_World_Latent_Variable.qmd         # Quarto presentation source
├── Hidden_World_Latent_Variable.html        # Rendered presentation
├── Hidden_World_Latent_Variable_files/      # Supporting files for presentation
├── README.md                                # This file
├── title_image.jpg                          # Title slide background image
├── big5.png                                 # Big Five personality visualization
├── LCA_plot.png                             # Latent Class Analysis plot
├── LCA_plot.emf                             # LCA plot (EMF format)
├── LV_setup.png                             # Latent variable setup diagram
├── social_capital.png                       # Social capital model visualization
└── telework_satisfaction.png                # Telework satisfaction model plot
```

### Key Files

- **`code.R`**: Complete R implementation of latent variable modeling techniques including:
  - Exploratory Factor Analysis (EFA) using the Big Five personality dataset
  - Confirmatory Factor Analysis (CFA) with demographic covariates
  - Latent Class Analysis (LCA) with cheating behavior data
  - Structural Equation Modeling (SEM) examples
  - Additional examples for social capital and telework satisfaction models

- **`Hidden_World_Latent_Variable.qmd`**: Quarto presentation that explains:
  - What latent variables are and why they matter
  - Different types of latent variable models
  - Practical applications across various fields
  - Step-by-step analysis workflows

- **Visualization Files**: PNG and EMF images showing model results and conceptual diagrams

## Getting Started

### Prerequisites

This project requires R and the following packages:
- `psych` - Exploratory Factor Analysis
- `lavaan` - Confirmatory Factor Analysis and SEM
- `poLCA` - Latent Class Analysis
- `GPArotation` - Factor rotation methods
- `corrplot` - Correlation plot visualization
- `ggplot2` - Advanced plotting
- `parameters` - Parameter processing

### Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/dtahlyan/NTC_hidden_world.git
   cd NTC_hidden_world
   ```

2. Install required R packages:
   ```r
   install.packages(c("psych", "lavaan", "poLCA", "GPArotation", 
                      "corrplot", "ggplot2", "parameters"))
   ```

3. Load and run the analyses:
   ```r
   source("code.R")
   # Run all analyses
   all_results <- run_all_analyses()
   ```

### Usage

The R script is organized into modular functions for different analysis types:

- **EFA**: `execute_efa_analysis()` - Exploratory factor analysis on Big Five data
- **CFA**: `execute_cfa_analysis()` - Confirmatory factor analysis with covariates  
- **LCA**: `execute_lca_analysis()` - Latent class analysis on cheating behavior
- **SEM**: `execute_basic_sem()` - Structural equation modeling example

You can run individual analyses or use `run_all_analyses()` to execute everything.

## Presentation

The interactive presentation is available in multiple formats:

- **Online**: View the [rendered HTML presentation](./Hidden_World_Latent_Variable.html)
- **Source**: Edit the [Quarto markdown file](./Hidden_World_Latent_Variable.qmd)
- **Live**: Open with Quarto and present interactively

The presentation covers:
- Introduction to latent variables and their applications
- Step-by-step walkthrough of different modeling approaches
- Practical examples with real datasets
- Interpretation of results and model fit assessment

## Techniques Covered

### 1. Exploratory Factor Analysis (EFA)
- Correlation analysis and visualization
- Sampling adequacy tests (KMO, Bartlett)
- Factor extraction and rotation
- Scree plots and eigenvalue analysis

### 2. Confirmatory Factor Analysis (CFA)
- Model specification with latent factors
- Estimation with ordinal indicators
- Fit assessment and model modification
- Inclusion of covariates and demographic variables

### 3. Latent Class Analysis (LCA)
- Unconditional class enumeration
- Model selection using information criteria
- Conditional models with covariates
- Class probability analysis

### 4. Structural Equation Modeling (SEM)
- Combined measurement and structural models
- Path analysis and mediation
- Multi-group analysis capabilities
- Advanced model specifications

## Example Datasets

The tutorial uses several built-in R datasets:
- **Big Five Personality (bfi)**: 25 personality items across 5 factors
- **Cheating Behavior**: 4 academic dishonesty indicators
- **Holzinger-Swineford**: Classic cognitive ability dataset

Additional examples show applications to:
- Social capital measurement
- Telework satisfaction modeling
- Remote work impact assessment

## Author

**Divyakant Tahlyan**  
Northwestern University Transportation Center  
October 12, 2023

## License

This project is licensed under the MIT License.

## Contributing

This educational resource is designed to help researchers and students learn latent variable modeling techniques. Feel free to:
- Submit issues for clarification or improvements
- Suggest additional examples or techniques
- Share your own applications of these methods

---

*For detailed implementation, see the [`code.R`](./code.R) file. For the complete tutorial, view the [`Hidden_World_Latent_Variable.html`](./Hidden_World_Latent_Variable.html) presentation.*