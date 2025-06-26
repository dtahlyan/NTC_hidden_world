# NTC Hidden World

Welcome to the **NTC Hidden World** project! This repository explores hidden patterns and insights in the NTC dataset using data analysis and visualization techniques.

## Project Overview

This project aims to uncover and present interesting findings from the NTC dataset. The workflow includes data cleaning, exploratory data analysis, and the creation of interactive visualizations.

## Directory Structure

```
.
├── data/
│   └── ntc_data.csv
├── src/
│   ├── analysis.py
│   ├── visualization.py
│   └── utils.py
├── presentation.qmd
└── README.md
```

- **data/**: Contains the raw NTC dataset.
- **src/**: Source code for data analysis and visualization.
    - `analysis.py`: Data cleaning and feature engineering.
    - `visualization.py`: Functions for generating plots.
    - `utils.py`: Helper functions.
- **presentation.qmd**: Quarto markdown presentation summarizing the project and results.

## Getting Started

1. Clone the repository:
     ```bash
     git clone https://github.com/yourusername/NTC_hidden_world.git
     cd NTC_hidden_world
     ```
2. Install dependencies:
     ```bash
     pip install -r requirements.txt
     ```
3. Run the analysis:
     ```bash
     python src/analysis.py
     ```

## Results

Key findings and visualizations are presented in [`presentation.qmd`](./presentation.qmd). Open it with Quarto or view the rendered HTML for a summary of insights.

## License

This project is licensed under the MIT License.

---

*For more details, see the code in the `src/` directory and the presentation in `presentation.qmd`.*