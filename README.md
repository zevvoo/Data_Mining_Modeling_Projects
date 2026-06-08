# Data Mining Projects

This repository contains two compact data mining projects:

1. 2026 FIFA World Cup ranking prediction
2. Microbiome-based multiclass disease prediction

The repository focuses on reproducible code. Raw datasets, generated reports, prediction result files, spreadsheets, and large model binaries are intentionally excluded.

## Repository Structure

```text
.
├── fifa2026_project/
│   └── scripts/
│       ├── 01_fifa_ranking_webcrawling.R
│       ├── 02_mining_prepare.R
│       ├── 03_preprocess_features.R
│       └── 04_model_predict.R
│
└── microbiome_disease_project/
    ├── scripts/
    │   └── 01_train_predict.R
    └── webapp/
        ├── README.md
        └── app.R
```

## Project 1: FIFA World Cup Ranking Prediction

This project predicts the expected final ranking of countries participating in the 2026 FIFA World Cup.

### Workflow

1. Crawl FIFA men's world ranking data.
2. Prepare ranking histories for 2026 participating countries.
3. Build historical World Cup training and test datasets.
4. Create ranking-based features such as pre-tournament rank, ranking points, recent rank changes, and recent average rank.
5. Compare regression models using the 2022 World Cup as a test split.
6. Predict final rankings for the 2026 tournament participants.

### Main Scripts

```text
fifa2026_project/scripts/01_fifa_ranking_webcrawling.R
```

Crawls FIFA ranking data from the official FIFA ranking page.

```text
fifa2026_project/scripts/02_mining_prepare.R
```

Prepares the 2026 participant list and extracts FIFA ranking histories.

```text
fifa2026_project/scripts/03_preprocess_features.R
```

Creates historical train/test features and 2026 prediction features.

```text
fifa2026_project/scripts/04_model_predict.R
```

Trains and compares models, then produces final 2026 ranking predictions.

## Project 2: Microbiome Disease Prediction

This project predicts disease status from microbiome feature profiles across 23 classes.

### Workflow

1. Load labeled microbiome training data and unlabeled test data.
2. Validate train/test feature alignment.
3. Remove near-zero variance features.
4. Select informative microbiome features using class-wise ANOVA-style scores.
5. Train an R-based ensemble of multinomial logistic regression and k-nearest neighbors.
6. Predict disease labels for test samples.
7. Serve predictions through a simple Shiny upload interface.

### Main Scripts

```text
microbiome_disease_project/scripts/01_train_predict.R
```

Runs preprocessing, model comparison, final training, and prediction generation.

```text
microbiome_disease_project/webapp/app.R
```

Runs a local Shiny web service that accepts TSV uploads and returns predicted disease labels.

## Requirements

### R Packages

The FIFA project uses R packages such as:

```text
RSelenium
rvest
dplyr
stringr
purrr
data.table
MASS
rpart
nnet
class
shiny
```

## Data and Model Files

Raw input data and generated outputs are not included in this repository.

To reproduce the full workflows, place the required input files in the expected input folders or update the path constants in each script.

For the microbiome modeling script, place the input files here:

```text
microbiome_disease_project/input_data/Q2_train.tsv
microbiome_disease_project/input_data/Q2_test.tsv
```

For the microbiome web service, a trained model file is required:

```text
microbiome_disease_project/model/microbiome_disease_model_R.rds
```

The Shiny app also accepts the model file inside `microbiome_disease_project/webapp/`.
Because this model file can be large, it is recommended to distribute it separately, for example through a GitHub Release asset or external storage link.

## Uploading This Project to GitHub

1. Create a new empty repository on GitHub.

2. Move to this repository folder:

```bash
cd "/path/to/Data_Mining_Modeling_Projects"
```

3. Initialize Git:

```bash
git init
```

4. Check the files:

```bash
git status
```

5. Add files:

```bash
git add README.md .gitignore fifa2026_project microbiome_disease_project
```

6. Commit:

```bash
git commit -m "Add data mining projects"
```

7. Connect to your GitHub repository:

```bash
git remote add origin https://github.com/YOUR_USERNAME/YOUR_REPOSITORY.git
```

8. Push:

```bash
git branch -M main
git push -u origin main
```

Before pushing, run `git status` and confirm that raw data files, reports, spreadsheets, and large model files are not staged.
