# Ecologically-informed Compression through Orthogonal Specialized Autoencoders for Interpretable Classification

## Overview

ECOSAIC is a reproducible workflow for mapping and validating potential benthic habitats. It integrates a dual autoencoder neural network (ECOSAIC model) with Self-Organizing Maps (SOM) and species occurrence data to produce an ecologically validated potential habitat map.

The workflow consists of three sequential scripts and one interactive Shiny application:

| Step | Script/code | Description |
|------|-------------|-------------|
| 1 | `ECOSAIC.ipynb` | Trains the dual autoencoder and generates latent features LE1 and LE2 |
| 2 | `SOM_benthic_habitat_classification.R` | Classifies pixels into benthic habitat clusters using SOM on LE1–LE2 |
| 3 | `app_benthic_habitat_validator.R` | Shiny app for biological validation of habitats using species occurrences |

---

## Repository Structure

```
ECOSAIC/
├── scripts/
│   ├── ECOSAIC.R
│   ├── SOM_benthic_habitat_classification.R
│   └── ECOSAIC_BenthicHabitat_Validator.R
├── README.md
├── REQUIREMENTS.R
├── DATA_DICTIONARY.md
├── LICENSE
└── .gitignore
```

---

## How to Reproduce the Analysis

### Prerequisites

**Note on dependencies:** All required R and Python packages are loaded at the top of each script. If any package is not yet installed on your system, you will need to install it manually before running the script. Alternatively, run `REQUIREMENTS.R` to install all R dependencies at once.

### Step 1 — ECOSAIC: Dual Autoencoder and Latent Feature Extraction (`ECOSAIC.R`)

Trains a dual autoencoder on physico-biological (LE1) and hydrogeomorphological (LE2) environmental variables to obtain compressed latent representations of the seabed environment.

**Key inputs** (set paths at the top of the script):
- Environmental raster layers for the Colombian Pacific

**Key outputs:**
- `LE_DUAL_OPTIMIZED_<timestamp>.csv` — table with coordinates, LE1, LE2, and all input variables

---

### Step 2 — SOM Benthic Habitat Classification (`SOM_benthic_habitat_classification.R`)

Applies a Self-Organizing Map (SOM) to the latent features LE1 and LE2 to classify pixels into discrete benthic habitat clusters. Includes hyperparameter tuning and raster export.

**Key inputs:**
- `LE_DUAL_OPTIMIZED_<timestamp>.csv` (output of Step 1)
- `species_occurrences.csv` — species occurrence records with coordinates

**Key outputs:**
- `ECOSAIC_SOM_BenthicHabitats_ColombianPacific_LE1LE2_4x4_WGS84.tif` — habitat raster (WGS84)
- `ECOSAIC_LatentFeatures_EnvVariables_SOMclass_ColombianPacific.csv` — pixel-level table with LE1, LE2, environmental variables, and cluster assignment
- `species_occurrences_with_cluster.csv` — species occurrences with assigned habitat cluster

---

### Step 3 — Biological Validation App (`app_benthic_habitat_validator.R`)

Interactive Shiny application for exploring dominant species per habitat cluster, computing specificity, fidelity, IndVal, and environmental range overlap against literature values.

**Key inputs** (auto-detected locally or set manually):
- `species_occurrences_with_cluster.csv`
- `ECOSAIC_LatentFeatures_EnvVariables_SOMclass_ColombianPacific.csv`
- `species_environmental_preferences_literature.csv`

Run the app with:
**To launch the app**, open `app_benthic_habitat_validator.R` in RStudio, select the line below, and press **Ctrl + Enter**:

---

## Adapting Paths to Your Local Machine

Each script reads and writes files using absolute paths. Before running, update the path variables at the top of each script to match your local directory structure. Example:

## Citation

If you use this workflow, please cite the associated publication:

> [Authors]. (*in preparation*). ECOSAIC: A deep learning framework for benthic habitat mapping and biological validation in the Colombian Pacific Ocean. *Methods in Ecology and Evolution*.

---

## License



