# 🗳️ Multidimensional Visualization of Survey Policy Preferences

## 1. Overview
This repository contains the R code, processed data, and visualization scripts for the project:

> *Add your project introduction here — describe the goal, data sources, and what insights your visualization framework provides.*

---

## 2. How to Run the Project

### Step 1 — Set up the R environment
Open **PowerShell** or **Git Bash** inside the project folder and run:

```
Rscript --% -e "install.packages('renv', repos='https://cloud.r-project.org')"
Rscript --% -e "renv::restore()"
```

This installs all required R packages listed in `renv.lock`.

---

### Step 2 — Download and process data
Raw data are publicly available.  
See `data/README.md` for download links and details.

To reproduce everything from scratch, run:

```
Rscript scripts/01_download_raw.R
Rscript scripts/02_process.R
```

Or skip directly to plotting using the processed data:

```
Rscript scripts/03_make_figures.R
```

To run the entire workflow (download → process → plot):

```
Rscript scripts/04_reproduce_all.R
```

All generated figures will be saved to the `figs/` folder.

---

## 3. Project Highlights

> *Add brief text describing key results or visualization findings here.*

Below are examples of visualization outputs:

| Figure | Description |
|---------|--------------|
| ![Main heatmap](figs/fig1_main.png) | **Main 2D Policy Heatmap.** Joint distribution of two policy domains (e.g., Gun Control vs. Environment). |
| ![Partisan breakdown](figs/fig2_partisan.png) | **Partisan Breakdown.** Heatmaps by political affiliation (Democrat, Republican, Independent). |
| ![Timeline visualization](figs/fig3_timeline.png) | **Temporal Evolution.** Year-by-year visualization of shifts in policy alignment. |

For more figures and explanations, see the `docs/` folder.

---

## 4. Data Sources
- **Public Survey Dataset** (e.g., CES 2014–2022 cumulative data):  
  [https://cces.govlink.org/](https://cces.govlink.org/)
- **Policy Table Data**:  
  [https://projectsource.org/](https://projectsource.org/)

See `data/README.md` for details on downloading and variable definitions.

---

## 5. Citation
If you use this code or visualization framework, please cite:

```
@misc{yourname2025surveyVis,
  author = {Your Name},
  title  = {Multidimensional Visualization of Survey Policy Preferences},
  year   = {2025},
  url    = {https://github.com/<yourname>/survey_MultiDimVis}
}
```

---

## 6. Contact
For questions or collaboration:
- **Author:** [Your Name]  
- **Email:** [your_email@domain.edu]  
- **Institution:** Cedars-Sinai Medical Center

---

### Quick Summary
| Task | Command |
|------|----------|
| Restore environment | `Rscript --% -e "renv::restore()"` |
| Run full pipeline | `Rscript scripts/04_reproduce_all.R` |
| Generate figures only | `Rscript scripts/03_make_figures.R` |
| View outputs | Open the `/figs/` folder |

---

*(Last updated: 2025-11-12)*
