# Triple-Difference (DDD) Event Study — California’s Emergency Zero-Bail Orders (Educational Guide)

**Purpose:** How to implement and interpret a **triple-difference (DDD)** event study in R using a policy case: California’s emergency **zero-bail** orders during COVID-19.  
**Data:** fully **synthetic** (safe to share), structured to mirror the approach in PPIC’s report *What Happened When California Suspended Bail during COVID?*

> ⚠️ This repository is **instructional**. Effect sizes reflect synthetic data and will differ from estimates using confidential California DOJ ACHS data.

## 📚 Cite

If you use this repository, code, or figures in your own work, please cite as follows:

> Nouri, S. (2025). *Illustrating Triple-Difference Causal Models with Synthetic Data*. GitHub repository. Retrieved from https://github.com/yourusername/your-repo-name
---

## Table of Contents
- [Overview](#overview)
- [What is Triple Difference (DDD)?](#what-is-triple-difference-ddd)
- [Policy Background](#policy-background)
- [Data & Design](#data--design)
- [Methods](#methods)
- [Quick Start](#quick-start)
- [Results (Monthly DDD)](#results-monthly-ddd)
- [Reproduce the Results Table](#reproduce-the-results-table)
- [How to Adapt This Template](#how-to-adapt-this-template)
- [Repo Layout](#repo-layout)
- [References](#references)
- [License](#license)

---

# 📋 Overview

In April 2020, California’s Judicial Council implemented an **emergency zero-bail** rule (most misdemeanors & some felonies at $0 bail) to reduce jail populations during COVID-19, then rescinded it in June 2020. Many county courts kept **emergency schedules** beyond June.

This repo shows how to evaluate such a policy with a **DDD event study** in R:
- Builds a **county × week** panel (2018–2023) with offense/race/gender groups
- Assigns **treated counties** (those that extended zero-bail)
- Fits a **DDD event-time model** with fixed effects & clustered SEs
- Produces **weekly** and **monthly** PPIC-style figures

---


## What is a Triple-Difference (DDD)?

A **Difference-in-Differences (DID)** model compares outcomes **before vs after** a policy change for **treated** and **control** groups.  
However, DID can still be biased if **other factors** change differently across groups during the same period (e.g., unrelated local events, economic shocks, or broad societal changes).

A **Triple-Difference (DDD)** model adds a **third layer of differencing**, which allows us to control for those extra confounding changes by introducing an **additional comparison dimension** inside both the treated and control groups.

---

**How it works in this project:**

1. **First difference:**  
   *Before vs after* the implementation of the zero-bail policy.  
   - Measures how outcomes change over time.

2. **Second difference:**  
   *Treated counties* (that implemented/extended zero-bail) **vs** *control counties* (that did not).  
   - Captures how policy-affected places differ from unaffected places.

3. **Third difference:**  
   *Offenses eligible for zero-bail* **vs** *offenses not eligible for zero-bail* within each county.  
   - Controls for broader changes that affect **all offenses** equally in a county (e.g., policing strategies, local shocks).

---

**Why add the third difference?**

- **Removes bias from unrelated shifts:**  
  Even if *treated counties* had crime trends that differed from *control counties* (due to the pandemic, seasonal crime cycles, or unrelated reforms), those shifts would affect both eligible and ineligible offenses. By subtracting away the ineligible-offense trends, I isolate the policy’s specific effect.
  
- **Improves causal inference:**  
  We’re essentially controlling for:
  - Time effects (seasonality, COVID disruptions).
  - Location-specific effects (county-level economic changes, local policing).
  - Offense-type-specific effects (statewide changes in enforcement for certain crimes).

- **Cleaner estimates:**  
  The DDD estimate answers the question:  
  > *“How much did zero-bail change outcomes for eligible offenses, above and beyond any changes I would expect from ineligible offenses in the same counties over the same time period?”*

---  

**Key assumption for validity:**  
Without the zero-bail policy, the **gap** between eligible and ineligible offenses in treated counties would have followed the same trajectory as the gap in control counties. This is the **parallel trends assumption** — extended to a third dimension.

---

---

**Implementation note:**  
When estimating Triple-Difference models in practice, I recommend using the **[fixest](https://cran.r-project.org/package=fixest)** package in R.  
It efficiently handles high-dimensional fixed effects and is well-suited for large datasets, making it ideal for complex specifications like DDD.  

⚠️ **Caution:** Using classical models such as `glm()` on large datasets with many fixed effects may be computationally expensive or may fail to converge.  
`fixest::feols()` or `fixest::feglm()` can provide much faster and more reliable estimation.`fixest::feglm()` is great for clustering cases while `fixest::feols()` 
for adjusting clustered standard errors.

---

## Policy Background
- **Statewide zero-bail in effect:** **Apr 13, 2020 → Jun 20, 2020**  
- **Continuation:** 27 counties extended some form of emergency zero-bail beyond June 2020 (treated group in this tutorial).  
- Original analysis: PPIC’s report *What Happened When California Suspended Bail during COVID?* (2024) and its technical appendix.

---

## Data & Design
- **Synthetic data only** (no sensitive records). I used R to generate fake data.  
- 58 counties × weekly dates (2018–2023) × offense category (violent/property/drugs/other) × race × gender × ZB eligibility.  
- Seasonality & a short **pandemic dip** are baked in to make series realistic.  
- **Treatment counties** (27): Alameda, Butte, Calaveras, Contra Costa, Fresno, Kings, Lake, Lassen, Los Angeles, Marin, Mendocino, Merced, Napa, Nevada, Plumas, Riverside, San Benito, San Bernardino, San Francisco, San Luis Obispo, Santa Clara, Santa Cruz, Sierra, Siskiyou, Solano, Tuolumne, Yuba.

---

## Methods
---

## 📊 Using `feols()` for Triple-Difference (DDD) Models

Below is a breakdown of the key parameters in `feols()` from the **fixest** package.

### **Function Signature**
```r
feols(
  fml,                 # formula (required)
  data,                # data frame (required)
  fixef = NULL,        # alternative fixed effects specification
  weights = NULL,      # optional weights
  subset = NULL,       # filter rows before estimation
  cluster = NULL,      # clustering for standard errors
  se = NULL,           # type of standard errors
  ssc = NULL,          # small-sample correction
  dof = NULL,          # degrees of freedom adjustment
  family = NULL,       # only for feglm()
  split = NULL,        # run same model on subsets
  na.rm = TRUE,        # drop NAs automatically
  env = parent.frame() # environment for evaluation
)
```

### **Key Parameters**

1. **`fml` (Formula)** – required  
   Example for DDD:  
   ```r
   outcome ~ post * treat * eligible | county + week + offense_type
   ```
   - Before `|`: covariates and triple interaction.
   - After `|`: fixed effects.

2. **`data`** – required  
   - Data frame containing all variables.

3. **`fixef`** – optional  
   Alternative FE specification:  
   ```r
   fixef = c("county", "week", "offense_type")
   ```

4. **`weights`** – optional  
   - Numeric vector or column name for weighted regression.

5. **`subset`** – optional  
   - Filter rows before estimation.

6. **`cluster`** – optional  
   - Clustering variables for SEs:  
     ```r
     cluster = ~county
     cluster = ~county + week
     ```

7. **`se`** – type of standard errors  
   - `"standard"`, `"hetero"`, `"cluster"`, `"twoway"`.

8. **`ssc`** – small-sample correction.

9. **`dof`** – degrees of freedom adjustment.

10. **`family`** – only for `feglm()` (GLMs with FE).  
    - Example: `family = "binomial"`.

11. **`split`** – run same model for subsets:  
    ```r
    split = ~gender
    ```

12. **`na.rm`** – drop NAs automatically (default `TRUE`).

13. **`env`** – advanced; where to evaluate variables.

---

### **Example: Triple-Difference with `feols`**

---


I estimate a **DDD event-study** using `fixest::feols`:


```
rate ~ 
    # 1️⃣ First Difference: Before vs After the Event
    i(et, ref = -5)                                 # Event time relative to reference period

    # 2️⃣ Second Difference: Treated vs Control Counties (Eligibility)
    + zb_eligible                                   # ZB-eligible vs non-eligible offenses

    # 3️⃣ Third Difference: Interaction of Time × Eligibility
    + i(et, zb_eligible, ref = -5)                  # Change over time by eligibility status

    # Additional Third Differences: Eligibility gaps within subgroups
    + i(race, zb_eligible)                          # Race-specific eligibility differences
    + i(offense_cat, zb_eligible)                   # Offense-category-specific eligibility differences
    + i(gender, zb_eligible)                        # Gender-specific eligibility differences

    # Fixed Effects: County-level treatment status + Time
    | county[zb_eligible] + week_id                 # County-by-eligibility FE + week FE


```

- **Outcome:** 30-day **rearrest rate**  
- **et:** weeks since statewide start (Apr 13, 2020); Ialso build a **monthly** version `et_m`  
- **Fixed effects:** `county[zb_eligible]` (varying slopes by eligibility) + **global week FE**  
- **Weights:** arrests; **SEs:** clustered by county  
- The **monthly** model uses `i(et_m, ref = -1)` so coefficients are relative to **30 days before** implementation (matching PPIC figures).

---

## Quick Start
1. **Requirements**: R ≥ 4.2; packages: `fixest`, `dplyr`, `tidyr`, `lubridate`, `ggplot2`, `broom`.  
   ```r
   install.packages(c("fixest","dplyr","tidyr","lubridate","ggplot2","broom"))
   ```
2. **Run the script**: open `code/triple_d_synthetic.R` in R/RStudio and `Source`.  
3. **Outputs**:
   - Weekly event-study plot (optionally save as `weekly_ddd_effects.png`)
   - Monthly PPIC-style plot (save as `monthly_ddd_effects.png`)
   - Printed DDD coefficient tables in the console

> Tip: The script is heavily commented so you can follow each step.

---

## Results (Monthly DDD)

**Model**: FEOLS with `county[ZB] + week` fixed effects; weighted by arrests; SEs clustered by county.  
**Outcome**: 30-day rearrest rate.  
**Interpretation**: Each β(m) is the **difference in likelihood of rearrest** (**percentage points**) between **zero-bail–eligible** and **non-eligible** offenses in month *m*, **relative to −1 month**.

## 📊 Results — Triple-Difference (DDD) Estimates

Below are the estimated monthly effects (**percentage point changes in rearrest rates**) for ZB-eligible offenses in treated counties, relative to the DDD control group.  

> **Reference period:** Month −1 (the month before the policy started)  
> **Reminder:** This dataset is synthetic — magnitudes are illustrative only.  
> In real data, effect sizes and statistical significance may differ.

| Month | Effect (pp) | p-value | Sig. |
|---:|---:|---:|:--:|
| −6 | +0.03 | 0.640 |  |
| −5 | +0.11 | 0.276 |  |
| −4 | +0.04 | 0.644 |  |
| −3 | +0.09 | 0.305 |  |
| −2 | +0.01 | 0.927 |  |
| −1 | **+0.76** | < 0.001 | *** |
| 0 | **+1.69** | < 0.001 | *** |
| 1 | **+1.54** | < 0.001 | *** |
| 2 | **+0.25** | 0.003 | ** |
| 3 | **+0.27** | 0.028 | * |
| 4 | **+0.30** | < 0.001 | *** |
| 5 | **+0.23** | 0.016 | * |
| 6 | **+0.31** | < 0.001 | *** |
| 7 | **+0.24** | 0.0016 | *** |
| 8 | **+0.29** | 0.012 | * |
| 9 | **+0.23** | 0.014 | * |
| 10 | +0.20 | 0.076 | . |
| 11 | +0.06 | 0.458 |  |

> Significance codes: `***` p<0.001, `**` p<0.01, `*` p<0.05, `.` p<0.10  

---

### 🖼 Event Study Plot  

![DDD Monthly Event Study](ddd_monthly_event_study.png)

---

### 🔍 Interpretation (Synthetic Data)

**1. Pre-policy period (Months −6 to −2):**  
- Effects are small and statistically insignificant, suggesting **no strong pre-trends** between treatment and control groups.  
- An exception is **Month −1**, which shows a significant **+0.76 pp** effect — in real data, this would prompt further investigation (e.g., early policy adoption or anticipatory behavior).

**2. Immediate policy impact (Months 0–1):**  
- **Largest jump at policy start**: Month 0 = **+1.69 pp**, Month 1 = **+1.54 pp**.  
- Suggests a **sharp, immediate effect** after the zero-bail emergency order.

**3. Short-term stabilization (Months 2–4):**  
- Effects remain **positive and significant** (+0.25 to +0.30 pp).  
- Indicates that the **policy effect persisted** in the months following implementation.

**4. Medium-term persistence (Months 5–9):**  
- Effects hold steady between +0.23 and +0.31 pp, all significant.  
- Suggests a **sustained elevation** in rearrest rates for the treated, ZB-eligible group.

**5. Return toward baseline (Months 10–11):**  
- Effects taper off: +0.20 pp (p≈0.076) in Month 10, +0.06 pp (ns) in Month 11.  
- Indicates the policy impact **fades roughly a year after introduction**.

---

📌 **Key Takeaway:**  
In this synthetic example, the zero-bail emergency order causes a **large immediate increase in rearrest rates** for ZB-eligible offenses in treated counties, which **persists for several months** before gradually returning to pre-policy levels. This pattern is exactly the type of dynamic that a **Triple-Difference** design is meant to detect.


*Notes:* Orange error bars are 95% CIs; *dashed red* line marks **−1 month**; *solid red* marks **implementation month (0)**.

---

## Reproduce the Results Table

```r

# m_ddd_m is the monthly FEOLS model from the script
broom::tidy(m_ddd_m) %>%
  filter(grepl("et_m::[-0-9]+:zb_eligible$", term)) %>%
  transmute(
    Month       = as.integer(sub(".*et_m::(-?\d+):zb_eligible$", "\1", term)),
    `Effect (pp)` = round(100*estimate, 2),
    `p-value`     = signif(p.value, 3),
    Sig. = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ ".",
      TRUE            ~ ""
    )
  ) %>%
  arrange(Month) %>%
  print(n = 50)
```

---

## How to Adapt This Template
- Swap synthetic outcomes for **real data** (e.g., FBI CDE, NACJD UCR, state open data, court booking data).
- Replace **treatment timing** with real county start/stop dates.
- Change the third dimension (e.g., age group, prior record) for your own DDD.
- For **staggered adoption** (closer to PPIC), compute **county-specific event time** and use interacted week FEs.

---

## Repo Layout
```
/code
  triple_d_synthetic.R        # Full simulation + DDD + plots (weekly + monthly)
/figures
  monthly_ddd_effects.png     # Save here to render in README (optional)
  weekly_ddd_effects.png      # Optional weekly plot
README.md
LICENSE
```

---

## References
- Premkumar, D., Skelton, A., Lofstrom, M., & Cremin, S. (2025). *What Happened When California Suspended Bail during COVID?.* 
- `fixest` package docs: event-study with `i()` terms and multi-way fixed effects  
- Angrist & Pischke (2009). *Mostly Harmless Econometrics* (chapters on DID/DDD)

---

## License
MIT © You — This is educational code and synthetic data; you’re free to adapt and share with attribution.
