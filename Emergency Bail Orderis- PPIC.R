# -------------------------------------------------------------------
# PPIC-style Triple Differences (DDD) Event Study — Synthetic dataset
# Accurate statewide dates; real treatment counties; PPIC-like spec.
# -------------------------------------------------------------------

# ===========================
# ZERO-BAIL DDD EVENT STUDY (Synthetic)
# - Accurate statewide dates (Apr 13, 2020 -> Jun 20, 2020)
# - Treatment counties from PPIC list
# - Use global week FE (not interacted), so event-time terms are identified
# ===========================

set.seed(123456)  # reproducibility for random draws

# Let's begin!

# ---- Packages (quietly load) ----
suppressPackageStartupMessages({
  library(dplyr)     # data wrangling
  library(tidyr)     # crossing(), expand grids
  library(lubridate) # dates & months
  library(fixest)    # FEOLS and i() for event-study terms (Trip D analyses)
  library(ggplot2)   # plotting
  library(broom)     # tidy model outputs
})

# ---- California county list (58 total) ----
all_ca <- c(
  "Alameda","Alpine","Amador","Butte","Calaveras","Colusa","Contra Costa","Del Norte",
  "El Dorado","Fresno","Glenn","Humboldt","Imperial","Inyo","Kern","Kings","Lake","Lassen",
  "Los Angeles","Madera","Marin","Mariposa","Mendocino","Merced","Modoc","Mono","Monterey",
  "Napa","Nevada","Orange","Placer","Plumas","Riverside","Sacramento","San Benito",
  "San Bernardino","San Diego","San Francisco","San Joaquin","San Luis Obispo","San Mateo",
  "Santa Barbara","Santa Clara","Santa Cruz","Shasta","Sierra","Siskiyou","Solano","Sonoma",
  "Stanislaus","Sutter","Tehama","Trinity","Tulare","Tuolumne","Ventura","Yolo","Yuba"
)

# ---- Treatment counties (from PPIC report) ----
treat_ctys <- c(
  "Alameda","Butte","Calaveras","Contra Costa","Fresno","Kings","Lake","Lassen",
  "Los Angeles","Marin","Mendocino","Merced","Napa","Nevada","Plumas","Riverside",
  "San Benito","San Bernardino","San Francisco","San Luis Obispo","Santa Clara",
  "Santa Cruz","Sierra","Siskiyou","Solano","Tuolumne","Yuba"
)

# ---- Policy timing (verified statewide dates) ----
statewide_start  <- as.Date("2020-04-13")  # emergency zero-bail effective
statewide_end    <- as.Date("2020-06-20")  # rescinded effective

# Placeholder continuation window for treated counties after rescission
# (Replace per county later with their true end dates if desired.)
treated_placeholder_end <- as.Date("2021-06-01")

# ---- Build panel: county × week (2018–2023) ----
weeks <- seq(from = as.Date("2018-01-05"), to = as.Date("2023-09-29"), by = "week")

panel <- crossing(county = all_ca, week = weeks) %>%
  arrange(county, week) %>%
  mutate(
    week_id = as.integer(factor(week)),     # numeric index for week FE
    treat   = county %in% treat_ctys        # treatment membership
  )

# ---- Define when zero-bail is in place ----
# - Statewide applies to ALL counties 4/13/2020–6/20/2020
# - After 6/20/2020, only treated counties continue (placeholder end)
panel <- panel %>%
  mutate(
    zbo_statewide = as.integer(week >= statewide_start & week < statewide_end),
    zbo_cont      = as.integer(treat & week >= statewide_end & week < treated_placeholder_end),
    zbo_in_place  = pmin(1L, zbo_statewide + zbo_cont),  # 1 if any rule applies
    post          = as.integer(week >= statewide_start)   # post indicator for info/debug
  )

# ---- Triple-diff groups and controls ----
# Third difference is ZB-eligible vs non-ZB; also include offense category, race, gender
offense_cats <- c("violent","property","drugs","other")
races        <- c("White","Black","Hispanic","Asian/PI","Other")
genders      <- c("Male","Female")

# Expand to county×week×(ZB, offense_cat, race, gender)
panel_g <- panel %>%
  crossing(
    zb_eligible = 0:1,
    offense_cat = offense_cats,
    race        = races,
    gender      = genders
  ) %>%
  arrange(county, week, zb_eligible, offense_cat, race, gender) %>%
  mutate(
    # Event time: weeks since statewide start
    event_time = as.integer((week - statewide_start)/7),
    et         = pmax(pmin(event_time, 40), -40)  # cap to keep the plot compact
  )

# We’re simulating data, so we need a simple “story” for how arrests and rearrests happen:
# ---- Simulation helpers (seasonality, pandemic dip, base intensities) ----
season <- function(d) { m <- month(d); if (m %in% c(6,7,8)) 1.10 else if (m %in% c(1,2)) 0.92 else 1 }
pandemic <- function(d){ if (d >= as.Date("2020-03-15") & d <= as.Date("2020-06-30")) 0.6 else 1 }

# Base weekly arrest volume by ZB × offense type "On average, how many people do we expect to arrest each week in this group?”
base_lambda <- function(zb, cat){
  if(zb==1 && cat=="violent")  35 else  # ZB--- > Zero Bail & cat ---> Category
  if(zb==1 && cat=="property") 80 else
  if(zb==1 && cat=="drugs")    70 else
  if(zb==0 && cat=="violent")  30 else
  if(zb==0 && cat=="property") 65 else
  if(zb==0 && cat=="drugs")    55 else 40
}

# Base rearrest probability (pre-policy) by ZB × offense type
base_p <- function(zb, cat){
  if(zb==1 && cat=="violent")  0.075 else
  if(zb==1 && cat=="property") 0.110 else
  if(zb==1 && cat=="drugs")    0.130 else
  if(zb==0 && cat=="violent")  0.060 else
  if(zb==0 && cat=="property") 0.090 else
  if(zb==0 && cat=="drugs")    0.100 else 0.080
}

# Small gender adjustments to the baseline probability
gender_bump <- function(g){
  if (g=="Male") 0.010 else if (g=="Female") -0.003 else 0.000
}

# Event-study policy bump (only for ZB-eligible; stronger early; small persistence)
event_bump <- function(et, zb, cat){
  if(is.na(et)) return(0)
  if(zb==1){
    if(et >= 0 && et <= 10){
      if(cat=="violent") 0.025 else 0.015
    } else if(et > 10 && et <= 52){
      if(cat=="violent") 0.010 else 0.004
    } else 0
  } else 0
}

# ---- Simulate arrests and 30-day rearrests at full granularity ----
# We're going to make a pretend/fake world ("sim") from the table "panel_g".
# We do it one row at a time because we roll dice (random numbers) per row.
sim <- panel_g %>%
  rowwise() %>%
  mutate(
    # 1) HOW MANY ARRESTS DO WE EXPECT?
    # Start with a base level, then wiggle it for seasons (like summer vs winter),
    # and make it dip during the pandemic time.
    expected_arrests =
      base_lambda(zb_eligible, offense_cat) *   # base level for this kind of person & offense
      season(week) *                            # up/down because of the time of year
      pandemic(week),                           # down because of the pandemic
    
    # Now we actually draw the number of arrests from a Poisson "dice throw".
    # We never let the expectation go below 1 so the dice isn’t super weird.
    arrests = rpois(1, lambda = pmax(expected_arrests, 1)),
    
    # 2) WHAT'S THE CHANCE OF A REARREST?
    # Start with a base chance, nudge it a bit for gender,
    # and only add the "policy nudge" when zero-bail is turned on.
    base_prob =
      base_p(zb_eligible, offense_cat) +        # base chance for this group
      gender_bump(gender),                      # tiny bump for gender
    
    policy_nudge = if_else(
      zbo_in_place == 1,                        # only when zero-bail is in place
      event_bump(event_time, zb_eligible, offense_cat),
      0
    ),
    
    # Add the pieces together to get the final probability.
    # Then we "keep it in bounds" between 0.1% and 70% so it stays sensible.
    prob = base_prob + policy_nudge,
    prob = pmax(pmin(prob, 0.7), 0.001),
    
    # 3) NOW WE ROLL FOR REARRESTS AND CALCULATE THE RATE
    # We flip "arrests" many coins, each with chance "prob".
    rearrests = rbinom(1, size = arrests, prob = prob),
    
    # If there were any arrests, the rate is rearrests / arrests,
    # otherwise it's NA (no rate when there are zero).
    rate = if_else(arrests > 0, rearrests / arrests, NA_real_)
  ) %>%
  ungroup()


gc()

# ---- Event-study Triple-D model ----
# Keep county FE interacted with ZB, but global week FE (NOT interacted),
# so the i(et, zb_eligible) series is not collinear with week FE.
m_ddd <- feols(
  rate ~ zb_eligible +
    i(et, ref = -5) +                  # event-time main series (normalized at -5)
    i(et, zb_eligible, ref = -5) +     # the key dynamic DDD terms (ZB × ET)
    i(race, zb_eligible) +             # ZB-interacted controls
    i(offense_cat, zb_eligible) +
    i(gender, zb_eligible) |
    county[zb_eligible] + week_id,     # FE: county×ZB and global week FE
  data    = sim,
  weights = ~ arrests,                 # weight by exposure (arrests)
  cluster = ~ county                   # cluster at county (treatment) level
)
### A note on the weight module
# We weight the regression by the number of arrests in each county-week-offense-race-gender cell.
# This follows the PPIC report’s approach and improves efficiency when modeling rates.
# Cells with more arrests produce more precise rearrest rate estimates than cells with very few arrests.
# Without weighting, small, noisy cells could distort results by contributing equally to large, precise cells.
# Weighting ensures the estimated effects reflect the population in proportion to its size, 
# giving more influence to precise observations and aligning the analysis with real-world sample structure.

gc()

# ---- Build coefficient table for the interaction series (ZB × event-time) ----
tn <- broom::tidy(m_ddd)                 # get tidy coefficients
unique(tn$term)                          # (peek at names if needed)

# Pull β(et × ZB) and parse the 'et' value out of the term label
coefs <- tn |>
  dplyr::filter(grepl("et::[-0-9]+:zb_eligible$", term)) |>
  dplyr::mutate(
    et     = as.integer(sub(".*et::(-?\\d+):zb_eligible$", "\\1", term)),
    ci_low = estimate - 1.96*std.error,
    ci_high= estimate + 1.96*std.error
  ) |>
  dplyr::arrange(et)

# ---- Quick default plot (sanity check) ----
ggplot(coefs, aes(et, estimate)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = .25) +
  labs(title="DDD Event Study: ZB × Event Time (ref = -5)",
       x="Weeks since statewide start (Apr 13, 2020)",
       y="Effect on rearrest rate") +
  theme_minimal()

# ---- Compute nice y-limits based on CI range ----
y_min <- min(coefs$ci_low,  na.rm = TRUE)
y_max <- max(coefs$ci_high, na.rm = TRUE)
pad   <- 0.1 * (y_max - y_min)
y_limits <- c(y_min - pad, y_max + pad)

# ---- Publication-style plot: cleaner axes; orange CIs; captioned spec ----
ggplot(coefs, aes(x = et, y = estimate)) +
  # reference lines
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey40") +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.4, color = "grey40") +
  # CIs (orange)
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.4, linewidth = 0.6, color = "orange") +
  # points
  geom_point(size = 1.8, shape = 16) +
  # x-axis ticks every 10 weeks; y-axis from CI range
  scale_x_continuous(breaks = seq(-40, 40, by = 10)) +
  coord_cartesian(ylim = y_limits) +
  labs(
    title    = "DDD Event Study: ZB × Event Time (ref = -5 weeks)",
    subtitle = "Effect on rearrest rate; vertical line = statewide start (Apr 13, 2020)",
    caption  = "Model: FEOLS with county[ZB] + week_id fixed effects; weighted by arrests; clustered by county.\nSynthetic data reflecting PPIC-style triple-difference setup.",
    x        = "Weeks since statewide start (Apr 13, 2020)",
    y        = "Effect on rearrest rate"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    plot.caption  = element_text(hjust = 0, size = 9, color = "grey40")
  )

# ---- Save if needed ----
# ggsave("ddd_event_study_clean.png", width = 8, height = 4.5, dpi = 300)

gc()

# ---- PPIC-style MONTHLY event-study (ref = -1 month) --------------------------

# 1) Month-relative event time: round(days / 30.44)
sim <- sim |>
  mutate(
    et_month_raw = as.numeric(difftime(week, statewide_start, units = "days")) / 30.44,
    et_m         = as.integer(round(et_month_raw)),
    et_m         = pmax(pmin(et_m, 11), -6)   # keep range like the PPIC figure
  )

# 2) Refit the DDD with months & ref = -1 (30 days prior)
m_ddd_m <- feols(
  rate ~ zb_eligible +
    i(et_m, ref = -1) +                      # monthly main series, ref = -1
    i(et_m, zb_eligible, ref = -1) +         # monthly interaction series, ref = -1
    i(race, zb_eligible) +
    i(offense_cat, zb_eligible) +
    i(gender, zb_eligible) |
    county[zb_eligible] + week_id,           # global week FE; county×ZB FE
  data    = sim,
  weights = ~ arrests,
  cluster = ~ county
);summary(m_ddd_m)

# 3) Pull β for the interaction series (ZB × month event-time) and build CIs
coefs_m <- broom::tidy(m_ddd_m) |>
  dplyr::filter(grepl("et_m::[-0-9]+:zb_eligible$", term)) |>
  dplyr::mutate(
    month  = as.integer(sub(".*et_m::(-?\\d+):zb_eligible$", "\\1", term)),
    est_pp = 100 * estimate,                      # percentage points
    ci_lo  = 100 * (estimate - 1.96 * std.error),
    ci_hi  = 100 * (estimate + 1.96 * std.error)
  ) |>
  dplyr::arrange(month)

# 4) Nice y-lims from CIs
{
  y_min_pp <- min(coefs_m$ci_lo, na.rm = TRUE)
  y_max_pp <- max(coefs_m$ci_hi, na.rm = TRUE)
  pad_pp   <- 0.1 * (y_max_pp - y_min_pp)
  y_lim_pp <- c(y_min_pp - pad_pp, y_max_pp + pad_pp)
}

# 5) PPIC-style monthly plot
ggplot(coefs_m, aes(x = month, y = est_pp)) +
  # grid & reference
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey40") +
  geom_vline(xintercept = 0,  linewidth = 0.7, color = "#C71E1D") +      # solid red at 0
  geom_vline(xintercept = -1, linetype = 2, linewidth = 0.7, color = "#C71E1D") +  # dashed red at -1
  # orange CIs and points
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                width = 0.35, linewidth = 0.8, color = "orange") +
  geom_point(size = 2.2, shape = 16, color = "orange4") +
  # axes & labels
  scale_x_continuous(breaks = seq(-6, 11, by = 1)) +
  coord_cartesian(ylim = y_lim_pp) +
  labs(
    title    = "Under emergency bail orders, differences in rearrests for ZB offenses subside after a few months",
    subtitle = "Difference in likelihood of rearrest within 30 days, ZB vs. non-ZB (percentage points)\nEvent time in months; solid line = implementation (0), dashed = 30 days before (−1)",
    x        = "Month relative to implementation",
    y        = "Difference in likelihood of rearrest (pp)",
    caption  = paste(
      "SOURCE: Synthetic data; model estimated with FEOLS (county[ZB] + week fixed effects),",
      "weighted by arrests; SEs clustered by county.",
      "NOTES: Figure mirrors PPIC’s monthly event-study design with ref = −1.",
      sep = " "
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    plot.caption  = element_text(hjust = 0, size = 9, color = "grey40")
  )

# Optional save
# ggsave("ddd_event_study_monthly_ppic_style.png", width = 8.5, height = 5.2, dpi = 300)
