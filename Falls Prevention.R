



############################################################
# Falls prevention REDCap-style CSV: (1) clean, (2) label,
# (3) descriptives, (4) regression with composite risk outcome
############################################################

# Packages
library(tidyverse)
library(janitor)
library(stringr)
library(forcats)
library(labelled)
library(skimr)
library(gtsummary)

#-----------------------------#
# 0) Read data
#-----------------------------#
# Replace with your file path
raw <- readr::read_csv("falls_prevention.csv", show_col_types = FALSE) %>%
  janitor::clean_names()

#-----------------------------#
# 1) Cleaning helpers
#-----------------------------#

# Extract the "code" from REDCap labelled exports like:
# "Female (female)", "No (0)", "Complete (2)", "80+ (80_)", "10/17 ... (11)"
extract_code <- function(x) {
  x <- as.character(x)
  out <- str_match(x, "\\(([^()]*)\\)")[, 2]
  # if no "(...)" present, keep original
  ifelse(is.na(out), x, out)
}

# Extract the human-readable label before the last parenthetical
# e.g., "Female (female)" -> "Female"
extract_label <- function(x) {
  x <- as.character(x)
  out <- str_replace(x, "\\s*\\([^()]*\\)\\s*$", "")
  out <- na_if(out, "")
  out
}

# Convert "Checked (1)" / "Unchecked (0)" / "N/A" / "NA" into 1/0/NA
to_binary01 <- function(x) {
  x0 <- as.character(x)
  # First try to pull the numeric code inside ()
  code <- extract_code(x0)
  # Coerce "1"/"0" to numeric; set non-numeric to NA
  suppressWarnings(num <- as.numeric(code))
  # Handle words like "Checked"/"Unchecked" if parentheses missing
  num <- ifelse(is.na(num) & str_detect(str_to_lower(x0), "checked"), 1, num)
  num <- ifelse(is.na(num) & str_detect(str_to_lower(x0), "unchecked"), 0, num)
  # Common NA tokens
  num <- ifelse(str_to_lower(x0) %in% c("na", "n/a", ""), NA_real_, num)
  num
}

# Parse age from categories like: "70-79 (70_79)", "80+ (80_)", "50-59 (50_59)"
age_to_midpoint <- function(age_cat_code) {
  x <- as.character(age_cat_code)
  # e.g., "70_79" -> midpoint 74.5
  ifelse(
    str_detect(x, "^\\d+_\\d+$"),
    (as.numeric(str_extract(x, "^\\d+")) + as.numeric(str_extract(x, "\\d+$"))) / 2,
    ifelse(
      str_detect(x, "^\\d+_$"),  # e.g., "80_"
      as.numeric(str_extract(x, "^\\d+")) + 5,  # arbitrary midpoint for open-ended (80+ -> 85)
      NA_real_
    )
  )
}

#-----------------------------#
# 2) Clean + derive variables
#-----------------------------#

# Identify your risk items (composite outcome = sum of these)
risk_items <- c(
  "fallen_withn_past_year",
  "rec_aids",
  "steady_walk",
  "steady_home",
  "fall_worry",
  "steady_chair",
  "risk_curb",
  "risk_toilet",
  "risk_feet",
  "risk_meds",
  "risk_meds2",
  "risk_mental"
)

# Optional: predictors you mentioned + common ones
# (edit freely based on what you want in the model)
candidate_predictors <- c(
  "age1", "gender",
  "fp_belief", "fp_aids", "fp_home_mods",
  "fp_importance", "fall_risk", "fall_severity",
  "fall_avoid", "fp_exercise", "fp_exercise_freq",
  "falls_number"
)

dat <- raw %>%
  # Keep only rows with a participant_id if your file has extra header junk
  filter(!is.na(participant_id)) %>%
  mutate(
    # Age
    age_cat_code  = extract_code(age1),
    age_cat_label = extract_label(age1),
    age_mid       = age_to_midpoint(age_cat_code),
    
    # Gender
    gender_code   = extract_code(gender),
    gender_label  = extract_label(gender),
    
    # Falls in last year + count
    fall_yr_num    = to_binary01(fall_yr),
    falls_number_n = suppressWarnings(as.numeric(extract_code(falls_number))),
    
    # Knowledge/perception/practices
    fp_belief_num     = to_binary01(fp_belief),
    fp_aids_num       = to_binary01(fp_aids),
    fp_home_mods_num  = to_binary01(fp_home_mods),
    fall_avoid_num    = to_binary01(fall_avoid),
    fp_exercise_num   = to_binary01(fp_exercise),
    fp_exercise_freq_code = extract_code(fp_exercise_freq),
    
    # Scales (often already numeric; extract_code helps if they came in as "7" or "7 (7)")
    fp_importance_n = suppressWarnings(as.numeric(extract_code(fp_importance))),
    fall_risk_n     = suppressWarnings(as.numeric(extract_code(fall_risk))),
    fall_severity_n = suppressWarnings(as.numeric(extract_code(fall_severity)))
  ) %>%
  # Convert risk items to 0/1 numeric consistently
  mutate(across(all_of(risk_items), to_binary01)) %>%
  # Composite risk score (sum of binary risk items)
  rowwise() %>%
  mutate(
    fall_risk_comp = sum(c_across(all_of(risk_items)), na.rm = TRUE),
    fall_risk_comp_missing_n = sum(is.na(c_across(all_of(risk_items))))
  ) %>%
  ungroup()

# Quick QC checks
dat %>%
  summarise(
    n = n(),
    comp_min = min(fall_risk_comp, na.rm = TRUE),
    comp_max = max(fall_risk_comp, na.rm = TRUE),
    comp_mean = mean(fall_risk_comp, na.rm = TRUE),
    any_out_of_range = any(fall_risk_comp < 0 | fall_risk_comp > length(risk_items), na.rm = TRUE)
  )

#-----------------------------#
# 3) Labeling (human-friendly)
#-----------------------------#

# Create factor versions for reporting (keep numeric versions for modeling)
dat <- dat %>%
  mutate(
    gender_f = fct_recode(
      factor(gender_code),
      Female = "female",
      Male   = "male"
      # add other codes if present (e.g., "nonbinary", "prefer_not")
    ),
    fp_belief_f    = factor(fp_belief_num, levels = c(0, 1), labels = c("No", "Yes")),
    fp_aids_f      = factor(fp_aids_num,   levels = c(0, 1), labels = c("No", "Yes")),
    fp_home_mods_f = factor(fp_home_mods_num, levels = c(0, 1), labels = c("No", "Yes")),
    fall_yr_f      = factor(fall_yr_num,   levels = c(0, 1), labels = c("No", "Yes"))
  )

# Add variable labels (useful for tables)
var_label(dat$age_mid)          <- "Age (midpoint from category)"
var_label(dat$gender_f)         <- "Gender"
var_label(dat$fall_yr_f)        <- "Fallen in last year"
var_label(dat$falls_number_n)   <- "Number of falls (if fallen)"
var_label(dat$fp_belief_f)      <- "Belief: falls can be prevented"
var_label(dat$fp_aids_f)        <- "Uses assistive device"
var_label(dat$fp_home_mods_f)   <- "Home modifications for falls prevention"
var_label(dat$fp_importance_n)  <- "Falls prevention importance (0–10)"
var_label(dat$fall_risk_n)      <- "Perceived likelihood of falling next year (0–10)"
var_label(dat$fall_severity_n)  <- "Perceived likelihood of serious injury (0–10)"
var_label(dat$fall_risk_comp)   <- paste0("Composite falls risk score (0–", length(risk_items), ")")

#-----------------------------#
# 4) Descriptive statistics
#-----------------------------#

# A) Data overview
skimr::skim(dat)

# B) Simple frequency tables
dat %>%
  count(gender_f, name = "n") %>%
  mutate(pct = n / sum(n)) %>%
  arrange(desc(n))

dat %>%
  count(fall_yr_f, name = "n") %>%
  mutate(pct = n / sum(n))

# C) Nice publication-style table (edit variables to taste)
desc_tbl <- dat %>%
  select(
    age_mid, gender_f,
    fall_yr_f, falls_number_n,
    fp_belief_f, fp_aids_f, fp_home_mods_f,
    fp_importance_n, fall_risk_n, fall_severity_n,
    fall_risk_comp
  ) %>%
  gtsummary::tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    missing = "ifany"
  )
desc_tbl

#-----------------------------#
# 5) Regression analyses
# Outcome: composite falls risk (count of risk items)
#-----------------------------#

# Model-ready dataset (drop rows with missing key predictors as needed)
model_dat <- dat %>%
  select(
    fall_risk_comp,
    age_mid,
    gender_f,
    fp_belief_num, fp_aids_num, fp_home_mods_num,
    fp_importance_n, fall_risk_n, fall_severity_n,
    fall_yr_num, falls_number_n
  )

# Option 1: Linear regression (simple, OK if score behaves ~continuous)
m_lm <- lm(
  fall_risk_comp ~ age_mid + gender_f +
    fp_belief_num + fp_aids_num + fp_home_mods_num +
    fp_importance_n + fall_risk_n + fall_severity_n,
  data = model_dat
)
summary(m_lm)

# Option 2: Poisson regression (more natural for count outcome)
# Note: check overdispersion; if present consider negative binomial
m_pois <- glm(
  fall_risk_comp ~ age_mid + gender_f +
    fp_belief_num + fp_aids_num + fp_home_mods_num +
    fp_importance_n + fall_risk_n + fall_severity_n,
  family = poisson(link = "log"),
  data = model_dat
)
summary(m_pois)

# Quick overdispersion check (rule of thumb: >>1 suggests overdispersion)
dispersion_ratio <- sum(residuals(m_pois, type = "pearson")^2, na.rm = TRUE) / m_pois$df.residual
dispersion_ratio

# If overdispersed, use negative binomial (requires MASS)
if (requireNamespace("MASS", quietly = TRUE)) {
  m_nb <- MASS::glm.nb(
    fall_risk_comp ~ age_mid + gender_f +
      fp_belief_num + fp_aids_num + fp_home_mods_num +
      fp_importance_n + fall_risk_n + fall_severity_n,
    data = model_dat
  )
  summary(m_nb)
}

# Reporting-friendly regression table
reg_tbl <- gtsummary::tbl_regression(m_lm) %>%
  gtsummary::add_glance_table(include = c(nobs, r.squared, adj.r.squared))
reg_tbl

############################################################
# Notes you’ll likely want to customize:
# 1) Which predictors are included in the model.
# 2) Whether "age1" is categorical (use age_cat_label as factor) vs midpoint numeric.
# 3) Whether your risk items are truly all 0/1 (this code enforces it).
############################################################





############################################################
# Visualizations for descriptives + regression assumptions
# (works with the dat / model_dat objects from earlier code)
############################################################

library(tidyverse)
library(janitor)

# If you don't already have them loaded:
# library(skimr); library(gtsummary)

# -----------------------------
# A) DESCRIPTIVE VISUALIZATIONS
# -----------------------------

# 1) Sample size + missingness heatmap (quick look)
# (Optional; requires naniar)
if (requireNamespace("naniar", quietly = TRUE)) {
  library(naniar)
  vis_miss(dat %>% select(age_mid, gender_f, fall_yr_num, falls_number_n,
                          fp_belief_num, fp_aids_num, fp_home_mods_num,
                          fp_importance_n, fall_risk_n, fall_severity_n,
                          fall_risk_comp))
}

# 2) Age distribution
ggplot(dat, aes(x = age_mid)) +
  geom_histogram(bins = 15, na.rm = TRUE) +
  labs(
    title = "Age distribution (midpoint from age category)",
    x = "Age (years)", y = "Count"
  )

# 3) Gender distribution
ggplot(dat, aes(x = gender_f)) +
  geom_bar(na.rm = TRUE) +
  labs(title = "Gender", x = NULL, y = "Count") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# 4) Fallen in last year
ggplot(dat, aes(x = fall_yr_f)) +
  geom_bar(na.rm = TRUE) +
  labs(title = "Fallen in the last year", x = NULL, y = "Count")

# 5) Falls prevention belief (Yes/No)
ggplot(dat, aes(x = fp_belief_f)) +
  geom_bar(na.rm = TRUE) +
  labs(title = "Belief that falls can be prevented", x = NULL, y = "Count")

# 6) Key 0–10 scales: importance / perceived risk / severity
scales_long <- dat %>%
  select(fp_importance_n, fall_risk_n, fall_severity_n) %>%
  pivot_longer(everything(), names_to = "scale", values_to = "value") %>%
  mutate(
    scale = recode(scale,
                   fp_importance_n = "Falls prevention importance (0–10)",
                   fall_risk_n     = "Perceived likelihood of falling (0–10)",
                   fall_severity_n = "Perceived likelihood of serious injury (0–10)")
  )

ggplot(scales_long, aes(x = value)) +
  geom_histogram(bins = 11, na.rm = TRUE) +
  facet_wrap(~ scale, ncol = 1) +
  labs(title = "Distributions of 0–10 scales", x = NULL, y = "Count")

# 7) Composite risk score distribution
ggplot(dat, aes(x = fall_risk_comp)) +
  geom_histogram(bins = length(unique(dat$fall_risk_comp[!is.na(dat$fall_risk_comp)])),
                 na.rm = TRUE) +
  labs(
    title = "Composite falls risk score distribution",
    x = "Composite risk (sum of risk items)", y = "Count"
  )

# 8) Composite risk by gender (boxplot + jitter)
ggplot(dat, aes(x = gender_f, y = fall_risk_comp)) +
  geom_boxplot(na.rm = TRUE) +
  geom_jitter(width = 0.15, height = 0, na.rm = TRUE) +
  labs(title = "Composite risk score by gender", x = NULL, y = "Composite risk")

# 9) Composite risk vs age (scatter + smooth)
ggplot(dat, aes(x = age_mid, y = fall_risk_comp)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", na.rm = TRUE) +
  labs(title = "Composite risk vs age", x = "Age (years)", y = "Composite risk")

# 10) Risk item prevalence (bar chart)
risk_items <- c(
  "fallen_withn_past_year","rec_aids","steady_walk","steady_home","fall_worry",
  "steady_chair","risk_curb","risk_toilet","risk_feet","risk_meds","risk_meds2","risk_mental"
)

risk_prev <- dat %>%
  summarise(across(all_of(risk_items), ~mean(.x == 1, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "item", values_to = "prevalence") %>%
  arrange(desc(prevalence)) %>%
  mutate(item = fct_inorder(item))

ggplot(risk_prev, aes(x = item, y = prevalence)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Prevalence of falls-risk items",
    x = NULL, y = "Percent 'Yes'"
  )

# 11) Correlation heatmap (numeric predictors + outcome)
num_for_corr <- dat %>%
  select(fall_risk_comp, age_mid, fp_belief_num, fp_aids_num, fp_home_mods_num,
         fp_importance_n, fall_risk_n, fall_severity_n, fall_yr_num, falls_number_n)

corr_mat <- cor(num_for_corr, use = "pairwise.complete.obs")
corr_long <- as.data.frame(as.table(corr_mat)) %>%
  rename(var1 = Var1, var2 = Var2, r = Freq)

ggplot(corr_long, aes(x = var1, y = var2, fill = r)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", r)), size = 3) +
  labs(title = "Correlation matrix (numeric variables)", x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -----------------------------
# B) REGRESSION ASSUMPTION CHECKS
# -----------------------------
# Assumes you fit:
#   m_lm   <- lm(...)
#   m_pois <- glm(..., family=poisson)
# and optionally m_nb <- MASS::glm.nb(...)

# --------
# 1) Linear regression diagnostics (lm)
# --------
# Base R diagnostic plots (residuals, QQ, scale-location, leverage)
par(mfrow = c(2, 2))
plot(m_lm)
par(mfrow = c(1, 1))

# Residual distribution
ggplot(data.frame(resid = resid(m_lm)), aes(x = resid)) +
  geom_histogram(bins = 20) +
  labs(title = "LM residuals histogram", x = "Residual", y = "Count")

# QQ plot for normality of residuals
ggplot(data.frame(sample = resid(m_lm)), aes(sample = sample)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "LM residuals Q-Q plot", x = "Theoretical quantiles", y = "Sample quantiles")

# Residuals vs fitted (linearity + homoscedasticity)
ggplot(data.frame(fitted = fitted(m_lm), resid = resid(m_lm)),
       aes(x = fitted, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess") +
  labs(title = "LM residuals vs fitted", x = "Fitted", y = "Residual")

# Formal tests (optional):
# Normality test (note: can be overly sensitive with larger n)
if (requireNamespace("nortest", quietly = TRUE)) {
  nortest::ad.test(resid(m_lm))  # Anderson-Darling
} else {
  shapiro.test(resid(m_lm))      # Shapiro-Wilk (best for smaller n)
}

# Homoscedasticity test (Breusch-Pagan)
if (requireNamespace("lmtest", quietly = TRUE)) {
  lmtest::bptest(m_lm)
}

# Influence / leverage
if (requireNamespace("broom", quietly = TRUE)) {
  library(broom)
  aug <- broom::augment(m_lm)
  # Cook's distance plot
  ggplot(aug, aes(x = seq_along(.cooksd), y = .cooksd)) +
    geom_point() +
    labs(title = "Cook's distance (LM)", x = "Observation", y = "Cook's D")
}

# Multicollinearity (VIF)
if (requireNamespace("car", quietly = TRUE)) {
  car::vif(m_lm)
}

# --------
# 2) Poisson regression diagnostics (glm Poisson)
# Assumptions: count outcome, log link; mean ~ variance (equidispersion)
# --------

# Overdispersion check (Pearson chi-square / df)
pois_disp <- sum(residuals(m_pois, type = "pearson")^2, na.rm = TRUE) / m_pois$df.residual
pois_disp

# Residuals vs fitted (look for structure)
ggplot(data.frame(fitted = fitted(m_pois), resid = residuals(m_pois, type = "pearson")),
       aes(x = fitted, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess") +
  labs(title = "Poisson: Pearson residuals vs fitted", x = "Fitted mean", y = "Pearson residual")

# Influence (approx) for glm
glm_infl <- influence.measures(m_pois)
summary(glm_infl)

# Multicollinearity for glm (use same approach)
if (requireNamespace("car", quietly = TRUE)) {
  car::vif(m_pois)
}

# Optional: goodness-of-fit test for Poisson (deviance)
# Large deviance relative to df can suggest lack of fit / overdispersion
c(deviance = deviance(m_pois), df = df.residual(m_pois))

# If overdispersion is present, consider:
# - Negative binomial (glm.nb)
# - Quasi-Poisson (adjusts SEs)

# Quasi-Poisson alternative
m_qpois <- glm(
  formula(m_pois),
  family = quasipoisson(link = "log"),
  data = model.frame(m_pois)
)
summary(m_qpois)

# If MASS glm.nb exists and you fit m_nb, check residuals similarly
if (exists("m_nb")) {
  nb_disp <- sum(residuals(m_nb, type = "pearson")^2, na.rm = TRUE) / m_nb$df.residual
  nb_disp
  
  ggplot(data.frame(fitted = fitted(m_nb), resid = residuals(m_nb, type = "pearson")),
         aes(x = fitted, y = resid)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(method = "loess") +
    labs(title = "NegBin: Pearson residuals vs fitted", x = "Fitted mean", y = "Pearson residual")
}

# --------
# 3) Linearity-of-predictors check (especially for LM)
# Component + residual plots (optional)
if (requireNamespace("car", quietly = TRUE)) {
  car::crPlots(m_lm)  # one plot per numeric predictor
}

# --------
# 4) Robust SEs (optional safeguard if heteroskedasticity)
if (requireNamespace("sandwich", quietly = TRUE) &&
    requireNamespace("lmtest", quietly = TRUE)) {
  library(sandwich); library(lmtest)
  lmtest::coeftest(m_lm, vcov = sandwich::vcovHC(m_lm, type = "HC3"))
  lmtest::coeftest(m_pois, vcov = sandwich::vcovHC(m_pois, type = "HC3"))
}
