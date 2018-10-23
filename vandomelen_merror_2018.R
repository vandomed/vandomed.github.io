# Oct. 16, 2018
# Dane Van Domelen
# R code for manuscript
# "From self-report to wearables: measurement error lives on"


# Load packages -----------------------------------------------------------

library("nhanesdata")
library("nhanesaccel")
library("dplyr")
library("survey")
library("dvmisc")
library("ggplot2")


# Process accelerometer data and prep dataset -----------------------------

# Process accelerometer data
pa <- process_nhanes(
  waves = 3,
  brevity = 2,
  weartime_maximum = 1200,
  artifact_thresh = 32767,
  return_form = "both"
)

# Extract per-person and per-day data frames
perperson <- pa[[1]]
perday <- pa[[2]]

# Merge demographics, body measurements, and HDL into perperson data frame
demo_cd <- full_join(demo_c, demo_d)
bmx_cd <- full_join(bmx_c, bmx_d)
hdl_cd <- full_join(l13_c, hdl_d, by = c("LBXHDD" = "LBDHDD", "SEQN", "LBDHDDSI"))
crp_cd <- full_join(l11_c, crp_d)

names(perperson) <- toupper(names(perperson))
df <- perperson %>% 
  left_join(demo_cd) %>% 
  left_join(bmx_cd) %>% 
  left_join(hdl_cd) %>% 
  dplyr::select(
    SEQN, SDDSRVYR, RIAGENDR, RIDAGEYR, RIDRETH1, SDMVPSU, SDMVSTRA, 
    WTMEC4YR_ADJ, INCLUDE, VALID_DAYS, VALID_MIN, MVPA_MIN, 
    COUNTS, CPM, SED_MIN, LIGHT_MIN, LIFE_MIN, MOD_MIN, VIG_MIN, 
    MAX_1MIN_COUNTS, MAX_5MIN_COUNTS, MAX_30MIN_COUNTS, SED_BREAKS, 
    SED_BOUTED_30MIN, GUIDELINE_MIN, 
    BMXBMI, BMXWAIST, RIDEXPRG, LBXHDD, LBXCRP
  ) %>%
  dplyr::mutate(
    LOGCRP = log(LBXCRP + 0.02),
    RACE = dplyr::recode_factor(RIDRETH1, `3` = "NH White", `4` = "NH Black", 
                                `1` = "Mexican American", `2` = "Other", `5` = "Other"), 
    INCLUDE1 = ifelse(INCLUDE == 1 & RIDAGEYR %in% 20: 39 & ! is.na(RACE) & 
                        (RIAGENDR == 1 | (RIAGENDR == 2 & RIDEXPRG == 2)), 1, 0), 
    INCLUDE7 = ifelse(INCLUDE1 == 1 & VALID_DAYS == 7, 1, 0)
  )

# Look at distribution of HDL and log(HDL)
p <- ggplot(subset(df, RIDAGEYR %in% 20: 39), aes(x = LBXHDD)) +
  geom_histogram(color = "black", fill = "white") + 
  labs(title = "HDL by Sex in NHANES 03-06 (age 20-39)") + 
  facet_grid(RIAGENDR ~ .) + 
  theme_bw()
p

p <- ggplot(subset(df, RIDAGEYR %in% 20: 39), aes(x = log(LBXHDD))) +
  geom_histogram(color = "black", fill = "white") + 
  labs(title = "log(HDL) by Sex in NHANES 03-06 (age 20-39)") + 
  facet_grid(RIAGENDR ~ .) + 
  theme_bw()
p

# Look at distribution of CRP and log(CRP)
p <- ggplot(subset(df, RIDAGEYR %in% 20: 39), aes(x = LBXCRP)) +
  geom_histogram(color = "black", fill = "white") + 
  labs(title = "CRP by Sex in NHANES 03-06 (age 20-39)") + 
  facet_grid(RIAGENDR ~ .) + 
  theme_bw()
p

p <- ggplot(subset(df, RIDAGEYR %in% 20: 39), aes(x = LOGCRP)) +
  geom_histogram(color = "black", fill = "white") + 
  labs(title = "log(CRP) by Sex in NHANES 03-06 (age 20-39)") + 
  facet_grid(RIAGENDR ~ .) + 
  theme_bw()
p

# Compare HDL in 2003-2004 vs. 2005-2006 waves
p <- ggplot(subset(df, RIDAGEYR %in% 20: 39), aes(x = LBXHDD)) +
  geom_histogram(color = "black", fill = "white") + 
  labs(title = "log(HDL) by Sex in NHANES 03-06 (age 20-39)") + 
  facet_grid(SDDSRVYR ~ .) + 
  theme_bw()
p

# Compare log(CRP) in 2003-2004 vs. 2005-2006 waves
p <- ggplot(subset(df, RIDAGEYR %in% 20: 39), aes(x = LOGCRP)) +
  geom_histogram(color = "black", fill = "white") + 
  labs(title = "log(HDL) by Sex in NHANES 03-06 (age 20-39)") + 
  facet_grid(SDDSRVYR ~ .) + 
  theme_bw()
p


# Estimate associations for MVPA and HDL ----------------------------------

# Create survey design object
design <- svydesign(
  data = subset(df, INCLUDE == 1 & RIDAGEYR %in% 20: 39 & ! is.na(LBXHDD) & 
                  (RIAGENDR == 1 | (RIAGENDR == 2 & RIDEXPRG == 2))), 
  ids = ~SDMVPSU, 
  weights = ~WTMEC4YR_ADJ, 
  strata = ~SDMVSTRA, 
  nest = TRUE
)

# Estimate covariate-adjusted association as function of sex and minimum number 
# of valid days
f.fit <- function(sex, days) {
  dsub <- subset(design, RIAGENDR == sex & VALID_DAYS >= days)
  fit <- svyglm(LBXHDD ~ MVPA_MIN + VALID_MIN + RIDAGEYR + RACE, design = dsub)
  ci = confint(fit)[2, ]
  list(n = nrow(dsub), bhat = fit$coef[2], lower = ci[1], upper = ci[2])
}
betas <- f.fit %>% iterate(sex = 1: 2, days = 1: 7)
betas$sex <- factor(betas$sex, levels = c(1, 2), labels = c("Males", "Females")) 

# Create Figure 1
p <- ggplot(betas, aes(x = days, y = bhat)) + 
  geom_point() + 
  ylim(-0.035, 0.365) +
  geom_hline(yintercept = 0, lty = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  facet_grid(. ~ sex) + 
  labs(title = "Estimated Association for MVPA and HDL", 
       y = expression(paste(hat(beta), " (95% confidence interval)", sep = ""))) + 
  theme_bw(base_size = 15) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.title.x = element_text(vjust = -1)) + 
  scale_x_discrete("Minimum number of valid days (sample size)", 
                   limits = 1: 7) + 
  geom_text(aes(y = -0.034, label = paste("(", n, ")", sep = "")), 
            size = 4, angle = 0)
p


# Resampling experiment ---------------------------------------------------

# Separate perday into lists according to SEQN
perday.list <- base::split(x = perday[, c("id", "valid_min", "mvpa_min")], f = perday$id)
locs <- which(df$INCLUDE7 == 1)
perday.list.subset <- perday.list[locs]

f.resampling <- function(sex, days) {
  
  # Sample certain number of days for each participant
  sampled <- lapply(perday.list.subset, function(x) x[sample(1: 7, days), ])
  df$VALID_MIN2 <- df$MVPA_MIN2 <- rep(NA, nrow(df))
  df$VALID_MIN2[locs] <- sapply(sampled, function(x) mean(x[, "valid_min"]))
  df$MVPA_MIN2[locs] <- sapply(sampled, function(x) mean(x[, "mvpa_min"]))
  
  # Define survey design object
  design <- svydesign(
    data = subset(df, INCLUDE == 1), 
    ids = ~SDMVPSU, 
    weights = ~WTMEC4YR_ADJ, 
    strata = ~SDMVSTRA, 
    nest = TRUE
  )
  
  # Subset design object
  dsub <- subset(
    design, INCLUDE7 == 1 & RIAGENDR == sex
  )
  
  # Regress HDL on MVPA and covariates and record estimates
  fit <- svyglm(
    LBXHDD ~ MVPA_MIN2 + VALID_MIN2 + RIDAGEYR + RACE, 
    design = dsub
  )
  
  # Return point estimate and CI
  ci <- confint(fit)
  return(list(bhat = fit$coef[2], lower = ci[2, 1], upper = ci[2, 2]))
  
}

# Run 1,000 trials for each scenario (takes roughly 1 hour)
set.seed(123)
betas <- f.resampling %>% iterate(sex = 1: 2, days = 1: 7, trials = 1000)
betas$sex <- factor(betas$sex, levels = c(1, 2), labels = c("Males", "Females"))

# Create Figure 2
df2 <- betas %>% 
  group_by(sex, days) %>% 
  dplyr::summarise(mbhat = median(bhat), 
                   mlower = median(lower), 
                   mupper = median(upper))
attens <- paste(
  "(", round(100 - c(df2$mbhat[1: 7] / df2$mbhat[7], 
                     df2$mbhat[8: 14] / df2$mbhat[14]) * 100, 0), "%)", sep = ""
)
p <- ggplot(df2, aes(x = days, y = mbhat)) + 
  geom_point() + 
  ylim(-0.035, 0.365) +
  geom_hline(yintercept = 0, lty = 2) + 
  geom_errorbar(aes(ymin = mlower, ymax = mupper), width = 0.2) + 
  facet_grid(. ~ sex) + 
  labs(title = "Estimated Association for MVPA and HDL", 
       y = expression(paste("Median ", hat(beta), " (95% confidence interval)", sep = ""))) + 
  theme_bw(base_size = 15) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.title.x = element_text(vjust = -1)) + 
  scale_x_discrete("Minimum number of valid days", 
                   limits = 1: 7) + 
  geom_text(aes(y = -0.034, label = attens), size = 4, angle = 0)
p


# Resampling experiment with various PA variables -------------------------

# Separate perday into lists according to SEQN
vars <- c("counts", "cpm", "sed_min", "light_min", "life_min", "mod_min", 
          "vig_min", "max_1min_counts", "max_5min_counts", 
          "max_30min_counts", "sed_breaks", "sed_bouted_30min", 
          "guideline_min")
perday.list <- base::split(
  x = perday[, c("id", "valid_min", vars)], 
  f = perday$id
)
perday.list.subset <- perday.list[locs]

# Function for single trial
df$MVPA2 <- df$VALID_MIN2 <- NULL
f.resampling <- function(sex, days) {
  
  # Sample certain number of days for each participant
  df[, c("valid_min", vars)] <- rep(NA, nrow(df))
  df[locs, c("valid_min", vars)] <- t(sapply(perday.list.subset, function(x)
    apply(x[sample(1: 7, days), -1], 2, mean)))
  
  # Define survey design object
  design <- svydesign(
    data = subset(df, INCLUDE == 1), 
    ids = ~SDMVPSU, 
    weights = ~WTMEC4YR_ADJ, 
    strata = ~SDMVSTRA, 
    nest = TRUE
  )
  
  # Subset design object
  dsub <- subset(
    design, INCLUDE7 == 1 & RIAGENDR == sex
  )
  
  # Regress HDL and log(CRP) on various PA variables and record estimates
  hdl.betas <- c()
  logcrp.betas <- c()
  for (ii in 1: length(vars)) {
    
    formula <- paste("LBXHDD ~ ", vars[ii], " + valid_min + RIDAGEYR + RACE", sep = "")
    fit <- svyglm(
      formula = formula, 
      design = dsub
    )
    hdl.betas[ii] <- fit$coef[2]
    
    formula <- paste("LOGCRP ~ ", vars[ii], " + valid_min + RIDAGEYR + RACE", sep = "")
    fit <- svyglm(
      formula = formula, 
      design = dsub
    )
    logcrp.betas[ii] <- fit$coef[2]
    
  }
  names(hdl.betas) <- names(logcrp.betas) <- vars
  return(c(hdl.betas, logcrp.betas))
  
}

# Run 1,000 trials for each scenario (takes roughly 4 hours)
set.seed(1234)
betas <- f.resampling %>% iterate(sex = 1: 2, days = 1: 7, trials = 1000)
betas$sex <- factor(betas$sex, levels = c(1, 2), labels = c("Males", "Females"))

# Go from wide to long format for ggplot
betas.hdl <- betas[, c(1, 2, 3: 15)] %>% 
  gather(key = "var", value = "bhat", vars, factor_key = TRUE)
betas.logcrp <- betas[, c(1, 2, 16: 28)] %>% 
  gather(key = "var", value = "bhat", vars, factor_key = TRUE)

sevens.hdl <- betas.hdl %>% 
  dplyr::filter(days == 7) %>%
  dplyr::group_by(sex, var) %>%
  dplyr::filter(row_number() == 1) %>% 
  dplyr::arrange(sex)
sevens.hdl <- sevens.hdl$bhat

sevens.logcrp <- betas.logcrp %>% 
  dplyr::filter(days == 7) %>%
  dplyr::group_by(sex, var) %>%
  dplyr::filter(row_number() == 1) %>% 
  dplyr::arrange(sex)
sevens.logcrp <- sevens.logcrp$bhat

# Calculation percent attenuation
df2.hdl <- betas.hdl %>% 
  group_by(days, sex, var) %>% 
  dplyr::summarise(mbhat = median(bhat))
df2.hdl$atten <- (1 - df2.hdl$mbhat / sevens.hdl) * 100

df2.logcrp <- betas.logcrp %>% 
  group_by(days, sex, var) %>% 
  dplyr::summarise(mbhat = median(bhat))
df2.logcrp$atten <- (1 - df2.logcrp$mbhat / sevens.logcrp) * 100

# Create Figure 3
df2 <- rbind(df2.hdl, df2.logcrp)
df2$outcome <- c(rep("HDL", nrow(df2) / 2), rep("log(CRP)", nrow(df2) / 2))
varlabs <- c("Counts", "Counts/min.", "Sed. time", "Light time", "Lifestyle time", 
             "Moderate time", "Vigorous time", "Max 1-min counts", "Max 5-min counts", 
             "Max 30-min counts", "Sed. breaks", 
             "Sed. time (30-min bouts)", "Guideline min.")
p <- ggplot(df2, aes(days, var)) + 
  geom_tile(aes(fill = atten), colour = "white") + 
  scale_fill_gradientn(colors = c("blue", "white", "red"), 
                       limits = c(-100, 100)) + 
  facet_grid(outcome ~ sex) + 
  labs(title = "Percent Attenuation Heat Map", 
       x = "Days sampled", y = "") + 
  theme_bw(base_size = 15) + 
  scale_x_discrete(limits = 1: 7, expand = c(0.001, 0.001)) + 
  scale_y_discrete(labels = varlabs) + 
  theme(legend.title=element_blank(), axis.title.y = element_blank()) 
p