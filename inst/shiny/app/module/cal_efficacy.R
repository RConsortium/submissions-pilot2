gluc_lmfit <- adlb %>%
  filter(AVISITN == 20) %>%
  lm(CHG ~ BASE + TRTPN, data = .)

## Raw summary statistics
t11 <- adlb %>%
  filter(AVISITN == 20) %>%
  group_by(TRTPN, TRTP) %>%
  summarise(
    N = n(),
    mean_bl = mean(BASE),
    sd_bl = sd(BASE),
    mean_chg = mean(CHG),
    sd_chg = sd(CHG),
    mean = mean(AVAL),
    sd = sd(AVAL)
  )

## Calculate LS mean
t12 <- emmeans(gluc_lmfit, "TRTPN")
t2 <- data.frame(pairs(t12))

## Merge and format data for reporting
apr0ancova1 <- merge(t11, t12) %>%
  mutate(emmean_sd = SE * sqrt(df)) %>%
  mutate(
    Trt = c("Study Drug", "Placebo"),
    N1 = N,
    Mean1 = fmt_est(mean_bl, sd_bl),
    N2 = N,
    Mean2 = fmt_est(mean, sd),
    N3 = N,
    Mean3 = fmt_est(mean_chg, sd_chg),
    CI = fmt_ci(emmean, lower.CL, upper.CL)
  ) %>%
  select(Trt:CI)

apr0ancova2 <- t2 %>%
  mutate(
    lower = estimate - 1.96 * SE,
    upper = estimate + 1.96 * SE
  ) %>%
  mutate(
    comp = "Study Drug vs. Placebo",
    mean = fmt_ci(estimate, lower, upper),
    p = fmt_pval(p.value)
  ) %>%
  select(comp:p)

apr0ancova3 <- data.frame(rmse = paste0(
  "Root Mean Squared Error of Change = ",
  formatC(sd(gluc_lmfit$residuals), digits = 2, format = "f", flag = "0")
))
