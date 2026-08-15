# Predicting Elite Marathon Winning Times (GLMM)

A Gamma GLMM with random effects for athlete and event, fitted on winners of the
six World Marathon Majors from 1980 to 2018. Predicts finish times to within
**3.3 minutes RMSE** on a held-out 20% test set.

Presented at the AUEB Sports Analytics Workshop (SAW) 2026.
Slides: [Presentation_SAW2026.pdf](Presentation_SAW2026.pdf)

## The path

**OLS first, and it failed.** Stepwise selection gave an out-of-sample RMSE of
0.0634 hours (about 3.8 minutes), but the diagnostics rejected it: Shapiro-Wilk
p = 2.97e-13 and Breusch-Pagan p = 6.41e-07. Both failures are structural rather
than incidental. Elite finish times are bounded below by human physiology, which
forces right skew, and residual variance grows as course and weather conditions
worsen.

**Gamma GLM.** A Gamma family absorbs the right skew and respects the biological
floor. Comparing link functions by AIC: identity -893.93, log -898.05, inverse
-901.65. The inverse link won, and it is also the interpretable choice, since it
models speed (1/time) rather than time.

**Mixed effects.** A pooled model treats five Kipchoge victories as five
independent athletes. Random intercepts for `Name` and `Event` fix that,
capturing per-athlete baselines and unmeasured race-day conditions. A scaled year
term controls for four decades of sport-wide improvement, and a natural cubic
spline on age (df = 3) captures the climb to peak and the decline after it. Three
interactions survived AIC-based forward selection: gender by shoes, gender by
route feature, and route feature by shoes.

## Results

Held-out 20% test set: **RMSE 0.056 hours (3.3 min), MAE 0.0429 hours (2.5 min).**

The interaction terms carry the interesting findings. There is no universally
fastest shoe in the data: Adidas is associated with faster times on flat straight
courses, ASICS on courses with numerous turns. Nike carbon-plate models show a
larger associated advantage for women than for men.

## Data

Winners of the Tokyo, Berlin, Boston, New York, London and Chicago marathons,
male and female, 1980 to 2018, with athlete biometrics, shoe brand, route
feature, weather and humidity.

The pre-1980 archive was deliberately excluded: women were barred from the sport
until the 1970s, fields were largely domestic so the global talent pool never
actually competed, distances were inconsistent (Boston ran 39.4 km for years),
and shoe and weather data are unrecorded.

The dataset is not redistributed here, so the script is provided for review rather
than execution.

## Limitations

- **Shoe effects are not causal.** Brand is confounded with sponsorship, and
  sponsors sign the fastest athletes. The interactions describe association in
  observational data and nothing stronger.
- **Winners only.** A few hundred observations, one per event per gender per
  year, which is a small and extreme sample. Random effects help, but the
  athlete-level variance is estimated from few repeats for most runners.
- **Random train/test split on temporal data.** Year is a predictor, so a random
  split lets the model see future years during training. A chronological holdout
  would be the harder and more honest test.
- **Era confounds.** A 38-year window spans changes in doping enforcement,
  nutrition, pacing and course records that no single year term fully absorbs.
- **Coarse environmental variables.** Weather and humidity are categorical
  factors, not measurements.

## Stack

R, `lme4`, `splines`, `lmtest`, `car`
