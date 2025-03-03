---
title: Instrument Correlations_1
execute:
  echo: true
  warning: false
format:
  html:
    grid:
      sidebar-width: 220px
      body-width: 1200px
      margin-width: 170px
      gutter-width: 1.0rem
  hugo-md:
    include: true
    html-math-method: mathjax
    output-file: cor1_hugo1.md
---


``` r
pacman::p_load(dplyr,purrr,tidyr,here, haven,tibble,ggplot2,ggh4x,lme4,knitr,kableExtra,gt,pander,flextable,ggh4x,psych,corrplot,factoextra)
options(digits=2, scipen=999, dplyr.summarise.inform=FALSE)

library(gridExtra)
library(factoextra)
library(mgcv)
library(lavaan)
library(CCA)
library(qgraph)
library(rpart)
library(rpart.plot)
library(mclust)
library(tidyLPA)

select = dplyr::select

source(here("scripts","survey_functions.R"))

draw <- readRDS(here("data","draw.rds"))
#dinst <- readRDS(here("data","dinst.rds"))

# Attari Energy Survey (Part 1)
aes1 <- draw |> select(id,ATT01:ATT18)
#aes2 <- dinst |> select(id,ATT01:ATT18)
aes_combined <- bind_rows(aes1)

att_useSave <- draw |> select(id,ATT19:ATT33)
#att_useSave2 <- dinst |> select(id,ATT19:ATT33)
att2_combined <- bind_rows(att_useSave)

els1 <- draw |> select(id,ELS01:ELS08)
#els2 <- dinst |> select(id,ELS01:ELS08)
els <- bind_rows(els1)

rs1 <- draw |> select(id,RS01:RS06)
#rs2 <- dinst |> select(id,RS01:RS06)
rs <- bind_rows(rs1)

attari1 <- analyze_attari_survey_part1(aes_combined)
attari2_scores <- analyze_attari_survey(att2_combined)
els_scores <- analyze_els_survey(els)
rs_scores <- analyze_recycling_survey(rs)

# Combine all scores into one dataframe
combined_scores <- attari1 %>%
  left_join(attari2_scores, by="id") %>%
  left_join(els_scores, by="id") %>%
  left_join(rs_scores, by="id")
```

``` r
# Analyze and score each survey
attari1 <- analyze_attari_survey_part1(aes_combined)
attari2_scores <- analyze_attari_survey(att2_combined)
els_scores <- analyze_els_survey(els)
rs_scores <- analyze_recycling_survey(rs)

# Combine all scores into one dataframe
combined_scores <- attari1 %>%
  left_join(attari2_scores, by = "id") %>%
  left_join(els_scores, by = "id") %>%
  left_join(rs_scores, by = "id")

# Rename columns for clarity
names(combined_scores) <- c(
  "id", "perceived_difficulty", "numeracy",
  "energy_use", "energy_save",
  "els_accuracy", "els_score",
  "env_attitude", "env_attitude_z",
  "pol_conservatism", "pol_conservatism_z"
)
```

``` r
attari1 <- analyze_attari_survey_part1(aes_combined)
attari2_scores <- analyze_attari_survey(att2_combined)
els_scores <- analyze_els_survey(els)
rs_scores <- analyze_recycling_survey(rs)
```

``` r
# Combine all scores into one dataframe
combined_scores <- attari1 %>%
  left_join(attari2_scores, by="id") %>%
  left_join(els_scores, by="id") %>%
  left_join(rs_scores, by="id")

# Rename columns for clarity
names(combined_scores) <- c("id", "perceived_difficulty", "numeracy", 
                          "energy_use", "energy_save", 
                          "els_accuracy", "els_score",
                          "env_attitude", "env_attitude_z",
                          "pol_conservatism", "pol_conservatism_z")

# Create correlation matrix for knowledge measures
knowledge_cors <- combined_scores %>%
  select(numeracy, energy_use, energy_save, els_score) %>%
  cor(use="pairwise.complete.obs")

# Create correlation matrix for motivation/attitude measures
motivation_cors <- combined_scores %>%
  select(perceived_difficulty, env_attitude_z, pol_conservatism_z) %>%
  cor(use="pairwise.complete.obs")

# Create correlation matrix between knowledge and motivation measures
knowledge_motivation_cors <- cor(
  combined_scores %>% select(numeracy, energy_use, energy_save, els_score),
  combined_scores %>% select(perceived_difficulty, env_attitude_z, pol_conservatism_z),
  use="pairwise.complete.obs"
)

# Principal Components Analysis for knowledge measures
knowledge_pca <- prcomp(combined_scores %>% 
                       select(numeracy, energy_use, energy_save, els_score),
                       scale=TRUE)

# Print correlations
print("Knowledge measure correlations:")
```

    [1] "Knowledge measure correlations:"

``` r
print(knowledge_cors)
```

                numeracy energy_use energy_save els_score
    numeracy        1.00       0.47        0.41      0.42
    energy_use      0.47       1.00        0.70      0.51
    energy_save     0.41       0.70        1.00      0.42
    els_score       0.42       0.51        0.42      1.00

``` r
print("\nMotivation measure correlations:")
```

    [1] "\nMotivation measure correlations:"

``` r
print(motivation_cors)
```

                         perceived_difficulty env_attitude_z pol_conservatism_z
    perceived_difficulty                 1.00          -0.45               0.26
    env_attitude_z                      -0.45           1.00              -0.42
    pol_conservatism_z                   0.26          -0.42               1.00

``` r
print("\nKnowledge-Motivation correlations:")
```

    [1] "\nKnowledge-Motivation correlations:"

``` r
print(knowledge_motivation_cors)
```

                perceived_difficulty env_attitude_z pol_conservatism_z
    numeracy                   -0.19           0.23              -0.31
    energy_use                 -0.37           0.33              -0.33
    energy_save                -0.34           0.31              -0.33
    els_score                  -0.27           0.31              -0.25

``` r
# Print PCA summary
print("\nPCA Summary for knowledge measures:")
```

    [1] "\nPCA Summary for knowledge measures:"

``` r
print(summary(knowledge_pca))
```

    Importance of components:
                            PC1   PC2   PC3    PC4
    Standard deviation     1.57 0.811 0.759 0.5353
    Proportion of Variance 0.62 0.164 0.144 0.0716
    Cumulative Proportion  0.62 0.784 0.928 1.0000

``` r
# Create scatterplot of first two knowledge components
pca_data <- as.data.frame(knowledge_pca$x)
ggplot(pca_data, aes(x=PC1, y=PC2)) +
  geom_point(alpha=0.5) +
  theme_minimal() +
  labs(title="PCA of Knowledge Measures",
       x="First Principal Component",
       y="Second Principal Component")
```

<img src="cors1.markdown_strict_files/figure-markdown_strict/unnamed-chunk-4-1.png" width="768" />

\[1\] "Knowledge measure correlations:"
numeracy energy_use energy_save els_score
numeracy 1.00 0.41 0.34 0.34
energy_use 0.41 1.00 0.57 0.40
energy_save 0.34 0.57 1.00 0.36
els_score 0.34 0.40 0.36 1.00
\[1\] "measure correlations:"
perceived_difficulty env_attitude_z pol_conservatism_z
perceived_difficulty 1.00 -0.39 0.13
env_attitude_z -0.39 1.00 -0.29
pol_conservatism_z 0.13 -0.29 1.00
\[1\] "-Motivation correlations:"
perceived_difficulty env_attitude_z pol_conservatism_z
numeracy -0.13 0.20 -0.25
energy_use -0.24 0.21 -0.23
energy_save -0.25 0.22 -0.22
els_score -0.22 0.27 -0.19
\[1\] "Summary for knowledge measures:"
Importance of components:
PC1 PC2 PC3 PC4
Standard deviation 1.492 0.834 0.812 0.648
Proportion of Variance 0.556 0.174 0.165 0.105
Cumulative Proportion 0.556 0.730 0.895 1.000

``` r
# --
# 1) Combine the instrument-level data
# --

# Make sure you have already run your survey-specific analysis functions so that
# attari1, attari2_scores, els_scores, rs_scores all exist and contain 'id' plus
# their computed subscale(s).

combined_df <- attari1 %>%
  full_join(attari2_scores, by = "id") %>%
  full_join(els_scores,       by = "id") %>%
  full_join(rs_scores,        by = "id")

# Check the dimensions
dim(combined_df)
```

    [1] 287  11

``` r
# Inspect a few rows
head(combined_df)
```

    # A tibble: 6 × 11
         id perceived_difficulty_score[,1] numeracy_score[,1] relative_energy_use_…¹
      <int>                          <dbl>              <dbl>                  <dbl>
    1     1                          0.515              0.982                0.760  
    2     2                         -0.481              0.982                0.00153
    3     3                          1.90              -1.87                -1.24   
    4     4                         -0.703             -1.26                 0.953  
    5     5                          0.791              0.982               -0.0468 
    6     6                         -0.260              0.982                0.899  
    # ℹ abbreviated name: ¹​relative_energy_use_score[,1]
    # ℹ 7 more variables: relative_energy_save_score <dbl[,1]>, accuracy <dbl>,
    #   els <dbl>, env_attitude <dbl>, env_attitude_z <dbl>,
    #   pol_conservatism <dbl>, pol_conservatism_z <dbl>

``` r
# --
# 2) Correlation among key subscales/items
# --

# For demonstration, let’s select these key measures from each instrument:
#   - perceived_difficulty_score (Attari Part 1)
#   - numeracy_score (Attari Part 1)
#   - relative_energy_use_score (Attari Part 2)
#   - relative_energy_save_score (Attari Part 2)
#   - els (Energy Literacy total standard score)
#   - env_attitude (Recycling Study)
#   - pol_conservatism (Recycling Study)

subscales_df <- combined_df %>%
  select(
    perceived_difficulty_score,
    numeracy_score,
    relative_energy_use_score,
    relative_energy_save_score,
    els,
    env_attitude,
    pol_conservatism
  )

# Compute pairwise correlations
subscale_cor <- cor(subscales_df, use = "pairwise.complete.obs")

# Print the correlation matrix
subscale_cor
```

                               perceived_difficulty_score numeracy_score
    perceived_difficulty_score                       1.00          -0.19
    numeracy_score                                  -0.19           1.00
    relative_energy_use_score                       -0.37           0.47
    relative_energy_save_score                      -0.34           0.41
    els                                             -0.27           0.42
    env_attitude                                    -0.45           0.23
    pol_conservatism                                 0.26          -0.31
                               relative_energy_use_score relative_energy_save_score
    perceived_difficulty_score                     -0.37                      -0.34
    numeracy_score                                  0.47                       0.41
    relative_energy_use_score                       1.00                       0.70
    relative_energy_save_score                      0.70                       1.00
    els                                             0.51                       0.42
    env_attitude                                    0.33                       0.31
    pol_conservatism                               -0.33                      -0.33
                                 els env_attitude pol_conservatism
    perceived_difficulty_score -0.27        -0.45             0.26
    numeracy_score              0.42         0.23            -0.31
    relative_energy_use_score   0.51         0.33            -0.33
    relative_energy_save_score  0.42         0.31            -0.33
    els                         1.00         0.31            -0.25
    env_attitude                0.31         1.00            -0.42
    pol_conservatism           -0.25        -0.42             1.00

``` r
# You can also visualize it with corrplot, e.g.:
corrplot::corrplot(subscale_cor, method = "ellipse", type = "upper",
                   tl.col = "black", tl.srt = 45,
                   addCoef.col = "black", number.cex = 0.7)
```

<img src="cors1.markdown_strict_files/figure-markdown_strict/unnamed-chunk-5-1.png" width="768" />

``` r
# --
# 3) Factor / PCA of subscales
# --
# We'll do a quick principal components analysis (PCA) on these 7 subscales.
# If you want a full factor analysis, you can use psych::fa() or similar.

library(psych)
pca_subscales <- principal(subscales_df, nfactors = 2, rotate = "varimax", scores = TRUE)
pca_subscales
```

    Principal Components Analysis
    Call: principal(r = subscales_df, nfactors = 2, rotate = "varimax", 
        scores = TRUE)
    Standardized loadings (pattern matrix) based upon correlation matrix
                                 RC1   RC2   h2   u2 com
    perceived_difficulty_score -0.17 -0.74 0.58 0.42 1.1
    numeracy_score              0.74  0.08 0.56 0.44 1.0
    relative_energy_use_score   0.81  0.28 0.74 0.26 1.2
    relative_energy_save_score  0.76  0.27 0.66 0.34 1.2
    els                         0.71  0.19 0.54 0.46 1.1
    env_attitude                0.15  0.84 0.72 0.28 1.1
    pol_conservatism           -0.27 -0.62 0.45 0.55 1.4

                           RC1  RC2
    SS loadings           2.42 1.83
    Proportion Var        0.35 0.26
    Cumulative Var        0.35 0.61
    Proportion Explained  0.57 0.43
    Cumulative Proportion 0.57 1.00

    Mean item complexity =  1.2
    Test of the hypothesis that 2 components are sufficient.

    The root mean square of the residuals (RMSR) is  0.11 
     with the empirical chi square  146  with prob <  0.0000000000000000000000000015 

    Fit based upon off diagonal values = 0.92

``` r
# Examine loadings
print(pca_subscales$loadings, cutoff = 0.30, sort = TRUE)
```


    Loadings:
                               RC1   RC2  
    numeracy_score              0.74      
    relative_energy_use_score   0.81      
    relative_energy_save_score  0.76      
    els                         0.71      
    perceived_difficulty_score       -0.74
    env_attitude                      0.84
    pol_conservatism                 -0.62

                    RC1  RC2
    SS loadings    2.42 1.82
    Proportion Var 0.35 0.26
    Cumulative Var 0.35 0.61

``` r
# The two-factor solution might (for example) split into a 
# “Knowledge / Numeracy” factor and a “Motivation / Attitude” factor, 
# but the actual pattern depends on your data.

# --
# 4) Some analyses focusing on the link between knowledge
#    and motivation items
# --

# (A) Simple correlation between ELS (knowledge) and env_attitude (motivation)
with(combined_df, cor(els, env_attitude, use = "pairwise.complete.obs"))
```

    [1] 0.31

``` r
# (B) Regression predicting motivation from knowledge:
#     e.g., env_attitude ~ ELS + numeracy_score + relative_energy_use_score
model_env <- lm(env_attitude ~ els + numeracy_score + relative_energy_use_score, 
                data = combined_df)
summary(model_env)
```


    Call:
    lm(formula = env_attitude ~ els + numeracy_score + relative_energy_use_score, 
        data = combined_df)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -2.1873 -0.4388  0.0569  0.5001  1.8183 

    Coefficients:
                              Estimate Std. Error t value            Pr(>|t|)    
    (Intercept)                 3.5906     0.0419   85.60 <0.0000000000000002 ***
    els                         0.1371     0.0505    2.72              0.0070 ** 
    numeracy_score              0.0367     0.0489    0.75              0.4533    
    relative_energy_use_score   0.1674     0.0517    3.24              0.0013 ** 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.71 on 283 degrees of freedom
    Multiple R-squared:  0.141, Adjusted R-squared:  0.131 
    F-statistic: 15.4 on 3 and 283 DF,  p-value: 0.00000000253

``` r
# (C) Scatterplot of perceived difficulty (motivation) vs. ELS (knowledge)
ggplot(combined_df, aes(x = els, y = perceived_difficulty_score)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, col = "red") +
  labs(
    x = "Energy Literacy (z-score)",
    y = "Perceived Difficulty of Energy-Saving Actions",
    title = "Relationship between Knowledge and Perceived Difficulty"
  ) +
  theme_minimal()
```

<img src="cors1.markdown_strict_files/figure-markdown_strict/unnamed-chunk-5-2.png" width="768" />

``` r
# (D) Another scatterplot: Numeracy vs. Env. Attitude
ggplot(combined_df, aes(x = numeracy_score, y = env_attitude)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(
    x = "Numeracy Score (z-score)",
    y = "Environmental Attitude",
    title = "Relationship between Numeracy and Environmental Attitude"
  ) +
  theme_minimal()
```

<img src="cors1.markdown_strict_files/figure-markdown_strict/unnamed-chunk-5-3.png" width="768" />

``` r
# Additional ideas:
#  - Add polynomial or interaction terms in regression
#  - Compare groups (e.g., high vs. low knowledge) on motivation
#  - Partial correlations controlling for politics or demographics
#  - Expand item-level factor analysis (including all items from each scale)
```

subscale_cor
perceived_difficulty_score numeracy_score relative_energy_use_score relative_energy_save_score els env_attitude pol_conservatism
perceived_difficulty_score 1.00 -0.13 -0.24 -0.25 -0.22 -0.39 0.13
numeracy_score -0.13 1.00 0.41 0.34 0.34 0.20 -0.25
relative_energy_use_score -0.24 0.41 1.00 0.57 0.40 0.21 -0.23
relative_energy_save_score -0.25 0.34 0.57 1.00 0.36 0.22 -0.22
els -0.22 0.34 0.40 0.36 1.00 0.27 -0.19
env_attitude -0.39 0.20 0.21 0.22 0.27 1.00 -0.29
pol_conservatism 0.13 -0.25 -0.23 -0.22 -0.19 -0.29 1.00

pca_subscales
Principal Components Analysis
Call: principal(r = subscales_df, nfactors = 2, rotate = "varimax",
scores = TRUE)
Standardized loadings (pattern matrix) based upon correlation matrix
RC1 RC2 h2 u2 com
perceived_difficulty_score -0.10 -0.77 0.60 0.40 1.0
numeracy_score 0.71 0.07 0.51 0.49 1.0
relative_energy_use_score 0.81 0.13 0.67 0.33 1.1
relative_energy_save_score 0.75 0.17 0.60 0.40 1.1
els 0.62 0.26 0.45 0.55 1.3
env_attitude 0.12 0.83 0.70 0.30 1.0
pol_conservatism -0.31 -0.42 0.27 0.73 1.8

                       RC1  RC2

SS loadings 2.22 1.57
Proportion Var 0.32 0.22
Cumulative Var 0.32 0.54
Proportion Explained 0.59 0.41
Cumulative Proportion 0.59 1.00

Mean item complexity = 1.2
Test of the hypothesis that 2 components are sufficient.

The root mean square of the residuals (RMSR) is 0.12
with the empirical chi square 339 with prob \< 0.00

# Examine loadings

    print(pca_subscales$loadings, cutoff = 0.30, sort = TRUE)

Loadings:
RC1 RC2  
numeracy_score 0.707  
relative_energy_use_score 0.809  
relative_energy_save_score 0.753  
els 0.617  
perceived_difficulty_score -0.769
env_attitude 0.828
pol_conservatism -0.308 -0.420

                RC1  RC2

SS loadings 2.22 1.57
Proportion Var 0.32 0.23
Cumulative Var 0.32 0.54

\# (A) Simple correlation between ELS (knowledge) and env_attitude (motivation)
with(combined_df, cor(els, env_attitude, use = "pairwise.complete.obs"))
\[1\] 0.27

summary(model_env)

Call:
lm(formula = env_attitude ~ els + numeracy_score + relative_energy_use_score,
data = combined_df)

Residuals:
Min 1Q Median 3Q Max
-2.2182 -0.4612 0.0582 0.5595 1.7604

Coefficients:
Estimate Std. Error t value Pr(\>\|t\|)  
(Intercept) 3.5832 0.0304 117.91 \< 0.0000000000000002 ***
els 0.1578 0.0339 4.65 0.0000041 ***
numeracy_score 0.0700 0.0342 2.05 0.041 \*  
relative_energy_use_score 0.0716 0.0350 2.05 0.041 \*  
--
Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.74 on 582 degrees of freedom
Multiple R-squared: 0.0936, Adjusted R-squared: 0.0889
F-statistic: 20 on 3 and 582 DF, p-value: 0.00000000000227

``` r
# Combine data from different instruments
attari1_scores <- analyze_attari_survey_part1(aes_combined)
attari2_scores <- analyze_attari_survey(att2_combined)
els_scores <- analyze_els_survey(els)
rs_scores <- analyze_recycling_survey(rs)

combined_data <- full_join(attari1_scores, attari2_scores, by = "id") %>%
  full_join(els_scores, by = "id") %>%
  full_join(rs_scores, by = "id")

# 1. Analyze instrument correlations
cor_matrix <- cor(combined_data[, -1], use = "pairwise.complete.obs")

# Visualize correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
```

<img src="cors1.markdown_strict_files/figure-markdown_strict/unnamed-chunk-6-1.png" width="768" />

``` r
# 2. Assess underlying structure using factor analysis
fa_result <- fa(combined_data[, -1], nfactors = 3, rotate = "varimax")
print(fa_result)
```

    Factor Analysis using method =  minres
    Call: fa(r = combined_data[, -1], nfactors = 3, rotate = "varimax")
    Standardized loadings (pattern matrix) based upon correlation matrix
                                 MR1   MR3   MR2   h2     u2 com
    perceived_difficulty_score -0.28 -0.40  0.14 0.25 0.7459 2.1
    numeracy_score              0.49  0.11 -0.22 0.30 0.6996 1.5
    relative_energy_use_score   0.64  0.22 -0.21 0.50 0.5017 1.5
    relative_energy_save_score  0.55  0.20 -0.23 0.39 0.6072 1.6
    accuracy                    0.91  0.14 -0.02 0.85 0.1533 1.0
    els                         0.91  0.14 -0.02 0.85 0.1533 1.0
    env_attitude                0.17  0.96 -0.18 0.99 0.0067 1.1
    env_attitude_z              0.17  0.96 -0.18 0.99 0.0067 1.1
    pol_conservatism           -0.19 -0.21  0.95 0.98 0.0199 1.2
    pol_conservatism_z         -0.19 -0.21  0.95 0.98 0.0199 1.2

                           MR1  MR3  MR2
    SS loadings           2.81 2.25 2.03
    Proportion Var        0.28 0.22 0.20
    Cumulative Var        0.28 0.51 0.71
    Proportion Explained  0.40 0.32 0.29
    Cumulative Proportion 0.40 0.71 1.00

    Mean item complexity =  1.3
    Test of the hypothesis that 3 factors are sufficient.

    df null model =  45  with the objective function =  107 with Chi Square =  30024
    df of  the model are 18  and the objective function was  NaN 

    The root mean square of the residuals (RMSR) is  0.06 
    The df corrected root mean square of the residuals is  0.1 

    The harmonic n.obs is  287 with the empirical chi square  103  with prob <  0.00000000000007 
    The total n.obs was  287  with Likelihood Chi Square =  NaN  with prob <  NaN 

    Tucker Lewis Index of factoring reliability =  NaN
    Fit based upon off diagonal values = 0.98
    Measures of factor score adequacy             
                                                       MR1  MR3  MR2
    Correlation of (regression) scores with factors   0.95 1.00 0.99
    Multiple R square of scores with factors          0.91 0.99 0.98
    Minimum correlation of possible factor scores     0.81 0.99 0.97

``` r
# 3. Analyze the relation between motivation and knowledge items
#    a. Correlation between perceived_difficulty_score and els
cor_motivation_knowledge <- cor(combined_data$perceived_difficulty_score, combined_data$els, use = "pairwise.complete.obs")
print(paste("Correlation between perceived difficulty and energy literacy:", cor_motivation_knowledge))
```

    [1] "Correlation between perceived difficulty and energy literacy: -0.268417318288949"

``` r
#    b. Correlation between env_attitude and els
cor_env_knowledge <- cor(combined_data$env_attitude, combined_data$els, use = "pairwise.complete.obs")
print(paste("Correlation between environmental attitude and energy literacy:", cor_env_knowledge))
```

    [1] "Correlation between environmental attitude and energy literacy: 0.312991297054729"

``` r
#    c. Regression analysis with els as the dependent variable and motivation variables as predictors
model <- lm(els ~ perceived_difficulty_score + env_attitude + pol_conservatism, data = combined_data)
summary(model)
```


    Call:
    lm(formula = els ~ perceived_difficulty_score + env_attitude + 
        pol_conservatism, data = combined_data)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -2.3489 -0.7106  0.0245  0.7152  2.2516 

    Coefficients:
                               Estimate Std. Error t value Pr(>|t|)   
    (Intercept)                 -0.6746     0.3692   -1.83   0.0688 . 
    perceived_difficulty_score  -0.1487     0.0623   -2.39   0.0176 * 
    env_attitude                 0.2526     0.0868    2.91   0.0039 **
    pol_conservatism            -0.0891     0.0424   -2.10   0.0364 * 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.94 on 283 degrees of freedom
    Multiple R-squared:  0.132, Adjusted R-squared:  0.123 
    F-statistic: 14.3 on 3 and 283 DF,  p-value: 0.0000000102

``` r
# 4. Visualize the relationship between perceived_difficulty_score, env_attitude_z, and els
ggplot(combined_data, aes(x = perceived_difficulty_score, y = els, color = env_attitude_z)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Energy Literacy vs. Perceived Difficulty",
       x = "Perceived Difficulty Score",
       y = "Energy Literacy Score",
       color = "Environmental Attitude") +
  theme_minimal()
```

<img src="cors1.markdown_strict_files/figure-markdown_strict/unnamed-chunk-6-2.png" width="768" />

``` r
# 5. Scree Plot and Parallel Analysis
scree(combined_data[,-1])
```

<img src="cors1.markdown_strict_files/figure-markdown_strict/unnamed-chunk-6-3.png" width="768" />

``` r
fa.parallel(combined_data[,-1], fa="both", n.iter=100, main="Parallel Analysis")
```

<img src="cors1.markdown_strict_files/figure-markdown_strict/unnamed-chunk-6-4.png" width="768" />

    Parallel analysis suggests that the number of factors =  4  and the number of components =  3 

cor_matrix
perceived_difficulty_score numeracy_score relative_energy_use_score relative_energy_save_score accuracy els env_attitude env_attitude_z
perceived_difficulty_score 1.00 -0.13 -0.24 -0.25 -0.22 -0.22 -0.39 -0.39
numeracy_score -0.13 1.00 0.41 0.34 0.34 0.34 0.20 0.20
relative_energy_use_score -0.24 0.41 1.00 0.57 0.40 0.40 0.21 0.21
relative_energy_save_score -0.25 0.34 0.57 1.00 0.36 0.36 0.22 0.22
accuracy -0.22 0.34 0.40 0.36 1.00 1.00 0.27 0.27
els -0.22 0.34 0.40 0.36 1.00 1.00 0.27 0.27
env_attitude -0.39 0.20 0.21 0.22 0.27 0.27 1.00 1.00
env_attitude_z -0.39 0.20 0.21 0.22 0.27 0.27 1.00 1.00
pol_conservatism 0.13 -0.25 -0.23 -0.22 -0.19 -0.19 -0.29 -0.29
pol_conservatism_z 0.13 -0.25 -0.23 -0.22 -0.19 -0.19 -0.29 -0.29
pol_conservatism pol_conservatism_z
perceived_difficulty_score 0.13 0.13
numeracy_score -0.25 -0.25
relative_energy_use_score -0.23 -0.23
relative_energy_save_score -0.22 -0.22
accuracy -0.19 -0.19
els -0.19 -0.19
env_attitude -0.29 -0.29
env_attitude_z -0.29 -0.29
pol_conservatism 1.00 1.00
pol_conservatism_z 1.00 1.00

print(fa_result)
Factor Analysis using method = minres
Call: fa(r = combined_data\[, -1\], nfactors = 3, rotate = "varimax")
Standardized loadings (pattern matrix) based upon correlation matrix
MR1 MR3 MR2 h2 u2 com
perceived_difficulty_score -0.23 -0.36 0.06 0.18 0.8181 1.8
numeracy_score 0.42 0.12 -0.19 0.23 0.7746 1.6
relative_energy_use_score 0.52 0.14 -0.16 0.32 0.6809 1.3
relative_energy_save_score 0.48 0.16 -0.15 0.28 0.7219 1.4
accuracy 0.92 0.12 0.00 0.87 0.1320 1.0
els 0.92 0.12 0.00 0.87 0.1320 1.0
env_attitude 0.15 0.98 -0.13 0.99 0.0051 1.1
env_attitude_z 0.15 0.98 -0.13 0.99 0.0051 1.1
pol_conservatism -0.17 -0.14 0.97 0.99 0.0138 1.1
pol_conservatism_z -0.17 -0.14 0.97 0.99 0.0138 1.1

                       MR1  MR3  MR2

SS loadings 2.53 2.17 2.00
Proportion Var 0.25 0.22 0.20
Cumulative Var 0.25 0.47 0.67
Proportion Explained 0.38 0.32 0.30
Cumulative Proportion 0.38 0.70 1.00

Mean item complexity = 1.3
Test of the hypothesis that 3 factors are sufficient.

df null model = 45 with the objective function = NaN with Chi Square = NaN
df of the model are 18 and the objective function was NaN

The root mean square of the residuals (RMSR) is 0.07
The df corrected root mean square of the residuals is 0.1

The harmonic n.obs is 586 with the empirical chi square 228 with prob \< 0.000000000000000000000000000000000000029
The total n.obs was 586 with Likelihood Chi Square = NaN with prob \< NaN

Tucker Lewis Index of factoring reliability = NaN
Fit based upon off diagonal values = 0.97
Measures of factor score adequacy  
MR1 MR3 MR2
Correlation of (regression) scores with factors 0.95 1.00 0.99
Multiple R square of scores with factors 0.91 1.00 0.99
Minimum correlation of possible factor scores 0.82 0.99 0.98

r$> # 3. Analyze the relation between motivation and knowledge items
    #    a. Correlation between perceived_difficulty_score and els
    cor_motivation_knowledge <- cor(combined_data$perceived_difficulty_score, combined_data\$els, use = "pairwise.complete.obs")

r\$\> print(paste("Correlation between perceived difficulty and energy literacy:", cor_motivation_knowledge))
\[1\] "Correlation between perceived difficulty and energy literacy: -0.221786397116371"

r$> #    b. Correlation between env_attitude and els
    cor_env_knowledge <- cor(combined_data$env_attitude, combined_data\$els, use = "pairwise.complete.obs")

r\$\> print(paste("Correlation between environmental attitude and energy literacy:", cor_env_knowledge))
\[1\] "Correlation between environmental attitude and energy literacy: 0.272737299645857"

r\$\> \# c. Regression analysis with els as the dependent variable and motivation variables as predictors
model \<- lm(els ~ perceived_difficulty_score + env_attitude + pol_conservatism, data = combined_data)

r\$\> summary(model)

Call:
lm(formula = els ~ perceived_difficulty_score + env_attitude +
pol_conservatism, data = combined_data)

Residuals:
Min 1Q Median 3Q Max
-3.171 -0.646 -0.004 0.716 2.333

Coefficients:
Estimate Std. Error t value Pr(\>\|t\|)  
(Intercept) -0.6422 0.2390 -2.69 0.0074 \*\*
perceived_difficulty_score -0.1338 0.0425 -3.15 0.0017 \*\*
env_attitude 0.2421 0.0571 4.24 0.000026 \***
pol_conservatism -0.0854 0.0289 -2.96 0.0032 **
--
Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.95 on 582 degrees of freedom
Multiple R-squared: 0.104, Adjusted R-squared: 0.0992
F-statistic: 22.5 on 3 and 582 DF, p-value: 0.0000000000000872

``` r
# Combine all survey scores into a single dataframe
combined <- attari1 %>%
  full_join(attari2_scores, by = "id") %>%
  full_join(els_scores %>% select(id, els), by = "id") %>%
  full_join(rs_scores %>% select(id, env_attitude_z, pol_conservatism_z), by = "id")

# Correlation matrix of key variables
cor_vars <- combined %>% select(
  perceived_difficulty_score, numeracy_score,
  relative_energy_use_score, relative_energy_save_score,
  els, env_attitude_z, pol_conservatism_z
)

cor_matrix <- cor(cor_vars, use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")
```

<img src="cors1.markdown_strict_files/figure-markdown_strict/unnamed-chunk-7-1.png" width="768" />

``` r
# Factor analysis to identify latent structure
fa_model <- psych::fa(cor_vars[complete.cases(cor_vars), ], nfactors = 2, 
                      rotate = "varimax", fm = "ml")
print(fa_model, cut = 0.3)
```

    Factor Analysis using method =  ml
    Call: psych::fa(r = cor_vars[complete.cases(cor_vars), ], nfactors = 2, 
        rotate = "varimax", fm = "ml")
    Standardized loadings (pattern matrix) based upon correlation matrix
                                 ML2   ML1   h2    u2 com
    perceived_difficulty_score -0.31 -0.42 0.27 0.727 1.9
    numeracy_score              0.52       0.30 0.702 1.2
    relative_energy_use_score   0.85       0.78 0.220 1.1
    relative_energy_save_score  0.75       0.62 0.384 1.2
    els                         0.53       0.35 0.652 1.4
    env_attitude_z                    0.98 0.98 0.018 1.0
    pol_conservatism_z         -0.31 -0.38 0.24 0.759 1.9

                           ML2  ML1
    SS loadings           2.06 1.48
    Proportion Var        0.29 0.21
    Cumulative Var        0.29 0.51
    Proportion Explained  0.58 0.42
    Cumulative Proportion 0.58 1.00

    Mean item complexity =  1.4
    Test of the hypothesis that 2 factors are sufficient.

    df null model =  21  with the objective function =  2 with Chi Square =  581
    df of  the model are 8  and the objective function was  0.05 

    The root mean square of the residuals (RMSR) is  0.03 
    The df corrected root mean square of the residuals is  0.05 

    The harmonic n.obs is  287 with the empirical chi square  12  with prob <  0.13 
    The total n.obs was  287  with Likelihood Chi Square =  15  with prob <  0.066 

    Tucker Lewis Index of factoring reliability =  0.97
    RMSEA index =  0.054  and the 90 % confidence intervals are  0 0.097
    BIC =  -31
    Fit based upon off diagonal values = 0.99
    Measures of factor score adequacy             
                                                       ML2  ML1
    Correlation of (regression) scores with factors   0.92 0.99
    Multiple R square of scores with factors          0.84 0.98
    Minimum correlation of possible factor scores     0.68 0.96

``` r
# Perceived Difficulty vs Knowledge
ggplot(combined, aes(perceived_difficulty_score, els)) +
  geom_point(alpha = 0.5) + geom_smooth(method = "lm") +
  labs(x = "Perceived Difficulty (z)", y = "Energy Literacy (z)")
```

<img src="cors1.markdown_strict_files/figure-markdown_strict/unnamed-chunk-7-2.png" width="768" />

``` r
# Environmental Attitude vs Knowledge
ggplot(combined, aes(env_attitude_z, els)) +
  geom_point(alpha = 0.5) + geom_smooth(method = "lm") +
  labs(x = "Pro-Environmental Attitude (z)", y = "Energy Literacy (z)")
```

<img src="cors1.markdown_strict_files/figure-markdown_strict/unnamed-chunk-7-3.png" width="768" />

``` r
# Predict Energy Literacy from Motivation
model_els <- lm(els ~ perceived_difficulty_score + env_attitude_z, 
                data = combined)
summary(model_els)
```


    Call:
    lm(formula = els ~ perceived_difficulty_score + env_attitude_z, 
        data = combined)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -2.1928 -0.6999 -0.0498  0.7130  2.2329 

    Coefficients:
                                            Estimate            Std. Error t value
    (Intercept)                -0.000000000000000151  0.055621684128591892    0.00
    perceived_difficulty_score -0.159761266067761393  0.062426844659397948   -2.56
    env_attitude_z              0.240945986236688586  0.062426844659397997    3.86
                               Pr(>|t|)    
    (Intercept)                 1.00000    
    perceived_difficulty_score  0.01101 *  
    env_attitude_z              0.00014 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.94 on 284 degrees of freedom
    Multiple R-squared:  0.118, Adjusted R-squared:  0.112 
    F-statistic: 19.1 on 2 and 284 DF,  p-value: 0.0000000172

``` r
# Predict Numeracy from Motivation
model_numeracy <- lm(numeracy_score ~ perceived_difficulty_score + 
                       env_attitude_z, data = combined)
summary(model_numeracy)
```


    Call:
    lm(formula = numeracy_score ~ perceived_difficulty_score + env_attitude_z, 
        data = combined)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -2.1369 -0.6210  0.0337  0.8865  1.6788 

    Coefficients:
                                           Estimate           Std. Error t value
    (Intercept)                 0.00000000000000176  0.05737104617084357    0.00
    perceived_difficulty_score -0.11638282529783350  0.06439023635052721   -1.81
    env_attitude_z              0.17373106915498060  0.06439023635052725    2.70
                               Pr(>|t|)   
    (Intercept)                  1.0000   
    perceived_difficulty_score   0.0717 . 
    env_attitude_z               0.0074 **
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.97 on 284 degrees of freedom
    Multiple R-squared:  0.062, Adjusted R-squared:  0.0554 
    F-statistic: 9.38 on 2 and 284 DF,  p-value: 0.000114

``` r
# Create composite scores
combined <- combined %>% mutate(
  composite_knowledge = scale(
    numeracy_score + relative_energy_use_score + 
    relative_energy_save_score + els
  ),
  composite_motivation = scale(
    (-perceived_difficulty_score) + env_attitude_z  # Reverse-code difficulty
  )
)

# Composite correlation
cor.test(combined$composite_knowledge, combined$composite_motivation, 
         use = "pairwise.complete.obs")
```


        Pearson's product-moment correlation

    data:  combined$composite_knowledge and combined$composite_motivation
    t = 8, df = 285, p-value = 0.000000000000006
    alternative hypothesis: true correlation is not equal to 0
    95 percent confidence interval:
     0.34 0.53
    sample estimates:
     cor 
    0.44 

cor_matrix
perceived_difficulty_score numeracy_score relative_energy_use_score relative_energy_save_score els env_attitude_z pol_conservatism_z
perceived_difficulty_score 1.00 -0.13 -0.24 -0.25 -0.22 -0.39 0.13
numeracy_score -0.13 1.00 0.41 0.34 0.34 0.20 -0.25
relative_energy_use_score -0.24 0.41 1.00 0.57 0.40 0.21 -0.23
relative_energy_save_score -0.25 0.34 0.57 1.00 0.36 0.22 -0.22
els -0.22 0.34 0.40 0.36 1.00 0.27 -0.19
env_attitude_z -0.39 0.20 0.21 0.22 0.27 1.00 -0.29
pol_conservatism_z 0.13 -0.25 -0.23 -0.22 -0.19 -0.29 1.00

print(fa_model, cut = 0.3)
Factor Analysis using method = ml
Call: psych::fa(r = cor_vars\[complete.cases(cor_vars), \], nfactors = 2,
rotate = "varimax", fm = "ml")
Standardized loadings (pattern matrix) based upon correlation matrix
ML2 ML1 h2 u2 com
perceived_difficulty_score -0.37 0.19 0.807 1.7
numeracy_score 0.51 0.28 0.720 1.2
relative_energy_use_score 0.78 0.63 0.374 1.1
relative_energy_save_score 0.69 0.50 0.497 1.1
els 0.49 0.29 0.709 1.4
env_attitude_z 0.99 1.00 0.005 1.0
pol_conservatism_z 0.14 0.860 2.0

                       ML2  ML1

SS loadings 1.71 1.31
Proportion Var 0.24 0.19
Cumulative Var 0.24 0.43
Proportion Explained 0.57 0.43
Cumulative Proportion 0.57 1.00

Mean item complexity = 1.4
Test of the hypothesis that 2 factors are sufficient.

df null model = 21 with the objective function = 1.3 with Chi Square = 760
df of the model are 8 and the objective function was 0.03

The root mean square of the residuals (RMSR) is 0.03
The df corrected root mean square of the residuals is 0.04

The harmonic n.obs is 586 with the empirical chi square 18 with prob \< 0.025
The total n.obs was 586 with Likelihood Chi Square = 16 with prob \< 0.036

Tucker Lewis Index of factoring reliability = 0.97
RMSEA index = 0.042 and the 90 % confidence intervals are 0.01 0.072
BIC = -35
Fit based upon off diagonal values = 0.99
Measures of factor score adequacy  
ML2 ML1
Correlation of (regression) scores with factors 0.87 1.00
Multiple R square of scores with factors 0.77 0.99
Minimum correlation of possible factor scores 0.53 0.99

summary(model_els)

Call:
lm(formula = els ~ perceived_difficulty_score + env_attitude_z,
data = combined)

Residuals:
Min 1Q Median 3Q Max
-3.0786 -0.6787 0.0272 0.7021 2.3542

Coefficients:
Estimate Std. Error t value Pr(\>\|t\|)  
(Intercept) 0.000000000000000221 0.039466198562163532 0.00 1.0000  
perceived_difficulty_score -0.137043156114381365 0.042805101308446926 -3.20 0.0014 \*\*
env_attitude_z 0.219932562179007085 0.042805101308446947 5.14 0.00000038 \*\*\*
--
Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.95 on 583 degrees of freedom
Multiple R-squared: 0.0904, Adjusted R-squared: 0.0873
F-statistic: 29 on 2 and 583 DF, p-value: 0.00000000000102

summary(model_numeracy)

Call:
lm(formula = numeracy_score ~ perceived_difficulty_score + env_attitude_z,
data = combined)

Residuals:
Min 1Q Median 3Q Max
-2.5895 -0.4958 0.0285 0.4315 2.0590

Coefficients:
Estimate Std. Error t value Pr(\>\|t\|)  
(Intercept) 0.0000000000000144 0.0404741361250743 0.00 1.00  
perceived_difficulty_score -0.0660112042229361 0.0438983119814999 -1.50 0.13  
env_attitude_z 0.1735929007614459 0.0438983119815000 3.95 0.000086 \*\*\*
--
Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.98 on 583 degrees of freedom
Multiple R-squared: 0.0433, Adjusted R-squared: 0.04
F-statistic: 13.2 on 2 and 583 DF, p-value: 0.00000247

\# Composite correlation
cor.test(combined$composite_knowledge, combined$composite_motivation,
use = "pairwise.complete.obs")

        Pearson's product-moment correlation

data: combined$composite_knowledge and combined$composite_motivation
t = 9, df = 584, p-value \<0.0000000000000002
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
0.28 0.42
sample estimates:
cor
0.35

``` r
combined_df <- attari1 %>%
  full_join(attari2_scores, by = "id") %>%
  full_join(els_scores, by = "id") %>%
  full_join(rs_scores, by = "id")

# View the first few rows
head(combined_df)
```

    # A tibble: 6 × 11
         id perceived_difficulty_score[,1] numeracy_score[,1] relative_energy_use_…¹
      <int>                          <dbl>              <dbl>                  <dbl>
    1     1                          0.515              0.982                0.760  
    2     2                         -0.481              0.982                0.00153
    3     3                          1.90              -1.87                -1.24   
    4     4                         -0.703             -1.26                 0.953  
    5     5                          0.791              0.982               -0.0468 
    6     6                         -0.260              0.982                0.899  
    # ℹ abbreviated name: ¹​relative_energy_use_score[,1]
    # ℹ 7 more variables: relative_energy_save_score <dbl[,1]>, accuracy <dbl>,
    #   els <dbl>, env_attitude <dbl>, env_attitude_z <dbl>,
    #   pol_conservatism <dbl>, pol_conservatism_z <dbl>

``` r
# 2) Examine correlations among the key subscales:
#    - Perceived Difficulty (motivation)
#    - Numeracy (knowledge)
#    - Relative Energy Usage (knowledge)
#    - Relative Energy Savings (knowledge)
#    - Energy Literacy (knowledge)
#    - Environmental Attitude (motivation)
#    - Political Conservatism (motivation)

key_vars <- c(
  "perceived_difficulty_score",
  "numeracy_score",
  "relative_energy_use_score",
  "relative_energy_save_score",
  "els",  # Standardized ELS accuracy
  "env_attitude",
  "pol_conservatism"
)

# Create a correlation matrix
cor_matrix <- combined_df %>%
  select(all_of(key_vars)) %>%
  cor(use = "pairwise.complete.obs")

# Print correlation matrix
print(cor_matrix, digits = 2)
```

                               perceived_difficulty_score numeracy_score
    perceived_difficulty_score                       1.00          -0.19
    numeracy_score                                  -0.19           1.00
    relative_energy_use_score                       -0.37           0.47
    relative_energy_save_score                      -0.34           0.41
    els                                             -0.27           0.42
    env_attitude                                    -0.45           0.23
    pol_conservatism                                 0.26          -0.31
                               relative_energy_use_score relative_energy_save_score
    perceived_difficulty_score                     -0.37                      -0.34
    numeracy_score                                  0.47                       0.41
    relative_energy_use_score                       1.00                       0.70
    relative_energy_save_score                      0.70                       1.00
    els                                             0.51                       0.42
    env_attitude                                    0.33                       0.31
    pol_conservatism                               -0.33                      -0.33
                                 els env_attitude pol_conservatism
    perceived_difficulty_score -0.27        -0.45             0.26
    numeracy_score              0.42         0.23            -0.31
    relative_energy_use_score   0.51         0.33            -0.33
    relative_energy_save_score  0.42         0.31            -0.33
    els                         1.00         0.31            -0.25
    env_attitude                0.31         1.00            -0.42
    pol_conservatism           -0.25        -0.42             1.00

``` r
corrplot::corrplot(cor_matrix, method = "ellipse", type = "upper", tl.srt = 45)
```

<img src="cors1.markdown_strict_files/figure-markdown_strict/unnamed-chunk-8-1.png" width="768" />

``` r
# 3) Assess whether these subscales share underlying structure via Factor Analysis (EFA)
#    For a quick exploration, we can request 2-3 factors and see how items load.

# Extract just the columns for the factor analysis
fa_data <- combined_df %>%
  select(all_of(key_vars)) %>%
  na.omit()

# Number of factors to extract determined by parallel analysis or theoretical reasons; 
# here we try with 2 factors for demonstration:
set.seed(123)
fa_results <- psych::fa(r = fa_data, nfactors = 2, rotate = "varimax", fm = "ml")
print(fa_results, digits = 2, sort = TRUE)
```

    Factor Analysis using method =  ml
    Call: psych::fa(r = fa_data, nfactors = 2, rotate = "varimax", fm = "ml")
    Standardized loadings (pattern matrix) based upon correlation matrix
                               item   ML2   ML1   h2    u2 com
    relative_energy_use_score     3  0.85  0.23 0.78 0.220 1.1
    relative_energy_save_score    4  0.75  0.22 0.62 0.384 1.2
    els                           5  0.53  0.25 0.35 0.652 1.4
    numeracy_score                2  0.52  0.16 0.30 0.702 1.2
    env_attitude                  6  0.13  0.98 0.98 0.018 1.0
    perceived_difficulty_score    1 -0.31 -0.42 0.27 0.727 1.9
    pol_conservatism              7 -0.31 -0.38 0.24 0.759 1.9

                           ML2  ML1
    SS loadings           2.06 1.48
    Proportion Var        0.29 0.21
    Cumulative Var        0.29 0.51
    Proportion Explained  0.58 0.42
    Cumulative Proportion 0.58 1.00

    Mean item complexity =  1.4
    Test of the hypothesis that 2 factors are sufficient.

    df null model =  21  with the objective function =  2 with Chi Square =  581
    df of  the model are 8  and the objective function was  0.05 

    The root mean square of the residuals (RMSR) is  0.03 
    The df corrected root mean square of the residuals is  0.05 

    The harmonic n.obs is  287 with the empirical chi square  12  with prob <  0.13 
    The total n.obs was  287  with Likelihood Chi Square =  15  with prob <  0.066 

    Tucker Lewis Index of factoring reliability =  0.97
    RMSEA index =  0.054  and the 90 % confidence intervals are  0 0.097
    BIC =  -31
    Fit based upon off diagonal values = 0.99
    Measures of factor score adequacy             
                                                       ML2  ML1
    Correlation of (regression) scores with factors   0.92 0.99
    Multiple R square of scores with factors          0.84 0.98
    Minimum correlation of possible factor scores     0.68 0.96

``` r
# 4) Inspect the specific relationship between motivation and knowledge items.
#    For instance, do an OLS regression testing if environmental attitude (motivation)
#    predicts Energy Literacy (knowledge):

model_env_els <- lm(els ~ env_attitude, data = combined_df)
summary(model_env_els)
```


    Call:
    lm(formula = els ~ env_attitude, data = combined_df)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -2.2841 -0.7300 -0.0017  0.7265  2.1682 

    Coefficients:
                 Estimate Std. Error t value    Pr(>|t|)    
    (Intercept)   -1.4739     0.2708   -5.44 0.000000113 ***
    env_attitude   0.4105     0.0738    5.56 0.000000061 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.95 on 285 degrees of freedom
    Multiple R-squared:  0.098, Adjusted R-squared:  0.0948 
    F-statistic:   31 on 1 and 285 DF,  p-value: 0.000000061

``` r
# Another regression: see if perceived difficulty (motivation) predicts numeracy (knowledge)
model_diff_num <- lm(numeracy_score ~ perceived_difficulty_score, data = combined_df)
summary(model_diff_num)
```


    Call:
    lm(formula = numeracy_score ~ perceived_difficulty_score, data = combined_df)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -2.3475 -0.6219 -0.0402  0.9201  1.4913 

    Coefficients:
                                           Estimate           Std. Error t value
    (Intercept)                 0.00000000000000174  0.05799966257793866    0.00
    perceived_difficulty_score -0.19472790345158641  0.05810097210943094   -3.35
                               Pr(>|t|)    
    (Intercept)                 1.00000    
    perceived_difficulty_score  0.00091 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.98 on 285 degrees of freedom
    Multiple R-squared:  0.0379,    Adjusted R-squared:  0.0345 
    F-statistic: 11.2 on 1 and 285 DF,  p-value: 0.000912

``` r
# 5) Example of a more complex model: 
#    Predict knowledge (ELS) from both environment attitude and numeracy, controlling for political conservatism

model_complex <- lm(els ~ env_attitude + pol_conservatism + numeracy_score, data = combined_df)
summary(model_complex)
```


    Call:
    lm(formula = els ~ env_attitude + pol_conservatism + numeracy_score, 
        data = combined_df)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -2.7036 -0.6392  0.0051  0.6859  2.2377 

    Coefficients:
                     Estimate Std. Error t value      Pr(>|t|)    
    (Intercept)       -0.9072     0.3312   -2.74        0.0065 ** 
    env_attitude       0.2773     0.0757    3.66        0.0003 ***
    pol_conservatism  -0.0338     0.0410   -0.83        0.4094    
    numeracy_score     0.3596     0.0551    6.52 0.00000000032 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.88 on 283 degrees of freedom
    Multiple R-squared:  0.23,  Adjusted R-squared:  0.222 
    F-statistic: 28.2 on 3 and 283 DF,  p-value: 0.000000000000000563

``` r
# 6) Quick plots to visualize the relationship between motivation and knowledge
ggplot(combined_df, aes(x = env_attitude, y = els)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Energy Literacy vs. Environmental Attitude",
    x = "Environmental Attitude (higher = more pro-environment)",
    y = "Energy Literacy (standardized)"
  ) +
  theme_minimal()
```

<img src="cors1.markdown_strict_files/figure-markdown_strict/unnamed-chunk-8-2.png" width="768" />

``` r
ggplot(combined_df, aes(x = perceived_difficulty_score, y = numeracy_score)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Numeracy vs. Perceived Difficulty",
    x = "Perceived Difficulty (standardized)",
    y = "Numeracy (standardized)"
  ) +
  theme_minimal()
```

<img src="cors1.markdown_strict_files/figure-markdown_strict/unnamed-chunk-8-3.png" width="768" />

print(cor_matrix, digits = 2)
perceived_difficulty_score numeracy_score relative_energy_use_score relative_energy_save_score els env_attitude pol_conservatism
perceived_difficulty_score 1.00 -0.13 -0.24 -0.25 -0.22 -0.39 0.13
numeracy_score -0.13 1.00 0.41 0.34 0.34 0.20 -0.25
relative_energy_use_score -0.24 0.41 1.00 0.57 0.40 0.21 -0.23
relative_energy_save_score -0.25 0.34 0.57 1.00 0.36 0.22 -0.22
els -0.22 0.34 0.40 0.36 1.00 0.27 -0.19
env_attitude -0.39 0.20 0.21 0.22 0.27 1.00 -0.29
pol_conservatism 0.13 -0.25 -0.23 -0.22 -0.19 -0.29 1.00

Factor Analysis using method = ml
Call: psych::fa(r = fa_data, nfactors = 2, rotate = "varimax", fm = "ml")
Standardized loadings (pattern matrix) based upon correlation matrix
item ML2 ML1 h2 u2 com
relative_energy_use_score 3 0.78 0.14 0.63 0.374 1.1
relative_energy_save_score 4 0.69 0.16 0.50 0.497 1.1
numeracy_score 2 0.51 0.15 0.28 0.720 1.2
els 5 0.49 0.23 0.29 0.709 1.4
pol_conservatism 7 -0.27 -0.26 0.14 0.860 2.0
env_attitude 6 0.09 0.99 1.00 0.005 1.0
perceived_difficulty_score 1 -0.24 -0.37 0.19 0.807 1.7

                       ML2  ML1

SS loadings 1.71 1.31
Proportion Var 0.24 0.19
Cumulative Var 0.24 0.43
Proportion Explained 0.57 0.43
Cumulative Proportion 0.57 1.00

Mean item complexity = 1.4
Test of the hypothesis that 2 factors are sufficient.

df null model = 21 with the objective function = 1.3 with Chi Square = 760
df of the model are 8 and the objective function was 0.03

The root mean square of the residuals (RMSR) is 0.03
The df corrected root mean square of the residuals is 0.04

The harmonic n.obs is 586 with the empirical chi square 18 with prob \< 0.025
The total n.obs was 586 with Likelihood Chi Square = 16 with prob \< 0.036

Tucker Lewis Index of factoring reliability = 0.97
RMSEA index = 0.042 and the 90 % confidence intervals are 0.01 0.072
BIC = -35
Fit based upon off diagonal values = 0.99
Measures of factor score adequacy  
ML2 ML1
Correlation of (regression) scores with factors 0.87 1.00
Multiple R square of scores with factors 0.77 0.99
Minimum correlation of possible factor scores 0.53 0.99

summary(model_env_els)

Call:
lm(formula = els ~ env_attitude, data = combined_df)

Residuals:
Min 1Q Median 3Q Max
-2.7955 -0.6605 -0.0334 0.7667 2.1901

Coefficients:
Estimate Std. Error t value Pr(\>\|t\|)  
(Intercept) -1.2680 0.1893 -6.70 0.000000000050 ***
env_attitude 0.3539 0.0517 6.85 0.000000000019 ***
--
Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.96 on 584 degrees of freedom
Multiple R-squared: 0.0744, Adjusted R-squared: 0.0728
F-statistic: 46.9 on 1 and 584 DF, p-value: 0.0000000000187

summary(model_diff_num)

Call:
lm(formula = numeracy_score ~ perceived_difficulty_score, data = combined_df)

Residuals:
Min 1Q Median 3Q Max
-2.5380 -0.5194 0.0836 0.3006 1.8322

Coefficients:
Estimate Std. Error t value Pr(\>\|t\|)  
(Intercept) 0.0000000000000144 0.0409782243863863 0.00 1.0000  
perceived_difficulty_score -0.1328990942184530 0.0410132335549985 -3.24 0.0013 \*\*
--
Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.99 on 584 degrees of freedom
Multiple R-squared: 0.0177, Adjusted R-squared: 0.016
F-statistic: 10.5 on 1 and 584 DF, p-value: 0.00126

summary(model_complex)

Call:
lm(formula = els ~ env_attitude + pol_conservatism + numeracy_score,
data = combined_df)

Residuals:
Min 1Q Median 3Q Max
-2.9385 -0.6354 0.0088 0.6822 2.1663

Coefficients:
Estimate Std. Error t value Pr(\>\|t\|)  
(Intercept) -0.7996 0.2201 -3.63 0.00031 ***
env_attitude 0.2563 0.0518 4.95 0.0000009827500 ***
pol_conservatism -0.0451 0.0285 -1.58 0.11393  
numeracy_score 0.2859 0.0395 7.23 0.0000000000015 \*\*\*
--
Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.92 on 582 degrees of freedom
Multiple R-squared: 0.164, Adjusted R-squared: 0.159
F-statistic: 38 on 3 and 582 DF, p-value: \<0.0000000000000002
