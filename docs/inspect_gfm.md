# Initial Data Inspect

2025-01-19

``` r
pacman::p_load(dplyr,purrr,tidyr,here, haven,tibble,ggplot2,ggh4x,lme4,knitr,gt,flextable,ggh4x,psych,corrplot)
options(digits=2, scipen=999, dplyr.summarise.inform=FALSE)

draw <- readRDS(here("data","draw.rds"))
dinst <- readRDS(here("data","dinst.rds"))


# Attari Energy Survey (Part 1)
aes1 <- draw |> select(id,ATT01:ATT18)
aes2 <- dinst |> select(id,ATT01:ATT18)

att_useSave <- draw |> select(id,ATT19:ATT33)
att_useSave2 <- dinst |> select(id,ATT19:ATT33)


els1 <- draw |> select(id,ELS01:ELS08)
els2 <- dinst |> select(id,ELS01:ELS08)

recs1 <- draw |> select(id,RECS01:RECS16)
recs2 <- dinst |> select(id,RECS01:RECS16)

lss1_1 <- draw |> select(id,LAN01:LAN09)
lss1_2 <- dinst |> select(id,LAN01:LAN09)

lss2_1 <- draw |> select(id,LAN10:LAN87)
lss2_2 <- dinst |> select(id,LAN10:LAN87)


nep1 <- draw |> select(id,NEP01:NEP15)
nep2 <- dinst |> select(id,NEP01:NEP15)

demo1 <- draw |> select(id,DEM01:DEM30)
demo2 <- dinst |> select(id,DEM01:DEM30)


rs1 <- draw |> select(id,RS01:RS06)
rs2 <- dinst |> select(id,RS01:RS06)
```

# Attari Energy Survey (Part 1)

- **A. Perceived Difficulty Items (ATT01-ATT15):**

  - **Description:** Measures how easy or hard it would be for
    participants to make various changes related to energy usage. Items
    are phrased as actions such as buying a fuel-efficient car, or
    changing light bulbs.
  - **Coding Scheme:**
    - 1 = “Do it already”
    - 2 = “Extremely easy”
    - 3 = “Very easy”
    - 4 = “Somewhat easy”
    - 5 = “Neither easy nor hard”
    - 6 = “Somewhat hard”
    - 7 = “Very hard”
    - 8 = “Extremely hard”
    - 9 = “Not applicable”
  - **Specific Items:**
    - ATT01: Buying a more fuel-efficient automobile
    - ATT02: Carpooling with one other person to work
    - ATT03: Replacing poorly insulated windows
    - ATT04: Cutting highway speed
    - ATT05: More efficient heating unit
    - ATT06: Turning down the thermostat in the day and night
    - ATT07: Turning up the thermostat on air conditioner in the summer
    - ATT08: Tuning up the car twice a year
    - ATT09: Replacing incandescent bulbs with CFLs
    - ATT10: Turning up the refrigerator thermostat
    - ATT11: Turning down the freezer thermostat
    - ATT12: Drying clothes on a clothes line
    - ATT13: Watching less TV
    - ATT14: Installing a more efficient washer
    - ATT15: Changing washer temperature settings

- **B. Numeracy Questions (ATT16-ATT18):**

  - **Description:** Measures numerical literacy through probability
    questions.
  - **Coding Scheme:** Text box entry where participants are asked to
    enter numeric answers with no other text. Correct answers are below.
  - **Specific Items:**
    - ATT16: Probability of coin flips (correct answer: 500)
    - ATT17: Probability of winning lottery (correct answer: 10)
    - ATT18: Percentage of winning car sweepstakes (correct answer:
      0.1%)

head(aes1) \|\> kable() \| id\| ATT01\| ATT02\| ATT03\| ATT04\| ATT05\|
ATT06\| ATT07\| ATT08\| ATT09\| ATT10\| ATT11\| ATT12\| ATT13\| ATT14\|
ATT15\| ATT16\| ATT17\| ATT18\|
\|–:\|—–:\|—–:\|—–:\|—–:\|—–:\|—–:\|—–:\|—–:\|—–:\|—–:\|—–:\|—–:\|—–:\|—–:\|—–:\|—–:\|—–:\|—–:\|
\| 1\| 6\| 3\| 5\| 6\| 6\| 5\| 5\| 5\| 4\| 2\| 5\| 3\| 5\| 5\| 4\| 500\|
10\| 0.1\| \| 2\| 7\| 2\| 5\| 1\| 7\| 1\| 1\| 4\| 4\| 4\| 5\| 1\| 7\|
1\| 1\| 500\| 10\| 0.1\| \| 3\| 7\| 6\| 8\| 6\| 8\| 6\| 8\| 6\| 5\| 7\|
5\| 7\| 5\| 4\| 6\| 900\| 250\| 350.0\| \| 4\| 6\| 5\| 7\| 3\| 6\| 1\|
1\| 5\| 1\| 2\| 6\| 1\| 1\| 1\| 1\| 500\| 100\| 10.0\| \| 5\| 5\| 6\|
4\| 5\| 4\| 5\| 6\| 6\| 5\| 4\| 5\| 6\| 4\| 5\| 4\| 500\| 10\| 0.1\| \|
6\| 6\| 4\| 6\| 2\| 8\| 1\| 1\| 3\| 1\| 5\| 3\| 3\| 6\| 1\| 5\| 500\|
10\| 0.1\|

``` r
aes_combined <- bind_rows(aes1, aes2)

diff_items <- aes_combined %>% 
  select(id, ATT01:ATT15)

diff_items_clean <- diff_items %>%
  mutate(across(ATT01:ATT15, ~ ifelse(. == 9, NA, .)))

pd_alpha <- psych::alpha(diff_items_clean %>% select(-id), check.keys = TRUE)
pd_alpha
```


    Reliability analysis   
    Call: psych::alpha(x = diff_items_clean %>% select(-id), check.keys = TRUE)

      raw_alpha std.alpha G6(smc) average_r S/N    ase mean  sd median_r
          0.84      0.84    0.86      0.26 5.3 0.0098  3.9 1.1     0.24

        95% confidence boundaries 
             lower alpha upper
    Feldt     0.82  0.84  0.86
    Duhachek  0.82  0.84  0.86

     Reliability if an item is dropped:
          raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
    ATT01      0.84      0.84    0.86      0.27 5.2   0.0099 0.021  0.26
    ATT02      0.84      0.85    0.87      0.28 5.5   0.0096 0.019  0.28
    ATT03      0.83      0.83    0.85      0.27 5.0   0.0102 0.020  0.26
    ATT04      0.83      0.83    0.86      0.26 5.0   0.0104 0.021  0.24
    ATT05      0.83      0.84    0.85      0.27 5.1   0.0101 0.020  0.27
    ATT06      0.82      0.83    0.85      0.25 4.8   0.0108 0.019  0.23
    ATT07      0.82      0.83    0.85      0.26 4.8   0.0107 0.021  0.23
    ATT08      0.83      0.83    0.85      0.26 4.9   0.0105 0.021  0.23
    ATT09      0.82      0.83    0.85      0.25 4.8   0.0107 0.018  0.23
    ATT10      0.82      0.82    0.85      0.25 4.7   0.0108 0.020  0.23
    ATT11      0.84      0.84    0.87      0.28 5.4   0.0096 0.020  0.28
    ATT12      0.83      0.83    0.86      0.26 5.0   0.0103 0.021  0.24
    ATT13      0.83      0.83    0.86      0.26 5.0   0.0104 0.022  0.23
    ATT14      0.82      0.83    0.85      0.25 4.7   0.0107 0.018  0.23
    ATT15      0.82      0.83    0.85      0.25 4.7   0.0107 0.017  0.23

     Item statistics 
            n raw.r std.r r.cor r.drop mean  sd
    ATT01 573  0.46  0.45  0.39   0.35  4.9 2.1
    ATT02 532  0.35  0.35  0.27   0.24  5.0 2.0
    ATT03 574  0.53  0.52  0.49   0.44  5.2 2.0
    ATT04 573  0.55  0.56  0.51   0.46  3.7 2.0
    ATT05 568  0.51  0.50  0.47   0.41  5.3 2.1
    ATT06 578  0.66  0.64  0.62   0.56  3.4 2.2
    ATT07 575  0.63  0.62  0.60   0.54  3.8 2.3
    ATT08 586  0.59  0.60  0.56   0.51  3.5 1.8
    ATT09 586  0.64  0.65  0.63   0.56  2.9 1.9
    ATT10 586  0.67  0.68  0.66   0.60  3.2 1.8
    ATT11 586  0.37  0.36  0.28   0.26  4.7 2.0
    ATT12 586  0.54  0.54  0.49   0.45  3.4 2.0
    ATT13 586  0.57  0.56  0.52   0.47  4.3 2.3
    ATT14 586  0.65  0.65  0.64   0.57  2.7 1.9
    ATT15 586  0.65  0.66  0.66   0.58  2.7 1.8

    Non missing response frequency for each item
             1    2    3    4    5    6    7    8 miss
    ATT01 0.10 0.07 0.09 0.17 0.07 0.25 0.14 0.11 0.02
    ATT02 0.04 0.09 0.10 0.20 0.11 0.19 0.14 0.13 0.09
    ATT03 0.08 0.04 0.08 0.17 0.08 0.28 0.14 0.13 0.02
    ATT04 0.12 0.23 0.17 0.16 0.09 0.12 0.07 0.05 0.02
    ATT05 0.07 0.05 0.08 0.14 0.11 0.22 0.19 0.15 0.03
    ATT06 0.30 0.13 0.12 0.15 0.05 0.11 0.07 0.05 0.01
    ATT07 0.22 0.12 0.15 0.11 0.10 0.14 0.09 0.07 0.02
    ATT08 0.20 0.11 0.18 0.22 0.10 0.15 0.02 0.02 0.00
    ATT09 0.34 0.16 0.16 0.14 0.06 0.08 0.03 0.02 0.00
    ATT10 0.19 0.22 0.21 0.16 0.10 0.08 0.03 0.02 0.00
    ATT11 0.10 0.07 0.10 0.17 0.11 0.24 0.13 0.07 0.00
    ATT12 0.22 0.15 0.19 0.16 0.09 0.10 0.04 0.04 0.00
    ATT13 0.22 0.06 0.10 0.10 0.11 0.24 0.11 0.06 0.00
    ATT14 0.37 0.19 0.15 0.10 0.09 0.05 0.03 0.02 0.00
    ATT15 0.31 0.27 0.16 0.09 0.07 0.07 0.02 0.02 0.00

``` r
item_stats <- diff_items_clean %>%
  select(-id) %>%
  summarise(across(everything(), 
                   list(Mean = ~ mean(.x, na.rm = TRUE),
                        SD   = ~ sd(.x, na.rm = TRUE),
                        N    = ~ sum(!is.na(.x))))) 

item_stats_long <- tidyr::pivot_longer(item_stats,
                                       cols = everything(),
                                       names_to = c("Item", ".value"),
                                       names_pattern = "(ATT\\d+)_([^_]+)$")
item_stats_long
```

    # A tibble: 15 × 4
       Item   Mean    SD     N
       <chr> <dbl> <dbl> <int>
     1 ATT01  4.87  2.14   573
     2 ATT02  4.98  2.01   532
     3 ATT03  5.16  2.02   574
     4 ATT04  3.74  2.01   573
     5 ATT05  5.33  2.05   568
     6 ATT06  3.39  2.23   578
     7 ATT07  3.83  2.27   575
     8 ATT08  3.54  1.84   586
     9 ATT09  2.87  1.88   586
    10 ATT10  3.22  1.81   586
    11 ATT11  4.75  2.05   586
    12 ATT12  3.41  2.00   586
    13 ATT13  4.29  2.30   586
    14 ATT14  2.73  1.88   586
    15 ATT15  2.73  1.79   586

``` r
numeracy_scored <- aes_combined %>%
  mutate(
    ATT16_correct = ifelse(ATT16 == 500, 1, 0),
    ATT17_correct = ifelse(ATT17 == 10, 1, 0),
    # Note: Some participants might enter 0.1, 0.1%, or 0.001 (depending on instructions).
    # Adjust the condition accordingly:
    ATT18_correct = ifelse(ATT18 == 0.1, 1, 0)
  )

numeracy_scored %>%
  summarise(across(c(ATT16_correct, ATT17_correct, ATT18_correct), mean, na.rm=TRUE)) 
```

    # A tibble: 1 × 3
      ATT16_correct ATT17_correct ATT18_correct
              <dbl>         <dbl>         <dbl>
    1         0.811         0.756         0.232

``` r
# version of plot where each question has it's own facet, with the question text as the facet label: 
diff_items_clean_long <- diff_items_clean %>%
  pivot_longer(cols = ATT01:ATT15, names_to = "Item", values_to = "Response") %>%
  mutate(Item = recode(Item, !!!setNames(c(
    "ATT01" = "Buying a more fuel-efficient automobile",
    "ATT02" = "Carpooling with one other person to work",
    "ATT03" = "Replacing poorly insulated windows",
    "ATT04" = "Cutting highway speed",
    "ATT05" = "More efficient heating unit",
    "ATT06" = "Turning down the thermostat in the day and night",
    "ATT07" = "Turning up the thermostat on air conditioner in the summer",
    "ATT08" = "Tuning up the car twice a year",
    "ATT09" = "Replacing incandescent bulbs with CFLs",
    "ATT10" = "Turning up the refrigerator thermostat",
    "ATT11" = "Turning down the freezer thermostat",
    "ATT12" = "Drying clothes on a clothes line",
    "ATT13" = "Watching less TV",
    "ATT14" = "Installing a more efficient washer",
    "ATT15" = "Changing washer temperature settings"
  ), c("ATT01", "ATT02", "ATT03", "ATT04", "ATT05", "ATT06", "ATT07", "ATT08", "ATT09", "ATT10", "ATT11", "ATT12", "ATT13", "ATT14", "ATT15"))))

ggplot(diff_items_clean_long, aes(x = Response)) +
    geom_histogram(binwidth = 1, fill = "darkgreen", color = "black") +
    facet_wrap(~Item, scales = "free_y") +
    labs(title = "Distribution of Perceived Difficulty Items",
         x = "Response (1=Do it already to 8=Extremely hard)",
         y = "Frequency") +
    theme_minimal()


# correlation matrix plot
diff_items_clean %>%
  select(-id) %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot::corrplot(method = "number", type = "upper", tl.col = "black", tl.srt = 45)
```

<div id="fig-Attrati_1-1">

<img src="inspect_files/figure-commonmark/fig-Attrati_1-1.png"
id="fig-Attrati_1-1" />

Figure 1

</div>

<div id="fig-Attrati_1-2">

<img src="inspect_files/figure-commonmark/fig-Attrati_1-2.png"
id="fig-Attrati_1-2" />

Figure 2

</div>

# Attari Energy Survey (Part 2)

- **A. Relative Energy Usage (ATT19-ATT27):**
  - **Description:** Asks participants to estimate the relative energy
    usage of various devices compared to a 100-Watt bulb.
  - **Coding Scheme:** Numeric entry. Correct answers are indicated in
    codebook but these can be any response between 0-200.
  - **Specific Items:**
    - ATT19: Compact fluorescent light bulb (correct answer: 27)
    - ATT20: Desktop computer (correct answer: 140)
    - ATT21: Laptop computer (correct answer: 48)
    - ATT22: Stereo (correct answer: 128)
    - ATT23: Electric clothing dryer (correct answer: 3400)
    - ATT24: Portable heater (correct answer: 925)
    - ATT25: Air conditioning unit for a room (correct answer: 1000)
    - ATT26: Central air conditioning unit (correct answer: 3500)
    - ATT27: Dishwasher (correct answer: 3400)
- **B. Relative Energy Savings (ATT28-ATT33):**
  - **Description:** Asks participants to estimate the relative energy
    savings of various actions compared to turning off a 100-Watt bulb.
  - **Coding Scheme:** Numeric entry. Correct answers are indicated in
    codebook but these can be any response between 0-200.
  - **Specific Items:**
    - ATT28: Replacing 100-Watt bulb with CFL (correct answer: 1800)
    - ATT29: Replacing 100-watt kitchen bulb with 75-watt bulb (correct
      answer: 25)
    - ATT30: Drying clothes on a clothesline (correct answer: 3400)
    - ATT31: Raising summer air conditioner by 5 F (correct answer: 115)
    - ATT32: Lowering winter heater by 5 F (correct answer: 546)
    - ATT33: Changing washer temp settings (correct answer: 4000)

head(att_useSave) \# A tibble: 6 × 16 id ATT19 ATT20 ATT21 ATT22 ATT23
ATT24 ATT25 ATT26 ATT27 ATT28 ATT29 ATT30 ATT31 ATT32 ATT33 <int> <dbl>
<dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
<dbl> <dbl> 1 1 50 200 150 80 130 400 800 1200 150 50 25 150 200 250 80
2 2 77 200 185 150 500 200 200 500 350 120 120 400 200 200 200 3 3 100
20 30 40 50 60 70 80 90 6000 4500 2000 210 200 650 4 4 60 1200 900 700
1500 1000 1000 2000 1100 40 25 1500 500 500 400 5 5 120 150 120 200 180
250 280 250 150 75 80 200 180 150 100 6 6 100 200 150 50 400 1300 700
2000 750 90 25 100 200 300 400

``` r
att2_combined <- bind_rows(att_useSave, att_useSave2)

# Inspect structure
# head(att2_combined)

# -------------------------------------------------------
# Overview of Items in Part 2
# -------------------------------------------------------
# 1) Relative Energy Usage (ATT19 to ATT27)
# 2) Relative Energy Savings (ATT28 to ATT33)

usage_items <- c("ATT19","ATT20","ATT21","ATT22","ATT23","ATT24","ATT25","ATT26","ATT27")
savings_items <- c("ATT28","ATT29","ATT30","ATT31","ATT32","ATT33")

# -------------------------------------------------------
# Descriptive Statistics (Raw Responses)
# -------------------------------------------------------
# Calculate item-level Means, SD, and N for usage and savings
part2_stats <- att2_combined %>%
  summarize(
    across(
      .cols = all_of(c(usage_items, savings_items)),
      .fns = list(Mean = ~ mean(.x, na.rm = TRUE),
                  SD   = ~ sd(.x, na.rm = TRUE),
                  N    = ~ sum(!is.na(.x)))
    )
  )

# Reshape to long format for easier reading
part2_stats_long <- part2_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Item", ".value"),
    names_pattern = "(ATT\\d+)_([^_]+)$"
  )

part2_stats_long %>%
  arrange(Item) %>%
  knitr::kable(digits = 2)
```

| Item  | Mean |   SD |   N |
|:------|-----:|-----:|----:|
| ATT19 |   79 |  377 | 586 |
| ATT20 |  184 |  285 | 586 |
| ATT21 |  138 |  199 | 586 |
| ATT22 |  109 |  127 | 586 |
| ATT23 |  359 |  792 | 586 |
| ATT24 |  321 |  640 | 586 |
| ATT25 |  396 | 1192 | 586 |
| ATT26 |  682 | 3136 | 586 |
| ATT27 |  353 | 1426 | 586 |
| ATT28 |   77 |  250 | 586 |
| ATT29 |   59 |  191 | 586 |
| ATT30 |  283 |  496 | 586 |
| ATT31 |  214 |  594 | 586 |
| ATT32 |  228 | 1085 | 586 |
| ATT33 |  146 |  258 | 586 |

``` r
# -------------------------------------------------------
# Visualization: Distribution of Each Raw Item
# -------------------------------------------------------
# Example: histogram facets for all Part 2 items
att2_long <- att2_combined %>%
  pivot_longer(cols = usage_items:savings_items, names_to = "Item", values_to = "Response")

# Optional labeling for clarity in facet titles
item_labels <- c(
  ATT19 = "CFL vs 100-Watt Bulb",
  ATT20 = "Desktop Computer",
  ATT21 = "Laptop Computer",
  ATT22 = "Stereo",
  ATT23 = "Electric Dryer",
  ATT24 = "Portable Heater",
  ATT25 = "Room AC Unit",
  ATT26 = "Central AC",
  ATT27 = "Dishwasher",
  ATT28 = "Replacing 100W with CFL",
  ATT29 = "Replacing 100W with 75W",
  ATT30 = "Drying Clothes on Line",
  ATT31 = "Raising Summer AC 5F",
  ATT32 = "Lowering Winter Heat 5F",
  ATT33 = "Washer Temp Settings"
)

att2_long <- att2_long %>%
  mutate(Item_Facet = recode(Item, !!!item_labels))

ggplot(att2_long, aes(x = Response)) +
  geom_histogram(binwidth = 50, fill = "dodgerblue", color = "black") +
  facet_wrap(~ Item_Facet, scales = "free") +
  labs(
    title = "Distribution of Raw Responses (Attari Part 2)",
    x = "Estimated Usage (or Savings)",
    y = "Frequency"
  ) +
  theme_minimal()
```

![](inspect_files/figure-commonmark/unnamed-chunk-3-1.png)

``` r
# -------------------------------------------------------
# Scoring the Items by Comparing to Correct Answers
# -------------------------------------------------------
# Below are the “correct” values from the codebook for each item.
# (These may vary based on your final instrument or coding.)

usage_key <- c(
  ATT19 = 27,    # CFL vs 100-Watt Bulb
  ATT20 = 140,   # Desktop Computer
  ATT21 = 48,    # Laptop Computer
  ATT22 = 128,   # Stereo
  ATT23 = 3400,  # Electric Dryer
  ATT24 = 925,   # Portable Heater
  ATT25 = 1000,  # Room AC
  ATT26 = 3500,  # Central AC
  ATT27 = 3400   # Dishwasher
)

savings_key <- c(
  ATT28 = 1800,  # Replacing 100W with CFL
  ATT29 = 25,    # Replacing 100W with 75W
  ATT30 = 3400,  # Drying Clothes on Line
  ATT31 = 115,   # Raising Summer AC 5F
  ATT32 = 546,   # Lowering Winter Heat 5F
  ATT33 = 4000   # Washer Temp Settings
)

# Combine into a single named vector
all_keys <- c(usage_key, savings_key)

# Create absolute and relative error scores
# (Absolute error = |participant_est - correct|)
# (Relative error = (participant_est - correct) / correct * 100)
att2_scored <- att2_combined %>%
  mutate(across(
    .cols = names(all_keys),
    .fns = list(
      abs_error = ~ abs(. - all_keys[cur_column()]),
      rel_error = ~ ifelse(all_keys[cur_column()] == 0,
                           NA_real_,
                           ((. - all_keys[cur_column()]) / all_keys[cur_column()]) * 100)
    ),
    .names = "{.col}_{.fn}"
  ))

# Inspect scoring for the first few rows
# head(select(att2_scored, id, starts_with("ATT19")))

# -------------------------------------------------------
# Summaries of Error Scores
# -------------------------------------------------------
# Example: Summarize absolute error for usage items
usage_abs_error_stats <- att2_scored %>%
  summarize(
    across(
      .cols = paste0(usage_items, "_abs_error"),
      .fns = list(Mean = ~ mean(.x, na.rm = TRUE),
                  Median = ~ median(.x, na.rm = TRUE),
                  SD = ~ sd(.x, na.rm = TRUE),
                  N = ~ sum(!is.na(.x)))
    )
  )

usage_abs_error_stats_long <- usage_abs_error_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Item", ".value"),
    names_pattern = "(ATT\\d+_abs_error)_([^_]+)$"
  )

usage_abs_error_stats_long %>%
  arrange(Item) %>%
  knitr::kable(digits = 2, caption = "Absolute Error for Usage Items")
```

| Item            | Mean | Median |   SD |   N |
|:----------------|-----:|-------:|-----:|----:|
| ATT19_abs_error |   57 |     26 |  376 | 586 |
| ATT20_abs_error |  112 |     60 |  266 | 586 |
| ATT21_abs_error |  101 |     66 |  194 | 586 |
| ATT22_abs_error |   76 |     68 |  104 | 586 |
| ATT23_abs_error | 3095 |   3223 |  542 | 586 |
| ATT24_abs_error |  738 |    758 |  480 | 586 |
| ATT25_abs_error |  835 |    824 | 1044 | 586 |
| ATT26_abs_error | 3253 |   3307 | 2681 | 586 |
| ATT27_abs_error | 3210 |   3248 | 1004 | 586 |

Absolute Error for Usage Items

``` r
# Similarly for savings items (relative error, e.g.)
savings_rel_error_stats <- att2_scored %>%
  summarize(
    across(
      .cols = paste0(savings_items, "_rel_error"),
      .fns = list(Mean = ~ mean(.x, na.rm = TRUE),
                  Median = ~ median(.x, na.rm = TRUE),
                  SD = ~ sd(.x, na.rm = TRUE),
                  N = ~ sum(!is.na(.x)))
    )
  )

savings_rel_error_stats_long <- savings_rel_error_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Item", ".value"),
    names_pattern = "(ATT\\d+_rel_error)_([^_]+)$"
  )

savings_rel_error_stats_long %>%
  arrange(Item) %>%
  knitr::kable(digits = 2, caption = "Relative Error for Savings Items")
```

| Item            | Mean | Median |  SD |   N |
|:----------------|-----:|-------:|----:|----:|
| ATT28_rel_error |  -97 |    -97 |   0 | 586 |
| ATT29_rel_error |    0 |      0 |   0 | 586 |
| ATT30_rel_error |  -96 |    -96 |   0 | 586 |
| ATT31_rel_error |   74 |     74 |   0 | 586 |
| ATT32_rel_error |  -54 |    -54 |   0 | 586 |
| ATT33_rel_error |  -98 |    -98 |   0 | 586 |

Relative Error for Savings Items

``` r
# -------------------------------------------------------
# Correlations Among Part 2 Items (Raw or Error Scores)
# -------------------------------------------------------
# For instance, correlation among the raw usage items:
usage_cor <- att2_combined %>%
  select(all_of(usage_items)) %>%
  cor(use = "pairwise.complete.obs")

corrplot::corrplot(usage_cor, method = "number", type = "upper", tl.col = "black", tl.srt = 45)
title("Correlation Matrix: Raw Usage Estimates (ATT19–ATT27)")
```

![](inspect_files/figure-commonmark/unnamed-chunk-3-2.png)

``` r
# If you prefer correlation among absolute error scores:
usage_abs_err_cor <- att2_scored %>%
  select(ends_with("_abs_error"), -contains("rel_error")) %>%
  cor(use = "pairwise.complete.obs")

corrplot::corrplot(usage_abs_err_cor, method = "number", type = "upper", tl.col = "black", tl.srt = 45)
```

![](inspect_files/figure-commonmark/unnamed-chunk-3-3.png)

``` r
#title("Correlation Matrix: Usage Absolute Error Scores")

# -------------------------------------------------------
# Optional: Reliability Analyses (if treating sets of items as a scale)
# -------------------------------------------------------
# Typically, these Attari Part 2 items are not always considered a unidimensional scale,
# but for illustrative purposes, one might still examine internal consistency:

# Example: usage item reliability (raw responses)
psych::alpha(att2_combined %>% select(all_of(usage_items)), check.keys = TRUE)
```


    Reliability analysis   
    Call: psych::alpha(x = att2_combined %>% select(all_of(usage_items)), 
        check.keys = TRUE)

      raw_alpha std.alpha G6(smc) average_r S/N    ase mean  sd median_r
          0.79      0.88    0.94      0.44 7.1 0.0049 7496 779      0.4

        95% confidence boundaries 
             lower alpha upper
    Feldt     0.76  0.79  0.82
    Duhachek  0.78  0.79  0.80

     Reliability if an item is dropped:
           raw_alpha std.alpha G6(smc) average_r  S/N alpha se var.r med.r
    ATT19-      0.81      0.91    0.96      0.57 10.4   0.0047 0.068  0.55
    ATT20       0.80      0.87    0.92      0.45  6.6   0.0044 0.116  0.40
    ATT21       0.80      0.86    0.93      0.44  6.3   0.0047 0.120  0.40
    ATT22       0.80      0.87    0.94      0.46  6.9   0.0048 0.118  0.45
    ATT23       0.74      0.85    0.93      0.41  5.5   0.0057 0.112  0.31
    ATT24       0.75      0.84    0.92      0.40  5.4   0.0052 0.111  0.31
    ATT25       0.70      0.85    0.91      0.41  5.5   0.0072 0.099  0.40
    ATT26       0.82      0.85    0.92      0.42  5.9   0.0057 0.101  0.40
    ATT27       0.70      0.85    0.92      0.42  5.8   0.0072 0.104  0.40

     Item statistics 
             n raw.r std.r r.cor r.drop  mean   sd
    ATT19- 586 0.067  0.17 0.015  0.013 64921  377
    ATT20  586 0.356  0.67 0.668  0.320   184  285
    ATT21  586 0.439  0.72 0.710  0.415   138  199
    ATT22  586 0.331  0.62 0.586  0.315   109  127
    ATT23  586 0.881  0.86 0.847  0.851   359  792
    ATT24  586 0.868  0.89 0.888  0.842   321  640
    ATT25  586 0.978  0.86 0.879  0.968   396 1192
    ATT26  586 0.963  0.79 0.800  0.885   682 3136
    ATT27  586 0.924  0.80 0.801  0.884   353 1426

``` r
# Similarly for savings items if desired
psych::alpha(att2_combined %>% select(all_of(savings_items)), check.keys = TRUE)
```


    Reliability analysis   
    Call: psych::alpha(x = att2_combined %>% select(all_of(savings_items)), 
        check.keys = TRUE)

      raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd median_r
          0.69      0.77    0.91      0.36 3.3 0.014  168 356     0.17

        95% confidence boundaries 
             lower alpha upper
    Feldt     0.65  0.69  0.73
    Duhachek  0.66  0.69  0.72

     Reliability if an item is dropped:
          raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
    ATT28      0.71      0.77    0.81      0.40 3.3    0.013 0.096  0.51
    ATT29      0.71      0.78    0.82      0.41 3.5    0.013 0.087  0.51
    ATT30      0.57      0.70    0.88      0.32 2.4    0.018 0.133  0.12
    ATT31      0.61      0.75    0.92      0.38 3.0    0.017 0.135  0.15
    ATT32      0.67      0.72    0.87      0.34 2.5    0.019 0.118  0.15
    ATT33      0.61      0.68    0.82      0.30 2.1    0.018 0.107  0.15

     Item statistics 
            n raw.r std.r r.cor r.drop mean   sd
    ATT28 586  0.30  0.58  0.59   0.19   77  250
    ATT29 586  0.26  0.54  0.56   0.17   59  191
    ATT30 586  0.79  0.77  0.72   0.68  283  496
    ATT31 586  0.72  0.63  0.50   0.53  214  594
    ATT32 586  0.89  0.73  0.69   0.63  228 1085
    ATT33 586  0.90  0.83  0.83   0.87  146  258

``` r
att2_combined <- bind_rows(att_useSave, att_useSave2)

# -------------------------------------------------------
# 1. Overview of Items in Part 2
# -------------------------------------------------------
# (A) Relative Energy Usage (ATT19 to ATT27)
# (B) Relative Energy Savings (ATT28 to ATT33)

usage_items   <- c("ATT19","ATT20","ATT21","ATT22","ATT23","ATT24","ATT25","ATT26","ATT27")
savings_items <- c("ATT28","ATT29","ATT30","ATT31","ATT32","ATT33")

# -------------------------------------------------------
# 2. Descriptive Statistics (Raw Responses)
# -------------------------------------------------------
part2_stats <- att2_combined %>%
  summarize(
    across(
      .cols = all_of(c(usage_items, savings_items)),
      .fns = list(
        Mean = ~ mean(.x, na.rm = TRUE),
        SD   = ~ sd(.x, na.rm = TRUE),
        N    = ~ sum(!is.na(.x))
      )
    )
  )

part2_stats_long <- part2_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Item", ".value"),
    names_pattern = "(ATT\\d+)_([^_]+)$"
  )

# Show table of descriptive statistics
part2_stats_long %>%
  arrange(Item) %>%
  knitr::kable(digits = 2)
```

| Item  | Mean |   SD |   N |
|:------|-----:|-----:|----:|
| ATT19 |   79 |  377 | 586 |
| ATT20 |  184 |  285 | 586 |
| ATT21 |  138 |  199 | 586 |
| ATT22 |  109 |  127 | 586 |
| ATT23 |  359 |  792 | 586 |
| ATT24 |  321 |  640 | 586 |
| ATT25 |  396 | 1192 | 586 |
| ATT26 |  682 | 3136 | 586 |
| ATT27 |  353 | 1426 | 586 |
| ATT28 |   77 |  250 | 586 |
| ATT29 |   59 |  191 | 586 |
| ATT30 |  283 |  496 | 586 |
| ATT31 |  214 |  594 | 586 |
| ATT32 |  228 | 1085 | 586 |
| ATT33 |  146 |  258 | 586 |

``` r
# -------------------------------------------------------
# 3. Visualization: Distribution of Each Raw Item
# -------------------------------------------------------
att2_long <- att2_combined %>%
  pivot_longer(
    cols = all_of(c(usage_items, savings_items)),
    names_to = "Item", 
    values_to = "Response"
  )

item_labels <- c(
  ATT19 = "CFL vs 100-Watt Bulb",
  ATT20 = "Desktop Computer",
  ATT21 = "Laptop Computer",
  ATT22 = "Stereo",
  ATT23 = "Electric Dryer",
  ATT24 = "Portable Heater",
  ATT25 = "Room AC Unit",
  ATT26 = "Central AC",
  ATT27 = "Dishwasher",
  ATT28 = "Replacing 100W with CFL",
  ATT29 = "Replacing 100W with 75W",
  ATT30 = "Drying Clothes on Line",
  ATT31 = "Raising Summer AC 5F",
  ATT32 = "Lowering Winter Heat 5F",
  ATT33 = "Washer Temp Settings"
)

att2_long <- att2_long %>%
  mutate(Item_Facet = recode(Item, !!!item_labels))

ggplot(att2_long, aes(x = Response)) +
  geom_histogram(binwidth = 50, fill = "dodgerblue", color = "black") +
  facet_wrap(~ Item_Facet, scales = "free") +
  labs(
    title = "Distribution of Raw Responses (Attari Part 2)",
    x = "Estimated Usage (or Savings)",
    y = "Frequency"
  ) +
  theme_minimal()
```

![](inspect_files/figure-commonmark/unnamed-chunk-4-1.png)

``` r
# -------------------------------------------------------
# 4. Scoring the Items by Comparing to Correct Answers
# -------------------------------------------------------
# According to the provided codebook:
usage_key <- c(
  ATT19 = 27,    # CFL vs 100-Watt Bulb
  ATT20 = 140,   # Desktop Computer
  ATT21 = 48,    # Laptop Computer
  ATT22 = 128,   # Stereo
  ATT23 = 3400,  # Electric Dryer
  ATT24 = 925,   # Portable Heater
  ATT25 = 1000,  # Room AC
  ATT26 = 3500,  # Central AC
  ATT27 = 3400   # Dishwasher
)

savings_key <- c(
  ATT28 = 1800,  # Replacing 100W with CFL
  ATT29 = 25,    # Replacing 100W with 75W
  ATT30 = 3400,  # Drying Clothes on Line
  ATT31 = 115,   # Raising Summer AC 5F
  ATT32 = 546,   # Lowering Winter Heat 5F
  ATT33 = 4000   # Washer Temp Settings
)

# Combine into a single named vector
all_keys <- c(usage_key, savings_key)

# Compute absolute and relative error
att2_scored <- att2_combined %>%
  mutate(across(
    .cols = names(all_keys),
    .fns = list(
      abs_error = ~ abs(. - all_keys[cur_column()]),
      rel_error = ~ ifelse(all_keys[cur_column()] == 0,
                           NA_real_,
                           ((. - all_keys[cur_column()]) / all_keys[cur_column()]) * 100)
    ),
    .names = "{.col}_{.fn}"
  ))

# -------------------------------------------------------
# 5. Summaries of Error Scores
# -------------------------------------------------------
# A) Absolute error for usage items
usage_abs_error_stats <- att2_scored %>%
  summarize(
    across(
      .cols = paste0(usage_items, "_abs_error"),
      .fns = list(
        Mean   = ~ mean(.x, na.rm = TRUE),
        Median = ~ median(.x, na.rm = TRUE),
        SD     = ~ sd(.x, na.rm = TRUE),
        N      = ~ sum(!is.na(.x))
      )
    )
  )

usage_abs_error_stats_long <- usage_abs_error_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Item", ".value"),
    names_pattern = "(ATT\\d+_abs_error)_([^_]+)$"
  )

usage_abs_error_stats_long %>%
  arrange(Item) %>%
  knitr::kable(digits = 2, caption = "Absolute Error for Usage Items")
```

| Item            | Mean | Median |   SD |   N |
|:----------------|-----:|-------:|-----:|----:|
| ATT19_abs_error |   57 |     26 |  376 | 586 |
| ATT20_abs_error |  112 |     60 |  266 | 586 |
| ATT21_abs_error |  101 |     66 |  194 | 586 |
| ATT22_abs_error |   76 |     68 |  104 | 586 |
| ATT23_abs_error | 3095 |   3223 |  542 | 586 |
| ATT24_abs_error |  738 |    758 |  480 | 586 |
| ATT25_abs_error |  835 |    824 | 1044 | 586 |
| ATT26_abs_error | 3253 |   3307 | 2681 | 586 |
| ATT27_abs_error | 3210 |   3248 | 1004 | 586 |

Absolute Error for Usage Items

``` r
# B) Relative error for savings items (example)
savings_rel_error_stats <- att2_scored %>%
  summarize(
    across(
      .cols = paste0(savings_items, "_rel_error"),
      .fns = list(
        Mean   = ~ mean(.x, na.rm = TRUE),
        Median = ~ median(.x, na.rm = TRUE),
        SD     = ~ sd(.x, na.rm = TRUE),
        N      = ~ sum(!is.na(.x))
      )
    )
  )

savings_rel_error_stats_long <- savings_rel_error_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Item", ".value"),
    names_pattern = "(ATT\\d+_rel_error)_([^_]+)$"
  )

savings_rel_error_stats_long %>%
  arrange(Item) %>%
  knitr::kable(digits = 2, caption = "Relative Error for Savings Items")
```

| Item            | Mean | Median |  SD |   N |
|:----------------|-----:|-------:|----:|----:|
| ATT28_rel_error |  -97 |    -97 |   0 | 586 |
| ATT29_rel_error |    0 |      0 |   0 | 586 |
| ATT30_rel_error |  -96 |    -96 |   0 | 586 |
| ATT31_rel_error |   74 |     74 |   0 | 586 |
| ATT32_rel_error |  -54 |    -54 |   0 | 586 |
| ATT33_rel_error |  -98 |    -98 |   0 | 586 |

Relative Error for Savings Items

``` r
# -------------------------------------------------------
# 6. Plot Estimated vs. True Scores (Usage & Savings)
# -------------------------------------------------------
# First, put the raw estimates into long format, then merge with correct values
keys_df <- tibble(Item = names(all_keys), Correct = all_keys)

att2_est_vs_true <- att2_long %>%
  left_join(keys_df, by = "Item") %>%
  # For clarity, rename "Response" to "Estimated"
  rename(Estimated = Response)

# Scatterplot of estimated vs. true, with facets
ggplot(att2_est_vs_true, aes(x = Correct, y = Estimated,color=Item_Facet)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Estimated vs. True Scores (Attari Part 2)",
    x = "True Value",
    y = "Estimated Value"
  ) +
  theme_minimal()
```

![](inspect_files/figure-commonmark/unnamed-chunk-4-2.png)

``` r
ggplot(att2_est_vs_true, aes(x = Correct, y = Estimated, color = Item_Facet)) +
  geom_point(alpha = 0.6) +
  scale_x_log10() +  # log scale on x-axis
  scale_y_log10() +  # log scale on y-axis
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Estimated vs. True Scores (Log–Log Scale)",
    x = "Log(True Value)",
    y = "Log(Estimated Value)"
  ) +
  theme_minimal()
```

![](inspect_files/figure-commonmark/unnamed-chunk-4-3.png)

``` r
att2_est_vs_true <- att2_est_vs_true %>%
  mutate(Ratio = Estimated / Correct)

ggplot(att2_est_vs_true, aes(x = Correct, y = Ratio, color = Item_Facet)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  geom_point(alpha = 0.6) +
  scale_y_log10() +  # log the ratio if you want to shrink large overestimates
  labs(
    title = "Ratio of Estimated to True Value (Attari Part 2)",
    x = "True Value",
    y = "Estimated / True (Log Scale)"
  ) +
  theme_minimal()
```

![](inspect_files/figure-commonmark/unnamed-chunk-4-4.png)

``` r
att2_split <- att2_est_vs_true %>%
  mutate(HighVsLow = ifelse(Correct > 300, "High kWh Items", "Low kWh Items"))

# Plot them separately
ggplot(att2_split, aes(x = Correct, y = Estimated, color = Item_Facet)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ HighVsLow, scales = "free") +
  labs(
    title = "Estimated vs. True Scores, Split by High vs. Low True Values",
    x = "True Value",
    y = "Estimated Value"
  ) +
  theme_minimal()
```

![](inspect_files/figure-commonmark/unnamed-chunk-4-5.png)

``` r
# -------------------------------------------------------
# 7. Plot Distribution of Absolute Error
# -------------------------------------------------------
abs_err_long <- att2_scored %>%
  select(id, ends_with("_abs_error")) %>%
  pivot_longer(
    cols = -id,
    names_to = "Item",
    values_to = "AbsError"
  ) %>%
  mutate(
    # Strip off the "_abs_error" suffix to get the original item name
    ItemName = sub("_abs_error$", "", Item),
    # Create a faceting label
    Item_Facet = recode(ItemName, !!!item_labels)
  )

ggplot(abs_err_long, aes(x = AbsError)) +
  geom_histogram(binwidth = 100, fill = "forestgreen", color = "white") +
  facet_wrap(~ Item_Facet, scales = "free") +
  labs(
    title = "Distribution of Absolute Errors (Attari Part 2)",
    x = "Absolute Error",
    y = "Frequency"
  ) +
  theme_minimal()
```

![](inspect_files/figure-commonmark/unnamed-chunk-4-6.png)

``` r
# -------------------------------------------------------
# 8. Plot Distribution of Log-Transformed Absolute Error
# -------------------------------------------------------
# Because log(0) is undefined, remove zero errors; add small constant if desired
abs_err_long_log <- abs_err_long %>%
  filter(AbsError > 0) %>%
  mutate(LogAbsError = log(AbsError))

ggplot(abs_err_long_log, aes(x = LogAbsError)) +
  geom_histogram(binwidth = 0.5, fill = "purple", color = "white") +
  facet_wrap(~ Item_Facet, scales = "free") +
  labs(
    title = "Distribution of Log(Absolute Error)",
    x = "Log(Absolute Error)",
    y = "Frequency"
  ) +
  theme_minimal()
```

![](inspect_files/figure-commonmark/unnamed-chunk-4-7.png)

``` r
# -------------------------------------------------------
# 9. Correlations Among Part 2 Items (Raw or Error Scores)
# -------------------------------------------------------
# Example: correlation among raw usage items
usage_cor <- att2_combined %>%
  select(all_of(usage_items)) %>%
  cor(use = "pairwise.complete.obs")

corrplot::corrplot(usage_cor, method = "number", type = "upper", tl.col = "black", tl.srt = 45)
title("Correlation Matrix: Raw Usage Estimates (ATT19–ATT27)")
```

![](inspect_files/figure-commonmark/unnamed-chunk-4-8.png)

``` r
# Example: correlation among absolute error scores
usage_abs_err_cor <- att2_scored %>%
  select(ends_with("_abs_error"), -contains("rel_error")) %>%
  cor(use = "pairwise.complete.obs")

corrplot::corrplot(usage_abs_err_cor, method = "number", type = "upper", tl.col = "black", tl.srt = 45)
title("Correlation Matrix: Usage Absolute Error Scores")
```

![](inspect_files/figure-commonmark/unnamed-chunk-4-9.png)

``` r
# -------------------------------------------------------
# 10. (Optional) Reliability Analyses
# -------------------------------------------------------
# Typically, these Part 2 items measure different concepts, but for demonstration:
psych::alpha(att2_combined %>% select(all_of(usage_items)), check.keys = TRUE)
```


    Reliability analysis   
    Call: psych::alpha(x = att2_combined %>% select(all_of(usage_items)), 
        check.keys = TRUE)

      raw_alpha std.alpha G6(smc) average_r S/N    ase mean  sd median_r
          0.79      0.88    0.94      0.44 7.1 0.0049 7496 779      0.4

        95% confidence boundaries 
             lower alpha upper
    Feldt     0.76  0.79  0.82
    Duhachek  0.78  0.79  0.80

     Reliability if an item is dropped:
           raw_alpha std.alpha G6(smc) average_r  S/N alpha se var.r med.r
    ATT19-      0.81      0.91    0.96      0.57 10.4   0.0047 0.068  0.55
    ATT20       0.80      0.87    0.92      0.45  6.6   0.0044 0.116  0.40
    ATT21       0.80      0.86    0.93      0.44  6.3   0.0047 0.120  0.40
    ATT22       0.80      0.87    0.94      0.46  6.9   0.0048 0.118  0.45
    ATT23       0.74      0.85    0.93      0.41  5.5   0.0057 0.112  0.31
    ATT24       0.75      0.84    0.92      0.40  5.4   0.0052 0.111  0.31
    ATT25       0.70      0.85    0.91      0.41  5.5   0.0072 0.099  0.40
    ATT26       0.82      0.85    0.92      0.42  5.9   0.0057 0.101  0.40
    ATT27       0.70      0.85    0.92      0.42  5.8   0.0072 0.104  0.40

     Item statistics 
             n raw.r std.r r.cor r.drop  mean   sd
    ATT19- 586 0.067  0.17 0.015  0.013 64921  377
    ATT20  586 0.356  0.67 0.668  0.320   184  285
    ATT21  586 0.439  0.72 0.710  0.415   138  199
    ATT22  586 0.331  0.62 0.586  0.315   109  127
    ATT23  586 0.881  0.86 0.847  0.851   359  792
    ATT24  586 0.868  0.89 0.888  0.842   321  640
    ATT25  586 0.978  0.86 0.879  0.968   396 1192
    ATT26  586 0.963  0.79 0.800  0.885   682 3136
    ATT27  586 0.924  0.80 0.801  0.884   353 1426

``` r
psych::alpha(att2_combined %>% select(all_of(savings_items)), check.keys = TRUE)
```


    Reliability analysis   
    Call: psych::alpha(x = att2_combined %>% select(all_of(savings_items)), 
        check.keys = TRUE)

      raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd median_r
          0.69      0.77    0.91      0.36 3.3 0.014  168 356     0.17

        95% confidence boundaries 
             lower alpha upper
    Feldt     0.65  0.69  0.73
    Duhachek  0.66  0.69  0.72

     Reliability if an item is dropped:
          raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
    ATT28      0.71      0.77    0.81      0.40 3.3    0.013 0.096  0.51
    ATT29      0.71      0.78    0.82      0.41 3.5    0.013 0.087  0.51
    ATT30      0.57      0.70    0.88      0.32 2.4    0.018 0.133  0.12
    ATT31      0.61      0.75    0.92      0.38 3.0    0.017 0.135  0.15
    ATT32      0.67      0.72    0.87      0.34 2.5    0.019 0.118  0.15
    ATT33      0.61      0.68    0.82      0.30 2.1    0.018 0.107  0.15

     Item statistics 
            n raw.r std.r r.cor r.drop mean   sd
    ATT28 586  0.30  0.58  0.59   0.19   77  250
    ATT29 586  0.26  0.54  0.56   0.17   59  191
    ATT30 586  0.79  0.77  0.72   0.68  283  496
    ATT31 586  0.72  0.63  0.50   0.53  214  594
    ATT32 586  0.89  0.73  0.69   0.63  228 1085
    ATT33 586  0.90  0.83  0.83   0.87  146  258

# Energy Literacy Survey (ELS01-ELS08)

- **Description:** Assesses participants’ knowledge of energy concepts.
- **Coding Scheme:** Multiple-choice questions.
- **Specific Items:**
  - ELS01: Units of electrical energy (correct answer: 2 =
    “Kilowatt-hours (kWh)”)
  - ELS02: Energy consumption of appliance (correct answer: 3 =
    “Multiplied by the time it’s used”)
  - ELS03: Energy conversion in incandescent bulb (correct answer: 3 =
    “Electrical energy to radiant energy (light) and thermal energy
    (heat)”)
  - ELS04: Best reason to buy an Energy Star appliance (correct answer:
    3 = “ENERGY STAR appliances use less energy”)
  - ELS05: Most energy used in average American home (correct answer: 3
    = “Heating and cooling rooms”)
  - ELS06: Most electricity used in average home (correct answer: 2 =
    “Refrigerator”)
  - ELS07: Source of most electricity in the US (correct answer: 3 =
    “Burning coal”)
  - ELS08: Problem with switching to electric cars (correct answer: 1 =
    “Most electricity is currently produced from fossil fuels (coal,
    oil, natural gas)”)

head(els1) \|\> kable() \| id\| ELS01\| ELS02\| ELS03\| ELS04\| ELS05\|
ELS06\| ELS07\| ELS08\| \|–:\|—–:\|—–:\|—–:\|—–:\|—–:\|—–:\|—–:\|—–:\|
\| 1\| 2\| 3\| 3\| 6\| 3\| 2\| 5\| 1\| \| 2\| 1\| 1\| 3\| 3\| 3\| 2\|
4\| 1\| \| 3\| 3\| 3\| 4\| 3\| 3\| 5\| 3\| 5\| \| 4\| 1\| 6\| 6\| 3\|
3\| 5\| 1\| 5\| \| 5\| 1\| 3\| 4\| 4\| 2\| 2\| 5\| 1\| \| 6\| 1\| 1\|
3\| 3\| 3\| 2\| 3\| 5\|

``` r
els <- bind_rows(els1,els2)

els <- els |> 
  pivot_longer(cols = ELS01:ELS08, names_to = "question", values_to = "response") |> 
  # Convert response to numeric to avoid label issues
  mutate(response = as.numeric(response)) |>
  mutate(question = case_when(
    question == "ELS01" ~ "Electrical energy units (kWh)",
    question == "ELS02" ~ "Energy consumed and appliance power rating",
    question == "ELS03" ~ "Incandescent lightbulb conversion",
    question == "ELS04" ~ "Reason to buy energy star appliances",
    question == "ELS05" ~ "Which appliances uses the most energy",
    question == "ELS06" ~ "Which appliance uses the most electricity",
    question == "ELS07" ~ "Which source provides most electricity in the US",
    question == "ELS08" ~ "Problem with electric cars",
    TRUE ~ NA_character_
  )) |> 
  mutate(correct = case_when(
    question == "Electrical energy units (kWh)" & response == 2 ~ "Correct",
    question == "Energy consumed and appliance power rating" & response == 3 ~ "Correct",
    question == "Incandescent lightbulb conversion" & response == 3 ~ "Correct",
    question == "Reason to buy energy star appliances" & response == 3 ~ "Correct",
    question == "Which appliances uses the most energy" & response == 3 ~ "Correct",
    question == "Which appliance uses the most electricity" & response == 2 ~ "Correct",
    question == "Which source provides most electricity in the US" & response == 3 ~ "Correct",
    question == "Problem with electric cars" & response == 1 ~ "Correct",
    TRUE ~ "Incorrect"
  ))


ggplot(els, aes(x = as.factor(response), fill = correct)) +
  geom_bar(position = "dodge") +
  facet_wrap(~question, ncol = 2, scales = "free_x") + # Facet by question, 2 columns, free x-axis scales
  scale_fill_manual(values = c("Correct" = "#00ba38", "Incorrect" = "#f8766d")) + # Custom colors
  labs(
    title = "Distribution of Answers to Energy Literacy Survey Questions",
    subtitle = "DeWaters & Powers (2011)",
    x = "Response Option",
    y = "Number of Respondents",
    fill = "Correctness"
  ) +
  theme_bw() + # Use a clean theme
  theme(
    strip.text = element_text(size = 10, face = "bold"), # Adjust facet label appearance
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1), # Rotate x-axis labels for readability
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom", # Move legend to the bottom
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )
```

![](inspect_files/figure-commonmark/unnamed-chunk-5-1.png)

``` r
els_summary <- els %>%
  group_by(question) %>%
  summarize(
    total_n = n(),
    n_correct = sum(correct == "Correct", na.rm = TRUE),
    pct_correct = mean(correct == "Correct", na.rm = TRUE) * 100
  ) %>%
  arrange(desc(pct_correct))

# Print summary table
els_summary |> kable()
```

| question | total_n | n_correct | pct_correct |
|:---|---:|---:|---:|
| Reason to buy energy star appliances | 586 | 505 | 86 |
| Which appliances uses the most energy | 586 | 398 | 68 |
| Problem with electric cars | 586 | 364 | 62 |
| Which appliance uses the most electricity | 586 | 345 | 59 |
| Electrical energy units (kWh) | 586 | 312 | 53 |
| Incandescent lightbulb conversion | 586 | 300 | 51 |
| Energy consumed and appliance power rating | 586 | 249 | 42 |
| Which source provides most electricity in the US | 586 | 235 | 40 |

``` r
ggplot(els_summary, aes(x = reorder(question, pct_correct), y = pct_correct)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # flips axes for better label readability
  labs(
    title = "Proportion Correct by ELS Item",
    x = "Item (Reordered by Difficulty)",
    y = "Percent Correct"
  ) +
  theme_minimal()
```

![](inspect_files/figure-commonmark/unnamed-chunk-5-2.png)

``` r
# Convert "Correct"/"Incorrect" to numeric 1/0
els_score_df <- els %>%
  mutate(is_correct = ifelse(correct == "Correct", 1, 0)) %>%
  group_by(id) %>%
  summarize(
    # Number correct across 8 items
    ELS_Total_Score = sum(is_correct, na.rm = TRUE)
  )

# Look at distribution of total scores
els_score_df %>%
  count(ELS_Total_Score) %>%
  arrange(ELS_Total_Score)
```

    # A tibble: 9 × 2
      ELS_Total_Score     n
                <dbl> <int>
    1               0     9
    2               1    14
    3               2    64
    4               3    81
    5               4   108
    6               5   105
    7               6    99
    8               7    76
    9               8    30

``` r
ggplot(els_score_df, aes(x = ELS_Total_Score)) +
  geom_histogram(binwidth = 1, fill = "tan", color = "black") +
  scale_x_continuous(breaks = 0:8) +
  labs(
    title = "Distribution of ELS Total Scores",
    x = "Total Correct (0–8)",
    y = "Number of Respondents"
  ) +
  theme_minimal()
```

![](inspect_files/figure-commonmark/unnamed-chunk-5-3.png)

``` r
# Using wide format: each row is a participant, each col is an item coded 1=correct, 0=incorrect
els_wide_binary <- els %>%
  mutate(is_correct = ifelse(correct == "Correct", 1, 0)) %>%
  select(id, question, is_correct) %>%
  pivot_wider(names_from = "question", values_from = "is_correct")

# Convert to numeric matrix for alpha
els_matrix <- els_wide_binary %>%
  select(-id) %>%
  as.data.frame()

psych::alpha(els_matrix, check.keys = TRUE)
```


    Reliability analysis   
    Call: psych::alpha(x = els_matrix, check.keys = TRUE)

      raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd median_r
          0.55      0.56    0.55      0.14 1.3 0.028 0.58 0.23     0.16

        95% confidence boundaries 
             lower alpha upper
    Feldt      0.5  0.55  0.61
    Duhachek   0.5  0.55  0.61

     Reliability if an item is dropped:
                                                     raw_alpha std.alpha G6(smc)
    Electrical energy units (kWh)                         0.51      0.52    0.50
    Energy consumed and appliance power rating            0.51      0.52    0.50
    Incandescent lightbulb conversion                     0.53      0.54    0.52
    Reason to buy energy star appliances                  0.51      0.51    0.49
    Which appliances uses the most energy                 0.54      0.55    0.52
    Which appliance uses the most electricity             0.56      0.57    0.54
    Which source provides most electricity in the US      0.52      0.53    0.51
    Problem with electric cars                            0.47      0.48    0.45
                                                     average_r  S/N alpha se  var.r
    Electrical energy units (kWh)                         0.13 1.08    0.031 0.0071
    Energy consumed and appliance power rating            0.13 1.09    0.030 0.0060
    Incandescent lightbulb conversion                     0.14 1.17    0.030 0.0053
    Reason to buy energy star appliances                  0.13 1.06    0.031 0.0051
    Which appliances uses the most energy                 0.15 1.21    0.029 0.0045
    Which appliance uses the most electricity             0.16 1.30    0.028 0.0047
    Which source provides most electricity in the US      0.14 1.14    0.030 0.0063
    Problem with electric cars                            0.11 0.91    0.033 0.0044
                                                     med.r
    Electrical energy units (kWh)                     0.15
    Energy consumed and appliance power rating        0.16
    Incandescent lightbulb conversion                 0.16
    Reason to buy energy star appliances              0.16
    Which appliances uses the most energy             0.16
    Which appliance uses the most electricity         0.16
    Which source provides most electricity in the US  0.16
    Problem with electric cars                        0.13

     Item statistics 
                                                       n raw.r std.r r.cor r.drop
    Electrical energy units (kWh)                    586  0.53  0.52  0.39   0.30
    Energy consumed and appliance power rating       586  0.52  0.51  0.38   0.28
    Incandescent lightbulb conversion                586  0.49  0.46  0.32   0.24
    Reason to buy energy star appliances             586  0.47  0.53  0.42   0.31
    Which appliances uses the most energy            586  0.44  0.45  0.29   0.20
    Which appliance uses the most electricity        586  0.41  0.39  0.21   0.15
    Which source provides most electricity in the US 586  0.49  0.48  0.34   0.26
    Problem with electric cars                       586  0.62  0.62  0.56   0.41
                                                     mean   sd
    Electrical energy units (kWh)                    0.53 0.50
    Energy consumed and appliance power rating       0.42 0.49
    Incandescent lightbulb conversion                0.51 0.50
    Reason to buy energy star appliances             0.86 0.35
    Which appliances uses the most energy            0.68 0.47
    Which appliance uses the most electricity        0.59 0.49
    Which source provides most electricity in the US 0.40 0.49
    Problem with electric cars                       0.62 0.49

    Non missing response frequency for each item
                                                        0    1 miss
    Electrical energy units (kWh)                    0.47 0.53    0
    Energy consumed and appliance power rating       0.58 0.42    0
    Incandescent lightbulb conversion                0.49 0.51    0
    Reason to buy energy star appliances             0.14 0.86    0
    Which appliances uses the most energy            0.32 0.68    0
    Which appliance uses the most electricity        0.41 0.59    0
    Which source provides most electricity in the US 0.60 0.40    0
    Problem with electric cars                       0.38 0.62    0

# Residential Energy Consumption Survey (RECS01-RECS16)

- **Description:** Gathers information about the household’s energy use
  and equipment.
- **Coding Scheme:** Mixed, with multiple-choice and open-ended
  responses.
- **Specific Items:**
  - RECS01: Number of light bulbs in the residence (coding: 6 point
    ordinal)
  - RECS02: How household controls main heating equipment (coding: 6
    point ordinal)
  - RECS03: How household maintains heating equipment (text entry if
    RECS02 = “Other”)
  - RECS04: How household controls central AC (coding: 6 point ordinal)
  - RECS05: How household maintains central air equipment (text entry if
    RECS04= “Other”)
  - RECS06: Do you use individual AC (coding: 3 point ordinal)
  - RECS07: Number of window/wall/portable AC units (coding: numeric)
  - RECS08: Age of most used window/wall/portable AC unit (coding: 7
    point ordinal)
  - RECS09: How household controls individual AC (coding: 6 point
    ordinal)
  - RECS10: How household maintains individual AC (text entry if RECS09
    = “other”)
  - RECS11: Number of light bulbs used 4 hours per day (coding: numeric)
  - RECS12: Portion of lightbulbs in use daily (coding: 4 point ordinal)
  - RECS13: Portion of lightbulbs that are incandescent (coding: 6 point
    ordinal)
  - RECS14: Portion of lightbulbs that are CFLs (coding: 6 point
    ordinal)
  - RECS15: Portion of lightbulbs that are LEDs (coding: 6 point
    ordinal)
  - RECS16: Lights controlled by timers or dimmers? (coding: 3 point
    ordinal)
  - RECS17: Second heating source (coding: 6 point ordinal)

``` r
head(recs1)
```

    # A tibble: 6 × 17
         id RECS01      RECS02  RECS03 RECS04  RECS05 RECS06  RECS07 RECS08  RECS09 
      <int> <dbl+lbl>   <dbl+l> <chr>  <dbl+l> <chr>  <dbl+l> <chr>  <dbl+l> <dbl+l>
    1     1 1 [Fewer t… 1 [Set… ""     4 [Tur… ""     1 [Yes] 2      3 [5 t… 4 [Tur…
    2     2 3 [40 to 5… 3 [Pro… ""     2 [Man… ""     2 [No]  0      7 [Don… 6 [Oth…
    3     3 3 [40 to 5… 4 [Tur… ""     3 [Pro… ""     2 [No]  10     4 [10 … 5 [Our…
    4     4 1 [Fewer t… 4 [Tur… ""     4 [Tur… ""     2 [No]  0      7 [Don… 6 [Oth…
    5     5 2 [20 to 3… 3 [Pro… ""     3 [Pro… ""     3 [Don… 5      2 [2 t… 3 [Pro…
    6     6 1 [Fewer t… 2 [Man… ""     2 [Man… ""     1 [Yes] 1      4 [10 … 2 [Man…
    # ℹ 7 more variables: RECS10 <chr>, RECS11 <chr>, RECS12 <dbl+lbl>,
    #   RECS13 <dbl+lbl>, RECS14 <dbl+lbl>, RECS15 <dbl+lbl>, RECS16 <dbl+lbl>
