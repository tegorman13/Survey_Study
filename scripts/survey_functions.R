




analyze_attari_survey_part1 <- function(aes_combined) {
  # Correct answers for Numeracy Questions (ATT16-ATT18)
  correct_numeracy <- c(
    ATT16 = 500, ATT17 = 10, ATT18 = 0.1
  )
  
  # Function to calculate the relative score for numeracy items
  calculate_relative_score_numeracy <- function(response, correct) {
    if (is.na(response)) {
        return(NA)
    } else if (response > correct) {
      1 - (response - correct) / response
    } else {
      response / correct
    }
  }
  
  # Calculate perceived difficulty scores (average of ATT01-ATT15, excluding 9)
  difficulty_scores <- aes_combined %>%
    select(id, ATT01:ATT15) %>%
    pivot_longer(
      cols = ATT01:ATT15,
      names_to = "item",
      values_to = "response"
    ) %>%
    filter(response != 9) %>% # Exclude "Not applicable" responses
    group_by(id) %>%
    summarize(perceived_difficulty_score = mean(response, na.rm = TRUE))
  
  # Calculate numeracy scores
  numeracy_scores <- aes_combined %>%
    select(id, ATT16:ATT18) %>%
    pivot_longer(
      cols = ATT16:ATT18,
      names_to = "item",
      values_to = "response"
    ) %>%
    mutate(correct = correct_numeracy[item]) %>%
    mutate(relative_score = map2_dbl(response, correct, calculate_relative_score_numeracy)) %>%
    group_by(id) %>%
    summarize(numeracy_score = mean(relative_score, na.rm = TRUE))
  
  # Combine the scores into a single data frame
  final_scores <- full_join(difficulty_scores, numeracy_scores, by = "id")

    final_scores <- final_scores %>%
    mutate(
      perceived_difficulty_score = scale(perceived_difficulty_score),
      numeracy_score = scale(numeracy_score)
    )
  
  return(final_scores)
}





analyze_attari_survey <- function(att2_combined) {
  # Correct answers for Relative Energy Usage (ATT19-ATT27)
  correct_usage <- c(
    ATT19 = 27, ATT20 = 140, ATT21 = 48, ATT22 = 128, ATT23 = 3400,
    ATT24 = 925, ATT25 = 1000, ATT26 = 3500, ATT27 = 3400
  )
  
  # Correct answers for Relative Energy Savings (ATT28-ATT33)
  correct_savings <- c(
    ATT28 = 1800, ATT29 = 25, ATT30 = 3400, ATT31 = 115, ATT32 = 546, ATT33 = 4000
  )
  
  # Function to calculate the relative score for each item
  calculate_relative_score <- function(response, correct) {
    if (response > correct) {
      1 - (response - correct) / response
    } else {
      response / correct
    }
  }
  
  # Calculate relative energy usage scores
  usage_scores <- att2_combined %>%
    select(id, ATT19:ATT27) %>%
    pivot_longer(
      cols = ATT19:ATT27,
      names_to = "item",
      values_to = "response"
    ) %>%
    mutate(correct = correct_usage[item]) %>%
    mutate(relative_score = map2_dbl(response, correct, calculate_relative_score)) %>%
    group_by(id) %>%
    summarize(relative_energy_use_score = mean(relative_score, na.rm = TRUE))
  
  # Calculate relative energy savings scores
  savings_scores <- att2_combined %>%
    select(id, ATT28:ATT33) %>%
    pivot_longer(
      cols = ATT28:ATT33,
      names_to = "item",
      values_to = "response"
    ) %>%
    mutate(correct = correct_savings[item]) %>%
    mutate(relative_score = map2_dbl(response, correct, calculate_relative_score)) %>%
    group_by(id) %>%
    summarize(relative_energy_save_score = mean(relative_score, na.rm = TRUE))
  
  # Combine the scores into a single data frame
  final_scores <- full_join(usage_scores, savings_scores, by = "id")
   # Standardize the scores
  final_scores <- final_scores %>%
    mutate(
      relative_energy_use_score = scale(relative_energy_use_score),
      relative_energy_save_score = scale(relative_energy_save_score)
    )
  
  return(final_scores)
}



analyze_els_survey <- function(els) {
  # Correct answers for ELS items (ELS01-ELS08)
  correct_answers <- c(
    ELS01 = 2, ELS02 = 3, ELS03 = 3, ELS04 = 3,
    ELS05 = 3, ELS06 = 2, ELS07 = 3, ELS08 = 1
  )

  els %>%
    mutate(
      accuracy = rowSums(across(ELS01:ELS08, ~ .x == correct_answers[cur_column()])),
      els = as.numeric(scale(accuracy))
    ) %>%
    select(id, accuracy, els)
}


analyze_recycling_survey <- function(rs_data) {
  
  # 1) Coerce columns to numeric
  rs_numeric <- rs_data %>%
    mutate(
      RS01_num = as.numeric(as.character(RS01)),
      RS02_num = as.numeric(as.character(RS02)),
      RS03_num = as.numeric(as.character(RS03)),
      RS04_num = as.numeric(as.character(RS04)),
      RS05_num = as.numeric(as.character(RS05)),
      RS06_num = as.numeric(as.character(RS06))
    )
  
  # 2) Recode items so that higher numbers consistently reflect
  #    "more" of the targeted construct:
  #
  # Environmental Attitude (RS01 & RS03 are negative, RS02 & RS06 are positive).
  # Original scale is 1=Agree ... 5=Disagree
  # For a 'positive' pro-environment item, do 6 - x => so 1 => 5 (strong agreement => higher = pro-env).
  # For a 'negative' pro-environment item, keep x => so 1 => 1 (strong agreement => lower pro-env).
  
  rs_recode <- rs_numeric %>%
    mutate(
      # Positive items
      RS02_env = 6 - RS02_num,  # now 5 = strongly pro-env
      RS06_env = 6 - RS06_num,  # now 5 = strongly pro-env
      
      # Negative items (keep the original so that 1 => 1 = strongly anti-env, 5 => 5 = strongly pro-env)
      RS01_env = RS01_num,
      RS03_env = RS03_num,
      
      # Political items: If we want higher = more conservative,
      # we can do 6 - x so that 1 => 5 (strongly conservative).
      # If you'd prefer the raw code to remain 1=Agree => "lowest" numeric,
      # skip the transformation. Below we invert it:
      RS04_cons = 6 - RS04_num, 
      RS05_cons = 6 - RS05_num
    )
  
  # 3) Compute subscales
  #    - "env_attitude": average of RS01_env, RS02_env, RS03_env, RS06_env
  #      such that 5 = most pro-environment, 1 = least pro-environment
  #    - "pol_conservatism": average of RS04_cons, RS05_cons
  #      such that 5 = strongly conservative, 1 = strongly liberal
  
  rs_subscales <- rs_recode %>%
    rowwise() %>%
    mutate(
      env_attitude = mean(c(RS01_env, RS02_env, RS03_env, RS06_env), na.rm = TRUE),
      pol_conservatism = mean(c(RS04_cons, RS05_cons), na.rm = TRUE)
    ) %>%
    ungroup()
  
  # 4) Standardize subscales if desired
  rs_final <- rs_subscales %>%
    mutate(
      env_attitude_z = as.numeric(scale(env_attitude)),
      pol_conservatism_z = as.numeric(scale(pol_conservatism))
    ) %>%
    # 5) Return the columns of interest
    select(id, 
           env_attitude, env_attitude_z,
           pol_conservatism, pol_conservatism_z)
  
  return(rs_final)
}