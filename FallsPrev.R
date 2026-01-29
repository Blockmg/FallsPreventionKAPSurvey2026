# Falls Prevention Survey Cleaning and Preliminary Graph Visualizations

#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load libraries
library(Hmisc)
library(tidyverse)
library(dplyr)
library(haven)
library(janitor)

#Read Data (replace with your file path for each survey data file)
rawBannerdata=read.csv("~/Downloads/FallsPreventionSurve_DATA_2026-01-21_1133.csv")
rawGardendata=read.csv("~/Downloads/FallsPreventionSurve_DATA_2026-01-23_1026.csv")
rawPCOAdata=read.csv("~/Downloads/FallsPreventionSurve_DATA_2026-01-22_1508.csv")

#Setting factors and cleaning for Banner Data
cleanedBanner <- rawBannerdata |>
  mutate(age1 = factor(age1,
                       levels = c("40_49",
                                  "50_59",
                                  "60_69",
                                  "70_79",
                                  "80_"),
                       labels = c("40-49", 
                                  "50-59", 
                                  "60-69", 
                                  "70-79", 
                                  "80+"))) |>
  mutate(gender = factor(gender,
                         levels = c("male",
                                    "female",
                                    "non_binary",
                                    "prefer_to_self_describe",
                                    "prefer_not_to_say"),
                         labels = c("Male",
                                    "Female",
                                    "Non-binary/third gender",
                                    "Prefer to self describe",
                                    "Prefer not to say"))) |>
  mutate(fall_yr = factor(fall_yr,
                          levels = c(1, 
                                     0),
                          labels = c("Yes",
                                     "No"))) |>
  mutate(falls_number = factor(falls_number,
                               levels = c("once",
                                          "2_to_3_times",
                                          "more_than_3_times"),
                               labels = c("Once",
                                          "2-3 times",
                                          "More than 3 times"))) |>
  mutate(health_risks___poor_balance = factor(health_risks___poor_balance,
                                              levels = c(0,
                                                         1),
                                              labels = c("Unchecked",
                                                         "Checked"))) |>
  mutate(health_risks___arthritis = factor(health_risks___arthritis,
                                              levels = c(0,
                                                         1),
                                              labels = c("Unchecked",
                                                         "Checked"))) |>
  mutate(health_risks___vision_problems = factor(health_risks___vision_problems,
                                              levels = c(0,
                                                         1),
                                              labels = c("Unchecked",
                                                         "Checked"))) |>
  mutate(health_risks___other = factor(health_risks___other,
                                              levels = c(0,
                                                         1),
                                              labels = c("Unchecked",
                                                         "Checked"))) |>
  mutate(section_1_complete = factor(section_1_complete,
                                              levels = c(0,
                                                         1,
                                                         2),
                                              labels = c("Incomplete",
                                                         "Unverified",
                                                         "Complete"))) |>
  mutate(fp_belief = factor(fp_belief,
                            levels = c("yes",
                                       "no",
                                       "unsure"),
                            labels = c("Yes",
                                       "No",
                                       "Unsure"))) |>
  mutate(fall_causes___poor_balance_or_coordination = factor(fall_causes___poor_balance_or_coordination,
                                              levels = c(0,
                                                         1),
                                              labels = c("Unchecked",
                                                         "Checked"))) |>
  mutate(fall_causes___vision_problems = factor(fall_causes___vision_problems,
                                              levels = c(0,
                                                         1),
                                              labels = c("Unchecked",
                                                         "Checked"))) |>
  mutate(fall_causes___medication_side_effects = factor(fall_causes___medication_side_effects,
                                              levels = c(0,
                                                         1),
                                              labels = c("Unchecked",
                                                         "Checked"))) |>
  mutate(fall_causes___clutter_or_obstacles_in_the_home = factor(fall_causes___clutter_or_obstacles_in_the_home,
                                              levels = c(0,
                                                         1),
                                              labels = c("Unchecked",
                                                         "Checked"))) |>
  mutate(fall_causes___slippery_surfaces = factor(fall_causes___slippery_surfaces,
                                              levels = c(0,
                                                         1),
                                              labels = c("Unchecked",
                                                         "Checked"))) |>
  mutate(fall_causes___weak_muscles = factor(fall_causes___weak_muscles,
                                              levels = c(0,
                                                         1),
                                              labels = c("Unchecked",
                                                         "Checked"))) |>
  mutate(fall_causes___other = factor(fall_causes___other,
                                              levels = c(0,
                                                         1),
                                              labels = c("Unchecked",
                                                         "Checked"))) |>
  mutate(fp_aids = factor(fp_aids,
                          levels = c(1,
                                     0),
                          labels = c("Yes",
                                     "No"))) |>
  mutate(fp_home_mods = factor(fp_home_mods,
                          levels = c(1,
                                     0),
                          labels = c("Yes",
                                     "No"))) |>
  mutate(fall_avoid = factor(fall_avoid,
                          levels = c(1,
                                     0),
                          labels = c("Yes",
                                     "No"))) |>
  mutate(fp_exercise = factor(fp_exercise,
                          levels = c(1,
                                     0),
                          labels = c("Yes",
                                     "No"))) |>
  mutate(fp_exercise_freq = factor(fp_exercise_freq,
                          levels = c(1,
                                     0),
                          labels = c("Yes",
                                     "No"))) |>
  mutate(exercise_increase = factor(exercise_increase,
                            levels = c("Yes",
                                       "No",
                                       "Maybe"),
                            labels = c("Yes",
                                       "No",
                                       "Maybe"))) |>
  mutate(section_2_knowledge_perception_and_practices_complete = factor(section_2_knowledge_perception_and_practices_complete,
                                                                        levels = c(0,
                                                                                   1,
                                                                                   2),
                                                                        labels = c("Incomplete",
                                                                                   "Unverified",
                                                                                   "Complete"))) |>
  mutate(fallen_withn_past_year = factor(fallen_withn_past_year,
                                   levels = c(1,
                                              0),
                                   labels = c("Yes",
                                              "No"))) |>
  mutate(rec_aids = factor(rec_aids,
                                   levels = c(1,
                                              0),
                                   labels = c("Yes",
                                              "No"))) |>
  mutate(steady_walk = factor(steady_walk,
                                   levels = c(1,
                                              0),
                                   labels = c("Yes",
                                              "No"))) |>
  mutate(steady_home = factor(steady_home,
                                   levels = c(1,
                                              0),
                                   labels = c("Yes",
                                              "No"))) |>
  mutate(fall_worry = factor(fall_worry,
                                   levels = c(1,
                                              0),
                                   labels = c("Yes",
                                              "No"))) |>
  mutate(steady_chair = factor(steady_chair,
                                   levels = c(1,
                                              0),
                                   labels = c("Yes",
                                              "No"))) |>
  mutate(risk_curb = factor(risk_curb,
                                   levels = c(1,
                                              0),
                                   labels = c("Yes",
                                              "No"))) |>
  mutate(risk_toilet = factor(risk_toilet,
                                   levels = c(1,
                                              0),
                                   labels = c("Yes",
                                              "No"))) |>
  mutate(risk_feet = factor(risk_feet,
                                   levels = c(1,
                                              0),
                                   labels = c("Yes",
                                              "No"))) |>
  mutate(risk_meds = factor(risk_meds,
                                   levels = c(1,
                                              0),
                                   labels = c("Yes",
                                              "No"))) |>
  mutate(risk_meds2 = factor(risk_meds2,
                                   levels = c(1,
                                              0),
                                   labels = c("Yes",
                                              "No"))) |>
  mutate(risk_mental = factor(risk_mental,
                                   levels = c(1,
                                              0),
                                   labels = c("Yes",
                                              "No"))) |>
  mutate(check_your_risk_on_falling_complete = factor(check_your_risk_on_falling_complete,
                                                      levels = c(0,
                                                                 1,
                                                                 2),
                                                      labels = c("Incomplete",
                                                                 "Unverified",
                                                                 "Complete"))) |>
  mutate(event_attending_complete = factor(event_attending_complete,
                                           levels = c(0,
                                                      1,
                                                      2),
                                           labels = c("Incomplete",
                                                      "Unverified",
                                                      "Complete"))) |>
  mutate(event_attended = factor(event_attended,
                                 levels = c(2:12),
                                 labels = c("9/3 Home Safety Check: A Fall Prevention Walkthrough",
                                            "9/10 Watch Your Step: Fall Prevention in Motion",
                                            "9/17 No Falls Fair & Mini Assessment Clinic",
                                            "9/17 Falls Prevention Presentation Exercises to Help Make You Stronger",
                                            "9/23 Fragility Fractures: What You Need to Know",
                                            "9/26 Arroyo Gardens Independent and Assisted Living - Falls Prevention and Awareness Health Fair",
                                            "9/30 Reclaiming Confidence, Connection and Wellbeing After a Fall",
                                            "9/30 Carondelet St. Josephs Hospital Trauma Center",
                                            "10/9 PCOA Falls Prevention Assessment Lab",
                                            "10/17 Banner and the University of Arizona Falls Prevention Fair",
                                            "10/25 Feast for your Brain")))

#Setting Labels for Banner Data (can also be set for other two datasets)
label(cleanedBanner$participant_id) = "Participant ID"
label(cleanedBanner$redcap_survey_identifier) = "Survey Identifier"
label(cleanedBanner$section_1_timestamp) = "Survey Timestamp"
label(cleanedBanner$age1) = "Age"
label(cleanedBanner$gender) = "How do you describe yourself?"
label(cleanedBanner$gender_self_describe) = "gender_self_describe"
label(cleanedBanner$fall_yr) = "Have you fallen in the last year?"
label(cleanedBanner$falls_number) = "If yes, how many times?"
label(cleanedBanner$health_risks___poor_balance) = "Do you have any health conditions that increase your risk of falling (e.g., poor balance, arthritis, vision problems)? (choice=Poor balance)"
label(cleanedBanner$health_risks___arthritis) = "Do you have any health conditions that increase your risk of falling (e.g., poor balance, arthritis, vision problems)? (choice=Arthritis)"
label(cleanedBanner$health_risks___vision_problems) = "Do you have any health conditions that increase your risk of falling (e.g., poor balance, arthritis, vision problems)? (choice=Vision Problems)"
label(cleanedBanner$health_risks___other) = "Do you have any health conditions that increase your risk of falling (e.g., poor balance, arthritis, vision problems)? (choice=Other)"
label(cleanedBanner$health_risks_other) = "health_risks_other"
label(cleanedBanner$section_1_complete) = "Complete?"
label(cleanedBanner$section_2_knowledge_perception_and_practices_timestamp) = "Survey Timestamp"
label(cleanedBanner$fp_belief) = "Do you believe falls can be prevented?"
label(cleanedBanner$fall_causes___poor_balance_or_coordination) = "What do you think are the most common causes of falls? (Select all that apply) (choice=Poor balance or coordination)"
label(cleanedBanner$fall_causes___vision_problems) = "What do you think are the most common causes of falls? (Select all that apply) (choice=Vision problems)"
label(cleanedBanner$fall_causes___medication_side_effects) = "What do you think are the most common causes of falls? (Select all that apply) (choice=Medication side effects)"
label(cleanedBanner$fall_causes___clutter_or_obstacles_in_the_home) = "What do you think are the most common causes of falls? (Select all that apply) (choice=Clutter or obstacles in the home)"
label(cleanedBanner$fall_causes___slippery_surfaces) = "What do you think are the most common causes of falls? (Select all that apply) (choice=Slippery surfaces)"
label(cleanedBanner$fall_causes___weak_muscles) = "What do you think are the most common causes of falls? (Select all that apply) (choice=Weak muscles)"
label(cleanedBanner$fall_causes___other) = "What do you think are the most common causes of falls? (Select all that apply) (choice=Other)"
label(cleanedBanner$fall_causes_other) = "fall_causes_other"
label(cleanedBanner$fp_importance) = "How important do you think falls prevention is compared to other health concerns (e.g., heart disease, diabetes)? (Scale: 0 = Not important, 10 = Most important)"
label(cleanedBanner$fall_risk) = "How likely do you think you are to fall in the next year? (Scale: 0 = Not likely, 10 = Very likely)"
label(cleanedBanner$fall_severity) = "How likely do you think a fall would result in serious injury (e.g., broken bones, hospitalization)? (Scale: 0 = Not likely, 10 = Very likely)"
label(cleanedBanner$fp_aids) = "Do you use any assistive devices (e.g., cane, walker) when walking?"
label(cleanedBanner$fp_home_mods) = "Have you made changes to your home to prevent falls (e.g., installing handrails, removing rugs)?"
label(cleanedBanner$fall_avoid) = "Are there activities you avoid because youre afraid of falling (e.g., going for a walk, using stairs)?"
label(cleanedBanner$fp_exercise) = "Do you exercise regularly to improve balance and strength?"
label(cleanedBanner$fp_exercise_freq) = "Do you engage in regular physical activities (e.g., walking, swimming, fitness classes)?"
label(cleanedBanner$exercise_increase) = "Are you eager to increase your participation in physical and recreational activities in the coming year?"
label(cleanedBanner$section_2_knowledge_perception_and_practices_complete) = "Complete?"
label(cleanedBanner$check_your_risk_on_falling_timestamp) = "Survey Timestamp"
label(cleanedBanner$fallen_withn_past_year) = "I have fallen in the past year"
label(cleanedBanner$rec_aids) = "I use or have been advised to use a cane or walker to get around safely"
label(cleanedBanner$steady_walk) = "Sometimes I feel unsteady when I am walking"
label(cleanedBanner$steady_home) = "I steady myself by holding onto furniture when walking at home"
label(cleanedBanner$fall_worry) = "I am worried about falling"
label(cleanedBanner$steady_chair) = "I need to push with my hands to stand up from a chair"
label(cleanedBanner$risk_curb) = "I have some trouble stepping up onto a curb"
label(cleanedBanner$risk_toilet) = "I often have to rush to the toilet"
label(cleanedBanner$risk_feet) = "I have lost some feeling in my feet"
label(cleanedBanner$risk_meds) = "I take medicine that sometimes makes me feel light-headed or more tired than usual"
label(cleanedBanner$risk_meds2) = "I take medicine to help me sleep or improve my mood"
label(cleanedBanner$risk_mental) = "I often feel sad or depressed"
label(cleanedBanner$check_your_risk_on_falling_complete) = "Complete?"
label(cleanedBanner$event_attending_timestamp) = "Survey Timestamp"
label(cleanedBanner$event_attended) = "Which event are you attending?"
label(cleanedBanner$event_attending_complete) = "Complete?"


#Visualizations for Banner Data
cleanedBanner |>
  ggplot(mapping = aes(x= age1, fill = gender)) + 
  geom_bar() +
  labs(title = "Age Distribution of Survey Participants", 
       subtitle = "For Banner and UA Falls Prevention Fair", 
       x = "Age Range (years)", y = "Count") +
  theme_minimal() +
  theme(legend.position = c(0.85,0.85)) +
  scale_fill_manual(values = c("Male" = "#AB0520",
                               "Female" = "#0C234B",
                               "Prefer not to say" = "#F4EDE5"))

cleanedBanner |>
  ggplot(mapping = aes(x = fall_risk, fill = age1)) +
  geom_histogram() +
  scale_x_continuous(breaks = seq(0,10, by = 1)) +
  scale_fill_manual(values = c("30-39" = "#1E5288",
                               "40-49" = "#E2E9EB",
                               "50-59" = "#0C234B",
                               "60-69" = "#378DBD",
                               "70-79" = "#AB0520",
                               "80+" = "#F4EDE5")) +
  theme_minimal() +
  theme(legend.position = c(0.8,0.85)) +
  labs(title = "Participants' Self-Evaluated Fall Risk",
       subtitle = "For Banner and UA Falls Prevention Fair",
       y = "Count",
       fill = "Age Range")
cleanedBanner |>
  ggplot(mapping = aes(x = fp_belief, fill = age1)) +
  geom_bar() +
  scale_fill_manual(values = c("40-49" = "#E2E9EB",
                               "50-59" = "#0C234B",
                               "60-69" = "#378DBD",
                               "70-79" = "#AB0520",
                               "80+" = "#F4EDE5")) +
  theme_minimal() +
  theme(legend.position = c(0.75,0.75)) +
  labs(title = "Participants' Belief in Fall Prevention",
       subtitle = "For Banner and UA Falls Prevention Fair",
       y = "Count",
       fill = "Age Range")

#Setting factors for Arroyo Garden Data
cleanedGarden <- rawGardendata |>
  mutate(age1 = factor(age1,
                       levels = c("40_49",
                                  "50_59",
                                  "60_69",
                                  "70_79",
                                  "80_"),
                       labels = c("40-49", 
                                  "50-59", 
                                  "60-69", 
                                  "70-79", 
                                  "80+"))) |>
  mutate(gender = factor(gender,
                         levels = c("male",
                                    "female",
                                    "non_binary",
                                    "prefer_to_self_describe",
                                    "prefer_not_to_say"),
                         labels = c("Male",
                                    "Female",
                                    "Non-binary/third gender",
                                    "Prefer to self describe",
                                    "Prefer not to say"))) |>
  mutate(fall_yr = factor(fall_yr,
                          levels = c(1, 
                                     0),
                          labels = c("Yes",
                                     "No"))) |>
  mutate(falls_number = factor(falls_number,
                               levels = c("once",
                                          "2_to_3_times",
                                          "more_than_3_times"),
                               labels = c("Once",
                                          "2-3 times",
                                          "More than 3 times"))) |>
  mutate(health_risks___poor_balance = factor(health_risks___poor_balance,
                                              levels = c(0,
                                                         1),
                                              labels = c("Unchecked",
                                                         "Checked"))) |>
  mutate(health_risks___arthritis = factor(health_risks___arthritis,
                                           levels = c(0,
                                                      1),
                                           labels = c("Unchecked",
                                                      "Checked"))) |>
  mutate(health_risks___vision_problems = factor(health_risks___vision_problems,
                                                 levels = c(0,
                                                            1),
                                                 labels = c("Unchecked",
                                                            "Checked"))) |>
  mutate(health_risks___other = factor(health_risks___other,
                                       levels = c(0,
                                                  1),
                                       labels = c("Unchecked",
                                                  "Checked"))) |>
  mutate(section_1_complete = factor(section_1_complete,
                                     levels = c(0,
                                                1,
                                                2),
                                     labels = c("Incomplete",
                                                "Unverified",
                                                "Complete"))) |>
  mutate(fp_belief = factor(fp_belief,
                            levels = c("yes",
                                       "no",
                                       "unsure"),
                            labels = c("Yes",
                                       "No",
                                       "Unsure"))) |>
  mutate(fall_causes___poor_balance_or_coordination = factor(fall_causes___poor_balance_or_coordination,
                                                             levels = c(0,
                                                                        1),
                                                             labels = c("Unchecked",
                                                                        "Checked"))) |>
  mutate(fall_causes___vision_problems = factor(fall_causes___vision_problems,
                                                levels = c(0,
                                                           1),
                                                labels = c("Unchecked",
                                                           "Checked"))) |>
  mutate(fall_causes___medication_side_effects = factor(fall_causes___medication_side_effects,
                                                        levels = c(0,
                                                                   1),
                                                        labels = c("Unchecked",
                                                                   "Checked"))) |>
  mutate(fall_causes___clutter_or_obstacles_in_the_home = factor(fall_causes___clutter_or_obstacles_in_the_home,
                                                                 levels = c(0,
                                                                            1),
                                                                 labels = c("Unchecked",
                                                                            "Checked"))) |>
  mutate(fall_causes___slippery_surfaces = factor(fall_causes___slippery_surfaces,
                                                  levels = c(0,
                                                             1),
                                                  labels = c("Unchecked",
                                                             "Checked"))) |>
  mutate(fall_causes___weak_muscles = factor(fall_causes___weak_muscles,
                                             levels = c(0,
                                                        1),
                                             labels = c("Unchecked",
                                                        "Checked"))) |>
  mutate(fall_causes___other = factor(fall_causes___other,
                                      levels = c(0,
                                                 1),
                                      labels = c("Unchecked",
                                                 "Checked"))) |>
  mutate(fp_aids = factor(fp_aids,
                          levels = c(1,
                                     0),
                          labels = c("Yes",
                                     "No"))) |>
  mutate(fp_home_mods = factor(fp_home_mods,
                               levels = c(1,
                                          0),
                               labels = c("Yes",
                                          "No"))) |>
  mutate(fall_avoid = factor(fall_avoid,
                             levels = c(1,
                                        0),
                             labels = c("Yes",
                                        "No"))) |>
  mutate(fp_exercise = factor(fp_exercise,
                              levels = c(1,
                                         0),
                              labels = c("Yes",
                                         "No"))) |>
  mutate(fp_exercise_freq = factor(fp_exercise_freq,
                                   levels = c(1,
                                              0),
                                   labels = c("Yes",
                                              "No"))) |>
  mutate(exercise_increase = factor(exercise_increase,
                                    levels = c("Yes",
                                               "No",
                                               "Maybe"),
                                    labels = c("Yes",
                                               "No",
                                               "Maybe"))) |>
  mutate(section_2_knowledge_perception_and_practices_complete = factor(section_2_knowledge_perception_and_practices_complete,
                                                                        levels = c(0,
                                                                                   1,
                                                                                   2),
                                                                        labels = c("Incomplete",
                                                                                   "Unverified",
                                                                                   "Complete"))) |>
  mutate(fallen_withn_past_year = factor(fallen_withn_past_year,
                                         levels = c(1,
                                                    0),
                                         labels = c("Yes",
                                                    "No"))) |>
  mutate(rec_aids = factor(rec_aids,
                           levels = c(1,
                                      0),
                           labels = c("Yes",
                                      "No"))) |>
  mutate(steady_walk = factor(steady_walk,
                              levels = c(1,
                                         0),
                              labels = c("Yes",
                                         "No"))) |>
  mutate(steady_home = factor(steady_home,
                              levels = c(1,
                                         0),
                              labels = c("Yes",
                                         "No"))) |>
  mutate(fall_worry = factor(fall_worry,
                             levels = c(1,
                                        0),
                             labels = c("Yes",
                                        "No"))) |>
  mutate(steady_chair = factor(steady_chair,
                               levels = c(1,
                                          0),
                               labels = c("Yes",
                                          "No"))) |>
  mutate(risk_curb = factor(risk_curb,
                            levels = c(1,
                                       0),
                            labels = c("Yes",
                                       "No"))) |>
  mutate(risk_toilet = factor(risk_toilet,
                              levels = c(1,
                                         0),
                              labels = c("Yes",
                                         "No"))) |>
  mutate(risk_feet = factor(risk_feet,
                            levels = c(1,
                                       0),
                            labels = c("Yes",
                                       "No"))) |>
  mutate(risk_meds = factor(risk_meds,
                            levels = c(1,
                                       0),
                            labels = c("Yes",
                                       "No"))) |>
  mutate(risk_meds2 = factor(risk_meds2,
                             levels = c(1,
                                        0),
                             labels = c("Yes",
                                        "No"))) |>
  mutate(risk_mental = factor(risk_mental,
                              levels = c(1,
                                         0),
                              labels = c("Yes",
                                         "No"))) |>
  mutate(check_your_risk_on_falling_complete = factor(check_your_risk_on_falling_complete,
                                                      levels = c(0,
                                                                 1,
                                                                 2),
                                                      labels = c("Incomplete",
                                                                 "Unverified",
                                                                 "Complete"))) |>
  mutate(event_attending_complete = factor(event_attending_complete,
                                           levels = c(0,
                                                      1,
                                                      2),
                                           labels = c("Incomplete",
                                                      "Unverified",
                                                      "Complete"))) |>
  mutate(event_attended = factor(event_attended,
                                 levels = c(2:12),
                                 labels = c("9/3 Home Safety Check: A Fall Prevention Walkthrough",
                                            "9/10 Watch Your Step: Fall Prevention in Motion",
                                            "9/17 No Falls Fair & Mini Assessment Clinic",
                                            "9/17 Falls Prevention Presentation Exercises to Help Make You Stronger",
                                            "9/23 Fragility Fractures: What You Need to Know",
                                            "9/26 Arroyo Gardens Independent and Assisted Living - Falls Prevention and Awareness Health Fair",
                                            "9/30 Reclaiming Confidence, Connection and Wellbeing After a Fall",
                                            "9/30 Carondelet St. Josephs Hospital Trauma Center",
                                            "10/9 PCOA Falls Prevention Assessment Lab",
                                            "10/17 Banner and the University of Arizona Falls Prevention Fair",
                                            "10/25 Feast for your Brain")))
# Visualizations for Arroyo Garden data
cleanedGarden |>
  ggplot(mapping = aes(x= age1, fill = gender)) + 
  geom_bar() +
  labs(title = "Age Distribution of Survey Participants", 
       subtitle = "For Arroyo Gardens", 
       x = "Age Range (years)", y = "Count",
       fill = "How do you describe yourself?") +
  theme_minimal() +
  theme(legend.position = c(0.15,0.85)) +
  scale_fill_manual(values = c("Male" = "#AB0520",
                               "Female" = "#0C234B"))

cleanedGarden |>
  ggplot(mapping = aes(x = fall_risk, 
                       fill = age1)) +
  geom_histogram() +
  scale_x_continuous(breaks = seq(0,10, by = 1)) +
  scale_fill_manual(values = c("30-39" = "#1E5288",
                               "40-49" = "#E2E9EB",
                               "50-59" = "#0C234B",
                               "60-69" = "#378DBD",
                               "70-79" = "#AB0520",
                               "80+" = "#F4EDE5")) +
  theme_minimal() +
  theme(legend.position = c(0.8,0.85)) +
  labs(title = "Participants' Self-Evaluated Fall Risk",
       subtitle = "For Arroyo Gardens",
       y = "Count",
       x = "How likely do you think you are to fall in the next year? (Scale: 0 = Not likely, 10 = Very likely)",
       fill = "Age Range")

cleanedGarden |>
  ggplot(mapping = aes(x = fp_belief, fill = age1)) +
  geom_bar() +
  scale_fill_manual(values = c("30-39" = "#1E5288",
                               "40-49" = "#E2E9EB",
                               "50-59" = "#0C234B",
                               "60-69" = "#378DBD",
                               "70-79" = "#AB0520",
                               "80+" = "#F4EDE5")) +
  theme_minimal() +
  theme(legend.position = c(0.8,0.75)) +
  labs(title = "Participants' Belief in Fall Prevention",
       subtitle = "For Arroyo Gardens",
       y = "Count",
       x = "Do you believe falls can be prevented?",
       fill = "Age Range")

#Setting factors for PCOA data
cleanedPCOA <- rawPCOAdata |>
  mutate(age1 = factor(age1,
                       levels = c("40_49",
                                  "50_59",
                                  "60_69",
                                  "70_79",
                                  "80_"),
                       labels = c("40-49", 
                                  "50-59", 
                                  "60-69", 
                                  "70-79", 
                                  "80+"))) |>
  mutate(gender = factor(gender,
                         levels = c("male",
                                    "female",
                                    "non_binary",
                                    "prefer_to_self_describe",
                                    "prefer_not_to_say"),
                         labels = c("Male",
                                    "Female",
                                    "Non-binary/third gender",
                                    "Prefer to self describe",
                                    "Prefer not to say"))) |>
  mutate(fall_yr = factor(fall_yr,
                          levels = c(1, 
                                     0),
                          labels = c("Yes",
                                     "No"))) |>
  mutate(falls_number = factor(falls_number,
                               levels = c("once",
                                          "2_to_3_times",
                                          "more_than_3_times"),
                               labels = c("Once",
                                          "2-3 times",
                                          "More than 3 times"))) |>
  mutate(health_risks___poor_balance = factor(health_risks___poor_balance,
                                              levels = c(0,
                                                         1),
                                              labels = c("Unchecked",
                                                         "Checked"))) |>
  mutate(health_risks___arthritis = factor(health_risks___arthritis,
                                           levels = c(0,
                                                      1),
                                           labels = c("Unchecked",
                                                      "Checked"))) |>
  mutate(health_risks___vision_problems = factor(health_risks___vision_problems,
                                                 levels = c(0,
                                                            1),
                                                 labels = c("Unchecked",
                                                            "Checked"))) |>
  mutate(health_risks___other = factor(health_risks___other,
                                       levels = c(0,
                                                  1),
                                       labels = c("Unchecked",
                                                  "Checked"))) |>
  mutate(section_1_complete = factor(section_1_complete,
                                     levels = c(0,
                                                1,
                                                2),
                                     labels = c("Incomplete",
                                                "Unverified",
                                                "Complete"))) |>
  mutate(fp_belief = factor(fp_belief,
                            levels = c("yes",
                                       "no",
                                       "unsure"),
                            labels = c("Yes",
                                       "No",
                                       "Unsure"))) |>
  mutate(fall_causes___poor_balance_or_coordination = factor(fall_causes___poor_balance_or_coordination,
                                                             levels = c(0,
                                                                        1),
                                                             labels = c("Unchecked",
                                                                        "Checked"))) |>
  mutate(fall_causes___vision_problems = factor(fall_causes___vision_problems,
                                                levels = c(0,
                                                           1),
                                                labels = c("Unchecked",
                                                           "Checked"))) |>
  mutate(fall_causes___medication_side_effects = factor(fall_causes___medication_side_effects,
                                                        levels = c(0,
                                                                   1),
                                                        labels = c("Unchecked",
                                                                   "Checked"))) |>
  mutate(fall_causes___clutter_or_obstacles_in_the_home = factor(fall_causes___clutter_or_obstacles_in_the_home,
                                                                 levels = c(0,
                                                                            1),
                                                                 labels = c("Unchecked",
                                                                            "Checked"))) |>
  mutate(fall_causes___slippery_surfaces = factor(fall_causes___slippery_surfaces,
                                                  levels = c(0,
                                                             1),
                                                  labels = c("Unchecked",
                                                             "Checked"))) |>
  mutate(fall_causes___weak_muscles = factor(fall_causes___weak_muscles,
                                             levels = c(0,
                                                        1),
                                             labels = c("Unchecked",
                                                        "Checked"))) |>
  mutate(fall_causes___other = factor(fall_causes___other,
                                      levels = c(0,
                                                 1),
                                      labels = c("Unchecked",
                                                 "Checked"))) |>
  mutate(fp_aids = factor(fp_aids,
                          levels = c(1,
                                     0),
                          labels = c("Yes",
                                     "No"))) |>
  mutate(fp_home_mods = factor(fp_home_mods,
                               levels = c(1,
                                          0),
                               labels = c("Yes",
                                          "No"))) |>
  mutate(fall_avoid = factor(fall_avoid,
                             levels = c(1,
                                        0),
                             labels = c("Yes",
                                        "No"))) |>
  mutate(fp_exercise = factor(fp_exercise,
                              levels = c(1,
                                         0),
                              labels = c("Yes",
                                         "No"))) |>
  mutate(fp_exercise_freq = factor(fp_exercise_freq,
                                   levels = c(1,
                                              0),
                                   labels = c("Yes",
                                              "No"))) |>
  mutate(exercise_increase = factor(exercise_increase,
                                    levels = c("Yes",
                                               "No",
                                               "Maybe"),
                                    labels = c("Yes",
                                               "No",
                                               "Maybe"))) |>
  mutate(section_2_knowledge_perception_and_practices_complete = factor(section_2_knowledge_perception_and_practices_complete,
                                                                        levels = c(0,
                                                                                   1,
                                                                                   2),
                                                                        labels = c("Incomplete",
                                                                                   "Unverified",
                                                                                   "Complete"))) |>
  mutate(fallen_withn_past_year = factor(fallen_withn_past_year,
                                         levels = c(1,
                                                    0),
                                         labels = c("Yes",
                                                    "No"))) |>
  mutate(rec_aids = factor(rec_aids,
                           levels = c(1,
                                      0),
                           labels = c("Yes",
                                      "No"))) |>
  mutate(steady_walk = factor(steady_walk,
                              levels = c(1,
                                         0),
                              labels = c("Yes",
                                         "No"))) |>
  mutate(steady_home = factor(steady_home,
                              levels = c(1,
                                         0),
                              labels = c("Yes",
                                         "No"))) |>
  mutate(fall_worry = factor(fall_worry,
                             levels = c(1,
                                        0),
                             labels = c("Yes",
                                        "No"))) |>
  mutate(steady_chair = factor(steady_chair,
                               levels = c(1,
                                          0),
                               labels = c("Yes",
                                          "No"))) |>
  mutate(risk_curb = factor(risk_curb,
                            levels = c(1,
                                       0),
                            labels = c("Yes",
                                       "No"))) |>
  mutate(risk_toilet = factor(risk_toilet,
                              levels = c(1,
                                         0),
                              labels = c("Yes",
                                         "No"))) |>
  mutate(risk_feet = factor(risk_feet,
                            levels = c(1,
                                       0),
                            labels = c("Yes",
                                       "No"))) |>
  mutate(risk_meds = factor(risk_meds,
                            levels = c(1,
                                       0),
                            labels = c("Yes",
                                       "No"))) |>
  mutate(risk_meds2 = factor(risk_meds2,
                             levels = c(1,
                                        0),
                             labels = c("Yes",
                                        "No"))) |>
  mutate(risk_mental = factor(risk_mental,
                              levels = c(1,
                                         0),
                              labels = c("Yes",
                                         "No"))) |>
  mutate(check_your_risk_on_falling_complete = factor(check_your_risk_on_falling_complete,
                                                      levels = c(0,
                                                                 1,
                                                                 2),
                                                      labels = c("Incomplete",
                                                                 "Unverified",
                                                                 "Complete"))) |>
  mutate(event_attending_complete = factor(event_attending_complete,
                                           levels = c(0,
                                                      1,
                                                      2),
                                           labels = c("Incomplete",
                                                      "Unverified",
                                                      "Complete"))) |>
  mutate(event_attended = factor(event_attended,
                                 levels = c(2:12),
                                 labels = c("9/3 Home Safety Check: A Fall Prevention Walkthrough",
                                            "9/10 Watch Your Step: Fall Prevention in Motion",
                                            "9/17 No Falls Fair & Mini Assessment Clinic",
                                            "9/17 Falls Prevention Presentation Exercises to Help Make You Stronger",
                                            "9/23 Fragility Fractures: What You Need to Know",
                                            "9/26 Arroyo Gardens Independent and Assisted Living - Falls Prevention and Awareness Health Fair",
                                            "9/30 Reclaiming Confidence, Connection and Wellbeing After a Fall",
                                            "9/30 Carondelet St. Josephs Hospital Trauma Center",
                                            "10/9 PCOA Falls Prevention Assessment Lab",
                                            "10/17 Banner and the University of Arizona Falls Prevention Fair",
                                            "10/25 Feast for your Brain")))

#Visualizations for PCOA data
cleanedPCOA |>
  ggplot(mapping = aes(x= age1, fill = gender)) + 
  geom_bar() +
  labs(title = "Age Distribution of Survey Participants", 
       subtitle = "For PCOA", 
       x = "Age Range (years)", y = "Count",
       fill = "How do you describe yourself?") +
  theme_minimal() +
  theme(legend.position = c(0.15,0.85)) +
  scale_fill_manual(values = c("Male" = "#AB0520",
                               "Female" = "#0C234B"))

cleanedPCOA |>
  ggplot(mapping = aes(x = fall_risk, 
                       fill = age1)) +
  geom_histogram() +
  scale_x_continuous(breaks = seq(0,10, by = 1)) +
  scale_fill_manual(values = c("30-39" = "#1E5288",
                               "40-49" = "#E2E9EB",
                               "50-59" = "#0C234B",
                               "60-69" = "#378DBD",
                               "70-79" = "#AB0520",
                               "80+" = "#F4EDE5")) +
  theme_minimal() +
  theme(legend.position = c(0.8,0.85)) +
  labs(title = "Participants' Self-Evaluated Fall Risk",
       subtitle = "For PCOA",
       y = "Count",
       x = "How likely do you think you are to fall in the next year? (Scale: 0 = Not likely, 10 = Very likely)",
       fill = "Age Range")

cleanedPCOA |>
  ggplot(mapping = aes(x = fp_belief, fill = age1)) +
  geom_bar() +
  scale_fill_manual(values = c("30-39" = "#1E5288",
                               "40-49" = "#E2E9EB",
                               "50-59" = "#0C234B",
                               "60-69" = "#378DBD",
                               "70-79" = "#AB0520",
                               "80+" = "#F4EDE5")) +
  theme_minimal() +
  theme(legend.position = c(0.8,0.75)) +
  labs(title = "Participants' Belief in Fall Prevention",
       subtitle = "For PCOA",
       y = "Count",
       x = "Do you believe falls can be prevented?",
       fill = "Age Range")

# Combining datasets

combinedraw <- read.csv("~/Downloads/FallsPreventionSurve_DATA_2026-01-21_1133.csv") |>
  full_join(read.csv("~/Downloads/FallsPreventionSurve_DATA_2026-01-23_1026.csv")) |>
  full_join(read.csv("~/Downloads/FallsPreventionSurve_DATA_2026-01-22_1508.csv"))

# Setting factors
combinedclean <- combinedraw |>
  mutate(age1 = factor(age1,
                       levels = c("30_39",
                                  "40_49",
                                  "50_59",
                                  "60_69",
                                  "70_79",
                                  "80_"),
                       labels = c("30-39", "40-49", "50-59", "60-69", "70-79", "80+"))) |>
  mutate(gender = factor(gender,
                         levels = c("male",
                                    "female",
                                    "non_binary",
                                    "prefer_to_self_describe",
                                    "prefer_not_to_say"),
                         labels = c("Male",
                                    "Female",
                                    "Non-binary/third gender",
                                    "Prefer to self describe",
                                    "Prefer not to say"))) |>
  mutate(fall_yr = factor(fall_yr,
                          levels = c(1, 
                                     0),
                          labels = c("Yes",
                                     "No"))) |>
  mutate(falls_number = factor(falls_number,
                               levels = c("once",
                                          "2_to_3_times",
                                          "more_than_3_times"),
                               labels = c("Once",
                                          "2-3 times",
                                          "More than 3 times"))) |>
  mutate(health_risks___poor_balance = factor(health_risks___poor_balance,
                                              levels = c(0,
                                                         1),
                                              labels = c("Unchecked",
                                                         "Checked"))) |>
  mutate(health_risks___arthritis = factor(health_risks___arthritis,
                                           levels = c(0,
                                                      1),
                                           labels = c("Unchecked",
                                                      "Checked"))) |>
  mutate(health_risks___vision_problems = factor(health_risks___vision_problems,
                                                 levels = c(0,
                                                            1),
                                                 labels = c("Unchecked",
                                                            "Checked"))) |>
  mutate(health_risks___other = factor(health_risks___other,
                                       levels = c(0,
                                                  1),
                                       labels = c("Unchecked",
                                                  "Checked"))) |>
  mutate(section_1_complete = factor(section_1_complete,
                                     levels = c(0,
                                                1,
                                                2),
                                     labels = c("Incomplete",
                                                "Unverified",
                                                "Complete"))) |>
  mutate(fp_belief = factor(fp_belief,
                            levels = c("yes",
                                       "no",
                                       "unsure"),
                            labels = c("Yes",
                                       "No",
                                       "Unsure"))) |>
  mutate(fall_causes___poor_balance_or_coordination = factor(fall_causes___poor_balance_or_coordination,
                                                             levels = c(0,
                                                                        1),
                                                             labels = c("Unchecked",
                                                                        "Checked"))) |>
  mutate(fall_causes___vision_problems = factor(fall_causes___vision_problems,
                                                levels = c(0,
                                                           1),
                                                labels = c("Unchecked",
                                                           "Checked"))) |>
  mutate(fall_causes___medication_side_effects = factor(fall_causes___medication_side_effects,
                                                        levels = c(0,
                                                                   1),
                                                        labels = c("Unchecked",
                                                                   "Checked"))) |>
  mutate(fall_causes___clutter_or_obstacles_in_the_home = factor(fall_causes___clutter_or_obstacles_in_the_home,
                                                                 levels = c(0,
                                                                            1),
                                                                 labels = c("Unchecked",
                                                                            "Checked"))) |>
  mutate(fall_causes___slippery_surfaces = factor(fall_causes___slippery_surfaces,
                                                  levels = c(0,
                                                             1),
                                                  labels = c("Unchecked",
                                                             "Checked"))) |>
  mutate(fall_causes___weak_muscles = factor(fall_causes___weak_muscles,
                                             levels = c(0,
                                                        1),
                                             labels = c("Unchecked",
                                                        "Checked"))) |>
  mutate(fall_causes___other = factor(fall_causes___other,
                                      levels = c(0,
                                                 1),
                                      labels = c("Unchecked",
                                                 "Checked"))) |>
  mutate(fp_aids = factor(fp_aids,
                          levels = c(1,
                                     0),
                          labels = c("Yes",
                                     "No"))) |>
  mutate(fp_home_mods = factor(fp_home_mods,
                               levels = c(1,
                                          0),
                               labels = c("Yes",
                                          "No"))) |>
  mutate(fall_avoid = factor(fall_avoid,
                             levels = c(1,
                                        0),
                             labels = c("Yes",
                                        "No"))) |>
  mutate(fp_exercise = factor(fp_exercise,
                              levels = c(1,
                                         0),
                              labels = c("Yes",
                                         "No"))) |>
  mutate(fp_exercise_freq = factor(fp_exercise_freq,
                                   levels = c(1,
                                              0),
                                   labels = c("Yes",
                                              "No"))) |>
  mutate(exercise_increase = factor(exercise_increase,
                                    levels = c("Yes",
                                               "No",
                                               "Maybe"),
                                    labels = c("Yes",
                                               "No",
                                               "Maybe"))) |>
  mutate(section_2_knowledge_perception_and_practices_complete = factor(section_2_knowledge_perception_and_practices_complete,
                                                                        levels = c(0,
                                                                                   1,
                                                                                   2),
                                                                        labels = c("Incomplete",
                                                                                   "Unverified",
                                                                                   "Complete"))) |>
  mutate(fallen_withn_past_year = factor(fallen_withn_past_year,
                                         levels = c(1,
                                                    0),
                                         labels = c("Yes",
                                                    "No"))) |>
  mutate(rec_aids = factor(rec_aids,
                           levels = c(1,
                                      0),
                           labels = c("Yes",
                                      "No"))) |>
  mutate(steady_walk = factor(steady_walk,
                              levels = c(1,
                                         0),
                              labels = c("Yes",
                                         "No"))) |>
  mutate(steady_home = factor(steady_home,
                              levels = c(1,
                                         0),
                              labels = c("Yes",
                                         "No"))) |>
  mutate(fall_worry = factor(fall_worry,
                             levels = c(1,
                                        0),
                             labels = c("Yes",
                                        "No"))) |>
  mutate(steady_chair = factor(steady_chair,
                               levels = c(1,
                                          0),
                               labels = c("Yes",
                                          "No"))) |>
  mutate(risk_curb = factor(risk_curb,
                            levels = c(1,
                                       0),
                            labels = c("Yes",
                                       "No"))) |>
  mutate(risk_toilet = factor(risk_toilet,
                              levels = c(1,
                                         0),
                              labels = c("Yes",
                                         "No"))) |>
  mutate(risk_feet = factor(risk_feet,
                            levels = c(1,
                                       0),
                            labels = c("Yes",
                                       "No"))) |>
  mutate(risk_meds = factor(risk_meds,
                            levels = c(1,
                                       0),
                            labels = c("Yes",
                                       "No"))) |>
  mutate(risk_meds2 = factor(risk_meds2,
                             levels = c(1,
                                        0),
                             labels = c("Yes",
                                        "No"))) |>
  mutate(risk_mental = factor(risk_mental,
                              levels = c(1,
                                         0),
                              labels = c("Yes",
                                         "No"))) |>
  mutate(check_your_risk_on_falling_complete = factor(check_your_risk_on_falling_complete,
                                                      levels = c(0,
                                                                 1,
                                                                 2),
                                                      labels = c("Incomplete",
                                                                 "Unverified",
                                                                 "Complete"))) |>
  mutate(event_attending_complete = factor(event_attending_complete,
                                           levels = c(0,
                                                      1,
                                                      2),
                                           labels = c("Incomplete",
                                                      "Unverified",
                                                      "Complete"))) |>
  mutate(event_attended = factor(event_attended,
                                 levels = c(2:12),
                                 labels = c("9/3 Home Safety Check: A Fall Prevention Walkthrough",
                                            "9/10 Watch Your Step: Fall Prevention in Motion",
                                            "9/17 No Falls Fair & Mini Assessment Clinic",
                                            "9/17 Falls Prevention Presentation Exercises to Help Make You Stronger",
                                            "9/23 Fragility Fractures: What You Need to Know",
                                            "9/26 Arroyo Gardens Independent and Assisted Living - Falls Prevention and Awareness Health Fair",
                                            "9/30 Reclaiming Confidence, Connection and Wellbeing After a Fall",
                                            "9/30 Carondelet St. Josephs Hospital Trauma Center",
                                            "10/9 PCOA Falls Prevention Assessment Lab",
                                            "10/17 Banner and the University of Arizona Falls Prevention Fair",
                                            "10/25 Feast for your Brain")))

# Preliminary graphs with combined data
combinedclean |>
  ggplot(mapping = aes(x= age1, fill = gender)) + 
  geom_bar() +
  labs(title = "Age Distribution of Survey Participants", 
       subtitle = "For all survey participants", 
       x = "Age Range (years)", y = "Count") +
  theme_minimal() +
  theme(legend.position = c(0.15,0.85)) +
  scale_fill_manual(values = c("Male" = "#AB0520",
                               "Female" = "#0C234B",
                               "Prefer not to say" = "#F4EDE5"))

combinedclean|>
  ggplot(mapping = aes(x = fall_risk, fill = age1)) +
  geom_histogram() +
  scale_x_continuous(breaks = seq(0,10, by = 1)) +
  scale_fill_manual(values = c("30-39" = "#1E5288",
                               "40-49" = "#E2E9EB",
                               "50-59" = "#0C234B",
                               "60-69" = "#378DBD",
                               "70-79" = "#AB0520",
                               "80+" = "#F4EDE5")) +
  theme_minimal() +
  theme(legend.position = c(0.8,0.85)) +
  labs(title = "Participants' Self-Evaluated Fall Risk",
       subtitle = "For all survey participants",
       y = "Count",
       x = "How likely do you think you are to fall in the next year? (Scale: 0 = Not likely, 10 = Very likely)",
       fill = "Age Range")

combinedclean |>
  ggplot(mapping = aes(x = fp_belief, fill = age1)) +
  geom_bar() +
  scale_fill_manual(values = c("40-49" = "#E2E9EB",
                               "50-59" = "#0C234B",
                               "60-69" = "#378DBD",
                               "70-79" = "#AB0520",
                               "80+" = "#F4EDE5")) +
  theme_minimal() +
  theme(legend.position = c(0.8,0.75)) +
  labs(title = "Participants' Belief in Fall Prevention",
       subtitle = "For all survey participants",
       y = "Count",
       x = "Do you believe falls can be prevented?",
       fill = "Age Range")
