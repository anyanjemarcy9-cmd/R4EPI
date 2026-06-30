# R FOR EPIDEMIOLOGY
library(dplyr)
demo <- tibble(
  id = c("001", "002", "003", "004"),
  age = c(30, 67, 52, 56),
  edu = c(3, 1, 4, 2))
demo <- tibble(
  id = c("001", "002", "003", "004"),
age = c(30, 67, 52, 56),
  edu = c(3, 1, 4, 2),
  edu_char = c(
    "Some college", "Less than high school", "College graduate",
    "High school graduate"))
demo
demo <- demo |>
  mutate(
    edu_f = factor(
      x
      = edu,
      levels = 1:4,
      labels = c(
        "Less than high school", "High school graduate", "Some college",
        "College graduate")))
demo
as.numeric(demo$edu_char)
as.numeric(demo$edu_f)
table(demo$edu_char)
table(demo$edu_f)
demo <- demo |>
  mutate(
    edu_5cat_f = factor(
      x
      = edu,
      levels = 1:5,
      labels = c(
        "Less than high school", "High school graduate", "Some college",
        "College graduate", "Graduate school")))
demo
table(demo$edu_char)
table(demo$edu_5cat_f)
demo <- demo |>
  mutate(
    edu_f_from_char = factor(
      x
      = edu_char,
      levels = c(
        "Less than high school", "High school graduate", "Some college",
        "College graduate", "Graduate school")))
demo
demo <- demo |>
  mutate(
    edu_f = factor(
      x
      = edu,
      levels = 1:4,
      labels = c(
        "Less than high school", "High school graduate", "Some college",
        "College graduate")))
demo
demo <- tibble(
  id = c("001", "002", "003", "004"),
  age = c(30, 67, 52, 56),
  edu = c(3, 1, 4, 2),
  edu_char = c(
    "Some college", "Less than high school", "College graduate",
    "High school graduate"))
demo
demo <- demo |>
  mutate(
    edu_f = factor(
      x
      = edu,
      levels = 1:4,
      labels = c(
        "Less than high school", "High school graduate", "Some college",
        "College graduate")))
demo
as.numeric(demo$edu_char)
as.numeric(demo$edu_f)
table(demo$edu_char)
table(demo$edu_f)
demo <- demo |>
  mutate(
    edu_5cat_f = factor(
      x
      = edu,
      levels = 1:5,
      labels = c(
        "Less than high school", "High school graduate", "Some college",
        "College graduate", "Graduate school")))
demo
table(demo$edu_char)
table(demo$edu_5cat_f)
#coerce a character variable
demo <- demo |>
  mutate(
    edu_f_from_char = factor(
      x
      = edu_char,
      levels = c(
        "Less than high school", "High school graduate", "Some college",
        "College graduate", "Graduate school")))
demo
table(demo$edu_f_from_char)
height_and_weight_20 <- tibble(
  id = c(
    "001", "002", "003", "004", "005", "006", "007", "008", "009", "010", "011",
    "012", "013", "014", "015", "016", "017", "018", "019", "020"
  ),
  sex = c(1, 1, 2, 2, 1, 1, 2, 1, 2, 1, 1, 2, 2, 2, 1, 2, 2, 2, 2, 2),
  sex_f = factor(sex, 1:2, c("Male", "Female")),
  ht_in = c(
    71, 69, 64, 65, 73, 69, 68, 73, 71, 66, 71, 69, 66, 68, 75, 69, 66, 65, 65,
    65
  ),
  wt_lbs = c(
    190, 176, 130, 154, 173, 182, 140, 185, 157, 155, 213, 151, 147, 196, 212,
    190, 194, 176, 176, 102))
#calculating frequencies
table(height_and_weight_20$sex)
gmodels::CrossTable(height_and_weight_20$sex)
#tidyverse
height_and_weight_20 |>
  summarise()
height_and_weight_20 |>
  summarise(n()) #to count rows
height_and_weight_20 |>
  group_by(sex_f) |>
  summarise(n())
height_and_weight_20 |>
  group_by(sex_f) |>
  summarise(number = n())
height_and_weight_20 |>
  count(sex_f)
# calculatig percentages
height_and_weight_20 |>
  count(sex_f) |>
  mutate(prop = n / 20)
height_and_weight_20 |>
  count(sex_f) |>
  mutate(prop = n / sum(n))
height_and_weight_20 |>
  count(sex_f) |>
  mutate(percent = n / sum(n) * 100)
#missing data
height_and_weight_20 <- height_and_weight_20 |>
  mutate(sex_f = replace(sex, c(2, 9), NA)) |>
  print()
height_and_weight_20|>
  count(sex_f) |>
  mutate(percent = n/ sum(n)*100)
height_and_weight_20 |>
  filter(!is.na(sex_f)) |>
  count(sex_f) |>
  mutate(percent = n / sum(n) * 100)
#formatting results
height_and_weight_20 |>
  filter(!is.na(sex_f)) |>
  count(sex_f) |>
  mutate(percent = (n / sum(n) * 100) |> round(2))
#using preqtables
library(freqtables)
height_and_weight_20 |>
  filter(!is.na(sex_f)) |>
  freq_table(sex_f)
#measures of central tendency
height_and_weight_20<-tribble(
  ~id, ~sex, ~ht_in, ~wt_lbs,
  "001", "Male", 71, 190,
  "002", "Male", 69, 177,
  "003", "Female", 64, 130,
  "004", "Female", 65, 153,
  "005", NA, 73, 173,
  "006", "Male", 69, 182,
  "007", "Female", 68, 186,
  "008", NA, 73, 185,
  "009", "Female", 71, 157,
  "010", "Male", 66, 155,
  "011", "Male", 71, 213,
  "012", "Female", 69, 151,
  "013", "Female", 66, 147,
  "014", "Female", 68, 196,
  "015", "Male", 75, 212,
  "016", "Female", 69, 19000,
  "017", "Female", 66, 194,
  "018", "Female", 65, 176,
  "019", "Female", 65, 176,
  "020", "Female", 65, 102
)
mean(height_and_weight_20$ht_in)
mode(height_and_weight_20$ht_in)
mode_val<-function(x){
  value_counts <-table(x)
  max_count <-max(value_counts)
  index <-value_counts== max_count
  unique_values <-names(value_counts)
  result <-unique_values[index]
  no_mode<-length(value_counts)== length(result)
  if (no_mode){
    result <-NA}
  result
}
mode_val(height_and_weight_20$ht_in)
height_and_weight_20 %>%
  summarise(
    min_weight = min(wt_lbs),
    mean_weight = mean(wt_lbs),
    median_weight = median(wt_lbs),
    mode_weight = mode_val(wt_lbs) %>% as.double(),
    max_weight = max(wt_lbs))
#missing data
mean(c(1, 2, 3))
mean(c(1, NA, 3))
mean(c(1,NA,3),na.rm= TRUE)
height_and_weight_20<-height_and_weight_20 %>%
  mutate(ht_in=replace(ht_in,c(1, 2), NA))
height_and_weight_20
summarise(
  min_height = min(ht_in),
  mean_height = mean(ht_in),
  median_height = median(ht_in),
  mode_height = mode_val(ht_in),
  max_height = max(ht_in))
names(height_and_weight_20)
height_and_weight_20 %>%
  filter(!is.na(ht_in)) %>%
  summarise(
    min_height = min(ht_in),
    mean_height = mean(ht_in),
    median_height = median(ht_in),
    mode_height = mode_val(ht_in),
    max_height = max(ht_in))
traceback()
glimpse(ht_and_weght_20)
# using meantables
library(meantables)
height_and_weight_20 %>%
  filter(!is.na(ht_in)) %>%
  mean_table(ht_in)
# measures of dispersion
library(dplyr)
height_and_weight_20 %>%
  summarise(
    min_height = min(ht_in),
    mean_height = mean(ht_in),
    max_height = max(ht_in),
    range = max_height- min_height) #range
var(c(rep(58, 3), rep(78, 3))) #variance
sd(c(rep(58, 3), rep(78, 3))) #variance
sim_data <- tibble(
  all_68 = rep(68, 20),
  half_58_78 = c(rep(58, 10), rep(78, 10)),
  even_58_78 = seq(from = 58, to = 78, length.out = 20),
  half_48_88 = c(rep(48, 10), rep(88, 10)),
  even_48_88 = seq(from = 48, to = 88, length.out = 20))
sim_data
tibble(
  Column = names(sim_data),
  mean = purrr::map_dbl(sim_data, mean),
  Variance = purrr::map_dbl(sim_data, var),
  SD = purrr::map_dbl(sim_data, sd))
#pearson correlation
library(ggplot2)
set.seed(123)
df<-tibble(
  id =1:20,
  x =sample(x =0:100,size = 20,replace= TRUE),
  y =sample(x =0:100,size = 20,replace= TRUE))
df
ggplot(df, aes(x, y)) +
  geom_point(color = "purple", shape = 17, alpha = 0.5) +
  theme_bw()
ggplot(df, aes(x, y)) +
  geom_point() +
  geom_text(aes(label = id), nudge_x = 1.5, nudge_y = 2) +
  theme_bw()
ggplot(df, aes(x, y)) +
  geom_text(aes(label = id), nudge_x = 1.5, nudge_y = 2, col = "blue") +
  geom_vline(xintercept = 30, col = "red", size = 0.25) +
  geom_hline(yintercept = 71, col = "red", size = 0.25) +
  geom_point(col = "yellow", shape = 17) +
  theme_bw()
ggplot(df, aes(x, y)) +
  geom_point() +
  geom_text(aes(label = id), nudge_x = 1.5, nudge_y = 2) +
  geom_point(aes(x, y), tibble(x = 100, y = 80), shape = 1, size = 16, col = "red") +
  geom_point(aes(x, y), tibble(x = 90, y = 8), shape = 1, size = 16, col = "blue") +
  theme_bw()
cor.test(x = df$x, y = df$y)
# Positively correlated data
tibble(
  x = 1:10,
  y = 100:109,
  r = cor(x, y)
) %>%
  ggplot() +
  geom_point(aes(x, y)) +
  geom_text(aes(x = 2.5, y = 107.5, label = paste("r = ", r)), col = "blue") +
  theme_classic()
df <- tibble(
  x = 1:-8,
  y = 100:91)
df
df%>%
  mutate(r= cor(x,y)) %>%
  ggplot() +
  geom_point(aes(x,y))+
  geom_text(aes(x=-6,y =98,label=paste("r=",r)), col= "blue")+
  theme_classic()
tibble(
  x = 1:10,
  y = 100:91,
  r = cor(x, y)) %>%
  ggplot() +
  geom_point(aes(x, y)) +
  geom_text(aes(x = 7.5, y = 98, label = paste("r = ", r)), col = "blue") +
  theme_classic()
class <- tibble(
  ht_in = c(70, 63, 62, 67, 67, 58, 64, 69, 65, 68, 63, 68, 69, 66, 67, 65,
            64, 75, 67, 63, 60, 67, 64, 73, 62, 69, 67, 62, 68, 66, 66, 62,
            64, 68, NA, 68, 70, 68, 68, 66, 71, 61, 62, 64, 64, 63, 67, 66,69, 76, NA,63,64,65,65,71,66, 65, 65, 71, 64, 71, 60,62,
            61, 69, 66,NA),
  wt_lbs = c(216,106, 145,195,143, 125,138, 140, 158,167, 145, 297,146,
             125,111, 125,130,182, 170,121, 98, 150, 132,250, 137,124,
             186,148, 134,155,122, 142,110, 132, 188,176, 188, 166,136,
             147,178, 125,102,140, 139,60,147, 147, 141,232, 186,212,
             110,110, 115,154,140, 150,130, NA, 171, 156,92,122, 102,
             163,141, NA))
ggplot(class,aes(ht_in,wt_lbs)) +
  geom_jitter() +
  theme_classic()
cor.test(class$ht_in, class$wt_lbs)
options(scipen = 999)
cor.test(class$ht_in, class$wt_lbs)
ggplot(class, aes(ht_in, wt_lbs)) +
  geom_smooth(method = "lm") +
  geom_jitter() +
  theme_classic()
# relationship between a continuous outcome and a categorical predictor
library(dplyr)
library(ggplot2)
class <- tibble(
  age
  = c(32, 30, 32, 29, 24, 38, 25, 24, 48, 29, 22, 29, 24, 28, 24, 25,
      25, 22, 25, 24, 25, 24, 23, 24, 31, 24, 29, 24, 22, 23, 26, 23,
      24, 25, 24, 33, 27, 25, 26, 26, 26, 26, 26, 27, 24, 43, 25, 24,
      27, 28, 29, 24, 26, 28, 25, 24, 26, 24, 26, 31, 24, 26, 31, 34,
      26, 25, 27, NA),
  age_group = c(2, 2, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1,
                1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2,2, 1, 1,1,NA),
  gender =c(2, 1, 1,2,1, 1, 1, 2,2,2, 1, 1, 2,1,1,1, 1, 2, 2,1,1,
            1, 1, 2,1,1, 2, 1, 1,1,2, 1, 1, 2,2,1,2, 2, 1, 2,2,1,
            1, 1, 1,1,1, 1, 1, 2,2,1, 1, 1, 1,2,2,1, 1, 2, 1,2,1,
            1, 1, 2,1,NA),
  ht_in =c(70, 63,62,67,67,58,64,69, 65, 68, 63, 68, 69, 66,67,65,
           64, 75,67,63,60,67,64,73, 62, 69, 67, 62, 68, 66,66,62,
           64, 68,NA,68,70,68,68,66, 71, 61, 62, 64, 64, 63,67,66,
           69, 76,NA,63,64,65,65,71, 66, 65, 65, 71, 64, 71,60,62,
           61, 69,66,NA),
  wt_lbs =c(216,106, 145, 195,143, 125, 138,140, 158,167,145, 297,146,
            125,111, 125, 130,182, 170, 121,98,150, 132,250, 137, 124,
            186,148, 134, 155,122, 142, 110,132, 188,176,188, 166,136,
            147,178, 125, 102,140, 139, 60,147,147, 141,232, 186, 212,
            110,110, 115, 154,140, 150, 130,NA,171, 156,92,122, 102,
            163,141, NA),
  bmi =c(30.99, 18.78, 26.52, 30.54,22.39,26.12,23.69,20.67,26.29,
         25.39, 25.68, 45.15, 21.56,20.17,17.38,20.8, 22.31, 22.75,
         26.62, 21.43, 19.14, 23.49,22.66,32.98,25.05,18.31,29.13,
         27.07, 20.37, 25.01, 19.69,25.97,18.88,20.07,NA,26.76,
         26.97, 25.24, 20.68, 23.72,24.82,23.62,18.65,24.03,23.86,
         10.63, 23.02, 23.72, 20.82,28.24,NA,37.55,18.88,18.3,
         19.13, 21.48, 22.59, 24.96,21.63,NA,29.35,21.76,17.97,
         22.31, 19.27, 24.07, 22.76,NA),
  bmi_3cat =c(3, 1, 2,3,1, 2, 1, 1,2,2, 2, 3, 1,1,1,1, 1, 1, 2,1,1,
              1, 1, 3,2,1, 2, 2, 1,2,1, 2, 1, 1,NA, 2,2,2, 1, 1, 1,1,
              1, 1, 1,1,1, 1, 1, 2,NA,3,1,1, 1, 1, 1,1,1, NA,2, 1,
              1, 1, 1,1,1, NA))%>%
  mutate(
    age_group= factor(age_group,labels= c("Youngerthan30", "30and Older")),
    gender= factor(gender,labels= c("Female", "Male")),
    bmi_3cat= factor(bmi_3cat,labels= c("Normal", "Overweight", "Obese"))
  ) %>%
  print()
#single predictor and single outcome
class_summary<-class %>%
  filter(!is.na(ht_in))%>%
  group_by(gender) %>%
  summarise(
    n = n(),
    mean = mean(ht_in),
    `standard deviation`= sd(ht_in),
    min = min(ht_in),
    max = max(ht_in)
  ) %>%
  print()
class %>%
  filter(!is.na(ht_in)) %>%
  ggplot(aes(x = gender, y = ht_in)) +
  geom_jitter(aes(col = gender), width = 0.20) +
  geom_segment(
    aes(x = c(0.75, 1.75), y = mean, xend = c(1.25, 2.25), yend = mean, col = gender),
    size = 1.5, data = class_summary
  ) +
  scale_x_discrete("Gender") +
  scale_y_continuous("Height (Inches)") +
  scale_color_manual(values = c("#BC581A", "#00519B")) +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(size = 12))
#multiple predictors
class_summary <- class %>%
  filter(!is.na(bmi)) %>%
  group_by(gender, age_group) %>%
  summarise(
    n = n(),
    mean = mean(bmi),
    `standard deviation` = sd(bmi),
    min = min(bmi),
    max = max(bmi)
  ) %>%
  print()
class %>%
  filter(!is.na(bmi)) %>%
  ggplot(aes(x = age_group, y = bmi)) +
  facet_wrap(vars(gender)) +
  geom_jitter(aes(col = age_group), width = 0.20) +
  geom_segment(
    aes(x = rep(c(0.75, 1.75), 2), y = mean, xend = rep(c(1.25, 2.25), 2), yend = mean,
        col = age_group),
    size = 1.5, data = class_summary
  ) +
  scale_x_discrete("Age Group") +
  scale_y_continuous("BMI") +
  scale_color_manual(values = c("#BC581A", "#00519B")) +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(size = 10))
# categorical outcome and a categorical predictor
class <- tibble(
  age = c(32, 30, 32, 29, 24, 38, 25, 24, 48, 29, 22, 29, 24, 28, 24, 25,
          25, 22, 25, 24, 25, 24, 23, 24, 31, 24, 29, 24, 22, 23, 26, 23,
          24, 25, 24, 33, 27, 25, 26, 26, 26, 26, 26, 27, 24, 43, 25, 24,
          27, 28, 29, 24, 26, 28, 25, 24, 26, 24, 26, 31, 24, 26, 31, 34,
          26, 25, 27, NA),
  age_group = c(2, 2, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1,
                1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2,
                2, 1, 1, 1, NA),
  gender = c(2, 1, 1, 2, 1, 1, 1, 2, 2, 2, 1, 1, 2, 1, 1, 1, 1, 2, 2, 1, 1,
             1, 1, 2, 1, 1, 2, 1, 1, 1, 2, 1, 1, 2, 2, 1, 2, 2, 1, 2, 2, 1,
             1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 2, 1, 1, 2, 1, 2, 1,
             1, 1, 2, 1, NA),
  ht_in = c(70, 63, 62, 67, 67, 58, 64, 69, 65, 68, 63, 68, 69, 66, 67, 65,
            64, 75, 67, 63, 60, 67, 64, 73, 62, 69, 67, 62, 68, 66, 66, 62,
            64, 68, NA, 68, 70, 68, 68, 66, 71, 61, 62, 64, 64, 63, 67, 66,
            69, 76, NA, 63, 64, 65, 65, 71, 66, 65, 65, 71, 64, 71, 60, 62,
            61, 69, 66, NA),
  wt_lbs = c(216, 106, 145, 195, 143, 125, 138, 140, 158, 167, 145, 297, 146,
             125, 111, 125, 130, 182, 170, 121, 98, 150, 132, 250, 137, 124,
             186, 148, 134, 155, 122, 142, 110, 132, 188, 176, 188, 166, 136,
             147, 178, 125, 102, 140, 139, 60, 147, 147, 141, 232, 186, 212,
             110, 110, 115, 154, 140, 150, 130, NA, 171, 156, 92, 122, 102,
             163, 141, NA),
  bmi = c(30.99, 18.78, 26.52, 30.54, 22.39, 26.12, 23.69, 20.67, 26.29,
          25.39, 25.68, 45.15, 21.56, 20.17, 17.38, 20.8, 22.31, 22.75,
          26.62, 21.43, 19.14, 23.49, 22.66, 32.98, 25.05, 18.31, 29.13,
          27.07, 20.37, 25.01, 19.69, 25.97, 18.88, 20.07, NA, 26.76,
          26.97, 25.24, 20.68, 23.72, 24.82, 23.62, 18.65, 24.03, 23.86,
          10.63, 23.02, 23.72, 20.82, 28.24, NA, 37.55, 18.88, 18.3,
          19.13, 21.48, 22.59, 24.96, 21.63, NA, 29.35, 21.76, 17.97,
          22.31, 19.27, 24.07, 22.76, NA),
  bmi_3cat = c(3, 1, 2, 3, 1, 2, 1, 1, 2, 2, 2, 3, 1, 1, 1, 1, 1, 1, 2, 1, 1,
               1, 1, 3, 2, 1, 2, 2, 1, 2, 1, 2, 1, 1, NA, 2, 2, 2, 1, 1, 1, 1,
               1, 1, 1, 1, 1, 1, 1, 2, NA, 3, 1, 1, 1, 1, 1, 1, 1, NA, 2, 1,
               1, 1, 1, 1, 1, NA),
  genhlth = c(2, 2, 3, 3, 2, 1, 2, 2, 2, 1, 3, 3, 1, 2, 2, 1, 2, NA, 3, 2, 3,
              1, 2, 2, 2, 4, 2, 2, 2, 2, 1, 2, 2, 1, 2, 2, 3, 3, 2, 1, 3, 3,
              2, 2, 3, 3, 2, 3, 2, 2, 3, 5, 3, 2, 3, 2, 3, 3, 2, 2, 3, 3, 3,
              1, 2, 2, 1, 3),
  persdoc = c(1, 2, 2, 1, 2, 0, 0, 1, 2, 0, 2, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0,
              0, 1, 1, 1, 1, 2, 0, 0, 1, 1, 2, 1, 2, 0, 0, 2, 0, 0, 2, 2, 0,
              NA, 0, 0, 0, 2, 0, 2, NA, 0, 2, 1, 1, 1, 2, 2, 0, 0, 0, 1, 2,
              1, 1, 0, 0, 0, NA)
) %>%
  mutate(
    age_group = factor(age_group, labels = c("Younger than 30", "30 and Older")),
    gender = factor(gender, labels = c("Female", "Male")),
    bmi_3cat = factor(bmi_3cat, labels = c("Normal", "Overweight", "Obese")),
    genhlth = factor(genhlth, labels = c("Excellent", "Very Good", "Good", "Fair", "Poor")),
    persdoc = factor(persdoc, labels = c("No", "Yes, only one", "Yes, more than one"))
  ) %>%
  print()
# comparing two variables
df<-filter(class,!is.na(bmi_3cat))#Droprows withmissingbmi
gmodels::CrossTable(df$persdoc,df$genhlth)
# DATA MANAGEMENT
df <- tibble(
  id = c(1, 2, 3),
  x = c(0, 1, 0)
)
df %>%
  filter(x == 0)
l <- list(
  id = c(1, 2, 3),
  x = c(0, 1, 0)
)
l %>%
  filter(x == 0)  
#MUTATE FUNCTION
library(dplyr)
set.seed(123)
drug_trial<-tibble(
  #Studyid,there are20peopleenrolledinthetrial.
  id =rep(1:20,each= 3),
  #Follow-upyear, 0 =baseline,1=yearone,2=yeartwo.
  year = rep(0:2,times= 20),
  #Participant age a baseline.Mustbebetweentheagesof35and75at
  #baselineto be eligibleforthestudy
  age= sample(35:75, 20, TRUE)%>%rep(each= 3),
  #Drug theparticipantreceived,Placebooractive
  drug = sample(c("Placebo","Active"),20,TRUE)%>%
    rep(each= 3),
  #Reportedheadachessideeffect,Y/N
  se_headache = if_else(
    drug == "Placebo",
    sample(0:1, 60, TRUE, c(.95,.05)),
    sample(0:1, 60, TRUE, c(.10,.90))
  ),
  #Report diarrhea sideeffect,Y/N
  se_diarrhea = if_else(
    drug == "Placebo",
    sample(0:1, 60, TRUE, c(.98,.02)),
    sample(0:1, 60, TRUE, c(.20,.80))
  ),
  #Report dry mouthsideeffect,Y/N
  se_dry_mouth =if_else(
    drug == "Placebo",
    sample(0:1, 60, TRUE, c(.97,.03)),
    sample(0:1, 60, TRUE, c(.30,.70))
  ),
  #Participant had myocardial infarction instudyyear,Y/N
  mi =if_else(
    drug == "Placebo",
    sample(0:1, 60, TRUE, c(.85,.15)),
    sample(0:1, 60, TRUE, c(.80,.20))
  )
)
# adding or modifying a single column
drug_trial%>%
  mutate(complete =c(
    0,0,0,0, 0, 0, 0,0,0, 0, 0, 0,0,0, 0, 0, 0,0,0,0, 0, 0, 0,0,0,
    0,0,0,0, 0, 0, 0,0,0, 0, 0, 0,0,0, 0, 0, 0,0,0,0, 0, 0, 0,0,0,
    0,0,0,0, 0, 0, 0,0,0, 0)
  )
drug_trial%>%
  mutate(complete =0)
drug_trial_df<-as.data.frame(drug_trial)
class(drug_trial_df)
drug_trial_df$complete <-c(0,1)
drug_trial_df
# recycling
drug_trial%>%
  mutate(complete =rep(0, 60))
#using existing variables in name-value pairs
drug_trial%>%
  mutate(mi_f = factor(mi,c(0,1), c("No","Yes")))
drug_trial%>%
  mutate(age_center= age-mean(age))
#adding or modifying multiple columns
drug_trial%>%
  mutate(
    se_headache_f = factor(se_headache, c(0, 1), c("No","Yes")),
    se_diarrhea_f = factor(se_diarrhea, c(0, 1), c("N0","Yes")),
    se_dry_mouth_f= factor(se_dry_mouth,c(0,1),c("No", "Yes"))
  )
#Create avector of 0/1levelsthatcanbereusedbelow.
yn_levs<-c(0, 1)
#Create avector of "No"/"Yes"labelsthatcanbereusedbelow.
yn_labs<-c("No", "Yes")
drug_trial%>%
  mutate(
    se_headache_f = factor(se_headache,yn_levs,yn_labs),
    se_diarrhea_f = factor(se_diarrhea,yn_levs,yn_labs),
    se_dry_mouth_f= factor(se_dry_mouth,yn_levs,yn_labs)
  )
# ROWWISE MUTATIONS
drug_trial%>%
  rowwise() %>%
  mutate(any_se_year = sum(se_headache,se_diarrhea,se_dry_mouth)>0)
drug_trial_sub<-drug_trial%>%
  select(id,year, starts_with("se")) %>%
  print()
drug_trial_sub%>%
  mutate(any_se_year = sum(se_headache,se_diarrhea,se_dry_mouth))
drug_trial_sub%>%
  rowwise() %>%
  mutate(any_se_year = sum(se_headache,se_diarrhea,se_dry_mouth))
drug_trial_sub%>%
  rowwise() %>%
  mutate(
    n_se_year = sum(se_headache,se_diarrhea,se_dry_mouth),
    any_se_year= n_se_year > 0
  )
drug_trial_sub%>%
  rowwise() %>%
  mutate(any_se_year = sum(se_headache,se_diarrhea,se_dry_mouth)>0)
drug_trial_sub%>%
  rowwise() %>%
  mutate(
    any_se_year= sum(se_headache,se_diarrhea,se_dry_mouth)> 0,
    all_se_year= sum(se_headache,se_diarrhea,se_dry_mouth)== 3
  )
#GROUP_BY MUTATIONS
drug_trial_sub %>%
  rowwise() %>%
  mutate(
    n_se_year = sum(se_headache, se_diarrhea, se_dry_mouth),
    any_se_year = n_se_year > 0,
    all_se_year = n_se_year == 3) %>%
  group_by(id) %>%
  mutate(any_se=sum(any_se_year) > 0)
library(dplyr)
library(ggplot2)
drug_trial_sub%>%
  rowwise() %>%
  mutate(
    n_se_year = sum(se_headache,se_diarrhea,se_dry_mouth),
    any_se_year= n_se_year > 0,
    all_se_year= n_se_year == 3
  ) %>%
  mutate(any_se=sum(any_se_year) > 0)
any_se_year<-c(TRUE, TRUE,TRUE)
any_se_year
sum_any_se_year <- sum(any_se_year)
sum_any_se_year
any_se <- sum_any_se_year > 0
any_se
#subsetting data frames
set.seed(123)
drug_trial <- tibble(
  # Follow-up year, 0 = baseline, 1 = year one, 2 = year two.
  year = rep(0:2, times = 20),
  # Participant age a baseline. Must be between the ages of 35 and 75 at
  # baseline to be eligible for the study
  age = sample(35:75, 20, TRUE) %>% rep(each = 3),
  # Drug the participant received, Placebo or active
  drug = sample(c("Placebo", "Active"), 20, TRUE) %>%
    rep(each = 3),
  # Reported headaches side effect, Y/N
  se_headache = if_else(
    drug == "Placebo",
    sample(0:1, 60, TRUE, c(.95,.05)),
    sample(0:1, 60, TRUE, c(.10, .90))),
  # Report diarrhea side effect, Y/N
  se_diarrhea = if_else(
    drug == "Placebo",
    sample(0:1, 60, TRUE, c(.98,.02)),
    sample(0:1, 60, TRUE, c(.20, .80))
  ),
  # Report dry mouth side effect, Y/N
  se_dry_mouth = if_else(
    drug == "Placebo",
    sample(0:1, 60, TRUE, c(.97,.03)),
    sample(0:1, 60, TRUE, c(.30, .70))
  ),
  # Participant had myocardial infarction in study year, Y/N
  mi = if_else(
    drug == "Placebo",
    sample(0:1, 60, TRUE, c(.85, .15)),
    sample(0:1, 60, TRUE, c(.80, .20))
  )
)
drug_trial<-drug_trial%>%
  mutate(
    #Studyid,thereare20peopleenrolledinthetrial.
    id= rep(1:20, each= 3)
  ) %>%
  print()
  #select function
drug_trial%>%
  select(id,year, age,se_headache,se_diarrhea,se_dry_mouth,mi)
?dplyr_tidy_select
drug_trial<-drug_trial%>%
  select(id,everything())%>%
  print()
summarise(drug_trial)
mean(drug_trial, age)
drug_trial<-drug_trial%>%
  mutate(age_center= age-mean(age)) %>%
  print()
drug_trial <- mean(age)
drug_trial
list(drug_trial)
data.frame(drug_trial)
drug_trial%>%
  select(age,age_center)
drug_trial%>%
  select(id,year, starts_with("se"))
drug_trial%>%
  select(id,year, se_headache:se_dry_mouth)
#Addthe sideeffectfactorcolumnstoourdataframeagain...
yn_levs<-c(0, 1)
yn_labs<-c("No", "Yes")
drug_trial %>%
  mean(age)
?mean
Mean <- mean(age)
drug_trial <- drug_trial %>%
  mutate(
    se_headache_f = factor(se_headache, yn_levs, yn_labs),
    se_diarrhea_f = factor(se_diarrhea, yn_levs, yn_labs),
    se_dry_mouth_f = factor(se_dry_mouth, yn_levs, yn_labs)
  )
M<- drug_trial %>%
  summarise(mean = mean(age))
glimpse(drug_trial)
drug_trial %>%
  select(id, year, ends_with("_f"))
drug_trial %>%
  select(1:2, 4)
drug_trial_sub <- drug_trial %>%
  rowwise() %>%
  mutate(
    n_se_year = sum(se_headache, se_diarrhea, se_dry_mouth),
    any_se_year = n_se_year > 0,
    all_se_year = n_se_year == 3
  ) %>%
  group_by(id) %>%
  mutate(any_se = sum(any_se_year) > 0) %>%
  ungroup() %>%
  select(id:year,n_se_year:any_se)%>%
  print()
drug_trial_sub%>%
  select(-n_se_year)
drug_trial_sub%>%
  select(!n_se_year)
drug_trial_sub%>%
  select(id:year,any_se_year:any_se)
#The rename function
nhanes <- tibble(
  SEQN = c(1:4),
  ALQ101 = c(1, 2, 1, 2),
  ALQ110 = c(2, 2, 2, 1)
) %>%
  print()
nhanes %>%
  rename(
    id = SEQN,
    drinks_12_year = ALQ101,
    drinks_12_life = ALQ110
  )
nhanes %>%
  rename_with(tolower)
#filter function
drug_trial %>%
  select(1:2, 4)
drug_trial %>%
  slice(1:5) # number of rows

