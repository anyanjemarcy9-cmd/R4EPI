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
#subgroup analysis
library(dplyr)
drug_trial%>%
  filter(year== 0)
drug_trial %>%
  filter(year == 0) %>%
  group_by(drug, se_headache_f) %>%
  summarise(n = n())
drug_trial %>%
  filter(year == 0) %>%
  filter(age < 65) %>%
  group_by(drug, se_headache_f) %>%
  summarise(n = n())
drug_trial %>%
  filter(year == 0) %>%
  filter(age < 65) %>%
  group_by(drug, se_headache_f, .drop = FALSE) %>%
  summarise(n = n())
drug_trial %>%
  filter(year == 0 & age < 65) %>%
  group_by(drug, se_headache_f, .drop = FALSE) %>%
  summarise(n = n())
#complete case analysis
drug_trial_short<-drug_trial%>%
  filter(year== 0) %>%
  slice(1:10)%>%
  mutate(
    age = replace(age, 1, NA),
    drug= replace(drug, 4,NA)
  ) %>%
  print()
drug_trial_short %>%
  group_by(drug) %>%
  summarise(mean_age = mean(age))
drug_trial_short %>%
  group_by(drug) %>%
  summarise(mean_age = mean(age, na.rm = TRUE))
drug_trial_short %>%
  filter(!is.na(age)) %>%
  group_by(drug) %>%
  summarise(mean_age = mean(age))
drug_trial_short %>%
  filter(!is.na(age) & !is.na(drug)) %>%
  group_by(drug) %>%
  summarise(mean_age = mean(age))
#Deduplication
df <- tribble(
  ~id, ~day, ~x,
  1, 1, 1,
  1, 2, 11,2,1,12,
  2,2,13,
  2,2,14,
  3,1,12,
  3,1,12,
  3,2,13,
  4,1,13,
  5,1,10,
  5,2,11,
  5,1,10
)%>%
  print()
#distinct function - remove all complete duplicates
df %>%
  distinct()
# complete duplicate row add tag
df%>%
  mutate(dup = duplicated(df))
df%>%
  group_by_all() %>%
  mutate(
    n_row= row_number(),
    dup = n_row> 1
  )
df%>%
  mutate(dup = duplicated(.)| duplicated(.,fromLast= TRUE))
#partial duplicate rows
df %>%
  distinct(id, day, .keep_all = TRUE)
# partial duplicate rows - add tag
df%>%
  group_by(id,day) %>%
  mutate(
    count= row_number(),#Countsrowsbygroup
    dup = count> 1 #TRUEifthereismorethan onerowpergroup
  )
# count the number of duplicates
df%>%
  group_by(id,day) %>%
  filter(n() > 1)%>%
  count()

#WORKING WITH DATES
#Date vector types
library(readr)
birth_dates <- read_csv("C:/Users/Immah/Downloads/birth_dates.csv")
birth_dates
# dates under the hood
as.Date("2000-01-01")
unclass(as.Date("2000-01-01"))
unclass(as.Date("1970-01-01"))
unclass(as.Date("1970-01-02"))
unclass(as.Date("1969-12-31"))
as.Date(10958, origin = "1970-01-01")
from_sas<-tibble(
  date = c(10958,10959,10960)
)
library(dplyr)
from_sas%>%
  mutate(new_date =as.Date(date, origin= "1960-01-01"))
#coercing date-times to dates
birth_dates%>%
  mutate(posix_to_date=as.Date(dob_actual)) %>%
  select(dob_actual,posix_to_date)
#coercing character strings to dates
tribble(
  ~Symbol, ~Description,~Example,
  "%a", "Abbreviated weekday name","Sat",
  "%A", "Fullweekdayname","Saturday",
  "%b", "Abbreviated month name", "Jan",
  "%B", "Full month name", "January",
  "%d", "Day of the month as a number(01–31)", "15",
  "%m", "Monthasa number","01",
  "%u", "Weekdayas a number(1–7,Monday is 1)","6",
  "%U", "Weekofthe year asa number(00–53)using Sunday as the first day 1 of the week",
  "%y", "Year without century (00-99)","00",
  "%Y", "Year with century","2000"
)%>%
  knitr::kable()
birth_dates
birth_dates%>%
  mutate(dob_typical_to_date=as.Date(dob_typical))%>%
  select(dob_typical,dob_typical_to_date)
birth_dates %>%
  mutate(dob_typical_to_date = as.Date(dob_typical, format = "%m %d %Y")) %>%
  select(dob_typical, dob_typical_to_date)
birth_dates%>%
  mutate(dob_typical_to_date=as.Date(dob_typical, format= "%m/%d/%Y")) %>%
  select(dob_typical,dob_typical_to_date)
select(birth_dates, dob_long)
birth_dates%>%
  mutate(dob_long_to_date=as.Date(dob_long,format="%B%d,%Y")) %>%
  select(dob_long,dob_long_to_date)
#change the appearance of dates with format()
birth_dates%>%
  mutate(dob_abbreviated= format(dob_actual, "%d%b%y")) %>%
  select(dob_actual,dob_abbreviated)
#some useful built-in dates
Sys.Date() #today's date
lubridate::today()
Sys.time() #today's date and time
lubridate::now()
set.seed(703)
rand_mill <- rnorm(1000000)
start <- lubridate::now()
sum <- sum(rand_mill)
length <- length(rand_mill)
mean <- sum / length
mean
stop <- lubridate::now()
stop- start
rm(mean)
start <- lubridate::now()
mean(rand_mill)
stop <- lubridate::now()
stop- start
library(dplyr)
#character vector of full month names
month.name
#abb month name
month.abb
#sequence of dates
seq.Date(
  from = as.Date("2020-01-01"),
  to = as.Date("2020-01-15"),
  by = "days")
#calculating date intervals
ages <- birth_dates %>%
  select(name_first, dob = dob_default) %>%
  print()
birth_dates
ages %>%
  mutate(today = Sys.Date())
ages <- ages %>%
  mutate(today = as.Date("2020-05-07")) %>%
  print()
#calculate age difference
library(lubridate)
ages %>%
  mutate(
    age_subtraction = today- dob,
    age_difftime
    = difftime(today, dob),
    age_lubridate = dob %--% today # lubridate's %--% operator creates a time interval
  )
ages %>%
  mutate(
    age_subtraction = as.numeric(today- dob) / 365.25,
    age_difftime
    = as.numeric(difftime(today, dob)) / 365.25,
    age_lubridate = (dob %--% today) / years(1)
  )
years(1)
start <- as.Date("2017-03-01")
end <- as.Date("2018-03-01")
as.numeric(difftime(end, start)) / 365.25
(start %--% end) / years(1)
start <- as.Date("2019-03-01")
end <- as.Date("2020-03-01")
as.numeric(difftime(end, start)) / 36
(start %--% end) / years(1)
start <- as.Date("2019-03-01")
end <- as.Date("2020-03-01")
as.numeric(difftime(end, start)) / 365.25
(start %--% end) / years(1)
#rounding time intervals
ages %>%
  mutate(
    age_years = (dob %--% today) / years(1),
    # If you want the age (in years) as of the person's last birthday
    age_last = trunc(age_years),
    # If you want to round the age to the nearest year
    age_near = round(age_years)
  )
ages %>%
  mutate(
    # If you want the age (in years) as of the person's last birthday
    age_years = (dob %--% today) %/% years(1)
  )
#extracting date parts
ages <- ages %>%
  select(-today) %>%
  print()
ages%>%
  mutate(
    day = day(dob),
    month= month(dob),
    year = year(dob)
  )
ages%>%
  mutate(
    wday = wday(dob),
    day_full = wday(dob,label= TRUE,abbr= FALSE),
    day_abb = wday(dob,label= TRUE,abbr= TRUE),
    week_of_year= week(dob),
    week_cdc = epiweek(dob)
  )
#sorting dates
ages %>%
  arrange(dob)
ages %>%
  arrange(desc(dob))


#29. WORKING WITH CHARACTER STRINGS
library(readr)
library(dplyr)
library(stringr) # All stringr functions begin with "str_"
ehr
ehr <- read_rds("ehr.Rds")
ehr <- read_rds("C:/Users/Immah/Downloads/ehr.Rds")
ehr
ehr %>%
  group_by(name) %>%
  mutate(dup = row_number() > 1) %>%
  arrange(name) %>%
  select(name, dup, dob, address, city)
ehr %>%
  arrange(name) %>%
  pull(name)
#coerce to lower
ehr %>%
  arrange(name) %>%
  pull(name) %>%
  str_to_lower()
#upper case
ehr %>%
  arrange(name) %>%
  pull(name) %>%
  str_to_upper()
#title case
ehr %>%
  arrange(name) %>%
  pull(name) %>%
  str_to_title()
#sentence case
ehr %>%
  arrange(name) %>%
  pull(name) %>%
  str_to_sentence()
ehr <- ehr %>%
  mutate(name = str_to_lower(name)) %>%
  print()
ehr %>%
  group_by(name) %>%
  mutate(dup = row_number() > 1) %>%
  arrange(name) %>%
  select(name, dup, dob, address, city)
#trim white space
str_trim("Ryan Edwards ")
ehr <- ehr %>%
  mutate(name = str_trim(name))
ehr %>%
  group_by(name) %>%
  mutate(dup = row_number() > 1) %>%
  arrange(name) %>%
  select(name, dup, dob, address, city)
#regular expressions
#remove comma
str_replace(
  string = "weston fox,",
  pattern = ",",
  replacement = ""
)
ehr <- ehr %>%
  mutate(name = str_replace(name, ",", ""))
ehr %>%
  group_by(name) %>%
  mutate(dup = row_number() > 1) %>%
  arrange(name) %>%
  select(name, dup, dob, address, city)
#remove middle initial
str_replace(
  string = "tatum s chavez",
  pattern = " \\w ",
  replacement = " ")
ehr <- ehr %>%
  mutate(name = str_replace(name, " \\w ", " "))
ehr %>%
  group_by(name) %>%
  mutate(dup = row_number() > 1) %>%
  arrange(name) %>%
  select(name, dup, dob, address, city)
#remove double spaces
str_replace(
  string = "Ivy Mccann",
  pattern = "\\s{2,}",
  replacement = " ")
ehr <- ehr %>%
  mutate(name = str_replace(name, "\\s{2,}", " "))
ehr %>%
  group_by(name) %>%
  mutate(dup = row_number() > 1) %>%
  arrange(name) %>%
  select(name, dup, dob, address, city)
ehr %>%
  group_by(name) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  summarise(`Unique People` = n())
ehr_unique <- ehr %>%
  group_by(name) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  print()
ehr %>%
  group_by(city) %>%
  summarise(n = n())
ehr <- ehr %>%
  mutate(
    address = tolower(address),
    city = tolower(city))
ehr%>%
  group_by(city) %>%
  summarise(n= n())
str_replace(
  string = "cityoffortworth",
  pattern = "cityof",
  replacement= ""
)
ehr<-ehr %>%
  mutate(city = str_replace(city, "cityof", ""))
ehr%>%
  group_by(city) %>%
  summarise(n= n())
#separate values into component parts
str_extract("zariah hernandez", "^\\w+")
str_extract("zariah hernandez", "\\w+$")
ehr <- ehr %>%
  mutate(
    # Separate name into first name and last name
    name_first = str_extract(name, "^\\w+"),
    name_last = str_extract(name, "\\w+$")
  )
ehr %>%
  select(name, name_first, name_last)
#Dummy Variables
ehr %>%
  select(name_first, name_last, symptoms)
ehr%>%
  group_by(symptoms)%>%
  summarise(n= n())
ehr<-ehr %>%
  mutate(
    pain = str_detect(symptoms,"Pain"),
    headache= str_detect(symptoms,"Headache"),
    nausea = str_detect(symptoms,"Nausea")
  )
ehr%>%
  select(symptoms,pain,headache,nausea)
table(ehr$headache)
ehr %>%
  select(pain) %>%
  mutate(pain_01 = as.numeric(pain))


#30 CONDITIONAL OPERATIONS
library(dplyr)
rainy_days <- tibble(
  day = 1:5,
  weather = c("rain", "rain", "no rain", "rain", "no rain")
) %>%
  print()
rainy_days%>%
  mutate(
    raincoat= if_else(
      condition =weather =="rain",
      true ="wear",
      false ="nowear"
    )
  )
if_else(TRUE, "wear", "no wear")
rainy_days %>%
  mutate(
    raincoat = if_else(TRUE, "wear", "no wear")
  )
rainy_days%>%
  mutate(
    raincoat= if_else(FALSE, "wear", "nowear")
  )
rainy_days%>%
  mutate(
    raincoat= if_else(c(TRUE, TRUE, FALSE,TRUE,FALSE), "wear","nowear")
  )
rainy_days$weather== "rain"
#Operands and operators (==, !, >=, %in%, %%, &, |)
df <- tibble(
  id = c(1, 1, 2, 2),
  outcome = c(0, 1, 1, 1)
) %>%
  print()
df %>%
  mutate(
    # Odd rows are treatment A
    # Even rows are treatment B
    treatment = if_else(row_number() %% 2 == 1, "A", "B")
  )
df <- tibble(
  name1 = c("Jon", "John", NA),
  name2 = c("Jon", "Jon", "Jon")
)
df %>%
  mutate(
    name_match = name1 == name2
  )
df %>%
  mutate(
    name_match = name1 == name2,
    name_match = if_else(is.na(name_match), FALSE, name_match)
  )
#Testing multiple conditions simultaneously
blood_pressure <- tibble(
  id = 1:10,
  sysbp = c(152, 120, 119, 123, 135, 83, 191, 147, 209, 166),
  diasbp = c(78, 60, 88, 76, 85, 54, 116, 95, 100, 106)
) %>%
  print()
blood_pressure %>%
  mutate(bp = if_else(sysbp < 120 & diasbp < 80, "Normal", "Not Normal"))
#Testing a sequence of conditions
blood_pressure %>%
  mutate(
    bp = case_when(
      sysbp < 120 & diasbp < 80 ~ "Normal",
      sysbp >= 120 & sysbp < 130 & diasbp < 80 ~"Elevated",
      sysbp >= 130 & sysbp < 140 | diasbp >= 80 & diasbp < 90 ~ "Hypertension Stage 1",
      sysbp >= 140 | diasbp >= 90 ~ "Hypertension Stage 2"
    )
  )
#Recoding variables
set.seed(123)
ages <- tibble(
  id = 1:10,
  age = c(sample(1:30, 9, TRUE), NA)
) %>%
  print()
ages%>%
  mutate(
    age_3cat= case_when(
      age< 12 ~1
    )
  )
ages %>%
  mutate(
    age_3cat = case_when(
      age < 12 ~ 1,
      age >= 12 & age < 18 ~ 2))
ages %>%
  mutate(
    age_3cat = case_when(
      age < 12 ~ 1,
      age >= 12 & < 18 ~ 2
    )
  )
ages %>%
  mutate(
    age_3cat = case_when(
      age < 12 ~ 1,
      age >= 12 & age < 18 ~ 2,
      age >= 18 ~ 3
    )
  )
#case when is lazy
df <- tibble(
  number = c(1, 2, 3)
) %>%
  print()
df %>%
  mutate(
    size = case_when(
      number < 2 ~ "Small",
      number < 3 ~ "Medium",
      number < 4 ~ "Large"
    )
  )
ages%>%
  mutate(
    age_3cat= case_when(
      age< 12 ~1,
      age< 18 ~2,
      age>= 18 ~3
    )
  )
#Recode missing
demographics <- ages %>%
  mutate(
    race
    = c(1, 2, 1, 4, 7, 1, 2, 9, 1, 3),
    hispanic = c(7, 0, 1, 0, 1, 0, 1, 9, 0, 1)
  ) %>%
  print()
demographics%>%
  mutate(
    #Recode 7and9 tomissing
    race_recode= if_else(race == 7| race== 9, NA,race),
    hispanic_recode= if_else(hispanic == 7| hispanic== 9,NA,hispanic)
  )
demographics%>%
  mutate(
    #Recode 7and9 tomissing
    race_recode= if_else(race == 7| race== 9, NA_real_,race),
    hispanic_recode= if_else(hispanic == 7| hispanic== 9,NA_real_,hispanic)
  )
demographics%>%
  mutate(
    #Recode 7and9 tomissing
    race_recode = if_else(race == 7| race== 9, NA_real_,race),
    hispanic_recode= if_else(hispanic == 7| hispanic== 9,NA_real_,hispanic),
    race_eth_4cat = case_when(
      # White, non-Hispanic
      race_recode== 1 &hispanic_recode ==0 ~ 1,
      # Black, non-Hispanic
      race_recode== 2 &hispanic_recode ==0 ~ 2,
      # AmericanIndianorAlaskanNativetoOtherrace,non-Hispanic
      race_recode== 3 &hispanic_recode ==0 ~ 4,
      # AsiantoOtherrace,non-Hispanic
      race_recode== 4 &hispanic_recode ==0 ~ 4,
      # Pacific IslandertoOtherrace,non-Hispanic
      race_recode== 4 &hispanic_recode ==0 ~ 4,
      # Hispanic,any race
      hispanic_recode ==1 ~ 3))
demographics%>%
  mutate(
    race_eth_4cat= case_when(
      is.na(hispanic)|hispanic%in% c(7,9) ~ NA_real_,#Unknownethnicity
      hispanic ==1 ~ 3, #Hispanic,anyrace
      is.na(race)| race%in% c(7, 9) ~ NA_real_,#non-Hispanic,unknownrace
      race ==1 ~ 1, #White,non-Hispanic
      race ==2 ~ 2, #Black,non-Hispanic
      TRUE ~ 4 #Otherrace,non-Hispanic
      ))
demographics %>%
  # Recode variables
  mutate(
    # Collapse continuous age into 3 categories
    age_3cat = case_when(
      age < 12 ~ 1, # child
      age < 18 ~ 2, # adolescent
      age >= 18 ~ 3 # adult
    ),
    age_3cat_f = factor(
      age_3cat,
      labels = c("child", "adolescent", "adult")
    ),
    #Combine raceandethnicity
    race_eth_4cat= case_when(
      is.na(hispanic)|hispanic%in% c(7,9) ~ NA_real_,#Unknownethnicity
      hispanic ==1 ~ 3, #Hispanic,anyrace
      is.na(race)| race%in% c(7, 9) ~ NA_real_,#non-Hispanic,unknownrace
      race ==1 ~ 1, #White,non-Hispanic
      race ==2 ~ 2, #Black,non-Hispanic
      TRUE ~ 4 #Otherrace,non-Hispanic
    ),
    race_eth_4cat_f= factor(
      race_eth_4cat,
      labels= c(
        "White,non-Hispanic", "Black,non-Hispanic","Hispanic,anyrace","Otherrace,non-Hispanic"
      )))


#WORKING WITH MULTIPLE DATA FRAMES
#Combining data frames vertically
trial <- tibble(
  year
  = c(2016, 2017, 2018, 2019),
  n
  = c(501, 499, 498, 502),
  outcome = c(51, 52, 49, 50)
) %>%
  print()
trial_2020<-tibble(
  year = 2020,
  n = 500,
  outcome= 48
)%>%
  print()
trial%>%
  bind_rows(trial_2020)
#combining more than 2 data frames
trial_2021 <- tibble(
  year = 2021,
  n = 598,
  outcome = 57
) %>%
  print()
library(dplyr)
trial %>%
  bind_rows(trial_2020, trial_2021)
#adding rows with different columns
trial_2020 <- tibble(
  year
  = 2020,
  n
  = 500,
  outcome = 48,
  adv_event = 3 # Here is the new column
) %>%
  print()
trial %>%
  bind_rows(trial_2020)
#differing column positions
trial_2020 <- tibble(
  year
  = 2020,
  n
  = 500,
  adv_event = 3, # This was previously the fourth column
  outcome = 48 # This is the thrid column in trial
) %>%
  print()
trial %>%
  bind_rows(trial_2020)
#differing column names
trial_2020 <- tibble(
  year
  = 2020,
  count
  = 500,
  adv_event = 3,
  outcomes = 48
) %>%
  print()
trial %>%
  bind_rows(trial_2020)
trial_2020_rename <- trial_2020 %>%
  rename(
    n = count,
    outcome = outcomes
  )
trial %>%
  bind_rows(trial_2020_rename)
trial %>%
  bind_rows(
    trial_2020 %>%rename(
      n = count,
      outcome = outcomes
    )
  )
#adding columns
#1. by position
df1 <- tibble(
  color = c("red", "green", "blue"),
  size = c("small", "medium", "large")
) %>%
  print()
df2 <- tibble(
  amount = c(1, 4, 3),
  dose
  = c(10, 20, 30)
) %>%
  print()
df1 %>%
  bind_cols(df2)
#2. by key values
demographics <- tibble(
  id
  = c("1001", "1002", "1003", "1004"),
  dob
  = as.Date(c("1968-12-14", "1952-08-03", "1949-05-27", "1955-03-12")),
  race_eth = c(1, 2, 2, 4)
) %>%
  print()
grip_strength <- tibble(
  id
  = c("1002", "1001", "1003", "1004"),
  grip_r = c(32, 28, 32, 22),
  grip_l = c(30, 30, 28, 22)
) %>%
  print()
demographics %>%
  bind_cols(grip_strength)
demographics%>%
  left_join(grip_strength,by= "id")
demographics%>%
  right_join(grip_strength,by="id")
demographics%>%
  full_join(grip_strength,by= "id")
demographics%>%
  inner_join(grip_strength,by="id")
grip_strength%>%
  left_join(demographics,by= "id")
#differing rows
demographics<-tibble(
  id =c("1001", "1002", "1003","1004","1005"),
  dob =as.Date(c(
    "1968-12-14","1952-08-03","1949-05-27", "1955-03-12", "1942-06-07"
  )),
  race_eth =c(1,2,2,4, 3)
)%>%
  print()
demographics%>%
  left_join(grip_strength,by= "id")
demographics%>%
  right_join(grip_strength,by="id")
demographics%>%
  full_join(grip_strength,by= "id")
demographics%>%
  inner_join(grip_strength,by="id")
grip_strength%>%
  left_join(demographics,by= "id")
#differing key column names
grip_strength <- tibble(
  pid
  = c("1002", "1001", "1003", "1004"),
  grip_r = c(32, 28, 32, 22),
  grip_l = c(30, 30, 28, 22)
) %>%
  print()
demographics %>%
  left_join(grip_strength, by = "id")
demographics %>%
  left_join(grip_strength, by = c("id" = "pid"))
#one to many relationship merge
demographics
grip_strength<-tibble(
  id = rep(c("1001","1002","1003", "1004"), each= 2),
  visit = rep(c("pre","post"),4),
  grip_r = c(32,33,28,27,32,34, 22, 27),
  grip_l = c(30,32,30,30,28,30, 22, 26)
)%>%
  print()
demographics%>%
  left_join(grip_strength,by= "id")
#multiple key columns
emr<-tibble(
  id = rep(c("1001","1002","1003", "1004"), each= 2),
  visit = rep(c("pre","post"),4),
  weight = c(105,99, 200, 201,136, 133, 170,175)
)%>%
  print()
demographics%>%
  left_join(grip_strength,emr, by= "id")
demographics%>%
  left_join(grip_strength,by= "id") %>%
  left_join(emr,by= "id")
demographics%>%
  left_join(grip_strength,by= "id") %>%
  left_join(emr,by= c("id", "visit"))


#32 RESTRUCTURING DATA FRAMES
library(tidyr)
library(ggplot2)
#pivoting longer
babies <- tibble(
  id
  = 1001:1008,
  sex
  = c("F", "F", "M", "F", "M", "M", "M", "F"),
  weight_3 = c(9, 11, 17, 16, 11, 17, 16, 15),
  weight_6 = c(13, 16, 20, 18, 15, 21, 17, 16),
  weight_9 = c(16, 17, 23, 21, 16, 25, 19, 18),
  weight_12 = c(17, 20, 24, 22, 18, 26, 21, 19)
) %>%
  print()
babies_long<-babies %>%
  pivot_longer(
    cols = starts_with("weight"),
    names_to = "months",
    names_prefix= "weight_",
    values_to = "weight"
  ) %>%
  print()
#names_to argument
babies %>%
  pivot_longer(
    cols = starts_with("weight")
  )
babies %>%
  pivot_longer(
    cols
    = starts_with("weight"),
    names_to = "months")
#names_prefix argument
babies%>%
  pivot_longer(
    cols = starts_with("weight"),
    names_to = "months",
    names_prefix= "weight_"
  )
babies%>%
  pivot_longer(
    cols = starts_with("weight"),
    names_to = "months",
    names_prefix= "\\w+_"
  )
#value_to argument
babies %>%
  pivot_longer(
    cols = starts_with("weight"),
    names_to = "months",
    names_prefix = "weight_",
    values_to = "weight")
#names_transform argument
babies%>%
  pivot_longer(
    cols = starts_with("weight"),
    names_to = "months",
    names_prefix= "weight_",
    values_to = "weight"
  ) %>%
  mutate(months=as.integer(months))
babies%>%
  pivot_longer(
    cols = starts_with("weight"),
    names_to = "months",
    names_prefix = "weight_",
    names_transform = list(months = as.integer),
    values_to
    = "weight")
#pivoting multiple sets of columns
set.seed(123)
babies <- tibble(
  id = 1001:1008,
  sex = c("F", "F", "M", "F", "M", "M", "M", "F"),
  weight_3 = c(9, 11, 17, 16, 11, 17, 16, 15),
  weight_6 = c(13, 16, 20, 18, 15, 21, 17, 16),
  weight_9 = c(16, 17, 23, 21, 16, 25, 19, 18),
  weight_12 =c(17, 20,24,22,18,26,21,19),
  length_3 =c(17, 19,23,20,18,22,21,18),
  length_6 =round(length_3+ rnorm(8,2,1)),
  length_9 =round(length_6+ rnorm(8,2,1)),
  length_12 =round(length_9+ rnorm(8,2,1)),
)%>%
  print()
babies%>%
  pivot_longer(
    cols = c(-id,-sex),
    names_to = c(".value","months"),
    names_sep= "_"
  )
babies_long<-babies %>%
  pivot_longer(
    cols = starts_with("weight"),
    names_to = "months",
    names_prefix= "weight_",
    values_to = "weight"
  ) %>%
  print()
babies_long<-babies %>%
  pivot_longer(
    cols = c(-id,-sex),
    names_to = "months",
    names_prefix= "weight_",
    values_to = "weight"
  ) %>%
  print()
babies_long<-babies %>%
  pivot_longer(
    cols = c(-id,-sex),
    names_to = "months",
    names_prefix= c("weight_","length_"),
    values_to = "weight"
  ) %>%
  print()
babies_long<-babies %>%
  pivot_longer(
    cols = c(-id,-sex),
    names_to = "months",
    values_to= "weight"
  ) %>%
  print()
babies_long<-babies %>%
  pivot_longer(
    cols = c(-id,-sex),
    names_to= "months"
  ) %>%
  print()
#names_sep argument
babies_long <- babies %>%
  pivot_longer(
    cols
    = c(-id,-sex),
    names_to = "months",
    names_sep = "_"
  ) %>%
  print()
babies_long <- babies %>%
  pivot_longer(
    cols
    = c(-id,-sex),
    names_to = c("measure", "months"),
    names_sep = "_"
  ) %>%
  print()
#the .value special value
babies_long <- babies %>%
  pivot_longer(
    cols
    = c(-id,-sex),
    names_to = c(".value", "months"),
    names_sep = "_",
    names_transform = list(months = as.integer)
  ) %>%
  print()
babies%>%
  pivot_longer(
    cols = c(-id,-sex),
    names_to = c("months",".value"),
    names_sep= "_")
#person period
babies_long %>%
  mutate(months = factor(months, c(3, 6, 9, 12))) %>%
  ggplot() +
  geom_point(aes(weight, length, color = months)) +
  labs(
    x = "Weight (Pounds)",
    y = "Length (Inches)",
    color = "Age (Months)"
  ) +
  theme_classic()
#pivoting wider
babies_long
babies <- babies_long %>%
  pivot_wider(
    names_from = "months",
    values_from = c("weight", "length")
  ) %>%
  print()
df<-tribble(
  ~id,~measure,~lbs_inches,
  1, "weight", 9,
  1, "length", 17,
  2, "weight", 11,
  2, "length", 19
)%>%
  print()
df%>%pivot_wider(
  names_from = "measure",
  values_from= "lbs_inches"
)
#person-level
babies %>%
  count(sex)
#pivot summaries
#wide to long
mean_weights <- babies %>%
  summarise(
    mean(weight_3),
    sd(weight_3),
    mean(weight_6),
    sd(weight_6),
    mean(weight_9),
    sd(weight_9),
    mean(weight_12),
    sd(weight_12),
  ) %>%
  print()
mean_weights %>%
  pivot_longer(
    cols = everything(),
    names_to = c(".value", "measure", "months"),
    names_pattern = "(\\w+)\\((\\w+)_(\\d+)"
  )
stringr::str_match("mean(weight_3)", "(\\w+)\\((\\w+)_(\\d+)")
#long to wide
summary_stats<-tribble(
  ~period, ~behavior, ~value, ~n, ~n_total,~percent,
  "SchoolYear Weekends", "Longsleeveshirt","Never", 6, 78, 8,
  "SchoolYear Weekends", "Longsleeveshirt","Seldom", 16,78, 21,
  "SchoolYear Weekends", "Longsleeveshirt","Sometimes", 33, 78, 42,
  "SchoolYear Weekends", "Longsleeveshirt","Often", 17, 78, 22,
  "SchoolYear Weekends", "Longsleeveshirt","Always", 6, 78, 8,
  "SchoolYear Weekends", "LongPants","Never", 5, 79, 6,
  "SchoolYear Weekends", "LongPants","Seldom", 15, 79, 19,
  "SchoolYear Weekends", "LongPants","Sometimes", 32, 79,41,
  "SchoolYear Weekends", "LongPants","Often", 19, 79, 24,
  "SchoolYear Weekends", "LongPants","Always", 8,79, 10,
  "Summer", "Longsleeveshirt","Never", 9,80,11,
  "Summer", "Longsleeveshirt","Seldom",18,80,22,
  "Summer", "Longsleeveshirt","Sometimes", 31, 80, 39,
  "Summer", "Longsleeveshirt","Often", 14, 80, 18,
  "Summer", "Longsleeveshirt","Always",8, 80, 10,
  "Summer", "LongPants", "Never", 7, 76,9,
  "Summer", "LongPants", "Seldom",16,76,21,
  "Summer", "LongPants", "Sometimes",27, 76, 36,
  "Summer", "LongPants", "Often", 18,76, 24,
  "Summer", "LongPants", "Always",8,76, 11
)%>%
  print()
summary_stats %>%
  # Combine n and percent into a single character string
  mutate(n_percent = paste0(n, " (", percent, ")")) %>%
  # We no longer need n, n_total, percent
  select(-n:-percent) %>%
  pivot_wider(
    names_from = "period",
    values_from = "n_percent"
  )
#Tidy data
#each value must have its column
births_ntd <- tibble(
  state = rep(c("CA", "FL", "TX"), each = 2),
  outcome = rep(c("births", "neural tube defects"), 3),
  count = c(454920, 318, 221542, 155, 378624, 265)
) %>%
  print()
births_ntd %>%
  pivot_wider(
    names_from = "outcome",
    values_from = "count"
  )
births_sex <- tibble(
  state = c("CA", "FL", "TX"),
  f_2018 = c(222911, 108556, 185526),
  m_2018 = c(232009, 112986, 193098)
) %>%
  print()
births_sex %>%
  pivot_longer(
    cols
    =-state,
    names_to = c("sex", "year"),
    names_sep = "_",
    values_to = "births"
  )
#each observation must have its own
babies
births_decade <- tibble(
  state = c("CA", "FL", "TX"),
  `2010` = c(409428, 199388, 340762),
  `2020` = c(454920, 221542, 378624)
) %>%
  print()
births_decade %>%
  pivot_longer(
    cols
    =-state,
    names_to = "year",
    values_to = "births"
  )
#each value must have its own cell
baby_sleep <- tibble(
  id = c(1001, 1002, 1003),
  sleep_range = c(".5-2", ".75-2.4", "1.1-3.8")
) %>%
  print()
baby_sleep %>%
  separate(
    col = sleep_range,
    into = c("min_hours", "max_hours"),
    sep = "-",
    convert = TRUE
  )
#complete function
reports <- tibble(
  date
  = as.Date(c(
    "2019-10-29", "2019-10-29", "2019-10-30", "2019-11-02", "2019-11-02"
  )),
  emp_id
  = c(5123, 2224, 5153, 9876, 4030),
  report_id = c("a8934", "af2as", "jzia3", "3293n", "dsf98")
) %>%
  print()
reports %>%
  count(date)
reports %>%
  count(date) %>%
  summarise(mean_reports_per_day = mean(n))
reports %>%
  count(date) %>%
  complete(
    date = seq.Date(
      from = as.Date("2019-10-28"),
      to = as.Date("2019-11-03"),
      by = "days"
    )
  )
reports%>%
  count(date) %>%
  complete(
    date= seq.Date(
      from = as.Date("2019-10-28"),
      to= as.Date("2019-11-03"),
      by= "days"
    ),
    fill= list(n= 0)
  )
reports%>%
  count(date) %>%
  complete(
    date= seq.Date(
      from = as.Date("2019-10-28"),
      to= as.Date("2019-11-03"),
      by= "days"
    ),
    fill= list(n= 0)
  ) %>%
  summarise(mean_reports_per_day=mean(n))


#REPEATED OPERATIONS
#MULTIPLE METHODS FOR REPEATED OPERATIONS
#tidy evaluation
#writing functions
library(dplyr)
study<-tibble(
  age =c(32, 30,32,29,24,38,25,24, 48, 29, 22, 29, 24, 28,24,25,
         25, 22,25,24,25,24,23,24, 31, 24, 29, 24, 22, 23,26,23,
         24, 25,24,33,27,25,26,26, 26, 26, 26, 27, 24, 43,25,24,
         27, 28,29,24,26,28,25,24, 26, 24, 26, 31, 24, 26,31,34,
         26, 25,27,NA),
  age_group =c(2, 2, 2,1,1, 2, 1, 1,2,1, 1, 1, 1,1,1,1, 1, 1, 1,1,1,
               1, 1, 1,2,1, 1, 1, 1,1,1, 1, 1, 1,1,2,1, 1, 1, 1,1,1,
               1, 1, 1,2,1, 1, 1, 1,1,1, 1, 1, 1,1,1,1, 1, 2, 1,1,2,
               2, 1, 1,1,NA),
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
         26.62, 21.43, 19.14, 23.49,22.66,32.98,25.05,18.31,29.13,27.07, 20.37, 25.01, 19.69, 25.97, 18.88, 20.07, NA, 26.76,
         26.97, 25.24, 20.68, 23.72, 24.82, 23.62, 18.65, 24.03, 23.86,
         10.63, 23.02, 23.72, 20.82, 28.24, NA, 37.55, 18.88, 18.3,
         19.13, 21.48, 22.59, 24.96, 21.63, NA, 29.35, 21.76, 17.97,
         22.31, 19.27, 24.07, 22.76, NA),
  bmi_3cat = c(3, 1, 2, 3, 1, 2, 1, 1, 2, 2, 2, 3, 1, 1, 1, 1, 1, 1, 2, 1, 1,
               1, 1, 3, 2, 1, 2, 2, 1, 2, 1, 2, 1, 1, NA, 2, 2, 2, 1, 1, 1, 1,
               1, 1, 1, 1, 1, 1, 1, 2, NA, 3, 1, 1, 1, 1, 1, 1, 1, NA, 2, 1,
               1, 1, 1, 1, 1, NA)
) %>%
  mutate(
    age_group = factor(age_group, labels = c("Younger than 30", "30 and Older")),
    gender
    = factor(gender, labels = c("Female", "Male")),
    bmi_3cat = factor(bmi_3cat, labels = c("Normal", "Overweight", "Obese"))
  ) %>%
  print()
study %>%
  summarise(
    n_miss = sum(is.na(age)),
    mean = mean(age, na.rm = TRUE),
    median = median(age, na.rm = TRUE),min = min(age, na.rm = TRUE),
max = max(age, na.rm = TRUE))
study %>%
  summarise(
    n_miss = sum(is.na(ht_in)),
    mean = mean(ht_in, na.rm = TRUE),
    median = median(ht_in, na.rm = TRUE),
    min = min(ht_in, na.rm = TRUE),
    max = max(ht_in, na.rm = TRUE))
study %>%
summarise(
  n_miss = sum(is.na(wt_lbs)),
  mean = mean(wt_lbs, na.rm = TRUE),
  median = median(wt_lbs, na.rm = TRUE),
  min = min(ht_in, na.rm = TRUE),
max = max(wt_lbs, na.rm = TRUE))
study %>%
  summarise(
    n_miss = sum(is.na(bmi)),
    mean = mean(bmi, na.rm = TRUE),
    median = median(bmi, na.rm = TRUE),
    min = min(bmi, na.rm = TRUE),
    max = max(bmi, na.rm = TRUE))
continuous_stats <- function(var) {
  study %>%
    summarise(
      n_miss = sum(is.na({{ var }})),
      mean = mean({{ var }}, na.rm = TRUE),
      median = median({{ var }}, na.rm = TRUE),
      min = min({{ var }}, na.rm = TRUE),
      max = max({{ var }}, na.rm = TRUE))
}
rlang::last_trace()
continuous_stats(age)
continuous_stats <- function(var) {
  study %>%
    summarise(
      n_miss = sum(is.na({{ var }})),
      mean = mean({{ var }}, na.rm = TRUE),
      median = median({{ var }}, na.rm = TRUE),
      min = min({{ var }}, na.rm = TRUE),
      max = max({{ var }}, na.rm = TRUE)
    )
}
continuous_stats(ht_in)
continuous_stats(wt_lbs)
continuous_stats(bmi)
function()
  people_1 <- tribble(
    ~id_1, ~name_first_1, ~name_last_1, ~street_1,
    1, "Easton", NA, "Alameda",
    2, "Elias", "Salazar", "Crissy Field",
    3, "Colton", "Fox", "San Bruno",
    4, "Cameron", "Warren", "Nottingham",
    5, "Carson", "Mills", "Jersey",
    6, "Addison", "Meyer", "Tingley",
    7, "Aubrey", "Rice", "Buena Vista",
    8, "Ellie", "Schmidt", "Division",
    9, "Robert", "Garza", "Red Rock",
    10, "Stella", "Daniels", "Holland") %>%
  print()
people_2 <- tribble(
  ~id_1, ~name_first_1, ~name_last_1, ~street_1,
  1, "Easton", "Stone", "Alameda",
  2, "Elias", "Salazar", "Crissy Field",
  3, NA, "Fox", NA,
  4, "Cameron", "Warren", "Nottingham",
  5, "Carson", "Mills", "Jersey",
  6, "Addison", NA, NA,
  7, "Aubrey", "Rice", "Buena Vista",
  8, NA, "Schmidt", "Division",
  9, "Robert", "Garza", "Red Rock",
  10, "Stella", NA, "Holland") %>%
  print()
people<-people_1 %>%
  bind_cols(people_2)%>%
  print()
people%>%
  mutate(
    name_first_match = name_first_1 == name_first_2,
    name_last_match = name_last_1 == name_last_2,
    street_match = street_1 == street_2
  ) %>% select(id_1, starts_with("name_f"), starts_with("name_l"), starts_with("s"))
names(people)
names(people) <- c(
  "id_1",
  "name_first_1",
  "name_last_1",
  "street_1",
  "id_2",
  "name_first_2",
  "name_last_2",
  "street_2"
)
people %>%
  mutate(
    name_first_match = name_first_1 == name_first_2,
    name_last_match = name_last_1 == name_last_2,
    street_match = street_1 == street_2
  ) %>%
  select(id_1, starts_with("name_f"), starts_with("name_l"), starts_with("s"))
#spotting aneed for a function
1 == 1
1 == 2
1 == NA
NA == 2
#MAKING THE CODE WORK FOR ONE SPECIFICCASE
"Colton" == NA
result <- "Colton" == NA
result
result <- "Colton" == NA
result <- if_else(is.na(result), FALSE, result)
result
#making the solution into a function
is_match <- function() {}
is_match <- function() {
  result <- "Colton" == NA
  result <- if_else(is.na(result), FALSE, result)
  result
  }
is_match()
result <- "Colton" == NA
result <- if_else(is.na(result), FALSE, result)
result
result <- "Colton" == NA
result <- if_else(is.na(result), FALSE, result)
result
is_match()
is_match(name = "Easton")
is_match <- function(name) {
  result <- "Colton" == NA
  result <- if_else(is.na(result), FALSE, result)
  result
}
is_match <- function(name_1, name_2) {
  result <- "Colton" == NA
  result <- if_else(is.na(result), FALSE, result)
  result
}
#start generalising the function
is_match <- function(first_name) {
  result <- first_name == NA
  result <- if_else(is.na(result), FALSE, result)
  result
}
is_match(first_name = "Easton")
result <- "Easton" == NA
result <- if_else(is.na(result), FALSE, result)
result
is_match <- function(first_name, first_name) {
  result <- first_name == first_name
  result <- if_else(is.na(result), FALSE, result)
  result
}
is_match <- function(first_name_1, first_name_2) {
  result <- first_name_1 == first_name_2
  result <- if_else(is.na(result), FALSE, result)
  result
}
is_match(first_name_1 = "Easton", first_name_2 = "Easton")
is_match(first_name_1 = "Easton", first_name_2 = NA)
is_match <- function(value_1, value_2) {
  result <- value_1 == value_2 # Don't forget to change the variable names here!!
  result <- if_else(is.na(result), FALSE, result)
  result
}
people %>%
  mutate(
    name_first_match = is_match(name_first_1, name_first_2),
    name_last_match = is_match(name_last_1, name_last_2),
    street_match
    = is_match(street_1, street_2)
  ) %>%
  # Order like columns next to each other for easier comparison
  select(id_1, starts_with("name_f"), starts_with("name_l"), starts_with("s"))
#giving your function default values
increment <- function(x) {
  x + 1
}
increment(2)
increment <- function(x, by) {
  x + by
}
increment(2, 2)
increment(2)
increment <- function(x, by = 1) {
  x + by
}
increment(2)
increment(2, 1)
increment(2, 2)
increment(c(1, 2, 3), 2)
#the values your functions return
is_match <- function(value_1, value_2) {
  result <- value_1 == value_2
  # Do this first
  result <- if_else(is.na(result), FALSE, result) # Then this
  result
}
is_match <- function(value_1, value_2) {
  result <- value_1 == value_2
  result <- if_else(is.na(result), FALSE, result)
}
is_match("Easton", "Easton")
sum(1, 1)
x <- sum(1, 1)
x <- is_match("Easton", "Easton")
x
increment <- function(x, by = 1) {
  x + by 
}
increment <- function(x, by = 1) {
  out <- x + by # Now we assign the value to an object
  out
}
increment(2)
increment <- function(x, by = 1) {
  out <- x + by
  return(out)
}
increment(2)
increment <- function(x, by = 1) {
  out <- x + by
  out <- out + 1 # Adding an extra 1
  return(out)
}
increment(2)
increment <- function(x, by = 1) {
  out <- x + by
  return(out)
# Return in the second line above adding an extra 1
out <- out + 1
}
increment(2)
increment <- function(x, by = 1) {
  out <- x + by
  print(out)
}
increment(2)
increment <- function(x, by = 1) {
  out <- x + by
  print(out)
}
x <- increment(2)
x
#lexical scoping and functions
increment <- function(x, by = 1) {
  out <- x + by # Assign the value to the out object inside the function
  out
}
x <- increment(2)
x
out
add <- function(x) {
  x + y
}
add(2)
y <- 100
add(2)
add <- function(x) {
  y <- 1
  x + y
}
y <- 100
add(2)
add <- function(x, y) {
  x + y
}
y <- 100
add(2)
#Tidy evaluation
study
study%>%
  count(age_group) %>%
  mutate(percent = n/ sum(n)*100)
cat_stats<-function(var){
  study %>%
    count(age_group)%>%
    mutate(percent= n /sum(n)* 100)
}
cat_stats()
cat_stats<-function(var){
  study %>%
    count(var) %>%
    mutate(percent= n /sum(n)* 100)
}
cat_stats(age_group)
cat_stats <- function(var) {
  study %>%
    count({{ var }}) %>%
    mutate(percent = n / sum(n) * 100)
}
cat_stats(age_group)
cat_stats(gender)
cat_stats(bmi_3cat)
other_study <- tibble(
  id = 1:10,
  age_group = c(rep("Younger", 9), "Older"),
) %>%
  print()
cat_stats(age_group)
cat_stats <- function(var) {
  study %>%
    count({{ var }}) %>%
    mutate(percent = n / sum(n) * 100)
}
cat_stats <- function(data, var) {
  study %>%
    count({{ var }}) %>%
    mutate(percent = n / sum(n) * 100)
}
cat_stats <- function(data, var) {
  data %>%
    count({{ var }}) %>%
    mutate(percent = n / sum(n) * 100)
}
cat_stats(other_study, age_group)
other_study %>%
  cat_stats(age_group)
age_group
other_study$age_group
other_study
cat_stats<-function(data,var){
  {{data}} %>%
    count({{ var }})%>%
    mutate(percent= n /sum(n)* 100)
}
cat_stats(other_study,age_group)


#COLUMN WISE OPERATIONS IN dplyr
library(dplyr, warn.conflicts = FALSE)
set.seed(123)
df_xyz <- tibble(
  row = 1:10,
  x = rnorm(10),
  y = rnorm(10),
  z = rnorm(10)
) %>%
  print()
df_xyz %>%
  summarise(
    x_mean = mean(x),
    y_mean = mean(y),
    z_mean = mean(y))
df_xyz %>%
  summarise(
    across(
      .cols = c(x:z),
      .fns
      = mean,
      .names = "{col}_mean"))
#The across function
df_xyz$x[2] <- NA_real_
df_xyz$y[4] <- NA_real_
df_xyz$z[6] <- NA_real_
df_xyz
df_xyz %>%
  summarise(
    x_mean = mean(x),
    y_mean = mean(y),
    z_mean = mean(y)
  )
df_xyz %>%
  summarise(
    x_mean = mean(x, na.rm = TRUE),
    y_mean = mean(y, na.rm = TRUE),
    z_mean = mean(z, na.rm = TRUE))
df_xyz %>%
  summarise(
    across(
      .cols = everything(),
      .fns = mean,
na.rm = TRUE, # Passing na.rm = TRUE to the ... argument
.names = "{col}_mean"))
df_xyz %>%
  summarise(
    across(
      .cols = everything(),
      .fns
      = mean,
      na.rm = TRUE))
df_xyz %>%
  summarise(
    across(
      .cols = everything(),
      .fns
      = mean,
      na.rm = TRUE,
      .names = "{col}_{fn}"))
library(dplyr)
df_xyz %>%
  summarise(
    across(
      .cols = everything(),
      .fns
      = mean,
      na.rm = TRUE))
df_xyz %>%
  summarise(
    across(
      .cols = everything(),
      .fns
      = mean,
      na.rm = TRUE,
      .names = "{col}_{fn}"))
df_xyz %>%
  summarise(
    across(
      .cols = everything(),
      .fns
      = list(mean = mean),
      na.rm = TRUE,
      .names = "{col}_{fn}"))
df_xyz %>%
  summarise(
    across(
      .cols = everything(),
      .fns
      = list(r4epi = mean),
      na.rm = TRUE,
      .names = "{col}_{fn}"))
df_xyz %>%
  summarise(
    across(
      .cols = everything(),
      .fns
      = list(mean = r4epi),
      na.rm = TRUE,
      .names = "{col}_{fn}"))
df_xyz %>%
  summarise(
    x_min = min(x, na.rm = TRUE),
    x_max = max(x, na.rm = TRUE),
    y_min = min(y, na.rm = TRUE),
    y_max = max(y, na.rm = TRUE),
    z_min = min(z, na.rm = TRUE),
    z_max = max(z, na.rm = TRUE))
df_xyz %>%
  summarise(
    across(
      .cols = everything(),
      .fns
      = list(min = min, max = max),
      na.rm = TRUE,
      .names = "{col}_{fn}"))
df_xyz %>%
  summarise(
    across(
      .cols = everything(),
      .fns
      = ~ mean(.x, na.rm = TRUE),
      .names = "{col}_mean"))
#Across with mutate
set.seed(123)
demographics <- tibble(
  id
  = 1:10,
  age = c(sample(1:30, 9, TRUE), NA),
  race = c(1, 2, 1, 4, 7, 1, 2, 9, 1, 3),
  hispanic = c(7, 0, 1, 0, 1, 0, 1, 9, 0, 1),
  edu_4cat = c(4, 2, 9, 1, 2, 3, 4, 9, 3, 3),
  inc_6cat =c(1,4,1,1, 5, 3,2,2,7, 9)
)%>%
  print()
demographics%>%
  mutate(
    race = if_else(race== 7| race== 9, NA_real_,race),
    hispanic= if_else(race== 7| hispanic== 9,NA_real_,hispanic),
    edu_4cat= if_else(edu_4cat== 7 |edu_4cat ==9,NA_real_,edu_4cat))
demographics%>%
  mutate(
    across(
      .cols =c(-id,-age),
      .fns =~ if_else(.x== 7| .x ==9,NA_real_,.x)))
set.seed(123)
drug_trial<-tibble(
  id =1:10,
  se_headache =sample(0:1,10,TRUE),
  se_diarrhea =sample(0:1,10,TRUE),
  se_dry_mouth =sample(0:1,10,TRUE),
  se_nausea =sample(0:1,10,TRUE)
)%>%
  print()
drug_trial%>%
  mutate(
    se_headache_f = factor(se_headache, 0:1, c("No","Yes")),
    se_diarrhea_f = factor(se_diarrhea, 0:1, c("No","Yes")),
    se_dry_mouth_f= factor(se_dry_mouth,0:1,c("No","Yes")))
drug_trial%>%
  mutate(
    across(
      .cols = starts_with("se"),
      .fns = ~factor(.x,0:1,c("No", "Yes")),
      .names= "{col}_f"))
#across with summarise
library(readr)
library(stringr)
library(here)
ehr <- read_rds("ehr.Rds")
symptoms <- ehr %>%
  select(symptoms) %>%
  print()
symptoms <- symptoms %>%
  mutate(
    pain
    = str_detect(symptoms, "Pain"),
    headache = str_detect(symptoms, "Headache"),
    nausea = str_detect(symptoms, "Nausea")
  ) %>%
  print()
table(symptoms$headache)
table(symptoms$pain)
table(symptoms$nausea)
symptoms %>%
  summarise(
    had_headache = sum(headache, na.rm = TRUE),
    had_pain
    = sum(pain, na.rm = TRUE),
    had_nausea = sum(nausea, na.rm = TRUE))
symptoms %>%
  summarise(
    across(
      .cols = c(headache, pain, nausea),
      .fns
      = ~ sum(.x, na.rm = TRUE)))
symptoms %>%
  summarise(
    had_headache = mean(headache, na.rm = TRUE),
    had_pain
    = mean(pain, na.rm = TRUE),
    had_nausea = mean(nausea, na.rm = TRUE))
symptoms %>%
  summarise(
    across(
      .cols = c(pain, headache, nausea),
      .fns = ~ mean(.x, na.rm = TRUE)))
symptom_summary <- symptoms %>%
  summarise(
    across(
      .cols = c(pain, headache, nausea),
      .fns = list(
        count = ~ sum(.x, na.rm = TRUE),
        prop = ~ mean(.x, na.rm = TRUE))))%>%
  print()
symptom_summary %>%
  tidyr::pivot_longer(
    cols
    = everything(),
    names_to = c("symptom", ".value"),
    names_sep = "_")
study<-tibble(
  age =c(32, 30,32,29,24,38,25,24, 48, 29, 22, 29, 24, 28,24,25,
         25, 22,25,24,25,24,23,24, 31, 24, 29, 24, 22, 23,26,23,
         24, 25,24,33,27,25,26,26, 26, 26, 26, 27, 24, 43,25,24,
         27, 28,29,24,26,28,25,24, 26, 24, 26, 31, 24, 26,31,34,
         26, 25,27,NA),
  age_group =c(2, 2, 2,1,1, 2, 1, 1,2,1, 1, 1, 1,1,1,1, 1, 1, 1,1,1,
               1, 1, 1,2,1, 1, 1, 1,1,1, 1, 1, 1,1,2,1, 1, 1, 1,1,1,
               1, 1, 1,2,1, 1, 1, 1,1,1, 1, 1, 1,1,1,1, 1, 2, 1,1,2,
               2, 1, 1,1,NA),
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
              1, 1, 1,1,1, NA))%>%mutate(
                age_group = factor(age_group, labels = c("Younger than 30", "30 and Older")),
                gender
                = factor(gender, labels = c("Female", "Male")),
                bmi_3cat = factor(bmi_3cat, labels = c("Normal", "Overweight", "Obese"))
              ) %>%
  print()
continuous_stats <- function(var) {
  study %>%
    summarise(
      n_miss = sum(is.na({{ var }})),
      mean = mean({{ var }}, na.rm = TRUE),
      median = median({{ var }}, na.rm = TRUE),
      min = min({{ var }}, na.rm = TRUE),
max = max({{ var }}, na.rm = TRUE))
}
continuous_stats(age)
continuous_stats(ht_in)
continuous_stats(wt_lbs)
continuous_stats(bmi)
summary_stats <- study %>%
  summarise(
    across(
      .cols = c(age, ht_in, wt_lbs, bmi),
      .fns = list(
        n_miss = ~ sum(is.na(.x)),
        mean = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x,na.rm= TRUE))))%>%
  print()
summary_stats%>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = c("characteristic",".value"),
    names_sep= "_")        
summary_stats <- study %>%
  summarise(
    across(
      .cols = c(age, ht_in, wt_lbs, bmi),
      .fns
      = list(
        n_miss = ~ sum(is.na(.x)),
        mean = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)),
      .names = "{col}-{fn}" # This is the new part of the code
    ))%>%
  print()
library(dplyr)
summary_stats %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = c("characteristic", ".value"),
    names_sep = "-")
#Across with filter
df_xyz %>%
  filter(!is.na(x))
df_xyz%>%
  filter(!is.na(x))%>%
  summarise(mean= mean(x))
df_xyz%>%
  filter(!is.na(x))%>%
  filter(!is.na(y))%>%
  filter(!is.na(z))
df_xyz %>%
  filter(
    if_all(
      .cols = c(x:z),
      .fns = ~ !is.na(.x)))
is.na(df_xyz$x)
!is.na(df_xyz$x)
df_xyz %>%
  filter(c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))
!is.na(df_xyz$y)
!is.na(df_xyz$z)
not_missing <- tibble(
  row = 1:10,
  x = !is.na(df_xyz$x),
  y = !is.na(df_xyz$y),
  z = !is.na(df_xyz$z)
) %>%
  print()
df_xyz %>%
  filter(
    if_any(
      .cols = c(x:z),
      .fns = ~ !is.na(.x)))
df_xyz %>%
  filter(
    if_all(
      .cols = c(x:z),
      .fns = ~ !is.na(.x)))


#WRITING FOR LOOPS
set.seed(123)
df_xyz <- tibble(
  x = rnorm(10),
  y = rnorm(10),
  z = rnorm(10)
) %>%
  print()
df_xyz %>%
  summarise(
    x_mean = mean(x),
    y_mean = mean(y),
    z_mean = mean(y))
df_xyz %>%
  summarise(
    across(
      .cols = everything(),
      .fns
      = mean,
      .names = "{col}_mean"))
xyz_means <- vector("double", ncol(df_xyz))
for(i in seq_along(df_xyz)) {
  xyz_means[[i]] <- mean(df_xyz[[i]])}
mean(df_xyz$x)
mean(df_xyz$y)
mean(df_xyz$z)
mean(df_xyz[["x"]])
mean(df_xyz[["y"]])
mean(df_xyz[["z"]])
mean(df_xyz[[1]])
mean(df_xyz[[2]])
mean(df_xyz[[3]])
#the for function
for(in 1) {
  print(i)
  }
for(i 1) {
  print(i)}
for(i in 1) {
  print(i)
  }
for(i in c(1, 2, 3)) {
  print(i)
}
for(x in c(1, 2, 3)) {
  print(x)
}
for(number in c(1, 2, 3)) {
  print(number)
}
for(i in df_xyz) {
  print(i)
}
for(i in df_xyz) {
  mean(i)
}
for(i in df_xyz) {
  print(mean(i))
}
xyz_means <- df_xyz %>%
  summarise(
    across(
      .cols = everything(),
      .fns
      = mean,
      .names = "{col}_mean"))
xyz_means
xyz_means <- for(i in df_xyz) {
  mean(i)
}
xyz_means
my_vec <- vector()
my_vec
xyz_means <- vector("numeric", 3)
xyz_means
for(i in df_xyz) {
  xyz_means <- mean(i)
}
xyz_means
my_vec <- c(1, 2, 3)
my_vec
my_vec <--0.4245589
xyz_means <- vector("numeric", 3)
xyz_means
xyz_means[[1]] <- 0.07462564
xyz_means[[2]] <- 0.208622
xyz_means[[3]] <--0.4245589
xyz_means
mean(df_xyz[[1]])
mean(df_xyz[[2]])
mean(df_xyz[[3]])
for(i in c(1, 2, 3)) {
  print(i)
}
xyz_means <- vector("numeric", 3)
xyz_means
for(i in c(1, 2, 3)) {
  xyz_means[[i]] <- mean(df_xyz[[i]])
}
xyz_means
seq_along(c(4, 5, 6))
seq_along(c("a", "b", "c", "d"))
seq_along(df_xyz)
xyz_means <- vector("numeric", 3)
for(i in seq_along(df_xyz)) {
  xyz_means[[i]] <- mean(df_xyz[[i]])
}
xyz_means
df_xyz <- df_xyz %>%
  mutate(a = rnorm(10)) %>%
  print()
xyz_means<-vector("numeric", length(df_xyz))
for(i in seq_along(df_xyz)){ #Usingseq_along()insteadofc(1,2,3)
  xyz_means[[i]]<-mean(df_xyz[[i]])
}
xyz_means
multi_means<-function(data){
  #Create astructuretocontainresults
  result <-vector("numeric", length(data))
  #Iterate overeach columnofdata
  for(i in seq_along(data)){
    result[[i]]<-mean(data[[i]])
  }
  # Return the result
  result
}
multi_means(df_xyz)
set.seed(123)
new_df <- tibble(
  age = rnorm(10, 50, 10),
  height = rnorm(10, 65, 5),
  weight = rnorm(10, 165, 10)
) %>%
  print()
multi_means(new_df)
xyz_means <- vector("numeric", length(df_xyz))
for(i in seq_along(df_xyz)) {
  xyz_means[[i]] <- mean(df_xyz[[i]])
  names(xyz_means)[[i]] <- paste0(names(df_xyz)[[i]], "_mean")
}
xyz_means
xyz_means %>%
  as.list() %>%
  as_tibble()
multi_means <- function(data) {
  # Create a structure to contain results
  result <- vector("numeric", length(data))
  # Iterate over each column of data
  for(i in seq_along(data)) {
    result[[i]] <- mean(data[[i]])
    names(result)[[i]] <- paste0(names(data)[[i]], "_mean")
  }
  # Return the result as a tibble
  as_tibble(as.list(result))
}
multi_means(new_df)
#using for loops for data transfer
library(readxl)
houston <- read_excel(
  "city_ses.xlsx",
  sheet = "Houston")%>%
  print()
atlanta <- city_ses %>%
  sheet = "Atlanta"%>%
  print()
city_ses
ls()
class(city_ses)
library(readxl)
atlanta <- read_excel(
  "C:/Users/Immah/Downloads/city_ses.xlsx",
  sheet = "Atlanta"
)
library(readxl)
Houston<- read_excel(
  "C:/Users/Immah/Downloads/city_ses.xlsx",
  sheet = "Houston"
)
library(readxl)

Charlotte<- read_excel(
  "C:/Users/Immah/Downloads/city_ses.xlsx",
  sheet = "Charlotte"
)
import_cities<-function(sheet){
  df <-read_excel(
    "city_ses.xlsx",
    sheet= sheet)
}
houston<-import_cities("Houston")%>%print()
import_cities <- function(city) {
  read_excel(
    "C:/Users/Immah/Downloads/city_ses.xlsx",
    sheet = city
  )
}
houston <- import_cities("Houston") %>% print()
atlanta<-import_cities("Atlanta")%>%print()
charlotte <- import_cities("Charlotte") %>% print()
# Save the file path to an object so we don't have to type it repeatedly
# or hard-code it in.
path <- "city_ses.xlsx"
# Use readxl::excel_sheets to get the name of each sheet in the workbook.
# this makes our code more robust.
sheets <- excel_sheets(path)
for(i in seq_along(sheets)) {
  # Convert sheet name to lowercase before using it to name the df
  new_nm <- tolower(sheets[[i]])
  assign(new_nm, read_excel(path, sheet = sheets[[i]]))
}
library(readxl)
path <- "C:/Users/Immah/Downloads/city_ses.xlsx"
sheets <- excel_sheets(path)
sheets
for (i in seq_along(sheets)) {
  new_nm <- tolower(sheets[[i]])
  assign(new_nm, read_excel(path, sheet = sheets[[i]]))
}
houston
atlanta
charlotte
#for loops for data management
people_1 <- tribble(
    ~id_1, ~name_first_1, ~name_last_1, ~street_1,
    1, "Easton",  NA,         "Alameda",
    2, "Elias",   "Salazar",  "Crissy Field",
    3, "Colton",  "Fox",      "San Bruno",
    4, "Cameron", "Warren",   "Nottingham",
    5, "Carson",  "Mills",    "Jersey",
    6, "Addison", "Meyer",    "Tingley",
    7, "Aubrey",  "Rice",     "Buena Vista",
    8, "Ellie",   "Schmidt",  "Division",
    9, "Robert",  "Garza",    "Red Rock",
    10, "Stella", "Daniels", "Holland")
people_2 <- tribble(
  ~id_1, ~name_first_1, ~name_last_1, ~street_1,
  1, "Easton",  NA,         "Alameda",
  2, "Elias",   "Salazar",  "Crissy Field",
  3, NA,  "Fox",      NA,
  4, "Cameron", "Warren",   "Nottingham",
  5, "Carson",  "Mills",    "Jersey",
  6, "Addison", NA,    NA,
  7, "Aubrey",  "Rice",     "Buena Vista",
  8, NA,   "Schmidt",  "Division",
  9, "Robert",  "Garza",    "Red Rock",
  10, "Stella", NA, "Holland")  
people<-people_1 %>%
  bind_cols(people_2)%>%
  print()
is_match<-function(value_1,value_2){
  result <-value_1== value_2
  result <-if_else(is.na(result), FALSE,result)
  result
}
people%>%
  mutate(
    name_first_match= is_match(name_first_1,name_first_2),
    name_last_match = is_match(name_last_1,name_last_2),
    street_match = is_match(street_1,street_2)
  ) %>%
  #Orderlike columnsnextto eachother foreasiercomparison
  select(id_1, starts_with("name_f"), starts_with("name_l"), starts_with("s"))
names(people)
library(dplyr)

people <- people %>%
  rename(
    id_1 = `id_1...1`,
    name_first_1 = `name_first_1...2`,
    name_last_1 = `name_last_1...3`,
    street_1 = `street_1...4`,
    id_2 = `id_1...5`,
    name_first_2 = `name_first_1...6`,
    name_last_2 = `name_last_1...7`,
    street_2 = `street_1...8`
  )
cols<-c("name_first","name_last", "street")
for(i in seq_along(cols)){
  col_1 <-paste0(cols[[i]],"_1")
  col_2 <-paste0(cols[[i]],"_2")
  new_col<-paste0(cols[[i]],"_match")
  people[[new_col]] <-is_match(people[[col_1]],people[[col_2]])
}
people%>%
  select(id_1, starts_with("name_f"), starts_with("name_l"), starts_with("s"))
cols<-c("name_first","name_last", "street")
for(i in seq_along(cols)){
  col_1 <-paste0(cols[[i]],"_1")
  col_2 <-paste0(cols[[i]],"_2")
  new_col<-paste0(cols[[i]],"_match")
  print(col_1)
  print(col_2)
  print(new_col)
}
cols <- c("name_first", "name_last", "street")
for(i in seq_along(cols)) {
  col_1 <- paste0(cols[[i]], "_1")
  col_2 <- paste0(cols[[i]], "_2")
  print(people[[col_1]])
}
cols <- c("name_first", "name_last", "street")
for(i in seq_along(cols)) {
  col_1 <- paste0(cols[[i]], "_1")
  col_2 <- paste0(cols[[i]], "_2")
  print(people[[col_2]])
}
cols <- c("name_first", "name_last", "street")
for(i in seq_along(cols)) {
  col_1 <- paste0(cols[[i]], "_1")
  col_2 <- paste0(cols[[i]], "_2")
  print(is_match(people[[col_1]], people[[col_2]]))
}
cols<-c("name_first","name_last", "street")
for(i in seq_along(cols)){
  col_1 <-paste0(cols[[i]],"_1")
  col_2 <-paste0(cols[[i]],"_2")
  new_col<-paste0(cols[[i]],"_match")
  people[[new_col]] <-is_match(people[[col_1]],people[[col_2]])
}
people%>%
  select(id_1, starts_with("name_f"), starts_with("name_l"), starts_with("s"))
#for loops for analysis
study<-tibble(
  age =c(32, 30,32,29,24,38,25,24, 48, 29, 22, 29, 24, 28,24,25,
         25, 22,25,24,25,24,23,24, 31, 24, 29, 24, 22, 23,26,23,
         24, 25,24,33,27,25,26,26, 26, 26, 26, 27, 24, 43,25,24,
         27, 28,29,24,26,28,25,24, 26, 24, 26, 31, 24, 26,31,34,
         26, 25,27,NA),
  age_group =c(2, 2, 2,1,1, 2, 1, 1,2,1, 1, 1, 1,1,1,1, 1, 1, 1,1,1,
               1, 1, 1,2,1, 1, 1, 1,1,1, 1, 1, 1,1,2,1, 1, 1, 1,1,1,
               1, 1, 1,2,1, 1, 1, 1,1,1, 1, 1, 1,1,1,1, 1, 2, 1,1,2,
               2, 1, 1,1,NA),
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
              1, 1, 1,1,1, NA)) %>%
  mutate(
    age_group = factor(age_group, labels = c("Younger than 30", "30 and Older")),
    gender
    = factor(gender, labels = c("Female", "Male")),
    bmi_3cat = factor(bmi_3cat, labels = c("Normal", "Overweight", "Obese"))
  ) %>%
  print()
summary_stats <- study %>%
  summarise(
    across(
      .cols = c(age, ht_in, wt_lbs, bmi),
      .fns
      = list(
        n_miss = ~ sum(is.na(.x)),
        mean = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
  ),
.names = "{col}-{fn}")) %>%
  print()
summary_stats %>%
  tidyr::pivot_longer(
    cols
    = everything(),
    names_to = c("characteristic", ".value"),
    names_sep = "-"
  )
study %>%
  count(age_group)
library(dplyr)
study %>%
  count(gender)
study %>%
  count(bmi_3cat)
study %>%
  summarise(
    across(
      .cols = c(age_group, gender, bmi_3cat),
      .fns = count))
study %>%
  summarise(
    across(
      .cols = c(age, ht_in),
      .fns = ~ mean(.x, na.rm = TRUE)))
# Structure 1. An object to contain the results.
# Create the data frame structure that will contain our results
cat_table <- tibble(
  variable = vector("character"),
  category = vector("character"),
  n
  = vector("numeric"))
# Structure 2. The actual for loop.
# For each column, get the column name, category names, and count.
# Then, add them to the bottom of the results data frame we created above.
for(i in c("age_group", "gender", "bmi_3cat")) {
  cat_stats <- study %>%
    count(.data[[i]]) %>% # Use .data to refer to the current data frame.
    mutate(variable = names(.)[1]) %>% # Use . to refer to the result to this point.
    rename(category = 1)
  # Here is where we update cat_table with the results for each column
  cat_table <- bind_rows(cat_table, cat_stats)
}
cat_table
cat_table <- tibble(
  variable = vector("character"),
  category = vector("character"),
  n
  = vector("numeric"))
str(cat_table)
for(i in c("age_group", "gender", "bmi_3cat")) {
  cat_stats <- study %>%
    count(.data[[i]]) %>%
    mutate(variable = names(.)[1]) %>%
    rename(category = 1)
  cat_table <- bind_rows(cat_table, cat_stats)
}
cat_stats <- study %>%
  count(.data[[i]]) %>%
  mutate(variable = names(.)[1]) %>%
  rename(category = 1)
cat_stats <- study %>%
  count(age_group) %>%
  mutate(variable = names(.)[1]) %>%
  rename(category = 1) %>%
  print()
cat_stats <- study %>%
  count(age_group) %>%
  print()
study %>%
  count(age_group) %>%
  tidyr::pivot_longer(
    cols
    = "age_group",
    names_to = "variable",
    values_to = "category")
names(cat_stats)
names(cat_stats)[1]
names(.)[1]
study %>%
  count(age_group)
names(study %>% count(age_group))
study %>% count(age_group) %>% names(.)
cat_stats <- study %>%
  count(age_group) %>%
  mutate(variable = names(.)[1]) %>%
  print()
study %>%
  count(age_group) %>%
  bind_rows(study %>% count(gender))
cat_stats <- study %>%
  count(age_group) %>%
  mutate(variable = names(.)[1]) %>%
  rename(category = age_group) %>%
  print()
cat_stats <- study %>%
  count(age_group) %>%
  mutate(variable = names(.)[1]) %>%
  rename(category = 1) %>% # Replace age_group with 1
  print()
cat_stats <- study %>%
  count(gender) %>% # Changed the column from age_group to gender
  mutate(variable = names(.)[1]) %>%
  rename(category = 1) %>% # Still have 1 here
  print()
cat_table <- tibble(
  variable = vector("character"),
  category = vector("character"),
  n
  = vector("numeric")
) %>%
  print()
cat_stats <- study %>%
  count(age_group) %>%
  mutate(variable = names(.)[1]) %>%
  rename(category = 1) %>%
  print()
cat_table <- cat_table %>%
  bind_rows(cat_stats) %>%
  print()
cat_stats <- study %>%
  count(gender) %>% # Change to gender
  mutate(variable = names(.)[1]) %>%
  rename(category = 1)
cat_table <- cat_table %>%
  bind_rows(cat_stats) %>%
  print()
cat_stats <- study %>%
  count(bmi_3cat) %>% # Change to bmi_3cat
  mutate(variable = names(.)[1]) %>%
  rename(category = 1)
cat_table <- cat_table %>%
  bind_rows(cat_stats) %>%
  print()
for(i in c(age_group, gender, bmi_3cat)) {
  study %>%
    count(i) %>%
    mutate(variable = names(.)[1]) %>%
    rename(category = 1)
}
for(i in c("age_group", "gender", "bmi_3cat")) {
  study %>%
    count(i) %>%
    mutate(variable = names(.)[1]) %>%
    rename(category = 1)
}
for(i in c("age_group", "gender", "bmi_3cat")) {
  study %>%
    count(.data[[i]]) %>%
    mutate(variable = names(.)[1]) %>%
    rename(category = 1) %>%
    print()
}
study %>%
  count(age_group)
table(age_group)
table(study[["age_group"]])
for(i in c("age_group", "gender", "bmi_3cat")) {
  study %>%
    count(i)
}
study %>%
  count(study[["age_group"]])
study %>%
  count(.data[["age_group"]])
# Create the data frame structure that will contain our results
cat_table <- tibble(
  variable = vector("character"),
  category = vector("character"),
  n
  = vector("numeric")
)
# For each column, get the column name, category names, and count.
# Then, add them to the bottom of the results data frame we created above.
for(i in c("age_group", "gender", "bmi_3cat")) {
  cat_stats <- study %>%
    count(.data[[i]]) %>% # Use .data to refer to the current data frame.
    mutate(variable = names(.)[1]) %>% # Use . to refer to the current data frame.
    rename(category = 1)
  # Here is where we update cat_table with the results for each column
  cat_table <- bind_rows(cat_table, cat_stats)
}
cat_table
cat_table%>%
  group_by(variable)%>%
  mutate(
    percent= n /sum(n)* 100)
cat_stats<-function(data,...){
  #Create the data framestructurethatwillcontainourresults
  cat_table <-tibble(
    variable= vector("character"),
    category= vector("character"),
    n = vector("numeric")
  )
  #Foreachcolumn in...,getthecolumnname,categorynames,andcount.
  # Then, add them to the bottom of the results data frame we created above.
  for(i in c(...)) {
    stats <- data %>%
      count(.data[[i]]) %>% # Use .data to refer to the current data frame.
      mutate(variable = names(.)[1]) %>% # Use . to refer to the current data frame.
      rename(category = 1)
    # Here is where we update cat_table with the results for each column
    cat_table <- bind_rows(cat_table, stats)
  }
  # Return results
  cat_table
}
cat_stats(study, "age_group", "gender", "bmi_3cat")


#USING THE PURR PACKAGE
library(purrr)
set.seed(123)
df_xyz <- tibble(
  x = rnorm(10),
  y = rnorm(10),
  z = rnorm(10)
) %>%
  print()
library(dplyr)
df_xyz %>%
  summarise(
    across(
      .cols = everything(),
      .fns
      = mean,
      .names = "{col}_mean"))
xyz_means <- vector("double", ncol(df_xyz))
for(i in seq_along(df_xyz)) {
  xyz_means[[i]] <- mean(df_xyz[[i]])
}
xyz_means
xyz_means <- map_dbl(
  .x = df_xyz,
  .f = mean)
xyz_means
names <- c("Avril", "Joe", "Whitney")
n_letters <- vector("double", length(names)) # Expecting double
for(i in seq_along(names)) {
  n_letters[[i]] <- stringr::str_extract(names[[i]], "\\w") # Returns character
}
n_letters
names <- c("Avril", "Joe", "Whitney")
n_letters <- map_dbl(
  .x = names,
  .f = stringr::str_extract, "\\w{1}")
#using purrr for data transfer
library(readxl)
houston <-import_cities("Houston")
atlanta <-import_cities("Atlanta")
charlotte<-import_cities("Charlotte")
readxl::read_excel("C:/Users/Immah/Downloads/city_ses.xlsx")
getwd()
find("import_cities")
import_cities
import_cities <- function(sheet) {
  readxl::read_excel(
    "C:/Users/Immah/Downloads/city_ses.xlsx",
    sheet = sheet
  )
}
houston <-import_cities("Houston")
atlanta <-import_cities("Atlanta")
charlotte<-import_cities("Charlotte")
path<-"city_ses.xlsx"
sheets<-excel_sheets(path)
for(i in seq_along(sheets)){
  new_nm <-tolower(sheets[[i]])
  assign(new_nm,read_excel(path, sheet= sheets[[i]]))
}
path<-"city_ses.xlsx"
walk(
  .x =excel_sheets(path),
  .f =function(x){
    new_nm <-tolower(x)
    assign(new_nm, read_excel(path, sheet=x), envir= .GlobalEnv)
  })
houston
atlanta
charlotte
list_of_df <- map(
  .x = excel_sheets(path),
  .f = ~ read_excel(path, sheet = .x))
str(list_of_df)
houston<-list_of_df[[1]]
houston
atlanta<-list_of_df[[2]]
atlanta
charlotte<-list_of_df[[2]]
charlotte
bind_rows(list_of_df)
cities<-map_dfr(
  .x =excel_sheets(path),
  .f =~ read_excel(path, sheet= .x))
cities
returned_by_walk <- walk(
  .x = excel_sheets(path),
  .f = function(x) {
    new_nm <- tolower(x)
    assign(new_nm, read_excel(path, sheet = x), envir = .GlobalEnv)
  })
returned_by_walk
path<-"city_ses.xlsx"
walk(
  .x =excel_sheets(path),
  .f =~ assign(tolower(.),read_excel(path, sheet= .),envir=.GlobalEnv))
houston
atlanta
charlotte
#using purrr for data management
set.seed(123)
df_xyz <- tibble(
  x = rnorm(10),
  y = rnorm(10),
  z = rnorm(10)
) %>%
  print()
df_xyz$x[2] <- NA_real_
df_xyz$y[4] <- NA_real_
df_xyz$z[6] <- NA_real_
df_xyz
add_na_at <- function(vect, pos) {
  vect[[pos]] <- NA
}
add_na_at(df_xyz$x, 2) %>% print()
add_na_at <- function(vect, pos) {
  vect[[pos]] <- NA
  vect
}
add_na_at(df_xyz$x, 2)
df_xyz %>%
  mutate(
    x = add_na_at(x, 2),
    y = add_na_at(y, 4),
    z = add_na_at(z, 6))
add_na_at(df_xyz$x)
map(
  .x = df_xyz,
  .f = add_na_at, 2)
map(
  .x =df_xyz,
  .f =~ add_na_at(.x, 2))
map(
  .x =df_xyz,
  .f =~ add_na_at(.x, 2)
)%>%
  as.data.frame()
map_dfc(
  .x = df_xyz,
  .f = ~ add_na_at(.x, 2))
map2_dfc(
  .x = df_xyz,
  .y = c(2, 4, 6),
  .f = ~ add_na_at(.x, .y))
set.seed(8142020)
map2_dfc(
  .x = df_xyz,
  .y = sample(1:10, 3, TRUE),
  .f = ~ add_na_at(.x, .y))
add_na_at<-function(vect,pos){
  vect[[pos]]<-NA
  vect
}
map2_dfc(
  .x =df_xyz,
  .y =c(2, 4, 6),
  .f =function(vect,pos){
    vect[[pos]]<-NA
    vect
  })
map2_dfc(
  .x = df_xyz,
  .y = c(2, 4, 6),
  .f = ~ {
    .x[[.y]] <- NA
    .x
  })
is_match<-function(value_1,value_2){
  result <-value_1== value_2
  result <-if_else(is.na(result), FALSE,result)
  result
}
people%>%
  mutate(
    name_first_match= is_match(name_first_1,name_first_2),
    name_last_match = is_match(name_last_1,name_last_2),
    street_match = is_match(street_1,street_2)
  ) %>%
  #Orderlike columnsnextto eachother foreasiercomparison
  select(id_1, starts_with("name_f"), starts_with("name_l"), starts_with("s"))
map_dfc(
  .x =c("name_first", "name_last","street"),
  .f =function(col,data= people){
    col_1<-paste0(col,"_1")
    col_2<-paste0(col,"_2")
    new_nm <-paste0(col, "_match")
    data[[new_nm]]<-data[[col_1]]== data[[col_2]]
    data[[new_nm]]<-if_else(is.na(data[[new_nm]]), FALSE,data[[new_nm]])
    data[c(col_1,col_2,new_nm)]
  })
#using purrr for analysis
study<-tibble(
  age =c(32, 30,32,29,24,38,25,24, 48, 29, 22, 29, 24, 28,24,25,
         25, 22,25,24,25,24,23,24, 31, 24, 29, 24, 22, 23,26,23,
         24, 25,24,33,27,25,26,26, 26, 26, 26, 27, 24, 43,25,24,
         27, 28,29,24,26,28,25,24, 26, 24, 26, 31, 24, 26,31,34,
         26, 25,27,NA),
  age_group =c(2, 2, 2,1,1, 2, 1, 1,2,1, 1, 1, 1,1,1,1, 1, 1, 1,1,1,
               1, 1, 1,2,1, 1, 1, 1,1,1, 1, 1, 1,1,2,1, 1, 1, 1,1,1,
               1, 1, 1,2,1, 1, 1, 1,1,1, 1, 1, 1,1,1,1, 1, 2, 1,1,2,
               2, 1, 1,1,NA),
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
         19.13, 21.48, 22.59, 24.96, 21.63, NA, 29.35, 21.76, 17.97,
         22.31, 19.27, 24.07, 22.76, NA),
  bmi_3cat = c(3, 1, 2, 3, 1, 2, 1, 1, 2, 2, 2, 3, 1, 1, 1, 1, 1, 1, 2, 1, 1,
               1, 1, 3, 2, 1, 2, 2, 1, 2, 1, 2, 1, 1, NA, 2, 2, 2, 1, 1, 1, 1,
               1, 1, 1, 1, 1, 1, 1, 2, NA, 3, 1, 1, 1, 1, 1, 1, 1, NA, 2, 1,
               1, 1, 1, 1, 1, NA)
) %>%
  mutate(
    age_group = factor(age_group, labels = c("Younger than 30", "30 and Older")),
    gender
    = factor(gender, labels = c("Female", "Male")),
    bmi_3cat = factor(bmi_3cat, labels = c("Normal", "Overweight", "Obese"))
  ) %>%
  print()
continuous_stats <- function(var) {
  study %>%
    summarise(
      n_miss = sum(is.na({{ var }})),
      mean = mean({{ var }}, na.rm = TRUE),
      median = median({{ var }}, na.rm = TRUE),
      min = min({{ var }}, na.rm = TRUE),
      max = max({{ var }}, na.rm = TRUE)
    )
  }
continuous_stats(age)
continuous_stats(ht_in)
continuous_stats(wt_lbs)
continuous_stats(bmi)
map_dfr(
  .x =quos(age,ht_in,wt_lbs,bmi),
  .f =continuous_stats)
continuous_stats<-function(var){
  study %>%
    summarise(
      variable =quo_name(var), #Addvariablenametotheoutput
      n_miss =sum(is.na({{var}})),
      mean =mean({{var}},na.rm= TRUE),
      median =median({{var}}, na.rm=TRUE),
      min =min({{var}},na.rm=TRUE),
      max =max({{var}},na.rm=TRUE))
}
map_dfr(
  .x = quos(age, ht_in, wt_lbs, bmi),
  .f = continuous_stats)
continuous_stats <- function(data, var) {
  data %>% # Don't forget to replace "study" with "data" here too!
    summarise(
      variable = quo_name(var),
      n_miss = sum(is.na({{ var}} )),
      mean
      = mean({{ var }}, na.rm = TRUE),
      median = median({{ var }}, na.rm = TRUE),
      min = min({{ var }}, na.rm = TRUE),
 max = max({{ var }}, na.rm = TRUE))
}
map_dfr(
  .x = quos(age, ht_in, wt_lbs, bmi),
  .f = continuous_stats, data = study)
map_dfr(
  .x = quos(x, y, z),
  .f = continuous_stats, data = df_xyz)
#categorical statistics
# Structure 1. An object to contain the results.
# Create the data frame structure that will contain our results
cat_table <- tibble(
  variable = vector("character"),
  category = vector("character"),
  n = vector("numeric")
)
# Structure 2. The actual for loop.
# For each column, get the column name, category names, and count.
# Then, add them to the bottom of the results data frame we created above.
for(i in c("age_group", "gender", "bmi_3cat")) {
  cat_stats <- study %>%
    count(.data[[i]]) %>% # Use .data to refer to the current data frame.
    mutate(variable = names(.)[1]) %>% # Use . to refer to the current data frame.
    rename(category = 1)
  # Here is where we update cat_table with the results for each column
  cat_table <- bind_rows(cat_table, cat_stats)
}
cat_table
map_dfr(
  .x = c("age_group", "gender", "bmi_3cat"),
  .f = function(x) {
    study %>%
      count(.data[[x]]) %>%
      mutate(variable = names(.)[1]) %>%
      rename(category = 1) %>%
      select(variable,category,n)
  }
)
map_dfr(
  .x =quos(age_group,gender, bmi_3cat), #Changec()toquos()
  .f =function(x){
    study%>%
      count({{x}}) %>%#Change.data[[x]]to{{x}}
      mutate(variable=names(.)[1]) %>%
      rename(category=1) %>%
      select(variable,category,n)
  }
)
map_dfr(
  .x =quos(age_group,gender, bmi_3cat),
  .f =function(x,data= study){
    data%>%# Don't forgettoreplace"study"with"data"heretoo!
      count({{x}}) %>%
      mutate(variable=names(.)[1]) %>%
      rename(category=1) %>%
      select(variable,category,n)
  }
)
