#POWER
# Number of polarized cells
wt_polarised <- 10
ko_polarised <- 14

# Total cells counted
wt_total <- 10 + 31
ko_total <- 14 + 21
wt_prop <- wt_polarised / wt_total
ko_prop <- ko_polarised / ko_total
wt_prop
ko_prop
power.prop.test(
  p1 = wt_prop,
  p2 = ko_prop,
  power = 0.80,
  sig.level = 0.05
)

#2. MICE WEIGHT - t-tes
weights <- c(27.2, 25.5, 26.0, 29.1)
mean(weights)
sd(weights)
difference <- mean(weights) * 0.10
difference
power.t.test(
  delta = difference,
  sd = sd(weights),
  power = 0.80,
  sig.level = 0.05,
  type = "two.sample"
)
spider_data <- read.csv("~/R_Stats_Data/spider.data.csv")
head(spider_data)
str(spider_data)
summary(spider_data)
library(dplyr)
spider_data %>%
  group_by(Group) %>%
  summarise(
    Mean = mean(Anxiety),
    SD = sd(Anxiety),
    n = n()
  )
names(spider_data)
library(dplyr)
spider_data %>%
  group_by(Group) %>%
  summarise(
    Mean = mean(Scores),
    SD = sd(Scores),
    n = n()
  )
mean_picture <- mean(spider_data$Scores[spider_data$Group == "Picture"])
mean_spider  <- mean(spider_data$Scores[spider_data$Group == "Spider"])
delta <- abs(mean_spider - mean_picture)
delta
sd_pooled <- spider_data %>%
  group_by(Group) %>%
  summarise(SD = sd(Scores)) %>%
  summarise(SD = mean(SD)) %>%
  pull(SD)
sd_pooled
power.t.test(
  delta = delta,
  sd = sd_pooled,
  power = 0.80,
  sig.level = 0.05,
  type = "two.sample"
)
spider_data$Group
unique(spider_data$Group)
mean_picture <- mean(spider_data$Scores[spider_data$Group == "Picture"])
mean_real <- mean(spider_data$Scores[spider_data$Group == "Real"])
delta <- abs(mean_real - mean_picture)
delta
library(tidyr)
library(dplyr)
spider_wide <- spider_data %>%
  pivot_wider(
    names_from = Group,
    values_from = Scores
  )
spider_wide
spider_wide <- spider_wide %>%
  mutate(Difference = Spider - Picture)
spider_wide
mean_diff <- mean(spider_wide$Difference)
sd_diff <- sd(spider_wide$Difference)

mean_diff
sd_diff
spider_wide <- spider_wide %>%
  mutate(Difference = Real - Picture)
names(spider_wide)
mean_diff <- mean(spider_wide$Difference)
sd_diff <- sd(spider_wide$Difference)
mean_diff
sd_diff
power.t.test(
  delta = mean_diff,
  sd = sd_diff,
  power = 0.80,
  sig.level = 0.05,
  type = "paired"
)
#2. working memory
library(tidyverse)
working.memory <- read_csv("working.memory.csv")
str(working.memory)
wd("C:set/Users/Immah/Documents/R_Stats_Data")  # adjust path to where the folder actually is
getwd()
list.files()
working.memory$difference <- working.memory$DA.depletion - working.memory$placebo
working.memory <- working.memory %>%
  mutate(difference = DA.depletion - placebo)
head(working.memory)
ggplot(working.memory, aes(x = "", y = difference)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1, color = "red") +
  labs(x = "", y = "Difference (DA depletion - placebo)", title = "Working memory: treatment effect") +
  theme_minimal()
ggplot(working.memory, aes(x = difference)) +
  geom_histogram(bins = 6, color = "black", fill = "purple")
ggplot(working.memory, aes(sample = difference)) +
  stat_qq() +
  stat_qq_line()
shapiro.test(working.memory$difference) #shapiro test
t.test(working.memory$difference ~ 1, mu = 0)
working.memory.long <- working.memory %>%
  select(Subject, placebo, DA.depletion) %>%
  pivot_longer(cols = c(placebo, DA.depletion), names_to = "treatment", values_to = "score") %>%
  arrange(Subject)
head(working.memory.long, 10)
t.test(score ~ treatment, data = working.memory.long, paired = TRUE)
t.test(working.memory$placebo, working.memory$DA.depletion, paired = TRUE)
library(ggpubr)
ggpaired(working.memory.long, x = "treatment", y = "score", id = "Subject",
         color = "treatment", line.color = "gray", line.size = 0.4) +
  stat_compare_means(paired = TRUE, method = "t.test") +
  labs(y = "Working memory score", x = "")
coffee <- read_csv("coffee.bean.species.csv")
str(coffee)
table(coffee$Species)
ggplot(coffee, aes(x = Species, y = Scores.Total)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.6) +
  labs(y = "Total score", title = "Coffee bean rating by species") +
  theme_minimal()
coffee %>%
  group_by(Species) %>%
  summarise(shapiro_p = shapiro.test(Scores.Total)[["p.value"]]) #normality
library(car)
leveneTest(Scores.Total ~ Species, data = coffee)
t.test(Scores.Total ~ Species, data = coffee, var.equal = TRUE)
library(ggpubr)
ggplot(coffee, aes(x = Species, y = Scores.Total, color = Species)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.6) +
  stat_compare_means(method = "t.test") +
  labs(y = "Total score", title = "Coffee bean rating: Arabica vs Robusta") +
  theme_minimal()


#3. ANOVA
library(dplyr)
library(readr)
getwd()
list.files()
setwd("C:/Users/Immah/Documents/R_Stats_Data")
crop <- read_csv("crop.yield.csv") %>%
  mutate(fertilizer = as.factor(fertilizer))
str(crop)
table(crop$fertilizer)
library(ggplot2)
ggplot(crop, aes(x = fertilizer, y = yield)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.4) +
  labs(x = "Fertilizer type", y = "Crop yield", title = "Crop yield by fertilizer type") +
  theme_minimal()
fit.crop <- lm(yield ~ fertilizer, data = crop)
shapiro.test(residuals(fit.crop))
leveneTest(yield ~ fertilizer, data = crop)
leveneTest()
library(car)
fit.crop <- lm(yield ~ fertilizer, data = crop)
shapiro.test(residuals(fit.crop))
leveneTest(yield ~ fertilizer, data = crop)
library(rstatix)
res.aov <- crop %>% anova_test(yield ~ fertilizer)
get_anova_table(res.aov)
crop %>% pairwise_t_test(yield ~ fertilizer, p.adjust.method = "bonferroni")
crop %>%
  group_by(fertilizer) %>%
  summarise(mean_yield = mean(yield), sd_yield = sd(yield))
crop %>%
  group_by(fertilizer) %>%
  summarise(mean_yield = round(mean(yield), 3), sd_yield = round(sd(yield), 3))
options(pillar.sigfig = 7)
crop %>%
  group_by(fertilizer) %>%
  summarise(mean_yield = mean(yield), sd_yield = sd(yield))
tapply(crop$yield, crop$fertilizer, mean)
#ANOVA NEUTROPHILS
neutrophils <- read_csv("neutrophils.long.csv") %>%
  mutate(Condition = as.factor(Condition))
str(neutrophils)
table(neutrophils$Experiment, neutrophils$Condition)
neutrophils <- neutrophils %>%
  mutate(Condition = factor(Condition, levels = c("WT", "KO", "KO+T1", "KO+T2")))

ggplot(neutrophils, aes(x = Condition, y = Values, group = Experiment, color = Experiment)) +
  geom_point(size = 2) +
  geom_line() +
  labs(x = "Condition", y = "Neutrophil count", title = "Neutrophil counts across conditions by experiment") +
  theme_minimal()
neutrophils %>%
  group_by(Condition) %>%
  shapiro_test(Values)
res.aov <- neutrophils %>%
  anova_test(dv = Values, wid = Experiment, within = Condition)
get_anova_table(res.aov)
neutrophils %>%
  pairwise_t_test(Values ~ Condition, paired = TRUE, p.adjust.method = "bonferroni")
neutrophils %>%
  pairwise_t_test(Values ~ Condition, paired = TRUE, p.adjust.method = "bonferroni")
library(dplyr)
library(car)
library(tidyverse)
library(rstatix)
library(car)
neutrophils <- read_csv("neutrophils.long.csv") %>%
  mutate(Condition = factor(Condition, levels = c("WT", "KO", "KO+T1", "KO+T2")))
neutrophils %>%
  pairwise_t_test(Values ~ Condition, paired = TRUE, p.adjust.method = "bonferroni")
neutrophils %>%
  group_by(Condition) %>%
  summarise(mean = mean(Values), sd = sd(Values))
library(ggpubr)

ggplot(neutrophils, aes(x = Condition, y = Values)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.05, aes(color = Experiment)) +
  stat_compare_means(comparisons = list(c("WT","KO"), c("KO","KO+T1"), c("KO","KO+T2"), c("KO+T1","KO+T2")),
                     method = "t.test", paired = TRUE) +
  labs(x = "Condition", y = "Neutrophil count", title = "Neutrophil counts by condition") +
  theme_minimal()


#4. CORRELATION - RELATIONSHIP BETWEEN PARASITE BURDEN AND BODY MASS
deer <- read_csv("Roe.deer.csv")
str(deer)
table(deer$sex)
ggplot(deer, aes(x = BM, y = PL, color = sex)) +
  geom_point() +
  labs(x = "Body mass (BM)", y = "Parasite load (PL)", title = "Roe deer: body mass vs parasite load") +
  theme_minimal()
deer <- read_csv("Roe.deer.csv")
str(deer)
table(deer$sex)
ggplot(deer, aes(x = BM, y = PL, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Body mass (BM)", y = "Parasite load (PL)", title = "Roe deer: body mass vs parasite load") +
  theme_minimal()
fit.male <- deer %>% filter(sex == "M") %>% lm(PL ~ BM, data = .)
fit.female <- deer %>% filter(sex == "F") %>% lm(PL ~ BM, data = .)
summary(fit.male)
summary(fit.female)
male_coef <- coefficients(fit.male)
female_coef <- coefficients(fit.female)
ggplot(deer, aes(x = BM, y = PL, color = sex)) + #lines of best fit
  geom_point() +
  geom_abline(intercept = male_coef[1], slope = male_coef[2], color = "blue") +
  geom_abline(intercept = female_coef[1], slope = female_coef[2], color = "red") +
  labs(x = "Body mass (BM)", y = "Parasite load (PL)", title = "Roe deer: BM vs PL by sex") +
  theme_minimal()
par(mfrow = c(2, 2)) #checking assumptions
plot(fit.male)
par(mfrow = c(2, 2))
plot(fit.female)
deer %>%
  group_by(sex) %>%
  cor_test(BM, PL)
exam <- read_csv("exam.anxiety.csv")
str(exam)
table(exam$Gender)
ggplot(exam, aes(x = Revise, y = Anxiety, color = Gender)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Revision time", y = "Exam anxiety", title = "Revision time vs exam anxiety") +
  theme_minimal()
fit.male <- exam %>% filter(Gender == "Male") %>% lm(Anxiety ~ Revise, data = .)
fit.female <- exam %>% filter(Gender == "Female") %>% lm(Anxiety ~ Revise, data = .)
summary(fit.male)
summary(fit.female)
male_coef <- coefficients(fit.male) #lines of best fit
female_coef <- coefficients(fit.female)
ggplot(exam, aes(x = Revise, y = Anxiety, color = Gender)) +
  geom_point() +
  geom_abline(intercept = male_coef[1], slope = male_coef[2], color = "blue") +
  geom_abline(intercept = female_coef[1], slope = female_coef[2], color = "red") +
  labs(x = "Revision time", y = "Exam anxiety", title = "Revision time vs exam anxiety by gender") +
  theme_minimal()
par(mfrow = c(2, 2)) #check for assumptions
plot(fit.male)
par(mfrow = c(2, 2))
plot(fit.female)
male_data <- exam %>% filter(Gender == "Male") #standardised residuals
male_data <- male_data %>%
  add_column(std.resid = rstandard(fit.male),
             cooks.d = cooks.distance(fit.male))
male_data %>% arrange(desc(abs(std.resid))) %>% head(6)
male_data_clean <- male_data %>% filter(std.resid > -2 & std.resid < 2)
fit.male.clean <- lm(Anxiety ~ Revise, data = male_data_clean)
summary(fit.male.clean)
female_data <- exam %>% filter(Gender == "Female") %>%
  add_column(std.resid = rstandard(fit.female), cooks.d = cooks.distance(fit.female))
exam_clean <- bind_rows(male_data_clean, female_data)
male_coef_clean <- coefficients(fit.male.clean)
female_coef <- coefficients(fit.female)
ggplot(exam_clean, aes(x = Revise, y = Anxiety, color = Gender)) +
  geom_point() +
  geom_abline(intercept = male_coef_clean[1], slope = male_coef_clean[2], color = "blue") +
  geom_abline(intercept = female_coef[1], slope = female_coef[2], color = "red") +
  labs(x = "Revision time", y = "Exam anxiety", title = "Revision vs anxiety (outlier removed)") +
  theme_minimal()


#5.NON PARAMETRIC TESTS
tshirts <- read_csv("smelly.teeshirt.csv")
str(tshirts)
table(tshirts$university)
ggplot(tshirts, aes(x = university, y = smell)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(x = "T-shirt university", y = "Disgust rating (1-7)", title = "Disgust rating by T-shirt origin") +
  theme_minimal()
wilcox.test(smell ~ university, data = tshirts)
library(ggpubr)
#WILCOXSON TEST
ggplot(tshirts, aes(x = university, y = smell)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  stat_compare_means(method = "wilcox.test") +
  labs(x = "T-shirt university", y = "Disgust rating (1-7)", title = "Disgust rating by T-shirt origin") +
  theme_minimal()
botulinum <- read_csv("botulinum.long.csv")
str(botulinum)
table(botulinum$treatment)
botulinum_wide <- botulinum %>%
  pivot_wider(names_from = treatment, values_from = scores) %>%
  mutate(difference = after - before)
botulinum_wide
ggplot(botulinum_wide, aes(x = "", y = difference)) +
  geom_jitter(width = 0.05, height = 0) +
  stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1, color = "red") +
  labs(x = "", y = "Difference (after - before)", title = "Botulinum: change in spasticity") +
  theme_minimal()
wilcox.test(botulinum_wide$before, botulinum_wide$after, paired = TRUE)
#CREATINE KRUSKAL WALLIS
creatine <- read_csv("creatine.csv")
str(creatine)
table(creatine$creatine)
ggplot(creatine, aes(x = creatine, y = gain)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(x = "Creatine group", y = "Weight gain", title = "Weight gain by creatine dosing group") +
  theme_minimal()
kruskal_test(gain ~ creatine, data = creatine)
dunn_test(gain ~ creatine, data = creatine)
#FRIEDMANN TEST
violin <- read_csv("violin.csv")
str(violin)
table(violin$Rater, violin$Violin)
ggplot(violin, aes(x = Violin, y = Score)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(x = "Violin", y = "Rating (1-10)", title = "Violin ratings by 10 violinists") +
  theme_minimal()
friedman_test(Score ~ Violin | Rater, data = violin)
pairwise.wilcox.test(violin$Score, violin$Violin, paired = TRUE, p.adjust.method = "bonferroni")
violin %>%
  group_by(Violin) %>%
  summarise(median = median(Score), mean = mean(Score))
#DOMINANCE  SPEARMAN RANK
dominance <- read_csv("Dominance.csv")
str(dominance)
dominance
ggplot(dominance, aes(x = rank, y = eggs.per.gram)) +
  geom_point(size = 3) +
  labs(x = "Social dominance rank (1 = most dominant)", y = "Nematode eggs per gram", title = "Dominance vs parasitism in colobus monkeys") +
  theme_minimal()
dominance %>% cor_test(rank, eggs.per.gram, method = "spearman")


#6. QUALITATIVE DATA
setwd("C:/Users/Immah/Documents/R_Stats_Data")
library(readr)
dogs <- read_csv("dogs.csv")
dogs
library(dplyr)
dogs <- dogs %>% mutate(Total = No + Yes)
dogs.long <- dogs %>%
  pivot_longer(cols = 2:4, names_to = "Dance", values_to = "count")
dogs.long
dogs.long <- dogs.long %>%
  group_by(Training) %>%
  mutate(fraction = count / count[Dance == "Total"]) %>%
  filter(Dance != "Total") %>%
  ungroup()
dogs.long
ggplot(dogs.long, aes(x = Training, y = fraction, fill = Dance)) +
  geom_col(colour = "black") +
  scale_fill_brewer(palette = 1) +
  labs(x = "Training reward", y = "Proportion", title = "Dogs: dancing by reward type") +
  theme_minimal()
dogs_matrix <- dogs %>% #chi-square
  select(Training, No, Yes) %>%
  column_to_rownames("Training") %>%
  as.matrix()
dogs_matrix
fisher.test(dogs_matrix)
toads <- read_csv("cane.toad.csv")
toads
toads.long <- toads %>%
  filter(Infection != "Total") %>%
  pivot_longer(cols = -Infection, names_to = "Area", values_to = "count") %>%
  group_by(Area) %>%
  mutate(fraction = count / sum(count)) %>%
  ungroup()
toads.long
ggplot(toads.long, aes(x = Area, y = fraction, fill = Infection)) +
  geom_col(colour = "black") +
  scale_fill_brewer(palette = 1) +
  labs(x = "Area", y = "Proportion", title = "Cane toad intestinal parasite infection by area") +
  theme_minimal()
toads_matrix <- toads %>%
  filter(Infection != "Total") %>%
  column_to_rownames("Infection") %>%
  as.matrix()
toads_matrix
fisher.test(toads_matrix)
pairwise_fisher_test(toads_matrix)


#7 t-tes, chi-square, regression
iris.species <- read_csv("iris.species.csv")
str(iris.species)
table(iris.species$Species)
ggplot(iris.species, aes(x = Species, y = Petal.Length)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(y = "Petal length", title = "Petal length by species") +
  theme_minimal()
iris.species %>%
  group_by(Species) %>%
  shapiro_test(Petal.Length)
leveneTest(Petal.Length ~ Species, data = iris.species)
t.test(Petal.Length ~ Species, data = iris.species)
iris.sepal <- read_csv("iris.sepal.csv")
str(iris.sepal)
ggplot(iris.sepal, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Sepal length", y = "Petal length", title = "Virginica: sepal length vs petal length") +
  theme_minimal()
cor_test(iris.sepal, Sepal.Length, Petal.Length)
recycling <- read_csv("recycling.csv") #chi-square test of independence
recycling
recycling_matrix <- recycling %>%
  column_to_rownames("Intervention") %>%
  as.matrix()
recycling_matrix
chisq.test(recycling_matrix)
pairwise_fisher_test(recycling_matrix)
#SIMPLE LINEAR REGRESSION
income <- read_csv("income.data.csv")
str(income)
ggplot(income, aes(x = income, y = happiness)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Income (10k units)", y = "Happiness (1-10)", title = "Income vs happiness") +
  theme_minimal()
fit.income <- lm(happiness ~ income, data = income)
summary(fit.income)
predict(fit.income, newdata = data.frame(income = 5))
#ANOVA
coffee.country <- read_csv("coffee.country.csv") %>%
  mutate(Country = as.factor(Country))
str(coffee.country)
table(coffee.country$Country)
ggplot(coffee.country, aes(x = Country, y = Score)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(y = "Score", title = "Coffee bean score by country") +
  theme_minimal()
fit.coffee <- lm(Score ~ Country, data = coffee.country)
shapiro.test(residuals(fit.coffee))
leveneTest(Score ~ Country, data = coffee.country)
kruskal_test(Score ~ Country, data = coffee.country)
dunn_test(Score ~ Country, data = coffee.country)
coffee.country %>%
  group_by(Country) %>%
  summarise(median = median(Score), mean = mean(Score))
