31 * 78
697 / 41
x <- 39; y <- 22; z <- x / y
z
log2(sqrt(2345))
my_name <- "Marcy"
nchar(my_name)
substr(my_name, 1, 1)
some_numbers <- c(2, 5, 8, 12, 16)
number_range <- 5:9
some_numbers - number_range
number_series <- seq(2, by = 3, length.out = 100)
number_series <- number_series * 1000
rep(c("WT","KO1","KO2","KO3"), each = 25)
normal_numbers <- rnorm(20)
t.test(normal_numbers)
library(tidyverse)
setwd("C:/Users/Immah/Documents/R_tidyverse_intro_data")
getwd()
list.files()
small_data <- read_delim("small_file.txt")
small_data
variants <- read_delim("Child_Variants.csv")
variants
median(log2(small_data$Length))
mean(variants$MutantReadPercent)
sd(variants$MutantReadPercent)
# small_file data
filter(small_data, Category == "A")
filter(small_data, Length > 80)
select(small_data, Length, Category)

# child variants data
filter(variants, CHR == "MT")%>%
filter(variants, MutantReadPercent >= 70)
filter(variants, QUAL == 200)
filter(variants, GENE == "IGFN1")
select(variants, -ENST, -dbSNP)
nrow(filter(small_data, Category == "A"))
nrow(filter(variants, CHR == "MT"))
nrow(filter(variants, GENE == "IGFN1"))
# Quality filtering — all 3 conditions true at once
filter(variants, QUAL == 200, COVERAGE > 50, MutantReadPercent > 70)
# Positional filtering — remove X, Y, MT chromosomes
filter(variants, !CHR %in% c("X", "Y", "MT"))
# Annotation filtering — chrom + position, only where dbSNP is valid (not ".")
variants %>%
  filter(dbSNP != ".") %>%
  select(CHR, POS)
filter(variants, nchar(REF) > 1)              # deletions
filter(variants, substr(GENE, 1, 1) == "Q")   # genes starting with Q
brain_data <- read_delim("brain_bodyweight.txt")
brain_data
ggplot(brain_data, aes(x = log.brain, y = log.body)) +
  geom_point(colour = "blue2", size = 3)
ggplot(brain_data, aes(x = log.brain, y = log.body, colour = Category)) +
  geom_point(size = 3)
ggplot(brain_data, aes(x = log.brain, y = log.body, colour = Category)) +
  geom_point(size = 3) +
  ggtitle("Brain weight vs body weight") +
  xlab("log(brain weight)") +
  ylab("log(body weight)")
ggplot(brain_data, aes(x = log.brain, y = log.body, colour = Category)) +
  geom_point(size = 3) +
  geom_text(aes(label = Species), hjust = -0.1)
cor.test(brain_data$log.brain, brain_data$log.body)
model <- lm(log.body ~ log.brain, data = brain_data)
model
ggplot(brain_data, aes(x = log.brain, y = log.body, colour = Category)) +
  geom_point(size = 3) +
  geom_abline(intercept = coef(model)[1], slope = coef(model)[2])
ggplot(brain_data, aes(x = log.brain, y = log.body, colour = Category)) +
  geom_point(size = 3) +
  geom_abline(intercept = -2.283, slope = 1.215) +
  ggtitle("Brain vs body weight (p = 2.44e-06)") +
  xlab("log(brain weight)") +
  ylab("log(body weight)")
ggplot(brain_data, aes(x = log.brain, y = log.body, colour = Category)) +
  geom_point(size = 3) +
  geom_abline(intercept = -2.283, slope = 1.215) +
  ggtitle("Brain vs body weight (p = 2.44e-06)") +
  xlab("log(brain weight)") +
  ylab("log(body weight)")
