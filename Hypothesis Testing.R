
# One-sample t-test
library(MASS)
chem
View(chem)
help (t.test)
#H0 : mu = 1
#Ha : mu > 1
t.test(MASS::chem, mu = 1, alternative = "greater", conf.level = 0.95)
View(cats)

#B) two sample t-test
malecats <- subset(MASS::cats, MASS::cats$Sex == "M")
malecats
femalecats <- subset(MASS::cats, MASS::cats$Sex == "F")

t.test(malecats$Bwt, femalecats$Bwt, var.equal = F)

View(shoes)

#C) Paired t test
# Ho: A wear is not better than B
# Ha: A wear is better than B

t.test(MASS::shoes$A, MASS::shoes$B,paired = TRUE)


#proportional test

View(bacteria)
summary(bacteria)
help ("prop.test")

prop.test(x = c(84,93), n = c(96,124), correct = T, alternative = "two.sided")

#test of variance

malecats <- subset(MASS::cats, MASS::cats$Sex == "M")
malecats
femalecats <- subset(MASS::cats, MASS::cats$Sex == "F")
femalecats
help("var.test")
var.test(malecats$Bwt,femalecats$Bwt)
