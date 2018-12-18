
# Test to confirm pp)_backcalc yields expected answers
popsizes <- read.csv("../exploratory/popsizes_default.csv", header = FALSE)
lifeparts <- read.csv("../exploratory/lifeparts_default.csv", header = FALSE)
rowages <- 13:18
colages <- 11:17
returnages <- 13:18
temp <- ppy_backcalc(popsizes, lifeparts, rowages, colages, 13:18)

popsizes <- AgeByDebutAge_num_f[1,,,1]
lifeparts <- AgeByDebutAge_lp_f[1,,,1]
rowages <- 13:18
ppy_f

ppy_f <- ppy_backcalc(popsizes, lifeparts, rowages, colages, 13:18)

