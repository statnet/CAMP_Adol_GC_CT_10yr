plot(schoolpops, ylim=c(min(schoolpops$totschoolpop)*0.9,max(schoolpops$totschoolpop*1.1)), type='b')
abline(h=mean(schoolpops$totschoolpop))
(max(schoolpops$totschoolpop) - min(schoolpops$totschoolpop))/mean(schoolpops$totschoolpop)

summary(lm(totschoolpop~year, data=schoolpops))


# Males are 50.7% of the population according to the weights
sum(wts_m) / (sum(wts_m) + sum(wts_f))

# No clear, monotonic pattern by age
plot(apply(wts_m, 2, sum) / ( apply(wts_m, 2, sum) + apply(wts_f, 2, sum) ))
abline(h=0.507)

# No clear, monotonic pattern by year
plot(apply(wts_m, 3, sum) / ( apply(wts_m, 3, sum) + apply(wts_f, 3, sum) ))
abline(h=0.507)





#### OLD
abspopsizes_f <- wts_f
for (i in 1:nyears) {
  abspopsizes_f[,,i] <- abspopsizes_f[,,i]* 
    unname(unlist(
      (schoolpops %>% filter(year==years[i]) %>% select("totschoolpop"))
    )) / 
    sum(wts_f[,,i])
}

abspopsizes_m <- wts_m
for (i in 1:nyears) {
  abspopsizes_m[,,i] <- abspopsizes_m[,,i]* 
    unname(unlist(
      (schoolpops %>% filter(year==years[i]) %>% select("totschoolpop"))
    )) / 
    sum(wts_m[,,i])
}

