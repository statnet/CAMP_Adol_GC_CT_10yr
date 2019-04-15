library(EasyABC)


my_priors=list(c("unif", 0, 1), c("unif", 0, 2))

my_model <- function(x) {
    return(sum(x))
}

tolerance=c(1, 0.5)
my_ABC<-ABC_sequential(method="Beaumont",
                              model=my_model,
                              prior=my_priors,
                              nb_simul=100,
                              summary_stat_target=2.4,
                              tolerance_tab=tolerance,
                              verbose=T)
plot(my_ABC$param )
result1 <- colMeans(aafi_Beaumont$param)

output1 <- aafi_model(result1)

plot(aafi_vec)
points(output1,col='red')

output1_mat <- matrix(NA, 6,7)
output1_mat[(row(output1_mat)>=col(output1_mat)) | col(output1_mat)==7] <-
  output1

pdf("simple model.pdf",8,8)
par(mfrow=c(3,2))
for (i in 1:6) {
  plot(age_at_1st_intercourse[i,], ylim=c(0,1),
       ylab = "% reporting", xlab = "Age at first intercourse",
       main = paste("Respondents of age ", ages[i], sep=''))
  points(output1_mat[i,], col='red')
  lines(output1_mat[i,1:6], col='red')
}
dev.off()
