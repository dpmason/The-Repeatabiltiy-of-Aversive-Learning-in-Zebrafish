###Analysis to find differences


#Step 1: Unlist distributions from rptr models and examine histograms 
rptr_male_boot <- unlist(rpt_e1_male$R_boot)
hist(rptr_male_boot, breaks = 50)
rptr_female_boot <- unlist(rpt_e1_female$R_boot)
hist(rptr_female_boot, breaks = 50)

#Step 2: Find the difference between bootstrap distributions and examine histogram

rptr_diff <- rptr2_boot - rptr1_boot
hist(rptr_diff, breaks = 50)

#Step 3: Find mean of the new distribution (difference between rptr models)

m3 <- mean(rptr_diff)
m3

#Step 4: Find quantiles at 0.025 and 0.975 (these two become your lower bound and upper bound 95% CI)

q <- quantile(rptr_diff, c(0.025, 0.975))
q


rpt_e1_male #1

rpt_e1_female #2
