y=104 - rgamma(n = 20, shape = 8, scale = 4.5)
# use a 10% trimmed mean
stat=function(y){mean(y,trim=0.1)}
(obs=stat(y)) # Observed value of stat.

R=5000 # use 5000 bootstraps
boot=r=0 ; while(r < R){r=r+1 # for each of R samples
boot[r]=stat(sample(y,replace=TRUE))} # get bootstrap stat.

# estimate bias in std. norm deviates
b=qnorm((sum(boot > obs)+sum(boot==obs)/2)/r)

alpha=0.05 # 95% limits
z=qnorm(c(alpha/2,1-alpha/2)) # Std. norm. limits
p=pnorm(z-2*b) # bias-correct & convert to proportions

quantile(boot,probs=pnorm(p)) # Bias-corrected percentile lims.





# use a 10% trimmed mean
stat=function(y){mean(y,trim=0.1)}
(obs=stat(y)) # Observed value of stat.

R=5000 # use 5000 bootstraps
boot=r=0 ; while(r < R){r=r+1 # for each of R samples
boot[r]=stat(sample(y,replace=TRUE))} # get bootstrap stat.

# estimate bias in std. norm deviates
b=qnorm((sum(boot > obs)+sum(boot==obs)/2)/r)

# estimate acceleration constant
n=length(y) ; n1=n-1 ; obsn=obs*n
pv=i=0 ; while(i < n){i=i+1 ; pv[i]=obsn-n1*stat(y[-i])}
je=mean(pv)-pv
a=sum(je^3)/(6*sum(je^2))^(3/2)

alpha=0.05 # 95% limits
z=qnorm(c(alpha/2,1-alpha/2)) # Std. norm. limits
p=pnorm((z-b)/(1-a*(z-b))-b) # correct & convert to proportions

quantile(boot,p=p) # ABC percentile lims.








# use a 10% trimmed mean
stat=function(y){mean(y,trim=0.1)}
(obs=stat(y)) # Observed value of stat.

R=5000 # use 5000 bootstraps
boot=r=0 ; while(r < R){r=r+1 # for each of R samples
boot[r]=stat(sample(y,replace=TRUE))} # get bootstrap stat.

# estimate bias in std. norm deviates
b=qnorm((sum(boot < obs)+sum(boot==obs)/2)/r)

# estimate acceleration constant
n=length(y) ; n1=n-1 ; obsn=obs*n
pv=i=0 ; while(i < n){i=i+1 ; pv[i]=obsn-n1*stat(y[-i])}
je=mean(pv)-pv
a=sum(je^3)/(6*sum(je^2))^(3/2)

alpha=0.05 # 95% limits
z=qnorm(c(alpha/2,1-alpha/2)) # Std. norm. limits
p=pnorm((z-b)/(1-a*(z-b))-b) # correct & convert to proportions

quantile(boot,p=p) # ABC percentile lims.











