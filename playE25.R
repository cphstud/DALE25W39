# "facit"
x=1:10
a=1.5
b=2
y=a*x+b
noise=rnorm(10,sd=1)
noise=rnorm(10,0)
yhat=y+noise
hist(noise)
mean(noise)
plot(x,yhat,xlim=c(0,10),ylim = c(0,20))
model=lm(yhat ~ x)
cv=model$coefficients
rv=model$residuals
sum(rv^2)
summary(model)
abline(cv[[1]],cv[[2]], col="blue")

# initialize variables
# the baseline
slope=0
b=0
# the first sum
prevsum=sum(y^2)
# the incrementer
slopeincr=0.1
# the tolerance
tol=10
# the runcondition
runcondition=T

while(runcondition) {
  # improve the parameter
  slope=slope+slopeincr
  ## find SSE - sum((yh-y)^2)
  # model
  yh=slope*x+b
  # fit to observation
  SSE=sum((y-yh)^2)
  # compute the difference to the previous fit
  SSEDiff=abs(prevsum-SSE)
  # is it below the tolerance?
  if(SSEDiff < tol) {
    # yes - so stop
    runcondition=F
  } else {
    # no - so save values to next iteration
    prevsum=SSE
    # and plot the line
    abline(a=b,b=slope, col="green")
  }
  Sys.sleep(1)
}


# R squared
TSS=sum((y-mean(y))^2)
#Rs=1-(SSE/TSS)
Rs=1-(colsumnew/TSS)
summary(model)

abline(a=mean(y),b=0)
abline(a=model$coefficients[1],b=model$coefficients[2], col="blue")
abline(a=b,b=slope, col="red")