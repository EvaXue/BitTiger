#bootstrap for DOE
##question: what if data is highly skewed or statistics is hard to approximate with CLT?
##bootstrap is a resampling method which can be used to estimate sampling distribution of any statistics.

##approaches:
##1. Randomly generate a sample of size n with replacement from the original data. 
##2. Repeate step 1
##3. Estimate statistics with sampling statistics of the generated samples.

## practice
#original dataset:
orig = rnorm(100,3,5)
#what is the sampling mean & variance for sample mean
print ("mean for original dataset ")
mean(orig)

##create an empty matrix
btsample = matrix(nrow = 200, ncol = 100)
##bootstrap
for (i in 1:200){
  btsample[i,] = orig[sample(1:100, replace = TRUE)]
}

btsample
##Margin=>apply to row or column
##MARGIN is a variable defining how the function is applied: 
##when MARGIN=1, it applies over rows, whereas with MARGIN=2, it works over columns. 
##Note that when you use the construct MARGIN=c(1,2), it applies to both rows and columns;
samplmeans = apply(btsample, MARGIN = 1, FUN = mean)
##Bt statistics
mean(samplmeans)
var(samplmeans)
#theoritical variance from CLT
##population variance/n
5^2/100
