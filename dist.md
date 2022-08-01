## Discrete distributions
Here you can run a simulation study to see the results of the CLT in practice. Choose the type of distrbution and the size of each sample; 
the app then generates 5000 samples of given size and plots an **emprical cumulative distribution function** (ECDF) of the means of each sample. 
You can then choose to plot the *cumulative distribution function* of the normal distribution the mean should tend to, either an empirical CDF with 
parameters estimated from the samples, or a theoretical CDF given by the central limit theorem. 

For discrete distributions, I chose to include the ECDF instead of a histogram, since the histogram visualisations are not great especially 
for small sample sizes as the *hist* function chooses the bins automatically and there might be only a handful of discrete values that the mean 
can take. 

Observe how the ECDF gets visually closer to the theoretical cumulative distribution function. The number of unique values the mean can take also
(obviously) rises, which gives us the illusion of continuity despite working with discrete random variables. This demonstrates the effect of CLT as
we can work with the means of discrete values as if they were realisations of a continuous distribution, namely the familiar *normal* distribution. 