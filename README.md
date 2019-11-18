---
title: "Introduction to Bayesian Data Analysis"
output: 
 html_document:
  toc: TRUE
  toc_depth: '3'
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
options(mc.cores = parallel::detectCores())
```
An introductory Youtube tutorial to Bayesian Data Analysis, by Rasmus Bååth. A must for anyone keen on learning the basics of Bayesian inference. Rasmus offers an intuitive understanding of Bayes' theorem, and the application of the Hamiltonian MC/NUTS algorithm used for posterior sampling. Practical examples are generated using Stan.

- **Rasmus' Youtube tutorial**

https://www.youtube.com/watch?v=3OJEae7Qb_o

- **Rasmus' DataCamp tutorial**

https://www.datacamp.com/courses/fundamentals-of-bayesian-data-analysis-in-r

# Approximate Bayesian Computation: Exercise 1
Number of random draws from the prior:
```{r Data}
nDraws <- 10000
```

Defining and drawing from the prior distribution:

``` {r Create prior}
priorRate <- runif(nDraws, 0, 1)
hist(priorRate, col = "darkgreen")
```

Defining the generative model:
```{r Creating a generative model}
genModel <- function(rate) {
 subscribers <- rbinom(1, size = 16, prob = rate)
 subscribers
}
```

Simulating the data:
```{r Simulating the Data with the Model}
subscribers <- rep(NA, nDraws)
for (i in 1:nDraws) {
 subscribers[i] <- genModel(priorRate[i])
}
```

Filtering out the parameter values that did not result in the data that we actually observed:
```{r Filtering the Posterior}
postRate <- priorRate[subscribers == 6]
```

Checking that there are enough samples left:
```{r Inspecting the processed sample output}
length(postRate)
```

Plotting and summarising the posterior:
```{r Plotting posterior samples}
hist(postRate, xlim = c(0, 1), 
     col = "lightblue")
mean(postRate)
quantile(postRate, c(.025, .975))
```

What’s the *probability* that method A has a higher rate of sign-up than telemarketing?
```{r Comparing probability rates}
sum(postRate> .2) / length(postRate)
```

If method A was used on 100 people what would be number of sign-ups?
```{r Simulating probabilities for 100 hypothetical individuals}
signUps <- rep(NA, length(postRate))
for (i in 1:length(postRate)) {
 signUps[i] <- rbinom(n = 1, size = 100, prob = postRate[i])
}
```

But since rbinom is vectorized we can simply write it like this:
```{r A vectorised coding of the previous example}
signUps <- rbinom(n = length(postRate), size = 100, prob = postRate)
hist(signUps, xlim = c(0, 100),
     col = "orange")
quantile(signUps, c(.025, .975))
```
So a decent guess is that is would be between 20 and 60 sign-ups

# Using Hamiltonian Monte Carlo to sample the posterior parameter space: Exercise 2
Setup options for this section:
```{r Libraries and Settings}
library(rstan)
rstan_options(auto_write = FALSE)
```

Generate data the data:
```{r Generating Data}
dataList <- list(nA = 16, nB = 16, sA = 6, sB = 10)
```

You can also generate a modelString from within R.  This is done by making the stan model code a **string**, directly in the R script:
```{r The Stan Model}
modelString <- "
// Data:
data {
  // Number of trials
  int nA;
  int nB;
  // Number of successes
  int sA;
  int sB;
}
// Parameters:
parameters {
  real<lower=0, upper=1> rateA;
  real<lower=0, upper=1> rateB;
}
// Model:
model {
  rateA ~ uniform(0, 1); // prior for rateA
  rateB ~ uniform(0, 1); // prior for rateB
  sA ~ binomial(nA, rateA); // likelihood for sA
  sB ~ binomial(nB, rateB); // likelihood for sB
}
// Generated Quantities:
generated quantities {
real rateDiff; // rateDiff is a real value
rateDiff = rateB - rateA; // generate difference probabilities....
}
"
fishModel <- stan(model_code = modelString, data = dataList)
```

Let's inspect the model:
```{r Model Object}
fishModel
```

We can also plot the model to visually inspect it:
```{r Plotting the Model}
traceplot(fishModel)
plot(fishModel, 
     col = "red")
```
So, which rate is likely higher? A or B?

We can export samples to a data.frame for easier handling:
```{r Exporting samples to a data frame}
posteriorSamples <- as.data.frame(fishModel)
sum(posteriorSamples$rateDiff > 0) / length(posteriorSamples$rateDiff)
```
There is roughly 91% probability that rate B is higher than rate A.

## Part II
The marketing department are starting to believe that it was a fluke that such a large proportion of the Danes signed up. In all other European markets the proportion that signs up for a year of salmon is around 5% to 15%, even when given a free salmon. Use this  information and make the priors in your model more informative.

We will represent the background knowledge using the following beta distribution which is mostly focused on the region 0.05-0.15, i.e. **this is between 5-15%**:
```{r Updated Our Beliefs}
hist(rbeta(9999, shape1 = 3, shape2 = 25), 
     xlim = c(0, 1), 30,
     col = "lightblue")
lines(c(.05, .15), c(0, 0),
      col = "red",
      lwd = 3)
```

Except for the prior, the model below is exactly the same as in question I:
```{r Updated Stan Model}
modelString <- "
// Data:
data {
  // number of trials:
  int nA;
  int nB;
  // number of successes:
  int sA;
  int sB;
}
// Parameters:
parameters {
  real <lower = 0, upper = 1> rateA;
  real <lower = 0, upper = 1> rateB;
}
// Model:
model {
  rateA ~ beta(3, 25);
  rateB ~ beta(3, 25);
  sA ~ binomial(nA, rateA);
  sB ~ binomial(nB, rateB);
}
// Generated Quantities:
generated quantities {
  real rateDiff;
  rateDiff = rateB - rateA;
}
"
```

```{r Creating the posterior samples}
fishModel <- stan(model_code = modelString, data = dataList)
```

Now we can inspect the model:
```{r Inspecting the outputs and post-summary plots}
fishModel

plot(fishModel,
     col = "grey")

traceplot(fishModel)
```

Here are some further summary Stats:
```{r Summary Statistics}
posteriorSamples <- as.data.frame(fishModel)
sum(posteriorSamples$rateDiff > 0) / length(posteriorSamples$rateDiff)
```
So rate B is still estimated to be higher than A with around 83% probability, but both rates are estimated to be much lower.

## Part III
The economy department gives you the following information:

-   A mail of type A costs 30 kr to send out.
-   A mail of type B costs 300 kr to send out (due to the cost of the free salmon).
-   A salmon subscription brings in 1000 kr in revenue.

Which method, A or B, is *most* likely to make Swedish Fish Incorporated the most money? Note: we don’t have to make any changes to the model, it is enough to “post-process” the 
posterior distribution in posterior.

```{r The average profit per add sent out}
profitA <- -30 + posteriorSamples$rateA * 1000
profitB <- -300 + posteriorSamples$rateB * 1000

expectedProfitDiff <- mean(profitA - profitB)
```

Now we inspect the ***distributional differences*** between the two advertising options:
```{r Visual differences between option A and option B}
hist(profitA, 
     col = "orange")

hist(profitB,
     col = "yellow")

hist(profitA - profitB,
     col = "lightgreen")
abline(v = expectedProfitDiff, 
       col = "red",
       lwd = 2)
```

The expected profit when using method A is around 190 kr higher than for method B (which actually has a negative expected profit). So I guess sending free salmon to people isn’t the best idea. But note that we got this result after having made the decision analysis based on the model with the informative priors. If we use the non-informative priors we get a different result, and it’s up to you, the analyst, to decide which version of the model you decide to use.
