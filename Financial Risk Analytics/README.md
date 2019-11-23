# Financial & Risk Analytics

<p>This repository is a collection of all homework assignments completed for Northwestern University MSDS program in 'Financial & Risk Analytics' (MSDS 451). Each assignment includes a PDF, which was generated using the accompanying R Markdown file.</p>
  
<h3>1 - Simulations</h3>

<blockquote>
  
  <h4>Background</h4>
  
  <p>Risk and returns are two sides of a coin: we should not be taking any risk if there is no potential return! Rather, we should just put our money in the bank to generate interest. A well-known metric expressed this very well: the Sharpe ratio (see www.stanford.edu/~wfsharpe/art/sr/sr.htm (Links to an external site.)), which is just the ratio of the excess return of an investment over its standard deviation. (Excess return means return over and above the bank interest rate.)</p>
  
  <ul>
	  <li>Understand risk/reward trade-offs.</li>
	  <li>When you have $100,000 in your hands, and want to invest in, say, corn futures. What are the possible risks you are taking? (Think exhaustively!) More advanced question: what are the possible risks if you want to invest in Australian dollars?</li>
	  <li>The formula for Sharpe ratio implies that risk is just standard deviation of returns. But this often does not reflect reality. We can have very volatile, but anti-correlated returns of a stock from one period to another, and make consistent profits with very little risks. Can you outline a trading strategy that might generate that profit?</li>
	  <li>We typically model the returns of a stock using normal (Gaussian) distributions. Does this under-estimate or over-estimate the true risk of holding a stock? Why? Is there a parametric distribution that better captures the true risk?</li>
  </ul>
  
  <h4>Solution Overview</h4>
  
  <p>Build a Monte-Carlo trading simulation in R.</p>

  <ul>
	  <li>Suppose a hedge fund owns $1,000,000 of stock and used $50,000 of its own capital and $950,000 in borrowed money for the purchase.</li>
	  <li>Suppose that if the value of the stock falls below $950,000 at the end of any trading day, then the hedge fund will sell all the stock and repay the loan. This will wipe out its $50,000 investment.</li>
	  <li>The hedge fund is said to be leveraged 20:1 since its position is 220 times the amount of its own capital invested.</li>
	  <li>Suppose that the daily log returns on the stock have a mean of 0.05/year and a standard deviation of 0.23/year.</li>
  </ul>
  
</blockquote>

<h3>2 - Stock Returns and Lognormal Distributions</h3>

<blockquote>
  
  <h4>Background</h4>
  
  <p>Assume that a stock's daily returns r=(P(t+1)-P(t))/P(t) follow a normal distribution...". There is a simple and obvious reason why returns cannot really have a normal distribution (even though it may be a good approximation). Nor can it really have a t-distribution mentioned below. However, log returns, defined as log(1+r) and used in Week 1 assignment, can actually have a normal or t-distribution distribution.</p>
  
  <ul>
	  <li>The CLT implies that even if stock returns are not actually normally distributed, as long as the actual distribution has finite variance, stock prices will still be lognormally distributed. Why is that?</li>
	  <li>Of course, people often think that stock returns really has a t-Distribution. Does the t-Distribution have finite variance?</li>
	  <li>The chi-squared distribution is important to us mainly as a building block of the t-Distribution. If you ever wonder why the chi-squared distribution has a variance of 2*n, just remember that the fourth moment of a normally distributed variable X with mean=0 and variance=1 is 3. That is, <X^4>=3.  Can you prove that the chi-squared distribution has a variance of 2*n?</li>
	  <li>Bivariate and conditional density are important for the Bayes theorem. Just remember this: P(X, Y)=P(Y|X)*P(X)=P(X|Y)*P(Y). This is true whether P is a discrete probability, or a probability density function.</li>
  </ul>
  
  <h4>Solution Overview</h4>
  
  <p>Conduct detailed returns analysis in R.</p>

  <ul>
	  <li>Assume that a stock’s log returns at any time scale have normal distribution.</li>
	  <li>What are its average (mu) and standard deviation (sigma) of daily log returns, assuming a year has 250 trading days?</li>
	  <li>Compute the net returns of these instances, and compute their average and standard deviation.</li>
	  <li>If we assume that the stock’s initial price is $1, what is the expected value of its log price log(P(t)) after t minutes expressed in terms of mu?</li>
  </ul>
  
</blockquote>

<h3>3 - Fitting a Bivariate t-Distribution</h3>

<blockquote>
  
  <h4>Background</h4>
  
  <p>One intuitive way to understand a variable with t-Distribution is that it is like a variable with normal distribution, but with a scale that's random, and can sometimes be infinite!</p>
  
  <ul>
	  <li>The scale of the t-Distribution is inversely related to the chi-squared distribution. Recall that the chi-squared distribution has mean of n, where n is the degrees of freedom. If the degrees of freedom increase, will it be more or less likely that the variable with t-Distribution becomes extremely large?</li>
	  <li>For a single stock, we believe that the t-Distribution is a more realistic model for its returns than a normal distribution. Do you remember why?</li>
	  <li>For a portfolio of multiple stocks, the  multivariate t-Distribution is a more realistic model for predicting the returns of all these stocks than a multivariate normal distribution. The reason it is more realistic goes beyond that given in 2). Do you know what it is?</li>
	  <li>Note that the estimation of statistical quantities such as covariance is model-dependent. For example, if you think that your data is described by the t-Distribution instead of a normal distribution, you would use a different MLE estimate of covariance.</li>
  </ul>
  
  <h4>Solution Overview</h4>
  
  <p>Conduct detailed returns analysis in R.</p>

  <ul>
	  <li>Let ˆθ = (μ1, μ2,A1,1,A1,2,A2,2, υ), where μj is the mean of the ijth variable, A1,A2, andA3 are the nonzero element of A, and υ is the degrees of freedom parameter.</li>
	  <li>The covariate matrix is then passed into chol, which computes the “Square Root” of the cov matrix using the Cholesky factorization method.</li>
	  <li>Find ˆθML, the MLE of ˆθ.</li>
	  <li>Find the MLE of p, the correlation between the two returns (Y1 and Y2).</li>
  </ul>
  
</blockquote>	
 
<h3>4 - Fitting Copula Models to Bivariate Returns</h3>

<blockquote>
  
  <h4>Background</h4>
  
  <p>Copulas look complicated, but they aren't difficult to understand once you read the chapter three times over! Here are some questions we will address in this module:</p>
  
  <ul>
	  <li>Why do we need copulas if we can just compute correlations?</li>
	  <li>What is the unique functional form of the univariate marginal distribution of a multivariate CDF that is a copula?</li>
	  <li>What does a correlation matrix corresponding to an independence copula look like? What about that corresponding to a co-monotonicity copula?</li>
	  <li>Suppose we have two stocks A and B with some bivariate distribution of returns. If stock A's return on day t is higher than its return on day s, then stock B's return on day t is also higher than its return on day s. Which correlation coefficient must be positive? If stock A's return on day t ranks high among its sample returns, then stock B's return on day t also ranks high among its sample returns. Which correlation coefficient must be positive?</li>
  </ul>
  
  <h4>Solution Overview</h4>
  
  <p>In this lab we are going to fit copula models to a bivariate data set of daily returns on IBM and S&P500 Index.</p>

  <ul>
	  <li>Using Kendall’s tau, compute omega, which is the estimate of the Pearson correlation from Kendall’s tau.</li>
	  <li>Here we are fitting the copulas to the uniformtransformed data. One method is parameteric (the tdistribution) and one is nonparametric (uniform).</li>
	  <li>Next, we will define a metatdistribution by specifying its t-copula and its univariate marginal distributions.</li>
	  <li>Was the estimation method maximum likelihood, semiparametric pseudomaximum likelihood, or parametric pseudomaximum likelihood?</li>
  	<li>Estimate the coefficent of lower tail dependence for this copula.</li>
  </ul>
</blockquote>

<h3>5 - Daily Returns Analysis</h3>

<blockquote>
  
  <h4>Background</h4>
  
  <p>There is a section in this chapter on forecasting. You might think that surely the whole point of all this time series mumbo-jumbo is so that we can forecast the next value of the time series. Actually, we do not need to be able to forecast in order to profit from studying time series. Recall that as long as we can determine a price series is stationary, we can easily trade it profitably. Much of quantitative trading is about constructing a portfolio that has stationary market value!</p>
  
  <ul>
	  <li>If the price series of a stock is stationary, we can easily trade it profitably. Can you suggest how? Does that mean that if a price series is white noise, it would be easy to profit from it?</li>
	  <li>If you difference the log price series of any stock, you will get the log returns series. The (log or net) returns series of any stock is stationary, but this won't make it easy to trade it profitably. Can you see why? In particular, if a returns series is white noise, do you think you can trade it profitably? What possible property of a returns series will enable us to trade profitably?</li>
	  <li>Dickey-Fuller test is my favorite test for stationarity of a time series. All it is testing is whether the change in price depends (linearly) on the current price, dP=k*P+noise. If the time series is a random walk, clearly there is no dependence. What should the sign of k be for a stationary price series?</li>
	  <li>The author said he prefers MA models over AR models because of parsimony. Why are MA models are more parsimonious than AR models? Parsimony is important because the fewer parameters a model has, the better it can forecast out-of-sample data as opposed to just fitting in-sample data. Algorithmic trading is all about forecasting out-of-sample returns, so parsimony of models is especially important there. That's also why I seldom use any non-linear model in forecasting returns.</li>
  </ul>
  
  <h4>Solution Overview</h4>
  
  <p>In this lab we are going to fit time series models to data sets consisting of daily returns on various instruments.</p>

  <ul>
	  <li>Explain what “lag” means in the two ACF plots. Why does lag differ between the plots?</li>
	  <li>At what values of lag are there significant autocorrelations in the CRSP returns?</li>
	  <li>Find a 95% confidence interval for ϕ for the AR(1) model:</li>
	  <li>Now we will find the ‘best’ AR(p) model, m0, for the return series using the Bayesian information criterion.</li>
	  <li>Now, we attempt to backtest a trading strategy based on this AR model and compute the cumulative return of such a strategy.</li>
  </ul>
  
</blockquote>

<h3>6 - Black Monday</h3>

<blockquote>
  
  <h4>Background</h4>
  
  <p>On Black Monday, the return on the S&P500 was 22.8%.</p>
  
  <ul>
	  <li>What are the 3 features of volatility of returns that a constant volatility model fails to describe?</li>
	  <li>What is the main difference between ARCH and GARCH, and why is GARCH better?</li>
	  <li>Can the returns in GARCH have heavy tails if the noise terms are Gaussian?</li>
  </ul>
  
  <h4>Solution Overview</h4>
  
  <p>In this lab we are going to look at GARCH models and how they relate to predicting extreme events in financial markets.</p>

  <ul>
	  <li>What is the conditional probability of a return less than or equal to 0.228 on Black Monday?</li>
	  <li>Compute and plot the standardized residuals. Also, plot the ACF of the standardized residuals and their squares.</li>
	  <li>Does an AR(1) model with a Gaussian conditional distribution prove an adequate fit?</li>
	  <li>Analytically prove various properties of an AR(1) + GARCH(1, 1) process.</li>
  </ul>
  
</blockquote>

<h3>7 - Value at Risk</h3>

<blockquote>
  
  <h4>Background</h4>
  
  <p>What is the biggest defect of the VaR measure that ES doesn't have? Why do we need a minus sign in front of S in Equation 19.6?</p>
  
  <ul>
	  <li>The key deficiency of Value-at-Risk as a risk measurement is that when the returns of all assets contained in a portfolio do not come from a joint normal distribution (as is the case with a diversified mixed-asset class portfolio, i.e., not just a portfolio of single-names equities) then the VaR measure is not subadditive. This means that when VaR is measured on a diverse set of assets, the resulting VaR calculation can be incredibly misleading. This is illuminated eloquently with the example of a portfolio holding two separate bonds from a single issuer having a negative value-at-risk, implying no losses, vs a portfolio with two bonds from separate issuers having a positive VaR, implying losses.</li>
	  <li>Since we are looking at the inverse of the cumulative distribution function for a given value of alpha, which is going to be low (typically, .1, .5 or .01), this value is (probably) going to be negative since it will be in the lower tail of the distribution (the lowest values of return series). VaR is representative of a loss of money, so we multiply by -1 to convert from the (negative) return to a dollar amount. </li>
  </ul>
  
  <h4>Solution Overview</h4>
  
  <p>Suppose the risk measure ℜ is V aR(α) for some α.
Let P1 and P2 be two portfolios whos returns have a joint normal distribution with means μ1 and μ2, standard deviations σ1 and
σ2, and correlation ρ.
Suppose the initial investments are S1 and S2.
Show that ℜ(P1 + P2) ≤ ℜ(P1) + ℜ(P2) under joint normality.</p>

  <ul>
	  <li>How many shares of each stock should one buy to invest $50 million in an equally weighted portfolio?</li>
	  <li>What are the sample mean vector and sample covariance matrix of the 499 returns on these stocks?</li>
	  <li>What is the one-day VaR(0.1)∗ for this equally weighted portfolio?</li>
	  <li>What is the five-day VaR(0.1)∗ for this portfolio?</li>
  </ul>
  
</blockquote>    
  

<h3>8 - Efficient Equity Portfolios</h3>

<blockquote>
  
  <h4>Background</h4>
  
  <p>What are the short-comings of the mean-variance optimization method?</p>
  
  <ul>
	  <li>There is a unique weightings of risky assets that form a portfolio with the maximum Sharpe ratio. Do you know what that optimal portfolio is called? If we allow a mixture of risky assets and riskless cash, can we improve on that maximum Sharpe ratio? By varying the weighting between the optimal portfolio of risky assets and the riskless cash, what do investors hope to customize?</li>
	  <li>There are now many ETFs being marketed as "minimum volatility" ETFs, which are essentially minimum variance portfolios. Examples are SPLV, ACWV, EEMV, and USMV. Do you think these ETFs are on the efficient frontier? Can you think of any reason why we should prefer these minimum variance portfolios over a mixture of a tangency portfolio and cash? (Afterall, the investors themselves can decide how much risk to take by deciding how much of their cash they want to invest in this tangency portfolio!)</li>
  </ul>
  
  <h4>Solution Overview</h4>
  
  <p>Write an R program to find the efficient frontier, the tangency portfolio, and the minimum variance portfolio, and plot on “risk-reward space” the location of each of the six stocks, the efficent frontier, the tangency portfolio, and the line of efficient portfolios.</p>

  <ul>
	  <li>Use the constraints that, −0.1 ≤ wj ≤ 0.5 for each stock.</li>
	  <li>The first constraint limits the short sales but does not rule them out completely.</li>
	  <li>The second constraint prohibits more than 50% of the investment in any single stock.</li>
	  <li>Assume that the annual risk-free rate is 3%.</li>
  </ul>
  
</blockquote>

<h3>9 - Factor Models</h3>

<blockquote>
  
  <h4>Background</h4>
  
  <p></p>
  
  <ul>
	  <li></li>
	  <li></li>
	  <li></li>
	  <li></li>
  </ul>
  
  <h4>Solution Overview</h4>
  
  <p></p>

  <ul>
	  <li></li>
	  <li></li>
	  <li></li>
	  <li></li>
  </ul>
  
</blockquote>