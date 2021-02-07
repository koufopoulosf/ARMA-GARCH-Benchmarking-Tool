# Benchmarking Tool for ARMA-GARCH Models

Multiple Financial Time Series Analysis combined with Benchmarking option for ARMA-GARCH Models.

In the field of time series analysis, some of the most popular models are the Auto-Regressive Moving Average (ARMA) models. ARMA models are excellent in modeling and forecasting univariate time series, but often fail in analyzing and forecasting financial time series. That is because they cannot capture a process with time-varying conditional variance (volatility) which is mostly common with economic and financial time series data. Thus, accurate modeling of time volatility is of great importance in financial time series analysis.

In practice, financial time series contain uncertainty, volatility, excess kurtosis, high standard deviation, high skewness and sometimes, non normality. Therefore, various models have been proposed to handle these characteristics of financial time series, such as Generalized Auto-Regressive Conditional Heteroscedastic (GARCH) models.

The ARMA-GARCH-Benchmarking-Tool.R script automates the process of studying multiple financial time series as well as providing the option to make multiple benchmarks between various AR(p) and MA(q) orders of ARMA-GARCH models on the following distributions, as provided by the "rugarch" package:

<ul>
<li>Normal Distribution</li>
<li>Student-t Distribution</li>
<li>Generalized Error Distribution</li>
<li>Skew Normal Distribution</li>
<li>Skew Student-t Distribution</li>
<li>Skew Generalized Error Distribution</li>
<li>Normal-Inverse Gaussian Distribution</li>
<li>Johnson's SU-Distribution</li>
<li>Generalized Hyperbolic Distribution</li>
</ul>

Results are automatically stored inside a folder in user's Desktop and best models are proposed based on the following Information Criteria:

<ul>
<li>Akaike Information Criterion</li>
<li>Bayesian Information Criterion</li>
<li>Shibata Information Criterion</li>
<li>Hannan-Quinn Information Criterion</li>
</ul>
