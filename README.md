# Assignment 2
# Team H

## Team Members
+ Bryant, StatsThoughts
+ Charlotte, cjlcasey
+ Yulia, YuliaYashneva
+ Abhinav, abhinav0397

## Task 1: Data exploration

![Fig1](https://github.com/eirenjacobson/MT4113-A2-TeamH/blob/master/Figures/Scatterplot.png)

The scatter plot of Fish Length and Age displays a positive linear increase in length as age increases. This trend is displayed with the white line in the plot above. 

<br></br>


| Age       | Mean      | Std.Dv     | Min    | Q25    | Median | Q75     | Max   |
|-----------|-----------|------------|--------|--------|--------|---------|-------|
| Age 1     | 21.44     | 3.794898   | 14.35  | 19.430 | 21.42  | 23.4725 | 30.97 |
| Age 2     | 42.11478  | 6.704027   | 28.89  | 37.335 | 42.95  | 46.125  | 59.81 |
| Age 3     | 67.01676  | 8.649106   | 52.31  | 58.875 | 65.405 | 73.740  | 86.97 |

The table above illustrates the mean, standard deviation, median, min, max, and 25% and 75% quantiles of fish lengths for fish of known age.  Note that the standard deviations increase as the age which suggests a non-uniform standard deviation (potentially a linear model if trying to fit a model of a single distribution)

<br></br>

![Fig2](https://github.com/eirenjacobson/MT4113-A2-TeamH/blob/master/Figures/Final%20Boxplot.png)

The figure above is a visualization of the previous chart, and shows that for the fish of known ages, there are distinct groupings of length for each age category.   

<br></br>

![Fig3](https://github.com/eirenjacobson/MT4113-A2-TeamH/blob/master/Figures/Plain%20Histo.png)

The histogram above contains an undefined Tri-Modal Shape, which suggests that a single distribution may not be the best choice for the model. We should can consider testing a Gaussian mixture distribution for three catagories.  

<br></br>

![Fig4](https://github.com/eirenjacobson/MT4113-A2-TeamH/blob/master/Figures/Density%20Histo.png)

The histogram overlaid with a normal curve clearly shows that a normal distribution is not suitable for the data due to the undefined tri-modal shape.

<br></br>

## Task 2: Methods description

![Fig5](https://github.com/eirenjacobson/MT4113-A2-TeamH/blob/master/Figures/EM-Algorithm_1-2.png)

The first part of the EM algorithm is initialization. Firstly, function (1) is created for finding the mean, standard deviation and lambda for each age class based on the observations from the dataset with known ages. Its output is used as the input in function (2) to generate initial probability estimates for elements with unknown age class and updating the mean, standard deviation and lambda based on those results. 

Then the iterative process is started. Using those updated estimates posterior probabilities for each observation and age class are calculated (function 3). Function (4) generates new estimates for mean, standard deviation and lambda at each iterative step. In order to determine convergence of the maximization algorithm function (5) calculates log-likelihoods based on the estimated values from the function (4) and function (3). This iterative process from function (3) to function (5) continues until convergence or the maximum number of iterations is reached. 

For this algorithm convergence is reached when the difference between log-likelihoods generated in two consecutive iterative steps is less than epsilon. Following that, the output of the EM algorithm is formatted and returned as the following: a data frame of estimates (function 4 output), a data frame of initial values (function 2 output), convergence of the values (TRUE/FALSE), a data frame of posterior probabilities (function 3) and a vector of log-likelihood values which were generated at each iterative step (function 5).

## Task 3: Algorithm implementation

- [Documentation on all functions made can be found here:](https://github.com/eirenjacobson/MT4113-A2-TeamH/wiki/Documentation)

- [Code for implementing the EM algorithm](https://github.com/eirenjacobson/MT4113-A2-TeamH/blob/master/Scripts/teamEM.R)

- [Code for initialising and functions that contribute to the main teamEM function.](https://github.com/eirenjacobson/MT4113-A2-TeamH/blob/master/Scripts/functions.R)

- [Code for Error traps used in the teamEM function](https://github.com/eirenjacobson/MT4113-A2-TeamH/blob/master/Scripts/ErrorChecks.R)


## Task 4: Function testing
- [Link to script for the Testing of the EM algorithm, including the function that generates simulated data sets](https://github.com/eirenjacobson/MT4113-A2-TeamH/blob/master/Scripts/Testing%20EM.R)

### Demonstrating that implementing the EM algorithm with similar data sets
For comparison, first it will be demonstrated with the original data set "x". The function imp.test.em(x) gives the output:

$classResult
[1] "All outputs in form expected."

|Class Check|check|
|-----------|-----|
|estimates  |  1  |
|inits      |  1  |
|posterior  |  1  |
|likelihood |  1  |
|converged  |  1  |

The $classCheck section shown above checks the class of each of the parts of the output of the teamEM function and returns a 1 if all is correct. If the sum of the column is 5, so all results are in the correct form, and then prints out the statement under $classResult. 

|Behaviour Check     |percentage_difference|
|--------------------|---------------------|
|initial to final (%)|     0.0005939173    |

The percentage difference shown above is the measure of how the distribution has changed over the course of the EM Algorithm. It is calculated by the integral between the two curves over the integral of the initial over the discrete interval the fish lengths cover.

![Fig6](https://github.com/eirenjacobson/MT4113-A2-TeamH/blob/master/Figures/OriginalRplot.png)

As you can see from the chart above the two probability density functions don't differ to greatly.

Now for testing a simulated data frame made out of similar data sets from the gen.test.data(), this time looking at the output of imp.test.em(gen.test.data(), test = TRUE).

$classResult
[1] "All outputs in form expected."

|Class Check|check|
|-----------|-----|
|estimates  |  1  |
|inits      |  1  |
|posterior  |  1  |
|likelihood |  1  |
|converged  |  1  |


|Behaviour Check     |percentage_difference|
|--------------------|---------------------|
|real to initial (%) |          0.024672079|
|real to final (%)   |          0.008049152|
|initial to final (%)|          0.032713160|

![Fig7](https://github.com/eirenjacobson/MT4113-A2-TeamH/blob/master/Figures/SimulationRplot.png)

In this test trial it becomes more evident with the inclusion of the probability density function described by the original parameters that then determined the Lengths generated. Comparing the shape that comes from the initial estimates to that of the real shape and how close the final estimate approach the real probability density function.

### Demonstrating correct inputs/outputs of functions in function
In Testing EM.R, the function test_functions() tests that the correct inputs are being fed into each function in the teamEM function by printing sample outputs from each function, which can be examined visually in the console. 

### Demonstrating similarity to normalmixEM
In Testing EM.R, the function working_test() tests a randomly generated valid dataframe from gen.test.data() by generating outputs of the mu, sigma and lambda and the final log likelihood in both teamEM() and normalmixEM() from the mixtools library. Test results for simluated data yielded similar results, often accurate to the 4th decimal place for mu, sigma and lambda estimates, and the same loglikelihood values. This is shown in the output of working_test().


## Task 5: Results reporting
| Parameter | mu | sigma | lambda |
|-----------|----|-------|--------|
| Age 1     | 23.11271  | 3.852847     | 0.2019192      |
| Age 2     | 41.80995  | 5.629878     | 0.4526787      |
| Age 3     | 66.86062  | 8.356780     | 0.3454021      |
 
The above table of parameters was generated using the default results for our teamEM() function. Different results will arise depending on the values of inc_known_as_unknown_iter and inc_known_iter in the maximization algorithim. 
 
![Fig5](https://github.com/eirenjacobson/MT4113-A2-TeamH/blob/master/Figures/Functions%20Histo.png)
Plot of the original data with the densities of the mixture components superimposed.
The distribution of Fish length is not normal and has a tri-modal spread instead. There does not seem to be presence of any outlier for the data and there is increase in variability in the data as the Fish length increases. 

[Link to the code for where all the plots shown in the README.md file was generated](https://github.com/eirenjacobson/MT4113-A2-TeamH/blob/master/Scripts/Task1%20plots.R)

## Task 6: Work attribution

- I confirm that this repository is the work of our team, except where clearly indicated in the text.

- In one sentence per team member, describe who did what (e.g., A wrote function x, tested function y, documented function z).

Charlotte: Wrote original functions for the Initialisation functions (before further optimisation and consistency edits), put together plots for Data Exploration stage and wrote and tested the function for generating similar data sets (gen.test.data) and the function to implement testing of the EM algorithm and compare with expected results (imp.test.em), using both the original data and similar dadta sets.

Bryant: General project oversight, optimized code, reviewed code figures and reports, coded the itertative portion of the EM algorithim, documented and commented existing code, tested teamEM.R and functions.R for correct inputs/outputs, and offered coding aid to team members on small misc tasks. 

Yulia: Wrote original functions for the Likelihood functions (before further opitmisation and consistency edits). Created the flow-chart showing the team approach and wrote the description of the EM-algorithm.
