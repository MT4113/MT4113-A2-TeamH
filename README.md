# Assignment 2
# Team H

## Team Members
+ Bryant, StatsThoughts
+ Charlotte, cjlcasey
+ Yulia, YuliaYashneva
+ Abhinav, abhinav0397

## Task 1: Data exploration

- Create two plots of the data: one with age on the x-axis and length on the y-axis, and one with length on the x-axis and frequency on the y-axis.  Add axis labels and any additional information you think is relevant (e.g., average length-at-age). Include your figure in your README file like this: 

![Fig1](https://github.com/eirenjacobson/MT4113-A2-TeamH/blob/master/Figures/Scatterplot.png)

* The Scatter plot between Fish Length and Age displays the exponential increase in length from Age 1 to Age 2, with the highest lengths of Age 1 and Age 2 being 31cm and 59.8 cm respectively. The mean length of fish from classes 1-3 are 21.4cm, 42.1cm and 67.7cm respectively.  

![Fig2](https://github.com/eirenjacobson/MT4113-A2-TeamH/blob/master/Figures/Boxplot.png)

* The mean fish length was found to be 46.68717 cm.

![Fig3](https://github.com/eirenjacobson/MT4113-A2-TeamH/blob/master/Figures/Plain%20Histo.png)

*	From the Summary Stats table we can observe the increase in Sd, suggesting increase in variation for the length of fishes from Age class 1-3.

![Fig4](https://github.com/eirenjacobson/MT4113-A2-TeamH/blob/master/Figures/Density%20Histo.png)

*	The Fish length data as predicted does not follow Normal distribution and has an Undefined Bi-Modal Shape.


## Task 2: Methods description

- In your own words, describe how you will apply the EM algorithm to this dataset (max 250 words).

- Include a diagram of your team's approach to the problem.  This can be digital or hand-drawn and scanned in as a PDF. Again, include your diagram in the README file:

![Diagram](https://github.com/MT4113/2018/blob/master/Assignments/A2/StarterRepo/Figures/Diagram.png)

![Fig5](https://github.com/eirenjacobson/MT4113-A2-TeamH/blob/master/Figures/EM-Algorithm-2.pdf)

## Task 3: Algorithm implementation

- Create modular code consisting of a set of documented and tested functions implementing this algorithm. Include links to these functions in the README file:

- [Code for implementing the EM algorithm](https://github.com/MT4113/2018/blob/master/Assignments/A2/StarterRepo/Scripts/teamEM.R)

- [Maybe you also have subroutine functions to e.g., calculate the likelihood](https://github.com/MT4113/2018/blob/master/Assignments/A2/StarterRepo/Scripts/calcLikelihood.R)

## Task 4: Function testing

- Write a function to create simulated datasets with similar properties to the "true" data.  Include a link to this function in the README file.

- Show that the algorithm returns correct values for simulated datasets.  How you do this is up to your team.

## Task 5: Results reporting

| Parameter | mu | sigma | lambda |
|-----------|----|-------|--------|
| Age 1     | 23.39995  | 3.897076     | 0.2054526      |
| Age 2     | 41.95479  | 5.488541     | 0.4531959      |
| Age 3     | 67.06457  | 8.153021     | 0.3413516      |
 
- Plot the original data with the densities of the mixture components superimposed.  Include your figure in the README file.

## Task 6: Work attribution

- I confirm that this repository is the work of our team, except where clearly indicated in the text.

- In one sentence per team member, describe who did what (e.g., A wrote function x, tested function y, documented function z).

Charlotte: Wrote original functions for the Initialisation functions (before further optimisation and consistency edits), put together plots for Data Exploration stage and wrote and tested the function for generating similar data sets (gen.test.data) and the function to implement testing of the EM algorithm and compare with expected results (imp.test.em), using both the original data and similar dadta sets.

Bryant: General project oversight, optimized code, reviewed code figures and reports, coded the itertative portion of the EM algorithim, documented and commented existing code, tested teamEM.R and functions.R for correct inputs/outputs, and offered coding aid to team members on small misc tasks. 
