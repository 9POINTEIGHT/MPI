MPI : Computing package for Multidimensional Poverty Index (MPI)
==========================================================
[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.5.0-6666ff.svg)](https://cran.r-project.org/)
[![](https://img.shields.io/badge/doi-10.35648%2F20.500.12413%2F11781%2Fii039-yellow)](https://opendocs.ids.ac.uk/opendocs/handle/20.500.12413/11821)
[![License](https://img.shields.io/badge/License-MIT-orange.svg)](https://spdx.org/licenses/MIT.html)

A framework to calculate Multidimensional Poverty Index (MPI)
by using Alkire-Foster method

Given N individuals, each person has D indicators of deprivation, the package compute MPI value to represent the degree of poverty in a population. 

The inputs are 
1) an N by D matrix, which has the element (i, j) represents whether an individual i is deprived in an indicator j (1 is deprived and 0 is not deprived)
2) the deprivation threshold.  

The main output is the MPI value, which has the range between zero and one. MPI value is approaching one if almost all people are deprived in all indicators, and it is approaching zero if almost no people are deprived in any indicator. 


Installation
----------------------------------------------------------------------------------
You can install the newest version from Github.
```r
devtools::install_github('9POINTEIGHT/MPI')
``` 


Explanation: AF_Par and AF_Seq
----------------------------------------------------------------------------------
First we will use simulation poverty data from build-in package. Which contains 30 rows of individuals, 16 columns of deprivatied dimensions (1 is deprived and 0 is not deprived), and simulated forth-level administrative division of France.
```r
MPI::examplePovertydf
```

<img src="https://github.com/9POINTEIGHT/MPI/blob/master/man/FIG/examplePovertydf.JPG?raw=true">

We use the following function to compute MPI.
```r
out_seq <- AF_Seq(df = examplePovertydf, g = "Region", k = 3)
```
Input will be...
* `df`  A poverty data frame
* `g`  A column name that will be used to divide data into groups. When the value is NULL, the entire data is not separated into groups.(default as NULL)
* `w`  An indicator weight vectors (default as 1)
* `k`  A poverty cut-off. If an aggregate value of indicators of a specific person is above or equal the value of k, then this person is considered to be a poor.(default as 1)

Output will be `list of lists` separated into group, and each list contains...
* `groupname` 
* `H`  Head count Ratio, the proportion of the population that is multidimensionally deprived calculated by dividing the number of poor people with the total number of people.
* `A` Average deprivation share among poor people, by aggregating the proportion of total deprivations each person and dividing by the total number of poor people.
* `M0` Multidimensional Poverty Index, calculated by H times A.

``` r
[[1]]
[[1]]$groupname
[1] "Bastia"

[[1]]$H
[1] 1

[[1]]$A
[1] 0.4090909

[[1]]$M0
[1] 0.4090909
```

* `DimentionalContribution` 
  * `indnames` The poverty indicators
  * `diCont` Dimensional contributions denotes the magnitude of each indicator impacts on MPI.
  * `UncensoredHCount` Uncensored head count of indicator denotes the population that are deprived in that indicator.
  * `UncensoredHRatio` Uncensored head count ratio of indicator denotes the proportion of the population deprived in that indicator.
  * `CensoredHCount` Censored head count of indicator denotes the population that are multidimensionally poor and deprived in that indicator at the same time.
  * `CensoredHCount` Censored head count ratio of indicator denotes the proportion that is multidimensionally poor and deprived in that indicator at the same time.

<img src="https://github.com/9POINTEIGHT/MPI/blob/master/man/FIG/DimentionalContribution.JPG?raw=true" width="250">

* `pov_df` poverty data frame 
  * `Cvector` is a vector of total values of deprived indicators adjusted by weight of indicators. Each element in Cvector represents a total value of each individual.
  * `IsPoverty` is a binary variable (1 and 0). 1 indicates that a person does not meet the threshold (poor person) and 0 indicates the opposite.
  * `Intensity` , The intensity of a deprived indication among impoverished people is computed by dividing the number of deprived indicators by the total number of indicators.

<img src="https://github.com/9POINTEIGHT/MPI/blob/master/man/FIG/pov_df.JPG?raw=true" width="1500">

Citation
----------------------------------------------------------------------------------
Alkire S., Chatterjee, M., Conconi, A., Seth, S. and Ana Vaz (2014) Global Multidimensional Poverty Index 2014. OPHI Briefing 21, Oxford: University of Oxford. 

Please, visit <a href="https://opendocs.ids.ac.uk/opendocs/handle/20.500.12413/11821">Global Multidimensional Poverty Index 2014</a>


Contact
----------------------------------------------------------------------------------
* Developer: Kittiya Kukiattikun
* <a href="https://www.nectec.or.th/en/research/dsaru/dsarg-sai.html">Strategic Analytics Networks with Machine Learning and AI (SAI)</a>, <a href="https://www.nectec.or.th/en/">NECTEC</a>, Thailand
* Email: kittiya.contact@gmail.com