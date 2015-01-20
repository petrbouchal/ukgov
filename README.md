Charting government
==============

This is a collection of code - mainly utilities and simple tools in R and Python - for working with data from a number of sources, all of them related to the UK government. 

More extensive code for working with specific sources is stored in repositories named `petrbouchal/ukgov-[source name]`. These might later move to `instituteforgovernment/ukgov-[source-name]` or be transferred to another member of  [`instituteforgovernment`](http://github.com/instituteforgovernment) on Github.

The code is of various degrees of completeness: some bits provide a complete product, some are mere proofs of concept, and some were only used for data exploration or purely experimental.

Some of the scripts rely on custom functions and data stored in the ```pbtools``` R package, which can be installed like so:

```
install.packages('devtools')
library(devtools)
install_github('petrbouchal/pbtools')
```
