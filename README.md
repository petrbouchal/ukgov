Charting government
==============

This is a large collection of code for working with data from a number of sources, all of them related to the UK government.

This includes the following:

* ONS Public Sector Employment data
* ONS time series data
* Civil Service People Survey
* Publications at Gov.uk
* Organogram data from gov.uk
* Government Business Plans
* Workforce Management Information from government departments
* UK Civil Service vacancies

This is roughly mirrored by the structure of the src folder of this repo, but often uses shared libraries, styles, etc.

The repo includes a different set of tools for each of these sources. For some, it starts with scraping or downloading data from the web and ends with a polished chart; for some, it's only a reshaping or plotting tool to be used with data created externally.

The code is of various degrees of completeness: some bits provide a complete product, some are mere proofs of concept, and some were only used for data exploration or purely experimental.

# To do
* Figure out where it makes sense to replace read.table with fread
* Figure out where it makes sense to replace merge with join (and how)
* Rewrite data reshapes and calculations with dplyr
* Simplify library script to deduplicate with what's in pbtools
