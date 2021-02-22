
<!-- README.md is generated from README.Rmd. Please edit that file -->

# retention.helpers

<!-- badges: start -->
<!-- badges: end -->

The goal of retention.helpers is to provide a few functions to help
working with the CSU Retention Team’s data set, within the R
environment.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("benwhicks/retention.helpers")
```

You should also have the tidyverse installed and loaded to get the most
out of the package:

``` r
# install.packages("tidyverse")
library(tidyverse)
```

The best way to interface with the data is through the `retention.data`
package. This package is not shared publicly, so you will need to obtain
a copy of the R Package itself and build the package yourself. This is
done in RStudio using `Ctrl-Shift-B` or by going to the *Build* pane
(usually in the top right along with *Environment*) and selecting
*Install and Restart*.

## Overview of functions

Most of the functions are split into groups based on their use and
prefix. The main families are `add_`, `affix_`, and `fetch_.`

### `add_*`

The `add_` family of functions create new columns in a data frame based
on data *in that table only*. This means they are the most broadly
useful as the do not require `retention.data` loaded. For example if
`offering` is in the data frame (in the form `ABC123_201990_W_D`) then
we can extract the subject `ABC123`, session `201990` or year `2019`.

<details>
<summary>
See an example of using the `add_*` functions
</summary>

``` r
# Given some data with offering
dat_with_offering <- tibble(
  offering = c("MTH100_189030_P_D", "PHL105_189060_B_I")
)

dat_with_offering
#> # A tibble: 2 x 1
#>   offering         
#>   <chr>            
#> 1 MTH100_189030_P_D
#> 2 PHL105_189060_B_I

# Add in subject and year from offering
dat_with_offering %>% 
  add_subject_from_offering() %>% 
  add_year_from_offering()
#> # A tibble: 2 x 3
#>   offering          subject  year
#>   <chr>             <chr>   <dbl>
#> 1 MTH100_189030_P_D MTH100   1890
#> 2 PHL105_189060_B_I PHL105   1890
```

</details>

### `affix_*`

The `affix_` family of functions create new columns in a data frame that
are aggregations of the data loaded, based on the groupings in the input
data frame. For instance if you feed `affix_demographics` a data frame
consisting of *subject* and *session*, it will aggregate by subject and
session and append a variety of demogrpahic measures to the data frame.

<details>
<summary>
See an example of using the `affix_*` functions
</summary>

``` r
# Given some data with a subject
dat_with_subject <- tibble(
  subject = c("MTH100", "MTH302")
)

dat_with_subject
#> # A tibble: 2 x 1
#>   subject
#>   <chr>  
#> 1 MTH100 
#> 2 MTH302

# Affix aggregated demographic data from the data set and include in this data frame
dat_with_subject %>% 
  affix_demographics()
#> Joining, by = "subject"
#> # A tibble: 2 x 12
#>   subject     n domestic domestic_p australian_indi~ australian_indi~ low_ses
#>   <chr>   <int>    <int>      <dbl>            <int>            <dbl>   <int>
#> 1 MTH100      2        2          1                0                0       0
#> 2 MTH302      1        1          1                0                0       0
#> # ... with 5 more variables: low_ses_p <dbl>, regional_or_remote <int>,
#> #   regional_or_remote_p <dbl>, parents_no_uni <int>, parents_no_uni_p <dbl>
```

</details>

### `fetch_*`

The `fetch_` family of functions create a new data frame by joining and
wrangling the existing data. These a tables that could just be stored as
part of the data package but are redundant. For example,
`fetch_students` combines the `student_demographics` and
`student_progess` tables into a larger table, attempting to get the most
recent relavent data for each student (this is because
`student_progress` is tied to a given session).

<details>
<summary>
See an example of using the `fetch_*` functions
</summary>

``` r
# Fetching latest student progress data, joined with demographic data 
fetch_students()
#> # A tibble: 3 x 18
#>   id    firstname lastname yob_approx gender domesticity atsi  parental_educat~
#>   <chr> <chr>     <chr>         <dbl> <chr>  <chr>       <chr> <chr>           
#> 1 1     Evariste  Galois         1811 Male   Domestic    Not ~ Not University ~
#> 2 2A    Emmy      Noether        1882 Female Domestic    Not ~ Not University ~
#> 3 3     Florence  Nightin~       1820 Female Domestic    Not ~ University Level
#> # ... with 10 more variables: remoteness <chr>, ses <fct>, course <chr>,
#> #   course_level <chr>, campus <chr>, course_enrolment <chr>,
#> #   course_faculty <chr>, commencing <chr>, attendance_type <chr>,
#> #   progress_rate <dbl>
```

</details>

## Loading the Data

This package is designed to work *with* the retention data set. This can
be loaded two ways:

You can load from a folder containing the .rda files directly:

``` r
# ============================================= #
# Loading via a folder of the .rda files
# ============================================= #
library(tidyverse)
folder_of_rda_files <- file.path('~', 'path', 'to', 'folder') # Change this to where you have them stored
rda_files <- list.files(folder_of_rda_files, 
                        pattern = ".rda", 
                        full.names = TRUE) # This grabs only rda files in folder_of_rda_files
# Loading the data into your environment
rda_files %>% 
  map(load)
```

Or you can load from the `retention.data` package:

``` r
# ============================================= #
# Loading data via the retention.data package
# ============================================= #
library(retention.data)
```

The benefit of using the package version of the data is that it comes
with in built documentation, using R’s usual help syntax, `?`.

``` r
?student_progress
```

## Data Model

This is the **intended** end product, but not all tables are complete or
tidy yet.

![](data-model.png)

## Further Examples

### Wrangling

### Adding variables

``` r
# creating a data frame with offerings
library(retention.helpers)
offs <- tibble(offering = c("MTH120_189030_P_D", "PHL100_189030_P_D"))
offs
#> # A tibble: 2 x 1
#>   offering         
#>   <chr>            
#> 1 MTH120_189030_P_D
#> 2 PHL100_189030_P_D

# adding the subject and year
offs %>% 
  add_subject_from_offering() %>% 
  add_year_from_offering()
#> # A tibble: 2 x 3
#>   offering          subject  year
#>   <chr>             <chr>   <dbl>
#> 1 MTH120_189030_P_D MTH120   1890
#> 2 PHL100_189030_P_D PHL100   1890
```

### Joining

``` r
# Joining demographics and academic
student_demographics %>% 
  inner_join(academic, by = "id") %>% 
  head()
#> # A tibble: 6 x 13
#>   id    firstname lastname yob_approx gender domesticity atsi  parental_educat~
#>   <chr> <chr>     <chr>         <dbl> <chr>  <chr>       <chr> <chr>           
#> 1 1     Evariste  Galois         1811 Male   Domestic    Not ~ Not University ~
#> 2 1     Evariste  Galois         1811 Male   Domestic    Not ~ Not University ~
#> 3 1     Evariste  Galois         1811 Male   Domestic    Not ~ Not University ~
#> 4 1     Evariste  Galois         1811 Male   Domestic    Not ~ Not University ~
#> 5 2A    Emmy      Noether        1882 Female Domestic    Not ~ Not University ~
#> 6 2A    Emmy      Noether        1882 Female Domestic    Not ~ Not University ~
#> # ... with 5 more variables: remoteness <chr>, ses <fct>, session <dbl>,
#> #   subject <chr>, grade <fct>

# Joining progress and academic and flags
flags %>% 
  inner_join(academic, by = c("id", "subject", "session"))
#> # A tibble: 3 x 8
#>   id    subject session offering          campaign    week concern        grade
#>   <chr> <chr>     <dbl> <chr>             <chr>      <dbl> <chr>          <fct>
#> 1 1     MTH120   189030 MTH120_189030_P_D pre census     3 low activity   FL   
#> 2 1     PHL100   189030 PHL100_189030_P_D pre census     3 non submission AW   
#> 3 2A    MTH302   189060 MTH302_189060_B_I pre census     3 non submission FW
```

### Visualising

``` r
# Trying to visualise grade distributions of flagged vs non flagged
academic %>% 
  left_join(
    flags %>% 
      filter(campaign == "pre census") %>% # only looking at pre census flags
      select(id, subject, session) %>% # only selecting what is required for joining with academic
      mutate(flagged = "At risk"), # left join as we want all academic, which creates NA's if no match
    by = c("id", "subject", "session")) %>%  
  mutate(flagged = replace_na(flagged, "Not at risk")) %>% # NA's did not appear in the flag table
  ggplot(aes(grade)) +
  stat_count(aes(fill = flagged)) +
  facet_grid(flagged ~ session) +
  theme_minimal() 
```

<img src="man/figures/README-example-visualising-1.png" width="100%" />
