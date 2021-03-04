# Tips for using R and the tidyverse

# Organising your code ----------------------------------------------------

# Use Ctr-Shift-R to insert a line like the one above. Notice it generates a little
# drop down arrow next to the title - this can be collapsed / expanded by clicking on
# it or using Alt + L / Shift + Alt + L.
# You can collapse / expand all with Alt + O / Shift + Alt + O


# Formatting a sequence of pipes ------------------------------------------

library(tidyverse)

# always put a %>% at the end of the line
mtcars %>%
  filter(mpg > 15)

# If you want to make many changes in the one function, create a new line after the `(` and each `,`
mtcars %>%
  filter( # This also makes a nice comment space after the open bracket new line
    mpg > 15,
    am == 1
  )

