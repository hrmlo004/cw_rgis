
# script begins with loading packages
# library() function would work, but pacman::p_load() allows you to load multiple packages at once
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse)


# section label -----------------------------------------------------------
## you can create a section label with `Ctr + Shift + R`

## write codes as you need
x <- c(1, 3, 4)


# another section ---------------------------------------------------------
## fold sections with `Alt + O`
## unfold sections with `Alt + Shift + O`

y <- seq(0, 100, by = 1)

z <- c(5, 6, 7)
