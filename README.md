# Probability and Statistical Inference for Economists
Based on the book The Practice of Statistics for Business and Economics 4th Edition by David S. Moore, George P. McCabe, Layth C. Alwan, Bruce A. Craig

This class is taught by Doğuhan Sündal who is my favorite professor. This is a study companion to help distill some main concepts that can help you better learn R, an incredible statistical analysis tool that does data manipulation. If you plan to use this resource to cheat I'd suggest you remind yourself of your University's policy on cheating and the consequences thereof. Then obviously don't.

## Table of Contents
- Variables & Manipulation
- Functions & Explanations

### Variables & Manipulation
Variables are important to understand and how to name correctly because we have the capability to manipulate them. The example below will show how to create a variable using the `<-` operator, and how to have it read a .csv file that the user chooses.

`## Import coupon information into the variable coupons`

`coupons <- read.csv(file.choose())`

When you run this in R by hitting control + enter the computer will allow the user to select the CSV file of your choice and assign the whole document to the variable "coupons". Be advised that the `read.csv()` function is built in to R, and there are SO MANY AMAZING FUNCTIONS LIKE IT! If you don't know what specific functions do, I implore you to google it.

To manipulate specific columns found within the variable coupons, use the `$` symbol after the variable name to denote which column specifically you want to manipulate. For example, we can create an a column with just 0's for every row:

`coupons$testcol <- 0`

You can also rename rows by doing the following. Keep in mind since the test column we made was the 7th column in the file (not base 0), then this is how we target it specifically:
`names(coupons)[7] <- "RealPrice"`

You can now use `coupons$RealPrice` as a variable for manipulation.

Lets do a simple example where within the coupons, there are the following columns:  `coupons$RegPrice` and  `coupons$DiscPrice`. Lets go ahead and do some simple arithematic and assign the end value to `coupons$RealPrice`.

To manipulate column values within `coupons` do the following:
`coupons$RealPrice <- coupons$RegPrice - coupons$DiscPrice`

This will minus the `coupons$RegPrice` and  `coupons$DiscPrice` values for every row and store the end result into `coupons$RealPrice`.

You can also specify specific values by targeting them as you would in excel. The first number in brackets would specify the rows or range of rows you want, while the second number specifies the column. **If you enter in only one value, it will return the whole column!** Examples:

`coupons[3,3]` will return "Smokey McSween's"

`coupons[1:3,3]` will return "Domo's" "Mama Rita's" "Smokey McSween's"

`coupons[7]` will return all values we assigned for `coupons$RealPrice`: 10, 8, 13, 9, 8, 5, 9

And that's really it!
