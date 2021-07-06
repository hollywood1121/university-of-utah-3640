# ECON 3640 Probability and Statistical Inference for Economists
Based on the book The Practice of Statistics for Business and Economics 4th Edition by David S. Moore, George P. McCabe, Layth C. Alwan, Bruce A. Craig

This class is taught by Doğuhan Sündal who is my favorite professor. This is a study companion to help distill some main concepts that can help you better learn R, an incredible statistical analysis tool that does data manipulation. If you plan to use this resource to cheat I'd suggest you remind yourself of your University's policy on cheating and the consequences thereof. Then obviously don't.

Because R is in the computer science computational arena, documentation and walkthroughs are everywhere. If this guide isn't good enough, google your problems and find the right one for you, guaranteed.

## Table of Contents
- Variables & Manipulation
- Functions

### Variables & Manipulation
Variables are important to understand and how to name correctly because we have the capability to manipulate them. The example below will show how to create a variable using the `<-` operator, and how to have it read a .csv file that the user chooses.

`## Import coupon information into the variable coupons`

`coupons <- read.csv(file.choose())`

When you run this in R by hitting control + enter the computer will allow the user to select the CSV file of your choice and assign the whole document to the variable "coupons". Be advised that the `read.csv()` function is built in to R, and there are SO MANY AMAZING FUNCTIONS LIKE IT! If you don't know what specific functions do, I implore you to google it.

As mentioned before, `coupons` has the columns, rows, and values associated from the document you selected. <u>You must set a variable every time you import csv files to manipulate it!</u> This is a very important concept.

##### Manipulation
To manipulate specific columns found within the variable coupons, use the `$` symbol after the variable name to denote which column specifically you want to manipulate. For example, we can create an a column with just 0's for every row:

`coupons$testcol <- 0`

You can also rename rows by doing the following. Keep in mind since the test column we made was the 7th column in the file (not base 0), then this is how we target it specifically:
`names(coupons)[7] <- "RealPrice"`

You can now use `coupons$RealPrice` as a variable for manipulation.

Lets do a simple example where within the coupons, there are the following columns:  `coupons$RegPrice` and  `coupons$DiscPrice`. Lets go ahead and do some simple arithematic and assign the end value to `coupons$RealPrice`.

To manipulate column values within `coupons` do the following:
`coupons$RealPrice <- coupons$RegPrice - coupons$DiscPrice`

This will minus the `coupons$RegPrice` and  `coupons$DiscPrice` values for every row and store the end result into `coupons$RealPrice`.
##### Targeting Specific Values
You can also specify specific values by targeting them as you would in excel. The first number in brackets would specify the rows or range of rows you want, while the second number specifies the column. **If you enter in only one value, it will return the whole column!** Examples:

`coupons[3,3]` will return "Smokey McSween's"

`coupons[1:3,3]` will return "Domo's" "Mama Rita's" "Smokey McSween's"

`coupons[7]` will return all values we assigned for `coupons$RealPrice`: 10, 8, 13, 9, 8, 5, 9

### Functions
Functions are what make R so flexible and quick to handle data anyway we like to. Functions are premade snippets of code (by R or using packages) that you use to help accomplish your goals. Whether its displaying a graph, manipulating data, or performing data heavy computations, they are all known as "functions". They are very specific to help you do what you need to do quickly.
##### Parameters of a Function
Keep in mind most functions take in something called "parameters" or some call it "arguments" (from here out known as Parameters). They both mean the same thing and are VERY important to understand. Parameters are what information gets passed to a function for the function to operate. A an example, let's take a look at our classic function `barplot()`. `barplot()` is a function that can take MANY parameters. Here are some examples:

`barplot(coupons, main="Coupon Prices", names=coupons$Name)`

`coupons` is the data that needs to be visualized that we used earlier.

`main="Coupon Prices"` is the title of the graph.

`names=coupons$Name` is the individual names of each business on the list.  

Notice both `main` and `names` have an `=` and some sort of value associated with it to determine what is being assigned with that variable.
 You can either assign variables beforehand like we did with `coupons` or you can assign values on the spot, either way works.

 ##### What Parameters Can Be Sent to a Function?
 Keep in mind that anyway you do it, **you need to know what parameters are available in the function**. I'd highly recommend googling everything you can about the function you're using. to understand it.

##### * The most important tip about Parameters *
**You must know what type of variable your Parameter needs to accept.** Parameters will mostly always expect a specific type of Parameter such as a string, column, or dataset (like the `coupons` we used previously). if you don't know what these are, I highly suggest googling them.



 In 3640 we go through a ton so here's a quick reference section to the ones we use often, and WHY we use them.

##### Visual Functions
- [Barplot graphs ](#Barplot-Graph)
- [Pie graphs](#)
- [Histograms](#)

##### Mathematic / Computational functions
- [Mean ](#Barplot-Graph)
- [Standard Deviation (SD)](#)
- [Median](#)



# Barplot Graphs
Barplot graphs are a great way to get across data in a very easy to understand way. While there are definitely limitations to what Barplot graphs can display, we need to learn how to utilize the function.
