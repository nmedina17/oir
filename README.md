# oir
Internal package to standardize preliminary data analysis, including common initial exploratory feedback  

Encodes common statistical decision trees, for a list of response variables for a project.  


Requires input data tbl to be "nested" into 2 columns --  
  1. "variable", a vector of response variable names, and  
  2. "varData", a list of  

This can be done with the 2 code lines, acting on typically cleaned "wide" data (i.e. response variables as columns), with dataset's independent variable keys also as columns (e.g. "plot", treatment", etc.) --  

#e.g.  
cleanData <- dplyr::tibble(  
  "plot" = c(1, 2, 3), "treat" = c(1, 2, 3),  
  "var1" = c(1, 2, 3), "var2" = c(1, 2, 3) #, [...]  
 )  

varTbl <- cleanData %>%  
  tidyr::pivot_longer(names_to = "variable", values_to = "value") %>%  
  purr::nest(cols = !c(-variable)). 

then processed using --  

statFormula <- variable ~ [independent variable of interest]  
resultsTbl <- varTbl %>% oir::getStatsTbl(formula = statFormula)  


Notes --  

- best used with simple model formula, i.e. 1 independent variable in "statFormula" (at a time)  
- the main oir::getStatsTbl() function has versions "1", "2", and "12" to run --  
  1. linear regressions at plot-/group-level median centers ("1")  
  2. non-linear version of regression (not nlme::) modifying independent variable with stats::poly(x, degree = 2, at finest row level ("2")  
  3. the same non-linear version of regression, but using plot-/group-level centers ("12")  
- more complex formulas (e.g. lmer) can be processed, but with limited insight into final appropriate p-values  
- non-parametric option included as back-up test for usable p-value, but may offer slightly different inference/interpretation  
