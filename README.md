# MLE

This Shiny app is designed to fit different distributions to data. There are two datasets with a few variables. Both probability mass and
cumulative distribution functions are plotted. 
The user can fit different distributions to decide which one is appropriate for maximum likelihood parameter estimation.

Estimation is an important part of statistics and, in particular, ML estimation differs from, say, OLS, by a distributional assumption. In other words, a student first has to decide what distribution can be assumed and then apply the ML. This task of guessing a correct distribution is at the core of the developed app.

Learning points:

- Both discrete and continous variables can be described by distributions
- Observed sample of data vs. reality/theoretical distribution

The app can be run with the following command:
```{r}
shiny::runGitHub('MLE', 'alionaBER')
```
