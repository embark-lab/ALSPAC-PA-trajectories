

```{r}
list <- unique(as.numeric(row.names(ranef(rand_int_dex)$id)))
filtered<- demo_ed.l.girls %>%
  filter(id %in% list)
```

```{r}
#predict gives the predicted value in terms of logits
plot.dat <- data.frame(prob = filtered$driven_exercise,
                       age = filtered$age_adjust,
                       fit = predict(rand_int_dex, filtered))
#convert those logit values to probabilities
plot.dat$fit_prob <- exp(plot.dat$fit)/(1+exp(plot.dat$fit))

library(ggplot2)
ggplot(plot.dat, aes(x=age, y=prob)) +
  geom_point() +
  geom_line(aes(x=age, y=fit_prob))
```
