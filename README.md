# berskons_simualtion
A simple simualtion of berksons paradox motivated by a conversation about GRE scores

library(ggplot2)

library(data.table)

N <- 1e4

gre <- rnorm(N, 0, 3)

other <- rbinom(N, size = 1, prob = .2)

full_dat <- setDT(data.frame(gre, other))

full_dat$accepted_logodds <- with(full_dat, .1 + 1.5*gre + 3*other)

prop <- plogis(full_dat$accepted_logodds)

full_dat$accepted = rbinom(N, size = 1, prob = prop)

accepted_fit <- glm(data = full_dat, formula = accepted ~ gre + other, family = binomial(link = 'logit'))

#### only the best are accepted!
full_dat$accepted_decision <- predict.glm(accepted_fit, type = "response") > .9

full_dat$success <- with(full_dat, rnorm(N, mean = 3 + 10*accepted_decision + rnorm(N, 0, 10)))

accept_dat <- full_dat[accepted_decision == TRUE,]

summary(lm(data = accept_dat, success ~ gre + other))

summary(lm(data = full_dat,  success ~ gre + other))



full_plot <- ggplot(data = full_dat, 
                    aes(y = success, x = gre)) + 
  geom_point(aes(color = accepted_decision), alpha = .4) + 
  scale_color_manual(values=c("#C4961A", "#293352")) + 
  geom_smooth(data = accept_dat, method = "lm", color = "green") + 
  geom_smooth(data = full_dat, method = "lm", color = "red") +
  labs(y = "Success", x = "GRE score", color = "Accepted to \n graduat school?") +
      # caption = "red line is ols fit to the full population \n the green line is ols fit to students that are accepted") + 
  theme_bw()
  
full_plot


![Screen Shot 2020-09-03 at 9 11 11 AM](https://user-images.githubusercontent.com/63907969/92119133-6bbb5200-edc5-11ea-8c0f-fb05ea1fb642.png)
