# berskons_simualtion
A simple simualtion of berksons paradox motivated by a conversation about GRE scores

library(ggplot2)

N <- 1e4

gre <- rnorm(N, 0, 3)

other <- rbinom(N, size = 1, prob = .2)

full_dat <- data.frame(gre, other)

full_dat$accepted_logodds <- with(full_dat, .1 + 1.1*gre + 3*other)

prop <- plogis(full_dat$accepted_logodds)

full_dat$accepted = rbinom(N, size = 1, prob = prop)

accepted_fit <- glm(data = full_dat, formula = accepted ~ gre + other, family = binomial(link = 'logit'))

full_dat$accepted_decision <- predict.glm(accepted_fit, type = "response") > .9

full_dat$success <- with(full_dat, rnorm(N, mean = 3 + 10*accepted_decision + rnorm(N, 0, 10)))

accept_dat <- full_dat[fulldat$accepted_decision == TRUE,]

summary(lm(data = accept_dat, success ~ gre + other))

summary(lm(data = full_dat,  success ~ gre + other))



full_plot <- ggplot(data = full_dat, 
                    aes(y = success, x = gre)) + 
  geom_point(aes(color = accepted_decision), alpha = .4) + 
  scale_color_manual(values=c("#56B4E9", "#D55E00"))+ 
  geom_smooth(data = accept_dat, method = "lm", linetype = 2, color = "#000000") + 
  geom_smooth(data = full_dat, method = "lm", linetype = 1, color = "#000000") +
  labs(y = "Success", x = "GRE score", color = "Accepted to \n graduate school?") +
      # caption = "red line is ols fit to the full population \n the green line is ols fit to students that are accepted") + 
  theme_bw()
  
full_plot

<img width="905" alt="Screen Shot 2020-09-04 at 10 32 54 AM" src="https://user-images.githubusercontent.com/63907969/92250867-fff6e900-ee99-11ea-8b04-76eff4b4d13a.png">
