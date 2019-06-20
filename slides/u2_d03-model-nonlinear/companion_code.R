ggplot(data = pp, aes(x = Width_in, y = Height_in)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, 
                col = "pink", # color
                lty = 1
    ) 

pp$a1 = (pp$Width_in/pp$Height_in - 1.61803398875)^2
pp$a2 = (pp$Height_in/pp$Width_in - 1.61803398875)^2


pp$discordance = 1:3135 
for(i in 1:3135){
    pp$discordance[i] = min(pp$a1[i], pp$a2[i]) 
}

ggplot(data = pp, aes(x = discordance, y = price)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "lm")


pp_numeric = pp %>% 
    dplyr::select_if(is.numeric)%>%
    select_if(~ !any(is.na(.))) 

painting_data = pp_numeric %>%
    dplyr::select(-price)

lm_full = lm(formula = logprice ~ .,
             data = (pp_numeric %>%
                         dplyr::select(-price)
             ) 
)

summary(lm_full)

step_lm = stepAIC(lm_full, direction = "both",
                  trace = FALSE)

summary(step_lm)

library(memisc)
show_html(mtable(lm_full, step_lm))
show_html(mtable(step_lm))
