pp <- read_csv("../../data/paris-paintings.csv", 
               na = c("n/a", "", "NA"))

pp <- pp %>%
  mutate(
    Shape = fct_collapse(Shape, oval = c("oval", "ovale"),
                                round = c("round", "ronde"),
                                squ_rect = "squ_rect",
                                other = c("octogon", "octagon", "miniature")),
    mat = fct_collapse(mat, metal = c("a", "br", "c"),
                            canvas = c("co", "t", "ta"),
                            paper = c("p", "ca"),
                            wood = "b",
                            other = c("e", "g", "h", "mi", "o", "pa", "v", "al", "ar", "m"))
  )

str(pp)


new_pp = cbind(dplyr::select_if(pp, is.numeric),
               dplyr::select_if(pp, is.factor))  %>%
    select(-c( Interm, Diam_in, Surface_Rnd, logprice, position))
new_pp = new_pp[complete.cases(new_pp),]

pp_lm = lm(formula = price ~(.^2), 
           data = new_pp)


both_stepwise_lms = step(pp_lm, 
                         direction = "both",
                         trace = 10^6)


simple_lm = lm(formula = price ~(.), 
           data = new_pp)


simple_stepwise_lms = step(simple_lm, 
                         direction = "both",
                         trace = 10^6)


?step
step
