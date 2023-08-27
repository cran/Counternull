## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Counternull)

y = sample_data$turn_angle # fish turn angles 
w = sample_data$w # treatment assignments (1 = exposed, 0 = control)
test_stat = "diffmeans" # average difference in the turn angles between the
                        # two groups of fish
cat(find_test_stat(y, w, test_stat))

## ---- fig.width=4, fig.height=4-----------------------------------------------
n_rand = create_null_rand(y, w, sample_matrix, test_stat) # obtain "null_rand"
                                                          # object
summary(n_rand)
plot(n_rand)

## -----------------------------------------------------------------------------
print(sample_matrix[1:10,1:2])

## ---- fig.width=4, fig.height=4-----------------------------------------------
c_value = find_counternull_values(n_rand)
summary(c_value)
plot(c_value)

## ---- fig.width=4, fig.height=4-----------------------------------------------
c_value = find_counternull_values(n_rand, c(55,60))
summary(c_value)
plot(c_value)

## ----fig.width=4, fig.height=4------------------------------------------------
fisher = create_fisher_interval(n_rand)
summary(fisher)
plot(fisher)
t.test(sample_data$turn_angle[w == 1], sample_data$turn_angle[w == 0],
       conf.level = 0.95)$conf.int

## ---- fig.width=4, fig.height=4-----------------------------------------------
fun = function(x,y){ # write a function to compute KS test
  return(invisible(ks.test(x,y)$statistic))
}

rand_matrix = create_randomization_matrix(156,1000) # create rand matrix
                                                   # 156 fish, 1000 permutations

n_rand_two = create_null_rand(y, w, rand_matrix, fun = fun, 
                              alternative = "greater") 
summary(n_rand_two)
plot(n_rand_two)

## ---- fig.width=4, fig.height=4-----------------------------------------------
adjusted_pvalues = adjust_pvalues(list(n_rand,n_rand_two))
cat(adjusted_pvalues)

