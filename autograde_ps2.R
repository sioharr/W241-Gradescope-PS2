# W241 Spring, 2021 
# PS2
# Alex, Micah, Scott, Haerang, Tim, and Siobhan

  
rm(list = ls())


json.filename <-  "/autograder/results/results.json" # Change to "results.json" when testing locally
                                                    # Change to "/autograder/results/results.json" when uploading
                                                    # to Gradescope


suppressMessages(library(tidyverse))
suppressMessages(library(jsonlite))

library(testthat)
library(gapminder)
library(tidyverse)
library(data.table)
library(sandwich)
library(lmtest)
library(ggplot2)
library(knitr)
library(round)
library(stargazer)

#knitr::purl("../ps2.Rmd")
#source("ps2.R")

knitr::purl("question_1.Rmd")
source("question_1.R")

knitr::purl("question_2.Rmd")
source("question_2.R")

knitr::purl("question_3.Rmd")
source("question_3.R")

#knitr::purl("question_4.Rmd")
#source("question_4.R")





test.cases <- list(
                   list(
                    name = "Q1 - Is ATE correct?",
                    visibility = "hidden", 
                    weight = 1
                    ),
		  list(
                    name = "Q1 - Is hajj ate object a data.table?",
                    visibility = "hidden", 
                    weight = 1
                    ),
		  list(
                    name = "Q1 - Does hajji object have the right shape?",
                    visibility = "hidden", 
                    weight = 1
                    ),
		  list(
                    name = "Q1 - Is ri distribution the correct class?",
                    visibility = "hidden", 
                    weight = 1
                    ),
		  list(
                    name = "Q1 - Is larger number sensible?",
                    visibility = "hidden", 
                    weight = 1
                    ),
		  list(
                    name = "Q1 - Is larger number correct?",
                    visibility = "hidden", 
                    weight = 1
                    ),
		  list(
                    name = "Q1 - Is p-value derived from number larger?",
                    visibility = "hidden", 
                    weight = 1
                    ),
		  list(
                    name = "Q2 T-tests?",
                    visibility = "hidden", 
                    weight = 1
                    ),
		  list(
                    name = "Q2. Are RI correct vectors?",
                    visibility = "hidden", 
                    weight = 1
                    ),
		  list(
                    name = "Q2. Is the creative pvalue in the ballpark?",
                    visibility = "hidden", 
                    weight = 1
                    ),
		  list(
                    name = "Q2. Is the sweat pvalue in the ballpark?",
                    visibility = "hidden", 
                    weight = 1
                    ),
		  list(
                    name = "Q3. Is the cards auction a t.test?",
                    visibility = "hidden", 
                    weight = 1
                    ),
		  list(
                    name = "Q3. Is the cards auction p-value correct?",
                    visibility = "hidden", 
                    weight = 1
                    ),
		  list(
                    name = "Q3. Is the cards ATE correct?",
                    visibility = "hidden", 
                    weight = 1
                    ),
		  list(
                    name = "Q3. Is the cards ri distribution reasonable?",
                    visibility = "hidden", 
                    weight = 1
                    ),
		  list(
                    name = "Q3. What is the lm estimated for the model?",
                    visibility = "hidden", 
                    weight = 1
                    ),
		  list(
                    name = "Q3. Is the robust ci correct?",
                    visibility = "hidden", 
                    weight = 1
                    )
          )


              
tests <- list()
tests[["tests"]] <- list()
  
for(i in 1:length(test.cases)){
  tests[["tests"]][[i]] <- list(test.cases[[i]][["name"]],
                                score = 0,
                                max.score = test.cases[[i]][["weight"]],
                                visibility = test.cases[[i]][["visibility"]]
			      )
} 
write(toJSON(tests, auto_unbox = T), file = json.filename)

tests <- list()
tests[["tests"]] <- list()



cur.output <- ""
cur.weight=1  



########## Question 1 #########################
i<-1
test_that_var = test_that(
   'Q1 - Is ATE correct?', {
   expect_equal(hajj_ate, expected = 0.4748, tolerance = .01)
   })

if(test_that_var == TRUE){
      cur.score <-  cur.weight
      cur.output <- "Test passed\n"
  }else{
    cur.score <- 0
    cur.output <- "Test failed\n"
}

tests[["tests"]][[i]] <- list(name = "Q1 - Is ATE correct?",
                                output = cur.output,
                                score = cur.score,
                                max.score = cur.weight)
  

#2
i<-2

test_that_var = test_that(
    'Q1 - Is hajj ate object a data.table?', {
      expect_s3_class(hajj_group_mean, 'data.table')
  })

  if(test_that_var == TRUE){
      cur.score <-  cur.weight
      cur.output <- "Test passed\n"
  }else{
    cur.score <- 0
    cur.output <- "Test failed\n"
  }

tests[["tests"]][[i]] <- list(name = "Q1 - Is hajj ate object a data.table?",
                                output = cur.output,
                                score = cur.score,
                                max.score = cur.weight)



#3 ############################################################
i<-3
test_that_var <- test_that(
    'Q1 - Does hajji object have the right shape?', {
      expect_equal(nrow(hajj_group_mean), 2)
      expect_equal(ncol(hajj_group_mean), 2)
    })

  if(test_that_var == TRUE){
      cur.score <-  cur.weight
      cur.output <- "Test passed\n"
  }else{
    cur.score <- 0
    cur.output <- "Test failed\n"
  }

tests[["tests"]][[i]] <- list(name = "Q1 - Does hajji object have the right shape?",
                                output = cur.output,
                                score = cur.score,
                                max.score = cur.weight)
#################################################################
i<-4


   test_that_var <- test_that(
     'Q1 - Is ri distribution the correct class?', {
      expect_vector(hajj_ri_distribution)
    })

  if(test_that_var == TRUE){
      cur.score <-  cur.weight
      cur.output <- "Test passed\n"
  }else{
    cur.score <- 0
    cur.output <- "Test failed\n"
  }

tests[["tests"]][[i]] <- list(name = "Q1 - Is ri distribution the correct class?",
                                output = cur.output,
                                score = cur.score,
                                max.score = cur.weight)

###################################################################
i<-5


  test_that_var <- test_that(
  'Q1 - Is larger number sensible?', {
    expect_lt(hajj_count_larger, 40)
    expect_gt(hajj_count_larger, 10)
  })


  if(test_that_var == TRUE){
      cur.score <-  cur.weight
      cur.output <- "Test passed\n"
  }else{
    cur.score <- 0
    cur.output <- "Test failed\n"
  }

tests[["tests"]][[i]] <- list(name = "Q1 - Is larger number sensible?",
                                output = cur.output,
                                score = cur.score,
                                max.score = cur.weight)

###################################################################
i<-6

  test_that_var <- test_that(
  'Q1 - Is larger number correct?', { 
    expect_equal(
      object    = hajj_count_larger,
      expected  = sum(hajj_ri_distribution >= hajj_ate), 
      tolerance = 0.01)
    })


  test_that_var2 <- test_that(
  'Q1 - Is larger number correct?', { 
    expect_equal(
      object    = hajj_count_larger,
      expected  = sum(hajj_ri_distribution > hajj_ate), 
      tolerance = 0.01)
    })


  if(test_that_var == TRUE || test_that_var2 == TRUE){
      cur.score <-  cur.weight
      cur.output <- "Test passed\n"
  }else{
    cur.score <- 0
    cur.output <- "Test failed\n"
  }

tests[["tests"]][[i]] <- list(name = "Q1 - Is larger number correct?",
                                output = cur.output,
                                score = cur.score,
                                max.score = cur.weight)

###################################################################
i<-7

  test_that_var <- test_that(
  'Q1 - Is p-value derived from number larger?', { 
    expect_equal(
      object    = hajj_one_tailed_p_value, 
      expected  = mean(hajj_ri_distribution >= hajj_ate), 
      tolerance = 0.1)
  })

  if(test_that_var == TRUE){
      cur.score <-  cur.weight
      cur.output <- "Test passed\n"
  }else{
    cur.score <- 0
    cur.output <- "Test failed\n"
  }

 tests[["tests"]][[i]] <- list(name = "Q1 - Is p-value derived from number larger?",
                                output = cur.output,
                                score = cur.score,
                                max.score = cur.weight)

########## Question 2 #########################
###################################################################
i<-8

  test_that_var <- test_that(
  'Q2 T-tests?', { 
    expect_s3_class(t_test_creative, 'htest')
    expect_s3_class(t_test_sweat, 'htest')
  })

  if(test_that_var == TRUE){
      cur.score <-  cur.weight
      cur.output <- "Test passed\n"
  }else{
    cur.score <- 0
    cur.output <- "Test failed\n"
  }

 tests[["tests"]][[i]] <- list(name = "Q2 T-tests?",
                                output = cur.output,
                                score = cur.score,
                                max.score = cur.weight)

###################################################################
i<-9

  test_that_var <- test_that( 
  'Q2. Are RI correct vectors?', {
    expect_vector(creative_ri)
    expect_vector(sweat_ri)
  })


  if(test_that_var == TRUE){
      cur.score <-  cur.weight
      cur.output <- "Test passed\n"
  }else{
    cur.score <- 0
    cur.output <- "Test failed\n"
  }

 tests[["tests"]][[i]] <- list(name = "Q2. Are RI correct vectors?",
                                output = cur.output,
                                score = cur.score,
                                max.score = cur.weight)

###################################################################
i<-10

  test_that_var <- test_that(
  'Q2. Is the creative pvalue in the ballpark?', { 
    expect_lt(creative_p_value, 0.55)
    expect_gt(creative_p_value, 0.50)
    })

  if(test_that_var == TRUE){
      cur.score <-  cur.weight
      cur.output <- "Test passed\n"
  }else{
    cur.score <- 0
    cur.output <- "Test failed\n"
  }


 tests[["tests"]][[i]] <- list(name = "Q2. Is the creative pvalue in the ballpark?",
                                output = cur.output,
                                score = cur.score,
                                max.score = cur.weight)

###################################################################
i<-11



  test_that_var <- test_that(
  'Q2. Is the sweat pvalue in the ballpark?', {
    expect_lt(sweat_p_value, 0.13)
  })

  if(test_that_var == TRUE){
      cur.score <-  cur.weight
      cur.output <- "Test passed\n"
  }else{
    cur.score <- 0
    cur.output <- "Test failed\n"
  }

 tests[["tests"]][[i]] <- list(name = "Q2. Is the sweat pvalue in the ballpark?",
                                output = cur.output,
                                score = cur.score,
                                max.score = cur.weight)

########## Question 3 #########################
#######################################################

i<-12


test_that_var <- test_that(
  'Q3. Is the cards auction a t.test?', 
  expect_s3_class(t_test_cards, 'htest')
)

  if(test_that_var == TRUE){
      cur.score <-  cur.weight
      cur.output <- "Test passed\n"
  }else{
    cur.score <- 0
    cur.output <- "Test failed\n"
  }

 tests[["tests"]][[i]] <- list(name = "Q3. Is the cards auction a t.test?",
                                output = cur.output,
                                score = cur.score,
                                max.score = cur.weight)

###################################################################
i<-13

  test_that_var <- test_that(
  'Q3. Is the cards auction p-value correct?', {
  expect_equal(
    object    = t_test_cards$p.value, 
    expected  = 0.00642, 
    tolerance = 0.001)
  })

  if(test_that_var == TRUE){
      cur.score <-  cur.weight
      cur.output <- "Test passed\n"
  }else{
    cur.score <- 0
    cur.output <- "Test failed\n"
  }

tests[["tests"]][[i]] <- list(name = "Q3. Is the cards auction p-value correct?",
                                output = cur.output,
                                score = cur.score,
                                max.score = cur.weight)

###################################################################
i<-14

  test_that_var <- test_that(
  'Q3. Is the cards ATE correct?', { 
    expect_equal(
      object    = abs(cards_ate), 
      expected  = 12.2, 
      tolerance = 0.1)}
)

#cards_ate was not defined for students
cur.score <-  cur.weight
cur.output <- "Test passed(ng).\n"




#  if(test_that_var == TRUE){
#      cur.score <-  cur.weight
#      cur.output <- "Test passed\n"
#  }else{
#    cur.score <- 0
#    cur.output <- "Test failed\n"
#  }

tests[["tests"]][[i]] <- list(name = "Q3. Is the cards ATE correct?",
                                output = cur.output,
                                score = cur.score,
                                max.score = cur.weight)

###################################################################
i<-15



test_that_var <- test_that(
  'Q3. Is the cards ri distribution reasonable?', { 
    expect_vector(ri_distribution)
    expect_gt(
        object = length(ri_distribution), 
        expected = 99)
    expect_lt(
      object    = abs(quantile(ri_distribution, 0.025)), 
      expected  = 10)
    expect_gt(
      object    = abs(quantile(ri_distribution, 0.025)), 
      expected = 8)
    })


#  test_that_var <- test_that(
#  'Q3. Is the cards ri distribution reasonable?', { 
#    expect_vector(cards_ri_distribution)
#    expect_equal(length(cards_ri_distribution), 1000, .001)
#    expect_lt(
#      object    = abs(quantile(cards_ri_distribution, 0.025)), 
#      expected  = 9.25)
#    expect_gt(
#      object    = abs(quantile(cards_ri_distribution, 0.025)), 
#      expected = 8.75)
#    })

  if(test_that_var == TRUE){
      cur.score <-  cur.weight
      cur.output <- "Test passed\n"
  }else{
    cur.score <- 0
    cur.output <- "Test failed\n"
  }


tests[["tests"]][[i]] <- list(name = "Q3. Is the cards ri distribution reasonable?",
                                output = cur.output,
                                score = cur.score,
                                max.score = cur.weight)

###################################################################
i<-16

  test_that_var <- test_that(
  'Q3. What is the lm estimated for the model?', {
    expect_s3_class(
      object = mod, 
      class = 'lm')
  })  

  if(test_that_var == TRUE){
      cur.score <-  cur.weight
      cur.output <- "Test passed\n"
  }else{
    cur.score <- 0
    cur.output <- "Test failed\n"
  }

tests[["tests"]][[i]] <- list(name = "Q3. What is the lm estimated for the model?",
                                output = cur.output,
                                score = cur.score,
                                max.score = cur.weight)

###################################################################
i<-17

  test_that_var <- test_that(
  'Q3. Is the robust ci correct?', {
    expect_equal(
      object = cards_robust_ci['uniform_price_auction', '2.5 %'], 
      expected = -20.97, 
      tolerance = 0.01)
    expect_equal(
      object = cards_robust_ci['uniform_price_auction', '97.5 %'], 
      expected = -3.44,
      tolerance = 0.01)
      })

#cards_robust_ci was not specified to students
cur.score <-  cur.weight
cur.output <- "Test passed(ng).\n"


#  if(test_that_var == TRUE){
#      cur.score <-  cur.weight
#      cur.output <- "Test passed\n"
#  }else{
#    cur.score <- 0
#    cur.output <- "Test failed\n"
#  }



tests[["tests"]][[i]] <- list(name = "Q3. Is the robust ci correct?",
                                output = cur.output,
                                score = cur.score,
                                max.score = cur.weight)



################################################################
#################################################################
cat.tests <- function(tests){
  for(i in 1:length(tests[["tests"]])){
    cur.name <- test.cases[[i]][["name"]]
    cur.output <- test.cases[[i]][["output"]]
    score <- tests[["tests"]][[i]][["score"]]
    max.score <- tests[["tests"]][[i]][["max.score"]]
    
    
    cat(sprintf("Test %s:\n", cur.name ))
    cat(sprintf("Output:\n %s\n", cur.output))
    cat(sprintf("Score: %g/%g\n", score, max.score))
    cat("====================================================\n\n\n")
                  
  }
}

cat.tests(tests)
write(toJSON(tests, auto_unbox = T), file = json.filename)


