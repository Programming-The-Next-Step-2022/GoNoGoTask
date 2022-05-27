library(gonogo)

context('Validate_user_response functionality')

test_that('validate_user_response return value depends on whether choice and choiceKeys match') {

  random_letters <- sample(x = letters, size = 3, replace = FALSE)
  choice_keys <- random_letters[1:2]
  choice_match <- sample(choice_keys, 1)
  choice_unmatch <- random_letters[3]

  result1 <- validate_user_response(choice = choice_match, choiceKeys = choice_keys)
  result2 <- validate_user_response(choice = choice_unmatch, choiceKeys = choice_keys)

  expect_false(result1[1] == result2[1])

})


