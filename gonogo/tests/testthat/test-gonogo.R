library('gonogo')

test_that('rrt() runs for at least interval time if choice key is not pressed', {
  
  inter <- c()
  time <- c()
  start <- c()
  finish <- c()
  
  for (i in 1:10) {
    inter[i] <- sample(seq(from = 0.4, to = 1.0, by = 0.01), size = 1)
    start[i] <- Sys.time()
    rrt(interval = inter[i])
    finish[i] <- Sys.time()
    time[i] <- (finish[i] - start[i])[[1]]
  }
  expect_true(all(time >= inter))
  
})

test_that('rrt() only accepts the correct format of arguments', {
  
  expect_error(rrt(choice_keys = 1, interval = 0.6))
  expect_error(rrt(choice_keys = "1", interval = 1:5))
  expect_error(rrt(choice_keys = "f", interval = "a"))
  
})

test_that('update_data() only accepts the correct format of arguments', {
  
  data1 <- data.frame(response = rep(" ", 5), rt = rep(0.1, 5),
                      stimulus = rep("A", 5))
  expect_error(update_data(data = data1, stimuli = c("A", "X")))
  
  data2 <- data.frame(response = rep(" ", 5), rt = rep(0.1, 5),
                      stimulus = rep("A", 5), correct = rep(1, 5))
  expect_error(update_data(data = as.matrix(data2), stimuli = c("A", "X")))
  
  expect_error(update_data(data = data2, stimuli = 1:2))
  expect_error(update_data(data = data2, stimuli = "A"))
  
  expect_error(update_data(data = data2, stimuli = c("A", "X"),
                           mean_error = 1:2))
  expect_error(update_data(data = data2, stimuli = c("A", "X"),
                           mean_error = "1"))
  
})

test_that('play_gonogo() only accepts the correct format of arguments', {
  
  expect_error(play_gonogo(id = 1:2))
  
  expect_error(play_gonogo(id = "participant", n_block = "a"))
  expect_error(play_gonogo(id = "participant", n_block = -1))
  expect_error(play_gonogo(id = "participant", n_block = 0.5))
  
  expect_error(play_gonogo(id = "participant", n_trial = "a"))
  expect_error(play_gonogo(id = "participant", n_trial = -1))
  expect_error(play_gonogo(id = "participant", n_trial = 0.5))
  
  expect_error(play_gonogo(id = "participant", stimuli = 1:2))
  expect_error(play_gonogo(id = "participant", stimuli = "A"))
  
  expect_error(play_gonogo(id = "participant", inter = "A"))
  expect_error(play_gonogo(id = "participant", inter = 1:5))
  
  expect_error(play_gonogo(id = "participant", prb = "A"))
  expect_error(play_gonogo(id = "participant", prb = 1:5))
  expect_error(play_gonogo(id = "participant", prb = c(1, 0.1)))
  
})

