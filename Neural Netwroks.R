##############################################
## Author:   Indrajit Haridas
## Date:     2018-26-04
## Title:    Neural Network - Mini Project
## Purpose:  Assignment 3
##############################################

#### (1) Sigmoid activation function####
sigmoid <- function(x){1.0/(1.0+exp(-x))}

#### (2) Binary step activation function####
binary_step <- function(x){
  ifelse(x >= 0, 1, 0)
}

#### (3) Neuron Input####
neuron_input <- function(b, w, x){
  # b is the bias, it is scalar
  # w is the weights vector of length n
  # x is the input variable (independent variable) of length n
  
  b + sum(w*x)
}


#### (4) Neuron with activation function ####

neuron <- function(activation, b, w, x){
  
  fn <- neuron_input(b, w, x)         # neuron input from question number 3
  
  if (activation == 'sigmoid') {
      
    sigmoid(fn)
    
  }
  else if (activation == 'binary step') {
    
    binary_step(fn)
    
  }
  
  else 
    print('Select either sigmoid or binary step function')
  
}


#### (5) XOR Gate ####

# bias and weights of fist nueron
b1 <- -10
w1 <- c(20, 20)

# bias and weights of second nueron
b2 <- 30
w2 <- c(-20, -20)

# bias and weights of final nueron
bf <- -30
wf <- c(20, 20)

# Test cases
x1 <- c(1, 1) # in the form of (x1, x2): from diagram, here variable name is also x1 which is not related to the diagram
x2 <- c(0, 1)
x3 <- c(1, 0)
x4 <- c(0, 0)

# Output for test cases embeded in print statements
# Inside the print statement, we are calling function 'neuron' (from part 4) which internally calls input function from part 3. output is the result of xor gate

# Output for test case I
print(paste0('For input of (1, 1), output is ', neuron('binary step', bf, wf, c(neuron('binary step', b1, w1, x1), neuron('binary step', b2, w2, x1)))))
# Output for test case II
print(paste0('For input of (0, 1), output is ', neuron('binary step', bf, wf, c(neuron('binary step', b1, w1, x2), neuron('binary step', b2, w2, x2)))))
# Output for test case III
print(paste0('For input of (1, 0), output is ', neuron('binary step', bf, wf, c(neuron('binary step', b1, w1, x3), neuron('binary step', b2, w2, x3)))))
# Output for test case IV
print(paste0('For input of (0, 0), output is ', neuron('binary step', bf, wf, c(neuron('binary step', b1, w1, x4), neuron('binary step', b2, w2, x4)))))

