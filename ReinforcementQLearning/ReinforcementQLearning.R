
#  ------------------------------------------------------------------
#  |FILE NAME:      ReinforcementQLearning.R
#  |DATE:           08/05/17
#  |CREATED BY:     Ezugo Nwosu 
#  |DATA SoURCE:    None   
#  |DATA FILE Name: None
#  |----------------------------------------------------------------


#  |------------------------------------------------------------------
#  |STEPS:               
#  |
#  |  STEP 1:  Define reinforcement learning parameters 
#  |  STEP 2:  Define other parameters - desired state & # of runs
#  |  STEP 3:  Set up the environment rewards in matrix R and visualize it
#  |  STEP 4:  Setup the Q matrix and visualize it
#  |  STEP 5:  Learn from experience 
#  |  STEP 6:  Determine the final state of the Q matrix and visualize it 
#  |------------------------------------------------------------------

#  |------------------------------------------------------------------
#  |OTHER REFERENCES:               
#  |
#  |  1:  https://cran.r-project.org/web/packages/ReinforcementLearning/vignettes/ReinforcementLearning.html
#  |  2:  http://www.mnemstudio.org/path-finding-q-learning-tutorial.htm
#  |  3:  https://stackoverflow.com/questions/39353580/how-to-implement-q-learning-in-r
#  |------------------------------------------------------------------


#  *------------------------------------------------------------------*
#  | STEP 1: Define reinforcement learning parameters - alpha & gamma
#  *------------------------------------------------------------------*

#The Gamma parameter has a range of 0 to 1 (0 <= Gamma > 1). If Gamma is closer to zero, the agent will tend to consider only immediate rewards.
#If Gamma is closer to one, the agent will consider future rewards with greater weight, willing to delay the reward.
gamma <- 0.1

## The learning rate, set between 0 and 1. Setting it to 0 means that the Q-values are never updated and, hence, nothing is learned. 
## Setting a high value, such as 0.9, means that learning can occur quickly.
alpha <- 1

#  *------------------------------------------------------------------*
#  | STEP 2: Define other parameters - desired state & number of runs
#  *------------------------------------------------------------------*

#There are 6 States starting from 0 to 5
desired_state <- 6

# Number of iterations within which to find the desired_state
run_iterations <- 1000

#  *------------------------------------------------------------------*
#  | STEP 3: Set up the environment rewards in matrix R and visualize it
#  *------------------------------------------------------------------*

#Define and initialize the R-matrix 
R <- matrix(c(-1,-1,-1,-1,0,-1,-1,-1,-1,0,-1,0,-1,-1,-1,0,-1,-1,-1,0,0,-1,0,-1,0,-1,-1,0,-1,0,-1,100,-1,-1,100,100),nrow=6)

# Print the R Matrix
print(R)

#  *------------------------------------------------------------------*
#  | STEP 4: Setup the Q matrix and visualize it
#  *------------------------------------------------------------------*
## Define the Q matrix to have the same size as R and initialize the Q by setting it to the zero matrix
Q <- matrix(rep(0,length(R)), nrow=nrow(R))

# Print the Q Matrix
print(Q)

#  *------------------------------------------------------------------*
#  | STEP 5: Learn from experience 
#  *------------------------------------------------------------------*
# The algorithm below is used by the agent to learn from experience. Each iteration (episode) is equivalent to one training 
# session. In each training session, the agent explores the environment (represented by matrix R ), receives the reward (if any) 
# until it reaches the goal state.
q.learn <- function(R, run_iterations, alpha, gamma, desired_state) {

  for (i in 1:run_iterations) {
    
    # For each episode, select a random initial state
    current_state <- sample(1:nrow(R), 1)
    
    # Do While the goal state hasn't been reached.
    while (1) {
      
      # Select one among all possible actions for the current state.
      next_state_to_go_to <- which(R[current_state,] > -1)
      
      # Using this possible action, consider going to the next state.
      if (length(next_state_to_go_to)==1)
        next_state <- next_state_to_go_to
      else
        next_state <- sample(next_state_to_go_to,1)
      
      # Compute: Q(state, action) = R(state, action) + Gamma * Max[Q(next state, all actions)]
      Q[current_state,next_state] <- Q[current_state,next_state] + alpha*(R[current_state,next_state] + gamma*max(Q[next_state, which(R[next_state,] > -1)]) - Q[current_state,next_state])
      
      # End the while loop only once the desired state is reached.     
      if (next_state == desired_state) break
      current_state <- next_state
    }
  }
  # Divide the Q by the max value
  return(100*Q/max(Q))
}

#  *------------------------------------------------------------------*
#  | STEP 6: Determine the final state of the Q matrix  and visualize it
#  *------------------------------------------------------------------*
# Define the Q-matrix but don't initialize it yet
Q <- q.learn(R,run_iterations,alpha,gamma,desired_state) # Remember, we set the initial value of the learning parameter Gamma = 0.8       

# Print the Q matrix. Note that it was divided by max(Q) in order to normalize it
print(Q)



