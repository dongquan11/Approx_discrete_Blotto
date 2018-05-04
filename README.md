# Approx_discrete_Blotto
The Colonel Blotto game is a famous game commonly used to model resource allocation problems in many domains ranging from security to advertising. Two players distribute a fixed budget of resources on multiple battlefields to maximize the aggregate value of battlefields they win, each battlefield being won by the player who allocates more resources to it. The continuous version of the game---where players can choose any fractional allocation---has been extensively studied, albeit only with partial results to date. Recently, the discrete version---where allocations can only be integers---started to gain traction and algorithms were proposed to compute the equilibrium in polynomial time; but these remain computationally impractical for large (or even moderate) numbers of battlefields. In this paper, we propose an algorithm to compute very efficiently an \emph{approximate} equilibrium for the discrete Colonel Blotto game with many battlefields. We provide a theoretical bound on the approximation error as a function of the game's parameters. We also propose an efficient dynamic programming algorithm in order to compute for each game instance the actual value of the error. We perform numerical experiments that show that the proposed strategy provides a good approximation to the equilibrium even for a relatively large number of battlefields. 