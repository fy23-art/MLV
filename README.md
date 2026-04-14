# MLV
A touch-by-touch point win probability model for MLV

In the fast-paced environment of Major League Volleyball (MLV), traditional metrics often fail to capture the nuanced contributions of players within the flow of a rally. Standard statistics like hitting percentage offer a binary view of success—kills versus errors—but ignore the incremental value of a "good" touch that sets up a teammate or a "bad" touch that puts the defense in a hole. This analysis implements a touch-by-touch win probability model to move beyond these limitations. By quantifying how each action shifts the likelihood of winning a point, we can identify players who consistently improve their team's position, providing a more comprehensive tool for player evaluation and in-game strategy than traditional box scores allow.

Methodology

The progression of a volleyball point is modeled as a second-order Markov chain. In this framework, the probability of transitioning to a future state is dependent on the two immediately preceding states. The state St at any touch t is defined by a tuple of four variables St = (Tt, Rt, At, Ot), where

Tt∈ {Serving, Recieving} represents the team currently making the touch (serving or receiving),
Rt∈ {1,2,3} represents the number of touches remaining,
At∈ {S,R,E,A,B,D,F} indicated the action type ((Serve, Reception, Set, Attack, Block, Dig, or Freeball),
Ot∈ {#,+, !,-,/} represents the subjective outcome rating of that touch.

We estimate the transition probability P between states using the second-order assumption  P(St+1|St,St-1). The model includes two terminal states: S{win, S} (Serving team wins) and S{win, R} (Receiving team wins). The Sideout Probability is c the expected value of reaching the receiving-team terminal state:

WPRec(S) = P(Receiving Team Wins | Current State = S).

For attackers, the Win Probability Added (WPA) is WP = WPafter-WPbefore

