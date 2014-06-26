
#HelloWorldOpen 2014 Need for C code
This is the winning code of HelloWorldOpen competition. I will try to explain the competition, code structure and strategies below.

##About the competition
The basic idea of the competition was to code AI to compete in slot car racing. The rules were simple. At the beginning you were given the track information, and then 60 times per second you receive information on all cars positions. The only decisions you can make is set Throttle to any value between 0 and 1, switch lane to right or left, or fire turbo once in a while. The only constraint that you have to follow is to keep your drift angle below 60 degrees, as above that your car crashes. You can read more about the rules and protocol in [problem technical specification](https://helloworldopen.com/techspec).

##Running the code
####Build
Cmake build: `./build`
To build with just pure makefile (without cmake) use `./build --skip_cmake`

####Running client
In home directory run:
> ./default_run [flags]

example:
> ./default_run --host=hakkinen.helloworldopen.com --bot=stepping --num_players=2 --track=imola

For more flags run --help

####Running simulator
Our local simulator supports any track provided in json format. However It only support one-car race, all actions are possible (throttle, turbo and switches).

Usage:

> ./throttle_tester --track=trackfilename --physics=physicssetnumber


##Architectural decisions
The main decision to make was the choice of language. The protocol required answers within ~5ms so efficency was crucial. The above, and our love to C++ made us choose  it as the bot language.

This repository contains many bots that we used to test our code over the time, and the one that is optimal is called **stepping** located under *cpp/bots/stepping/*

##Physics used for simulation
One part of the competition was to figure out physics behind certain parts of simulation. I'll try to present them below.

###Velocity
Velocity of the car followed a simple formula

> Velocity(t) = Velocity(t - 1) * X + Throttle * Y

This way two first ticks allowed us to calculate the velocity parameters (X and Y)

###Drift angle
Drift angle physics was a little bit more difficult but easy enough to follow.

Each state of the car (its velocity and radius of the bent that the car is on) defines so called **target angle** computed using following formula:

> TargetAngle(t) = max(0, X * Velocity(t) / sqrt(BentRadius) - Y))

given target angle that the car *wants* to reach, we can compute actual drift angle in the next tick with this formula:

> Angle(t+1) = Angle(t) + A * (Angle(t) - Angle(t-1)) - B * Velocity(t) * (TargetAngle - Angle(t))


Having those formulas allowed us to save each tick of data as an equation, and once we received 3 ticks of data we computed the approximation of all parameters using LS fit algorithm.

###Turbo

Tubo follows very simple rule specified in technical rules: Each turbo is defined by its **duration** and its **modifier**. Once you send Turbo command your throttle value is multiplied by **modifier** for next "**duration**" ticks.

###Bumps

Bump of two or more cars happen if in any moment distance between two cars is less than length of the car.

Lets say car A is behind car B. If newly calculated positions cause distance between them to be less than CarLength, states of those cars are recalculated as follows:

> new car A position stays the same as before

> new car B position = car A position + CarLength

> new car A speed = car B speed * 0.8

> new car B speed = car A speed * 0.9


###Switches 
All switches are modelled as bezier curves. Both lengths and curvature is computed as an approximation.

More in-depth explaination coming soon.

 
##General racing idea

We decided to split the code into separate parts that are (atleast was supposed to be) separated from each other. Before getting into more details I will describe roughly the general  purpose of most of them.


The code is divided into two main parts - recording part managing all physics parameters, predictions and enemy tracking, and decisive part, that figures out the most optimal command.

###Recording code
#### CarTracker
Main class that manage all physics parameters, approximations, and length/radius predictions is **CarTracker**. At the beginning of each *CarPositions* message, **Record** command is issued to save data about the position and generate **CarState** containing all data about car like position velocity, angle etc. All data about position if forwarded to physics models (**VelocityModel**, **DriftModel**, **LaneLengthModel** etc) that take care of figuring out all necessary parameters.

After enough data is collected, **CarTracker** provides some of the most important methods like:

* **Predict** returning a CarState after issuing given command.
* **DistanceBetween** returning real distance between two positions
* **IsSafe** checking if the state is safe. Safe state means that there exist a sequence of commands that lead us to safe state (with velocity of 2).

And bump safety methods (which probably shouldnt be here but due to short deadline got here for convenience)

* **CanBumpAfterNTicks**
* **IsBumpInevitable**
* **IsSafeAttack**
* **IsSafeBehind**

####RaceTracker
**RaceTracker** works in the similar way to **CarTracker**, but instead of predicting general events, it save and predict all cars behavior and interactions. 

It provides car interaction methods like:

* **BumpOccured** checking if bump occured between two cars now
* **ScoreLane** returning score of the lane based on cars there (more details below)

Moreover it hold **EnemyTracker** object for each car, keeping track of its death state, spawn times or lap times. But the most important part is **VelocityPredictor** that keep track of all car states that occured during the race, and use that data to predict car velocity in any position on the map. Thats why there are two important methods:

* **ExpectedVelocity** returning expected velocity of the car in given position the track
* **TimeToPosition** returning expected time for car to reach given position.


###Decision making
Decision process was divided into 4 separate entities that optimized some part of the racing. After computing optimal choices from *their* perspective, all that data was joined into final decision.
Those parts are as follows:

* **BumpScheduler** - Making decisions if we should attack somebody in front of us or not
* **ThrottleScheduler** - Calculating the most optimal throttle to use in the next ~40 ticks.
* **SwitchScheduler** - Calculating the most optimal switch to take based on lane lengths and cars that are or can be on them.
* **TurboScheduler** - Calculating the most optimal place to use turbo

All those schedulers were separated from each other, and connected in one point of the code. Decision making based on aforementioned schedulers were made as follows:

1. If **BumpScheduler** scheduled an attack, do it without checking any other scheduler. Attacks has highest priority, as once attack is scheduled, we are 100% sure that we will be safe and hit the guy. As attacks as almost never suboptimal, we prioritize them above anything else.
2. If there is no attack, we proceed to schedule turbo with **TurboScheduler**. If scheduler says that now is the perfect moment to use it, we do it right away without checking other schedules.
3. After that switch/throttle combination follows. **SwitchScheduler** is ther first one to invoke. It scores all possible decisions to make on the next switch, and save the best one together with the information how much time do we have to issue switch command (if necessary).
Then **ThrottleScheduler** is invoked. It schedules the next ~40 throttle values. If there was any switch scheduled by **SwitchScheduler** it also schedule it in the most optimal place.
4. After those schedules **ThrottleScheduler** says if its time to switch - then we issue appropriate Switch command, otherwise we send scheduled throttle command.

As many of you may notice, commands issued under points 2 - 4 may not always be safe.  Thats why after figuring out the theoretical most optimal command, we check if its safe by using **IsSafeAhead** method. It checks if issuing given command can lead to bumping into another car, that would render our state unsafe. Thats why we never crash after bumping another car.

All of the above is managed by **BulkScheduler**.

Now that you can understand general idea of the algorithm we will reveal some details behind certain schedulers.

##Throttle
The ThrottleScheduler has two responsibilities. For a given tick it decides:
1. what throttle value should be set and
2. whether to send the switch command.

Once executed it searches for a schedule that maximizes the total distance driven in the next 41 ticks. (The schedule must also include a place to make a switch, if requested). The search procedure consists of two main phases: i) branch and bound and ii) local search. 

The ThrottleScheduler always tries to return a schedule that is safe. This is possible unless some unfortunate interaction with other cars happened.

### Branch and bound
In this phase we use binary throttle values only. The solution space consists of 2^41 points, and searching it in 5ms time is computationally unfeasible. In order to cope with it we join consecutive ticks into groups. All ticks in a group will get the same throttle value. The lengths of the groups we experimentally found to work the best are following: 1, 1, 4, 2, 2, 4, 1, 8, 8, 4, 4, 2. The search space reduced in this way consists of 2^12 schedules, which is still a bit too much for an exhaustive search.

The branch and bound procedure expands the search tree using a simple recursion (DFS). The search tree is prunned in two ways. First if at the given node the drift angle is higher than the crash angle, expanding the node is not necessary. Second, at each node, we compute the lower bound of the total schedule distance by applying a sequence of zeros from the given node to the end of the schedule. If the computed value is less than the distance of the currently best known schedule (upper bound) the branch is cut.

If the schedule at the leaf node is safe, we update the upper bound of the distance.

It is also important to note that before executing the branch and bound procedure, we compute the initial upper bound by using the schedule found in the previous tick shifted by one tick to the left. Such initial schedule is, in the vast majority of situations, safe and the branch and bound procedure rarely can improve it. In cases, where there is a new switch planned, a turbo has been turned on or there was a bump, the initial schedule could be not safe. Before we replace it with a sequence of zeros, we first try to repair it with some simple heuristics.

If required, the switch is scheduled at the last possible place in the schedule.

### Local search
The local search procedure starts from the schedule returned by the branch and bound. It starts from the first throttle in the schedule. While the resulting schedule is safe, it increases the throttle 0.1. If this is no longer possible, it moves to the next throttle and repeat the procedure. This simple procedure makes the schedule contain non-binary throttle values improving the lap-time by 2-3 ticks (0.032-0.048s) on average.

Overally, the ThrottleScheduler takes around 0.5ms on average on a modern Macbook Pro.

##Switching
Switch decisions made in **SwitchScheduler** are based solely on the lane scores. Those scores are decided based on two distinct scorers. The first one, optimizes the length of the chosen path. Second one, takes all cars under consideration, and checks if they can interrupt us. Those scores are then combined into total score and then the best lane that is safe to switch (that there exist a throttle mask that wont crash us) is chosen.

###Length based scores
We decided to use length based heuristic to asses how optimal given path is. After trying some real simulations we decided that figuring out scores based on real driving time would require huge amount of computation, as the racing time through the lane differs based on entrance and exit velocity/angle.

All lane scores are precomputed. At first, all lane lengths (all possible combinations of [from_lane][to_lane][from_switch] to the next switch) are saved.

Then optimal lap length is calculated, so for each position on the track we have length of the full lap ending in any lane. This way, for each position we can assign *decision scores* representing how much time (length-based) will we loose if we choose given direction, compared to the optimal one. This way one of the scores (left, center, right) is always 0 and others are >=0.


###Car based scores
The main idea behind car based scores is to check if (or how much) particular car is threat to me if I choose certain direction. I will use *target switch* as the switch we want to make decision for, and *end switch* as next switch defining the distance span that has to be safe in order to make certain decision.

First, for each car we check who is closer to the *end switch*. If we are closer than him, we ignore him for now. This is one of assumptions we could make based on the fact that we have fastest throttle algorithms. The only problem here was guys that we switched into - this case wasn't managed by this part of the code. On the other hand, if the car was closer to the *end switch* we scored it based on if its dead or alive.

#####Scoring living enemy
To score living enemy, we simulate movements of both our car, and enemy car. We generate all car states from now until the *end switch*. Then we check if there is intersection of those states (a.k.a a bump occurs).

If the bump doesnt occur based on the prediction, we return neutral score (as if noone was there).

If the bump occurs, we check what velocities of both cars in the moment of the bump, and score the lane based on the ratio of cars velocity. If my velocity is greater in the moment of the bump, it means that it will probably slow us down.

In addition, if there was N consecutive bumps between me and given enemy in the M past ticks, we force overtake as its probably a case where we push an enemy (previous check doesn't solve that, as if we are already behind en enemy, we wouldnt be able to accelerate enough to create big enough difference in velocity.

#####Scoring dead enemy

Dead enemies are only a threat if they spawn before we pass them. Thats why we simulate our movement until *end switch* and check if in the moment of enemy spawn we are atleast 3 * CarLength ahead. We give that much mistake margin due to a fact that hitting a spawning enemy almost always result in a crash. Its highest priority to avoid that, and margin of that size prevents us from making any mistakes.


































































##Attacking

All cars should crash!

Attack is an action that is meant to do everything possible to cause another car crash. There is simple bulk mechanism to make those decisions, that use a sophisticated prediction method called `IsSafeAttack`

There are two basic types of attacks, bump with turbo and bump without turbo. In most of the cases, turbo bump is not necessary, as regular bump is enough - especially on tracks with longer straights. 

Before making decision on each tick's command, we check if there is a possibility to attack. An attack desicion that is made any time is supposed to be always successful. In other words, if we decide that we can attack someone we are theoretically 100% sure that the bump will happen, and enemy will crash. The *theoretical* part comes from some very rare possibilities of three-way-bumps or unexpected bumps of me or enemy.

The attack check procedure is simple:

* IIf we have target already locked:
 * If attack is still safe, follow algorithm to execute attack safely and successfully
 * If attack is no longer safe (probably someone hit us or he hit somebody and changes the parameters of the bump), try to save yourself and unlock the target.
* If there is no locked target
 * Check if its possible to execute regular bump, and lock target if necessary
 * If its not possible to do regular bump, check for turbo bump possibilities. If the enemy is *worth bumping* then lock target on him and use turbo.

####IsSafeAttack method

*Work in progres. For now on you can chceck CarTracker::IsSafeAttack method for details*

##Checking safety

*Work in progress. For now on you can check RaceTracker::IsSafeAhead method for details*

## Other details worth noting
###Enemy movement prediction
We use specific mechanism to predict movement of enemies (and ourselves). It's crucial to switch decisions, as it allows us to predict if the lane will be congested, and what effect does it have on us.

`VelocityPredictor` holds information on so-called `ExpectedVelocity` which defines what is the most probable velocity of a car in given place. Given car state and target position, we can calculate all transition states of the car. Its mainly used to:

* Check if we can pass another dead car before he spawns
* Check if two cars will bump if they follow the same lane
* Check if two cars will bump if any lane switch happens
* Check the velocity of cars during the future bump to assess how bad it is for faster car (us)

Starting from the first position, we check what is the expected velocity of the car, and check if its possible to issue a throttle that transform current velocity to target velocity. If its possible, we issue it and repeat the process with the newly calculated state. If its not possible, we issue 1 (or 0) throttle to reach target velocity as fast as possible. Its important in case of accelerating from a crash, or slowing down after bump. We stop after reaching target position.

Its worth noting that the approximation is not 100% accurate, as we skip all drift angle validation during the process. We also use the top velocity of the car in given position, which may theoreticaly cause some inaccuracies. Anyway, It never happened that the prediction was mistaken by much enough to cause any decision miscalculation.

####Velocity predictor
`Velocity predictior` role is to save state of each car in order to predict their velocity in each point of the map. Its important not to feed the predictor with *dirty* data, which iclude:

* Short time after car spawn (accelerating time)
* Short time before car crash (it was surely going too fast)
* Short time after moment of bump involving given car (the car could be hit and steal too high velocity.
* Short time before car bump (the car could accelerate wrongly just to hit another car, providing too high expected velocity value)

Predictior  saves only points, that provide *higher* velocity values than the previous ones. In other words, for each lane of the track, we save fastest point and *connect* them with lines. If we run another lap, and our current *connected line* crosses the old line created from old points, it means that the car started to drive faster, and we have to override old (slower) points with new data, until the new data gets slower. This way we save the fastest theoretical velocity of the car in given point. Once we are asked for a velocity in given point, we return the value that corresponds to value on the aforementioned line connecting points.

If we do not have any data in given piece of track, we just save it without checking if its faster. 
Also, if we ask for expected velocity on lane X, and we have only data on lane Y, we just take corresponding point on the lane we have data on, and take the velocity from there.

Generally the top value method works, expecially with the method used to predict velocidy, when we dont immediately use expected velocity value, but check if there is a possible throttle to satisfy it.

Surprisingly, it also works in most turbo cases. Lets say we have spoiled top velocity values with turbo data. Its really high, but if the car doesnt have turbo on, the only thing we will do is issuing throttle(1) commands, which actually is possible (as if its possible to go even faster with turbo, you can freely drive normal 1 throttle safely). The moment we have to brake below topspeed will happen in the same moment, as optimal entrance velocity is very similar to the case with and without trubo. On the other hand if the enemy really have used turbo there, our state predictor will treat 1 throttle as turbo-modified throttle, so the prediction will include turbo acceleration and will work very well.

The only case that may not be managed too well, is when we have non-turbo data and enemy uses turbo there. We decided not to make any additional decision in this situation, as we assumed that after some number of laps, we will have quite accurate data. In other words, turbo usage of cars is very regular. It car used turbo in the past on straight X and straight Y only, its highly probable that he will only use it there (except for bumping which we don't care for as we don't record it). The predictor never failed for us (on finals and on trainings), so our assumptions turned out to be quite accurate.


