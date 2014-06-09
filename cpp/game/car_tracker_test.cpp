#include <iostream>

#include "game/car_tracker.h"
#include "gtest/gtest.h"

using jsoncons::json;

namespace game {

class CarTrackerTest : public testing::Test {
 protected:
  const double kEps = 1e-9;

  void SetUp() {
    json game_init_json = json::parse_file("data/gameInit.json");
    const auto& race_json = game_init_json["data"]["race"];

    race_.ParseFromJson(race_json);
    car_tracker_.reset(new CarTracker(&race_, PhysicsParams()));
  }

  Command ParseCommand(const json& data) {
    if (data[0]["msgType"] == "throttle")
      return Command(data[0]["data"].as_double());
    if (data[0]["msgType"] == "switchLane") {
      if (data[0]["data"] == "Right")
        return Command(Switch::kSwitchRight);
      return Command(Switch::kSwitchLeft);
    }
    if (data[0]["msgType"] == "turbo")
      return Command(TurboToggle::kToggleOn);
    return Command();
  }

  Race race_;
  std::unique_ptr<CarTracker> car_tracker_;
};

class DistanceBetweenTest : public CarTrackerTest {
};

TEST_F(DistanceBetweenTest, TheSamePiece) {
  Position position1;
  position1.set_piece_distance(25.0);

  Position position2;
  position2.set_piece_distance(75.0);
  EXPECT_NEAR(50.0, car_tracker_->DistanceBetween(position1, position2), kEps);
}

TEST_F(DistanceBetweenTest, TheNextPiece) {
  Position position1;
  position1.set_piece_distance(25.0);

  Position position2;
  position2.set_piece(2);
  position2.set_piece_distance(75.0);
  EXPECT_NEAR(250.0, car_tracker_->DistanceBetween(position1, position2), kEps);
}

TEST_F(DistanceBetweenTest, AroundTheTrack) {
  const double kTrackLength = 3399.7874452256719;
  Position position1;
  position1.set_piece_distance(75.0);

  Position position2;
  position2.set_piece_distance(25.0);
  EXPECT_NEAR(kTrackLength + 50, car_tracker_->DistanceBetween(position1, position2), kEps);
}

TEST_F(DistanceBetweenTest, AroundTheTrackWithMaxDistance) {
  const double kTrackLength = 3399.7874452256719;
  Position position1;
  position1.set_piece_distance(75.0);

  Position position2;
  position2.set_piece_distance(25.0);
  EXPECT_GE(car_tracker_->DistanceBetween(position1, position2, nullptr, 100), 100);
  EXPECT_LT(car_tracker_->DistanceBetween(position1, position2, nullptr, 100), 200);
}

TEST_F(DistanceBetweenTest, WeCanSwitchTheLane) {
  Position position1;
  position1.set_piece_distance(0.0);

  Position position2;
  position2.set_piece(4);
  position2.set_start_lane(1);
  position2.set_end_lane(1);
  position2.set_piece_distance(0.0);
  EXPECT_NEAR(401.9803902718557, car_tracker_->DistanceBetween(position1, position2), kEps);
}

TEST_F(DistanceBetweenTest, BothOnTheSameSwitch) {
  Position position1;
  position1.set_piece(3);
  position1.set_start_lane(0);
  position1.set_end_lane(1);
  position1.set_piece_distance(5.0);


  Position position2;
  position2.set_piece(3);
  position2.set_start_lane(0);
  position2.set_end_lane(1);
  position2.set_piece_distance(10.0);
  EXPECT_NEAR(5.0, car_tracker_->DistanceBetween(position1, position2), kEps);
}

TEST_F(DistanceBetweenTest, BothOnTheSameSwitchDifferentLanes) {
  Position position1;
  position1.set_piece(3);
  position1.set_start_lane(0);
  position1.set_end_lane(1);
  position1.set_piece_distance(5.0);


  Position position2;
  position2.set_piece(3);
  position2.set_start_lane(0);
  position2.set_end_lane(0);
  position2.set_piece_distance(10.0);
  EXPECT_NEAR(3442.5238274753829, car_tracker_->DistanceBetween(position1, position2), kEps);
}

TEST_F(DistanceBetweenTest, TwoOtherSwitches) {
  Position position1;
  position1.set_piece(3);
  position1.set_start_lane(0);
  position1.set_end_lane(1);
  position1.set_piece_distance(5.0);


  Position position2;
  position2.set_piece(8);
  position2.set_start_lane(1);
  position2.set_end_lane(0);
  position2.set_piece_distance(10.0);
  EXPECT_NEAR(389.72372909493703, car_tracker_->DistanceBetween(position1, position2), kEps);
}

TEST_F(CarTrackerTest, GreedyRun) {
  auto history = json::parse_file("data/greedyRun.json");
  auto commands = history["commands"];
  auto positions = history["positions"];

  for (int i = 0; !car_tracker_->IsReady() && i < commands.size(); ++i) {
    Command command = ParseCommand(commands[i]);
    Position position;
    position.ParseFromJson(positions[i]);

    car_tracker_->Record(position);
    car_tracker_->RecordCommand(command);
  }

  ASSERT_TRUE(car_tracker_->IsReady());

  CarState state;
  for (int i = 0; i < positions.size() - 1; ++i) {
    Command command = ParseCommand(commands[i]);
    Position position;
    position.ParseFromJson(positions[i + 1]);

    state = car_tracker_->Predict(state, command);

    EXPECT_NEAR(position.angle(), state.position().angle(), kEps) << i << "expected: " << position.DebugString() << " actual: " << state.DebugString();
    EXPECT_NEAR(position.piece_distance(), state.position().piece_distance(), kEps);
    EXPECT_EQ(position.piece(), state.position().piece());
    EXPECT_EQ(position.start_lane(), state.position().start_lane());
    EXPECT_EQ(position.end_lane(), state.position().end_lane());
    EXPECT_EQ(position.lap(), state.position().lap());
  }
}

TEST_F(CarTrackerTest, TurboRun) {
  auto history = json::parse_file("data/turboRun.json");
  auto commands = history["commands"];
  auto positions = history["positions"];

  for (int i = 0; i < 10; ++i) {
    Command command = ParseCommand(commands[i]);
    Position position;
    position.ParseFromJson(positions[i]);

    car_tracker_->Record(position);
    car_tracker_->RecordCommand(command);
  }

  // Note: We ride too slow to simulate the angles, so car_tracker is not ready.

  CarState state;
  for (int i = 0; i < positions.size() - 1; ++i) {
    Command command = ParseCommand(commands[i]);
    Position position;
    position.ParseFromJson(positions[i + 1]);

    state = car_tracker_->Predict(state, command);
    if (i % 600 == 0) {
      state.AddNewTurbo(Turbo(30, 3.0));
    }

    EXPECT_NEAR(position.angle(), state.position().angle(), kEps);
    EXPECT_NEAR(position.piece_distance(), state.position().piece_distance(), kEps) << position.DebugString();
    EXPECT_EQ(position.piece(), state.position().piece());
    EXPECT_EQ(position.start_lane(), state.position().start_lane());
    EXPECT_EQ(position.end_lane(), state.position().end_lane());
    EXPECT_EQ(position.lap(), state.position().lap());
  }
}

TEST_F(CarTrackerTest, TurboRun2) {
  const double kEps = 1e-9;
  auto history = json::parse_file("data/turboRun.json");
  auto commands = history["commands"];
  auto positions = history["positions"];

  for (int i = 0; i < positions.size() - 2; ++i) {
    Command command = ParseCommand(commands[i]);
    Position position;
    position.ParseFromJson(positions[i]);

    car_tracker_->Record(position);
    car_tracker_->RecordCommand(command);

    if (i % 600 == 0) {
      car_tracker_->RecordTurboAvailable(Turbo(30, 3.0));
    }

    if (car_tracker_->IsReady()) {
      auto next = car_tracker_->Predict(car_tracker_->current_state(), command);
      Position next_position;
      next_position.ParseFromJson(positions[i+1]);

      EXPECT_NEAR(next_position.angle(), next.position().angle(), kEps);
      EXPECT_NEAR(next_position.piece_distance(), next.position().piece_distance(), kEps) << next_position.DebugString() << std::endl << next.position().DebugString();
      EXPECT_EQ(next_position.piece(), next.position().piece());
      EXPECT_EQ(next_position.start_lane(), next.position().start_lane()) << next_position.DebugString() << std::endl << next.position().DebugString();
      EXPECT_EQ(next_position.end_lane(), next.position().end_lane()) << next_position.DebugString() << std::endl << next.position().DebugString();
      EXPECT_EQ(next_position.lap(), next.position().lap()) << i;
    }
  }
}

TEST_F(CarTrackerTest, SwitchRun) {
  auto history = json::parse_file("data/switchRun.json");
  auto commands = history["commands"];
  auto positions = history["positions"];

  for (int i = 0; i < positions.size() - 2; ++i) {
    Command command = ParseCommand(commands[i]);
    Position position;
    position.ParseFromJson(positions[i]);

    car_tracker_->Record(position);
    car_tracker_->RecordCommand(command);

    if (car_tracker_->IsReady()) {
      auto next = car_tracker_->Predict(car_tracker_->current_state(), command);
      Position next_position;
      next_position.ParseFromJson(positions[i+1]);

      EXPECT_NEAR(next_position.angle(), next.position().angle(), kEps);
      double error = position.lap() == 0 ? 3 : (position.lap() == 1 ? 0.002 : kEps);
      EXPECT_NEAR(next_position.piece_distance(), next.position().piece_distance(), error);
      EXPECT_EQ(next_position.piece(), next.position().piece());
      EXPECT_EQ(next_position.start_lane(), next.position().start_lane()) << next_position.DebugString() << std::endl << next.position().DebugString();
      EXPECT_EQ(next_position.end_lane(), next.position().end_lane()) << next_position.DebugString() << std::endl << next.position().DebugString();
      EXPECT_EQ(next_position.lap(), next.position().lap()) << i;
    }
  }
}

TEST_F(CarTrackerTest, SwitchRun2) {
  auto history = json::parse_file("data/switchRun.json");
  auto commands = history["commands"];
  auto positions = history["positions"];

  // Train
  int game_tick;
  for (game_tick = 0; car_tracker_->current_state().position().lap() == 0; ++game_tick) {
    Command command = ParseCommand(commands[game_tick]);
    Position position;
    position.ParseFromJson(positions[game_tick]);

    car_tracker_->Record(position);
    car_tracker_->RecordCommand(command);
  }

  // Predict everything until the next switch
  CarState state = car_tracker_->current_state();
  for (; state.position().piece() < 9; ++game_tick) {
    Command command = ParseCommand(commands[game_tick - 1]);
    Position position;
    position.ParseFromJson(positions[game_tick]);

    state = car_tracker_->Predict(state, command);

    EXPECT_NEAR(position.angle(), state.position().angle(), kEps);
    EXPECT_NEAR(position.piece_distance(), state.position().piece_distance(), position.lap() == 1 ? 0.002 : kEps);
    EXPECT_EQ(position.piece(), state.position().piece()) << position.DebugString() << std::endl << state.position().DebugString();
    EXPECT_EQ(position.start_lane(), state.position().start_lane()) << position.DebugString() << std::endl << state.position().DebugString();
    EXPECT_EQ(position.end_lane(), state.position().end_lane()) << position.DebugString() << std::endl << state.position().DebugString();
    EXPECT_EQ(position.lap(), state.position().lap());
  }
}

TEST_F(CarTrackerTest, BoundaryThrottle1) {
  double throttle;
  EXPECT_TRUE(car_tracker_->BoundaryThrottle(car_tracker_->current_state(), &throttle));
  EXPECT_NEAR(1, throttle, 1e-9);
}

TEST_F(CarTrackerTest, BoundaryThrottle2) {
  double throttle;
  Position position;
  position.set_piece(3);
  position.set_piece_distance(95);
  double velocity = 5.0;
  CarState state(position, velocity, 0, Switch::kStay, 0, TurboState());

  EXPECT_TRUE(car_tracker_->BoundaryThrottle(state, &throttle));
  EXPECT_NEAR(0.5, throttle, 1e-9);
}

TEST_F(CarTrackerTest, BoundaryThrottleTheSamePiece) {
  double throttle;
  Position position;
  position.set_piece(2);
  position.set_piece_distance(95);
  double velocity = 5.0;
  CarState state(position, velocity, 0, Switch::kStay, 0, TurboState());

  EXPECT_TRUE(car_tracker_->BoundaryThrottle(state, &throttle));
  EXPECT_NEAR(1, throttle, 1e-9);
}

TEST_F(CarTrackerTest, BoundaryThrottleOnSwitch) {
  double throttle;
  Position position;
  position.set_piece(3);
  position.set_start_lane(0);
  position.set_end_lane(1);
  position.set_piece_distance(95);
  double velocity = 5.0;
  CarState state(position, velocity, 0, Switch::kStay, 0, TurboState());

  EXPECT_FALSE(car_tracker_->BoundaryThrottle(state, &throttle));
}

class GetCurvesTest : public CarTrackerTest {
};

TEST_F(GetCurvesTest, Basic) {
  Position position;
  position.set_piece(3);
  position.set_piece_distance(95);
  CarState state(position, 0, 0, Switch::kStay, 0, TurboState());

  vector<CarTracker::Curve> curves = car_tracker_->GetCurves(state, 500);
  ASSERT_EQ(4, curves.size());

  EXPECT_EQ(0, curves[0].direction);
  EXPECT_NEAR(0, curves[0].radius, 1e-9);
  EXPECT_NEAR(0, curves[0].distance, 1e-9);

  EXPECT_EQ(-1, curves[1].direction);
  EXPECT_NEAR(110, curves[1].radius, 1e-9);
  EXPECT_NEAR(5, curves[1].distance, 1e-9);

  EXPECT_EQ(-1, curves[2].direction);
  EXPECT_NEAR(210, curves[2].radius, 1e-9);
  EXPECT_NEAR(350.57519189487726, curves[2].distance, 1e-9);

  EXPECT_EQ(0, curves[3].direction);
  EXPECT_NEAR(0, curves[3].radius, 1e-9);
  EXPECT_NEAR(433.04199905160931, curves[3].distance, 1e-9);
}

class PredictPositionTest : public ::testing::Test {
 protected:
  const double kEps = 1e-9;

  void SetUp() {
    json race_json;
    race_json["cars"] = json(json::an_array);
    race_json["raceSession"] = json();
    race_json["track"] = json::parse_file("data/maps/suzuka.json");

    race_.ParseFromJson(race_json);
    car_tracker_.reset(new CarTracker(&race_, PhysicsParams()));
  }

  Race race_;
  std::unique_ptr<CarTracker> car_tracker_;
};

TEST_F(PredictPositionTest, Basic) {
  Position position;

  Position result = car_tracker_->PredictPosition(position, 10.0);

  EXPECT_EQ(0, result.piece());
  EXPECT_EQ(10.0, result.piece_distance());
}

TEST_F(PredictPositionTest, SkipPiece) {
  // Track
  // piece=0 length=100
  // piece=1 length=100
  // piece=2 length=32
  // piece=3 length=?
  Position position;
  position.set_piece_distance(20);

  Position result = car_tracker_->PredictPosition(position, 213.0);

  EXPECT_EQ(3, result.piece());
  EXPECT_EQ(1.0, result.piece_distance());
}

TEST_F(PredictPositionTest, IncreaseLap) {
  ASSERT_EQ(65, race_.track().pieces().size());
  // Track
  // piece=64 length=33
  // piece=0 length=100
  Position position;
  position.set_piece(64);
  position.set_piece_distance(0);

  Position result = car_tracker_->PredictPosition(position, 40.0);

  EXPECT_EQ(0, result.piece());
  EXPECT_EQ(7.0, result.piece_distance());
  EXPECT_EQ(1, result.lap());
}

TEST_F(PredictPositionTest, OnSwitch) {
  ASSERT_NEAR(81.028059516725861,
              car_tracker_->lane_length_model().SwitchOnTurnLength(110, 90, 45),
              1e-5);

  // Track
  // piece=3 length=81.028059516725861 (switch 110, 90, 45)
  // piece=4 length=...
  Position position;
  position.set_piece(3);
  position.set_start_lane(0);
  position.set_end_lane(1);
  position.set_piece_distance(80.028059516725861045);

  Position result = car_tracker_->PredictPosition(position, 5);

  EXPECT_EQ(4, result.piece());
  EXPECT_EQ(4.0, result.piece_distance());
  EXPECT_EQ(1, result.start_lane());
  EXPECT_EQ(1, result.end_lane());
}

TEST_F(PredictPositionTest, TargetLine) {
  // Track
  // piece=2 length=32
  // piece=3 length=81.028059516725861 (switch 110, 90, 45)
  // piece=4 length=...
  Position position;
  position.set_piece(2);
  position.set_start_lane(0);
  position.set_end_lane(0);
  position.set_piece_distance(0);

  Position result = car_tracker_->PredictPosition(position, 35, 1);

  EXPECT_EQ(3, result.piece());
  EXPECT_EQ(3.0, result.piece_distance());
  EXPECT_EQ(0, result.start_lane());
  EXPECT_EQ(1, result.end_lane());
}

TEST_F(PredictPositionTest, SkipSwitchWithTargetLine) {
  ASSERT_NEAR(81.028059516725861,
              car_tracker_->lane_length_model().SwitchOnTurnLength(110, 90, 45),
              1e-5);

  // Track
  // piece=2 length=32
  // piece=3 length=81.028059516725861 (switch 110, 90, 45)
  // piece=4 length=...
  Position position;
  position.set_piece(2);
  position.set_start_lane(0);
  position.set_end_lane(0);
  position.set_piece_distance(31);

  Position result = car_tracker_->PredictPosition(position, 83.028059516725861, 1);

  EXPECT_EQ(4, result.piece());
  EXPECT_EQ(1.0, result.piece_distance());
  EXPECT_EQ(1, result.start_lane());
  EXPECT_EQ(1, result.end_lane());
}

class CanBumpAfterNTicksTest : public ::testing::Test {
 protected:
  const double kEps = 1e-9;

  void SetUp() {
    json game_init_json = json::parse_file("data/gameInit.json");
    const auto& race_json = game_init_json["data"]["race"];

    race_.ParseFromJson(race_json);
    car_tracker_.reset(new CarTracker(&race_, PhysicsParams()));
  }

  CarState Simulate(const Position& position,
                    double velocity,
                    int ticks) {
    CarState my_state(position);
    my_state.set_velocity(velocity);
    for (int i = 0; i < ticks; ++i) {
      my_state = car_tracker_->Predict(my_state, Command(0));
    }
    return my_state;
  }

  Race race_;
  std::unique_ptr<CarTracker> car_tracker_;
};

TEST_F(CanBumpAfterNTicksTest, EnemyIsBehindUs) {
  const int kTicks = 30;
  CarState my_state = Simulate(Position(1, 5.0), 5.0, kTicks);

  CarState enemy_state(Position(0, 50.0));
  enemy_state.set_velocity(6);

  double velocity;
  EXPECT_FALSE(car_tracker_->CanBumpAfterNTicks(my_state, enemy_state, kTicks, &velocity));
}

TEST_F(CanBumpAfterNTicksTest, EnemyIsInFrontAndCanBump) {
  const int kTicks = 10;
  CarState my_state = Simulate(Position(1, 5.0), 5.0, kTicks);

  CarState enemy_state(Position(1, 56.0));
  enemy_state.set_velocity(3);

  double velocity;
  ASSERT_TRUE(car_tracker_->CanBumpAfterNTicks(my_state, enemy_state, kTicks, &velocity));
  EXPECT_NEAR(2.4512184206626402, velocity, 1e-5);
}

TEST_F(CanBumpAfterNTicksTest, EnemyIsInFrontAndCanNotBump) {
  const int kTicks = 10;
  CarState my_state = Simulate(Position(1, 5.0), 5.0, kTicks);

  CarState enemy_state(Position(1, 56.0));
  enemy_state.set_velocity(5);

  double velocity;
  EXPECT_FALSE(car_tracker_->CanBumpAfterNTicks(my_state, enemy_state, kTicks, &velocity));
}

TEST_F(CanBumpAfterNTicksTest, SwitchCanBump) {
  const int kTicks = 20;
  CarState my_state = Simulate(Position(3, 10.0), 5.0, kTicks);

  // Enemy is on different lane! Piece 3 is a switch.
  CarState enemy_state(Position(3, 20.0, 0.0, 1, 0));
  enemy_state.set_velocity(3);

  double velocity;
  ASSERT_TRUE(car_tracker_->CanBumpAfterNTicks(my_state, enemy_state, kTicks, &velocity));
  EXPECT_NEAR(3.1739114350623394, velocity, 1e-5);
}

TEST_F(CanBumpAfterNTicksTest, SwitchCannotBump) {
  const int kTicks = 20;
  CarState my_state = Simulate(Position(3, 10.0), 5.0, kTicks);

  // Enemy is on different lane! Piece 3 is a switch.
  CarState enemy_state(Position(3, 10.0, 0.0, 1, 0));
  enemy_state.set_velocity(3);

  double velocity;
  ASSERT_FALSE(car_tracker_->CanBumpAfterNTicks(my_state, enemy_state, kTicks, &velocity));
}

TEST_F(CanBumpAfterNTicksTest, EnemyIsStanding) {
  // TODO
}

class IsBumpInevitableTest : public ::testing::Test {
 protected:
  const double kEps = 1e-9;

  void SetUp() {
    json game_init_json = json::parse_file("data/gameInit.json");
    const auto& race_json = game_init_json["data"]["race"];

    race_.ParseFromJson(race_json);
    car_tracker_.reset(new CarTracker(&race_, PhysicsParams()));
  }

  CarState Simulate(const Position& position,
                    double velocity,
                    int ticks) {
    CarState my_state(position);
    my_state.set_velocity(velocity);
    for (int i = 0; i < ticks; ++i) {
      my_state = car_tracker_->Predict(my_state, Command(0));
    }
    return my_state;
  }

  Race race_;
  std::unique_ptr<CarTracker> car_tracker_;
};

TEST_F(IsBumpInevitableTest, Basic) {
  const int kTicks = 2;
  CarState my_state_before = Simulate(Position(4, 80.9611), 80.9611 - 73.8948, 0);
  CarState my_state_after = Simulate(Position(4, 80.9611), 80.9611 - 73.8948, kTicks);

  CarState enemy_state = CarState(Position(5, 41.5524));

  EXPECT_TRUE(car_tracker_->IsBumpInevitable(my_state_before, my_state_after, enemy_state, kTicks));
}

class IsSafeAttackTest : public ::testing::Test {
 protected:
  const double kEps = 1e-9;

  void SetUp() {
    json game_init_json = json::parse_file("data/gameInit.json");
    const auto& race_json = game_init_json["data"]["race"];

    race_.ParseFromJson(race_json);
    car_tracker_.reset(new CarTracker(&race_, PhysicsParams()));
    car_tracker_->mutable_crash_model()->force_angle(60.0);
  }

  CarState Simulate(const Position& position,
                    double velocity,
                    int ticks) {
    CarState my_state(position);
    my_state.set_velocity(velocity);
    for (int i = 0; i < ticks; ++i) {
      my_state = car_tracker_->Predict(my_state, Command(0));
    }
    return my_state;
  }

  Race race_;
  std::unique_ptr<CarTracker> car_tracker_;
};

TEST_F(IsSafeAttackTest, AttackIsSafe) {
  CarState my_state = CarState(Position(15, 6.89269, -33.8908, 0, 0), 6.64891, -29.9215, Switch::kStay, 1, TurboState());
  CarState enemy_state = CarState(Position(15, 48.3466, -54.7536, 0, 0), 5.98552, -52.4519, Switch::kStay, 1, TurboState());

  Command command;
  ASSERT_TRUE(car_tracker_->IsSafeAttack(my_state, enemy_state, &command));
  EXPECT_TRUE(command.ThrottleSet());
  EXPECT_EQ(1.0, command.throttle());
}

TEST_F(IsSafeAttackTest, AttackIsNotSafe) {
  CarState my_state = CarState(Position(14, 64.145, -25.8268, 0, 0), 6.92306, -21.6981, Switch::kStay, 1, TurboState());
  CarState enemy_state = CarState(Position(15, 36.2534, -49.7306, 0, 0), 6.23232, -46.6116, Switch::kStay, 1, TurboState());

  Command command;
  EXPECT_FALSE(car_tracker_->IsSafeAttack(my_state, enemy_state, &command));
}

TEST_F(IsSafeAttackTest, AttackWouldNotCrashEnemy) {
  CarState my_state = CarState(Position(5, 23.6888, 26.6101, 1, 1), 7.04921, 23.5348, Switch::kStay, 1, TurboState());
  CarState enemy_state = CarState(Position(5, 67.7012, 46.953, 1, 1), 6.29279, 44.2028, Switch::kStay, 1, TurboState());

  Command command;
  EXPECT_FALSE(car_tracker_->IsSafeAttack(my_state, enemy_state, &command));
}

TEST_F(IsSafeAttackTest, ConsiderAllSwitches) {
  CarState my_state = CarState(Position(2, 30, 1.5, 1, 1), 11.0, 1.6, Switch::kStay, 1, TurboState());
  CarState enemy_state = CarState(Position(2, 90, 1.5, 1, 1), 8.0, 1.6, Switch::kStay, 1, TurboState());

  Command command;
  EXPECT_FALSE(car_tracker_->IsSafeAttack(my_state, enemy_state, &command));
}

TEST_F(IsSafeAttackTest, FollowEnemy) {
  CarState my_state = CarState(Position(2, 50, 1.5, 0, 0), 11.0, 1.6, Switch::kStay, 1, TurboState());
  CarState enemy_state = CarState(Position(3, 10, 1.5, 0, 1), 8.0, 1.6, Switch::kStay, 1, TurboState());

  Command command;
  EXPECT_TRUE(car_tracker_->IsSafeAttack(my_state, enemy_state, &command));
  ASSERT_TRUE(command.SwitchSet());
  EXPECT_EQ(Switch::kSwitchRight, command.get_switch());
}

TEST_F(IsSafeAttackTest, FollowEnemyWithAlreadySwitched) {
  CarState my_state = CarState(Position(2, 50, 1.5, 0, 0), 11.0, 1.6, Switch::kSwitchRight, 1, TurboState());
  CarState enemy_state = CarState(Position(3, 10, 1.5, 0, 1), 8.0, 1.6, Switch::kStay, 1, TurboState());

  Command command;
  EXPECT_TRUE(car_tracker_->IsSafeAttack(my_state, enemy_state, &command));
  EXPECT_TRUE(command.ThrottleSet());
  EXPECT_EQ(1.0, command.throttle());
}

TEST_F(IsSafeAttackTest, CantFollowEnemy) {
  CarState my_state = CarState(Position(2, 50, 1.5, 1, 1), 11.0, 1.6, Switch::kSwitchLeft, 1, TurboState());
  CarState enemy_state = CarState(Position(3, 10, 1.5, 1, 1), 8.0, 1.6, Switch::kStay, 1, TurboState());

  Command command;
  EXPECT_FALSE(car_tracker_->IsSafeAttack(my_state, enemy_state, &command));
}

TEST_F(IsSafeAttackTest, AppliedSwitchButCanFollow) {
  CarState my_state = CarState(Position(2, 50, 1.5, 1, 1), 15.0, 1.6, Switch::kSwitchLeft, 1, TurboState());
  CarState enemy_state = CarState(Position(3, 10, 1.5, 1, 0), 8.0, 1.6, Switch::kStay, 1, TurboState());

  Command command;
  EXPECT_TRUE(car_tracker_->IsSafeAttack(my_state, enemy_state, &command));
}

TEST_F(IsSafeAttackTest, Turbo) {
  CarState my_state = CarState(Position(1, 50, 1.5, 1, 1), 15.0, 1.6, Switch::kSwitchLeft, 1, TurboState());
  my_state.AddNewTurbo(Turbo(30, 3));
  CarState enemy_state = CarState(Position(3, 10, 1.5, 1, 0), 8.0, 1.6, Switch::kStay, 1, TurboState());

  Command command;
  EXPECT_FALSE(car_tracker_->IsSafeAttack(my_state, enemy_state, &command));
  EXPECT_TRUE(car_tracker_->IsSafeAttack(my_state, enemy_state, &command, true));
  EXPECT_TRUE(command.TurboSet());
}

class IsSafeBehindTest : public ::testing::Test {
 protected:
  const double kEps = 1e-9;

  void SetUp() {
    json game_init_json = json::parse_file("data/gameInit.json");
    const auto& race_json = game_init_json["data"]["race"];

    race_.ParseFromJson(race_json);
    car_tracker_.reset(new CarTracker(&race_, PhysicsParams()));
    car_tracker_->mutable_crash_model()->force_angle(60.0);
  }

  CarState Simulate(const Position& position,
                    double velocity,
                    int ticks) {
    CarState my_state(position);
    my_state.set_velocity(velocity);
    for (int i = 0; i < ticks; ++i) {
      my_state = car_tracker_->Predict(my_state, Command(0));
    }
    return my_state;
  }

  Race race_;
  std::unique_ptr<CarTracker> car_tracker_;
};

TEST_F(IsSafeBehindTest, Basic) {
  CarState my_state = CarState(Position(15, 55.7086, -57.3014, 0, 0), 5.75364, -55.8652, Switch::kSwitchLeft, 1, TurboState());
  my_state = car_tracker_->Predict(my_state, Command(0));
  CarState enemy_state = CarState(Position(15, 15.384, -38.9491, 0, 0), 7.21031, -34.7034, Switch::kStay, 1, TurboState());

  double velocity;
  EXPECT_TRUE(car_tracker_->MaxVelocity(enemy_state, my_state, 1, &velocity));
  EXPECT_NEAR(7.2661037999999998, velocity, 1e-5);
}

TEST_F(IsSafeBehindTest, CarIsAhead) {
  CarState my_state = CarState(Position(8, 10.3849,59.8309, 1, 0), 7.31955, 59.5423, Switch::kSwitchLeft, 1, TurboState());
  CarState enemy_state = CarState(Position(8, 0.752464, 59.6429, 0, 0), 7.29057, 58.6743, Switch::kStay, 1, TurboState());

  double velocity;
  EXPECT_FALSE(car_tracker_->MaxVelocity(enemy_state, my_state, 8, &velocity));
}

}  // namespace game
