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

TEST_F(CarTrackerTest, DistanceBetweenTheSamePiece) {
  Position position1;
  position1.set_piece_distance(25.0);

  Position position2;
  position2.set_piece_distance(75.0);
  EXPECT_NEAR(50.0, car_tracker_->DistanceBetween(position1, position2), kEps);
}

TEST_F(CarTrackerTest, DistanceBetweenTheOtherPiece) {
  Position position1;
  position1.set_piece_distance(25.0);

  Position position2;
  position2.set_piece(2);
  position2.set_piece_distance(75.0);
  EXPECT_NEAR(250.0, car_tracker_->DistanceBetween(position1, position2), kEps);
}

TEST_F(CarTrackerTest, AroundTheTrack) {
  const double kTrackLength = 3399.7874452256719;
  Position position1;
  position1.set_piece_distance(75.0);

  Position position2;
  position2.set_piece_distance(25.0);
  EXPECT_NEAR(kTrackLength + 50, car_tracker_->DistanceBetween(position1, position2), kEps);
}

TEST_F(CarTrackerTest, WeCanSwitchTheLane) {
  Position position1;
  position1.set_piece_distance(0.0);

  Position position2;
  position2.set_piece(4);
  position2.set_start_lane(1);
  position2.set_end_lane(1);
  position2.set_piece_distance(0.0);
  EXPECT_NEAR(401.9803902718557, car_tracker_->DistanceBetween(position1, position2), kEps);
}

TEST_F(CarTrackerTest, BothOnTheSameSwitch) {
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

TEST_F(CarTrackerTest, BothOnTheSameSwitchDifferentLanes) {
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
  EXPECT_NEAR(3440.0089916087445, car_tracker_->DistanceBetween(position1, position2), kEps);
}

TEST_F(CarTrackerTest, TwoOtherSwitches) {
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

}  // namespace game
