#include <iostream>

#include "game/car_tracker.h"
#include "gtest/gtest.h"

using jsoncons::json;

namespace game {

class CarTrackerTest : public testing::Test {
 protected:
  void SetUp() {
    json game_init_json = json::parse_file("data/gameInit.json");
    const auto& race_json = game_init_json["data"]["race"];

    race_.ParseFromJson(race_json);
    car_tracker_.reset(new CarTracker(&race_));
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

TEST_F(CarTrackerTest, GreedyRun) {
  auto history = json::parse_file("data/greedyRun.json");
  auto commands = history["commands"];
  auto positions = history["positions"];

  for (int i = 0; !car_tracker_->IsReady(); ++i) {
    Command command = ParseCommand(commands[i]);
    Position position;
    position.ParseFromJson(positions[i]);

    car_tracker_->Record(position);
    car_tracker_->RecordCommand(command);
  }

  ASSERT_TRUE(car_tracker_->IsReady());

  CarState state;
  const double kEps = 1e-9;
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
  const double kEps = 1e-9;
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
  const double kEps = 1e-9;
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

      // TODO(tomek) uncomment once we have better switch prediction.
      // EXPECT_NEAR(next_position.angle(), next.position().angle(), kEps);
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
  const double kEps = 1e-9;
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

    // EXPECT_NEAR(position.angle(), state.position().angle(), kEps);
    EXPECT_NEAR(position.piece_distance(), state.position().piece_distance(), position.lap() == 1 ? 0.002 : kEps);
    EXPECT_EQ(position.piece(), state.position().piece()) << position.DebugString() << std::endl << state.position().DebugString();
    EXPECT_EQ(position.start_lane(), state.position().start_lane()) << position.DebugString() << std::endl << state.position().DebugString();
    EXPECT_EQ(position.end_lane(), state.position().end_lane()) << position.DebugString() << std::endl << state.position().DebugString();
    EXPECT_EQ(position.lap(), state.position().lap());
  }
}

class VelocityModelTest : public testing::Test {
 protected:
  VelocityModel velocity_model_;
};

TEST_F(VelocityModelTest, FinlandTrack) {
  EXPECT_FALSE(velocity_model_.IsReady());

  // Ignore when standing
  velocity_model_.Record(0.0, 0, 0.5);
  velocity_model_.Record(0.0, 0, 0.5);
  velocity_model_.Record(0.0, 0, 0.5);
  velocity_model_.Record(0.0, 0, 0.5);

  EXPECT_FALSE(velocity_model_.IsReady());

  velocity_model_.Record(0.1, 0, 0.5);

  EXPECT_FALSE(velocity_model_.IsReady());

  velocity_model_.Record(0.198, 0.1, 0.5);

  EXPECT_TRUE(velocity_model_.IsReady());
  EXPECT_DOUBLE_EQ(0.1, velocity_model_.Predict(0.0, 0.5));
  EXPECT_DOUBLE_EQ(0.198, velocity_model_.Predict(0.1, 0.5));
  EXPECT_DOUBLE_EQ(0.29404, velocity_model_.Predict(0.198, 0.5));
}

}  // namespace game
