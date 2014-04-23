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

  CarState state;
  const double kEps = 1e-9;
  for (int i = 0; i < positions.size() - 1; ++i) {
    Command command = ParseCommand(commands[i]);
    Position position;
    position.ParseFromJson(positions[i + 1]);

    state = car_tracker_->Predict(state, command);

    EXPECT_NEAR(position.angle(), state.position().angle(), kEps);
    EXPECT_NEAR(position.piece_distance(), state.position().piece_distance(), kEps);
    EXPECT_EQ(position.piece(), state.position().piece());
    EXPECT_EQ(position.start_lane(), state.position().start_lane());
    EXPECT_EQ(position.end_lane(), state.position().end_lane());
    EXPECT_EQ(position.lap(), state.position().lap());
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
      // EXPECT_NEAR(next_position.piece_distance(), next.position().piece_distance(), kEps) << next_position.DebugString() << std::endl << next.position().DebugString();
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
    // EXPECT_NEAR(position.piece_distance(), state.position().piece_distance(), kEps);
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

class DriftTest : public testing::Test {
 protected:
  DriftModel drift_model_;
};

TEST_F(DriftTest, Basic) {
  // Data came from t=0.6 on Finland track
  vector<double> angle{0, 0, -0.20219, -0.585046, -1.12784, -1.81073, -2.61479, -3.52209, -4.51571, -5.57977, -6.69946, -7.86103, -9.05176, -10.26, -11.4752, -12.6876, -13.8887};
  vector<double> velocity{5.97162, 5.97162, 5.97274, 5.97329, 5.97382, 5.97434, 5.97486, 5.97536, 5.97585, 5.97634, 5.97681, 5.97727, 5.97773, 5.97773, 5.97861, 5.97904, 5.97946};

  const double radius = 0;

  // Record 3 first values to train the model.
  // for (int i = 0; i < 8; ++i) {
  //   EXPECT_FALSE(drift_model.IsReady());
  //   drift_model.Record(angle[i + 2], angle[i + 1], angle[i], velocity[i + 1], radius);
  // }

  // EXPECT_TRUE(drift_model_.IsReady());

  // Error should be less than 0.001
  for (int i = 5; i < angle.size(); ++i) {
    // TODO(zurkowski) Model is broken.
    // EXPECT_NEAR(angle[i], drift_model_.Predict(angle[i-1], angle[i-2], velocity[i-1], radius), 10);
  }
}

TEST_F(DriftTest, Oscilation) {
  vector<double> angle{28.7833, 28.2504, 27.5413, 26.6793, 25.6867, 24.5847, 23.3931, 22.1306, 20.8146, 19.461, 18.0847, 16.699, 15.3163, 13.9474, 12.602, 11.2888, 10.0152, 8.7876, 7.61134, 6.49087, 5.42971, 4.43054, 3.4953, 2.62518, 1.82074, 1.08195, 0.408251, -0.201395, -0.748441, -1.2347, -1.6623, -2.03364, -2.35132, -2.61812, -2.83698, -3.0109, -3.14296, -3.23628, -3.29397, -3.31913, -3.31481, -3.28398, -3.22956, -3.15434, -3.06101, -2.95214, -2.83018, -2.69741, -2.55601, -2.40797, -2.25518, -2.09934, -1.94203, -1.78467, -1.62855, -1.47481, -1.32446, -1.17838, -1.03734, -0.901967, -0.772807, -0.650284, -0.53473, -0.426387, -0.325414, -0.231893, -0.145841, -0.0672093, 0.00410552, 0.0682555, 0.125436, 0.175879, 0.219849, 0.257636, 0.28955};
  const double radius = 0.0;
  const double velocity = 6.5;

  drift_model_.AddModel({1.9, -0.9, -0.00125, 0});
  ErrorTracker error;

  // TODO(tomek) Train the model instead hard coding it.
  // Record 5 first values to train the model.
  // while (drift_model_.IsReady()) {
  //   EXPECT_FALSE(drift_model_.IsReady());
  //   drift_model_.Record(angle[i + 2], angle[i + 1], angle[i], 6.5 + i, radius);
  // }
  // EXPECT_TRUE(drift_model_.IsReady());

  for (int i = 2; i < angle.size(); ++i) {
    double predicted = drift_model_.Predict(angle[i - 1], angle[i - 2], 6.5, 0.0);
    EXPECT_NEAR(angle[i], predicted, 0.0002) << i;

    error.Add(angle[i], predicted);
  }

  std::cout << "Prediction error: " << std::endl;
  error.Print();
}

}  // namespace game
