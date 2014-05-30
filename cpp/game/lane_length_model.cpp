#include <cstdlib>

#include "game/lane_length_model.h"
#include "gflags/gflags.h"
#include "jsoncons_ext/csv/csv_reader.hpp"
#include "jsoncons_ext/csv/csv_serializer.hpp"

DECLARE_string(race_id);
DECLARE_bool(print_models);
DECLARE_bool(read_switch_models);
DECLARE_bool(write_switch_models);

using jsoncons_ext::csv::csv_reader;
using jsoncons_ext::csv::csv_serializer;

namespace game {

LaneLengthModel::LaneLengthModel(const Track* track) : track_(track) {
  if (FLAGS_read_switch_models) {
    LoadSwitchLengths();
  }
}

LaneLengthModel::~LaneLengthModel() {
  if (FLAGS_print_models) {
    std::cout << "==== Lane Length Model ====" << std::endl;
    std::cout << "Straight:" << std::endl;
    for (const auto& p : switch_on_straight_length_) {
      std::cout << "(" << p.first.first << "," << p.first.second << ") => " << p.second << std::endl;
    }
    std::cout << "Turn:" << std::endl;
    for (const auto& p : switch_on_turn_length_) {
      std::cout << "(" << std::get<0>(p.first) << "," << std::get<1>(p.first) << "," << std::get<2>(p.first) << ") => " << p.second << std::endl;
    }
  }

  if (FLAGS_write_switch_models) {
    SaveSwitchLengths();
  }
}

double LaneLengthModel::Length(const Position& position, bool* perfect) const {
  if (perfect) *perfect = true;
  const auto& piece = track_->pieces()[position.piece()];

  if (piece.type() == PieceType::kStraight) {
    if (position.start_lane() == position.end_lane()) {
      return piece.length();
    }
    if (!piece.has_switch()) {
      std::cerr << "Changing lane on non switch piece?" << std::endl;
    }

    const double width = fabs(track_->lanes()[position.start_lane()].distance_from_center() - track_->lanes()[position.end_lane()].distance_from_center());

    if (switch_on_straight_length_.count({piece.length(), width}) > 0) {
      return switch_on_straight_length_.at({piece.length(), width});
    }

    if (perfect) *perfect = false;
    return std::sqrt(width * width + piece.length() * piece.length());
  }

  if (position.start_lane() == position.end_lane()) {
    double radius = track_->LaneRadius(position.piece(), position.start_lane());
    return 2.0 * M_PI * radius * (fabs(piece.angle()) / 360.0);
  }

  if (!piece.has_switch()) {
    std::cerr << "Changing lane on non switch piece?" << std::endl;
  }

  double radius1 = track_->LaneRadius(position.piece(), position.start_lane());
  double radius2 = track_->LaneRadius(position.piece(), position.end_lane());

  if (switch_on_turn_length_.count({radius1, radius2, fabs(piece.angle())}) > 0) {
    return switch_on_turn_length_.at({radius1, radius2, fabs(piece.angle())});
  }
  if (perfect) *perfect = false;
  // The opposite switch is much better predictor if available
  if (switch_on_turn_length_.count({radius2, radius1, fabs(piece.angle())}) > 0) {
    return switch_on_turn_length_.at({radius2, radius1, fabs(piece.angle())});
  }
  return M_PI * radius1 * (fabs(piece.angle()) / 360.0) + M_PI * radius2 * (fabs(piece.angle()) / 360.0);
}

void LaneLengthModel::Record(const Position& previous, const Position& current, double predicted_velocity) {
  if (previous.piece() == current.piece())
    return;
  if (previous.start_lane() == previous.end_lane())
    return;

  const auto& piece = track_->pieces()[previous.piece()];

  double length = previous.piece_distance() + predicted_velocity - current.piece_distance();

  if (piece.type() == PieceType::kStraight) {
    const double width = fabs(track_->lanes()[previous.start_lane()].distance_from_center() - track_->lanes()[previous.end_lane()].distance_from_center());
    switch_on_straight_length_[{piece.length(), width}] = length;
    return;
  }

  double radius1 = track_->LaneRadius(previous.piece(), previous.start_lane());
  double radius2 = track_->LaneRadius(previous.piece(), previous.end_lane());
  switch_on_turn_length_[{radius1, radius2, fabs(piece.angle())}] = length;
}

static jsoncons::json LoadCSV(const string& file_name) {
  std::ifstream file(file_name);
  if (!file.good()) {
    file.close();
    return jsoncons::json(::jsoncons::json::an_array);
  }

  jsoncons::json_deserializer handler;
  jsoncons::json params;
  params["has_header"] = true;

  csv_reader reader(file, handler, params);
  reader.read();
  jsoncons::json j = std::move(handler.root());
  return j;
}

static double ToDouble(jsoncons::json data) {
  return std::strtod(data.as_string().c_str(), nullptr);
}

void LaneLengthModel::LoadSwitchLengths() {
  jsoncons::json straight_lengths = LoadCSV("data/switch-straight-lengths.csv");
  for (auto it = straight_lengths.begin_elements(); it != straight_lengths.end_elements(); ++it) {
    const auto& data = *it;
    switch_on_straight_length_[{ToDouble(data["length"]), ToDouble(data["width"])}] = ToDouble(data["switch_length"]);
  }

  jsoncons::json turn_lengths = LoadCSV("data/switch-turn-lengths.csv");
  for (auto it = turn_lengths.begin_elements(); it != turn_lengths.end_elements(); ++it) {
    const auto& data = *it;
    switch_on_turn_length_[{ToDouble(data["start_radius"]), ToDouble(data["end_radius"]), ToDouble(data["angle"])}] = ToDouble(data["switch_length"]);
  }

  // Check if we are missing any data for current track.
  bool has_all = true;
  for (const auto& piece : track_->pieces()) {
    if (!piece.has_switch()) continue;

    if (piece.type() == PieceType::kStraight) {
      for (int i = 1; i < track_->lanes().size(); ++i) {
        double width = fabs(track_->lanes()[i].distance_from_center() - track_->lanes()[i - 1].distance_from_center());
        if (switch_on_straight_length_.count({piece.length(), width}) == 0) {
          has_all = false;
          std::cout << "WARNING: Missing length for switch on straight with length " << piece.length() << " and width " << width << std::endl;
        }
      }
    } else {
      for (int i = 1; i < track_->lanes().size(); ++i) {
        double start_radius = piece.radius() + track_->lanes()[i - 1].distance_from_center();
        double end_radius = piece.radius() + track_->lanes()[i].distance_from_center();
        if (switch_on_turn_length_.count({start_radius, end_radius, fabs(piece.angle())}) == 0) {
          has_all = false;
          std::cout << "WARNING: Missing length for switch on turn start_radius: " << start_radius << " end_radius: " << end_radius << " angle: " << fabs(piece.angle()) << std::endl;
        }
        if (switch_on_turn_length_.count({end_radius, start_radius, fabs(piece.angle())}) == 0) {
          has_all = false;
          std::cout << "WARNING: Missing length for switch on turn start_radius: " << end_radius << " end_radius: " << start_radius << " angle: " << fabs(piece.angle()) << std::endl;
        }
      }
    }
  }
  if (has_all) {
    std::cout << "We have all switch lengths!" << std::endl;
  }
}

void LaneLengthModel::SaveSwitchLengths() {
  std::ofstream file("data/switch-straight-lengths.csv");
  file << "length,width,switch_length" << std::endl;
  for (const auto& it : switch_on_straight_length_) {
    file << std::setprecision(20) << it.first.first << "," << it.first.second << "," << it.second << std::endl;
  }
  file.close();

  file.open("data/switch-turn-lengths.csv");
  file << "start_radius,end_radius,angle,switch_length" << std::endl;
  for (const auto& it : switch_on_turn_length_) {
    file << std::setprecision(20) << std::get<0>(it.first) << "," << std::get<1>(it.first) << "," << std::get<2>(it.first) << "," << it.second << std::endl;
  }
  file.close();
}

}  // namespace game
