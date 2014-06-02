#include "game/physics_params.h"
#include "game/track.h"
#include "jsoncons_ext/csv/csv_reader.hpp"

using jsoncons_ext::csv::csv_reader;

namespace game {

namespace {
jsoncons::json LoadCSV(const string& file_name) {
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

double ToDouble(jsoncons::json data) {
  return std::strtod(data.as_string().c_str(), nullptr);
}
}  // namespace

void SwitchLengthParams::Load() {
  jsoncons::json straight_lengths = LoadCSV("data/switch-straight-lengths.csv");
  for (auto it = straight_lengths.begin_elements(); it != straight_lengths.end_elements(); ++it) {
    const auto& data = *it;
    switch_on_straight_length[{ToDouble(data["length"]), ToDouble(data["width"])}] = ToDouble(data["switch_length"]);
  }

  jsoncons::json turn_lengths = LoadCSV("data/switch-turn-lengths.csv");
  for (auto it = turn_lengths.begin_elements(); it != turn_lengths.end_elements(); ++it) {
    const auto& data = *it;
    switch_on_turn_length[std::make_tuple(ToDouble(data["start_radius"]), ToDouble(data["end_radius"]), ToDouble(data["angle"]))] = ToDouble(data["switch_length"]);
  }
}

void SwitchLengthParams::Save() const {
  // Just in case some other bot already written new data.
  Load();

  std::ofstream file("data/switch-straight-lengths.csv");
  file << "length,width,switch_length" << std::endl;
  for (const auto& it : switch_on_straight_length) {
    file << std::setprecision(20) << it.first.first << "," << it.first.second << "," << it.second << std::endl;
  }
  file.close();

  file.open("data/switch-turn-lengths.csv");
  file << "start_radius,end_radius,angle,switch_length" << std::endl;
  for (const auto& it : switch_on_turn_length) {
    file << std::setprecision(20) << std::get<0>(it.first) << "," << std::get<1>(it.first) << "," << std::get<2>(it.first) << "," << it.second << std::endl;
  }
  file.close();
}

void SwitchLengthParams::LogMissingData(const Track& track) const {
  // Check if we are missing any data for current track.
  bool has_all = true;
  for (const auto& piece : track.pieces()) {
    if (!piece.has_switch()) continue;

    if (piece.type() == PieceType::kStraight) {
      for (int i = 1; i < track.lanes().size(); ++i) {
        double width = fabs(track.lanes()[i].distance_from_center() - track.lanes()[i - 1].distance_from_center());
        if (switch_on_straight_length.count({piece.length(), width}) == 0) {
          has_all = false;
          std::cout << "WARNING: Missing length for switch on straight with length " << piece.length() << " and width " << width << std::endl;
        }
      }
    } else {
      for (int i = 1; i < track.lanes().size(); ++i) {
        double start_radius = piece.radius() + track.lanes()[i - 1].distance_from_center();
        double end_radius = piece.radius() + track.lanes()[i].distance_from_center();
        if (switch_on_turn_length.count(std::make_tuple(start_radius, end_radius, fabs(piece.angle()))) == 0) {
          has_all = false;
          std::cout << "WARNING: Missing length for switch on turn start_radius: " << start_radius << " end_radius: " << end_radius << " angle: " << fabs(piece.angle()) << std::endl;
        }
        if (switch_on_turn_length.count(std::make_tuple(end_radius, start_radius, fabs(piece.angle()))) == 0) {
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

void SwitchRadiusParams::Load() {
  jsoncons::json radiuses = LoadCSV("data/switch-radiuses.csv");
  for (auto it = radiuses.begin_elements(); it != radiuses.end_elements(); ++it) {
    const auto& data = *it;
    model.insert({std::make_tuple<double, double, double, int>(
                         ToDouble(data["start_radius"]),
                         ToDouble(data["end_radius"]),
                         ToDouble(data["angle"]),
                         static_cast<int>(ToDouble(data["percent"]))), ToDouble(data["switch_radius"])});
  }
}

void SwitchRadiusParams::Save() const {
  // Just in case some other bot already written new data.
  Load();

  std::ofstream file("data/switch-radiuses.csv");
  file << "start_radius,end_radius,angle,percent,switch_radius" << std::endl;
  for (const auto& it : model) {
    file << std::setprecision(20)
         << std::get<0>(it.first) << ","
         << std::get<1>(it.first) << ","
         << std::get<2>(it.first) << ","
         << std::get<3>(it.first) << ","
         << it.second << std::endl;
  }
  file.close();
}

void SwitchRadiusParams::LogMissingData(const Track& track) const {
  // Check if we are missing any data for current track.
  bool has_all = true;
  for (const auto& piece : track.pieces()) {
    if (!piece.has_switch()) continue;
    if (piece.type() == PieceType::kStraight) continue;

    for (int i = 1; i < track.lanes().size(); ++i) {
      double start_radius = piece.radius() + track.lanes()[i - 1].distance_from_center();
      double end_radius = piece.radius() + track.lanes()[i].distance_from_center();
      double angle = fabs(piece.angle());

      int missing = 0;
      for (int percent = 1; percent <= 99; ++percent) {
        if (model.count(std::make_tuple(start_radius, end_radius, angle, percent)) == 0)
          missing++;
      }

      if (missing > 0) {
        has_all = false;
        std::cout << "Missing " << missing << " percents of radiuses for "
                  << " start_radius: " << start_radius
                  << " end_radius: " << end_radius
                  << " angle: " << angle << std::endl;
      }
    }

    for (int i = 1; i < track.lanes().size(); ++i) {
      double start_radius = piece.radius() + track.lanes()[i].distance_from_center();
      double end_radius = piece.radius() + track.lanes()[i - 1].distance_from_center();
      double angle = fabs(piece.angle());

      int missing = 0;
      for (int percent = 1; percent <= 99; ++percent) {
        if (model.count(std::make_tuple(start_radius, end_radius, angle, percent)) == 0)
          missing++;
      }

      if (missing > 0) {
        has_all = false;
        std::cout << "Missing " << missing << " percents of radiuses for "
                  << " start_radius: " << start_radius
                  << " end_radius: " << end_radius
                  << " angle: " << angle << std::endl;
      }
    }
  }
  if (has_all) {
    std::cout << "We have all switch radiuses!" << std::endl;
  }
}

}  // namespace game
