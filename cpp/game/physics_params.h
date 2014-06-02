#ifndef CPP_GAME_PHYSICS_PARAMS_H
#define CPP_GAME_PHYSICS_PARAMS_H

#include <vector>
#include <cmath>
#include <map>
#include <iostream>

#include <sys/file.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

namespace game {

class Track;

class VelocityModelParams {
 public:
  // Model used to drive.
  // next_velocity = x0 * velocity + x1 * throttle
  std::vector<double> model;

  VelocityModelParams() : model({0.98, 0.2}) {}
};

class DriftModelParams {
 public:
  // next_angle = x0 * angle +
  //              x1 * previous_angle +
  //              x2 * angle * velocity +
  //              -direction * max(0, x3 * velocity * velocity * sqrt(1 / radius) -
  //                                  x4 * velocity)
 std::vector<double> model;

 DriftModelParams() : model({1.9, -0.9, -0.00125, 0.00125 * sqrt(180000), 0.3}) {}
};

class SwitchLengthParams {
 public:
  // {length, width} => switch_length
  std::map<std::pair<double, double>, double> switch_on_straight_length;
  // {start_radius, end_radius, angle} => switch_length
  std::map<std::tuple<double, double, double>, double> switch_on_turn_length;

  // Loads the params from file.
  void Load();
  void Save() const;

  // Logs to stdout the switches from track that are unknown.
  void LogMissingData(const Track& track) const;
};

// TODO
class SwitchRadiusParams {
 public:
  // {start_radius, end_radius, angle, percent} => radius
  std::map<std::tuple<double, double, double, int>, double> model;

  void Load();
  void Save() const;

  // Logs to stdout the switches from track that are unknown.
  void LogMissingData(const Track& track) const;
};

class PhysicsParams {
 public:
  VelocityModelParams velocity_model_params;
  DriftModelParams drift_model_params;
  SwitchLengthParams switch_length_params;
  SwitchRadiusParams switch_radius_params;

  static PhysicsParams Load() {
    int fd = LockFile();

    PhysicsParams params;
    params.switch_length_params.Load();
    params.switch_radius_params.Load();

    UnlockFile(fd);

    return params;
  }

  void Save() const {
    int fd = LockFile();

    switch_length_params.Save();
    switch_radius_params.Save();

    UnlockFile(fd);
  }

  static int LockFile() {
    std::cout << "Waiting to lock the file ..." << std::endl;
    const char* lockfile = "/tmp/hwo.lock";
    int fd;
    struct stat st0, st1;

    while(1) {
        fd = open(lockfile, O_CREAT);
        flock(fd, LOCK_EX);

        fstat(fd, &st0);
        stat(lockfile, &st1);
        if(st0.st_ino == st1.st_ino) break;

        close(fd);
    }
    std::cout << "File locked." << std::endl;
    return fd;
  }

  static void UnlockFile(int fd) {
    std::cout << "Unlocking file ..." << std::endl;
    unlink("/tmp/hwo.lock");
    flock(fd, LOCK_UN);
    std::cout << "File unlocked." << std::endl;
  }

  void LogMissingData(const Track& track) const {
    switch_radius_params.LogMissingData(track);
    switch_length_params.LogMissingData(track);
  }
};

}  // namespace game

#endif  // CPP_GAME_PHYSICS_PARAMS_H
