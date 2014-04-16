#ifndef CPP_GAME_RACE_H_
#define CPP_GAME_RACE_H_

namespace game {

class Race {
 public:
 private:
   int laps_;
   int max_lap_time_;
   bool quick_race_;

   Track track_;
   std::vector<Car> cars_;
};

}  // namespace game

#endif  // CPP_GAME_RACE_H_
