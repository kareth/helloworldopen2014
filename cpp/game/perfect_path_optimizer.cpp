#include "game/perfect_path_optimizer.h"

#include "game/simulator.h"
#include "bots/raw_bot.h"
#include "bots/switch_optimizer/bot.h"

namespace game {

DEFINE_bool(perfect_switches, true, "Calculate switches magically");

PerfectPathOptimizer::PerfectPathOptimizer(const Race& race, const PhysicsParams& physics)
  : race_(race), physics_(physics), car_tracker_(&race_, physics),
    velocity_predictor_(car_tracker_, race_) {
}

std::map<Switch, double> PerfectPathOptimizer::Score(const Position& position) {
  return lane_scores_[position.piece()][position.end_lane()];
}

void PerfectPathOptimizer::Optimize(std::atomic<bool>* ready_flag) {
  if (FLAGS_perfect_switches) {
    printf("---------- Switch Optimizer Started!\n");
    printf("---------- Starting calculation\n");
    ComputeScores();
    ready_flag->store(true);
    printf("---------- Optimizer Finished!\n");
  }
}

void PerfectPathOptimizer::ComputeScores() {
  lane_times_.resize(race_.track().pieces().size());
  for (int i = 0; i < lane_times_.size(); i++) {
    lane_times_[i].resize(race_.track().lanes().size());
    for (int j = 0; j < lane_times_[i].size(); j++) {
      lane_times_[i][j].resize(race_.track().lanes().size());
    }
  }

  for (int piece = 0; piece < lane_times_.size(); piece++) {
    for (int lane = 0; lane < lane_times_[piece].size(); lane++) {
      if (race_.track().pieces()[piece].has_switch()) {
        for (int lane2 = 0; lane2 < lane_times_[piece].size(); lane2++) {
          if (lane2 == lane || lane2 == lane + 1 || lane2 == lane -1) {
            Position a(piece, 0);
            a.set_start_lane(lane);
            a.set_end_lane(lane2);
            Position b((piece + 1) % race_.track().pieces().size(), 0);
            b.set_start_lane(lane2);
            b.set_end_lane(lane2);

            lane_times_[piece][lane][lane2] = car_tracker_.DistanceBetween(a, b);
            //printf("%d %d-%d = %lf\n",piece, lane, lane2, lane_times_[piece][lane][lane2]);
          }
        }
      } else {
        Position a(piece, 0);
        a.set_start_lane(lane);
        a.set_end_lane(lane);
        Position b((piece + 1) % race_.track().pieces().size(), 0);
        b.set_start_lane(lane);
        b.set_end_lane(lane);

        lane_times_[piece][lane][lane] = car_tracker_.DistanceBetween(a, b);
        //printf("%d %d-%d = %lf\n",piece, lane, lane, lane_times_[piece][lane][lane]);
      }
    }
  }



  lane_scores_.resize(race_.track().pieces().size());
  for (int i = 0; i < lane_scores_.size(); i++)
    lane_scores_[i].resize(race_.track().lanes().size());

  for (int piece = 0; piece < lane_times_.size(); piece++) {
    for (int lane = 0; lane < lane_times_[piece].size(); lane++) {

      int next_switch = (race_.track().NextSwitch(piece)) % race_.track().pieces().size();
      int next_after_switch = (race_.track().NextSwitch(piece) + 1) % race_.track().pieces().size();

      double shortest = 1000000;
      if (lane > 0) {
        lane_scores_[piece][lane][Switch::kSwitchLeft] =
          LapLength(next_after_switch, lane - 1) + lane_times_[next_switch][lane][lane - 1];
        shortest = std::min(shortest, lane_scores_[piece][lane][Switch::kSwitchLeft]);
      }
      //printf(":%lf\n", lane_scores_[piece][lane][Switch::kSwitchLeft]);

      lane_scores_[piece][lane][Switch::kStay] =
          LapLength(next_after_switch, lane) + lane_times_[next_switch][lane][lane];
      shortest = std::min(shortest, lane_scores_[piece][lane][Switch::kStay]);

      //printf(":%lf\n", lane_scores_[piece][lane][Switch::kStay]);

      if (lane < lane_scores_[piece].size() - 1) {
        lane_scores_[piece][lane][Switch::kSwitchRight] =
          LapLength(next_after_switch, lane + 1) + lane_times_[next_switch][lane][lane + 1];
        shortest = std::min(shortest, lane_scores_[piece][lane][Switch::kSwitchRight]);
      }

      //printf(":%lf\n", lane_scores_[piece][lane][Switch::kSwitchRight]);

      for (auto el : lane_scores_[piece][lane]) {
        lane_scores_[piece][lane][el.first] = lane_scores_[piece][lane][el.first] - shortest;
      }
    }
  }

  // for (int piece = 0; piece < lane_times_.size(); piece++)
  //   printf("%d > %lf %lf\n",piece, lane_scores_[piece][1][Switch::kStay], lane_scores_[piece][1][Switch::kSwitchLeft] );

}

// Its slow as !@#$, but who cares...
// piece can be arbitrary
//
// 1000 10 ...
// 0    12
// 1000 13
double PerfectPathOptimizer::LapLength(int piece, int lane) {
  vector<vector<double>> dp(race_.track().pieces().size(),
                            vector<double>(race_.track().lanes().size(), 1000000));
  piece = piece % dp.size();

  dp[piece][lane] = lane_times_[piece][lane][lane];

  for (int p = 0; p < dp.size() - 1; p++) {
    int curr = (piece + p) % dp.size();
    int next = (piece + p + 1) % dp.size();

    for (int l = 0; l < dp[p].size(); l++)  // kStay
      dp[next][l] = dp[curr][l] + lane_times_[next][l][l];

    if (race_.track().pieces()[next].has_switch()) {
      for (int l = 0; l < dp[p].size() - 1; l++)  // kRight
        dp[next][l] = min(dp[curr][l + 1] + lane_times_[next][l + 1][l], dp[next][l]) ;

      for (int l = 1; l < dp[p].size(); l++)  // kLeft
        dp[next][l] = min(dp[curr][l - 1] + lane_times_[next][l - 1][l], dp[next][l]);
    }
  }

  double best = 1000000;
  int prev = (piece + dp.size() - 1) % dp.size();
  for (int l = 0; l < dp[prev].size(); l++)
    best = min(best, dp[prev][l]);

  //printf("%d %d : %lf\n",piece, lane, best);

  return best;
}

}  // namespace game
