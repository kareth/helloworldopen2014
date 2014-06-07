#include "game/perfect_path_optimizer.h"

#include "game/simulator.h"
#include "bots/raw_bot.h"
#include "bots/switch_optimizer/bot.h"

namespace game {

PerfectPathOptimizer::PerfectPathOptimizer(const Race& race, const PhysicsParams& physics)
  : race_(race), physics_(physics) {
}

std::map<Switch, int> PerfectPathOptimizer::Score(const Position& position) {
  //return lane_scores_[position.piece()][position.end_lane()];
  return std::map<Switch, int>();
}

void PerfectPathOptimizer::Optimize(std::atomic_bool* ready_flag) {
  printf("---------- Switch Optimizer Started!\n");
  printf("---------- Starting simulation\n");
  SimulateLanes();
  printf("---------- Simulation done, calculating scores\n");
  ComputeScores();
  //ready_flag->store(true);
  printf("---------- Optimizer Finished!\n");
}

void PerfectPathOptimizer::SimulateLanes() {
  lane_times_.resize(race_.track().pieces().size());
  for (int i = 0; i < lane_times_.size(); i++)
    lane_times_[i].resize(race_.track().lanes().size());


  for (int lane = 0; lane < race_.track().lanes().size(); lane++) {
     bots::RawBot raw(new bots::switch_optimizer::Bot());
     Simulator simulator;

     Simulator::Options opt;
     opt.physics_params = physics_;
     opt.starting_lane = lane;
     opt.race = race_;

     simulator.Run(&raw, opt);

     ParseBotData(static_cast<bots::switch_optimizer::Bot*>(raw.bot()));
  }
}

void PerfectPathOptimizer::ParseBotData(bots::switch_optimizer::Bot* bot) {
  printf("------------- Total steps: %lu\n", bot->states().size());
  printf("------------- Parsing lane data!\n");
}

void PerfectPathOptimizer::ComputeScores() {
  lane_scores_.resize(race_.track().pieces().size());
  for (int i = 0; i < lane_scores_.size(); i++)
    lane_scores_[i].resize(race_.track().lanes().size());

  for (int piece = 0; piece < lane_times_.size(); piece++) {
    for (int lane = 0; lane < lane_times_[piece].size(); lane++) {

      double shortest = 1000000;
      if (lane > 0) {
        lane_scores_[piece][lane][Switch::kSwitchLeft] = LapLength(piece + 1, lane - 1);
        shortest = std::min(shortest, lane_scores_[piece][lane][Switch::kSwitchLeft]);
      }

      lane_scores_[piece][lane][Switch::kStay] = LapLength(piece + 1, lane);
      shortest = std::min(shortest, lane_scores_[piece][lane][Switch::kStay]);

      if (lane < lane_scores_[piece].size() - 1) {
        lane_scores_[piece][lane][Switch::kSwitchRight] = LapLength(piece + 1, lane + 1);
        shortest = std::min(shortest, lane_scores_[piece][lane][Switch::kSwitchRight]);
      }

      for (auto el : lane_scores_[piece][lane])
        lane_scores_[piece][lane][el.first] = lane_scores_[piece][lane][el.first] - shortest;
    }
  }
}

// Its slow as !@#$, but who cares...
// piece can be arbitrary
//
// 1000 10 ...
// 0    12
// 1000 13
double PerfectPathOptimizer::LapLength(int piece, int lane) {
  auto dp = lane_times_;
  piece = piece % dp.size();

  for (int i = 0; i < dp[piece].size(); i++)
    dp[piece][i] = 1000000;
  dp[piece][lane] = 0;

  for (int p = 0; p < dp.size(); p++) {
    int curr = (piece + p) % dp.size();
    int next = (piece + p + 1) % dp.size();

    for (int l = 0; l < dp[p].size(); l++)  // kStay
      dp[next][l] = dp[curr][l];

    for (int l = 0; l < dp[p].size() - 1; l++)  // kRight
      dp[next][l] = min(dp[curr][l + 1], dp[next][l]);

    for (int l = 1; l < dp[p].size(); l++)  // kLeft
      dp[next][l] = min(dp[curr][l - 1], dp[next][l]);

    for (int l = 0; l < dp[p].size(); l++)   // add lengths
      dp[next][l] += lane_times_[next][l];
  }

  double best = 1000000;
  for (int l = 0; l < dp[piece].size(); l++)
    best = min(best, dp[piece][l]);

  //printf("%d %d : %lf\n",piece, lane, best);

  return best;
}

}  // namespace game
