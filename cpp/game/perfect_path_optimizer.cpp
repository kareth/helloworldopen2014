#include "game/perfect_path_optimizer.h"

namespace game {

PerfectPathOptimizer::PerfectPathOptimizer(const Race& race, const PhysicsParams& physics)
  : race_(race), physics_(physics) {
}

std::map<Switch, int> PerfectPathOptimizer::Score(const Position& position) {
  return lane_scores_[position.piece()][position.end_lane()];
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

  for (int piece = 0; piece < lane_times_.size(); piece++) {
    for (int lane = 0; lane < lane_times_[piece].size(); lane++) {
      lane_times[piece][lane] = 1;
    }
  }
}


void PerfectPathOptimizer::ComputeScores() {
  lane_scores_.resize(race_.track().pieces().size());
  for (int i = 0; i < lane_scores_.size(); i++)
    lane_scores_[i].resize(race_.track().lanes().size());

  for (int piece = 0; piece < lane_times_.size(); piece++) {
    for (int lane = 0; lane < lane_times_[piece].size(); lane++) {

      double shortest = 1000000;
      if (lane > 0) {
        lane_scores[piece][lane][Switch::kSwitchLeft] = LapLength(piece + 1, lane - 1);
        shortest = min(shortest, lane_scores[piece][lane][Switch::kSwitchLeft]);
      }

      lane_scores[piece][lane][Switch::kStay] = LapLength(piece + 1, lane);
      shortest = min(shortest, lane_scores[piece][lane][Switch::kStay]);

      if (lane < lane_scores[piece].size() - 1) {
        lane_scores[piece][lane][Switch::kSwitchRight] = LapLength(piece + 1, lane + 1);
        shortest = min(shortest, lane_scores[piece][lane][Switch::kSwitchRight]);
      }

      for (auto el : lane_scores_[piece][lane])
        lane_scores_[piece][lane][el.first] = lane_scores_[piece][lane][el.first] - shortest;
    }
  }

  for (int lane = 0; lane < lane_times_[0].size(); lane++) {
    for (int piece = 0; piece < lane_times_.size(); piece++) {
      printf("{ ");
      for (auto el : lane_scores_[piece][lane]) {
        printf("{%d,%d} \t",el.first, el.second);
      printf("}   ");
    }
    printf("\n");
  }
  printf("\n");
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
      dp[next][lane] = dp[curr][lane];

    for (int l = 0; l < dp[p].size() - 1; l++)  // kRight
      dp[next][lane] = min(dp[curr][lane + 1], dp[next][lane]);

    for (int l = 1; l < dp[p].size(); l++)  // kLeft
      dp[next][lane] = min(dp[curr][lane - 1], dp[next][lane]);

    for (int l = 0; l < dp[p].size(); l++)   // add lengths
      dp[next][lane] += lane_times_[next][lane];
  }

  double best = 1000000;
  for (int l = 0; l < dp[piece].size(); l++)
    best = min(best, dp[piece][l]);

  return best;
}

}  // namespace game
