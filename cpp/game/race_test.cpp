#include <iostream>

#include "game/race.h"
#include "gtest/gtest.h"
#include "jsoncons/json.hpp"

using jsoncons::json;

namespace game {

class RaceParseJsonTest : public testing::Test {
 protected:
  void SetUp() {
    json game_init_json = json::parse_file("data/gameInit.json");
    const auto& race_json = game_init_json["data"]["race"];

    race_.ParseFromJson(race_json);
  }
  Race race_;
};

TEST_F(RaceParseJsonTest, Parses) {
  EXPECT_EQ("keimola", race_.track().id());
  EXPECT_EQ("Keimola", race_.track().name());

  EXPECT_EQ(3, race_.laps());
  EXPECT_EQ(60000, race_.max_lap_time_ms());
  EXPECT_EQ(true, race_.quick_race());

  ASSERT_EQ(1, race_.cars().size());
  EXPECT_EQ("Need for C", race_.cars()[0].name());
  EXPECT_EQ("red", race_.cars()[0].color());
  EXPECT_EQ(20.0, race_.cars()[0].width());
  EXPECT_EQ(40.0, race_.cars()[0].length());
  EXPECT_EQ(10.0, race_.cars()[0].guide_flag_position());
}

}  // namespace game
