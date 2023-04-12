/*
 * Copyright 2020 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

mod TestMatcher {

/*

    /////////////////////////////////////////////////////////////////////////////
    // point                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def point01(): Bool & Impure =
        use Text.Regex.Matcher.{runsMatcher, point};
        runsMatcher(point(3), ".*", "aaaaa") == Ok(3)

    /////////////////////////////////////////////////////////////////////////////
    // getMatch                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def getMatch01(): Bool & Impure =
        use Text.Regex.Matcher.{runsMatcher, getMatch};
        runsMatcher(getMatch(), ".*", "aa") == Ok("aa")


    @test
    def getMatch02(): Bool & Impure =
        use Text.Regex.Matcher.{runsMatcher, flatMap, point, getMatch};
        let p1 = forM (_ <- getMatch(); b <- getMatch()) yield b;
        runsMatcher(p1, "\\w+", "aa bb") == Ok("bb")

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool & Impure =
        use Text.Regex.Matcher.{runsMatcher, getMatch, count};
        let regex = "\\p{Alpha}+";
        runsMatcher(count(3, getMatch()), regex, "aa bb cc") == Ok("aa" :: "bb" :: "cc" :: Nil)
*/
}