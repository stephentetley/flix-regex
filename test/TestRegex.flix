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

namespace TestRegex {


    /////////////////////////////////////////////////////////////////////////////
    // isMatch                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isMatch01(): Bool =
        use Result.flatMap;
        use Text/Regex.{isMatch};
        let step = () -> {
            let* p1 = Text/Regex/Pattern.compile(".*");
            Ok(isMatch(p1, ""))
        };
        step() == Ok(true)

    @test
    def isMatch02(): Bool =
        use Result.flatMap;
        use Text/Regex.{isMatch};
        let step = () -> {
            let* p1 = Text/Regex/Pattern.compile(".*");
            Ok(isMatch(p1, "a"))
        };
        step() == Ok(true)

    @test
    def isMatch03(): Bool =
        use Result.flatMap;
        use Text/Regex.{isMatch};
        let step = () -> {
            let* p1 = Text/Regex/Pattern.compile(".+");
            Ok(isMatch(p1, ""))
        };
        step() == Ok(false)

    @test
    def isMatch04(): Bool =
        use Result.flatMap;
        use Text/Regex.{isMatch};
        let step = () -> {
            let* p1 = Text/Regex/Pattern.compile("\\d+");    // 1+ digits
            Ok(isMatch(p1, "aaa"))
        };
        step() == Ok(false)

    @test
    def isMatch05(): Bool =
        use Result.flatMap;
        use Text/Regex.{isMatch};
        let step = () -> {
            let* p1 = Text/Regex/Pattern.compile("\\d+");
            Ok(isMatch(p1, "1234a"))
        };
        step() == Ok(false)
/*
    /////////////////////////////////////////////////////////////////////////////
    // isSubMmatch                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def contains01(): Bool =
        use Result.flatMap;
        use Text/Regex.{contains};
        let step = () -> {
            let* p1 = Text/Regex/Pattern.compile("\\d+");
            Ok(contains(p1, ""))
        };
        step() == Ok(None)

    @test
    def contains02(): Bool =
        use Result.flatMap;
        use Text/Regex.{contains};
        let step = () -> {
            let* p1 = Text/Regex/Pattern.compile("\\d+");
            Ok(contains(p1, "123"))
        };
        step() == Ok(Some("123"))

    @test
    def contains03(): Bool =
        use Result.flatMap;
        use Text/Regex.{contains};
        let step = () -> {
            let* p1 = Text/Regex/Pattern.compile("\\d+");
            Ok(contains(p1, "123abc"))
        };
        step() == Ok(Some("123"))

    /////////////////////////////////////////////////////////////////////////////
    // startsWith                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def startsWith01(): Bool =
        use Result.flatMap;
        use Text/Regex.{startsWith};
        let step = () -> {
            let* p1 = Text/Regex/Pattern.compile("\\d+");
            Ok(startsWith(p1, ""))
        };
        step() == Ok(false)

    @test
    def startsWith02(): Bool =
        use Result.flatMap;
        use Text/Regex.{startsWith};
        let step = () -> {
            let* p1 = Text/Regex/Pattern.compile("\\d+");
            Ok(startsWith(p1, "123"))
        };
        step() == Ok(true)

    @test
    def startsWith03(): Bool =
        use Result.flatMap;
        use Text/Regex.{startsWith};
        let step = () -> {
            let* p1 = Text/Regex/Pattern.compile("\\d+");
            Ok(startsWith(p1, "123abc"))
        };
        step() == Ok(true)

    /// /////////////////////////////////////////////////////////////////////////////
    /// // evalMatches                                                             //
    /// /////////////////////////////////////////////////////////////////////////////

    /// @test
    /// def evalMatches01(): Bool & Impure =
    ///     use Result.flatMap;
    ///     use Text/Regex/Eval.{evalMatches};
    ///     let eval = Text/Regex/Eval/MatchEvaluator.getCaptureNamed("num");
    ///     let step = () -> {
    ///         let* p1 = Text/Regex/Pattern.compile("(?<num>\\d+)");
    ///         evalMatches(p1, eval, "123")
    ///     };
    ///     step() == Ok("123")

    /// /////////////////////////////////////////////////////////////////////////////
    /// // evalContains                                                            //
    /// /////////////////////////////////////////////////////////////////////////////

    /// @test
    /// def evalContains01(): Bool & Impure =
    ///     use Result.flatMap;
    ///     use Text/Regex.{evalContains};
    ///     let step = () -> {
    ///         let* p1     = Text/Regex/Pattern.compile("(?<num>\\d+)");
    ///         let eval    = Text/Regex/Eval/MatchEvaluator.getCaptureNamed("num");
    ///         evalContains(p1, eval, "aa 123 bb")
    ///     };
    ///     step() == Ok("123")

    /// /////////////////////////////////////////////////////////////////////////////
    /// // evalStartsWith                                                          //
    /// /////////////////////////////////////////////////////////////////////////////

    /// @test
    /// def evalStartsWith01(): Bool & Impure =
    ///     use Result.flatMap;
    ///     use Text/Regex.{evalStartsWith};
    ///     let step = () -> {
    ///         let* p1     = Text/Regex/Pattern.compile("(?<num>\\d+)");
    ///         let eval    = Text/Regex/Eval/MatchEvaluator.getCaptureNamed("num");
    ///         evalStartsWith(p1, eval, "123 bb")
    ///     };
    ///     step() == Ok("123")

    /// /////////////////////////////////////////////////////////////////////////////
    /// // results                                                                 //
    /// /////////////////////////////////////////////////////////////////////////////

    /// @test
    /// def results01(): Bool & Impure =
    ///     use Result.flatMap;
    ///     use Text/Regex.{results};
    ///     use Text/Regex/Eval/MatchEvaluator.{content};
    ///     let step = () -> {
    ///         let* p1 = Text/Regex/Pattern.compile("\\p{Alpha}+");
    ///         let* xs = results(p1, content(), "aa bb cc");
    ///         Ok(xs)
    ///     };
    ///     step() == Ok("aa" :: "bb" :: "cc" :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // contents                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def contents01(): Bool & Impure =
        use Result.flatMap;
        use Text/Regex.{contents};
        let step = () -> {
            let* p1 = Text/Regex/Pattern.compile("\\p{Alpha}+");
            let* xs = contents(p1, "aa bb cc");
            Ok(xs)
        };
        step() == Ok("aa" :: "bb" :: "cc" :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // ranges                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def ranges01(): Bool & Impure =
        use Result.flatMap;
        use Text/Regex.{ranges};
        let toTuple = rng -> (rng.start, rng.end);
        let step = () -> {
            let* p1 = Text/Regex/Pattern.compile("\\p{Alpha}+");
            let* xs = ranges(p1, "aa bb cc");
            Ok(List.map(toTuple, xs))
        };
        
        step() == Ok((0,2) :: (3,5) :: (6,8) :: Nil)
*/
}
