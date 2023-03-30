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

namespace TestPattern {


    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toString01(): Result[String, String] =
        use Functor.{<$$>};
        let step = () -> {
            Text/Regex/Pattern.compile("\\s+") <$$> ToString.toString
        };
        step()

    /////////////////////////////////////////////////////////////////////////////
    // getRegex                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def getRegex01(): Bool =
        use Functor.{<$$>};
        let step = () -> {
            Text/Regex/Pattern.compile("\\s+") <$$> Text/Regex/Pattern.getRegex
        };
        step() == Ok("\\s+")

    /////////////////////////////////////////////////////////////////////////////
    // getFlags                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def getFlags01(): Bool =
        use Functor.{<$$>};
        let step = () -> {
            Text/Regex/Pattern.compile("\\s+") <$$> Text/Regex/Pattern.getFlags
        };
        step() == Ok(Nil)

    @test
    def getFlags02(): Bool =
        use Functor.{<$$>};
        use Text/Regex/Pattern.Flag.{CanonEq};
        let step = () -> {
            Text/Regex/Pattern.compileWithFlags("\\s+", CanonEq :: Nil) <$$> Text/Regex/Pattern.getFlags
        };
        step() == Ok(CanonEq :: Nil)

    @test
    def getFlags03(): Bool =
        use Functor.{<$$>};
        use Text/Regex/Pattern.Flag.{CanonEq, Multiline};
        let step = () -> {
            Text/Regex/Pattern.compileWithFlags("\\s+", CanonEq ::  Multiline :: Nil) <$$> Text/Regex/Pattern.getFlags
        };
        step() == Ok(CanonEq :: Multiline :: Nil) or step() == Ok(Multiline :: CanonEq :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // unmatchable                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unmatchable01(): Bool =
        use Text/Regex/Pattern.{unmatchable};
        Text/Regex.isMatch(unmatchable(), "") == false

    @test
    def unmatchable02(): Bool =
        use Text/Regex/Pattern.{unmatchable};
        Text/Regex.isMatch(unmatchable(), "aaa") == false

    @test
    def unmatchable03(): Bool =
        use Text/Regex/Pattern.{unmatchable};
        Text/Regex.isMatch(unmatchable(), "aaa bbb") == false

    @test
    def unmatchable04(): Bool =
        use Text/Regex/Pattern.{unmatchable};
        Text/Regex.isMatch(unmatchable(), String.unlines("aaa" :: "bbb" :: Nil)) == false



}
