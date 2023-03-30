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
        use Functor.{<$$>};
        let step = () -> {
            Text/Regex/Pattern.compile(".*") <$$> flip(Text/Regex.isMatch, "")
        };
        step() == Ok(true)

    @test
    def isMatch02(): Bool =
        use Functor.{<$$>};
        let step = () -> {
            Text/Regex/Pattern.compile(".*") <$$> flip(Text/Regex.isMatch, "a")
        };
        step() == Ok(true)

    @test
    def isMatch03(): Bool =
        use Functor.{<$$>};
        let step = () -> {
            Text/Regex/Pattern.compile(".+") <$$> flip(Text/Regex.isMatch, "")
        };
        step() == Ok(false)

    @test
    def isMatch04(): Bool =
        use Functor.{<$$>};
        let step = () -> {
            Text/Regex/Pattern.compile("\\d+") <$$> flip(Text/Regex.isMatch, "aaa")
        };
        step() == Ok(false)

    @test
    def isMatch05(): Bool =
        use Functor.{<$$>};
        let step = () -> {
            Text/Regex/Pattern.compile("\\d+") <$$> flip(Text/Regex.isMatch, "1234a")
        };
        step() == Ok(false)

}
