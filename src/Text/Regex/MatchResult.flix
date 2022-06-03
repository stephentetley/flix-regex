/*
 * Copyright 2022 Stephen Tetley
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

/// A MatchResult made by Matcher.toMatchResult() is immutable
/// but it may be in an error state.
/// Unfortunately it does not handle named groups.



namespace Text/Regex {

    ///
    /// Represents a MatchResult.
    ///
    pub enum MatchResult(##java.util.regex.MatchResult)


}

namespace Text/Regex/MatchResult {

    use Text/Regex.MatchResult;
    use Text/Regex.MatchResult.{MatchResult};

    def getMatchResult(m: MatchResult): ##java.util.regex.MatchResult = 
        let MatchResult(m1) = m;
        m1

    ///
    /// Return the start position of the match result
    ///
    pub def start(m: MatchResult): Result[Int32, String] =
        import java.util.regex.MatchResult.start(): Int32 & Pure;
        import java.lang.Throwable.getMessage(): String & Pure;
        try {
            getMatchResult(m) |> start |> Ok
        } catch {
            case ex: ##java.lang.Exception => Err("start: ${getMessage(ex)}")
        }

    ///
    /// Return the end position of the match result
    ///
    pub def end(m: MatchResult): Result[Int32, String] =
        import java.util.regex.MatchResult.end(): Int32 & Pure;
        import java.lang.Throwable.getMessage(): String & Pure;
        try {
            getMatchResult(m) |> end |> Ok
        } catch {
            case ex: ##java.lang.Exception => Err("end: ${getMessage(ex)}")
        }

    ///
    /// Return the start and end positions of the match result
    ///
    pub def range(m: MatchResult): Result[{start :: Int32, end :: Int32}, String] =
        use Result.flatMap;
        let* s = start(m);
        let* e = end(m);
        Ok({start = s, end = e})

    ///
    /// Return all of the text of the match result
    ///
    pub def content(m: MatchResult): Result[String, String] =
        import java.util.regex.MatchResult.group(): String & Pure;
        import java.lang.Throwable.getMessage(): String & Pure;
        try {
            getMatchResult(m) |> group |> Ok
        } catch {
            case ex: ##java.lang.Exception => Err("group: ${getMessage(ex)}")
        }

    ///
    /// Return the text of the group at index `i` in the match result
    ///
    /// 0 indexed.
    ///
    pub def groupAt(i: Int32, m: MatchResult): Result[String, String] =
        import java.util.regex.MatchResult.group(Int32): String & Pure;
        import java.lang.Throwable.getMessage(): String & Pure;
        try {
            let m1 = getMatchResult(m);
            group(m1, i-1) |> Ok
        } catch {
            case ex: ##java.lang.Exception => Err("groupAt: ${getMessage(ex)}")
        }

    ///
    /// Return the number of capturing groups in the pattern used by the match result.
    ///
    pub def groupCount(m: MatchResult): Result[Int32, String] =
        import java.util.regex.MatchResult.groupCount(): Int32 & Pure;
        import java.lang.Throwable.getMessage(): String & Pure;
        try {
            getMatchResult(m) |> groupCount |> Ok
        } catch {
            case ex: ##java.lang.Exception => Err("groupAt: ${getMessage(ex)}")
        }


}
