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



mod Text.Regex {

    ///
    /// Represents a MatchResult.
    ///
    pub enum MatchResult(Result[String, ##java.util.regex.MatchResult])


}

mod Text.Regex.MatchResult {

    use Text.Regex.MatchResult;
    use Text.Regex.MatchResult.{MatchResult};

    def getMatchResult(m: MatchResult): Result[String, ##java.util.regex.MatchResult] = 
        let MatchResult(m1) = m;
        m1

    ///
    /// is the match result valid?
    ///
    pub def isValid(m: MatchResult): Bool =
        match start(m) {
            case Err(_) => false
            case Ok(_)  => true
        }
    
    pub def noMatch(): MatchResult = MatchResult(Err("noMatch"))

    pub def throwError(msg: String): MatchResult = MatchResult(Err(msg))

    ///
    /// Return the start position of the match result
    ///
    pub def start(m: MatchResult): Result[String, Int32] =
        import java.util.regex.MatchResult.start(): Int32 \ {};
        import java.lang.Throwable.getMessage(): String \ {};
        try {
            getMatchResult(m) |> Result.map(start)
        } catch {
            case ex: ##java.lang.Exception => Err("start: ${getMessage(ex)}")
        }

    ///
    /// Return the end position of the match result
    ///
    pub def end(m: MatchResult): Result[String, Int32] =
        import java.util.regex.MatchResult.end(): Int32 \ {};
        import java.lang.Throwable.getMessage(): String \ {};
        try {
            getMatchResult(m) |> Result.map(end)
        } catch {
            case ex: ##java.lang.Exception => Err("end: ${getMessage(ex)}")
        }

    ///
    /// Return the start and end positions of the match result
    ///
    pub def range(m: MatchResult): Result[String, {start = Int32, end = Int32}] =
        forM (s <- start(m);
                e <- end(m))
        yield {start = s, end = e}

    ///
    /// Return all of the text of the match result
    ///
    pub def content(m: MatchResult): Result[String, String] =
        import java.util.regex.MatchResult.group(): String \ {};
        import java.lang.Throwable.getMessage(): String \ {};
        try {
            getMatchResult(m) |> Result.map(group)
        } catch {
            case ex: ##java.lang.Exception => Err("group: ${getMessage(ex)}")
        }

    ///
    /// Return the text of the group at index `i` in the match result
    ///
    /// 0 indexed.
    ///
    pub def groupAt(i: Int32, m: MatchResult): Result[String, String] =
        import java.util.regex.MatchResult.group(Int32): String \ {};
        import java.lang.Throwable.getMessage(): String \ {};
        try {
            getMatchResult(m) |> Result.map(m1 -> group(m1, i+1))
        } catch {
            case ex: ##java.lang.Exception => Err("groupAt: ${getMessage(ex)}")
        }

    ///
    /// Return the number of capturing groups in the pattern used by the match result.
    ///
    pub def groupCount(m: MatchResult): Result[String, Int32] =
        import java.util.regex.MatchResult.groupCount(): Int32 \ {};
        try {
            getMatchResult(m) |> Result.map(groupCount)
        } catch {
            case ex: ##java.lang.Exception => 
                import java.lang.Throwable.getMessage(): String \ {};
                Err("groupAt: ${getMessage(ex)}")
        }


}
