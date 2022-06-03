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


/// TODO - if we add MatchResult does make for a tidier API?

namespace Text/Regex {

    pub enum Matcher[_: Region](##java.util.regex.Matcher)

    
    // No New instance - matcher must be created from pattern and input

    instance Scoped[Matcher] {
        pub def regionOf(_: Matcher[r]): Region[r] = () as Region[r]
    }

}

namespace Text/Regex/Matcher {


    use Text/Regex.{Matcher};
    use Text/Regex.Matcher.{Matcher};

    use Text/Regex.MatchResult;
    use Text/Regex.MatchResult.{MatchResult};
    use Text/Regex.Pattern;
    use Text/Regex.Pattern.{Pattern};

    def getMatcher(m: Matcher[r]): ##java.util.regex.Matcher = 
        let Matcher(m1) = m;
        m1

    ///
    /// Create a Matcher for Pattern `patt` on the source String `input`.
    ///
    pub def newMatcher(_: Region[r], p: Pattern, s: String): Matcher[r] \ Write(r) =
        import java.util.regex.Pattern.matcher(##java.lang.CharSequence): ##java.util.regex.Matcher & r;
        let Pattern(p1) = p;
        Matcher(matcher(p1, s as ##java.lang.CharSequence))


    /// ///
    /// /// Fully run a match function.
    /// ///
    /// pub def runMatcher(f: Matcher[r] -> a \ Read(r), p: Pattern, s: String): a =
    ///     region r {
    ///         let m = newMatcher(r, p, s);
    ///         f(m)
    ///     }


    ///
    /// Tests whether the matcher matches all of the input region.
    ///
    pub def matches(m: Matcher[r]): Bool \ Read(r) = 
        import java.util.regex.Matcher.matches(): Bool & r;
        getMatcher(m) |> matches

    ///
    /// Tests whether the matcher matches the start of the input region.
    ///
    pub def lookingAt(m: Matcher[r]): Bool \ Read(r) = 
        import java.util.regex.Matcher.lookingAt(): Bool & r;
        getMatcher(m) |> lookingAt

    ///
    /// Attempt to find the next match. Returns `true` and moves to the next match
    /// if there is a next match otherwise returns `false`.
    ///
    /// The internal state of the matcher is updated.
    ///
    pub def find!(m: Matcher[r]): Bool \ Write(r) = 
        import java.util.regex.Matcher.find(): Bool & r;
        getMatcher(m) |> find

    ///
    /// Attempt to find the start of next match. Returns `Some(position)` if there is a match 
    /// otherwise returns `None`.
    ///
    /// The internal state of the matcher is updated.
    ///
    pub def findStart!(m: Matcher[r]): Option[Int32] \ Write(r) = 
        if (find!(m)) 
            start(m) |> Result.toOption
        else
            None


    ///
    /// Return the MatchResult of the preceeding `find`
    ///
    pub def matchResult(m: Matcher[r]): MatchResult \ Read(r) =
        import java.util.regex.Matcher.toMatchResult(): ##java.util.regex.MatchResult & r;
        import java.lang.Throwable.getMessage(): String & r;
        try {
            getMatcher(m) |> toMatchResult |> Ok |> MatchResult
        } catch {
            case ex: ##java.lang.Exception => MatchResult(Err("matchResult: ${getMessage(ex)}"))
        }


    ///
    /// Return the start position of the string matched by the preceeding `find`
    ///
    pub def start(m: Matcher[r]): Result[Int32, String] \ Read(r) =
        import java.util.regex.Matcher.start(): Int32 & r;
        import java.lang.Throwable.getMessage(): String & r;
        try {
            getMatcher(m) |> start |> Ok
        } catch {
            case ex: ##java.lang.Exception => Err(getMessage(ex))
        }


    ///
    /// Return the end position of the string matched by the preceeding `find`
    ///
    pub def end(m: Matcher[r]): Result[Int32, String] \ Read(r) =
        import java.util.regex.Matcher.end(): Int32 & r;
        import java.lang.Throwable.getMessage(): String & r;
        try {
            getMatcher(m) |> end |> Ok
        } catch {
            case ex: ##java.lang.Exception => Err("end: ${getMessage(ex)}")
        }

    pub def range(m: Matcher[r]): Result[{start :: Int32, end :: Int32}, String] \ Read(r) =
        use Result.flatMap;
        let* s = start(m);
        let* e = end(m);
        Ok({start = s, end = e})

    pub def findLast!(m: Matcher[r]): Bool \ Write(r) =
        match findStart!(m) {
            case None      => false
            case Some(pos) => match findLastHelper(pos, m) {
                case Err(_) => false
                case Ok(end) => Result.getWithDefault(false, findFrom!(end - 1, m))
            }
        }
        
    def findLastHelper(lastPos: Int32, m: Matcher[r]): Result[Int32, String] \ Write(r) =
        let more = find!(m);
        if (more)
            Result.flatMap(pos -> findLastHelper(pos, m), start(m))
        else 
            Ok(lastPos)


    ///
    /// Attempt to find the next match after the supplied position `pos`.
    /// Returns `true` and moves to the next match if there is a next match
    /// otherwise returns `false`.
    ///
    /// The internal state of the matcher is updated.
    ///
    pub def findFrom!(pos: Int32, m: Matcher[r]): Result[Bool, String] \ Write(r) =
        import java.util.regex.Matcher.find(Int32): Bool & r;
        import java.lang.Throwable.getMessage(): String & r;
        try {
            getMatcher(m) |> flip(find, pos) |> Ok
        } catch {
            case ex: ##java.lang.Exception => Err("findFrom!: ${getMessage(ex)}")
        }

    ///
    /// Attempt to find the start of first match starting from `pos`. 
    /// Returns `Some(position)` if there is a match otherwise returns `None`.
    ///
    /// The internal state of the matcher is updated.
    ///
    pub def findStartFrom!(pos: Int32, m: Matcher[r]): Result[Option[Int32], String] \ Write(r) = 
        match findFrom!(pos, m) {
            case Err(msg)  => Err(msg)
            case Ok(true)  => start(m) |> Result.map(Some)
            case Ok(false) => Ok(None)
        }


    pub def findLastFrom!(pos: Int32, m: Matcher[r]): Result[Bool, String] \ Write(r) =
        let step = x -> match x {
            case None         => Ok(false)
            case Some(first)  => match findLastHelper(first, m) {
                case Err(msg) => Err(msg)
                case Ok(end)  => findFrom!(end - 1, m)
            }
        };
        Result.flatMap(step, findStartFrom!(pos, m))


    ///
    /// Return the string matched by the preceeding `find`
    ///
    pub def content(m: Matcher[r]): Result[String, String] \ Read(r) =
        import java.util.regex.Matcher.group(): String & r;
        import java.lang.Throwable.getMessage(): String & r;
        try {
            getMatcher(m) |> group |> Ok
        } catch {
            case ex: ##java.lang.Exception => Err("content: ${getMessage(ex)}")
        }



    ///
    /// Reset this matcher.
    ///
    pub def reset!(m: Matcher[r]): Unit  \ Write(r) =
        import java.util.regex.Matcher.reset(): Unit & r;
        let Matcher(m1) = m;
        let _ = reset(m1);
        ()

    ///
    /// Reset the input of this matcher.
    ///
    pub def resetInput!(s: String, m: Matcher[r]): Unit \ Write(r) =
        import java.util.regex.Matcher.reset(##java.lang.CharSequence): Unit & r;
        let Matcher(m1) = m;
        reset(m1, s as ##java.lang.CharSequence);
        ()

    ///
    /// Set the bounds of the matcher's region.
    ///
    pub def setBounds!(bounds: {start :: Int32, end :: Int32}, m: Matcher[r]): Result[Unit, String] \ Write(r) =
        import java.util.regex.Matcher.region(Int32, Int32): ##java.util.regex.Matcher & r as region1;
        import java.lang.Throwable.getMessage(): String & r;
        try {
            let Matcher(m1) = m;
            let _ = region1(m1, bounds.start, bounds.end);
            Ok()
        } catch {
            case ex: ##java.lang.Exception => Err("setBounds!: ${getMessage(ex)}")
        }

    ///
    /// Change the pattern used by this matcher.
    ///
    pub def setPattern!(p: Pattern, m: Matcher[r]): Result[Unit, String] \ Write(r) =
        import java.util.regex.Matcher.usePattern(##java.util.regex.Pattern): ##java.util.regex.Matcher & r;
        try {
            let Matcher(m1) = m;
            let _ = usePattern(m1, Text/Regex/Pattern.getPattern(p));
            Ok()
        } catch {
            case _: ##java.lang.Exception => Err("usePattern!")
        }


    ///
    /// Replace all the matches found by this matcher with the string `rep`.
    ///
    pub def replaceAll(rep: String, m: Matcher[r]): String \ Write(r) =
        import java.util.regex.Matcher.replaceAll(String): String & r;
        let Matcher(m1) = m;
        replaceAll(m1, rep)

    ///
    /// Replace the first match found by this matcher with the string `rep`.
    ///
    pub def replaceFirst(rep: String, m: Matcher[r]): String \ Write(r) =
        import java.util.regex.Matcher.replaceFirst(String): String & r;
        let Matcher(m1) = m;
        replaceFirst(m1, rep)

    ///
    /// Append replacement...
    ///
    pub def appendReplacement!(rep: String, sb: StringBuilder[r1], m: Matcher[r2]): Unit \ {Write(r1), Write(r2)} =
        import java.util.regex.Matcher.appendReplacement(##java.lang.StringBuilder, String): ##java.util.regex.Matcher & r1 and r2;
        let Matcher(m1) = m;
        let StringBuilder(sb1) = sb;
        let _ = appendReplacement(m1, sb1, rep);
        ()

    ///
    /// Append tail of matcher to string builder...
    ///
    pub def appendTail!(sb: StringBuilder[r1], m: Matcher[r2]): Unit \ {Write(r1), Write(r2)} =
        import java.util.regex.Matcher.appendTail(##java.lang.StringBuilder): ##java.lang.StringBuilder & r1 and r2;
        let Matcher(m1) = m;
        let StringBuilder(sb1) = sb;
        let _ = appendTail(m1, sb1);
        ()



}

