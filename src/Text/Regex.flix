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

namespace Text/Regex {

    use Text/Regex.Pattern;
    use Text/Regex.Pattern.Pattern;


    // # Predicates
    // Names follow `String.flix`

    ///
    /// Returns `true` if the entire string `s` is matched by the pattern `patt`.
    ///
    pub def isMatch(p: Pattern, s: String): Bool =
        use Text/Regex/Matcher.{newMatcher, matches};
        region r {
            newMatcher(r, p, s) |> matches
        }


    ///
    /// Returns `true` if the string `input` is matched by the regular expression pattern `p`
    /// at any position within the string `s`.
    ///
    pub def isSubmatch(p: Pattern, s: String): Bool =
        use Text/Regex/Matcher.{newMatcher, find!};
        region r {
            newMatcher(r, p, s) |> find!
        }

    ///
    /// Returns `true` if the string `s` starts the regular expression pattern `p`.
    ///
    pub def startsWith(p: Pattern, s: String): Bool =
        use Text/Regex/Matcher.{newMatcher, lookingAt};
        region r {
            newMatcher(r, p, s) |> lookingAt
        }

    ///
    /// Returns `true` if the string `input` ends the regular expression pattern `patt`.
    ///
    /// This will be slower than `startsWith` because there is no primitive Java function
    /// to call, instead the matches of `patt` are iterated until the last one is found.
    ///
    pub def endsWith(p: Pattern, s: String): Bool =
        match lastSubmatch(p, s) |> Option.toOk("err") |> Result.flatMap(Text/Regex/MatchResult.range) {
            case Err(_)  => false
            case Ok(rng) => String.length(s) - 1 == rng.end
        }


    // # Simple extractors

    ///
    /// Returns `Some(ans)` if the entire string `s` is matched by the pattern `p`.
    ///
    pub def matchAll(p: Pattern, s: String): Option[MatchResult] =
        use Text/Regex/Matcher.{newMatcher, matches, matchResult};
        region r {
            let m = newMatcher(r, p, s);
            if (matches(m)) 
                matchResult(m) |> Result.toOption 
            else
                None
        }


    ///
    /// Returns the first result matched by the pattern `p` on the input `s`.
    ///
    pub def firstSubmatch(p: Pattern, s: String): Option[MatchResult] =
        use Text/Regex/Matcher.{newMatcher, find!, matchResult};
        region r {
            let m = newMatcher(r, p, s);
            if (find!(m)) 
                matchResult(m) |> Result.toOption 
            else
                None
        }


    ///
    /// Returns the last result matched by the pattern `p` on the input `s`.
    ///
    pub def lastSubmatch(p: Pattern, s: String): Option[MatchResult] =
        use Text/Regex/Matcher.{newMatcher, findLast!, matchResult};
        region r {
            let m = newMatcher(r, p, s);
            if (findLast!(m)) 
                matchResult(m) |> Result.toOption 
            else
                None
        }
    
    ///
    /// Returns the last result matched by the pattern `p` on the input `s`.
    ///
    pub def lastSubmatchWithOffset(p: Pattern, o: Int32, s: String): Option[MatchResult] =
        use Text/Regex/Matcher.{newMatcher, findLastFrom!, matchResult};
        region r {
            let m = newMatcher(r, p, s);
            match findLastFrom!(o, m) {
                case Ok(true) => matchResult(m) |> Result.toOption 
                case _        => None
            }
        }

    ///
    /// Returns the first result matched by the pattern `p` on the input `s`.
    ///
    pub def firstSubmatchWithOffset(p: Pattern, o: Int32, s: String): Option[MatchResult] =
        use Text/Regex/Matcher.{newMatcher, findFrom!, matchResult};
        region r {
            let m = newMatcher(r, p, s);
            match findFrom!(o, m) {
                case Ok(true) => matchResult(m) |> Result.toOption 
                case _        => None
            }
        }


    ///
    /// Returns the start substring of `s` matched by the pattern `p`.
    ///
    pub def matchPrefix(p: Pattern, s: String): Option[MatchResult] =
        use Text/Regex/Matcher.{newMatcher, lookingAt, matchResult};
        region r {
            let m = newMatcher(r, p, s);
            if (lookingAt(m)) 
                matchResult(m) |> Result.toOption 
            else
                None
        }

    ///
    /// Match input `s` with pattern `p`, must match the whole of the bounded region...
    ///
    pub def isBoundedMatch(bounds: {start :: Int32, end :: Int32}, p: Pattern, s: String): Bool =
        use Text/Regex/Matcher.{newMatcher, setBounds!, matches};
        region r {
            let m = newMatcher(r, p, s);
            match setBounds!(bounds, m) {
                case Err(_) => false
                case Ok()   => matches(m)
            }
        }

    ///
    /// Match input within region
    ///
    pub def isBoundedSubmatch(bounds: {start :: Int32, end :: Int32}, p: Pattern, s: String): Bool =
        use Text/Regex/Matcher.{newMatcher, setBounds!, find!};
        region r {
            let m = newMatcher(r, p, s);
            match setBounds!(bounds, m) {
                case Err(_) => false
                case Ok()   => find!(m)
            }
        }



    /// Match input within region, match must start at start of region...
    pub def boundedStartsWith(bounds: {start :: Int32, end :: Int32}, p: Pattern, s: String): Bool =
        use Text/Regex/Matcher.{newMatcher, setBounds!, lookingAt};
        region r {
            let m = newMatcher(r, p, s);
            match setBounds!(bounds, m) {
                case Err(_) => false
                case Ok()   => lookingAt(m)
            }
        }

    // # Bounded extractors

    ///
    ///
    pub def boundedMatchAll(bounds: {start :: Int32, end :: Int32}, p: Pattern, s: String): Option[MatchResult] =
        use Text/Regex/Matcher.{newMatcher, setBounds!, matches, matchResult};
        region r {
            let m = newMatcher(r, p, s);
            match setBounds!(bounds, m) {
                case Err(_) => None
                case Ok()   =>
                    if (matches(m)) 
                        matchResult(m) |> Result.toOption 
                    else
                        None
            }
        }


    pub def boundedFirstSubmatch(bounds: {start :: Int32, end :: Int32}, p: Pattern, s: String): Option[MatchResult] =
        use Text/Regex/Matcher.{newMatcher, setBounds!, find!, matchResult};
        region r {
            let m = newMatcher(r, p, s);
            match setBounds!(bounds, m) {
                case Err(_) => None
                case Ok()   =>
                    if (find!(m)) 
                        matchResult(m) |> Result.toOption 
                    else
                        None
            }
        }

    pub def boundedLastSubmatch(bounds: {start :: Int32, end :: Int32}, p: Pattern, s: String): Option[MatchResult] =
        use Text/Regex/Matcher.{newMatcher, setBounds!, findLast!, matchResult};
        region r {
            let m = newMatcher(r, p, s);
            match setBounds!(bounds, m) {
                case Err(_) => None
                case Ok()   =>
                    if (findLast!(m)) 
                        matchResult(m) |> Result.toOption 
                    else
                        None
            }
        }


    pub def boundedMatchPrefix(bounds: {start :: Int32, end :: Int32}, p: Pattern, s: String): Option[MatchResult] =
        use Text/Regex/Matcher.{newMatcher, setBounds!, lookingAt, matchResult};
        region r {
            let m = newMatcher(r, p, s);
            match setBounds!(bounds, m) {
                case Err(_) => None
                case Ok()   =>
                    if (lookingAt(m)) 
                        matchResult(m) |> Result.toOption 
                    else
                        None
            }
        }

/*

    // # Multiple results

    ///
    /// TODO: use a "Stream" when stdlib has an implementation finalized
    ///
    def results(patt: Pattern, eval: ##java.util.regex.Matcher -> Result[a, String] & Impure, input: String): Result[List[a], String] & Impure =
        let Pattern(patt1) = patt;
        let m1 = Text/Regex/Internal/Prim.newMatcher(patt1, input);
        let iter = _ -> match Text/Regex/Internal/Prim.find!(m1) {
            case true  => eval(m1) |> Result.map(Some)
            case false => Ok(None)
        };
        List.unfoldWithOkIter(iter)

    ///
    /// Stream...
    ///
    pub def contents(patt: Pattern, input: String): Result[List[String], String] & Impure =
        results(patt, Text/Regex/Internal/Prim.group, input)

    ///
    /// Stream...
    ///
    pub def ranges(patt: Pattern, input: String): Result[List[{start :: Int32, end :: Int32}], String] & Impure =
        use Result.flatMap;
        let getRange = x -> {
            let* start  = Text/Regex/Internal/Prim.start(x);
            let* end    = Text/Regex/Internal/Prim.end(x);
            Ok({start = start, end = end})
        };
        results(patt, getRange, input)



    // # String API functions using regexes

    ///
    /// Count the occurences of `patt` in string `s`.
    ///
    pub def countSubmatches(patt: Pattern, s: String): Int32 =
        Text/Regex/Matcher.runMatcher(countMatcher(0), patt, s)
            |> Result.getWithDefault(0)

    def countMatcher(acc: Int32, m: Matcher[r]): Int32 \Read(r) =
        let first = Text/Regex/Matcher.find!(m);
        if (first) countMatcher(acc+1) else acc

*/
    ///
    /// Splits the string `s` around matches of the pattern `p`.
    ///
    pub def split(p: Pattern, s: String): List[String] =
        import java.util.regex.Pattern.split(##java.lang.CharSequence): Array[String] & Impure;
        let p1 = Text/Regex/Pattern.getPattern(p);
        (split(p1, s as ##java.lang.CharSequence) |> Array.toList) as & Pure
        

    ///
    /// Splits the string `s` around matches of the pattern `p` within the region `bounds`.
    ///
    pub def boundedSplit(bounds: {start :: Int32, end :: Int32}, patt: Pattern, input: String): List[String] =
        let input1 = String.slice(bounds.start, bounds.end, input);
        split(patt, input1)


    ///
    /// Returns string `s` with every match of the pattern `from` replaced by the string `to`.
    ///
    pub def replace(from: {from :: Pattern}, to: {to :: String}, s: String): String =
        region r {
            let m1 = Text/Regex/Matcher.newMatcher(r, from.from, s);
            Text/Regex/Matcher.replaceAll(to.to, m1)
        }

    ///
    /// Returns string `s` with the first match of the regular expression `patt` replaced by the string `rep`.
    ///
    pub def replaceFirstMatch(from: {from :: Pattern}, to: {to :: String}, s: String): String =
        region r {
            let m1 = Text/Regex/Matcher.newMatcher(r, from.from, s);
            Text/Regex/Matcher.replaceFirst(to.to, m1)
        }


    ///
    /// Return the index of the first occurence of `patt` in `s` from the left.
    ///
    /// If `patt` is not present in `s` return None.
    ///
    /// If `patt` is the empty string return None.
    ///
    pub def indexOfLeft(p: Pattern, s: String): Option[Int32] =
        Option.flatMap(a -> Text/Regex/MatchResult.start(a) |> Result.toOption, firstSubmatch(p, s))

    ///
    /// Find index of rightmost match.
    ///
    pub def indexOfRight(p: Pattern, s: String): Option[Int32] =
        Option.flatMap(a -> Text/Regex/MatchResult.start(a) |> Result.toOption, lastSubmatch(p, s))


    ///
    /// Find the first instance of `patt` in string `s`, return a pair of the
    /// prefix of string `s` up to `sub` and the rest of string `s` including `sub`.
    ///
    pub def breakOnLeft(p: Pattern, s: String): (String, String) =
        match firstSubmatch(p, s) |> Option.toOk("err") |> Result.flatMap(Text/Regex/MatchResult.range) {
            case Err(_) => (s, "")
            case Ok(r)  => (String.sliceLeft(r.start, s), String.sliceRight(r.start, s))
        }

    pub def breakAfterLeft(p: Pattern, s: String): (String, String) =
        match firstSubmatch(p, s) |> Option.toOk("err") |> Result.flatMap(Text/Regex/MatchResult.range) {
            case Err(_) => (s, "")
            case Ok(r)  => (String.sliceLeft(r.end, s), String.sliceRight(r.end, s))
        }


    ///
    /// Find the last instance of `substr` in string `s`, return a pair of the
    /// initial string including `substr` and suffix from `substr`.
    ///
    pub def breakOnRight(p: Pattern, s: String): (String, String) =
        match lastSubmatch(p, s) |> Option.toOk("err") |> Result.flatMap(Text/Regex/MatchResult.range) {
            case Err(_) => (s, "")
            case Ok(r)  => (String.sliceLeft(r.end, s), String.sliceRight(r.end, s))
        }

    ///
    /// Find the last instance of `substr` in string `s`, return a pair of the
    /// initial string including `substr` and suffix from `substr`.
    ///
    pub def breakBeforeRight(p: Pattern, s: String): (String, String) =
        match lastSubmatch(p, s) |> Option.toOk("err") |> Result.flatMap(Text/Regex/MatchResult.range) {
            case Err(_) => (s, "")
            case Ok(r)  => (String.sliceLeft(r.start, s), String.sliceRight(r.start, s))
        }



    ///
    /// Returns `Some(suffix)` of string `s` if its prefix matches `patt`.
    ///
    pub def stripPrefix(p: Pattern, s: String): Option[String] =
        match matchPrefix(p, s) |> Option.toOk("err") |> Result.flatMap(Text/Regex/MatchResult.range) {
            case Err(_) => None
            case Ok(r)  => Some(String.takeRight(r.end, s))
        }



    // stripSuffix
    pub def stripSuffix(p: Pattern, s: String): Option[String] =
        match lastSubmatch(p, s) |> Option.toOk("err") |> Result.flatMap(Text/Regex/MatchResult.range) {
            case Err(_) => None
            case Ok(r)  => if (r.end == String.length(s) - 1) Some(String.takeLeft(r.start, s)) else None
        }

    /// Rewrite Strategies

/*
    pub def rewriteAll(fn: String -> String, p: Pattern, s: String): String =
        let* more = find();
        if (more) {
            let* s = content();
            let* _ = appendReplacement!(fn(s));
            rewriteAll!(fn)
        } else
            appendTail!()
*/

}
