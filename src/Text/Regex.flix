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

    use Regex.Pattern;
    use Regex.Pattern.Pattern;


    // # Matchers

    ///
    /// Returns `Some(ans)` if the entire string `s` is matched by the pattern `p`.
    ///
    pub def matchAll(p: Pattern, s: String): MatchResult =
        use Text/Regex/Matcher.{newMatcher, matches, matchResult};
        region r {
            let m = newMatcher(r, p, s);
            if (matches(m)) 
                matchResult(m)
            else
                Text/Regex/MatchResult.noMatch()
        }

    ///
    /// Returns `Some(ans)` if the entire string `s` is matched by the pattern `p`.
    ///
    pub def matchAllContent(p: Pattern, s: String): Option[String] =
        matchAll(p, s) |> Text/Regex/MatchResult.content |> Result.toOption


    ///
    /// Returns the first result matched by the pattern `p` on the input `s`.
    ///
    pub def firstSubmatch(p: Pattern, s: String): MatchResult =
        use Text/Regex/Matcher.{newMatcher, find!, matchResult};
        region r {
            let m = newMatcher(r, p, s);
            if (find!(m)) 
                matchResult(m)
            else
                Text/Regex/MatchResult.noMatch()
        }

    pub def firstSubmatchContent(p: Pattern, s: String): Option[String] =
        firstSubmatch(p, s) |> Text/Regex/MatchResult.content |> Result.toOption


    ///
    /// Returns the last result matched by the pattern `p` on the input `s`.
    ///
    pub def lastSubmatch(p: Pattern, s: String): MatchResult =
        use Text/Regex/Matcher.{newMatcher, findLast!, matchResult};
        region r {
            let m = newMatcher(r, p, s);
            if (findLast!(m)) 
                matchResult(m)
            else
                Text/Regex/MatchResult.noMatch()
        }
    

    pub def lastSubmatchContent(p: Pattern, s: String): Option[String] =
        lastSubmatch(p, s) |> Text/Regex/MatchResult.content |> Result.toOption


    ///
    /// Returns the first result matched by the pattern `p` on the input `s`.
    ///
    pub def firstSubmatchWithOffset(p: Pattern, o: Int32, s: String): MatchResult =
        use Text/Regex/Matcher.{newMatcher, findFrom!, matchResult};
        region r {
            let m = newMatcher(r, p, s);
            match findFrom!(o, m) {
                case Ok(true) => matchResult(m) 
                case _        => Text/Regex/MatchResult.noMatch()
            }
        }

    pub def firstSubmatchWithOffsetContent(p: Pattern, o: Int32, s: String): Option[String] =
        firstSubmatchWithOffset(p, o, s) |> Text/Regex/MatchResult.content |> Result.toOption

    ///
    /// Returns the last result matched by the pattern `p` on the input `s`.
    ///
    pub def lastSubmatchWithOffset(p: Pattern, o: Int32, s: String): MatchResult =
        use Text/Regex/Matcher.{newMatcher, findLastFrom!, matchResult};
        region r {
            let m = newMatcher(r, p, s);
            match findLastFrom!(o, m) {
                case Ok(true) => matchResult(m)
                case _        => Text/Regex/MatchResult.noMatch()
            }
        }

    pub def lastSubmatchWithOffsetContent(p: Pattern, o: Int32, s: String): Option[String] =
        lastSubmatchWithOffset(p, o, s) |> Text/Regex/MatchResult.content |> Result.toOption


    ///
    /// Returns the start substring of `s` matched by the pattern `p`.
    ///
    pub def matchPrefix(p: Pattern, s: String): MatchResult =
        use Text/Regex/Matcher.{newMatcher, lookingAt, matchResult};
        region r {
            let m = newMatcher(r, p, s);
            if (lookingAt(m)) 
                matchResult(m) 
            else
                Text/Regex/MatchResult.noMatch()
        }

    pub def matchPrefixContent(p: Pattern, s: String): Option[String] =
        matchPrefix(p, s) |> Text/Regex/MatchResult.content |> Result.toOption


    ///
    /// Match input `s` with pattern `p`, must match the whole of the bounded region...
    ///
    pub def isBoundedMatch(start: {start = Int32}, end: {end = Int32}, p: Pattern, s: String): Bool =
        use Text/Regex/Matcher.{newMatcher, setBounds!, matches};
        region r {
            let m = newMatcher(r, p, s);
            match setBounds!(start, end, m) {
                case Err(_) => false
                case Ok()   => matches(m)
            }
        }

    ///
    /// Match input within region
    ///
    pub def isBoundedSubmatch(start: {start = Int32}, end: {end = Int32}, p: Pattern, s: String): Bool =
        use Text/Regex/Matcher.{newMatcher, setBounds!, find!};
        region r {
            let m = newMatcher(r, p, s);
            match setBounds!(start, end, m) {
                case Err(_) => false
                case Ok()   => find!(m)
            }
        }



    /// Match input within region, match must start at start of region...
    pub def boundedStartsWith(start: {start = Int32}, end: {end = Int32}, p: Pattern, s: String): Bool =
        use Text/Regex/Matcher.{newMatcher, setBounds!, lookingAt};
        region r {
            let m = newMatcher(r, p, s);
            match setBounds!(start, end, m) {
                case Err(_) => false
                case Ok()   => lookingAt(m)
            }
        }

    // # Bounded matchers

    ///
    ///
    pub def boundedMatchAll(start: {start = Int32}, end: {end = Int32}, p: Pattern, s: String): MatchResult =
        use Text/Regex/Matcher.{newMatcher, setBounds!, matches, matchResult};
        region r {
            let m = newMatcher(r, p, s);
            match setBounds!(start, end, m) {
                case Err(msg) => Text/Regex/MatchResult.throwError(msg)
                case Ok()     =>
                    if (matches(m)) 
                        matchResult(m) 
                    else
                        Text/Regex/MatchResult.noMatch()
            }
        }


    pub def boundedMatchAllContent(start: {start = Int32}, end: {end = Int32}, p: Pattern, s: String): Option[String] =
        boundedMatchAll(start, end, p, s) |> Text/Regex/MatchResult.content |> Result.toOption

    pub def boundedFirstSubmatch(start: {start = Int32}, end: {end = Int32}, p: Pattern, s: String): MatchResult =
        use Text/Regex/Matcher.{newMatcher, setBounds!, find!, matchResult};
        region r {
            let m = newMatcher(r, p, s);
            match setBounds!(start, end, m) {
                case Err(msg) => Text/Regex/MatchResult.throwError(msg)
                case Ok()     =>
                    if (find!(m)) 
                        matchResult(m)
                    else
                        Text/Regex/MatchResult.noMatch()
            }
        }

    pub def boundedFirstSubmatchContent(start: {start = Int32}, end: {end = Int32}, p: Pattern, s: String): Option[String] =
        boundedFirstSubmatch(start, end, p, s) |> Text/Regex/MatchResult.content |> Result.toOption

    pub def boundedLastSubmatch(start: {start = Int32}, end: {end = Int32}, p: Pattern, s: String): MatchResult =
        use Text/Regex/Matcher.{newMatcher, setBounds!, findLast!, matchResult};
        region r {
            let m = newMatcher(r, p, s);
            match setBounds!(start, end, m) {
                case Err(msg) => Text/Regex/MatchResult.throwError(msg)
                case Ok()   =>
                    if (findLast!(m)) 
                        matchResult(m)
                    else
                        Text/Regex/MatchResult.noMatch()
            }
        }

    pub def boundedLastSubmatchContent(start: {start = Int32}, end: {end = Int32}, p: Pattern, s: String): Option[String] =
        boundedLastSubmatch(start, end, p, s) |> Text/Regex/MatchResult.content |> Result.toOption

    pub def boundedMatchPrefix(start: {start = Int32}, end: {end = Int32}, p: Pattern, s: String): MatchResult =
        use Text/Regex/Matcher.{newMatcher, setBounds!, lookingAt, matchResult};
        region r {
            let m = newMatcher(r, p, s);
            match setBounds!(start, end, m) {
                case Err(msg) => Text/Regex/MatchResult.throwError(msg)
                case Ok()   =>
                    if (lookingAt(m)) 
                        matchResult(m)
                    else
                        Text/Regex/MatchResult.noMatch()
            }
        }

    pub def boundedMatchPrefixContent(start: {start = Int32}, end: {end = Int32}, p: Pattern, s: String): Option[String] =
        boundedMatchPrefix(start, end, p, s) |> Text/Regex/MatchResult.content |> Result.toOption


    // # Multiple results

    ///
    /// TODO: use a "Stream" when stdlib has an implementation finalized
    ///
    pub def matchResultsAll(p: Pattern, s: String): List[MatchResult] =
        use Text/Regex/Matcher.{newMatcher};
        region r {
            let m = newMatcher(r, p, s);
            allMatchResultsHelper(m, identity)
        }


    def allMatchResultsHelper(m: Matcher[r], k: List[MatchResult] -> List[MatchResult]): List[MatchResult] \Read(r) =
        use Text/Regex/Matcher.{find!, matchResult};
        if (find!(m))
            match matchResult(m) {
                case r1 if not Text/Regex/MatchResult.isValid(r1) => k(Text/Regex/MatchResult.throwError("matchResultsAll Fail") :: Nil)
                case r1                                           => allMatchResultsHelper(m, ks -> k(r1 :: ks))
            }
        else
            k(Nil)




        
    /// TODO split - Iterator / stream version...


    ///
    /// Splits the string `s` around matches of the pattern `p` within the region `bounds`.
    ///
    pub def boundedSplit(start: {start = Int32}, end: {end = Int32}, patt: Pattern, input: String): List[String] =
        let input1 = String.slice(start, end, input);
        Regex.split(pattern = patt, input1)




    /// Rewrite Strategies

    pub def rewriteAll(f: String -> String, p: Pattern, s: String): String =
        use Text/Regex/Matcher.{newMatcher};
        region rc {
            let m = newMatcher(rc, p, s);
            let sb = StringBuilder.new(rc);
            rewriteAllHelper(f, sb, m);
            StringBuilder.toString(sb)
        }

    def rewriteAllHelper(f: String -> String, sb: StringBuilder[r], m: Matcher[r]): Unit \ Write(r) =
        use Text/Regex/Matcher.{find!, matchResult, appendReplacement!, appendTail!};
        use Text/Regex/MatchResult.{content};
        if (find!(m))
            match matchResult(m) |> content {
                case Err(_) => ()
                case Ok(s)  => {appendReplacement!(f(s), sb, m); rewriteAllHelper(f, sb, m)}
            }
        else
            appendTail!(sb, m)


    pub def rewriteSome(f: String -> Option[String], p: Pattern, s: String): String =
        use Text/Regex/Matcher.{newMatcher};
        region rc {
            let m = newMatcher(rc, p, s);
            let sb = StringBuilder.new(rc);
            rewriteSomeHelper(f, sb, m);
            StringBuilder.toString(sb)
        }

    def rewriteSomeHelper(f: String -> Option[String], sb: StringBuilder[r], m: Matcher[r]): Unit \ Write(r) =
        use Text/Regex/Matcher.{find!, matchResult, appendReplacement!, appendTail!};
        use Text/Regex/MatchResult.{content};
        if (find!(m))
            match matchResult(m) |> content {
                case Err(_) => ()
                case Ok(s)  => match f(s) {
                    case Some(s1) => {appendReplacement!(s1, sb, m); rewriteSomeHelper(f, sb, m)}
                    case None     => {appendReplacement!(s, sb, m); rewriteSomeHelper(f, sb, m)}
                }
            }
        else
            appendTail!(sb, m)

    pub def rewriteAllWithIndex(f: (Int32, String) -> String, p: Pattern, s: String): String =
        use Text/Regex/Matcher.{newMatcher};
        region rc {
            let m = newMatcher(rc, p, s);
            let sb = StringBuilder.new(rc);
            rewriteAllWithIndexHelper(f, 0, sb, m);
            StringBuilder.toString(sb)
        }

    def rewriteAllWithIndexHelper(f: (Int32, String) -> String, acc: Int32, sb: StringBuilder[r], m: Matcher[r]): Unit \ Write(r) =
        use Text/Regex/Matcher.{find!, matchResult, appendReplacement!, appendTail!};
        use Text/Regex/MatchResult.{content};
        if (find!(m))
            match matchResult(m) |> content {
                case Err(_) => ()
                case Ok(s)  => {appendReplacement!(f(acc, s), sb, m); rewriteAllWithIndexHelper(f, acc+1, sb, m)}
            }
        else
            appendTail!(sb, m)


    pub def rewriteSomeWithIndex(f: (Int32, String) -> Option[String], p: Pattern, s: String): String =
        use Text/Regex/Matcher.{newMatcher};
        region rc {
            let m = newMatcher(rc, p, s);
            let sb = StringBuilder.new(rc);
            rewriteSomeWithIndexHelper(f, 0, sb, m);
            StringBuilder.toString(sb)
        }

    def rewriteSomeWithIndexHelper(f: (Int32, String) -> Option[String], acc: Int32, sb: StringBuilder[r], m: Matcher[r]): Unit \ Write(r) =
        use Text/Regex/Matcher.{find!, matchResult, appendReplacement!, appendTail!};
        use Text/Regex/MatchResult.{content};
        if (find!(m))
            match matchResult(m) |> content {
                case Err(_) => ()
                case Ok(s)  => match f(acc, s) {
                    case Some(s1) => {appendReplacement!(s1, sb, m); rewriteSomeWithIndexHelper(f, acc+1, sb, m)}
                    case None     => {appendReplacement!(s, sb, m); rewriteSomeWithIndexHelper(f, acc+1, sb, m)}
                }
            }
        else
            appendTail!(sb, m)


    pub def rewriteFirstN(ntimes: Int32, f: String -> String, p: Pattern, s: String): String =
        use Text/Regex/Matcher.{newMatcher};
        if (ntimes <= 0 )
            s
        else 
            region rc {
                let m = newMatcher(rc, p, s);
                let sb = StringBuilder.new(rc);
                rewriteFirstNHelper(ntimes, f, 0, sb, m);
                StringBuilder.toString(sb)
            }

    def rewriteFirstNHelper(ntimes: Int32, f: String -> String, acc: Int32, sb: StringBuilder[r], m: Matcher[r]): Unit \ Write(r) =
        use Text/Regex/Matcher.{find!, matchResult, appendReplacement!, appendTail!};
        use Text/Regex/MatchResult.{content};
        if (ntimes < acc and find!(m))
            match matchResult(m) |> content {
                case Err(_) => ()
                case Ok(s)  => {appendReplacement!(f(s), sb, m); rewriteFirstNHelper(ntimes, f, acc+1, sb, m)}
            }
        else
            appendTail!(sb, m)

}
