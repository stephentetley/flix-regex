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
    use Text/Regex/Internal.PrimMatcher;

    // # Helpers

    def testMatch(test: ##java.util.regex.Matcher -> Bool & ef,
                    modify!: ##java.util.regex.Matcher -> Result[Unit, String] & Impure,
                    patt: Pattern,
                    input: String): Bool =
        match (testMatchHelper(test, modify!, patt, input) as & Pure) {
            case Err(_) => false
            case Ok(ans) => ans
        }

    def testMatchHelper(test: ##java.util.regex.Matcher -> Bool & ef,
                        modify!: ##java.util.regex.Matcher -> Result[Unit, String] & Impure,
                        patt: Pattern,
                        input: String): Result[Bool, String] & Impure =

        use Result.flatMap;
        let Pattern(patt1) = patt;
        let m1 = Text/Regex/Internal/Prim.newMatcher(patt1, input);
        let* _ = modify!(m1);
        Ok(test(m1))

    def doNotModify(_: ##java.util.regex.Matcher): Result[Unit, String] & Impure = Ok(()) as & Impure

    def setRegion!(bounds: {start:: Int32, end :: Int32}, x: ##java.util.regex.Matcher): Result[Unit, String] & Impure =
        Text/Regex/Internal/Prim.setRegion!(x, bounds)


    def extract(test: ##java.util.regex.Matcher -> Bool & ef,
                modify!: ##java.util.regex.Matcher -> Result[Unit, String] & Impure,
                patt: Pattern,
                input: String): Option[String] =
        extractHelper(test, modify!, patt, input) as & Pure


    def extractHelper(test: ##java.util.regex.Matcher -> Bool & ef,
                        modify!: ##java.util.regex.Matcher -> Result[Unit, String] & Impure,
                        patt: Pattern,
                        input: String): Option[String] & Impure =
        use Option.flatMap;
        try {
            let Pattern(patt1) = patt;
            let m1 = Text/Regex/Internal/Prim.newMatcher(patt1, input);
            let* _ = Result.toOption(modify!(m1));
            if (test(m1))
                Result.toOption(Text/Regex/Internal/Prim.group(m1))
            else
                None
        } catch {
            case _: ##java.lang.Exception => None
        }






    // # Predicates

    ///
    /// Returns `true` if the entire string `input` is matched by the pattern `patt`.
    ///
    pub def isMatch(patt: Pattern, input: String): Bool =
        testMatch(Text/Regex/Internal/Prim.matches, doNotModify, patt, input)

    ///
    /// Returns `true` if the string `input` is matched by the regular expression pattern `patt`
    /// at any position within the string.
    ///
    pub def isSubmatch(patt: Pattern, input: String): Bool =
        testMatch(Text/Regex/Internal/Prim.find!, doNotModify, patt, input)


    ///
    /// Returns `true` if the string `input` starts the regular expression pattern `patt`.
    ///
    pub def startsWith(patt: Pattern, input: String): Bool =
        testMatch(Text/Regex/Internal/Prim.lookingAt, doNotModify, patt, input)

    ///
    /// Returns `true` if the string `input` ends the regular expression pattern `patt`.
    ///
    /// This may be slower than `startsWith` because there is no primitive Java function
    /// to call, instead the matches of `patt` are iterated until the last one is found.
    ///
    pub def endsWith(patt: Pattern, input: String): Bool =
        match rangeOfRight(patt, input) {
            case None      => false
            case Some(rng) => String.length(input) - 1 == rng.end
        }



    // # Simple extractors

    ///
    /// Returns `true` if the entire string `input` is matched by the pattern `patt`.
    ///
    pub def matches(patt: Pattern, input: String): Option[String] =
        extract(Text/Regex/Internal/Prim.matches, doNotModify, patt, input)


    ///
    /// Returns the first substring of `input` matched by the pattern `patt`.
    ///
    /// TODO `contains` returns Bool in String - needs new name
    ///
    pub def contains(patt: Pattern, input: String): Option[String] =
        extract(Text/Regex/Internal/Prim.find!, doNotModify, patt, input)

    ///
    /// Returns the start of `input` matched by the pattern `patt`.
    ///
    pub def prefix(patt: Pattern, input: String): Option[String] =
        extract(Text/Regex/Internal/Prim.lookingAt, doNotModify, patt, input)





    ///
    /// Match input within region, `patt` must match the whole region...
    ///
    pub def isBoundedMatch(bounds: {start :: Int32, end :: Int32}, patt: Pattern, input: String): Bool =
        testMatch(Text/Regex/Internal/Prim.matches, setRegion!(bounds), patt, input)

    ///
    /// Match input within region
    ///
    pub def isBoundedSubmatch(bounds: {start :: Int32, end :: Int32}, patt: Pattern, input: String): Bool =
        testMatch(Text/Regex/Internal/Prim.find!, setRegion!(bounds), patt, input)


    /// Match input within region, match must start at start of region...
    pub def boundedStartsWith(bounds: {start :: Int32, end :: Int32}, patt: Pattern, input: String): Bool =
        testMatch(Text/Regex/Internal/Prim.lookingAt, setRegion!(bounds), patt, input)


    // # Bounded extractors

    ///
    ///
    pub def boundedMatches(bounds: {start :: Int32, end :: Int32}, patt: Pattern, input: String): Option[String] =
        extract(Text/Regex/Internal/Prim.matches, setRegion!(bounds), patt, input)

    pub def boundedContains(bounds: {start :: Int32, end :: Int32}, patt: Pattern, input: String): Option[String] =
        extract(Text/Regex/Internal/Prim.find!, setRegion!(bounds), patt, input)

    pub def boundedPrefix(bounds: {start :: Int32, end :: Int32}, patt: Pattern, input: String): Option[String] =
        extract(Text/Regex/Internal/Prim.lookingAt, setRegion!(bounds), patt, input)



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



    // # String API functions using regexs

    ///
    /// Count the occurences of `patt` in string `s`.
    ///
    pub def countSubmatches(patt: Pattern, s: String): Int32 =
        Text/Regex/Matcher.runMatcher(countMatcher(0), patt, s) as & Pure
            |> Result.getWithDefault(0)

    def countMatcher(acc: Int32): Matcher[Int32] & Impure =
        use Text/Regex/Matcher.{flatMap, find!, point};
        let* first = find!();
        if (first) countMatcher(acc+1) else point(acc)


    ///
    /// Splits the string `s` around matches of the pattern `p`.
    ///
    pub def split(patt: Pattern, input: String): Array[String] & Impure =
        import java.util.regex.Pattern.split(##java.lang.CharSequence): Array[String] & Impure;
        let Pattern(p1) = patt;
        let cs1 = input as ##java.lang.CharSequence;
        split(p1, cs1)

    ///
    /// Splits the string `s` around matches of the pattern `p` within the region `bounds`.
    ///
    pub def boundedSplit(bounds: {start :: Int32, end :: Int32}, patt: Pattern, input: String): Array[String] & Impure =
        let input1 = String.slice(bounds.start, bounds.end, input);
        split(patt, input1)

    ///
    /// Returns `input` with every match of the pattern `from` replaced by the string `to`.
    ///
    pub def replace(from: {from :: Pattern}, to: {to :: String}, input: String): String =
        let Pattern(patt1) = from.from;
        let m1 = Text/Regex/Internal/Prim.newMatcher(patt1, input) as & Pure;
        Text/Regex/Internal/Prim.replaceAll(m1, to.to) as & Pure

    ///
    /// Returns `input` with the first match of the regular expression `patt` replaced by the string `rep`.
    ///
    pub def replaceFirstMatch(from: {from :: Pattern}, to: {to :: String}, input: String): String =
        let Pattern(patt1) = from.from;
        let m1 = Text/Regex/Internal/Prim.newMatcher(patt1, input) as & Pure;
        Text/Regex/Internal/Prim.replaceFirst(m1, to.to) as & Pure



    /// Range of leftmost match
    pub def rangeOfLeft(patt: Pattern, s: String): Option[{start :: Int32, end :: Int32}] =
        rangeOfHelper(Text/Regex/Matcher.find!(), patt, s) as & Pure

    /// Range of rightmost match
    pub def rangeOfRight(patt: Pattern, s: String): Option[{start :: Int32, end :: Int32}] =
        rangeOfHelper(Text/Regex/Matcher.findLast!(), patt, s) as & Pure

    def rangeOfHelper(finder: Matcher[Bool], patt: Pattern, s: String): Option[{start :: Int32, end :: Int32}] & Impure =
        use Text/Regex/Matcher.{flatMap, range, throwError};
        let m1 = {
            let* bfound = finder;
            if (bfound) range() else throwError("none")
        };
        Text/Regex/Matcher.runMatcher(m1, patt, s) |> Result.toOption

    ///
    /// Return the index of the first occurence of `patt` in `s` from the left.
    ///
    /// If `patt` is not present in `s` return None.
    ///
    /// If `patt` is the empty string return None.
    ///
    pub def indexOfLeft(patt: Pattern, s: String): Option[Int32] =
        rangeOfLeft(patt, s) |> Option.map(r -> r.start)

    ///
    /// Find index of rightmost match.
    ///
    pub def indexOfRight(patt: Pattern, s: String): Option[Int32] =
        rangeOfRight(patt, s) |> Option.map(r -> r.start)



    ///
    /// This is `indexOfLeft` with a start offset.
    ///
    /// Returns `None` if `patt` does not match.
    ///
    pub def rangeOfLeftWithOffset(patt: Pattern, offset: Int32, s: String): Option[{start :: Int32, end :: Int32}] =
        rangeOfHelper(Text/Regex/Matcher.findFrom!(offset), patt, s) as & Pure

    ///
    /// This is `indexOfRight` with a start offset.
    ///
    /// Returns `None` if `patt` does not match.
    ///
    pub def rangeOfRightWithOffset(patt: Pattern, offset: Int32, s: String): Option[{start :: Int32, end :: Int32}] =
        rangeOfHelper(Text/Regex/Matcher.findLastFrom!(offset), patt, s) as & Pure



    ///
    /// Find the first instance of `patt` in string `s`, return a pair of the
    /// prefix of string `s` up to `sub` and the rest of string `s` including `sub`.
    ///
    pub def breakOnLeft(patt: Pattern, s: String): (String, String) =
        match rangeOfLeft(patt, s) {
            case None    => (s, "")
            case Some(r) => (String.sliceLeft(r.start, s), String.sliceRight(r.start, s))
        }

    pub def breakAfterLeft(patt: Pattern, s: String): (String, String) =
        match rangeOfLeft(patt, s) {
            case None    => (s, "")
            case Some(r) => (String.sliceLeft(r.end, s), String.sliceRight(r.end, s))
        }


    ///
    /// Find the last instance of `substr` in string `s`, return a pair of the
    /// initial string including `substr` and suffix from `substr`.
    ///
    pub def breakOnRight(patt: Pattern, s: String): (String, String) =
        match rangeOfRight(patt, s) {
            case None    => (s, "")
            case Some(r) => (String.sliceLeft(r.end, s), String.sliceRight(r.end, s))
        }

    ///
    /// Find the last instance of `substr` in string `s`, return a pair of the
    /// initial string including `substr` and suffix from `substr`.
    ///
    pub def breakBeforeRight(patt: Pattern, s: String): (String, String) =
        match rangeOfRight(patt, s) {
            case None    => (s, "")
            case Some(r) => (String.sliceLeft(r.start, s), String.sliceRight(r.start, s))
        }

    ///
    /// Returns `Some(suffix)` of string `s` if its prefix matches `patt`.
    ///
    pub def stripPrefix(patt: Pattern, s: String): Option[String] =
        (rangeOfHelper(Text/Regex/Matcher.lookingAt(), patt, s) as & Pure)
            |> Option.map(rng -> String.takeRight(rng.end, s))

    // stripSuffix
    pub def stripSuffix(patt: Pattern, s: String): Option[String] =
        rangeOfRight(patt, s)
            |> Option.flatMap(rng -> if (String.length(s) - 1 == rng.end) Some(String.takeLeft(rng.start, s)) else None)


}
