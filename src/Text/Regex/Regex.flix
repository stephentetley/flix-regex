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
    use Text/Regex.MatchEvaluator;
    use Text/Regex.MatchEvaluator.{MatchEvaluator};
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

    def notModified(_: ##java.util.regex.Matcher): Result[Unit, String] & Impure = Ok(()) as & Impure

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

    // Design note - if a function uses a MatchEvaluator it should return `Result[a, String]`
    // as MatchEvaluator has user supplied error messages.

    def evaluate(test: ##java.util.regex.Matcher -> Bool & ef,
                    modify!: ##java.util.regex.Matcher -> Result[Unit, String] & Impure,
                    patt: Pattern,
                    eval: MatchEvaluator[a],
                    input: String): Result[a, String] & Impure =
        use Result.flatMap;
        let Pattern(patt1) = patt;
        let m1 = Text/Regex/Internal/Prim.newMatcher(patt1, input);
        let* _ = modify!(m1);
        if (test(m1)) {
            let MatchEvaluator(fn) = eval;
            fn(m1)
        } else
            Err("evalWithTest - no match")

    /// Return `Option[a]` with None indicating "no match" (no match being necessarily an error).
    def evaluateOptional(test: ##java.util.regex.Matcher -> Bool & ef,
                            modify!: ##java.util.regex.Matcher -> Result[Unit, String] & Impure,
                            patt: Pattern,
                            eval: MatchEvaluator[a],
                            input: String): Result[Option[a], String] & Impure =
        use Result.flatMap;
        let Pattern(patt1) = patt;
        let m1 = Text/Regex/Internal/Prim.newMatcher(patt1, input);
        let* _ = modify!(m1);
        if (test(m1)) {
            let MatchEvaluator(fn) = eval;
            fn(m1) |> Result.map(Some)
        } else
            Ok(None)




    // # Predicates

    ///
    /// Returns `true` if the entire string `input` is matched by the pattern `patt`.
    ///
    pub def isMatch(patt: Pattern, input: String): Bool =
        testMatch(Text/Regex/Internal/Prim.matches, notModified, patt, input)

    ///
    /// Returns `true` if the string `input` is matched by the regular expression pattern `patt`
    /// at any position within the string.
    ///
    pub def isSubmatch(patt: Pattern, input: String): Bool =
        testMatch(Text/Regex/Internal/Prim.find!, notModified, patt, input)


    ///
    /// Returns `true` if the string `input` starts the regular expression pattern `patt`.
    ///
    pub def startsWith(patt: Pattern, input: String): Bool =
        testMatch(Text/Regex/Internal/Prim.lookingAt, notModified, patt, input)

    ///
    /// Returns `true` if the string `input` ends the regular expression pattern `patt`.
    ///
    /// Note this rewrites the pattern to end with the "$" boundary matcher and changes flags
    /// to use `Multiline`.
    ///
    pub def endsWith(patt: Pattern, input: String): Bool =
        match Text/Regex/Pattern.modifyPattern(s -> s + "$" , patt) {
            case Err(_) => false
            case Ok(patt2) => isSubmatch(patt2, input)
        }


    // # Simple extractors

    ///
    /// Returns `true` if the entire string `input` is matched by the pattern `patt`.
    ///
    pub def matches(patt: Pattern, input: String): Option[String] =
        extract(Text/Regex/Internal/Prim.matches, notModified, patt, input)


    ///
    /// Returns the first substring of `input` matched by the pattern `patt`.
    ///
    pub def contains(patt: Pattern, input: String): Option[String] =
        extract(Text/Regex/Internal/Prim.find!, notModified, patt, input)

    ///
    /// Returns the start of `input` matched by the pattern `patt`.
    ///
    pub def prefix(patt: Pattern, input: String): Option[String] =
        extract(Text/Regex/Internal/Prim.lookingAt, notModified, patt, input)





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





    // Using MatchEvaluator...


    /// Whole input matches...
    /// The MatchEvaluator `eval` is potentially impure (e.g. it might create an array),
    /// so this function is impure.
    ///
    pub def evalMatches(patt: Pattern, eval: MatchEvaluator[a], input: String): Result[a, String] & Impure =
        evaluate(Text/Regex/Internal/Prim.matches, notModified, patt, eval, input)

    /// Whole input matches...
    /// The MatchEvaluator `eval` is potentially impure (e.g. it might create an array),
    /// so this function is impure.
    /// Returns `Ok(None)` on no match
    pub def evalMatchesOptional(patt: Pattern, eval: MatchEvaluator[a], input: String): Result[Option[a], String] & Impure =
        evaluateOptional(Text/Regex/Internal/Prim.matches, notModified, patt, eval, input)


    /// Input matches at least once (left to right)...
    ///
    /// The MatchEvaluator `eval` is potentially impure (e.g. it might create an array),
    /// so this function is impure.
    ///
    pub def evalContains(patt: Pattern, eval: MatchEvaluator[a], input: String): Result[a, String] & Impure =
        evaluate(Text/Regex/Internal/Prim.find!, notModified, patt, eval, input)

    /// Returns `Ok(None)` on no match
    pub def evalContainsOptional(patt: Pattern, eval: MatchEvaluator[a], input: String): Result[Option[a], String] & Impure =
        evaluateOptional(Text/Regex/Internal/Prim.find!, notModified, patt, eval, input)

    /// Input matches at least once (left to right)...
    pub def evalStartsWith(patt: Pattern, eval: MatchEvaluator[a], input: String): Result[a, String] & Impure =
        evaluate(Text/Regex/Internal/Prim.lookingAt, notModified, patt, eval, input)

    /// Returns `Ok(None)` on no match
    pub def evalStartsWithOptional(patt: Pattern, eval: MatchEvaluator[a], input: String): Result[Option[a], String] & Impure =
        evaluateOptional(Text/Regex/Internal/Prim.lookingAt, notModified, patt, eval, input)



    /// Match input within region, match must start at start of region...
    pub def evalBoundedMatches(bounds: {start :: Int32, end :: Int32},
                                patt: Pattern,
                                eval: MatchEvaluator[a],
                                input: String): Result[a, String] & Impure =
        evaluate(Text/Regex/Internal/Prim.matches, setRegion!(bounds), patt, eval, input)

    pub def evalBoundedMatchesOptional(bounds: {start :: Int32, end :: Int32},
                                        patt: Pattern,
                                        eval: MatchEvaluator[a],
                                        input: String): Result[Option[a], String] & Impure =
        evaluateOptional(Text/Regex/Internal/Prim.matches, setRegion!(bounds), patt, eval, input)

    /// Match input within region, match must start at start of region...
    pub def evalBoundedContains(bounds: {start :: Int32, end :: Int32},
                                patt: Pattern,
                                eval: MatchEvaluator[a],
                                input: String): Result[a, String] & Impure =
        evaluate(Text/Regex/Internal/Prim.find!, setRegion!(bounds), patt, eval, input)

    pub def evalBoundedContainsOptional(bounds: {start :: Int32, end :: Int32},
                                        patt: Pattern,
                                        eval: MatchEvaluator[a],
                                        input: String): Result[Option[a], String] & Impure =
        evaluateOptional(Text/Regex/Internal/Prim.find!, setRegion!(bounds), patt, eval, input)

    /// Match input within region, match must start at start of region...
    pub def evalBoundedStartsWith(bounds: {start :: Int32, end :: Int32},
                                    patt: Pattern,
                                    eval: MatchEvaluator[a],
                                    input: String): Result[a, String] & Impure =
        evaluate(Text/Regex/Internal/Prim.lookingAt, setRegion!(bounds), patt, eval, input)

    pub def evalBoundedStartsWithOptional(bounds: {start :: Int32, end :: Int32},
                                            patt: Pattern,
                                            eval: MatchEvaluator[a],
                                            input: String): Result[Option[a], String] & Impure =
        evaluateOptional(Text/Regex/Internal/Prim.lookingAt, setRegion!(bounds), patt, eval, input)


    // # Multiple results

    ///
    /// TODO: use a "Stream" when stdlib has an implementation finalized
    ///
    pub def results(patt: Pattern, eval: MatchEvaluator[a], input: String): Result[List[a], String] & Impure =
        let Pattern(patt1) = patt;
        let MatchEvaluator(eval1) = eval;
        let m1 = Text/Regex/Internal/Prim.newMatcher(patt1, input);
        let iter = _ -> match Text/Regex/Internal/Prim.find!(m1) {
            case true  => eval1(m1) |> Result.map(Some)
            case false => Ok(None)
        };
        List.unfoldWithOkIter(iter)

    ///
    /// Stream...
    ///
    pub def contentResults(patt: Pattern, input: String): Result[List[String], String] & Impure =
        use Text/Regex/MatchEvaluator.{content};
        results(patt, content(), input)

    ///
    /// Stream...
    ///
    pub def rangeResults(patt: Pattern, input: String): Result[List[{start :: Int32, end :: Int32}], String] & Impure =
        use Text/Regex/MatchEvaluator.{getRange};
        results(patt, getRange(), input)

    ///
    /// Count the occurences of `patt` in string `s`.
    ///
    pub def countMatches(patt: Pattern, s: String): Int32 =
        Text/Regex/Matcher.runMatcher(countMatcher(0), patt, s) as & Pure
            |> Result.getWithDefault(0)

    def countMatcher(acc: Int32): Matcher[Int32] & Impure =
        use Text/Regex/Matcher.{flatMap, find!, point};
        let* first = find!();
        if (first) countMatcher(acc+1) else point(acc)


    // # String API functions using regexs

    ///
    /// Splits the string `s` around matches of the pattern `p`.
    ///
    pub def split(patt: Pattern, input: String): Array[String] & Impure =
        import java.util.regex.Pattern.split(##java.lang.CharSequence);
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

    ///
    /// Rewrite the string `input` by applying the function `f` to the
    /// substring produced by each match of the pattern `patt`.
    ///
    pub def rewriteWith(from: {from :: Pattern}, to: {to :: MatchEvaluator[String]}, input: String): Result[String, String] & Impure =
        use Text/Regex/Replacer.{execReplacer, evalRewriteAll!};
        execReplacer(evalRewriteAll!(to.to), from.from, input)

    ///
    /// Rewrite the string `input` by apply the function `f` to the substring produced by each match of the pattern `patt`.
    ///
    pub def rewriteWithIndex(patt: Pattern, eval: Int32 -> MatchEvaluator[String], input: String): Result[String, String] & Impure =
        use Text/Regex/Replacer.{execReplacer, evalRewriteAllWithIndex!};
        execReplacer(evalRewriteAllWithIndex!(eval), patt, input)


    ///
    /// Return the index of the first occurence of `patt` in `s` from the left.
    ///
    /// If `patt` is not present in `s` return None.
    ///
    /// If `patt` is the empty string return None.
    ///
    pub def indexOfLeft(patt: Pattern, s: String): Option[Int32] = indexOfLeftHelper(patt, s) as & Pure

    def indexOfLeftHelper(patt: Pattern, s: String): Option[Int32] & Impure =
        use Text/Regex/Matcher.{flatMap, find!, start, throwError};
        let m1 = {
            let* bfound = find!();
            if (bfound) start() else throwError("none")
        };
        Text/Regex/Matcher.runMatcher(m1, patt, s) |> Result.toOption

    ///
    /// Find index of rightmost match.
    ///
    pub def indexOfRight(patt: Pattern, s: String): Option[Int32] = indexOfRightHelper(patt, s) as & Pure

    def indexOfRightHelper(patt: Pattern, s: String): Option[Int32] & Impure =
        use Text/Regex/Matcher.{flatMap, findLast!, start, throwError};
        let m1 = {
            let* bfound = findLast!();
            if (bfound) start() else throwError("none")
        };
        Text/Regex/Matcher.runMatcher(m1, patt, s) |> Result.toOption


    pub def indexOfEndLeft(patt: Pattern, s: String): Option[Int32] = indexOfEndLeftHelper(patt, s) as & Pure

    def indexOfEndLeftHelper(patt: Pattern, s: String): Option[Int32] & Impure =
        use Text/Regex/Matcher.{flatMap, find!, end, throwError};
        let m1 = {
            let* bfound = find!();
            if (bfound) end() else throwError("none")
        };
        Text/Regex/Matcher.runMatcher(m1, patt, s) |> Result.toOption

    pub def rangeOfRight(patt: Pattern, s: String): Option[{start :: Int32, end :: Int32}] = rangeOfRightHelper(patt, s) as & Pure

    def rangeOfRightHelper(patt: Pattern, s: String): Option[{start :: Int32, end :: Int32}] & Impure =
        use Text/Regex/Matcher.{flatMap, findLast!, range, throwError};
        let m1 = {
            let* bfound = findLast!();
            if (bfound) range() else throwError("none")
        };
        Text/Regex/Matcher.runMatcher(m1, patt, s) |> Result.toOption


    ///
    /// This is `indexOfLeft` with a start offset.
    ///
    /// Returns `None` if `patt` does not match.
    ///
    pub def indexOfLeftWithOffset(patt: Pattern, offset: Int32, s: String): Option[Int32] =
        indexOfLeftWithOffsetHelper(patt, offset, s) as & Pure

    def indexOfLeftWithOffsetHelper(patt: Pattern, offset: Int32, s: String): Option[Int32] & Impure =
        use Text/Regex/Matcher.{flatMap, findFrom!, start, throwError};
        let m1 = {
            let* bfound = findFrom!(offset);
            if (bfound) start() else throwError("none")
        };
        Text/Regex/Matcher.runMatcher(m1, patt, s) |> Result.toOption

    ///
    /// This is `indexOfRight` with a start offset.
    ///
    /// Returns `None` if `patt` does not match.
    ///
    pub def indexOfRightWithOffset(patt: Pattern, offset: Int32, s: String): Option[Int32] =
        indexOfRightWithOffsetHelper(patt, offset, s) as & Pure

    def indexOfRightWithOffsetHelper(patt: Pattern, offset: Int32, s: String): Option[Int32] & Impure =
        use Text/Regex/Matcher.{flatMap, findLastFrom!, start, throwError};
        let m1 = {
            let* bfound = findLastFrom!(offset);
            if (bfound) start() else throwError("none")
        };
        Text/Regex/Matcher.runMatcher(m1, patt, s) |> Result.toOption

    ///
    /// Find the first instance of `patt` in string `s`, return a pair of the
    /// prefix of string `s` up to `sub` and the rest of string `s` including `sub`.
    ///
    pub def breakOnLeft(patt: Pattern, s: String): (String, String) =
        match indexOfLeft(patt, s) {
            case None => (s, "")
            case Some(i) => (String.sliceLeft(i, s), String.sliceRight(i, s))
        }

    pub def breakAfterLeft(patt: Pattern, s: String): (String, String) =
        match indexOfEndLeft(patt, s) {
            case None => (s, "")
            case Some(i) => (String.sliceLeft(i, s), String.sliceRight(i, s))
        }


    ///
    /// Find the last instance of `substr` in string `s`, return a pair of the
    /// initial string including `substr` and suffix from `substr`.
    ///
    pub def breakOnRight(patt: Pattern, s: String): (String, String) =
        match rangeOfRight(patt, s) {
            case None => (s, "")
            case Some(rng) => (String.sliceLeft(rng.end, s), String.sliceRight(rng.end, s))
        }

    ///
    /// Find the last instance of `substr` in string `s`, return a pair of the
    /// initial string including `substr` and suffix from `substr`.
    ///
    pub def breakBeforeRight(patt: Pattern, s: String): (String, String) =
        match rangeOfRight(patt, s) {
            case None => (s, "")
            case Some(rng) => (String.sliceLeft(rng.start, s), String.sliceRight(rng.start, s))
        }

}
