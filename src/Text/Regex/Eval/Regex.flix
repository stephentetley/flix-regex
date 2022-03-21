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

namespace Text/RegexEval/Regex {

    use Text/Regex/Internal.PrimMatcher;
    use Text/Regex.Pattern;
    use Text/Regex.Pattern.{Pattern};
    use Text/Regex.Matcher;
    use Text/Regex.Replacer;
    use Text/Regex.Replacer.{Replacer};

    use Text/Regex/Eval.MatchEvaluator;
    use Text/Regex/Eval.MatchEvaluator.{MatchEvaluator};

    // # Helpers


    def notModified(_: ##java.util.regex.Matcher): Result[Unit, String] & Impure = Ok(()) as & Impure

    def setRegion!(bounds: {start:: Int32, end :: Int32}, x: ##java.util.regex.Matcher): Result[Unit, String] & Impure =
        Text/Regex/Internal/Prim.setRegion!(x, bounds)



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



    // # Simple extractors

    /// Whole input matches...
    /// The MatchEvaluator `eval` is potentially impure (e.g. it might create an array),
    /// so this function is impure.
    ///
    pub def matches(patt: Pattern, eval: MatchEvaluator[a], input: String): Result[a, String] & Impure =
        evaluate(Text/Regex/Internal/Prim.matches, notModified, patt, eval, input)


    /// Input matches at least once (left to right)...
    ///
    /// The MatchEvaluator `eval` is potentially impure (e.g. it might create an array),
    /// so this function is impure.
    ///
    pub def contains(patt: Pattern, eval: MatchEvaluator[a], input: String): Result[a, String] & Impure =
        evaluate(Text/Regex/Internal/Prim.find!, notModified, patt, eval, input)

    ///
    /// Returns the start of `input` matched by the pattern `patt`.
    ///
    pub def prefix(patt: Pattern, eval: MatchEvaluator[a], input: String): Result[a, String] & Impure =
        evaluate(Text/Regex/Internal/Prim.lookingAt, notModified, patt, eval, input)


    /// Whole input matches...
    /// The MatchEvaluator `eval` is potentially impure (e.g. it might create an array),
    /// so this function is impure.
    /// Returns `Ok(None)` on no match
    pub def matchesOptional(patt: Pattern, eval: MatchEvaluator[a], input: String): Result[Option[a], String] & Impure =
        evaluateOptional(Text/Regex/Internal/Prim.matches, notModified, patt, eval, input)


    /// Returns `Ok(None)` on no match
    pub def containsOptional(patt: Pattern, eval: MatchEvaluator[a], input: String): Result[Option[a], String] & Impure =
        evaluateOptional(Text/Regex/Internal/Prim.find!, notModified, patt, eval, input)

    /// Returns `Ok(None)` on no match
    pub def prefixOptional(patt: Pattern, eval: MatchEvaluator[a], input: String): Result[Option[a], String] & Impure =
        evaluateOptional(Text/Regex/Internal/Prim.lookingAt, notModified, patt, eval, input)




    /// Match input within region, match must start at start of region...
    pub def noundedMatches(bounds: {start :: Int32, end :: Int32},
                                patt: Pattern,
                                eval: MatchEvaluator[a],
                                input: String): Result[a, String] & Impure =
        evaluate(Text/Regex/Internal/Prim.matches, setRegion!(bounds), patt, eval, input)

    /// Match input within region, match must start at start of region...
    pub def boundedContains(bounds: {start :: Int32, end :: Int32},
                                patt: Pattern,
                                eval: MatchEvaluator[a],
                                input: String): Result[a, String] & Impure =
        evaluate(Text/Regex/Internal/Prim.find!, setRegion!(bounds), patt, eval, input)

    /// Match input within region, match must start at start of region...
    pub def boundedPrefix(bounds: {start :: Int32, end :: Int32},
                                    patt: Pattern,
                                    eval: MatchEvaluator[a],
                                    input: String): Result[a, String] & Impure =
        evaluate(Text/Regex/Internal/Prim.lookingAt, setRegion!(bounds), patt, eval, input)


    /// Returns `Ok(None)` on no match
    pub def boundedMatchesOptional(bounds: {start :: Int32, end :: Int32},
                                        patt: Pattern,
                                        eval: MatchEvaluator[a],
                                        input: String): Result[Option[a], String] & Impure =
        evaluateOptional(Text/Regex/Internal/Prim.matches, setRegion!(bounds), patt, eval, input)

    /// Returns `Ok(None)` on no match
    pub def boundedContainsOptional(bounds: {start :: Int32, end :: Int32},
                                        patt: Pattern,
                                        eval: MatchEvaluator[a],
                                        input: String): Result[Option[a], String] & Impure =
        evaluateOptional(Text/Regex/Internal/Prim.find!, setRegion!(bounds), patt, eval, input)

    /// Returns `Ok(None)` on no match
    pub def boundedPrefixOptional(bounds: {start :: Int32, end :: Int32},
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
    pub def contents(patt: Pattern, input: String): Result[List[String], String] & Impure =
        use Text/Regex/Eval/MatchEvaluator.{content};
        results(patt, content(), input)

    ///
    /// Stream...
    ///
    pub def ranges(patt: Pattern, input: String): Result[List[{start :: Int32, end :: Int32}], String] & Impure =
        use Text/Regex/Eval/MatchEvaluator.{getRange};
        results(patt, getRange(), input)



    ///
    /// Rewrite the string `input` by applying the function `f` to the
    /// substring produced by each match of the pattern `patt`.
    ///
    pub def replace(from: {from :: Pattern}, to: {to :: MatchEvaluator[String]}, input: String): Result[String, String] & Impure =
        use Text/Regex/Replacer.{execReplacer};
        use Text/Regex/Eval/Replacer.{rewriteAll!};
        execReplacer(rewriteAll!(to.to), from.from, input)

    /// TODO...
    /// ///
    /// /// Returns `input` with the first match of the regular expression `patt` replaced by the string `rep`.
    /// ///
    /// pub def replaceFirstMatch(from: {from :: Pattern}, to: {to :: String}, input: String): String =
    ///     let Pattern(patt1) = from.from;
    ///     let m1 = Text/Regex/Internal/Prim.newMatcher(patt1, input) as & Pure;
    ///     Text/Regex/Internal/Prim.replaceFirst(m1, to.to) as & Pure

    ///
    /// Rewrite the string `input` by apply the function `f` to the substring produced by each match of the pattern `patt`.
    ///
    pub def rewriteWithIndex(patt: Pattern, eval: Int32 -> MatchEvaluator[String], input: String): Result[String, String] & Impure =
        use Text/Regex/Replacer.{execReplacer};
        use Text/Regex/Eval/Replacer.{rewriteAllWithIndex!};
        execReplacer(rewriteAllWithIndex!(eval), patt, input)


}
