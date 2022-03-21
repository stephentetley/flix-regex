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

/// Augments the Regex.Replacer API with versions using MatchEvaluator

namespace Text/Regex/Eval/Replacer {

    use Text/Regex/Internal.PrimMatcher;
    use Text/Regex.Pattern;
    use Text/Regex.Pattern.{Pattern};
    use Text/Regex.Matcher;
    use Text/Regex.Replacer;
    use Text/Regex.Replacer.{Replacer};

    use Text/Regex/Eval.MatchEvaluator;
    use Text/Regex/Eval.MatchEvaluator.{MatchEvaluator};


    /// Replacer

    def evaluate(eval: MatchEvaluator[a]): Replacer[a] =
        let MatchEvaluator(fn) = eval;
        Text/Regex/Replacer.liftPrimResult(x -> fn(x) as & Impure)


    pub def replace!(eval: MatchEvaluator[String]): Replacer[Unit] =
        use Text/Regex/Replacer.{flatMap};
        let* ans = Text/Regex/Replacer.find();
        if (ans) {
            let* s = evaluate(eval);
            Text/Regex/Replacer.appendReplacement!(s)
        } else
            Text/Regex/Replacer.throwError("replaceStep!")


    pub def rewriteAll!(eval: MatchEvaluator[String]): Replacer[Unit] & Impure =
        use Text/Regex/Replacer.{flatMap};
        let* more = Text/Regex/Replacer.find();
        if (more) {
            let* s = evaluate(eval);
            let* _ = Text/Regex/Replacer.appendReplacement!(s);
            rewriteAll!(eval)
        } else
            Text/Regex/Replacer.appendTail!()


    pub def rewriteSome!(eval: MatchEvaluator[Option[String]]): Replacer[Unit] & Impure =
        use Text/Regex/Replacer.{flatMap, point};
        let* more = Text/Regex/Replacer.find();
        if (more) {
            let* opt = evaluate(eval);
            let* _ = match opt {
                        case Some(s) =>  Text/Regex/Replacer.appendReplacement!(s)
                        case None => point()
                    };
            rewriteSome!(eval)
        } else
             Text/Regex/Replacer.appendTail!()


    pub def rewriteAllWithIndex!(eval: Int32 -> MatchEvaluator[String]): Replacer[Unit] & Impure =
        use Text/Regex/Replacer.{flatMap};
        let* more = Text/Regex/Replacer.find();
        if (more) {
            let* ix = Text/Regex/Replacer.getIndex();
            let* s = evaluate(eval(ix));
            let* _ = Text/Regex/Replacer.appendReplacement!(s);
            rewriteAllWithIndex!(eval)
        } else
            Text/Regex/Replacer.appendTail!()


    pub def rewriteSomeWithIndex!(eval: Int32 -> MatchEvaluator[Option[String]]): Replacer[Unit] & Impure =
        use Text/Regex/Replacer.{flatMap, point};
        let* more = Text/Regex/Replacer.find();
        if (more) {
            let* ix  = Text/Regex/Replacer.getIndex();
            let* opt = evaluate(eval(ix));
            let* _ = match opt {
                        case Some(s) => Text/Regex/Replacer.appendReplacement!(s)
                        case None => point()
                    };
            rewriteSomeWithIndex!(eval)
        } else
            Text/Regex/Replacer.appendTail!()


    pub def rewriteCount!(ntimes: Int32, eval: MatchEvaluator[String]): Replacer[Unit] & Impure =
        if (ntimes < 0)
            Text/Regex/Replacer.point()
        else
            rewriteCountHelper!(ntimes, eval, 0)

    def rewriteCountHelper!(ntimes: Int32, eval: MatchEvaluator[String], i: Int32): Replacer[Unit] & Impure =
        use Text/Regex/Replacer.{flatMap};
        let* more = Text/Regex/Replacer.find();
        if (more or i < ntimes) {
            let* s = evaluate(eval);
            let* _ = Text/Regex/Replacer.appendReplacement!(s);
            rewriteCountHelper!(ntimes, eval, i + 1)
        } else {
            Text/Regex/Replacer.appendTail!()
        }

}
