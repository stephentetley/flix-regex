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



/// This is a "steppable" replacer like Matcher.
/// Index is number of `finds` not the number of replacements.

namespace Text/Regex {

/*
    pub enum Replacer[r: Region, a: Type]((StringBuilder[r], ##java.util.regex.Matcher, Int32) -> Result[(a, Int32), String] \ {Impure, Write(r)})

    instance Functor[Replacer[r]] {
        pub def map(f: a -> b & ef, o: Replacer[r, a]): Replacer[r, b] & ef = Text/Regex/Replacer.map(f, o)
    }

    instance Applicative[Replacer[r]] {
        pub def point(x: a) : Replacer[r, a] = Text/Regex/Replacer.point(x)

        pub def ap(f: Replacer[r, a -> b & ef], x: Replacer[r, a]) : Replacer[r, b] & ef = Text/Regex/Replacer.ap(f, x)
    }

    instance Monad[Replacer[r]] {
        pub def flatMap(f: a -> Replacer[r, b] & ef, x: Replacer[r, a]) : Replacer[r, b] & ef = Text/Regex/Replacer.flatMap(f, x)
    }
*/

}

namespace Text/Regex/Replacer {

    use Text/Regex.{Replacer};
    use Text/Regex.Replacer.{Replacer};

    use Text/Regex.Pattern;
    use Text/Regex.Pattern.{Pattern};
    
/*
    pub def runReplacer(rep: Replacer[r, a], patt: Pattern, input: String): Result[(a, String), String] & Impure =
        use Result.flatMap;
        region r {
            let sb = new StringBuilder(r);
            let Replacer(rep1) = rep;
            let Pattern(patt1) = patt;
            let m1 = Text/Regex/Internal/Prim.newMatcher(patt1, input);
            /// Start index at -1 because we haven't found anything yet...
            match rep1(sb, m1, -1) {
                case Ok(a, _) => {
                    let s = StringBuilder.toString(sb);
                    Ok((a, s))
                }
                case Err(e) => Err(e)
            }
        }

    pub def execReplacer(rep: Replacer[r, a], patt: Pattern, input: String): Result[String, String] & Impure =
        runReplacer(rep, patt, input) |> Result.map(snd)

    ///
    /// Version of `runReplacer` that takes a regex string and compiles it to a Pattern.
    ///
    pub def runsReplacer(rep: Replacer[r, a], regex: String, input: String): Result[(a, String), String] & Impure =
        use Result.flatMap;
        let* patt = Text/Regex/Pattern.compile(regex) |> Result.mapErr(constant("runsReplacer - invalid regex: ${regex}"));
        runReplacer(rep, patt, input)

    pub def execsReplacer(rep: Replacer[r, a], regex: String, input: String): Result[String, String] & Impure =
        runsReplacer(rep, regex, input) |> Result.map(snd)

    pub def point(x: a): Replacer[r, a] =
        Replacer((_, _, n) -> Ok(x, n))


    pub def throwError(msg: String): Replacer[r, a] =
        Replacer((_, _, _) -> Err(msg))

    def apply1(rep: Replacer[r, a], sb: StringBuilder[r], m: ##java.util.regex.Matcher, n: Int32): Result[(a, Int32), String] & Impure =
        let Replacer(f) = rep;
        f(sb, m, n)

    pub def map(f: a -> b & ef, rep: Replacer[r, a]): Replacer[r, b] & ef =
        Replacer((sb, m, n) ->
            match apply1(rep, sb, m, n) {
                case Ok((a, n1)) => {
                    let a1 = f(a) as & Impure;
                    Ok(a1, n1)
                }
                case Err(e) => Err(e)
            }
        ) as & ef

    pub def ap(repf: Replacer[r, a -> b & ef], rep: Replacer[r, a]): Replacer[r, b] & ef =
        Replacer((sb, m, n) ->
            match apply1(repf, sb, m, n) {
                case Ok((f, n1)) => match apply1(rep, sb, m, n1) {
                    case Ok((a, n2)) => Ok((f(a) as & Impure, n2))
                    case Err(e2) => Err(e2)
                }
                case Err(e1) => Err(e1)
            }
        ) as & ef

    pub def bind(rep: Replacer[r, a], k: a -> Replacer[r, b] & ef): Replacer[r, b] & ef =
        Replacer((sb, m, n) ->
            match apply1(rep, sb, m, n) {
                case Ok((a, n1)) => apply1(k(a) as & Impure, sb , m, n1)
                case Err(e) => Err(e)
            }
        ) as & ef

    pub def flatMap(k: a -> Replacer[r, b] & ef, x: Replacer[r, a]): Replacer[r, b] & ef = bind(x, k)

    pub def ignore(x: Replacer[r, a]): Replacer[r, Unit] = map(_ -> (), x)


    def liftPrim(fn: ##java.util.regex.Matcher -> a & Impure): Replacer[r, a] =
        Replacer((_, m, n) -> Ok((fn(m), n)))

    /// TODO - we need to export this, but it should have a better name...
    pub def liftPrimResult(fn: ##java.util.regex.Matcher -> Result[a, String] & Impure): Replacer[r, a] =
        Replacer((_, m, n) -> Result.map(x -> (x, n), fn(m)))

    def liftWriterPrim(fn : (##java.util.regex.Matcher, StringBuilder[r]) -> a \ {Impure, Write(r)}): Replacer[r, a] =
        Replacer((sb, m, n) -> Ok((fn(m, sb), n)))

    def liftWriterPrimResult(fn : (##java.util.regex.Matcher, StringBuilder[r]) -> Result[a, String] \ {Impure, Write(r)}): Replacer[r, a] =
        Replacer((sb, m, n) -> Result.map(x -> (x, n), fn(m, sb)))



    pub def appendTail!(): Replacer[r, Unit] =
        liftWriterPrim((m, sb) -> Text/Regex/Internal/Prim.appendTail!(m, sb))

    pub def appendReplacement!(rep: String): Replacer[r, Unit] =
        liftWriterPrimResult((m, sb) -> Text/Regex/Internal/Prim.appendReplacement!(m, sb, rep))

    pub def getIndex(): Replacer[r, Int32] =
        Replacer((_, _, n) -> Ok((n, n)) as & Impure)

    def incrementFinds!(incr: Bool): Replacer[r, Unit] =
        Replacer((_, _, n) -> Ok(((), if (incr) n + 1 else n)) as & Impure)

    pub def find(): Replacer[r, Bool] =
        let* bfound = liftPrim(x -> Text/Regex/Internal/Prim.find!(x));
        let* _ = incrementFinds!(bfound);
        point(bfound)

    pub def findFrom(start: Int32): Replacer[r, Bool] =
        let* bfound = liftPrimResult(x -> Text/Regex/Internal/Prim.findFrom!(x, start));
        let* _ = incrementFinds!(bfound);
        point(bfound)

    pub def content(): Replacer[r, String] = liftPrimResult(Text/Regex/Internal/Prim.group)


    pub def replaceStep!(fn: String -> String): Replacer[r, Unit] =
        let* ans = find();
        if (ans) {
            let* s = content();
            appendReplacement!(fn(s))
        } else
            throwError("replaceStep!")


    /// `cond` must not perform a rewrite step.
    def ifThenElse(cond: Replacer[r, s], thenm: s -> Replacer[r, t], elsem: Replacer[r, t]): Replacer[r, t] =
        Replacer((sb, m, n) ->
            match apply1(cond, sb, m, n) {
                case Ok((a, n1)) => apply1(thenm(a), sb, m, n1)
                case Err(_) => apply1(elsem, sb, m, n)
            }
        )

    pub def done(): Replacer[r, Bool] = ifThenElse(content(), _ -> point(false), point(true))

    pub def setRegion!(bounds: {start :: Int32, end :: Int32}): Replacer[r, Unit] =
        liftPrimResult(x -> Text/Regex/Internal/Prim.setRegion!(x, bounds))

    pub def setPattern!(patt: Pattern): Replacer[r, Unit] =
        let Pattern(patt1) = patt;
        liftPrimResult(x -> Text/Regex/Internal/Prim.usePattern!(x, patt1))

    pub def countWith(append: (a, ac) -> ac, acc: ac, n: Int32, ma: Replacer[r, a]): Replacer[r, ac] =
        if (n <= 0)
            point(acc)
        else {
            let* a = ma;
            countWith(append, append(a, acc), n-1, ma)
        }

    pub def count(n: Int32, rep: Replacer[r, a]): Replacer[r, List[a]] =
        use Chain.{empty, snoc, toList};
        map(toList, countWith((x, xs) -> snoc(xs, x), empty(), n, rep))



    /// Strategies

    pub def rewriteAll!(fn: String -> String): Replacer[r, Unit] & Impure =
        let* more = find();
        if (more) {
            let* s = content();
            let* _ = appendReplacement!(fn(s));
            rewriteAll!(fn)
        } else
            appendTail!()


    pub def rewriteSome!(fn: String -> Option[String]): Replacer[r, Unit] & Impure =
        let* more = find();
        if (more) {
            let* s = content();
            let* _ = match fn(s) {
                        case Some(s1) => appendReplacement!(s1)
                        case None => point()
                    };
            rewriteSome!(fn)
        } else
            appendTail!()


    pub def rewriteAllWithIndex!(fn: (Int32, String) -> String): Replacer[r, Unit] & Impure =
        let* more = find();
        if (more) {
            let* ix = getIndex();
            let* s = content();
            let* _ = appendReplacement!(fn(ix, s));
            rewriteAllWithIndex!(fn)
        } else
            appendTail!()



    pub def rewriteSomeWithIndex!(fn: (Int32, String) -> Option[String]): Replacer[r, Unit] & Impure =
        let* more = find();
        if (more) {
            let* ix = getIndex();
            let* s = content();
            let* _ = match fn(ix, s) {
                        case Some(s1) => appendReplacement!(s1)
                        case None => point()
                    };
            rewriteSomeWithIndex!(fn)
        } else
            appendTail!()

    pub def rewriteCount!(ntimes: Int32, fn: String -> String): Replacer[r, Unit] & Impure =
        if (ntimes < 0)
            point()
        else
            rewriteCountHelper!(ntimes, fn, 0)

    def rewriteCountHelper!(ntimes: Int32, fn: String -> String, i: Int32): Replacer[r, Unit] & Impure =
        let* more = find();
        if (more or i < ntimes) {
            let* s = content();
            let* _ = appendReplacement!(fn(s));
            rewriteCountHelper!(ntimes, fn, i + 1)
        } else
            appendTail!()

*/
}



