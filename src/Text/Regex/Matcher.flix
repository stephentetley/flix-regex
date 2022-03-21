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


/// This is a "steppable" matcher like .Net `Regex.Match` but wrapped as a monad.
/// It does not support backtracking so cannot be made an instance of `Alternative`.

namespace Text/Regex {

    pub opaque type Matcher[a] = ##java.util.regex.Matcher -> Result[a, String] & Impure

    instance Functor[Matcher] {
        pub def map(f: a -> b & ef, o: Matcher[a]): Matcher[b] & ef = Text/Regex/Matcher.map(f, o)
    }

    instance Applicative[Matcher] {
        pub def point(x: a) : Matcher[a] = Text/Regex/Matcher.point(x)

        pub def ap(f: Matcher[a -> b & ef], x: Matcher[a]) : Matcher[b] & ef = Text/Regex/Matcher.ap(f, x)
    }

    instance Monad[Matcher] {
        pub def flatMap(k: a -> Matcher[b] & ef, ma: Matcher[a]) : Matcher[b] & ef = Text/Regex/Matcher.flatMap(k, ma)
    }

    /// Don't make Matcher Alternative because Matcher has imperative state (a handle) rather
    /// than a pure, resettable context (an environment).

}

namespace Text/Regex/Matcher {


    use Text/Regex.{Matcher};
    use Text/Regex.Matcher.{Matcher};

    use Text/Regex.Pattern;
    use Text/Regex.Pattern.{Pattern};

    pub def runMatcher(ma: Matcher[a], patt: Pattern, input: String): Result[a, String] & Impure =
        use Result.flatMap;
        let Matcher(f) = ma;
        let Pattern(patt1) = patt;
        let m1 = Text/Regex/Internal/Prim.newMatcher(patt1, input);
        f(m1)

    ///
    /// Version of `runMatcher` that takes a regex string and compiles it to a Pattern.
    ///
    pub def runsMatcher(ma: Matcher[a], regex: String, input: String): Result[a, String] & Impure =
        use Result.flatMap;
        let* patt = Text/Regex/Pattern.compile(regex) |> Result.mapErr(constant("runsMatcher - invalid regex: ${regex}"));
        runMatcher(ma, patt, input)

    ///
    /// Returns `x` wrapped as a Matcher.
    ///
    pub def point(x: a): Matcher[a] =
        Matcher(_ -> Ok(x) as & Impure)

    pub def throwError(msg: String): Matcher[a] =
        Matcher(_ -> Err(msg) as & Impure)


    def apply1(x: Matcher[a], r: ##java.util.regex.Matcher): Result[a, String] & Impure =
        let Matcher(f) = x;
        f(r)

    pub def map(f: a -> b & ef, x: Matcher[a]): Matcher[b] & ef =
        Matcher(r ->
            match apply1(x, r) {
                case Ok(a) => {
                    let ans = f(a) as & Impure;
                    Ok(ans)
                }
                case Err(e) => Err(e)
            }
        ) as & ef

    pub def ap(mf: Matcher[a -> b & ef], ma: Matcher[a]): Matcher[b] & ef =
        Matcher(r ->
            match apply1(mf, r) {
                case Ok(f) => match apply1(ma, r) {
                    case Ok(a) => Ok(f(a) as & Impure)
                    case Err(e2) => Err(e2)
                }
                case Err(e1) => Err(e1)
            }
        ) as & ef

    pub def bind(x: Matcher[a], k: a -> Matcher[b] & ef): Matcher[b] & ef =
        Matcher(r ->
            match apply1(x, r) {
                case Ok(a) => apply1(k(a) as & Impure, r)
                case Err(e) => Err(e)
            }
        ) as & ef

    pub def flatMap(k: a -> Matcher[b] & ef, x: Matcher[a]): Matcher[b] & ef = bind(x, k)

    def liftPrim(fn : ##java.util.regex.Matcher -> a & Impure): Matcher[a] =
        Matcher(r -> Ok(fn(r)))

    def liftPrimResult(fn : ##java.util.regex.Matcher -> Result[a, String] & Impure): Matcher[a] =
        Matcher(r -> fn(r))

    pub def matches(): Matcher[Bool] = liftPrim(Text/Regex/Internal/Prim.matches)

    pub def lookingAt(): Matcher[Bool] = liftPrim(Text/Regex/Internal/Prim.lookingAt)

    pub def find!(): Matcher[Bool] = liftPrim(Text/Regex/Internal/Prim.find!)

    pub def start(): Matcher[Int32] = liftPrimResult(Text/Regex/Internal/Prim.start)

    pub def end(): Matcher[Int32] = liftPrimResult(Text/Regex/Internal/Prim.end)

    pub def range(): Matcher[{start :: Int32, end :: Int32}] =
        Applicative.liftA2((s, e) -> {start = s, end = e}, start(), end())

    pub def findLast!(): Matcher[Bool] =
        let* first = find!();
        if (first) {
            let* pos = start();
            let* endpos = findLastHelper(pos);
            findFrom!(endpos - 1)
        } else point(false)

    def findLastHelper(lastPos: Int32): Matcher[Int32] =
        let* more = find!();
        if (more) {
            let* pos = start();
            findLastHelper(pos)
        } else point(lastPos)


    pub def findFrom!(b: Int32): Matcher[Bool] =
        liftPrimResult(m1 -> Text/Regex/Internal/Prim.findFrom!(m1, b))


    pub def findLastFrom!(b: Int32): Matcher[Bool] =
        let* first = findFrom!(b);
        if (first) {
            let* pos = start();
            let* endpos = findLastHelper(pos);
            findFrom!(endpos - 1)
        } else point(false)

    pub def content(): Matcher[String] =
        liftPrimResult(Text/Regex/Internal/Prim.group)



    pub def getMatch(): Matcher[String] =
        let* ans = find!();
        if (ans)
            content()
        else
            throwError("getMatch")

    def ifThenElse(cond: Matcher[s], thenm: s -> Matcher[t], elsem: Matcher[t]): Matcher[t] =
        Matcher(r ->
            match apply1(cond, r) {
                case Ok(a) => apply1(thenm(a), r)
                case Err(_) => apply1(elsem, r)
            }
        )

    pub def done(): Matcher[Bool] = ifThenElse(content(), _ -> point(false), point(true))

    pub def reset!(): Matcher[Unit] = liftPrim(Text/Regex/Internal/Prim.reset!)

    pub def resetInput!(input: String): Matcher[Unit] =
        liftPrim(x -> Text/Regex/Internal/Prim.resetInput!(x, input))

    pub def setRegion!(bounds: {start :: Int32, end :: Int32}): Matcher[Unit] =
        liftPrimResult(x -> Text/Regex/Internal/Prim.setRegion!(x, bounds))

    pub def setPattern!(patt: Pattern): Matcher[Unit] =
        let Pattern(p1) = patt;
        liftPrimResult(x -> Text/Regex/Internal/Prim.usePattern!(x, p1))



    pub def countWith(append: (a, ac) -> ac, acc: ac, n: Int32, ma: Matcher[a]): Matcher[ac] =
        if (n <= 0)
            point(acc)
        else {
            let* a = ma;
            countWith(append, append(a, acc), n-1, ma)
        }

    pub def count(n: Int32, ma: Matcher[a]): Matcher[List[a]] =
        use Functor.{<$>};
        use Chain.{empty, snoc, toList};
        toList <$> countWith(flip(snoc), empty(), n, ma)

}

