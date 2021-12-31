/*
 * Copyright 2021 Stephen Tetley
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

    /// Resumption-Reader-Error monad

    pub enum ResMatcher[a] {
        case MatchError(String)
        case Done(a)
        case Pause(##java.util.regex.Matcher -> ResMatcher[a] & Impure)
    }

    instance Functor[ResMatcher] {
        pub def map(f: a -> b & ef, o: ResMatcher[a]): ResMatcher[b] & ef = Text/Regex/ResMatcher.map(f, o)
    }

    instance Applicative[ResMatcher] {
        pub def point(x: a): ResMatcher[a] = Text/Regex/ResMatcher.point(x)

        pub def ap(f: ResMatcher[a -> b & ef], x: ResMatcher[a]) : ResMatcher[b] & ef = Text/Regex/ResMatcher.ap(f, x)
    }

    instance Monad[ResMatcher] {
        pub def flatMap(k: a -> ResMatcher[b] & ef, ma: ResMatcher[a]) : ResMatcher[b] & ef = Text/Regex/ResMatcher.flatMap(k, ma)
    }

    // Don't make ResMatcher an Alternative because the underlying Matcher is an imperative "handle" rather
    // than pure, resettable "state".

}

namespace Text/Regex/ResMatcher {


    use Text/Regex.{ResMatcher};
    use Text/Regex.ResMatcher.{MatchError, Done, Pause};

    use Text/Regex.Pattern;
    use Text/Regex.Pattern.{Pattern};
    use Text/Regex.{MatchEvaluator};
    use Text/Regex.MatchEvaluator.{MatchEvaluator};


    pub def runResMatcher(ma: ResMatcher[a], patt: Pattern, input: String): Result[a, String] & Impure =
        use Result.flatMap;
        let Pattern(patt1) = patt;
        let m1 = Text/Regex/Internal/Prim.newMatcher(patt1, input);
        def loop(rm) = match rm {
            case MatchError(msg) => Err(msg)
            case Done(a)         => Ok(a)
            case Pause(f)        => loop(f(m1))
        };
        loop(ma)


    ///
    /// Version of `runMatcher` that takes a regex string and compiles it to a Pattern.
    ///
    pub def runsResMatcher(ma: ResMatcher[a], regex: String, input: String): Result[a, String] & Impure =
        use Result.flatMap;
        let* patt = Text/Regex/Pattern.compile(regex) |> Result.mapErr(constant("runsMatcher - invalid regex: ${regex}"));
        runResMatcher(ma, patt, input)

    ///
    /// Returns `x` wrapped as a Matcher.
    ///
    pub def point(x: a): ResMatcher[a] = Done(x)


    pub def throwError(msg: String): ResMatcher[a] = MatchError(msg)


    pub def map(f: a -> b & ef, x: ResMatcher[a]): ResMatcher[b] & ef = match x {
        case MatchError(msg) => MatchError(msg)
        case Done(a)         => Done(f(a))
        case Pause(step)     => Pause(r -> map(f, step(r)))

    }


    pub def ap(mf: ResMatcher[a -> b & ef], ma: ResMatcher[a]): ResMatcher[b] & ef = match (mf, ma) {
        case (MatchError(msg), _)         => MatchError(msg)
        case (_, MatchError(msg))         => MatchError(msg)
        case (Done(f), Done(a))           => Done(f(a))
        case (Done(f), Pause(step))       => Pause(r -> map(f, step(r)))
        case (Pause(step), Done(a))       => Pause(r -> ap(step(r), Done(a)))
        case (Pause(step1), Pause(step2)) => Pause(r -> ap(step1(r), step2(r)))
    }

    pub def bind(x: ResMatcher[a], k: a -> ResMatcher[b] & ef): ResMatcher[b] & ef = match x {
       case MatchError(msg) => MatchError(msg)
       case Done(a)         => k(a)
       case Pause(step1)    => Pause(r -> bind(step1(r), k))
    }


    pub def flatMap(k: a -> ResMatcher[b] & ef, x: ResMatcher[a]): ResMatcher[b] & ef = bind(x, k)


    def liftPrim(fn : ##java.util.regex.Matcher -> a & Impure): ResMatcher[a] =
        Pause(r -> {let ans = fn(r); Done(ans)})


    def liftPrimResult(fn : ##java.util.regex.Matcher -> Result[a, String] & Impure): ResMatcher[a] =
        Pause(r -> match fn(r) {
            case Err(msg) => MatchError(msg)
            case Ok(a)    => Done(a)
        })

    pub def matches(): ResMatcher[Bool] = liftPrim(Text/Regex/Internal/Prim.matches)

    pub def lookingAt(): ResMatcher[Bool] = liftPrim(Text/Regex/Internal/Prim.lookingAt)

    pub def find(): ResMatcher[Bool] = liftPrim(Text/Regex/Internal/Prim.find!)

    pub def start(): ResMatcher[Int32] = liftPrimResult(Text/Regex/Internal/Prim.start)

    pub def end(): ResMatcher[Int32] = liftPrimResult(Text/Regex/Internal/Prim.end)

    pub def findLast(): ResMatcher[Bool] =
        let* first = find();
        if (first) {
            let* pos = start();
            let* endpos = findLastHelper(pos);
            findFrom(endpos - 1)
        } else point(false)

    def findLastHelper(lastPos: Int32): ResMatcher[Int32] =
        let* more = find();
        if (more) {
            let* pos = start();
            findLastHelper(pos)
        } else point(lastPos)


    pub def findFrom(b: Int32): ResMatcher[Bool] =
        liftPrimResult(m1 -> Text/Regex/Internal/Prim.findFrom!(m1, b))


    pub def findLastFrom(b: Int32): ResMatcher[Bool] =
        let* first = findFrom(b);
        if (first) {
            let* pos = start();
            let* endpos = findLastHelper(pos);
            findFrom(endpos - 1)
        } else point(false)


    pub def content(): ResMatcher[String] =
        liftPrimResult(Text/Regex/Internal/Prim.group)

    pub def evaluate(eval: MatchEvaluator[a]): ResMatcher[a] =
        Pause(r -> match Text/Regex/MatchEvaluator.runMatchEvaluator(eval, r) {
            case Err(msg) => MatchError(msg)
            case Ok(a)    => Done(a)
        })

    pub def getMatch(): ResMatcher[String] =
        let* ans = find();
        if (ans)
            content()
        else
            throwError("getMatch")

    def ifThenElse(cond: ResMatcher[s], thenm: s -> ResMatcher[t], elsem: ResMatcher[t]): ResMatcher[t] =
        let* s = cond;
        lbchoice(thenm(s), elsem)

    def lbchoice(ma: ResMatcher[a], mb: ResMatcher[a]): ResMatcher[a] = match ma {
        case MatchError(_) => mb
        case Done(a)       => Done(a)
        case Pause(step)   => Pause(r -> step(r))
    }

    pub def done(): ResMatcher[Bool] = ifThenElse(content(), _ -> point(false), point(true))

    pub def reset!(): ResMatcher[Unit] = liftPrim(Text/Regex/Internal/Prim.reset!)

    pub def resetInput!(input: String): ResMatcher[Unit] =
        liftPrim(x -> Text/Regex/Internal/Prim.resetInput!(x, input))

    pub def setRegion!(bounds: {start :: Int32, end :: Int32}): ResMatcher[Unit] =
        liftPrimResult(x -> Text/Regex/Internal/Prim.setRegion!(x, bounds))

    pub def setPattern!(patt: Pattern): ResMatcher[Unit] =
        let Pattern(p1) = patt;
        liftPrimResult(x -> Text/Regex/Internal/Prim.usePattern!(x, p1))


    // TODO - if we have matchers returning multiple results, should they call `find` internally?

    pub def countWith(append: (a, ac) -> ac, acc: ac, n: Int32, ma: ResMatcher[a]): ResMatcher[ac] =
        if (n <= 0)
            point(acc)
        else {
            let* a = ma;
            countWith(append, append(a, acc), n-1, ma)
        }

    pub def count(n: Int32, ma: ResMatcher[a]): ResMatcher[List[a]] =
        use Functor.{<$>};
        use Chain.{empty, snoc, toList};
        toList <$> countWith(flip(snoc), empty(), n, ma)


}

