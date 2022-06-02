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

    pub enum Matcher[_: Region](##java.util.regex.Matcher)

    
    // No New instance - matcher must be created from pattern and input

    instance Scoped[Matcher] {
        pub def regionOf(_: Matcher[r]): Region[r] = () as Region[r]
    }

}

namespace Text/Regex/Matcher {


    use Text/Regex.{Matcher};
    use Text/Regex.Matcher.{Matcher};

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

/*
    ///
    /// Fully run a match function.
    ///
    pub def runMatcher(f: Matcher[r] -> a \ Read(r), p: Pattern, s: String): a =
        region r {
            let m = newMatcher(r, p, s);
            f(m)
        }
*/

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
    pub def find!(m: Matcher[r]): Bool \ Read(r) = 
        import java.util.regex.Matcher.find(): Bool & r;
        getMatcher(m) |> find

    ///
    /// Attempt to find the start of next match. Returns `Some(position)` if there is a match 
    /// otherwise returns `None`.
    ///
    /// The internal state of the matcher is updated.
    ///
    pub def findStart!(m: Matcher[r]): Option[Int32] \ Read(r) = 
        if (find!(m)) 
            start(m) |> Result.toOption
        else
            None


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

    pub def findLast!(m: Matcher[r]): Bool \ Read(r) =
        match findStart!(m) {
            case None      => false
            case Some(pos) => match findLastHelper(pos, m) {
                case Err(_) => false
                case Ok(end) => Result.getWithDefault(false, findFrom!(end - 1, m))
            }
        }
        
    def findLastHelper(lastPos: Int32, m: Matcher[r]): Result[Int32, String] \ Read(r) =
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
    pub def findFrom!(pos: Int32, m: Matcher[r]): Result[Bool, String] \ Read(r) =
        import java.util.regex.Matcher.find(Int32): Bool & r;
        import java.lang.Throwable.getMessage(): String & r;
        try {
            getMatcher(m) |> flip(find, pos) |> Ok
        } catch {
            case ex: ##java.lang.Exception => Err("findFrom: ${getMessage(ex)}")
        }

    ///
    /// Attempt to find the start of first match starting from `pos`. 
    /// Returns `Some(position)` if there is a match otherwise returns `None`.
    ///
    /// The internal state of the matcher is updated.
    ///
    pub def findStartFrom!(pos: Int32, m: Matcher[r]): Result[Option[Int32], String] \ Read(r) = 
        match findFrom!(pos, m) {
            case Err(msg)  => Err(msg)
            case Ok(true)  => start(m) |> Result.map(Some)
            case Ok(false) => Ok(None)
        }


    pub def findLastFrom!(pos: Int32, m: Matcher[r]): Result[Bool, String] \ Read(r) =
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

    pub def getMatch(m: Matcher[r]): Option[String] \ Read(r) =
        if (find!(m))
            content(m) |> Result.toOption
        else
            None

    /// pub def done(): Matcher[Bool] = ifThenElse(content(), _ -> point(false), point(true))

    /// pub def reset!(): Matcher[Unit] = liftPrim(Text/Regex/Internal/Prim.reset!)

    /// pub def resetInput!(input: String): Matcher[Unit] =
    ///     liftPrim(x -> Text/Regex/Internal/Prim.resetInput!(x, input))

    ///
    /// Set the bounds of the matcher's region.
    ///
    pub def setBounds!(bounds: {start :: Int32, end :: Int32}, m: Matcher[r]): Result[Unit, String] \ Write(r) =
        import java.util.regex.Matcher.region(Int32, Int32): ##java.util.regex.Matcher & r;
        import java.lang.Throwable.getMessage(): String & r;
        try {
            let Matcher(m1) = m;
            let _ = region(m1, bounds.start, bounds.end);
            Ok()
        } catch {
            case ex: ##java.lang.Exception => Err("setBounds!: ${getMessage(ex)}")
        }

    /// pub def setPattern!(patt: Pattern): Matcher[Unit] =
    ///     let Pattern(p1) = patt;
    ///     liftPrimResult(x -> Text/Regex/Internal/Prim.usePattern!(x, p1))



    /// pub def countWith(append: (a, ac) -> ac, acc: ac, n: Int32, ma: Matcher[a]): Matcher[ac] =
    ///     if (n <= 0)
    ///         point(acc)
    ///     else {
    ///         let* a = ma;
    ///         countWith(append, append(a, acc), n-1, ma)
    ///     }

    /// pub def count(n: Int32, ma: Matcher[a]): Matcher[List[a]] =
    ///     use Functor.{<$>};
    ///     use Chain.{empty, snoc, toList};
    ///     toList <$> countWith(flip(snoc), empty(), n, ma)

}

