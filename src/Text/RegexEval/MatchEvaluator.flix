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



namespace Text/RegexEval {

    ///
    /// Matcher is a monad (handle, error).
    ///
    pub opaque type MatchEvaluator[a] = ##java.util.regex.Matcher -> Result[a, String] & Impure

    instance Functor[MatchEvaluator] {
        pub def map(f: a -> b & ef, o: MatchEvaluator[a]): MatchEvaluator[b] & ef =
            Text/RegexEval/MatchEvaluator.map(f, o)
    }

    instance Applicative[MatchEvaluator] {
        pub def point(x: a): MatchEvaluator[a] = 
            Text/RegexEval/MatchEvaluator.point(x)

        pub def ap(f: MatchEvaluator[a -> b & ef], x: MatchEvaluator[a]): MatchEvaluator[b] & ef =
            Text/RegexEval/MatchEvaluator.ap(f, x)
    }

    instance Monad[MatchEvaluator] {
        pub def flatMap(f: a -> MatchEvaluator[b] & ef, o: MatchEvaluator[a]): MatchEvaluator[b] & ef =
            Text/RegexEval/MatchEvaluator.flatMap(f, o)
    }

}

namespace Text/RegexEval/MatchEvaluator {

    use Applicative.{liftA2};
    use FromString.fromString;

    use Text/RegexEval.MatchEvaluator;
    use Text/RegexEval.MatchEvaluator.{MatchEvaluator};

    use Text/Regex.Pattern;
    use Text/Regex.Pattern.{Pattern};

    pub def point(x: a): MatchEvaluator[a] = MatchEvaluator(_ -> Ok(x) as & Impure)


    def apply1(x: MatchEvaluator[a], m: ##java.util.regex.Matcher): Result[a, String] & Impure =
        let MatchEvaluator(f) = x;
        f(m)

    pub def runMatchEvaluator(x: MatchEvaluator[a], m: ##java.util.regex.Matcher): Result[a, String] & Impure =
        apply1(x, m)


    pub def throwError(msg: String): MatchEvaluator[b] =
        MatchEvaluator(_ -> Err(msg) as & Impure)

    pub def swapError(msg: String, mx: MatchEvaluator[a]): MatchEvaluator[a] =
        MatchEvaluator(r -> match apply1(mx, r) {
            case Ok(a) => Ok(a)
            case Err(_) => Err(msg)
        })

    pub def map(f: a -> b & ef, x: MatchEvaluator[a]): MatchEvaluator[b] & ef =
        MatchEvaluator(r ->
            match apply1(x, r) {
                case Ok(a) => {
                    let ans = f(a) as & Impure;
                    Ok(ans)
                }
                case Err(e) => Err(e)
            }
        ) as & ef

    pub def ap(mf: MatchEvaluator[a -> b & ef], ma: MatchEvaluator[a]): MatchEvaluator[b] & ef =
        MatchEvaluator(r ->
            match apply1(mf, r) {
                case Ok(f) => match apply1(ma, r) {
                    case Ok(a) => Ok(f(a) as & Impure)
                    case Err(e2) => Err(e2)
                }
                case Err(e1) => Err(e1)
            }
        ) as & ef

    pub def bind(x: MatchEvaluator[a], k: a -> MatchEvaluator[b] & ef): MatchEvaluator[b] & ef =
        MatchEvaluator(r ->
            match apply1(x, r) {
                case Ok(a) => apply1(k(a) as & Impure, r)
                case Err(e) => Err(e)
            }
        ) as & ef

    pub def flatMap(k: a -> MatchEvaluator[b] & ef, x: MatchEvaluator[a]): MatchEvaluator[b] & ef = bind(x, k)

    // MatchEvaluator can be alternative because it does not "step".
    // It is only run on the current match.

    pub def mzero(): MatchEvaluator[a] = throwError("MatchEvaluator.mzero")

    pub def alt(ma: MatchEvaluator[a], mb: MatchEvaluator[a]): MatchEvaluator[a] =
        MatchEvaluator(r ->
            match apply1(ma, r) {
                case Ok(a) => Ok(a)
                case Err(_) => apply1(mb, r)
            }
        )


    def liftPrimResult(fn: ##java.util.regex.Matcher -> Result[a, String] & Impure): MatchEvaluator[a] =
        MatchEvaluator(r -> fn(r) as & Impure)

    pub def liftOption(x: Option[a], errMsg: String): MatchEvaluator[a] = match x {
        case Some(x1) => point(x1)
        case None => throwError(errMsg)
    }


    pub def liftResult(x: Result[a, String]): MatchEvaluator[a] =
        MatchEvaluator(_ -> x as & Impure)

    pub def liftOperation(fn: Unit -> Result[a, String] & ef): MatchEvaluator[a] =
        MatchEvaluator(_ -> fn() as & Impure)


    pub def optional(mx: MatchEvaluator[a]): MatchEvaluator[Option[a]] =
        MatchEvaluator(r -> match apply1(mx, r) {
            case Ok(a) => Ok(Some(a))
            case Err(_) => Ok(None)
        })

    pub def withDefault(mx: MatchEvaluator[a], d: a): MatchEvaluator[a] =
        MatchEvaluator(r -> match apply1(mx, r) {
            case Ok(a) => Ok(a)
            case Err(_) => Ok(d)
        })


    pub def content(): MatchEvaluator[String] = liftPrimResult(Text/Regex/Internal/Prim.group)

    /// TODO - is this worthwhile?
    pub def transformContent(f: String -> a): MatchEvaluator[a] = map(f, content())


    pub def getStart(): MatchEvaluator[Int32] = liftPrimResult(Text/Regex/Internal/Prim.start)

    pub def getEnd(): MatchEvaluator[Int32] =
        liftPrimResult(Text/Regex/Internal/Prim.end)

    pub def getLength(): MatchEvaluator[Int32] =
        liftA2((x,y) -> y - x, getStart(), getEnd()) |> swapError("getLength")

    ///
    ///
    pub def getRange(): MatchEvaluator[{start :: Int32, end :: Int32}] =
        liftA2((x,y) -> {start = x, end = y}, getStart(), getEnd()) |> swapError("getRange")


    ///
    /// Note - this function uses 0-indexing, the equivalent Java method `group(int group)` uses 1-indexing.
    ///
    pub def getCaptureAt(ix: Int32): MatchEvaluator[String] =
        liftPrimResult(x -> Text/Regex/Internal/Prim.groupWithIndex(x, ix+1))
            |> swapError("getCaptureAt(${ix})")

    pub def getCaptureNamed(label: String): MatchEvaluator[String] =
        liftPrimResult(x -> Text/Regex/Internal/Prim.groupWithLabel(x, label))
            |> swapError("getCaptureNamed('${label}')")

    ///
    /// Note - this function uses 0-indexing, the equivalent Java method `group(int group)` uses 1-indexing.
    ///
    pub def getCaptureStartAt(ix: Int32): MatchEvaluator[Int32] =
        liftPrimResult(x -> Text/Regex/Internal/Prim.startWithIndex(x, ix+1))
            |> swapError("getCaptureStartAt(${ix})")

    pub def getCaptureStartNamed(label: String): MatchEvaluator[Int32] =
        liftPrimResult(x -> Text/Regex/Internal/Prim.startWithLabel(x, label))
            |> swapError("getCaptureStartNamed('${label}')")

    ///
    /// Note - this function uses 0-indexing, the equivalent Java method `group(int group)`
    /// uses 1-indexing.
    ///
    pub def getCaptureEndAt(ix: Int32): MatchEvaluator[Int32] =
        liftPrimResult(x -> Text/Regex/Internal/Prim.endWithIndex(x, ix+1))
            |> swapError("getCaptureEndAt(${ix})")

    pub def getCaptureEndNamed(label: String): MatchEvaluator[Int32] =
        liftPrimResult(x -> Text/Regex/Internal/Prim.endWithLabel(x, label))
            |> swapError("getCaptureEndNamed('${label}')")

    ///
    /// Note - this function uses 0-indexing, the equivalent Java method `group(int group)`
    /// uses 1-indexing.
    ///
    pub def getCaptureRangeAt(ix: Int32): MatchEvaluator[{start :: Int32, end :: Int32}] =
        liftA2((x,y) -> {start = x, end = y}, getCaptureStartAt(ix), getCaptureEndAt(ix))
            |> swapError("getCaptureRangeAt(${ix})")

    pub def getCaptureRangeNamed(label: String): MatchEvaluator[{start :: Int32, end :: Int32}] =
        liftA2((x, y) -> {start = x, end = y}, getCaptureStartNamed(label), getCaptureEndNamed(label))
            |> swapError("getCaptureRangeNamed('${label}')")


    // Note - type annos are a temporary fix while Flix's runtime cast error is resolved.

    pub def getInt8At(ix: Int32): MatchEvaluator[Int8] =
        let* str = getCaptureAt(ix);
        liftOption(fromString(str): Option[Int8], "getInt8At(${ix})")

    pub def getInt8Named(label: String): MatchEvaluator[Int8] =
        let* str = getCaptureNamed(label);
        liftOption(fromString(str): Option[Int8], "getInt8Named('${label}')")

    pub def getInt16At(ix: Int32): MatchEvaluator[Int16] =
        let* str = getCaptureAt(ix);
        liftOption(fromString(str): Option[Int16], "getInt16At(${ix})")

    pub def getInt16Named(label: String): MatchEvaluator[Int16] =
        let* str = getCaptureNamed(label);
        liftOption(fromString(str): Option[Int16], "getInt16Named('${label}')")

    pub def getInt32At(ix: Int32): MatchEvaluator[Int32] =
        let* str = getCaptureAt(ix);
        liftOption(fromString(str): Option[Int32], "getInt32At(${ix})")

    pub def getInt32Named(label: String): MatchEvaluator[Int32] =
        let* str = getCaptureNamed(label);
        liftOption(fromString(str): Option[Int32], "getInt32Named('${label}')")

    pub def getInt64At(ix: Int32): MatchEvaluator[Int64] =
        let* str = getCaptureAt(ix);
        liftOption(fromString(str): Option[Int64], "getInt64At(${ix})")

    pub def getInt64Named(label: String): MatchEvaluator[Int64] =
        let* str = getCaptureNamed(label);
        liftOption(fromString(str): Option[Int64], "getInt64Named('${label}')")

    pub def getBigIntAt(ix: Int32): MatchEvaluator[BigInt] =
        let* str = getCaptureAt(ix);
        liftOption(fromString(str): Option[BigInt], "getBigIntAt(${ix})")

    pub def getBigIntNamed(label: String): MatchEvaluator[BigInt] =
        let* str = getCaptureNamed(label);
        liftOption(fromString(str): Option[BigInt], "getBigIntNamed('${label}')")

    pub def getFloat32At(ix: Int32): MatchEvaluator[Float32] =
        let* str = getCaptureAt(ix);
        liftOption(fromString(str): Option[Float32], "getFloat32At(${ix})")

    pub def getFloat32Named(label: String): MatchEvaluator[Float32] =
        let* str = getCaptureNamed(label);
        liftOption(fromString(str): Option[Float32], "getFloat32Named('${label}')")

    pub def getFloat64At(ix: Int32): MatchEvaluator[Float64] =
        let* str = getCaptureAt(ix);
        liftOption(fromString(str): Option[Float64], "getFloat64At(${ix})")

    pub def getFloat64Named(label: String): MatchEvaluator[Float64] =
        let* str = getCaptureNamed(label);
        liftOption(fromString(str): Option[Float64], "getFloat64Named('${label}')")


}
