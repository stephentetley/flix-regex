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


namespace Text/Regex/Internal/Prim {


    ///
    /// Create a Matcher for Pattern `patt` on the source String `input`.
    ///
    pub def newMatcher(patt: ##java.util.regex.Pattern, input: String): ##java.util.regex.Matcher & Impure =
        import java.util.regex.Pattern.matcher(##java.lang.CharSequence);
        let cs1 = input as ##java.lang.CharSequence;
        matcher(patt, cs1)

    ///
    /// Tests whether the matcher matches all of the input region.
    ///
    pub def matches(x: ##java.util.regex.Matcher): Bool & Impure =
        import java.util.regex.Matcher.matches();
        matches(x)

    ///
    /// Tests whether the matcher matches the start of the input region.
    ///
    pub def lookingAt(x: ##java.util.regex.Matcher): Bool & Impure =
        import java.util.regex.Matcher.lookingAt();
        lookingAt(x)

    ///
    /// Attempt to find the next match. Returns `true` and moves to the next match
    /// if there is a next match otherwise returns `false`.
    ///
    /// The internal state of the matcher is updated.
    ///
    pub def find(x: ##java.util.regex.Matcher): Bool & Impure =
        import java.util.regex.Matcher.find();
        find(x)

    ///
    /// Attempt to find the next match after the supplied position `start`.
    /// Returns `true` and moves to the next match if there is a next match
    /// otherwise returns `false`.
    ///
    /// The internal state of the matcher is updated.
    ///
    pub def findFrom(x: ##java.util.regex.Matcher, start: Int32): Result[Bool, String] & Impure =
        import java.util.regex.Matcher.find(Int32);
        try {
            Ok(find(x, start))
        } catch {
            case _: ##java.lang.Exception => Err("findFrom")
        }

    ///
    /// Replace all the matches found by this matcher with the string `rep`.
    ///
    pub def replaceAll(x: ##java.util.regex.Matcher, rep: String): String & Impure =
        import java.util.regex.Matcher.replaceAll(String);
        replaceAll(x, rep)

    ///
    /// Replace the first match found by this matcher with the string `rep`.
    ///
    pub def replaceFirst(x: ##java.util.regex.Matcher, rep: String): String & Impure =
        import java.util.regex.Matcher.replaceFirst(String);
        replaceFirst(x, rep)

    ///
    /// Reset this matcher.
    ///
    /// Note Java's reset imperatively resets `x` and returns it (Java's builder pattern).
    /// This is idiomatically functional, so we imperatively reset `x` and return Unit.
    ///
    pub def reset!(x: ##java.util.regex.Matcher): Unit & Impure =
        import java.util.regex.Matcher.reset();
        reset(x);
        ()

    ///
    /// Reset the input of this matcher.
    ///
    pub def resetInput!(x: ##java.util.regex.Matcher, input: String): Unit & Impure =
        import java.util.regex.Matcher.reset(##java.lang.CharSequence);
        let cs = input as ##java.lang.CharSequence;
        reset(x, cs);
        ()

    ///
    /// Change the pattern used by this matcher.
    ///
    pub def usePattern!(x: ##java.util.regex.Matcher, patt: ##java.util.regex.Pattern): Result[Unit, String] & Impure =
        import java.util.regex.Matcher.usePattern(##java.util.regex.Pattern);
        try {
            usePattern(x, patt);
            Ok()
        } catch {
            case _: ##java.lang.Exception => Err("usePattern!")
        }

    ///
    /// Set the bounds of the matcher's region.
    ///
    pub def setRegion!(x: ##java.util.regex.Matcher, bounds: {start :: Int32, end :: Int32}): Result[Unit, String] & Impure =
        import java.util.regex.Matcher.region(Int32, Int32);
        try {
            region(x, bounds.start, bounds.end);
            Ok()
        } catch {
            case _: ##java.lang.Exception => Err("region")
        }

    ///
    /// Return the start position of the string matched by the preceeding `find`
    ///
    pub def start(x: ##java.util.regex.Matcher): Result[Int32, String] & Impure =
        import java.util.regex.Matcher.start();
        try {
            Ok(start(x))
        } catch {
            case _: ##java.lang.Exception => Err("start")
        }

    ///
    /// Return the end position of the string matched by the preceeding `find`
    ///
    pub def end(x: ##java.util.regex.Matcher): Result[Int32, String] & Impure =
        import java.util.regex.Matcher.end();
        try {
            Ok(end(x))
        } catch {
            case _: ##java.lang.Exception => Err("end")
        }

    ///
    /// Return the string matched by the preceeding `find`
    ///
    pub def group(x: ##java.util.regex.Matcher): Result[String, String] & Impure =
        import java.util.regex.Matcher.group();
        try {
            Ok(group(x))
        } catch {
            case _: ##java.lang.Exception => Err("group")
        }


    ///
    /// Returns the number of capture groups in the pattern used by this PrimMatcher.
    ///
    /// This is not the number of groups matched, which should be all groups in a pattern
    /// or none.
    ///
    pub def groupCount(x: ##java.util.regex.Matcher): Int32 & Impure =
        import java.util.regex.Matcher.groupCount();
        groupCount(x)

    ///
    /// Return the part of string matched by the preceeding `find` for the given group `ix`.
    ///
    /// This follows the Java convention where index 0 matches everything.
    ///
    pub def groupWithIndex(x: ##java.util.regex.Matcher, ix: Int32): Result[String, String] & Impure =
        import java.util.regex.Matcher.group(Int32);
        try {
            Ok(group(x, ix))
        } catch {
            case _: ##java.lang.Exception => Err("groupWithIndex - ${ix}")
        }

    ///
    /// Return the part of string matched by the preceeding `find` for the given group `label`.
    ///
    pub def groupWithLabel(x: ##java.util.regex.Matcher, label: String): Result[String, String] & Impure =
        import java.util.regex.Matcher.group(String);
        try {
            Ok(group(x, label))
        } catch {
            case _: ##java.lang.Exception => Err("groupWithLabel - '${label}'")
        }

    ///
    /// Return the start index of string matched by the preceeding `find` for the given group `ix`.
    ///
    /// This follows the Java convention where index 0 matches everything.
    ///
    pub def startWithIndex(x: ##java.util.regex.Matcher, ix: Int32): Result[Int32, String] & Impure =
        import java.util.regex.Matcher.start(Int32);
        try {
            Ok(start(x, ix))
        } catch {
            case _: ##java.lang.Exception => Err("startWithIndex")
        }

    ///
    /// Return the start index of string matched by the preceeding `find` for the given group `label`.
    ///
    pub def startWithLabel(x: ##java.util.regex.Matcher, label: String): Result[Int32, String] & Impure =
        import java.util.regex.Matcher.start(String);
        try {
            Ok(start(x, label))
        } catch {
            case _: ##java.lang.Exception => Err("startWithLabel")
        }

    ///
    /// Return the end position of string matched by the preceeding `find` for the given group `ix`.
    ///
    /// This follows the Java convention where index 0 matches everything.
    ///
    pub def endWithIndex(x: ##java.util.regex.Matcher, ix: Int32): Result[Int32, String] & Impure =
        import java.util.regex.Matcher.end(Int32);
        try {
            Ok(end(x, ix))
        } catch {
            case _: ##java.lang.Exception => Err("endWithIndex")
        }

    ///
    /// Return the end position of string matched by the preceeding `find` for the given group `label`.
    ///
    pub def endWithLabel(x: ##java.util.regex.Matcher, label: String): Result[Int32, String] & Impure =
        import java.util.regex.Matcher.end(String);
        try {
            Ok(end(x, label))
        } catch {
            case _: ##java.lang.Exception => Err("endWithLabel")
        }

    ///
    /// Append the replacement `rep` to the string builder `sb` for the current match step.
    ///
    pub def appendReplacement!(x: ##java.util.regex.Matcher, sb: StringBuilder, rep: String): Result[Unit, String] & Impure =
        import java.util.regex.Matcher.appendReplacement(##java.lang.StringBuilder, String);
        try {
            let StringBuilder(sb1) = sb;
            let _ = appendReplacement(x, sb1, rep);
            Ok()
        } catch {
            case _: ##java.lang.Exception => Err("appendReplacement")
        }

    ///
    /// Append the remaining tail of the input to the string builder `sb` for the final match step.
    ///
    pub def appendTail!(x: ##java.util.regex.Matcher, sb: StringBuilder): Unit & Impure =
        import java.util.regex.Matcher.appendTail(##java.lang.StringBuilder);
        let StringBuilder(sb1) = sb;
        appendTail(x, sb1);
        ()

}
