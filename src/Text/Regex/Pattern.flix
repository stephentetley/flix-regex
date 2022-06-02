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

    ///
    /// Represents a compiled Pattern.
    ///
    pub enum Pattern(##java.util.regex.Pattern)

    instance ToString[Pattern] {
        ///
        /// Return the string representation of the Pattern `patt`.
        ///
        pub def toString(patt: Pattern): String =
            import java.util.regex.Pattern.toString(): String & Pure;
            let Pattern(p1) = patt;
            toString(p1)
    }

    instance FromString[Pattern] {
        ///
        /// Return the string representation of the Pattern `patt`.
        ///
        pub def fromString(regex: String): Option[Pattern] =
            import static java.util.regex.Pattern.compile(String): ##java.util.regex.Pattern & Pure;
            try {
                Some(Pattern(compile(regex)))
            } catch {
                case _: ##java.util.regex.PatternSyntaxException => None
            }
    }

}

namespace Text/Regex/Pattern {

    use Text/Regex.Pattern;
    use Text/Regex.Pattern.{Pattern};

    pub enum Flag with Eq, Order, ToString {
        case CanonEq,
        case CaseInsenstive,
        case Comments,
        case Dotall,
        case Literal,
        case Multiline,
        case UnicodeCase,
        case UnicodeCharacterClass,
        case UnixLines
    }


    def toInt(flag: Flag): Int32 =
        import static get java.util.regex.Pattern.CANON_EQ: Int32 & Pure as get_CANON_EQ;
        import static get java.util.regex.Pattern.CASE_INSENSITIVE: Int32 & Pure as get_CASE_INSENSITIVE;
        import static get java.util.regex.Pattern.COMMENTS: Int32 & Pure as get_COMMENTS;
        import static get java.util.regex.Pattern.DOTALL: Int32 & Pure as get_DOTALL;
        import static get java.util.regex.Pattern.LITERAL: Int32 & Pure as get_LITERAL;
        import static get java.util.regex.Pattern.MULTILINE: Int32 & Pure as get_MULTILINE;
        import static get java.util.regex.Pattern.UNICODE_CASE: Int32 & Pure as get_UNICODE_CASE;
        import static get java.util.regex.Pattern.UNICODE_CHARACTER_CLASS: Int32 & Pure as get_UNICODE_CHARACTER_CLASS;
        import static get java.util.regex.Pattern.UNIX_LINES: Int32 & Pure as get_UNIX_LINES;
        match flag {
            case CanonEq                => get_CANON_EQ()
            case CaseInsenstive         => get_CASE_INSENSITIVE()
            case Comments               => get_COMMENTS()
            case Dotall                 => get_DOTALL()
            case Literal                => get_LITERAL()
            case Multiline              => get_MULTILINE()
            case UnicodeCase            => get_UNICODE_CASE()
            case UnicodeCharacterClass  => get_UNICODE_CHARACTER_CLASS()
            case UnixLines              => get_UNIX_LINES()
        }

    def sumFlags(flags: List[Flag]): Int32 =
        List.foldLeft((ac,e) -> ac + toInt(e), 0, flags)

    def listFlags(x: Int32): List[Flag] =
        let check = y -> {
            let y1 = toInt(y);
            (x &&& y1) == y1
        };
        List.filter(check, CanonEq :: CaseInsenstive :: Comments :: Dotall :: Literal ::
                            Multiline :: UnicodeCase :: UnicodeCharacterClass :: UnixLines :: Nil)

    ///
    /// Compile the regular expression `regex` into a Pattern.
    ///
    /// Returns `Err` if the regex is ill-formed and cannot be compiled.
    ///
    pub def compile(regex: String): Result[Pattern, String] =
        import static java.util.regex.Pattern.compile(String): ##java.util.regex.Pattern & Pure;
        try {
            Ok(Pattern(compile(regex)))
        } catch {
            case _: ##java.util.regex.PatternSyntaxException => Err("Text/Regex/Pattern.compile failed - '${regex}'")
        }



    ///
    /// Compile the regular expression `regex` into a Pattern.
    ///
    /// Returns `Err` if the regex is ill-formed and cannot be compiled.
    ///
    pub def compileWithFlags(regex: String, flags: List[Flag]): Result[Pattern, String] =
        import static java.util.regex.Pattern.compile(String, Int32): ##java.util.regex.Pattern & Pure;
        try {
            Ok(Pattern(compile(regex, sumFlags(flags))))
        } catch {
            case _: ##java.lang.Exception => Err("Text/Regex/Pattern.compileWithFlags failed - '${regex}', ${flags}")
        }

    ///
    /// Irrefutabily compile the regular expression `regex` into a Pattern.
    /// If compilation fails the pattern will be substituted by the "unmatchable" pattern.
    ///
    /// Use this only in circumstances where you know the `regex` is a valid pattern
    /// and you need to avoid wrapping it in an `Ok`.
    ///
    pub def irrefutablePattern(regex: String): Pattern =
        import static java.util.regex.Pattern.compile(String): ##java.util.regex.Pattern & Pure;
        try {
            Pattern(compile(regex))
        } catch {
            case _: ##java.util.regex.PatternSyntaxException => unmatchable()
        }

    ///
    /// Return the unmatchable pattern - a pattern that will not match any input.
    ///
    pub def unmatchable(): Pattern =
        import static java.util.regex.Pattern.compile(String): ##java.util.regex.Pattern & Pure;
        try {
            Pattern(compile("^\\b$"))
        } catch {
            case _: ##java.util.regex.PatternSyntaxException => unreachable!()
        }


    pub def quote(s: String): String =
        import static java.util.regex.Pattern.quote(String): String & Pure;
        quote(s)

    ///
    /// Return the regular expression used to build this pattern.
    ///
    pub def getPattern(patt: Pattern): String =
        import java.util.regex.Pattern.pattern(): String & Pure;
        let Pattern(p1) = patt;
        pattern(p1)

    ///
    /// Return the flags used to build this pattern.
    ///
    pub def getFlags(patt: Pattern): List[Flag] =
        import java.util.regex.Pattern.flags(): Int32 & Pure;
        let Pattern(p1) = patt;
        let i = flags(p1);
        listFlags(i)

    ///
    /// Recompile the pattern `patt`, modifying its source pattern with `f`.
    ///
    pub def modifyPattern(f: String -> String, patt: Pattern): Result[Pattern, String] =
        let flags = getFlags(patt);
        let src = getPattern(patt);
        compileWithFlags(f(src), Multiline :: flags)



}
