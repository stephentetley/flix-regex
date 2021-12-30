

def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running...");
    runTest("test01", _ -> test01());
    runTest("test02", _ -> test02());
    runTest("test03", _ -> test03());
    0


def runTest(msg: String, f: Unit -> Result[a, String] & Impure): Unit & Impure with ToString[a]= 
    println(msg);
    f() |> println


def test01(): Result[Bool, String] & Impure =
    use Result.{flatMap};
    use Text/Regex/ResMatcher.{runResMatcher, matches};
    let* patt = Text/Regex/Pattern.compile("(\\p{Digit}+)");
    runResMatcher(matches(), patt, "1000")


def test02(): Result[(Int32, Int32, Int32), String] & Impure =
    use Result.{flatMap};
    use Applicative.{*>};
    use Text/Regex/ResMatcher.{evaluate, find, runResMatcher};
    let* patt = Text/Regex/Pattern.compile("(?<num>\\p{Digit}+)");
    let eval1 = Text/Regex/MatchEvaluator.getInt32Named("num");
    let getInt = find() *> evaluate(eval1);
    let extractor = Applicative.product3(getInt, getInt, getInt);
    runResMatcher(extractor, patt, "Game 4, Set 2, Match 1")


def test03(): Result[List[Int32], String] & Impure =
    use Result.{flatMap};
    use Applicative.{*>};
    use Text/Regex/ResMatcher.{evaluate, find, count, runResMatcher};
    let* patt = Text/Regex/Pattern.compile("(?<num>\\p{Digit}+)");
    let eval1 = Text/Regex/MatchEvaluator.getInt32Named("num");
    let getInt = find() *> evaluate(eval1);
    let extractor = count(3, getInt);
    runResMatcher(extractor, patt, "Game 4, Set 2, Match 1")
