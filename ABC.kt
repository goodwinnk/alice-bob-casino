import java.util.ArrayList
import java.util.regex.Pattern
import java.util.HashMap
import java.util.HashSet
import Strategy.BobStrategyBuilder

/* 
 * Solution for Alice Bob Casino problem. 
 * http://domino.research.ibm.com/comm/wwwr_ponder.nsf/challenges/September2013.html 
 */
val strategy = generateStrategy {
    case("0........", STANDARD_BOB_STRATEGY, STANDARD_ALICE_STRATEGY)
    case(".111.....", STANDARD_BOB_STRATEGY, STANDARD_ALICE_STRATEGY)
    case(".000.....", STANDARD_BOB_STRATEGY, STANDARD_ALICE_STRATEGY)
    case("....111..", STANDARD_BOB_STRATEGY, STANDARD_ALICE_STRATEGY)
    case("....000..", STANDARD_BOB_STRATEGY, STANDARD_ALICE_STRATEGY)
    case("........0", STANDARD_BOB_STRATEGY, STANDARD_ALICE_STRATEGY)

    case("1(001|010)(110|101|011).1",
            "0 {warn_about_error(2)} {two_of_three_with_hint_in_error(5, c8)} c8 1",

            "0 {e[1]=0}  0 {f[2]=1} 1 0  1 {e[3]?} 1 {e[3]?} 1 {e[3]?} {number_of_errors=3}  (e[3])  1",
            "0 {e[1]=0}  0 0 {f[2]=1} 1  1 {e[3]?} 1 {e[3]?} 1 {e[3]?} {number_of_errors=3}  (e[3])  1")

    case("1(110|101)(001|010|100).1",
            "1 {warn_about_error(2)} {two_of_three_with_hint_in_error(5, c8)} c8 1",

            "0 {e[1]=1}  1 {f[2]=0} 0 1  0 {e[3]?} 0 {e[3]?} 0 {e[3]?} {number_of_errors=3}  (e[3])  1",
            "0 {e[1]=1}  1 1 {f[2]=0} 0  0 {e[3]?} 0 {e[3]?} 0 {e[3]?} {number_of_errors=3}  (e[3])  1")

    //------------------------

    case("1...(001|010)11",
            "{two_of_three_hint(2)} {two_of_three_with_hint_in_error(2, 0)} {warn_about_error(5)} 1 1",

            "0 {number_of_errors=1}  (e[1]) {e[2]?} (e[1]) {e[2]?} (e[1]) {number_of_errors=2}  (e[2]) {f[3]=1} 1 (e[2])  1  1",
            "0 {number_of_errors=1}  (e[1]) {e[2]?} (e[1]) {e[2]?} (e[1]) {number_of_errors=2}  (e[2]) (e[2]) {f[3]=1} 1  1  1")

    case("1...(101|110)01",
            "{two_of_three_hint(2)} {two_of_three_with_hint_in_error(2, 1)} {warn_about_error(5)} 0 1",

            "0 {number_of_errors=1}  (e[1]) {e[2]?} (e[1]) {e[2]?} (e[1]) {e[2]?} {number_of_errors=2}  (e[2]) {f[3]=0} 0 (e[2])  0  1",
            "0 {number_of_errors=1}  (e[1]) {e[2]?} (e[1]) {e[2]?} (e[1]) {e[2]?} {number_of_errors=2}  (e[2]) (e[2]) {f[3]=0} 0  0  1")

    //------------------------

    case("1(001|010|100)(001|010|100)01",
            "1 {two_hints_in_errors(2, c5, c6)} c5 c6 c7 0 1",
            "0 {e[1]=1}  1 {e[2]?} 1 {e[2]?} {e[3]?} 1 {e[3]?} {number_of_errors=3}  (e[2]) (e[3]) (make_two_of_three[0, c5, c6])  0  1")

    case("1(110|101|011)(110|101|011)11",
            "0 {two_hints_in_errors(2, c5, c6)} c5 c6 c7 1 1",
            "0 {e[1]=0}  0 {e[2]?} 0 {e[2]?} {e[3]?} 0 {e[3]?} {number_of_errors=3}  (e[2]) (e[3]) (make_two_of_three[1, c5, c6])  1  1")

    //       1   2  3                                                                          1 2 3
    case("1 011 010 0 1", "1 001 010 0 1", "0 {e[1]=1} 1 {e[2]=0} 1 {f[3]=0} 1 010 0 1")    // 1 0 0
    case("1 100 101 1 1", "0 110 101 1 1", "0 {e[1]=0} 0 {e[2]=1} 0 {f[3]=1} 0 101 1 1")    // 0 1 1

    case("1 011 001 0 1", "1 010 001 0 1", "0 {e[1]=1} 1 {e[2]=0} 1 1 {f[3]=0} 001 0 1")    // 1 0 0
    case("1 100 110 1 1", "0 101 110 1 1", "0 {e[1]=0} 0 {e[2]=1} 0 0 {f[3]=1} 110 1 1")    // 0 1 1

    //------------------------

    case("1 011 100 0 1", "0 110 100 0 1", "0 {e[1]=0} 0 {f[2]=1} 1 0 {e[3]=0} 100 0 1")    // 1 0 0
    case("1 100 011 1 1", "1 001 011 1 1", "0 {e[1]=1} 1 {f[2]=0} 0 1 {e[3]=1} 011 1 1")    // 0 1 1

    //------------------------

    case("1 100 100 1 1", "0 010 100 1 1", "0 {e[1]=0} 0 {e[2]=0} 0 {f[3]=1} 0 100 1 1")    // 0 0 1
    case("1 011 011 0 1", "1 101 011 0 1", "0 {e[1]=1} 1 {e[2]=1} 1 {f[3]=0} 1 011 0 1")    // 1 1 0

    case("1 100 011 0 1", "0 001 011 0 1", "0 {e[1]=0} 0 {e[2]=0} 0 0 {f[3]=1} 011 0 1")    // 0 1 0
    case("1 011 100 1 1", "1 110 100 1 1", "0 {e[1]=1} 1 {e[2]=1} 1 1 {f[3]=0} 100 1 1")    // 1 0 1

    //------------------------

    case("1 001 100 1 1", "0 101 100 1 1", "0 {e[1]=0} 0 {f[2]=1} 1 {e[3]=0} 1 100 1 1")    // 0 0 1
    case("1 110 011 0 1", "1 010 011 0 1", "0 {e[1]=1} 1 {f[2]=0} 0 {e[3]=1} 0 011 0 1")    // 1 1 0

    //------------------------

    case("1 010 100 1 1", "0 001 100 1 1", "0 {e[1]=0} 0 0 {e[2]=0} 0 {f[3]=1} 100 1 1")    // 0 0 1
    case("1 101 011 0 1", "1 110 011 0 1", "0 {e[1]=1} 1 1 {e[2]=1} 1 {f[3]=0} 011 0 1")    // 1 1 0
}

fun main(args: Array<String>) {
    for (casinoMoves in 0..("1 1111 1111").fromBinary()) {
        val casino = Casino(casinoMoves.toMoves())
        val bob = strategy.prepareBob(casino)
        val alice = strategy.prepareAlice()

        if (!runGame(alice, bob, casino)) {
            throw IllegalStateException("Algorithm doesn't work for $casino")
        }
    }
}

fun runGame(alice: Alice, bob: Bob, casino: Casino, logger: Logger = Logger()): Boolean {
    try {
        var wins = 0
        val errors = ArrayList<Error>()

        for (round in 1..NUMBER_OF_ROUNDS) {
            val a = alice.move(round)
            val b = bob.move(round)
            val c = casino.move(round)

            val roundError = error(round, errors.size() + 1, a, b, c)

            when {
                roundError != null -> errors.add(roundError)
                else -> wins++
            }

            alice.adjustStrategy(round, bob.getHistory(), casino.getHistory(), errors, roundError)

            when {
                wins >= EXPECTED_WINS -> break
                errors.size > EXPECTED_ERRORS -> {
                    logger.print(alice, bob, casino, errors, false)
                    return false
                }
            }
        }

        logger.print(alice, bob, casino, errors, true)
        return true
    } catch (e: Throwable) {
        throw IllegalStateException("Error during processing $casino.", e)
    }

}

val ALICE_DEFAULT = false

val NUMBER_OF_ROUNDS = 9
val EXPECTED_WINS = 6
val EXPECTED_ERRORS = NUMBER_OF_ROUNDS - EXPECTED_WINS

val STANDARD_ALICE_STRATEGY = "standard"
val STANDARD_BOB_STRATEGY = "standard"

abstract class Player {
    private val history = ArrayList<Boolean>()

    fun move(round: Int): Boolean {
        val move = makeMove(round)
        history.add(move)

        return move
    }

    fun getHistory() : Moves = Moves(history)

    protected abstract fun makeMove(round: Int): Boolean
}

open class PredefinedMovesPlayer(private val moves: Moves): Player() {
    {
        if (moves.size != NUMBER_OF_ROUNDS) throw IllegalArgumentException("All $NUMBER_OF_ROUNDS must be predefined")
    }

    override fun makeMove(round: Int): Boolean = moves[round]
    fun toString() = "${javaClass.getName()}: ${moves.toString()}"
}

class Casino(val moves: Moves): PredefinedMovesPlayer(moves)

class Bob(moves: Moves): PredefinedMovesPlayer(moves)

class Alice(strategies: List<AliceStrategy>): Player() {
    var actualStrategies = strategies

    var actualBobHistory = Moves(listOf())
    var actualCasinoHistory = Moves(listOf())
    var actualErrors: List<Error> = listOf()

    override fun makeMove(round: Int): Boolean {
        val movesVariants = actualStrategies.groupBy { strategy ->
            strategy.makeMove(round, getHistory(), actualBobHistory, actualCasinoHistory, actualErrors)
        }

        if (movesVariants.size() != 1) {
            throw IllegalStateException("Strategies suggest different steps at round $round:\n" +
                                        "  '${movesVariants[true]!!.first()}'\n  '${movesVariants[false]!!.first()}'")
        }

        return movesVariants.keySet().first()
    }

    fun adjustStrategy(round: Int, bobHistory: Moves, casinoHistory: Moves, errors: List<Error>, thisRoundError: Error?) {
        actualBobHistory = bobHistory
        actualCasinoHistory = casinoHistory
        actualErrors = errors

        val filterOutStrategies = actualStrategies.filter {
            strategy -> strategy.checkActual(round, getHistory(), actualBobHistory, actualCasinoHistory, actualErrors, thisRoundError)
        }

        if (filterOutStrategies.isEmpty()) {
            throw IllegalStateException("All strategies are filtered out after round $round. Previous:\n" +
                                        "${actualStrategies.makeString(separator = "\n")}")
        }

        actualStrategies = filterOutStrategies
    }
}

fun error(round: Int, errorNumber: Int, a: Boolean, b: Boolean, c: Boolean): Error? {
    fun isForceError(a: Boolean, b: Boolean, c: Boolean) = (a == c) && (a != b)
    fun isError(a: Boolean, b: Boolean, c: Boolean) = !(a == b && b == c)

    if (!isError(a, b, c)) return null

    return Error(
            errorNumber,
            isForceError(a, b, c),
            round,
            b)
}

class Error(val number: Int, val isForce: Boolean, val round: Int, val bobValue: Boolean)

trait AliceStrategy {
    fun checkActual(round: Int,
                    aliceHistory: Moves, bobHistory: Moves, casinoHistory: Moves,
                    errors: List<Error>, thisRoundError: Error?): Boolean
    fun makeMove(round: Int, aliceHistory: Moves, bobHistory: Moves, casinoHistory: Moves, errors: List<Error>): Boolean
}

class Strategy {
    class StandardAliceStrategy: AliceStrategy {
        var nextMove = ALICE_DEFAULT
        var currentValidTo = 1

        var fromLastAliceMistake: Boolean? = null

        override fun makeMove(round: Int,
                              aliceHistory: Moves, bobHistory: Moves, casinoHistory: Moves,
                              errors: List<Error>): Boolean {
            return getNextMove(round, errorFromRound(round - 1, errors))
        }

        override fun checkActual(round: Int,
                                 aliceHistory: Moves, bobHistory: Moves, casinoHistory: Moves,
                                 errors: List<Error>, thisRoundError: Error?): Boolean {
            // Force error or all three errors before round 5 invalidates standard strategy
            return !((errors.size() == EXPECTED_ERRORS && round < 5) || errors.any { it.isForce })
        }

        fun getNextMove(round: Int, previousError: Error?): Boolean {
            if (round == 1) return ALICE_DEFAULT

            if (previousError != null) {
                fromLastAliceMistake = previousError.bobValue
            }

            if (round > currentValidTo) {
                if (fromLastAliceMistake != null) {
                    // When Alice is mistaken Bob gives information how to take at least 2 rounds of three next rounds,
                    // so this information will be actual in this round and two rounds more.
                    // If there's not enough rounds left Bob will tell how to take one round
                    currentValidTo = if (NUMBER_OF_ROUNDS - round >= 2) round + 2 else round

                    nextMove = fromLastAliceMistake!!
                    fromLastAliceMistake = null
                } else {
                    nextMove = ALICE_DEFAULT
                    currentValidTo = round
                }
            }

            return nextMove
        }

        public fun toString(): String = STANDARD_ALICE_STRATEGY
    }

    class PatternAliceStrategy(val strategy: String): AliceStrategy {
        data class ErrorPattern(val pattern: String = "") {
            val isNoError = pattern.isEmpty()
            val isNumberOfErrorsAssertion = pattern.matches("""number_of_errors=\d""") // number_of_errors=1 or number_of_errors=3
            val isPossibleErrorAssertion = pattern.matches("""[e|f]\[\d\]\?""") // e[2]? or f[2]?
            val isExactErrorAssertion = pattern.matches("""[e|f]\[\d\]=[0|1]""") // e[1]=1 or f[2]=0

            val isForceError = when {
                isExactErrorAssertion -> pattern[0] == 'f'
                isPossibleErrorAssertion -> pattern[0] == 'f'
                else -> null
            }

            val expectedNumberOfErrors = if (isNumberOfErrorsAssertion) pattern["number_of_errors=".length].charToInt() else null

            val errorNumber: Int? = when {
                isNoError -> null
                isNumberOfErrorsAssertion -> null
                isPossibleErrorAssertion -> pattern[2].charToInt()
                isExactErrorAssertion -> pattern[2].charToInt()
                else -> throw IllegalArgumentException("Unknown error pattern: $pattern")
            }

            val expectedBobMove: Boolean? = when {
                isExactErrorAssertion -> pattern[5].fromBitChar()
                else -> null
            };

            fun matches(numberOfErrors: Int, error: Error?): Boolean {
                return when {
                    isNoError -> error == null
                    isNumberOfErrorsAssertion -> numberOfErrors == expectedNumberOfErrors
                    isPossibleErrorAssertion -> error == null || error.isForce == isForceError
                    isExactErrorAssertion ->
                        error != null &&
                        error.number == errorNumber &&
                        error.bobValue == expectedBobMove &&
                        error.isForce == isForceError
                    else -> throw IllegalStateException()
                }
            }
        }

        data class AliceMovePattern(val pattern: String) {
            val isSimpleMove = pattern == "0" || pattern == "1"
            val simpleMove = if (isSimpleMove) pattern[0].fromBitChar() else null

            val isFromErrorPattern = pattern.matches("""^e\[\d\]$""")
            val errorNumber = if (isFromErrorPattern) Integer.parseInt(pattern["e[".length].toString()) else null

            val isMakeTwoOfThree = pattern.startsWith("make_two_of_three[");
            val expectedForTwoOfThree = if (isMakeTwoOfThree) pattern["make_two_of_three[".length].fromBitChar() else null

            {
                if (!(isSimpleMove || isFromErrorPattern || isMakeTwoOfThree)) throw IllegalStateException("Unknown move pattern: $pattern")
            }

            fun move(casinoMoves: Moves, errors: List<Error>): Boolean {
                return when {
                    isSimpleMove -> simpleMove!!
                    isFromErrorPattern -> errors[errorNumber!! - 1].bobValue
                    isMakeTwoOfThree -> {
                        if (casinoMoves[5] == casinoMoves[6]) {
                            if (casinoMoves[5] != expectedForTwoOfThree!!) {
                                throw IllegalStateException("$expectedForTwoOfThree should be a hint how to win at least two round in 5, 6, 7")
                            }

                            !expectedForTwoOfThree
                        } else {
                            expectedForTwoOfThree!!
                        }
                    }
                    else -> throw IllegalStateException()
                }
            }
        }

        class object {
            val MOVE_PATTERN = Pattern.compile("""0|1|\(([^\(\)]*)\)""")
            val ERROR_ASSERT_PATTERN = Pattern.compile("""\{([^\}]*)\}""")

            val MOVE_OR_ASSERT_PATTERN = Pattern.compile("""(${MOVE_PATTERN.pattern()})|(${ERROR_ASSERT_PATTERN.pattern()})""")

            val MOVE_WITH_KNOWN_ERROR_PATTERN = Pattern.compile("""\((0|1)=>(\w\[[0|1|2]\]=[0|1])\)""")
            val MOVE_WITH_POSSIBLE_ERROR = Pattern.compile("""""")

            val NO_ERROR_PATTERN = ErrorPattern()
        }

        val moveDirectives: List<AliceMovePattern>
        val afterRoundAssert: Map<Int, Set<ErrorPattern>>

        {
            val directives = ArrayList<AliceMovePattern>()
            val errorAsserts = HashMap<Int, HashSet<ErrorPattern>>()

            val matcher = MOVE_OR_ASSERT_PATTERN.matcher(strategy.replaceAll(" ", ""))
            while (matcher.find()) {
                val moveOrDirectiveStr = matcher.group()
                when {
                    MOVE_PATTERN.matcher(moveOrDirectiveStr).matches() -> {
                        val simpleMove = matcher.group(1)
                        val patternMove = matcher.group(2)
                        directives.add(AliceMovePattern(if (patternMove != null) patternMove else simpleMove!!))
                    }
                    ERROR_ASSERT_PATTERN.matcher(moveOrDirectiveStr).matches() -> {
                        val forRound = directives.size
                        val errorAssertStr = matcher.group(4)!!
                        errorAsserts.getOrPut(forRound, { HashSet<ErrorPattern>() }).add(ErrorPattern(errorAssertStr))
                    }
                    else -> throw IllegalStateException("Can't match $moveOrDirectiveStr")
                }
            }

            if (directives.size != 9) {
                throw IllegalStateException("There should be 9 bits in strategy $strategy")
            }

            afterRoundAssert = errorAsserts
            moveDirectives = directives
        }

        override fun checkActual(round: Int,
                                 aliceHistory: Moves, bobHistory: Moves, casinoHistory: Moves,
                                 errors: List<Error>, thisRoundError: Error?): Boolean {
            return (afterRoundAssert[round] ?: setOf(NO_ERROR_PATTERN)).all { it.matches(errors.size(), thisRoundError) }
        }

        override fun makeMove(round: Int,
                              aliceHistory: Moves, bobHistory: Moves, casinoHistory: Moves,
                              errors: List<Error>): Boolean {
            return moveDirectives[round - 1].move(casinoHistory, errors)
        }

        public fun toString(): String = "Alice pattern strategy: $strategy"
    }

    class BobStrategyBuilder(casinoMatchRule: String, val bobStrategy: String) {
        private val pattern = Pattern.compile("^${casinoMatchRule.replaceAll(" ", "")}$")
        fun match(casinoMoves: String) = pattern.matcher(casinoMoves).matches()
        fun toString() = pattern.toString()
    }

    class BobStandardStrategy {
        fun generateMoves(casino: Casino): Moves {
            var aliceNextMove = ALICE_DEFAULT
            var validTillRound = 1

            var fromLastError: Boolean? = null

            val bobMoves = ArrayList<Boolean>()

            for (round in 1..NUMBER_OF_ROUNDS) {
                if (aliceNextMove != casino.moves[round]) {
                    // Alice is going to mistake in this round

                    val lastRoundWithInfo = Math.max(round, validTillRound)

                    if (NUMBER_OF_ROUNDS - lastRoundWithInfo >= 3) {
                        fromLastError = winTwoOfThree(casino.moves, lastRoundWithInfo + 1)
                    } else if (lastRoundWithInfo < NUMBER_OF_ROUNDS) {
                        fromLastError = casino.moves[lastRoundWithInfo + 1]
                    }

                    assert(fromLastError != null || lastRoundWithInfo == NUMBER_OF_ROUNDS)
                    bobMoves.add(if (fromLastError != null) fromLastError!! else ALICE_DEFAULT)
                } else {
                    // Alice move conforms with casino - Bob should do the same to win the round
                    bobMoves.add(aliceNextMove)
                }

                if (validTillRound <= round) {
                    if (fromLastError != null) {
                        aliceNextMove = fromLastError!!
                        validTillRound = if (NUMBER_OF_ROUNDS - round >= 3) round + 3 else round + 1
                        fromLastError = null
                    } else {
                        // There were no errors for additional information - Alice will return to default move
                        aliceNextMove = ALICE_DEFAULT
                        validTillRound = round + 1
                    }
                }
            }

            return Moves(bobMoves)
        }
    }

    val bobStrategies = ArrayList<BobStrategyBuilder>()
    val aliceStrategies = ArrayList<PatternAliceStrategy>()

    fun case(casinoMatch: String, bobStrategy: String, vararg aliceStrategyPatterns: String) {
        aliceStrategies.addAll(aliceStrategyPatterns.filter { it != STANDARD_ALICE_STRATEGY }.map { PatternAliceStrategy(it) })
        bobStrategies.add(BobStrategyBuilder(casinoMatch, bobStrategy))
    }

    fun prepareAlice() = Alice(aliceStrategies + StandardAliceStrategy())

    fun prepareBob(casino: Casino): Bob {
        val casinoMovesStr = casino.moves.toBinaryString()

        val directivePattern = Pattern.compile("""\{([^\}]*)\}""")

        val standardPattern = Pattern.compile("^standard$")
        val twoOfThreeHintPattern = Pattern.compile("""^two_of_three_hint\((\d)\)$""")
        val twoOfThreeWithHintInErrorPattern = Pattern.compile("""^two_of_three_with_hint_in_error\((\d),(0|1)\)$""")
        val warnAboutErrorPattern = Pattern.compile("""^warn_about_error\((\d)\)$""")
        val twoHintsInErrorsPattern = Pattern.compile("""^two_hints_in_errors\((\d),(0|1),(0|1)\)$""")

        val bobStrategyBuilder = bobStrategies.find { it.match(casinoMovesStr) }!!

        fun processDirective(directive: String): String {
            if (standardPattern.matcher(directive).matches()) {
                return "standard"
            }

            val twoOfThreeMatcher = twoOfThreeHintPattern.matcher(directive)
            if (twoOfThreeMatcher.matches()) {
                val startFrom = twoOfThreeMatcher.group(1)!!.toInt()
                return winTwoOfThree(casino.moves, startFrom).toBitString()
            }

            val twoOfThreeWithHintInErrorMatcher = twoOfThreeWithHintInErrorPattern.matcher(directive)
            if (twoOfThreeWithHintInErrorMatcher.matches()) {
                val roundPosition = twoOfThreeWithHintInErrorMatcher.group(1)!!.toInt()
                val hint = twoOfThreeWithHintInErrorMatcher.group(2)!!

                val expected = winTwoOfThree(casino.moves, roundPosition)
                val expectedStr = expected.toBitString()

                if (expected != casino.moves[roundPosition]) return "$hint$expectedStr$expectedStr"
                if (expected != casino.moves[roundPosition + 1]) return "$expectedStr$hint$expectedStr"
                if (expected != casino.moves[roundPosition + 2]) return "$expectedStr$expectedStr$hint"

                throw IllegalStateException("No error for '$directive' directive in $casinoMovesStr")
            }

            val warnAboutErrorMatcher = warnAboutErrorPattern.matcher(directive)
            if (warnAboutErrorMatcher.matches()) {
                val roundPosition = warnAboutErrorMatcher.group(1)!!.toInt()
                val expected = winTwoOfThree(casino.moves, roundPosition)

                val expectedStr = expected.toBitString()
                val warnStr = (!expected).toBitString()

                if (expected != casino.moves[roundPosition]) throw IllegalStateException(
                        "'$directive' can't be applied to first error round of three in ${casinoMovesStr}")

                if (expected != casino.moves[roundPosition + 1]) return "$warnStr$warnStr$expectedStr"
                if (expected != casino.moves[roundPosition + 2]) return "$expectedStr$warnStr$warnStr"

                throw IllegalStateException("No error for '$directive' directive in ${casinoMovesStr}")
            }

            val twoHintsInErrorsMatcher = twoHintsInErrorsPattern.matcher(directive)
            if (twoHintsInErrorsMatcher.matches()) {
                val roundPosition = twoHintsInErrorsMatcher.group(1)!!.toInt()
                val hint1 = twoHintsInErrorsMatcher.group(2)!!
                val hint2 = twoHintsInErrorsMatcher.group(3)!!

                val expected = !winTwoOfThree(casino.moves, roundPosition)
                val expectedStr = expected.toBitString()

                if (expected == casino.moves[roundPosition]) return "$expectedStr$hint1$hint2"
                if (expected == casino.moves[roundPosition + 1]) return "$hint1$expectedStr$hint2"
                if (expected == casino.moves[roundPosition + 2]) return "$hint1$hint2$expectedStr"

                throw IllegalStateException("There're must be two errors for '$directive' directive in ${casinoMovesStr}")
            }

            throw IllegalStateException("Can't match directive: $directive")
        }

        val bobStrategyStr = bobStrategyBuilder.bobStrategy
                .replaceAll(" ", "")
                .let { strategy ->
                    var modified = strategy
                    (1..9).forEach { round -> modified = modified.replaceAll("c$round", casino.moves[round].toBitString()) }
                    modified
                }
                .let { strategy ->
                    var modified = strategy

                    var matcher = directivePattern.matcher(modified)
                    while (matcher.find()) {
                        val directive = matcher.group(1)!!

                        modified = StringBuilder(modified).replace(matcher.start(), matcher.end(), processDirective(directive)).toString()

                        matcher = directivePattern.matcher(modified)
                    }

                    modified
                }

        val moves = if (bobStrategyStr == STANDARD_BOB_STRATEGY) BobStandardStrategy().generateMoves(casino) else bobStrategyStr.toMoves()

        return Bob(moves)
    }
}

fun generateStrategy(init: Strategy.() -> Unit): Strategy {
    val strategyBuilder = Strategy()
    strategyBuilder.init()
    return strategyBuilder
}

fun moreBooleans(values: Iterable<Boolean>): Boolean {
    var trueValues = 0
    var falseValues = 0
    values.forEach { value -> if (value) trueValues++ else falseValues++ }

    return trueValues > falseValues
}


class Moves(val moves: List<Boolean>): Iterable<Boolean> {
    val size = moves.size
    fun get(round: Int) = moves[round - 1]
    override fun iterator(): Iterator<Boolean> = moves.iterator()
    fun toString() = toBinaryString()
}

class Logger {
    fun print(alice: Player, bob: Player, casino: Casino, errors: List<Error>, result: Boolean) {
        println("-----")
        println("a: ${alice.getHistory().toBinaryString()}")
        println("b: ${bob.getHistory().toBinaryString()}")
        println("c: ${casino.moves.toBinaryString()}")

        if (errors.isNotEmpty()) {
            val errorsString = StringBuilder(" ".repeat(NUMBER_OF_ROUNDS))
            errors.forEach { error -> errorsString.replace(error.round - 1, error.round - 1, if (error.isForce) "!" else "^") }
            println("e: $errorsString")
        }

        println(if (result) "OK" else "FAIL")
    }
}

fun winTwoOfThree(moves: Moves, round: Int): Boolean = moreBooleans((0..2).map { delta -> moves[round + delta] })

fun errorFromRound(round: Int, errors: List<Error>): Error? = errors.find { it.round == round }

fun Boolean.toBitString() = if (this) "1" else "0"
fun Char.fromBitChar(): Boolean = when (this) {
    '0' -> false
    '1' -> true
    else -> throw IllegalArgumentException("Can't transform to boolean '$this' char")
}

fun Char.charToInt(): Int = Integer.parseInt(this.toString())

fun Int.toBinaryString(size: Int) = java.lang.String.format("%${size}s", Integer.toBinaryString(this)).replace(' ', '0')

fun String.toMoves(): Moves =
    Moves(this.map { ch ->
        when(ch) {
            '0' -> false
            '1' -> true
            else -> throw IllegalArgumentException("Moves must contain only '0' and '1' but not $ch")
        }
    }.toList())

fun Int.toMoves(): Moves = this.toBinaryString(9).toMoves()

fun Moves.toBinaryString() = this.map { if (it) '1' else '0' }.makeString(separator = "")

fun String.fromBinary(): Int = Integer.parseInt(this.replaceAll("\\s", ""), 2)
