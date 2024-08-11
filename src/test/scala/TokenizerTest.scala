import com.fdk.compiler.parser.FToken.FTokenKind
import com.fdk.compiler.parser.{FToken, FTokenizer}
import org.scalatest.funsuite.AnyFunSuite

class TokenizerTestSuite extends AnyFunSuite {
	test("tokenize") {
		println("-- Source Text --")
		io.Source.fromFile("src/main/resources/parsingtest/Animal.java").getLines().foreach(println)
		println("-- Tokenizing...")
		val tokenizer = FTokenizer("src/main/resources/parsingtest/Animal.java")
		var token: FToken = tokenizer.readToken()
		var i = 1
		while (token != null && token.kind != FTokenKind.EOF) {
			println(s"$i ${token}")
			i += 1
			token = tokenizer.readToken()
		}
		println("-- Done...")
	}
}
