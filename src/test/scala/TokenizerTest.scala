import com.fdk.compiler.parser.FToken.FTokenKind
import com.fdk.compiler.parser.{FToken, FTokenizer}
import org.scalatest.funsuite.AnyFunSuite

class TokenizerTestSuite extends AnyFunSuite {
	test("tokenize") {
		val tokenizer = FTokenizer("src/main/resources/parsingtest/Person.scala")
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
