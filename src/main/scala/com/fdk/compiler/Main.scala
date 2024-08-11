package com.fdk.compiler

import com.fdk.compiler.parser.FToken.FTokenKind
import com.fdk.compiler.parser.{FToken, FTokenizer}

object Main extends App {
	io.Source.fromFile("src/main/resources/parsingtest/Animal.java").getLines().foreach(println)
	val tokenizer = FTokenizer("src/main/resources/parsingtest/Animal.java")
	var token: FToken = tokenizer.readToken()
	while(token != null && token.kind != FTokenKind.EOF) {
		println(token)
		token = tokenizer.readToken()
	}
	println("Exiting...")
}