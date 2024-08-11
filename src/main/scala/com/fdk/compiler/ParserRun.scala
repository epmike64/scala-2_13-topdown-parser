package com.fdk.compiler

import com.fdk.compiler.parser.{FParser, FTokenizer, Scanner}

object ParserRun extends App {

	val scanner = Scanner(FTokenizer("src/main/resources/parsingtest/Person.scala"))
	var parser = FParser(scanner)
	parser.compilationUnit()
}
