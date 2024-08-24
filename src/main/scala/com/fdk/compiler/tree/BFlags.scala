package com.fdk.compiler.tree;

object BFlags {

	object Modifier {
		val FINAL =                   0b0000000000000001;
		val ABSTRACT =           0b0000000000000010;
		val SEALED =                0b0000000000000100;
		val IMPLICIT =               0b0000000000001000;
		val LAZY =                    0b0000000000010000;
		val OVERRIDE =             0b0000000000100000;
		val PRIVATE =                0b0000000001000000;
		val PROTECTED =          0b0000000010000000;
		val ACCSSQUAL_ID =     0b0000000100000000;
		val ACCSSQUAL_THIS =  0b0000001000000000;
	}

	object Ident {
		val IDENTIFIER = 0b0000000000000001;
		val UNDERSCORE = 0b0000000000000010;
		val THIS = 0b0000000000000100;
		val SUPER = 0b0000000000001000;
		val CLASS_QLFR = 0b0000000000010000;
	}
}