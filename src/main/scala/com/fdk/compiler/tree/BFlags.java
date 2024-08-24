package com.fdk.compiler.tree;

class BFlags {

	public interface Modifiers {
		 int FINAL =                   0b0000000000000001;
		 int ABSTRACT =           0b0000000000000010;
		 int SEALED =                0b0000000000000100;
		 int IMPLICIT =               0b0000000000001000;
		 int LAZY =                    0b0000000000010000;
		 int OVERRIDE =             0b0000000000100000;
		 int PRIVATE =                0b0000000001000000;
		 int PROTECTED =          0b0000000010000000;
		 int ACCSSQUAL_ID =     0b0000000100000000;
		 int ACCSSQUAL_THIS =  0b0000001000000000;
	}
}