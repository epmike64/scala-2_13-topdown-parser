package com.flint.tools.flintc.parser;

public class Animal {
	public void eat() {
		System.out.println("Animal is eating");
	}

	public static class Cat extends Animal {
	}

	public static void main(String[] args) {
		Animal a = new Cat();
		a.eat();
	}
}
