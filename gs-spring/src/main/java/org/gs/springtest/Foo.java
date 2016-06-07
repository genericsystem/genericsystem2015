package org.gs.springtest;

import org.springframework.stereotype.Component;

@Component
public class Foo {
	String test = "SPRING ";

	public Foo() {

	}

	public String getTest() {
		return test;
	}

	public void setTest(String test) {
		this.test = test;
	}

	public String run(final String input) {
		return input + " from Foo";
	}

}