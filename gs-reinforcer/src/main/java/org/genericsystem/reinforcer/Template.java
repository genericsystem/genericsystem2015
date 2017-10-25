package org.genericsystem.reinforcer;

import java.util.HashSet;
import java.util.Set;

public class Template {

	protected final Set<Labels> contents = new HashSet<>();


	public double getMatchRate(Labels absoluteLabels) {
		return 0;
	}

	public boolean contains(Labels al) {
		return contents.contains(al);
	}

	public void reinforce(Labels absoluteLabels) {

	}

}
