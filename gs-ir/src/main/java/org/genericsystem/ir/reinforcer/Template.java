package org.genericsystem.ir.reinforcer;

import java.util.HashSet;
import java.util.Set;

public class Template {

	protected final Set<AbsoluteLabels> absoluteLabels = new HashSet<>();

	public double getMatchRate(AbsoluteLabels absoluteLabels) {
		return 0;
	}

	public boolean contains(AbsoluteLabels al) {
		return absoluteLabels.contains(al);
	}

	public void reinforce(AbsoluteLabels absoluteLabels) {

	}

}
