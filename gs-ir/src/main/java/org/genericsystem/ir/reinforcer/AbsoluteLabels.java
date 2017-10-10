package org.genericsystem.ir.reinforcer;

import java.util.HashSet;
import java.util.Set;

public class AbsoluteLabels {

	private final Set<AbsoluteLabel> absoluteLabels = new HashSet<>();

	public boolean addAbsoluteLabel(double x1, double y1, double x2, double y2, String candidateLabel) {
		AbsoluteLabel candidate = new AbsoluteLabel(x1, y1, x2, y2, candidateLabel);
		for (AbsoluteLabel label : absoluteLabels)
			if (label.intersectWith(candidate))
				throw new IllegalStateException(label + " intersect with : " + candidate);
		return absoluteLabels.add(candidate);
	}

	@Override
	public String toString() {
		return absoluteLabels.toString();
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof AbsoluteLabels))
			return false;
		return absoluteLabels.equals(((AbsoluteLabels) obj).absoluteLabels);
	}

	@Override
	public int hashCode() {
		return absoluteLabels.hashCode();
	}

}
