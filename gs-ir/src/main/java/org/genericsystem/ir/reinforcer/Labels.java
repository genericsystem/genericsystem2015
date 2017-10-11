package org.genericsystem.ir.reinforcer;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Labels {

	private final Set<Label> labels = new HashSet<>();

	public boolean addLabel(double tlx, double tly, double brx, double bry, String candidateLabel) {
		Label candidate = new Label(tlx, tly, brx, bry, candidateLabel);
		for (Label label : labels)
			if (label.intersectWith(candidate))
				throw new IllegalStateException(label + " intersect with : " + candidate);
		return labels.add(candidate);
	}

	@Override
	public String toString() {
		return labels.toString();
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof Labels))
			return false;
		return labels.equals(((Labels) obj).labels);
	}

	@Override
	public int hashCode() {
		return labels.hashCode();
	}

	public List<Label> normalizeLabels() {
		double mintlx = Double.MAX_VALUE, mintly = Double.MAX_VALUE, maxbrx = 0, maxbry = 0;
		for (Label label : labels) {
			if (label.getTlx() < mintlx)
				mintlx = label.getTlx();
			if (label.getTly() < mintly)
				mintly = label.getTly();
			if (label.getBrx() > maxbrx)
				maxbrx = label.getBrx();
			if (label.getBry() > maxbry)
				maxbry = label.getBry();
		}
		double width = maxbrx - mintlx;
		double height = maxbry - mintly;
		List<Label> normalized = new ArrayList<>();
		for (Label label : labels)
			normalized.add(label.normalize(mintlx, mintly, width, height));
		return normalized;
	}
}
