package org.genericsystem.cv.docPattern;

import java.util.List;

import org.genericsystem.common.Generic;

public class DocPattern {

	private Generic docPattern;

	public DocPattern(String patternName, List<ZoneStream> zoneStreams) {
		this.init(patternName, zoneStreams);
	}

	public void init(String patternName, List<ZoneStream> zoneStreams) {
		// Engine engine = new Engine();
		// this.docPattern = engine.addInstance(patternName);
		// Generic fields = this.docPattern.addAttribute("fields");
		for (ZoneStream zs : zoneStreams) {
			if (zs.getType() == ZoneStream.typeAttribute) {
				// System.out.println("ATTRIBUTE : " + zs.getAuthorizedValues().get(0));
				// Generic field = fields.addAttribute(zs.getAuthorizedValues().get(0));
				// double distance = Double.MAX_VALUE;
				// for (ZoneStream zs2 : zoneStreams) {
				// if (zs.isAbove(zs2) || zs.isLeft(zs2)) {
				// double dist = zs.distance(zs2);
				// if (dist < distance) {
				// distance = dist;
				// // zs.setHolder(zs2);
				// }
				// }
				// }
			} // else
				// System.out.println(zs.getAuthorizedValues());
		}
	}

}
