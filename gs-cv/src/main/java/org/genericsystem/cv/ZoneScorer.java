package org.genericsystem.cv;

import java.util.stream.Stream;

public class ZoneScorer {

	private final Scores scores = new Scores();
	private final Zone zone;

	public ZoneScorer(Zone zone, Stream<Img> imgs) {
		this.zone = zone;
		imgs.forEach(img -> {
			String s = zone.ocr(img);
			System.out.println("------ " + s);
			scores.put(s);
		});
	}

	public Zone getZone() {
		return zone;
	}

	public double getBestScore() {
		return scores.getBestScore();
	}

	public String getBestText() {
		return scores.getBestText();
	}

	public String getBestText2() {
		return scores.getBestText2();
	}

}
