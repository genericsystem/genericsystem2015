package org.genericsystem.cv;

import java.util.stream.Stream;

public class RotatedZoneScorer {
	private final Scores scores = new Scores();
	private final RotatedZone zone;

	public RotatedZoneScorer(RotatedZone zone, Stream<Img> imgs) {
		this.zone = zone;
		imgs.forEach(img -> {
			String s = zone.ocr(img);
			s = s.replace("\n", "").replace(" ", "").trim();
			scores.put(s);
		});
	}

	public RotatedZone getZone() {
		return zone;
	}

	public double getBestScore() {
		return scores.getBestScore();
	}

	public String getBestText() {
		return scores.getBestText();
	}

}
