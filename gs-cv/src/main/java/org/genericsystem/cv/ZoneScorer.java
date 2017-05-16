package org.genericsystem.cv;

import java.util.List;

import org.opencv.core.Mat;

public class ZoneScorer {

	private final Zone zone;

	public ZoneScorer(Zone zone) {
		this.zone = zone;
	}

	public Zone getZone() {
		return zone;
	}

	public static class SupervisedZoneScorer extends ZoneScorer {

		public SupervisedZoneScorer(Zone zone) {
			super(zone);
		}

	}

	public static class UnsupervisedZoneScorer extends ZoneScorer {

		private final Scores scores = new Scores();

		public UnsupervisedZoneScorer(Zone zone, List<Mat> imgs) {
			super(zone);
			for (Mat img : imgs) {
				String s = Ocr.doWork(new Mat(img, getZone().getRect()).clone());
				s = s.replace("\n", "").replace(" ", "").trim();
				scores.put(s);
			}
		}

		public double getBestScore() {
			return scores.getBestScore();
		}

		public String getBestText() {
			return scores.getBestText();
		}
	}

}
