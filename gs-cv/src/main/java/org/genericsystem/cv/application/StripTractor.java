package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.opencv.core.Mat;

public class StripTractor {
	public final double derivative;
	public final double magnitude;

	public StripTractor(double derivative, double magnitude) {
		this.derivative = derivative;
		this.magnitude = magnitude;
	}

	double getInfluence(double derivative) {
		return magnitude * Math.pow(Math.atan(derivative) - Math.atan(this.derivative), 2);
	}

	public static StripTractor[] stripInfluences(List<TrajectStep> neigbourTraject, double step) {
		StripTractor[] result = noInfluences(neigbourTraject.size());
		for (int row = 0; row < neigbourTraject.size(); row++) {
			TrajectStep trajectStep = neigbourTraject.get(row);
			int newy = (int) Math.round(trajectStep.derivative * step + row);
			if (newy >= 0 && newy < neigbourTraject.size()) {
				// if (result[newy] != null)
				// throw new IllegalStateException("");
				result[newy] = new StripTractor(trajectStep.derivative, trajectStep.magnitude);
			}
		}
		return result;
	}

	public static StripTractor[] noInfluences(int size) {
		StripTractor[] result = new StripTractor[size];
		Arrays.fill(result, new StripTractor(0, 0));
		return result;
	}

	public static List<List<TrajectStep>> optimize(List<Mat> vHoughs, int blurSize, double anglePenality, double neigbourPenality, List<List<TrajectStep>> vHoughTrajs, double vStep) {
		List<List<TrajectStep>> vInfluencedTrajs = new ArrayList<>();
		for (int strip = 0; strip < vHoughTrajs.size(); strip++) {
			StripTractor[] prevStripInfluences = strip != 0 ? StripTractor.stripInfluences(vHoughTrajs.get(strip - 1), vStep) : noInfluences(vHoughTrajs.get(strip).size());
			StripTractor[] nextStripInfluences = strip != vHoughTrajs.size() - 1 ? StripTractor.stripInfluences(vHoughTrajs.get(strip + 1), vStep) : noInfluences(vHoughTrajs.get(strip).size());
			vInfluencedTrajs.add(FHT.bestInfluencedTrajectFHT(vHoughs.get(strip), 21, -0.08, -1000, prevStripInfluences, nextStripInfluences));
		}
		return vInfluencedTrajs;
	}

}