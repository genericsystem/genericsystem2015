package org.genericsystem.cv.application;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.opencv.core.Point;

public class ProjectionLines {
	public static List<List<OrientedPoint>[]> toHorizontalsOrientedPoints(List<List<TrajectStep>> trajectSteps, double vStep, double localTheshold, double globalTheshold) {
		List<List<OrientedPoint>[]> fhtHorizontals = new ArrayList<>();
		for (int vStripIndex = 0; vStripIndex < trajectSteps.size(); vStripIndex++)
			fhtHorizontals.add(toHorizontalOrientedPoints(trajectSteps.get(vStripIndex), vStep * vStripIndex, localTheshold, globalTheshold));
		return fhtHorizontals;
	}

	public static List<OrientedPoint>[] toHorizontalOrientedPoints(List<TrajectStep> trajectSteps, double x, double localTheshold, double globalTheshold) {
		List<OrientedPoint> topPoints = new ArrayList<>();
		List<OrientedPoint> bottomPoints = new ArrayList<>();
		List<TrajectStep[]> lines = getStripLinesFHT(trajectSteps, localTheshold, globalTheshold);
		lines.stream().forEach(trajectStep -> {
			topPoints.add(new OrientedPoint(new Point(x, trajectStep[0].y), Math.atan(trajectStep[0].derivative), trajectStep[0].magnitude, trajectStep[0].derivative));
			bottomPoints.add(new OrientedPoint(new Point(x, trajectStep[1].y), Math.atan(trajectStep[1].derivative), trajectStep[1].magnitude, trajectStep[1].derivative));
		});
		return new List[] { topPoints, bottomPoints };
	}

	public static List<List<OrientedPoint>[]> toVerticalsOrientedPoints(List<List<TrajectStep>> trajectSteps, double hStep, double localTheshold, double globalTheshold) {
		List<List<OrientedPoint>[]> fhtHorizontals = new ArrayList<>();
		for (int vStripIndex = 0; vStripIndex < trajectSteps.size(); vStripIndex++)
			fhtHorizontals.add(toVerticalOrientedPoints(trajectSteps.get(vStripIndex), hStep * vStripIndex, localTheshold, globalTheshold));
		return fhtHorizontals;
	}

	public static List<OrientedPoint>[] toVerticalOrientedPoints(List<TrajectStep> trajectSteps, double y, double localTheshold, double globalTheshold) {
		List<OrientedPoint> leftPoints = new ArrayList<>();
		List<OrientedPoint> rightPoints = new ArrayList<>();

		List<TrajectStep[]> lines = getStripLinesFHT(trajectSteps, localTheshold, globalTheshold);

		lines.stream().forEach(trajectStep -> {
			leftPoints.add(new OrientedPoint(new Point(trajectStep[0].y, y), -Math.atan(trajectStep[0].derivative), trajectStep[0].magnitude, trajectStep[0].derivative));
			rightPoints.add(new OrientedPoint(new Point(trajectStep[1].y, y), -Math.atan(trajectStep[1].derivative), trajectStep[1].magnitude, trajectStep[1].derivative));
		});
		return new List[] { leftPoints, rightPoints };
	}

	public static List<TrajectStep[]> getStripLinesFHT(List<TrajectStep> magnitudes, double localTheshold, double globalTheshold) {
		Set<TrajectStep> alreadyComputed = new HashSet<>();
		double max = magnitudes.stream().mapToDouble(ts -> ts.magnitude).max().getAsDouble();
		// System.out.println("Max : " + max);
		List<TrajectStep[]> result = new ArrayList<>();
		for (TrajectStep trajectStep : magnitudes.stream().sorted().collect(Collectors.toList())) {
			if (trajectStep.magnitude < globalTheshold * max)
				break;
			if (!alreadyComputed.contains(trajectStep)) {
				double tAlpha = localTheshold * trajectStep.magnitude;

				int y1 = trajectStep.y;
				while (y1 >= 0 && magnitudes.get(y1).magnitude >= tAlpha)
					y1--;
				if (y1 != 0)
					y1++;

				int y2 = trajectStep.y;
				while (y2 < magnitudes.size() && magnitudes.get(y2).magnitude >= tAlpha)
					y2++;
				if (y2 != magnitudes.size() - 1)
					y2--;

				boolean alreadyVisited = false;
				for (int y = y1; y <= y2; y++)
					if (alreadyComputed.contains(magnitudes.get(y))) {
						alreadyVisited = true;
						break;
					}
				for (int y = y1; y <= y2; y++)
					alreadyComputed.add(magnitudes.get(y));
				if (!alreadyVisited)
					result.add(new TrajectStep[] { magnitudes.get(y1), magnitudes.get(y2) });
			}
		}
		return result;
	}

}
