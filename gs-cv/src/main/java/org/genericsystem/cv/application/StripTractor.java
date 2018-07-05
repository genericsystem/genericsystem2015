// package org.genericsystem.cv.application;
//
// import java.util.ArrayList;
// import java.util.Arrays;
// import java.util.List;
//
// import org.opencv.core.Mat;
//
// public class StripTractor {
// public final double derivative;
// public final double magnitude;
//
// public StripTractor(double derivative, double magnitude) {
// this.derivative = derivative;
// this.magnitude = magnitude;
// }
//
// double getInfluence(double derivative) {
// return magnitude * Math.pow(Math.atan(derivative) - Math.atan(this.derivative), 2);
// }
//
// public static StripTractor[] stripInfluences(List<TrajectStep> neigbourTraject, double step) {
// StripTractor[] result = noInfluences(neigbourTraject.size());
// for (int row = 0; row < neigbourTraject.size(); row++) {
// TrajectStep trajectStep = neigbourTraject.get(row);
// int newy = (int) Math.round(trajectStep.derivative * step + row);
// if (newy >= 0 && newy < neigbourTraject.size()) {
// // if (result[newy] != null)
// // throw new IllegalStateException("");
// result[newy] = new StripTractor(trajectStep.derivative, 1);
// }
// }
// return result;
// }
//
// public static StripTractor[] noInfluences(int size) {
// StripTractor[] result = new StripTractor[size];
// Arrays.fill(result, new StripTractor(0, 0));
// return result;
// }
//
// public static List<List<TrajectStep>> optimize(List<Mat> vHoughs, int blurSize, double anglePenality, double neigbourPenality, List<List<TrajectStep>> vHoughTrajs, double vStep, int optimizationsCount) {
// List<List<TrajectStep>> result = vHoughTrajs;
// for (int count = 0; count < optimizationsCount; count++) {
// List<List<TrajectStep>> vInfluencedTrajs = new ArrayList<>();
// for (int strip = 0; strip < result.size(); strip++) {
// StripTractor[] prevStripInfluences = strip != 0 ? StripTractor.stripInfluences(result.get(strip - 1), vStep) : noInfluences(result.get(strip).size());
// StripTractor[] nextStripInfluences = strip != result.size() - 1 ? StripTractor.stripInfluences(result.get(strip + 1), vStep) : noInfluences(result.get(strip).size());
// vInfluencedTrajs.add(FHT.bestInfluencedTrajectFHT(vHoughs.get(strip), blurSize, anglePenality, neigbourPenality, prevStripInfluences, nextStripInfluences));
// }
// result = vInfluencedTrajs;
// }
// return result;
// }
//
// }