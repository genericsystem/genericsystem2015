// package org.genericsystem.cv.application;
//
// import java.util.List;
//
// public class SuperContourInterpolator implements Interpolator {
//
// private final double pow;
// private List<SuperContour> superContours;
//
// private double hCoef; // coefficient pour l'angle horizontal
// private double vCoef; // coefficient pour l'angle vertical
// private double sumHCoefs; // somme des coefficients pour l'angle horizontal
// private double sumVCoefs;
// private double hAngle; // angle horizontal
// private double vAngle; // angle vertical
//
// public SuperContourInterpolator(List<SuperContour> superContours, double pow) {
// this.superContours = superContours;
// this.pow = pow;
// }
//
// private double squaredEuclidianDistance(double x, double y, SuperContour sc) { // distance euclidienne au carré
// double result = Math.pow(x - sc.center.x, 2) + Math.pow(y - sc.center.y, 2);
// double minDist = 10;
// return result >= Math.pow(minDist, 2) ? result : Math.pow(minDist, 2);
// }
//
// @Override
// public double interpolateHorizontals(double x, double y) {
// double sumHCoefs = 0; // somme des coefficients pour l'angle horizontal
// double hAngle = 0; // angle horizontal
// for (SuperContour op : superContours) {
// double geoCoef = Math.pow(1 / (squaredEuclidianDistance(x, y, op)), pow / 2);
// hCoef = geoCoef * op.dx;
// hAngle += hCoef * op.angle;
// sumHCoefs += hCoef;
// }
// return hAngle / sumHCoefs;
// }
//
// @Override
// public double interpolateVerticals(double x, double y) {
// double vAngle = 0;
// double sumVCoefs = 0;
// for (SuperContour op : superContours) {
// double geoCoef = Math.pow(1 / (squaredEuclidianDistance(x, y, op)), pow / 2);
// vCoef = geoCoef * op.dx;
// vAngle += vCoef * op.vertical;
// sumVCoefs += vCoef;
// }
// return vAngle / sumVCoefs;
// }
//
// }