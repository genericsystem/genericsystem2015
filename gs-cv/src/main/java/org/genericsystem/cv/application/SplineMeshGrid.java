// package org.genericsystem.cv.application;
//
// import org.apache.commons.math3.analysis.FunctionUtils;
// import org.apache.commons.math3.analysis.differentiation.UnivariateDifferentiableFunction;
// import org.apache.commons.math3.analysis.function.Identity;
// import org.apache.commons.math3.analysis.function.Minus;
// import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction;
// import org.apache.commons.math3.analysis.solvers.BisectionSolver;
// import org.opencv.core.Mat;
// import org.opencv.core.Point;
// import org.slf4j.Logger;
// import org.slf4j.LoggerFactory;
//
// import java.util.List;
//
// public class SplineMeshGrid {
//
// private static final Logger logger = LoggerFactory.getLogger(MeshGridRadon.class);
//
// // Array of points: top left, top right, bottom right, bottom left.
// private final Mat image;
// private final int xStep;
// private final int yStep;
// // private final int nVerts;
// // private final int nLines;
//
// public SplineMeshGrid(int xStep, int yStep, Mat image) {
// this.image = image;
// this.xStep = xStep;
// this.yStep = yStep;
// // nLines = (image.height() - 1) / yStep + 1;
// // nVerts = (image.width() - 1) / xStep + 1;
// }
//
// // TODO: Possibility to configure all the parameters.
// public Point[][] build(List<PolynomialSplineFunction> vLines, List<PolynomialSplineFunction> hLines) {
// Point[][] intersections = new Point[hLines.size()][vLines.size()];
// System.out.println();
// for (int col = 0; col < vLines.size(); col++) {
// for (int row = 0; row < hLines.size(); row++) {
// PolynomialSplineFunction hLine = hLines.get(row);
// PolynomialSplineFunction vLine = vLines.get(col);
//
// UnivariateDifferentiableFunction f = FunctionUtils.add(FunctionUtils.compose(new UnivariateDifferentiableFunction[] { vLine, hLine }), FunctionUtils.compose(new UnivariateDifferentiableFunction[] { new Minus(), new Identity() }));
// try {
// double[] knots = hLine.getKnots();
// double x = new BisectionSolver().solve(100, f, knots[0], knots[knots.length - 1]);
// intersections[row][col] = new Point(x, hLine.value(x));
// System.out.println(col + " " + row + " " + intersections[row][col]);
// } catch (Exception ignore) {
// try {
// f = FunctionUtils.add(FunctionUtils.compose(new UnivariateDifferentiableFunction[] { hLine, vLine }), FunctionUtils.compose(new UnivariateDifferentiableFunction[] { new Minus(), new Identity() }));
// double[] knots = vLine.getKnots();
// double y = new BisectionSolver().solve(100, f, knots[0], knots[knots.length - 1]);
// intersections[row][col] = new Point(vLine.value(y), y);
// System.out.println(col + " " + row + " " + intersections[row][col]);
// } catch (Exception ignore2) {
// System.out.println(col + " " + row + " None ");
// }
// }
//
// }
// }
// return intersections;
// }
//
// // private int[][] toRectIndices() {
// // int[][] rects = new int[mesh.size()][4];
// // List<Point[]> meshPoints = new ArrayList<>(mesh.values());
// // for (int i = 0; i < rects.length; i++)
// // for (int j = 0; j < 4; j++)
// // rects[i][j] = points.indexOf(meshPoints.get(i)[j]);
// // return rects;
// // }
// //
// // private Map<Key, Point3[]> toPoint3d() {
// // if (mesh3D == null) {
// // points3D = Svd.solve(points, toRectIndices());
// // mesh3D = new HashMap<>();
// // for (Entry<Key, Point[]> entry : mesh.entrySet()) {
// // Point3[] para3D = new Point3[4];
// // for (int j = 0; j < 4; j++)
// // para3D[j] = points3D.get(points.indexOf(entry.getValue()[j]));
// // mesh3D.put(entry.getKey(), para3D);
// // }
// // }
// // return mesh3D;
// // }
// //
// // public Mat draw3Dsurface(Scalar colorStart, Scalar colorEnd) {
// // Map<Key, Point3[]> mesh3D = toPoint3d();
// // double xMin = Double.POSITIVE_INFINITY;
// // double yMin = Double.POSITIVE_INFINITY;
// // double zMin = Double.POSITIVE_INFINITY;
// // double xMax = Double.NEGATIVE_INFINITY;
// // double yMax = Double.NEGATIVE_INFINITY;
// // double zMax = Double.NEGATIVE_INFINITY;
// // for (Point3 p3 : points3D) {
// // if (p3.x > xMax)
// // xMax = p3.x;
// // if (p3.x < xMin)
// // xMin = p3.x;
// // if (p3.y > yMax)
// // yMax = p3.y;
// // if (p3.y < yMin)
// // yMin = p3.y;
// // if (p3.z > zMax)
// // zMax = p3.z;
// // if (p3.z < zMin)
// // zMin = p3.z;
// // }
// // int newWidth = image.width();
// // int newHeight = (int) Math.ceil((yMax - yMin) * image.width() / (xMax - xMin));
// // Mat result = new Mat(newHeight, newWidth, CvType.CV_16SC3, new Scalar(0, 0, 0));
// // // Useless variables because of Java’s lack of closures...
// // double xMin_ = xMin;
// // double xMax_ = xMax;
// // double yMin_ = yMin;
// // double yMax_ = yMax;
// // double zMin_ = zMin;
// // double zMax_ = zMax;
// // List<Point3> normalizedPoints = points3D.stream().map(p -> normalize(p, 0, result.width() - 1, 0, result.height() - 1, xMin_, xMax_, yMin_, yMax_)).collect(Collectors.toList());
// // Map<Key, Point3[]> normalizedMesh = normalize(mesh3D, normalizedPoints);
// // normalizedMesh.values().forEach(p -> {
// // Point[] p2 = new Point[] { new Point(p[0].x, p[0].y), new Point(p[1].x, p[1].y), new Point(p[2].x, p[2].y), new Point(p[3].x, p[3].y) };
// // double lambda = (p[0].z - zMin_) / (zMax_ - zMin_);
// // drawPolygon(result, p2, combine(colorStart, colorEnd, lambda));
// // });
// // return result;
// // }
// //
// // private Map<Key, Point3[]> normalize(Map<Key, Point3[]> mesh, List<Point3> newPoints) {
// // Map<Key, Point3[]> newMesh = new HashMap<>();
// // for (Entry<Key, Point3[]> entry : mesh.entrySet())
// // newMesh.put(entry.getKey(), exchangePoints(entry.getValue(), points3D, newPoints));
// // return newMesh;
// // }
// //
// // private Point3[] exchangePoints(Point3[] pts, List<Point3> oldPts, List<Point3> newPts) {
// // Point3[] result = new Point3[pts.length];
// // for (int i = 0; i < pts.length; i++)
// // result[i] = newPts.get(oldPts.indexOf(pts[i]));
// // return result;
// // }
// //
// // private Point3 normalize(Point3 p, double xMin, double xMax, double yMin, double yMax, double xMinOrig, double xMaxOrig, double yMinOrig, double yMaxOrig) {
// // return new Point3(normalize(p.x, xMin, xMax, xMinOrig, xMaxOrig), normalize(p.y, yMin, yMax, yMinOrig, yMaxOrig), p.z);
// // }
// //
// // private double normalize(double x, double xMin, double xMax, double xMinOrig, double xMaxOrig) {
// // return (xMax - xMin) * (x - xMinOrig) / (xMaxOrig - xMinOrig) + xMin;
// // }
// //
// // private Scalar combine(Scalar colorStart, Scalar colorEnd, double lambda) {
// // double[] c1 = colorStart.val;
// // double[] c2 = colorEnd.val;
// // double[] c = new double[c1.length];
// // for (int i = 0; i < c.length; i++)
// // c[i] = (1 - lambda) * c1[i] + lambda * c2[i];
// // return new Scalar(c);
// // }
// //
// // @Override
// // public Mat dewarp() {
// // Map<Key, Point3[]> mesh3D = toPoint3d();
// //
// // // Average width of the 3D edges for each column.
// // double[] widths = new double[nVerts - 1];
// // for (int j = 0; j < widths.length; j++) {
// // double sum = 0;
// // for (int i = 0; i < nLines - 1; i++) {
// // Point3[] para = mesh3D.get(new Key(i, j));
// // sum += euclideanDistance(para[0], para[1]);
// // }
// // // Last line, bottom edge.
// // Point3[] para = mesh3D.get(new Key(nLines - 2, j));
// // sum += euclideanDistance(para[2], para[3]);
// // widths[j] = sum / nLines;
// // }
// //
// // // Average height of the 3D edges for each line.
// // double[] heights = new double[nLines - 1];
// // for (int i = 0; i < heights.length; i++) {
// // double sum = 0;
// // for (int j = 0; j < nVerts - 1; j++) {
// // Point3[] para = mesh3D.get(new Key(i, j));
// // sum += euclideanDistance(para[0], para[3]);
// // }
// // // Last column, right edge.
// // Point3[] para = mesh3D.get(new Key(i, nVerts - 2));
// // sum += euclideanDistance(para[1], para[2]);
// // heights[i] = sum / nVerts;
// // }
// //
// // // Normalize the heights and widths so the dewarped image has the same height as the original image.
// // double totalHeight = sum(heights, heights.length);
// // for (int i = 0; i < heights.length; i++)
// // heights[i] *= image.height() / totalHeight;
// // for (int i = 0; i < widths.length; i++)
// // widths[i] *= image.width() / totalHeight;
// //
// // Mat dewarpedImage = new Mat(image.height(), (int) Math.round(sum(widths, widths.length)) + 1, CvType.CV_8UC3, new Scalar(255, 255, 255));
// //
// // for (int i = 0, y = 0; i < nLines - 1; y += heights[i], i++) {
// // for (int j = 0, x = 0; j < nVerts - 1; x += widths[j], j++) {
// // if (inImageBorders(mesh.get(new Key(i, j)))) {
// // Rect subImageRect = subImageRect(i, j);
// // if (!subImageRect.empty()) {
// // Mat homography = dewarpPolygon(mesh.get(new Key(i, j)), subImageRect, heights[i], widths[j]);
// // Rect dewarpedRect = new Rect(new Point(x, y), new Point(x + widths[j], y + heights[i]));
// // Mat subDewarpedImage = new Mat(dewarpedImage, dewarpedRect);
// // Mat subImage = new Mat(image, subImageRect);
// // Imgproc.warpPerspective(subImage, subDewarpedImage, homography, subDewarpedImage.size(), Imgproc.INTER_LINEAR, Core.BORDER_REPLICATE, Scalar.all(0));
// // subImage.release();
// // subDewarpedImage.release();
// // homography.release();
// // }
// // }
// // }
// // }
// // // Draw lines and columns on dewarped image
// // int y = 0;
// // for (int i = 0; i < nLines - 1; y += heights[i], i++)
// // Imgproc.line(dewarpedImage, new Point(0, y), new Point(dewarpedImage.width() - 1, y), new Scalar(255, 0, 255), 1);
// // // Last horizontal line.
// // Imgproc.line(dewarpedImage, new Point(0, y), new Point(dewarpedImage.width() - 1, y), new Scalar(255, 0, 255), 1);
// // int x = 0;
// // for (int j = 0; j < nVerts - 1; x += widths[j], j++)
// // Imgproc.line(dewarpedImage, new Point(x, 0), new Point(x, dewarpedImage.height() - 1), new Scalar(255, 0, 255), 1);
// // // Last vertical line.
// // Imgproc.line(dewarpedImage, new Point(x, 0), new Point(x, dewarpedImage.height() - 1), new Scalar(255, 0, 255), 1);
// // return dewarpedImage;
// // }
// //
// // private double sum(double[] array, int end) {
// // double sum = 0;
// // for (int i = 0; i < end; i++)
// // sum += array[i];
// // return sum;
// // }
// //
// // // Returns true if:
// // // – no corner of the polygon lies outside the image.
// // // – at least a corner of the polygon is in the image proper,
// // // that is, not outside and not in one of the borders whose size is defined by xBorder and yBorder.
// // protected boolean inImageBorders(Point[] p) {
// // for (Point pt : p)
// // if (!inImage(pt))
// // return false;
// // for (Point pt : p)
// // if (inImageBorders(pt))
// // return true;
// // return false;
// // }
// //
// // private boolean inImage(Point p) {
// // return p.x >= 0 && p.x < image.width() && p.y >= 0 && p.y < image.height();
// // }
// //
// // private boolean inImageBorders(Point p) {
// // return p.x >= xBorder && p.x < image.width() - xBorder && p.y >= yBorder && p.y < image.height() - yBorder;
// // }
// //
// // private double euclideanDistance(Point3 p1, Point3 p2) {
// // return Math.sqrt((p2.x - p1.x) * (p2.x - p1.x) + (p2.y - p1.y) * (p2.y - p1.y) + (p2.z - p1.z) * (p2.z - p1.z));
// // }
// }
