package org.genericsystem.cv.application.mesh;

import org.genericsystem.cv.application.Interpolator;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MeshManager {

	private static final Logger logger = LoggerFactory.getLogger(MeshManager.class);

	private final double deltaX, deltaY;
	private final int xBorder;
	private final int yBorder;
	private final Mat image;

	private final int halfWidth;
	private final int halfHeight;
	private final Interpolator interpolator;
	private final double focale;

	private Points points;
	private Mesh mesh;
	private Mesh3D mesh3D;

	public MeshManager(int halfWidth, int halfHeight, Interpolator interpolatorFHT, Mat src, double focale) {
		this(halfWidth, halfHeight, interpolatorFHT, src.width() / (2 * halfWidth), src.height() / (2 * halfHeight), src, focale);
	}

	public MeshManager(int halfWidth, int halfHeight, Interpolator interpolator, double deltaX, double deltaY, Mat image, double focale) {
		this.focale = focale;
		this.interpolator = interpolator;
		this.deltaX = deltaX;
		this.deltaY = deltaY;
		this.image = new Mat();
		xBorder = 2 * (int) deltaX;
		yBorder = 2 * (int) deltaY;
		Core.copyMakeBorder(image, this.image, yBorder, yBorder, xBorder, xBorder, Core.BORDER_REPLICATE);
		this.halfHeight = halfHeight;
		this.halfWidth = halfWidth;
	}

	public Points getPoints() {
		return points != null ? points : (points = new Points(new Point(image.width() / 2, image.height() / 2), halfWidth, halfHeight, deltaX, deltaY, xBorder, yBorder, interpolator));
	}

	private Mesh getMesh() {
		return mesh != null ? mesh : (mesh = new Mesh(getPoints(), halfWidth, halfHeight));
	}

	private Mesh3D getMesh3D() {
		return mesh3D != null ? mesh3D : (mesh3D = new Mesh3D(getMesh(), image.size(), focale));
	}

	// private void reverseCoeffs(double[] coeffs) {
	// for (int i = 0; i < coeffs.length; i++)
	// coeffs[i] = 1 / coeffs[i];
	// }

	// public void recomputeGrid() {
	// double[] widths = mesh3D.getWidths();
	// reverseCoeffs(widths);
	// mesh3D.normalize(widths, getOriginalSize().width);
	//
	// double[] heights = mesh3D.getHeights();
	// reverseCoeffs(heights);
	// mesh3D.normalize(heights, getOriginalSize().height);
	// points = new Points(new Point(image.width() / 2, image.height() / 2), halfWidth, halfHeight, deltaX, deltaY, xBorder, yBorder, interpolator) {
	// @Override
	// double getHeightCoeff(double deltaY, int j) {
	// return heights[j];
	// }
	//
	// @Override
	// double getWidthCoeff(double deltaX, int j) {
	// return widths[j];
	// }
	// };
	// mesh = null;
	// mesh3D = null;
	// }

	// --------------------------------------------------

	public Mat drawOnCopy(Scalar meshColor, Scalar ptsColor) {
		Mat copy = image.clone();
		getMesh().draw(copy, meshColor, ptsColor);
		// getMesh().draw(image, meshColor, ptsColor);
		return copy;
	}

	private Size getOriginalSize() {
		return new Size(image.width() - 2 * xBorder, image.height() - 2 * yBorder);
	}

	public Mat draw3Dsurface(Scalar colorStart, Scalar colorEnd) {
		return getMesh3D().draw3Dsurface(colorStart, colorEnd, getOriginalSize());
	}

	public Mat dewarp() {
		return getMesh().dewarp(image, getOriginalSize());
	}

	public Mat dewarp3D() {
		return getMesh3D().dewarp(image, getOriginalSize());
	}

}
