package org.genericsystem.cv.application.mesh;

import org.genericsystem.cv.application.GeneralInterpolator;
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

	private Interpolator interpolator;
	private Points points;
	private Mesh mesh;
	private Mesh3D mesh3D;

	public MeshManager(int halfWidth, int halfHeight, GeneralInterpolator interpolatorFHT, Mat src) {
		this(halfWidth, halfHeight, interpolatorFHT, src.width() / 12, src.height() / 8, src);
	}

	public MeshManager(int halfWidth, int halfHeight, Interpolator interpolator, double deltaX, double deltaY, Mat image) {
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
		return mesh3D != null ? mesh3D : (mesh3D = new Mesh3D(getMesh()));
	}

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
