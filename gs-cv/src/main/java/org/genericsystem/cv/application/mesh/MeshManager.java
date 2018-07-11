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

	private final Size frameSize;
	private final Size enlargedSize;

	private final int halfWidth;
	private final int halfHeight;
	private final Interpolator interpolator;
	private final double focale;

	private Points points;
	private Mesh mesh;
	private Mesh3D mesh3D;
	private Mesh reverseMesh;

	public MeshManager(int halfWidth, int halfHeight, Interpolator interpolatorFHT, Size frameSize, double focale) {
		this(halfWidth, halfHeight, interpolatorFHT, frameSize.width / (2 * halfWidth), frameSize.height / (2 * halfHeight), frameSize, focale);
	}

	public MeshManager(int halfWidth, int halfHeight, Interpolator interpolator, double deltaX, double deltaY, Size frameSize, double focale) {
		this.frameSize = frameSize;
		this.focale = focale;
		this.interpolator = interpolator;
		this.deltaX = deltaX;
		this.deltaY = deltaY;

		xBorder = 2 * (int) deltaX;
		yBorder = 2 * (int) deltaY;
		this.enlargedSize = new Size(frameSize.width + 2 * getxBorder(), frameSize.height + 2 * yBorder);
		this.halfHeight = halfHeight;
		this.halfWidth = halfWidth;
	}

	public Points getPoints() {
		return points != null ? points : (points = new CandidatePoints(new Point(enlargedSize.width / 2, enlargedSize.height / 2), halfWidth, halfHeight, deltaX, deltaY, getxBorder(), yBorder, interpolator));
	}

	private Mesh getMesh() {
		return mesh != null ? mesh : (mesh = new Mesh(getPoints(), halfWidth, halfHeight));
	}

	private Mesh3D getMesh3D() {
		return mesh3D != null ? mesh3D : (mesh3D = new Mesh3D(getMesh(), enlargedSize, focale));
	}

	public Mesh getReverseMesh() {
		return reverseMesh != null ? reverseMesh : (reverseMesh = getMesh3D().reverseMesh());
	}

	public Mat draw(Mat image, Scalar meshColor, Scalar ptsColor) {
		Mat enlarged = new Mat();
		Core.copyMakeBorder(image, enlarged, yBorder, yBorder, getxBorder(), getxBorder(), Core.BORDER_REPLICATE);
		getMesh().draw(enlarged, meshColor, ptsColor);
		getReverseMesh().draw(enlarged, new Scalar(0, 0, 255), new Scalar(255, 0, 0));
		return enlarged;
	}

	public Mat drawReverse(Mat image, Scalar meshColor, Scalar ptsColor) {
		Mat enlarged = buildEnlarged(image);
		getReverseMesh().draw(enlarged, meshColor, ptsColor);
		return enlarged;
	}

	public Mat buildEnlarged(Mat image) {
		Mat enlarged = new Mat();
		Core.copyMakeBorder(image, enlarged, yBorder, yBorder, getxBorder(), getxBorder(), Core.BORDER_REPLICATE);
		return enlarged;
	}

	public Mat dewarpReverse(Mat image) {
		return getReverseMesh().dewarp(buildEnlarged(image), image.size());
	}

	private Size getOriginalSize() {
		return frameSize;
	}

	public Mat draw3Dsurface(Scalar colorStart, Scalar colorEnd) {
		return getMesh3D().draw3Dsurface(colorStart, colorEnd, getOriginalSize());
	}

	public Mat dewarp(Mat image) {
		Mat enlarged = new Mat();
		Core.copyMakeBorder(image, enlarged, yBorder, yBorder, getxBorder(), getxBorder(), Core.BORDER_REPLICATE);
		return getMesh().dewarp(enlarged, getOriginalSize());
	}

	public Mat dewarp3D(Mat image) {
		Mat enlarged = new Mat();
		Core.copyMakeBorder(image, enlarged, yBorder, yBorder, getxBorder(), getxBorder(), Core.BORDER_REPLICATE);
		return getMesh3D().dewarp(enlarged, getOriginalSize());
	}

	public int getxBorder() {
		return xBorder;
	}

	public int getyBorder() {
		return yBorder;
	}

}
