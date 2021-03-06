package org.genericsystem.cv.application.fht;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction;
import org.genericsystem.cv.application.GeneralInterpolator;
import org.genericsystem.cv.application.OrientedPoint;
import org.genericsystem.cv.application.ProjectionLines;
import org.genericsystem.cv.application.Segment;
import org.genericsystem.cv.application.SplineInterpolator;
import org.genericsystem.cv.application.TrajectStep;
import org.genericsystem.cv.application.mesh.MeshManager;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.Size;

import javafx.beans.binding.Bindings;
import javafx.beans.binding.DoubleBinding;
import javafx.beans.property.DoubleProperty;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.property.SimpleIntegerProperty;

public class FHTManager {

	private IntegerProperty vBlurSize = new SimpleIntegerProperty(81);
	private IntegerProperty hBlurSize = new SimpleIntegerProperty(81);
	// private DoubleProperty vNeighbourPenality = new SimpleDoubleProperty(-100);
	// private DoubleProperty hNeighbourPenality = new SimpleDoubleProperty(-100);
	private DoubleProperty vAnglePenality = new SimpleDoubleProperty(-0.12);
	private DoubleProperty hAnglePenality = new SimpleDoubleProperty(-0.12);
	private DoubleProperty vRecover = new SimpleDoubleProperty(0.6);
	private DoubleProperty hRecover = new SimpleDoubleProperty(0.6);
	private IntegerProperty vStripsNumber = new SimpleIntegerProperty(16);
	private IntegerProperty hStripsNumber = new SimpleIntegerProperty(10);
	private DoubleBinding stripWidth;
	private DoubleBinding stripHeight;
	private DoubleBinding vStep;
	private DoubleBinding hStep;
	private DoubleProperty vLocalThreshold = new SimpleDoubleProperty(0.5);
	private DoubleProperty vGlobalThreshold = new SimpleDoubleProperty(0.05);
	private DoubleProperty hLocalThreshold = new SimpleDoubleProperty(0.5);
	private DoubleProperty hGlobalThreshold = new SimpleDoubleProperty(0.05);

	private DoubleProperty hMaxConnectDistance = new SimpleDoubleProperty(0.05);
	private DoubleProperty vMaxConnectDistance = new SimpleDoubleProperty(0.05);
	private DoubleProperty interpolatorPow = new SimpleDoubleProperty(4);
	private DoubleProperty interpolatorMinDist = new SimpleDoubleProperty(0.0001);
	private IntegerProperty halfGridWidth = new SimpleIntegerProperty(8);
	private IntegerProperty halfGridHeight = new SimpleIntegerProperty(8);
	private DoubleProperty focale;

	private final Size binarySize;
	// private Mat frame;
	private Mat binarized;
	private Mat transposedBinarized;

	public FHTManager(Size binarySize) {
		this.binarySize = binarySize;
		stripWidth = Bindings.createDoubleBinding(() -> (binarySize.width / (vStripsNumber.get() * (1 - vRecover.get()) + vRecover.get() - 1)), vStripsNumber, vRecover);
		vStep = Bindings.createDoubleBinding(() -> ((1 - vRecover.get()) * stripWidth.get()), vRecover, stripWidth);
		stripHeight = Bindings.createDoubleBinding(() -> (binarySize.height / (hStripsNumber.get() * (1 - hRecover.get()) + hRecover.get() - 1)), hStripsNumber, hRecover);
		hStep = Bindings.createDoubleBinding(() -> ((1 - hRecover.get()) * stripHeight.get()), hRecover, stripHeight);
		focale = new SimpleDoubleProperty(Math.max(binarySize.width, binarySize.height) / Math.tan((60d / 180) * Math.PI / 2) / 2);
	}

	public boolean isInitialized() {
		return binarized != null;
	}

	public FHTManager init(Mat binarized) {
		this.binarized = binarized;
		assert binarized.size().equals(binarySize);
		transposedBinarized = new Mat();
		Core.transpose(binarized, transposedBinarized);
		vStrips = null;
		hStrips = null;

		vHoughs = null;
		hHoughs = null;

		vHoughTrajs = null;
		hHoughTrajs = null;

		fhtHorizontals = null;
		fhtVerticals = null;

		horizontalSegments = null;
		verticalSegments = null;
		hSplines = null;
		vSplines = null;

		flatHorizontalSegments = null;
		flatVerticalSegments = null;

		interpolatorFHT = null;
		superInterpolator = null;
		meshManager = null;
		return this;
	}

	private List<Mat> vStrips;
	private List<Mat> hStrips;

	private List<Mat> vHoughs;
	private List<Mat> hHoughs;

	private List<List<TrajectStep>> vHoughTrajs;
	private List<List<TrajectStep>> hHoughTrajs;

	private List<List<OrientedPoint>[]> fhtHorizontals;
	private List<List<OrientedPoint>[]> fhtVerticals;

	private List<List<Segment>>[] horizontalSegments;
	private List<List<Segment>>[] verticalSegments;

	private List<PolynomialSplineFunction>[] hSplines;
	private List<PolynomialSplineFunction>[] vSplines;

	private List<OrientedPoint> flatHorizontalSegments;
	private List<OrientedPoint> flatVerticalSegments;

	private GeneralInterpolator interpolatorFHT;
	private SplineInterpolator superInterpolator;
	private MeshManager meshManager;

	public List<Mat> getVStrips() {
		return vStrips != null ? vStrips : (vStrips = FHT.extractStrips(binarized, vStripsNumber.get(), stripWidth.get(), vStep.get()));
	}

	public List<Mat> getHStrips() {
		return hStrips != null ? hStrips : (hStrips = FHT.extractStrips(transposedBinarized, hStripsNumber.get(), stripHeight.get(), hStep.get()));
	}

	public List<Mat> getvHoughs() {
		return vHoughs != null ? vHoughs : (vHoughs = getVStrips().stream().map(strip -> FHT.fastHoughTransform(strip)).collect(Collectors.toList()));
	}

	public List<Mat> gethHoughs() {
		return hHoughs != null ? hHoughs : (hHoughs = getHStrips().stream().map(strip -> FHT.fastHoughTransform(strip)).collect(Collectors.toList()));
	}

	public List<List<TrajectStep>> getvHoughTrajs() {
		return vHoughTrajs != null ? vHoughTrajs : (vHoughTrajs = getvHoughs().stream().map(projectionMap -> FHT.bestTrajectFHT(projectionMap, vBlurSize.getValue(), vAnglePenality.getValue())).collect(Collectors.toList()));
	}

	public List<List<TrajectStep>> gethHoughTrajs() {
		return hHoughTrajs != null ? hHoughTrajs : (hHoughTrajs = gethHoughs().stream().map(projectionMap -> FHT.bestTrajectFHT(projectionMap, hBlurSize.getValue(), hAnglePenality.getValue())).collect(Collectors.toList()));
	}

	// public List<List<TrajectStep>> getOptimizedvHoughTrajs() {
	// return optimizedVHoughTrajs != null ? optimizedVHoughTrajs
	// : (optimizedVHoughTrajs = StripTractor.optimize(getvHoughs(), vBlurSize.getValue(), vAnglePenality.getValue(), vNeighbourPenality.get(), getvHoughTrajs(), vStep.get(), optimizationsCount.get()));
	// }
	//
	// public List<List<TrajectStep>> getOptimizedhHoughTrajs() {
	// return optimizedHHoughTrajs != null ? optimizedHHoughTrajs
	// : (optimizedHHoughTrajs = StripTractor.optimize(gethHoughs(), hBlurSize.getValue(), hAnglePenality.getValue(), hNeighbourPenality.get(), gethHoughTrajs(), hStep.get(), optimizationsCount.get()));
	// }

	public List<List<OrientedPoint>[]> getFhtHorizontals() {
		return fhtHorizontals != null ? fhtHorizontals : (fhtHorizontals = ProjectionLines.toHorizontalsOrientedPoints(getvHoughTrajs(), vStep.get(), vLocalThreshold.get(), vGlobalThreshold.get()));
	}

	public List<List<OrientedPoint>[]> getFhtVerticals() {
		return fhtVerticals != null ? fhtVerticals : (fhtVerticals = ProjectionLines.toVerticalsOrientedPoints(gethHoughTrajs(), hStep.get(), hLocalThreshold.get(), hGlobalThreshold.get()));
	}

	public List<List<Segment>>[] getHorizontalSegments() {
		return horizontalSegments != null ? horizontalSegments : (horizontalSegments = Segment.connect(getFhtHorizontals(), vStep.get(), hMaxConnectDistance.get(), false));
	}

	public List<List<Segment>>[] getVerticalSegments() {
		return verticalSegments != null ? verticalSegments : (verticalSegments = Segment.connect(getFhtVerticals(), hStep.get(), vMaxConnectDistance.get(), true));
	}

	public List<PolynomialSplineFunction>[] gethSplines() {
		return hSplines != null ? hSplines : (hSplines = Segment.toSplines(getHorizontalSegments(), false));
	}

	public List<PolynomialSplineFunction>[] getvSplines() {
		return vSplines != null ? vSplines : (vSplines = Segment.toSplines(getVerticalSegments(), true));
	}

	public List<OrientedPoint> getFlatHorizontalSegments() {
		return flatHorizontalSegments != null ? flatHorizontalSegments
				: (flatHorizontalSegments = Stream.of(getHorizontalSegments()).flatMap(h -> h.stream()).flatMap(h -> h.stream()).flatMap(edge -> Stream.of(edge.op1, edge.op2)).collect(Collectors.toList()));
	}

	public List<OrientedPoint> getFlatVerticalSegments() {
		return flatVerticalSegments != null ? flatVerticalSegments : (flatVerticalSegments = Stream.of(getVerticalSegments()).flatMap(h -> h.stream()).flatMap(h -> h.stream()).flatMap(edge -> Stream.of(edge.op1, edge.op2)).collect(Collectors.toList()));
	}

	public GeneralInterpolator getInterpolatorFHT() {
		return interpolatorFHT != null ? interpolatorFHT : (interpolatorFHT = new GeneralInterpolator(getFlatHorizontalSegments(), getFlatVerticalSegments(), interpolatorPow.get(), interpolatorMinDist.get()));
	}

	public SplineInterpolator getSuperInterpolator() {
		return superInterpolator != null ? superInterpolator : (superInterpolator = new SplineInterpolator(getInterpolatorFHT(), gethSplines(), getvSplines()));
	}

	public MeshManager getMeshManager() {
		return meshManager != null ? meshManager : (meshManager = new MeshManager(halfGridWidth.get(), halfGridHeight.get(), getSuperInterpolator(), binarySize, focale.get()));
	}

	public Mat dewarp(Mat img) {
		return getMeshManager().dewarp3D(img);
	}

	public IntegerProperty getvBlurSize() {
		return vBlurSize;
	}

	public IntegerProperty gethBlurSize() {
		return hBlurSize;
	}

	// public DoubleProperty gethNeighbourPenality() {
	// return hNeighbourPenality;
	// }

	// public DoubleProperty getvNeighbourPenality() {
	// return vNeighbourPenality;
	// }

	public DoubleProperty getvAnglePenality() {
		return vAnglePenality;
	}

	public DoubleProperty gethAnglePenality() {
		return hAnglePenality;
	}

	public DoubleBinding getStripWidth() {
		return stripWidth;
	}

	public DoubleBinding getStripHeight() {
		return stripHeight;
	}

	public IntegerProperty getvStripsNumber() {
		return vStripsNumber;
	}

	public IntegerProperty gethStripsNumber() {
		return hStripsNumber;
	}

	public DoubleBinding gethStep() {
		return hStep;
	}

	public DoubleBinding getvStep() {
		return vStep;
	}

	public DoubleProperty getvRecover() {
		return vRecover;
	}

	public DoubleProperty gethRecover() {
		return hRecover;
	}

	public IntegerProperty getHalfGridHeight() {
		return halfGridHeight;
	}

	public IntegerProperty getHalfGridWidth() {
		return halfGridWidth;
	}

	public DoubleProperty gethGlobalThreshold() {
		return hGlobalThreshold;
	}

	public DoubleProperty getvGlobalThreshold() {
		return vGlobalThreshold;
	}

	public DoubleProperty gethLocalThreshold() {
		return hLocalThreshold;
	}

	public DoubleProperty getvLocalThreshold() {
		return vLocalThreshold;
	}

	public DoubleProperty gethMaxConnectDistance() {
		return hMaxConnectDistance;
	}

	public DoubleProperty getvMaxConnectDistance() {
		return vMaxConnectDistance;
	}

	public DoubleProperty getInterpolatorMinDist() {
		return interpolatorMinDist;
	}

	public DoubleProperty getInterpolatorPow() {
		return interpolatorPow;
	}

	public DoubleProperty getFocale() {
		return focale;
	}

}
