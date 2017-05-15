package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

public class ImgClass {

	private final Img classModel;
	private Img mean;
	private Img variance;
	private final String directory;
	private final List<Function<Img, Img>> mappers = new ArrayList<>();

	public static ImgClass fromDirectory(Img classModel, String bgrDirectory) {
		return new ImgClass(classModel, bgrDirectory);
	}

	private Img applyMappers(Img img) {
		for (Function<Img, Img> mapper : mappers) {
			img = mapper.apply(img);
			System.gc();
			System.runFinalization();

		}
		return img;
	}

	public ImgClass(Img classModel, String bgrDirectory) {
		this.classModel = classModel;
		this.directory = bgrDirectory;
		computeMeanVariance();

	}

	public Stream<Img> classImgsStream() {
		return Tools.classImgsStream(directory);
	}

	private void computeMeanVariance() {
		Img img0 = applyMappers(classImgsStream().iterator().next());
		boolean gray = img0.channels() == 1;
		int type = gray ? CvType.CV_32S : CvType.CV_32SC3;

		Mat mean = new Mat(img0.size(), type, Scalar.all(0));
		Mat m2 = new Mat(img0.size(), type, Scalar.all(0));
		Mat mask = Mat.ones(img0.size(), CvType.CV_8U);
		int count = 1;
		Iterator<Img> it = classImgsStream().iterator();
		while (it.hasNext()) {
			Mat img = new Mat();
			applyMappers(it.next()).getSrc().convertTo(img, type);
			Mat delta = new Mat(img.size(), type);
			Core.subtract(img, mean, delta, mask, type);
			Core.addWeighted(mean, 1, delta, 1d / count, 0, mean, type);
			Mat delta2 = new Mat(m2.size(), type);
			Core.subtract(img, mean, delta2, mask, type);
			Mat product = delta.mul(delta2);
			Core.add(m2, product, m2);
			count++;
		}
		Mat variance = new Mat(m2.size(), type);
		Core.multiply(m2, new Scalar(1d / count, 1d / count, 1d / count), variance);
		variance.convertTo(variance, CvType.CV_8U);
		mean.convertTo(mean, CvType.CV_8U);

		this.mean = new Img(mean);
		this.variance = new Img(variance);
	}

	public void addMapper(Function<Img, Img> after) {
		mappers.add(after);
		computeMeanVariance();
	}

	public Img getClassModel() {
		return classModel;
	}

	public Img getMean() {
		return mean;
	}

	public Img getVariance() {
		return variance;
	}

	public String getDirectory() {
		return directory;
	}

	public Zones getClosedMeanZones(double minarea, double dx, double dy, Size morphClose) {
		return Zones.get(mean.morphologyEx(Imgproc.MORPH_CLOSE, new StructuringElement(Imgproc.MORPH_RECT, morphClose)), minarea, dx, dy);
	}

	public Zones getClosedVarianceZones(double minarea, double dx, double dy, Size morphClose) {
		return Zones.get(variance.morphologyEx(Imgproc.MORPH_CLOSE, new StructuringElement(Imgproc.MORPH_RECT, morphClose)), minarea, dx, dy);
	}
}
