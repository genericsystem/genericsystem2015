package org.genericsystem.cv;

import java.util.Iterator;
import java.util.function.Function;
import java.util.stream.Stream;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;

public class ImgClass2 {

	private final Img classModel;
	private final SimpleObjectProperty<Img> observableMean = new SimpleObjectProperty<>();
	private final SimpleObjectProperty<Img> observableVariance = new SimpleObjectProperty<>();
	private final String directory;
	private final SimpleObjectProperty<Function<Img, Img>> preprocessor = new SimpleObjectProperty<>();

	public static ImgClass2 fromDirectory(Img classModel, String bgrDirectory) {
		return new ImgClass2(classModel, bgrDirectory);
	}

	private Img applyPreprocessor(Img img) {
		return preprocessor.getValue() != null ? preprocessor.getValue().apply(img) : img;
	}

	private LongTaskOverrider taskManager = new LongTaskOverrider();

	public ImgClass2(Img classModel, String bgrDirectory) {
		this.classModel = classModel;
		this.directory = bgrDirectory;
		computeMeanVariance();
		preprocessor.addListener((o, ov, nv) -> {
			taskManager.schedule(() -> computeMeanVariance());
		});
	}

	public Stream<Img> classImgsStream() {
		return Tools.classImgsStream(directory);
	}

	private synchronized void computeMeanVariance() {
		Img img0 = applyPreprocessor(classImgsStream().iterator().next());
		boolean gray = img0.channels() == 1;
		int type = gray ? CvType.CV_32S : CvType.CV_32SC3;

		Mat mean = new Mat(img0.size(), type, Scalar.all(0));
		Mat m2 = new Mat(img0.size(), type, Scalar.all(0));
		Mat mask = Mat.ones(img0.size(), CvType.CV_8U);
		img0.close();
		int count = 1;
		Iterator<Img> it = classImgsStream().iterator();
		while (it.hasNext()) {
			Mat img = new Mat();
			applyPreprocessor(it.next()).getSrc().convertTo(img, type);
			Mat delta = new Mat(img.size(), type);
			Core.subtract(img, mean, delta, mask, type);
			Core.addWeighted(mean, 1, delta, 1d / count, 0, mean, type);
			Mat delta2 = new Mat(m2.size(), type);
			Core.subtract(img, mean, delta2, mask, type);
			Mat product = delta.mul(delta2);
			Core.add(m2, product, m2);
			count++;
			img.release();
			delta.release();
			delta2.release();
			product.release();
		}
		Mat variance = new Mat(m2.size(), type);
		Core.multiply(m2, new Scalar(1d / count, 1d / count, 1d / count), variance);
		variance.convertTo(variance, CvType.CV_8U);
		mean.convertTo(mean, CvType.CV_8U);

		m2.release();
		mask.release();
		this.observableMean.setValue(new Img(mean, false));
		this.observableVariance.setValue(new Img(variance, false));
	}

	public void setPreprocessor(Function<Img, Img> after) {
		preprocessor.setValue(after);
	}

	public Img getClassModel() {
		return classModel;
	}

	public ObservableValue<Img> getObservableMean() {
		return observableMean;
	}

	public ObservableValue<Img> getObservableVariance() {
		return observableVariance;
	}

	public String getDirectory() {
		return directory;
	}

	public Img getClosedMean(Size morphClose) {
		return observableMean.getValue().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, morphClose);
	}

	public Img getClosedVariance(Size morphClose) {
		return observableVariance.getValue().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_RECT, morphClose);
	}

}
