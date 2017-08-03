package org.genericsystem.cv.comparator;

public enum ImgFilterFunction {
	ORIGINAL("original", i -> i),
	REALITY("reality", i -> i),
	EQUALIZE_HISTO("equalizeHisto", i -> i.equalizeHisto()),
	EQUALIZE_HISTO_ADAPTATIVE("equalizeHistoAdaptative", i -> i.equalizeHistoAdaptative()),
	OTSU_AFTER_GAUSSIAN_BLUR("otsuAfterGaussianBlur", i -> i.otsuAfterGaussianBlur()),
	ADAPTATIVE_GAUSSIAN_THRESHOLD("adaptativeGaussianThreshold", i -> i.adaptativeGaussianThreshold());

	private String name;
	private ImgFunction lambda;

	private ImgFilterFunction(String name, ImgFunction function) {
		this.name = name;
		this.lambda = function;
	}

	public ImgFunction getLambda() {
		return this.lambda;
	}

	public String getName() {
		return this.name;
	}

}
