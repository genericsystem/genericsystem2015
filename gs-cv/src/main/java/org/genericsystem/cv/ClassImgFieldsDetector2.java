package org.genericsystem.cv;

import org.opencv.core.Core;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.geometry.Insets;
import javafx.scene.control.Label;
import javafx.scene.control.Slider;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class ClassImgFieldsDetector2 extends AbstractApp {
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	private final static String classImgRepertory = "classes/passport-fr";

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {

		Slider valueSlider = new Slider();
		Slider blueSlider = new Slider();
		Slider saturationSlider = new Slider();

		valueSlider.setMin(0);
		valueSlider.setMax(255);
		valueSlider.setValue(86);
		blueSlider.setMin(0);
		blueSlider.setMax(255);
		blueSlider.setValue(76);
		saturationSlider.setMin(0);
		saturationSlider.setMax(255);
		saturationSlider.setValue(255);

		ImgClass2 imgClass = ImgClass2.fromDirectory(null, classImgRepertory);

		ObservableValue<Img> observableMean = imgClass.getObservableMean();
		Img model = observableMean.getValue();
		ObservableValue<Img> observableVariance = imgClass.getObservableVariance();

		mainGrid.add(observableMean.getValue().getImageView(), 0, 0);
		mainGrid.add(observableVariance.getValue().getImageView(), 0, 1);

		imgClass.setPreprocessor(img -> img.eraseCorners(0.1).dilateBlacks(valueSlider.getValue(),
				blueSlider.getValue(), saturationSlider.getValue(), new Size(15, 3)));

		mainGrid.add(new AwareImageView(observableMean), 1, 0);
		mainGrid.add(new AwareImageView(observableVariance), 1, 1);

		mainGrid.add(new AwareZonageImageView(model, observableVariance), 1, 2);

		GridPane sliders = new GridPane();
		sliders.setPadding(new Insets(40, 40, 40, 40));

		Label valueLabel = new Label("Value : " + valueSlider.getValue());
		sliders.add(valueLabel, 0, 0);
		sliders.add(valueSlider, 0, 1);

		Label saturationLabel = new Label("Saturation : " + saturationSlider.getValue());
		saturationLabel.setPadding(new Insets(20, 0, 0, 0));
		sliders.add(saturationLabel, 0, 2);
		sliders.add(saturationSlider, 0, 3);

		Label blueLabel = new Label("Blue : " + blueSlider.getValue());
		blueLabel.setPadding(new Insets(20, 0, 0, 0));
		sliders.add(blueLabel, 0, 4);
		sliders.add(blueSlider, 0, 5);

		mainGrid.add(sliders, 0, 2);

		valueSlider.valueChangingProperty().addListener(new ChangeListener<Boolean>() {
			@Override
			public void changed(ObservableValue<? extends Boolean> o, Boolean wasChanging, Boolean changing) {
				if (!changing) {
					imgClass.setPreprocessor((img) -> img.eraseCorners(0.1).dilateBlacks(valueSlider.getValue(),
							blueSlider.getValue(), saturationSlider.getValue(), new Size(15, 3)));
					valueLabel.setText("Value : " + ((Number) valueSlider.getValue()).intValue());
				}
			}
		});

		saturationSlider.valueChangingProperty().addListener(new ChangeListener<Boolean>() {
			@Override
			public void changed(ObservableValue<? extends Boolean> o, Boolean wasChanging, Boolean changing) {
				if (!changing) {
					imgClass.setPreprocessor((img) -> img.eraseCorners(0.1).dilateBlacks(valueSlider.getValue(),
							blueSlider.getValue(), saturationSlider.getValue(), new Size(15, 3)));
					saturationLabel.setText("Saturation : " + ((Number) saturationSlider.getValue()).intValue());
				}
			}
		});

		blueSlider.valueChangingProperty().addListener(new ChangeListener<Boolean>() {
			@Override
			public void changed(ObservableValue<? extends Boolean> o, Boolean wasChanging, Boolean changing) {
				if (!changing) {
					imgClass.setPreprocessor((img) -> img.eraseCorners(0.1).dilateBlacks(valueSlider.getValue(),
							blueSlider.getValue(), saturationSlider.getValue(), new Size(15, 3)));
					blueLabel.setText("Blue : " + ((Number) blueSlider.getValue()).intValue());
				}
			}
		});

		// for (File file : new File(classImgRepertory).listFiles())
		// if (file.getName().endsWith(".png")) {
		// System.out.println("file : " + file.getName());
		// Img img = new Img(Imgcodecs.imread(file.getPath()));
		// try {
		// List<Mat> sameMats = Tools.getClassMats(classImgRepertory + "/mask/"
		// + file.getName().replace(".png", ""));
		// for (Zone zone : zones.get()) {
		// zone.draw(img, new Scalar(0, 255, 0), -1);
		// UnsupervisedZoneScorer scorer = zone.newUnsupervisedScorer(sameMats);
		// zone.write(img, scorer.getBestText() + " " +
		// Math.floor((scorer.getBestScore() * 10000)) / 100 + "%", 2.5, new
		// Scalar(0, 0, 255), 2);
		// }
		// mainGrid.add(img.getImageView(), columnIndex, rowIndex++);
		// } catch (Exception ignore) {
		//
		// }
		// }

	}
	// Img img = Tools.classImgsStream(classImgRepertory).iterator().next();
	// ImgZoner.drawZones(img.sobel(), img, 300, new Scalar(0, 255, 0), 3);
	// mainGrid.add(img.getImageView(), columnIndex, rowIndex++);
	//
	// Img img2 = Tools.classImgsStream(classImgRepertory).iterator().next();
	// ImgZoner.drawZones(img2.mser(), img2, 300, new Scalar(0, 255, 0), 3);
	// mainGrid.add(img2.getImageView(), columnIndex, rowIndex++);
	//
	// Img img3 = Tools.classImgsStream(classImgRepertory).iterator().next();
	// ImgZoner.drawZones(img3.grad(), img3, 300, new Scalar(0, 255, 0), 3);
	// mainGrid.add(img3.getImageView(), columnIndex, rowIndex++);

	private Img imageProcessing(Img img, int value, int blue, int saturation) {
		return img.eraseCorners(0.1).dilateBlacks(value, blue, saturation, new Size(15, 3));
	}

	public static class AwareImageView extends ImageView {

		public AwareImageView(ObservableValue<Img> observableImg) {
			observableImg.addListener((o, ov, nv) -> {
				Platform.runLater(new Runnable() {
					@Override
					public void run() {
						setImage(observableImg.getValue().getImageView().getImage());
					}
				});
			});
			setImage(observableImg.getValue().getImageView().getImage());
		}

	}

	public static class AwareZonageImageView extends ImageView {

		private final Img mean;

		public AwareZonageImageView(Img mean, ObservableValue<Img> observableImg) {
			this.mean = new Img(mean.getSrc());
			observableImg.addListener((o, ov, nv) -> {
				Zones zones = Zones.get(nv.morphologyEx(Imgproc.MORPH_CLOSE,
						new StructuringElement(Imgproc.MORPH_RECT, new Size(9, 10))), 300, 6, 6);
				Img zonedMean = new Img(mean.getSrc());
				zones.draw(zonedMean, new Scalar(0, 255, 0), 3);
				Platform.runLater(new Runnable() {
					@Override
					public void run() {
						setImage(zonedMean.getImageView().getImage());
					}
				});
			});
			setImage(mean.getImageView().getImage());
		}

	}
}