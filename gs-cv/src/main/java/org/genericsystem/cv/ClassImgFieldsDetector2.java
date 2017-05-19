package org.genericsystem.cv;

import org.opencv.core.Core;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

import javafx.application.Platform;
import javafx.beans.binding.Bindings;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.geometry.Insets;
import javafx.scene.control.Label;
import javafx.scene.control.Slider;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.VBox;

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

		ImgClass2 imgClass = ImgClass2.fromDirectory(null, classImgRepertory);

		ObservableValue<Img> observableMean = imgClass.getObservableMean();
		Img model = observableMean.getValue();
		ObservableValue<Img> observableVariance = imgClass.getObservableVariance();

		mainGrid.add(observableMean.getValue().getImageView(), 0, 0);
		mainGrid.add(observableVariance.getValue().getImageView(), 0, 1);

		mainGrid.add(new AwareImageView(observableMean), 1, 0);
		mainGrid.add(new AwareImageView(observableVariance), 1, 1);

		mainGrid.add(new ClassImgsBoard(imgClass), 0, 2);
		mainGrid.add(new AwareZonageImageView(model, observableVariance), 1, 2);
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

		public AwareZonageImageView(Img mean, ObservableValue<Img> observableImg) {
			observableImg.addListener((o, ov, nv) -> {
				Zones zones = Zones.get(nv.morphologyEx(Imgproc.MORPH_CLOSE, new StructuringElement(Imgproc.MORPH_RECT, new Size(9, 10))), 300, 6, 6);
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

	public static class LabelledSpinner extends VBox {
		private final Label label = new Label();
		private final Slider slider = new Slider();

		private ChangeListener<Boolean> sliderListener;

		public LabelledSpinner(String name, double min, double max, double value) {
			slider.setMin(min);
			slider.setMax(max);
			slider.setValue(value);
			label.textProperty().bind(Bindings.createStringBinding(() -> name + " : " + slider.valueProperty().doubleValue(), slider.valueProperty()));
			getChildren().add(label);
			getChildren().add(slider);
		}

		public double getValue() {
			return slider.getValue();
		}

		public void setListener(Runnable action) {
			sliderListener = (o, wasChanging, changing) -> {
				if (!changing) {
					action.run();
				}
			};
			slider.valueChangingProperty().addListener(sliderListener);
		}

	}

	public static class ClassImgsBoard extends VBox {

		public ClassImgsBoard(ImgClass2 imgClass) {
			setPadding(new Insets(40, 40, 40, 40));
			LabelledSpinner valueSpinner = new LabelledSpinner("value", 0, 255, 86);
			LabelledSpinner blueSpinner = new LabelledSpinner("blue", 0, 255, 76);
			LabelledSpinner saturationSpinner = new LabelledSpinner("saturation", 0, 255, 255);
			Runnable action = () -> imgClass.setPreprocessor(img -> img.eraseCorners(0.1).dilateBlacks(valueSpinner.getValue(), blueSpinner.getValue(), saturationSpinner.getValue(), new Size(15, 3)));
			valueSpinner.setListener(action);
			blueSpinner.setListener(action);
			saturationSpinner.setListener(action);
			action.run();
			getChildren().add(valueSpinner);
			getChildren().add(blueSpinner);
			getChildren().add(saturationSpinner);
		}
	}

}