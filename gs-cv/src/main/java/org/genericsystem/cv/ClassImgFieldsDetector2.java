package org.genericsystem.cv;

import java.util.Arrays;
import java.util.List;

import javafx.application.Platform;
import javafx.beans.binding.Bindings;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.geometry.Insets;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.RadioButton;
import javafx.scene.control.Slider;
import javafx.scene.control.ToggleGroup;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.VBox;

import org.opencv.core.Core;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

public class ClassImgFieldsDetector2 extends AbstractApp {
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	private final static String classImgRepertory = "classes/id-fr-front";

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
		mainGrid.add(new AwareZonageImageView(imgClass, model, observableVariance), 1, 2);

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

		public AwareZonageImageView(ImgClass2 imgClass, Img mean, ObservableValue<Img> observableImg) {
			observableImg.addListener((o, ov, nv) -> {
				Zones zones = imgClass.buildZones(nv);
				Img zonedMean = new Img(mean.getSrc());
				zones.draw(zonedMean, new Scalar(0, 255, 0), 3);
				Platform.runLater(() -> setImage(zonedMean.getImageView().getImage()));
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
				if (!changing)
					action.run();
			};
			slider.valueChangingProperty().addListener(sliderListener);
		}

	}

	public static class ClassImgsBoard extends VBox {

		public ClassImgsBoard(ImgClass2 imgClass) {
			setPadding(new Insets(40, 40, 40, 40));

			ToggleGroup group = new ToggleGroup();

			RadioButton average = new RadioButton("Average");
			average.setToggleGroup(group);
			average.setUserData(imgClass.getObservableMean());

			RadioButton variance = new RadioButton("Variance");
			variance.setToggleGroup(group);
			variance.setSelected(true);
			average.setUserData(imgClass.getObservableVariance());

			group.selectedToggleProperty().addListener((o, ov, nv) -> {
				System.out.println("change");
				group.getSelectedToggle().getUserData();// TODO
				});
			getChildren().add(average);
			getChildren().add(variance);
			List<LabelledSpinner> spinners = Arrays.asList(new LabelledSpinner("min hue", 0, 255, 0), new LabelledSpinner("min saturation", 0, 255, 0), new LabelledSpinner("min value", 0, 255, 0), new LabelledSpinner("max hue", 0, 255, 255),
					new LabelledSpinner("max saturation", 0, 255, 255), new LabelledSpinner("max value", 0, 255, 86), new LabelledSpinner("min blue", 0, 255, 0), new LabelledSpinner("min green", 0, 255, 0), new LabelledSpinner("min red", 0, 255, 0),
					new LabelledSpinner("max blue", 0, 255, 76), new LabelledSpinner("max green", 0, 255, 255), new LabelledSpinner("max red", 0, 255, 255), new LabelledSpinner("horizontal dilatation", 1, 60, 17), new LabelledSpinner(
							"vertical dilatation", 1, 30, 3));
			Runnable action = () -> imgClass.setPreprocessor(img -> img.eraseCorners(0.1)
					.range(new Scalar(spinners.get(0).getValue(), spinners.get(1).getValue(), spinners.get(2).getValue()), new Scalar(spinners.get(3).getValue(), spinners.get(4).getValue(), spinners.get(5).getValue()), true)
					.range(new Scalar(spinners.get(6).getValue(), spinners.get(7).getValue(), spinners.get(8).getValue()), new Scalar(spinners.get(9).getValue(), spinners.get(10).getValue(), spinners.get(11).getValue()), false).gray()
					.morphologyEx(Imgproc.MORPH_DILATE, new StructuringElement(Imgproc.MORPH_RECT, new Size(spinners.get(12).getValue(), spinners.get(13).getValue()))));
			spinners.forEach(spinner -> spinner.setListener(action));
			spinners.forEach(getChildren()::add);
			Button saveButton = new Button("Save");
			saveButton.setOnAction((e) -> imgClass.saveZones());
			getChildren().add(saveButton);
			action.run();
		}
	}

}