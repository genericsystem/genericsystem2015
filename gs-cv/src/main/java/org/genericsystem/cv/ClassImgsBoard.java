package org.genericsystem.cv;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.function.Function;

import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

import javafx.application.Platform;
import javafx.beans.InvalidationListener;
import javafx.beans.WeakInvalidationListener;
import javafx.beans.binding.Binding;
import javafx.beans.binding.Bindings;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.geometry.Insets;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.RadioButton;
import javafx.scene.control.Slider;
import javafx.scene.control.ToggleGroup;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

public class ClassImgsBoard extends VBox {

	private final SimpleObjectProperty<ObservableValue<Img>> zonesRef = new SimpleObjectProperty<>();
	private Runnable action;
	private final SimpleObjectProperty<Img> imgToDisplay = new SimpleObjectProperty<>();
	private final ObjectBinding<Img> ob;
	private Zones zones;

	public ClassImgsBoard(ImgClass2 imgClass, Img model) {

		VBox vBox = new VBox();

		setPadding(new Insets(40, 40, 40, 40));

		ToggleGroup group = new ToggleGroup();

		RadioButton average = new RadioButton("Average");
		average.setToggleGroup(group);
		average.setUserData(imgClass.getObservableMean());

		RadioButton variance = new RadioButton("Variance");
		variance.setToggleGroup(group);
		variance.setUserData(imgClass.getObservableVariance());
		variance.setSelected(true);

		zonesRef.setValue(imgClass.getObservableVariance());

		group.selectedToggleProperty().addListener((o, ov, nv) -> {
			zonesRef.setValue((ObservableValue<Img>) group.getSelectedToggle().getUserData());
		});

		ob = createTransitive(zonesRef, ov -> ov.getValue());
		ob.addListener((o, ov, nv) -> {
			zones = Zones.get(
					nv.morphologyEx(Imgproc.MORPH_CLOSE, new StructuringElement(Imgproc.MORPH_RECT, new Size(9, 10))),
					300.0, 6.0, 6.0);
			Img zonedMean = new Img(model.getSrc());
			zones.draw(zonedMean, new Scalar(0, 255, 0), 3);
			Platform.runLater(() -> imgToDisplay.setValue(zonedMean));
		});
		zones = Zones.get(zonesRef.getValue().getValue()
				.morphologyEx(Imgproc.MORPH_CLOSE, new StructuringElement(Imgproc.MORPH_RECT, new Size(9, 10))).gray(),
				300.0, 6.0, 6.0);
		zones.draw(model, new Scalar(0, 255, 0), 3);
		imgToDisplay.setValue(model);

		AwareImageView zonesImageView = new AwareImageView(imgToDisplay);

		HBox zoneBase = new HBox();
		zoneBase.getChildren().add(average);
		zoneBase.getChildren().add(variance);
		zoneBase.setSpacing(15);
		vBox.getChildren().add(zoneBase);
		List<LabelledSpinner> spinners = Arrays.asList(new LabelledSpinner("min hue", 0, 255, 0),
				new LabelledSpinner("min saturation", 0, 255, 0), new LabelledSpinner("min value", 0, 255, 0),
				new LabelledSpinner("max hue", 0, 255, 255), new LabelledSpinner("max saturation", 0, 255, 255),
				new LabelledSpinner("max value", 0, 255, 86), new LabelledSpinner("min blue", 0, 255, 0),
				new LabelledSpinner("min green", 0, 255, 0), new LabelledSpinner("min red", 0, 255, 0),
				new LabelledSpinner("max blue", 0, 255, 76), new LabelledSpinner("max green", 0, 255, 255),
				new LabelledSpinner("max red", 0, 255, 255), new LabelledSpinner("horizontal dilatation", 1, 60, 17),
				new LabelledSpinner("vertical dilatation", 1, 30, 3));
		action = () -> imgClass.setPreprocessor(img -> img.eraseCorners(0.1)
				.range(new Scalar(spinners.get(0).getValue(), spinners.get(1).getValue(), spinners.get(2).getValue()),
						new Scalar(spinners.get(3).getValue(), spinners.get(4).getValue(), spinners.get(5).getValue()),
						true)
				.range(new Scalar(spinners.get(6).getValue(), spinners.get(7).getValue(), spinners.get(8).getValue()),
						new Scalar(spinners.get(9).getValue(), spinners.get(10).getValue(),
								spinners.get(11).getValue()),
						false)
				.gray().morphologyEx(Imgproc.MORPH_DILATE, new StructuringElement(Imgproc.MORPH_RECT,
						new Size(spinners.get(12).getValue(), spinners.get(13).getValue()))));
		spinners.forEach(spinner -> spinner.setListener(action));
		spinners.forEach(vBox.getChildren()::add);
		Button saveButton = new Button("Save");
		saveButton.setOnAction((e) -> zones.save(new File(imgClass.getDirectory() + "/zones/zones.json")));
		vBox.getChildren().add(saveButton);
		getChildren().add(zonesImageView);
		getChildren().add(vBox);
		action.run();
	}

	public static <T> ObjectBinding<T> createTransitive(ObservableValue<ObservableValue<T>> master,
			Function<ObservableValue<ObservableValue<T>>, ObservableValue<T>> slaveFromValue) {
		return transmitSuccessiveInvalidations(new ObjectBinding<T>() {
			private ObservableValue<T> slave;
			private final InvalidationListener onMasterInvalidation = (c) -> onMasterInvalidation();

			{
				master.addListener(new WeakInvalidationListener(onMasterInvalidation));
				onMasterInvalidation();
			}

			private void onMasterInvalidation() {
				unbind(slave);
				invalidate();
				bind(slave = slaveFromValue.apply(master));
			}

			@Override
			protected T computeValue() {
				return slave.getValue();
			}
		});
	}

	public static <U extends Binding<V>, V> U transmitSuccessiveInvalidations(U binding) {
		binding.addListener((o, v, nv) -> {
		});
		return binding;
	}

	public static class LabelledSpinner extends VBox {
		private final Label label = new Label();
		private final Slider slider = new Slider();

		private ChangeListener<Boolean> sliderListener;

		public LabelledSpinner(String name, double min, double max, double value) {
			slider.setMin(min);
			slider.setMax(max);
			slider.setValue(value);
			label.textProperty().bind(Bindings.createStringBinding(
					() -> name + " : " + slider.valueProperty().intValue(), slider.valueProperty()));
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

}
