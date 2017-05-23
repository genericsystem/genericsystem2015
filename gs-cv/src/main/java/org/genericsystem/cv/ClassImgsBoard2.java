package org.genericsystem.cv;

import java.util.Arrays;
import java.util.List;

import org.genericsystem.cv.ClassImgsBoard.LabelledSpinner;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

import javafx.beans.binding.Bindings;
import javafx.beans.value.ObservableValue;
import javafx.geometry.Insets;
import javafx.scene.control.RadioButton;
import javafx.scene.control.ToggleGroup;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

public class ClassImgsBoard2 extends VBox {

	private ToggleGroup group = new ToggleGroup();
	private ObservableValue<Img> average;
	private ObservableValue<Img> variance;

	private ObservableValue<Img> imgToZone;
	private ObservableValue<Img> zonedImg;
	private Runnable action;

	public ClassImgsBoard2(ImgClass2 imgClass, Img model) {
		average = imgClass.getObservableMean();
		variance = imgClass.getObservableVariance();

		RadioButton averageRadio = new RadioButton("Average");
		averageRadio.setToggleGroup(group);
		averageRadio.setUserData(imgClass.getObservableMean());

		RadioButton varianceRadio = new RadioButton("Variance");
		varianceRadio.setToggleGroup(group);
		varianceRadio.setUserData(imgClass.getObservableVariance());
		varianceRadio.setSelected(true);

		imgToZone = Bindings.createObjectBinding(() -> {
			ObservableValue<Img> o = ((ObservableValue<Img>) group.getSelectedToggle().getUserData());
			return o != null ? o.getValue() : null;
		}, group.selectedToggleProperty(), average, variance);
		zonedImg = Bindings.createObjectBinding(() -> {
			Img zonedMean = new Img(model.getSrc());
			if (imgToZone.getValue() != null) {
				Zones zones = Zones.get(imgToZone.getValue().morphologyEx(Imgproc.MORPH_CLOSE, new StructuringElement(Imgproc.MORPH_RECT, new Size(9, 10))), 300.0, 6.0, 6.0);
				zones.draw(zonedMean, new Scalar(0, 255, 0), 3);
				return zonedMean;
			}
			return zonedMean;
		}, imgToZone);

		VBox vBox = new VBox();
		setPadding(new Insets(40, 40, 40, 40));

		HBox zoneBase = new HBox();
		zoneBase.getChildren().add(averageRadio);
		zoneBase.getChildren().add(varianceRadio);
		zoneBase.setSpacing(15);
		vBox.getChildren().add(zoneBase);
		List<LabelledSpinner> spinners = Arrays.asList(new LabelledSpinner("min hue", 0, 255, 0), new LabelledSpinner("min saturation", 0, 255, 0), new LabelledSpinner("min value", 0, 255, 0), new LabelledSpinner("max hue", 0, 255, 255),
				new LabelledSpinner("max saturation", 0, 255, 255), new LabelledSpinner("max value", 0, 255, 86), new LabelledSpinner("min blue", 0, 255, 0), new LabelledSpinner("min green", 0, 255, 0), new LabelledSpinner("min red", 0, 255, 0),
				new LabelledSpinner("max blue", 0, 255, 76), new LabelledSpinner("max green", 0, 255, 255), new LabelledSpinner("max red", 0, 255, 255), new LabelledSpinner("horizontal dilatation", 1, 60, 17),
				new LabelledSpinner("vertical dilatation", 1, 30, 3));
		action = () -> imgClass.setPreprocessor(
				img -> img.eraseCorners(0.1).range(new Scalar(spinners.get(0).getValue(), spinners.get(1).getValue(), spinners.get(2).getValue()), new Scalar(spinners.get(3).getValue(), spinners.get(4).getValue(), spinners.get(5).getValue()), true)
						.range(new Scalar(spinners.get(6).getValue(), spinners.get(7).getValue(), spinners.get(8).getValue()), new Scalar(spinners.get(9).getValue(), spinners.get(10).getValue(), spinners.get(11).getValue()), false)
						.morphologyEx(Imgproc.MORPH_DILATE, new StructuringElement(Imgproc.MORPH_RECT, new Size(spinners.get(12).getValue(), spinners.get(13).getValue()))));
		spinners.forEach(spinner -> spinner.setListener(action));
		spinners.forEach(vBox.getChildren()::add);
		AwareImageView zonesImageView = new AwareImageView(zonedImg);
		getChildren().add(zonesImageView);
		getChildren().add(vBox);
		action.run();
	}

}
