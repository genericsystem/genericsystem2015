package org.genericsystem.cv.params;

import java.util.HashMap;
import java.util.Map;

import javafx.beans.property.DoubleProperty;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.scene.control.Slider;
import javafx.scene.layout.VBox;

public class ParamsPanel extends VBox {

	private Map<String, SimpleDoubleProperty> model = new HashMap<>();

	public DoubleProperty addProperty(String name, double value) {
		DoubleProperty property = new SimpleDoubleProperty(value);
		model.put(name, new SimpleDoubleProperty(value));
		return property;
	}

	public DoubleProperty addSliderProperty(String propertyName, double value, double min, double max) {
		DoubleProperty property = addProperty(propertyName, value);
		property.addListener((ov, oldV, newV) -> System.out.println(propertyName + " " + newV));
		Slider slider = new Slider(min, max, value);
		slider.setShowTickLabels(true);
		slider.setShowTickMarks(true);
		// slider.setMajorTickUnit(50);
		// slider.setMinorTickCount(5);
		// slider.setBlockIncrement(10);
		slider.valueProperty().addListener((ov, oldValue, newValue) -> {
			if (!slider.isValueChanging())
				property.setValue(newValue);
		});
		slider.valueChangingProperty().addListener((ov, oldValue, newValue) -> {
			if (!newValue)
				property.setValue(slider.getValue());
		});
		getChildren().add(slider);
		return property;
	}

	// public DoubleProperty getProperty(String propertyName) {
	// return model.get(propertyName);
	// }

	// public DoubleProperty getSliderProperty(String propertyName) {
	// return getProperty(propertyName);
	// }

}