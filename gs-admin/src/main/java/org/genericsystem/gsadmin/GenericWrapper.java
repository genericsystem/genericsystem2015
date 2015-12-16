package org.genericsystem.gsadmin;

import java.util.Objects;

import javafx.beans.binding.Bindings;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.scene.layout.HBox;

import org.genericsystem.common.Generic;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSLabel;

public class GenericWrapper {

	private Generic generic;
	private StringProperty stringProperty = new SimpleStringProperty();
	private ObservableValue<String> removeButtonTextProperty = Bindings.concat("Remove : ", stringProperty);

	public GenericWrapper(Generic generic) {
		this.generic = generic;
		stringProperty.set(Objects.toString(this.generic.getValue()));
	}

	public void remove() {
		generic.remove();
	}

	public ObservableValue<String> getObservable() {
		return stringProperty;
	}

	public ObservableValue<String> getRemoveButtonTextProperty() {
		return removeButtonTextProperty;
	}

	public static void init(Element<HBox> todoHBox) {
		new GSLabel(todoHBox, GenericWrapper::getObservable).setPrefWidth(250);
		new GSButton(todoHBox, "remove").setAction(GenericWrapper::remove).setPrefWidth(100);
	}
}
