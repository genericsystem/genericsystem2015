package org.genericsystem.distributed.ui.models;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

import org.genericsystem.distributed.ui.Model;

/**
 * @author Nicolas Feybesse
 *
 */
public class StringModel extends Model {

	private final Property<String> string;

	public StringModel(Property<String> string) {
		this.string = string;
	}

	public ObservableValue<String> getString() {
		return string;
	}
}
