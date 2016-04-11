package org.genericsystem.distributed.cacheonserver.todocarmvc;

import java.util.Objects;

import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleBooleanProperty;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.Model;

public class CarModel extends Model {

	private final Generic generic;
	private Property<String> todoString;
	private Property<Boolean> completed = new SimpleBooleanProperty(false);

	CarModel(CarListModel parentModel, Generic generic) {
		this.generic = generic;
		todoString = new ReadOnlyObjectWrapper<>(Objects.toString(generic));
	}

	public Property<String> getTodoString() {
		return todoString;
	}

	public Property<Boolean> getCompleted() {
		return completed;
	}

	public void select() {
		((CarListModel) getParent()).getSelection().setValue(this);
	}

	public void remove() {
		generic.remove();
	}
}
