package org.genericsystem.gsadmin;

import java.util.Objects;

import javafx.beans.binding.Bindings;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.layout.HBox;

import org.genericsystem.common.Generic;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.utils.Transformation;

public class GenericWrapper {

	private Generic generic;
	private StringProperty stringProperty = new SimpleStringProperty();
	private ObservableValue<String> removeButtonTextProperty = Bindings.concat("Remove : ", stringProperty);
	private Transformation<GenericWrapper, Generic> genericList;
	private ObservableList<Generic> dependenciesObservableList;

	private ObservableList<Generic> AttributesObservableList = FXCollections.observableArrayList();
	private Transformation<GenericWrapper, Generic> genericListAttributes;

	/************************************************************************************/
	public static void init(Element<HBox> parent) {
		new GSLabel(parent, GenericWrapper::getObservable).setPrefWidth(250);
		// new GSLabel(parent, GenericWrapper::getObservable).forEach(GenericWrapper::getGenericList).setPrefWidth(100);
		new GSButton(parent, "remove").setAction(GenericWrapper::remove).setPrefWidth(100);
		new GSButton(parent, "select").setMetaAction((gl, gw) -> ((GenericList) gl).getSelection().setValue((GenericWrapper) gw)).setPrefWidth(90);

	}

	/***********************************************************************************/
	public GenericWrapper(Generic generic) {
		this.generic = generic;
		stringProperty.set(Objects.toString(this.generic.getValue()));
		AttributesObservableList.add(generic);
		AttributesObservableList.addAll(FXCollections.observableArrayList(this.generic.getAttributes().filter(attribute -> attribute.isCompositeForInstances(this.generic)).toList()));

		genericListAttributes = new Transformation<GenericWrapper, Generic>(AttributesObservableList, gen -> new GenericWrapper(gen));

		dependenciesObservableList = FXCollections.observableArrayList(this.generic.getInstances().toList());
		genericList = new Transformation<GenericWrapper, Generic>(dependenciesObservableList, gen -> new GenericWrapper(gen));

	}

	/***********************************************************************************/
	public void remove() {
		generic.remove();
	}

	/***********************************************************************************/

	public Transformation<GenericWrapper, Generic> getGenericListAttributes() {
		return genericListAttributes;
	}

	public ObservableList<GenericWrapper> getGenericList() {
		return genericList;
	}

	public ObservableValue<String> getObservable() {
		return stringProperty;
	}

	public ObservableValue<String> getRemoveButtonTextProperty() {
		return removeButtonTextProperty;
	}
}
