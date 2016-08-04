package org.genericsystem.reactor.gs;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gs.GSCellDisplayer.GSAttributeCreator;
import org.genericsystem.reactor.gs.GSHolderEditor.GSHolderCreator;
import org.genericsystem.reactor.gstag.GSButton;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.ObservableListExtractor;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

public class GSInstanceCreator extends GSComposite {

	private GSHolderEditor instanceValueInput;

	public GSInstanceCreator(GSTag parent, FlexDirection flexDirection) {
		super(parent, flexDirection);
		createNewProperty(ReactorStatics.HOLDERS_MAP);
		initProperty(ReactorStatics.HOLDERS_MAP, model -> new HashMap<Generic, Property<Serializable>>());
		createNewProperty(ReactorStatics.COMPONENTS_MAP);
		initProperty(ReactorStatics.COMPONENTS_MAP, model -> new HashMap<Generic, List<Property<GenericModel>>>());
		createNewProperty(ReactorStatics.INVALID_LIST);
		initProperty(ReactorStatics.INVALID_LIST, model -> new ArrayList<ObservableValue<Boolean>>());
	}

	@Override
	protected void header() {
		instanceValueInput = new GSHolderCreator(this);
	}

	@Override
	protected void sections() {
		new GSAttributeCreator(this, FlexDirection.ROW) {
			{
				forEach(ObservableListExtractor.ATTRIBUTES_OF_TYPE);
			}
		};
	}

	@Override
	protected void footer() {
		new GSSection(this, this.getDirection()) {
			{
				if (this.getDirection().equals(FlexDirection.ROW)) {
					addStyle("flex", "0");
					addStyle("min-width", "100px");
				} else {
					addStyle("flex", "1");
				}
				addStyle("background-color", "#dda5a5");
				addStyle("margin-right", "1px");
				addStyle("margin-bottom", "1px");
				new GSButton(this) {
					{
						bindAttribute(ReactorStatics.DISABLED, ReactorStatics.DISABLED, model -> {
							ObservableValue<Boolean> invalid = Bindings.createBooleanBinding(
									() -> ((List<ObservableValue<Boolean>>) getProperty(ReactorStatics.INVALID_LIST, model).getValue()).stream().map(input -> input.getValue()).filter(bool -> bool != null).reduce(false, (a, b) -> a || b),
									((List<ObservableValue<Boolean>>) getProperty(ReactorStatics.INVALID_LIST, model).getValue()).stream().toArray(ObservableValue[]::new));
							return Bindings.createStringBinding(() -> Boolean.TRUE.equals(invalid.getValue()) ? ReactorStatics.DISABLED : "", invalid);
						});
						bindAction(model -> {
							Generic newInstance = model.getGeneric().setInstance((Serializable) model.getProperty(instanceValueInput.input, ReactorStatics.VALUE).getValue());
							for (Entry<Generic, Property<Serializable>> entry : ((Map<Generic, Property<Serializable>>) getProperty(ReactorStatics.HOLDERS_MAP, model).getValue()).entrySet())
								if (entry.getValue().getValue() != null) {
									newInstance.setHolder(entry.getKey(), entry.getValue().getValue());
									entry.getValue().setValue(null);
								}
							for (Entry<Generic, List<Property<GenericModel>>> entry : ((Map<Generic, List<Property<GenericModel>>>) getProperty(ReactorStatics.COMPONENTS_MAP, model).getValue()).entrySet()) {
								List<Generic> selectedGenerics = entry.getValue().stream().filter(obs -> obs.getValue() != null).map(obs -> obs.getValue().getGeneric()).filter(gen -> gen != null).collect(Collectors.toList());
								if (!selectedGenerics.isEmpty() && selectedGenerics.size() + 1 == entry.getKey().getComponents().size())
									newInstance.setHolder(entry.getKey(), null, selectedGenerics.stream().toArray(Generic[]::new));
								entry.getValue().stream().forEach(sel -> sel.setValue(null));
							}
							model.getProperty(instanceValueInput.input, ReactorStatics.VALUE).setValue(null);
						});
						setText("Add");
						addStyle("width", "100%");
						addStyle("height", "100%");
					}
				};
			}
		};
	}
}