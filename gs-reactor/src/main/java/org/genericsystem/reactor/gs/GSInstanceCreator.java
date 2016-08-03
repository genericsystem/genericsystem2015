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
import org.genericsystem.reactor.model.StringExtractor;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

public class GSInstanceCreator extends GSComposite {

	private Property<Serializable> newInstanceValue;
	private final Map<Generic, Property<Serializable>> holdersValues = new HashMap<>();
	private final Map<Generic, List<Property<GenericModel>>> componentsValues = new HashMap<>();
	private final List<ObservableValue<Boolean>> propertiesInvalid = new ArrayList<>();

	public GSInstanceCreator(GSTag parent, FlexDirection flexDirection) {
		super(parent, flexDirection);
	}

	public Map<Generic, Property<Serializable>> getHoldersValues() {
		return holdersValues;
	}

	public Map<Generic, List<Property<GenericModel>>> getLinksValues() {
		return componentsValues;
	}

	public List<ObservableValue<Boolean>> getPropertiesInvalid() {
		return propertiesInvalid;
	}

	@Override
	protected void header() {
		new GSHolderCreator(this) {
			{
				input.addPrefixBinding(model -> newInstanceValue = model.getProperty(input, ReactorStatics.VALUE));
			}
		};
	}

	@Override
	protected void sections() {
		new GSAttributeCreator(this, FlexDirection.ROW) {
			{
				forEach(StringExtractor.SIMPLE_CLASS_EXTRACTOR, ObservableListExtractor.ATTRIBUTES_OF_TYPE);
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
							ObservableValue<Boolean> invalid = Bindings.createBooleanBinding(() -> propertiesInvalid.stream().map(input -> input.getValue()).filter(bool -> bool != null).reduce(false, (a, b) -> a || b),
									propertiesInvalid.stream().toArray(ObservableValue[]::new));
							return Bindings.createStringBinding(() -> Boolean.TRUE.equals(invalid.getValue()) ? ReactorStatics.DISABLED : "", invalid);
						});
						bindAction(model -> {
							Generic newInstance = model.getGeneric().setInstance(newInstanceValue.getValue());
							for (Entry<Generic, Property<Serializable>> entry : holdersValues.entrySet())
								if (entry.getValue().getValue() != null) {
									newInstance.setHolder(entry.getKey(), entry.getValue().getValue());
									entry.getValue().setValue(null);
								}
							for (Entry<Generic, List<Property<GenericModel>>> entry : componentsValues.entrySet()) {
								List<Generic> selectedGenerics = entry.getValue().stream().filter(obs -> obs.getValue() != null).map(obs -> obs.getValue().getGeneric()).filter(gen -> gen != null).collect(Collectors.toList());
								if (!selectedGenerics.isEmpty() && selectedGenerics.size() + 1 == entry.getKey().getComponents().size())
									newInstance.setHolder(entry.getKey(), null, selectedGenerics.stream().toArray(Generic[]::new));
								entry.getValue().stream().forEach(sel -> sel.setValue(null));
							}
							newInstanceValue.setValue(null);
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