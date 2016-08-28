package org.genericsystem.reactor.gs;

import java.io.Serializable;
import java.util.List;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gs.GSHolderEditor.GSHolderBuilder;
import org.genericsystem.reactor.gs.GSSubcellDisplayer.GSAttributeBuilder;
import org.genericsystem.reactor.gstag.HtmlButton;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.modelproperties.GSBuilderDefaults;

public class GSInstanceBuilder extends GSComposite implements GSBuilderDefaults {

	private GSHolderEditor instanceValueInput;

	public GSInstanceBuilder(Tag parent, FlexDirection flexDirection) {
		super(parent, flexDirection);
		createHoldersMapProperty();
		createComponentsMapProperty();
		createInvalidListProperty();
	}

	@Override
	protected void header() {
		instanceValueInput = new GSHolderBuilder(this) {
			{
				addStyle("margin-right", "1px");
				addStyle("margin-bottom", "1px");

			}
		};
	}

	@Override
	protected void sections() {
		new GSAttributeBuilder(this, FlexDirection.ROW) {
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
				new HtmlButton(this) {
					{
						bindAttribute(ReactorStatics.DISABLED, ReactorStatics.DISABLED, model -> Bindings.createStringBinding(
								() -> Boolean.TRUE.equals(getInvalidList(model).stream().map(input -> input.getValue()).filter(bool -> bool != null).reduce(false, (a, b) -> a || b)) ? ReactorStatics.DISABLED : "",
								getInvalidList(model).stream().toArray(ObservableValue[]::new)));
						bindAction(model -> {
							Generic newInstance = model.getGeneric().setInstance(instanceValueInput.input.getConvertedValueProperty(model).getValue());
							for (Entry<Generic, Property<Serializable>> entry : getHoldersMap(model).entrySet())
								if (entry.getValue().getValue() != null) {
									newInstance.setHolder(entry.getKey(), entry.getValue().getValue());
									entry.getValue().setValue(null);
								}
							for (Entry<Generic, List<Property<Context>>> entry : getComponentsMap(model).entrySet()) {
								List<Generic> selectedGenerics = entry.getValue().stream().filter(obs -> obs.getValue() != null).map(obs -> obs.getValue().getGeneric()).filter(gen -> gen != null).collect(Collectors.toList());
								if (!selectedGenerics.isEmpty() && selectedGenerics.size() + 1 == entry.getKey().getComponents().size())
									newInstance.setHolder(entry.getKey(), null, selectedGenerics.stream().toArray(Generic[]::new));
								entry.getValue().stream().forEach(sel -> sel.setValue(null));
							}
							instanceValueInput.input.getConvertedValueProperty(model).setValue(null);
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