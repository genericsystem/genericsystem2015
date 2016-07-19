package org.genericsystem.reactor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Model.TriFunction;
import org.genericsystem.reactor.model.GenericModel;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

public class Visitor {

	public void visit(Model modelContext) {
		prefix(modelContext);
		for (Model childContext : modelContext.allSubContexts()) { // why all the subcontexts here ?
			visit(childContext);
		}
		postfix(modelContext);
	}

	public void prefix(Model modelContext) {

	}

	public void postfix(Model modelContext) {

	}

	public static class HolderVisitor extends Visitor {
		private Generic newInstance;

		@Override
		public void prefix(Model model) {
			GenericModel gModel = (GenericModel) model;
			for (Map<String, ObservableValue<Object>> properties : model.getProperties().values()) {
				if (properties.containsKey(ReactorStatics.VALUE) && properties.containsKey(ReactorStatics.ACTION) && properties.get(ReactorStatics.VALUE).getValue() != null && properties.get(ReactorStatics.ACTION).getValue() != null) {
					TriFunction<Generic[], Serializable, Generic, Generic> action = (TriFunction<Generic[], Serializable, Generic, Generic>) properties.get(ReactorStatics.ACTION).getValue();
					Generic g = action.apply(gModel.getGenerics(), (Serializable) properties.get(ReactorStatics.VALUE).getValue(), newInstance);
					if (newInstance == null)
						newInstance = g;
				}
			}
		}

		@Override
		public void postfix(Model model) {
			List<Generic> generics = new ArrayList<>();
			for (Model subModel : model.allSubContexts())
				for (Tag tag : subModel.getProperties().keySet())
					if (subModel.getProperties().get(tag).containsKey(ReactorStatics.SELECTOR_TAG))
						if (subModel.getProperty(tag, ReactorStatics.SELECTION).getValue() != null)
							generics.add(((GenericModel) subModel.getProperty(tag, ReactorStatics.SELECTION).getValue()).getGeneric());
			if (!generics.isEmpty() && generics.size() + 1 == ((GenericModel) model).getGeneric().getComponents().size()) // test OK?
				newInstance.setHolder(((GenericModel) model).getGeneric(), null, generics.stream().toArray(Generic[]::new));
		}
	}

	public static class ClearVisitor extends Visitor {
		@Override
		public void prefix(Model model) {
			for (Map<String, ObservableValue<Object>> properties : model.getProperties().values()) {
				if (properties.containsKey(ReactorStatics.VALUE))
					((Property) properties.get(ReactorStatics.VALUE)).setValue(null);
				if (properties.containsKey(ReactorStatics.SELECTION))
					((Property) properties.get(ReactorStatics.SELECTION)).setValue(null);
			}
		}
	}

	public static class CheckInputsValidityVisitor extends Visitor {
		private final List<ObservableValue> propertiesInvalid = new ArrayList<>();
		private final ObservableValue<Boolean> invalid;

		public CheckInputsValidityVisitor(Model modelContext) {
			super();
			visit(modelContext);
			invalid = Bindings.createBooleanBinding(() -> propertiesInvalid.stream().map(input -> (Boolean) input.getValue()).filter(bool -> bool != null).reduce(false, (a, b) -> a || b), propertiesInvalid.stream().toArray(ObservableValue[]::new));
		}

		public ObservableValue<Boolean> isInvalid() {
			return invalid;
		}

		@Override
		public void prefix(Model model) {
			propertiesInvalid.addAll(model.getProperties().values().stream().filter(properties -> properties.containsKey(ReactorStatics.INVALID)).map(properties -> properties.get(ReactorStatics.INVALID)).collect(Collectors.toList()));
		}
	}
}
