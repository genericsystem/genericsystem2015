package org.genericsystem.reactor;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import javafx.beans.binding.Bindings;
import javafx.beans.value.ObservableValue;

public class Visitor {

	public void visit(Model modelContext) {
		prefix(modelContext);
		for (Model childContext : modelContext.subContexts())
			visit(childContext);
		postfix(modelContext);
	}

	public void prefix(Model modelContext) {

	}

	public void postfix(Model modelContext) {

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
			propertiesInvalid.addAll(model.getPropertiesMaps().stream().filter(properties -> properties.containsKey(ReactorStatics.INVALID)).map(properties -> properties.get(ReactorStatics.INVALID)).collect(Collectors.toList()));
		}
	}
}
