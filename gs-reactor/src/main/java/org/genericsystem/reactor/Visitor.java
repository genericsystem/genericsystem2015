package org.genericsystem.reactor;

import java.util.ArrayList;
import java.util.List;

import javafx.beans.binding.Bindings;
import javafx.beans.value.ObservableValue;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.InputGenericModel;
import org.genericsystem.reactor.model.SelectorModel;

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
			GenericModel cModel = (GenericModel) model;
			if (cModel instanceof InputGenericModel) {
				InputGenericModel icModel = (InputGenericModel) cModel;
				if (icModel.getValue() != null) {
					Generic g = icModel.getInputAction().getValue().apply(cModel.getGenerics(), icModel.getValue(), newInstance);
					if (newInstance == null)
						newInstance = g;
				}
			}
		}

		@Override
		public void postfix(Model model) {
			List<Generic> generics = new ArrayList<>();
			boolean createLink = true;
			for (Model subModel : model.allSubContexts())
				if (subModel instanceof SelectorModel) {
					GenericModel value = ((SelectorModel) subModel).getSelection().getValue();
					if (value != null)
						generics.add(value.getGeneric());
					else
						createLink = false;
				}
			if (createLink && !generics.isEmpty())
				newInstance.setHolder(((GenericModel) model).getGeneric(), null, generics.stream().toArray(Generic[]::new));
		}
	}

	public static class ClearVisitor extends Visitor {
		@Override
		public void prefix(Model model) {
			if (model instanceof InputGenericModel)
				((InputGenericModel) model).getInputString().setValue(null);
			if (model instanceof SelectorModel)
				((SelectorModel) model).getSelection().setValue(null);
		}
	}

	public static class CheckInputsValidityVisitor extends Visitor {
		private final List<InputGenericModel> inputModels = new ArrayList<>();
		private final ObservableValue<Boolean> invalid;

		public CheckInputsValidityVisitor(Model modelContext) {
			super();
			visit(modelContext);
			invalid = Bindings.createBooleanBinding(() -> checkInvalidity(), inputModels.stream().map(inputModel -> inputModel.getInputString()).toArray(ObservableValue[]::new));
		}

		public ObservableValue<Boolean> isInvalid() {
			return invalid;
		}

		private Boolean checkInvalidity() {
			return inputModels.stream().map(inputModel -> inputModel.getInvalid().getValue()).reduce(false, (a, b) -> a || b);
		}

		@Override
		public void prefix(Model model) {
			if (model instanceof InputGenericModel) {
				inputModels.add((InputGenericModel) model);
			}
		}
	}
}