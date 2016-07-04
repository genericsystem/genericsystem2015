package org.genericsystem.reactor;

import java.util.ArrayList;
import java.util.List;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.InputCompositeModel;
import org.genericsystem.reactor.model.SelectorModel;

import javafx.beans.binding.Bindings;
import javafx.beans.value.ObservableValue;

public class Visitor {

	public void visit(ModelContext modelContext) {
		prefix(modelContext);
		for (ModelContext childContext : modelContext.allSubContexts()) {
			visit(childContext);
		}
		postfix(modelContext);
	}

	public void prefix(ModelContext modelContext) {

	}

	public void postfix(ModelContext modelContext) {

	}

	public static class HolderVisitor extends Visitor {
		private Generic newInstance;

		@Override
		public void prefix(ModelContext model) {
			GenericModel cModel = model.getModel();
			if (cModel instanceof InputCompositeModel) {
				InputCompositeModel icModel = (InputCompositeModel) cModel;
				if (icModel.getValue() != null) {
					Generic g = icModel.getInputAction().getValue().apply(cModel.getGenerics(),	icModel.getValue(), newInstance);
					if (newInstance == null)
						newInstance = g;
				}
			}
		}

		@Override
		public void postfix(ModelContext model) {
			List<Generic> generics = new ArrayList<>();
			boolean createLink = true;
			for (ModelContext subModel : model.allSubContexts())
				if (subModel.getModel() instanceof SelectorModel) {
					GenericModel value = ((SelectorModel) subModel.getModel()).getSelection().getValue();
					if (value != null)
						generics.add(value.getGeneric());
					else
						createLink = false;
			}
			if (createLink && !generics.isEmpty())
				newInstance.setHolder(model.<GenericModel> getModel().getGeneric(), null, generics.stream().toArray(Generic[]::new));
		}
	}

	public static class ClearVisitor extends Visitor {
		@Override
		public void prefix(ModelContext model) {
			if (model.getModel() instanceof InputCompositeModel)
				((InputCompositeModel) model.getModel()).getInputString().setValue(null);
			if (model.getModel() instanceof SelectorModel)
				((SelectorModel) model.getModel()).getSelection().setValue(null);
		}
	}

	public static class CheckInputsValidityVisitor extends Visitor {
		private final List<InputCompositeModel> inputModels = new ArrayList<>();
		private final ObservableValue<Boolean> invalid;

		public CheckInputsValidityVisitor(ModelContext modelContext) {
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
		public void prefix(ModelContext model) {
			if (model.getModel() instanceof InputCompositeModel) {
				inputModels.add((InputCompositeModel) model.getModel());
			}
		}
	}
}