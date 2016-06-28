package org.genericsystem.reactor;

import java.util.ArrayList;
import java.util.List;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.model.CompositeModel;
import org.genericsystem.reactor.model.InputCompositeModel;
import org.genericsystem.reactor.model.SelectorModel;

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
			CompositeModel cModel = model.getModel();
			if (cModel instanceof InputCompositeModel) {
				InputCompositeModel icModel = (InputCompositeModel) cModel;
				Generic g = icModel.getInputAction().getValue().apply(cModel.getGenerics(),	icModel.getInputString().getValue(), newInstance);
				if (newInstance == null)
					newInstance = g;
			}
		}

		@Override
		public void postfix(ModelContext model) {
			List<Generic> generics = new ArrayList<>();
			for (ModelContext subModel : model.allSubContexts())
				if (subModel.getModel() instanceof SelectorModel)
					generics.add(((SelectorModel) subModel.getModel()).getSelection().getValue().getGeneric());
			if (!generics.isEmpty())
				newInstance.setHolder(model.<CompositeModel> getModel().getGeneric(), null,	generics.toArray(new Generic[generics.size()]));
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
}