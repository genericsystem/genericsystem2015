package org.genericsystem.reactor.gs;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gs.GSInputTextWithConversion.GSInputTextEditorWithConversion;
import org.genericsystem.reactor.gstag.GSButton;
import org.genericsystem.reactor.model.GenericModel;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

public class GSHolderEditor extends GSSection {

	protected GSInputTextWithConversion<?> input;

	public GSHolderEditor(GSTag parent) {
		this(parent, GSInputTextEditorWithConversion::new);
	}

	public GSHolderEditor(GSTag parent, GSInputTextConstructor constructor) {
		super(parent, FlexDirection.ROW);
		addStyle("flex", "1");
		addStyle("width", "100%");
		addStyle("height", "100%");
		input = constructor.build(this);
	}

	@FunctionalInterface
	public interface GSInputTextConstructor {
		GSInputTextWithConversion<?> build(GSTag parent);
	}

	public static class GSHolderEditorWithRemoval extends GSHolderEditor {

		public GSHolderEditorWithRemoval(GSTag parent) {
			super(parent);
			new GSButton(this) {
				{
					addStyle("justify-content", "center");
					addStyle("align-items", "center");
					setText("×");
					bindAction(GenericModel::remove);
				}
			};
		}
	}

	public static class GSHolderAdder extends GSHolderEditor {

		public GSHolderAdder(GSTag parent) {
			super(parent, GSInputTextWithConversion::new);
			new GSButton(this) {
				{
					addStyle("justify-content", "center");
					addStyle("align-items", "center");
					setText("+");
					bindAction(model -> {
						Property<Serializable> observable = input.getProperty(ReactorStatics.VALUE, model);
						if (observable.getValue() != null) {
							model.getGenerics()[3].addHolder(model.getGenerics()[2], observable.getValue());
							observable.setValue(null);
						}
					});
				}
			};
		}
	}

	public static class GSHolderCreator extends GSHolderEditor {

		public GSHolderCreator(GSTag parent) {
			super(parent, GSInputTextWithConversion::new);
			if (parent != null && parent.getParent() != null && parent.getParent().getParent() instanceof GSInstanceCreator) {
				input.addPrefixBinding(model -> ((Map<Generic, Property<Serializable>>) getProperty(ReactorStatics.HOLDERS_MAP, model).getValue()).put(model.getGeneric(), model.getProperty(input, ReactorStatics.VALUE)));
				input.addPrefixBinding(model -> ((List<ObservableValue<Boolean>>) getProperty(ReactorStatics.INVALID_LIST, model).getValue()).add(model.getObservableValue(input, ReactorStatics.INVALID)));
			}
		}
	}
}