package org.genericsystem.reactor.gs;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gs.GSInputTextWithConversion.GSInputTextEditorWithConversion;
import org.genericsystem.reactor.gstag.GSHyperLink;
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
			new GSHyperLink(this) {
				{
					addStyle("justify-content", "center");
					addStyle("text-decoration", "none");
					addStyle("height", "100%");
					setText("×");
					bindAction(GenericModel::remove);
				}
			};
		}
	}

	public static class GSHolderAdder extends GSHolderEditor {

		public GSHolderAdder(GSTag parent) {
			super(parent, GSInputTextWithConversion::new);
			new GSHyperLink(this) {
				{
					addStyle("justify-content", "center");
					addStyle("text-decoration", "none");
					addStyle("height", "100%");
					setText("+");
					bindAction(model -> {
						Property<Serializable> observable = input.getProperty(ReactorStatics.VALUE, model);
						if (observable.getValue() != null) {
							Serializable newValue = observable.getValue();
							observable.setValue(null);
							model.getGenerics()[1].addHolder(model.getGeneric(), newValue);
						}
					});
				}
			};
		}
	}

	public static class GSHolderBuilder extends GSHolderEditor {

		public GSHolderBuilder(GSTag parent) {
			super(parent, GSInputTextWithConversion::new);
			if (parent != null && parent.getParent() != null && parent.getParent().getParent() instanceof GSInstanceBuilder) {
				input.addPrefixBinding(model -> {
					Property<Map<Generic, Property<Serializable>>> holders = getProperty(ReactorStatics.HOLDERS_MAP, model);
					holders.getValue().put(model.getGeneric(), input.getProperty(ReactorStatics.VALUE, model));
					Property<List<ObservableValue<Boolean>>> invalids = getProperty(ReactorStatics.INVALID_LIST, model);
					invalids.getValue().add(input.getObservableValue(ReactorStatics.INVALID, model));
				});
			}
		}
	}
}
