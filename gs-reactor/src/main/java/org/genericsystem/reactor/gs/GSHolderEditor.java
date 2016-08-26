package org.genericsystem.reactor.gs;

import java.io.Serializable;

import org.genericsystem.reactor.gs.GSInputTextWithConversion.GSInputTextEditorWithConversion;
import org.genericsystem.reactor.gstag.HtmlHyperLink;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.modelproperties.GSBuilderDefaults;

import javafx.beans.property.Property;

public class GSHolderEditor extends GSSection {

	protected GSInputTextWithConversion<?> input;

	public GSHolderEditor(GSTag parent) {
		this(parent, GSInputTextEditorWithConversion::new);
	}

	public GSHolderEditor(GSTag parent, GSInputTextConstructor constructor) {
		super(parent, FlexDirection.ROW);
		addStyle("flex", "1");

		input = constructor.build(this);
	}

	@FunctionalInterface
	public interface GSInputTextConstructor {
		GSInputTextWithConversion<?> build(GSTag parent);
	}

	public static class GSHolderEditorWithRemoval extends GSHolderEditor {

		public GSHolderEditorWithRemoval(GSTag parent) {
			super(parent);
			new HtmlHyperLink(this) {
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
			input.addConvertedValueChangeListener((model, nva) -> {
				if (nva != null)
					model.getGenerics()[1].addHolder(model.getGeneric(), nva);
			});
			new HtmlHyperLink(this) {
				{
					addStyle("justify-content", "center");
					addStyle("text-decoration", "none");
					addStyle("height", "100%");
					setText("+");
					bindAction(model -> {
						Property<Serializable> observable = input.getConvertedValueProperty(model);
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

	public static class GSHolderBuilder extends GSHolderEditor implements GSBuilderDefaults {

		public GSHolderBuilder(GSTag parent) {
			super(parent, GSInputTextWithConversion::new);
			if (parent != null && parent.getParent() != null && parent.getParent().getParent() instanceof GSInstanceBuilder) {
				input.addPrefixBinding(model -> {
					getHoldersMap(model).put(model.getGeneric(), input.getConvertedValueProperty(model));
					getInvalidList(model).add(input.getInvalidObservable(model));
				});
			}
		}
	}
}
