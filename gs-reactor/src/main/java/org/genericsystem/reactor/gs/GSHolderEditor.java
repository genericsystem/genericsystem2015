package org.genericsystem.reactor.gs;

import java.io.Serializable;

import javafx.beans.property.Property;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gs.GSInputTextWithConversion.GSInputTextEditorWithConversion;
import org.genericsystem.reactor.gstag.HtmlHyperLink;
import org.genericsystem.reactor.modelproperties.GSBuilderDefaults;

public class GSHolderEditor extends GSSection {

	protected GSInputTextWithConversion<?> input;

	public GSHolderEditor(Tag parent) {
		this(parent, GSInputTextEditorWithConversion::new);
	}

	public GSHolderEditor(Tag parent, GSInputTextConstructor constructor) {
		super(parent, FlexDirection.ROW);
		addStyle("flex", "1");
		input = constructor.build(this);
	}

	@FunctionalInterface
	public interface GSInputTextConstructor {
		GSInputTextWithConversion<?> build(Tag parent);
	}

	public static class GSHolderEditorWithRemoval extends GSHolderEditor {

		public GSHolderEditorWithRemoval(Tag parent) {
			super(parent);
			new HtmlHyperLink(this) {
				{
					addStyle("justify-content", "center");
					addStyle("text-decoration", "none");
					addStyle("height", "100%");
					setText("×");
					bindAction(Context::remove);
				}
			};
		}
	}

	public static class GSHolderAdder extends GSHolderEditor {

		public GSHolderAdder(Tag parent) {
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

		public GSHolderBuilder(Tag parent) {
			super(parent, GSInputTextWithConversion::new);
			input.addPrefixBinding(model -> {
				if (getHoldersMapProperty(model) != null)
					getHoldersMapProperty(model).getValue().put(model.getGeneric(), input.getConvertedValueProperty(model));
				if (getInvalidListProperty(model) != null)
					getInvalidListProperty(model).getValue().add(input.getInvalidObservable(model));
			});
		}
	}
}
