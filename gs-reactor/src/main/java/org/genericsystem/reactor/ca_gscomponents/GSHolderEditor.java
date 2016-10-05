package org.genericsystem.reactor.ca_gscomponents;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.aa_modelproperties.GSBuilderDefaults;
import org.genericsystem.reactor.ca_gscomponents.GSInputTextWithConversion.GSInputTextEditorWithConversion;

public class GSHolderEditor extends GSDiv {

	protected GSInputTextWithConversion<?> input;

	public GSHolderEditor() {
		this(GSInputTextEditorWithConversion::new);
	}

	public GSHolderEditor(Tag parent) {
		this(parent, GSInputTextEditorWithConversion::new);
	}

	public GSHolderEditor(GSInputTextConstructor constructor) {
		super(FlexDirection.ROW);
		addStyle("flex", "1");
		input = constructor.build(this);
		input.addStyle("flex", "1");
	}

	public GSHolderEditor(Tag parent, GSInputTextConstructor constructor) {
		super(parent, FlexDirection.ROW);
		addStyle("flex", "1");
		input = constructor.build(this);
		input.addStyle("flex", "1");
	}

	@FunctionalInterface
	public interface GSInputTextConstructor {
		GSInputTextWithConversion<?> build(Tag parent);
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
