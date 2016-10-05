package org.genericsystem.reactor.ca_gscomponents;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.aa_modelproperties.GSBuilderDefaults;
import org.genericsystem.reactor.ca_gscomponents.GSCheckBoxWithValue.GSCheckBoxEditor;

public class GSBooleanHolderEditor extends GSDiv {

	protected GSCheckBoxWithValue checkbox;

	public GSBooleanHolderEditor(Tag parent) {
		this(parent, GSCheckBoxEditor::new);
	}

	public GSBooleanHolderEditor(Tag parent, GSCheckBoxConstructor constructor) {
		super(parent, FlexDirection.ROW);
		addStyle("flex", "1");
		new GSDiv(this, FlexDirection.ROW) {
			{
				addStyle("flex", "1");
				addStyle("justify-content", "center");
				addStyle("align-items", "center");
				checkbox = constructor.build(this);
			}
		};
	}

	@FunctionalInterface
	public interface GSCheckBoxConstructor {
		GSCheckBoxWithValue build(Tag parent);
	}

	public static class GSBooleanHolderBuilder extends GSBooleanHolderEditor implements GSBuilderDefaults {

		public GSBooleanHolderBuilder(Tag parent) {
			super(parent, GSCheckBoxWithValue::new);
			checkbox.addPrefixBinding(model -> {
				if (getHoldersMapProperty(model) != null)
					getHoldersMapProperty(model).getValue().put(model.getGeneric(), checkbox.getConvertedValueProperty(model));
			});
		}
	}
}