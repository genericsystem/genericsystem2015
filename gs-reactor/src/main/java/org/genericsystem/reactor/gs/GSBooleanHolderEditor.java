package org.genericsystem.reactor.gs;

import java.io.Serializable;

import javafx.beans.property.Property;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gs.GSCheckBoxWithValue.GSCheckBoxEditor;
import org.genericsystem.reactor.gstag.HtmlHyperLink;
import org.genericsystem.reactor.modelproperties.GSBuilderDefaults;

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

	public static class GSBooleanHolderEditorWithRemoval extends GSBooleanHolderEditor {

		public GSBooleanHolderEditorWithRemoval(Tag parent) {
			super(parent);
			new HtmlHyperLink(this) {
				{
					addStyle("justify-content", "center");
					addStyle("text-decoration", "none");
					setText("×");
					bindAction(Context::remove);
				}
			};
		}
	}

	public static class GSBooleanHolderAdder extends GSBooleanHolderEditor {

		public GSBooleanHolderAdder(Tag parent) {
			super(parent, GSCheckBoxWithValue::new);
			checkbox.addConvertedValueChangeListener((model, nva) -> {
				if (nva != null)
					model.getGenerics()[1].addHolder(model.getGeneric(), nva);
			});
			new HtmlHyperLink(this) {
				{
					addStyle("justify-content", "center");
					addStyle("text-decoration", "none");
					setText("+");
					bindAction(model -> {
						Property<Serializable> observable = checkbox.getConvertedValueProperty(model);
						Boolean newValue = (Boolean) observable.getValue();
						observable.setValue(null);
						model.getGenerics()[1].addHolder(model.getGeneric(), newValue);
					});
				}
			};
		}
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