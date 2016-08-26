package org.genericsystem.reactor.gs;

import java.io.Serializable;

import org.genericsystem.reactor.gs.GSCheckBoxWithValue.GSCheckBoxEditor;
import org.genericsystem.reactor.gstag.HtmlHyperLink;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.modelproperties.GSBuilderDefaults;

import javafx.beans.property.Property;

public class GSBooleanHolderEditor extends GSSection {

	protected GSCheckBoxWithValue checkbox;

	public GSBooleanHolderEditor(GSTag parent) {
		this(parent, GSCheckBoxEditor::new);
	}

	public GSBooleanHolderEditor(GSTag parent, GSCheckBoxConstructor constructor) {
		super(parent, FlexDirection.ROW);
		addStyle("flex", "1");
		new GSSection(this, FlexDirection.ROW) {
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
		GSCheckBoxWithValue build(GSTag parent);
	}

	public static class GSBooleanHolderEditorWithRemoval extends GSBooleanHolderEditor {

		public GSBooleanHolderEditorWithRemoval(GSTag parent) {
			super(parent);
			new HtmlHyperLink(this) {
				{
					addStyle("justify-content", "center");
					addStyle("text-decoration", "none");
					setText("×");
					bindAction(GenericModel::remove);
				}
			};
		}
	}

	public static class GSBooleanHolderAdder extends GSBooleanHolderEditor {

		public GSBooleanHolderAdder(GSTag parent) {
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

		public GSBooleanHolderBuilder(GSTag parent) {
			super(parent, GSCheckBoxWithValue::new);
			if (parent != null && parent.getParent() != null && parent.getParent().getParent() instanceof GSInstanceBuilder)
				checkbox.addPrefixBinding(model -> getHoldersMap(model).put(model.getGeneric(), checkbox.getConvertedValueProperty(model)));
		}
	}
}