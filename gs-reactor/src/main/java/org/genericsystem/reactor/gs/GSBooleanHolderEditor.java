package org.genericsystem.reactor.gs;

import java.io.Serializable;
import java.util.Map;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gs.GSCheckBoxWithValue.GSCheckBoxEditor;
import org.genericsystem.reactor.gstag.GSCheckBox;
import org.genericsystem.reactor.gstag.GSHyperLink;
import org.genericsystem.reactor.model.GenericModel;

import javafx.beans.property.Property;

public class GSBooleanHolderEditor extends GSSection {

	protected GSCheckBox checkbox;

	public GSBooleanHolderEditor(GSTag parent) {
		this(parent, GSCheckBoxEditor::new);
	}

	public GSBooleanHolderEditor(GSTag parent, GSCheckBoxConstructor constructor) {
		super(parent, FlexDirection.ROW);
		addStyle("flex", "1");
		addStyle("width", "100%");
		addStyle("height", "100%");
		new GSSection(this, FlexDirection.ROW) {
			{
				addStyle("justify-content", "center");
				addStyle("align-items", "center");
				addStyle("width", "100%");
				addStyle("height", "100%");
				checkbox = constructor.build(this);
			}
		};
	}

	@FunctionalInterface
	public interface GSCheckBoxConstructor {
		GSCheckBox build(GSTag parent);
	}

	public static class GSBooleanHolderEditorWithRemoval extends GSBooleanHolderEditor {

		public GSBooleanHolderEditorWithRemoval(GSTag parent) {
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

	public static class GSBooleanHolderAdder extends GSBooleanHolderEditor {

		public GSBooleanHolderAdder(GSTag parent) {
			super(parent, GSCheckBoxWithValue::new);
			new GSHyperLink(this) {
				{
					addStyle("justify-content", "center");
					addStyle("text-decoration", "none");
					addStyle("height", "100%");
					setText("+");
					bindAction(model -> {
						Property<Boolean> observable = checkbox.getProperty(ReactorStatics.VALUE, model);
						Boolean newValue = observable.getValue();
						observable.setValue(null);
						model.getGenerics()[1].addHolder(model.getGeneric(), newValue);
					});
				}
			};
		}
	}

	public static class GSBooleanHolderCreator extends GSBooleanHolderEditor {

		public GSBooleanHolderCreator(GSTag parent) {
			super(parent, GSCheckBoxWithValue::new);
			if (parent != null && parent.getParent() != null && parent.getParent().getParent() instanceof GSInstanceCreator)
				checkbox.addPrefixBinding(model -> {
					Property<Map<Generic, Property<Serializable>>> holders = getProperty(ReactorStatics.HOLDERS_MAP, model);
					holders.getValue().put(model.getGeneric(), checkbox.getProperty(ReactorStatics.VALUE, model));
				});
		}
	}
}