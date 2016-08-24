package org.genericsystem.reactor.gs;

import java.io.Serializable;
import java.util.Map;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gs.GSCheckBoxWithValue.GSCheckBoxEditor;
import org.genericsystem.reactor.gstag.HtmlCheckBox;
import org.genericsystem.reactor.gstag.HtmlHyperLink;
import org.genericsystem.reactor.model.GenericModel;

import javafx.beans.property.Property;

public class GSBooleanHolderEditor extends GSSection {

	protected HtmlCheckBox checkbox;

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
		HtmlCheckBox build(GSTag parent);
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
			checkbox.addPropertyChangeListener(ReactorStatics.VALUE, (model, nva) -> {
				if (nva != null)
					model.getGenerics()[1].addHolder(model.getGeneric(), nva);
			});
			new HtmlHyperLink(this) {
				{
					addStyle("justify-content", "center");
					addStyle("text-decoration", "none");
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

	public static class GSBooleanHolderBuilder extends GSBooleanHolderEditor {

		public GSBooleanHolderBuilder(GSTag parent) {
			super(parent, GSCheckBoxWithValue::new);
			if (parent != null && parent.getParent() != null && parent.getParent().getParent() instanceof GSInstanceBuilder)
				checkbox.addPrefixBinding(model -> {
					Property<Map<Generic, Property<Serializable>>> holders = getProperty(ReactorStatics.HOLDERS_MAP, model);
					holders.getValue().put(model.getGeneric(), checkbox.getProperty(ReactorStatics.VALUE, model));
				});
		}
	}
}