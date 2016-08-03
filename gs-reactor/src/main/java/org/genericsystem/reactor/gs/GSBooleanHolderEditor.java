package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.gs.GSCheckBoxWithValue.GSCheckBoxEditor;
import org.genericsystem.reactor.gstag.GSButton;
import org.genericsystem.reactor.model.GenericModel;

public class GSBooleanHolderEditor extends GSSection {

	protected GSCheckBoxWithValue checkbox;

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
		GSCheckBoxWithValue build(GSTag parent);
	}

	public static class GSBooleanHolderEditorWithRemoval extends GSBooleanHolderEditor {

		public GSBooleanHolderEditorWithRemoval(GSTag parent) {
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

	public static class GSBooleanHolderAdder extends GSBooleanHolderEditor {

		public GSBooleanHolderAdder(GSTag parent) {
			super(parent, GSCheckBoxWithValue::new);
			new GSButton(this) {
				{
					addStyle("justify-content", "center");
					addStyle("align-items", "center");
					setText("+");
					bindAction(model -> {
						model.getGenerics()[3].addHolder(model.getGenerics()[2], checkbox.valueProperty.getValue(model.getGenerics()[2]));
						checkbox.valueProperty.setValue(model.getGenerics()[2], null);
					});
				}
			};
		}
	}

	public static class GSBooleanHolderCreator extends GSBooleanHolderEditor {

		public GSBooleanHolderCreator(GSTag parent) {
			super(parent, GSCheckBoxWithValue::new);
			if (parent != null && parent.getParent() != null && parent.getParent().getParent() instanceof GSInstanceCreator)
				checkbox.addPrefixBinding(model -> ((GSInstanceCreator) parent.getParent().getParent()).getHoldersValues().put(model.getGeneric(), checkbox.valueProperty.getProperty(model.getGeneric())));
		}
	}
}