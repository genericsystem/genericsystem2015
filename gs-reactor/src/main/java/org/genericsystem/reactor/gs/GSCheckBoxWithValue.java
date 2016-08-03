package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.TagProperty;
import org.genericsystem.reactor.TagProperty.ValueProperty;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gstag.GSCheckBox;

public class GSCheckBoxWithValue extends GSCheckBox {

	protected final TagProperty<Boolean> valueProperty;

	public GSCheckBoxWithValue(GSTag parent) {
		super(parent);
		valueProperty = createNewProperty(ValueProperty::new);
		bindOptionalBiDirectionalAttribute(valueProperty, ReactorStatics.CHECKED, ReactorStatics.CHECKED);
	}

	public static class GSCheckBoxEditor extends GSCheckBoxWithValue {

		public GSCheckBoxEditor(GSTag parent) {
			super(parent);
			initProperty(valueProperty, model -> (Boolean) model.getGeneric().getValue());
			bindActionToValueChangeListener(valueProperty, (model, nva) -> model.getGeneric().updateValue(nva));
		}
	}

	public static class GSCheckBoxDisplayer extends GSCheckBoxEditor {

		public GSCheckBoxDisplayer(GSTag parent) {
			super(parent);
			addAttribute(ReactorStatics.DISABLED, ReactorStatics.DISABLED);
		}
	}
}
