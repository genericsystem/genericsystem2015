package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gstag.HtmlCheckBox;
import org.genericsystem.reactor.modelproperties.ConvertedValueDefaults;

public class GSCheckBoxWithValue extends HtmlCheckBox implements ConvertedValueDefaults {

	public GSCheckBoxWithValue(Tag parent) {
		super(parent);
		createConvertedValueProperty();
		bindOptionalBiDirectionalAttribute(VALUE, ReactorStatics.CHECKED, ReactorStatics.CHECKED);
	}

	public static class GSCheckBoxEditor extends GSCheckBoxWithValue {

		public GSCheckBoxEditor(Tag parent) {
			super(parent);
			initValueProperty(model -> (Boolean) model.getGeneric().getValue());
			addConvertedValueChangeListener((model, nva) -> model.getGeneric().updateValue(nva));
		}
	}

	public static class GSCheckBoxDisplayer extends GSCheckBoxEditor {

		public GSCheckBoxDisplayer(Tag parent) {
			super(parent);
			addAttribute(ReactorStatics.DISABLED, ReactorStatics.DISABLED);
		}
	}
}
