package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.modelproperties.ConvertedValueDefaults;

import org.genericsystem.reactor.htmltag.HtmlCheckBox;

import org.genericsystem.reactor.ReactorStatics;

public class GSCheckBoxWithValue extends HtmlCheckBox implements ConvertedValueDefaults {

	public GSCheckBoxWithValue() {
		createConvertedValueProperty();
		bindOptionalBiDirectionalAttribute(VALUE, ReactorStatics.CHECKED, ReactorStatics.CHECKED);
	}

	public static class GSCheckBoxEditor extends GSCheckBoxWithValue {

		public GSCheckBoxEditor() {
			initValueProperty(model -> (Boolean) model.getGeneric().getValue());
			addConvertedValueChangeListener((model, nva) -> model.getGeneric().updateValue(nva));
		}
	}

	public static class GSCheckBoxDisplayer extends GSCheckBoxEditor {

		public GSCheckBoxDisplayer() {
			addAttribute(ReactorStatics.DISABLED, ReactorStatics.DISABLED);
		}
	}
}
