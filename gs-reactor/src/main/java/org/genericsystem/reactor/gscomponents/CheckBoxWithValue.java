package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.modelproperties.ConvertedValueDefaults;

import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlCheckBox;

public class CheckBoxWithValue extends HtmlCheckBox implements ConvertedValueDefaults {

	public CheckBoxWithValue() {
		createConvertedValueProperty();
		bindOptionalBiDirectionalAttribute(VALUE, ReactorStatics.CHECKED, ReactorStatics.CHECKED);
	}

	public static class CheckBoxEditor extends CheckBoxWithValue {

		public CheckBoxEditor() {
			initValueProperty(model -> (Boolean) model.getGeneric().getValue());
			addConvertedValueChangeListener((model, nva) -> model.getGeneric().updateValue(nva));
		}
	}

	public static class CheckBoxDisplayer extends CheckBoxEditor {

		public CheckBoxDisplayer() {
			addAttribute(ReactorStatics.DISABLED, ReactorStatics.DISABLED);
		}
	}
}
