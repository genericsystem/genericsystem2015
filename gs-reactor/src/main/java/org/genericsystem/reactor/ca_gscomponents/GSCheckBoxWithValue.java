package org.genericsystem.reactor.ca_gscomponents;

import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.aa_modelproperties.ConvertedValueDefaults;
import org.genericsystem.reactor.ba_htmltag.HtmlCheckBox;

public class GSCheckBoxWithValue extends HtmlCheckBox implements ConvertedValueDefaults {

	public GSCheckBoxWithValue() {
		createConvertedValueProperty();
		bindOptionalBiDirectionalAttribute(VALUE, ReactorStatics.CHECKED, ReactorStatics.CHECKED);
	}

	public GSCheckBoxWithValue(Tag parent) {
		super(parent);
		createConvertedValueProperty();
		bindOptionalBiDirectionalAttribute(VALUE, ReactorStatics.CHECKED, ReactorStatics.CHECKED);
	}

	public static class GSCheckBoxEditor extends GSCheckBoxWithValue {

		public GSCheckBoxEditor() {
			initValueProperty(model -> (Boolean) model.getGeneric().getValue());
			addConvertedValueChangeListener((model, nva) -> model.getGeneric().updateValue(nva));
		}

		public GSCheckBoxEditor(Tag parent) {
			super(parent);
			initValueProperty(model -> (Boolean) model.getGeneric().getValue());
			addConvertedValueChangeListener((model, nva) -> model.getGeneric().updateValue(nva));
		}
	}

	public static class GSCheckBoxDisplayer extends GSCheckBoxEditor {

		public GSCheckBoxDisplayer() {
			addAttribute(ReactorStatics.DISABLED, ReactorStatics.DISABLED);
		}

		public GSCheckBoxDisplayer(Tag parent) {
			super(parent);
			addAttribute(ReactorStatics.DISABLED, ReactorStatics.DISABLED);
		}
	}
}
