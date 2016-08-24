package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gstag.HtmlCheckBox;

public class GSCheckBoxWithValue extends HtmlCheckBox {

	public GSCheckBoxWithValue(GSTag parent) {
		super(parent);
		createNewProperty(ReactorStatics.VALUE);
		bindOptionalBiDirectionalAttribute(ReactorStatics.VALUE, ReactorStatics.CHECKED, ReactorStatics.CHECKED);
	}

	public static class GSCheckBoxEditor extends GSCheckBoxWithValue {

		public GSCheckBoxEditor(GSTag parent) {
			super(parent);
			initProperty(ReactorStatics.VALUE, model -> (Boolean) model.getGeneric().getValue());
			addPropertyChangeListener(ReactorStatics.VALUE, (model, nva) -> model.getGeneric().updateValue(nva));
		}
	}

	public static class GSCheckBoxDisplayer extends GSCheckBoxEditor {

		public GSCheckBoxDisplayer(GSTag parent) {
			super(parent);
			addAttribute(ReactorStatics.DISABLED, ReactorStatics.DISABLED);
		}
	}
}
