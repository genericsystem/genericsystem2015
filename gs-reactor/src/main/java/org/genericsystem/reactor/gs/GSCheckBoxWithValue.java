package org.genericsystem.reactor.gs;

import java.io.Serializable;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Model.TriFunction;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gstag.GSCheckBox;

public class GSCheckBoxWithValue extends GSCheckBox {

	public GSCheckBoxWithValue(GSTag parent) {
		super(parent);
		createNewProperty(ReactorStatics.VALUE);
		bindOptionalBiDirectionalAttribute(ReactorStatics.VALUE, ReactorStatics.CHECKED, ReactorStatics.CHECKED);
	}

	public static class GSCheckBoxEditor extends GSCheckBoxWithValue {

		public GSCheckBoxEditor(GSTag parent) {
			super(parent);
			initProperty(ReactorStatics.VALUE, model -> (Boolean) model.getGeneric().getValue());
			bindActionToValueChangeListener(ReactorStatics.VALUE, (model, nva) -> model.getGeneric().updateValue(nva));
		}
	}

	public static class GSCheckBoxCreator extends GSCheckBoxWithValue {

		public GSCheckBoxCreator(GSTag parent) {
			super(parent);
			createNewProperty(ReactorStatics.ACTION);
			this.<TriFunction<Generic[], Serializable, Generic, Generic>> initProperty(ReactorStatics.ACTION, (gs, value, g) -> g.setHolder(gs[0], value));
		}
	}
}
