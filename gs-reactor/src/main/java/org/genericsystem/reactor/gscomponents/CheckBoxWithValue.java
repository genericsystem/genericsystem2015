package org.genericsystem.reactor.gscomponents;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.contextproperties.ConvertedValueDefaults;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlCheckBox;

import javafx.beans.property.Property;

public class CheckBoxWithValue extends HtmlCheckBox implements ConvertedValueDefaults {

	public CheckBoxWithValue() {
		createConvertedValueProperty();
		bindOptionalBiDirectionalAttribute(VALUE, ReactorStatics.CHECKED, ReactorStatics.CHECKED);
	}

	public static class CheckBoxEditor extends CheckBoxWithValue implements SelectionDefaults {

		@Override
		public void init() {
			initValueProperty(model -> (Boolean) model.getGeneric().getValue());
			addConvertedValueChangeListener((model, nva) -> {
				Generic updatedGeneric = model.getGeneric().updateValue(nva);
				Property<Generic> genericProperty = getUpdatedGenericProperty(model);
				if (genericProperty != null)
					genericProperty.setValue(updatedGeneric);
			});
		}
	}

	public static class CheckBoxDisplayer extends CheckBoxEditor {

		@Override
		public void init() {
			super.init();
			addAttribute(ReactorStatics.DISABLED, ReactorStatics.DISABLED);
		}
	}
}
