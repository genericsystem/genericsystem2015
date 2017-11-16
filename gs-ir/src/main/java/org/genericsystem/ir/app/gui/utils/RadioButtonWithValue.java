package org.genericsystem.ir.app.gui.utils;

import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.annotations.TagName;
import org.genericsystem.reactor.contextproperties.ConvertedValueDefaults;
import org.genericsystem.reactor.gscomponents.TagImpl;

@TagName(value = TagName.INPUT, type = TagName.RADIO)
public class RadioButtonWithValue extends TagImpl implements ConvertedValueDefaults {

	public RadioButtonWithValue() {
		createConvertedValueProperty();
		bindOptionalBiDirectionalAttribute(VALUE, ReactorStatics.CHECKED, ReactorStatics.CHECKED);
	}

	// TODO: update the fcode to match the new Model
	// public static class RadioButtonEditor extends RadioButtonWithValue implements SelectionDefaults {
	// @Override
	// public void init() {
	// initValueProperty(context -> {
	// // Get the current item (from DOC_CLASS_SELECTOR)
	// Generic currentItem = context.getGeneric();
	// // Get the docClass from the document
	// Generic currentDocClass = context.getGenerics()[2];
	// // If there is a match, that means the current item is also the current docClass
	// return currentItem == currentDocClass;
	// });
	//
	// addConvertedValueChangeListener((context, nva) -> {
	// // NB: nva will always return true, since it represents the selected element
	// DocClassInstance currentItem = (DocClassInstance) context.getGeneric();
	// // Store the current item in the context property
	// getContextProperty(HomePageTable.DOCCLASS_CONTEXT_PROPERTY, context).setValue(currentItem);
	// });
	//
	// // Fix the "name" attribute with the name of the document
	// addPrefixBinding(context -> {
	// getDomNodeAttributes(context).put("name", context.getGenerics()[1].getValue().toString());
	// });
	// }
	// }
	//
	// public static class RadioButtonDisplayer extends RadioButtonEditor {
	// @Override
	// public void init() {
	// super.init();
	// addAttribute(ReactorStatics.DISABLED, ReactorStatics.DISABLED);
	// }
	// }
}
