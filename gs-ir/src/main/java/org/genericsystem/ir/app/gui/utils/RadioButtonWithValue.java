package org.genericsystem.ir.app.gui.utils;

import org.genericsystem.common.Generic;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.ir.app.gui.pages.HomePageTable;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.annotations.TagName;
import org.genericsystem.reactor.contextproperties.ConvertedValueDefaults;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.gscomponents.TagImpl;

@TagName(value = TagName.INPUT, type = TagName.RADIO)
public class RadioButtonWithValue extends TagImpl implements ConvertedValueDefaults {

	public RadioButtonWithValue() {
		createConvertedValueProperty();
		bindOptionalBiDirectionalAttribute(VALUE, ReactorStatics.CHECKED, ReactorStatics.CHECKED);
	}

	public static class RadioButtonEditor extends RadioButtonWithValue implements SelectionDefaults {
		@Override
		public void init() {
			initValueProperty(context -> {
				// Get the current item (from DOC_CLASS_SELECTOR)
				Generic currentItem = context.getGeneric();
				// Get the docClass from the document
				Generic currentDocClass = context.getGenerics()[2];
				// If there is a match, that means the current item is also the current docClass
				System.out.println(String.format("%s\n--- item: %s\n--- currentDocClass: %s\n--- boolean: %s\n", context.getGenerics()[1], currentItem, currentDocClass, currentItem == currentDocClass));
				return currentItem == currentDocClass;
			});

			addConvertedValueChangeListener((context, nva) -> {
				// NB: nva will always return true, since it represents the selected element
				System.out.println("---- value change listener: ");
				// Get the selected item, the current doc and its docClass
				DocClassInstance currentItem = (DocClassInstance) context.getGeneric();
				DocInstance currentDoc = (DocInstance) context.getGenerics()[1];
				DocClassInstance currentDocClass = (DocClassInstance) context.getGenerics()[2];

				getContextProperty(HomePageTable.DOCCLASS_CONTEXT_PROPERTY, context).setValue(currentItem);

				System.out.println(String.format("currentitem: %s\ncurrentDoc: %s\ncurrentDocClass: %s", currentItem, currentDoc, currentDocClass));
				System.out.println("----");
			});

			// Fix the "name" attribute with the name of the document
			addPrefixBinding(context -> {
				// Arrays.asList(context.getGenerics()).forEach(g -> System.out.println(g.info()));
				getDomNodeAttributes(context).put("name", context.getGenerics()[1].getValue().toString());

			});
		}
	}

	public static class RadioButtonDisplayer extends RadioButtonEditor {
		@Override
		public void init() {
			super.init();
			addAttribute(ReactorStatics.DISABLED, ReactorStatics.DISABLED);
		}
	}
}
