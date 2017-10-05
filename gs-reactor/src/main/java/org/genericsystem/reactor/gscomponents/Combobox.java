package org.genericsystem.reactor.gscomponents;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Style.GenericValueBackgroundColor;
import org.genericsystem.reactor.annotations.TagName;
import org.genericsystem.reactor.context.ForEachExtractor;
import org.genericsystem.reactor.context.StringExtractor;
import org.genericsystem.reactor.contextproperties.ComponentsDefaults;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.gscomponents.Combobox.HtmlRepeatedOption;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlOption;

import javafx.beans.property.Property;

@Children(HtmlRepeatedOption.class)
@BindText(path = HtmlRepeatedOption.class)
@ForEach(path = HtmlRepeatedOption.class, value = ForEachExtractor.SUBINSTANCES_ALPHABETICAL_ORDER.class)
@GenericValueBackgroundColor(path = HtmlOption.class, value = "")
@TagName(TagName.SELECT)
public class Combobox extends TagImpl implements SelectionDefaults, ComponentsDefaults {

	public static class HtmlRepeatedOption extends HtmlOption {
	}

	@Override
	public void init() {
		createSelectionProperty();
		bindBiDirectionalSelection(find(HtmlOption.class));
		addPrefixBinding(context -> {
			if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(context.getGeneric()))) {
				getSelectionString(context).addListener((o, old, newValue) -> addStyle(context, "background-color", newValue));
				addStyle(context, "background-color", getSelectionString(context).getValue());
			}
		});
	}

	@Children({ HtmlOption.class, HtmlRepeatedOption.class })
	@ForEach(path = HtmlOption.class, pos = 0, value = ForEachExtractor.NO_FOR_EACH.class)
	@ForEach(path = HtmlOption.class, pos = 1, value = ForEachExtractor.SUBINSTANCES.class)
	public static class ComboboxWithEmptyEntry extends Combobox {

		@Override
		public void init() {
			setSelectionShift(1);
			createSelectionProperty();
			bindBiDirectionalSelection(find(HtmlOption.class, 1));
			addPrefixBinding(context -> {
				if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(context.getGeneric()))) {
					getSelectionString(context).addListener((o, old, newValue) -> addStyle(context, "background-color", newValue));
					addStyle(context, "background-color", getSelectionString(context).getValue());
				}
			});
		}
	}

	@ForEach(path = HtmlOption.class, value = ForEachExtractor.SUBINSTANCES_OF_META.class)
	public static class InstanceEditorCombobox extends Combobox {
		@Override
		public void init() {
			super.init();
			addPostfixBinding(model -> {
				if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric().getMeta())))
					addStyle(model, "background-color", getSelectionString(model).getValue());
			});
			addPostfixBinding(model -> getSelectionProperty(model).addListener((ov, ova, nva) -> {
				Generic updatedGeneric = model.getGenerics()[1].updateComponent(nva.getGeneric(), model.getGenerics()[1].getComponents().indexOf(model.getGeneric()));
				Property<Generic> genericProperty = getUpdatedGenericProperty(model);
				if (genericProperty != null && genericProperty.getValue() != null && genericProperty.getValue().getMeta().equals(updatedGeneric.getMeta()))
					genericProperty.setValue(updatedGeneric);
			}));
		}
	}
}
