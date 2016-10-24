package org.genericsystem.reactor.gscomponents3;

import org.genericsystem.reactor.modelproperties.ComponentsDefaults;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import org.genericsystem.reactor.htmltag.HtmlOption;
import org.genericsystem.reactor.htmltag.HtmlSelect;

import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style.GenericValueBackgroundColor;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

@Children(HtmlOption.class)
@BindText(path = HtmlOption.class)
@ForEach(path = HtmlOption.class, value = ObservableListExtractor.SUBINSTANCES.class)
@GenericValueBackgroundColor(path = HtmlOption.class, value = "")
public class GSSelect extends HtmlSelect implements SelectionDefaults, ComponentsDefaults {
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

	@Children({ HtmlOption.class, HtmlOption.class })
	@SetText(path = HtmlOption.class, pos = 0, value = "")
	@ForEach(path = HtmlOption.class, pos = 0, value = ObservableListExtractor.NO_FOR_EACH.class)
	@ForEach(path = HtmlOption.class, pos = 1, value = ObservableListExtractor.SUBINSTANCES.class)
	public static class GSSelectWithEmptyEntry extends GSSelect {
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

	@ForEach(path = HtmlOption.class, value = ObservableListExtractor.SUBINSTANCES_OF_META.class)
	public static class InstanceCompositeSelect extends GSSelect {
		@Override
		public void init() {
			super.init();
			addPostfixBinding(model -> {
				if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric().getMeta())))
					addStyle(model, "background-color", getSelectionString(model).getValue());
			});
			addPostfixBinding(model -> getSelectionProperty(model).addListener((ov, ova, nva) -> model.getGenerics()[1].updateComponent(nva.getGeneric(), model.getGenerics()[1].getComponents().indexOf(model.getGeneric()))));
		}
	}
}
