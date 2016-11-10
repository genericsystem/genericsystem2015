package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import java.util.Arrays;
import java.util.stream.Collectors;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.GenericValueBackgroundColor;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDatalist;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlOption;
import org.genericsystem.reactor.model.ObservableListExtractor;

import javafx.beans.property.Property;

@Style(path = InputTextWithConversion.class, name = "flex", value = "1")
@Style(path = InputTextWithConversion.class, name = "width", value = "100%")
@Style(name = "flex", value = "1")
@Style(name = "width", value = "100%")
@GenericValueBackgroundColor(path = { HtmlDatalist.class, HtmlOption.class }, value = "")
@Children({ InputTextWithConversion.class, HtmlDatalist.class })
@Children(path = HtmlDatalist.class, value = HtmlOption.class)
@ForEach(path = { HtmlDatalist.class, HtmlOption.class }, value = ObservableListExtractor.SUBINSTANCES.class)
@BindText(path = { HtmlDatalist.class, HtmlOption.class })
public class InputWithDatalist extends FlexDiv {
	@Override
	public void init() {
		addPostfixBinding(context -> find(InputTextWithConversion.class).addAttribute(context, "list", context.getHtmlDomNode(find(HtmlDatalist.class)).getId()));
	}

	@ForEach(path = { HtmlDatalist.class, HtmlOption.class }, value = ObservableListExtractor.SUBINSTANCES_OF_META.class)
	public static class InputTextEditorWithDatalist extends InputWithDatalist implements SelectionDefaults {
		@Override
		public void init() {
			super.init();
			find(InputTextWithConversion.class).initValueProperty(model -> model.getGeneric().getValue());

			find(InputTextWithConversion.class).addConvertedValueChangeListener((context, nva) -> {
				System.out.println(Arrays.asList(context.getGenerics()).stream().map(g -> g.info()).collect(Collectors.toList()));
				Generic updatedGeneric = context.getGenerics()[1].updateComponent(context.getGeneric().getMeta().setInstance(nva), context.getGenerics()[1].getComponents().indexOf(context.getGeneric()));
				Property<Generic> genericProperty = getUpdatedGenericProperty(context);
				if (genericProperty != null)
					genericProperty.setValue(updatedGeneric);
			});
		}
	}
}
