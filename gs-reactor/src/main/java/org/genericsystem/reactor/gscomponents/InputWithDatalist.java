package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.GenericValueBackgroundColor;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDatalist;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlOption;
import org.genericsystem.reactor.gscomponents.InputTextWithConversion.InputTextEditorWithConversionForDatalist;
import org.genericsystem.reactor.model.ObservableListExtractor;

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
	@Children({ InputTextEditorWithConversionForDatalist.class, HtmlDatalist.class })
	public static class InputTextEditorWithDatalist extends InputWithDatalist implements SelectionDefaults {
	}
}
