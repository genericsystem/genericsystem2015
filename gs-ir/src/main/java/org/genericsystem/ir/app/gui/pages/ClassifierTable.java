package org.genericsystem.ir.app.gui.pages;

import org.genericsystem.ir.app.gui.pages.ClassifierTable.ContentRow;
import org.genericsystem.ir.app.gui.pages.ClassifierTable.HeaderRow;
import org.genericsystem.ir.app.gui.utils.ObservableListExtractorCustom.DOC_SELECTOR;
import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;

@Children({ HeaderRow.class, FlexDiv.class })
@Children(path = FlexDiv.class, pos = 1, value = ContentRow.class)
@ForEach(path = FlexDiv.class, pos = 1, value = DOC_SELECTOR.class)
@StyleClass(path = FlexDiv.class, pos = 1, value = "alternate-rows")
@FlexDirectionStyle(path = FlexDiv.class, pos = 1, value = FlexDirection.COLUMN)
public class ClassifierTable extends FlexDiv implements SelectionDefaults {

	@Children({ HtmlLabel.class })
	@FlexDirectionStyle(FlexDirection.ROW)
	@Attribute(path = HtmlLabel.class, name = "name", value = "title")
	@StyleClass("statistics-header-row")
	@Style(path = HtmlLabel.class, name = "justify-content", value = "center")
	@Style(path = HtmlLabel.class, name = "align-items", value = "center")
	@Style(path = HtmlLabel.class, name = "flex", value = "1")
	@Style(path = HtmlLabel.class, name = "text-align", value = "center")
	@Style(path = HtmlLabel.class, name = "font-weight", value = "bold")
	@SetText(path = HtmlLabel.class, value = { "Document's name" })
	public static class HeaderRow extends FlexDiv {

	}

	@Children({ FlexDiv.class })
	@FlexDirectionStyle(FlexDirection.ROW)
	@FlexDirectionStyle(path = FlexDiv.class, value = FlexDirection.COLUMN)
	@BindText(path = FlexDiv.class)
	@Style(path = FlexDiv.class, name = "justify-content", value = "center")
	@Style(path = FlexDiv.class, name = "align-items", value = "center")
	@Style(path = FlexDiv.class, name = "flex", value = "1")
	public static class ContentRow extends FlexDiv {

	}

}
