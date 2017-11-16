package org.genericsystem.ir.app.gui.utils;

import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlImg;

import javafx.beans.property.SimpleStringProperty;

/**
 * This class define how the document's image is displayed.
 */
@Style(name = "margin", value = "0.5em")
@Style(name = "flex", value = "0 0 auto")
@Style(name = "justify-content", value = "center")
@Style(name = "align-items", value = "center")
public class DocumentImage extends HtmlImg {
	@Override
	public void init() {
		bindAttribute("src", "imgadr", context -> new SimpleStringProperty((String) context.getGeneric().getValue()));
	}
}
