package org.genericsystem.ir.app.gui.pages;

import org.genericsystem.ir.app.gui.pages.DocZonesShow.TextDiv;
import org.genericsystem.ir.app.gui.utils.ContextActionCustom.REFRESH_BEST_TEXT;
import org.genericsystem.ir.app.gui.utils.DocumentImage;
import org.genericsystem.ir.app.gui.utils.ObservableListExtractorCustom.ZONE_SELECTOR_BEST;
import org.genericsystem.ir.app.gui.utils.TextBindingCustom.LAST_REFRESH_UPDATE_LABEL;
import org.genericsystem.ir.app.gui.utils.TextBindingCustom.ZONE_LABEL;
import org.genericsystem.ir.app.gui.utils.TextBindingCustom.ZONE_TEXT;
import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.InheritStyle;
import org.genericsystem.reactor.annotations.SelectContext;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.context.ContextAction.CANCEL;
import org.genericsystem.reactor.context.ContextAction.RESET_SELECTION;
import org.genericsystem.reactor.context.ObservableContextSelector.SELECTION_SELECTOR;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;
import org.genericsystem.reactor.gscomponents.Modal.ModalEditor;

@Children(FlexDiv.class)
@Children(path = FlexDiv.class, value = { HtmlHyperLink.class, TextDiv.class })
@InheritStyle("background-color")
@Style(path = FlexDiv.class, name = "max-height", value = "fit-content")
@Style(path = FlexDiv.class, name = "width", value = "inherit")
@BindAction(path = { FlexDiv.class, HtmlHyperLink.class }, value = RESET_SELECTION.class)
public class DocZonesShow extends ModalEditor {

	@FlexDirectionStyle(FlexDirection.COLUMN)
	@FlexDirectionStyle(path = FlexDiv.class, value = FlexDirection.ROW)
	@Children({ FlexDiv.class, FlexDiv.class, FlexDiv.class })
	@Children(path = FlexDiv.class, pos = 1, value = { FlexDiv.class, FlexDiv.class })
	@Children(path = { FlexDiv.class, FlexDiv.class }, pos = { 1, 0 }, value = { DocumentImage.class, LastUpdate.class })
	@Children(path = FlexDiv.class, pos = 2, value = { RefreshButton.class, CloseButton.class })
	@Children(path = { FlexDiv.class, FlexDiv.class }, pos = { 1, 1 }, value = ZoneTextDiv.class)
	@BindText(path = FlexDiv.class, pos = 0)
	@StyleClass(path = FlexDiv.class, pos = 0, value = "doc-title")
	@Style(path = FlexDiv.class, pos = 2, name = "justify-content", value = "center")
	@Style(path = FlexDiv.class, pos = 2, name = "align-items", value = "center")
	@SelectContext(path = FlexDiv.class, pos = 0, value = SELECTION_SELECTOR.class)
	@SelectContext(path = FlexDiv.class, pos = 1, value = SELECTION_SELECTOR.class)
	@SelectContext(path = { FlexDiv.class, FlexDiv.class }, pos = { 1, -1 }, value = SELECTION_SELECTOR.class)
	public static class TextDiv extends FlexDiv {

	}

	@SetText("Refresh")
	@BindAction(value = REFRESH_BEST_TEXT.class)
	public static class RefreshButton extends HtmlButton {
		// Run the best text selection algorithm
	}

	@SetText("Close")
	@BindAction(value = { CANCEL.class, RESET_SELECTION.class })
	public static class CloseButton extends HtmlButton {
		// Close the window
	}

	@FlexDirectionStyle(FlexDirection.COLUMN)
	@Children(ZoneLabelField.class)
	@ForEach(ZONE_SELECTOR_BEST.class)
	public static class ZoneTextDiv extends FlexDiv {
		// For each zone, create a div with label + inputText and create a div for the results for all filters
	}

	@FlexDirectionStyle(FlexDirection.ROW)
	@Children({ ZoneLabel.class, ZoneField.class })
	public static class ZoneLabelField extends FlexDiv {

	}

	@BindText(ZONE_LABEL.class)
	@Attribute(name = "name", value = "zone")
	public static class ZoneLabel extends FlexDiv {
		// Define the zone label in normal mode
	}

	@BindText(ZONE_TEXT.class)
	@StyleClass("input-like")
	public static class ZoneField extends FlexDiv {
		// Define the div containing the OCR text
	}

	@BindText(LAST_REFRESH_UPDATE_LABEL.class)
	@Style(name = "margin", value = "0.5em")
	@Style(name = "flex", value = "0 0 auto")
	@Style(name = "justify-content", value = "center")
	@Style(name = "align-items", value = "center")
	public static class LastUpdate extends FlexDiv {
		// Print the timestamp of the last refresh
	}

}
