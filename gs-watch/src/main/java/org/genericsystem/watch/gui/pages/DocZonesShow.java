package org.genericsystem.watch.gui.pages;

import org.apache.commons.lang3.StringEscapeUtils;
import org.genericsystem.common.Root;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.Doc.RefreshTimestamp;
import org.genericsystem.cv.model.Doc.RefreshTimestamp.RefreshTimestampInstance;
import org.genericsystem.cv.model.ModelTools;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
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
import org.genericsystem.reactor.context.TextBinding;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlImg;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.reactor.gscomponents.Modal.ModalEditor;
import org.genericsystem.watch.gui.pages.DocZonesShow.TextDiv;
import org.genericsystem.watch.gui.utils.ContextActionCustom.REFRESH_BEST_TEXT;
import org.genericsystem.watch.gui.utils.ObservableListExtractorCustom.ZONE_SELECTOR_BEST;

import javafx.beans.binding.Bindings;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;

@Children(FlexDiv.class)
@Children(path = FlexDiv.class, value = { HtmlHyperLink.class, TextDiv.class })
@InheritStyle("background-color")
@Style(path = FlexDiv.class, name = "max-height", value = "90%")
@Style(path = FlexDiv.class, name = "width", value = "inherit")
@BindAction(path = { FlexDiv.class, HtmlHyperLink.class }, value = RESET_SELECTION.class)
public class DocZonesShow extends ModalEditor {

	@FlexDirectionStyle(FlexDirection.COLUMN)
	@FlexDirectionStyle(path = FlexDiv.class, value = FlexDirection.ROW)
	@Children({ FlexDiv.class, FlexDiv.class, FlexDiv.class })
	@Children(path = FlexDiv.class, pos = 1, value = { FlexDiv.class, FlexDiv.class })
	@Children(path = { FlexDiv.class, FlexDiv.class }, pos = { 1, 0 }, value = { Image.class, LastUpdate.class })
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

	@Style(name = "margin", value = "0.5em")
	@Style(name = "flex", value = "0 0 auto")
	@Style(name = "justify-content", value = "center")
	@Style(name = "align-items", value = "center")
	public static class Image extends HtmlImg {
		@Override
		public void init() {
			bindAttribute("src", "imgadr", context -> new SimpleStringProperty((String) context.getGeneric().getValue()));
		}
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

	@Children(HtmlLabel.class)
	@BindText(ZONE_TEXT.class)
	@StyleClass("input-like")
	public static class ZoneField extends FlexDiv {
		// Define the div containing the OCR text
	}

	@BindText(LAST_UPDATE_LABEL.class)
	@Style(name = "margin", value = "0.5em")
	@Style(name = "flex", value = "0 0 auto")
	@Style(name = "justify-content", value = "center")
	@Style(name = "align-items", value = "center")
	public static class LastUpdate extends FlexDiv {
		// Print the timestamp of the last refresh
	}

	public static class ZONE_LABEL implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			return new SimpleStringProperty("Zone " + ((ZoneTextInstance) context.getGeneric()).getZone());
		}
	}

	public static class ZONE_TEXT implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			return new SimpleStringProperty(StringEscapeUtils.escapeHtml4(context.getGeneric().getValue().toString()));
		}
	}

	public static class LAST_UPDATE_LABEL implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			DocInstance currentDoc = (DocInstance) context.getGeneric();
			Root root = currentDoc.getRoot();
			RefreshTimestamp refreshTimestamp = root.find(RefreshTimestamp.class);
			return Bindings.createStringBinding(() -> {
				RefreshTimestampInstance timeStamp = refreshTimestamp.getRefreshTimestamp(currentDoc);
				return timeStamp == null ? "Last update: none" : "Last update: " + ModelTools.formatDate((Long) timeStamp.getValue());
			}, refreshTimestamp.getInstances().toObservableList());
		}
	}

}
