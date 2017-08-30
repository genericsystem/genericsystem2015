package org.genericsystem.ir.app.gui.pages;

import java.io.Serializable;

import org.genericsystem.common.Generic;
import org.genericsystem.ir.app.gui.pages.DocZonesEdit.TextDiv;
import org.genericsystem.ir.app.gui.utils.ContextActionCustom.MODAL_DISPLAY_FLEX_CUSTOM;
import org.genericsystem.ir.app.gui.utils.ContextActionCustom.SAVE;
import org.genericsystem.ir.app.gui.utils.DocumentImage;
import org.genericsystem.ir.app.gui.utils.ObservableListExtractorCustom.DATALIST_SELECTOR;
import org.genericsystem.ir.app.gui.utils.ObservableListExtractorCustom.ZONE_SELECTOR_REALITY;
import org.genericsystem.ir.app.gui.utils.TextBindingCustom.ZONE_LABEL;
import org.genericsystem.reactor.Context;
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
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.context.ContextAction.CANCEL;
import org.genericsystem.reactor.context.ContextAction.RESET_SELECTION;
import org.genericsystem.reactor.context.ObservableContextSelector.SELECTION_SELECTOR;
import org.genericsystem.reactor.context.TagSwitcher;
import org.genericsystem.reactor.contextproperties.SelectionDefaults;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDatalist;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlOption;
import org.genericsystem.reactor.gscomponents.InputTextWithConversion.InputTextEditorWithConversionForDatalist;
import org.genericsystem.reactor.gscomponents.InputWithDatalist;
import org.genericsystem.reactor.gscomponents.Modal.ModalEditor;

@Children(FlexDiv.class)
@Children(path = FlexDiv.class, value = { HtmlHyperLink.class, TextDiv.class })
@InheritStyle("background-color")
@Style(path = FlexDiv.class, name = "max-height", value = "90%")
@Style(path = FlexDiv.class, name = "width", value = "inherit")
@BindAction(path = { FlexDiv.class, HtmlHyperLink.class }, value = RESET_SELECTION.class)
public class DocZonesEdit extends ModalEditor {

	@FlexDirectionStyle(FlexDirection.COLUMN)
	@FlexDirectionStyle(path = FlexDiv.class, value = FlexDirection.ROW)
	@Children({ FlexDiv.class, FlexDiv.class, FlexDiv.class })
	@Children(path = FlexDiv.class, pos = 1, value = { FlexDiv.class, FlexDiv.class })
	@Children(path = { FlexDiv.class, FlexDiv.class }, pos = { 1, 0 }, value = DocumentImage.class)
	@Children(path = FlexDiv.class, pos = 2, value = { Validate.class, Reset.class, Cancel.class })
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

	@SetText("Save")
	@BindAction(value = SAVE.class)
	public static class Validate extends HtmlButton {
		// Persists the changes
	}

	@SetText("Reset")
	@BindAction(value = CANCEL.class)
	public static class Reset extends HtmlButton {
		// Reset the changes
	}

	@SetText("Cancel")
	@BindAction(value = { CANCEL.class, RESET_SELECTION.class })
	public static class Cancel extends HtmlButton {
		// Cancel the changes
	}

	@FlexDirectionStyle(FlexDirection.COLUMN)
	@Children({ ZoneLabelInput.class, DocZonesShowDetails.class })
	@ForEach(ZONE_SELECTOR_REALITY.class)
	public static class ZoneTextDiv extends FlexDiv {
		// For each zone, create a div with label + inputText and create a div for the results for all filters
	}

	@FlexDirectionStyle(FlexDirection.ROW)
	@Children({ ZoneLabelAdmin.class, ZoneLabelNormal.class, ZoneInput.class })
	public static class ZoneLabelInput extends FlexDiv {

	}

	@Switch(TagSwitcher.ADMIN_MODE_ONLY.class)
	@BindText(ZONE_LABEL.class)
	@BindAction(MODAL_DISPLAY_FLEX_CUSTOM.class)
	@Attribute(name = "name", value = "zone")
	public static class ZoneLabelAdmin extends HtmlHyperLink {
		// Define the zone label in admin mode
	}

	@Switch(TagSwitcher.NORMAL_MODE_ONLY.class)
	@BindText(ZONE_LABEL.class)
	@Attribute(name = "name", value = "zone")
	public static class ZoneLabelNormal extends HtmlLabel { // FlexDiv?
		// Define the zone label in normal mode
	}

	@ForEach(path = { HtmlDatalist.class, HtmlOption.class }, value = DATALIST_SELECTOR.class)
	@Children({ CustomInputDatalist.class, HtmlDatalist.class })
	@StyleClass(path = CustomInputDatalist.class, value = "glowing-border")
	public static class ZoneInput extends InputWithDatalist implements SelectionDefaults {
		// Define the inputText
		// TODO: add a remove button to empty the field
		// TODO: generate the zonetextinstance only when necessary (i.e., not empty)?
	}

	public static class CustomInputDatalist extends InputTextEditorWithConversionForDatalist {
		// TODO: remove next function?
		@Override
		protected Generic updateGeneric(Context context, Serializable newValue) {
			long start = System.nanoTime();
			Generic updateValue = context.getGeneric().updateValue(newValue);
			System.out.println("==> update: " + (System.nanoTime() - start) / 1_000_000 + "ms");
			return updateValue;
		}
	}

}
