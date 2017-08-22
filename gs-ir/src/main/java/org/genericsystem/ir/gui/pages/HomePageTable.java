package org.genericsystem.ir.gui.pages;

import org.genericsystem.ir.gui.pages.HomePageTable.DocumentsList;
import org.genericsystem.ir.gui.pages.HomePageTable.HeaderRow;
import org.genericsystem.ir.gui.utils.ContextActionCustom.REMOVE_CUSTOM;
import org.genericsystem.ir.gui.utils.DocPropertiesSwitcher.DOC_DEZONED;
import org.genericsystem.ir.gui.utils.DocPropertiesSwitcher.DOC_NOT_DEZONED;
import org.genericsystem.ir.gui.utils.DocPropertiesSwitcher.DOC_NOT_OCRD;
import org.genericsystem.ir.gui.utils.DocPropertiesSwitcher.DOC_NOT_SUPERVISED;
import org.genericsystem.ir.gui.utils.DocPropertiesSwitcher.DOC_OCRD;
import org.genericsystem.ir.gui.utils.DocPropertiesSwitcher.DOC_SUPERVISED;
import org.genericsystem.ir.gui.utils.ObservableListExtractorCustom.DOC_SELECTOR;
import org.genericsystem.ir.gui.utils.TextBindingCustom.LAST_DOC_UPDATE_LABEL;
import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.InheritStyle;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.context.ContextAction.CANCEL;
import org.genericsystem.reactor.context.ContextAction.RESET_SELECTION;
import org.genericsystem.reactor.context.ContextAction.SET_SELECTION;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlSpan;
import org.genericsystem.reactor.gscomponents.Modal.ModalEditor;

@Children({ HeaderRow.class, FlexDiv.class })
@Children(path = FlexDiv.class, pos = 1, value = DocumentsList.class)
@ForEach(path = FlexDiv.class, pos = 1, value = DOC_SELECTOR.class)
@StyleClass(path = FlexDiv.class, pos = 1, value = "alternate-rows")
public class HomePageTable extends FlexDiv {

	@Children({ HtmlLabel.class, HtmlLabel.class, HtmlLabel.class, HtmlLabel.class, HtmlLabel.class, HtmlLabel.class })
	@FlexDirectionStyle(FlexDirection.ROW)
	@Attribute(path = HtmlLabel.class, name = "name", value = "title")
	@StyleClass("statistics-header-row")
	@Style(path = HtmlLabel.class, name = "justify-content", value = "center")
	@Style(path = HtmlLabel.class, name = "align-items", value = "center")
	@Style(path = HtmlLabel.class, pos = 0, name = "flex", value = "3")
	@Style(path = HtmlLabel.class, name = "flex", value = "1")
	@Style(path = HtmlLabel.class, name = "text-align", value = "center")
	@Style(path = HtmlLabel.class, name = "font-weight", value = "bold")
	@SetText(path = HtmlLabel.class, value = { "Document's name", "De-zoned", "OCR'd", "Supervised", "Delete", "Last update" })
	public static class HeaderRow extends FlexDiv {

	}

	@FlexDirectionStyle(FlexDirection.ROW)
	@Children({ DocumentName.class, FlexDiv.class, ModalFlexDiv.class, FlexDiv.class, DocumentDeleteButtonDiv.class, LastDocumentUpdateDiv.class })
	// TODO: include a link to a dezoner for the first column
	@Children(path = FlexDiv.class, pos = 1, value = HtmlHyperLink.class)
	@Children(path = { FlexDiv.class, HtmlHyperLink.class }, pos = { 1, 0 }, value = { CheckedImage.class, FailedImage.class })
	@Switch(path = { FlexDiv.class, HtmlHyperLink.class, CheckedImage.class }, pos = { 1, 0, 0 }, value = DOC_DEZONED.class)
	@Switch(path = { FlexDiv.class, HtmlHyperLink.class, FailedImage.class }, pos = { 1, 0, 0 }, value = DOC_NOT_DEZONED.class)
	@BindAction(path = { FlexDiv.class, HtmlHyperLink.class }, pos = { 1, 0 }, value = CANCEL.class)
	@Children(path = FlexDiv.class, pos = 2, value = { DocZonesShow.class, HtmlHyperLink.class })
	@Children(path = { FlexDiv.class, HtmlHyperLink.class }, pos = { 2, 0 }, value = { CheckedImage.class, FailedImage.class })
	@BindAction(path = { FlexDiv.class, HtmlHyperLink.class }, pos = { 2, 0 }, value = SET_SELECTION.class)
	@Switch(path = { FlexDiv.class, HtmlHyperLink.class, CheckedImage.class }, pos = { 2, 0, 0 }, value = DOC_OCRD.class)
	@Switch(path = { FlexDiv.class, HtmlHyperLink.class, FailedImage.class }, pos = { 2, 0, 0 }, value = DOC_NOT_OCRD.class)
	@Children(path = FlexDiv.class, pos = 3, value = { HtmlHyperLink.class })
	@Children(path = { FlexDiv.class, HtmlHyperLink.class }, pos = { 3, 0 }, value = { CheckedImage.class, FailedImage.class })
	@BindAction(path = { FlexDiv.class, HtmlHyperLink.class }, pos = { 3, 0 }, value = SET_SELECTION.class)
	@Switch(path = { FlexDiv.class, HtmlHyperLink.class, CheckedImage.class }, pos = { 3, 0, 0 }, value = DOC_SUPERVISED.class)
	@Switch(path = { FlexDiv.class, HtmlHyperLink.class, FailedImage.class }, pos = { 3, 0, 0 }, value = DOC_NOT_SUPERVISED.class)
	@Style(path = FlexDiv.class, pos = 0, name = "flex", value = "3")
	@Style(path = FlexDiv.class, name = "flex", value = "1")
	@Style(path = FlexDiv.class, name = "justify-content", value = "center")
	@Style(path = FlexDiv.class, pos = 0, name = "align-items", value = "left")
	@Style(path = FlexDiv.class, name = "align-items", value = "center")
	public static class DocumentsList extends FlexDiv {

	}

	public static class ModalFlexDiv extends FlexDiv {
		@Override
		public void init() {
			super.init();
			createSelectionProperty();
		}
	}

	@StyleClass({ "fa", "fa-check-square-o", "fa-2x" })
	@Style(name = "color", value = "#6fcc9f")
	public static class CheckedImage extends HtmlSpan {

	}

	@StyleClass({ "fa", "fa-times", "fa-2x" })
	@Style(name = "color", value = "#d33f3f")
	public static class FailedImage extends HtmlSpan {

	}

	@BindText
	@FlexDirectionStyle(FlexDirection.COLUMN)
	@Style(name = "justify-content", value = "center")
	@Style(name = "align-items", value = "center")
	@Style(name = "flex", value = "3")
	public static class DocumentName extends FlexDiv {

	}

	@FlexDirectionStyle(FlexDirection.COLUMN)
	@Style(name = "justify-content", value = "center")
	@Style(name = "align-items", value = "center")
	@Style(name = "flex", value = "1 0 auto")
	@Children({ DeleteConfirmation.class, DocumentDeleteButton.class })
	public static class DocumentDeleteButtonDiv extends ModalFlexDiv {

	}

	@Children(HtmlHyperLink.class)
	@Children(path = HtmlHyperLink.class, value = HtmlSpan.class)
	@StyleClass(path = { HtmlHyperLink.class, HtmlSpan.class }, value = { "fa", "fa-trash-o", "fa-2x" })
	@Style(path = { HtmlHyperLink.class, HtmlSpan.class }, name = "color", value = "#d33f3f")
	@BindAction(path = HtmlHyperLink.class, value = SET_SELECTION.class)
	public static class DocumentDeleteButton extends FlexDiv {
		// Delete button to delete the selected Generic
	}

	@Children(FlexDiv.class)
	@Children(path = FlexDiv.class, value = { HtmlHyperLink.class, FlexDiv.class })
	@InheritStyle("background-color")
	@Style(path = FlexDiv.class, name = "max-height", value = "90%")
	@Style(path = FlexDiv.class, name = "width", value = "auto")
	@BindAction(path = { FlexDiv.class, HtmlHyperLink.class }, value = RESET_SELECTION.class)
	@Children(path = { FlexDiv.class, FlexDiv.class }, value = FlexDiv.class)
	@Children(path = { FlexDiv.class, FlexDiv.class, FlexDiv.class }, value = { HtmlButton.class, HtmlButton.class })
	@FlexDirectionStyle(path = { FlexDiv.class, FlexDiv.class }, value = FlexDirection.COLUMN)
	@FlexDirectionStyle(path = { FlexDiv.class, FlexDiv.class, FlexDiv.class }, value = FlexDirection.ROW)
	@SetText(path = { FlexDiv.class, FlexDiv.class }, pos = { 0, -1 }, value = "Are you sure?")
	@SetText(path = { FlexDiv.class, FlexDiv.class, FlexDiv.class, HtmlButton.class }, pos = { 0, 0, 0, 0 }, value = "Confirm")
	@SetText(path = { FlexDiv.class, FlexDiv.class, FlexDiv.class, HtmlButton.class }, pos = { 0, 0, 0, 1 }, value = "Cancel")
	@BindAction(path = { FlexDiv.class, FlexDiv.class, FlexDiv.class, HtmlButton.class }, pos = { 0, 0, 0, 0 }, value = REMOVE_CUSTOM.class)
	@BindAction(path = { FlexDiv.class, FlexDiv.class, FlexDiv.class, HtmlButton.class }, pos = { 0, 0, 0, 1 }, value = RESET_SELECTION.class)
	public static class DeleteConfirmation extends ModalEditor {

	}

	@BindText(LAST_DOC_UPDATE_LABEL.class)
	@Style(name = "justify-content", value = "center")
	@Style(name = "align-items", value = "center")
	@Style(name = "flex", value = "3")
	public static class LastDocumentUpdateDiv extends FlexDiv {

	}

}
