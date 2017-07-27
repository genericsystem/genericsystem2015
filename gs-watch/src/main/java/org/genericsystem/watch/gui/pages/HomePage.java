package org.genericsystem.watch.gui.pages;

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
import org.genericsystem.reactor.context.ContextAction.SET_ADMIN_MODE;
import org.genericsystem.reactor.context.ContextAction.SET_NORMAL_MODE;
import org.genericsystem.reactor.context.ContextAction.SET_SELECTION;
import org.genericsystem.reactor.context.TagSwitcher.ADMIN_MODE_ONLY;
import org.genericsystem.reactor.context.TagSwitcher.NORMAL_MODE_ONLY;
import org.genericsystem.reactor.gscomponents.AppHeader;
import org.genericsystem.reactor.gscomponents.AppHeader.AppTitleDiv;
import org.genericsystem.reactor.gscomponents.AppHeader.Logo;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlImg;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.reactor.gscomponents.Modal.ModalEditor;
import org.genericsystem.watch.gui.pages.HomePage.DocClassDiv;
import org.genericsystem.watch.gui.pages.HomePage.GeneralActionsButtonsDiv;
import org.genericsystem.watch.gui.pages.HomePage.HeaderRow;
import org.genericsystem.watch.gui.utils.ContextActionCustom.CALL_STATISTICS_PAGE;
import org.genericsystem.watch.gui.utils.ContextActionCustom.REMOVE_CUSTOM;
import org.genericsystem.watch.gui.utils.ContextActionCustom.TEST;
import org.genericsystem.watch.gui.utils.DocPropertiesSwitcher.DOC_DEZONED;
import org.genericsystem.watch.gui.utils.DocPropertiesSwitcher.DOC_NOT_DEZONED;
import org.genericsystem.watch.gui.utils.DocPropertiesSwitcher.DOC_NOT_OCRD;
import org.genericsystem.watch.gui.utils.DocPropertiesSwitcher.DOC_NOT_SUPERVISED;
import org.genericsystem.watch.gui.utils.DocPropertiesSwitcher.DOC_OCRD;
import org.genericsystem.watch.gui.utils.DocPropertiesSwitcher.DOC_SUPERVISED;
import org.genericsystem.watch.gui.utils.ObservableListExtractorCustom.DOC_CLASS_SELECTOR;
import org.genericsystem.watch.gui.utils.ObservableListExtractorCustom.DOC_SELECTOR;
import org.genericsystem.watch.gui.utils.PageSwitcher.HOME_PAGE;
import org.genericsystem.watch.gui.utils.TextBindingCustom.DOC_CLASS_LABEL;
import org.genericsystem.watch.gui.utils.TextBindingCustom.LAST_DOC_UPDATE_LABEL;

@Switch(HOME_PAGE.class)
@Children({ DocZonesEdit.class, AppHeader.class, FlexDiv.class/* , Monitor.class */ })
@Children(path = FlexDiv.class, pos = 2, value = { GeneralActionsButtonsDiv.class, HeaderRow.class, DocClassDiv.class })
@Children(path = AppHeader.class, value = { Logo.class, AppTitleDiv.class, FlexDiv.class })
@Children(path = { AppHeader.class, FlexDiv.class }, pos = { 0, 2 }, value = { HtmlButton.class, HtmlButton.class })
@SetText(path = { AppHeader.class, FlexDiv.class, HtmlButton.class }, pos = { 0, 2, 0 }, value = "Switch to admin mode")
@SetText(path = { AppHeader.class, FlexDiv.class, HtmlButton.class }, pos = { 0, 2, 1 }, value = "Switch to normal mode")
@BindAction(path = { AppHeader.class, FlexDiv.class, HtmlButton.class }, pos = { 0, 2, 0 }, value = SET_ADMIN_MODE.class)
@BindAction(path = { AppHeader.class, FlexDiv.class, HtmlButton.class }, pos = { 0, 2, 1 }, value = SET_NORMAL_MODE.class)
@Switch(path = { AppHeader.class, FlexDiv.class, HtmlButton.class }, pos = { 0, 2, 0 }, value = NORMAL_MODE_ONLY.class)
@Switch(path = { AppHeader.class, FlexDiv.class, HtmlButton.class }, pos = { 0, 2, 1 }, value = ADMIN_MODE_ONLY.class)
@Style(path = AppHeader.class, name = "background-color", value = "#00afeb")
@SetText(path = { AppHeader.class, AppTitleDiv.class, HtmlH1.class }, value = "GS-Watch interface")
public class HomePage extends FlexDiv {

	@FlexDirectionStyle(FlexDirection.ROW)
	@Children({ HtmlButton.class, HtmlButton.class })
	@SetText(path = HtmlButton.class, value = { "Visualize statistics", "Resume interrupted tasks" })
	@BindAction(path = HtmlButton.class, pos = 0, value = CALL_STATISTICS_PAGE.class)
	@BindAction(path = HtmlButton.class, pos = 1, value = TEST.class)
	public static class GeneralActionsButtonsDiv extends FlexDiv {
		// TODO implement the action that will resume the waiting tasks
	}

	@Children({ HtmlLabel.class, HtmlLabel.class, HtmlLabel.class, HtmlLabel.class, HtmlLabel.class, HtmlLabel.class })
	@FlexDirectionStyle(FlexDirection.ROW)
	@Attribute(path = HtmlLabel.class, name = "name", value = "title")
	@Style(name = "margin", value = "0.5em")
	@Style(path = HtmlLabel.class, name = "justify-content", value = "center")
	@Style(path = HtmlLabel.class, name = "align-items", value = "center")
	@Style(path = HtmlLabel.class, pos = 0, name = "flex", value = "3")
	@Style(path = HtmlLabel.class, name = "flex", value = "1")
	@Style(path = HtmlLabel.class, name = "text-align", value = "center")
	@Style(path = HtmlLabel.class, name = "font-weight", value = "bold")
	@SetText(path = HtmlLabel.class, value = { "Document's name", "De-zoned", "OCR'd", "Supervised", "Delete", "Last update" })
	public static class HeaderRow extends FlexDiv {

	}

	@Children(FlexDiv.class)
	@ForEach(path = FlexDiv.class, value = DOC_CLASS_SELECTOR.class)
	@Children(path = FlexDiv.class, value = { FlexDiv.class, FlexDiv.class })
	@Children(path = { FlexDiv.class, FlexDiv.class }, pos = { 0, 1 }, value = DocumentsList.class)
	@BindText(path = { FlexDiv.class, FlexDiv.class }, pos = { 0, 0 }, value = DOC_CLASS_LABEL.class)
	@StyleClass(path = { FlexDiv.class, FlexDiv.class }, pos = { 0, 0 }, value = "doc-class-title")
	@Style(name = "width", value = "90%")
	@Style(name = "margin", value = "auto")
	public static class DocClassDiv extends FlexDiv {

	}

	@ForEach(DOC_SELECTOR.class)
	@StyleClass("alternate-rows")
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
	@StyleClass(path = { FlexDiv.class, HtmlImg.class }, value = "img")
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

	@StyleClass("img")
	@Attribute(name = "src", value = "checked.png")
	public static class CheckedImage extends HtmlImg {

	}

	@StyleClass("img")
	@Attribute(name = "src", value = "failed.png")
	public static class FailedImage extends HtmlImg {

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
	@Children(path = HtmlHyperLink.class, value = HtmlImg.class)
	@Attribute(path = { HtmlHyperLink.class, HtmlImg.class }, name = "src", value = "delete.png")
	@StyleClass(path = { HtmlHyperLink.class, HtmlImg.class }, value = "img")
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
