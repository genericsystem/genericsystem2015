package org.genericsystem.reactor.gscomponents3;

import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import org.genericsystem.reactor.htmltag.HtmlButton;
import org.genericsystem.reactor.htmltag.HtmlHyperLink;

import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.SelectModel;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.GSDiv;
import org.genericsystem.reactor.gscomponents3.DivWithTitle.TitledInstanceEditor;
import org.genericsystem.reactor.model.ContextAction.DISPLAY_NONE;
import org.genericsystem.reactor.model.ContextAction.RESET_SELECTION;
import org.genericsystem.reactor.model.ContextAction.UNMOUNT_CLOSE;
import org.genericsystem.reactor.model.ObservableModelSelector.SELECTION_SELECTOR;

import javafx.beans.binding.Bindings;

@Children(GSDiv.class)
@Children(path = GSDiv.class, value = HtmlHyperLink.class)
@StyleClass("modal")
@StyleClass(path = GSDiv.class, value = { "widthResponsive", "modal-content" })
@StyleClass(path = { GSDiv.class, HtmlHyperLink.class }, value = "close")
@Attribute(path = { GSDiv.class, HtmlHyperLink.class }, name = "name", value = "close")
@Style(path = GSDiv.class, name = "overflow", value = "auto")
@Style(path = GSDiv.class, name = "padding", value = "10px")
@Style(path = GSDiv.class, name = "border-radius", value = "10px")
@Style(path = GSDiv.class, name = "background-color", value = "white")
@SetText(path = { GSDiv.class, HtmlHyperLink.class }, value = "×")
public class Modal extends GSDiv {

	@Children(path = GSDiv.class, value = { TitledInstanceEditor.class, GSDiv.class })
	@Children(path = { GSDiv.class, GSDiv.class }, pos = { 0, 1 }, value = { HtmlButton.class, HtmlButton.class })
	@SetText(path = { GSDiv.class, GSDiv.class, HtmlButton.class }, value = { "Ok", "Cancel" })
	@Style(path = { GSDiv.class, TitledInstanceEditor.class }, name = "min-height", value = "300px")
	@Style(path = { GSDiv.class, GSDiv.class, HtmlButton.class }, pos = { 0, 1, 0 }, name = "flex", value = "1")
	@Style(path = { GSDiv.class, GSDiv.class, HtmlButton.class }, pos = { 0, 1, 1 }, name = "flex", value = "1")
	@Attribute(path = { GSDiv.class, GSDiv.class, HtmlButton.class }, pos = { 0, 1, 1 }, name = "name", value = "close")
	@FlexDirectionStyle(path = { GSDiv.class, GSDiv.class }, pos = { 0, 1 }, value = FlexDirection.ROW)
	@SelectModel(path = { GSDiv.class, TitledInstanceEditor.class }, value = SELECTION_SELECTOR.class)
	@BindAction(path = { GSDiv.class, GSDiv.class, HtmlButton.class }, pos = { 0, 1, 0 }, value = RESET_SELECTION.class)
	@BindAction(path = { GSDiv.class, GSDiv.class, HtmlButton.class }, pos = { 0, 1, 1 }, value = UNMOUNT_CLOSE.class)
	public static class ModalEditor extends Modal implements SelectionDefaults {
		@Override
		public void init() {
			bindStyle(DISPLAY, DISPLAY, model -> Bindings.createStringBinding(() -> getSelectionProperty(model).getValue() != null ? "flex" : "none", getSelectionProperty(model)));
		}
	}

	@BindAction(path = { GSDiv.class, HtmlHyperLink.class }, value = DISPLAY_NONE.class)
	public static class ModalWithDisplay extends Modal {
		@Override
		public void init() {
			createInitializedDisplayProperty("none");
			bindStyle(DISPLAY, DISPLAY);
		}
	}
}
