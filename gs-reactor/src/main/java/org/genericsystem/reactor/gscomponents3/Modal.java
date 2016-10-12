package org.genericsystem.reactor.gscomponents3;

import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import org.genericsystem.reactor.htmltag.HtmlHyperLink;

import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Select.SelectModel;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.gscomponents.GSDiv;
import org.genericsystem.reactor.gscomponents3.DivWithTitle.TitledInstanceEditor;
import org.genericsystem.reactor.model.ContextAction.RESET_SELECTION;
import org.genericsystem.reactor.model.ObservableModelSelector.SELECTION_SELECTOR;

import javafx.beans.binding.Bindings;

@ReactorDependencies(GSDiv.class)
@ReactorDependencies(path = GSDiv.class, value = HtmlHyperLink.class)
@StyleClass("modal")
@StyleClass(path = GSDiv.class, value = "modal-content")
@StyleClass(path = { GSDiv.class, HtmlHyperLink.class }, value = "close")
@Style(path = GSDiv.class, name = "max-width", value = "40%")
@Style(path = GSDiv.class, name = "padding", value = "10px")
@Style(path = GSDiv.class, name = "border-radius", value = "10px")
@Style(path = GSDiv.class, name = "background-color", value = "white")
@SelectModel(path = { GSDiv.class, GSDiv.class }, value = SELECTION_SELECTOR.class)
@SetText(path = { GSDiv.class, HtmlHyperLink.class }, value = "×")
@BindAction(path = { GSDiv.class, HtmlHyperLink.class }, value = RESET_SELECTION.class)
public class Modal extends GSDiv implements SelectionDefaults {
	@Override
	public void init() {
		bindStyle(DISPLAY, DISPLAY, model -> Bindings.createStringBinding(() -> getSelectionProperty(model).getValue() != null ? "flex" : "none", getSelectionProperty(model)));
	}

	@ReactorDependencies(path = GSDiv.class, value = { HtmlHyperLink.class, TitledInstanceEditor.class })
	@Style(path = { GSDiv.class, TitledInstanceEditor.class }, name = "min-height", value = "300px")
	public static class ModalEditor extends Modal {

	}
}
