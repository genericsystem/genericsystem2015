package org.genericsystem.reactor.gscomponents3;

import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import org.genericsystem.reactor.htmltag.HtmlHyperLink;

import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Select.SelectModel;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.StyleClasses.StyleClass;
import org.genericsystem.reactor.annotations.Styles.Style;
import org.genericsystem.reactor.gscomponents.GSDiv;
import org.genericsystem.reactor.model.ContextAction.RESET_SELECTION;
import org.genericsystem.reactor.model.ObservableModelSelector.SELECTION_SELECTOR;

import javafx.beans.binding.Bindings;

@StyleClass("modal")
@ReactorDependencies(GSDiv.class)
@ReactorDependencies(path = GSDiv.class, value = HtmlHyperLink.class)
@Style(path = GSDiv.class, name = "max-width", value = "40%")
@StyleClass(path = GSDiv.class, value = "modal-content")
@Style(path = GSDiv.class, name = "padding", value = "10px")
@Style(path = GSDiv.class, name = "border-radius", value = "10px")
@Style(path = GSDiv.class, name = "background-color", value = "white")
@SelectModel(path = { GSDiv.class, GSDiv.class }, value = SELECTION_SELECTOR.class)
@StyleClass(path = { GSDiv.class, HtmlHyperLink.class }, value = "close")
@SetText(path = { GSDiv.class, HtmlHyperLink.class }, value = "×")
@BindAction(path = { GSDiv.class, HtmlHyperLink.class }, value = RESET_SELECTION.class)
public class Modal extends GSDiv implements SelectionDefaults {
	@Override
	public void init() {
		bindStyle(DISPLAY, DISPLAY, model -> Bindings.createStringBinding(() -> getSelectionProperty(model).getValue() != null ? "flex" : "none", getSelectionProperty(model)));
	}

	@ReactorDependencies(path = GSDiv.class, value = { HtmlHyperLink.class, InstanceEditor.class })
	@Style(path = { GSDiv.class, InstanceEditor.class }, name = "min-height", value = "300px")
	public static class ModalEditor extends Modal {

	}
}
