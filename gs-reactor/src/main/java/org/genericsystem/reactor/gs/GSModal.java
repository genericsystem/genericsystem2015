package org.genericsystem.reactor.gs;

import java.util.function.Consumer;

import javafx.beans.binding.Bindings;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gstag.HtmlHyperLink;
import org.genericsystem.reactor.modelproperties.SelectionDefaults;

/**
 * @author Nicolas Feybesse
 *
 */
public class GSModal extends GSDiv implements SelectionDefaults {

	public GSModal(Tag parent, Consumer<Tag> contentTagConsumer) {
		this(parent, FlexDirection.COLUMN, contentTagConsumer);
	}

	public GSModal(Tag parent, FlexDirection direction, Consumer<Tag> contentTagConsumer) {
		super(parent, direction);
		addStyleClass("modal");
		bindStyle(DISPLAY, DISPLAY, model -> Bindings.createStringBinding(() -> getSelectionProperty(model).getValue() != null ? "flex" : "none", getSelectionProperty(model)));

		new GSDiv(this, FlexDirection.COLUMN) {
			{
				addStyle("max-width", "40%");
				addStyleClass("modal-content");
				new HtmlHyperLink(this) {
					{
						addStyleClass("close");
						setText("×");
						bindAction(model -> getSelectionProperty(model).setValue(null));
					}
				};
				new GSDiv(this, FlexDirection.COLUMN) {
					{
						select__(model -> getSelectionProperty(model));
						contentTagConsumer.accept(this);
					}
				};
			}
		};
	}
}
