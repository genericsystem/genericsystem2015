package org.genericsystem.reactor.ca_gscomponents;

import java.util.function.Consumer;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.aa_modelproperties.SelectionDefaults;
import org.genericsystem.reactor.ba_htmltag.HtmlHyperLink;

import javafx.beans.binding.Bindings;

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
				addStyle("padding", "10px");
				addStyle("border-radius", "10px");
				addStyle("background-color", "white");

				new GSDiv(this, FlexDirection.COLUMN) {
					{
						new HtmlHyperLink(this) {
							{
								addStyleClass("close");
								setText("×");
								bindAction(model -> getSelectionProperty(model).setValue(null));
							}
						};
						select__(model -> getSelectionProperty(model));
						contentTagConsumer.accept(this);
					}
				};
			}
		};
	}
}
