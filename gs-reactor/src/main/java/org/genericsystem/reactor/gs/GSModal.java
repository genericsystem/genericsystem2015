package org.genericsystem.reactor.gs;

import java.util.function.Consumer;

import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gs.FlexDirection;
import org.genericsystem.reactor.gs.GSSection;
import org.genericsystem.reactor.gs.GSTag;
import org.genericsystem.reactor.gs.SelectionDefaults;
import org.genericsystem.reactor.gstag.HtmlHyperLink;

import javafx.beans.binding.Bindings;

/**
 * @author Nicolas Feybesse
 *
 */
public class GSModal extends GSSection implements SelectionDefaults {

	public GSModal(GSTag parent, Consumer<GSTag> contentTagConsumer) {
		this(parent, FlexDirection.COLUMN, contentTagConsumer);
	}

	public GSModal(GSTag parent, FlexDirection direction, Consumer<GSTag> contentTagConsumer) {
		super(parent, direction);
		addStyleClass("modal");
		bindStyle(ReactorStatics.DISPLAY, ReactorStatics.DISPLAY, model -> Bindings.createStringBinding(() -> getSelectionProperty(model).getValue() != null ? "flex" : "none", getSelectionProperty(model)));

		new GSSection(this, FlexDirection.COLUMN) {
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
				new GSSection(this, FlexDirection.COLUMN) {
					{
						select_(model -> getSelectionProperty(model));
						contentTagConsumer.accept(this);
					}
				};
			}
		};
	}
}
