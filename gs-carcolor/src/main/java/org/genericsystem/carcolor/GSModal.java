package org.genericsystem.carcolor;

import java.util.function.Consumer;

import javafx.beans.binding.Bindings;

import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gs.FlexDirection;
import org.genericsystem.reactor.gs.GSSection;
import org.genericsystem.reactor.gs.GSTag;
import org.genericsystem.reactor.html.HtmlHyperLink;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.StringExtractor;

/**
 * @author Nicolas Feybesse
 *
 */
public class GSModal extends GSSection {

	public GSModal(GSTag parent, Consumer<GSTag> contentTagConsumer) {
		this(parent, FlexDirection.COLUMN, contentTagConsumer);
	}

	public GSModal(GSTag parent, FlexDirection direction, Consumer<GSTag> contentTagConsumer) {
		super(parent, direction);
		addStyleClass("modal");
		bindStyle(ReactorStatics.DISPLAY, ReactorStatics.DISPLAY, model -> Bindings.createStringBinding(() -> getProperty(ReactorStatics.SELECTION, model).getValue() != null ? "flex" : "none", this.getProperty(ReactorStatics.SELECTION, model)));

		new GSSection(this, FlexDirection.COLUMN) {
			{
				addStyle("max-width", "40%");
				addStyle("max-height", "40%");
				addStyleClass("modal-content");
				new HtmlHyperLink<GenericModel>(this) {
					{
						addStyleClass("close");
						setText("×");
						bindAction(model -> getProperty(ReactorStatics.SELECTION, model).setValue(null));
					}
				};
				new GSSection(this, FlexDirection.COLUMN) {
					{
						select_(StringExtractor.TYPE_INSTANCE_EXTRACTOR, model -> getProperty(ReactorStatics.SELECTION, model));
						contentTagConsumer.accept(this);
					}
				};
			}
		};
	}
}
