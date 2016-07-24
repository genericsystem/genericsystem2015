package org.genericsystem.carcolor;

import java.util.function.Consumer;

import javafx.beans.binding.Bindings;

import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.flex.FlexDirection;
import org.genericsystem.reactor.flex.GenericSection;
import org.genericsystem.reactor.html.HtmlHyperLink;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.StringExtractor;

/**
 * @author Nicolas Feybesse
 *
 */
public class GenericModal extends GenericSection {

	public GenericModal(Tag<?> parent, FlexDirection direction, Consumer<Tag<?>> contentTagConsumer) {
		super(parent, direction);
		addStyleClass("modal");
		bindStyle(ReactorStatics.DISPLAY, ReactorStatics.DISPLAY, model -> Bindings.createStringBinding(() -> getProperty(ReactorStatics.SELECTION, model).getValue() != null ? "flex" : "none", this.getProperty(ReactorStatics.SELECTION, model)));

		new GenericSection(this, FlexDirection.COLUMN) {
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
				new GenericSection(this, FlexDirection.COLUMN) {
					{
						select_(StringExtractor.TYPE_INSTANCE_EXTRACTOR, model -> getProperty(ReactorStatics.SELECTION, model));
						contentTagConsumer.accept(this);
					}
				};
			}
		};
	}
}
