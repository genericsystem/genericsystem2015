package org.genericsystem.carcolor;

import java.util.function.Consumer;

import org.genericsystem.reactor.TagProperty;
import org.genericsystem.reactor.TagProperty.DisplayProperty;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.gs.FlexDirection;
import org.genericsystem.reactor.gs.GSSection;
import org.genericsystem.reactor.gs.GSTag;
import org.genericsystem.reactor.html.HtmlHyperLink;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.StringExtractor;

import javafx.beans.binding.Bindings;

/**
 * @author Nicolas Feybesse
 *
 */
public class GSModal extends GSSection {

	public GSModal(GSTag parent, Consumer<GSTag> contentTagConsumer, TagProperty<GenericModel> selectionProperty) {
		this(parent, FlexDirection.COLUMN, contentTagConsumer, selectionProperty);
	}

	public GSModal(GSTag parent, FlexDirection direction, Consumer<GSTag> contentTagConsumer, TagProperty<GenericModel> selectionProperty) {
		super(parent, direction);
		addStyleClass("modal");
		bindStyle(ReactorStatics.DISPLAY, DisplayProperty::new, model -> Bindings.createStringBinding(() -> selectionProperty.getValue(model.getGeneric()) != null ? "flex" : "none", selectionProperty.getProperty(model.getGeneric())));

		new GSSection(this, FlexDirection.COLUMN) {
			{
				addStyle("max-width", "40%");
				// addStyle("max-height", "40%");
				addStyleClass("modal-content");
				new HtmlHyperLink<GenericModel>(this) {
					{
						addStyleClass("close");
						setText("×");
						bindAction(model -> selectionProperty.setValue(model.getGeneric().getMeta(), null));
					}
				};
				new GSSection(this, FlexDirection.COLUMN) {
					{
						select_(StringExtractor.TYPE_INSTANCE_EXTRACTOR, model -> selectionProperty.getProperty(model.getGeneric().getMeta()));
						contentTagConsumer.accept(this);
					}
				};
			}
		};
	}
}
