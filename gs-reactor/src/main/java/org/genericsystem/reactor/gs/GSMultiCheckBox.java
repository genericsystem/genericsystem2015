package org.genericsystem.reactor.gs;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gstag.HtmlLabel;
import org.genericsystem.reactor.model.ObservableListExtractor;
import org.genericsystem.reactor.model.StringExtractor;

import javafx.beans.binding.Bindings;
import javafx.beans.value.ObservableValue;

public class GSMultiCheckBox extends GSDiv {

	public GSMultiCheckBox(Tag parent) {
		this(parent, FlexDirection.COLUMN);
	}

	public GSMultiCheckBox(Tag parent, FlexDirection flexDirection) {
		super(parent, FlexDirection.COLUMN);
		addStyle("flex-wrap", "wrap");
		addStyle("overflow", "auto");

		new HtmlLabel(this) {
			{
				addStyle("flex", "1 0 auto");
				addStyle("justify-content", "center");
				addStyle("align-items", "center");
				addStyle("text-align", "center");
				forEach(gs -> ObservableListExtractor.SUBINSTANCES.apply(ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])).stream().toArray(Generic[]::new)));
				addPrefixBinding(model -> {
					if ("Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(model.getGeneric().getMeta())))
						addStyle(model, "background-color", getGenericStringProperty(model).getValue());
				});
				bindText();
				new GSCheckBoxWithValue(this) {
					{
						addStyle("float", "left");
						addStyle("vertical-align", "middle");
						addStyle("margin", "4px");
						initValueProperty(context -> context.getGenerics()[2].getLink(context.getGenerics()[1], context.getGeneric()) != null ? true : false);
						storeProperty("exists", context -> {
							ObservableValue<Boolean> exists = Bindings.createBooleanBinding(() -> context.getGenerics()[2].getObservableLink(context.getGenerics()[1], context.getGeneric()).getValue() != null ? true : false,
									context.getGenerics()[2].getObservableLink(context.getGenerics()[1], context.getGeneric()));
							exists.addListener((o, v, nva) -> {
								if (!context.isDestroyed())
									getConvertedValueProperty(context).setValue(nva);
							});
							return exists;
						});
						addConvertedValueChangeListener((context, nva) -> {
							if (Boolean.TRUE.equals(nva))
								context.getGenerics()[2].setHolder(context.getGenerics()[1], null, context.getGeneric());
							if (Boolean.FALSE.equals(nva)) {
								Generic link = context.getGenerics()[2].getLink(context.getGenerics()[1], context.getGeneric());
								if (link != null)
									link.remove();
							}
						});
					}
				};
			}
		};
	}

}
