package org.gs.events.components;

import org.genericsystem.common.Generic;
import org.genericsystem.geography.components.InputSelectInstance;
import org.genericsystem.geography.model.City;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.context.StringExtractor;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.gs.events.TestEvent.CityInput;
import org.gs.events.TestEvent.CityLabel;
import org.gs.events.TestEvent.Div;
import org.gs.events.TestEvent.InputDate1;
import org.gs.events.model.Date;

@Children({ CityLabel.class, CityInput.class, Div.class })
public class FormEvent extends HtmlDiv {

	@SetText(value = "City")
	@Style(name = "display", value = "inline")
	public static class CityLabel extends HtmlLabel {
	}

	@DirectSelect(City.class)
	@Style(name = "display", value = "inline")
	public static class CityInput extends InputSelectInstance {
		@Override
		public String displayInstance(Generic g) {
			String str = StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(g);
			while (g.getBaseComponent() != null) {
				g = g.getBaseComponent();
				str += ", " + g.getValue();
			}
			return str;
		}

		// preselected city
		@Override
		public void init() {
			super.init();
			addPrefixBinding(context -> {
				getContextProperty("selected", context).setValue(context.getGeneric().getInstance("Nantes"));
			});
		}
	}

	@DirectSelect(Date.class)
	@Children(InputDate1.class)
	public static class Div extends HtmlDiv {

	}

}
