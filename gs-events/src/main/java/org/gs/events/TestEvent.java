package org.gs.events;

import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.geography.components.InputSelectInstance;
import org.genericsystem.geography.model.AdministrativeTerritory;
import org.genericsystem.geography.model.City;
import org.genericsystem.geography.model.Country;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.appserver.Script;
import org.genericsystem.reactor.context.ObservableValueSelector;
import org.genericsystem.reactor.context.StringExtractor;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.reactor.gscomponents.RootTagImpl;
import org.gs.events.TestEvent.CityInput;
import org.gs.events.TestEvent.CityLabel;
import org.gs.events.TestEvent.Div;
import org.gs.events.TestEvent.InitTest;
import org.gs.events.components.InputDate;
import org.gs.events.model.Date;
import org.gs.events.model.Date.Day;
import org.gs.events.model.Date.Month;
import org.gs.events.model.Date.Year;
import org.gs.events.model.Event;

@RunScript(InitTest.class)
@DependsOnModel({ AdministrativeTerritory.class, Country.class, City.class, Date.class })
@Children({ CityLabel.class, CityInput.class, Div.class })
public class TestEvent extends RootTagImpl {

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

	@Select(value = DATE_SELECTOR.class)
	public static class InputDate1 extends InputDate {
	}

	public static class DATE_SELECTOR implements ObservableValueSelector {

		@Override
		public Generic apply(Generic[] generics) {
			return generics[0].getSubInstances().toList().get(2);
		}

	}

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, TestEvent.class, "/GeoApp");
	}

	public static class InitTest implements Script {

		@Override
		public void run(Root engine) {

			Generic year = engine.find(Year.class);
			Generic month = engine.find(Month.class);
			Generic day = engine.find(Day.class);
			Generic date = engine.find(Date.class);
			Generic year2004 = year.setInstance(2004);
			Generic month200411 = month.setInstance(11, year2004);
			Generic day20041112 = day.setInstance(12, month200411);
			Generic day20041113 = day.setInstance(13, month200411);

			Generic city = engine.find(City.class);
			Generic nantes = city.getInstance("Nantes");

			Generic event = engine.find(Event.class);
			Generic myEvent = engine.setInstance("Meeting", nantes, day20041112, day20041113);

			System.out.println(myEvent.getComponent(0));
			System.out.println(myEvent.getComponent(1));
			System.out.println(myEvent);

			engine.getCurrentCache().flush();

		}
	}

}
