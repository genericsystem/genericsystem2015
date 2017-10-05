package org.gs.events;

import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.defaults.tools.RxJavaHelpers;
import org.genericsystem.geography.components.InputSelectInstance;
import org.genericsystem.geography.model.AdministrativeTerritory;
import org.genericsystem.geography.model.City;
import org.genericsystem.geography.model.Country;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.appserver.Script;
import org.genericsystem.reactor.context.GenericSelector;
import org.genericsystem.reactor.context.StringExtractor;
import org.genericsystem.reactor.context.TextBinding;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlP;
import org.genericsystem.reactor.gscomponents.RootTagImpl;
import org.gs.events.TestEvent.CityInput;
import org.gs.events.TestEvent.CityLabel;
import org.gs.events.TestEvent.DateInput;
import org.gs.events.TestEvent.DateInput2;
import org.gs.events.TestEvent.DateLabel1;
import org.gs.events.TestEvent.DateLabel2;
import org.gs.events.TestEvent.InitTest;
import org.gs.events.TestEvent.Test;
import org.gs.events.components.InputDate;
import org.gs.events.components.InputDate.DivContainer;
import org.gs.events.model.Date;
import org.gs.events.model.Date.Day;
import org.gs.events.model.Date.Month;
import org.gs.events.model.Date.Year;
import org.gs.events.model.Event;

import io.reactivex.Observable;
import javafx.beans.property.Property;

@RunScript(InitTest.class)
@DependsOnModel({ AdministrativeTerritory.class, Country.class, City.class, Date.class })
@Children({ CityLabel.class, CityInput.class, DateLabel1.class, DateInput.class, DateLabel2.class, DateInput2.class,
	Test.class })
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

	@SetText(value = "Date 1 yyyy/mm/dd : ")
	public static class DateLabel1 extends HtmlLabel {
	}

	@DirectSelect(Date.class)
	@Select(path = DivContainer.class, value = DATE_SELECTOR.class)
	public static class DateInput extends InputDate {
	}

	@SetText(value = "Date 2 yyyy/mm/dd : ")
	public static class DateLabel2 extends HtmlLabel {
	}

	@DirectSelect(Date.class)
	public static class DateInput2 extends InputDate {
	}

	public static class DATE_SELECTOR implements GenericSelector {

		@Override
		public Generic apply(Generic[] generics) {
			return generics[0].getSubInstances().toList().get(2);
		}

	}

	@BindText(GENERIC_TEXT.class)
	public static class Test extends HtmlP {
	}

	// public String displayDate(Generic g) {
	// if (g.isInstanceOf(g.getRoot().find(Day.class))) // yyyy/mm/dd
	// return g.getBaseComponent().getBaseComponent().getValue() + "/" + g.getBaseComponent().getValue() + "/"
	// + g.getValue();
	// else if (g.isInstanceOf(g.getRoot().find(Month.class))) // yyyy/mm
	// return g.getBaseComponent().getValue() + "/" + g.getValue(); // yyyy
	// else
	// return g.getValue() + "";
	// }

	public static class GENERIC_TEXT implements TextBinding {
		@Override
		public Observable<String> apply(Context context, Tag tag) {
			Tag inputTag = tag.getParent().find(InputDate.class, 1);
			Context ctx = context.getSubContexts(inputTag).get(0);
			Property<?> prop = inputTag.getContextProperty("selected", ctx);
			return RxJavaHelpers.optionalValuesOf(prop).map(opt -> opt.isPresent() ? opt.get().toString() : "");
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
