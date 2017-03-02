package org.genericsystem.geography.app;

import org.genericsystem.common.Generic;
import org.genericsystem.geography.app.GeoSearchPerso.Input1;
import org.genericsystem.geography.app.GeoSearchPerso.Input2;
import org.genericsystem.geography.app.GeoSearchPerso.Test1;
import org.genericsystem.geography.app.GeoSearchPerso.Test2;
import org.genericsystem.geography.model.AdministrativeTerritory;
import org.genericsystem.geography.model.Building;
import org.genericsystem.geography.model.City;
import org.genericsystem.geography.model.Country;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.context.StringExtractor;
import org.genericsystem.reactor.context.TextBinding;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlP;
import org.genericsystem.reactor.gscomponents.RootTagImpl;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.transformation.SortedList;

@DependsOnModel({ AdministrativeTerritory.class, Country.class, City.class, Building.class })
@Children({ Input1.class, Input2.class, Test1.class, Test2.class })
public class GeoSearchPerso extends RootTagImpl {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, GeoSearchPerso.class, "/GeoApp");
	}

	@DirectSelect(City.class)
	public static class Input1 extends InputSelectInstance {
		@Override
		public String displayInstance(Generic g) {
			String str = StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(g);
			while (g.getBaseComponent() != null) {
				g = g.getBaseComponent();
				str += ", " + g.getValue();
			}
			return str;
		}
	}

	@DirectSelect(City.class)
	public static class Input2 extends InputSelectInstance {
		@Override
		public SortedList<Generic> filterInstances(Context c, Property<String> t) {
			return c.getGeneric().getObservableSubInstances()
					.filtered(res -> (t.getValue() != null && t.getValue().length() > 1)
							? ((String) res.getValue()).toLowerCase().contains(t.getValue().toLowerCase()) : false)
					.sorted();
		}
	}

	@BindText(GENERIC_TEXT.class)
	public static class Test1 extends HtmlP {
	}

	@BindText(GENERIC_TEXT2.class)
	public static class Test2 extends HtmlP {
	}

	public static class GENERIC_TEXT implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			Tag inputTag = tag.getParent().find(Input1.class);
			Context ctx = context.getSubContexts(inputTag).get(0);
			Property<?> prop = inputTag.getContextProperty("selected", ctx);
			return Bindings.createStringBinding(() -> prop.getValue() != null ? prop.getValue().toString() : "", prop);
		}
	}

	public static class GENERIC_TEXT2 implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			Tag inputTag = tag.getParent().find(Input2.class);
			Context ctx = context.getSubContexts(inputTag).get(0);
			Property<?> prop = inputTag.getContextProperty("selected", ctx);
			return Bindings.createStringBinding(() -> prop.getValue() != null ? prop.getValue().toString() : "", prop);
		}
	}

}
