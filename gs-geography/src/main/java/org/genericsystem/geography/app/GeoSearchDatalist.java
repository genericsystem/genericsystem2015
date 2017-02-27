package org.genericsystem.geography.app;

import org.genericsystem.common.Generic;
import org.genericsystem.geography.app.GeoSearchDatalist.SearchInputDatalist;
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
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDatalist;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlOption;
import org.genericsystem.reactor.gscomponents.InputWithDatalist;
import org.genericsystem.reactor.gscomponents.RootTagImpl;

import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.value.ObservableValue;

@DependsOnModel({ AdministrativeTerritory.class, Country.class, City.class, Building.class })
@Children(SearchInputDatalist.class)
public class GeoSearchDatalist extends RootTagImpl {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, GeoSearchDatalist.class, "/GeoApp");
	}

	public static class GENERIC_TEXT implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			Generic g = context.getGeneric();
			String str = StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(g);
			while (g.getBaseComponent() != null) {
				g = g.getBaseComponent();
				str += ", " + g.getValue();
			}
			return new ReadOnlyStringWrapper(str);
		}
	}

	@DirectSelect(City.class)
	@BindText(path = { HtmlDatalist.class, HtmlOption.class }, value = GENERIC_TEXT.class)
	public static class SearchInputDatalist extends InputWithDatalist {

	}

}
