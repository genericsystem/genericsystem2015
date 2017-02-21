package org.genericsystem.geography.app;

import org.genericsystem.common.Generic;
import org.genericsystem.geography.app.GeoSearch.AdmDiv;
import org.genericsystem.geography.model.AdministrativeTerritory;
import org.genericsystem.geography.model.Building;
import org.genericsystem.geography.model.City;
import org.genericsystem.geography.model.Continent;
import org.genericsystem.geography.model.Country;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEachContext;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.context.ObservableListExtractorFromContext;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlInputText;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLi;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlUl;
import org.genericsystem.reactor.gscomponents.RootTagImpl;

import javafx.beans.binding.ListBinding;
import javafx.beans.property.Property;
import javafx.collections.MapChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;

@DependsOnModel({ AdministrativeTerritory.class, Continent.class, Country.class, City.class, Building.class })
@Children(AdmDiv.class)
@Style(name = "background-color", value = "#8dde6d")
public class GeoSearch extends RootTagImpl {

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, GeoSearch.class, "/GeoApp");
	}

	@DirectSelect(AdministrativeTerritory.class)
	@Children({ SearchInput.class, AdmUl.class })
	public static class AdmDiv extends HtmlDiv {

	}

	@Children(AdmLi.class)
	@Style(name = "background-color", value = "yellow")
	@Style(name = "display", value = "inline-block")
	@Style(name = "list-style-type", value = "none")
	@Style(name = "position", value = "absolute")
	@Style(name = "z-index", value = "none")
	public static class AdmUl extends HtmlUl {
	}

	@ForEachContext(TEXT_FILTERED.class)
	@Children({ AdmLink.class })
	public static class AdmLi extends HtmlLi {
	}

	@BindText
	public static class AdmLink extends HtmlHyperLink {

	}

	public static class SearchInput extends HtmlInputText {

		@Override
		public void init() {
			addPrefixBinding(context -> {
				this.getDomNodeAttributes(context).addListener((MapChangeListener<String, String>) change -> {
					if ("value".equals(change.getKey())) {
						if (change.wasAdded())
							getParent().getContextProperty(TextAction.TEXT, context.getParent())
									.setValue(change.getValueAdded());
					}
				});
			});
		}
	}

	public static class TextAction {
		public final static String TEXT = "text";
	}

	public static class TEXT_FILTERED implements ObservableListExtractorFromContext {

		@Override
		public ObservableList<Generic> apply(Context context, Tag tag) {

			if (tag.getParent().getParent().getContextProperty(TextAction.TEXT, context.getParent()) == null)
				tag.getParent().getParent().createNewContextProperty(TextAction.TEXT, context.getParent());
			Property<String> text = tag.getParent().getParent().getContextProperty(TextAction.TEXT,
					context.getParent());

			// Map<Object, Object> map = context.getGeneric().getObservableSubInstances().filtered(adm -> {
			// boolean belongsTo = false;
			// if (text.getValue() != null && !text.getValue().trim().isEmpty())
			// belongsTo = ((String) adm.getValue()).trim().toLowerCase()
			// .contains(text.getValue().trim().toLowerCase());
			// return belongsTo;
			// }).stream().collect(Collectors.toMap(p -> p, p -> p));
			//
			// System.out.println(map);

			return new ListBinding<Generic>() {
				{
					bind(text);
				}

				@Override
				protected ObservableList<Generic> computeValue() {

					FilteredList<Generic> results = context.getGeneric().getObservableSubInstances().filtered(adm -> {
						// System.out.println(((AdministrativeTerritoryInstance) adm).getCode());
						return (text.getValue() != null && text.getValue().length() > 1) ? ((String) adm.getValue())
								.trim().toLowerCase().contains(text.getValue().trim().toLowerCase()) : false;
					});

					// Map<Object, Object> map = results.stream().collect(Collectors.toMap(p -> p, p -> p.getCode()));

					return results;
				}
			};
		}
	}

}
