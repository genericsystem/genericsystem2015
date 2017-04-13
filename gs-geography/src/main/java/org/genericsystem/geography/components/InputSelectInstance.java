package org.genericsystem.geography.components;

import org.genericsystem.common.Generic;
import org.genericsystem.geography.components.InputSelectInstance.ResultUl;
import org.genericsystem.geography.components.InputSelectInstance.SearchInput;
import org.genericsystem.geography.components.InputSelectInstance.SimpleBr;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.ForEachContext;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.context.ObservableListExtractorFromContext;
import org.genericsystem.reactor.context.StringExtractor;
import org.genericsystem.reactor.context.TextBinding;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlBr;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlInputText;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLi;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlUl;

import javafx.beans.binding.ListBinding;
import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.MapChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.transformation.SortedList;

@Children({ SearchInput.class, SimpleBr.class, ResultUl.class })
public class InputSelectInstance extends HtmlDiv {

	@Override
	public void init() {
		createNewContextProperty("txt");
		createNewContextProperty("selected");
	}

	@Children(ResultLi.class)
	@Style(name = "margin-top", value = "0")
	@Style(name = "background-color", value = "#fff")
	@Style(name = "display", value = "inline-block")
	@Style(name = "list-style-type", value = "none")
	@Style(name = "position", value = "absolute")
	@Style(name = "z-index", value = "200")
	@Style(name = "padding-left", value = "0")
	public static class ResultUl extends HtmlUl {
	}

	@ForEachContext(TEXT_FILTERED.class)
	@Children({ ResultLink.class })
	@Style(name = "background-color", value = "1.4em")
	@StyleClass("autocomplete")
	public static class ResultLi extends HtmlLi {
	}

	@BindText(GENERIC_TEXT.class)
	@Style(name = "display", value = "block")
	@Style(name = "text-decoration", value = "none")
	@Style(name = "padding-left", value = "10px")
	@Style(name = "padding-right", value = "10px")
	@Style(name = "color", value = "black")
	@BindAction(AutocompleteAction.class)
	public static class ResultLink extends HtmlHyperLink {
	}

	public static class GENERIC_TEXT implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			return new ReadOnlyStringWrapper(((InputSelectInstance) tag.getParent().getParent().getParent())
					.displayInstance(context.getGeneric()));
		}
	}

	public String displayInstance(Generic g) {
		return StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(g);
	}

	@BindText(DEFAULT_TEXT.class)
	public static class SearchInput extends HtmlInputText {

		@Override
		public void init() {
			addPrefixBinding(context -> {
				getDomNodeAttributes(context).addListener((MapChangeListener<String, String>) change -> {
					if ("value".equals(change.getKey())) {
						if (change.wasAdded())
							getContextProperty("txt", context).setValue(change.getValueAdded());
					}
				});
			});
		}
	}

	public static class DEFAULT_TEXT implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			String str = "";
			if (tag.getContextProperty("selected", context).getValue() != null)
				str = ((InputSelectInstance) tag.getParent())
						.displayInstance((Generic) tag.getContextProperty("selected", context).getValue());
			return new ReadOnlyStringWrapper(str);
		}
	}

	public static class SimpleBr extends HtmlBr {

	}

	public static class AutocompleteAction implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			tag.getParent().getParent().getParent().find(SearchInput.class).getDomNodeAttributes(context.getParent())
					.put("value", ((InputSelectInstance) tag.getParent().getParent().getParent())
							.displayInstance(context.getGeneric()));
			tag.getContextProperty("txt", context).setValue("");
			tag.getContextProperty("selected", context).setValue(context.getGeneric());
		}
	}

	public SortedList<Generic> filterInstances(Context c, Property<String> t) {
		return c.getGeneric().getObservableSubInstances()
				.filtered(res -> (t.getValue() != null && t.getValue().length() > 1)
						? ((String) res.getValue()).toLowerCase().startsWith(t.getValue().toLowerCase()) : false)
				.sorted();
	}

	public static class TEXT_FILTERED implements ObservableListExtractorFromContext {
		@Override
		public ObservableList<Generic> apply(Context context, Tag tag) {
			Property<String> text = tag.getContextProperty("txt", context);
			return new ListBinding<Generic>() {
				{
					bind(text);
				}

				@Override
				protected ObservableList<Generic> computeValue() {
					return ((InputSelectInstance) tag.getParent().getParent()).filterInstances(context, text);
				}
			};
		}
	}

}
