package org.gs.events.components;

import java.util.Calendar;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.RxJavaHelpers;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.context.TextBinding;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlInputText;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlP;
import org.gs.events.components.InputDate.DivContainer;
import org.gs.events.model.Date;
import org.gs.events.model.Date.Day;
import org.gs.events.model.Date.Month;
import org.gs.events.model.Date.Year;

import io.reactivex.Observable;
import javafx.beans.property.Property;
import javafx.collections.MapChangeListener;

@Children(DivContainer.class)
public class InputDate extends HtmlDiv {

	@Override
	public void init() {
		createNewContextProperty("error");
		createNewContextProperty("selected");
	}

	@Children({ YearSelect.class, Slash1.class, MonthSelect.class, Slash2.class, DaySelect.class, ErrorMsg.class })
	public static class DivContainer extends HtmlDiv {

		@Override
		public void init() {
			addPrefixBinding(context -> {
				if (context.getGeneric().isInstanceOf(context.find(Date.class)))
					getContextProperty("selected", context.getParent()).setValue(context.getGeneric());
			});
		}

	}

	@Attribute(name = "maxlength", value = "4")
	@Style(name = "width", value = "32px")
	@Style(name = "display", value = "inline")
	@BindText(YEAR_TEXT.class)
	public static class YearSelect extends HtmlInputText {
		@Override
		public void init() {
			addPrefixBinding(context -> {
				this.getDomNodeAttributes(context).addListener((MapChangeListener<String, String>) change -> {
					if ("value".equals(change.getKey())) {
						if (change.wasAdded())
							getContextProperty("selected", context).setValue(checkDate(context, this.getParent()));
					}
				});
			});
		}
	}

	@SetText(value = "/")
	@Style(name = "margin", value = "3px")
	public static class Slash1 extends HtmlLabel {
	}

	public static Generic checkDate(Context context, Tag tag) {
		String yyyy = tag.find(YearSelect.class).getDomNodeAttributes(context).get("value");
		if (yyyy == "")
			yyyy = null;
		String mm = tag.find(MonthSelect.class).getDomNodeAttributes(context).get("value");
		if (mm == "")
			mm = null;
		String dd = tag.find(DaySelect.class).getDomNodeAttributes(context).get("value");
		if (dd == "")
			dd = null;
		if (yyyy != null) {
			try {
				int yr = Integer.parseInt(yyyy);
				Generic year = context.find(Year.class);
				Generic y = year.setInstance(yr);
				if (mm != null) {
					try {
						int mo = Integer.parseInt(mm);
						if (mo < 1 || mo > 12) {
							tag.getContextProperty("error", context)
							.setValue("The month must be an integer between 1 and 12");
							return null;
						} else {
							Generic month = context.find(Month.class);
							Generic m = month.setInstance(mo, y);
							if (dd != null) {
								try {
									int da = Integer.parseInt(dd);
									Calendar cal = Calendar.getInstance();
									cal.set(yr, mo - 1, 15); // the month starts with 0
									int maxVal = cal.getActualMaximum(Calendar.DAY_OF_MONTH);
									if (da >= 1 && da <= maxVal) {
										Generic day = context.find(Day.class);
										tag.getContextProperty("error", context).setValue(null);
										return day.setInstance(da, m);
									} else {
										tag.getContextProperty("error", context)
										.setValue("The day must be an integer between 1 and " + maxVal);
										return null;
									}
								} catch (Exception e) {
									tag.getContextProperty("error", context).setValue("The day must be an integer");
									return null;
								}
							} else {
								tag.getContextProperty("error", context).setValue(null);
								return m;
							}
						}
					} catch (Exception e) {
						tag.getContextProperty("error", context).setValue("The month must be an integer");
						return null;
					}
				} else {
					if (dd == null) {
						tag.getContextProperty("error", context).setValue(null);
						return y;
					} else {
						tag.getContextProperty("error", context).setValue("The month is not set");
						return null;
					}
				}
			} catch (Exception e) {
				tag.getContextProperty("error", context).setValue("The year must be an integer");
				return null;
			}
		} else {
			if (mm != null || dd != null)
				tag.getContextProperty("error", context).setValue("The year is not set");
			else
				tag.getContextProperty("error", context).setValue(null);
			return null;
		}

	}

	@Attribute(name = "maxlength", value = "2")
	@Style(name = "width", value = "16px")
	@Style(name = "display", value = "inline")
	@BindText(MONTH_TEXT.class)
	public static class MonthSelect extends HtmlInputText {
		@Override
		public void init() {
			addPrefixBinding(context -> {
				this.getDomNodeAttributes(context).addListener((MapChangeListener<String, String>) change -> {
					if ("value".equals(change.getKey())) {
						if (change.wasAdded())
							getContextProperty("selected", context).setValue(checkDate(context, this.getParent()));
					}
				});
			});
		}
	}

	@SetText(value = "/")
	@Style(name = "margin", value = "3px")
	public static class Slash2 extends HtmlLabel {
	}

	@Attribute(name = "maxlength", value = "2")
	@Style(name = "width", value = "16px")
	@Style(name = "display", value = "inline")
	@BindText(DAY_TEXT.class)
	public static class DaySelect extends HtmlInputText {
		@Override
		public void init() {
			addPrefixBinding(context -> {
				this.getDomNodeAttributes(context).addListener((MapChangeListener<String, String>) change -> {
					if ("value".equals(change.getKey())) {
						if (change.wasAdded())
							getContextProperty("selected", context).setValue(checkDate(context, this.getParent()));
					}
				});
			});
		}
	}

	@BindText(GENERIC_TEXT.class)
	public static class ErrorMsg extends HtmlP {
	}

	public static class YEAR_TEXT implements TextBinding {
		@Override
		public Observable<String> apply(Context context, Tag tag) {
			Integer year = getYear(context.getGeneric(), context.find(Year.class), context.find(Month.class),
					context.find(Day.class));
			return Observable.just(year != null ? String.valueOf(year) : "");
		}

		public static Integer getYear(Generic g, Generic year, Generic month, Generic day) {
			if (g.isInstanceOf(day))
				return (Integer) g.getBaseComponent().getBaseComponent().getValue();
			else if (g.isInstanceOf(month))
				return (Integer) g.getBaseComponent().getValue();
			else if (g.isInstanceOf(year))
				return (Integer) g.getValue();
			else
				return null;
		}
	}

	public static class MONTH_TEXT implements TextBinding {
		@Override
		public Observable<String> apply(Context context, Tag tag) {
			Integer month = getMonth(context.getGeneric(), context.find(Year.class), context.find(Month.class),
					context.find(Day.class));
			return Observable.just(month != null ? String.valueOf(month) : "");
		}

		public static Integer getMonth(Generic g, Generic year, Generic month, Generic day) {
			if (g.isInstanceOf(day))
				return (Integer) g.getBaseComponent().getValue();
			else if (g.isInstanceOf(month))
				return (Integer) g.getValue();
			else
				return null;
		}
	}

	public static class DAY_TEXT implements TextBinding {
		@Override
		public Observable<String> apply(Context context, Tag tag) {
			Integer day = getDay(context.getGeneric(), context.find(Year.class), context.find(Month.class),
					context.find(Day.class));
			return Observable.just(day != null ? String.valueOf(day) : "");
		}

		public static Integer getDay(Generic g, Generic year, Generic month, Generic day) {
			if (g.isInstanceOf(day))
				return (Integer) g.getValue();
			else
				return null;
		}
	}

	public static class GENERIC_TEXT implements TextBinding {
		@Override
		public Observable<String> apply(Context context, Tag tag) {
			Property<?> prop = tag.getContextProperty("error", context);
			return RxJavaHelpers.optionalValuesOf(prop).map(opt -> opt.isPresent() ? opt.get().toString() : "");
		}
	}

}
