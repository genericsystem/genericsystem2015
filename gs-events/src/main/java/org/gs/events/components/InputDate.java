package org.gs.events.components;

import java.util.Calendar;

import org.genericsystem.common.Generic;
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
import org.gs.events.components.InputDate.DateLabel;
import org.gs.events.components.InputDate.DaySelect;
import org.gs.events.components.InputDate.ErrorMsg;
import org.gs.events.components.InputDate.MonthSelect;
import org.gs.events.components.InputDate.Slash1;
import org.gs.events.components.InputDate.Slash2;
import org.gs.events.components.InputDate.YearSelect;
import org.gs.events.model.Date.Day;
import org.gs.events.model.Date.Month;
import org.gs.events.model.Date.Year;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.MapChangeListener;

@Children({ DateLabel.class, YearSelect.class, Slash1.class, MonthSelect.class, Slash2.class, DaySelect.class,
		ErrorMsg.class })
public class InputDate extends HtmlDiv {

	@Override
	public void init() {
		createNewContextProperty("year");
		createNewContextProperty("month");
		createNewContextProperty("day");
		createNewContextProperty("error");
	}

	@SetText(value = "Date yyyy/mm/dd : ")
	public static class DateLabel extends HtmlLabel {
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
						if (change.wasAdded()) {
							if (!"".equals(change.getValueAdded())) {
								try {
									int yr = Integer.parseInt(change.getValueAdded());
									getContextProperty("error", context).setValue(null);
									getContextProperty("year", context).setValue(yr);
								} catch (Exception e) {
									getContextProperty("error", context).setValue("Incorrect year !");
									getContextProperty("year", context).setValue(null);
								}
							} else {
								if (getContextProperty("month", context).getValue() != null
										|| getContextProperty("day", context).getValue() != null)
									getContextProperty("error", context).setValue("The year is not set !");
								else
									getContextProperty("error", context).setValue(null);
								getContextProperty("year", context).setValue(null);
								getContextProperty("month", context).setValue(null);
								getContextProperty("day", context).setValue(null);
							}
						}
					}
				});
			});
		}
	}

	@SetText(value = "/")
	@Style(name = "margin", value = "3px")
	public static class Slash1 extends HtmlLabel {
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
						if (change.wasAdded()) {
							if (getContextProperty("year", context).getValue() != null) {
								if (!"".equals(change.getValueAdded())) {
									try {
										int mo = Integer.parseInt(change.getValueAdded());
										if (mo >= 1 && mo <= 12) {
											getContextProperty("error", context).setValue(null);
											getContextProperty("month", context).setValue(mo - 1); // starts with 0
										} else {
											getContextProperty("error", context).setValue("Incorrect month !");
											getContextProperty("month", context).setValue(null);
										}
									} catch (Exception e) {
										getContextProperty("error", context).setValue("Incorrect month !");
										getContextProperty("month", context).setValue(null);
									}
								} else {
									getContextProperty("error", context).setValue(null);
									getContextProperty("month", context).setValue(null);
									getContextProperty("day", context).setValue(null);
								}
							} else {
								if (!"".equals(change.getValueAdded())
										|| getContextProperty("day", context).getValue() != null)
									getContextProperty("error", context).setValue("The year is not set !");
								else
									getContextProperty("error", context).setValue(null);
								getContextProperty("month", context).setValue(null);
								getContextProperty("day", context).setValue(null);
							}

						}
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
						if (change.wasAdded()) {
							if (getContextProperty("year", context).getValue() != null) {
								if (getContextProperty("month", context).getValue() != null) {
									if (!"".equals(change.getValueAdded())) {
										try {
											int day = Integer.parseInt(change.getValueAdded());
											Calendar cal = Calendar.getInstance();
											cal.set((int) getContextProperty("year", context).getValue(),
													(int) getContextProperty("month", context).getValue(), 15);
											int maxVal = cal.getActualMaximum(Calendar.DAY_OF_MONTH);
											if (day >= 1 && day <= maxVal) {
												getContextProperty("error", context).setValue(null);
												getContextProperty("day", context).setValue(day);
											} else {
												getContextProperty("error", context).setValue("Incorrect day !");
												getContextProperty("day", context).setValue(null);
											}
										} catch (Exception e) {
											getContextProperty("error", context).setValue("Incorrect day !");
											getContextProperty("day", context).setValue(null);
										}
									} else {
										getContextProperty("error", context).setValue(null);
										getContextProperty("day", context).setValue(null);
									}
								} else {
									if (!"".equals(change.getValueAdded()))
										getContextProperty("error", context).setValue("The month is not set !");
									else
										getContextProperty("error", context).setValue(null);
									getContextProperty("day", context).setValue(null);
								}
							} else {
								if (!"".equals(change.getValueAdded()))
									getContextProperty("error", context).setValue("The year is not set !");
								else
									getContextProperty("error", context).setValue(null);
								getContextProperty("day", context).setValue(null);
							}
						}
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
		public ObservableValue<String> apply(Context context, Tag tag) {
			Integer year = getYear(context.getGeneric(), context.find(Year.class), context.find(Month.class),
					context.find(Day.class));
			return new ReadOnlyStringWrapper(year != null ? String.valueOf(year) : "");
		}

		public static Integer getYear(Generic g, Generic year, Generic month, Generic day) {
			if (g.isInstanceOf(day))
				return (Integer) g.getBaseComponent().getBaseComponent().getValue();
			else if (g.isInstanceOf(month))
				return (Integer) g.getBaseComponent().getValue();
			else
				return (Integer) g.getValue();
		}
	}

	public static class MONTH_TEXT implements TextBinding {
		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			Integer month = getMonth(context.getGeneric(), context.find(Year.class), context.find(Month.class),
					context.find(Day.class));
			return new ReadOnlyStringWrapper(month != null ? String.valueOf(month) : "");
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
		public ObservableValue<String> apply(Context context, Tag tag) {
			Integer day = getDay(context.getGeneric(), context.find(Year.class), context.find(Month.class),
					context.find(Day.class));
			return new ReadOnlyStringWrapper(day != null ? String.valueOf(day) : "");
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
		public ObservableValue<String> apply(Context context, Tag tag) {
			Property<?> prop = tag.getContextProperty("error", context);
			return Bindings.createStringBinding(() -> prop.getValue() != null ? prop.getValue().toString() : "", prop);
		}
	}

}
