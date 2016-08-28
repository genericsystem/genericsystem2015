package org.genericsystem.todomvc;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Predicate;

import javafx.beans.Observable;
import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ObservableObjectValue;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.ObservableListWrapperExtended;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.MetaBinding;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.gs.GSApp;
import org.genericsystem.reactor.gs.GSSection;
import org.genericsystem.reactor.gstag.HtmlButton;
import org.genericsystem.reactor.gstag.HtmlCheckBox;
import org.genericsystem.reactor.gstag.HtmlDiv;
import org.genericsystem.reactor.gstag.HtmlFooter;
import org.genericsystem.reactor.gstag.HtmlH1;
import org.genericsystem.reactor.gstag.HtmlHeader;
import org.genericsystem.reactor.gstag.HtmlHyperLink;
import org.genericsystem.reactor.gstag.HtmlInputText;
import org.genericsystem.reactor.gstag.HtmlLabel;
import org.genericsystem.reactor.gstag.HtmlLi;
import org.genericsystem.reactor.gstag.HtmlSpan;
import org.genericsystem.reactor.gstag.HtmlStrong;
import org.genericsystem.reactor.gstag.HtmlUl;
import org.genericsystem.todomvc.Todos.Completed;

/**
 * @author Nicolas Feybesse
 *
 */
@DependsOnModel({ Todos.class, Completed.class })
public class TodoApp extends GSApp {

	public static final String FILTER_MODE = "mode";
	public static final String TODOS = "todos";
	public static final String FILTERED_TODOS = "filteredTodos";
	public static final String ACTIVE_TODOS = "activeTodos";
	public static final String COMPLETED = "completed";
	public static final String COMPLETED_TODOS = "completedTodos";

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, TodoApp.class, "/todo/");
	}

	private Property<Predicate<Generic>> getModeProperty(Context model) {
		return getProperty(FILTER_MODE, model);
	}

	private ObservableList<Generic> getTodos(Context model) {
		return this.<ObservableList<Generic>> getProperty(TODOS, model).getValue();
	}

	private ObservableList<Generic> getFilteredTodos(Context model) {
		return this.<ObservableList<Generic>> getProperty(FILTERED_TODOS, model).getValue();
	}

	private ObservableList<Generic> getActiveTodos(Context model) {
		return this.<ObservableList<Generic>> getProperty(ACTIVE_TODOS, model).getValue();
	}

	private ObservableList<Generic> getCompletedTodos(Context model) {
		return this.<ObservableList<Generic>> getProperty(COMPLETED_TODOS, model).getValue();
	}

	private Map<Generic, Observable[]> getExtractors(Context model) {
		return this.<Map<Generic, Observable[]>> getProperty("extractorMap", model).getValue();
	}

	static Predicate<Generic> ALL = null;
	static Predicate<Generic> ACTIVE = todo -> {
		Generic completed = todo.getHolder(todo.getRoot().find(Completed.class));
		return completed != null ? Boolean.FALSE.equals(completed.getValue()) : true;
	};
	static Predicate<Generic> COMPLETE = ACTIVE.negate();

	public TodoApp() {

		createNewInitializedProperty("extractorMap", model -> new HashMap<Generic, Observable[]>() {

			private static final long serialVersionUID = -435743147955810836L;

			@Override
			public Observable[] get(Object key) {
				Observable[] result = super.get(key);
				if (result == null)
					put((Generic) key, result = new Observable[] { ((Generic) key).getObservableHolder(((Generic) key).getRoot().find(Completed.class)) });
				return result;
			};
		});
		createNewInitializedProperty(TODOS, model -> new ObservableListWrapperExtended<>(model.find(Todos.class).getObservableSubInstances(), todo -> getExtractors(model).get(todo)));
		createNewInitializedProperty(FILTER_MODE, model -> ALL);
		createNewInitializedProperty(FILTERED_TODOS, model -> {
			FilteredList<Generic> filtered = new FilteredList<>(getTodos(model));
			filtered.predicateProperty().bind(getModeProperty(model));
			return filtered;
		});
		createNewInitializedProperty(ACTIVE_TODOS, model -> getTodos(model).filtered(ACTIVE));
		createNewInitializedProperty(COMPLETED_TODOS, model -> getTodos(model).filtered(COMPLETE));

		new HtmlDiv(this) {
			{
				new GSSection(this) {
					{
						addStyleClass("todoapp");
						new HtmlHeader(this) {
							{
								addStyleClass("header");
								new HtmlH1(this) {
									{
										setText("todos");
									}
								};
								new HtmlInputText(this) {
									{
										addStyleClass("new-todo");
										addAttribute("placeholder", "What needs to be done?");
										bindAction(model -> {
											String value = getDomNodeAttributes(model).get("value");
											if (value != null && !value.isEmpty())
												model.find(Todos.class).addInstance(value);
											getDomNodeAttributes(model).put("value", null);
										});
									}
								};
							}
						};
						new GSSection(this) {
							{
								addStyleClass("main");
								new HtmlUl(this) {
									{
										addStyleClass("todo-list");
										new HtmlLi(this) {
											{
												storeProperty(COMPLETED, model -> {
													Generic completed = model.getGeneric().getHolder(model.getGeneric().getRoot().find(Completed.class));
													return new SimpleBooleanProperty(completed != null && Boolean.TRUE.equals(completed.getValue()) ? true : false);
												});
												forEach(model -> getFilteredTodos(model), MetaBinding.MODEL_BUILDER);
												bindOptionalStyleClass(COMPLETED, COMPLETED);
												new HtmlDiv(this) {
													{
														addStyleClass("view");
														new HtmlCheckBox(this) {
															{
																addStyleClass("toggle");
																addPrefixBinding(todo -> {
																	if (Boolean.TRUE.equals(getObservableValue(COMPLETED, todo).getValue())) {
																		getDomNodeAttributes(todo).put(ReactorStatics.CHECKED, ReactorStatics.CHECKED);
																	}
																});
																bindOptionalBiDirectionalAttribute(COMPLETED, ReactorStatics.CHECKED, ReactorStatics.CHECKED);
																addPropertyChangeListener(COMPLETED, (model, nva) -> model.getGeneric().setHolder(model.find(Completed.class), nva));
															}
														};
														new HtmlLabel(this) {
															{
																bindText();
															}
														};
														new HtmlButton(this) {
															{
																addStyleClass("destroy");
																bindAction(Context::remove);
															}
														};
													}
												};
											}
										};
									}
								};
							}
						};

						new HtmlFooter(this) {
							{
								addStyleClass("footer");
								bindOptionalStyleClass("hide", "hasNoTodo", model -> Bindings.createBooleanBinding(() -> getTodos(model).size() == 0 ? true : false, getTodos(model)));
								new HtmlDiv(this) {
									{
										new HtmlSpan(this) {
											{
												addStyleClass("todo-count");
												new HtmlStrong(this) {
													{
														bindText(model -> Bindings.createStringBinding(() -> {
															int size = getActiveTodos(model).size();
															return size + " item" + (size > 1 ? "s" : "") + " left";
														}, getActiveTodos(model)));
													}
												};
											}
										};
										new HtmlUl(this) {
											{
												addStyleClass("filters");
												new HtmlLi(this) {
													{
														new HtmlHyperLink(this, "All", model -> getModeProperty(model).setValue(ALL)).bindOptionalStyleClass("selected", "allMode",
																model -> Bindings.equal((ObservableObjectValue<?>) getModeProperty(model), ALL));
													}
												};
												new HtmlLi(this) {
													{
														new HtmlHyperLink(this, "Actives", model -> getModeProperty(model).setValue(ACTIVE)).bindOptionalStyleClass("selected", "activeMode",
																model -> Bindings.equal((ObservableObjectValue<?>) getModeProperty(model), ACTIVE));
													}
												};
												new HtmlLi(this) {
													{
														new HtmlHyperLink(this, "Completes", model -> getModeProperty(model).setValue(COMPLETE)).bindOptionalStyleClass("selected", "completeMode",
																model -> Bindings.equal((ObservableObjectValue<?>) getModeProperty(model), COMPLETE));
													}
												};
											}
										};
										new HtmlButton(this) {
											{
												addStyleClass("clear-completed");
												bindAction(model -> new ArrayList<>(getCompletedTodos(model)).forEach(Generic::remove));
												bindText(model -> Bindings.createStringBinding(() -> "Clear completed (" + getCompletedTodos(model).size() + ")", getCompletedTodos(model)));
												bindOptionalStyleClass("hide", "hasNoCompleted", model -> Bindings.createBooleanBinding(() -> getCompletedTodos(model).size() == 0 ? true : false, getCompletedTodos(model)));
											}
										};
									}
								};
							}
						};
					}
				};
				new HtmlFooter(this) {
					{
						new HtmlDiv(this) {
							{
								addStyleClass("save-cancel");
								new HtmlButton(this) {
									{
										addStyleClass("save");
										setText("Save");
										bindAction(Context::flush);
									}
								};
								new HtmlButton(this) {
									{
										addStyleClass("cancel");
										setText("Cancel");
										bindAction(Context::cancel);
									}
								};
								// new HtmlButton(this) {
								// {
								// addStyleClass("cancel");
								// setText("Garbage");
								// bindAction(model -> System.gc());
								// }
								// };
							}
						};
					}
				};
			}
		};
	}
}