package org.genericsystem.todomvc;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Predicate;

import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.defaults.tools.ObservableListWrapperExtended;
import org.genericsystem.reactor.Model;
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
import org.genericsystem.reactor.gstag.HtmlHyperLink;
import org.genericsystem.reactor.gstag.HtmlInputText;
import org.genericsystem.reactor.gstag.HtmlLabel;
import org.genericsystem.reactor.gstag.HtmlLi;
import org.genericsystem.reactor.gstag.HtmlSpan;
import org.genericsystem.reactor.gstag.HtmlStrong;
import org.genericsystem.reactor.gstag.HtmlUl;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.todomvc.Todos.Completed;

import javafx.beans.Observable;
import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ObservableObjectValue;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;

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
	public static final String COMPLETED_TODOS = "completedTodos";

	public static void main(String[] mainArgs) {
		ApplicationServer.sartSimpleGenericApp(mainArgs, TodoApp.class, "/todo/");
	}

	private Property<Predicate<Generic>> getModeProperty(Model model) {
		return getProperty(FILTER_MODE, model);
	}

	private ObservableList<Generic> getTodos(Model model) {
		return this.<ObservableList<Generic>> getProperty(TODOS, model).getValue();
	}

	private ObservableList<Generic> getFilteredTodos(Model model) {
		return this.<ObservableList<Generic>> getProperty(FILTERED_TODOS, model).getValue();
	}

	private ObservableList<Generic> getActiveTodos(Model model) {
		return this.<ObservableList<Generic>> getProperty(ACTIVE_TODOS, model).getValue();
	}

	private ObservableList<Generic> getCompletedTodos(Model model) {
		return this.<ObservableList<Generic>> getProperty(COMPLETED_TODOS, model).getValue();
	}

	private Map<Generic, Observable[]> getExtractors(Model model) {
		return this.<Map<Generic, Observable[]>> getProperty("extractorMap", model).getValue();
	}

	static Predicate<Generic> ALL = null;
	static Predicate<Generic> ACTIVE = todo -> {
		Generic completed = todo.getHolder(todo.getRoot().find(Completed.class));
		return completed != null ? Boolean.FALSE.equals(completed.getValue()) : true;
	};
	static Predicate<Generic> COMPLETE = ACTIVE.negate();

	public TodoApp(Root engine) {

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
		createNewInitializedProperty(TODOS, model -> new ObservableListWrapperExtended<>(engine.find(Todos.class).getObservableSubInstances(), todo -> getExtractors(model).get(todo)));
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
						new GSSection(this) {
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
										bindAction(model -> {
											String value = model.getObservableAttributes(this).get(ReactorStatics.VALUE);
											if (value != null && !value.isEmpty())
												engine.find(Todos.class).addInstance(value);
											model.getObservableAttributes(this).put(ReactorStatics.VALUE, null);
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
												storeProperty("observableHolder", model -> model.getGeneric().getObservableHolder(model.getGeneric().getRoot().find(Completed.class)));
												storeProperty(ReactorStatics.COMPLETED, model -> new SimpleBooleanProperty(
														getObservableValue("observableHolder", model).getValue() != null && Boolean.TRUE.equals(((Generic) getObservableValue("observableHolder", model).getValue()).getValue()) ? true : false));
												forEach(model -> getFilteredTodos(model), (model, generic) -> new GenericModel(model, GenericModel.addToGenerics(generic, ((GenericModel) model).getGenerics())));
												bindOptionalStyleClass(ReactorStatics.COMPLETED, ReactorStatics.COMPLETED);
												new HtmlDiv(this) {
													{
														addStyleClass("view");
														new HtmlCheckBox(this) {
															{
																addStyleClass("toggle");
																addPrefixBinding(todo -> {
																	if (Boolean.TRUE.equals(getObservableValue(ReactorStatics.COMPLETED, todo).getValue())) {
																		todo.getObservableAttributes(this).put(ReactorStatics.CHECKED, ReactorStatics.CHECKED);
																	}
																});
																bindOptionalBiDirectionalAttribute(ReactorStatics.COMPLETED, ReactorStatics.CHECKED, ReactorStatics.CHECKED);
																addPropertyChangeListener(ReactorStatics.COMPLETED, (model, nva) -> model.getGeneric().setHolder(model.getGeneric().getRoot().find(Completed.class), nva));
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
																bindAction(GenericModel::remove);
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
										bindAction(model -> engine.getCurrentCache().flush());
									}
								};
								new HtmlButton(this) {
									{
										addStyleClass("cancel");
										setText("Cancel");
										bindAction(model -> engine.getCurrentCache().clear());
									}
								};
								new HtmlButton(this) {
									{
										addStyleClass("cancel");
										setText("Garbage");
										bindAction(model -> System.gc());
									}
								};
							}
						};
					}
				};
			}
		};
	}
}