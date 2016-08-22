package org.genericsystem.todomvc;

import java.util.ArrayList;
import java.util.function.Predicate;

import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.defaults.tools.TransformationObservableList;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.gs.GSApp;
import org.genericsystem.reactor.gs.GSSection;
import org.genericsystem.reactor.gstag.GSButton;
import org.genericsystem.reactor.gstag.GSCheckBox;
import org.genericsystem.reactor.gstag.GSDiv;
import org.genericsystem.reactor.gstag.GSFooter;
import org.genericsystem.reactor.gstag.GSH1;
import org.genericsystem.reactor.gstag.GSHyperLink;
import org.genericsystem.reactor.gstag.GSInputText;
import org.genericsystem.reactor.gstag.GSLabel;
import org.genericsystem.reactor.gstag.GSLi;
import org.genericsystem.reactor.gstag.GSSpan;
import org.genericsystem.reactor.gstag.GSStrong;
import org.genericsystem.reactor.gstag.GSUl;
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

	public static void main(String[] mainArgs) {
		ApplicationServer.sartSimpleGenericApp(mainArgs, TodoApp.class, "/todo/");
	}

	private Property<Predicate<Generic>> getModeProperty(Model model) {
		return getProperty(FILTER_MODE, model);
	}

	private ObservableList<Generic> getTodos(Model model) {
		return (ObservableList<Generic>) getProperty(TODOS, model).getValue();
	}

	private ObservableList<Generic> getFilteredTodos(Model model) {
		return (ObservableList<Generic>) getProperty(FILTERED_TODOS, model).getValue();
	}

	static Predicate<Generic> ALL = null;
	static Predicate<Generic> ACTIVE = todo -> {
		Generic completed = todo.getObservableHolder(todo.getRoot().find(Completed.class)).getValue();
		return completed != null ? Boolean.FALSE.equals(completed.getValue()) : true;
	};
	static Predicate<Generic> COMPLETE = ACTIVE.negate();

	public TodoApp(Root engine) {

		createNewInitializedProperty(TODOS,
				// New todos not displayed with ObservableListWrapper instead of TransformationObservableList
				model -> new TransformationObservableList<>(engine.find(Todos.class).getObservableSubInstances(), g -> g, todo -> new Observable[] { ((Generic) todo).getObservableHolder(((Generic) todo).getRoot().find(Completed.class)) }));
		createNewInitializedProperty(FILTER_MODE, model -> ALL);
		createNewInitializedProperty(FILTERED_TODOS, model -> {
			FilteredList<Generic> filtered = new FilteredList<>(getTodos(model));
			filtered.predicateProperty().bind(getModeProperty(model));
			return filtered;
		});

		new GSDiv(this) {
			{
				new GSSection(this) {
					{
						addStyleClass("todoapp");
						new GSSection(this) {
							{
								addStyleClass("header");
								new GSH1(this) {
									{
										setText("todos");
									}
								};
								new GSInputText(this) {
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
								new GSUl(this) {
									{
										addStyleClass("todo-list");
										new GSLi(this) {
											{
												storeProperty("observableHolder", model -> model.getGeneric().getObservableHolder(model.getGeneric().getRoot().find(Completed.class)));
												storeProperty(ReactorStatics.COMPLETED, model -> new SimpleBooleanProperty(
														getObservableValue("observableHolder", model).getValue() != null && Boolean.TRUE.equals(((Generic) getObservableValue("observableHolder", model).getValue()).getValue()) ? true : false));
												forEach(model -> getFilteredTodos(model), (model, generic) -> new GenericModel(model, GenericModel.addToGenerics(generic, ((GenericModel) model).getGenerics())));
												bindOptionalStyleClass(ReactorStatics.COMPLETED, ReactorStatics.COMPLETED);
												new GSDiv(this) {
													{
														addStyleClass("view");
														new GSCheckBox(this) {
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
														new GSLabel(this) {
															{
																bindText();
															}
														};
														new GSButton(this) {
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

						new GSFooter(this) {
							{
								addStyleClass("footer");
								bindOptionalStyleClass("hide", "hasNoTodo", model -> Bindings.createBooleanBinding(() -> getTodos(model).size() == 0 ? true : false, getTodos(model)));
								new GSDiv(this) {
									{
										new GSSpan(this) {
											{
												addStyleClass("todo-count");
												new GSStrong(this) {
													{
														bindText(model -> Bindings.createStringBinding(() -> {
															int size = getTodos(model).filtered(ACTIVE).size();
															return size > 1 ? size + " items left" : "1 item left";
														}, getTodos(model).filtered(ACTIVE)));
													}
												};
											}
										};
										new GSUl(this) {
											{
												addStyleClass("filters");
												new GSLi(this) {
													{
														new GSHyperLink(this, "All", model -> getModeProperty(model).setValue(ALL)).bindOptionalStyleClass("selected", "allMode", model -> Bindings.equal((ObservableObjectValue) getModeProperty(model), ALL));
													}
												};
												new GSLi(this) {
													{
														new GSHyperLink(this, "Actives", model -> getModeProperty(model).setValue(ACTIVE)).bindOptionalStyleClass("selected", "activeMode",
																model -> Bindings.equal((ObservableObjectValue) getModeProperty(model), ACTIVE));
													}
												};
												new GSLi(this) {
													{
														new GSHyperLink(this, "Completes", model -> getModeProperty(model).setValue(COMPLETE)).bindOptionalStyleClass("selected", "completeMode",
																model -> Bindings.equal((ObservableObjectValue) getModeProperty(model), COMPLETE));
													}
												};
											}
										};
										new GSButton(this) {
											{
												addStyleClass("clear-completed");
												bindAction(model -> {
													for (Generic todo : new ArrayList<>(getTodos(model).filtered(COMPLETE)))
														todo.remove();
												});
												bindText(model -> Bindings.createStringBinding(() -> "Clear completed (" + getTodos(model).filtered(COMPLETE).size() + ")", getTodos(model).filtered(COMPLETE)));
												bindOptionalStyleClass("hide", "hasNoCompleted", model -> Bindings.createBooleanBinding(() -> getTodos(model).filtered(COMPLETE).size() == 0 ? true : false, getTodos(model).filtered(COMPLETE)));
											}
										};
									}
								};
							}
						};
					}
				};
				new GSFooter(this) {
					{
						new GSDiv(this) {
							{
								addStyleClass("save-cancel");
								new GSButton(this) {
									{
										addStyleClass("save");
										setText("Save");
										bindAction(model -> engine.getCurrentCache().flush());
									}
								};
								new GSButton(this) {
									{
										addStyleClass("cancel");
										setText("Cancel");
										bindAction(model -> engine.getCurrentCache().clear());
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