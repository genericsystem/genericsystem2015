package org.genericsystem.todomvc;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Predicate;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.ObservableListWrapperExtended;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.StyleClasses.StyleClass;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.ba_htmltag.HtmlButton;
import org.genericsystem.reactor.ba_htmltag.HtmlCheckBox;
import org.genericsystem.reactor.ba_htmltag.HtmlDiv;
import org.genericsystem.reactor.ba_htmltag.HtmlFooter;
import org.genericsystem.reactor.ba_htmltag.HtmlH1;
import org.genericsystem.reactor.ba_htmltag.HtmlHeader;
import org.genericsystem.reactor.ba_htmltag.HtmlHyperLink;
import org.genericsystem.reactor.ba_htmltag.HtmlInputText;
import org.genericsystem.reactor.ba_htmltag.HtmlLabel;
import org.genericsystem.reactor.ba_htmltag.HtmlLi;
import org.genericsystem.reactor.ba_htmltag.HtmlSpan;
import org.genericsystem.reactor.ba_htmltag.HtmlStrong;
import org.genericsystem.reactor.ba_htmltag.HtmlUl;
import org.genericsystem.reactor.ca_gscomponents.GSApp;
import org.genericsystem.reactor.ca_gscomponents.GSDiv;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyDiv;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyDiv.MyDiv2;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyDiv.MyDiv2.MyHtmlUl;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyDiv.MyDiv2.MyHtmlUl.MyHtmlLi;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyDiv.MyDiv2.MyHtmlUl.MyHtmlLi.MyHtmlDiv2;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyDiv.MyDiv2.MyHtmlUl.MyHtmlLi.MyHtmlDiv2.MyHtmlButton2;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyDiv.MyDiv2.MyHtmlUl.MyHtmlLi.MyHtmlDiv2.MyHtmlCheckBox;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyDiv.MyDiv2.MyHtmlUl.MyHtmlLi.MyHtmlDiv2.MyHtmlLabel;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyDiv.MyHeader;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyDiv.MyHeader.MyHtmlH1;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyDiv.MyHeader.MyHtmlInputText;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyDiv.MyHtmlFooter1;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyDiv.MyHtmlFooter1.MyHtmlDiv3;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyDiv.MyHtmlFooter1.MyHtmlDiv3.MyHtmlButton;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyDiv.MyHtmlFooter1.MyHtmlDiv3.MyHtmlSpan;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyDiv.MyHtmlFooter1.MyHtmlDiv3.MyHtmlSpan.MyHtmlStrong;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyDiv.MyHtmlFooter1.MyHtmlDiv3.MyHtmlUl2;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyDiv.MyHtmlFooter1.MyHtmlDiv3.MyHtmlUl2.MyHtmlLi2;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyDiv.MyHtmlFooter1.MyHtmlDiv3.MyHtmlUl2.MyHtmlLi2.MyHtmlHyperLink;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyDiv.MyHtmlFooter1.MyHtmlDiv3.MyHtmlUl2.MyHtmlLi3;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyDiv.MyHtmlFooter1.MyHtmlDiv3.MyHtmlUl2.MyHtmlLi3.MyHtmlHyperLink2;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyDiv.MyHtmlFooter1.MyHtmlDiv3.MyHtmlUl2.MyHtmlLi4;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyDiv.MyHtmlFooter1.MyHtmlDiv3.MyHtmlUl2.MyHtmlLi4.MyHtmlHyperLink3;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyHtmlFooter2;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyHtmlFooter2.MyHtmlDiv4;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyHtmlFooter2.MyHtmlDiv4.MyHtmlButton3;
import org.genericsystem.todomvc.TodoApp2.MyHtmlDiv.MyHtmlFooter2.MyHtmlDiv4.MyHtmlButton4;
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
@ReactorDependencies({ MyHtmlDiv.class })
// @ReactorDependencies(path = { MyHtmlDiv.class }, value = { MyHeader.class })
// @ReactorDependencies(path = { MyHeader.class }, value = { MyHtmlH1.class, MyHtmlInputText.class })
public class TodoApp2 extends GSApp {

	public static final String FILTER_MODE = "mode";
	public static final String TODOS = "todos";
	public static final String FILTERED_TODOS = "filteredTodos";
	public static final String ACTIVE_TODOS = "activeTodos";
	public static final String COMPLETED = "completed";
	public static final String COMPLETED_TODOS = "completedTodos";

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, TodoApp2.class, "/todo2/");
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

	@Override
	public void init() {
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
	}

	@ReactorDependencies({ MyDiv.class, MyHtmlFooter2.class })
	public static class MyHtmlDiv extends HtmlDiv {

		@StyleClass("todoapp")
		@ReactorDependencies({ MyHeader.class, MyDiv2.class, MyHtmlFooter1.class })
		public static class MyDiv extends GSDiv {

			@StyleClass("header")
			@ReactorDependencies({ MyHtmlH1.class, MyHtmlInputText.class })
			public static class MyHeader extends HtmlHeader {

				public static class MyHtmlH1 extends HtmlH1 {

					@Override
					public void init() {
						setText("todos");
					}
				}

				@StyleClass("new-todo")
				public static class MyHtmlInputText extends HtmlInputText {

					@Override
					public void init() {
						addAttribute("placeholder", "What needs to be done?");
						bindAction(model -> {
							String value = getDomNodeAttributes(model).get("value");
							if (value != null && !value.isEmpty())
								model.find(Todos.class).addInstance(value);
							getDomNodeAttributes(model).put("value", null);
						});
					}
				}
			}

			@StyleClass("main")
			@ReactorDependencies(MyHtmlUl.class)
			public static class MyDiv2 extends GSDiv {

				@StyleClass("todo-list")
				@ReactorDependencies(MyHtmlLi.class)
				public static class MyHtmlUl extends HtmlUl {

					@ReactorDependencies(MyHtmlDiv2.class)
					public static class MyHtmlLi extends HtmlLi {

						@Override
						public void init() {
							storeProperty(COMPLETED, model -> {
								Generic completed = model.getGeneric().getHolder(model.getGeneric().getRoot().find(Completed.class));
								return new SimpleBooleanProperty(completed != null && Boolean.TRUE.equals(completed.getValue()) ? true : false);
							});
							forEach2(model -> getFilteredTodos(model));
							bindOptionalStyleClass(COMPLETED, COMPLETED);
						}

						private ObservableList<Generic> getFilteredTodos(Context model) {
							return this.<ObservableList<Generic>> getProperty(FILTERED_TODOS, model).getValue();
						}

						@StyleClass("view")
						@ReactorDependencies({ MyHtmlCheckBox.class, MyHtmlLabel.class, MyHtmlButton2.class })
						public static class MyHtmlDiv2 extends HtmlDiv {

							@StyleClass("toggle")
							public static class MyHtmlCheckBox extends HtmlCheckBox {

								@Override
								public void init() {
									addPrefixBinding(todo -> {
										if (Boolean.TRUE.equals(getObservableValue(COMPLETED, todo).getValue())) {
											getDomNodeAttributes(todo).put(ReactorStatics.CHECKED, ReactorStatics.CHECKED);
										}
									});
									bindOptionalBiDirectionalAttribute(COMPLETED, ReactorStatics.CHECKED, ReactorStatics.CHECKED);
									addPropertyChangeListener(COMPLETED, (model, nva) -> model.getGeneric().setHolder(model.find(Completed.class), nva));
								}
							}

							public static class MyHtmlLabel extends HtmlLabel {

								@Override
								public void init() {
									bindText();
								}
							}

							@StyleClass("destroy")
							public static class MyHtmlButton2 extends HtmlButton {

								@Override
								public void init() {
									bindAction(Context::remove);
								}
							}
						}
					}
				}
			}

			@StyleClass("footer")
			@ReactorDependencies(MyHtmlDiv3.class)
			public static class MyHtmlFooter1 extends HtmlFooter {

				@Override
				public void init() {
					bindOptionalStyleClass("hide", "hasNoTodo", model -> Bindings.createBooleanBinding(() -> getTodos(model).size() == 0 ? true : false, getTodos(model)));
				}

				private ObservableList<Generic> getTodos(Context model) {
					return this.<ObservableList<Generic>> getProperty(TODOS, model).getValue();
				}

				@ReactorDependencies({ MyHtmlSpan.class, MyHtmlUl2.class, MyHtmlButton.class })
				public static class MyHtmlDiv3 extends HtmlDiv {

					@StyleClass("todo-count")
					@ReactorDependencies(MyHtmlStrong.class)
					public static class MyHtmlSpan extends HtmlSpan {

						public static class MyHtmlStrong extends HtmlStrong {

							@Override
							public void init() {
								bindText(model -> Bindings.createStringBinding(() -> {
									int size = getActiveTodos(model).size();
									return size + " item" + (size > 1 ? "s" : "") + " left";
								}, getActiveTodos(model)));
							}

							private ObservableList<Generic> getActiveTodos(Context model) {
								return this.<ObservableList<Generic>> getProperty(ACTIVE_TODOS, model).getValue();
							}
						}
					}

					@StyleClass("filters")
					@ReactorDependencies({ MyHtmlLi2.class, MyHtmlLi3.class, MyHtmlLi4.class })
					public static class MyHtmlUl2 extends HtmlUl {

						@ReactorDependencies(MyHtmlHyperLink.class)
						public static class MyHtmlLi2 extends HtmlLi {

							public static class MyHtmlHyperLink extends HtmlHyperLink {

								@Override
								public void init() {
									setText("All");
									bindAction(model -> getModeProperty(model).setValue(ALL));
									bindOptionalStyleClass("selected", "allMode", model -> Bindings.equal((ObservableObjectValue<?>) getModeProperty(model), ALL));
								}

								private Property<Predicate<Generic>> getModeProperty(Context model) {
									return getProperty(FILTER_MODE, model);
								}
							}
						}

						@ReactorDependencies(MyHtmlHyperLink2.class)
						public static class MyHtmlLi3 extends HtmlLi {

							public static class MyHtmlHyperLink2 extends HtmlHyperLink {

								@Override
								public void init() {
									setText("Actives");
									bindAction(model -> getModeProperty(model).setValue(ACTIVE));
									bindOptionalStyleClass("selected", "activeMode", model -> Bindings.equal((ObservableObjectValue<?>) getModeProperty(model), ACTIVE));
								}

								private Property<Predicate<Generic>> getModeProperty(Context model) {
									return getProperty(FILTER_MODE, model);
								}
							}
						}

						@ReactorDependencies(MyHtmlHyperLink3.class)
						public static class MyHtmlLi4 extends HtmlLi {

							public static class MyHtmlHyperLink3 extends HtmlHyperLink {

								@Override
								public void init() {
									setText("Completes");
									bindAction(model -> getModeProperty(model).setValue(COMPLETE));
									bindOptionalStyleClass("selected", "completeMode", model -> Bindings.equal((ObservableObjectValue<?>) getModeProperty(model), COMPLETE));
								}

								private Property<Predicate<Generic>> getModeProperty(Context model) {
									return getProperty(FILTER_MODE, model);
								}
							}
						}

					}

					@StyleClass("clear-completed")
					public static class MyHtmlButton extends HtmlButton {

						@Override
						public void init() {
							bindAction(model -> new ArrayList<>(getCompletedTodos(model)).forEach(Generic::remove));
							bindText(model -> Bindings.createStringBinding(() -> "Clear completed (" + getCompletedTodos(model).size() + ")", getCompletedTodos(model)));
							bindOptionalStyleClass("hide", "hasNoCompleted", model -> Bindings.createBooleanBinding(() -> getCompletedTodos(model).size() == 0 ? true : false, getCompletedTodos(model)));
						}

						private ObservableList<Generic> getCompletedTodos(Context model) {
							return this.<ObservableList<Generic>> getProperty(COMPLETED_TODOS, model).getValue();
						}
					}
				}
			}
		}

		@ReactorDependencies(MyHtmlDiv4.class)
		public static class MyHtmlFooter2 extends HtmlFooter {

			@StyleClass("save-cancel")
			@ReactorDependencies({ MyHtmlButton3.class, MyHtmlButton4.class })
			public static class MyHtmlDiv4 extends HtmlFooter {

				@StyleClass("save")
				public static class MyHtmlButton3 extends HtmlButton {

					@Override
					public void init() {
						setText("Save");
						bindAction(Context::flush);
					}
				}

				@StyleClass("cancel")
				public static class MyHtmlButton4 extends HtmlButton {

					@Override
					public void init() {
						setText("Cancel");
						bindAction(Context::cancel);
					}
				}
			}
		}
	}
}