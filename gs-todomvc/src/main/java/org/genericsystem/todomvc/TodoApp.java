package org.genericsystem.todomvc;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Predicate;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.BidirectionalBinding;
import org.genericsystem.defaults.tools.ObservableListWrapper;
import org.genericsystem.defaults.tools.RxJavaHelpers;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.annotations.TagName;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlInputText;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLi;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlSpan;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlUl;
import org.genericsystem.reactor.gscomponents.RootTagImpl;
import org.genericsystem.reactor.gscomponents.TagImpl;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyDiv;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyDiv.MyDiv2;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyDiv.MyDiv2.MyHtmlUl;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyDiv.MyDiv2.MyHtmlUl.MyHtmlLi;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyDiv.MyDiv2.MyHtmlUl.MyHtmlLi.MyHtmlDiv2;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyDiv.MyDiv2.MyHtmlUl.MyHtmlLi.MyHtmlDiv2.MyHtmlButton2;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyDiv.MyDiv2.MyHtmlUl.MyHtmlLi.MyHtmlDiv2.MyHtmlCheckBox;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyDiv.MyDiv2.MyHtmlUl.MyHtmlLi.MyHtmlDiv2.MyHtmlLabel;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyDiv.MyHeader;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyDiv.MyHeader.MyHtmlH1;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyDiv.MyHeader.MyHtmlInputText;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyDiv.MyHtmlFooter1;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyDiv.MyHtmlFooter1.MyHtmlDiv3;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyDiv.MyHtmlFooter1.MyHtmlDiv3.MyHtmlButton;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyDiv.MyHtmlFooter1.MyHtmlDiv3.MyHtmlSpan;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyDiv.MyHtmlFooter1.MyHtmlDiv3.MyHtmlSpan.MyHtmlStrong;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyDiv.MyHtmlFooter1.MyHtmlDiv3.MyHtmlUl2;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyDiv.MyHtmlFooter1.MyHtmlDiv3.MyHtmlUl2.MyHtmlLi2;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyDiv.MyHtmlFooter1.MyHtmlDiv3.MyHtmlUl2.MyHtmlLi2.MyHtmlHyperLink;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyDiv.MyHtmlFooter1.MyHtmlDiv3.MyHtmlUl2.MyHtmlLi3;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyDiv.MyHtmlFooter1.MyHtmlDiv3.MyHtmlUl2.MyHtmlLi3.MyHtmlHyperLink2;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyDiv.MyHtmlFooter1.MyHtmlDiv3.MyHtmlUl2.MyHtmlLi4;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyDiv.MyHtmlFooter1.MyHtmlDiv3.MyHtmlUl2.MyHtmlLi4.MyHtmlHyperLink3;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyHtmlFooter2;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyHtmlFooter2.MyHtmlDiv4;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyHtmlFooter2.MyHtmlDiv4.MyHtmlButton3;
import org.genericsystem.todomvc.TodoApp.MyHtmlDiv.MyHtmlFooter2.MyHtmlDiv4.MyHtmlButton4;
import org.genericsystem.todomvc.Todos.Completed;

import javafx.beans.Observable;
import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableObjectValue;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;

/**
 * @author Nicolas Feybesse
 *
 */
@DependsOnModel({ Todos.class, Completed.class })
@Children({ MyHtmlDiv.class })
public class TodoApp extends RootTagImpl {

	public static final String FILTER_MODE = "mode";
	public static final String TODOS = "todos";
	public static final String FILTERED_TODOS = "filteredTodos";
	public static final String ACTIVE_TODOS = "activeTodos";
	public static final String COMPLETED = "completed";
	public static final String COMPLETED_TODOS = "completedTodos";

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, TodoApp.class, "/todo2/");
	}

	private Property<Predicate<Generic>> getModeProperty(Context model) {
		return getContextProperty(FILTER_MODE, model);
	}

	private ObservableList<Generic> getTodos(Context model) {
		return this.<ObservableList<Generic>> getContextProperty(TODOS, model).getValue();
	}

	private Map<Generic, Observable[]> getExtractors(Context model) {
		return this.<Map<Generic, Observable[]>> getContextProperty("extractorMap", model).getValue();
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
		createNewInitializedProperty(TODOS, model -> new ObservableListWrapper<>(model.find(Todos.class).getSubInstances().toObservableList(), todo -> getExtractors(model).get(todo)));
		createNewInitializedProperty(FILTER_MODE, model -> ALL);
		createNewInitializedProperty(FILTERED_TODOS, model -> {
			FilteredList<Generic> filtered = new FilteredList<>(getTodos(model));
			filtered.predicateProperty().bind(getModeProperty(model));
			return filtered;
		});
		createNewInitializedProperty(ACTIVE_TODOS, model -> getTodos(model).filtered(ACTIVE));
		createNewInitializedProperty(COMPLETED_TODOS, model -> getTodos(model).filtered(COMPLETE));
	}

	@Children({ MyDiv.class, MyHtmlFooter2.class })
	public static class MyHtmlDiv extends HtmlDiv {

		@StyleClass("todoapp")
		@Children({ MyHeader.class, MyDiv2.class, MyHtmlFooter1.class })
		public static class MyDiv extends FlexDiv {

			@StyleClass("header")
			@Children({ MyHtmlH1.class, MyHtmlInputText.class })
			@TagName(TagName.HEADER)
			public static class MyHeader extends TagImpl {

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
			@Children(MyHtmlUl.class)
			public static class MyDiv2 extends FlexDiv {

				@StyleClass("todo-list")
				@Children(MyHtmlLi.class)
				public static class MyHtmlUl extends HtmlUl {

					@Children(MyHtmlDiv2.class)
					public static class MyHtmlLi extends HtmlLi {

						@Override
						public void init() {
							addContextAttribute(COMPLETED, model -> {
								Generic completed = model.getGeneric().getHolder(model.getGeneric().getRoot().find(Completed.class));
								return new SimpleBooleanProperty(completed != null && Boolean.TRUE.equals(completed.getValue()) ? true : false);
							});
							forEach2(model -> getFilteredTodos(model));
							bindOptionalStyleClass(COMPLETED, COMPLETED);
						}

						private ObservableList<Generic> getFilteredTodos(Context model) {
							return this.<ObservableList<Generic>> getContextProperty(FILTERED_TODOS, model).getValue();
						}

						@StyleClass("view")
						@Children({ MyHtmlCheckBox.class, MyHtmlLabel.class, MyHtmlButton2.class })
						public static class MyHtmlDiv2 extends HtmlDiv {

							@StyleClass("toggle")
							@TagName(value = TagName.INPUT, type = TagName.CHECKBOX)
							public static class MyHtmlCheckBox extends TagImpl {
								Map<Generic, Observable[]> getExtractors(Context model) {
									return this.<Map<Generic, Observable[]>> getContextProperty("extractorMap", model).getValue();
								}

								@Override
								public void init() {
									addPrefixBinding(todo -> {
										Property<Boolean> completedProperty = getContextProperty(COMPLETED, todo);
										ObservableValue<Generic> completed = (ObservableValue<Generic>) getExtractors(todo.getParent()).get(todo.getGeneric())[0];
										Property<Generic> completedGenericProperty = new SimpleObjectProperty(completed.getValue());
										completed.addListener((ov, v, nv) -> completedGenericProperty.setValue(nv));
										BidirectionalBinding.bind(completedProperty, completedGenericProperty, b -> todo.getGeneric().isAlive() ? todo.getGeneric().setHolder(todo.find(Completed.class), b) : null,
												g -> g == null ? false : (Boolean) g.getValue());
									});
									addStyleClass("toggle");
									addPrefixBinding(todo -> {
										if (Boolean.TRUE.equals(getContextObservableValue(COMPLETED, todo).getValue())) {
											getDomNodeAttributes(todo).put(ReactorStatics.CHECKED, ReactorStatics.CHECKED);
										}
									});
									bindOptionalBiDirectionalAttribute(COMPLETED, ReactorStatics.CHECKED, ReactorStatics.CHECKED);
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
			@Children(MyHtmlDiv3.class)
			@TagName(TagName.FOOTER)
			public static class MyHtmlFooter1 extends TagImpl {

				@Override
				public void init() {
					bindOptionalStyleClass("hide", "hasNoTodo", model -> Bindings.createBooleanBinding(() -> getTodos(model).size() == 0 ? true : false, getTodos(model)));
				}

				private ObservableList<Generic> getTodos(Context model) {
					return this.<ObservableList<Generic>> getContextProperty(TODOS, model).getValue();
				}

				@Children({ MyHtmlSpan.class, MyHtmlUl2.class, MyHtmlButton.class })
				public static class MyHtmlDiv3 extends HtmlDiv {

					@StyleClass("todo-count")
					@Children(MyHtmlStrong.class)
					public static class MyHtmlSpan extends HtmlSpan {

						@TagName(TagName.STRONG)
						public static class MyHtmlStrong extends TagImpl {

							@Override
							public void init() {
								bindText(model -> RxJavaHelpers.changesOf(getActiveTodos(model)).map(list -> {
									int size = list.size();
									return size + " item" + (size > 1 ? "s" : "") + " left";
								}));
							}

							private ObservableList<Generic> getActiveTodos(Context model) {
								return this.<ObservableList<Generic>> getContextProperty(ACTIVE_TODOS, model).getValue();
							}
						}
					}

					@StyleClass("filters")
					@Children({ MyHtmlLi2.class, MyHtmlLi3.class, MyHtmlLi4.class })
					public static class MyHtmlUl2 extends HtmlUl {

						@Children(MyHtmlHyperLink.class)
						public static class MyHtmlLi2 extends HtmlLi {

							public static class MyHtmlHyperLink extends HtmlHyperLink {

								@Override
								public void init() {
									setText("All");
									bindAction(model -> getModeProperty(model).setValue(ALL));
									bindOptionalStyleClass("selected", "allMode", model -> Bindings.equal((ObservableObjectValue<?>) getModeProperty(model), ALL));
								}

								private Property<Predicate<Generic>> getModeProperty(Context model) {
									return getContextProperty(FILTER_MODE, model);
								}
							}
						}

						@Children(MyHtmlHyperLink2.class)
						public static class MyHtmlLi3 extends HtmlLi {

							public static class MyHtmlHyperLink2 extends HtmlHyperLink {

								@Override
								public void init() {
									setText("Actives");
									bindAction(model -> getModeProperty(model).setValue(ACTIVE));
									bindOptionalStyleClass("selected", "activeMode", model -> Bindings.equal((ObservableObjectValue<?>) getModeProperty(model), ACTIVE));
								}

								private Property<Predicate<Generic>> getModeProperty(Context model) {
									return getContextProperty(FILTER_MODE, model);
								}
							}
						}

						@Children(MyHtmlHyperLink3.class)
						public static class MyHtmlLi4 extends HtmlLi {

							public static class MyHtmlHyperLink3 extends HtmlHyperLink {

								@Override
								public void init() {
									setText("Completes");
									bindAction(model -> getModeProperty(model).setValue(COMPLETE));
									bindOptionalStyleClass("selected", "completeMode", model -> Bindings.equal((ObservableObjectValue<?>) getModeProperty(model), COMPLETE));
								}

								private Property<Predicate<Generic>> getModeProperty(Context model) {
									return getContextProperty(FILTER_MODE, model);
								}
							}
						}

					}

					@StyleClass("clear-completed")
					public static class MyHtmlButton extends HtmlButton {

						@Override
						public void init() {
							bindAction(model -> new ArrayList<>(getCompletedTodos(model)).forEach(Generic::remove));
							bindText(model -> RxJavaHelpers.changesOf(getCompletedTodos(model)).map(list -> "Clear completed (" + list.size() + ")"));
							bindOptionalStyleClass("hide", "hasNoCompleted", model -> Bindings.createBooleanBinding(() -> getCompletedTodos(model).size() == 0 ? true : false, getCompletedTodos(model)));
						}

						private ObservableList<Generic> getCompletedTodos(Context model) {
							return this.<ObservableList<Generic>> getContextProperty(COMPLETED_TODOS, model).getValue();
						}
					}
				}
			}
		}

		@Children(MyHtmlDiv4.class)
		@TagName(TagName.FOOTER)
		public static class MyHtmlFooter2 extends TagImpl {

			@StyleClass("save-cancel")
			@Children({ MyHtmlButton3.class, MyHtmlButton4.class })
			@TagName(TagName.FOOTER)
			public static class MyHtmlDiv4 extends TagImpl {

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