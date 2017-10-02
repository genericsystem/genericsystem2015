package org.genericsystem.todomvc;

import java.util.function.Predicate;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.BidirectionalBinding;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.SetText;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.annotations.TagName;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.context.ContextAction.CANCEL;
import org.genericsystem.reactor.context.ContextAction.FLUSH;
import org.genericsystem.reactor.context.ObservableListExtractor.SUBINSTANCES;
import org.genericsystem.reactor.context.TagSwitcher;
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

import io.reactivex.Observable;
import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableObjectValue;
import javafx.beans.value.ObservableValue;

/**
 * @author Nicolas Feybesse
 *
 */
@DependsOnModel({ Todos.class, Completed.class })
@Children({ MyHtmlDiv.class })
public class TodoApp extends RootTagImpl {

	public static final String FILTER_MODE = "mode";
	public static final String COMPLETED = "completed";

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, TodoApp.class, "/todo2/");
	}

	protected static Property<Predicate<ObservableValue<Generic>>> getModeProperty(Tag tag, Context model) {
		return tag.getContextProperty(FILTER_MODE, model);
	}

	@Override
	public void init() {
		addContextAttribute(FILTER_MODE, c -> new SimpleObjectProperty<Predicate<ObservableValue<Generic>>>(ALL));
	}

	public static class StateFilter implements TagSwitcher {
		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			Generic todo = context.getGeneric();
			ObservableValue<Generic> completed = todo.getObservableHolder(todo.getRoot().find(Completed.class));
			Property<Predicate<ObservableValue<Generic>>> mode = getModeProperty(tag, context);
			return Bindings.createBooleanBinding(() -> mode.getValue().test(completed), completed, mode);
		}
	}

	static Predicate<ObservableValue<Generic>> ALL = o -> true;
	static Predicate<ObservableValue<Generic>> ACTIVE = completedObs -> {
		return completedObs.getValue() != null ? Boolean.FALSE.equals(completedObs.getValue().getValue()) : true;
	};
	static Predicate<ObservableValue<Generic>> COMPLETE = ACTIVE.negate();

	static Predicate<Generic> completed = todo -> {
		Generic completed = todo.getHolder(todo.getRoot().find(Completed.class));
		return completed != null && Boolean.TRUE.equals(completed.getValue());
	};

	protected static Snapshot<Generic> getTodos(Context context) {
		return context.find(Todos.class).getSubInstances();
	}

	protected static Snapshot<Generic> getCompletedTodos(Context context) {
		return context.find(Completed.class).getSubInstances().filter(comp -> Boolean.TRUE.equals(comp.getValue())).map(comp -> comp.getComponent(0));
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
				@DirectSelect(Todos.class)
				public static class MyHtmlUl extends HtmlUl {

					@Children(MyHtmlDiv2.class)
					@ForEach(SUBINSTANCES.class)
					@Switch(StateFilter.class)
					public static class MyHtmlLi extends HtmlLi {

						@Override
						public void init() {
							addContextAttribute(COMPLETED, context -> new SimpleBooleanProperty(completed.test(context.getGeneric())));
							bindOptionalStyleClass(COMPLETED, COMPLETED);
						}

						@StyleClass("view")
						@Children({ MyHtmlCheckBox.class, MyHtmlLabel.class, MyHtmlButton2.class })
						public static class MyHtmlDiv2 extends HtmlDiv {

							@StyleClass("toggle")
							@TagName(value = TagName.INPUT, type = TagName.CHECKBOX)
							public static class MyHtmlCheckBox extends TagImpl {
								@Override
								public void init() {
									addPrefixBinding(todo -> {
										Property<Boolean> completedProperty = getContextProperty(COMPLETED, todo);
										ObservableValue<Generic> completed = todo.getGeneric().getObservableHolder(todo.getGeneric().getRoot().find(Completed.class));
										Property<Generic> completedGenericProperty = new SimpleObjectProperty<>(completed.getValue());
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
					addContextAttribute("hasNoTodo", context -> new SimpleBooleanProperty());
					addPrefixBinding(context -> context.getHtmlDomNode(this).getDisposables().add(getTodos(context).setOnChanged()
							.map(set -> set.isEmpty()).subscribe(empty -> getContextProperty("hasNoTodo", context).setValue(empty))));
					bindOptionalStyleClass("hidden", "hasNoTodo");
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
								bindText(context -> Observable.combineLatest(getTodos(context).setOnChanged(), getCompletedTodos(context).setOnChanged(),
										(todos, completed) -> todos.size() - completed.size()).map(size -> size + " item" + (size > 1 ? "s" : "") + " left"));
							}
						}
					}

					@StyleClass("filters")
					@Children({ MyHtmlLi2.class, MyHtmlLi3.class, MyHtmlLi4.class })
					public static class MyHtmlUl2 extends HtmlUl {

						@Children(MyHtmlHyperLink.class)
						public static class MyHtmlLi2 extends HtmlLi {

							@SetText("All")
							public static class MyHtmlHyperLink extends HtmlHyperLink {

								@Override
								public void init() {
									bindAction(model -> getModeProperty(this, model).setValue(ALL));
									bindOptionalStyleClass("selected", "allMode", model -> Bindings.equal((ObservableObjectValue<?>) getModeProperty(this, model), ALL));
								}
							}
						}

						@Children(MyHtmlHyperLink2.class)
						public static class MyHtmlLi3 extends HtmlLi {

							@SetText("Actives")
							public static class MyHtmlHyperLink2 extends HtmlHyperLink {

								@Override
								public void init() {
									bindAction(model -> getModeProperty(this, model).setValue(ACTIVE));
									bindOptionalStyleClass("selected", "activeMode", model -> Bindings.equal((ObservableObjectValue<?>) getModeProperty(this, model), ACTIVE));
								}
							}
						}

						@Children(MyHtmlHyperLink3.class)
						public static class MyHtmlLi4 extends HtmlLi {

							@SetText("Completes")
							public static class MyHtmlHyperLink3 extends HtmlHyperLink {

								@Override
								public void init() {
									bindAction(model -> getModeProperty(this, model).setValue(COMPLETE));
									bindOptionalStyleClass("selected", "completeMode", model -> Bindings.equal((ObservableObjectValue<?>) getModeProperty(this, model), COMPLETE));
								}
							}
						}
					}

					@StyleClass("clear-completed")
					public static class MyHtmlButton extends HtmlButton {

						@Override
						public void init() {
							bindAction(model -> getCompletedTodos(model).toList().forEach(Generic::remove));
							bindText(model -> getCompletedTodos(model).setOnChanged().map(list -> list.size()).map(size -> "Clear completed (" + size + ")"));
							addContextAttribute("hasNoCompleted", context -> new SimpleBooleanProperty());
							addPrefixBinding(context -> context.getHtmlDomNode(this).getDisposables().add(getCompletedTodos(context).setOnChanged()
									.map(set -> set.isEmpty()).subscribe(empty -> getContextProperty("hasNoCompleted", context).setValue(empty))));
							bindOptionalStyleClass("hidden", "hasNoCompleted");
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
				@SetText("Save")
				@BindAction(FLUSH.class)
				public static class MyHtmlButton3 extends HtmlButton {
				}

				@StyleClass("cancel")
				@SetText("Cancel")
				@BindAction(CANCEL.class)
				public static class MyHtmlButton4 extends HtmlButton {
				}
			}
		}
	}
}