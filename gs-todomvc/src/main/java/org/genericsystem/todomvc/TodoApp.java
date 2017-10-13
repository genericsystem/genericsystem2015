package org.genericsystem.todomvc;

import java.util.Optional;
import java.util.function.Predicate;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.RxJavaHelpers;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Attribute;
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
import org.genericsystem.reactor.context.ForEachExtractor.SUBINSTANCES;
import org.genericsystem.reactor.context.TagSwitcher;
import org.genericsystem.reactor.gscomponents.FlexDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlFooter;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlH1;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHeader;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlHyperLink;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlInputText;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLabel.GSLabelDisplayer;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlLi;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlSpan;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlStrong;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlUl;
import org.genericsystem.reactor.gscomponents.RootTagImpl;
import org.genericsystem.reactor.gscomponents.TagImpl;
import org.genericsystem.todomvc.TodoApp.RootDiv;
import org.genericsystem.todomvc.TodoApp.RootDiv.ContentDiv;
import org.genericsystem.todomvc.TodoApp.RootDiv.ContentDiv.TodoInput;
import org.genericsystem.todomvc.TodoApp.RootDiv.ContentDiv.TodosContent;
import org.genericsystem.todomvc.TodoApp.RootDiv.ContentDiv.TodosContent.TodoItem;
import org.genericsystem.todomvc.TodoApp.RootDiv.ContentDiv.TodosContent.TodoItem.ItemContent;
import org.genericsystem.todomvc.TodoApp.RootDiv.ContentDiv.TodosContent.TodoItem.ItemContent.RemoveButton;
import org.genericsystem.todomvc.TodoApp.RootDiv.ContentDiv.TodosContent.TodoItem.ItemContent.TodoCheckBox;
import org.genericsystem.todomvc.TodoApp.RootDiv.ContentDiv.TodosFooter;
import org.genericsystem.todomvc.TodoApp.RootDiv.ContentDiv.TodosFooter.ClearCompletedButton;
import org.genericsystem.todomvc.TodoApp.RootDiv.ContentDiv.TodosFooter.Filters;
import org.genericsystem.todomvc.TodoApp.RootDiv.ContentDiv.TodosFooter.Filters.ModeActiveLink;
import org.genericsystem.todomvc.TodoApp.RootDiv.ContentDiv.TodosFooter.Filters.ModeAllLink;
import org.genericsystem.todomvc.TodoApp.RootDiv.ContentDiv.TodosFooter.Filters.ModeCompleteLink;
import org.genericsystem.todomvc.TodoApp.RootDiv.ContentDiv.TodosFooter.ItemsLeft;
import org.genericsystem.todomvc.TodoApp.RootDiv.MainFooter;
import org.genericsystem.todomvc.Todos.Completed;

import io.reactivex.Observable;
import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableObjectValue;

/**
 * @author Nicolas Feybesse
 *
 */
@DependsOnModel({ Todos.class, Completed.class })
@Children({ RootDiv.class })
public class TodoApp extends RootTagImpl {

	public static final String FILTER_MODE = "mode";
	public static final String COMPLETED = "completed";

	public static void main(String[] mainArgs) {
		ApplicationServer.startSimpleGenericApp(mainArgs, TodoApp.class, "/todo2/");
	}

	protected static Property<Predicate<Generic>> getModeProperty(Tag tag, Context model) {
		return tag.getContextProperty(FILTER_MODE, model);
	}

	@Override
	public void init() {
		addContextAttribute(FILTER_MODE, c -> new SimpleObjectProperty<Predicate<Generic>>(ALL));
	}

	public static class StateFilter implements TagSwitcher {
		@Override
		public Observable<Boolean> apply(Context context, Tag tag) {
			Generic todo = context.getGeneric();
			Observable<Optional<Generic>> completed = todo.getObservableHolder(todo.getRoot().find(Completed.class));
			Property<Predicate<Generic>> modeProp = getModeProperty(tag, context);
			return Observable.combineLatest(completed, RxJavaHelpers.valuesOf(modeProp),
					(optCompleted, mode) -> mode.test(optCompleted.isPresent() ? optCompleted.get() : null));
		}
	}

	static Predicate<Generic> ALL = o -> true;
	static Predicate<Generic> ACTIVE = completedObs -> {
		return completedObs != null ? Boolean.FALSE.equals(completedObs.getValue()) : true;
	};
	static Predicate<Generic> COMPLETE = ACTIVE.negate();

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

	@Children({ ContentDiv.class, MainFooter.class })
	public static class RootDiv extends HtmlDiv {

		@StyleClass("todoapp")
		@Children({ HtmlHeader.class, TodosContent.class, TodosFooter.class })
		// Header, contains title + input
		@StyleClass(path = HtmlHeader.class, value = "header")
		@Children(path = HtmlHeader.class, value = { HtmlH1.class, TodoInput.class })
		// Page title
		@SetText(path = { HtmlHeader.class, HtmlH1.class}, value = "todos")
		public static class ContentDiv extends FlexDiv {

			@StyleClass("new-todo")
			@Attribute(name = "placeholder", value = "What needs to be done?")
			public static class TodoInput extends HtmlInputText {

				@Override
				public void init() {
					bindAction(model -> {
						String value = getDomNodeAttributes(model).get("value");
						if (value != null && !value.isEmpty())
							model.find(Todos.class).addInstance(value);
						getDomNodeAttributes(model).put("value", null);
					});
				}
			}

			@StyleClass("main")
			@Children(HtmlUl.class)
			// Settings for the list
			@StyleClass(path = HtmlUl.class, value = "todo-list")
			@Children(path = HtmlUl.class, value = TodoItem.class)
			@DirectSelect(path = HtmlUl.class, value = Todos.class)
			public static class TodosContent extends FlexDiv {

				@Children(ItemContent.class)
				@ForEach(SUBINSTANCES.class)
				@Switch(StateFilter.class)
				public static class TodoItem extends HtmlLi {

					@Override
					public void init() {
						// Initialization of the COMPLETED property with the real state of the item.
						addContextAttribute(COMPLETED, context -> new SimpleBooleanProperty(completed.test(context.getGeneric())));
						bindOptionalStyleClass(COMPLETED, COMPLETED);
						// Set to true when the remove button is clicked. Used because otherwise, the removal of the Completed holder
						// causes a change to the COMPLETED property, which triggers recreation of the holder, which
						// prevents the generic’s removal.
						addContextAttribute("removed", context -> new SimpleBooleanProperty(false));
					}

					@StyleClass("view")
					@Children({ TodoCheckBox.class, GSLabelDisplayer.class, RemoveButton.class })
					// Remove button appearing on the right when hovering
					@StyleClass(path = HtmlButton.class, value = "destroy")
					public static class ItemContent extends HtmlDiv {

						@StyleClass("toggle")
						@TagName(value = TagName.INPUT, type = TagName.CHECKBOX)
						public static class TodoCheckBox extends TagImpl {
							@Override
							public void init() {
								addPrefixBinding(todo -> {
									Property<Boolean> completedProperty = getContextProperty(COMPLETED, todo);
									Property<Boolean> removedProperty = getContextProperty("removed", todo);
									Observable<Boolean> completed = todo.getGeneric().getObservableHolder(todo.getGeneric().getRoot().find(Completed.class))
											.map(opt -> opt.isPresent() && (Boolean) opt.get().getValue()).distinctUntilChanged();
									todo.getHtmlDomNode(this).getDisposables().add(completed.subscribe(bool -> completedProperty.setValue(bool)));
									completedProperty.addListener((o, v, nv) -> {
										if (todo.getGeneric().isAlive() && !removedProperty.getValue())
											todo.getGeneric().setHolder(todo.find(Completed.class), nv);
									});
								});
								bindOptionalBiDirectionalAttribute(COMPLETED, ReactorStatics.CHECKED, ReactorStatics.CHECKED);
							}
						}

						public static class RemoveButton extends HtmlButton {
							@Override
							public void init() {
								bindAction(context -> {
									getContextProperty("removed", context).setValue(true);
									context.remove();
								});
							}
						}
					}
				}
			}

			@StyleClass("footer")
			@Children(HtmlDiv.class)
			@Children(path = HtmlDiv.class, value = { HtmlSpan.class, Filters.class, ClearCompletedButton.class })
			@Children(path = { HtmlDiv.class, HtmlSpan.class }, value = ItemsLeft.class)
			@StyleClass(path = { HtmlDiv.class, HtmlSpan.class }, value = "todo-count")
			public static class TodosFooter extends HtmlFooter {

				@Override
				public void init() {
					addContextAttribute("hasNoTodo", context -> new SimpleBooleanProperty());
					addPrefixBinding(context -> context.getHtmlDomNode(this).getDisposables().add(getTodos(context).setOnChanged()
							.map(set -> set.isEmpty()).subscribe(empty -> getContextProperty("hasNoTodo", context).setValue(empty))));
					bindOptionalStyleClass("hidden", "hasNoTodo");
				}

				public static class ItemsLeft extends HtmlStrong {

					@Override
					public void init() {
						bindText(context -> Observable.combineLatest(getTodos(context).setOnChanged(), getCompletedTodos(context).setOnChanged(),
								(todos, completed) -> todos.size() - completed.size()).map(size -> size + " item" + (size > 1 ? "s" : "") + " left"));
					}
				}

				@StyleClass("filters")
				@Children({ HtmlLi.class, HtmlLi.class, HtmlLi.class })
				@Children(path = HtmlLi.class, pos = 0, value = ModeAllLink.class)
				@Children(path = HtmlLi.class, pos = 1, value = ModeActiveLink.class)
				@Children(path = HtmlLi.class, pos = 2, value = ModeCompleteLink.class)
				public static class Filters extends HtmlUl {

					@SetText("All")
					public static class ModeAllLink extends HtmlHyperLink {

						@Override
						public void init() {
							bindAction(model -> getModeProperty(this, model).setValue(ALL));
							bindOptionalStyleClass("selected", "allMode", model -> Bindings.equal((ObservableObjectValue<?>) getModeProperty(this, model), ALL));
						}
					}

					@SetText("Actives")
					public static class ModeActiveLink extends HtmlHyperLink {

						@Override
						public void init() {
							bindAction(model -> getModeProperty(this, model).setValue(ACTIVE));
							bindOptionalStyleClass("selected", "activeMode", model -> Bindings.equal((ObservableObjectValue<?>) getModeProperty(this, model), ACTIVE));
						}
					}

					@SetText("Completes")
					public static class ModeCompleteLink extends HtmlHyperLink {

						@Override
						public void init() {
							bindAction(model -> getModeProperty(this, model).setValue(COMPLETE));
							bindOptionalStyleClass("selected", "completeMode", model -> Bindings.equal((ObservableObjectValue<?>) getModeProperty(this, model), COMPLETE));
						}
					}
				}

				@StyleClass("clear-completed")
				public static class ClearCompletedButton extends HtmlButton {

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

		@StyleClass("save-cancel")
		@Children({ HtmlButton.class, HtmlButton.class})
		// Save button
		@StyleClass(path = HtmlButton.class, pos = 0, value = "save")
		@SetText(path = HtmlButton.class, pos = 0, value = "Save")
		@BindAction(path = HtmlButton.class, pos = 0, value = FLUSH.class)
		// Cancel button
		@StyleClass(path = HtmlButton.class, pos = 1, value = "cancel")
		@SetText(path = HtmlButton.class, pos = 1, value = "Cancel")
		@BindAction(path = HtmlButton.class, pos = 1, value = CANCEL.class)
		public static class MainFooter extends HtmlFooter {
		}
	}
}