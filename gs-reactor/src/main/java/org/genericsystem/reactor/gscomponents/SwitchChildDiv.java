package org.genericsystem.reactor.gscomponents;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.Map;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.BindAction;
import org.genericsystem.reactor.annotations.BindText;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.Switch;
import org.genericsystem.reactor.annotations.Switcher;
import org.genericsystem.reactor.context.ContextAction;
import org.genericsystem.reactor.context.TagSwitcher;
import org.genericsystem.reactor.context.TextBinding;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlButton;
import org.genericsystem.reactor.gscomponents.HtmlTag.HtmlDiv;
import org.genericsystem.reactor.gscomponents.SwitchChildDiv.MainSwitcher;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.ReadOnlyBooleanWrapper;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.value.ObservableValue;

@Switch(MainSwitcher.class)
@Children({ HtmlButton.class, HtmlButton.class })
@BindAction(path = HtmlButton.class, pos = { 0 }, value = SwitchChildDiv.PrevAction.class)
@BindAction(path = HtmlButton.class, pos = { 1 }, value = SwitchChildDiv.NextAction.class)
@BindText(path = HtmlButton.class, pos = { 0 }, value = SwitchChildDiv.PrevTextBinding.class)
@BindText(path = HtmlButton.class, pos = { 1 }, value = SwitchChildDiv.NextTextBinding.class)
@Switch(path = HtmlButton.class, pos = { 0 }, value = SwitchChildDiv.PrevSwitcher.class)
@Switch(path = HtmlButton.class, pos = { 1 }, value = SwitchChildDiv.NextSwitcher.class)
public class SwitchChildDiv extends HtmlDiv {

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface First {
		Class<? extends TagImpl>[] path() default {};

		int[] pos() default {};

		String text()

		default "<<";

		Class<? extends TagImpl> value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface Prev {
		Class<? extends TagImpl>[] path() default {};

		int[] pos() default {};

		String text()

		default "<<";

		Class<? extends TagImpl> value();
	}

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface Next {
		Class<? extends TagImpl>[] path() default {};

		int[] pos() default {};

		String text()

		default ">>";

		Class<? extends TagImpl> value();
	}

	public static class MainSwitcher implements TagSwitcher {

		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			Property<?> selectedClassProperty = tag.getProperty(Switcher.SELECTED_CLASS, context);
			return Bindings.createBooleanBinding(() -> tag.getClass().equals(selectedClassProperty.getValue()), selectedClassProperty);
		}
	}

	public static class PrevAction implements ContextAction {
		@Override
		public void accept(Context context, Tag tag) {
			if (tag.getParent().getMetaBinding() == null) {
				SwitchChildDiv.Prev prev = tag.getParent().getClass().getAnnotation(SwitchChildDiv.Prev.class);
				tag.getProperty(Switcher.SELECTED_CLASS, context).setValue(prev.value());
			} else {
				Property<Map<Tag, SimpleIntegerProperty>> property = tag.getParent().<Map<Tag, SimpleIntegerProperty>> getProperty(Switcher.TAG_INDEX_MAP, context);
				Map<Tag, SimpleIntegerProperty> map = property.getValue();
				SimpleIntegerProperty indexProperty = map.get(tag.getParent());
				if (indexProperty.get() != 0)
					indexProperty.set(indexProperty.get() - 1);
				else {
					SwitchChildDiv.Prev prev = tag.getParent().getClass().getAnnotation(SwitchChildDiv.Prev.class);
					tag.getProperty(Switcher.SELECTED_CLASS, context).setValue(prev.value());
				}
			}
		}
	}

	public static class NextAction implements ContextAction {

		@Override
		public void accept(Context context, Tag tag) {
			if (tag.getParent().getMetaBinding() == null) {
				SwitchChildDiv.Next next = tag.getParent().getClass().getAnnotation(SwitchChildDiv.Next.class);
				tag.getProperty(Switcher.SELECTED_CLASS, context).setValue(next.value());
			} else {
				Property<Map<Tag, SimpleIntegerProperty>> property = tag.getParent().<Map<Tag, SimpleIntegerProperty>> getProperty(Switcher.TAG_INDEX_MAP, context);
				Map<Tag, SimpleIntegerProperty> map = property.getValue();
				SimpleIntegerProperty indexProperty = map.get(tag.getParent());
				if (indexProperty.get() != 1)
					indexProperty.set(indexProperty.get() + 1);
				else {
					SwitchChildDiv.Next next = tag.getParent().getClass().getAnnotation(SwitchChildDiv.Next.class);
					tag.getProperty(Switcher.SELECTED_CLASS, context).setValue(next.value());
				}
			}
		}

	}

	public static class PrevTextBinding implements TextBinding {

		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			SwitchChildDiv.Prev prev = tag.getParent().getClass().getAnnotation(SwitchChildDiv.Prev.class);
			return new ReadOnlyStringWrapper(prev.text() + (tag.getParent().getMetaBinding() != null ? "#" + tag.getParent().<Map<Tag, SimpleIntegerProperty>> getProperty(Switcher.TAG_INDEX_MAP, context).getValue().get(tag.getParent()).get() : ""));
		}

	}

	public static class NextTextBinding implements TextBinding {

		@Override
		public ObservableValue<String> apply(Context context, Tag tag) {
			SwitchChildDiv.Next next = tag.getParent().getClass().getAnnotation(SwitchChildDiv.Next.class);
			return new ReadOnlyStringWrapper(
					next != null ? next.text() + (tag.getParent().getMetaBinding() != null ? "#" + tag.getParent().<Map<Tag, SimpleIntegerProperty>> getProperty(Switcher.TAG_INDEX_MAP, context).getValue().get(tag.getParent()).get() : "") : ">>");
		}

	}

	public static class PrevSwitcher implements TagSwitcher {

		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			if (tag.getParent().getMetaBinding() == null)
				return new ReadOnlyBooleanWrapper(tag.getParent().getClass().getAnnotation(SwitchChildDiv.Prev.class) != null);
			else {
				Property<Map<Tag, SimpleIntegerProperty>> property = tag.getParent().<Map<Tag, SimpleIntegerProperty>> getProperty(Switcher.TAG_INDEX_MAP, context);
				Map<Tag, SimpleIntegerProperty> map = property.getValue();
				SimpleIntegerProperty indexProperty = map.get(tag.getParent());
				return Bindings.createBooleanBinding(() -> indexProperty.get() != 0 || tag.getParent().getClass().getAnnotation(SwitchChildDiv.Prev.class) != null, indexProperty);
			}

		}

	}

	public static class NextSwitcher implements TagSwitcher {

		@Override
		public ObservableValue<Boolean> apply(Context context, Tag tag) {
			if (tag.getParent().getMetaBinding() == null)
				return new ReadOnlyBooleanWrapper(tag.getParent().getClass().getAnnotation(SwitchChildDiv.Next.class) != null);
			else {
				Property<Map<Tag, SimpleIntegerProperty>> property = tag.getParent().<Map<Tag, SimpleIntegerProperty>> getProperty(Switcher.TAG_INDEX_MAP, context);
				Map<Tag, SimpleIntegerProperty> map = property.getValue();
				SimpleIntegerProperty indexProperty = map.get(tag.getParent());
				return Bindings.createBooleanBinding(() -> indexProperty.get() != 1 || tag.getParent().getClass().getAnnotation(SwitchChildDiv.Next.class) != null, indexProperty);
			}
		}
	}
}