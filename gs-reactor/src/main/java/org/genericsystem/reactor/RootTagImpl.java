package org.genericsystem.reactor;

import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;

import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Parent;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.AlignItems;
import org.genericsystem.reactor.annotations.Style.BackgroundColor;
import org.genericsystem.reactor.annotations.Style.FlexDirection;
import org.genericsystem.reactor.annotations.Style.FlexGrow;
import org.genericsystem.reactor.annotations.Style.FlexWrap;
import org.genericsystem.reactor.annotations.Style.JustifyContent;
import org.genericsystem.reactor.gs.GSDiv;

public class RootTagImpl extends GSDiv implements Tag {

	private final HashMap<Class<? extends TagImpl>, TagImpl> nodes = new LinkedHashMap<Class<? extends TagImpl>, TagImpl>() {
		private static final long serialVersionUID = -6835018021862236920L;

		@Override
		public TagImpl get(Object key) {
			Class<? extends TagImpl> searchClass = (Class<? extends TagImpl>) key;
			TagImpl tag = super.get(searchClass);
			if (tag == null)
				for (Class<? extends TagImpl> clazz : keySet()) {
					if (searchClass.isAssignableFrom(clazz)) {
						tag = super.get(clazz);
						break;
					}
				}
			return tag;
		};
	};

	public RootTagImpl() {
		super();
		nodes.put(getClass(), this);
		ReactorDependencies deps = getClass().getAnnotation(ReactorDependencies.class);
		if (deps != null) {
			System.out.println("Declaring classes :   " + Arrays.toString(getClass().getDeclaredClasses()));
			System.out.println("ReactorDependencies : " + Arrays.toString(deps.value()));
			for (Class<? extends TagImpl> clazz : deps.value())
				find(clazz);
		}
		for (Tag tag : nodes.values())
			tag.postfix();
	}

	public RootTagImpl(Tag parent) {
		super(parent);
		init();
		style();
		nodes.put(getClass(), this);
		ReactorDependencies deps = getClass().getAnnotation(ReactorDependencies.class);
		if (deps != null)
			for (Class<? extends TagImpl> clazz : deps.value())
				find(clazz);
		for (Tag tag : nodes.values())
			tag.postfix();
	}

	public Class<? extends TagImpl> getParentTagClass(Class<? extends TagImpl> tagClass) {
		Parent parent = tagClass.getAnnotation(Parent.class);
		if (parent != null)
			return parent.value();
		Class<? extends TagImpl> enclosing = (Class<? extends TagImpl>) tagClass.getEnclosingClass();
		if (enclosing != null && !enclosing.isAssignableFrom(tagClass))
			return enclosing;
		return null;

	}

	@Override
	public TagImpl find(Class<? extends TagImpl> tagClass) {
		TagImpl result = nodes.get(tagClass);
		if (result == null) {
			try {
				result = tagClass.newInstance();
			} catch (IllegalAccessException | InstantiationException e) {
				throw new IllegalStateException();
			}
			Class<? extends TagImpl> parentClass = getParentTagClass(tagClass);
			System.out.println(tagClass + " " + parentClass + " on " + getClass());
			if (parentClass == null || parentClass.isAssignableFrom(getClass()))
				result.setParent(this);
			else
				result.setParent(find(parentClass));
			ForEach forEach = tagClass.getAnnotation(ForEach.class);
			if (forEach != null) {
				try {
					result.forEach(forEach.value().newInstance().get());
				} catch (InstantiationException | IllegalAccessException e) {
					throw new IllegalStateException(e);
				}
			}
			Select select = tagClass.getAnnotation(Select.class);
			if (select != null) {
				try {
					result.select(select.value().newInstance().get());
				} catch (InstantiationException | IllegalAccessException e) {
					throw new IllegalStateException(e);
				}
			}
			DirectSelect directSelect = tagClass.getAnnotation(DirectSelect.class);
			if (directSelect != null)
				result.select(directSelect.value());
			BackgroundColor backgroundColor = tagClass.getAnnotation(BackgroundColor.class);
			if (backgroundColor != null)
				result.addStyle("background-color", backgroundColor.value());
			FlexDirection flexDirection = tagClass.getAnnotation(FlexDirection.class);
			if (flexDirection != null)
				result.addStyle("flex-direction", flexDirection.value());
			FlexWrap flexWrap = tagClass.getAnnotation(FlexWrap.class);
			if (flexWrap != null)
				result.addStyle("flex-wrap", flexWrap.value());
			FlexGrow flexGrow = tagClass.getAnnotation(FlexGrow.class);
			if (flexGrow != null)
				result.addStyle("flex-grow", flexGrow.value());
			AlignItems alignItems = tagClass.getAnnotation(AlignItems.class);
			if (alignItems != null)
				result.addStyle("align-items", alignItems.value());
			JustifyContent justifyContent = tagClass.getAnnotation(JustifyContent.class);
			if (justifyContent != null)
				result.addStyle("justify-content", justifyContent.value());
			Style style = tagClass.getAnnotation(Style.class);
			if (style != null)
				result.addStyle(style.propertyName(), style.propertyValue());
			result.init();
			result.style();
			nodes.put(tagClass, result);
		}
		return result;
	}

}