package org.genericsystem.reactor.gs3;

import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.DirectSelect;
import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.ForEach.ChildForEach;
import org.genericsystem.reactor.annotations.ForEach.ParentForEach;
import org.genericsystem.reactor.annotations.Parent;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.AlignItems;
import org.genericsystem.reactor.annotations.Style.BackgroundColor;
import org.genericsystem.reactor.annotations.Style.ChildFlexDirection;
import org.genericsystem.reactor.annotations.Style.Color;
import org.genericsystem.reactor.annotations.Style.Flex;
import org.genericsystem.reactor.annotations.Style.FlexDirection;
import org.genericsystem.reactor.annotations.Style.FlexWrap;
import org.genericsystem.reactor.annotations.Style.JustifyContent;
import org.genericsystem.reactor.annotations.Style.Overflow;
import org.genericsystem.reactor.annotations.Style.ParentFlexDirection;
import org.genericsystem.reactor.gs.GSDiv;
import org.genericsystem.reactor.gs.GSTagImpl;

public class CompositeTagImpl extends GSDiv implements Tag {

	private final HashMap<Class<? extends GSTagImpl>, GSTagImpl> nodes = new LinkedHashMap<Class<? extends GSTagImpl>, GSTagImpl>() {
		private static final long serialVersionUID = -6835018021862236920L;

		@Override
		public GSTagImpl get(Object key) {
			Class<? extends GSTagImpl> searchClass = (Class<? extends GSTagImpl>) key;
			GSTagImpl tag = super.get(searchClass);
			if (tag == null)
				for (Class<? extends GSTagImpl> clazz : keySet()) {
					if (searchClass.isAssignableFrom(clazz)) {
						if (tag == null) {
							tag = super.get(clazz);
							System.out.println("search : " + searchClass.getSimpleName() + " find polymorphic class : " + CompositeTagImpl.this.getClass().getSimpleName());
						} else
							System.out.println("Warning : Found several results for class : " + searchClass.getSimpleName() + " on : " + CompositeTagImpl.this.getClass().getSimpleName() + " exact paths for them : " + searchClass + " " + getClass());
					}
				}
			return tag;
		};

	};

	public CompositeTagImpl() {
		super();
		nodes.put(getClass(), this);
		ReactorDependencies deps = getClass().getAnnotation(ReactorDependencies.class);
		if (deps != null) {
			System.out.println("Declaring classes :   " + Arrays.toString(getClass().getDeclaredClasses()));
			System.out.println("ReactorDependencies : " + Arrays.toString(deps.value()));
			for (Class<? extends GSTagImpl> clazz : deps.value())
				find(clazz);
		}
		for (Tag tag : nodes.values())
			tag.postfix();
	}

	public CompositeTagImpl(Tag parent) {
		super(parent);
		init();
		style();
		nodes.put(getClass(), this);
		ReactorDependencies deps = getClass().getAnnotation(ReactorDependencies.class);
		if (deps != null) {
			System.out.println("Declaring classes :   " + Arrays.toString(getClass().getDeclaredClasses()));
			System.out.println("ReactorDependencies : " + Arrays.toString(deps.value()));
			for (Class<? extends GSTagImpl> clazz : deps.value())
				find(clazz);
		}
		for (Tag tag : nodes.values())
			tag.postfix();
	}

	private static Class<? extends GSTagImpl> getParentTagClass(Class<? extends GSTagImpl> tagClass) {
		Parent parent = tagClass.getAnnotation(Parent.class);
		if (parent != null)
			return parent.value();
		Class<? extends GSTagImpl> enclosing = (Class<? extends GSTagImpl>) tagClass.getEnclosingClass();
		if (enclosing != null && !enclosing.isAssignableFrom(tagClass))
			return enclosing;
		return null;
	}

	@Override
	public GSTagImpl find(Class<? extends GSTagImpl> tagClass) {
		GSTagImpl result = nodes.get(tagClass);
		if (result == null) {
			try {
				result = tagClass.newInstance();
			} catch (IllegalAccessException | InstantiationException e) {
				throw new IllegalStateException(e);
			}
			Class<? extends GSTagImpl> parentClass = getParentTagClass(tagClass);
			result.setParent(parentClass != null ? find(parentClass) : this);
			processAnnotations(tagClass, result);
			result.init();
			result.style();
			nodes.put(tagClass, result);
		}
		return result;
	}

	private static void processAnnotations(Class<? extends GSTagImpl> tagClass, Tag result) {
		ParentForEach parentForEach = tagClass.getAnnotation(ParentForEach.class);
		if (parentForEach == null) {
			ForEach forEach = tagClass.getAnnotation(ForEach.class);
			if (forEach != null) {
				try {
					result.forEach(forEach.value().newInstance().get());
				} catch (InstantiationException | IllegalAccessException e) {
					throw new IllegalStateException(e);
				}
			}
		} else {

			ChildForEach childForEach = result.getParent().getClass().getAnnotation(ChildForEach.class);
			if (childForEach != null) {
				try {
					result.forEach(childForEach.value()[parentForEach.pos()].newInstance().get());
				} catch (InstantiationException | IllegalAccessException e) {
					throw new IllegalStateException(e);
				}
			} else
				System.out.println("Warning : unable to find childForEach on : " + result.getParent().getClass().getSimpleName() + " for : " + tagClass.getSimpleName());
		}

		Select select = tagClass.getAnnotation(Select.class);
		if (select != null) {
			try {
				result.select(select.value().newInstance().get());
			} catch (InstantiationException | IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
		}
		ParentFlexDirection parentFlexDirection = tagClass.getAnnotation(ParentFlexDirection.class);
		if (parentFlexDirection == null) {
			FlexDirection flexDirection = tagClass.getAnnotation(FlexDirection.class);
			if (flexDirection != null)
				result.addStyle("flex-direction", flexDirection.value());
		} else {
			ChildFlexDirection childFlexDirection = result.getParent().getClass().getAnnotation(ChildFlexDirection.class);
			if (childFlexDirection != null)
				result.addStyle("flex-direction", childFlexDirection.value()[parentFlexDirection.pos()]);
		}

		DirectSelect directSelect = tagClass.getAnnotation(DirectSelect.class);
		if (directSelect != null)
			result.select(directSelect.value());
		BackgroundColor backgroundColor = tagClass.getAnnotation(BackgroundColor.class);
		if (backgroundColor != null)
			result.addStyle("background-color", backgroundColor.value());
		FlexWrap flexWrap = tagClass.getAnnotation(FlexWrap.class);
		if (flexWrap != null)
			result.addStyle("flex-wrap", flexWrap.value());
		Flex flex = tagClass.getAnnotation(Flex.class);
		if (flex != null)
			result.addStyle("flex", flex.value());
		AlignItems alignItems = tagClass.getAnnotation(AlignItems.class);
		if (alignItems != null)
			result.addStyle("align-items", alignItems.value());
		JustifyContent justifyContent = tagClass.getAnnotation(JustifyContent.class);
		if (justifyContent != null)
			result.addStyle("justify-content", justifyContent.value());
		Overflow overlflow = tagClass.getAnnotation(Overflow.class);
		if (overlflow != null)
			result.addStyle("overflow", overlflow.value());
		Color color = tagClass.getAnnotation(Color.class);
		if (color != null)
			result.addStyle("color", color.value());

		Style style = tagClass.getAnnotation(Style.class);
		if (style != null)
			result.addStyle(style.propertyName(), style.propertyValue());
	}
}