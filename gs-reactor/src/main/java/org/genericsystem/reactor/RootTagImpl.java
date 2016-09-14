package org.genericsystem.reactor;

import java.util.HashMap;
import java.util.LinkedHashMap;

import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Parent;
import org.genericsystem.reactor.annotations.ReactorDependencies;
import org.genericsystem.reactor.annotations.Select;
import org.genericsystem.reactor.gs.GSDiv;

public class RootTagImpl extends GSDiv implements Tag {

	private final HashMap<Class<? extends TagImpl>, TagImpl> nodes = new LinkedHashMap<Class<? extends TagImpl>, TagImpl>() {
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
		Class<? extends TagImpl> parentClass = getParentTagClass(getClass());
		if (parentClass == null)
			throw new IllegalStateException("Unable to find parent class of : " + getClass());
		setParent(find(parentClass));
		for (Class<? extends TagImpl> clazz : parentClass.getAnnotation(ReactorDependencies.class).value())
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

	public RootTagImpl(Tag parent, Class<? extends TagImpl> parentClass) {
		super(parent);
		for (Class<? extends TagImpl> clazz : parentClass.getAnnotation(ReactorDependencies.class).value())
			find(clazz);
		for (Tag tag : nodes.values())
			tag.postfix();
	}

	@Override
	public TagImpl find(Class<? extends TagImpl> tagClass) {
		TagImpl result = nodes.get(tagClass);
		if (result == null) {
			TagImpl newTag = null;
			try {
				newTag = tagClass.newInstance();
			} catch (IllegalAccessException | InstantiationException e) {
				throw new IllegalStateException(e);
			}

			Class<? extends TagImpl> parentClass = getParentTagClass(tagClass);
			System.out.println(tagClass + " " + parentClass + " on " + getClass());
			newTag.setParent(parentClass != null && !parentClass.isAssignableFrom(getClass()) ? find(parentClass) : this);
			ForEach forEach = tagClass.getAnnotation(ForEach.class);
			if (forEach != null) {
				try {
					newTag.forEach(forEach.value().newInstance().get());
				} catch (InstantiationException | IllegalAccessException e) {
					throw new IllegalStateException(e);
				}
			}
			Select select = tagClass.getAnnotation(Select.class);
			if (select != null) {
				try {
					newTag.select(select.value().newInstance().get());
				} catch (InstantiationException | IllegalAccessException e) {
					throw new IllegalStateException(e);
				}
			}
			newTag.init();
			newTag.style();
			nodes.put(tagClass, result = newTag);
		}
		return result;
	}

}