package org.genericsystem.reactor;

import java.util.HashMap;
import java.util.LinkedHashMap;

import org.genericsystem.reactor.annotations.ForEach;
import org.genericsystem.reactor.annotations.Parent;
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

	public RootTagImpl(Tag parent, Class<? extends TagImpl>... specifiedClasses) {
		super(parent);
		for (Class<? extends TagImpl> clazz : specifiedClasses)
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

			Parent parent = tagClass.getAnnotation(Parent.class);

			Class<? extends TagImpl> parentClass = null;
			if (parent != null)
				parentClass = parent.value();
			else {
				Class<? extends TagImpl> enclosing = (Class<? extends TagImpl>) tagClass.getEnclosingClass();
				if (enclosing != null && !enclosing.isAssignableFrom(tagClass)) {
					parentClass = enclosing;
					// System.out.println(tagClass + " " + enclosing);
				}
			}
			System.out.println(parentClass + " " + getClass());
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