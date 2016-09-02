package org.genericsystem.reactor.gs2;

import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gs.FlexDirection;
import org.genericsystem.reactor.gs.GSDiv;

public abstract class GSComposite extends GSDiv {

	public GSComposite(GSComposite parent, MetaTag metaTag) {
		super(parent, FlexDirection.COLUMN);
		for (MetaTag subMetaTag : metaTag.getSubNodes())
			createChild(subMetaTag);
	}

	protected <TAG extends GSComposite> void createChild(MetaTag subMetaTag) {
		try {
			subMetaTag.<TAG> getBuilder().getConstructor(GSComposite.class, MetaTag.class).newInstance(this, subMetaTag);
		} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
			throw new IllegalStateException(e);
		}
	}

	public Stream<Tag> tags(Class<? extends GSComposite>... classes) {
		return tags(Arrays.asList(classes));
	}

	private Stream<Tag> tags(List<Class<? extends GSComposite>> classes) {
		if (classes.isEmpty())
			return Stream.empty();
		return getObservableChildren().stream().filter(tag -> classes.get(0) == null || classes.get(0).equals(tag.getClass())).flatMap(tag -> ((GSComposite) tag).tags(classes.subList(1, classes.size())));
	}
}
