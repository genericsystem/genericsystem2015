package org.genericsystem.reactor.annotations;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.function.BiConsumer;

import org.genericsystem.api.core.AxedPropertyClass;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Children.ChildrenMult;
import org.genericsystem.reactor.annotations.Children.ChildrenProcessor;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTag;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GenericTagNode;
import org.genericsystem.reactor.gscomponents.TagImpl;

/**
 * @author Nicolas Feybesse
 *
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE })
@Repeatable(ChildrenMult.class)
@Process(ChildrenProcessor.class)
public @interface Children {
	Class<? extends TagImpl>[] path() default {};

	Class<? extends TagImpl>[] value();

	int[] pos() default {};

	@Retention(RetentionPolicy.RUNTIME)
	@Target({ ElementType.TYPE })
	public @interface ChildrenMult {
		Children[] value();
	}

	public static class ChildrenProcessor implements BiConsumer<Annotation, Tag> {

		@Override
		public void accept(Annotation annotation, Tag tag) {
			if (ExtendedRootTag.class.isAssignableFrom(tag.getRootTag().getClass())) {
				GTag delegate = ((GenericTagNode) ((TagImpl) tag).getTagNode()).getDelegateGeneric();
				Class<? extends TagImpl>[] classes = ((Children) annotation).value();
				for (int i = 0; i < classes.length; i++) {
					// System.out.println("Parent: " + delegate + ", adding child to GS: " + classes[i]);
					delegate.getMeta().setInstance(delegate, new AxedPropertyClass(classes[i], i));// Aie! , marchera pas, comment distinguer les signatures des generics ???
				}
			} else {
				for (Class<? extends TagImpl> clazz : ((Children) annotation).value()) {
					TagImpl result = null;
					try {
						result = clazz.newInstance();
					} catch (IllegalAccessException | InstantiationException e) {
						throw new IllegalStateException(e);
					}
					result.setParent(tag);
					result.setTagNode(tag.getRootTag().buildTagNode(result));
					tag.getObservableChildren().add(result);
					tag.getRootTag().getAnnotationsManager().processAnnotations(result);
					result.init();
				}
			}
		}
	}
}
