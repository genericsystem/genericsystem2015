package org.genericsystem.reactor.gscomponents;

import java.io.Serializable;
import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.genericsystem.api.core.AxedPropertyClass;
import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueClassConstraint;
import org.genericsystem.api.core.annotations.constraints.NoInheritance;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.api.core.annotations.value.ClassGenericValue;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.reactor.AnnotationsManager;
import org.genericsystem.reactor.ExtendedAnnotationsManager;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.TagNode;
import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.CustomAnnotations;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagType.AnnotationParameter;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagType.AnnotationParameterValue;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagType.TagAnnotation;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;

public class ExtendedRootTag extends RootTagImpl {

	private final Root engine;

	private final Generic tagAnnotationType;
	private final Generic annotationParameter;
	private final Generic parameterValue;

	// To avoid duplicate work.
	private final Map<Class<?>, GTag> storedClasses = new HashMap<>();

	public ExtendedRootTag(Root engine) {
		this.engine = engine;
		storedClasses.put(TagImpl.class, engine.find(GTag.class));
		tagAnnotationType = engine.find(TagAnnotation.class);
		annotationParameter = engine.find(AnnotationParameter.class);
		parameterValue = engine.find(AnnotationParameterValue.class);
		storeClass(this.getClass());
		annotationsManager = new ExtendedAnnotationsManager();
		Annotation annotations = getClass().getAnnotation(CustomAnnotations.class);
		if (annotations != null)
			for (Class<? extends Annotation> annotation : ((CustomAnnotations) annotations).value())
				annotationsManager.registerAnnotation(annotation);
		setTagNode(buildTagNode(this));
		createSubTree();
	}

	@Override
	protected void initRoot() {
	}

	public Root getEngine() {
		return engine;
	}

	@Override
	public TagNode buildTagNode(Tag tag) {
		return new GenericTagNode(tag).init();
	}

	private GTag storeClass(Class<?> clazz) {
		if (!TagImpl.class.isAssignableFrom(clazz))
			return getEngine().find(GTag.class);

		if (storedClasses.containsKey(clazz))
			return storedClasses.get(clazz);

		Generic parentGeneric = storeClass(clazz.getSuperclass());
		GTag classGeneric = (GTag) parentGeneric.getMeta().setInstance(parentGeneric, clazz);

		Children[] annotations = clazz.getAnnotationsByType(Children.class);
		for (int i = 0; i < annotations.length; i++) {
			Children childrenAnnotation = annotations[i];
			Generic annotationGeneric = classGeneric.setHolder(tagAnnotationType, new AxedPropertyClass(Children.class, i));
			Class<?>[] childClasses = childrenAnnotation.value();
			storeAnnotationParameter(annotationGeneric, "path", childrenAnnotation.path());
			storeAnnotationParameter(annotationGeneric, "value", childClasses);
			storeAnnotationParameter(annotationGeneric, "pos", childrenAnnotation.pos());
			for (Class<?> childClass : childClasses)
				storeClass(childClass);
		}

		Style[] styleAnnotations = clazz.getAnnotationsByType(Style.class);
		for (int i = 0; i < styleAnnotations.length; i++) {
			Style styleAnnotation = styleAnnotations[i];
			Generic styleAnnotationGeneric = classGeneric.setHolder(tagAnnotationType, new AxedPropertyClass(Style.class, i));
			storeAnnotationParameter(styleAnnotationGeneric, "path", styleAnnotation.path());
			storeAnnotationParameter(styleAnnotationGeneric, "value", styleAnnotation.value());
			storeAnnotationParameter(styleAnnotationGeneric, "pos", styleAnnotation.pos());
			storeAnnotationParameter(styleAnnotationGeneric, "name", styleAnnotation.name());
		}
		storedClasses.put(clazz, classGeneric);
		getEngine().getCurrentCache().flush();
		return classGeneric;
	}

	private void storeAnnotationParameter(Generic annotationGeneric, String parameterName, Serializable value) {
		Generic paramName = annotationGeneric.getComposites(parameterName).filter(g -> annotationParameter.equals(g.getMeta())).first();
		if (paramName == null)
			paramName = annotationGeneric.addHolder(annotationParameter, parameterName);
		Generic paramValue = paramName.getComposites().filter(g -> parameterValue.equals(g.getMeta())).first();
		if (paramValue != null) {
			if (!Objects.deepEquals(value, paramValue.getValue()))
				paramName.setHolder(parameterValue, value);
		} else
			paramName.addHolder(parameterValue, value);
	}

	public class GenericTagNode implements TagNode {
		private ObservableList<Tag> children = FXCollections.observableArrayList();
		private Tag tag;

		public GenericTagNode(Tag tag) {
			this.tag = tag;
		}

		public GenericTagNode init() {
			List<Class<?>> classesToResult = new ArrayList<>();
			Tag current = tag;
			Generic applyingAnnotation = null;
			while (current != null) {
				Generic annotationFound = selectAnnotation(current.getClass(), Children.class, classesToResult, tag);
				Class<?> superClass = current.getClass().getSuperclass();
				while (annotationFound == null && Tag.class.isAssignableFrom(superClass)) {
					annotationFound = selectAnnotation(superClass, Children.class, classesToResult, tag);
					superClass = superClass.getSuperclass();
				}
				if (annotationFound != null)
					applyingAnnotation = annotationFound;
				classesToResult.add(0, current.getClass());
				current = current.getParent();
			}
			if (applyingAnnotation != null) {
				Class<? extends TagImpl>[] childrenClasses = (Class<? extends TagImpl>[]) applyingAnnotation.getHolder(annotationParameter, "value").getHolder(parameterValue).getValue();
				for (Class<? extends TagImpl> childClass : childrenClasses)
					children.add(createChild(tag, childClass));
			}
			return this;
		}

		private Generic selectAnnotation(Class<?> annotatedClass, Class<? extends Annotation> annotationClass, List<Class<?>> classesToResult, Tag tag) {
			Generic annotationFound = null;
			Generic tagClass = storedClasses.get(annotatedClass);
			List<Generic> annotations = tagClass.getObservableHolders(tagAnnotationType).filtered(g -> annotationClass.equals(((AxedPropertyClass) g.getValue()).getClazz()));
			for (Generic annotation : annotations) {
				Class<?>[] path = (Class<?>[]) annotation.getHolder(annotationParameter, "path").getHolder(parameterValue).getValue();
				int[] pos = (int[]) annotation.getHolder(annotationParameter, "pos").getHolder(parameterValue).getValue();
				if (pos.length != 0 && pos.length != path.length)
					throw new IllegalStateException("The annotation " + annotationClass.getSimpleName() + " contains a path and an array of class positions of different lengths. path: "
							+ Arrays.asList(path).stream().map(c -> c.getSimpleName()).collect(Collectors.toList()) + ", positions: " + Arrays.stream(pos).boxed().collect(Collectors.toList()) + " found on class " + annotatedClass.getSimpleName());
				if (AnnotationsManager.isAssignableFrom(Arrays.asList(path), classesToResult) && AnnotationsManager.posMatches(pos, path, tag)) {
					if (annotationFound != null && !(Style.class.equals(annotationClass) || Attribute.class.equals(annotationClass)))
						throw new IllegalStateException("Multiple annotations applicable to same tag defined at same level. Annotation: " + annotationClass.getSimpleName() + ", path to tag: "
								+ Arrays.asList(path).stream().map(c -> c.getSimpleName()).collect(Collectors.toList()));
					annotationFound = annotation;
				}
			}
			return annotationFound;
		}

		@Override
		public ObservableList<Tag> getObservableChildren() {
			return children;
		}

		@Override
		public ObservableMap<String, String> getObservableStyles() {
			return FXCollections.observableHashMap();
		}
	}

	@SystemGeneric
	@InstanceClass(GTag.class)
	@Dependencies({ TagAnnotation.class })
	@InstanceValueClassConstraint(Class.class)
	public static interface GTagType extends Generic {

		@SystemGeneric
		@Components(GTagType.class)
		@InstanceValueClassConstraint(AxedPropertyClass.class)
		@Dependencies({ AnnotationParameter.class, AnnotationParameterValue.class })
		@NoInheritance
		public static interface TagAnnotation extends Generic {

		}

		@SystemGeneric
		@Components(TagAnnotation.class)
		@InstanceValueClassConstraint(String.class)
		public static interface AnnotationParameter extends Generic {

		}

		@SystemGeneric
		@Components(AnnotationParameter.class)
		@PropertyConstraint
		public static interface AnnotationParameterValue extends Generic {

		}
	}

	@SystemGeneric
	@Meta(GTagType.class)
	@ClassGenericValue(TagImpl.class)
	public static interface GTag extends Generic {

	}
}
