package org.genericsystem.reactor.gscomponents;

import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
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
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.TagNode;
import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.Children;
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
	private final Set<Class<?>> storedClasses = new HashSet<>();

	public ExtendedRootTag(Root engine) {
		this.engine = engine;
		storedClasses.add(AbstractTag.class);
		tagAnnotationType = engine.find(TagAnnotation.class);
		annotationParameter = engine.find(AnnotationParameter.class);
		parameterValue = engine.find(AnnotationParameterValue.class);
		super.initRoot();
	}

	@Override
	protected void initRoot() {
	}

	public Root getEngine() {
		return engine;
	}

	@Override
	public TagNode buildTagNode(Tag tag) {
		storeClass(tag.getClass());
		return new GenericTagNode(tag).init();
	}

	private GTag storeClass(Class<?> clazz) {
		System.out.println("storeClass " + clazz);
		if (!Tag.class.isAssignableFrom(clazz))
			return getEngine().find(GTag.class);

		if (storedClasses.contains(clazz))
			return (GTag) getEngine().find(GTagType.class).getInstance(clazz);

		Generic parentGeneric = storeClass(clazz.getSuperclass());
		GTag classGeneric = (GTag) parentGeneric.getMeta().setInstance(parentGeneric, clazz);

		Children[] annotations = clazz.getAnnotationsByType(Children.class);
		for (int i = 0; i < annotations.length; i++) {
			Children childrenAnnotation = annotations[i];
			Generic annotationGeneric = classGeneric.setHolder(tagAnnotationType, new AxedPropertyClass(Children.class, i));
			Generic pathParam = annotationGeneric.setHolder(annotationParameter, "path");
			pathParam.setHolder(parameterValue, childrenAnnotation.path());
			Generic valueParam = annotationGeneric.setHolder(annotationParameter, "value");
			valueParam.setHolder(parameterValue, childrenAnnotation.value());
			Generic posParam = annotationGeneric.setHolder(annotationParameter, "pos");
			posParam.setHolder(parameterValue, childrenAnnotation.pos());
		}

		Style[] styleAnnotations = clazz.getAnnotationsByType(Style.class);
		for (int i = 0; i < styleAnnotations.length; i++) {
			Style styleAnnotation = styleAnnotations[i];
			Generic styleAnnotationGeneric = classGeneric.setHolder(tagAnnotationType, new AxedPropertyClass(Style.class, i));
			Generic pathParam = styleAnnotationGeneric.setHolder(annotationParameter, "path");
			pathParam.setHolder(parameterValue, styleAnnotation.path());
			Generic valueParam = styleAnnotationGeneric.setHolder(annotationParameter, "value");
			valueParam.setHolder(parameterValue, styleAnnotation.value());
			Generic posParam = styleAnnotationGeneric.setHolder(annotationParameter, "pos");
			posParam.setHolder(parameterValue, styleAnnotation.pos());
			Generic nameParam = styleAnnotationGeneric.setHolder(annotationParameter, "name");
			nameParam.setHolder(parameterValue, styleAnnotation.name());
		}
		storedClasses.add(clazz);
		getEngine().getCurrentCache().flush();
		return classGeneric;
	}

	@Override
	public void processChildren(Tag tag, Class<? extends TagImpl>[] classes) {
		for (Class<?> clazz : classes)
			storeClass(clazz);
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
				while (annotationFound == null && superClass != null) {
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
				for (Class<? extends TagImpl> childClass : childrenClasses) {
					AbstractTag result = createChild(tag, childClass);
					GenericTagNode tagNode = new GenericTagNode(result);
					result.setTagNode(tagNode);
					tagNode.init();
					processAnnotations(result);
					result.init();
					children.add(result);
				}
			}
			return this;
		}

		private Generic selectAnnotation(Class<?> annotatedClass, Class<? extends Annotation> annotationClass, List<Class<?>> classesToResult, Tag tag) {
			Generic annotationFound = null;
			Generic tagClass = storeClass(annotatedClass);
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
	@ClassGenericValue(AbstractTag.class)
	public static interface GTag extends Generic {

	}
}
