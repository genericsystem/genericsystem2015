package org.genericsystem.reactor.gscomponents;

import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;

import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javafx.beans.binding.MapBinding;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;
import javafx.collections.WeakListChangeListener;

import org.genericsystem.api.core.TagAnnotation;
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
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.ExtendedAnnotationsManager;
import org.genericsystem.reactor.HtmlDomNode.RootHtmlDomNode;
import org.genericsystem.reactor.HtmlDomNode.Sender;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.TagNode;
import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagType.AnnotationContent;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.GTagType.TagAnnotationGeneric;

public class ExtendedRootTag extends RootTagImpl {

	private final Root engine;

	private final Generic tagAnnotationType;
	private final Generic annotationContent;

	// To avoid duplicate work.
	private final Map<Class<?>, GTag> storedClasses = new HashMap<>();

	private final ListChangeListener<? super Generic> listener = c -> {
		while (c.next()) {
			if (c.wasRemoved()) {
				for (Generic valueGeneric : c.getRemoved()) {
					Generic tagAnnotationGeneric = valueGeneric.getBaseComponent();
					TagAnnotation tagAnnotation = (TagAnnotation) tagAnnotationGeneric.getValue();
					Class<?>[] path = tagAnnotation.getPath();
					Class<?> targetTagClass = path.length == 0 ? (Class<?>) tagAnnotationGeneric.getBaseComponent().getValue() : path[path.length - 1];
					if (Style.class.equals(tagAnnotation.getAnnotationClass())) {
						Set<Tag> concernedTags = searchTags(this, targetTagClass, tagAnnotation.getPath(), tagAnnotation.getPos());
						concernedTags.forEach(tag -> ((GenericTagNode) tag.getTagNode()).applyingStyles.remove(tagAnnotationGeneric));
					}
				}
			}
			if (c.wasAdded()) {
				for (Generic valueGeneric : c.getAddedSubList()) {
					Generic tagAnnotationGeneric = valueGeneric.getBaseComponent();
					TagAnnotation tagAnnotation = (TagAnnotation) tagAnnotationGeneric.getValue();
					Class<?>[] path = tagAnnotation.getPath();
					Class<?> targetTagClass = path.length == 0 ? (Class<?>) tagAnnotationGeneric.getBaseComponent().getValue() : path[path.length - 1];
					if (Style.class.equals(tagAnnotation.getAnnotationClass())) {
						Set<Tag> concernedTags = searchTags(this, targetTagClass, tagAnnotation.getPath(), tagAnnotation.getPos());
						concernedTags.forEach(tag -> updateStyle(tag, tagAnnotationGeneric));
					}
				}
			}
		}
	};

	private Set<Tag> searchTags(Tag subTree, Class<?> targetTagClass, Class<?>[] path, int[] pos) {
		Set<Tag> foundTags = new HashSet<>();
		if (targetTagClass.equals(subTree.getClass()) && AnnotationsManager.posMatches(pos, path, subTree))
			foundTags.add(subTree);
		subTree.getObservableChildren().forEach(child -> foundTags.addAll(searchTags(child, targetTagClass, path, pos)));
		return foundTags;
	}

	private void updateStyle(Tag tag, Generic tagAnnotationGeneric) {
		List<Generic> applyingStyles = ((GenericTagNode) tag.getTagNode()).applyingStyles;
		Generic applyingAnnotation = tagAnnotationGeneric;
		TagAnnotation newAnnotation = (TagAnnotation) tagAnnotationGeneric.getValue();
		for (Generic styleAnnotationGeneric : applyingStyles) {
			TagAnnotation annotation = (TagAnnotation) styleAnnotationGeneric.getValue();
			if (annotation.getName().equals(newAnnotation.getName()) && annotation.getPath().length > newAnnotation.getPath().length)
				applyingAnnotation = styleAnnotationGeneric;
		}
		if (tagAnnotationGeneric.equals(applyingAnnotation))
			applyingStyles.add(tagAnnotationGeneric);
	}

	public ExtendedRootTag(Root engine) {
		this.engine = engine;
		storedClasses.put(TagImpl.class, engine.find(GTag.class));
		tagAnnotationType = engine.find(TagAnnotationGeneric.class);
		annotationContent = engine.find(AnnotationContent.class);

		storeClass(this.getClass());
		annotationsManager = new ExtendedAnnotationsManager(this.getClass());
		setTagNode(buildTagNode(this));
		createSubTree();
	}

	@Override
	protected void initRoot() {
	}

	@Override
	public RootHtmlDomNode init(Context rootModelContext, String rootId, Sender send) {
		return new RootHtmlDomNode(rootModelContext, this, rootId, send) {
			private final ObservableList<Generic> annotationContentInstances;
			{
				annotationContentInstances = annotationContent.getObservableSubInstances();
				annotationContentInstances.addListener(new WeakListChangeListener<>(listener));
			}
		};
	}

	public Root getEngine() {
		return engine;
	}

	@Override
	public TagNode buildTagNode(Tag tag) {
		return new GenericTagNode(tag).init();
	}

	static long start;
	static long time;
	static long total;

	private GTag storeClass(Class<?> clazz) {
		if (!TagImpl.class.isAssignableFrom(clazz))
			return getEngine().find(GTag.class);

		if (storedClasses.containsKey(clazz))
			return storedClasses.get(clazz);

		Generic parentGeneric = storeClass(clazz.getSuperclass());
		GTag classGeneric = (GTag) parentGeneric.getMeta().setInstance(parentGeneric, clazz);

		storedClasses.put(clazz, classGeneric);

		Children[] annotations = clazz.getAnnotationsByType(Children.class);
		for (Children childrenAnnotation : annotations) {
			storeChildrenAnnotation(classGeneric, childrenAnnotation);
			for (Class<?> childClass : childrenAnnotation.value())
				storeClass(childClass);
		}

		Style[] styleAnnotations = clazz.getAnnotationsByType(Style.class);
		for (Style styleAnnotation : styleAnnotations)
			storeStyleAnnotation(classGeneric, styleAnnotation);

		getEngine().getCurrentCache().flush();
		return classGeneric;
	}

	private void storeChildrenAnnotation(Generic tagClassGeneric, Children annotation) {
		Generic annotationGeneric = tagClassGeneric.setHolder(tagAnnotationType, new TagAnnotation(Children.class, annotation.path(), annotation.pos()));
		JsonObject json = new JsonObject();
		json.put("value", new JsonArray(Arrays.asList(annotation.value())));
		annotationGeneric.setHolder(annotationContent, json.encodePrettily());
	}

	private void storeStyleAnnotation(Generic tagClassGeneric, Style annotation) {
		Generic styleAnnotationGeneric = tagClassGeneric.setHolder(tagAnnotationType, new TagAnnotation(Style.class, annotation.path(), annotation.pos(), annotation.name()));
		JsonObject json = new JsonObject();
		json.put("value", annotation.value());
		styleAnnotationGeneric.setHolder(annotationContent, json.encodePrettily());
	}

	// Called only by addStyle, not used to treat @Style annotations.
	// Use the most precise path and pos possible to make sure that this style applies to the given tag and nothing else.
	// Problems:
	// – All the annotations are stored on the root tag.
	// – This method is slow.
	@Override
	public void processStyle(Tag tag, String name, String value) {
		Generic tagClassGeneric = storedClasses.get(tag.getRootTag().getClass());
		List<Class<?>> path = new ArrayList<>();
		List<Integer> pos = new ArrayList<>();
		Tag current = tag;
		while (current != null) {
			path.add(0, current.getClass());
			pos.add(0, current.getParent() != null ? AnnotationsManager.position(current, current.getClass()) : -1);
			current = current.getParent();
		}
		Generic styleAnnotationGeneric = tagClassGeneric.setHolder(tagAnnotationType, new TagAnnotation(Style.class, path.stream().toArray(Class<?>[]::new), pos.stream().mapToInt(i -> i).toArray(), name));
		JsonObject json = new JsonObject();
		json.put("value", value);
		styleAnnotationGeneric.setHolder(annotationContent, json.encode());
	}

	public class GenericTagNode implements TagNode {
		private ObservableList<Tag> children = FXCollections.observableArrayList();
		private ObservableList<Generic> applyingStyles = FXCollections.observableArrayList();
		private Tag tag;

		public GenericTagNode(Tag tag) {
			this.tag = tag;
		}

		public List<Generic> getApplyingStyles() {
			return applyingStyles;
		}

		public GenericTagNode init() {
			List<Class<?>> classesToResult = new ArrayList<>();

			// @Children annotations
			Tag current = tag;
			Generic applyingAnnotation = null;
			while (current != null) {
				List<Generic> annotationFound = selectAnnotations(current.getClass(), Children.class, classesToResult, tag);
				Class<?> superClass = current.getClass().getSuperclass();
				while (annotationFound.isEmpty() && Tag.class.isAssignableFrom(superClass)) {
					annotationFound = selectAnnotations(superClass, Children.class, classesToResult, tag);
					superClass = superClass.getSuperclass();
				}
				if (!annotationFound.isEmpty())
					applyingAnnotation = annotationFound.get(0);
				classesToResult.add(0, current.getClass());
				current = current.getParent();
			}
			if (applyingAnnotation != null) {
				Class<? extends TagImpl>[] childrenClasses;
				JsonArray json = new JsonObject((String) applyingAnnotation.getHolder(annotationContent).getValue()).getJsonArray("value");
				List<Class<?>> childClasses = ((Stream<String>) json.getList().stream()).map(className -> {
					try {
						return Class.forName(className);
					} catch (ClassNotFoundException e) {
						throw new IllegalStateException("Class " + className + " not found in children of " + tag.getClass().getSimpleName());
					}
				}).collect(Collectors.toList());
				for (Class<?> childClass : childClasses)
					children.add(createChild(tag, (Class<? extends TagImpl>) childClass));
			}

			// @Style annotations
			classesToResult = new ArrayList<>();
			current = tag;
			while (current != null) {
				Class<?> superClass = current.getClass();
				List<Generic> annotationsFound = new ArrayList<>();
				while (Tag.class.isAssignableFrom(superClass)) {
					annotationsFound.addAll(selectAnnotations(superClass, Style.class, classesToResult, tag));
					superClass = superClass.getSuperclass();
				}
				Collections.reverse(annotationsFound);
				applyingStyles.addAll(annotationsFound);
				classesToResult.add(0, current.getClass());
				current = current.getParent();
			}

			return this;
		}

		private List<Generic> selectAnnotations(Class<?> annotatedClass, Class<? extends Annotation> annotationClass, List<Class<?>> classesToResult, Tag tag) {
			List<Generic> annotationFound = new ArrayList<>();
			Generic tagClass = storedClasses.get(annotatedClass);
			List<Generic> annotations = tagClass.getObservableComposites().filtered(g -> tagAnnotationType.equals(g.getMeta()) && annotationClass.equals(((TagAnnotation) g.getValue()).getAnnotationClass()));
			for (Generic annotation : annotations) {
				Class<?>[] path = ((TagAnnotation) annotation.getValue()).getPath();
				int[] pos = ((TagAnnotation) annotation.getValue()).getPos();
				if (pos.length != 0 && pos.length != path.length)
					throw new IllegalStateException("The annotation " + annotationClass.getSimpleName() + " contains a path and an array of class positions of different lengths. path: "
							+ Arrays.asList(path).stream().map(c -> c.getSimpleName()).collect(Collectors.toList()) + ", positions: " + Arrays.stream(pos).boxed().collect(Collectors.toList()) + " found on class " + annotatedClass.getSimpleName());
				if (AnnotationsManager.isAssignableFrom(Arrays.asList(path), classesToResult) && AnnotationsManager.posMatches(pos, path, tag)) {
					if (!annotationFound.isEmpty() && !(Style.class.equals(annotationClass) || Attribute.class.equals(annotationClass)))
						throw new IllegalStateException("Multiple annotations applicable to same tag defined at same level. Annotation: " + annotationClass.getSimpleName() + ", path to tag: "
								+ Arrays.asList(path).stream().map(c -> c.getSimpleName()).collect(Collectors.toList()));
					annotationFound.add(annotation);
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
			return new MapBinding<String, String>() {
				{
					bind(applyingStyles);
				}

				@Override
				protected ObservableMap<String, String> computeValue() {
					ObservableMap<String, String> styles = FXCollections.observableHashMap();
					for (Generic applyingStyle : applyingStyles) {
						JsonObject json = new JsonObject((String) applyingStyle.getComposites().filter(g -> annotationContent.equals(g.getMeta())).first().getValue());
						styles.put(((TagAnnotation) applyingStyle.getValue()).getName(), json.getString("value"));
					}
					return styles;
				}
			};
		}
	}

	@SystemGeneric
	@InstanceClass(GTag.class)
	@Dependencies({ TagAnnotationGeneric.class })
	@InstanceValueClassConstraint(Class.class)
	public static interface GTagType extends Generic {

		@SystemGeneric
		@Components(GTagType.class)
		@InstanceValueClassConstraint(TagAnnotation.class)
		@Dependencies({ AnnotationContent.class })
		@NoInheritance
		public static interface TagAnnotationGeneric extends Generic {

		}

		@SystemGeneric
		@Components(TagAnnotationGeneric.class)
		@InstanceValueClassConstraint(String.class)
		@PropertyConstraint
		@NoInheritance
		public static interface AnnotationContent extends Generic {

		}
	}

	@SystemGeneric
	@Meta(GTagType.class)
	@ClassGenericValue(TagImpl.class)
	public static interface GTag extends Generic {

	}
}
