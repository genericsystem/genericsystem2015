package org.genericsystem.reactor.gscomponents;

import java.lang.annotation.Annotation;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.api.core.Snapshot;
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
import org.genericsystem.reactor.ExtendedAnnotationsManager.IGenericAnnotationProcessor;
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.HtmlDomNode.RootHtmlDomNode;
import org.genericsystem.reactor.HtmlDomNode.Sender;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.TagNode;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.TagType.TagAnnotationAttribute;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.TagType.TagAnnotationContentAttribute;

import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.WeakListChangeListener;
import javafx.collections.transformation.SortedList;

public class ExtendedRootTag extends RootTagImpl {

	private final Root engine;

	// To avoid duplicate work.
	private final Map<Class<?>, GTag> storedClasses = new HashMap<>();

	private final ListChangeListener<? super GTagAnnotationContent> listener = c -> {
		while (c.next()) {
			if (c.wasRemoved())
				updateApplyingAnnotations(c.getRemoved().stream(), (annotations, value) -> annotations.remove(value));
			if (c.wasAdded())
				updateApplyingAnnotations(c.getAddedSubList().stream(), (annotations, value) -> annotations.add(value));
		}
	};

	private void updateApplyingAnnotations(Stream<? extends GTagAnnotationContent> streamToConsum, BiConsumer<ObservableList<GenericAnnotationWithContent>, GenericAnnotationWithContent> action) {
		streamToConsum.forEach(valueGeneric -> {
			GTagAnnotation gTagAnnotation = valueGeneric.getBaseComponent();
			AnnotationClassName key = new AnnotationClassName(gTagAnnotation.getValue().getAnnotationClass(), gTagAnnotation.getValue().getName());
			GenericAnnotationWithContent value = new GenericAnnotationWithContent(gTagAnnotation, valueGeneric);
			TagAnnotation tagAnnotation = gTagAnnotation.getValue();
			LinkedList<Class<?>> path = new LinkedList<>(Stream.of(tagAnnotation.getPath()).collect(Collectors.toList()));
			path.addFirst(gTagAnnotation.getBaseComponent().getValue());
			LinkedList<Integer> pos = Arrays.stream(tagAnnotation.getPos()).boxed().collect(Collectors.toCollection(LinkedList<Integer>::new));
			if (!pos.isEmpty())
				pos.addFirst(-1);
			Set<Tag> concernedTags = searchTags(this, path, pos);
			concernedTags.forEach(tag -> action.accept(((GenericTagNode) tag.getTagNode()).tagAnnotations.get(key), value));
		});
	}

	// pos is either empty or the same length as path.
	private Set<Tag> searchTags(Tag subTree, LinkedList<Class<?>> path, LinkedList<Integer> pos) {
		Set<Tag> foundTags = new HashSet<>();
		foundTags.addAll(searchTagsFromFirst(subTree, path, pos));
		subTree.getObservableChildren().forEach(child -> foundTags.addAll(searchTags(child, path, pos)));
		return foundTags;
	}

	private Set<Tag> searchTagsFromFirst(Tag firstTag, LinkedList<Class<?>> path, LinkedList<Integer> pos) {
		if (path.isEmpty())
			return new HashSet<>();
		Set<Tag> foundTags = new HashSet<>();
		if (path.peek().isAssignableFrom(firstTag.getClass()) && (pos.isEmpty() || pos.peek() == -1 || firstTag.getParent() != null && AnnotationsManager.position(firstTag, path.peek()) == pos.peek())) {
			if (path.size() == 1)
				foundTags.add(firstTag);
			else
				firstTag.getObservableChildren().forEach(child -> foundTags.addAll(searchTagsFromFirst(child, new LinkedList<>(path.subList(1, path.size())), pos.isEmpty() ? pos : new LinkedList<>(pos.subList(1, pos.size())))));
		}
		return foundTags;
	}

	public ExtendedRootTag(Root engine) {
		this.engine = engine;
		annotationsManager = new ExtendedAnnotationsManager(getClass());
		storedClasses.put(TagImpl.class, engine.find(GTag.class));
		storeClass(this.getClass());
		setTagNode(buildTagNode(this));
		processAnnotations(this);
		init();
	}

	@Override
	protected void initRoot() {
	}

	@Override
	public RootHtmlDomNode init(Context rootModelContext, String rootId, Sender send) {
		return new RootHtmlDomNode(rootModelContext, this, rootId, send) {
			private final ObservableList<GTagAnnotationContent> annotationContentInstances;
			{
				annotationContentInstances = (ObservableList) engine.find(TagAnnotationContentAttribute.class).getObservableSubInstances();
				annotationContentInstances.addListener(new WeakListChangeListener<>(listener));
			}
		};
	}

	@Override
	public void initDomNode(HtmlDomNode htmlDomNode) {
		((GenericTagNode) htmlDomNode.getTag().getTagNode()).getSortedAnnotationsLists().entrySet()
				.forEach(entry -> entry.getValue().addListener(getApplyingAnnotationsListener(htmlDomNode.getTag(), htmlDomNode.getModelContext(), entry.getKey().getAnnotationClass())));
	}

	@SuppressWarnings({ "unchecked" })
	public ListChangeListener<? super GenericAnnotationWithContent> getApplyingAnnotationsListener(Tag tag, Context context, Class<? extends Annotation> annotationClass) {
		return c -> {
			if (!context.isDestroyed()) {
				Map<Class<? extends Annotation>, IGenericAnnotationProcessor> processors = ((ExtendedAnnotationsManager) annotationsManager).getProcessors();
				if (processors.containsKey(annotationClass)) {
					while (c.next()) {
						if (c.wasAdded() && c.getFrom() == 0) {
							if (c.getList().size() > c.getAddedSize()) {
								GenericAnnotationWithContent formerApplyingAnnotation = c.getList().get(c.getAddedSize());
								processors.get(annotationClass).onRemove(tag, context, formerApplyingAnnotation.getgTagAnnotation(), formerApplyingAnnotation.getAnnotationContent());
							}
							processors.get(annotationClass).onAdd(tag, context, c.getAddedSubList().get(0).getgTagAnnotation(), c.getAddedSubList().get(0).getAnnotationContent());
						}
						if (c.wasRemoved() && c.getFrom() == 0) {
							processors.get(annotationClass).onRemove(tag, context, c.getRemoved().get(0).getgTagAnnotation(), c.getRemoved().get(0).getAnnotationContent());
							if (!c.getList().isEmpty()) {
								GenericAnnotationWithContent newApplyingAnnotation = c.getList().get(0);
								processors.get(annotationClass).onAdd(tag, context, newApplyingAnnotation.getgTagAnnotation(), newApplyingAnnotation.getAnnotationContent());
							}
						}
					}
				}
			}
		};
	}

	public ListChangeListener<? super GenericAnnotationWithContent> getApplyingAnnotationsListener(Tag tag, Class<? extends Annotation> annotationClass) {
		return c -> {
			Map<Class<? extends Annotation>, IGenericAnnotationProcessor> processors = ((ExtendedAnnotationsManager) annotationsManager).getProcessors();
			if (processors.containsKey(annotationClass)) {
				while (c.next()) {
					if (c.wasAdded() && c.getFrom() == 0) {
						if (c.getList().size() > c.getAddedSize()) {
							GenericAnnotationWithContent formerApplyingAnnotation = c.getList().get(c.getAddedSize());
							processors.get(annotationClass).onRemove(tag, formerApplyingAnnotation.getgTagAnnotation(), formerApplyingAnnotation.getAnnotationContent());
						}
						processors.get(annotationClass).onAdd(tag, c.getAddedSubList().get(0).getgTagAnnotation(), c.getAddedSubList().get(0).getAnnotationContent());
					}
					if (c.wasRemoved() && c.getFrom() == 0) {
						processors.get(annotationClass).onRemove(tag, c.getRemoved().get(0).getgTagAnnotation(), c.getRemoved().get(0).getAnnotationContent());
						if (!c.getList().isEmpty()) {
							GenericAnnotationWithContent newApplyingAnnotation = c.getList().get(0);
							processors.get(annotationClass).onAdd(tag, newApplyingAnnotation.getgTagAnnotation(), newApplyingAnnotation.getAnnotationContent());
						}
					}
				}
			}
		};
	}

	public Root getEngine() {
		return engine;
	}

	@Override
	public TagNode buildTagNode(Tag tag) {
		return new GenericTagNode(tag);
	}

	private GTag storeClass(Class<?> clazz) {
		if (!TagImpl.class.isAssignableFrom(clazz))
			return getEngine().find(GTag.class);

		GTag result = storedClasses.get(clazz);
		if (result != null)
			return result;

		Generic parentGeneric = storeClass(clazz.getSuperclass());
		result = (GTag) parentGeneric.getMeta().setInstance(parentGeneric, clazz);

		storedClasses.put(clazz, result);

		for (Children childrenAnnotation : clazz.getAnnotationsByType(Children.class)) {
			for (Class<?> childClass : childrenAnnotation.value())
				storeClass(childClass);
		}

		for (Class<? extends Annotation> annotationClass : ((ExtendedAnnotationsManager) annotationsManager).getProcessors().keySet())
			for (Annotation annotation : clazz.getAnnotationsByType(annotationClass))
				((ExtendedAnnotationsManager) annotationsManager).getProcessors().get(annotationClass).setAnnotation(result, annotation);

		getEngine().getCurrentCache().flush();
		return result;
	}

	public static class AnnotationClassName {
		private Class<? extends Annotation> annotationClass;
		private String name;

		public AnnotationClassName(Class<? extends Annotation> annotationClass, String name) {
			this.annotationClass = annotationClass;
			this.name = name;
		}

		public Class<? extends Annotation> getAnnotationClass() {
			return annotationClass;
		}

		public String getName() {
			return name;
		}

		@Override
		public boolean equals(Object obj) {
			if (!(obj instanceof AnnotationClassName))
				return false;
			AnnotationClassName other = (AnnotationClassName) obj;
			return annotationClass.equals(other.annotationClass) && Objects.equals(name, other.name);
		}

		@Override
		public int hashCode() {
			return annotationClass.hashCode();
		}
	}

	public class GenericAnnotationWithContent {
		private GTagAnnotation gTagAnnotation;
		private GTagAnnotationContent annotationContent;

		public GenericAnnotationWithContent(GTagAnnotation gTagAnnotation, GTagAnnotationContent annotationContent) {
			this.gTagAnnotation = gTagAnnotation;
			this.annotationContent = annotationContent;
		}

		public GTagAnnotation getgTagAnnotation() {
			return gTagAnnotation;
		}

		public GTagAnnotationContent getAnnotationContent() {
			return annotationContent;
		}

		@Override
		public boolean equals(Object obj) {
			if (!(obj instanceof GenericAnnotationWithContent))
				return false;
			GenericAnnotationWithContent other = (GenericAnnotationWithContent) obj;
			return gTagAnnotation.equals(other.gTagAnnotation) && Objects.equals(annotationContent, other.annotationContent);
		}

		@Override
		public int hashCode() {
			return gTagAnnotation.hashCode();
		}

		@Override
		public String toString() {
			return "GenericAnnotationValue [gTagAnnotation=" + gTagAnnotation + ", annotationContent=" + annotationContent + "]";
		}
	}

	// Stores all the (generic) annotations applying to a tag.
	public class GenericTagNode extends SimpleTagNode {

		private Map<AnnotationClassName, SortedList<GenericAnnotationWithContent>> sortedAnnotationsLists = new TreeMap<>((an1, an2) -> {
			List<Class<? extends Annotation>> processors = ((ExtendedAnnotationsManager) annotationsManager).getProcessors().keySet().stream().collect(Collectors.toList());
			Class<? extends Annotation> class1 = an1.getAnnotationClass();
			Class<? extends Annotation> class2 = an2.getAnnotationClass();
			if (an1.equals(an2))
				return 0;
			return class1.equals(class2) ? an1.getName().compareTo(an2.getName()) : Integer.compare(processors.indexOf(class1), processors.indexOf(class2));
		});
		private Map<AnnotationClassName, ObservableList<GenericAnnotationWithContent>> tagAnnotations = new HashMap<AnnotationClassName, ObservableList<GenericAnnotationWithContent>>() {

			private static final long serialVersionUID = -3404232263162064472L;

			@Override
			public ObservableList<GenericAnnotationWithContent> get(Object key) {
				ObservableList<GenericAnnotationWithContent> result = super.get(key);
				if (result == null && key instanceof AnnotationClassName) {
					result = FXCollections.observableArrayList();
					SortedList<GenericAnnotationWithContent> sortedList = new SortedList<>(result, new Comparator<GenericAnnotationWithContent>() {

						@Override
						public int compare(GenericAnnotationWithContent o1, GenericAnnotationWithContent o2) {
							Class<?>[] firstPath = o1.getgTagAnnotation().getValue().getPath();
							Class<?>[] secondPath = o2.getgTagAnnotation().getValue().getPath();
							return firstPath.length < secondPath.length || firstPath.length == secondPath.length && AnnotationsManager.isAssignableFrom(Arrays.asList(firstPath), Arrays.asList(secondPath)) ? 1 : -1;
						}
					});
					sortedAnnotationsLists.put((AnnotationClassName) key, sortedList);
					put((AnnotationClassName) key, result);
				}
				return result;
			}
		};

		public Map<AnnotationClassName, ObservableList<GenericAnnotationWithContent>> getTagAnnotations() {
			return tagAnnotations;
		}

		public Map<AnnotationClassName, SortedList<GenericAnnotationWithContent>> getSortedAnnotationsLists() {
			return sortedAnnotationsLists;
		}

		public GenericTagNode(Tag tag) {
			Deque<Class<?>> classesToResult = new ArrayDeque<>();

			Tag current = tag;
			while (current != null) {
				Set<GTagAnnotation> annotationsFound = selectAnnotations(current.getClass(), classesToResult, tag);
				annotationsFound.forEach(annotation -> tagAnnotations.get(new AnnotationClassName(annotation.getValue().getAnnotationClass(), annotation.getValue().getName())).add(new GenericAnnotationWithContent(annotation, annotation.getContent())));
				classesToResult.push(current.getClass());
				current = current.getParent();
			}
		}

		private Set<GTagAnnotation> selectAnnotations(Class<?> annotatedClass, Deque<Class<?>> classesToResult, Tag tag) {
			Set<GTagAnnotation> annotationsFound = new HashSet<>();
			for (GTagAnnotation annotation : storedClasses.get(annotatedClass).getAnnotations()) {
				Class<?>[] path = annotation.getValue().getPath();
				int[] pos = annotation.getValue().getPos();
				if (pos.length != 0 && pos.length != path.length)
					throw new IllegalStateException("The annotation " + annotation.getValue().getAnnotationClass().getSimpleName() + " contains a path and an array of class positions of different lengths. path: "
							+ Arrays.asList(path).stream().map(c -> c.getSimpleName()).collect(Collectors.toList()) + ", positions: " + Arrays.stream(pos).boxed().collect(Collectors.toList()) + " found on class " + annotatedClass.getSimpleName());
				if (AnnotationsManager.isAssignableFrom(Arrays.asList(path), new ArrayList<>(classesToResult)) && AnnotationsManager.posMatches(pos, path, tag))
					annotationsFound.add(annotation);
			}
			return annotationsFound;
		}
	}

	@SystemGeneric
	@InstanceClass(GTag.class)
	@Dependencies({ TagAnnotationAttribute.class })
	@InstanceValueClassConstraint(Class.class)
	public static interface TagType extends Generic {

		@SystemGeneric
		@Components(TagType.class)
		@InstanceValueClassConstraint(TagAnnotation.class)
		@Dependencies({ TagAnnotationContentAttribute.class })
		@InstanceClass(GTagAnnotation.class)
		public static interface TagAnnotationAttribute extends Generic {
		}

		@SystemGeneric
		@Components(TagAnnotationAttribute.class)
		@InstanceValueClassConstraint(String.class)
		@PropertyConstraint
		@NoInheritance
		@InstanceClass(GTagAnnotationContent.class)
		public static interface TagAnnotationContentAttribute extends Generic {
		}
	}

	@SystemGeneric
	@Meta(TagType.class)
	@ClassGenericValue(TagImpl.class)
	public static interface GTag extends Generic {
		@Override
		default public Class<?> getValue() {
			return (Class<?>) Generic.super.getValue();
		}

		@SuppressWarnings("unchecked")
		default Snapshot<GTagAnnotation> getAnnotations() {
			return (Snapshot) getHolders(getRoot().find(TagAnnotationAttribute.class));
		}

		default GTagAnnotation setAnnotation(Class<? extends Annotation> annotationClass, String name, String value, Class<?>[] path, int[] positions) {
			GTagAnnotation styleAnnotation = (GTagAnnotation) setHolder(getRoot().find(TagAnnotationAttribute.class), new TagAnnotation(annotationClass, path, positions, name));
			styleAnnotation.setHolder(getRoot().find(TagAnnotationContentAttribute.class), new JsonObject().put("value", value).encodePrettily());
			return styleAnnotation;
		}

		default void setArrayValueAnnotation(Class<? extends Annotation> annotationClass, String name, Object[] value, Class<?>[] path, int[] positions) {
			GTagAnnotation gTagAnnotation = (GTagAnnotation) setHolder(getRoot().find(TagAnnotationAttribute.class), new TagAnnotation(annotationClass, path, positions));
			gTagAnnotation.setHolder(getRoot().find(TagAnnotationContentAttribute.class), new JsonObject().put("value", new JsonArray(Arrays.asList(value))).encodePrettily());
		}
	}

	public static interface GTagAnnotation extends Generic {
		@Override
		default public GTag getBaseComponent() {
			return (GTag) Generic.super.getBaseComponent();
		}

		@Override
		default public TagAnnotation getValue() {
			return (TagAnnotation) Generic.super.getValue();
		}

		default GTagAnnotationContent getContent() {
			return (GTagAnnotationContent) getHolder(getRoot().find(TagAnnotationContentAttribute.class));
		}

	}

	public static interface GTagAnnotationContent extends Generic {
		@Override
		default public GTagAnnotation getBaseComponent() {
			return (GTagAnnotation) Generic.super.getBaseComponent();
		}

		@SuppressWarnings("unchecked")
		default public Stream<Class<?>> getClassesStream() {
			return (Stream) getContentJSonArray().stream().map(className -> {
				try {
					return Class.forName((String) className);
				} catch (ClassNotFoundException e) {
					throw new IllegalStateException("Class " + className + " not found");
				}
			});
		}

		@Override
		default public String getValue() {
			return (String) Generic.super.getValue();
		}

		default public JsonObject getJsonValue() {
			return new JsonObject(getValue());
		}

		default public JsonArray getContentJSonArray() {
			return getJsonValue().getJsonArray("value");
		}

		default public String getContentValue() {
			return getJsonValue().getString("value");
		}

		default public Class<?> getClassContent() {
			try {
				return Class.forName(getContentValue());
			} catch (ClassNotFoundException e) {
				throw new IllegalStateException("Class " + getContentValue() + " not found");
			}
		}

		default public Class<?>[] getClassArrayContent() {
			return getClassesStream().toArray(Class<?>[]::new);
		}

		default public String[] getStringArrayContent() {
			return getContentJSonArray().stream().toArray(String[]::new);
		}
	}
}
