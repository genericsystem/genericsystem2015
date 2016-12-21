package org.genericsystem.reactor.gscomponents;

import java.lang.annotation.Annotation;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
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
import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.HtmlDomNode.RootHtmlDomNode;
import org.genericsystem.reactor.HtmlDomNode.Sender;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.TagNode;
import org.genericsystem.reactor.annotations.Attribute;
import org.genericsystem.reactor.annotations.Children;
import org.genericsystem.reactor.annotations.Style;
import org.genericsystem.reactor.annotations.Style.FlexDirectionStyle;
import org.genericsystem.reactor.annotations.Style.GenericValueBackgroundColor;
import org.genericsystem.reactor.annotations.Style.KeepFlexDirection;
import org.genericsystem.reactor.annotations.Style.ReverseFlexDirection;
import org.genericsystem.reactor.annotations.StyleClass;
import org.genericsystem.reactor.context.StringExtractor;
import org.genericsystem.reactor.contextproperties.FlexDirectionDefaults;
import org.genericsystem.reactor.contextproperties.GenericStringDefaults;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.TagType.TagAnnotationAttribute;
import org.genericsystem.reactor.gscomponents.ExtendedRootTag.TagType.TagAnnotationContentAttribute;

import com.sun.javafx.collections.ObservableMapWrapper;

import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import javafx.collections.ListChangeListener;
import javafx.collections.MapChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;
import javafx.collections.WeakListChangeListener;

public class ExtendedRootTag extends RootTagImpl {

	private final Root engine;

	// To avoid duplicate work.
	private final Map<Class<?>, GTag> storedClasses = new HashMap<>();

	private final ListChangeListener<? super GTagAnnotationContent> listener = c -> {
		while (c.next()) {
			if (c.wasRemoved())
				updateApplyingAnnotations(c.getRemoved().stream(), (annotations, gTagAnnotation) -> annotations.remove(gTagAnnotation));
			if (c.wasAdded())
				updateApplyingAnnotations(c.getAddedSubList().stream(), (annotations, gTagAnnotation) -> annotations.put(gTagAnnotation, gTagAnnotation.getContent()));
		}
	};

	private void updateApplyingAnnotations(Stream<? extends GTagAnnotationContent> streamToConsum, BiConsumer<ObservableMap<GTagAnnotation, GTagAnnotationContent>, GTagAnnotation> action) {
		streamToConsum.forEach(valueGeneric -> {
			GTagAnnotation gTagAnnotation = valueGeneric.getBaseComponent();
			TagAnnotation tagAnnotation = gTagAnnotation.getValue();
			LinkedList<Class<?>> path = new LinkedList<>(Stream.of(tagAnnotation.getPath()).collect(Collectors.toList()));
			path.addFirst(gTagAnnotation.getBaseComponent().getValue());
			LinkedList<Integer> pos = Arrays.stream(tagAnnotation.getPos()).boxed().collect(Collectors.toCollection(LinkedList<Integer>::new));
			if (!pos.isEmpty())
				pos.addFirst(-1);
			Set<Tag> concernedTags = searchTags(this, path, pos);
			concernedTags.forEach(tag -> action.accept(((GenericTagNode) tag.getTagNode()).tagAnnotations, gTagAnnotation));
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
		storedClasses.put(TagImpl.class, engine.find(GTag.class));
		storeClass(this.getClass());
		annotationsManager = new ExtendedAnnotationsManager(getClass());
		createSubTree();
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
		((GenericTagNode) htmlDomNode.getTag().getTagNode()).getTagAnnotations().addListener(getApplyingAnnotationsListener(htmlDomNode.getTag(), htmlDomNode.getModelContext()));
	}

	private MapChangeListener<? super GTagAnnotation, ? super GTagAnnotationContent> getApplyingAnnotationsListener(Tag tag, Context context) {
		return c -> {
			if (!context.isDestroyed()) {
				GTagAnnotation gTagAnnotation = c.getKey();
				if (c.wasRemoved()) {
					Class<?> annotationClass = gTagAnnotation.getValue().getAnnotationClass();

					if (Style.class.equals(annotationClass)) {
						tag.getDomNodeStyles(context).remove(gTagAnnotation.getValue().getName());
						tag.addPrefixBinding(context_ -> tag.getDomNodeStyles(context_).remove(gTagAnnotation.getValue().getName()));
					}

					if (GenericValueBackgroundColor.class.equals(annotationClass))
						tag.getDomNodeStyles(context).remove("background-color");

					if (StyleClass.class.equals(annotationClass))
						c.getValueRemoved().getJSonValue().getJsonArray("value").forEach(styleClass -> {
							tag.removeStyleClass((String) styleClass);
							tag.removeStyleClass(context, (String) styleClass);
						});

					if (Attribute.class.equals(annotationClass)) {
						tag.getDomNodeAttributes(context).remove(gTagAnnotation.getValue().getName());
						tag.addPrefixBinding(context_ -> tag.getDomNodeAttributes(context_).remove(gTagAnnotation.getValue().getName()));
					}
					// TODO: Other annotations.
				}
				if (c.wasAdded()) {
					Class<?> annotationClass = gTagAnnotation.getValue().getAnnotationClass();

					if (Style.class.equals(annotationClass)) {
						tag.addStyle(context, gTagAnnotation.getValue().getName(), gTagAnnotation.getContentValue());
						tag.addStyle(gTagAnnotation.getValue().getName(), gTagAnnotation.getContentValue());
					}

					if (GenericValueBackgroundColor.class.equals(annotationClass)) {
						tag.addStyle(context, "background-color",
								"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(context.getGeneric().getMeta())) ? ((GenericStringDefaults) tag).getGenericStringProperty(context).getValue() : gTagAnnotation.getContentValue());
						tag.addPrefixBinding(context_ -> tag.addStyle(context_, "background-color",
								"Color".equals(StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(context.getGeneric().getMeta())) ? ((GenericStringDefaults) tag).getGenericStringProperty(context).getValue() : gTagAnnotation.getContentValue()));
					}

					if (FlexDirectionStyle.class.equals(annotationClass)) {
						FlexDirection direction = FlexDirection.valueOf(gTagAnnotation.getContentValue());
						if (FlexDirectionDefaults.class.isAssignableFrom(tag.getClass()))
							((FlexDirectionDefaults) tag).setDirection(context, direction);
						tag.getRootTag().processFlexDirectionStyle(tag, direction);
					}

					if (ReverseFlexDirection.class.equals(annotationClass)) {
						tag.getRootTag().processReverseFlexDirection(tag);
						((FlexDirectionDefaults) tag).reverseDirection(context);
					}

					if (KeepFlexDirection.class.equals(annotationClass)) {
						tag.getRootTag().processKeepFlexDirection(tag);
						((FlexDirectionDefaults) tag).keepDirection(context);
					}

					if (StyleClass.class.equals(annotationClass))
						gTagAnnotation.getContentJSonArray().forEach(styleClass -> {
							tag.addStyleClass((String) styleClass);
							tag.addStyleClass(context, (String) styleClass);
						});

					if (Attribute.class.equals(annotationClass)) {
						tag.addAttribute(context, gTagAnnotation.getValue().getName(), gTagAnnotation.getContentValue());
						tag.addAttribute(gTagAnnotation.getValue().getName(), gTagAnnotation.getContentValue());
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
			result.setChildrenAnnotation(childrenAnnotation);
			for (Class<?> childClass : childrenAnnotation.value())
				storeClass(childClass);
		}

		for (Style styleAnnotation : clazz.getAnnotationsByType(Style.class))
			result.setStyleAnnotation(styleAnnotation);

		for (GenericValueBackgroundColor gvbColorAnnotation : clazz.getAnnotationsByType(GenericValueBackgroundColor.class))
			result.setGVBColorAnnotation(gvbColorAnnotation);

		for (FlexDirectionStyle fdAnnotation : clazz.getAnnotationsByType(FlexDirectionStyle.class))
			result.setFlexDirectionAnnotation(fdAnnotation);

		for (KeepFlexDirection kfdAnnotation : clazz.getAnnotationsByType(KeepFlexDirection.class))
			result.setKeepFlexDirectionAnnotation(kfdAnnotation);

		for (ReverseFlexDirection rfdAnnotation : clazz.getAnnotationsByType(ReverseFlexDirection.class))
			result.setReverseFlexDirectionAnnotation(rfdAnnotation);

		for (StyleClass scAnnotation : clazz.getAnnotationsByType(StyleClass.class))
			result.setStyleClassAnnotation(scAnnotation);

		for (Attribute annotation : clazz.getAnnotationsByType(Attribute.class))
			result.setAttributeAnnotation(annotation);

		getEngine().getCurrentCache().flush();
		return result;
	}

	public class GenericTagNode extends SimpleTagNode {

		private ObservableMap<GTagAnnotation, GTagAnnotationContent> tagAnnotations = new ObservableMapWrapper<GTagAnnotation, GTagAnnotationContent>(new LinkedHashMap<>()) {

			@Override
			public GTagAnnotationContent put(GTagAnnotation annotation, GTagAnnotationContent value) {
				Optional<GTagAnnotation> equivAnnotation = this.keySet().stream().filter(gta -> gta.getValue().equivs(annotation.getValue())).findAny();
				if (equivAnnotation.isPresent()) {
					GTagAnnotation overriddenAnnotation = equivAnnotation.get();
					if (overriddenAnnotation.getValue().getPath().length <= annotation.getValue().getPath().length) {
						remove(overriddenAnnotation);
					} else
						// Found an annotation applying to this tag that’s more precise than the new annotation,
						// so the new annotation is not added to the Set.
						return null;
				}
				return super.put(annotation, value);
			}
		};

		public ObservableMap<GTagAnnotation, GTagAnnotationContent> getTagAnnotations() {
			return tagAnnotations;
		}

		public Map<GTagAnnotation, GTagAnnotationContent> getTagAnnotations(Class<? extends Annotation> annotationClass) {
			return tagAnnotations.keySet().stream().filter(gta -> annotationClass.equals(gta.getValue().getAnnotationClass())).collect(Collectors.toMap(key -> key, key -> tagAnnotations.get(key)));
		}

		public GTagAnnotation getTagAnnotation(Class<? extends Annotation> annotationClass) {
			Map<GTagAnnotation, GTagAnnotationContent> annotations = getTagAnnotations(annotationClass);
			return annotations.isEmpty() ? null : annotations.keySet().stream().findAny().get();
		}

		public GenericTagNode(Tag tag) {
			Deque<Class<?>> classesToResult = new ArrayDeque<>();

			// Retrieve all applying annotations.
			Tag current = tag;
			while (current != null) {
				Set<GTagAnnotation> annotationsFound = selectAnnotations(current.getClass(), classesToResult, tag);
				annotationsFound.forEach(annotation -> tagAnnotations.put(annotation, annotation.getContent()));
				classesToResult.push(current.getClass());
				current = current.getParent();
			}

			// Build children if there are any.
			Optional<GTagAnnotation> childrenAnnotation = tagAnnotations.keySet().stream().filter(ta -> Children.class.equals(ta.getValue().getAnnotationClass())).findFirst();
			if (childrenAnnotation.isPresent())
				childrenAnnotation.get().childrenClassesStream().forEach(childClass -> getObservableChildren().add(createChild(tag, (Class<? extends TagImpl>) childClass)));
		}

		private Set<GTagAnnotation> selectAnnotations(Class<?> annotatedClass, Deque<Class<?>> classesToResult, Tag tag) {
			Set<GTagAnnotation> annotationsFound = new HashSet<>();
			for (GTagAnnotation annotation : storedClasses.get(annotatedClass).getAnnotations()) {
				Class<?>[] path = annotation.getValue().getPath();
				int[] pos = annotation.getValue().getPos();
				Class<?> annotationClass = annotation.getValue().getAnnotationClass();
				if (pos.length != 0 && pos.length != path.length)
					throw new IllegalStateException("The annotation " + annotationClass.getSimpleName() + " contains a path and an array of class positions of different lengths. path: "
							+ Arrays.asList(path).stream().map(c -> c.getSimpleName()).collect(Collectors.toList()) + ", positions: " + Arrays.stream(pos).boxed().collect(Collectors.toList()) + " found on class " + annotatedClass.getSimpleName());
				if (AnnotationsManager.isAssignableFrom(Arrays.asList(path), new ArrayList<>(classesToResult)) && AnnotationsManager.posMatches(pos, path, tag)) {
					annotationsFound.add(annotation);
				}
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

		default void setStyleAnnotation(Style annotation) {
			setAnnotation(Style.class, annotation.name(), annotation.value(), annotation.path(), annotation.pos());
		}

		default void setChildrenAnnotation(Children annotation) {
			GTagAnnotation gTagAnnotation = (GTagAnnotation) setHolder(getRoot().find(TagAnnotationAttribute.class), new TagAnnotation(Children.class, annotation.path(), annotation.pos()));
			gTagAnnotation.setHolder(getRoot().find(TagAnnotationContentAttribute.class), new JsonObject().put("value", new JsonArray(Arrays.asList(annotation.value()))).encodePrettily());
		}

		default void setGVBColorAnnotation(GenericValueBackgroundColor annotation) {
			setAnnotation(GenericValueBackgroundColor.class, null, annotation.value(), annotation.path(), annotation.pos());
		}

		default void setFlexDirectionAnnotation(FlexDirectionStyle annotation) {
			GTagAnnotation gTagAnnotation = (GTagAnnotation) setHolder(getRoot().find(TagAnnotationAttribute.class), new TagAnnotation(FlexDirectionStyle.class, annotation.path(), annotation.pos()));
			gTagAnnotation.setHolder(getRoot().find(TagAnnotationContentAttribute.class), new JsonObject().put("value", annotation.value()).encodePrettily());
		}

		default void setKeepFlexDirectionAnnotation(KeepFlexDirection annotation) {
			setAnnotation(KeepFlexDirection.class, null, null, annotation.path(), annotation.pos());
		}

		default void setReverseFlexDirectionAnnotation(ReverseFlexDirection annotation) {
			setAnnotation(ReverseFlexDirection.class, null, null, annotation.path(), annotation.pos());
		}

		default void setStyleClassAnnotation(StyleClass annotation) {
			GTagAnnotation gTagAnnotation = (GTagAnnotation) setHolder(getRoot().find(TagAnnotationAttribute.class), new TagAnnotation(StyleClass.class, annotation.path(), annotation.pos()));
			gTagAnnotation.setHolder(getRoot().find(TagAnnotationContentAttribute.class), new JsonObject().put("value", new JsonArray(Arrays.asList(annotation.value()))).encodePrettily());
		}

		default void setAttributeAnnotation(Attribute annotation) {
			setAnnotation(Attribute.class, annotation.name(), annotation.value(), annotation.path(), annotation.pos());
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

		@SuppressWarnings("unchecked")
		default public Stream<Class<?>> childrenClassesStream() {
			return (Stream) getContentJSonArray().stream().map(className -> {
				try {
					return Class.forName((String) className);
				} catch (ClassNotFoundException e) {
					throw new IllegalStateException("Class " + className + " not found");
				}
			});
		}

		default GTagAnnotationContent getContent() {
			return (GTagAnnotationContent) getHolder(getRoot().find(TagAnnotationContentAttribute.class));
		}

		default public JsonArray getContentJSonArray() {
			return getContentJsonObject().getJsonArray("value");
		}

		default public String getContentValue() {
			return getContentJsonObject().getString("value");
		}

		default public JsonObject getContentJsonObject() {
			return getContent().getJSonValue();
		}
	}

	public static interface GTagAnnotationContent extends Generic {
		@Override
		default public GTagAnnotation getBaseComponent() {
			return (GTagAnnotation) Generic.super.getBaseComponent();
		}

		@Override
		default public String getValue() {
			return (String) Generic.super.getValue();
		}

		default public JsonObject getJSonValue() {
			return new JsonObject(getValue());
		}
	}
}
