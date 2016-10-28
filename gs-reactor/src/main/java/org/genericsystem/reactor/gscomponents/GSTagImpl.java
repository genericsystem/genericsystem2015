package org.genericsystem.reactor.gscomponents;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

import org.genericsystem.defaults.tools.ObservableListWrapperExtended;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.MetaBinding;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.model.ModeSelector;

import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;

public abstract class GSTagImpl implements Tag {

	private MetaBinding<?> metaBinding;
	private final List<Consumer<Context>> preFixedBindings = new ArrayList<>();
	private final List<Consumer<Context>> postFixedBindings = new ArrayList<>();
	private Tag parent;
	private final ObservableList<Tag> children = FXCollections.observableArrayList();
	private ObservableList<Tag> filteredChildren = children;
	protected ModeSelector modeSelector;

	protected GSTagImpl(Tag parent) {
		setParent(parent);
		beforeProcessAnnotations();
		getRootTag().getAnnotationsManager().processAnnotations(this);
		init();
	}

	public void setParent(Tag parent) {
		this.parent = parent;
		if (parent != null)
			parent.getObservableChildren().add(this);
	}

	protected GSTagImpl() {
	}

	@Override
	public String toString() {
		return getTag() + " " + getClass().getName();
	}

	@Override
	public List<Consumer<Context>> getPreFixedBindings() {
		return preFixedBindings;
	}

	@Override
	public List<Consumer<Context>> getPostFixedBindings() {
		return postFixedBindings;
	}

	@Override
	@SuppressWarnings("unchecked")
	public <BETWEEN> MetaBinding<BETWEEN> getMetaBinding() {
		return (MetaBinding<BETWEEN>) metaBinding;
	}

	@Override
	public <BETWEEN> void setMetaBinding(MetaBinding<BETWEEN> metaBinding) {
		if (this.metaBinding != null)
			throw new IllegalStateException("MetaBinding already defined");
		this.metaBinding = metaBinding;
	}

	@Override
	public ObservableList<Tag> getObservableChildren() {
		return children;
	}

	@Override
	public ObservableList<Tag> getObservableChildren(Context context) {
		if (!context.containsProperty(this, "extractorsMap"))
			createNewInitializedProperty("extractorsMap", context, c -> new HashMap<Tag, ObservableValue[]>() {

				private static final long serialVersionUID = -435743147955810836L;

				@Override
				public ObservableValue[] get(Object key) {
					ObservableValue[] result = super.get(key);
					Tag child = (Tag) key;
					if (result == null)
						put(child, result = child.getModeSelector() != null ? new ObservableValue[] { child.getModeSelector().apply(context, child) } : new ObservableValue[] { new SimpleBooleanProperty(true) });
					return result;
				};
			});
		ObservableList<Tag> extObs = new ObservableListWrapperExtended<Tag>(children, child -> getExtractors(context).get(child));
		filteredChildren = new FilteredList<Tag>(extObs, child -> Boolean.TRUE.equals(getExtractors(context).get(child)[0].getValue()));
		//		filteredChildren = new MinimalChangesObservableList<Tag>(new ListBinding<Tag>() {
		//			{
		//				for (Tag child : children)
		//					if (child.getModeSelector() != null)
		//						bind(child.getModeSelector().apply(context, child));
		//			}
		//
		//			@Override
		//			protected ObservableList<Tag> computeValue() {
		//				return children.filtered(child -> child.getModeSelector() != null ? Boolean.TRUE.equals(child.getModeSelector().apply(context, child).getValue()) : true);
		//			}
		//		});
		return filteredChildren;
	}

	private Map<Tag, ObservableValue[]> getExtractors(Context context) {
		return this.<Map<Tag, ObservableValue[]>> getProperty("extractorsMap", context).getValue();
	}

	@Override
	@SuppressWarnings("unchecked")
	public <COMPONENT extends Tag> COMPONENT getParent() {
		return (COMPONENT) parent;
	}

	@Override
	public void setModeSelector(ModeSelector modeSelector) {
		this.modeSelector = modeSelector;
	}

	@Override
	public ModeSelector getModeSelector() {
		return modeSelector;
	}
}