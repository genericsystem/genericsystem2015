package org.genericsystem.reactor.gscomponents;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.MetaBinding;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.model.TagSwitcher;

public abstract class TagImpl extends AbstractTag implements Tag {

	private MetaBinding<?> metaBinding;
	private final List<Consumer<Context>> preFixedBindings = new ArrayList<>();
	private final List<Consumer<Context>> postFixedBindings = new ArrayList<>();
	protected List<TagSwitcher> switchers = new ArrayList<>();

	@Override
	public void setParent(Tag parent) {
		super.setParent(parent);
		if (parent != null)
			parent.getObservableChildren().add(this);
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
	public void addSwitcher(TagSwitcher switcher) {
		switchers.add(switcher);
	}

	@Override
	public List<TagSwitcher> getSwitchers() {
		return switchers;
	}
}