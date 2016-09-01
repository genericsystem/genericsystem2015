package org.genericsystem.reactor.gs2;

public class MetaTag {
	private Class<? extends GSComposite> builder;
	private MetaTag[] subNodes;

	public <TAG extends GSComposite> MetaTag(Class<? extends GSComposite> builder, MetaTag... subNodes) {
		this.builder = builder;
		this.subNodes = subNodes;
	}

	public <TAG extends GSComposite> Class<TAG> getBuilder() {
		return (Class<TAG>) builder;
	}

	public MetaTag[] getSubNodes() {
		return subNodes;
	}
}