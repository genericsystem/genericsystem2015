package org.genericsystem.kernel;

import org.genericsystem.common.Generic;
import org.genericsystem.kernel.BasicEngine;
import org.testng.annotations.Test;

@Test
public class MetasTests extends AbstractTest {

	public void test001() {
		BasicEngine root = new BasicEngine();
		Generic metaAttribute = root.getMetaAttribute();
		Generic metaRelation = metaAttribute.getInheritings().first();

		assert metaAttribute.getMeta() == metaAttribute;
		assert metaAttribute.isMeta();
		assert metaAttribute.getBaseComponent().equals(root);
		assert metaAttribute.inheritsFrom(root);

		assert metaRelation.isMeta();
		assert metaRelation.getBaseComponent().equals(root);
		assert metaRelation.getTargetComponent().equals(root);
		assert metaRelation.inheritsFrom(metaAttribute);
	}

	public void test002() {
		BasicEngine root = new BasicEngine();
		assert root.getCurrentCache().getMeta(1).equals(root.getMetaAttribute());
		assert root.getCurrentCache().getMeta(5) == null;
		Generic pentaMeta = root.getCurrentCache().setMeta(5);
		assert root.getCurrentCache().getMeta(5) == pentaMeta;
		assert pentaMeta == (root.getCurrentCache()).setMeta(5);
		assert pentaMeta.equals(root.getCurrentCache().getMeta(5));
		assert root.getCurrentCache().getMeta(3) == null;
		Generic ternaryMeta = (root.getCurrentCache()).setMeta(3);
		assert !pentaMeta.isAlive();
		assert root.getCurrentCache().getMeta(5).inheritsFrom(ternaryMeta);
	}

	public void test003() {
		BasicEngine root = new BasicEngine();
		assert root.setInstance(root.getValue(), root).equals(root.getMetaAttribute());
		assert root.setInstance(root.getValue(), root, root).equals(root.getMetaRelation());
		assert root.setInstance(root.getValue(), root, root, root).equals(root.getCurrentCache().getMeta(3));
	}
}
