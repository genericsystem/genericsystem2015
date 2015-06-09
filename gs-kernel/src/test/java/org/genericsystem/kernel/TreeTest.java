package org.genericsystem.kernel;

import java.util.Arrays;
import java.util.Collections;
import java.util.stream.Collectors;

import org.genericsystem.api.core.IVertex.Visitor;
import org.genericsystem.api.core.exceptions.AmbiguousSelectionException;
import org.genericsystem.api.core.exceptions.ExistsException;
import org.testng.annotations.Test;

@Test
public class TreeTest extends AbstractTest {

	public void test001() {
		Root root = new Root();
		Generic tree = root.addInstance("Tree");
		assert tree.getSupers().isEmpty();
	}

	public void test002() {
		Root root = new Root();
		Generic tree = root.addInstance("Tree");
		Generic rootNode = tree.addInstance("rootNode");

		assert tree.equals(rootNode.getMeta()) : rootNode.detailedInfo();
		assert rootNode.getSupers().isEmpty();

		assert tree.getInstances().contains(rootNode);
		assert tree.getInstances().size() == 1;
		assert tree.getSubInstances().contains(rootNode) : tree.getSubInstances().stream().collect(Collectors.toList());
		assert tree.getSubInstances().size() == 1;
		assert rootNode.getSupers().isEmpty();
	}

	public void test003() {
		Root root = new Root();
		Generic tree = root.addInstance("tree");
		Generic html = tree.addInstance("html");
		assert html.getMeta().equals(tree);
		assert html.getSupers().isEmpty();
	}

	public void test004() {
		Root root = new Root();
		Generic tree = root.addInstance("tree");
		Generic html = tree.addInstance("html");
		Generic head = tree.addInstance(html, "head");
		Generic body = tree.addInstance(html, "body");
		Generic div = tree.addInstance(body, "div");

		assert !html.getInheritings().contains(html);
		assert html.getInheritings().containsAll(Arrays.asList(head, body)) : html.getInheritings().info();
		assert html.getInheritings().size() == 2;
		assert html.getSubInheritings().containsAll(Arrays.asList(html, head, body, div));
		assert html.getSubInheritings().size() == 4;

		assert head.getInheritings().isEmpty();
		assert head.getSubInheritings().contains(head);
		assert head.getSubInheritings().size() == 1;

		assert body.getInheritings().contains(div);
		assert body.getInheritings().size() == 1;
		assert body.getSubInheritings().containsAll(Arrays.asList(body, div));
		assert body.getSubInheritings().size() == 2;

		assert div.getInheritings().isEmpty();
		assert div.getSubInheritings().contains(div);
		assert div.getSubInheritings().size() == 1;

	}

	public void test005() {
		Root root = new Root();
		Generic tree = root.addInstance("tree");
		Generic rootNode = tree.addInstance("rootNode");
		Generic htmlNode = tree.addInstance(rootNode, "htmlNode");
		Generic bodyNode = tree.addInstance(htmlNode, "bodyNode");
		Generic divNode = tree.addInstance(bodyNode, "divNode");
		Generic formNode = tree.addInstance(divNode, "formNode");

		assert tree.getSubInstances().contains(rootNode);
		assert tree.getSubInstances().contains(bodyNode);
		assert tree.getSubInstances().contains(divNode);
		assert tree.getSubInstances().contains(formNode);
		assert tree.getSubInstances().size() == 5;
	}

	public void test006() {
		Root root = new Root();
		root.addInstance("Tree");
		catchAndCheckCause(() -> root.addInstance("Tree"), ExistsException.class);
	}

	public void test007() {
		Root root = new Root();

		Generic tree = root.addInstance("Tree");
		Generic color = root.addInstance("Color");
		Generic treeColor = tree.addRelation("TreeColor", color);

		Generic blue = color.addInstance("blue");
		Generic red = color.addInstance("red");
		Generic green = color.addInstance("green");

		tree.setLink(treeColor, "treeIsBlueByDefault", blue);

		Generic html = tree.addInstance("html");
		html.setLink(treeColor, "htmlIsRed", red);
		Generic head = tree.addInstance(html, "head");
		Generic body = tree.addInstance(html, "body");
		Generic div = tree.addInstance(body, "div");
		div.setLink(treeColor, "divIsGreen", green);

		assert tree.getLinks(treeColor).first().getTargetComponent().equals(blue);
		assert html.getLinks(treeColor).first().getTargetComponent().equals(red);
		assert head.getLinks(treeColor).first().getTargetComponent().equals(red);
		assert body.getLinks(treeColor).first().getTargetComponent().equals(red);
		assert div.getLinks(treeColor).first().getTargetComponent().equals(green);
	}

	public void test008() {
		Root root = new Root();

		Generic html5Tags = root.addInstance("Html5Tags");

		Generic html = html5Tags.addInstance("html");

		html5Tags.addInstance(html, "header");
		Generic body = html5Tags.addInstance(html, "body");
		html5Tags.addInstance(html, "footer");

		html5Tags.addInstance(body, "p");
		html5Tags.addInstance(body, "table");

		int[] result = { 0 };

		html5Tags.traverse(new Visitor<Generic>() {
			@Override
			public void before(Generic node) {
				if (node.getValue().equals("html")) {
					result[0] += 1;
				} else if (node.getValue().equals("header") || node.getValue().equals("body") || node.getValue().equals("footer")) {
					result[0] += 2;
				} else if (node.getValue().equals("p") || node.getValue().equals("table")) {
					result[0] += 3;
				}
			}

			@Override
			public void after(Generic node) {
				if (node.getValue().equals("html")) {
					result[0] -= 1;
				} else if (node.getValue().equals("header") || node.getValue().equals("body") || node.getValue().equals("footer")) {
					result[0] -= 2;
				} else if (node.getValue().equals("p") || node.getValue().equals("table")) {
					result[0] -= 3;
				}
			}
		});

		assert result[0] == 0;
	}

	public void test009() {
		Root root = new Root();
		Generic a1 = root.addInstance("A");
		Generic b = root.addInstance(a1, "B");
		Generic a2 = root.addInstance(b, "A");
		assert root.getInstance(Collections.emptyList(), "A").equals(a1);
		catchAndCheckCause(() -> root.getInstance("A"), AmbiguousSelectionException.class);
		assert root.getInstance(Arrays.asList(b), "A").equals(a2);
	}

	public void test010() {
		Root root = new Root();
		Generic a1 = root.addInstance("A");
		Generic b = root.addInstance("B");
		Generic a2 = root.addInstance(b, "A");
		assert root.getInstance(Collections.emptyList(), "A").equals(a1);
		catchAndCheckCause(() -> root.getInstance("A"), AmbiguousSelectionException.class);
		assert root.getInstance(Arrays.asList(b), "A").equals(a2);
	}

	public void test011() {
		Root root = new Root();
		Generic b = root.addInstance("B");
		Generic c = root.addInstance("C");
		Generic a1 = root.addInstance(b, "A");
		root.addInstance(c, "A");
		catchAndCheckCause(() -> root.getInstance("A"), AmbiguousSelectionException.class);
		assert root.getInstance(Arrays.asList(b), "A").equals(a1);
	}

}
