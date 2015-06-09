package org.genericsystem.issuetracker;

import javax.enterprise.inject.Produces;
import javax.enterprise.inject.spi.InjectionPoint;
import javax.inject.Inject;

import org.genericsystem.issuetracker.model.Issue;
import org.genericsystem.issuetracker.model.IssuePriority;
import org.genericsystem.issuetracker.model.IssueStatut;
import org.genericsystem.mutability.Engine;
import org.testng.annotations.Test;

@Test
public class IssueTrackerTest extends AbstractTest {

	@Inject
	Engine engine;

	@Inject
	Issue issue;

	// public void addNewIssue() {
	// Issue issue = engine.find(Issue.class);
	// issue.addInstance();
	// }

	public void test() {
		assert null != engine.find(IssuePriority.class);
		assert null != engine.find(IssueStatut.class);
	}

	public static interface A {
		String getNom();
	}

	public static class B implements A {

		@Override
		public String getNom() {
			return "### B";
		}
	}

	public static class ProviderA {
		@Produces
		public A getA(InjectionPoint ip) {
			return () -> "### A";
		}
	}

	public static class ProviderB {
		// @Produces
		// public B getB(InjectionPoint ip) {
		// return new B() {
		// @Override
		// public String getNom() {
		// return "C";
		// }
		// };
		// }
	}

	@Inject
	B b;

	public void testDInsertion() {
		System.out.println("@@@@@@@@@@" + b.getNom());
	}
}
