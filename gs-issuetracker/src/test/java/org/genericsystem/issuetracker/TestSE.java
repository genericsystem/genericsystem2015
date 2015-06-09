package org.genericsystem.issuetracker;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.mutability.Engine;
import org.genericsystem.mutability.Generic;
import org.testng.annotations.Test;

@Test
public class TestSE {

	public void test() {
		Engine engine = new Engine();
		Generic issue = engine.addInstance("Issue");
		Generic statut = engine.addInstance("Statut").setInstanceValueClassConstraint(String.class);
		Generic issueStatut = issue.setRelation("IssueStatut", statut).enableSingularConstraint(ApiStatics.BASE_POSITION);
		Generic priority = engine.addInstance("Priority").setInstanceValueClassConstraint(String.class);
		Generic issuePriority = issue.setRelation("IssuePriority", priority).enableSingularConstraint(ApiStatics.BASE_POSITION);
		issue.setRelation("IssuePriority", priority).enableSingularConstraint(ApiStatics.BASE_POSITION);
		issue.setRelation("IssueStatut", statut).enableSingularConstraint(ApiStatics.BASE_POSITION);
	}

	public void test2() {
		Engine engine = new Engine();
		Generic issue = engine.addInstance("Issue");
		Generic comment = engine.addInstance("Comment").setInstanceValueClassConstraint(String.class);
		Generic issueComment = issue.setRelation("IssueComment", comment).enableSingularConstraint(ApiStatics.TARGET_POSITION);
		Generic myIssue = issue.setInstance("myIssue");
		myIssue.setLink(issueComment, "link", comment.setInstance("comment1"));
		System.out.println(myIssue.getLinks(issueComment).info());
		myIssue.setLink(issueComment, "link", comment.setInstance("comment2"));
		System.out.println(myIssue.getLinks(issueComment).info());
	}

	public void testRequiredConstraint() {
		Engine engine = new Engine();
		Generic issue = engine.addInstance("Issue");
		Generic priority = engine.addInstance("Priority");
		Generic statut = engine.addInstance("Statut");

		Generic description = issue.addAttribute("description");
		Generic issuePriority = issue.setRelation("IssuePriority", priority);
		Generic issueStatut = issue.setRelation("IssueStatut", statut);
		issuePriority.enableSingularConstraint(ApiStatics.BASE_POSITION);
		issuePriority.enableRequiredConstraint(ApiStatics.BASE_POSITION);
		issueStatut.enableSingularConstraint(ApiStatics.BASE_POSITION);

		Generic myPriority = priority.addInstance("myPriority");
		Generic myStatut = statut.addInstance("myStatut");
		Generic myIssue = issue.addInstance("myIssue");

		issue.setLink(issueStatut, "myIssueStatut", myStatut);
		issue.setHolder(description, "myDescription");
		Generic myIssueWithPriority = myIssue.setLink(issuePriority, "myIssueWithPriority", myPriority);
		engine.getCurrentCache().flush();

		myIssue.remove();
		assert !myIssue.isAlive();
		assert !myIssueWithPriority.isAlive();
		engine.getCurrentCache().flush();
		assert issue.isAlive();
		assert priority.isAlive();
		assert issuePriority.isAlive();
		assert myPriority.isAlive();
		assert !myIssue.isAlive();
		assert !myIssueWithPriority.isAlive();
	}
}
