package org.genericsystem.issuetracker.bean;

import java.io.Serializable;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import javax.inject.Inject;

import org.genericsystem.issuetracker.model.Comment;
import org.genericsystem.issuetracker.model.Issue;
import org.genericsystem.issuetracker.qualifier.Provide;
import org.genericsystem.mutability.Generic;

public class CommentBean extends AbstractBean implements Serializable {
	private static final long serialVersionUID = 5563656957808416784L;

	@Inject
	@Provide
	private transient Issue issue;

	@Inject
	@Provide
	private transient Comment comment;

	public List<String> getAllComments() {
		return comment.getInstances().stream().map(generic -> Objects.toString(generic.getValue())).collect(Collectors.toList());
	}

	public List<Generic> getCommentsByIssue(Generic issue) {
		return issue.getHolders(comment).stream().collect(Collectors.toList());
	}

	public void deleteComment(Generic comment) {
		comment.remove();
	}

	public Comment getComment() {
		return comment;
	}

}
