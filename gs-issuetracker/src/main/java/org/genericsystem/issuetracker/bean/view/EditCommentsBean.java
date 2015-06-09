package org.genericsystem.issuetracker.bean.view;

import java.util.ArrayList;
import java.util.List;

import javax.enterprise.context.RequestScoped;
import javax.inject.Inject;
import javax.inject.Named;

import org.genericsystem.issuetracker.bean.AbstractBean.ElStringWrapper;
import org.genericsystem.issuetracker.bean.CommentBean;
import org.genericsystem.mutability.Generic;

@Named
@RequestScoped
public class EditCommentsBean {

	@Inject
	private IssueSelectedBean issueSelectedBean;

	@Inject
	private CommentSelectedBean commentSelectedBean;

	@Inject
	private CommentBean commentBean;

	public List<String> getAllComments() {
		return commentBean.getAllComments();
	}

	public List<Generic> getList() {
		if (issueSelectedBean.getSelectedIssue() == null)
			return new ArrayList<Generic>();
		return commentBean.getCommentsByIssue(issueSelectedBean.getSelectedIssue());
	}

	public ElStringWrapper getComment(Generic issue) {
		return commentBean.updateMultiHolder(issue, null, commentBean.getComment());
	}

	public String delete(Generic comment) {
		commentSelectedBean.setSelected(null);
		commentBean.deleteComment(comment);
		return "#";
	}

	public Generic getSelected() {
		return commentSelectedBean.getSelected();
	}

	public void setSelected(Generic selectedComment) {
		commentSelectedBean.setSelected(selectedComment);
	}

	public Generic getSelectedIssue() {
		return issueSelectedBean.getSelectedIssue();
	}

	public void setSelectedIssue(Generic selectedIssue) {
		issueSelectedBean.setSelectedIssue(selectedIssue);
	}
}
