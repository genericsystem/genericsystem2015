package org.genericsystem.issuetracker.bean.view;

import java.io.Serializable;

import javax.enterprise.context.SessionScoped;
import javax.inject.Inject;
import javax.inject.Named;

import org.genericsystem.issuetracker.bean.AbstractBean.ElStringWrapper;
import org.genericsystem.issuetracker.bean.CommentBean;
import org.genericsystem.mutability.Generic;

@Named
@SessionScoped
public class CommentSelectedBean implements Serializable {
	private static final long serialVersionUID = -7086725227684166363L;

	@Inject
	private IssueSelectedBean issueSelectedBean;

	@Inject
	private CommentBean commentBean;

	private transient Generic selected;

	public ElStringWrapper getComment(Generic issue) {
		return commentBean.updateMultiHolder(issue, selected, commentBean.getComment());
	}

	public Serializable getSelectedComment() {
		return selected.getValue();
	}

	public Generic getSelectedIssue() {
		return issueSelectedBean.getSelectedIssue();
	}

	public void setSelectedIssue(Generic selectedIssue) {
		issueSelectedBean.setSelectedIssue(selectedIssue);
	}

	public Generic getSelected() {
		return selected;
	}

	public void setSelected(Generic selected) {
		this.selected = selected;
	}

}
