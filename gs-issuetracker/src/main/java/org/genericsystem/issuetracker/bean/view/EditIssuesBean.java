package org.genericsystem.issuetracker.bean.view;

import java.util.List;

import javax.enterprise.context.RequestScoped;
import javax.inject.Inject;
import javax.inject.Named;

import org.genericsystem.issuetracker.bean.AbstractBean.ElStringWrapper;
import org.genericsystem.issuetracker.bean.IssueBean;
import org.genericsystem.issuetracker.bean.PriorityBean;
import org.genericsystem.issuetracker.bean.StatutBean;
import org.genericsystem.mutability.Generic;

@Named
@RequestScoped
public class EditIssuesBean {

	@Inject
	private IssueSelectedBean issueSelectedBean;

	@Inject
	private IssueBean issueBean;

	@Inject
	private PriorityBean priorityBean;

	@Inject
	private StatutBean statutBean;

	private String newIssueDescription;

	public String addIssue() {
		issueBean.addIssue(newIssueDescription);
		return "#";
	}

	public String delete(Generic issue) {
		issueSelectedBean.setSelectedIssue(null);
		issueBean.deleteIssue(issue);
		return "#";
	}

	public ElStringWrapper getDescription(Generic issue) {
		return issueBean.updateSingleHolder(issue, issueBean.getDescription());
	}

	public ElStringWrapper getPriority(Generic issue) {
		return issueBean.updateSingleHolder(issue, issueBean.getIssuePriority());
	}

	public ElStringWrapper getStatut(Generic issue) {
		return issueBean.updateSingleHolder(issue, issueBean.getIssueStatut());
	}

	public String setSelected(Generic selectedIssue) {
		issueSelectedBean.setSelectedIssue(selectedIssue);
		return "#";
	}

	public List<Generic> getList() {
		return issueBean.getIssuesByStatut();
	}

	public List<String> getPriorities() {
		return priorityBean.getPriorities();
	}

	public List<String> getStatuts() {
		return statutBean.getStatuts();
	}

	public String getNewIssueDescription() {
		return newIssueDescription;
	}

	public void setNewIssueDescription(String newIssueDescription) {
		this.newIssueDescription = newIssueDescription;
	}
}
