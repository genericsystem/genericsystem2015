package org.genericsystem.issuetracker.bean.view;

import java.io.Serializable;
import java.util.List;

import javax.enterprise.context.SessionScoped;
import javax.inject.Inject;
import javax.inject.Named;

import org.genericsystem.issuetracker.bean.AbstractBean.ElStringWrapper;
import org.genericsystem.issuetracker.bean.IssueBean;
import org.genericsystem.issuetracker.bean.PriorityBean;
import org.genericsystem.issuetracker.bean.StatutBean;
import org.genericsystem.issuetracker.bean.VersionBean;
import org.genericsystem.mutability.Generic;

@Named
@SessionScoped
public class IssueSelectedBean implements Serializable {
	private static final long serialVersionUID = -6707062384436255711L;

	@Inject
	private IssueBean issueBean;

	@Inject
	private PriorityBean priorityBean;

	@Inject
	private StatutBean statutBean;

	@Inject
	private VersionBean versionBean;

	private transient Generic selectedIssue;

	public List<String> getPriorities() {
		return priorityBean.getPriorities();
	}

	public List<String> getStatuts() {
		return statutBean.getStatuts();
	}

	public List<String> getVersions() {
		return versionBean.getVersions();
	}

	public ElStringWrapper updateVersions(Generic issue) {
		return issueBean.updateSingleHolder(issue, issueBean.getIssueVersion());
	}

	public ElStringWrapper updateDescription(Generic issue) {
		return issueBean.updateSingleHolder(issue, issueBean.getDescription());
	}

	public ElStringWrapper updatePriority(Generic issue) {
		return issueBean.updateSingleHolder(issue, issueBean.getIssuePriority());
	}

	public ElStringWrapper updateStatut(Generic issue) {
		return issueBean.updateSingleHolder(issue, issueBean.getIssueStatut());
	}

	public IssueBean getIssueBean() {
		return issueBean;
	}

	public Generic getSelectedIssue() {
		return selectedIssue;
	}

	public void setSelectedIssue(Generic selectedIssue) {
		this.selectedIssue = selectedIssue;
	}

}
