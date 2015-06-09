package org.genericsystem.issuetracker.bean;

import java.io.Serializable;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;
import javax.inject.Inject;

import org.genericsystem.issuetracker.model.Description;
import org.genericsystem.issuetracker.model.Issue;
import org.genericsystem.issuetracker.model.IssuePriority;
import org.genericsystem.issuetracker.model.IssueStatut;
import org.genericsystem.issuetracker.model.IssueVersion;
import org.genericsystem.issuetracker.model.Version;
import org.genericsystem.issuetracker.qualifier.Provide;
import org.genericsystem.mutability.Generic;

public class IssueBean extends AbstractBean implements Serializable {
	private static final long serialVersionUID = 4142394683395145575L;

	@Inject
	@Provide
	private transient Issue issue;

	@Inject
	@Provide
	private transient Description description;

	@Inject
	@Provide
	private transient IssueStatut issueStatut;

	@Inject
	@Provide
	private transient IssuePriority issuePriority;

	@Inject
	@Provide
	private transient IssueVersion issueVersion;

	@Inject
	@Provide
	private transient Version version;

	@Inject
	private transient FilterBean filterBean;

	public void addIssue(String newIssueDescription) {
		issue.addInstance(null).setHolder(description, newIssueDescription);
		FacesContext.getCurrentInstance().addMessage(null, new FacesMessage("Priority is required."));
	}

	public List<Generic> getIssuesByStatut() {
		return (filterBean.getPredicate(issueStatut) != null) ? issue.getSubInstances().stream().filter(filterBean.getPredicate(issueStatut)).collect(Collectors.toList()) : issue.getSubInstances().stream().collect(Collectors.toList());
	}

	public void deleteIssue(Generic issue) {
		issue.remove();
	}

	public List<String> getVersionsByIssue(Generic selectedIssue) {
		return selectedIssue.getLinks(issueVersion).stream().map(generic -> Objects.toString(generic.getValue())).collect(Collectors.toList());
	}

	public void addVersionsToIssue(Generic issue, List<String> selectedVersions) {
		for (String selectVersion : selectedVersions)
			issue.setLink(issueVersion, selectVersion, version);
		for (Generic link : issue.getLinks(issueVersion).stream().collect(Collectors.toList()))
			if (!selectedVersions.contains(link.getValue()))
				issue.getLink(issueVersion, link.getValue(), version).remove();
	}

	public Description getDescription() {
		return description;
	}

	public IssueStatut getIssueStatut() {
		return issueStatut;
	}

	public IssuePriority getIssuePriority() {
		return issuePriority;
	}

	public IssueVersion getIssueVersion() {
		return issueVersion;
	}

}
