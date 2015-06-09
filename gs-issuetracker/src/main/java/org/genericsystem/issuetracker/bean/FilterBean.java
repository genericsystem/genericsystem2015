package org.genericsystem.issuetracker.bean;

import java.io.Serializable;
import java.util.List;
import java.util.function.Predicate;

import javax.enterprise.context.SessionScoped;
import javax.inject.Inject;
import javax.inject.Named;

import org.genericsystem.mutability.Generic;

@Named
@SessionScoped
public class FilterBean implements Serializable {
	private static final long serialVersionUID = 3895394856549620861L;

	private Predicate<? super Generic> predicate;

	@Inject
	private StatutBean statutBean;

	private String searchedStatut;
	private List<String> statuts;

	public Predicate<? super Generic> getPredicate(Generic relation) {
		predicate = (searchedStatut != null) ? generic -> generic.getLinks(relation).stream().anyMatch(link -> link.getTargetComponent().getValue().equals(searchedStatut)) : null;
		return predicate;
	}

	public String getSearchedStatut() {
		return searchedStatut;
	}

	public void setSearchedStatut(String searchedStatut) {
		this.searchedStatut = searchedStatut;
	}

	public List<String> getStatuts() {
		statuts = statutBean.getStatuts();
		return statuts;
	}

}
