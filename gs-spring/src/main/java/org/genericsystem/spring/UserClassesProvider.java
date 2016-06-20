package org.genericsystem.spring;

import java.util.ArrayList;
import java.util.List;

import org.springframework.context.annotation.Lazy;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Scope("singleton")
@Component
@Lazy
public class UserClassesProvider {

	List<Class<?>> userClasses = new ArrayList<>();

	public void addUserClasse(Class<?> userClasse) {
		userClasses.add(userClasse);
	}

	public void setUserClasse(List<Class<?>> userClasses) {
		this.userClasses = userClasses;
	}

	public Class<?>[] getUserClassesArray() {
		return userClasses.toArray(new Class[userClasses.size()]);
	}
}
