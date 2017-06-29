package org.genericsystem.watch.beta;

import java.util.ArrayList;
import java.util.List;

public class RoundRobin {

	private int index = 0;
	private List<String> privateAdresses = new ArrayList<>();

	public List<String> getPrivateAdresses() {
		return privateAdresses;
	}

	public boolean Register(String remotePrivateAdress) {
		return privateAdresses.contains(remotePrivateAdress) ? false : privateAdresses.add(remotePrivateAdress);
	}

	public String getNextAddress() {
		if (privateAdresses.size() > 0) {
			if (privateAdresses.size() > 1)
				index = (index + 1) % privateAdresses.size();
			return privateAdresses.get(index);
		} else {
			return null;
		}
	}

	public void remove(String Adresse) {
		if (privateAdresses.contains(Adresse))
			privateAdresses.remove(Adresse);
	}
}
