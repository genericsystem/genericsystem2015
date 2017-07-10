package org.genericsystem.watch.beta;

import java.util.ArrayList;
import java.util.List;

public class RoundRobin {

	private int index = 0;
	private List<String> privateAdresses = new ArrayList<>();

	public List<String> getPrivateAdresses() {
		return privateAdresses;
	}

	public boolean register(String remotePrivateAdress) {
		return privateAdresses.contains(remotePrivateAdress) ? false : privateAdresses.add(remotePrivateAdress);
	}

	public String getNextAddress() {
		if (privateAdresses.isEmpty())
			return null;
		if (privateAdresses.size() == 1)
			return privateAdresses.get(0);
		return privateAdresses.get(index++ % privateAdresses.size());

	}

	public void remove(String Adresse) {
		if (privateAdresses.contains(Adresse))
			privateAdresses.remove(Adresse);
	}

	@Override
	public String toString() {
		return "Round robin : " + privateAdresses.toString();
	}
}
