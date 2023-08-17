package das.ist.ein.package;

class EineSuperDeutscheKlasse<Tomate> {
    private final int WIRD_DAS_HIER_AUCH_GEFUNDEN = 1337;
    private final String ANANAS = "PIZZA";

    public EineSuperDeutscheKlasse() {
        String wuerfel = "Würfel";
        int aepfelCounter = 3;
        String oeffentlich = "public";
        String strasse = "Straße";
    }

    public void MyFunction() {
        String testString = "Ahhhhh";
        int myRunsVar = 1;

        for (int maus = 0; maus < 10; maus++) {
            String meineZeichenfolge = "Hier";
        }
    }
}

public class Pair<TypParameter, VollParameter> {

    private K key;
    private V value;

    public Pair(K key, V value) {
    this.key = key;
    this.value = value;
    }

    public K getKey()    { return key; }
    public V getValue() { return value; }
}