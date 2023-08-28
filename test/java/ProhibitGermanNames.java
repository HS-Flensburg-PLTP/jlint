class EineSuperDeutscheKlasse<Liste extends List> {
    private final int WIRD_DAS_HIER_AUCH_GEFUNDEN = 1337;
    private final String ANANAS = "PIZZA";

    private Liste apfel = "Gurke";

    public EineSuperDeutscheKlasse() {
        String wuerfel = "Würfel";
        int aepfelCounter = 3;
        String oeffentlich = wuerfel + "public";
        String strasse = "Straße";
    }

    public void OneFunction() {
        String testString = "test";
        int runsVar = 1;

        for (int maus = 0; maus < 10; maus++) {
            String meineZeichenfolge = "Hier";
            meineZeichenfolge.map((zeichen) -> { return zeichen;});
        }
    }
}

enum DasIstOneEnum {
    One,
    Two,
    Drei
}

interface IchBinEinTier<Tier> {
    public void tierSound();
    public void run();
    public void berechneErgebnis();
}