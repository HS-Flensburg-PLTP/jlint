class NeedBraces {

    void foo() {
        for(int i = 0; i < 10; i++) System.out.println(i);
        for(int i = 0; i < 10; i++) 
            System.out.println(i);

        for (int i : arr) System.out.println(i);
        for (int i : arr) 
            System.out.println(i);

        do i++; while (true);
        do
            i++;
        while (true);

        while (true) i++;
        while (true)
            i++;
        
        if(true) i++;
        if(true)
            i++;

        if(true) 
            i++;
        else {
            i--;
        }
        if(true) {
            i++;
        } else 
            i--;
        if(true) // erzeugt nur eine Msg statt zwei
            i++;
        else 
            i--;
    }
}
