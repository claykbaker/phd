package com.example.twentyquestionsgame;

import java.util.Map;
import java.util.HashMap;  // Or another implementation of Map

public class GameObject {
    private String name;
    private String description;
    Map<String, Boolean> attributes;

    public GameObject(String name, String description) {
        this.name = name;
        this.description = description;
        this.attributes = new HashMap<>();
    }

    public void addAttribute(String key, boolean value) {
        attributes.put(key, value);
    }

    public String getName() {
        return name;
    }

    public String getDescription() {
        return description;
    }

    public Boolean getAttribute(String key) {
        return attributes.getOrDefault(key, null);
    }
}
