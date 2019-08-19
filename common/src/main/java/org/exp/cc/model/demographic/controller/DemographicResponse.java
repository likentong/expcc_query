package org.exp.cc.model.demographic.controller;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Demographic response model.
 */
public class DemographicResponse {
    private final Map<String, Object> result;
    private final List<Map<String, Object>> details;

    @JsonCreator
    public DemographicResponse(@JsonProperty("result") final Map<String, Object> result,
                               @JsonProperty("details") final List<Map<String, Object>> details) {
        this.result = result;
        this.details = details;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final DemographicResponse that = (DemographicResponse) o;
        return Objects.equals(result, that.result) &&
                Objects.equals(details, that.details);
    }

    @Override
    public int hashCode() {
        return Objects.hash(result, details);
    }
}
