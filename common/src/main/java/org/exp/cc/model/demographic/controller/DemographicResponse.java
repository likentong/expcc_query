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
    private final List<Map<String, Object>> result;
    private final Map<String, Object> summary;

    @JsonCreator
    public DemographicResponse(@JsonProperty("result") final List<Map<String, Object>> result,
                               @JsonProperty("details") final Map<String, Object> summary) {
        this.result = result;
        this.summary = summary;
    }

    public List<Map<String, Object>> getResult() {
        return result;
    }

    public Map<String, Object> getSummary() {
        return summary;
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
                Objects.equals(summary, that.summary);
    }

    @Override
    public int hashCode() {
        return Objects.hash(result, summary);
    }
}
