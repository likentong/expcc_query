package org.exp.cc.service.impl;

import com.google.common.collect.ImmutableMap;
import org.exp.cc.constant.ApplicationConstant;
import org.exp.cc.constant.PersistenceConstant;
import org.exp.cc.model.service.Result;
import org.exp.cc.model.service.demographic.DemographicQuery;
import org.exp.cc.service.impl.steps.DemographicServiceImplStep;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullSource;

import java.util.HashSet;
import java.util.List;
import java.util.Map;

import static org.assertj.core.util.Lists.newArrayList;
import static org.mockito.Mockito.mock;

/**
 * Demographic service implementation test.
 */
public class DemographicServiceImplTest {
    private DemographicServiceImplStep step;
    private DemographicQuery query;

    public DemographicServiceImplTest() {
        this.step = new DemographicServiceImplStep();
    }

    /**
     * Test initialization.
     */
    @BeforeEach
    public void init() {
        query = mock(DemographicQuery.class);
    }

    /**
     * getIdWithPersonCount with invalid demographic query, should throws {@link IllegalArgumentException}.
     * @param query demographic query
     */
    @ParameterizedTest
    @NullSource
    public void getIdWithPersonCount_WithInvalidDemographicQuery_ShouldThrowsIllegalArgumentException(final DemographicQuery query) {
        this.step.givenISetupAThrowingCallable(() -> this.step.givenIHaveNoSetup()
                .whenIGetIDWithPersonCount(query))
                .thenExceptionMatchCorrectType(IllegalArgumentException.class)
                .thenExceptionWithCorrectMessage("demographic query cannot be null.");
    }

    /**
     * getIdWithPersonCount with valid demographic query, should return result.
     */
    @Test
    public void getIdWithPersonCount_WithValidDemographicQuery_ShouldReturnResult() {
        final List<Object> id = newArrayList(1, 2);
        final List<Map<String, Object>> data = newArrayList(ImmutableMap.of("ID", 1, "sum", 2), ImmutableMap.of("ID", 2, "sum", 5));

        this.step.givenISetupDemographicDAOGetIds(query, id)
                .givenISetupPersonDAOGetPersonCountByDemographicId(new HashSet<>(id), data)
                .whenIGetIDWithPersonCount(query)
                .thenResultShouldBe(new Result(data, ImmutableMap.of(PersistenceConstant.Demographic.ID, id, ApplicationConstant.Summary.RECORD_COUNT, 2)));
    }

}
