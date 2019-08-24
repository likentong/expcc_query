package org.exp.cc.datastore.dao.demographic;

import com.google.common.collect.ImmutableMap;
import org.exp.cc.constant.PersistenceConstant;
import org.exp.cc.datastore.dao.demographic.steps.DemographicDAOImplStep;
import org.exp.cc.model.persistence.QueryCriteria;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.NullSource;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import static org.assertj.core.util.Lists.newArrayList;
import static org.mockito.Mockito.mock;

/**
 * DemograhicDAO implementation test.
 */
public class DemographicDAOImplTest {
    private final DemographicDAOImplStep step;
    private QueryCriteria queryCriteria;

    public DemographicDAOImplTest() {
        this.step = new DemographicDAOImplStep();
    }

    /**
     * Test Initialization.
     */
    @BeforeEach
    public void init() {
        this.queryCriteria = mock(QueryCriteria.class);

        this.step.givenISetupDemographicDAOImpl();
    }

    /**
     * getIds with null query criteria, should throw {@link IllegalArgumentException}.
     */
    @ParameterizedTest
    @NullSource
    public void getIds_NullQueryCriteria_ThrowsIllegalArgumentException(final QueryCriteria queryCriteria) {
        this.step.givenISetupAThrowingCallable(() -> this.step.whenIGetIds(queryCriteria))
                .thenExceptionMatchCorrectType(IllegalArgumentException.class)
                .thenExceptionWithCorrectMessage("queryCriteria cannot be null.");
    }

    /**
     * getIds with query criteria, should return expected results.
     * @param dataStoreResults data store results
     * @param results getIds results
     */
    @ParameterizedTest(name = "{0}")
    @MethodSource("dataStoreResultsWithGetIdsResults")
    public void getIds_WithCorrectQueryCriteria_ShouldReturnResults(final String testCaseName, final List<Map<String, Object>> dataStoreResults, final List<Object> results) {
        this.step.givenIMockDataStoreDAOQueryData(dataStoreResults)
                .whenIGetIds(this.queryCriteria)
                .thenResultsShouldBe(results);
    }

    private static Stream<Arguments> dataStoreResultsWithGetIdsResults() {
        return Stream.of(
                Arguments.of("DataStoreDAO_ReturnsEmptyList", Collections.emptyList(), Collections.emptyList()),
                Arguments.of("DataStoreDAO_ReturnsResultsWithDemographicID", newArrayList(
                        ImmutableMap.of(PersistenceConstant.Demographic.ID, 1),
                        ImmutableMap.of(PersistenceConstant.Demographic.ID, 2)
                ), newArrayList(1, 2)),
                Arguments.arguments("DataStoreDAO_ReturnsResultsWithoutDemographicID", newArrayList(
                        ImmutableMap.of("id", 1)
                ), Collections.emptyList())
        );
    }

}
