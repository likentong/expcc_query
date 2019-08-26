package org.exp.cc.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.ImmutableMap;
import org.apache.commons.io.FileUtils;
import org.exp.cc.model.controller.Response;
import org.exp.cc.model.controller.demographic.DemographicRequest;
import org.exp.cc.model.service.Result;
import org.exp.cc.model.service.demographic.DemographicQuery;
import org.exp.cc.service.DemographicService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.util.Lists.newArrayList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Demographic controller test.
 */
public class DemographicControllerTest {
    private MockMvc mockMvc;
    private DemographicService demographicService;
    private final ObjectMapper mapper;
    private static final String DEMOGRAPHIC_REQUEST_FILE_LOCATION = "demograhic-request.json";

    public DemographicControllerTest() {
        this.mapper = new ObjectMapper();
    }

    /**
     * Test Initialization.
     */
    @BeforeEach
    public void init() {
        this.demographicService = mock(DemographicService.class);
        this.mockMvc = MockMvcBuilders.standaloneSetup(new DemographicController(this.demographicService)).build();
    }

    /**
     * getIdWithPersonCount with valid demographic request, should return response.
     * @throws Exception exception
     */
    @Test
    public void getIdWithPersonCount_WithValidDemographicRequest_ShouldReturnsResponse() throws Exception {
        final String request = readFileToString(DEMOGRAPHIC_REQUEST_FILE_LOCATION);
        final DemographicRequest demographicRequest = this.mapper.readValue(request, DemographicRequest.class);

        final List<Map<String, Object>> data = newArrayList(ImmutableMap.of("ID", 2, "sum", 7));
        final Map<String, Object> summary = ImmutableMap.of("record_count", 1);
        final Result result = new Result(data, summary);

        when(this.demographicService.getIDWithPersonCount(eq(new DemographicQuery(demographicRequest.getQuery())))).thenReturn(result);

        final MvcResult mvcResult = this.mockMvc.perform(post("/demographic/id/person_count")
                .contentType(MediaType.APPLICATION_JSON)
                .content(request))
                .andExpect(status().is(200))
                .andReturn();

        final Response actualResponse = this.mapper.readValue(mvcResult.getResponse().getContentAsString(), Response.class);

        assertThat(actualResponse).isEqualTo(new Response(data, summary));
    }

    private String readFileToString(final String fileName) throws IOException {
        final File file = new File(getClass().getClassLoader().getResource(fileName).getFile());
        return FileUtils.readFileToString(file, UTF_8);
    }

}
