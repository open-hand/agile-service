package io.choerodon.agile.infra.dto;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

/**
 * @author chihao.ran@hand-china.com
 * 2021/06/21 16:28
 */
@Table(name = "fd_custom_chart")
@ModifyAudit
@VersionAudit
public class CustomChartDTO extends AuditDomain {
    @Id
    @GeneratedValue
    private Long id;
    private String name;
    private String description;
    private String analysisField;
    private Boolean analysisFieldPredefined;
    private String comparedField;
    private Boolean comparedFieldPredefined;
    private String statisticsType;
    private String chartType;
    private String searchJson;
    private Long projectId;
    private Long organizationId;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public String getStatisticsType() {
        return statisticsType;
    }

    public void setStatisticsType(String statisticsType) {
        this.statisticsType = statisticsType;
    }

    public String getAnalysisField() {
        return analysisField;
    }

    public void setAnalysisField(String analysisField) {
        this.analysisField = analysisField;
    }

    public Boolean getAnalysisFieldPredefined() {
        return analysisFieldPredefined;
    }

    public void setAnalysisFieldPredefined(Boolean analysisFieldPredefined) {
        this.analysisFieldPredefined = analysisFieldPredefined;
    }

    public String getComparedField() {
        return comparedField;
    }

    public void setComparedField(String comparedField) {
        this.comparedField = comparedField;
    }

    public Boolean getComparedFieldPredefined() {
        return comparedFieldPredefined;
    }

    public void setComparedFieldPredefined(Boolean comparedFieldPredefined) {
        this.comparedFieldPredefined = comparedFieldPredefined;
    }

    public String getChartType() {
        return chartType;
    }

    public void setChartType(String chartType) {
        this.chartType = chartType;
    }

    public String getSearchJson() {
        return searchJson;
    }

    public void setSearchJson(String searchJson) {
        this.searchJson = searchJson;
    }
}
